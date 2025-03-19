(ns atproto.lexicon.translator
  "Translate Lexicon schemas to Clojure spec forms."
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [atproto.data.json :as json]
            [atproto.lexicon.specs :as lexicon]
            [atproto.lexicon.schema :as schema]))

;; TODO:
;; - should conform set the default value? (it needs to be on the container)
;; - test data generators for specs

(defn lex-uri->spec-key
  "The Spec key corresponding to this Lexicon schema URI."
  [s]
  (let [[nsid type-name] (str/split s #"#")]
    (if type-name
      (keyword nsid type-name)
      (let [idx (str/last-index-of nsid \.)]
        (keyword (subs nsid 0 idx)
                 (subs nsid (inc idx)))))))

(defn- spec-key
  "The spec key in the current context."
  [ctx]
  (lex-uri->spec-key (:spec-ns ctx)))

(defn- nest
  "Add the type name to the current context, if not 'main'."
  [ctx type-name]
  (if (= "main" type-name)
    ctx
    (update ctx :spec-ns str "." type-name)))

(defn- add-spec!
  "Add the spec definition to the context."
  [ctx spec]
  (swap! (:def-specs ctx) conj `(s/def ~(spec-key ctx) ~spec)))

(defmulti ^:private field-type-def->spec
  "Translate the field type definition into a spec form and return it.

  Implementations can add 'sub specs' in the context as needed."
  (fn [ctx type-def] (:type type-def)))

(defmulti ^:private translate-primary-type-def!
  "Translate the Lexicon Primary Type Definition into spec forms and add them to the context."
  (fn [ctx primary-type-def] (:type primary-type-def)))

(defn translate
  "Translate the Lexicon file into Clojure spec forms and return them as a seq."
  [{:keys [id] :as file}]
  (let [conformed-file (s/conform ::schema/file file)]
    (if (= ::s/invalid conformed-file)
      (throw (ex-info "Invalid Lexicon schema file." {:file file}))
      (->> (reduce (fn [ctx [type-key [type type-def]]]
                     (let [ctx (nest ctx (name type-key))]
                       (case type
                         :primary (translate-primary-type-def! ctx type-def)
                         :field   (add-spec! ctx (field-type-def->spec ctx type-def))))
                     ctx)
                   {:nsid id
                    :spec-ns id
                    :def-specs (atom [])}
                   (:defs conformed-file))
           :def-specs
           deref))))

(defn- rkey-type->spec
  "Translate the record key type into a spec form and return it."
  [[rkey-type rkey-value]]
  (case rkey-type
    :tid     ::lexicon/tid
    :nsid    ::lexicon/nsid
    :literal #{rkey-value}
    :any     ::lexicon/record-key))

(defmethod translate-primary-type-def! "record"
  [{:keys [spec-ns] :as ctx} {:keys [key record]}]
  (let [rkey-spec-key (let [ctx (nest ctx "key")]
                        (add-spec! ctx (rkey-type->spec key))
                        (spec-key ctx))]
    (add-spec! ctx `(s/and
                     (s/keys :req-un [~rkey-spec-key])
                     ~(field-type-def->spec ctx record)))))

(defn- translate-http-body-schema-def!
  "Translate a HTTP body schema into a Clojure spec form and add it to the context."
  [ctx {:keys [encoding schema]}]
  (let [spec-keys (cond-> []
                    encoding
                    (conj (let [ctx (nest ctx "encoding")]
                            (add-spec! ctx (lexicon/mime-type-pattern->spec encoding))
                            (spec-key ctx)))
                    schema
                    (conj (let [ctx (nest ctx "body")]
                            (add-spec! ctx (field-type-def->spec ctx (second schema)))
                            (spec-key ctx))))]
    (add-spec! ctx
               `(s/keys :req-un [~@spec-keys]))))

(defn translate-primary-type-field!
  [ctx [field-name field-def]]
  (case field-name
    :parameters
    (let [ctx (nest ctx "params")]
      (add-spec! ctx (field-type-def->spec ctx field-def))
      (spec-key ctx))

    (:input :output)
    (let [ctx (nest ctx (name field-name))]
      (translate-http-body-schema-def! ctx field-def)
      (spec-key ctx))

    :errors
    (let [ctx (nest ctx "error")]
      (add-spec! ctx (set field-def))
      (spec-key ctx))

    :message
    (let [ctx (nest ctx "message")]
      (add-spec! ctx (field-type-def->spec ctx (:schema field-def)))
      (spec-key ctx))))

;; shared implementation for query/procedure/subscription
(defmethod translate-primary-type-def! :default
  [ctx type-def]
  (let [spec-keys (mapv #(translate-primary-type-field! ctx %)
                        (select-keys type-def
                                     [:parameters :input :output :errors :message]))]
    (add-spec! ctx
               `(s/keys :opt-un [~@spec-keys]))))

;; Field Type Definitions

(defmethod field-type-def->spec "null"
  [ctx def]
  ::json/null)

(defmethod field-type-def->spec "boolean"
  [ctx {:keys [default const]}]
  (let [specs (cond-> [::json/boolean]
                const (conj `#{~const}))]
    `(s/and ~@specs)))

(defmethod field-type-def->spec "integer"
  [ctx {:keys [minimum maximum enum default const]}]
  (let [specs (cond-> [::json/integer]
                minimum  (conj `#(<= ~minimum %))
                maximum  (conj `#(<= % ~maximum))
                enum     (conj `#{~@enum})
                const    (conj `#{~const}))]
    `(s/and ~@specs)))

(defmethod field-type-def->spec "string"
  [ctx {:keys [format maxLength minLength maxGraphemes minGraphemes
               knownValues enum default const]}]
  (let [specs (cond-> [::json/string]
                format       (conj (keyword "atproto.lexicon.specs" format))
                maxLength    (conj `#(<= (count %) ~maxLength))
                minLength    (conj `#(<= ~minLength (count %)))
                maxGraphemes (conj `#(<= (lexicon/count-graphemes %) ~maxGraphemes))
                minGraphemes (conj `#(<= ~minGraphemes (~lexicon/count-graphemes %)))
                enum         (conj `#{~@enum})
                const        (conj `#{~const}))]
    `(s/and ~@specs)))

(defmethod field-type-def->spec "bytes"
  [ctx {:keys [minLength maxLength]}]
  (let [specs (cond-> [::json/bytes]
                minLength (conj `#(<= ~minLength (count %)))
                maxLength (conj `#(<= (count %) ~maxLength)))]
    `(s/and ~@specs)))

(defmethod field-type-def->spec "cid-link"
  [ctx def]
  `::json/cid-link)

(defmethod field-type-def->spec "blob"
  [ctx {:keys [accept maxSize]}]
  (let [specs (cond-> [::json/blob]
                accept  (conj `#(lexicon/accept-mime-type? ~accept (:mimeType %)))
                maxSize (conj `#(<= (:size %) ~maxSize)))]
    `(s/and ~@specs)))

(defmethod field-type-def->spec "array"
  [ctx {:keys [items minLength maxLength]}]
  (let [items-spec (field-type-def->spec ctx items)
        opts (cond-> {}
               minLength (assoc :min-count minLength)
               maxLength (assoc :max-count maxLength))]
    `(s/and ::json/array
            (s/coll-of ~items-spec ~opts))))

(defmethod field-type-def->spec "object"
  [ctx {:keys [properties required nullable]}]
  (let [required? (set (map keyword required))
        nullable? (set (map keyword nullable))
        keys (reduce (fn [keys [prop-key prop-type]]
                       (let [ctx (nest ctx (name prop-key))
                             spec (let [spec (field-type-def->spec ctx prop-type)]
                                    (if (nullable? prop-key)
                                      `(s/nilable ~spec)
                                      spec))]
                         (add-spec! ctx spec)
                         (update keys
                                 (if (required? prop-key) :required :optional)
                                 conj
                                 (spec-key ctx))))
                     {:required []
                      :optional []}
                     properties)]
    `(s/and ::json/object
            (s/keys :req-un ~(:required keys)
                    :opt-un ~(:optional keys)))))

(defmethod field-type-def->spec "params"
  [ctx {:keys [required properties]}]
  (let [required? (set (map keyword required))
        keys (reduce (fn [keys [prop-key prop-type]]
                       (let [ctx (nest ctx (name prop-key))
                             spec (field-type-def->spec ctx prop-type)]
                         (add-spec! ctx spec)
                         (update keys
                                 (if (required? prop-key) :required :optional)
                                 conj
                                 (spec-key ctx))))
                     {:required []
                      :optional []}
                     properties)]
    `(s/and ::json/object
            (s/keys :req-un ~(:required keys)
                    :opt-un ~(:optional keys)))))

(defmethod field-type-def->spec "token"
  [ctx _]
  `(constantly false))

(defn resolve-ref
  "Resolve the Lexicon reference."
  [ctx [ref-type ref]]
  (case ref-type
    :local-ref  (str (:nsid ctx) "#" ref)
    :global-ref ref))

(defmethod field-type-def->spec "ref"
  [ctx {:keys [ref]}]
  ;; We could simply return the spec key here but that would
  ;; require toposorting the specs before defining them.
  ;; I'm not even sure there can't be any cycle in the Lexicon schemas.
  ;; Downside: we only detect missing specs at validation time.
  (let [spec-key (lex-uri->spec-key (resolve-ref ctx ref))]
    `(s/nonconforming
      (s/or ~spec-key ~spec-key))))

(defmulti object-spec
  "Return the spec for an object at validation time based on its $type field."
  (constantly nil))

(defmethod object-spec :default
  [v]
  (if (:$type v)
    (lex-uri->spec-key (:$type v))
    any?))

(defmethod field-type-def->spec "union"
  [ctx {:keys [refs closed]}]
  (let [spec (if (not (seq refs))
               ;; closed is false by definition
               (s/multi-spec object-spec identity)
               (let [branches (cond-> (->> refs
                                           (map #(lex-uri->spec-key (resolve-ref ctx %)))
                                           (mapcat #(repeat 2 %)))
                                (not closed)
                                (concat `[:unknown (s/multi-spec object-spec identity)]))]
                 `(s/or ~@branches)))]
    `(s/and ::json/object
            #(contains? % :$type)
            ~spec)))

(defmethod field-type-def->spec "unknown"
  [ctx _]
  `(s/and ::json/object
          (s/multi-spec object-spec identity)))
