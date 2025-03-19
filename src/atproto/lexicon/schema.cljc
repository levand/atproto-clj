(ns atproto.lexicon.schema
  "Clojure specs to validate a Lexicon schema.

  See https://atproto.com/specs/lexicon"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [atproto.lexicon.specs :as lexicon]
            ;; Aliases to use shorter spec keys
            [atproto.lexicon.schema.file              :as-alias file]
            [atproto.lexicon.schema.type-def          :as-alias type-def]
            [atproto.lexicon.schema.error             :as-alias error]
            [atproto.lexicon.schema.body              :as-alias body]
            [atproto.lexicon.schema.message           :as-alias message]
            [atproto.lexicon.schema.type.null         :as-alias null]
            [atproto.lexicon.schema.type.boolean      :as-alias boolean]
            [atproto.lexicon.schema.type.integer      :as-alias integer]
            [atproto.lexicon.schema.type.string       :as-alias string]
            [atproto.lexicon.schema.type.bytes        :as-alias bytes]
            [atproto.lexicon.schema.type.cid-link     :as-alias cid-link]
            [atproto.lexicon.schema.type.array        :as-alias array]
            [atproto.lexicon.schema.type.object       :as-alias object]
            [atproto.lexicon.schema.type.blob         :as-alias blob]
            [atproto.lexicon.schema.type.params       :as-alias params]
            [atproto.lexicon.schema.type.token        :as-alias token]
            [atproto.lexicon.schema.type.ref          :as-alias ref]
            [atproto.lexicon.schema.type.union        :as-alias union]
            [atproto.lexicon.schema.type.unknown      :as-alias unkown]
            [atproto.lexicon.schema.type.record       :as-alias record]
            [atproto.lexicon.schema.type.query        :as-alias query]
            [atproto.lexicon.schema.type.procedure    :as-alias procedure]
            [atproto.lexicon.schema.type.subscription :as-alias subscription]))

;; -----------------------------------------------------------------------------
;; Schema
;; -----------------------------------------------------------------------------

(defn- valid-defs?
  "Wether the defs property in a lexicon schema is valid.

  - A schema can have at most one definition with one of the primary types.
  - Primary types should always have the name main."
  [defs]
  (let [primary-type-defs (filter (fn [[name def]]
                                    (= :primary (first def)))
                                  defs)]
    (or (zero? (count primary-type-defs))
        (and (= 1 (count primary-type-defs))
             (= :main (ffirst primary-type-defs))))))

(s/def ::file
  (s/keys :req-un [::file/lexicon
                   ::file/id
                   ::file/defs]
          :opt-un [::file/$type
                   ::file/revision
                   ::file/description]))

(s/def ::file/lexicon #{1})
(s/def ::file/$type #{"com.atproto.lexicon.schema"})
(s/def ::file/id ::lexicon/nsid)
(s/def ::file/revision int?)
(s/def ::file/description string?)
(s/def ::file/defs (s/and (s/map-of keyword?
                                    (s/or :primary ::primary-type-def
                                          :field   ::field-type-def))
                          valid-defs?))

;; shared properties across all type definitions
(s/def ::type-def/type string?)

(s/def ::type-def/description string?)

(defn field-type-def
  "Return a spec expecting a field type definition of the given type(s)."
  [& types]
  (s/and #(contains? (set types) (:type %))
         ::field-type-def))

;; -----------------------------------------------------------------------------
;; Field Type Definition
;; -----------------------------------------------------------------------------

(defmulti field-type-spec :type)

(s/def ::field-type-def
  (s/and (s/keys :req-un [::type-def/type]
                 :opt-un [::type-def/description])
         (s/multi-spec field-type-spec :type)))

(defmethod field-type-spec "null" [_]
  any?)

(defmethod field-type-spec "boolean" [_]
  (s/keys :opt-un [::boolean/default
                   ::boolean/const]))

(s/def ::boolean/default boolean?)
(s/def ::boolean/const boolean?)

(defmethod field-type-spec "integer" [_]
  (s/keys :opt-un [::integer/minimum
                   ::integer/maximum
                   ::integer/enum
                   ::integer/default
                   ::integer/const]))

(s/def ::integer/minimum int?)
(s/def ::integer/maximum int?)
(s/def ::integer/enum (s/coll-of int?))
(s/def ::integer/default int?)
(s/def ::integer/const int?)

(defmethod field-type-spec "string" [_]
  (s/keys :opt-un [::string/format
                   ::string/maxLength
                   ::string/minLength
                   ::string/maxGraphemes
                   ::string/minGraphemes
                   ::string/knownValues
                   ::string/enum
                   ::string/default
                   ::string/const]))

(s/def ::string/format #{"at-identifier" "at-uri" "cid" "datetime" "did"
                         "handle" "nsid" "tid" "record-key" "uri" "language"})
(s/def ::string/maxLength int?)
(s/def ::string/minLength int?)
(s/def ::string/maxGraphemes int?)
(s/def ::string/minGraphemes int?)
(s/def ::string/knownValues (s/coll-of string?))
(s/def ::string/enum (s/coll-of string?))
(s/def ::string/default string?)
(s/def ::string/const string?)

(defmethod field-type-spec "bytes" [_]
  (s/keys :opt-un [::bytes/minLength
                   ::bytes/maxLength]))

(s/def ::bytes/minLength int?)
(s/def ::bytes/maxLength int?)

(defmethod field-type-spec "cid-link" [_]
  any?)

(defmethod field-type-spec "blob" [_]
  (s/keys :opt-un [::blob/accept
                   ::blob/maxSize]))

(s/def ::blob/accept (s/coll-of ::lexicon/mime-type-pattern))
(s/def ::blob/maxSize int?)

(defmethod field-type-spec "array" [_]
  (s/keys :req-un [::array/items]
          :opt-un [::array/minLength
                   ::array/maxLength]))

(s/def ::array/items ::field-type-def)
(s/def ::array/minLength int?)
(s/def ::array/maxLength int?)

(defmethod field-type-spec "object" [_]
  (s/keys :req-un [::object/properties]
          :opt-un [::object/required
                   ::object/nullable]))

(s/def ::object/properties (s/map-of keyword? ::field-type-def))
(s/def ::object/required (s/coll-of string?))
(s/def ::object/nullable (s/coll-of string?))

(defmethod field-type-spec "params" [_]
  (s/keys :req-un [::params/properties]
          :opt-un [::params/required]))

(s/def ::params/properties
  (s/map-of keyword?
            (field-type-def "boolean" "integer" "string" "unknown" "array")))

(s/def ::params/required
  (s/coll-of string?))

(defmethod field-type-spec "token" [_]
  any?)

(defmethod field-type-spec "ref" [_]
  (s/keys :req-un [::ref/ref]))

(s/def ::ref/ref
  (s/or :local-ref (s/conformer
                    (fn [s]
                      (if (str/starts-with? s "#")
                        (subs s 1)
                        ::s/invalid)))
        :global-ref (s/conformer
                     (fn [s]
                       (let [[nsid name] (str/split s #"#")]
                         (if (s/valid? ::lexicon/nsid nsid)
                           s
                           ::s/invalid))))))

(defmethod field-type-spec "union" [_]
  (s/and
   (s/keys :opt-un [::union/refs
                    ::union/closed])
   ;; refs is only optional if closed=false
   (fn [{:keys [refs closed]}]
     (or (not (empty? refs))
         (not closed)))))

(s/def ::union/refs (s/coll-of ::ref/ref))

(s/def ::union/closed boolean?)

(defmethod field-type-spec "unknown" [_]
  any?)

;; -----------------------------------------------------------------------------
;; Primary Type Definitions
;; -----------------------------------------------------------------------------

(s/def ::error
  (s/keys :req-un [::error/name]
          :opt-un [::error/description]))

(s/def ::error/name (s/and string? #(not (re-matches #"\s" %))))
(s/def ::error/description string?)

(s/def ::body
  (s/keys :req-un [::body/encoding]
          :opt-un [::body/description
                   ::body/schema]))

(s/def ::body/encoding ::lexicon/mime-type-pattern)
(s/def ::body/description string?)
(s/def ::body/schema (s/or :object (field-type-def "object")
                           :ref    (field-type-def "ref")
                           :union  (field-type-def "union")))

(defmulti primary-type-spec :type)

(s/def ::primary-type-def
  (s/and (s/keys :req-un [::type-def/type]
                 :opt-un [::type-def/description])
         (s/multi-spec primary-type-spec :type)))

(defmethod primary-type-spec "record" [_]
  (s/keys :req-un [::record/key
                   ::record/record]))

(s/def ::record/record (field-type-def "object"))
(s/def ::record/key
  (s/or :tid     #{"tid"}
        :nsid    #{"nsid"}
        :literal #(let [[_ value] (re-matches #"literal:(.+)$" %)]
                    (or value ::s/invalid))
        :any     #{"any"}))

(defmethod primary-type-spec "query" [_]
  (s/keys :opt-un [::query/parameters
                   ::query/output
                   ::query/errors]))

(s/def ::query/parameters (field-type-def "params"))
(s/def ::query/output ::body)
(s/def ::query/errors (s/coll-of ::error))

(defmethod primary-type-spec "procedure" [_]
  (s/keys :opt-un [::procedure/parameters
                   ::procedure/output
                   ::procedure/input
                   ::procedure/errors]))

(s/def ::procedure/parameters (field-type-def "params"))
(s/def ::procedure/output ::body)
(s/def ::procedure/input ::body)
(s/def ::procedure/errors (s/coll-of ::error))

(defmethod primary-type-spec "subscription" [_]
  (s/keys :opt-un [::subscription/parameters
                   ::subscription/message
                   ::subscription/errors]))

(s/def ::subscription/parameters (field-type-def "params"))
(s/def ::subscription/message (s/keys :req-un [::message/schema]
                                      :opt-un [::message/description]))
(s/def ::message/schema (field-type-def "union"))
(s/def ::message/description string?)
