(ns atproto.lexicon.specs
  "Primitive Lexicon specs used by `atproto.lexicon.schema` and `atproto.lexicon.generator`."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [atproto.data.json :as json])
  #?(:clj (:import [java.time Instant]
                   [java.util Base64])))

(defn count-graphemes
  [s]
  #?(:clj (.codePointCount s 0 (count s))))

(s/def ::did
  (s/and string?
         #(<= (count %) 2048)
         #(re-matches #"^did:[a-z]+:[a-zA-Z0-9._:%-]*[a-zA-Z0-9._-]$" %)
         (s/conformer str/lower-case)))

(s/def ::handle
  (s/and string?
         #(<= (count %) 253)
         #(re-matches #"^([a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?$" %)))

(s/def ::at-identifier
  (s/or :handle ::handle
        :did ::did))

(s/def ::nsid
  (s/and string?
         #(<= (count %) 317)
         #(re-matches #"^[a-zA-Z]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)+(\.[a-zA-Z]([a-zA-Z0-9]{0,62})?)$" %)))

(s/def ::record-key
  (s/and string?
         #(<= 1 (count %) 512)
         #(re-matches #"^[a-zA-Z0-9_~.:-]{1,512}$" %)
         #(not (#{"." ".."} %))))

(s/def ::tid
  (s/and string?
         #(= 13 (count %))
         #(re-matches #"^[234567abcdefghij][234567abcdefghijklmnopqrstuvwxyz]{12}$" %)))

(s/def ::cid
  json/cid?)

;; todo: valid handle or did in authority
;;       valid nsid in collection (optional)
;;       make regexp work with ClojureScript. In Java, inside a character class, the escape character (\) must be escaped.
(s/def ::at-uri
  (s/and string?
         #(< (count %) (* 8 1024))
         #(re-matches #"^at:\/\/([a-zA-Z0-9._:%-]+)(\/([a-zA-Z0-9-.]+)(\/([a-zA-Z0-9._~:@!$&%')(*+,;=-]+))?)?(#(\/[a-zA-Z0-9._~:@!$&%')(*+,;=\\-[\\]/\\]*))?$" %)))

;; todo: support `1985-04-12T23:20:50.12345678912345Z`?
(s/def ::datetime
  #?(:clj #(try (Instant/parse %) (catch Exception _))))

(s/def ::uri
  #?(:clj #(try (java.net.URI. %) (catch Exception _))))

(s/def ::language
  #?(:clj #(try (-> (java.util.Locale$Builder.) (.setLanguageTag %)) (catch Exception _))))

(s/def ::mime-type
  (s/conformer
   #(or (when-let [[_ type subtype parameter] (re-matches #"^(\w+)\/((?:[\w\.-]+)(?:\+[\w\.-]+)?)(?:\s*;\s*([\S\.-=]+)?)?$" %)]
          (cond-> {:type type
                   :subtype subtype}
            (not (str/blank? parameter)) (assoc :parameter parameter)))
        ::s/invalid)))

;; todo: support more complex glob patterns?
(s/def ::mime-type-pattern
  (s/or :any     #{"*/*"}
        :type    (s/conformer #(or (when-let [[_ type] (re-matches #"^(\w+)\/\*" %)]
                                     {:type type})
                                   ::s/invalid))
        :subtype ::mime-type))

(defn mime-type-pattern->spec
  "Translate a MIME type pattern into a Spec form and return it."
  [[pattern-type pattern]]
  (let [spec (case pattern-type
               :any     'any?
               :type    `#(= ~pattern (select-keys % [:type]))
               :subtype `#(= ~pattern (select-keys % [:type :subtype])))]
    `(s/and ::mime-type
            ~spec)))

(defn accept-mime-type?
  [accept mime-type]
  (some #(s/valid? (mime-type-pattern->spec %) mime-type)
        accept))
