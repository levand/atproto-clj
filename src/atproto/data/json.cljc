(ns atproto.data.json
  "Records and messages in atproto are stored, transmitted, encoded, and authenticated in a consistent way.

  The core data model supports both binary (CBOR) and textual (JSON) representations.

  See https://atproto.com/specs/data-model"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [multiformats.cid :as cid]
            [clojure.math :as math]
            ;; aliases for specs
            [atproto.data.json.bytes    :as-alias bytes]
            [atproto.data.json.cid-link :as-alias cid-link]
            [atproto.data.json.object   :as-alias object]
            [atproto.data.json.blob     :as-alias blob])
  #?(:clj (:import [java.util Base64]
                   [clojure.lang ExceptionInfo])))

;; Limit integers because JS has only 53-bit precision
(def js-max-integer (math/pow 2 53))

;; Helpers

(defn mime-type?
  [s]
  (re-matches #"^(\w+)\/((?:[\w\.-]+)(?:\+[\w\.-]+)?)(?:\s*;\s*([\S\.-=]+)?)?$"
              s))

(defn nsid?
  [s]
  (and (string? s)
       (<= (count s) 317)
       (re-matches #"^[a-zA-Z]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)+(\.[a-zA-Z]([a-zA-Z0-9]{0,62})?)$" s)))

(defn cid?
  [s]
  (when (string? s)
    (try
      (cid/parse s)
      true
      (catch ExceptionInfo _
        false))))

(s/def ::type
  ;; nonconforming: we don't need to tag the object and it causes some
  ;; complications with the Lexicon specs that reuse those json types.
  (s/nonconforming
   (s/or :null     ::null
         :boolean  ::boolean
         :integer  ::integer
         :string   ::string
         :bytes    ::bytes
         :cid-link ::cid-link
         :array    ::array
         :object   ::object
         :blob     ::blob)))

(s/def ::null nil?)

(s/def ::boolean boolean?)

(s/def ::integer
  (s/conformer #(or (and (int? %)
                         (<= (- js-max-integer) % (dec js-max-integer))
                         (long %))
                    ::s/invalid)))

(s/def ::string string?)

(s/def ::bytes
  (s/keys :req-un [::bytes/$bytes]))

(s/def ::bytes/$bytes
  (s/conformer #(or #?(:clj (try
                              (.decode (Base64/getDecoder) %)
                              (catch Exception _)))
                    ::s/invalid)))

(s/def ::cid-link
  (s/keys :req-un [::cid-link/$link]))

(s/def ::cid-link/$link cid?)

(s/def ::array
  (s/coll-of ::type))

(def reserved-field? #{:$type :$bytes :$link})
(defn $field? [kwd] (str/starts-with? (name kwd) "$"))

(s/def ::object
  (s/and
   (s/keys :opt-un [::object/$type])
   (s/conformer
    (fn [object]
      ;; Cannot use reserved fields
      (if (some reserved-field? (keys (dissoc object :$type)))
        ::s/invalid
        ;; Ignore $-fields for validation
        (let [object-without-$fields (into {} (remove #($field? (key %)) object))
              conformed-object (s/conform (s/map-of keyword? ::type)
                                          object-without-$fields)]
          (if (= ::s/invalid conformed-object)
            ::s/invalid
            ;; Put back the $fields in the object
            (merge conformed-object
                   (filter #($field? (key %)) object)))))))))

(s/def ::object/$type
  nsid?)

(s/def ::blob
  (s/keys :req-un [::blob/ref
                   ::blob/mimeType
                   ::blob/size]))

(s/def ::blob/ref ::cid-link)
(s/def ::blob/mimeType mime-type?)
(s/def ::blob/size pos-int?)
