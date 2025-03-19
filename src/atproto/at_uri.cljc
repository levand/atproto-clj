(ns atproto.at-uri
  "The AT URI scheme (at://) makes it easy to reference individual records in a specific repository,
  identified by either DID or handle.

  See https://atproto.com/specs/at-uri-scheme"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [atproto.http :as http]
            [atproto.did :as did]
            [atproto.handle :as handle]))

(defn parse-uri
  "Parse the string and return a map with: [scheme:][//authority][path][?query][#fragment].

  Return nil if the string is not a well-formed URI."
  [s]
  #?(:clj (try
            (let [uri (java.net.URI. s)]
              (cond-> {:scheme (.getScheme uri)
                       :authority (.getAuthority uri)}
                (not (str/blank? (.getPath uri)))     (assoc :path (.getPath uri))
                (not (str/blank? (.getQuery uri)))    (assoc :query (.getQuery uri))
                (not (str/blank? (.getFragment uri))) (assoc :fragment (.getFragment uri))))
            (catch Exception _))))

;; todo: finish validation
;; see https://github.com/bluesky-social/atproto/blob/main/packages/syntax/src/aturi_validation.ts
(defn parse-at-uri
  "Parse the at-uri string and return a map with :authority, :collection, and :rkey.

  Return nil if the string is not a well-formed AT URI."
  [s]
  (when-let [{:keys [scheme authority path query fragment] :as m} (parse-uri s)]
    (when (and (= scheme "at")
               (or (s/valid? ::did/did authority)
                   (s/valid? ::handle/handle authority)))
      (let [[collection rkey & xs] (str/split path #"/")]
        (when (empty? xs)
          (cond-> {:authority authority}
            collection (assoc :collection collection)
            rkey (assoc :rkey rkey)))))))

(s/def ::at-uri
  (s/and string?
         parse-at-uri))

(comment

  (require 'atproto.at-uri :reload)

  (atproto.at-uri/parse-at-uri "at://example.com")

  )
