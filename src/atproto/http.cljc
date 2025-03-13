(ns atproto.http
  "Cross-platform HTTP client."
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            #?(:clj [atproto.impl.jvm :as jvm])))

(defn success?
  [code]
  (and (number? code)
       (<= 200 code 299)))

(defn client-error?
  [code]
  (and (number? code)
       (<= 400 code 499)))

(defn redirect?
  [code]
  (and (number? code)
       (<= 300 code 399)))

(defn error-map
  "Given an unsuccessful HTTP response, convert to an error map"
  [resp]
  {:error (str "HTTP " (:status resp))
   :http-response resp})

(def url-encode
  "Encode the string to be included in a URL."
  #?(:clj jvm/url-encode))

(def url-decode
  "Encode the string to be included in a URL."
  #?(:clj jvm/url-decode))

(def parse-url
  "Return a map with the following keys:

  <:protocol>://<:host>:<:port><:path>?<:query-params>#<:fragment>

  All values are strings except for query-params which is a
  map of keywords to URL-decoded values

  Return nil if the URL is invalid."
  #?(:clj jvm/parse-url))

(defn serialize-url
  "Take a URL map and return the URL string.

  The query-params values will be URL-encoded."
  [{:keys [protocol host port path query-params fragment]}]
  (str protocol "://"
       host
       (when port (str ":" port))
       path
       (when (not (empty? query-params))
         (->> query-params
              (map (fn [[k v]] (str (name k) "=" (url-encode v))))
              (str/join "&")
              (str "?")))
       (when fragment (str "#" fragment))))

(s/def ::url
  (s/conformer #(if (parse-url %)
                  %
                  ::s/invalid)))

(def interceptor
  "Return implementation-specific HTTP interceptor."
  #?(:clj jvm/httpkit-interceptor))
