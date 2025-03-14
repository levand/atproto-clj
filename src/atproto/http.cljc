(ns atproto.http
  "Cross-platform HTTP client."
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            #?(:clj [atproto.impl.jvm :as jvm])))

(defn success?
  [code]
  (and (number? code)
       (<= 200 code 299)))

(defn error-map
  "Given an unsuccessful HTTP response, convert to an error map"
  [resp]
  {:error (str "HTTP " (:status resp))
   :http-response resp})

(def parse-url
  "Return a map with the following keys:

  <:protocol>://<:host>:<:port><:path>?<:query-string>#<:fragment>

  Return nil if the URL is invalid."
  #?(:clj jvm/parse-url))

(s/def ::url
  (s/conformer #(if (parse-url %)
                  %
                  ::s/invalid)))

(def interceptor
  "Return implementation-specific HTTP interceptor."
  #?(:clj jvm/httpkit-interceptor))
