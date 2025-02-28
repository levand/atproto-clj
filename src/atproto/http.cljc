(ns atproto.http
  "Cross-platform HTTP client for the atproto client."
  (:require [clojure.string :as str]
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

(def impl-interceptor
  "Return implementation-specific HTTP interceptor."
  #?(:clj jvm/httpkit-interceptor))
