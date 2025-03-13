(ns atproto.json
  "Cross platform JSON parser/serializer."
  (:require #?(:clj [atproto.impl.jvm :as jvm])))

(def read-str
  #?(:clj jvm/json-read-str))

(def write-str
  #?(:clj jvm/json-write-str))

(def interceptor
  #?(:clj jvm/json-interceptor))
