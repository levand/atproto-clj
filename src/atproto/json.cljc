(ns atproto.json
  "Cross platform JSON parser/serializer."
  (:require #?(:clj [atproto.impl.jvm :as jvm])))

(def interceptor
  #?(:clj jvm/json-interceptor))
