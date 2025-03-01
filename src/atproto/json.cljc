(ns atproto.json
  "Cross platform JSON parser/serializer for the atproto client."
  (:require #?(:clj [atproto.impl.jvm :as jvm])))

(def interceptor
  #?(:clj jvm/json-interceptor))
