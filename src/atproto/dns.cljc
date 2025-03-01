(ns atproto.dns
  "Cross-platform DNS client for the atproto client."
  (:require #?(:clj [atproto.impl.jvm :as jvm])))

(def interceptor
  #?(:clj jvm/dns-interceptor))
