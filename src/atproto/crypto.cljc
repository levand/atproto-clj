(ns atproto.crypto
  "Cross-platform crypto functions."
  (:require [clojure.string :as str]
            #?(:clj [atproto.impl.jvm :as jvm])))

(def now
  "Number of seconds since epoch."
  #?(:clj jvm/now))

(def random-bytes
  "Random byte array of the given size."
  #?(:clj jvm/random-bytes))

(def sha256
  "sha256 of the bytes"
  #?(:clj jvm/sha256))

(def base64url-encode
  "encode the bytes to a url-safe base64 string."
  #?(:clj jvm/base64url-encode))

(defn generate-pkce
  "Proof Key for Code Exchange (S256) with a verifier of the given size."
  [size]
  (let [verifier (base64url-encode (random-bytes size))]
    {:verifier verifier
     :challenge (base64url-encode (sha256 verifier))
     :method "S256"}))

(defn generate-nonce
  "Generate a random base64 url-safe string."
  [size]
  (base64url-encode (random-bytes size)))
