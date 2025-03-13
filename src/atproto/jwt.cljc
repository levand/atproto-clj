(ns atproto.jwt
  "Cross platform JSON Web Token."
  (:require #?(:clj [atproto.impl.jvm :as jvm])))

(def generate-jwk
  "Generate a JSON Web Key for digital signatures."
  #?(:clj jvm/generate-jwk))

(def parse-jwks
  "Parse a JSON Web Keyset from a JSON-serialized string."
  #?(:clj jvm/parse-jwks))

(def public-jwks
  "Return a JSON Web Keyset with only the public keys."
  #?(:clj jvm/public-jwks))

(def query-jwks
  "A seq of JSON Web Keys matching the query.

  The query accepts either of the following keys:
  :alg   a JSON Web Algorithm identifier
  :kid   a key identifier"
  #?(:clj jvm/query-jwks))

(def public-jwk
  "The public JWK for this private JWK."
  #?(:clj jvm/public-jwk))

(def jwk-kid
  "The Key identifier for this JWK."
  #?(:clj jvm/jwk-kid))

(def generate
  "Generate a JSON Web token and sign it with the given key."
  #?(:clj jvm/generate-jwt))
