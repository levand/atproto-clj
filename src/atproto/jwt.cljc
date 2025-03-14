(ns atproto.jwt
  "Cross platform JSON Web Token."
  (:require #?(:clj [atproto.impl.jvm :as jvm])))

(def public-jwks
  "Return a JSON Web Keyset with only the public keys."
  #?(:clj jvm/public-jwks))

(def query-jwks
  "A seq of JSON Web Keys matching the query.

  The query accepts either of the following keys:
  :alg   a JSON Web Algorithm identifier
  :kid   a key identifier"
  #?(:clj jvm/query-jwks))

(def generate-jwk
  "Generate a JSON Web Key for digital signatures."
  #?(:clj jvm/generate-jwk))

(def public-jwk
  "The public JWK for this private JWK."
  #?(:clj jvm/public-jwk))

(def jwk-kid
  "The Key identifier for this JWK."
  #?(:clj jvm/jwk-kid))

(def generate
  "Base64url-encoded serialization of the JSON Web token signed with the given key."
  #?(:clj jvm/generate-jwt))
