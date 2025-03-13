(ns atproto.impl.jvm
  "JVM interceptor implementations"
  (:require [clojure.string :as str]
            [clojure.core.async :as a]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]
            [charred.api :as json]
            [atproto.interceptor :as i]
            [org.httpkit.client :as http])
  (:import [java.util Set Hashtable UUID Base64 Date]
           [java.net URLEncoder URLDecoder]
           [javax.naming.directory InitialDirContext]
           [javax.naming NamingException]
           [java.net URL MalformedURLException]
           [java.nio.charset StandardCharsets]
           [java.security SecureRandom MessageDigest]
           [com.nimbusds.jose.util Base64URL]
           [com.nimbusds.jwt JWTClaimsSet$Builder SignedJWT]
           [com.nimbusds.jose Algorithm JWSAlgorithm JWSHeader$Builder JOSEObjectType JWSSigner]
           [com.nimbusds.jose.jwk JWKSet JWK JWKSelector JWKMatcher JWKMatcher$Builder ECKey Curve KeyUse KeyType]
           [com.nimbusds.jose.jwk.gen JWKGenerator ECKeyGenerator OctetKeyPairGenerator RSAKeyGenerator]
           [com.nimbusds.jose.crypto.factories DefaultJWSSignerFactory]))

(set! *warn-on-reflection* true)

(defn- json-content-type?
  "Test if a request or response should be interpreted as json"
  [req-or-resp]
  (when-let [^String ct (:content-type (:headers req-or-resp))]
    (or (.startsWith ct "application/json")
        (.startsWith ct "application/did+ld+json"))))

(def json-read-str  #(json/read-json % :key-fn keyword))
(def json-write-str #(json/write-json-str %))

(def json-interceptor
  "Interceptor for JSON request and response bodies"
  {::i/name ::json
   ::i/enter (fn enter-json [ctx]
               (update ctx
                       ::i/request
                       (fn [{:keys [headers body] :as request}]
                         (if body
                           (let [request (if (not (:content-type (:headers request)))
                                           (assoc-in request  [:headers :content-type] "application/json")
                                           request)]
                             (if (json-content-type? request)
                               (update request :body json-write-str)
                               request))
                           request))))
   ::i/leave (fn leave-json [{:keys [::i/response] :as ctx}]
               (if (json-content-type? response)
                 (update-in ctx [::i/response :body] json-read-str)
                 ctx))})

(defn- normalize-headers
  "Convert keyword keys to strings for a header"
  [headers]
  (when headers
    (zipmap
     (map name (keys headers))
     (vals headers))))

(def httpkit-interceptor
  "Interceptor to handle HTTP requests using httpkit"
  {::i/name ::http
   ::i/enter (fn [ctx]
               (let [ctx (update-in ctx [::i/request :headers] normalize-headers)]
                 (http/request (::i/request ctx)
                               (fn [{:keys [^Throwable error] :as resp}]
                                 (i/continue
                                  (assoc ctx ::i/response (if error
                                                            {:error (.getName (.getClass error))
                                                             :message (.getMessage error)
                                                             :exception error}
                                                            (dissoc resp :opts))))))
                 nil))})


(defn url-encode [s] (URLEncoder/encode s))
(defn url-decode [s] (URLDecoder/decode s))

(defn parse-url
  [s]
  (try
    (let [url (URL. s)
          params (when-let [qs (.getQuery url)]
                   (some->> (str/split qs #"&")
                            (map #(let [[k v] (str/split % #"=")]
                                    (when v
                                      [(keyword k) (url-decode v)])))
                            (into {})))
          fragment (.getRef url)]
      (cond-> {:protocol (.getProtocol url)
               :host (.getHost url)}
        (not (= -1 (.getPort url)))       (assoc :port (.getPort url))
        (not (str/blank? (.getPath url))) (assoc :path (.getPath url))
        (seq params)                      (assoc :query-params params)
        fragment                          (assoc :fragment fragment)))
    (catch MalformedURLException _ nil)))

(defn- fetch-dns-record-values
  "Fetch DNS record values, blocking call."
  [{:keys [^String hostname type]}]
  (try
    (let [dir-ctx (InitialDirContext.
                   (Hashtable.
                    {"java.naming.factory.initial"
                     "com.sun.jndi.dns.DnsContextFactory"}))]
      {:values (seq (some-> dir-ctx
                            (.getAttributes hostname ^String/1 (into-array String [type]))
                            (.get type)
                            (.getAll)
                            (enumeration-seq)))})
    (catch NamingException ne
      {:error "DNS name not found"
       :exception ne})))

;; To configure timeout and retries,
;; See https://download.oracle.com/otn_hosted_doc/jdeveloper/904preview/jdk14doc/docs/guide/jndi/jndi-dns.html
(def dns-interceptor
  "Interceptor to query DNS record values."
  {::i/name ::dns
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [ch (a/io-thread
                         (fetch-dns-record-values request))]
                 (a/go
                   (i/continue (assoc ctx ::i/response (a/<! ch))))
                 nil))})

;; Crypto

(defn now
  []
  (int (/ (System/currentTimeMillis) 1000)))

(defn random-bytes
  [size]
  (let [seed (byte-array size)]
    (.nextBytes (SecureRandom.) seed)
    seed))

(defn sha256
  [^String s]
  (.digest (MessageDigest/getInstance "SHA-256")
           (.getBytes s StandardCharsets/UTF_8)))

(defn base64url-encode
  [^byte/1 bytes]
  (str (Base64URL/encode bytes)))

;; Java Web token

(defn- ^JWKGenerator jwk-generator
  "Return a KeyPair Generator for the given key type and options."
  [kty {:keys [^String alg ^String curve size]}]
  (let [curve (or (Curve/forStdName curve)
                  (first (Curve/forJWSAlgorithm (JWSAlgorithm/parse alg))))]
    (case (str kty)
      "RSA" (RSAKeyGenerator. (or size 2048) false)
      "EC"  (ECKeyGenerator. curve)
      "OKP" (OctetKeyPairGenerator. curve)
      nil)))

(defn generate-jwk
  [{:keys [^String alg ^String kid] :as opts}]
  (when-let [kty (KeyType/forAlgorithm (JWSAlgorithm/parse alg))]
    (when-let [generator (jwk-generator kty opts)]
      (-> generator
          (.keyUse KeyUse/SIGNATURE)
          (.keyID kid)
          (.generate)))))

(defn parse-jwks
  [^String s]
  (JWKSet/parse s))

(defn public-jwks
  [^JWKSet jwks]
  (.toPublicJWKSet jwks))

(defn- query->jwk-matcher
  [{:keys [^String alg ^String kid]}]
  (cond
    kid
    (-> (JWKMatcher$Builder.)
        (.keyIDs ^Set (set [kid]))
        (.build))

    alg
    (let [jws-alg (JWSAlgorithm/parse alg)]
      (-> (JWKMatcher$Builder.)
          (.keyType (KeyType/forAlgorithm jws-alg))
          (.curves ^Set (set (Curve/forJWSAlgorithm jws-alg)))
          (.build)))))

(defn query-jwks
  [^JWKSet jwks query]
  (.select (JWKSelector. (query->jwk-matcher query))
           jwks))

(defn jwk-kid
  [^JWK jwk]
  (.getKeyID jwk))

(defn public-jwk
  [^JWK jwk]
  (.toPublicJWK jwk))

(defn generate-jwt
  [^JWK jwk {:keys [^String alg] :as headers} claims]
  (let [header (->> headers
                    ^JWSHeader$Builder
                    (reduce (fn [^JWSHeader$Builder builder [k v]]
                              (case k
                                :alg builder
                                :kid (.keyID builder v)
                                :typ (.type builder (JOSEObjectType. v))
                                :jwk (.jwk builder v)
                                (.customParam builder (name k) v)))
                            (JWSHeader$Builder. (JWSAlgorithm/parse alg)))
                    (.build))
        payload (->> claims
                     ^JWTClaimsSet$Builder
                     (reduce (fn [^JWTClaimsSet$Builder builder [k v]]
                               (.claim builder (name k) v))
                             (JWTClaimsSet$Builder.))
                     (.build))
        jwt (SignedJWT. header payload)
        signer (.createJWSSigner (DefaultJWSSignerFactory.) jwk)]
    (.sign jwt signer)
    (.serialize jwt)))
