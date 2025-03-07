(ns atproto.impl.jvm
  "JVM interceptor implementations"
  (:require [clojure.string :as str]
            [clojure.core.async :as a]
            [clojure.spec.alpha :as s]
            [charred.api :as json]
            [atproto.interceptor :as i]
            [org.httpkit.client :as http])
  (:import [java.util Hashtable]
           [javax.naming.directory InitialDirContext]
           [javax.naming NamingException]
           [java.net URL MalformedURLException]))

(set! *warn-on-reflection* true)

(defn- json-content-type?
  "Test if a request or response should be interpreted as json"
  [req-or-resp]
  (when-let [^String ct (:content-type (:headers req-or-resp))]
    (or (.startsWith ct "application/json")
        (.startsWith ct "application/did+ld+json"))))

(def json-interceptor
  "Interceptor for JSON request and response bodies"
  {::i/name ::json
   ::i/enter (fn enter-json [{:keys [::i/request] :as ctx}]
               (if (and (:body request)
                        (or (json-content-type? request)
                            (not (:content-type (:headers request)))))
                 (update-in ctx [::i/request :body] json/write-json-str)
                 ctx))
   ::i/leave (fn leave-json [{:keys [::i/response] :as ctx}]
               (if (json-content-type? response)
                 (update-in ctx [::i/response :body]
                            #(json/read-json % :key-fn keyword))
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
                                 (if error
                                   (i/continue (assoc ctx
                                                      ::i/response {:error (.getName (.getClass error))
                                                                    :message (.getMessage error)
                                                                    :exception error}))
                                   (i/continue (assoc ctx
                                                      ::i/response resp
                                                      ::response resp)))))
                 nil))})

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

(defn parse-url
  [input]
  (try
    (let [url (URL. input)]
      (cond-> {:protocol (.getProtocol url)
               :host (.getHost url)
               :query-string (.getQuery url)
               :fragment (.getRef url)}
        (not (= -1 (.getPort url)))       (assoc :port (.getPort url))
        (not (str/blank? (.getPath url))) (assoc :path (.getPath url))))
    (catch MalformedURLException _ nil)))
