(ns atproto.impl.jvm
  "JVM interceptor implementations"
  (:require [charred.api :as json]
            [atproto.interceptor :as i]
            [org.httpkit.client :as http]))

(defn- json-content-type?
  "Test if a request or response should be interpreted as json"
  [req-or-resp]
  (when-let [ct (:content-type (:headers req-or-resp))]
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

(def httpkit-handler
  "Interceptor to handle HTTP requests using httpkit"
  {::i/name ::http
   ::i/enter (fn enter-httpkit [ctx]
               (let [ctx (update-in ctx [::i/request :headers]
                           normalize-headers)]
                 (http/request (::i/request ctx)
                   (fn [{:keys [error] :as resp}]
                     (if error
                       (i/continue (assoc ctx ::i/response
                                     {:error (.getName (.getClass error))
                                      :message (.getMessage error)
                                      :exception error}))
                       (i/continue (assoc ctx ::i/response resp
                                     ::response resp)))))))})
