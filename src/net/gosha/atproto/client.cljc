(ns net.gosha.atproto.client
  "Cross-platform API"
  (:require [net.gosha.atproto.interceptor :as i]
            #?@(:cljd [] :default [[clojure.core.async :as a]])
            #?(:clj [net.gosha.atproto.impl.jvm :as jvm])))

(defn- success?
  [code]
  (and (number? code)
    (<= 200 code 299)))

(defn- http-error-map
  "Given an unsuccessful HTTP response, convert to an error map"
  [resp]
  {:error (str "HTTP " (:status resp))
   :http-response resp})

(defn- impl-interceptors
  "Return implementation-specific HTTP & content type interceptors"
  []
  #?(:clj [jvm/json-interceptor, jvm/httpkit-handler]))

(defn- add-auth-header
  [ctx token]
  (assoc-in ctx [::i/request :headers "Authorization"]
    (str "Bearer " token)))

(declare procedure)

(defn- authenticate
  [ctx identifier password auth-atom]
  (procedure (-> ctx
               (assoc :skip-auth true)
               (dissoc ::i/response))
    :com.atproto.server.createSession
    {:identifier identifier :password password}
    :callback
    (fn auth-result [resp]
      (if (:error resp)
        (i/continue (assoc ctx ::i/response resp))
        (do
          (reset! auth-atom resp)
          (i/continue
            (add-auth-header ctx (:accessJwt resp))))))))

(defn- refresh
  [ctx identifier password auth-atom]
  (procedure (-> ctx
               (assoc :skip-auth true)
               (dissoc ::i/response)
               (add-auth-header (:refreshJwt @auth-atom)))
    :com.atproto.server.refreshSession
    :callback
    (fn reauth-result [resp]
      (if (:error resp)
        ;; Try a full reauthenticate if refresh fails
        (authenticate ctx identifier password auth-atom)
        (do
          (reset! auth-atom resp)
          (i/continue
            (add-auth-header ctx (:accessJwt resp))))))))

(defn- password-auth-interceptor
  "Construct an interceptor for password-based authentication"
  [identifier password]
  (let [auth (atom nil)]
    {::i/name ::password-auth
     ::i/enter (fn enter-app-password [ctx]
                 (if (:skip-auth ctx)
                   ctx
                   (if-let [{token :accessJwt} @auth]
                     (add-auth-header ctx token)
                     (authenticate ctx identifier password auth))))
     ::i/leave (fn leave-app-password [{:keys [::i/response] :as ctx}]
                 ctx
                 (if-not (= "ExpiredToken" (-> response :body :error))
                   ctx
                   (refresh ctx identifier password auth)))}))


(def xrpc-interceptor
  "Construct an interceptor that converts XRPC requests to HTTP requests against
  the provided endpoint.

  An XRPC request has a :nsid and either :parameters or :input. :parameters
  indicates an XRPC 'query' (GET request) while `input` indicates a 'procedure'
  (POST request)."
  {::i/name ::xrpc
   ::i/enter (fn xrpc-enter [{:keys [::i/request :endpoint] :as ctx}]
               (let [url (str endpoint "/xrpc/" (name (:nsid request)))
                     req (if (:input request)
                           {:method :post
                            :body (:input request)
                            :headers {"content-type" "application/json"}}
                           {:method :get
                            :query-params (:parameters request)})]
                 (assoc ctx ::i/request (assoc req :url url))))
   ::i/leave (fn xrpc-leave [{:keys [::i/response] :as ctx}]
               (cond
                 (:error response)
                 ctx

                 (success? (:status response))
                 (assoc ctx ::i/response (:body response))

                 (:error (:body response))
                 (assoc ctx ::i/response (:body response))

                 :else
                 (assoc ctx ::i/response (http-error-map response))))})

(defn init
  "Initialize an ATProto session using the given endpoint and options. Valid
   options are:

   - `:identifier` + `:password` - Password-based authentication"
  [endpoint & {:keys [:identifier :password] :as opts}]
  (let [interceptors (concat
                       [xrpc-interceptor]
                       (when identifier
                         [(password-auth-interceptor identifier password)])
                       (impl-interceptors))]
    {:endpoint endpoint
     :interceptors interceptors}))

(defn- exec
  "Given an XRPC request, execute it against the specified session. The
   mechanism for returning results is specified via a :channel, :callback, or
   :promise keyword arg, defaulting to a platform-appropriate type."
  [session request & {:keys [:channel :callback :promise] :as opts}]
  (let [promise #?(:clj (if (empty? opts) (clojure.core/promise) promise)
                   :default promise)
        channel #?(:cljs (if (empty? opts) (a/chan) channel)
                   :default channel)
        cb (cond
             channel #?(:clj #(a/>!! channel (::i/response %))
                        :cljs #(a/go (a/>! channel (::i/response %)))
                        :cljd #(throw (ex-info
                                         "core.async not supported" {})))
             promise #?(:clj #(deliver promise (::i/response %))
                        :default #(throw (ex-info
                                           "JVM promises not supported" {})))
             callback #(callback (::i/response %)))]
    (i/execute (assoc session
                 ::i/queue (:interceptors session)
                 ::i/request request) cb)
    (or channel promise)))

(defn query
  "Query using the provided NSID and parameters. The mechanism for returning
   results is specified via a :channel, :callback, or :promise keyword arg,
   which is returned. Defaults to a platform-appropriate type."
  [session nsid parameters & {:as opts}]
  (exec session {:nsid nsid :parameters parameters} opts))

(defn procedure
  "Execute a procedure using the provided NSID and parameters. The mechanism
   for returning results is specified via a :channel, :callback, or :promise
   keyword arg which is returned. Defaults to a platform-appropriate type."
  [session nsid input & {:as opts}]
  (exec session {:nsid nsid :input input} opts))

;; TODO: Validate by reading Lexicon files, and converting to schema/spec/other
(defn validate
  [nsid parameters-or-input]
  (throw (ex-info "Validation not yet implemented" {})))
