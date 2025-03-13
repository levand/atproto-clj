(ns atproto.session.oauth
  "OAuth 2 authentication session."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [atproto.interceptor :as i]
            [atproto.json :as json]
            [atproto.http :as http]
            [atproto.crypto :as crypto]
            [atproto.jwt :as jwt]
            [atproto.identity :as identity]
            [atproto.session :as session]))

;; atproto clients and servers must support ES256
(def default-alg "ES256")

;; OAuth session

(declare refresh-session auth-interceptor)

(defn- oauth-session
  [client {:keys [did handle pds tokens dpop-key] :as data}]
  (let [creds (select-keys data [:tokens :dpop-key])]
    (with-meta
      #::session{:service pds
                 :did did
                 :handle handle
                 :authenticated? true
                 :refreshable? true}
      {`session/auth-interceptor (fn [_] (auth-interceptor client creds))
       `session/refresh-session (fn [_] (refresh-session client creds))})))

;; DPoP

(defn- generate-dpop-key []
  (jwt/generate-jwk {:alg default-alg}))

;; host -> nonce
(def nonce-store (atom {}))

(defn- dpop-wrapper
  [{:keys [iss dpop-key]}]
  (fn [{:keys [::i/request] :as ctx}]
    (let [{:keys [server-nonce method url headers]} request
          url-map (http/parse-url url)
          nonce (or server-nonce
                    (get @nonce-store (:host url-map)))
          ath (when-let [auth (:authorization headers)]
                (when (str/starts-with? auth "DPoP ")
                  (crypto/base64url-encode
                   (crypto/sha256
                    (subs auth 5)))))
          proof (jwt/generate dpop-key
                              {:typ "dpop+jwt"
                               :alg default-alg
                               :jwk (jwt/public-jwk dpop-key)}
                              {:iss iss
                               :jti (crypto/generate-nonce 12)
                               :htm (str/upper-case (name method))
                               :htu (http/serialize-url (dissoc url-map :query-params :fragment))
                               :iat (crypto/now)
                               :nonce nonce
                               :ath ath})]
      (-> ctx
          (update ::i/request assoc-in [:headers :dpop] proof)
          (assoc ::original-ctx ctx)
          (assoc ::origin (:host url-map))
          (assoc ::nonce nonce)))))

(defn- use-dpop-nonce-error?
  [{:keys [status headers body]}]
  (case status
    ;; resource server returns a 401 w/ WWW-Authenticate header
    401 (let [{:keys [www-authenticate]} headers]
          (and (str/starts-with? www-authenticate "DPoP")
               (str/includes? www-authenticate "error=\"use_dpop_nonce\"")))
    ;; authorization server returns a 400 w/ code in body
    400 (= "use_dpop_nonce" (:error body))
    false))

(defn-  dpop-interceptor
  [{:keys [iss dpop-key] :as opts}]
  (let [wrap-dpop (dpop-wrapper opts)]
    {::i/name ::dpop-interceptor
     ::i/enter wrap-dpop
     ::i/leave (fn [{:keys [::i/response ::original-ctx ::origin ::nonce] :as ctx}]
                 (let [{:keys [status headers body]} response]
                   ;; store the nonce for this origin if new
                   (when-let [next-nonce (:dpop-nonce headers)]
                     (when (not (= next-nonce nonce))
                       (swap! nonce-store assoc origin next-nonce)))
                   ;; retry if server asks to use their nonce
                   (if (and (use-dpop-nonce-error? response)
                            (not (:server-nonce (::i/request original-ctx))))
                     (i/continue (wrap-dpop (assoc-in original-ctx [::i/request :server-nonce] (:dpop-nonce headers))))
                     (dissoc ctx ::original-ctx ::origin ::nonce))))}))

;; OAuth client

;; todo:
;; - ensure we have a ES256-compatible private key in the keyset
;; - make session store pluggable
(defn client
  "Initialize the OAuth client."
  [{:keys [client-metadata keys] :as opts}]
  (let [jwks (jwt/parse-jwks
              (json/write-str
               {:keys (->> keys
                           (filter string?)
                           (map json/read-str))}))]
    {:client-metadata (assoc client-metadata
                             ;; only add the public keys to the client metadata
                             :jwks (json/read-str (str (jwt/public-jwks jwks))))
     ;; private keys
     :jwks jwks
     ;; map iss -> authorization-server-metadata
     :issuers (atom {})
     ;; short-term state store for oauth authorization
     :state-store (atom {})
     ;; longer-term oauth session store
     :sessions (atom {})}))

(defn set-issuer!
  [client iss metadata]
  (swap! (:issuers client) assoc iss metadata))

(defn get-issuer
  [client iss]
  (get @(:issuers client) iss))

(defn get-state
  [client state-key]
  (get @(:state-store client) state-key))

(defn set-state!
  [client state-key state]
  (swap! (:state-store client) assoc state-key state))

(defn delete-state!
  [client state-key]
  (swap! (:state-store client) dissoc state-key))

(defn set-session!
  [client did session]
  (swap! (:sessions client) assoc did session))

(defn get-session
  [client did]
  (get @(:sessions client) did))

;; Resolve identity

(defn- fetch-rsmd
  "Fetch the resource server metadata."
  [server cb]
  (i/execute {::i/request {:method :get
                           :url (str server "/.well-known/oauth-protected-resource")}
              ::i/queue [json/interceptor http/interceptor]}
             :callback (fn [{:keys [error body] :as resp}]
                         (cb (if error resp body)))))

(defn- fetch-asmd
  "Fetch the authorization server metadata."
  [server cb]
  (i/execute {::i/request {:method :get
                           :url (str server "/.well-known/oauth-authorization-server")}
              ::i/queue [json/interceptor http/interceptor]}
             :callback (fn [{:keys [error body] :as resp}]
                         (cb (if error resp body)))))

(defn- handle-asmd
  [client {:keys [rsmd asmd identity] :as ctx} cb]
  (let [{:keys [protected_resources issuer]} asmd]
    (if (and (seq protected_resources)
             (not (contains? (set protected_resources) (:resource rsmd))))
      (cb (assoc ctx :error "PDS not protected by issuer."))
      (do
        (set-issuer! client issuer asmd)
        (cb {:identity identity
             :iss issuer})))))

(defn- handle-rsmd
  [client {:keys [rsmd] :as ctx} cb]
  (let [{:keys [authorization_servers]} rsmd]
    (cond
      (= 0 (count authorization_servers)) (cb (assoc ctx :error "No authorization server found."))
      (< 1 (count authorization_servers)) (cb (assoc ctx :error "Too many authorization servers found."))
      :else  (fetch-asmd (first authorization_servers)
                         (fn [{:keys [error] :as resp}]
                           (if error
                             (cb (merge ctx resp))
                             (handle-asmd client (assoc ctx :asmd resp) cb)))))))

(defn- handle-identity
  [client {:keys [identity] :as ctx} cb]
  (fetch-rsmd (:pds identity)
              (fn [{:keys [error] :as resp}]
                (if error
                  (cb (merge ctx resp))
                  (handle-rsmd client (assoc ctx :rsmd resp) cb)))))

;; todo: options map with :no-cache? :allow-stale?
(defn- resolve
  "Resolve the identity and return it with the issuer's URL.

  Return a map with:
  :identity  The verified identity
  :iss       The issuer's URL."
  [client input cb]
  (identity/resolve input
                    (fn [{:keys [error] :as resp}]
                      (if error
                        (cb resp)
                        (handle-identity client {:identity resp} cb)))))

;; Pushed Authorization Request

(defmulti client-auth
  "The authentication payload for this client and issuer."
  (fn [client issuer]
    (let [client-method (get-in client [:client-metadata :token_endpoint_auth_method] "none")]
      (when (contains? (set (:token_endpoint_auth_methods_supported issuer))
                       client-method)
        client-method))))

(defmethod client-auth "none"
  [client _]
  {:client_id (:client_id (:client-metadata client))})

(defmethod client-auth "private_key_jwt"
  [{:keys [jwks] :as client} issuer]
  (let [jwk (first (jwt/query-jwks jwks {:alg default-alg}))
        {:keys [client_id]} (:client-metadata client)]
    {:client_id client_id
     :client_assertion_type "urn:ietf:params:oauth:client-assertion-type:jwt-bearer"
     :client_assertion (jwt/generate jwk
                                     {:alg default-alg
                                      :kid (jwt/jwk-kid jwk)}
                                     {:iss client_id
                                      :sub client_id
                                      :aud (:issuer issuer)
                                      :jti (crypto/generate-nonce 16)
                                      :iat (crypto/now)})}))

(defn par-interceptor
  "Interceptor for this client to send the push authorization request (PAR) to this issuer."
  [{:keys [client-metadata] :as client}
   {:keys [issuer dpop-key] :as server}]
  (let [{:keys [client_id redirect_uris scope]} client-metadata
        {:keys [pushed_authorization_request_endpoint authorization_endpoint]} issuer]
    {::i/name ::par
     ::i/enter (fn [{:keys [::i/request] :as ctx}]
                 (update ctx
                         ::i/request
                         (fn [{:keys [opts identity input]}]
                           (let [pkce (crypto/generate-pkce 63)
                                 state (crypto/generate-nonce 16)
                                 oauth-params (cond-> {:client_id client_id
                                                       :redirect_uri (or (:redirect-uri opts)
                                                                         (first redirect_uris))
                                                       :code_challenge (:challenge pkce)
                                                       :code_challenge_method (:method pkce)
                                                       :state state
                                                       :response_mode "query"
                                                       :response_type "code"
                                                       :scope (or (:scope opts) scope)}
                                                identity (assoc :login_hint input))]
                             (set-state! client state {:iss (:issuer issuer)
                                                       :dpop-key dpop-key
                                                       :identity identity
                                                       :verifier (:verifier pkce)
                                                       :app-state (:state opts)})
                             {:method :post
                              :url pushed_authorization_request_endpoint
                              :body (merge oauth-params
                                           (client-auth client issuer))}))))

     ::i/leave (fn [ctx]
                 (update ctx
                         ::i/response
                         (fn [{:keys [error status body] :as response}]
                           (cond
                             error
                             response

                             (not (http/success? status))
                             (http/error-map response)

                             :else
                             {:authorization-url (-> authorization_endpoint
                                                     http/parse-url
                                                     (update :query-params
                                                             merge
                                                             {:client_id client_id
                                                              :request_uri (:request_uri body)})
                                                     http/serialize-url)}))))}))

(defn authorize
  "Generate the end-user authorization URL.

  Accept the following options:
  :scope          OAuth scope
  :redirect-uri   OAuth callback redirect URI
  :state          app-specific state that will be returned in `callback`."
  [client input & {:as opts}]
  (let [[cb val] (i/platform-async opts)
        {:keys [client_id]} (:client-metadata client)]
    (resolve client
             input
             (fn [{:keys [error identity iss] :as resp}]
               (if error
                 (cb resp)
                 (let [server {:issuer (get-issuer client iss)
                               :dpop-key (generate-dpop-key)}]
                   (if (not (:require_pushed_authorization_requests (:issuer server)))
                     (cb {:error "Server does not support PAR."})
                     (i/execute {::i/request {:opts opts
                                              :identity identity
                                              :input input
                                              :issuer iss}
                                 ::i/queue [(par-interceptor client server)
                                            (dpop-interceptor {:iss client_id
                                                               :dpop-key (:dpop-key server)})
                                            json/interceptor
                                            http/interceptor]}
                                :callback cb))))))
    val))

(defn exchange-code
  "Exchange an authorization code for a set of tokens."
  [client server {:keys [code verifier]} cb]
  (let [{:keys [client_id redirect_uris]} (:client-metadata client)
        {:keys [issuer dpop-key]} server
        {:keys [token_endpoint]} issuer]
    (i/execute {::i/request {:method :post
                             :url token_endpoint
                             :body (merge {:grant_type "authorization_code"
                                           :redirect_uri (first redirect_uris)
                                           :code code
                                           :code_verifier verifier}
                                          (client-auth client issuer))
                             :dpop-key dpop-key}
                ::i/queue [(dpop-interceptor {:iss client_id
                                              :dpop-key dpop-key})
                           json/interceptor
                           http/interceptor]}
               :callback
               (fn [{:keys [error status body] :as http-response}]
                 (cond
                   error
                   (cb http-response)

                   (:error body)
                   (cb body)

                   :else
                   ;; The token response must be valid before the 'sub' it contains can be trusted
                   (resolve client
                            (:sub body)
                            (fn [{:keys [error iss] :as resp}]
                              (cb (cond
                                    error resp
                                    (not (= iss (:issuer issuer))) {:error "Issuer mismatch."}
                                    :else (assoc body :aud (-> resp :identity :pds)))))))))))

(defn- validate-callback-params
  "Validate the callback params.

  Return a map with:
  :state    The local state stored for this session.
  :issuer   The issuer for this request.
  :error    In case of an error."
  [client {:keys [response iss state error code] :as params}]
  (if-let [saved-state (get-state client state)]
    (do
      (delete-state! client state)
      (if error
        {:error error :params params :state saved-state}
        (if (not (= iss (:iss saved-state)))
          {:error "Issuer mismatch." :params params :state saved-state}
          {:state saved-state
           :issuer (get-issuer client iss)})))
    {:error "Unknown state" :state state}))

(defn callback
  "Exchange the authorization code for OAuth tokens and return a OAuth session.

  Return a map with:
  :session  The OAuth session.
  :state    The app state passed in authorize, if any."
  [client params & {:as opts}]
  (let [[cb val] (i/platform-async opts)
        {:keys [error state issuer] :as resp} (validate-callback-params client params)]
    (if error
      (cb resp)
      (let [{:keys [verifier dpop-key app-state identity]} state
            server {:issuer issuer
                    :dpop-key dpop-key}]
        (exchange-code client
                       server
                       {:code (:code params)
                        :verifier verifier}
                       (fn [{:keys [error] :as resp}]
                         (if error
                           (cb resp)
                           (let [did (:sub resp)
                                 session (merge identity
                                                {:did did
                                                 :tokens resp
                                                 :dpop-key dpop-key})]
                             (set-session! client did session)
                             (cb (cond-> {:session (oauth-session client session)}
                                   app-state (assoc :state app-state)))))))))
    val))

(defn restore
  "The OAuth session for this did."
  [client did & {:as opts}]
  (let [data (get-session client did)]
    (oauth-session client data)))

;; todo
(defn refresh-session
  [client creds]
  'todo)

(defn auth-interceptor
  [client creds]
  (let [{:keys [client_id]} (:client-metadata client)
        {:keys [tokens dpop-key]} creds
        {:keys [token_type access_token]} tokens]
    {::i/name ::auth-interceptor
     ::i/enter (fn [ctx]
                 (-> ctx
                     (assoc-in [::i/request :headers :authorization]
                               (str token_type " " access_token))
                     (update ::i/queue #(cons (dpop-interceptor {:iss client_id
                                                                 :dpop-key dpop-key})
                                              %))))
     ::i/leave (fn [ctx]
                 ;; todo: auto-refresh
                 ctx)}))
