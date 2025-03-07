(ns atproto.oauth
  "Cross-platform OAuth client."
  (:require [atproto.interceptor :as i]
            [atproto.identity :as identity]
            [atproto.json :as json]
            [atproto.http :as http]))

;; OAuth resolver

(defn- fetch-rsmd
  "Fetch the resource server metadata."
  [server cb]
  (i/execute {::i/queue [json/interceptor http/interceptor]
              ::i/request {:method :get
                           :url (str server "/.well-known/oauth-protected-resource")}}
             :callback (fn [{:keys [error body] :as resp}]
                         (if error
                           (cb resp)
                           (cb body)))))

(defn- fetch-asmd
  "Fetch the authorization server metadata."
  [server cb]
  (i/execute {::i/queue [json/interceptor http/interceptor]
              ::i/request {:method :get
                           :url (str server "/.well-known/oauth-authorization-server")}}
             :callback (fn [{:keys [error body] :as resp}]
                         (if error
                           (cb resp)
                           (cb body)))))

(defn- handle-asmd
  [{:keys [rsmd asmd identity] :as ctx} cb]
  (let [{:keys [protected_resources]} asmd]
    (if (and (seq protected_resources)
             (not (contains? (set protected_resources) (:resource rsmd))))
      (cb (assoc ctx :error "PDS not protected by issuer."))
      (cb {:identity identity
           :metadata asmd}))))

(defn- handle-rsmd
  [{:keys [rsmd] :as ctx} cb]
  (let [{:keys [authorization_servers]} rsmd]
    (cond
      (= 0 (count authorization_servers)) (cb (assoc ctx :error "No authorization server found."))
      (< 1 (count authorization_servers)) (cb (assoc ctx :error "Too many authorization servers found."))
      :else (fetch-asmd (first authorization_servers)
                        (fn [{:keys [error] :as resp}]
                          (if error
                            (cb (assoc ctx :error error))
                            (handle-asmd (assoc ctx :asmd resp) cb)))))))

(defn- handle-identity
  [{:keys [identity] :as ctx} cb]
  (let [{:keys [pds]} identity]
    (fetch-rsmd pds
                (fn [{:keys [error] :as resp}]
                  (if error
                    (cb (assoc ctx :error error))
                    (handle-rsmd (assoc ctx :rsmd resp) cb))))))

(defn- resolve-from-identity
  [input cb]
  (identity/resolve input
                    :callback (fn [{:keys [error] :as resp}]
                                (if error
                                  (cb error)
                                  (handle-identity {:identity resp} cb)))))

;; OAuth Params builder

(defn- random-b64
  "Random base64 string of the given size."
  [size]
  (let [seed (byte-array size)]
    (.nextBytes (SecureRandom.) seed)
    (.encodeToString (Base64/getEncoder) seed)))

(defn- b64-url
  "Encode the base64 string to be included in a URL."
  [s]
  (-> s (str/replace "+" "-") (str/replace "/" "_")))

(defn- random-verifier
  []
  (b64-url (random-b64 63)))

;; TODO: Make platform specific
(defn code-challenge
  [verifier]
  (let [s (->> (.getBytes verifier StandardCharsets/UTF_8)
               (.digest (MessageDigest/getInstance "SHA-256"))
               (.encodeToString (Base64/getEncoder)))]
    {:code_challenge (-> s b64-url (str/replace "=" ""))
     :code_challenge_method "S256"}))

(defn build-oauth-authorization-request-params
  [client-metadata]
  (let [verifier (random-verifier)]
    (merge (code-challenge verifier)
           {:client_id (:client_id client_metadata)
            :redirect_uri (first (:redirect_uris client_metadata))
            :scope (:scope client_metadata)
            :response_type "code"})))

(defn- push-par
  "Issue the pushed authorization request to the server.

  Throws if the server does not support it."
  [{:keys [asmd oauth-params]} cb]
  (if (not (:require_pushed_authorization_requests asmd))
    (cb {:error "Server does not support PAR."})
    (let [url-str (:pushed_authorization_request_endpoint asmd)]
      (if (not url-str)
        (cb {:error "Server missing PAR endpoint."})
        (let [url (java.net.URL. url-str)]
          (i/execute {::i/request {:method :post
                                   :url url
                                   :headers {"Content-Type" "TODO"}
                                   :body 'todo}
                      ::i/queue [http/interceptor]}
                     :callback
                     (fn [{:keys [error status body] :as resp}]
                       (if error
                         (cb error)
                         ))))))))

(defn- handle-identity-and-metadata
  [{:keys [identity metadata] :as resp} cb]
  (let [oauth-params {:client_id }
        authorization-url (java.net.URL. (:authorization_endpoint metadata))]
    (cb resp)))

;; todo: accept PDS URL or Entryway URL as input
(defn authorize
  [input & {:as opts}]
  (let [[cb val] (i/platform-async opts)]
    (resolve-from-identity input
                           (fn [{:keys [error] :as ctx}]
                             (if error
                               (cb error)
                               (handle-identity-and-metadata ctx cb))))
    val))
