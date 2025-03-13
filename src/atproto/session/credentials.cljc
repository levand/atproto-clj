(ns atproto.session.credentials
  "Authentication session with your username and password.

  Use this session for command line tools with your own credentials. If you
  want to connect to your users' accounts, use an oauth session instead."
  (:require [atproto.interceptor :as i]
            [atproto.did :as did]
            [atproto.identity :as identity]
            [atproto.xrpc :as xrpc]
            [atproto.session :as session]
            [atproto.session.unauthenticated :as unauthenticated-session]))

(declare refresh-session auth-interceptor)

(defn credentials-session
  [{:keys [did handle didDoc] :as data}]
  (with-meta
    #::session{:service (did/pds didDoc)
               :did did
               :handle handle
               :authenticated? true
               :refreshable? true}
    {`session/auth-interceptor #(auth-interceptor % (select-keys data [:accessJwt :refreshJwt]))
     `session/refresh-session refresh-session}))

(defn create
  "Authenticate those credentials and return a session (async).

  The identifier can be an atproto handle or did."
  [{:keys [identifier password]} & {:as opts}]
  (let [[cb val] (i/platform-async opts)]
    (unauthenticated-session/create identifier
                                    :callback
                                    (fn [{:keys [error] :as unauth-session}]
                                      (if error
                                        (cb unauth-session)
                                        (xrpc/procedure unauth-session
                                                        {:op :com.atproto.server.createSession
                                                         :params {:identifier (::session/did unauth-session)
                                                                  :password password}}
                                                        :callback
                                                        (fn [{:keys [error] :as data}]
                                                          (if error
                                                            (cb data)
                                                            (cb (credentials-session data))))))))
    val))

(defn refresh-session
  [session cb]
  (xrpc/procedure (assoc session :refresh? true)
                  {:op :com.atproto.server.refreshSession}
                  :callback
                  (fn [{:keys [error] :as data}]
                    (if error
                      (cb data)
                      (cb (credentials-session data))))))

(defn auth-interceptor
  [session {:keys [accessJwt refreshJwt]}]
  {::i/name ::auth-interceptor
   ::i/enter (fn [ctx]
               (assoc-in ctx
                         [::i/request :headers :authorization]
                         (str "Bearer " (if (:refresh? session)
                                          refreshJwt
                                          accessJwt))))
   ::i/leave (fn [ctx]
               ;; todo: auto-refresh
               ctx)})
