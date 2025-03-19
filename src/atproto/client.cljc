(ns atproto.client
  "Cross-platform API.

  All functions are async, and follow the same pattern for specifying how
  results are returned. Each function takes keyword argument options, and
  callers can specify a :channel, :callback, or :promise which will receive
  the results.

  Not all mechanisms are supported on all platforms. If no result mechanism is
  specified, a platform-appropriate deferred value (e.g. promise or core.async
  channel will be returned.)"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [atproto.interceptor :as i]
            [atproto.xrpc :as xrpc]
            [atproto.session :as session]
            [atproto.lexicon :as lexicon]
            [atproto.session.unauthenticated :as unauthenticated-session]
            [atproto.session.credentials :as credentials-session]
            [atproto.client.config :as-alias config]))

(s/def ::config
  (s/and
   (s/keys :opt-un [::config/session
                    ::config/credentials
                    ::config/service])
   (fn [{:keys [session credentials service]}]
     (or session credentials service))))

(s/def ::config/session ::session/session)
(s/def ::config/credentials ::credentials-session/credentials)
(s/def ::config/service ::unauthenticated-session/service)

(defn create
  "Create a new atproto client with the given config map.

  Supported keys:
  :session     (optional) The unauthenticated or authenticated session to use.
  :credentials (optional) Convenience to create a Credentials-based session (if no :session).
  :service     (optional) Convenience to create an Unauthenticated session (if no :session).
  :validate?   (optional) Whether to validate the arguments to procedure and query."
  [{:keys [session credentials service validate?] :as config} & {:as opts}]
  (let [client {::validate? validate?}
        [cb val] (i/platform-async opts)]
    (cond
      session
      (cb (assoc client ::session session))

      credentials
      (credentials-session/create credentials
                                  :callback
                                  (fn [{:keys [error] :as session}]
                                    (if error
                                      (cb session)
                                      (cb (assoc client ::session session)))))
      service
      (unauthenticated-session/create service
                                      :callback
                                      (fn [{:keys [error] :as session}]
                                        (if error
                                          (cb session)
                                          (cb (assoc client ::session session))))))
    val))

(defn did
  "The did of the authenticated user, or nil."
  [{:keys [::session]}]
  (and (::session/authenticated? session)
       (::session/did session)))

(defn invoke
  [client args & {:as opts}]
  (let [[cb val] (i/platform-async opts)]
    (if (and (::validate? client)
             (not (lexicon/valid-call? args)))
      (cb (lexicon/invalid-call args))
      (if (lexicon/procedure? args)
        (xrpc/procedure (:session client) args :callback cb)
        (xrpc/query (:session client) args :callback cb)))
    val))

(defn procedure
  "Issue a procedure call with the given arguments."
  [client args & {:as opts}]
  (invoke client args opts))

(defn query
  "Issue a query with the igven arguments."
  [client args & {:as opts}]
  (invoke client args opts))
