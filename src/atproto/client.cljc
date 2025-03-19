(ns atproto.client
  "Cross-platform API.

  All functions are async, and follow the same pattern for specifying how
  results are returned. Each function takes keyword argument options, and
  callers can specify a :channel, :callback, or :promise which will recieve
  the results.

  Not all mechanisms are supported on all platforms. If no result mechanism is
  specified, a platform-appropriate deferred value (e.g. promise or core.async
  channel will be returned.)"
  (:require [clojure.string :as str]
            [atproto.interceptor :as i]
            [atproto.identity :as identity]
            [atproto.xrpc :as xrpc]
            [atproto.session :as session]
            [atproto.lexicon :as lexicon]))

(defn client
  "Create a new client for the config map.

  Supported keys:
  :session    Session (see atproto.session)
  :validate?  Whether to validate the arguments to query and procedure."
  [{:keys [session validate?]}]
  {:session session
   :validate? validate?})

(defn did
  "The did of the authenticated user, or nil."
  [client]
  (let [{:keys [session]} client]
    (and (::session/authenticated? session)
         (::session/did session))))

(defn invoke
  [client args & {:as opts}]
  (let [[cb val] (i/platform-async opts)]
    (if (and (:validate? client)
             (not (lexicon/valid-call? args)))
      (cb (lexicon/invalid-call args))
      (if (lexicon/procedure? args)
        (xrpc/procedure (:session client) args :callback cb)
        (xrpc/query (:session client) args :callback cb)))
    val))

(defn procedure
  [client args & {:as opts}]
  (invoke client args opts))

(defn query
  [client args & {:as opts}]
  (invoke client args opts))
