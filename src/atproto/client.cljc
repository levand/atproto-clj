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
            [atproto.xrpc :as xrpc]))

(defn client
  "Create a new client for the given session."
  [session]
  {:session session})

(defn procedure
  [client args & {:as opts}]
  (xrpc/procedure (:session client) args opts))

(defn query
  [client args & {:as opts}]
  (xrpc/query (:session client) args opts))
