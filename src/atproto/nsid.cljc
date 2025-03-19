(ns atproto.nsid
  "Namespaced Identifiers (NSIDs) are used to reference Lexicon schemas for records, XRPC endpoints, and more.

  See https://atproto.com/specs/nsid"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [atproto.interceptor :as i]
            [atproto.dns :as dns]
            [atproto.did :as did]))

(defn parse
  "Parse the input string into a map with :domain-authority and :name."
  [nsid]
  (when-let [sep (str/last-index-of nsid \.)]
    {:authority (subs nsid 0 sep)
     :name (subs nsid sep)}))
