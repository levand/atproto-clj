(ns atproto.identity
  "Cross platform identity resolver for atproto."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [atproto.interceptor :as i]
            [atproto.did :as did]
            [atproto.handle :as handle]))

(defn- handle-did-doc-response
  [{:keys [error doc] :as resp} did cb]
  (if error
    (cb resp)
    (cb {:did did
         :pds (did/doc->pds doc)})))

(defn resolve
  "Take a handle or DID and return a resolved identity, or nil.

  A resolved identity is a map with the following keys:
  :did The identity's DID
  :pds The URL of this DID's atproto personal data server(PDS)."
  [input & {:as opts}]
  (let [[cb val] (i/platform-async opts)]
    (if (did/valid? input)
      (did/resolve input :callback #(handle-did-doc-response % input cb))
      (if (handle/valid? input)
        (handle/resolve input
                        :callback
                        (fn [{:keys [error did] :as resp}]
                          (if error
                            (cb resp)
                            (did/resolve did :callback #(handle-did-doc-response % did cb)))))
        (cb {:error "Input is neither a valid DID not handle."
             :input input})))
    val))

(comment

  (require '[atproto.identity :as id] :reload-all)

  @(id/resolve "benfle.com")
  @(id/resolve "jay.bsky.team")
  @(id/resolve "jessitron.bsky.social")

  )
