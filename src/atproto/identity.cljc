(ns atproto.identity
  "Cross platform identity resolver for atproto."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [atproto.interceptor :as i]
            [atproto.did :as did]
            [atproto.handle :as handle]))

(defn resolve
  "Take a handle or DID and return a resolved identity, or nil.

  A resolved identity is a map with the following keys:
  :did The identity's DID
  :pds The URL of this DID's atproto personal data server(PDS)."
  [input & {:as opts}]
  (if (did/valid? input)
    (did/resolve input opts)
    (if (handle/valid? input)
      (let [[cb val] (i/platform-async)]
        (handle/resolve
         input
         :callback (fn [{:keys [error did] :as resp}]
                     (if error
                       (cb resp)
                       (did/resolve
                        did
                        :callback (fn [{:keys [error doc] :as resp}]
                                    (if error
                                      (cb resp)
                                      (cb {:did did
                                           :pds (did/doc->pds doc)})))))))
        val))))

(comment

  (require '[atproto.identity :as id] :reload-all)

  (clojure.pprint/pprint
   @(id/resolve "benfle.com"))

  )
