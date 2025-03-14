(ns atproto.session.unauthenticated
  (:require [atproto.interceptor :as i]
            [atproto.handle :as handle]
            [atproto.did :as did]
            [atproto.identity :as identity]
            [atproto.session :as session]))

(defn unauthenticated-session
  [{:keys [service did handle]}]
  (cond-> #::session{:service service
                     :authenticated? false
                     :refreshable? false}
    did (assoc ::session/did did)
    handle (assoc ::session/handle handle)))

(defn create
  "Take a handle, did, or app URL and return an unauthenticated session."
  [input & {:as opts}]
  (let [[cb val] (i/platform-async opts)]
    (if (or (did/valid? input)
            (handle/valid? input))
      (identity/resolve input
                        :callback
                        (fn [{:keys [error pds did handle] :as resp}]
                          (cb (if error resp (unauthenticated-session {:service pds
                                                                       :did did
                                                                       :handle handle})))))
      (cb (unauthenticated-session {:service input})))
    val))
