(ns atproto.identity
  "Cross platform identity resolver for atproto."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [atproto.interceptor :as i]
            [atproto.did :as did]
            [atproto.handle :as handle]))

(def resolve-with-did-interceptor
  {::i/name ::resolve-with-did
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [{:keys [did]} request]
                 (if (did/valid? did)
                   (-> ctx
                       (assoc ::did did)
                       (update ::i/queue #(cons did/resolve-interceptor %)))
                   (assoc ctx ::i/response {:error "Invalid DID."}))))
   ::i/leave (fn [{:keys [::did ::i/response] :as ctx}]
               (let [{:keys [error doc]} response]
                 (if error
                   ctx
                   (assoc ctx
                          ::i/response
                          (if-let [pds (did/pds doc)]
                            {:did did
                             :pds pds}
                            {:error "No PDS URL found in DID document."
                             :did did
                             :doc doc})))))})

(def resolve-with-handle-interceptor
  {::i/name ::resolve-with-handle
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [{:keys [handle]} request
                     conformed-handle (handle/conform handle)]
                 (if (= ::handle/invalid conformed-handle)
                   (assoc ctx ::i/response {:error "Invalid handle."})
                   (handle/resolve conformed-handle
                                   :callback
                                   (fn [{:keys [error did] :as resp}]
                                     (i/continue
                                      (if error
                                        (assoc ctx ::i/response resp)
                                        (-> ctx
                                            (assoc ::i/request {:did did})
                                            (update ::i/queue #(cons resolve-with-did-interceptor %))))))))))
   ::i/leave (fn [{:keys [::i/response] :as ctx}]
               (let [{:keys [error did doc pds]} response]
                 (if error
                   ctx
                   (if (did/also-known-as? doc (::conformed-handle ctx))
                     ctx
                     (assoc-in ctx
                               [::i/response :error]
                               "DID document does not include the handle.")))))})

(def resolve-interceptor
  {::i/name ::resolve
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [{:keys [input]} request]
                 (cond
                   (did/valid? input)
                   (-> ctx
                       (assoc ::i/request {:did input})
                       (update ::i/queue #(cons resolve-with-did-interceptor %)))

                   (handle/valid? input)
                   (-> ctx
                       (assoc ::i/request {:handle input})
                       (update ::i/queue #(cons resolve-with-handle-interceptor %)))

                   :else
                   (assoc ctx ::i/response {:error "The input is neither a valid handle nor a valid DID."}))))})

(defn resolve
  "Take a handle or DID and return a resolved identity.

  A resolved identity is a map with the following keys:
  :did The identity's DID
  :pds The URL of this DID's atproto personal data server(PDS)."
  [input & {:as opts}]
  (i/execute {::i/queue [resolve-interceptor]
              ::i/request {:input input}}
             opts))

(comment

  (require '[atproto.identity :as id] :reload-all)

  @(id/resolve "did:plc:5yamojdko6zzesl4luisgkzg")
  @(id/resolve "benfle.com")
  @(id/resolve "jay.bsky.team")
  @(id/resolve "jessitron.bsky.social")

  )
