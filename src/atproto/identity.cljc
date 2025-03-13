(ns atproto.identity
  "Cross platform identity resolver for atproto."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [atproto.interceptor :as i]
            [atproto.did :as did]
            [atproto.handle :as handle]))

(defn- handle-did-doc
  [{:keys [error handle did doc] :as resp}]
  (if error
    resp
    (if (and handle (not (did/also-known-as? doc handle)))
      (assoc resp :error "DID document does not include the handle.")
      (if-let [pds (did/pds doc)]
        (cond-> {:did did
                 :pds pds}
          handle (assoc :handle handle))
        (assoc resp :error "No PDS URL found in DID document.")))))

(defn- resolve-with-did
  [did cb]
  (did/resolve did :callback #(cb (handle-did-doc %))))

(defn- resolve-with-handle
  [handle cb]
  (handle/resolve handle
                  :callback
                  (fn [{:keys [error did] :as resp}]
                    (if error
                      (cb resp)
                      (did/resolve did
                                   :callback
                                   #(cb (handle-did-doc (assoc % :handle handle))))))))

(defn resolve
  "Take a handle or DID and return a resolved identity.

  A resolved identity is a map with the following keys:
  :did    The identity's DID
  :pds    The URL of this DID's atproto personal data server(PDS).
  :handle The handle, if used to resolve."
  [input cb]
  (cond
    (did/valid? input)    (resolve-with-did input cb)
    (handle/valid? input) (resolve-with-handle input cb)
    :else                 (cb {:error "The input is neither a valid handle nor a valid DID."})))
