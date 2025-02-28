(ns atproto.handle
  "Cross platform handle resolution for the atproto client.

  Handles are human-friendly but less-permanent identifiers for atproto accounts.

  See https://atproto.com/specs/handle."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [atproto.interceptor :as i]
            [atproto.dns :as dns]
            [atproto.json :as json]
            [atproto.http :as http]))

;; From https://github.com/bluesky-social/atproto/blob/main/packages/syntax/src/handle.ts
(def regex
  #"^([a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?$")

(s/def ::handle
  (s/and string?
         #(re-find regex %)
         (s/conformer str/lower-case)))

(defn conform
  "Conform the atproto handle.

  Return :atproto.handle/invalid if the handle is invalid."
  [handle]
  (let [conformed-handle (s/conform ::handle handle)]
    (if (= conformed-handle ::s/invalid)
      ::invalid
      conformed-handle)))

(defn valid?
  "Whether the string is a valid atproto handle."
  [s]
  (not (= ::invalid (conform s))))

(def resolve-dns-interceptor
  {::i/name ::resolve-dns
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [{:keys [handle]} request
                     hostname (str "_atproto." handle)]
                 (if (< 253 (count hostname))
                   (assoc ctx ::i/response {:error "Handle Too Long"})
                   (assoc ctx ::i/request {:hostname hostname :type "txt"}))))
   ::i/leave (fn [{:keys [::i/response] :as ctx}]
               (let [{:keys [error values]} response]
                 (if error
                   ctx
                   (let [dids (->> values
                                   (map #(some->> %
                                                  (re-matches #"^did=(.+)$")
                                                  (second)))
                                   (remove nil?)
                                   (seq))]
                     (assoc ctx
                            ::i/response
                            (cond
                              (empty? dids) {:error "DID not found."}
                              (< 1 (count dids)) {:error "Too many DIDs found." :dids dids}
                              :else {:did (first dids)}))))))})

(defn resolve-with-dns
  "Resolve the handle and return its DID using the DNS method."
  [handle & {:as opts}]
  (i/execute {::i/queue [resolve-dns-interceptor
                         dns/impl-interceptor]
              ::i/request {:handle handle}}
             opts))

(def resolve-https-interceptor
  "Resolve a handle using the https method."
  {::i/name ::resolve-https
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [{:keys [handle]} request]
                 (assoc ctx ::i/request {:method :get
                                         :timeout 3000
                                         :url (str "https://" handle "/.well-known/atproto-did")})))
   ::i/leave (fn [{:keys [::i/response] :as ctx}]
               (let [{:keys [error status body]} response]
                 (cond
                   error ctx
                   (http/success? status) (assoc ctx ::i/response {:did (str/trim body)})
                   :else (assoc ctx ::i/response (http/error-map response)))))})

(defn resolve-with-https
  "Resolve the handle and return its DID using the HTTPS method."
  [handle & {:as opts}]
  (i/execute {::i/queue [resolve-https-interceptor
                         http/impl-interceptor]
              ::i/request {:handle handle}}
             opts))

;; todo: parallelize
(defn resolve
  "Resolve a handle and return its DID, or nil."
  [handle & {:as opts}]
  (let [[cb val] (i/platform-async opts)]
    (resolve-with-dns handle
                      :callback (fn [{:keys [error] :as resp}]
                                  (if error
                                    (resolve-with-https handle :callback cb)
                                    (cb resp))))
    val))
