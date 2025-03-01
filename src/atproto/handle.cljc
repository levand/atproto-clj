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

(def resolve-with-dns-interceptor
  {::i/name ::resolve-with-dns
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [{:keys [handle]} request
                     hostname (str "_atproto." handle)]
                 (if (< 253 (count hostname))
                   (assoc ctx ::i/response {:error "Handle Too Long"})
                   (-> ctx
                       (assoc ::i/request {:hostname hostname :type "txt"})
                       (update ::i/queue #(cons dns/interceptor %))))))
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

(def resolve-with-https-interceptor
  "Resolve a handle using the https method."
  {::i/name ::resolve-with-https
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [{:keys [handle]} request]
                 (-> ctx
                     (assoc ::i/request {:method :get
                                         :timeout 3000
                                         :url (str "https://" handle "/.well-known/atproto-did")})
                     (update ::i/queue #(into [json/interceptor
                                               http/interceptor]
                                              %)))))
   ::i/leave (fn [{:keys [::i/response] :as ctx}]
               (let [{:keys [error status body]} response]
                 (cond
                   error ctx
                   (http/success? status) (assoc ctx ::i/response {:did (str/trim body)})
                   :else (assoc ctx ::i/response (http/error-map response)))))})

;; todo: find a way to parallelize
(def resolve-interceptor
  {::i/name ::resolve
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [{:keys [handle]} request
                     conformed-handle (conform handle)]
                 (if (= ::invalid conformed-handle)
                   (assoc ctx ::i/response {:error "Invalid handle." :handle handle})
                   (let [new-ctx {::i/request {:handle conformed-handle}}]
                     (i/execute (assoc new-ctx ::i/queue [resolve-with-dns-interceptor])
                                :callback
                                (fn [{:keys [error did] :as resp}]
                                  (if error
                                    (i/execute (assoc new-ctx ::i/queue [resolve-with-https-interceptor])
                                               :callback #(i/continue (assoc ctx ::i/response %)))
                                    (i/continue (assoc ctx ::i/response resp)))))))))})
