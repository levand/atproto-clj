(ns atproto.did
  "Cross platform DID resolver.

  Decentralized identifiers (DIDs) are persistent, long-term identifiers for atproto accounts.

  DID = \"did:<method>:<method-specific-identifier>\"

  DIDs resolve to DID documents that contain information associated with the DID.

  See https://atproto.com/specs/did."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [atproto.interceptor :as i]
            [atproto.http :as http]
            [atproto.json :as json])
  (:import [java.net URL MalformedURLException]))

(defn- char-in
  "Whether the character is between start and end, both inclusive."
  [c start end]
  (<= (int start) (int c) (int end)))

(defn- digit? [c] (char-in c \0 \9))
(defn- hex-char? [c] (or (digit? c) (char-in c \A \F)))
(defn- method-char? [c] (or (digit? c) (char-in c \a \z)))

(defn- method?
  "Whether the string is a well-formed method name.

  See https://www.w3.org/TR/did-1.0/#did-syntax"
  [s]
  (and (not (str/blank? s))
       (every? method-char? s)))

(defn msid?
  "Whether the string is a well-formed method-specific identifier.

  See https://www.w3.org/TR/did-1.0/#did-syntax"
  [msid]
  (loop [[c & xs] (seq msid)
         i 0]
    (cond
      ;; EOS -> success!
      (not c)
      true

      ;; regular character
      (or (char-in c \a \z)
          (char-in c \A \Z)
          (char-in c \0 \9)
          (#{\. \- \_} c))
      (recur xs (inc i))

      ;; colon is acceptable if not at the end
      (and (= \: c)
           (not (= (inc i) (count msid))))
      (recur xs (inc i))

      ;; percent-encoded char
      (and (= \% c)
           (first xs) (hex-char? (first xs))
           (second xs) (hex-char? (second xs)))
      (recur (drop 2 xs) (+ i 3))

      ;; Otherwise invalid
      :else false)))

(defn- parse-string-did
  "Parse a DID string into a map with :scheme, :method, and :msid keys.

  Return nil if the DID strign is not well formed."
  [s]
  (when-let [first-colon-idx (str/index-of s \:)]
    (when-let [second-colon-idx (str/index-of s \: (inc first-colon-idx))]
      {:scheme (subs s 0 first-colon-idx)
       :method (subs s (inc first-colon-idx) second-colon-idx)
       :msid (subs s (inc second-colon-idx))})))

(s/def ::did
  (s/and string?
         (s/conformer #(or (parse-string-did %) ::s/invalid))
         (s/keys :req-un [::scheme ::method ::msid])))

(s/def ::scheme #{"did"})
(s/def ::method (s/and string? method?))
(s/def ::msid (s/and string? msid?))

(defmulti method-spec :method)

(s/def ::at-proto-did
  (s/and ::did
         (s/multi-spec method-spec :method)))

(defn conform
  "Conform the atproto DID string.

  Return a map with 3 keys:
  :scheme \"did\"
  :method \"plc\" or \"web\"
  :msid   method-specific identifier

  Return :atproto.did/invalid if the DID is invalid."
  [did]
  (let [conformed-did (s/conform ::at-proto-did did)]
    (if (= conformed-did ::s/invalid)
      ::invalid
      conformed-did)))

(defn valid?
  "Whether the string is a valid atproto DID."
  [s]
  (not (= ::invalid (conform s))))

(defmulti fetch-doc-interceptor
  "The interceptor to fetch the DID document for this method."
  :method)

(def resolve-interceptor
  {::i/name ::resolve
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [conformed-did (conform (:did request))]
                 (if (= ::invalid conformed-did)
                   (assoc ctx ::i/response {:error "Invalid DID."})
                   (let [interceptor (fetch-doc-interceptor conformed-did)]
                     (update ctx ::i/queue #(cons interceptor %))))))
   ::i/leave (fn [{:keys [::i/response] :as ctx}]
               (let [{:keys [doc error]} response]
                 (if error
                   ctx
                   ;; todo: validate doc in the response
                   ctx)))})

;; PLC method

(defn base32-char? [c] (or (char-in c \a \z) (char-in c \2 \7)))

(defmethod method-spec "plc"
  [_]
  (fn [{:keys [msid]}]
    (and (= 24 (count msid))
         (every? base32-char? msid))))

(defmethod fetch-doc-interceptor "plc"
  [_]
  {::i/name ::fetch-doc-plc
   ::i/enter (fn [{:keys [::i/request] :as ctx}]
               (let [{:keys [did]} request]
                 (-> ctx
                     (assoc ::i/request {:method :get
                                         :url (str "https://plc.directory/" did)})
                     (update ::i/queue #(into [json/interceptor
                                               http/interceptor]
                                              %)))))
   ::i/leave (fn [{:keys [::i/response] :as ctx}]
               (let [{:keys [error status body]} response]
                 (cond
                   error ctx
                   (http/success? status) (assoc ctx ::i/response {:doc body})
                   :else (assoc ctx ::i/response (http/error-map response)))))})

;; Web method

(defn- ^URL web-did-msid->url
  "Transform an atproto Web DID msid into a URL."
  [msid]
  (let [hostname (str/replace msid "%3A" ":")
        scheme (if (str/starts-with? hostname "localhost")
                 "http"
                 "https")]
    (try
      (URL. (str scheme "://" hostname "/"))
      (catch MalformedURLException _))))

(defn web-did->url
  "Transform an atproto Web DID into a URL."
  [did]
  (when (valid? did)
    (web-did-msid->url (:msid (conform did)))))

(defn url->web-did
  "Take a DID URL and return the atproto DID web."
  [^URL url]
  (str "did:web:"
       (.getHost url)
       (when (not (= -1 (.getPort url)))
         (str "%3A" (.getPort url)))))

(defmethod method-spec "web"
  [_]
  (fn [{:keys [msid] :as conformed-did}]
    (and
     ;; Ensure we can generate well formed URL from this DID
     (some? (web-did-msid->url msid))
     ;; Atproto does not allow path components in Web DIDs
     (not (str/index-of msid \:))
     ;; Atproto does not allow port numbers in Web DIDs, except for localhost
     (or (str/starts-with? msid "localhost")
         (not (str/index-of msid "%3A"))))))

(defmethod fetch-doc-interceptor "web"
  [_]
  {::i/name ::fetch-doc-plc
   ::i/enter (fn [ctx]
               (assoc ctx ::i/response {:error "Not Implemented"}))
   ::i/leave (fn [{:keys [::i/response] :as ctx}]
               ctx)})

;; DID document

(defn ^URL pds
  "The atproto personal data server declared in this DID document."
  [doc]
  (when-let [url-str (some->> doc
                              :service
                              (filter (fn [service]
                                        (and (str/ends-with? (:id service) "#atproto_pds")
                                             (= "AtprotoPersonalDataServer" (:type service)))))
                              (first)
                              :serviceEndpoint)]
    (URL. url-str)))

(defn handles
  "The atproto handles defined in this document."
  [doc]
  (->> doc
       :alsoKnownAs
       (map #(second (re-matches #"^at://(.+)$" %)))
       (remove nil?)))

(defn also-known-as?
  "Whether the handle is defined in the DID document."
  [doc handle]
  (= handle (first (handles doc))))
