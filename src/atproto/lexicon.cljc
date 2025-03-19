(ns atproto.lexicon
  "Lexicon is a schema definition language used to describe atproto records,
  HTTP endpoints (XRPC), and event stream messages.

  See https://atproto.com/specs/lexicon"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [atproto.did :as did]
            [atproto.nsid :as nsid]
            [atproto.interceptor :as i]
            [atproto.dns :as dns]
            [atproto.xrpc :as xrpc]
            [atproto.json :as json]
            [atproto.lexicon.schema :as schema]
            [atproto.lexicon.translator :refer [lex-uri->spec-key translate]]))

;; TODO:
;; - caching of DNS/lexicon

(def lexicon-record-collection "com.atproto.lexicon.schema")

(def loaded-schemas (atom {}))

(defn loaded-schema
  "The loaded schema with the given NSID."
  [nsid]
  (get @loaded-schemas nsid))

(defn load-schema!
  [schema]
  (if (not (s/valid? ::schema/file schema))
    (throw (ex-info "Invalid Lexicon schema."
                    (s/explain-data ::schema/file schema)))
    (let [spec-defs (translate schema)]
      (eval `(do ~spec-defs))
      (swap! loaded-schemas assoc (:id schema) schema)
      :done)))

(defn load-dir!
  "Load all the Lexicon schemas from the directory."
  [dir]
  (->> dir
       (file-seq)
       (filter #(str/ends-with? (.getName %) ".json"))
       (map #(json/read-str (slurp %)))
       (map load-schema!)
       (doall))
  :done)

;; Docuemntation

(defn procedure?
  [args]
  (when-let [nsid (name (:op args))]
    (if (not (loaded-schema nsid))
      (throw (ex-info (str "Unknown schema: " nsid)))
      (= "procedure" (get-in @loaded-schemas [nsid :defs :main :type])))))

;; Validation

(defn valid-call?
  "Whether this is a valid procedure or subscription call."
  [args]
  (when-let [nsid (name (:op args))]
    (if (not (loaded-schema nsid))
      (throw (ex-info (str "Unknown schema: " nsid) args))
      (s/valid? (lex-uri->spec-key nsid) args))))

(defn invalid-call
  "The error to return in case of an invalid call."
  [args]
  (let [nsid (name (:op args))]
    (if (not (loaded-schema nsid))
      {:error (str "Missing schema for " nsid)})
    (merge {:error "Invalid call."}
           (s/explain-data (lex-uri->spec-key nsid) args))))

(defn valid?
  "Whether the atproto data is valid.

  Assume that the schemas have been loaded."
  [lex-uri data]
  (s/valid? (lex-uri->spec-key lex-uri)
            data))

;; Lexicon resolution

(defn- fetch-lexicon
  [{:keys [did pds rkey]} cb]
  (xrpc/query {:atproto.session/service pds}
              {:op :com.atproto.repo.getRecord
               :params {:repo did
                        :collection lexicon-record-collection
                        :rkey rkey}}
              :callback
              (fn [{:keys [error] :as resp}]
                (tap> resp)
                (if error (cb resp) (cb (:value resp))))))

(defn- nsid->did
  "Resolve this NSID to a DID using DNS."
  [nsid cb]
  (let [{:keys [domain-authority]} (nsid/parse nsid)
        hostname (->> (str/split domain-authority #"\.")
                      (reverse)
                      (into ["_lexicon"])
                      (str/join "."))]
    (println hostname)
    (if (< 253 (count hostname))
      (cb {:error (str "Cannot resolve nsid, hostname too long: " hostname)})
      (i/execute {::i/request {:hostname hostname
                               :type "txt"}
                  ::i/queue [dns/interceptor]}
                 :callback
                 (fn [{:keys [error values] :as resp}]
                   (if error
                     (cb resp)
                     (let [dids (->> values
                                     (map #(some->> %
                                                    (re-matches #"^did=(.+)$")
                                                    (second)))
                                     (remove nil?)
                                     (seq))]
                       (cond
                         (empty? dids)      (cb {:error "Cannot resolve NSID. DID not found."})
                         :else              (cb {:did (first dids)})))))))))

(defn resolve-nsid
  "Resolve the NSID into a Lexicon."
  [nsid & {:as opts}]
  (let [[cb val] (i/platform-async opts)]
    (nsid->did nsid
               (fn [{:keys [error did] :as resp}]
                 (if error
                   (cb resp)
                   (did/resolve did
                                :callback
                                (fn [{:keys [error did doc] :as resp}]
                                  (if error
                                    (cb resp)
                                    (if-let [pds (did/pds doc)]
                                      (fetch-lexicon {:did did
                                                      :pds pds
                                                      :rkey nsid} cb)
                                      (cb {:error "DID doc is missing the PDS url."
                                           :nsid nsid
                                           :did did
                                           :doc doc}))))))))
    val))
