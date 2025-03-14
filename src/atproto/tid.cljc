(ns atproto.tid
  "A TID (timestamp identifier) is a compact string identifier based on an integer timestamp.

  See https://atproto.com/specs/tid"
  (:require [clojure.math :refer [floor random]]
            #?(:clj [clojure.pprint :refer [cl-format]]
               :cljs [cljs.pprint :refer [cl-format]])))

(defn- left-pad
  [s len ch]
  (cl-format nil (str "~" len ",'" ch "d") (str s)))

(def s32-chars "234567abcdefghijklmnopqrstuvwxyz")

(defn s32-encode
  [n]
  (loop [n n
         s '()]
    (if (zero? n)
      (apply str (or (seq s) [\2]))
      (recur (floor (/ n 32))
             (cons (.charAt s32-chars (mod n 32)) s)))))

(def timestamp-ms
  "Current timestamp in milliseconds."
  #?(:clj System/currentTimeMillis))

(def last-timestamp-ms (atom 0))

(defn- monotime-ms
  "Monotonically increasing timestamps in milliseconds."
  []
  (swap! last-timestamp-ms
         (fn [last-ts]
           (let [ts (timestamp-ms)]
             (if (< last-ts ts)
               ts
               (+ 1 last-ts))))))

;; 10 bits for the clock-id
(def clock-id (floor (* 1024 (random))))

(defn next-tid
  "The next monotonically increasing TID."
  []
  (str (s32-encode (* 1000 (monotime-ms)))
       (left-pad (s32-encode clock-id) 2 "2")))

(comment

  (require '[atproto.tid :as tid] :reload)

  (tid/next-tid)

  (def tids (take 10 (repeatedly tid/next-tid)))

  (take 50 (map #(tid/left-pad (tid/s32-encode %) 5 "2") (range)))

  )
