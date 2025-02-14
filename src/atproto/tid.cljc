(ns atproto.tid
  (:require [clojure.string :as str]))

(def base32-sortable "234567abcdefghijklmnopqrstuvwxyz")

(defn encode
  "Encode a 64-bit integer to a base32-sortable string"
  [i]
  (str/join (map #(nth base32-sortable
                    (bit-and 31 (bit-shift-right i (* % 5))))
              (range 12 -1 -1))))

#?(:clj
   (let [last (volatile! 0)]
     (defn clj-tid []
       (locking last
         (let [nt (System/nanoTime)
               us (^[long long] Math/floorDiv nt 1000)
               tid (bit-or (bit-shift-left us 10)
                     (bit-and nt 1023))]
           (if (= @last tid)
             (do (println "bounce") (Thread/sleep 0 1) (clj-tid))
             (do (vreset! last tid) tid)))))))

(defn tid
  "Return a ATProto timestamp identifier using the spec defined at
   https://atproto.com/specs/tid"
  []
  (encode
    #?(:clj (clj-tid))))

(def example "3lhmaghq7vs2k")

(defn extract-ts
  [] nil
  )



;; 1000 ms in a s
;; 1000 microseconds in a ms
;; 1000 nanoseconds in a us

(java.lang.Long/toString
  (/ (Math/pow 2 53) 1000) 10)



(comment

  (range 13)
  (range 12 -1 -1)

  (encode 123456)
  (encode2 123456)

  (java.lang.Integer/toBinaryString 31)
  (java.lang.Integer/toBinaryString 1234)
  (Long/parseLong "10010" 2)

  (bit-and 31  (bit-shift-right 1234 60))

  )
