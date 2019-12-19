(ns advent-of-code-2019.day-16
  (:require
   [clojure.core.matrix :as m]
   [clojure.core.matrix.operators :as o]
   [clojure.string :as str]
   [clojure.string :as s]))

(defn mapint [coll]
  (mapv #(Integer/parseInt %) coll))

(defn parse-input [s]
  (-> s (str/split #"") mapint))

(def example-1 (parse-input "80871224585914546619083218645595"))
(def example-2 (apply concat (repeat 2 example-1)))
(def input (parse-input "59790132880344516900093091154955597199863490073342910249565395038806135885706290664499164028251508292041959926849162473699550018653393834944216172810195882161876866188294352485183178740261279280213486011018791012560046012995409807741782162189252951939029564062935408459914894373210511494699108265315264830173403743547300700976944780004513514866386570658448247527151658945604790687693036691590606045331434271899594734825392560698221510565391059565109571638751133487824774572142934078485772422422132834305704887084146829228294925039109858598295988853017494057928948890390543290199918610303090142501490713145935617325806587528883833726972378426243439037"))
(defonce input-2 (apply concat (repeat 10000 input)))

(defn preserve-ones-place [n]
  (last (str/split (str n) #"")))

(defn repeating-pattern [index]
  (let [base [0 1 0 -1]
        index (inc index)]
    (->> base
         (mapcat #(repeat index %))
         repeat
         (apply concat)
         (drop 1))))

(defn fft [input phases]
  (loop [input input i 0]
    (println "Phase " i)
    (if (= i phases)
      input
      (let [next (for [[digit i] (partition 2 (interleave input (range)))]
                   (preserve-ones-place
                    (reduce + (m/mul (drop i input) (drop i (take (count input) (repeating-pattern i)))))))]
        (recur (mapint next)
               (inc i))))))

(defn day-16-2 []
  )
