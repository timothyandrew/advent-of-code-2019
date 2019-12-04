(ns advent-of-code-2019.day-4
  (:require [advent-of-code-2019.util :as util]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn enumerate-range []
  (range 254032 (inc 789860)))

(defn is-valid-1 [n]
  (let [digits (map #(Integer/parseInt %) (str/split (str n) #""))
        repeats (set (map str [11 22 33 44 55 66 77 88 99 00]))]
    (and
     (apply <= digits)
     (not (empty? (set/intersection repeats (set (map str/join (partition 2 1 digits)))))))))


(defn is-valid-2 [n]
  (let [digits (map #(Integer/parseInt %) (str/split (str n) #""))
        repeats-ref (set (map str [11 22 33 44 55 66 77 88 99 00]))
        pairs (map str/join (partition 2 1 digits))
        repeats (set/intersection repeats-ref (set pairs))]
    
    (and
     (apply <= digits)
     (some #(= % 1) (for [repeat repeats]
                     (count (filter #(= repeat %) pairs)))))))

(defn day-4-1 []
  (count (filter true? (map is-valid-1 (enumerate-range)))))

(defn day-4-2 []
  (count (filter true? (map is-valid-2 (enumerate-range)))))
