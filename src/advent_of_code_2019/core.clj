(ns advent-of-code-2019.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent-of-code-2019.util :as util]))

(defn fuel [mass]
  (- (int (/ mass 3))
     2))

(defn fuel-recursive
  ([mass] (fuel-recursive 0 mass))
  ([f mass]
   (let [new-fuel (fuel mass)]
     (if (<= new-fuel 0)
       f
       (fuel-recursive (+ f new-fuel) new-fuel)))))

(defn day-1-1 []
  (->> (util/read-lines "input/1.txt")
       (map #(Integer/parseInt %))
       (map fuel)
       (reduce +)))

(defn day-1-2 []
  (->> (util/read-lines "input/1.txt")
       (map #(Integer/parseInt %))
       (map fuel-recursive)
       (reduce +)))
