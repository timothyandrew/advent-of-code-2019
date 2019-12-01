(ns advent-of-code-2019.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

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

(defn read-lines [filename]
  (s/split-lines
   (slurp (io/resource filename))))

(defn day-1-1 []
  (->> (read-lines "input/1.txt")
       (map #(Integer/parseInt %))
       (map fuel)
       (reduce +)))

(defn day-1-2 []
  (->> (read-lines "input/1.txt")
       (map #(Integer/parseInt %))
       (map fuel-recursive)
       (reduce +)))
