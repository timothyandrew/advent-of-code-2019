(ns advent-of-code-2019.day-19
  (:require [advent-of-code-2019.intcode :as intcode]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.core.async :as a]
            [advent-of-code-2019.util :as util]
            [clojure.set :as set]))

(def program (edn/read-string (slurp (io/resource "input/19.edn"))))

(defn drone [[i j]]
  (let [[{:keys [input output]} a] (intcode/setup program)]
    (intcode/exec a)
    (do
      (a/>!! input i)
      (a/>!! input j)
      (let [output (a/<!! output)]
        output))))

(defn first-on-row [i]
  (loop [cells (partition 2 (interleave (range) (repeat i)))]
    (let [value (drone (first cells))]
      (if (= value 1)
        (first (first cells))
        (recur (next cells))))))

(defn width-of-row [i]
  (loop [cells (partition 2 (interleave (range) (repeat i)))
         started-at nil]
    (let [value (drone (first cells))]
      (cond
        (and started-at (= value 0)) (- (ffirst cells) (first started-at))
        (and (not started-at) (= value 1)) (recur (next cells) (first cells))
        :else (recur (next cells) started-at)))))

(defn within? [n [from to]]
  (and (>= n from)
       (<= n to)))

(defn stats [i]
  (let [start (first-on-row i)
        size (width-of-row i)
        end (+ start size)]
    [{:start start :size size :end end}]))

(defn square-fits? [i]
  (let [start (first-on-row i)
        size (width-of-row i)
        end (+ start size)

        prev-start (first-on-row (- i 99))
        prev-size (width-of-row (- i 99))
        prev-end (+ prev-start prev-size)]

    (>=
     (count
      (set/intersection
       (set (range start end))
       (set (range prev-start prev-end))))
     100)))

(defn day-19-1 []
  (let [beam
        (->>
         (for [i (range 50)
               j (range 50)
               :let [output (drone [i j])]
               :when (= output 1)]
           [i output])
         (group-by first)
         (util/map-vals count))]
    beam))

;;  8150932 < answer < 9320815
