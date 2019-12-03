(ns advent-of-code-2019.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent-of-code-2019.util :as util]))

(defn op [operator tape pos]
  (let [positions [(nth tape (+ pos 1)) (nth tape (+ pos 2))]
        dest-position (nth tape (+ pos 3))
        values (map #(nth tape %) positions)]
    (assoc tape dest-position (reduce operator values))))

(defn tick
  ([tape] (tick tape 0))
  ([tape pos]
   (case (nth tape pos)
     1 (tick (op + tape pos) (+ pos 4))
     2 (tick (op * tape pos) (+ pos 4))
     99 tape)))

(defn reset-tape [a b]
  [1 a b 3 1 1 2 3 1 3 4 3 1 5 0 3 2 13 1 19 1 19 10 23 1 23 6 27
   1 6 27 31 1 13 31 35 1 13 35 39 1 39 13 43 2 43 9 47 2 6 47 51
   1 51 9 55 1 55 9 59 1 59 6 63 1 9 63 67 2 67 10 71 2 71 13 75 1
   10 75 79 2 10 79 83 1 83 6 87 2 87 10 91 1 91 6 95 1 95 13 99 1
   99 13 103 2 103 9 107 2 107 10 111 1 5 111 115 2 115 9 119 1 5
   119 123 1 123 9 127 1 127 2 131 1 5 131 0 99 2 0 14 0])

(defn day-2-1 []
  (take 3 (tick (reset-tape 12 2))))

(defn day-2-2 []
  (for [i (range 99)
        j (range 99)
        :let [tape (tick (reset-tape i j))]
        :when (= (first tape) 19690720)]
    (take 3 tape)))
