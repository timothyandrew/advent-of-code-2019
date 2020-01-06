(ns advent-of-code-2019.day-23
  (:require [advent-of-code-2019.intcode :as intcode]
            [clojure.core.async :as a]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [advent-of-code-2019.util :as util]))

(def program (edn/read-string (slurp (io/resource "input/23.edn"))))

(defn computers []
  (vec (for [i (range 50)]
         (let [[channels a] (intcode/setup program {:nonblocking-input true})]
           (intcode/exec a)
           (a/>!! (:input channels) i)
           channels))))

(defn idle? [inputs]
  (every? #(= 0 (util/chan-count %)) inputs))

(defn NAT [])

(defn day-23-1 []
  (let [c (computers)
        outputs (mapv :output c)
        inputs (mapv :input c)]
    (loop [NAT nil last nil]
      (let [[dest c] (a/alts!! (conj outputs (a/timeout 20)))]
        (if dest
          (let [X (a/<!! c) Y (a/<!! c)]
            (if (< dest 50)
              (do
                (a/>!! (nth inputs dest) X)
                (a/>!! (nth inputs dest) Y)
                (recur NAT last))
              (recur [X Y] last)))
          (if (idle? inputs)
            (do
              (a/>!! (first inputs) (first NAT))
              (a/>!! (first inputs) (second NAT))
              (if (= last (second NAT))
                NAT
                (recur NAT (second NAT))))
            (recur NAT last)))))))
