(ns advent-of-code-2019.day-17
  (:require [advent-of-code-2019.intcode :as intcode]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [advent-of-code-2019.util :as util]
            [clojure.core.async :as a]
            [clojure.string :as str]
            [clojure.set :as set]))

(def program (edn/read-string (slurp (io/resource "input/17.edn"))))
(def program-2 (edn/read-string (slurp (io/resource "input/17-2.edn"))))

(defn find-intersections [coll]
  (for [i (range (count coll))
        j (range (count (first coll)))
        :let [current (get-in coll [i j])]
        :when (and (= current \#)
                   (< 0 i (dec (count coll)))
                   (< 0 j (dec (count (first coll))))
                   (= (get-in coll [(dec i) j]) \#)
                   (= (get-in coll [(inc i) j]) \#)
                   (= (get-in coll [i (dec j)]) \#)
                   (= (get-in coll [i (inc j)]) \#))]
    [i j]))

(defn calc-alignment [intersections]
  (reduce + (map #(reduce * %) intersections)))

(defn parse-routine [routine]
  (-> (re-seq #"[A-Za-z]|\d+" routine)
      (interleave (repeat ","))
      drop-last
      str/join))

(defn asciify [s]
  (-> (mapv int s)
      (conj 10)))

(defn send-all [channel vals]
  (doseq [val vals]
    (a/>!! channel val)))

(defn provide-inputs [{:keys [input output]}]
  (let [main "AACBCBCABA"
        fn-a "R8L12R8"
        fn-b "L12L12L10R10"
        fn-c "L10L10R8"
        continuous? "n"]
    (send-all input (-> main parse-routine asciify))
    (send-all input (-> fn-a parse-routine asciify))
    (send-all input (-> fn-b parse-routine asciify))
    (send-all input (-> fn-c parse-routine asciify))
    (send-all input (-> continuous? asciify))))

(defn day-17-1 []
  (let [[channels a] (intcode/setup program)
        _ (intcode/exec a)
        _ (await a)
        acc (util/drain-q (:output channels))]
    (-> char
        (map (a/<!! acc))
        str/join
        (str/split #"\n")
        find-intersections
        calc-alignment)))

(defn pp []
  (let [[channels a] (intcode/setup program)
        _ (intcode/exec a)
        _ (await a)
        acc (util/drain-q (:output channels))
        coll (str/join (map char (a/<!! acc)))]
    (print coll)))

(defn day-17-2 []
  (let [[channels a] (intcode/setup program-2)]
    (intcode/exec a)
    (provide-inputs channels)

    (loop []
      (if-let [val (a/<!! (:output channels))]
        (if (<= val 255)
          (do
            (print (char val))
            (recur))
          (println "ANSWER IS " val))
        (println "FAILED")))))

;; R 8 L 13 R 9 R 9 L 13 R 9 L 11 L 11 R 9 L 13 L 14 L 11 R 11 L 11 L 11 R 9 L 13 L 13 L 11 R 11 L 11 L 11 R 9 R 9 L 13 R 9 L 13 L 13 L 11 R 11 R 9 L 13 R 9
