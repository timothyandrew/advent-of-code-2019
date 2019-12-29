(ns advent-of-code-2019.day-21
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [advent-of-code-2019.intcode :as intcode]
            [clojure.core.async :as a]
            [advent-of-code-2019.util :as util]))

(def program (edn/read-string (slurp (io/resource "input/21.edn"))))


(defn print-output [out]
  (a/go-loop []
    (when-let [val (a/<! out)]
      (if (<= val 255)
        (print (char val))
        (print val))
      (recur))))

(defn send-instruction [in instr]
  (doseq [c (conj (mapv int instr) 10)]
    (a/>!! in c)))

(def instructions
  [
   ;; nothing at 1
   "NOT A J"


   ;; nothing at 3, something at 4, something at 9
   "NOT C T"
   "AND D T"
   "AND H T"
   "OR T J"

   ;; nothing at 2, something at 4
   "NOT B T"
   "AND D T"
   "OR T J"

   "RUN"
   ])

(defn day-21-1 []
  (let [[{:keys [input output]} a] (intcode/setup program)
        printer (print-output output)]
    (intcode/exec a)

    (doseq [i instructions]
      (send-instruction input i))

    (a/<!! printer)))
