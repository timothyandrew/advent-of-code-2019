(ns advent-of-code-2019.day-5
  (:require [advent-of-code-2019.util :as u]
            [clojure.string :as str]))

(def input-1 [3 225 1 225 6 6 1100 1 238 225 104 0 1002 36 25 224 1001 224 -2100 224 4 224 1002 223 8 223 101 1 224 224 1 223 224 223 1102 31 84 225 1102 29 77 225 1 176 188 224 101 -42 224 224 4 224 102 8 223 223 101 3 224 224 1 223 224 223 2 196 183 224 1001 224 -990 224 4 224 1002 223 8 223 101 7 224 224 1 224 223 223 102 14 40 224 101 -1078 224 224 4 224 1002 223 8 223 1001 224 2 224 1 224 223 223 1001 180 64 224 101 -128 224 224 4 224 102 8 223 223 101 3 224 224 1 223 224 223 1102 24 17 224 1001 224 -408 224 4 224 1002 223 8 223 101 2 224 224 1 223 224 223 1101 9 66 224 1001 224 -75 224 4 224 1002 223 8 223 1001 224 6 224 1 223 224 223 1102 18 33 225 1101 57 64 225 1102 45 11 225 1101 45 9 225 1101 11 34 225 1102 59 22 225 101 89 191 224 1001 224 -100 224 4 224 1002 223 8 223 1001 224 1 224 1 223 224 223 4 223 99 0 0 0 677 0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1 99999 1005 227 99999 1005 0 256 1105 1 99999 1106 227 99999 1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105 1 280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300 1105 1 99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 8 226 677 224 1002 223 2 223 1006 224 329 1001 223 1 223 108 226 226 224 1002 223 2 223 1006 224 344 1001 223 1 223 7 677 226 224 102 2 223 223 1005 224 359 101 1 223 223 7 226 677 224 102 2 223 223 1006 224 374 101 1 223 223 1008 677 226 224 1002 223 2 223 1006 224 389 101 1 223 223 8 677 677 224 1002 223 2 223 1005 224 404 101 1 223 223 8 677 226 224 102 2 223 223 1005 224 419 1001 223 1 223 1107 677 226 224 102 2 223 223 1005 224 434 1001 223 1 223 1107 226 677 224 1002 223 2 223 1006 224 449 1001 223 1 223 107 677 226 224 1002 223 2 223 1005 224 464 1001 223 1 223 1008 677 677 224 1002 223 2 223 1006 224 479 1001 223 1 223 1108 677 226 224 1002 223 2 223 1006 224 494 1001 223 1 223 1108 677 677 224 1002 223 2 223 1006 224 509 1001 223 1 223 107 677 677 224 1002 223 2 223 1005 224 524 101 1 223 223 1007 677 226 224 102 2 223 223 1005 224 539 1001 223 1 223 1107 226 226 224 1002 223 2 223 1006 224 554 1001 223 1 223 1008 226 226 224 1002 223 2 223 1006 224 569 101 1 223 223 1108 226 677 224 1002 223 2 223 1006 224 584 101 1 223 223 108 677 677 224 1002 223 2 223 1006 224 599 1001 223 1 223 1007 677 677 224 102 2 223 223 1006 224 614 101 1 223 223 107 226 226 224 102 2 223 223 1006 224 629 101 1 223 223 1007 226 226 224 102 2 223 223 1005 224 644 1001 223 1 223 108 226 677 224 102 2 223 223 1005 224 659 1001 223 1 223 7 677 677 224 102 2 223 223 1006 224 674 1001 223 1 223 4 223 99 226])

(defn input []
  (Integer/parseInt (read-line)))

(defn read-tape [tape pos mode]
  (case (or mode :position)
    :immediate (nth tape pos)
    :position (nth tape (nth tape pos))))

(defn op [operator tape pos [lmode rmode]]
  (let [dest-position (nth tape (+ pos 3))
        values [(read-tape tape (inc pos) lmode) (read-tape tape (+ pos 2) rmode)]]
    [4 (assoc tape dest-position (reduce operator values))]))

(defn mov [in tape pos]
  (let [dest-position (nth tape (inc pos))]
    [2 (assoc tape dest-position in)]))

(defn output [tape pos [mode]]
  (let [dest (read-tape tape (inc pos) mode)]
    (println dest)
    [2 tape]))

(defn jmp-if-false [tape pos [mode1 mode2]]
  (let [param-1 (read-tape tape (inc pos) mode1)
        param-2 (read-tape tape (+ pos 2) mode2)]
    (if (= param-1 0)
      [(- param-2 pos) tape]
      [3 tape])))

(defn jmp-if-true [tape pos [mode1 mode2]]
  (let [param-1 (read-tape tape (inc pos) mode1)
        param-2 (read-tape tape (+ pos 2) mode2)]
    (if (= param-1 0)
      [3 tape]
      [(- param-2 pos) tape])))

(defn equals [tape pos [lmode rmode]]
  (let [param-1 (read-tape tape (inc pos) lmode)
        param-2 (read-tape tape (+ pos 2) rmode)
        dest-position (nth tape (+ pos 3))]
    (if (= param-1 param-2)
      [4 (assoc tape dest-position 1)]
      [4 (assoc tape dest-position 0)])))

(defn less-than [tape pos [lmode rmode]]
  (let [param-1 (read-tape tape (inc pos) lmode)
        param-2 (read-tape tape (+ pos 2) rmode)
        dest-position (nth tape (+ pos 3))]
    (if (< param-1 param-2)
      [4 (assoc tape dest-position 1)]
      [4 (assoc tape dest-position 0)])))

(defn parse-opcode [opcode]
  (if (< opcode 100)
    {:opcode opcode}
    (letfn [(mode-sym [mode]
              (case mode
                "0" :position
                "1" :immediate))]
      {:opcode (rem opcode 10)
       :modes (->> (str/split (str (int (/ opcode 100))) #"")
                   reverse
                   (map mode-sym))})))

(defn intcode
  ([tape] (intcode tape 0))
  ([tape pos]
   (let [{:keys [opcode modes]} (parse-opcode (nth tape pos))]
     (if (= opcode 99)
       :halt
       (let [[jump new-tape] (case opcode
                               1 (op + tape pos modes)
                               2 (op * tape pos modes)
                               3 (mov (input) tape pos)
                               4 (output tape pos modes)
                               5 (jmp-if-true tape pos modes)
                               6 (jmp-if-false tape pos modes)
                               7 (less-than tape pos modes)
                               8 (equals tape pos modes))]
         (intcode new-tape (+ pos jump)))))))
