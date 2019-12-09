(ns advent-of-code-2019.intcode
  (:require [advent-of-code-2019.util :as u]
            [clojure.math.combinatorics :as c]
            [clojure.core.async :as a]
            [clojure.string :as str]))

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

(defn read-input [{:keys [input]}]
  (let [input (a/<!! input)]
    input))

(defn send-output [tape pos [mode] {:keys [output]}]
  (let [out (read-tape tape (inc pos) mode)]
   (a/>!! output out)
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

(defn intcode-async
  ([tape pos channels]
   (let [{:keys [opcode modes]} (parse-opcode (nth tape pos))]
     (case opcode
       99 [tape pos channels]
       (let [[jump new-tape] (case opcode
                               1 (op + tape pos modes)
                               2 (op * tape pos modes)
                               3 (mov (read-input channels) tape pos)
                               4 (send-output tape pos modes channels)
                               5 (jmp-if-true tape pos modes)
                               6 (jmp-if-false tape pos modes)
                               7 (less-than tape pos modes)
                               8 (equals tape pos modes))]
         (intcode-async new-tape (+ pos jump) channels))))))

(defn setup
  ([program]
   (let [channels {:input (a/chan 10) :output (a/chan 10)}]
     (setup program channels)))
  ([program channels]
    [channels (agent [program 0 channels])]))

(defn exec [a]
  (send-off a #(apply intcode-async %)))
