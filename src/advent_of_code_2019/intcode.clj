(ns advent-of-code-2019.intcode
  (:require [advent-of-code-2019.util :as u]
            [clojure.math.combinatorics :as c]
            [clojure.core.async :as a]
            [clojure.string :as str]))

(defn read-tape [tape pos mode rel-base]
  (case (or mode :position) 
    :immediate (nth tape pos)
    :position (nth tape (nth tape pos))
    :relative (nth tape (+ rel-base (nth tape pos)))))

(defn dest-pos [tape pos rel-base mode]
  (case (or mode :position)
          :position (nth tape pos)
          :relative (+ rel-base (nth tape pos))))

(defn op [operator tape pos [lmode rmode wmode] rel-base]
  (let [dest-position (dest-pos tape (+ pos 3) rel-base wmode) 
        values [(read-tape tape (inc pos) lmode rel-base)
                (read-tape tape (+ pos 2) rmode rel-base)]]
    [{:jump 4} (assoc tape dest-position (reduce operator values))]))

(defn mov [in tape pos [mode] rel-base]
  (let [dest-position (dest-pos tape (inc pos) rel-base mode)]
    [{:jump 2} (assoc tape dest-position in)]))

(defn read-input [{:keys [input]}]
  (let [input (a/<!! input)]
    input))

(defn send-output [tape pos [mode] {:keys [output]} rel-base]
  (let [out (read-tape tape (inc pos) mode rel-base)]
   (a/>!! output out)
   [{:jump 2} tape]))

(defn jmp-if-false [tape pos [mode1 mode2] rel-base]
  (let [param-1 (read-tape tape (inc pos) mode1 rel-base)
        param-2 (read-tape tape (+ pos 2) mode2 rel-base)]
    (if (= param-1 0)
      [{:jump (- param-2 pos)} tape]
      [{:jump 3} tape])))

(defn jmp-if-true [tape pos [mode1 mode2] rel-base]
  (let [param-1 (read-tape tape (inc pos) mode1 rel-base)
        param-2 (read-tape tape (+ pos 2) mode2 rel-base)]
    (if (= param-1 0)
      [{:jump 3} tape]
      [{:jump (- param-2 pos)} tape])))

(defn equals [tape pos [lmode rmode wmode] rel-base]
  (let [param-1 (read-tape tape (inc pos) lmode rel-base)
        param-2 (read-tape tape (+ pos 2) rmode rel-base)
        dest-position (dest-pos tape (+ pos 3) rel-base wmode)]
    (if (= param-1 param-2)
      [{:jump 4} (assoc tape dest-position 1)]
      [{:jump 4} (assoc tape dest-position 0)])))

(defn less-than [tape pos [lmode rmode wmode] rel-base]
  (let [param-1 (read-tape tape (inc pos) lmode rel-base)
        param-2 (read-tape tape (+ pos 2) rmode rel-base)
        dest-position (dest-pos tape (+ pos 3) rel-base wmode)]
    (if (< param-1 param-2)
      [{:jump 4} (assoc tape dest-position 1)]
      [{:jump 4} (assoc tape dest-position 0)])))

(defn adj-base [tape pos [mode] rel-base]
  (let [base (read-tape tape (inc pos) mode rel-base)]
    [{:jump 2 :base (+ rel-base base)} tape]))

(defn parse-opcode [opcode]
  (if (< opcode 100)
    {:opcode opcode}
    (letfn [(mode-sym [mode]
              (case mode
                "0" :position
                "1" :immediate
                "2" :relative))]
      {:opcode (rem opcode 10)
       :modes (->> (str/split (str (int (/ opcode 100))) #"")
                   reverse
                   (map mode-sym))})))

(defn intcode-async
  ([tape pos channels]
   (intcode-async tape pos channels 0))
  ([tape pos channels rel-base]
   (loop [tape tape pos pos channels channels rel-base rel-base]
     (let [{:keys [opcode modes]} (parse-opcode (nth tape pos))]
       (case opcode
         99 (do
              (a/close! (:output channels))
              [tape pos channels])
         (let [[{:keys [jump base]} new-tape] (case opcode
                                                1 (op + tape pos modes rel-base)
                                                2 (op * tape pos modes rel-base)
                                                3 (mov (read-input channels) tape pos modes rel-base)
                                                4 (send-output tape pos modes channels rel-base)
                                                5 (jmp-if-true tape pos modes rel-base)
                                                6 (jmp-if-false tape pos modes rel-base)
                                                7 (less-than tape pos modes rel-base)
                                                8 (equals tape pos modes rel-base)
                                                9 (adj-base tape pos modes rel-base))]
           (recur new-tape (+ pos jump) channels (or base rel-base))))))))

(defn setup
  ([program]
   (let [channels {:input (a/chan 10000) :output (a/chan 100000)}]
     (setup program channels)))
  ([program channels]
   (let [padded-program (into [] (concat program (take 1000000 (repeat 0))))]
     [channels (agent [padded-program 0 channels])])))

(defn exec [a]
  (send-off a #(apply intcode-async %)))
