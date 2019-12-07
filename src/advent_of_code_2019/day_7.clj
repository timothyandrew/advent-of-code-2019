(ns advent-of-code-2019.day-7
  (:require [advent-of-code-2019.util :as u]
            [clojure.math.combinatorics :as c]
            [clojure.core.async :as a]
            [clojure.string :as str]))

;; intcode

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
       99 :halt
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


;; end intcode

(defn amplifier-controller []
  [3,8,1001,8,10,8,105,1,0,0,21,42,55,64,85,98,179,260,341,422,99999,3,9,101,2,9,9,102,5,9,9,1001,9,2,9,1002,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,3,9,9,4,9,99,3,9,1002,9,4,9,101,3,9,9,102,5,9,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99])

(defn amp-2 []
  [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

(defn build-amp [state]
  (intcode-async (amplifier-controller) 0 state)
  state)

(defn execute-amplifiers [phase-range]
  (let [vals (for [phases (c/permutations phase-range)]
              (let [channels (repeatedly 5 #(a/chan 10))
                    connections [[0 1] [1 2] [2 3] [3 4] [4 0]]
                    amps (map (fn [[in out]]
                                (agent {:idx in :input (nth channels in) :output (nth channels out)}))
                              connections)]

                (doseq [[channel phase] (partition 2 (interleave channels phases))]
                  (a/>!! channel phase))

                (a/>!! (first channels) 0)

                (doall (for [amp amps] (send-off amp build-amp)))

                (apply await amps)

                (-> amps
                    last
                    deref
                    :output
                    a/<!!)))]

    (apply max vals)))


(defn day-7-1 []
  (execute-amplifiers (range 5)))

(defn day-7-2 []
  (execute-amplifiers (range 5 10)))
