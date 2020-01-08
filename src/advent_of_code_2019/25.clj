(ns advent-of-code-2019.25
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [advent-of-code-2019.intcode :as intcode]
            [clojure.core.async :as a]
            [clojure.string :as s]
            [clojure.string :as str]
            [advent-of-code-2019.util :as util]
            [clojure.math.combinatorics :as c]))

(def program (edn/read-string (slurp (io/resource "input/25.edn"))))

(def floor ["floor" "SSESESS"])

(def items
  (->> [
        ["escape pod" "WN"]
        ["molten lava" "E"]
        ["infinite loop" "SSE"]
        ["giant electromagnet" "SSES"]
        ["photons" "SWSE"]

        ["astrolabe" "S"]
        ["hologram" "SW"]
        ["space law space brochure" "SWS"]
        ["wreath" "SWSW"]
        ["hypercube" "SWSWW"]
        ["cake" "SS"]
        ["coin" "SSWN"]
        ["food ration" "SSESE"]]
       (group-by first)
       (util/map-vals first)))

(def item-combos
  (mapcat #(c/combinations (keys items) %)
          (range (inc (count items)))))

(defn to-instructions [s]
  (->> (s/split s #"")
       (mapv #(case % "S" "south" "N" "north" "E" "east" "W" "west"))
       (mapv #(conj (mapv int %) 10))
       flatten
       vec))

(defn to-rev-instructions [s]
  (->> (s/split s #"")
       reverse
       (mapv #(case % "S" "north" "N" "south" "E" "west" "W" "east"))
       (mapv #(conj (mapv int %) 10))
       flatten
       vec))

(defn take-instruction [s]
  (conj (mapv int (str "take " s)) 10))

(defn there-and-back [[item s]]
  (if (= item "floor")
    (vec (concat (to-instructions "SSESESS")
                 (to-rev-instructions "SSESES")))
    (vec (concat (to-instructions s)
                 (take-instruction item)
                 (to-rev-instructions s)))))


(defn update-buffer [b item]
  (let [size (count "Command?")]
    (if (= (count b) size)
      (conj (vec (drop 1 b)) item)
      (conj b item))))

(defn auto-io [in out]
  (let [buffer (atom [])
        target (mapv int "ejected ")]
    (a/go-loop []
      (when-let [val (a/<! out)]
        (swap! buffer update-buffer val)
        (do
          (cond
            (<= val 255) (print (char val))
            :else (print val))
          (flush)
          (recur))))))

(defn interactive-io [in out]
  (let [buffer (atom [])
        target (mapv int "Command?")]
    (a/go-loop []
      (when-let [val (a/<! out)]
        (swap! buffer update-buffer val)
        (cond
          (= @buffer target) (doseq [c (conj (mapv int (read-line)) 10)]
                               (a/>! in c))
          (<= val 255) (print (char val))
          :else (print val))
        (recur)))))

(defn goto [[{:keys [input output]} a] s]
  (if (= s "floor")
    (doseq [i (there-and-back floor)]
      (a/>!! input i))
    (doseq [i (there-and-back (get items s))]
      (a/>!! input i))))

(defn command [[{:keys [input output]} a] s]
  (doseq [i (conj (mapv int s) 10)]
    (a/>!! input i)))

(defn day-25-1 []
  (let [[channels a] (intcode/setup program)
        printer (auto-io (:input channels) (:output channels))]
    (intcode/exec a)
    (future (a/<!! printer))

    (doseq [x ["hologram"  "cake" "coin" "hypercube"]]
      (goto [channels a] x))
    
    (goto [channels a] "floor")))

