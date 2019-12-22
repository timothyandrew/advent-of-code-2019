(ns advent-of-code-2019.util
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.core.async :as a]))

(defn read-lines [filename]
  (s/split-lines
   (slurp (io/resource filename))))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn filter-vals [f m]
  (into {} (filter (fn [[k v]] (f v)) m)))

(defn drain-q [q]
  (a/go-loop [accumulator []]
                (let [item (a/<! q)]
                  (if (nil? item)
                    accumulator
                    (recur (conj accumulator item))))))
