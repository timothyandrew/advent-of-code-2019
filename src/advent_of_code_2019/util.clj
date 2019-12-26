(ns advent-of-code-2019.util
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.core.async :as a])
  (:import [java.util PriorityQueue HashMap]))

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

(defn resolve-path [m a b came-from]
  (when (get came-from b)
    (let [pathxy
          (loop [current b path [b]]
            (let [next (get came-from current)]
              (if (= next a)
                (conj path a)
                (recur next (conj path next)))))]
      [pathxy
       (->> pathxy
            (mapv #(get-in m %))
            (remove #(= % \.))
            reverse
            (drop 1)
            drop-last
            vec)])))

(defn dijkstra [m {:keys [start neighbors-fn done? parse-final cost log]}]
  (let [cost-so-far (atom {start 0}) came-from (atom {})
        c-fn (comparator (fn [pxy qxy]  (< (get @cost-so-far pxy) (get @cost-so-far qxy))))
        frontier (PriorityQueue. c-fn)]
    (.add frontier start)
    (loop [i 0]
      (let [current (.poll frontier)
            neighbors (neighbors-fn current)]

        (when (= (mod i 1000) 0)
          (log i current))

        (cond
          (done? current) 
          (parse-final (resolve-path m start current @came-from))

          (nil? current)
          :FAILED
          
          :else
          (do
            (doseq [next neighbors 
                    :let [cost (+ (get @cost-so-far current)
                                  (cost current next))]
                    :when (or (not (get @cost-so-far next))
                              (< cost (get @cost-so-far next)))]
              (swap! cost-so-far assoc next cost)
              (swap! came-from assoc next current)
              (.add frontier next))
            (recur (inc i))))))))
