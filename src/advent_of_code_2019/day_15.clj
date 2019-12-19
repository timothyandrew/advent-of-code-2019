(ns advent-of-code-2019.day-15
  (:require [clojure.core.async :as a]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.edn :as edn]
            [advent-of-code-2019.intcode :as intcode]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn program []
  (edn/read-string (slurp (io/resource "input/15.edn"))))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn opposite [direction]
  (case direction
    1 2
    2 1
    3 4
    4 3))

(defn move [{:keys [input output]} direction]
  (a/>!! input direction)
  (a/<!! output))

(defn attempt-move-and-return [{:keys [input output] :as channels} direction]
  (a/>!! input direction)
  (let [val (a/<!! output)]
    (case val
      0 :wall
      1 (do (move channels (opposite direction))
            :free) 
      2 :done)))

(defn examine [channels]
  {1 (attempt-move-and-return channels 1)
   2 (attempt-move-and-return channels 2)
   3 (attempt-move-and-return channels 3)
   4 (attempt-move-and-return channels 4)})

(defn perform-initial-moves [channels moves]
  (doseq [m moves]
    (move channels m)))

(defn coord->direction [[x y] direction]
  (case direction
    1 [x (inc y)]
    2 [x (dec y)]
    3 [(dec x) y]
    4 [(inc x) y]))

(defn execute-maze [channels moves-so-far coord-so-far]
  (loop [coord (or coord-so-far [0 0]) moves-so-far (or moves-so-far []) pending-paths #{} free-tiles #{[0 0]}]
    (let [surroundings (if (empty? moves-so-far)
                         (examine channels)
                         (dissoc (examine channels) (opposite (last moves-so-far))))
          free-dests (map first (filter (fn [[k v]] (= v :free)) surroundings))
          [next-move & others] free-dests
          done (first (filter (fn [[k v]] (= v :done)) surroundings))
          alternates (set (map (fn [o]
                                 {:coord (coord->direction coord o)
                                  :moves (conj moves-so-far o)}) others))]
      (if next-move
        (do
          (move channels next-move)
          (recur (coord->direction coord next-move)
                 (conj moves-so-far next-move)
                 (set/union pending-paths alternates)
                 (set/union free-tiles (set (map #(coord->direction coord %) free-dests)))))
        (if done
          {:done true
           :oxy (coord->direction coord (first done))
           :pending (set/union pending-paths alternates)
           :free (conj free-tiles coord) 
           :path (conj moves-so-far (first done))}
          {:done false
           :free (conj free-tiles coord)
           :pending (set/union pending-paths alternates)})))))

(defn adjacent? [[x1 y1] [x2 y2]]
  (or (and
       (= y1 y2)
       (= x1 (inc x2)))

      (and
       (= y1 y2)
       (= x1 (dec x2)))

      (and
       (= x1 x2)
       (= y1 (inc y2)))

      (and
       (= x1 x2)
       (= y1 (dec y2)))))

(defn adjacent-to-any? [coll x]
  (some #(adjacent? x %) coll))

(defn propagate-oxygen [start free-tiles]
  (loop [i 0 oxygenated #{start} free free-tiles]
    (if (empty? free)
      i
      (let [fresh (set (filter #(adjacent-to-any? oxygenated %) free))]
        (recur (inc i)
               (set/union oxygenated fresh)
               (set/difference free fresh))))))

(defn day-15-1 []
  (loop [pending-paths #{} i 0]
    (let [initial-path (first pending-paths)
          pending-paths (disj pending-paths initial-path)
          [channels a] (intcode/setup (program))
          _ (intcode/exec a)
          _ (perform-initial-moves channels (:moves initial-path))
          {:keys [done pending path]} (execute-maze channels (:moves initial-path) (:coord initial-path))]
      (if done 
        (count path)
        (do
          (doseq [c (vals channels)]
            (a/close! c))
          (recur (set/union pending-paths pending) (inc i)))))))


(defn day-15-2 []
  (loop [pending-paths #{} i 0 oxy-station nil free-tiles #{[0 0]}]
    (let [initial-path (first pending-paths)
          pending-paths (disj pending-paths initial-path)
          [channels a] (intcode/setup (program))
          _ (intcode/exec a)
          _ (perform-initial-moves channels (:moves initial-path))
          {:keys [oxy free done pending path]} (execute-maze channels (:moves initial-path) (:coord initial-path))
          oxy-station (or oxy-station oxy)]
      (if (empty? (set/union pending-paths pending))
        (propagate-oxygen oxy-station (set/union free-tiles free))
        (do
          (doseq [c (vals channels)] (a/close! c))
          (recur (set/union pending-paths pending) (inc i) oxy-station (set/union free-tiles free)))))))
