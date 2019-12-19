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

(defn execute-maze [channels moves-so-far]
  (loop [moves-so-far (or moves-so-far []) pending-paths #{}]
    (let [surroundings (if (empty? moves-so-far)
                         (examine channels)
                         (dissoc (examine channels) (opposite (last moves-so-far))))
          [next-move & others] (map first (filter (fn [[k v]] (= v :free)) surroundings))
          done (first (filter (fn [[k v]] (= v :done)) surroundings))
          alternates (set (map (fn [o] (conj moves-so-far o)) others))]
      (if next-move
        (do
          (move channels next-move)
          (recur (conj moves-so-far next-move)
                 (set/union pending-paths alternates)))
        (if done
          {:done true :path (conj moves-so-far (first done))}
          {:done false :pending (set/union pending-paths alternates)})))))


(defn day-15-1 []
  (loop [pending-paths #{} i 0]
    (let [initial-path (first pending-paths)
          pending-paths (disj pending-paths initial-path)
          [channels a] (intcode/setup (program))
          _ (intcode/exec a)
          _ (perform-initial-moves channels initial-path)
          {:keys [done pending path]} (execute-maze channels initial-path)]
      (if done 
        (count path)
        (do
          (doseq [c (vals channels)]
            (a/close! c))
          (recur (set/union pending-paths pending) (inc i)))))))

