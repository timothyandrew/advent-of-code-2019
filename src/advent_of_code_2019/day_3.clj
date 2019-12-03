(ns advent-of-code-2019.day-3
  (:require [advent-of-code-2019.util :as util]
            [clojure.set :as set]
            [clojure.string :as str]))

;; strategies
;; 1. tag each point the first line passes
;; 2. tag each vertex in the first line

(defn point
  ([x y i]
   {:x x :y y :length i})
  ([x y]
     {:x x :y y}))

(defn up [{:keys [x y]} distance]
  (point x (+ y distance)))

(defn down [{:keys [x y]} distance]
  (point x (- y distance)))

(defn left [{:keys [x y]} distance]
  (point (- x distance) y))

(defn right [{:keys [x y]} distance]
  (point (+ x distance) y))

(defn tag-points-between [from to {:keys [wire-length]}]
  (if (or (< (:x from) (:x to))
          (< (:y from) (:y to)))
    (set (for [x (range (:x from) (inc (:x to)))
               y (range (:y from) (inc (:y to)))
               :let [i (+ wire-length (- x (:x from)) (- y (:y from)))]]
           (point x y i)))
    (set (for [x (range (:x from) (dec (:x to)) -1)
               y (range (:y from) (dec (:y to)) -1)
               :let [i (+ wire-length (- (:x from) x) (- (:y from) y))]]
           (point x y i)))))

(defn wire-length-between [from to]
  (+ (Math/abs (- (:x from) (:x to)))
     (Math/abs (- (:y from) (:y to)))))

(defn parse-instr [{:keys [points last wire-length]} [direction distance]]
  (let [destination (case direction
                      :U (up last distance)
                      :D (down last distance)
                      :L (left last distance)
                      :R (right last distance))]
    {:points (set/union points (tag-points-between last destination {:wire-length wire-length}))
     :last destination
     :wire-length (+ wire-length (wire-length-between last destination))}))

(defn parse [input]
  (->> (str/split input #",")
       (map #(map str/join (split-at 1 %)))
       (map (fn [[direction distance]] [(keyword direction) (Integer/parseInt distance)]))))

(defn strip-lengths [points]
  (set (map #(dissoc % :length) points)))

(defn day-3-1 [input]
  (let [points (->> input
                    (map parse)
                    (map #(reduce parse-instr {:points #{} :last (point 0 0) :wire-length 0} %))
                    (map :points)
                    (map strip-lengths))
        intersections (apply set/intersection points)]
    (->> (map #(+ (Math/abs (:x %)) (Math/abs (:y %))) (disj intersections (point 0 0)))
         (apply min))))

(defn day-3-2 [input]
  (let [points (->> input
                    (map parse)
                    (map #(reduce parse-instr {:points #{} :last (point 0 0) :wire-length 0} %))
                    (map :points))
        bare-points (map strip-lengths points)
        point->lengths (group-by #(vec [(:x %) (:y %)]) (apply concat (map vec points)))
        intersections (disj (apply set/intersection bare-points) (point 0 0))]
    (->> intersections
         (map (comp point->lengths vec vals))
         (map #(map :length %))
         (map #(reduce + %))
         (apply min))))
