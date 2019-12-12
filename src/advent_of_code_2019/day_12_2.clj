(ns advent-of-code-2019.day-12-2
  (:require [clojure.math.combinatorics :as c]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as o]))

(defn world []
  {:pos [[-1 2 4 3]
         [0 -10 -8 5]
         [2 -7 8 -1]]
   :vel [[0 0 0 0]
         [0 0 0 0]
         [0 0 0 0]]})

(defn world-2 []
  {:pos [[-8 5 2 9]
         [-10 5 -7 -8]
         [0 10 3 -3]]
   :vel [[0 0 0 0]
         [0 0 0 0]
         [0 0 0 0]]})

(defn input []
  {:pos [[3 5 -10 8]
         [15 -1 8 4]
         [8 -2 2 -5]]
   :vel [[0 0 0 0]
         [0 0 0 0]
         [0 0 0 0]]})

(defn pos->vel [pos vel]
  (reduce
   (fn [pos [[i1 x1] [i2 x2]]]
     (-> pos
         (update i1 + (cond
                        (> x1 x2) -1
                        (< x1 x2) 1
                        (= x1 x2) 0))
         (update i2 + (cond
                        (> x2 x1) -1
                        (< x2 x1) 1
                        (= x2 x1) 0))))
   vel 
   (c/combinations (map-indexed vector pos) 2)))

(defn apply-gravity [world]
  (let [[x y z] (:pos world)
        [vx vy vz] (:vel world)]
    {:pos [x y z]
     :vel [(pos->vel x vx)
           (pos->vel y vy)
           (pos->vel z vz)]}))

(defn apply-velocity [world]
  {:pos (m/add (:pos world) (:vel world))
   :vel (:vel world)})

(defn tick [start]
  (reduce
   (fn [{:keys [world compl]} i]
     (if (>= (count compl) 3)
       (reduced compl)
       (let [world (-> world apply-gravity apply-velocity)]
         {:compl
          (cond-> compl
            (and (= (get-in start [:pos 0]) (get-in world [:pos 0]))
                 (= (get-in start [:vel 0]) (get-in world [:vel 0]))
                 (not (get compl :X))) (assoc :X i) 
            (and (= (get-in start [:pos 1]) (get-in world [:pos 1]))
                 (= (get-in start [:vel 1]) (get-in world [:vel 1]))
                 (not (get compl :Y))) (assoc :Y i)
            (and (= (get-in start [:pos 2]) (get-in world [:pos 2]))
                 (= (get-in start [:vel 2]) (get-in world [:vel 2]))
                 (not (get compl :Z))) (assoc :Z i))
          :world world})))
   {:world start :compl {}}
   (range)))
