(ns advent-of-code-2019.day-20
  (:require [advent-of-code-2019.util :as util]
            [clojure.math.combinatorics :as c]))

(defonce example-1 (util/read-lines "input/20-0.txt"))
(defonce example-2 (util/read-lines "input/20-1.txt"))
(defonce input (util/read-lines "input/20.txt"))

(defn surroundings [m [i j]]
  (into {}
        [[[(dec i) j] (get-in m [(dec i) j])]
         [[(inc i) j] (get-in m [(inc i) j])]
         [[i (dec j)] (get-in m [i (dec j)])]
         [[i (inc j)] (get-in m [i (inc j)])]]))

(defn accessible-portals [m from]
  (->> m
       (filter (fn [[[a b] dist]] (= from a)))
       (map (comp second first))))

(defn calc-portals [m]
  (remove
   nil?
   (for [i (range (count m))
         j (range (count (first m)))
         :let [current (get-in m [i j])
               up [(get-in m [(dec i) j]) (get-in m [(dec (dec i)) j])]
               down [(get-in m [(inc i) j]) (get-in m [(inc (inc i)) j])]
               left [(get-in m [i (dec j)]) (get-in m [i (dec (dec j))])]
               right [(get-in m [i (inc j)]) (get-in m [i (inc (inc j))])]]
         :when (re-find #"[A-Z]" (str current))]
     (cond
       (and (re-find #"[A-Z]" (str (first up))) (= \. (second up))) 
       [[(first up) current] [(dec (dec i)) j]]

       (and (re-find #"[A-Z]" (str (first down))) (= \. (second down))) 
       [[current (first down)] [(inc (inc i)) j]]

       (and (re-find #"[A-Z]" (str (first left))) (= \. (second left))) 
       [[(first left) current] [i (dec (dec j))]]

       (and (re-find #"[A-Z]" (str (first right))) (= \. (second right))) 
       [[current (first right)] [i (inc (inc j))]]))))

(defn dijk [m from to]
  (util/dijkstra m {:start (second from)
                    :done? #(= % (second to))
                    :parse-final first
                    :cost (constantly 1)
                    :log (constantly nil)
                    :neighbors-fn
                    (fn [current]
                      (when current
                        (keys (util/filter-vals #(= % \.) (surroundings m current)))))}))

(defn dijk-portals [m from to]
  (util/dijkstra m {:start from 
                    :done? #(= % to)
                    :parse-final identity
                    :cost (fn [current next]
                            (get m [current next]))
                    :log (constantly nil)
                    :neighbors-fn #(accessible-portals m %)}))

(defn connect-portals [m portals]
  (let [groups (for [[_ group] (group-by first portals)
                     :when (> (count group) 1)]
                 group)]
    (reduce
     (fn [m [l r]]
       (-> m
           (assoc [l r] 0)
           (assoc [r l] 0)))
     m
     groups)))

(defn find-portal [portals target]
  (first (for [portal portals
               :when (= (first portal) target)]
           portal)))

(defn step-count [m path]
  (dec (reduce + (for [[from to] (partition 2 1 path)]
                   (get m [from to])))))

(defn precompute [m]
  (let [portals (calc-portals m)
        pairs (remove (fn [[x y]] (= x y)) (c/selections portals 2))
        portal-map (into {} (for [[from to] pairs
                                  :let [path (dijk m from to)]
                                  :when (not= path :FAILED)]
                              [[from to] (count path)]))
        portal-map (connect-portals portal-map portals)
        [path _] (dijk-portals portal-map
                               (find-portal portals [\A \A])
                               (find-portal portals [\Z \Z]))]
    (step-count portal-map (reverse path))))
