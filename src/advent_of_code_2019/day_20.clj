(ns advent-of-code-2019.day-20
  (:require [advent-of-code-2019.util :as util]
            [clojure.math.combinatorics :as c]
            [clojure.set :as set]))

(defonce example-1 (util/read-lines "input/20-0.txt"))
(defonce example-2 (util/read-lines "input/20-1.txt"))
(defonce example-3 (util/read-lines "input/20part2.txt"))
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

(defn level-0-portals [m from]
  (->> m
       (filter (fn [[[a b] dist]]
                 (and (= (take 3 from) a)
                      (or
                       (= [\Z \Z] (first b))
                       (= [\A \A] (first b))
                       (= (nth b 2) :inner)))))
       (map (comp second first))
       (map #(conj % 0))))


(defn find-inner-portal [m portal]
  (ffirst
   (first
    (filter (fn [[[a b] dist]]
              (and
               (= (first a) portal)
               (= (nth a 2) :inner)))
            m))))

(defn find-outer-portal [m portal]
  (ffirst
   (first
    (filter (fn [[[a b] dist]]
              (and
               (= (first a) portal)
               (= (nth a 2) :outer)))
            m))))

(defn opposite-portal [m [portal coord type level]]
  (if (= type :inner)
    (when-let [outer (find-outer-portal m portal)]
      (conj outer (inc level)))
    (when-let [inner (find-inner-portal m portal)]
      (conj inner (dec level)))))

(defn accessible-portals-recursive [m [portal coord type level :as from]]
  (if (= level 0)
    (remove nil? (conj
                  (set (level-0-portals m from))
                  (opposite-portal m from)))
    (remove nil? (conj
                  (set (map #(conj % level) (->> m
                                                 (filter (fn [[[a b] dist]] (= (take 3 from) a)))
                                                 (map (comp second first)))))
                  (opposite-portal m from)))))

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
       (if (= i (dec (count m)))
         [[(first up) current] [(dec (dec i)) j] :outer]
         [[(first up) current] [(dec (dec i)) j] :inner])

       (and (re-find #"[A-Z]" (str (first down))) (= \. (second down))) 
       (if (= i 0)
         [[current (first down)] [(inc (inc i)) j] :outer]
         [[current (first down)] [(inc (inc i)) j] :inner])

       (and (re-find #"[A-Z]" (str (first left))) (= \. (second left))) 
       (if (= j (dec (count (first m))))
         [[(first left) current] [i (dec (dec j))] :outer]
         [[(first left) current] [i (dec (dec j))] :inner]
         )

       (and (re-find #"[A-Z]" (str (first right))) (= \. (second right))) 
       (if (= j 0)
         [[current (first right)] [i (inc (inc j))] :outer]
         [[current (first right)] [i (inc (inc j))] :inner])))))

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


(defn dijk-recursive-portals [m from to]
  (util/dijkstra m {:start (conj from 0) 
                    :done? #(= % (conj to 0))
                    :parse-final identity
                    :cost (fn [[p1 c1 t1 l1] [p2 c2 t2 l2]]
                            (if (and (= p1 p2) (not= t1 t2))
                              0
                              (get m [[p1 c1 t1] [p2 c2 t2]])))
                    :log (constantly nil)
                    :neighbors-fn (fn [current]
                                    (let [acc (accessible-portals-recursive m current)]
                                      acc))}))


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
                   (if (= (first from) (first to))
                     0
                     (get m [from to]))))))

(defn day-20-1 [m]
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

(defn day-20-2 [m]
  (let [portals (calc-portals m) 
        pairs (remove (fn [[x y]] (= x y)) (c/selections portals 2))
        portal-map (into {} (for [[from to] pairs
                                  :let [path (dijk m from to)]
                                  :when (not= path :FAILED)]
                              [[from to] (count path)]))
        _ (def x portal-map)
        [path _] (dijk-recursive-portals portal-map
                                         (find-portal portals [\A \A])
                                         (find-portal portals [\Z \Z]))]
    (->>
     path
     reverse
     (map #(take 3 %))
     (step-count portal-map))))
