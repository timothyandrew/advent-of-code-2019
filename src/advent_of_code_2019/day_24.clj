(ns advent-of-code-2019.day-24
  (:require [advent-of-code-2019.util :as util]
            [clojure.set :as set]))

(def input
  ["..#.#"
   ".#.##"
   "..X#."
   "...##"
   "#.###"])

(def example-1
  ["....#"
   "#..#."
   "#..##"
   "..#.."
   "#...."])

(def example-2
  ["....."
   "....."
   "....."
   "#...."
   ".#..."])

(defn fresh []
  ["....."
   "....."
   "..X.."
   "....."
   "....."])

(defn count-adj [m [i j]]
  (count
   (filter #(= % \#)
           [(get-in m [(dec i) j])
            (get-in m [(inc i) j])
            (get-in m [i (dec j)])
            (get-in m [i (inc j)])])))

(defn count-adj-z [m [z i j]]
  (->> (cond-> #{[z (dec i) j]
                 [z (inc i) j]
                 [z i (dec j)]
                 [z i (inc j)]}

         (= i 0) (conj [(dec z) 1 2])
         (= j 0) (conj [(dec z) 2 1])
         (= i 4) (conj [(dec z) 3 2])
         (= j 4) (conj [(dec z) 2 3])

         (and (= i 1) (= j 2)) (set/union (set (for [n (range 5)] [(inc z) 0 n])))
         (and (= i 3) (= j 2)) (set/union (set (for [n (range 5)] [(inc z) 4 n])))
         (and (= i 2) (= j 1)) (set/union (set (for [n (range 5)] [(inc z) n 0])))
         (and (= i 2) (= j 3)) (set/union (set (for [n (range 5)] [(inc z) n 4]))))

       (remove #(= [(nth % 1) (nth % 2)] [2 2]))
       (map #(get-in m %))
       (filter #(= % \#))
       count))

(defn v [c]
  (->> c
       (partition 5)
       (map vec)
       vec))

(defn vz [c]
  (->> c
       (group-by first)
       (util/map-vals #(mapv second %))
       (util/map-vals #(vec (map vec (partition 5 %))))))

(defn rating [m]
  (reduce
   #(.add %1 %2)
   (for [i (range 5) j (range 5)
         :let [cell (get-in m [i j])]
         :when (= cell \#)]
     (.pow (biginteger 2) (biginteger (+ j (* i 5)))))))

(defn tick [m]
  (let [adj-m (v (for [i (range 5) j (range 5)]
                   (count-adj m [i j])))]
    (v
     (for [i (range 5)
           j (range 5)
           :let [cell (get-in m [i j])
                 adj (get-in adj-m [i j])]]
       (cond
         (and (= cell \#) (= adj 1)) \#
         (and (= cell \#) (not= adj 1)) \.
         (and (= cell \.) (contains? #{1 2} adj)) \#
         :else cell)))))

(defn pad-z [m]
  (let [min-k (apply min (keys m))
        max-k (apply max (keys m))]
    (-> m
        (assoc (dec min-k) (fresh))
        (assoc (inc max-k) (fresh)))))

(defn tick-z [m]
  (println "TICKING")
  (let [m (pad-z m)]
    (let [adj-z (vz
                 (for [z (keys m) i (range 5) j (range 5)]
                   [z (count-adj-z m [z i j])]))]
      (vz
       (for [z (keys m) i (range 5) j (range 5)
             :let [cell (get-in m [z i j])
                   adj (get-in adj-z [z i j])]]
         [z 
          (cond
            (and (= cell \#) (not= adj 1)) \.
            (and (= cell \.) (contains? #{1 2} adj)) \#
            :else cell)])))))

(defn count-bugs [m]
  (->> m
       vals
       flatten
       (filter #(= % \#))
       count))

(defn day-24-1 []
  (loop [seen #{} m input]
    (let [next (tick m)]
      (if (contains? seen next)
        (rating next)
        (recur (conj seen next) next)))))

(defn day-24-2 []
  (count-bugs
   (reduce
    (fn [m _] (tick-z m))
    {0 input}
    (range 200))))
