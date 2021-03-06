(ns advent-of-code-2019.day-18
  (:require [advent-of-code-2019.util :as util]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as c])
  (:import [java.util PriorityQueue HashMap]))

(defonce example-0 (util/read-lines "input/18-0.txt"))
(defonce example-1 (util/read-lines "input/18-1.txt"))
(defonce example-2 (util/read-lines "input/18-2.txt"))
(defonce example-3 (util/read-lines "input/18-3.txt"))
(def input (util/read-lines "input/18.txt"))
(def input2 (util/read-lines "input/18part2.txt"))

(defn cell [map [i j]]
  (get-in map [i j]))

(defn traverse [map f]
  (for [i (range (count map))
        j (range (count (first map)))
        :let [current (cell map [i j])]
        :when (f [i j])]
    [i j]))

(defn map-find-all [map a]
  (traverse map #(= a (get-in map %))))

(defn map-find [map a]
  (first (map-find-all map a)))

(defn pp [m]
  (doall (map println m)))

(defn surroundings [m [i j]]
  (into {}
        [[[(dec i) j] (get-in m [(dec i) j])]
         [[(inc i) j] (get-in m [(inc i) j])]
         [[i (dec j)] (get-in m [i (dec j)])]
         [[i (inc j)] (get-in m [i (inc j)])]]))

(defn accessible-keys [m doors keys-so-far from]
  (->> doors
       (filter (fn [[k v]] (and
                            (not= (second k) \@)
                            (not (contains? keys-so-far (second k)))
                            (= (first k) (get-in m from)))))
       (filter (fn [[k v]] (empty? (set/difference v keys-so-far))))
       (map (comp second first))))

(defn calc-answer [m {:keys [dist]} path]
  (->> path
       (partition 2 1)
       (map #(get dist %))
       (reduce +)))


(defn parse-path [[full-path filtered-path]]
  {:keys (filterv #(re-find #"[a-z]" (str %)) filtered-path)
   :doors (filterv #(re-find #"[A-Z]" (str %)) filtered-path)
   :dist (dec (count full-path))})

(defn dijk-precompute [m from to]
  (util/dijkstra
   m
   {:start from 
    :neighbors-fn (fn [current] (keys (util/filter-vals #(not= % \#) (surroundings m current))))
    :done? #(= to %)
    :parse-final parse-path
    :cost (constantly 1)
    :log (constantly nil)}))

(defn dijk-part2 [m from to]
  (util/dijkstra
   m
   {:start from 
    :neighbors-fn (fn [current] (keys (util/filter-vals #(not= % \#) (surroundings m current))))
    :done? #(= to %)
    :parse-final identity
    :cost (constantly 1)
    :log (constantly nil)}))

(defn dijk-traversal [m {:keys [doors dist keyref map-lookup] :as state}]
  (util/dijkstra
   m
   {:start [(map-find m \@) #{}]

    :neighbors-fn
    (fn [[current keys-so-far]]
      (let [accessible (accessible-keys m doors (set (map #(get-in m %) keys-so-far)) current)]
        (map #(vector % (conj keys-so-far %)) (map #(get map-lookup %) accessible))))

    :done?
    (fn [[current keys-so-far]]
      (= (dec (count keyref)) (count keys-so-far)))

    :parse-final
    (fn [[full filtered]] (vec (reverse (map #(get-in m %) (map first full)))))

    :cost
    (fn [current next]
      (get dist [(get-in m (first current)) (get-in m (first next))]))

    :log
    (fn [i [current keys-so-far]]
      (println i (count keys-so-far)))}))

(defn precompute [m]
  (let [start (map-find m \@)
        ks (cons \@ (filter #(re-find #"[a-z]" (str %)) (str/join m)))
        pairs (remove #(apply = %) (map vec (c/selections ks 2)))
        state (zipmap
               pairs
               (map #(dijk-precompute
                      m
                      (map-find m (first %))
                      (map-find m (second %))) pairs))]
    {:keyref (into {} (map vec (partition 2 (interleave ks (range))))) 
     :map-lookup (zipmap ks (map #(map-find m %) ks))
     :doors (util/map-vals (fn [x] (set (map #(first (str/lower-case %)) (:doors x)))) state)
     :dist (util/map-vals :dist state)}))

(defn which-vault? [m [i j]]
  (cond
    (and (< i 40) (< j 40)) 0
    (and (< i 40) (> j 40)) 1
    (and (> i 40) (> j 40)) 2
    (and (> i 40) (< j 40)) 3))

(defn calc-part2-answer [path]
  (let [m input2
        state (precompute input)
        robots (mapv #(map-find m (first (str %))) [1 2 3 4])
        path (drop 1 (mapv #(get (:map-lookup state) %) path))]
    (loop [dist 0 path path robots robots]
      (if (empty? path)
        dist
        (let [current (first path)
              vault (which-vault? m current)
              robot (nth robots vault)
              move-to (dijk-part2 m robot current)
              new-dist (count (first move-to))]
          (recur (+ dist (dec new-dist))
                 (next path)
                 (assoc robots vault current)))))))

(defn day-18-1 [m]
  (loop []
    (let [state (precompute m)
          _ (println "PRECOMPUTED")
          path (dijk-traversal m state)]
      (if (= path :FAILED)
        :FAILED
        [(calc-answer m state path)
         path]))))
