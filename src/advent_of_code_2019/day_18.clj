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

(defn cell [map [i j]]
  (get-in map [i j]))

(defn traverse [map f]
  (for [i (range (count map))
        j (range (count (first map)))
        :let [current (cell map [i j])]
        :when (f [i j])]
    [i j]))

(defn map-find [map a]
  (first (traverse map #(= a (get-in map %)))))

(defn pp [m]
  (doall (map println m)))

(defn surroundings [m [i j]]
  (into {}
        [[[(dec i) j] (get-in m [(dec i) j])]
         [[(inc i) j] (get-in m [(inc i) j])]
         [[i (dec j)] (get-in m [i (dec j)])]
         [[i (inc j)] (get-in m [i (inc j)])]]))

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

(defn parse-path [[full-path filtered-path]]
  {:keys (filterv #(re-find #"[a-z]" (str %)) filtered-path)
   :doors (filterv #(re-find #"[A-Z]" (str %)) filtered-path)
   :dist (dec (count full-path))})

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

(defn dijk-precompute [m from to]
  (dijkstra
   m
   {:start from 
    :neighbors-fn (fn [current] (keys (util/filter-vals #(not= % \#) (surroundings m current))))
    :done? #(= to %)
    :parse-final parse-path
    :cost (constantly 1)
    :log (constantly nil)}))

(defn dijk-traversal [m {:keys [doors dist keyref map-lookup] :as state}]
  (dijkstra
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

(defn day-18-1 [m]
  (loop []
    (let [state (precompute m)
          _ (def dist (:dist state)) 
          _ (println "PRECOMPUTED")
          path (dijkstra-2 m state)]
      (if (= path :FAILED)
        :FAILED
        [(calc-answer m state path)
         path]))))
