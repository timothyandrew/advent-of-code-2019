(ns advent-of-code-2019.day-18
  (:require [advent-of-code-2019.util :as util]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as c])
  (:import [java.util PriorityQueue HashMap]))

(defonce example-1 (util/read-lines "input/18-1.txt"))
(defonce example-2 (util/read-lines "input/18-2.txt"))
(defonce example-3 (util/read-lines "input/18-3.txt"))
(def input (util/read-lines "input/18.txt"))

(defn cell [map [i j]]
  (get-in map [i j]))

(defn map-get [m cell]
  (get-in m cell))

(defn traverse [map f]
  (for [i (range (count map))
        j (range (count (first map)))
        :let [current (cell map [i j])]
        :when (f [i j])]
    [i j]))

(defn remove-from-map [m c]
  (->> (for [i (range (count m))
             j (range (count (first m)))
             :let [current (cell m [i j])]]
         (if (= current c) \. current))
       (partition (count (first m)))
       (mapv str/join)))

(defn find-start [map]
  (first (traverse map #(= \@ (get-in map %)))))

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
   :dist (count full-path)})

(defn dijkstra [m axy bxy]
  (let [cost-so-far (atom {axy 0}) came-from (atom {})
        c-fn (comparator (fn [pxy qxy] (< (get @cost-so-far pxy) (get @cost-so-far qxy))))
        frontier (PriorityQueue. c-fn)]
    (.add frontier axy)
    (loop []
      (let [current (.poll frontier)
            neighbors (util/filter-vals #(not= % \#) (surroundings m current))]
        (if (= current bxy)
          (parse-path (resolve-path m axy bxy @came-from))
          (do
            (doseq [next (keys neighbors)
                    :let [cost (+ (get @cost-so-far current) 0)]
                    :when (or (not (get @cost-so-far next))
                              (< cost (get @cost-so-far next)))]
              (swap! cost-so-far assoc next cost)
              (swap! came-from assoc next current)
              (.add frontier next))
            (recur)))))))


(defn accessible-keys [m state from]
  (->> state
       (filter (fn [[k v]] (and
                            (not= (second k) \@)
                            (= (first k) (get-in m from)))))
       (filter (fn [[k v]] (empty? (:doors v))))
       (map (comp second first))))

(defn dfs
  ([m] (apply min (dfs (remove-from-map m \@) (find-start m) 0 [])))
  ([m pos dist ks]
   (let [[k->pos k->dist] (accessible-keys m pos)]
     (if (empty? k->pos) 
       dist
       (flatten
        (for [k (keys k->pos)]
          (dfs
           (-> m (remove-from-map k) (remove-from-map (first (str/upper-case k))))
           (get k->pos k)
           (+ dist (get k->dist k))
           (conj ks k))))))))

(defn precompute [m]
  (let [start (map-find m \@)
        ks (cons \@ (filter #(re-find #"[a-z]" (str %)) (str/join m)))
        pairs (remove #(apply = %) (map vec (c/selections ks 2)))]
    {:keycount (count ks)
     :map-lookup (zipmap ks (map #(map-find m %) ks))
     :state
     (zipmap
      pairs
      (map #(dijkstra
             m
             (map-find m (first %))
             (map-find m (second %))) pairs))}))

(defn update-state [state keys-collected]
  (util/map-vals
   (fn [v]
     (-> v
         (update :keys (fn [k] (remove #(contains? keys-collected %) k)))
         (update :doors (fn [k] (remove #(contains? keys-collected (first (str/lower-case %))) k)))))
   state))

(defn calc-answer [m {:keys [state]} path]
  (->> path
       (partition 2 1)
       (map #(get-in state [% :dist]))
       (map dec)
       (reduce +)))

(defn dijkstra-2 [m {:keys [keycount map-lookup state]}]
  (println map-lookup keycount)
  (let [start [(map-find m \@) #{}]
        cost-so-far (atom {start 0}) came-from (atom {})
        c-fn (comparator (fn [pxy qxy]  (< (get @cost-so-far pxy) (get @cost-so-far qxy))))
        frontier (PriorityQueue. c-fn)]
    (.add frontier start)
    (loop [i 0]
      (let [current (.poll frontier)
            keys-so-far (second current)
            state (update-state state (set (map #(get-in m %) keys-so-far)))
            neighbors (map #(vector % (conj keys-so-far %))
                           (map #(get map-lookup %) (accessible-keys m state (first current))))]
        (when (= (mod i 1000) 0)
          (println i (count keys-so-far)))

        (cond
          (= (dec keycount) (count keys-so-far))  
          (vec (reverse (map #(get-in m %) (map first (first (resolve-path m start current @came-from))))))

          (nil? current)
          :FAILED
          
          :else
          (do
            (doseq [next neighbors 
                    :let [cost (+ (get @cost-so-far current)
                                  (get-in state [[(get-in m (first current)) (get-in m (first next))] :dist])
                                  (- (count (get-in state [[(get-in m (first current)) (get-in m (first next))] :keys]))))]
                    :when (or (not (get @cost-so-far next))
                              (< cost (get @cost-so-far next)))]
              (swap! cost-so-far assoc next cost)
              (swap! came-from assoc next current)
              (.add frontier next))
            (recur (inc i))))))))

(defn day-18-1 [m]
  (loop []
    (let [state (precompute m)
          _ (println "PRECOMPUTED")
          path (dijkstra-2 m state)]
      (if (= path :FAILED)
        :FAILED
        (calc-answer m state path)))))
