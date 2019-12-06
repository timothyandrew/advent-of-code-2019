(ns advent-of-code-2019.day-6
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def input-1 (edn/read-string (slurp (io/resource "input/6.edn"))))

(defn build-adj [pairs]
  (reduce
   (fn [memo [l r]]
     (update memo r conj l))
   {}
   pairs))

(defn resolve-adj-with-distances
  ([adj roots dist]
   (let [roots (map #(vec [% dist]) (remove nil? roots))]
     (if (and roots (not (empty? roots)))
       (concat roots (resolve-adj-with-distances adj (flatten (map #(get adj (first %)) roots)) (inc dist))))))
  ([adj]
   (for [[k v] adj]
     (let [direct (set v)
           indirect (set/difference (set (resolve-adj-with-distances adj v 0)) direct)]
       [k {:direct direct :indirect indirect}]))))

(defn resolve-adj
  ([adj roots]
   (let [roots (remove nil? roots)]
     (if (and roots (not (empty? roots)))
       (concat roots (resolve-adj adj (flatten (map #(get adj %) roots)))))))
  ([adj]
   (for [[k v] adj]
     (let [direct (set v)
           indirect (set/difference (set (resolve-adj adj v)) direct)]
       [k {:direct direct :indirect indirect}]))))

(defn find-nearest-branch [adj l r]
  (let [l-adj (into {} (:indirect (get adj l)))
        r-adj (into {} (:indirect (get adj r)))
        reachable (set/intersection (set (keys l-adj)) (set (keys r-adj)))
        branch (apply min-key #(+ (get l-adj %) (get r-adj %)) reachable)]
    (+ (get l-adj branch) (get r-adj branch))))

(defn day-6-1 [in]
  (->> in
      build-adj
      resolve-adj
      (map second)
      (map vals)
      flatten
      (map count)
      (reduce +)))

(defn day-6-2 [in]
  (let [adj (->> in
                 build-adj
                 resolve-adj-with-distances
                 (into {}))]
    (find-nearest-branch adj :YOU :SAN)))
