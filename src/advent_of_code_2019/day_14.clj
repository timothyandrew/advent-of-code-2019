(ns advent-of-code-2019.day-14
  (:require [clojure.set :as set]
            [advent-of-code-2019.util :as util]
            [clojure.string :as s]))

(def example-1
  {:A {:count 10 :needs {:ORE 10}}
   :B {:count 1 :needs {:ORE 1}}
   :C {:count 1 :needs {:A 7 :B 1}}
   :D {:count 1 :needs {:A 7 :C 1}}
   :E {:count 1 :needs {:A 7 :D 1}}
   :FUEL {:count 1 :needs {:A 7 :E 1}}})

(def example-2
  {:A {:count 2 :needs {:ORE 9}}
   :B {:count 3 :needs {:ORE 8}}
   :C {:count 5 :needs {:ORE 7}}
   :AB {:count 1 :needs {:A 3 :B 4}}
   :BC {:count 1 :needs {:B 5 :C 7}}
   :CA {:count 1 :needs {:C 4 :A 1}}
   :FUEL {:count 1 :needs {:AB 2 :BC 3 :CA 4}}})

(defn parse-input [file]
  (into {} (for [line (util/read-lines file)]
             (let [[l r] (s/split line #" => ")
                   needs (map #(s/split % #" ") (s/split l #", "))
                   [target-count target] (s/split r #" ")
                   needs (map (fn [[n1 n2]] (vector (keyword n2) (Integer/parseInt n1) )) needs)]
               [(keyword target)
                { :count (Integer/parseInt target-count) :needs (into {} needs) }]))))

(defn done? [needs]
  (and
   (every? #(<= % 0) (vals (dissoc needs :ORE)))))

(defn matching-eqn [equations c]
  (when (not= c :ORE)
    (get equations c)))

(defn replacement [equations all-needs [chemical n]]
  (let [{:keys [count needs]} (matching-eqn equations chemical)
        factor (int (Math/ceil (/ n count)))
        needs (into {} (map (fn [[k v]] [k (* v factor)]) needs))]
    (cond
      (= 0 (mod n count)) (merge-with + (dissoc all-needs chemical) needs)
      (> count n) (merge-with + (assoc all-needs chemical (- n count)) needs) 
      (< count n) (merge-with + (assoc all-needs chemical (- n (* count factor))) needs))))

(defn pick-need [all-needs]
  (first (filter (fn [[k v]]
                   (and
                    (not= k :ORE)
                    (> v 0)))
                 all-needs)))

(defn day-14-1 [equations]
  (let [all-needs (get-in equations [:FUEL :needs])]
    (loop [i 0 needs all-needs]
      (if (done? needs)
        needs
        (recur (inc i) (replacement equations needs (pick-need needs)))))))
