(ns advent-of-code-2019.day-12
  (:require [clojure.math.combinatorics :as c]))

(defn moon [name x y z]
  {:position [x y z]
   :name name
   :velocity [0 0 0]})

(defn array-+ [a1 a2]
  (->> [a1 a2]
       (apply interleave)
       (partition 2)
       (map #(reduce + %))))

(defn start-world []
  (into {} [[:io (moon :io 3 15 8)]
            [:europa (moon :europa 5 -1 -2)]
            [:ganymede (moon :ganymede -10 8 2)]
            [:callisto (moon :callisto 8 4 -5)]]))

(defn example-1 []
  (into {} [[:io (moon :io -1 0 2)]
            [:europa (moon :europa 2 -10 -7)]
            [:ganymede (moon :ganymede 4 -8 8)]
            [:callisto (moon :callisto 3 5 -1)]]))

(defn example-2 []
  (into {} [[:io (moon :io -8 -10 0)]
            [:europa (moon :europa 5 5 10)]
            [:ganymede (moon :ganymede 2 -7 3)]
            [:callisto (moon :callisto 9 -8 -3)]]))

(defn test-inp [a b c d]
  (-> (map first (map :velocity (vals (apply-gravity (into {} [[:io (moon :io a -10 0)]
                                                               [:europa (moon :europa b 5 10)]
                                                               [:ganymede (moon :ganymede c -7 3)]
                                                               [:callisto (moon :callisto d -8 -3)]])))))

      ))

(defn apply-gravity
  ([world]
   (reduce
    (fn [world moon-names]
      (let [[moon1 moon2] (map #(get world %) moon-names)
            [moon1 moon2] (apply-gravity moon1 moon2)]
        (-> world
            (assoc (:name moon1) moon1)
            (assoc (:name moon2) moon2))))
    world
    (c/combinations (map :name (vals world)) 2)))
  ([moon1 moon2] 
   (letfn [(gravity-diff [a b]
             (cond
               (> a b) -1
               (< a b) 1
               (= a b) 0))]
     (let [[[x1 y1 z1] [x2 y2 z2]] (map :position [moon1 moon2])]
       [(-> moon1
            (update-in [:velocity 0] + (gravity-diff x1 x2))
            (update-in [:velocity 1] + (gravity-diff y1 y2))
            (update-in [:velocity 2] + (gravity-diff z1 z2)))
        (-> moon2
            (update-in [:velocity 0] + (gravity-diff x2 x1))
            (update-in [:velocity 1] + (gravity-diff y2 y1))
            (update-in [:velocity 2] + (gravity-diff z2 z1)))]))))

(defn apply-velocity [world]
  (into {}
    (for [[name {:keys [position velocity] :as moon}] world]
      [name (update moon :position array-+ velocity)])))

(defn total-energy [world]
  (->> (for [[_ {:keys [position velocity]}] world]
        (let [[px py pz] (map #(Math/abs %) position)
              [vx vy vz] (map #(Math/abs %) velocity)]
          (* (+ px py pz)
             (+ vx vy vz))))
      (reduce +)))

(defn tick [world n]
  (reduce
   (fn [world _]
     (-> world apply-gravity apply-velocity))
   world
   (range n)))

