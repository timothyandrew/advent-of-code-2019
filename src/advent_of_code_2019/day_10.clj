(ns advent-of-code-2019.day-10
  (:require [clojure.set :as set]
            [clojure.string :as s]))

(defn parse-grid [grid]
  (->>
   (s/split grid #"\n")
   (map-indexed
        (fn [i row]
          (map-indexed (fn [j char]
                         (when (= char \#)
                           [j i]))
                       row)))
  (apply concat) 
  (remove nil?)
  set))

(defn preserve-sign [n c]
  (let [n (int n)
        c (int c)]
      (if (< c 0)
        (- (Math/abs n))
        (Math/abs n))))

(defn simplify [a b]
  (let [ratio (/ a b)]
    (cond
      (ratio? ratio) [(preserve-sign (numerator ratio) a)
                      (preserve-sign (denominator ratio) b)]
      :else [(preserve-sign ratio a)
             (preserve-sign 1 b)])))

(defn ->1 [n]
  (cond
    (> n 0) 1
    (< n 0) -1))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn navigate [[x y] [move-x move-y]]
  [(+ x move-x) (+ y move-y)])

(defn navigate-infinitely [start [move-x move-y] [max-x max-y]]
  (let [[move-x move-y] (cond
                          (= move-x 0) [0 (->1 move-y)]
                          (= move-y 0) [(->1 move-x) 0]
                          :else (simplify move-x move-y))]
      (loop [waypoints []
             [x y] start]
        (if (or (< x 0) (< y 0) (> x max-x) (> y max-y))
          waypoints
          (recur (conj waypoints [x y])
                 (navigate [x y] [move-x move-y]))))))

(defn unblocked-asteroids [viewport all-asteroids]
  (loop [blocked #{} asteroids (disj all-asteroids viewport)]
    (if asteroids
      (let [[_ & blocks]
            (navigate-infinitely
             (first asteroids)
             (manhattan-distance viewport (first asteroids))
             [(apply max (map first all-asteroids))
              (apply max (map second all-asteroids))])]
        (recur (set/union blocked (set blocks)) (next asteroids)))
      (disj (set/difference all-asteroids blocked) viewport))))

(defn angle [[x1 y1] [x2 y2]]
  (let [result (Math/toDegrees (Math/atan2 (- x2 x1) (- y1 y2)))]
    (if (< result 0)
      (+ 360 result)
      result)))

(defn vaporize [viewport unblocked]
  (sort-by #(angle viewport %) unblocked))

(defn day-10-1 [in]
  (->> in
       (map #(vec [% (count (unblocked-asteroids % in))]))
       (apply max-key second)))

(defn day-10-2
  ([in] (day-10-2 in (first (day-10-1 in))))
  ([in viewport]
   (loop [vaporized [] all in]
     (cond
       (> (count vaporized) 200) (nth vaporized (dec 200))
       (empty? all) (last vaporized)
       :else
       (let [unblocked (unblocked-asteroids viewport all)]
         (recur
          (concat vaporized (vaporize viewport unblocked))
          (set/difference all unblocked)))))))
