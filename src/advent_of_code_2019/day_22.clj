(ns advent-of-code-2019.day-22
  (:require [advent-of-code-2019.util :as util]))

(def input (util/read-lines "input/22.txt"))

(defn deal-with-increment [increment deck]
  (loop [deck deck pos 0 final deck]
    (if-let [item (first deck)]
      (recur
       (next deck)
       (mod (+ pos increment) (count final))
       (assoc final pos item))
      final)))

(defn cut [n deck]
  (if (>= n 0)
    (concat (drop n deck) (take n deck))
    (concat (take-last (- n) deck) (drop-last (- n) deck))))

(defn parse
  ([instruction]
   (first
    (remove
     nil?
     [(when-let [[_ increment] (re-find #"deal with increment (\d+)" instruction)]
        (partial deal-with-increment (Integer/parseInt increment)))

      (when-let [[_ n] (re-find #"cut (\-?\d+)" instruction)]
        (partial cut (Integer/parseInt n)))

      (when-let [_ (re-find #"deal into new stack" instruction)]
        reverse)])))
  ([instructions deck]
   (loop [deck (vec deck) instructions instructions]
     (if-let [i (first instructions)]
       (recur (vec ((parse i) deck)) (next instructions))
       deck))))

(defn reverse-deal-with-increment [increment deck-size index]
  (->> (range increment)
       (map #(-> deck-size (* %) (+ index) (/ increment)))
       (filter #(not (ratio? %)))
       first))

(defn reverse-deal-new-stack [deck-size index]
  (dec (- deck-size index)))

(defn reverse-cut [n deck-size index]
  (if (>= n 0)
    (mod (+ index n) deck-size)
    (mod (+ index (+ deck-size n)) deck-size)))

(defn reverse-parse
  ([instruction]
   (first
    (remove
     nil?
     [(when-let [[_ increment] (re-find #"deal with increment (\d+)" instruction)]
        (partial reverse-deal-with-increment (Integer/parseInt increment)))

      (when-let [[_ n] (re-find #"cut (\-?\d+)" instruction)]
        (partial reverse-cut (Integer/parseInt n)))

      (when-let [_ (re-find #"deal into new stack" instruction)]
        reverse-deal-new-stack)])))
  ([instructions deck-size target-index]
   (loop [index target-index instructions instructions]
     (if-let [i (first instructions)]
       (recur
        ((reverse-parse i) deck-size index)
        (next instructions))
       index))))

(defn single [n size]
  (-> (bigint n)
      (* 24)
      (+ 9655)
      (* 20)
      (+ 3052)
      (* 14)
      (inc) (-)
      (* 12)
      (- 2041)
      (inc) (-)
      (* 13)
      (+ 5574)
      (inc) (-)
      (* 52)
      (- 2735)
      (* 14)
      (inc) (-)
      (* 72)
      (inc) (-)
      (* 11)
      (+ 7008)
      (* 7)
      (+ 3920)
      (inc) (-)
      (* 68)
      (+ 7497)
      (* 7)
      (- 8878)
      (* 39)
      (+ 3407)
      (* 74)
      (+ 3728)
      (inc) (-)
      (- 483)
      (* 55)
      (- 8147)
      (* 48)
      (- 5734)
      (* 35)
      (inc) (-)
      (* 53)
      (inc) (-)
      (- 9833)
      (* 21)
      (+ 1328)
      (* 29)
      (- 469)
      (* 34)
      (inc) (-)
      (* 50)
      (- 8218)
      (* 8)
      (- 1546)
      (* 27)
      (- 3699)
      (* 44)
      (- 1167)
      (inc) (-)
      (+ 9744)
      (* 71)
      (+ 6111)
      (* 19)
      (- 2592)
      (* 17)
      (- 3257)
      (* 11)
      (- 4618)
      (* 64)
      (inc) (-)
      (+ 1513)
      (inc) (-)
      (+ 2976)
      (* 58)
      (- 2744)
      (* 4)
      (- 6408)
      (* 66)
      (- 5182)
      (* 6)
      (+ 1767)
      (* 12)
      (+ 7805)
      (* 45)
      (+ 4126)
      (* 52)
      (- 2112)
      (* 35)
      (- 863)
      (* 55)
      (- 3159)
      (* 67)
      (inc) (-)
      (* 65)
      (+ 3194)
      (* 69)
      (- 5695)
      (* 7)
      (+ 5035)
      (* 30)
      (+ 2282)
      (* 70)
      ))

(defn fast-single [n size]
  (-> n
      (* 99190791877526252471032425539306547829895160953988710400000000000N)
      (+ 40537373627240065825057057808820766757603319284954939820348986547440N)
      (mod size)))


(defn day-22-1 []
  (.indexOf
   (vec (parse input (vec (range 10007))))
   2019))

(def a (biginteger 99190791877526252471032425539306547829895160953988710400000000000))
(def b (biginteger 40537373627240065825057057808820766757603319284954939820348986547440))
(def p (biginteger 101741582076661))
(def q (biginteger 119315717514047))
;; (def p (biginteger 3))
;; (def q (biginteger 10007))

(defn simplify [base power]
  (if (zero? power)
    (biginteger 0)
    (if (odd? power)
      (.add
       (.modPow base (.subtract power (biginteger 1)) q)
       (simplify base (.subtract power (biginteger 1))))
      (.multiply
       (.add (.modPow base (.divide power (biginteger 2)) q) (biginteger 1))
       (simplify base (.divide power (biginteger 2)))))))

(defn solve [x]
  (mod
   (.add
    (.multiply (.modPow a p q) (biginteger x))
    (.multiply b (simplify a p)))
   q))

(defn solve-rev [final]
  (mod
   (.multiply
    (.subtract (biginteger final) (.multiply b (simplify a p)))
    (.modInverse (.modPow a p q) q))
   q))
