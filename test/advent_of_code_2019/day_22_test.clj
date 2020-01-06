(ns advent-of-code-2019.day-22-test
  (:require [advent-of-code-2019.day-22 :refer :all]
            [clojure.test :refer :all]))

(deftest test-cut
  (is (= [3 4 5 6 7 8 9 0 1 2]
         (cut 3 [0 1 2 3 4 5 6 7 8 9])))

  (is (= [6 7 8 9 0 1 2 3 4 5]
         (cut -4 [0 1 2 3 4 5 6 7 8 9]))))

(deftest test-deal-with-increment
  (is (= [0 7 4 1 8 5 2 9 6 3]
         (deal-with-increment 3 [0 1 2 3 4 5 6 7 8 9]))))

(deftest test-combinations
  (is (= [0 3 6 9 2 5 8 1 4 7]
         (parse ["deal with increment 7"
                 "deal into new stack"
                 "deal into new stack"]
                (vec (range 10)))))

  (is (= [3 0 7 4 1 8 5 2 9 6]
         (parse ["cut 6"
                 "deal with increment 7"
                 "deal into new stack"]
                (vec (range 10)))))

  (is (= [6 3 0 7 4 1 8 5 2 9]
         (parse ["deal with increment 7"
                 "deal with increment 9"
                 "cut -2"]
                (vec (range 10)))))

  (is (= [9 2 5 8 1 4 7 0 3 6]
         (parse ["deal into new stack"
                 "cut -2"
                 "deal with increment 7"
                 "cut 8"
                 "cut -4"
                 "deal with increment 7"
                 "cut 3"
                 "deal with increment 9"
                 "deal with increment 3"
                 "cut -1"]
                (vec (range 10))))))
