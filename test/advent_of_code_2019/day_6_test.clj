(ns advent-of-code-2019.day-6-test
  (:require [advent-of-code-2019.day-6 :refer :all]
            [clojure.test :refer :all]))

(deftest test-day-6-1
  (is (= 42 (day-6-1 [[:COM :B] [:B :C] [:C :D] [:D :E] [:E :F] [:B :G] [:G :H] [:D :I] [:E :J] [:J :K] [:K :L]])))
  (is (= 621125 (day-6-1 input-1))))

(deftest test-day-6-2
  (is (= 4 (day-6-2 [[:COM :B] [:B :C] [:C :D] [:D :E] [:E :F] [:B :G] [:G :H] [:D :I] [:E :J] [:J :K] [:K :L] [:K :YOU] [:I :SAN]])))
  (is (= 550 (day-6-2 input-1))))
