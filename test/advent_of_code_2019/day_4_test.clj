(ns advent-of-code-2019.day-4-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day-4 :refer :all]))

(deftest test-day-4
  (is (= 1033 (day-4-1)))
  (is (= 670 (day-4-2))))

