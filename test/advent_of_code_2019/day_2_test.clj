(ns advent-of-code-2019.day-2-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day-2 :refer :all]))

(deftest test-fuel
  (testing "FIXME, I fail."
    (is (= (tick [1 0 0 0 99]) [2 0 0 0 99]))
    (is (= (tick [2 3 0 3 99]) [2 3 0 6 99]))
    (is (= (tick [2 4 4 5 99 0]) [2 4 4 5 99 9801]))
    (is (= (tick [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99]))
    ))
