(ns advent-of-code-2019.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.core :refer :all]))

(deftest test-fuel
  (testing "FIXME, I fail."
    (is (= (fuel 12) 2))
    (is (= (fuel 14) 2))
    (is (= (fuel 1969) 654))
    (is (= (fuel 100756) 33583))))

(deftest test-fuel-recursive
  (testing "FIXME, I fail."
    (is (= (fuel-recursive 14) 2))
    (is (= (fuel-recursive 1969) 966))
    (is (= (fuel-recursive 100756) 50346))))
