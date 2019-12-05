(ns advent-of-code-2019.day-5-test
  (:require [advent-of-code-2019.day-5 :refer :all]
            [clojure.test :refer :all]))

(deftest day-5-1
  (is (= {:opcode 2 :modes [:position :immediate]} (parse-opcode 1002))))
