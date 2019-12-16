(ns advent-of-code-2019.day-14-test
  (:require [advent-of-code-2019.day-14 :refer :all]
            [clojure.test :refer :all]))

(deftest test-14-1
  (is (= {:ORE 165.0} (day-14-1 (parse-input "input/14-0.txt"))))
  (is (= {:ORE 13312.0} (day-14-1 (parse-input "input/14-1.txt"))))
  (is (= {:ORE 180697.0} (day-14-1 (parse-input "input/14-2.txt"))))
  (is (= {:ORE 180697.0} (day-14-1 (parse-input "input/14-3.txt"))))
  (is (= {:ORE 180697.0} (day-14-1 (parse-input "input/14.txt"))))
  



  )

