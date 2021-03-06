(ns advent-of-code-2019.day-9-test
  (:require [advent-of-code-2019.day-9 :refer :all]
            [clojure.test :refer :all]))

(deftest test-day-9-1
  (is (= [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
         (day-9-1 [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])))

  (is (= [1219070632396864]
         (day-9-1 [1102,34915192,34915192,7,4,7,99,0])))

  (is (= [1125899906842624]
         (day-9-1 [104,1125899906842624,99])))

  (is (= [3409270027]
         (day-9-1 (input)))))
