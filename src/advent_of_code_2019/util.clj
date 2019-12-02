(ns advent-of-code-2019.util
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-lines [filename]
  (s/split-lines
   (slurp (io/resource filename))))
