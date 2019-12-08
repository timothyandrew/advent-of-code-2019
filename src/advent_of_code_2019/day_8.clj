(ns advent-of-code-2019.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [quil.core :as q]))

(defn read-input []
  (map #(Integer/parseInt %) (s/split (s/trim (slurp (io/resource "input/8.txt"))) #"")))

(defn split-layers [in w h]
  (vec (map vec (partition h (map vec (partition w in))))))

(defn count-layer [layer n]
  (count (filter #(= % n) (flatten layer))))

(defn decode [layers w h]
  (for [i (range h)
        j (range w)
        :let [pixels (map #(get-in % [i j]) layers)]]
    (first (filter #(not= 2 %) pixels))))

(defn render [image w h]
  (q/background 217 255 254)
  (let [img (q/create-image w h :rgb)
        scale 5]
    (doseq [i (range h)
            j (range w)
            :let [color (case (get-in image [i j])
                          0 (q/color 0 0 0)
                          1 (q/color 255 255 255))]]
      (q/set-pixel img j i color))
    (q/resize img (* w scale) (* h scale))
    (q/image img 20 20)))

(defn day-8-1 [in]
  (let [[w h] [25 6]
        layers (split-layers in w h)
        layer (apply min-key #(count-layer % 0) layers)]
    (* (count-layer layer 1)
       (count-layer layer 2))))

(defn day-8-2 [in]
  (let [[w h] [25 6]
        layers (split-layers in w h)
        image (vec (map vec (partition w (decode layers w h))))]

    (q/sketch
     :size [200 100]
     :setup #(q/no-loop)
     :draw #(render image w h))

    image))
