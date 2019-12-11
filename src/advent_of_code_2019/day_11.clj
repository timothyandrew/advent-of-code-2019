(ns advent-of-code-2019.day-11
  (:require [advent-of-code-2019.intcode :as intcode]
            [clojure.core.async :as a]
            [quil.core :as q]
            [clojure.set :as set]))

(def program [3,8,1005,8,299,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,29,1,1007,14,10,2,1106,8,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,58,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,80,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,103,1,5,6,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,128,1,106,18,10,1,7,20,10,1006,0,72,1006,0,31,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,164,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,186,1,1007,8,10,1006,0,98,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,216,2,102,8,10,1,1008,18,10,1,1108,8,10,1006,0,68,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1001,8,0,253,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,274,1,1105,7,10,101,1,9,9,1007,9,1060,10,1005,10,15,99,109,621,104,0,104,1,21102,936995738520,1,1,21102,316,1,0,1106,0,420,21101,0,936995824276,1,21102,1,327,0,1106,0,420,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,248129784923,1,1,21102,1,374,0,1105,1,420,21102,29015149735,1,1,21101,385,0,0,1106,0,420,3,10,104,0,104,0,3,10,104,0,104,0,21101,983925826304,0,1,21101,0,408,0,1105,1,420,21102,825012036364,1,1,21101,0,419,0,1105,1,420,99,109,2,22101,0,-1,1,21101,0,40,2,21101,0,451,3,21102,441,1,0,1105,1,484,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,446,447,462,4,0,1001,446,1,446,108,4,446,10,1006,10,478,1101,0,0,446,109,-2,2105,1,0,0,109,4,2102,1,-1,483,1207,-3,0,10,1006,10,501,21102,0,1,-3,21201,-3,0,1,22102,1,-2,2,21102,1,1,3,21101,520,0,0,1106,0,525,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,548,2207,-4,-2,10,1006,10,548,21201,-4,0,-4,1105,1,616,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,1,567,0,1105,1,525,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,586,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,608,21201,-1,0,1,21102,1,608,0,106,0,483,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0])

(defn create-robot []
  {:pos [200 200] :facing :north :painted {:white #{} :black #{}} :pos->color {}})

(def cardinal [:north :east :south :west])
(def relative {:left -1 :right 1})

(defn relative->cardinal [current rel]
  (let [i (.indexOf cardinal current)
        new (+ i (get relative rel))]
    (nth cardinal (mod new (count cardinal)))))

(defn move-forward [{:keys [facing pos] :as robot}]
  (case facing
    :north (update robot :pos (fn [[x y]] [x (inc y)]))
    :east (update robot :pos (fn [[x y]] [(inc x) y]))
    :south (update robot :pos (fn [[x y]] [x (dec y)]))
    :west (update robot :pos (fn [[x y]] [(dec x) y]))))

(defn turn-and-move [robot direction]
  (-> robot
      (assoc :facing (relative->cardinal (:facing robot) direction))
      move-forward))

(defn paint [robot color direction]
  (-> robot
      (update-in [:painted color] conj (:pos robot))
      (assoc-in [:pos->color (:pos robot)]  color)
      (turn-and-move direction)))

(defn current-color [robot]
  (or (get (:pos->color robot) (:pos robot))
      :white))

(defn day-11-1 []
  (let [[channels a] (intcode/setup program)]
    (intcode/exec a)

    (loop [robot (create-robot)]
      (a/>!! (:input channels) (case (current-color robot) :black 0 :white 1))
      (let [color (a/<!! (:output channels))
            direction (a/<!! (:output channels))]
        (if (and color direction)
          (recur (paint robot
                        (case color 0 :black 1 :white)
                        (case direction 0 :left 1 :right)))
          robot)))))

(defn render [positions]
  (q/background 255 0 0)
  (let [img (q/create-image 1000 1000 :rgb)]
    (doseq [[[x y] color] positions]
      (q/set-pixel img x (- 700 y) (case  color :black (q/color 0 0 0) :white (q/color 255 255 255))))
    (q/image img 0 0)))


(defn day-11-2 []
  (q/sketch
   :size [1000 1000]
   :setup #(q/no-loop)
   :draw #(render (-> (day-11-1) :pos->color))))

