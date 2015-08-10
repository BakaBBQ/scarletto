(ns scarletto.lifted
  (:require [scarletto.factory :refer :all])
  (:import [com.badlogic.gdx.math Vector2]))

(defn expand-to
  [bullet angle n]
  (let
      [starting-angle-d (* (/ (dec n) 2) angle)
       starting-angle (- (.angle ^Vector2 (:vel bullet)) starting-angle-d)]
    (for [i (range n)
          :let [theta (+ starting-angle (* i angle))]]
      (-> bullet
          (update :vel (fn [x] (reangle x theta)))
          (rotate-bullet (* i angle))))))

(defn expand-to2
  [bullet angle n]
  (let
      [starting-angle-d (* (/ (dec n) 2) angle)
       starting-angle (- (.angle ^Vector2 (:vel bullet)) starting-angle-d)]
    (for [i (range n)
          :let [theta (+ starting-angle (* i angle))]]
      (-> bullet
          (update :vel (fn [x] (reangle x theta)))
          (rotate-bullet starting-angle)))))

(defn rect-trianglize
  [bullet dis]
  (let [x (:x bullet)
        y (:y bullet)
        angle (+ (.angle ^Vector2 (:vel bullet)) (:rot-angle bullet))
        ^Vector2 b2-offset (polar-vector dis (+ angle 0))
        ^Vector2 b3-offset (polar-vector dis (+ angle 90))

        b2 (assoc bullet :x (+ x (.x b2-offset)) :y (+ y (.y b2-offset)))
        b3 (assoc bullet :x (+ x (.x b3-offset)) :y (+ y (.y b3-offset)))]
    [bullet b2 b3]))

(defn vamp-bullet [b angle]
  (update b :vel (fn [x] (rotate-vector x (+ (- angle) (rand (* 2 angle)))))))
