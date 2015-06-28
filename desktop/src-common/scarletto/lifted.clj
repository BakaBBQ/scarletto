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
       (update bullet :vel (fn [x] (reangle x theta))))))
