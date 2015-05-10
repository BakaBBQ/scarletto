(ns scarletto.lifted
  (:require [scarletto.factory :refer :all]))

(defn expand-to
  [bullet angle n]
  (let
      [starting-angle-d (* (/ (- n 1) 2) angle)
       starting-angle (- (.angle (:vel bullet)) starting-angle-d)]
    (for [i (range n)
          :let [theta (+ starting-angle (* i angle))]]
       (update-in bullet [:vel] (fn [x] (reangle x theta))))))
