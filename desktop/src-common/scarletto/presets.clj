(ns scarletto.presets
  (:require [scarletto.factory :refer :all]))

(defn scale
  [x y vel]
  (assoc (bullet-polygon [(rect-vector -6 -6) (rect-vector 6 -6) (rect-vector 0 6)] x y vel)
    :graphics-type :scale :color 0))

(defn ring
  [x y vel]
  (assoc (bullet-circle 8 x y vel) :graphics-type :ring :color 0))

(defn circle
  [x y vel]
  (assoc (bullet-circle 9 x y vel) :graphics-type :circle :color 0))

(defn big-circle
  [x y vel]
  (assoc (bullet-circle 16 x y vel) :graphics-type :big-circle :color 0))

(defn rice
  [x y vel]
  (assoc (bullet-rice 10 6 x y vel) :graphics-type :rice :color 0))

(defn big-oval
  [x y vel]
  (assoc (bullet-rice 15 7 x y vel) :graphics-type :big-oval :color 0))

(defn crystal
  [x y vel]
  (assoc (bullet-rice 10 6 x y vel) :graphics-type :crystal :color 0))
