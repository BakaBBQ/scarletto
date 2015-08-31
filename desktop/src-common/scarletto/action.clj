(ns scarletto.action
  "defines a dsl for bullet actions"
  (:require [scarletto.factory :as f])
  (:import [com.badlogic.gdx.math Vector2]))


(def action-format
  [[[:at 50] [:turn 1] [:aim-at :player] [:speed-change 1] [:respeed 0]]
   [[:between 10 19] [:turn 1]]])

(defmulti run-action (fn [bullet player coll] (first coll)))
(defmulti run-predicate (fn [bullet player predicate] (first predicate)))

(defmethod run-action :rotate [bullet player coll]
  (let [args (rest coll)
        angle (first args)]
    (f/rotate-bullet bullet angle)))

(defmethod run-action :rotate-aim [bullet player coll]
  (let [args (rest coll)
        angle (first args)]
    (f/reangle-bullet bullet (+ angle (f/bullet-angle bullet)))))

(defmethod run-action :rotate-both [bullet player coll]
  (let [args (rest coll)
        angle (first args)]
    (-> bullet
        (f/rotate-bullet angle)
        (f/reangle-bullet (+ angle (f/bullet-angle bullet))))
    ))

(defmethod run-action :aim-at-player [bullet player coll]
  (let [args (rest coll)
        v (f/vector-between bullet player)
        angle (.angle ^Vector2 v)
        angle-diff (- angle (:rot-angle bullet))]
    (-> bullet
        (f/rotate-bullet angle-diff)
        (f/reangle-bullet angle))))

(defmethod run-action :set-aim [bullet player coll]
  (let [args (rest coll)
        angle (first args)]
    (f/reangle-bullet bullet angle)))

(defmethod run-action :nothing [bullet player coll]
  (let [args (rest coll)
        angle (first args)]
    bullet))

(defmethod run-action nil [bullet player coll]
  bullet)

(defmethod run-predicate :at [bullet player coll]
  (= (:timer bullet) (second coll)))

(defmethod run-predicate :between [bullet player coll]
  (let [args (rest coll)
        f (first args)
        s (second args)]
    (<= f (:timer bullet) s)))

(defmethod run-predicate :every [bullet player coll]
  (let [args (rest coll)
        t (first args)]
    (= (mod (:timer bullet) t) 0)))

(defmethod run-predicate :and [bullet player coll]
  (let [args (rest coll)
        p (map #(run-predicate bullet player %) args)]
    (every? true? p)))

(defmethod run-predicate :always [bullet player coll]
  true)

(defn execute-action [bullet player action-list]
  (let [avaliable-actions (concat (for [a action-list
                                        :let [
                                              predicate (first a)
                                              actions (rest a)
                                              r (if (run-predicate bullet player predicate)
                                                  actions
                                                  [])]]
                                    r))
        a (map first avaliable-actions)
        r (loop [b bullet
                 x 0]
            (if (< x (count a))
              (recur (run-action b player (nth a x)) (inc x))
              b))]
    r))
