(ns scarletto.decorators
  (:require [scarletto.factory :as f]
            [scarletto.lifted :refer :all])
  (:import [com.badlogic.gdx.math Vector2]))

(defmulti get-new-bullets
  (fn [s entities screen]
    (:tag s)))

(defmulti update-single-shooter
  (fn [s entities screen]
    (:mtag s)))

(defmacro defshoot
  [tag-name & body]
  (concat
   `(defmethod get-new-bullets ~tag-name)
   body))

(defmacro defmovement
  [tag-name & body]
  (concat
   `(defmethod update-single-shooter ~tag-name)
   body))

(defn every
  [timable interval results]
  (let [t (:timer timable)
        q (mod t interval)]
    (if (= q 0)
      results
      [])))

(defn insert-shooters
  [entities screen]
  (every screen 20
         (concat
          [(assoc
               (f/bullet-shooter-w-path
                   :test :n 0 0
                   [(Vector2. 200 200)
                    (Vector2. 300 100)
                    (Vector2. 300 300)
                    (Vector2. 100 200)
                    (Vector2. 400 200)]
                   (f/linear 400))
              :dtag :test)])))
(defshoot :test
  [s entities screen]
  (let [player (first entities)
        x (:x s)
        y (:y s)
        b (f/bullet-circle 5 x y (f/vector-to s player 2))]
    (every s 20
           (expand-to b 30 3))))

(defmovement :test
  [s entities screen]
  (-> s
      (update-in [:x] inc)))

(defn nn
  [x]
  (if (= x nil)
    (throw (Exception. "detected nil value"))
    x))

(defmovement :default
  [s entities screen]
  (let [t (nn (:timer s))
        p (nn (:path s))
        m (nn (:movement s))
        p (f/calc-point t p m)]
    (-> s
        (assoc :x (.x p))
        (assoc :y (.y p)))))

(defn update-shooters
  [entities screen]
  (let [b (flatten (for [e entities
                          :when (= (:type e) :shooter)]
                     (get-new-bullets e entities screen)))
        s (flatten (insert-shooters entities screen))]
    (let [r (concat entities b s)]
      r)))
