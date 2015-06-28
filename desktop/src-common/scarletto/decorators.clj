(ns scarletto.decorators
  (:require [scarletto.factory :as f]
            [scarletto.lifted :refer :all])
  (:import [com.badlogic.gdx.math Vector2]))

(defmulti get-new-bullets
  (fn [s entities screen]
    (:tag s)))

(defmulti get-boss-bullets
  (fn [spell-card-tag boss entities screen]
    spell-card-tag))

(defmulti get-boss-movement
  (fn [spell-card-tag boss entities screen]
    spell-card-tag))

(defmulti get-boss-movement
  (fn [spell-card-tag boss entities screen]
    spell-card-tag))

(defmethod get-boss-bullets :test-sc
  [tag boss entities screen]
  (let [x (:x boss)
        y (:y boss)
        player (first entities)
        ^Vector2 a (f/vector-to boss player (+ 1 (mod (:timer boss) 3)))
        b (f/bullet-small-rice x y a)
        b2 (f/rotate-bullet b (.angle a))]
     (if (= 0 (mod (:timer boss) 2))
       (f/nway-shoot b2 4)
       [])))

(defmethod get-boss-movement :test-sc
  [tag boss entities screen]
  boss)

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
    (if (zero? q)
      results
      [])))

(defn get-new-shooters
  [entities screen timer]
  ())

(def test-shooter
  (assoc
              (f/bullet-shooter-w-path
               :meow :n 200 200
               [(Vector2. 200 200)
                (Vector2. 400 400)
                (Vector2. 200 200)
                (Vector2. 400 400)
                (Vector2. 800 800)]
               (f/splined 800))
            :dtag :test
            :exempt-once true
            :tag :test
            :radius 12
            :mtag nil
    :hp 10))

(defn insert-shooters
  [entities screen]
  (let []
    (case (int (:gtimer screen))
      60 [test-shooter]
      100 [test-shooter]
      140 [test-shooter]
      [])))

(defshoot :test
  [s entities screen]
  (let [player (first entities)
        x (:x s)
        y (:y s)
        a (f/vector-to s player (+ 1 (mod (:timer s) 3)))
        b (f/bullet-small-rice x y a)
        b2 (f/rotate-bullet b (.angle a))]
    (every s 30
           (f/nway-shoot b2 10))))

(defshoot :boss
  [s entities screen]
  [])

(defmovement :test
  [s entities screen]
  (-> s
      (update-in [:x] inc)))

(defmovement :n
  [s entities screen]
  s)

(defn nn
  [x]
  (if (nil? x)
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
  (let [entities-grouped (:entities-grouped screen)
        boss (filter :boss (:shooter entities-grouped))
        b (flatten (for [e entities
                         :when (and
                                (= (:type e) :shooter)
                                (not (:boss e)))]
                     (get-new-bullets e entities screen)))

        spell-card-tags (map :tag (:sc entities-grouped))
        boss-entities (flatten
                       (for [every-boss boss]
                        (for [every-tag spell-card-tags]
                          (get-boss-bullets every-tag every-boss entities screen))))
        entities-updated (for [e entities]
                           (if (and (:boss e) ((comp not empty?) spell-card-tags))
                             (get-boss-movement (first spell-card-tags) e entities screen)
                             e))
        s (flatten (insert-shooters entities screen))]
    (let [r (concat entities-updated b s boss-entities)]
      r)))
