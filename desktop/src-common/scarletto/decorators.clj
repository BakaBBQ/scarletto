(ns scarletto.decorators
  (:require [scarletto.factory :as f]
            [scarletto.presets :as p]
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

(defn gen-shooter
  []
  (let [
        point1 (Vector2. (+ (rand 600) 240) (+ (rand 480) 240))
        point2 (Vector2. (+ (rand 600) 240) (+ (rand 480) 240))
        point3 (Vector2. (+ (rand 600) 240) (+ (rand 480) 240))
        point4 (Vector2. (+ (rand 600) 240) (+ (rand 480) 240))
        t (+ 300 (rand 400))]
    (assoc
        (f/bullet-shooter-w-path
         :meow :n (.x point1) (.y point1)
         [point1 point2 point3 point4]
         (f/splined t))
      :dtag :test
      :exempt-once true
      :tag (rand-nth [:test :test2])
      :radius 12
      :boss true
      :mtag nil
      :hp 5)))


;; gen-shooter
(comment defn insert-shooters
  [entities screen]
  (if (= (mod (int (:gtimer screen)) 40) 0)
    [(gen-shooter)]
    []))

(defn insert-shooters
  [entities screen]
  (let [^long timer (:gtimer screen)]
    (case timer
      60 [(f/stage-text :3c)]
      [])))

(defshoot :test
  [s entities screen]
  (let [player (first entities)
        x (:x s)
        y (:y s)
        a (f/vector-to s player (+ 4 (mod (:timer s) 3)))
        b (p/big-circle x y a)
        b2 (f/rotate-bullet b (.angle ^Vector2 a))]
    (every s 50
           (f/nway-shoot b2 10))))

(defshoot :test2
  [s entities screen]
  (let [player (first entities)
        x (:x s)
        y (:y s)
        a (f/vector-to s player (+ 4 (mod (:timer s) 5)))
        b (f/bullet-circle-small x y a)
        b2 (assoc (f/rotate-bullet b (.angle ^Vector2 a)) :color (rand 12))]
    (every s 50
           (f/nway-shoot b2 20))))

(defshoot :test3
  [s entities screen]
  (let [player (first entities)
        x (:x s)
        y (:y s)
        a (f/vector-to s player (+ 4 (mod (:timer s) 5)))
        b (f/bullet-circle-small x y a)
        b2 (f/rotate-bullet b (mod (:timer s) 360))]
    (every s 30
           (f/nway-shoot b2 3))))

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
        (assoc :x (.x ^Vector2 p))
        (assoc :y (.y ^Vector2 p)))))


(defn normal-shooter? [e]
  "returns whether the entity is both a shooter and a boss"
  (and (= (:type e) :shooter) (not (:boss e))))

(defn update-shooters
  [entities screen]
  (let [entities-grouped (:entities-grouped screen)
        boss (filter :boss (:shooter entities-grouped))
        b (flatten (for [e entities
                         :when (normal-shooter? e)]
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
