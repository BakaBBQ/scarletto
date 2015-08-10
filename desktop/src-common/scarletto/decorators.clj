(ns scarletto.decorators
  (:require [scarletto.factory :as f]
            [scarletto.presets :as p]
            [scarletto.lifted :refer :all]
            [scarletto.config :refer :all])
  (:import [com.badlogic.gdx.math Vector2]))

(declare gen-shooter)

(defn insert-shooters
  [entities screen]
  (let [^long timer (:gtimer screen)]
    (case timer
      20 [(gen-shooter)]
      [])))

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

(defn every-amplify
  [timable interval n results]
  (let [t (:timer timable)
        q (mod (int (* t n)) interval)]
    (if (zero? q)
      results
      [])))

(defn get-new-shooters
  [entities screen timer]
  ())

(def test-shooter
  (assoc
      (f/bullet-shooter-w-path
       :meow :n (- stage-right-bound stage-left-bound) 350
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
        point1 (Vector2. (/ (- stage-right-bound stage-left-bound) 2) 450)
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
      :tag (rand-nth [:tewi-nonspell-1])
      :radius 12
      :mtag :test
      :hp 500)))


;; gen-shooter
(comment defn insert-shooters
  [entities screen]
  (if (= (mod (int (:gtimer screen)) 40) 0)
    [(gen-shooter)]
    [(f/particle-effect (:maple-green screen) 100 100)]))


(defshoot :test
  [s entities screen]
  (let [player (first entities)
        t (:timer s)
        x (:x s)
        y (:y s)
        a (f/vector-to s player 4)
        b (assoc (p/rice x y (f/polar-vector 3 (mod t 360))) :color 1)
        b2 (f/rotate-bullet b (mod t 360))]
    (every s 10
           (f/nway-shoot b2 30))))

(defshoot :kaguya-nonspell-1
  [s entities screen]
  (let [player (first entities)
        x (:x s)
        y (:y s)
        pos1 (- x 50)
        pos2 (+ x 50)
        t (:timer s)

        a (f/vector-to s player 4)
        bo (assoc (p/big-oval x y a) :color 4)
        wave1? (< (mod t 360) 180)
        wave2? (>= (mod t 360) 180)
        b (assoc (p/big-oval x y (f/polar-vector 3 (mod (int (* t 4)) 360))) :color 1 :btag :dc)
        b2 (assoc (p/big-oval x y (f/polar-vector 3 (- 360 (mod (int (* t 4)) 360)))) :color 3 :btag :dc)
        ]
    (concat
     (every-amplify s 4 1.2
            (cond
             wave1? (map #(f/update-bullet-speed % (partial + (* (rand) 0.4) 0.8))
                         (flatten (map (comp #(expand-to % 10 3)) (f/nway-shoot b 2))))
             wave2? (map #(f/update-bullet-speed % (partial + (* (rand) 0.4) 0.8))
                         (flatten (map (comp #(expand-to2 % 10 2)) (f/nway-shoot b2 2))))
             :default []))
     [
      (case (mod t 180)
       0 bo
       5 bo
       10 bo
       15 bo
       20 bo
       nil)
      ])))

(defshoot :kaguya-nonspell-2
  [s entities screen]
  (let [player (first entities)
        x (:x s)
        y (:y s)
        t (:timer s)
        a (f/vector-to s player (+ 1.8 (/ (mod t 40) 100)))
        alternate (even? (quot t 20))
        tb (f/rotate-bullet (assoc (p/circle x y a) :color 3) (mod (* 2 t) 360))
        tb2 (f/rotate-bullet (assoc (p/circle x y a) :color 5) (mod (* 3 t) 360))
        cb (assoc (p/circle x y (f/polar-vector 2 (* 72 (mod (quot t 60) 5)))) :color (if alternate 1 14))]
    (concat
     (every s 60
           (map #(expand-to % 3 8) (f/nway-shoot cb 6)))
     (case (mod t 80)
           20 (map (comp #(f/nway-shoot % 4) #(vamp-bullet % 30)) (rect-trianglize tb 20))
           40 (map (comp #(f/nway-shoot % 4) #(vamp-bullet % 30)) (rect-trianglize tb 20))
       [])
     (if (> t 120)
       (every s 30
            (map (comp #(f/nway-shoot % 4) #(vamp-bullet % 30)) (rect-trianglize tb2 20)))
       []))))

(defshoot :kaguya-nonspell-3
  [s entities screen]
  (let [player (first entities)
        x (:x s)
        y (:y s)
        t (:timer s)
        b (assoc (p/rice x y (f/polar-vector 1.2 90)) :color 3)
        nb (f/nway-shoot b 30)]
    (concat
     (case (mod t 180)
       40 nb
       55 nb
       70 nb
       []))))

(defshoot :tewi-nonspell-1
  [s entities screen]
  (let [player (first entities)
        t (:timer s)
        x (:x s)
        y (:y s)
        cc (mod t 240)
        angle1 (+ 120 (mod (* t 6) 360))
        rice (assoc (p/rice x y (f/polar-vector 2 angle1)) :color 1 :btag :tewi :turn-angle 80 :slow-time 50 :turn-time 110)
        rice2 (assoc (p/rice x y (f/polar-vector 2 (- 180 (+ 180 (mod (* t 6) 360))))) :color 3 :btag :tewi :turn-angle -80 :slow-time 60 :turn-time 110)
        rice3 (assoc (p/rice x y (f/polar-vector 2 angle1)) :color 13 :btag :tewi :turn-angle 30 :slow-time 50 :turn-time 110)
        rice4 (assoc (p/rice x y (f/polar-vector 2 (- 180 (+ 180 (mod (* t 6) 360))))) :color 14 :btag :tewi :turn-angle -30 :slow-time 50 :turn-time 100)
        rice-expanded (expand-to rice 5 5)
        rice2-expanded (expand-to rice2 5 5)
        rice3-expanded (expand-to rice3 5 5)
        rice4-expanded (expand-to rice4 5 5)
        b (assoc (p/big-circle x y (f/polar-vector 2 (+ 90 (mod (quot t 5) 360)))) :color 1)
        nb (f/nway-shoot b 10)
        b2 (assoc (p/big-circle x y (f/polar-vector 2 (+ 110 (mod (quot t 5) 360)))) :color 1)
        nb2 (f/nway-shoot b2 10)
        rb (assoc (p/ring x y (f/polar-vector 2.4 2)) :color 1)]
    (concat
     (every s 4
           (concat
            (if (< cc 120) (concat rice-expanded rice2-expanded)
              (concat rice3-expanded rice4-expanded))
            ))
     (if (= cc 110) (concat nb))
     (if (= cc 120) (concat nb2)))))


(defshoot :test2
  [s entities screen]
  (let [player (first entities)
        x (:x s)
        y (:y s)
        a (f/vector-to s player 4)
        b (p/circle x y a)
        b2 (assoc (f/rotate-bullet b (.angle ^Vector2 a)) :color 0)]
    (every s 20
           b)))

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
  s)

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

(defmulti update-bullet :btag)

(defmethod update-bullet :dc [bullet]
  (let [t (:timer bullet)]
    (case t
      60 (update bullet :vel #(f/update-speed % (fn [x] (max (dec x) 0))))
      70 (update bullet :vel #(f/update-speed % (fn [x] (max (dec x) 0))))
      bullet)))

(defmethod update-bullet :turn30 [bullet]
  (let [t (:timer bullet)]
    (case t
      60 (-> bullet
             (f/rotate-bullet 60)
             (f/reangle-bullet (+ 60 (f/bullet-angle bullet))))
      bullet)))

(defmethod update-bullet :rot-at60 [bullet]
  (let [t (:timer bullet)]
    (case t
      60 (-> bullet
             (f/rotate-bullet 100)
             (f/reangle-bullet (+ 100 (f/bullet-angle bullet))))
      bullet)))

(defmethod update-bullet :tewi [bullet]
  (let [t (:timer bullet)
        st (:slow-time bullet)
        tt (:turn-time bullet)]
    (cond
      (= t (- st 20)) (update bullet :vel #(f/update-speed % (fn [x] (max (dec x) 0))))
      (= t st) (update bullet :vel #(f/update-speed % (fn [x] (max (- x 0.5) 0))))
      (= t tt) (-> bullet
             (f/rotate-bullet (:turn-angle bullet))
             (f/reangle-bullet (+ (:turn-angle bullet) (f/bullet-angle bullet)))
             (update :vel #(f/update-speed % (fn [x] (max (+ x 1.5) 0)))))

      :default bullet)))

(defmethod update-bullet :tewi2 [bullet]
  (let [t (:timer bullet)]
    (case t
      90 (update bullet :vel #(f/update-speed % (fn [x] (max (dec x) 0))))
      110 (update bullet :vel #(f/update-speed % (fn [x] (max (- x 0.5) 0))))
      120 (-> bullet
             (f/rotate-bullet -30)
             (f/reangle-bullet (+ -30 (f/bullet-angle bullet)))
             (update :vel #(f/update-speed % (fn [x] (max (+ x 2.5) 0)))))

      bullet)))

(defmethod update-bullet :rot-at602 [bullet]
  (let [t (:timer bullet)]
    (case t
      60 (-> bullet
             (f/rotate-bullet -100)
             (f/reangle-bullet (+ -100 (f/bullet-angle bullet))))
      bullet)))

(defmethod update-bullet :default [bullet]
  bullet)


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
