(ns scarletto.factory
  (:require [scarletto.collide :as c]
            [play-clj.math :refer :all])
  (:import [com.badlogic.gdx.math Vector2 CatmullRomSpline]))

(defn rect-vector
  [x y]
  (new Vector2 x y))

(defn polar-vector
  [r theta]
  (doto (Vector2. r 0)
    (.setAngle theta)))

(defn respeed
  [^Vector2 vec speed]
  (let [angle (.angle vec)]
    (polar-vector speed angle)))

(defn update-speed
  [^Vector2 vec f]
  (let [speed (.len vec)
        angle (.angle vec)]
    (polar-vector (f speed) angle)))

(defn vector-between
  [a b]
  (let [x1 (:x a)
        x2 (:x b)
        y1 (:y a)
        y2 (:y b)]
    (rect-vector (- x2 x1) (- y2 y1))))

(defn vector-to ^Vector2
  [a b speed]
  (respeed (vector-between a b) speed))

(defn rotate-vector
  [^Vector2 vector angle]
  (doto (Vector2. vector)
    (.rotate angle)))

(defn vector-len
  [^Vector2 vector]
  (.len vector))

(defn reangle
  [^Vector2 vector angle]
  (doto (Vector2. vector)
    (.setAngle angle)))

(defn rotate-bullet
  [bullet angle]
  (if (:vectors angle)
    (update-in bullet [:vectors] (partial map (fn [x] (rotate-vector x angle))))
    bullet))

(defn bullet-circle
  [r x y vel]
  {:radius r :x x :y y :type :circle :vel vel})

(defn d-mangnitude
  "default magnitude function for item"
  [max-frame]
  (fn [x]
    (let [portion (/ x max-frame)
          r (- 1 portion)]
      (if (>= r 0)
        r
        0))))

(defn item
  [x y tag blast-vec]
  {:x x :y y :tag tag :type :item :vel blast-vec :timer 0 :radius 7.07})

(defn bullet-polygon
  "arguments: vector of Vector2, int x, int y, Vector2 vel"
  [vectors x y vel]
  (let
      [r
       (apply
        max
        (map
         (fn [^Vector2 v]
           (c/distance 0 0 (.x v) (.y v)))
         vectors))]
    {:vectors vectors :x x :y y :vel vel :type :polygon :radius r}))

(defn bullet-square
  [a x y vel]
  (bullet-polygon
   [(rect-vector a a)
    (rect-vector a (- 0 a))
    (rect-vector (- 0 a) (- 0 a))
    (rect-vector (- 0 a) a)]
   x y vel))

(defn bullet-star
  [s x y vel]
  (let [ts (* 2 s)]
    (bullet-polygon
     (apply vector
            (for [i (range 10)]
              (polar-vector (if (even? i)
                              ts
                              s)
                            (* i 36))))
     x y vel)))

(defn player-bullet
  [r x y vel dmg]
  {:radius r :x x :y y :type :pbullet :vel vel :dmg dmg
   :exempt-once true :ngc true})

(defn player
  [shottype subtype]
  {:radius 2 :x (/ 382 2) :y 50 :type :player :collide false
   :power 0
   :focused 0
   :shottype shottype :subtype subtype :timer 0})

(defn focused? [player]
  (let [v (:focused player)]
    (= v 40)))

(defn get-player-option-pos
  "options are functions relative to player's power and timer"
  [player]
  (if (> 100 (:power player))
    []
    (let [focused (:focused player)
          dis (- 60 (* 30 (/ focused 40)))
          power (:power player)
          option-cnt (int (Math/floor (/ power 100)))
          timer (:timer player)

          div-angle (/ 360 option-cnt)

          offset (mod (* 4 timer) 360)

          angles  (for [i (range option-cnt)]
                    (+ offset (* i div-angle)))
          vectors (map (fn [theta] (polar-vector dis theta)) angles)]
      vectors)))

(defn ellipse-approximate
  [a b points]
  (let [step (/ (* 2 Math/PI) points)]
    (for [i (range points)]
      (Vector2.
       (* a (Math/cos (* step i)))
       (* b (Math/sin (* step i)))))))

(defn heart-approximate
  [s points]
  (let [step (/ (* 2 Math/PI) points)]
    (for [i (range points)
          :let [theta (* i step)]]
       (Vector2.
        (* 16 (Math/pow (Math/sin theta) 3))
        (- (* 13 (Math/cos theta)) (* 5 (Math/cos (* 2 theta)))
           (* 2 (Math/cos (* 3 theta))) (Math/cos (* 4 theta)))))))

(defn bullet-rice
  [a b x y vel]
  (let [points 8]
    (bullet-polygon
     (ellipse-approximate a b points)
     x y vel)))

(defn bullet-heart
  [s x y vel]
  (let [points 15]
    (bullet-polygon
     (heart-approximate s points)
     x y vel)))

(defn nway-shoot
  [bullet ways]
  (for [x (range ways)
        :let [div (* (/ 360 ways) x)]]
    (-> bullet
        (update-in [:vel] (fn [vel] (rotate-vector vel (* (/ 360 ways) x))))
        (update-in [:vectors] (partial map (fn [x] (rotate-vector x div)))))))

(defn bullet-shooter
  [tag mtag x y]
  {:type :shooter :x x :y y :radius 6 :tag tag :mtag mtag :hp 20 :timer 0
   :exempt-once true :ngc true})

(defn bullet-shooter-w-path
  [tag mtag x y path-points movement]
  (let [path (catmull-rom-spline path-points true)]
    (assoc (bullet-shooter tag mtag x y) :path path :movement movement)))

(defn splined
  [time-span]
  (fn [frame]
    (let [x (/ frame time-span)]
      (- (* 3 (* x x)) (* 2 (Math/pow x 3))))))
(defn linear
  [time-span]
  (fn [frame]
    (let [x (/ frame time-span)]
      x)))

(defn calc-point ^Vector2
  [frame ^CatmullRomSpline path f2t]
  (let [t (f2t frame)]
    (.valueAt path (Vector2. 0 0) t)))
