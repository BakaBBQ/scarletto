(ns scarletto.factory
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [scarletto.collide :as c]
            [scarletto.entities]
            [play-clj.math :refer :all]
            [scarletto.config :refer :all]
            [clojure.core.typed :refer :all])
  (:import [com.badlogic.gdx.math Vector2 CatmullRomSpline]
           [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect BitmapFont TextureRegion]
           [scarletto.entities CircleBullet PolygonBullet]
           [java.lang ArrayIndexOutOfBoundsException]
           [com.badlogic.gdx.graphics.g2d ParticleEffectPool ParticleEffect ParticleEffectPool$PooledEffect]))

(def grid-div 12)
(def grid-x (/ game-width grid-div))
(def grid-y (/ game-height grid-div))
(defn grid-at [x y] [(* grid-x x) (* grid-y y)])

(defn pause? [screen]
  (:pause screen))

(defn player-dead?
  [player]
  (let [d (:dead player)]
    (and d (pos? d))))

(defn player-can-bomb?
  [player]
  (let [p (:power player)
        bcd (:bomb-cd player)]
    (and
     (>= p 100)
     (== bcd 0)
     (<= (:bomb-timer player) 0))))

(defn rect-vector
  [x :- Num, y :- Num]
  (new Vector2 x y))

(defn polar-vector
  [r :- Num, theta :- Num]
  (doto (Vector2. r 0)
    (.setAngle theta)))

(defn respeed
  [v2 :- Vector2, speed :- Num]
  (let [angle (.angle ^Vector2 v2)]
    (polar-vector speed angle)))

(defn reangle
  [v2 :- Vector2, angle :- Num]
  (let [speed (.len ^Vector2 v2)]
    (polar-vector speed angle)))

(defn reangle-bullet
  [b :- Any, angle :- Num]
  (assoc b :vel (reangle (:vel b) angle)))

(defn bullet-angle
  [b :- Any]
  (let [^Vector2 v (:vel b)]
    (.angle v)))

(defn update-speed
  [v2 :- Vector2, f :- (Fn [Num -> Num])]
  (let [speed (.len ^Vector2 v2)
        angle (.angle ^Vector2 v2)]
    (polar-vector (f speed) angle)))

(defn update-bullet-speed
  [b :- Any, f :- (Fn [Num -> Num])]
  (update b :vel (fn [v] (update-speed v f))))

(defn flip-texture [t :- TextureRegion b1 :- Boolean b2 :- Boolean]
  (let [nt (TextureRegion. ^TextureRegion t)
        flipped (.flip nt b1 b2)]
    nt))

(defn stage-text [index :- Sym]
  {:timer 0 :index index :type :stage-text :ngc true})

(defn particle-effect [particle-pool x y]
  (let [e (doto ^ParticleEffectPool$PooledEffect (.obtain ^ParticleEffectPool particle-pool)
            (.setPosition x y))]
    {:timer 0 :type :particle :ngc true :object e}))

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
  [v2 :- Vector2, angle :- Num]
  (doto (Vector2. v2)
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
  (if (:vectors bullet)
    (do
      (-> bullet
        (update :vectors (partial map (fn [x] (rotate-vector x angle))))
        (update :rot-angle + angle)))
    (update bullet :rot-angle + angle)))

(defn bullet-circle
  [r x y vel]
  {:rot-angle 0 :radius r :x x :y y :type :circle :vel vel :graphics-type :circle :timer 0})

(comment defn bullet-circle
  [r x y vel]
  (CircleBullet. 0 r x y :circle vel :circle 0))

(defn bullet-circle-small
  [x y vel]
  (assoc (bullet-circle 10 x y vel) :color 0))

(defn wait-until-all-clear
  []
  {:ngc true :wait true :type :wait})

(defn explosion
  [x y color]
  {:type :explosion :x x :y y :color color :timer 0})

(defn d-mangnitude
  "default magnitude function for item"
  [max-frame]
  (fn [x]
    (let [portion (/ x max-frame)
          r (- 1 portion)]
      (if (>= r 0)
        r
        0))))


;; states: idle active
(defn character-face [n pos]
  "n => name; pos => 0 or 1"
  {:type :face :pos 0 :state :idle :state-timer 0 :name n})

(defn update-character-state [c state]
  (assoc c :state state :state-timer 0))

(def color-schemes
  {:black 0
   :red 1
   :pink 2
   :purple 3
   :magneta 4
   :blue 5
   :sky 6
   :sea 7
   :cloud 8
   :dgreen 9
   :green 10
   :lgreen 11})

(defn get-color-scheme
  [bullet]
  (let [c (:color bullet)]
    (if c
      c
      ;; default: 0
      0)))

(defn item
  [x y tag blast-vec]
  {:x x :y y :tag tag :type :item :vel blast-vec :timer 0 :radius 7.07
   :gc-up (* 490 1.5)})

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
    (rotate-bullet {:rot-angle 0 :vectors vectors :x x :y y :vel vel :type :polygon :radius r :timer 0}
                   (.angle ^Vector2 vel))))

(comment defn bullet-polygon
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
    (PolygonBullet. 0 vectors x y vel :polygon r 0)))


(defn bullet-square
  [a x y vel]
  (assoc
    (bullet-polygon
     [(rect-vector a a)
      (rect-vector a (- 0 a))
      (rect-vector (- 0 a) (- 0 a))
      (rect-vector (- 0 a) a)]
   x y vel)
    :graphics-type :square)
  )

(defn get-spellcard-bonus [sc]
  (let [t (:timer sc)
        starting-value (*' 1000000 (+ 3 1))
        r (* (- 40 (/ t 60)) 1/40 starting-value)
        f (:failed sc)]
    (do
    (if f 0 (int r)))))

(defn bullet-star
  [s x y vel]
  (let [ts (* 2 s)
        rb (bullet-polygon
       (apply vector
              (for [i (range 10)]
                (polar-vector (if (even? i)
                                ts
                                s)
                              (* i 36))))
     x y vel)]
    (assoc rb :graphics-type :star)))

(defn player-bullet
  [r x y vel dmg]
  {:radius r :x x :y y :type :pbullet :vel vel :dmg dmg
   :gc-down -50 :timer 0})

(defn single-dialog
  [s]
  {:type :dialog :ngc true :str s :timer 0})

(defn spellcard-bonus
  [score]
  {:type :bonus :ngc true :score score :timer 0})

(defn player
  [shottype subtype]
  {:radius 2 :x (/ game-width 2) :y 75 :type :player :collide false
   :power 400
   :focused 0
   :dead 0
   :ngc true
   :invincible 0
   :velocity 0
   :lives 0
   :bomb-timer 0
   :bomb-cd 0
   :score 0
   :shottype shottype :subtype subtype :timer 0})

(defn player-invincible?
  [player]
  (pos? (:invincible player)))

(defn focused? [player]
  (let [v (:focused player)]
    (= v 40)))

(comment defn get-player-option-pos
  ;; the old reimu option poses...
  "options are functions relative to player's power and timer"
  [player]
  (if (> 100 (:power player))
    []
    (let [focused (:focused player)
          dis (- 90 (* 45 (/ focused 40)))
          power (:power player)
          option-cnt (int (Math/floor (/ power 100)))
          timer (:timer player)

          div-angle (/ 360 option-cnt)

          offset (mod (* 5 timer) 360)

          angles  (for [i (range option-cnt)]
                    (+ offset (* i div-angle)))
          vectors (map (fn [theta] (polar-vector dis theta)) angles)]
      vectors)))

(defn get-player-option-pos
  ;; sanae option poses
  "options are functions relative to player's power and timer"
  [player]
  (if (> 100 (:power player))
    []
    (let [focused (:focused player)
          dis (- 60 (* 30 (/ focused 20)))
          power (:power player)
          option-cnt (quot power 100)
          timer (:timer player)

          div-angle (/ 360 option-cnt)

          offset 0

          angles  (for [i (range option-cnt)]
                    (+ offset (* i div-angle)))
          angles (case option-cnt
                   1 [270]
                   2 [315 225]
                   3 [315 225 90]
                   4 [315 225 0 180])
          vectors (map (fn [theta] (polar-vector dis theta)) angles)]
      vectors)))

(defn get-player-option-angles
   [player]
  (if (> 100 (:power player))
    []
    (let [focused (:focused player)
          dis (- 90 (* 45 (/ focused 40)))
          power (:power player)
          option-cnt (int (Math/floor (/ power 100)))
          timer (:timer player)

          div-angle (/ 360 option-cnt)

          offset (mod (* 4 timer) 360)

          angles  (for [i (range option-cnt)]
                    (+ offset (* i div-angle)))
          vectors (map (fn [theta] (polar-vector dis theta)) angles)]
      angles)))

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

(defn bullet-small-rice
  [x y vel]
  (assoc (bullet-rice 10 6 x y vel) :graphics-type :rice :color 0))

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
        (update :vel (fn [vel] (rotate-vector vel (* (/ 360 ways) x))))
        (update :vectors (partial map (fn [x] (rotate-vector x div))))
        (update :rot-angle + div))))

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
      (- (* 3 x x) (* 2 (Math/pow x 3))))))

(defn linear
  [time-span]
  (fn [frame]
    (let [x (/ frame time-span)]
      x)))

(defn spellcard
  [tag dtag hp]
  {:type :sc :tag tag :dtag dtag :timer 0
   :ngc true :hp hp})

(defn calc-point ^Vector2
  [frame ^CatmullRomSpline path f2t]
  (let [t (f2t frame)]
    (.valueAt path (Vector2. 0 0) t)))

(defn calc-point-derivative-e ^Vector2
  [frame ^CatmullRomSpline path f2t]
  (if (neg? frame)
    (Vector2. 0 0)
    (let [t (f2t frame)]
      (if (or
           (> t 0.98)
           (< t 0.02))
        (Vector2. 0 0)
        (.derivativeAt path (Vector2. 0 0) t)))))

(defn calc-point-derivative ^Vector2
  [frame ^CatmullRomSpline path f2t]
  (try (calc-point-derivative-e frame path f2t)
    (catch ArrayIndexOutOfBoundsException e (Vector2. 0 0))))
