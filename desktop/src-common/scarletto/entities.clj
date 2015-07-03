(ns scarletto.entities
  (:import [com.badlogic.gdx.math Vector2]))

(comment defn player
  [shottype subtype]
  {:radius 2 :x ( * (/ 382 2) 1.5) :y 75 :type :player :collide false
   :power 400
   :focused 0
   :ngc true
   :velocity 0
   :shottype shottype :subtype subtype :timer 0})

(comment defn player-bullet
  [r x y vel dmg]
  {:radius r :x x :y y :type :pbullet :vel vel :dmg dmg
   :gc-down -50})

(comment
  (defn bullet-circle
  [r x y vel]
  {:rot-angle 0 :radius r :x x :y y :type :circle :vel vel :graphics-type :circle :timer 0}))

(comment defn item
  [x y tag blast-vec]
  {:x x :y y :tag tag :type :item :vel blast-vec :timer 0 :radius 7.07
   :gc-up (* 490 1.5)})

(comment defn bullet-shooter
  [tag mtag x y]
  {:type :shooter :x x :y y :radius 6 :tag tag :mtag mtag :hp 20 :timer 0
   :exempt-once true :ngc true})

(comment defn spellcard
  [tag dtag]
  {:type :sc :tag tag :dtag dtag :timer 0
   :ngc true :hp 800})

(comment polygon :rot-angle 0 :vectors vectors :x x :y y :vel vel :type :polygon :radius r :timer 0)

(defrecord Player [radius x y type collide power focused ngc velocity shottype subtype timer])

(defrecord PlayerBullet [radius ^long x ^long y type ^Vector2 vel dmg gc-down])

(defrecord CircleBullet [^double rot-angle ^double radius ^double x ^double y type ^Vector2 vel graphics-type ^long timer])

(defrecord Item [x y tag type vel timer radius gc-up])

(defrecord PolygonBullet [rot-angle vectors x y vel type radius timer])

(defrecord BulletShooter [type x y radius tag mtag hp timer exempt-once ngc])

(defrecord SpellCard [type tag dtag timer ngc hp])
