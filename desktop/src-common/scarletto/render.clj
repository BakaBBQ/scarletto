(ns scarletto.render
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [scarletto.factory :as f]
            [play-clj.core :refer :all]
            [scarletto.consts :refer :all]
            [scarletto.config :as c]
            [scarletto.magic :as magic]
            [clojure.core.typed :refer :all]
            [play-clj.g2d :refer :all])
  (:import [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect BitmapFont TextureRegion]
           [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx Gdx]
           [org.ninesyllables.scarletto BackgroundUtils]
           [com.badlogic.gdx.scenes.scene2d Stage]
           [com.badlogic.gdx.math Vector2 Vector3]
           [com.badlogic.gdx.graphics FPSLogger Color]
           [org.ninesyllables.scarletto BlurUtils]
           [com.badlogic.gdx.graphics Pixmap]
           [com.badlogic.gdx.graphics OrthographicCamera PerspectiveCamera]
           [com.badlogic.gdx.graphics.glutils ShapeRenderer ShapeRenderer$ShapeType]
           [com.badlogic.gdx.graphics.g3d.decals Decal DecalBatch CameraGroupStrategy]
           [com.badlogic.gdx.graphics.g2d ParticleEffectPool ParticleEffect ParticleEffectPool$PooledEffect]))

(defmacro set-all [object & fields-and-values]
  (let [obj-sym (gensym)]
    `(let [~obj-sym ~object]
        ~@(for [[field value] (partition 2 fields-and-values)]
           `(set! (. ~obj-sym ~field)
                  ~value)))))

(defprotocol spritebatch-utils
  (set-color [this r g b a])
  (draw-batch [this tex x y])
  (draw-with-zx-zy [this tex x y zx zy]))

(defprotocol bitmapfont-draw
  (draw [this batch string x y])
  (draw-center [this batch string x y w]))

(extend-protocol spritebatch-utils
  SpriteBatch
  (set-color [this r g b a] (.setColor ^SpriteBatch this ^double r ^double g ^double b ^double a))
  (draw-batch [this tex x y] (.draw ^SpriteBatch this ^TextureRegion tex ^double x ^double y))
  (draw-with-zx-zy [this tex x y zx zy] (.draw ^SpriteBatch this ^TextureRegion tex ^double x ^double y (/ (.getRegionWidth ^TextureRegion tex) 2) (/ (.getRegionHeight ^TextureRegion tex) 2) (.getRegionWidth ^TextureRegion tex) (.getRegionHeight ^TextureRegion tex) ^double zx ^double zy 0.0)))

(extend-protocol bitmapfont-draw
  BitmapFont
  (draw [this batch string x y] (.draw ^BitmapFont this ^SpriteBatch batch ^CharSequence string ^double x ^double y))
  (draw-center [this batch string x y w] (.draw ^BitmapFont this ^SpriteBatch batch ^CharSequence string ^double x ^double y ^double w 1 false)))

(defn add-decal-to-screen! [screen :- Any, decal :- Decal]
  (let [batch ^DecalBatch (:decal-batch screen)]
    (.add batch ^Decal decal)))

(defn draw-in-center-with-rotation
  "Calling .draw with batch textureregion to render texture, with rotation"
  [batch tr x y rotation]
  (let [w (.getRegionWidth ^TextureRegion tr)
        h (.getRegionHeight ^TextureRegion tr)
        ^double rx (- x (/ w 2))
        ^double ry (- y (/ h 2))]
    (.draw ^SpriteBatch batch ^TextureRegion tr rx ry (/ w 2) (/ h 2) w h 1 1 ^double (+ 270 rotation))))

(defn draw-on-batch
  [batch tr x y]
  (let [w (.getRegionWidth ^TextureRegion tr)
        h (.getRegionHeight ^TextureRegion tr)]
    (.draw ^SpriteBatch batch ^TextureRegion tr ^double x ^double y)))

(defn draw-in-center
  "calling .draw to draw batch at center"
  [^SpriteBatch batch ^TextureRegion tr x y]
  (let [w (.getRegionWidth tr)
        h (.getRegionHeight tr)
        ^double rx (- x (/ w 2))
        ^double ry (- y (/ h 2))]
    (.draw batch tr rx ry)))

(defn draw-in-center-with-rotation-and-zoom
  [batch :- SpriteBatch tr :- TextureRegion x :- Num y :- Num rotation :- Num zoom :- Num]
  (let [w (.getRegionWidth ^TextureRegion tr)
        h (.getRegionHeight ^TextureRegion tr)
        ^double rx (- x (/ w 2))
        ^double ry (- y (/ h 2))]
    (.draw ^SpriteBatch batch ^TextureRegion tr rx ry (/ w 2) (/ h 2) w h zoom zoom ^double (+ 270 rotation))))

(defn draw-in-center-with-rotation-and-zoom-rx-ry
  [batch :- SpriteBatch tr :- TextureRegion x :- Num y :- Num rotation :- Num zoom :- Num rx :- Num ry :- Num]
  (let [w (.getRegionWidth ^TextureRegion tr)
        h (.getRegionHeight ^TextureRegion tr)
        ^double hx (- x (/ w 2))
        ^double hy (- y (/ h 2))]
    (.draw ^SpriteBatch batch ^TextureRegion tr hx hy rx ry w h zoom zoom ^double rotation)))

(defmulti render-debug-entity
  (fn [entity renderer]
    (entity :type)))

(defmethod render-debug-entity :player [entity ^ShapeRenderer renderer]
  (if (:collide entity)
    (.setColor renderer 1 0 0 1)
    (.setColor renderer 0 1 0 1))
  (.circle renderer (entity :x) (entity :y) (entity :radius))
  (.setColor renderer 0 1 1 1)
  (doseq [^Vector2 v (f/get-player-option-pos entity)
          :let [px (:x entity)
                py (:y entity)
                rx (+ (.x v) px)
                ry (+ (.y v) py)]]
    (.circle renderer rx ry 5)))

(defmethod render-debug-entity :circle [entity ^ShapeRenderer renderer]
  (.setColor renderer 1 1 1 1)
  (.circle renderer (entity :x) (entity :y) (entity :radius)))

(defmethod render-debug-entity :shooter [entity ^ShapeRenderer renderer]
  (.setColor renderer 1 0 0 1)
  (.circle renderer (entity :x) (entity :y) (entity :radius)))

(defmethod render-debug-entity :polygon [entity ^ShapeRenderer renderer]
  (.setColor renderer 1 1 1 1)
  (let [vecs (:vectors entity)
        ^double x (:x entity)
        ^double y (:y entity)
        exp-vec (flatten (map (fn [^Vector2 v] [(+ x (.x v)) (+ y (.y v))]) vecs))]
    (.polygon renderer (float-array exp-vec))))

(defmethod render-debug-entity :item [entity ^ShapeRenderer renderer]
  (.setColor renderer 1 1 0 1)
  (let [i entity
        r 4
        ix (:x i)
        iy (:y i)
        rx (- ix r)
        ry (- iy r)
        w (* 2 r)
        h w]
    (.rect renderer rx ry w h)))

(defmethod render-debug-entity :pbullet [entity ^ShapeRenderer renderer]
  (.setColor renderer 0 1 1 1)
  (.circle renderer (:x entity) (:y entity) (:radius entity)))

(defmethod render-debug-entity :default [entity renderer])

(defn render-debug
  "debug renderer for collisions"
  [{:keys [shape-renderer ortho-cam] :as screen} entities]
  (let [^ShapeRenderer renderer shape-renderer
        ^OrthographicCamera cam ortho-cam]
    (.update cam)
    (.setProjectionMatrix renderer (.combined cam))
    (.begin renderer (ShapeRenderer$ShapeType/Line))
    (.setColor renderer 1 1 1 1)
    (doseq [entity entities]
      (render-debug-entity entity renderer))
    (.end renderer))
  entities)

(defn get-star-rotation
  [t]
  (let [rt (- 300 t)]
    (Math/pow t 1.4)))

(defn get-star-size
  [t]
  (let [rt (- 300 t)]
    (cond
     (< rt 60) (* rt 1/60)
     (> rt 240) (* (- 300 rt) 1/60)
     :default 1.0)))

(defn abs-x [x]
  (- x 34))

(defn abs-y [y]
  (- y 25))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn get-render-type [e]
  (let [m [:circle :polygon]]
    (if (in? m (:type e))
      :bullet
      (:type e))))

(defmulti render-real-entity
  (fn [entity ^SpriteBatch batch font screen]
    (get-render-type entity)))

(defn draw-font [font batch st x y]
  (.draw ^BitmapFont font ^Batch batch ^CharSequence st ^double x ^double y))

(defmethod render-real-entity :fps-counter [entity batch ^BitmapFont font screen]
  (draw-font font batch (str "fps: " (:fps entity)) 765 0))

(defmethod render-real-entity :dialog
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [^TextureRegion tex (:message-back screen)

        fading? (integer? (:ftimer entity))

        non-fading-opacity (min 1.0 (* 0.1 (:timer entity)))
        ftimer (or (:ftimer entity) 0)
        fading-opacity (max 0.0 (- 1 (* 0.1 ftimer)))
        f (:message-font screen)
        opacity (if fading?
                  fading-opacity
                  non-fading-opacity)
        ^Color ori-color (.getColor batch)]
    (.setColor batch (.r ori-color) (.g ori-color) (.b ori-color) opacity)
    (.draw batch tex 49.0 (- 720.0 683.0))
    (draw ^BitmapFont f batch ^CharSequence (:str entity) 77 (- 720 542))
    (.setColor batch ori-color)))


(defmethod render-real-entity :new-message [{:keys [name msg] :as entity} ^SpriteBatch batch ^BitmapFont font screen]
  (let [^TextureRegion tex (:message-back screen)
        ^BitmapFont f (:message-font screen)
        ^BitmapFont fb (:message-font-big screen)
        fading? (integer? (:ftimer entity))

        non-fading-opacity (min 1.0 (* 0.1 (:timer entity)))
        ftimer (or (:ftimer entity) 0)
        fading-opacity (max 0.0 (- 1 (* 0.1 ftimer)))
        upper (:pos entity)
        opacity (if fading?
                  fading-opacity
                  non-fading-opacity)]
    (let [x1 -80
          y1 (if upper 350 100)]
      (do
        (set-color batch 1 1 1 opacity)
        (draw-on-batch batch tex x1 y1)
        (draw-font fb batch name (+ 100 150 80 x1) (+ 190 y1))
        (draw-font f batch (str "[WHITE]" msg) (+ 100 180 80 x1) (+ 140 y1))
        (set-color batch 1 1 1 1)))))

(defn get-bullet-row [bullet]
  (case (:graphics-type bullet)
    :big-star 0
    :big-circle 1
    :big-butterfly 2
    :big-knife 3
    :big-oval 4
    :scale 1
    :ring 2
    :circle 3
    :crystal 6
    :rice 4
    :giant 3
    :star 10))

(defn get-bullet-asset-type [bullet]
  (case (:graphics-type bullet)
    :big-star :textures-big
    :big-circle :textures-big
    :big-butterfly :textures-big
    :big-knife :textures-big
    :big-oval :textures-big
    :giant :textures-giant
    :bullet-textures))

(defn get-bullet-column [bullet]
  (:color bullet))

(defn get-bullet-texture-region-e [bullet screen]
  (let [r (get-bullet-row bullet)

        c (get-bullet-column bullet)]
    ^TextureRegion (nth (nth ((get-bullet-asset-type bullet) screen) c) r)))

(defn get-player-texture-bounds [player]
  (let [vel (:velocity player)
        abs-vel (if (neg? vel)
                  (* vel -1))
        t (:timer player)
        raw-bounds (cond
                    (pos? vel) (if (= vel 7)
                                 [(+ vel (- (quot (mod (quot t 5) 15) 3) 4)) 1]
                                 [vel 1])
                    (neg? vel) (if (= abs-vel 7)
                                 [(* (+ abs-vel (- (quot (mod (quot t 5) 15) 3) 4)) 1) 1]
                                 [(* vel -1) 1])
                    :else [(int (/ (mod (quot t 2) 24) 3)) 0])]
    [(* 48 (first raw-bounds)) (* 72 (last raw-bounds))]))

(defn get-player-texture-flipped? [player]
  (let [vel (:velocity player)]
    (pos? vel)))

(defn get-player-texture [player screen]
  (let [^TextureRegion t (:player-texture screen)
        bounds (get-player-texture-bounds player)
        flipped (get-player-texture-flipped? player)
        exact-region (TextureRegion. ^TextureRegion t ^int (first bounds) ^int (last bounds) 48 72)]
    (f/flip-texture exact-region flipped false)))

(defn get-player-focus-texture [player screen]
  (let [^TextureRegion t (:etama2 screen)
        exact-region (TextureRegion. t 0 24 96 96)]
    exact-region))

(defn get-player-option-texture [player screen]
  (:option screen))

(defn get-player-bullet-texture [bullet screen]
  (let [^TextureRegion t (if (= (:dmg bullet) 3)
                           (:reimu-shot2 screen)
                           (:paper screen))
        exact-region t]
    exact-region))

(def ^TextureRegion get-bullet-texture-region get-bullet-texture-region-e)


(defn draw-pure-bullet
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [
        ^TextureRegion btex (get-bullet-texture-region entity screen)
        x (:x entity)
        y (:y entity)
        t (:timer entity)
        zoom (max (- 1.5 (* 1/20 t)) 1)
        opacity-mult (if (= (:graphics-type entity) :giant) 0.9 1.0)
        opacity (* (min (* t 1/10) 1) opacity-mult)]
    (do
      (set-color batch 1 1 1 opacity)
      (draw-in-center-with-rotation-and-zoom batch btex x y (:rot-angle entity) zoom)
      (set-color batch 1 1 1 1))))

(defn draw-appear-effect
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [^TextureRegion etex (first (:appear-textures screen))
        x (:x entity)
        y (:y entity)
        t (:timer entity)
        zoom (* 1.2 (- 1 (/ t 10)))]
    (draw-in-center-with-rotation-and-zoom batch etex x y (:rot-angle entity) zoom)))

(defmethod render-real-entity :bullet
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [t (:timer entity)]
    (draw-pure-bullet entity batch font screen)))

(defmethod render-real-entity :stage-text
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [
        t (:timer entity)
        index (:index entity)

        textures-blah (:3c (:stage-textures screen))
        stage-name-tex (nth textures-blah 0)
        stage-sub-tex1 (nth textures-blah 1)
        stage-sub-tex2 (nth textures-blah 2)

        stage-name-y 0
        stage-name-x (cond
                      (<= t 60) (- (- 60 t))
                      (<= t 240) 0
                      (<= t 300) (let [at (- t 240)]
                                   (- (- 60 (- 60 at))))
                      :default 0)

        subtex-x 0
        subtex-y 0
        subtex-zx (cond
                      (<= t 60) (+ 1 (* (- 60 t) 1/60))
                      (<= t 240) 1
                      (<= t 300) (+ 1 (* (- t 240) 1/60))
                      :default 1.0)
        subtex-zy (cond
                      (<= t 60) (+ 0 (* t 1/60))
                      (<= t 240) 1
                      (<= t 300) (+ 0 (* (- 60 (- t 240)) 1/60))
                      :default 1.0)
        opacity (if (> t 60)
                  (if (> t 240)
                    (let [at (- t 240)]
                      (- 1 (min (* at 1/60) 1)))
                    1)
                  (* (/ 1 60) t))]
    (do
      (.setColor batch 1 1 1 opacity)
      (draw-with-zx-zy batch ^TextureRegion stage-name-tex stage-name-x stage-name-y subtex-zx subtex-zy)
      (draw-with-zx-zy batch ^TextureRegion stage-sub-tex1 subtex-x stage-name-y subtex-zx subtex-zy)
      (draw-with-zx-zy batch ^TextureRegion stage-sub-tex2 subtex-x stage-name-y subtex-zx subtex-zy)
      (.setColor batch 1 1 1 1))))

(defn render-player-when-dead
  [entity ^SpriteBatch batch ^BitmapFont font screen])

(defn render-player-normal
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (draw-in-center batch (get-player-texture entity screen) (:x entity) (:y entity))
  (.setColor batch 1 1 1 (/ (:focused entity) 40.0))
  (let [t (:timer entity)
        rot (mod t 360)
        ftexture (get-player-focus-texture entity screen)]
    (do
      (draw-in-center-with-rotation batch ftexture (:x entity) (:y entity) rot)
      (draw-in-center-with-rotation batch ftexture (:x entity) (:y entity) (- 360 rot))))
  (.setColor batch 1 1 1 1))

(defn render-player-when-invincible
  ;; we need this blink effect
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [t (:timer entity)
        rt (quot t 5)]
    (if (even? rt)
      (render-player-normal entity batch font screen))))

(defmethod render-real-entity :player
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (cond
   (f/player-dead? entity) (render-player-when-dead entity batch font screen)
   (f/player-invincible? entity) (render-player-when-invincible entity batch font screen)
   :default (render-player-normal entity batch font screen))
  (if-not (f/player-dead? entity)
    (doseq [i (range (count (f/get-player-option-pos entity)))
          :let [^Vector2 v (nth (f/get-player-option-pos entity) i)
                ^ParticleEffectPool$PooledEffect e (nth (:option-effects screen) i)
                px (:x entity)
                py (:y entity)
                rx (+ (.x v) px)
                ry (+ (.y v) py)
                t (:timer entity)
                angle (mod (* 2 t) 360)]]
    (do
      (comment draw-in-center-with-rotation batch (get-player-option-texture entity screen) rx ry angle)
      (.draw (doto e
               (.setPosition rx ry)) batch (.getDeltaTime Gdx/graphics)))))
  (if (pos? (:bomb-timer entity))
    (let [x (:x entity)
        y (:y entity)
        t (:bomb-timer entity)]
      (do
        (.draw (doto ^ParticleEffect (:star-effect screen)
               (.setPosition x (- y 0))) batch (.getDeltaTime Gdx/graphics))
        (draw-in-center-with-rotation-and-zoom-rx-ry batch (:sanae-bomb screen) x y (get-star-rotation t) (get-star-size t) 197 169))
    )))

(defmethod render-real-entity :pbullet
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [t (:timer entity)
        opacity (min (* 0.1 t) 1)]
    (do
      (.setColor batch 1 1 1 (* 0.6 opacity))
      (if (:ttimer entity)
        (draw-in-center-with-rotation batch (get-player-bullet-texture entity screen) (:x entity) (:y entity) 0)
        (draw-in-center-with-rotation batch (get-player-bullet-texture entity screen) (:x entity) (:y entity) (.angle ^Vector2 (:vel entity))))
      (.setColor batch 1 1 1 1))))


(defn is-horz-moving-at-frame-before-ticks-with-dir?
  [entity dir f]
  (let [s entity
        ticks (* 3 f)
        t (- (:timer s) ticks)
        p (:path s)
        m (:movement s)
        ^Vector2 p (f/calc-point-derivative t p m)
        dx (.x p)
        dy (.y p)
        rt (quot t 3) ;;revamped t
        horz-moving? (> (Math/abs dx) 0.5)
        r (if horz-moving?
            (= (pos? dx) (pos? dir))
            false)]
    r))



(defn get-dx-of-shooter
  [entity]
    (let [s entity
        t (:timer entity)
        p (:path s)
        m (:movement s)
        ^Vector2 p (f/calc-point-derivative t p m)
        dx (.x p)]
    dx))

(defn judge-movement
  ;; if the shooter has moved during the previous 1 tick => 1
  ;; if the shooter has moved during the previos 2 ticks => 2
  ;; if the shooter has moved during the previous 3 ticks => 3
  ;; if the shooter has moved during the previous 4 ticks => 4
  [entity]
  (let [current-movement-dir (get-dx-of-shooter entity)]
    (count
     (transduce (comp (map (partial is-horz-moving-at-frame-before-ticks-with-dir? entity current-movement-dir))
                  (filter true?)) conj
            [1 2 3 4 5]))))

(defn get-shooter-style
  [shooter]
  (if (:style shooter)
    (:style shooter)
    0))

(defn render-normal-shooter
  [entity ^SpriteBatch batch ^BitmapFont font screen]
    (let [s entity
          movement (judge-movement entity)
        t (:timer s)
        p (:path s)
        m (:movement s)
        ^Vector2 p (f/calc-point-derivative t p m)
        dx (.x p)
        dy (.y p)
        rt (int (/ t 3)) ;;revamped t
        horz-moving? (> (Math/abs dx) 0.5)

        tindex (if horz-moving?
                 (+ 5 (if (>= movement 4)
                        (+ 3 (mod rt 3))
                        movement))
                 (mod rt 5))

        flipped (if horz-moving?
                  (neg? dx)
                  false)
        texs (:enemy-textures screen)
        ^TextureRegion tex (nth (nth texs tindex) (get-shooter-style entity))
        nt (TextureRegion. tex)
        n (.flip nt flipped false)
        x (:x s)
        y (:y s)]
    (do
      (draw-in-center batch nt x y))))

(defn render-boss-shooter
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [s entity
        movement (judge-movement entity)
        hexagram-tex (:hexagram screen)
        t (:timer s)
        p (:path s)
        m (:movement s)
        ^Vector2 p (f/calc-point-derivative t p m)
        dx (.x p)
        dy (.y p)
        rt (quot t 3)
        rt2 (quot t 12)
        horz-moving? (> (Math/abs dx) 0.5)
        flipped (if horz-moving?
                  (neg? dx)
                  false)
        texs (:kaguya screen)
        tex-y (if horz-moving? 1 2)
        ^BitmapFont font (:pron-font screen)
        tex-x (if horz-moving?
                (min (quot movement 12) 2)
                (mod rt 3))
        tex (nth (nth texs tex-x) tex-y)
        nt (f/flip-texture tex flipped false)
        size (+ (* (Math/sin (Math/toRadians (mod t 360))) 0.1) 1)
        msize (* 1.0 size)
        opacity (+ (* (Math/sin (Math/toRadians (mod t 360))) 0.2) 0.6)
        x (:x s)
        y (:y s)]
    (do
      (.setColor batch 1 1 1 opacity)
      (draw-in-center-with-rotation-and-zoom batch hexagram-tex x y (mod (* (min (Math/pow t 1.1) (* t 3600)) 2) 360) msize)
      (.setColor batch 1 1 1 1)
      (draw-center font batch (:name entity) 0 400 960)
      (.draw (doto ^ParticleEffect (:flame-effect screen)
               (.setPosition x (- y 0))) batch (.getDeltaTime Gdx/graphics))
      (draw-in-center batch nt x y))))

(defmethod render-real-entity :shooter
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (if (:boss entity)
    (render-boss-shooter entity ^SpriteBatch batch ^BitmapFont font screen)
    (render-normal-shooter entity ^SpriteBatch batch ^BitmapFont font screen)))

(defmethod render-real-entity :explosion
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [t (:timer entity)
        c (:color entity)
        explosion-textures (:explosion-textures screen)
        ^TextureRegion tex (get explosion-textures c)
        opacity (max (- 1 (* 0.05 t)) 0)
        size (* t (/ 1 20) 1.8)]
    (do
      (.setColor batch 1 1 1 opacity)
      (draw-in-center-with-rotation-and-zoom batch tex (:x entity) (:y entity) 0 size)
      (.setColor batch 1 1 1 1))))

(defmethod render-real-entity :particle
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [^ParticleEffectPool$PooledEffect p (:object entity)
        dtime (.getDeltaTime Gdx/graphics)]
    (do
      (.draw p batch dtime))))


(defn orig-bullet-size
  [t]
  (+ 1 (* t 1/60)))
(defn orig-bullet-opacity
  [t]
  (let [ti (- 30 t)]
    (* ti 1/30)))

(defn draw-orig-bullet?
  [b]
  (< (:timer b) 30))

(defn draw-orig-bullet
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (if (and (:orig-bullet entity) (draw-orig-bullet? entity))
    (let [t (:timer entity)
          b (:orig-bullet entity)]
      (do
        (.setColor batch 1 1 1 (orig-bullet-opacity t))
        (draw-in-center-with-rotation-and-zoom batch (get-bullet-texture-region b screen) (:x b) (:y b) (:rot-angle b) (orig-bullet-size t))
        (.setColor batch 1 1 1 1)))))

(defmethod render-real-entity :item
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [t (:timer entity)
        ^ParticleEffectPool$PooledEffect magical-item-eff (:magical-item-effect screen)
        index (case (:tag entity)
                :power 0
                :score 1
                :big-p 3
                :shard 7)
        draw-graphics (not= (:tag entity) :shard)
        ^TextureRegion item-tex (nth (:item-textures screen) index)
        rotation (if (> t 30)
                   90
                   (+ 90 (mod (* t 24) 360)))
        opacity (if (= (:tag entity) :shard) 0.5 1)]
    (do
      (.setColor batch 1 1 1 opacity)
      (draw-in-center-with-rotation batch item-tex (:x entity) (:y entity) rotation)

      (draw-orig-bullet entity batch font screen)
      (.setColor batch 1 1 1 1))
      ))



(defmethod render-real-entity :face
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [texs (:face-textures screen)
        ^TextureRegion tex (:name texs)
        w (.getRegionWidth tex)
        xpos (case (:pos entity)
               0 0
               1 (- c/stage-right-bound w))
        ypos 0]
    (draw-on-batch batch tex xpos ypos)))

(defmethod render-real-entity :default [entity ^SpriteBatch batch font screen])

(defn get-front-frame-texture [screen]
  (:front-texture screen))



(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))



(defn render-sc-background
  [^SpriteBatch batch entities {:keys [entities-grouped timer ^ShapeRenderer shape-renderer] :as screen}]
  (if (and (first (:sc entities-grouped)) (:graphics (first (:sc entities-grouped))))
    (let [sc (first (:sc entities-grouped))
          res (:kaguya-sc-background screen)
          ^BitmapFont f (:ename-font screen)
          boss (filter :boss (:shooter entities-grouped))
          b (first boss)
          hp (:hp b)
          mhp (:hp sc)
          ^TextureRegion grunge (:grunge res)
          background (:background res)
          front (:front res)
          ^long t (:timer sc)
          current-time (- (:timeout sc) t)
          current-time-f (str (int (Math/ceil ^double (f/frames-to-seconds current-time))))
          global-opacity-mult (min (* t 1/30) 1)
          h (.getRegionWidth grunge)
          y-offset (mod (* 2 timer) h)
          xo -22
          yo -24]
      (set-color batch 1 1 1 global-opacity-mult)
      (draw-batch batch background xo yo)
      (draw-font f batch current-time-f 100 100)
      (set-color batch 1 1 1 (* 0.2 global-opacity-mult))
      (draw-batch batch grunge xo (+ yo y-offset h))
      (draw-batch batch grunge xo (+ yo y-offset))
      (draw-batch batch grunge xo (+ yo y-offset (- h)))
      (set-color batch 1 1 1 global-opacity-mult)
      (draw-batch batch front xo yo)
      (set-color batch 1 1 1 1))))

(defn get-all-black-opacity
  [screen]
  (let [t (:timer screen)]
    (- 1 (min (* t 1/80) 1))))

(defn get-all-black-zoom [screen]
  (let [t (:timer screen)]
    (+ 0.5 (min (* t 1/160) 0.5))))

(defn get-background-opacity
  [screen]
  (let [t (:timer screen)]
    (cond
     (<= t 50) 1
     :default (let [rt (- t 50)]
                (- 1 (min (* rt 1/80) 1))))))

(defn render-and-update-back-to-title
  [btimer ^TextureRegion texture ^SpriteBatch batch]
  (do
    (set-color batch 1 1 1 (min (* btimer 1/60) 1))
    (draw-on-batch batch texture 0 0)
    (if (>= btimer 60)
      (magic/switch-to-title!))
    (set-color batch 1 1 1 1)))

(defn render-real
  "the real renderer for game"
  [{:keys [^TextureRegion test-title ^Stage renderer back-to-title ^SpriteBatch speedbatch ^DecalBatch decal-batch font ^OrthographicCamera ortho-cam ^OrthographicCamera hub-cam ^SpriteBatch hub-batch entities-grouped] :as screen} entities]

  (let [^SpriteBatch batch (.getBatch renderer)
        black-opacity (get-all-black-opacity screen)
        black-tex ^TextureRegion (:all-black screen)
        ^BackgroundUtils utils (:background-utils screen)]
    (do
      (.render utils)
      (if-not (or (:gameover screen) (:paused screen))
        (let [^PerspectiveCamera c (:3d-cam screen)
              p (first entities)
              t (:timer p)
              x (+ (mod (* 2 t) 512) 512)
              ^Vector3 pos (.position c)]
          (do
            (.rotate c (* (Math/sin (Math/toRadians t)) -0.1) 0 1 0)
            (set! (.x pos) x)
            (.update c))))
      (doseq [^Decal d (:decals screen)]
        (do
          (.add decal-batch ^Decal d)))
      (comment .flush decal-batch)
      (set! (.zoom hub-cam) 1.0)
      (.update ortho-cam)
      (.setProjectionMatrix speedbatch (.combined hub-cam))
      (.begin speedbatch)
      (set-color speedbatch 1 1 1 (get-background-opacity screen))
      (draw-batch speedbatch black-tex 0 0)
      (set-color speedbatch 1 1 1 1)
      (.end speedbatch)
      (set! (.zoom ortho-cam) (get-all-black-zoom screen))
      (.update ortho-cam)
      (.setProjectionMatrix batch (.combined ortho-cam))
      (.update hub-cam)
      (.setProjectionMatrix hub-batch (.combined hub-cam))
      (let [^ShapeRenderer shaperenderer (:shape-renderer screen)]
        (.setProjectionMatrix shaperenderer (.combined hub-cam)))

      (.begin batch)
      (render-sc-background batch entities screen)
      (doseq [entity entities]
        (render-real-entity entity batch font screen))
      (.end batch)
      (if (first (:sc (:entities-grouped screen)))
        (let [entities-grouped (:entities-grouped screen)
              ^ShapeRenderer shaperenderer (:shape-renderer screen)
              boss (filter :boss (:shooter entities-grouped))
              b (first boss)
              hp (:hp b)
              mhp (:hp (first (:sc (:entities-grouped screen))))
              rate (/ hp mhp)
              rate1 (min 0.5 rate)
              rate2 (- rate rate1)]
          (.begin shaperenderer (ShapeRenderer$ShapeType/Filled))
          (.setColor shaperenderer 1 0.65 0.65 1)
          (.rect shaperenderer 60 (- 720 35) (* 343 rate1 2) 4)
          (.rect shaperenderer 565 (- 720 35) (* 343 rate2 2) 4)
          (.end shaperenderer)))
      (.begin hub-batch)
      (if (first (:sc (:entities-grouped screen)))
        (let [sc (first (:sc (:entities-grouped screen)))
              ^BitmapFont pron (:pron-font screen)
              sc-bonus (f/get-spellcard-bonus sc)]
          (draw-center pron hub-batch (str "Bonus: " (format "%010d" sc-bonus)) 0 680 960)))

      (let [^BitmapFont font (:pron-font screen)]
        (.draw font hub-batch ^CharSequence (format "%010d" (:score (first entities))) 60.0 50.0))
      (.draw hub-batch ^TextureRegion (get-front-frame-texture screen) 0.0 0.0)
      (if-not (empty? (:bonus entities-grouped))
        (let [entity (first (:bonus entities-grouped))
              t (:timer entity)
              ^TextureRegion tex (get (:bonus-textures screen) (not (zero? (:score entity))))
              w (.getRegionWidth tex)
              h (.getRegionHeight tex)
              x (- 480 (/ w 2))
              y (- 360 (/ h 2))]
          (do
            (cond
             (<= t 15) (do
                         (set-color hub-batch 1 1 1 (* t 1/15))
                         (draw-batch hub-batch tex x (+ y t -15)))
             (<= t 75) (do
                         (set-color hub-batch 1 1 1 1)
                         (draw-batch hub-batch tex x y))
             (<= t 90) (do
                         (set-color hub-batch 1 1 1 (* (- 90 t) 1/15))
                         (draw-batch hub-batch tex x (+ y (- t 75)))))
            (draw-center ^BitmapFont (:message-font screen) hub-batch (format "%010d" (:score entity)) 0 330 960))))
      (set-color hub-batch 1 1 1 1)
      (if-not (empty? (filter :boss (:shooter entities-grouped)))
        (let [single-boss (first (filter :boss (:shooter entities-grouped)))
              boss-x (:x single-boss)
              boss-y (:y single-boss)
              ^TextureRegion enemy-bar (:enemy-bar screen)
              h (.getRegionHeight enemy-bar)]
          (do
            (draw-batch hub-batch enemy-bar boss-x 0))))
      (let [fonts (:adobe-arabic screen)
            s (:small fonts)
            m (:middle fonts)
            b (:big fonts)

            x1 418
            y (- 720 663)

            x2 469

            y2 (- 720 680)

            x3 518

            y3 (- 720 689)

            power (:power (first entities))
            first-digit (quot power 100)
            second-digit (mod power 100)
            lives (:lives (first entities))
            first-str (str first-digit ".")
            second-str (format "%02d" second-digit)
            third-str (str "x" lives)]
        (do
          (draw b hub-batch first-str x1 y)
          (draw m hub-batch second-str x2 y2)
          (draw s hub-batch third-str x3 y3)))
      (set-color hub-batch 1 1 1 black-opacity)
      (draw-batch hub-batch black-tex 0 0)
      (set-color hub-batch 1 1 1 1)

      (if back-to-title
        (do
          (update! screen :back-to-title (inc back-to-title))
          (render-and-update-back-to-title back-to-title black-tex hub-batch)))
      (.end hub-batch)
      (if (key-pressed? :escape)
        (do
          (if (and (:paused screen) (> (:paused screen) 10))
            (update! screen :paused-back (:paused screen))
            (update! screen :paused 0))))))
  entities)


(defn render-pause
  [entities {:keys [^Stage renderer ^TextureRegion paused-screenshot ^DecalBatch decal-batch font ^OrthographicCamera ortho-cam ^SpriteBatch hub-batch] :as screen} ]
  (let [^SpriteBatch batch (.getBatch renderer)
        ^TextureRegion all-black (:all-black screen)
        t (:paused screen)
        opacity (if (:paused-back screen)
                  (min 0.5 (* 0.01 (- t (:paused-back screen))))
                  (min 0.5 (* 0.01 t)))]
    (do
      (.begin hub-batch)
      (set-color hub-batch 1 1 1 opacity)
      (draw-batch hub-batch all-black 0 0)
      (set-color hub-batch 1 1 1 1)
      ;(draw-on-batch hub-batch paused-screenshot 0 0)
      (.end hub-batch))))

(defn render-and-update-gameover
  [entities {:keys [^SpriteBatch hub-batch gameover-timer] :as screen}]
  (.begin hub-batch)
  (let [t gameover-timer
        assets (:gameover-assets screen)
        background (:background assets)
        flash (:flash assets)
        title (:title assets)
        rx 383
        ry (- 720 408)]
    (do
      (set-color hub-batch 1 1 1 1)
      (draw-on-batch hub-batch background 0 0)
      (set-color hub-batch 1 1 1 (/ (+ 60 (Math/abs (+ -40 (mod (quot t 2) 80)))) 100))
      (draw-on-batch hub-batch flash rx ry)
      (set-color hub-batch 1 1 1 (min (* t 1/20) 1))
      (draw-on-batch hub-batch title rx ry)
      (set-color hub-batch 1 1 1 1)
      (if (key-pressed? :x)
        (magic/switch-to-title!))))
  (.end hub-batch))
