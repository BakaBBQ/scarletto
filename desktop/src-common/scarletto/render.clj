(ns scarletto.render
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [scarletto.factory :as f]
            [play-clj.core :refer :all]
            [scarletto.consts :refer :all]
            [clojure.core.typed :refer :all]
            [play-clj.g2d :refer :all])
  (:import [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect BitmapFont TextureRegion]
           [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx Gdx]
           [com.badlogic.gdx.scenes.scene2d Stage]
           [com.badlogic.gdx.math Vector2 Vector3]
           [com.badlogic.gdx.graphics FPSLogger Color]
           [com.badlogic.gdx.graphics OrthographicCamera PerspectiveCamera]
           [com.badlogic.gdx.graphics.glutils ShapeRenderer ShapeRenderer$ShapeType]
           [com.badlogic.gdx.graphics.g3d.decals Decal DecalBatch CameraGroupStrategy]
           [com.badlogic.gdx.graphics.g2d ParticleEffectPool ParticleEffect ParticleEffectPool$PooledEffect]))

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
    (.draw ^SpriteBatch batch ^TextureRegion tr rx ry (/ w 2) (/ h 2) w h zoom zoom ^double rotation)))

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

(defmethod render-real-entity :fps-counter [entity batch ^BitmapFont font screen]
  (.draw font batch (str "fps: " (:fps entity)) 765 0))

(defmethod render-real-entity :dialog
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [tex-obj (texture "dialog.png")
        ^TextureRegion tex (:object tex-obj)

        fading? (integer? (:ftimer entity))

        non-fading-opacity (min 1.0 (* 0.1 (:timer entity)))
        ftimer (or (:ftimer entity) 0)
        fading-opacity (max 0.0 (- 1 (* 0.1 ftimer)))
        opacity (if fading?
                  fading-opacity
                  non-fading-opacity)
        ^Color ori-color (.getColor batch)]
    (.setColor batch (.r ori-color) (.g ori-color) (.b ori-color) opacity)
    (.draw batch tex 10.0 10.0)
    (.draw font batch (:str entity) 20 50)
    (.setColor batch ori-color)))

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
    :star 10))

(defn get-bullet-asset-type [bullet]
  (case (:graphics-type bullet)
    :big-star :textures-big
    :big-circle :textures-big
    :big-butterfly :textures-big
    :big-knife :textures-big
    :big-oval :textures-big
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
                                 [(+ vel (- (quot (mod t 15) 3) 4)) 2]
                                 [vel 2])
                    (neg? vel) (if (= abs-vel 7)
                                 [(* (+ abs-vel (- (quot (mod t 15) 3) 4)) 1) 1]
                                 [(* vel -1) 1])
                    :else [(int (/ (mod t 24) 3)) 0])]
    [(* 48 (first raw-bounds)) (* 72 (last raw-bounds))]))

(defn get-player-texture [player screen]
  (let [^TextureRegion t (:player-texture screen)
        bounds (get-player-texture-bounds player)
        exact-region (TextureRegion. ^TextureRegion t ^int (first bounds) ^int (last bounds) 48 72)]
    exact-region))

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
        y (:y entity)]
    (draw-in-center-with-rotation batch btex x y (:rot-angle entity))))

(defn draw-appear-effect
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [^TextureRegion etex (first (:appear-textures screen))
        x (:x entity)
        y (:y entity)
        t (:timer entity)
        zoom (* 0.6 (- 1 (/ t 20)))]
    (draw-in-center-with-rotation-and-zoom batch etex x y (:rot-angle entity) zoom)))

(defmethod render-real-entity :bullet
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [t (:timer entity)]
    (if (<= t 10)
      (draw-appear-effect entity batch font screen)
      (draw-pure-bullet entity batch font screen))))

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
        stage-name-x 0
        opacity (if (> t 60)
                  (if (> t 240)
                    0
                    1)
                  (* (/ 1 60) t))]
    (do
      (.setColor batch 1 1 1 opacity)
      (draw-on-batch batch ^TextureRegion stage-name-tex stage-name-x stage-name-y)
      (draw-on-batch batch ^TextureRegion stage-sub-tex1 stage-name-x stage-name-y)
      (draw-on-batch batch ^TextureRegion stage-sub-tex2 stage-name-x stage-name-y)
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
  (.setColor batch 1 1 1 1)
  (doseq [^Vector2 v (f/get-player-option-pos entity)
          :let [px (:x entity)
                py (:y entity)
                rx (+ (.x v) px)
                ry (+ (.y v) py)
                t (:timer entity)
                angle (mod (* 2 t) 360)]]
    (draw-in-center-with-rotation batch (get-player-option-texture entity screen) rx ry angle)))

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
   :default (render-player-normal entity batch font screen)))

(defmethod render-real-entity :pbullet
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [t (:timer entity)
        opacity (min (* 0.1 t) 1)]
    (do
      (.setColor batch 1 1 1 (* 0.6 opacity))
      (draw-in-center-with-rotation batch (get-player-bullet-texture entity screen) (:x entity) (:y entity) (.angle ^Vector2 (:vel entity)))
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
        tex-x (if horz-moving?
                (min (quot movement 12) 2)
                (mod rt 3))
        tex (nth (nth texs tex-x) tex-y)
        nt (f/flip-texture tex flipped false)
        size (+ (* (Math/sin (Math/toRadians (mod t 360))) 0.1) 1)
        msize (* 1.6 size)
        opacity (+ (* (Math/sin (Math/toRadians (mod t 360))) 0.2) 0.6)
        x (:x s)
        y (:y s)]
    (do
      (.setColor batch 1 1 1 opacity)
      (draw-in-center-with-rotation-and-zoom batch hexagram-tex x y (mod (* t 3) 360) msize)
      (.setColor batch 1 1 1 1)
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

(defmethod render-real-entity :item
  [entity ^SpriteBatch batch ^BitmapFont font screen]
  (let [t (:timer entity)
        ^TextureRegion item-tex (first (:item-textures screen))
        rotation (if (> t 30)
                   90
                   (+ 90 (mod (* t 24) 360)))]
    (draw-in-center-with-rotation batch item-tex (:x entity) (:y entity) rotation)))

(defmethod render-real-entity :default [entity ^SpriteBatch batch font screen])

(defn get-front-frame-texture [screen]
  (:front-texture screen))

(defn render-real
  "the real renderer for game"
  [{:keys [^Stage renderer ^DecalBatch decal-batch font ^OrthographicCamera ortho-cam ^SpriteBatch hub-batch] :as screen} entities]

  (let [^SpriteBatch batch (.getBatch renderer)]
    (do
      (let [^PerspectiveCamera c (:3d-cam screen)
            p (first entities)
            t (:timer p)
            x (+ (mod (* 2 t) 512) 512)
            ^Vector3 pos (.position c)]
        (do
          (.rotate c (* (Math/sin (Math/toRadians t)) -0.1) 0 1 0)
          (set! (.x pos) x)
          (.update c)))
      (doseq [^Decal d (:decals screen)]
        (do
          (if (key-pressed? :k)
            (.translateX d -1))
          (if (key-pressed? :l)
            (.translateX d 1))
          (.add decal-batch ^Decal d)))
      (.flush decal-batch)
      (.update ortho-cam)
      (.setProjectionMatrix batch (.combined ortho-cam))
      (.begin batch)
      (doseq [entity entities]
        (render-real-entity entity batch font screen))
      (.end batch)
      (.begin hub-batch)
      (.draw hub-batch ^TextureRegion (get-front-frame-texture screen) 0.0 0.0)

      (.end hub-batch)
      (if (key-pressed? :o)
        (screenshot! "screenshot.png"))))
  entities)
