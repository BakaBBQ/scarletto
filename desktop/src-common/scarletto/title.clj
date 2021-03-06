(ns scarletto.title
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [scarletto.render :as r]
            [scarletto.magic :as m]
            [scarletto.particles :as p]
            [clojure.core.typed :refer :all])
  (:import [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect BitmapFont TextureRegion
            ParticleEffectPool ParticleEffect ParticleEffectPool$PooledEffect]
           [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx Gdx]
           [com.badlogic.gdx.math Matrix4]
           [com.badlogic.gdx.graphics OrthographicCamera]
           [com.badlogic.gdx.graphics.g2d Animation]
           [com.badlogic.gdx.scenes.scene2d Stage]))

(defprotocol libgdx-camera-zoom
  (zoom [this x]))

(def node-dirs
  {0 {4 2}})

(defn update-warp-in-range [f :- (Fn [Num -> Num]) l :- Num r :- Num x :- Num]
  (let [result (f x)]
    (cond
     (> l result) r
     (< r result) l
     :default result)))

(defn update-within-range [f :- (Fn [Num -> Num]) l :- Num r :- Num x :- Num]
  (let [result (f x)]
    (cond
     (> l result) l
     (< r result) r
     :default result)))

(defn raw-tex [x :- String] (:object (texture x)))

(defn get-all-logo-textures []
  (for [i (range 1 7)]
    (raw-tex (str "title/logo-frame-" i ".png"))))

(defn get-logo-anim []
  (animation 0.15 (get-all-logo-textures) :set-play-mode (play-mode :loop-pingpong)))

(defn get-all-choice1-textures []
  (for [i (range 1 7)]
    (raw-tex (str "title/choice1-frame-" i ".png"))))

(defn get-choice1-anim []
  (animation 0.15 (get-all-choice1-textures) :set-play-mode (play-mode :loop-pingpong)))

(defn get-all-choice2-textures []
  (for [i (range 1 7)]
    (raw-tex (str "title/choice2-frame-" i ".png"))))

(defn get-choice2-anim []
  (animation 0.15 (get-all-choice2-textures) :set-play-mode (play-mode :loop-pingpong)))

(defn get-all-choice3-textures []
  (for [i (range 1 7)]
    (raw-tex (str "title/choice3-frame-" i ".png"))))

(defn get-choice3-anim []
  (animation 0.15 (get-all-choice3-textures) :set-play-mode (play-mode :loop-pingpong)))

(defn get-global-zoom [t choosen?]
  (if choosen?
    (max (- 1 (* (Math/pow t 1.4) 0.005)) 0.2)
    (min (+ 0.8 (* t 0.005)) 1)))

(defn in-music-room? [screen]
  (let [t (:musicroom-timer screen)]
    (and t (== t 40))))

(defn should-quit? [screen]
  (let [t (:quit-timer screen)]
    (== t 40)))

(defn fade-back! [screen]
  (update! screen :fade-back-timer 1))

(defn fade-back? [screen]
  (:fade-back-timer screen))

(defn faded-back? [screen]
  (= (:fade-back-timer screen) 40))


(defn render-real
  [{:keys [^Stage renderer ^Animation logo-anim
           ^TextureRegion copyright-tex
           ^Animation choice1-anim ^Animation choice2-anim ^Animation choice3-anim
           choice-timers
           ^TextureRegion info1
           ^TextureRegion info2
           ^TextureRegion info3
           ^OrthographicCamera ortho-cam
           timer
           test-switch
           selected-timer
           font state-time ^ParticleEffectPool$PooledEffect title-particle ^TextureRegion t] :as screen} entities]
  (let [
        ^SpriteBatch batch (.getBatch renderer)
        dtime (.getDeltaTime Gdx/graphics)
        global-opacity-multiplier (if (fade-back? screen)
                                    (let [t (:fade-back-timer screen)]
                                      (do
                                        (min (* t 1/40) 1)))
                                    (let [either-timer (or (:selected-timer screen) (:musicroom-timer screen) (:quit-timer screen))]
                                      (if either-timer
                                        (max (* (- 40 either-timer) 1/40) 0)
                                        (min (* timer 1/120) 1))))
        calc-opacity #(* (+ 0.5 (* 1/20 % 0.5)) global-opacity-multiplier)
        calc-opacity2 #(* 1/20 % global-opacity-multiplier)
        calc-zoom #(+ 0.9 (* 1/20 % 0.1))
        global-zoom 1
        zoom-matrix (.scale (.cpy (.getProjectionMatrix batch)) global-zoom global-zoom 0)
        opacities (map calc-opacity choice-timers)
        info-opacities (map calc-opacity2 choice-timers)
        zooms (map calc-zoom choice-timers)
        nothing (.setProjectionMatrix batch (.combined ortho-cam))]
    (do
      (if (:selected-timer screen)
        (zoom ortho-cam (get-global-zoom (:selected-timer screen) true))
        (zoom ortho-cam (get-global-zoom timer false)))
      (if (and (:selected-timer screen) (= (:selected-timer screen) 180))
        (m/switch-to-main!))
      (.update ortho-cam)
      (.setProjectionMatrix batch (.combined ortho-cam))
      (.begin batch)
      (.setColor batch 1 1 1 global-opacity-multiplier)
      (let [tex (.getKeyFrame logo-anim state-time)
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-on-batch batch tex (- 480 (/ w 2)) 341))
      (.setColor batch 1 1 1 (nth opacities 0))
      (let [tex (.getKeyFrame choice1-anim state-time)
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 279 90 (nth zooms 0)))
      (.setColor batch 1 1 1 (nth info-opacities 0))
      (let [tex info1
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 148 90 1.0))
      (.setColor batch 1 1 1 (nth info-opacities 1))
      (let [tex info2
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 148 90 1.0))
      (.setColor batch 1 1 1 (nth info-opacities 2))
      (let [tex info3
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 148 90 1.0))
      (.setColor batch 1 1 1 (nth opacities 1))
      (let [tex (.getKeyFrame choice2-anim state-time)
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 239 90 (nth zooms 1)))
      (.setColor batch 1 1 1 (nth opacities 2))
      (let [tex (.getKeyFrame choice3-anim state-time)
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 199 90 (nth zooms 2)))
      (.setColor batch 1 1 1 global-opacity-multiplier)
      (let [tex copyright-tex
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-on-batch batch tex (- 480 (/ w 2)) 71))
      (.setColor batch 1 1 1 global-opacity-multiplier)
      (if-not (nil? selected-timer)
        (do
          (-> title-particle
              (.scaleEffect global-opacity-multiplier))
          (-> title-particle
            (.getEmitters)
            (.get 0)
            (.setContinuous false))))
      (.draw title-particle batch dtime)
      (.end batch)))
  entities)

(defn get-key-result [{:keys [current-index] :as screen}]
  (if (:selected-timer screen)
    -1
    (if (key-pressed? :z)
    current-index
    -1)))

(defn title-update [{:keys [current-index choice-timers cursor-cd] :as screen} entities]
  (clear!)
  (update! screen :timer (inc (:timer screen)))
  (update! screen :cursor-cd (update-within-range dec 0 20 (:cursor-cd screen)))
  (let [r (for [i (range 3)]
            (update-within-range (if (== current-index i) inc dec) 0 20 (nth choice-timers i)))]
    (update! screen :choice-timers r))
  (update! screen :state-time ((partial + (.getDeltaTime Gdx/graphics)) (:state-time screen)))
  (if (and (key-pressed? :up) (= cursor-cd 0))
    (do
      (update! screen :current-index (update-warp-in-range dec 0 2 (:current-index screen)))
      (update! screen :cursor-cd 10)))
  (if (:selected-timer screen)
    (update! screen :selected-timer (inc (:selected-timer screen))))
  (if (:fade-back-timer screen)
    (update! screen :fade-back-timer (inc (:fade-back-timer screen))))
  (if (:musicroom-timer screen)
    (do
      (update! screen :musicroom-timer (inc (:musicroom-timer screen)))
      (if (>= (:musicroom-timer screen) 40)
        (update! screen :musicroom-timer 40))))
  (if (and (in-music-room? screen) (key-pressed? :x))
    (fade-back! screen))
  (if (faded-back? screen)
    (do
      (update! screen :musicroom-timer nil)
      (update! screen :selected-timer nil)
      (update! screen :fade-back-timer nil)))
  (if (:quit-timer screen)
    (do
      (if (should-quit? screen)
        (System/exit 0))
      (update! screen :quit-timer (inc (:quit-timer screen)))))
  (case (get-key-result screen)
    -1 nil
    0 (do
         (update! screen :selected-timer 1))
    1 (do
         (update! screen :musicroom-timer 1))
    2 (do
         (update! screen :quit-timer 1)))
  (if (and (key-pressed? :down) (= cursor-cd 0))
    (do
      (update! screen :current-index (update-warp-in-range inc 0 2 (:current-index screen)))
      (update! screen :cursor-cd 10)))
  (render-real screen entities))



(defmacro set-all [object & fields-and-values]
  (let [obj-sym (gensym)]
    `(let [~obj-sym ~object]
        ~@(for [[field value] (partition 2 fields-and-values)]
           `(set! (. ~obj-sym ~field)
                  ~value)))))

(extend-protocol libgdx-camera-zoom
  OrthographicCamera
  (zoom [this x] (set-all this zoom x)))

(defn title-init [screen entities]
  (doseq [[k v] screen]
      (if-not (contains? {:ui-listeners 1, :input-listeners 1, :layers 1, :on-timer 1, :options 1, :update-fn! 1, :execute-fn-on-gl! 1, :execute-fn! 1} k)
        (update! screen k nil)))
  (update! screen :current-index 0)
  (update! screen :choice-timers [0 0 0])
  (update! screen :state-time 0)
  (update! screen :timer 0)
  (update! screen :zoom-switch false)
  (update! screen :cursor-cd 0)
  (update! screen :ortho-cam
             (let [cam (doto (OrthographicCamera. 960 720)
                         (.translate 480 320))]
               (do
                 (zoom cam 0.8)
                 (.update cam)
                 cam)))
  (update! screen :logo-anim (get-logo-anim))
  (update! screen :choice1-anim (get-choice1-anim))
  (update! screen :choice2-anim (get-choice2-anim))
  (update! screen :choice3-anim (get-choice3-anim))
  (update! screen :info1 (raw-tex "title/info1.png"))
  (update! screen :info2 (raw-tex "title/info2.png"))
  (update! screen :info3 (raw-tex "title/info3.png"))
  (update! screen :copyright-tex (raw-tex "title/copyright.png"))
  (update! screen :renderer (stage))
  (update! screen :t (:object (texture "test-title.png")))
  (update! screen :title-particle (.obtain (p/particle-particle-pool-for "title-particle.pt"))))

(defscreen title-screen
  :on-show title-init
  :on-render title-update)

(defn get-title-screen []
  title-screen)
