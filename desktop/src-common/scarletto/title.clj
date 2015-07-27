(ns scarletto.title
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [scarletto.render :as r]
            [scarletto.particles :as p]
            [clojure.core.typed :refer :all])
  (:import [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect BitmapFont TextureRegion
            ParticleEffectPool ParticleEffect ParticleEffectPool$PooledEffect]
           [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx Gdx]
           [com.badlogic.gdx.graphics.g2d Animation]
           [com.badlogic.gdx.scenes.scene2d Stage]))


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

(defn render-real
  [{:keys [^Stage renderer ^Animation logo-anim
           ^TextureRegion copyright-tex
           ^Animation choice1-anim ^Animation choice2-anim ^Animation choice3-anim
           choice-timers
           ^TextureRegion info1
           ^TextureRegion info2
           ^TextureRegion info3
           font state-time ^ParticleEffectPool$PooledEffect title-particle ^TextureRegion t] :as screen} entities]
  (let [
        ^SpriteBatch batch (.getBatch renderer)
        dtime (.getDeltaTime Gdx/graphics)
        calc-opacity #(+ 0.5 (* 1/20 % 0.5))
        calc-opacity2 #(* 1/20 %)
        calc-zoom #(+ 0.9 (* 1/20 % 0.1))
        opacities (map calc-opacity choice-timers)
        info-opacities (map calc-opacity2 choice-timers)
        zooms (map calc-zoom choice-timers)]
    (do
      (.begin batch)
      (let [tex (.getKeyFrame logo-anim state-time)
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-on-batch batch tex (- 480 (/ w 2)) 341))
      (.setColor batch 1 1 1 (nth opacities 0))
      (let [tex (.getKeyFrame choice1-anim state-time)
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 279 0 (nth zooms 0)))
      (.setColor batch 1 1 1 (nth info-opacities 0))
      (let [tex info1
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 148 0 1.0))
      (.setColor batch 1 1 1 (nth info-opacities 1))
      (let [tex info2
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 148 0 1.0))
      (.setColor batch 1 1 1 (nth info-opacities 2))
      (let [tex info3
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 148 0 1.0))
      (.setColor batch 1 1 1 (nth opacities 1))
      (let [tex (.getKeyFrame choice2-anim state-time)
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 239 0 (nth zooms 1)))
      (.setColor batch 1 1 1 (nth opacities 2))
      (let [tex (.getKeyFrame choice3-anim state-time)
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-in-center-with-rotation-and-zoom batch tex 480 199 0 (nth zooms 2)))
      (.setColor batch 1 1 1 1)
      (let [tex copyright-tex
            w (.getRegionWidth tex)
            h (.getRegionHeight tex)]
        (r/draw-on-batch batch tex (- 480 (/ w 2)) 71))
      (.draw title-particle batch dtime)
      (.end batch)))
  entities)

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
  (if (and (key-pressed? :down) (= cursor-cd 0))
    (do
      (update! screen :current-index (update-warp-in-range inc 0 2 (:current-index screen)))
      (update! screen :cursor-cd 10)))
  (render-real screen entities))

(defn title-init [screen entities]
  (update! screen :current-index 0)
  (update! screen :choice-timers [0 0 0])
  (update! screen :state-time 0)
  (update! screen :timer 0)
  (update! screen :cursor-cd 0)
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
