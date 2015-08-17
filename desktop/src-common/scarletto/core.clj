(ns scarletto.core
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [clojure.core.typed :refer :all]
            [play-clj.g2d :refer :all]
            [scarletto.render :as r]
            [scarletto.logics :as l]
            [scarletto.title :as t]
            [scarletto.font :refer :all]
            [scarletto.config :refer :all]
            [scarletto.magic :as magic]
            [scarletto.prologue :as p]
            [scarletto.factory :as f]
            [scarletto.particles :as pt])
  (:import [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect TextureRegion BitmapFont]
           [com.badlogic.gdx.graphics Texture PerspectiveCamera Camera]
           [com.badlogic.gdx Gdx]
           [com.badlogic.gdx.math Vector2]
           [com.badlogic.gdx.graphics FPSLogger]
           [com.badlogic.gdx.graphics OrthographicCamera]
           [com.badlogic.gdx.graphics.glutils ShapeRenderer]
           [org.ninesyllables.scarletto PrescreenScreen PauseScreen]
           [com.badlogic.gdx.graphics.g3d.decals Decal DecalBatch CameraGroupStrategy]
           [com.badlogic.gdx.graphics.g2d ParticleEffectPool ParticleEffect ParticleEffectPool$PooledEffect]))

(defonce manager (asset-manager))
(set-asset-manager! manager)

(defn pause! [screen])

(defn raw-tex [st]
  (:object (texture st)))

(defn new-tr ^TextureRegion [src :- TextureRegion x :- Num y :- Num w :- Num h :- Num]
  (TextureRegion. ^TextureRegion src ^long x ^long y ^long w ^long h))

(defn choose-renderer-and-render [entities screen]
  (let [current-renderer (:current-renderer screen)]
    (if (= current-renderer :real)
      (r/render-real screen entities)
      (r/render-debug screen entities))))

(defn load-all-possible-bullet-textures! [screen]
  (let [^TextureRegion etama (:object (texture "lbq-flavored-bullets.png"))
        textures (for [i (range 16)]
                   (for [j (range 16)]
                     (new-tr ^TextureRegion etama (* 24 i) (* 24 j) 24 24)))]
    (update! screen :bullet-textures textures)))

(defn load-all-possible-bullet-appear-textures! [screen]
  (let [^TextureRegion etama (:object (texture "etama.png"))
        textures (for [i (range 8)]
                   (new-tr ^TextureRegion etama (* 48 i) 312 48 48))]
    (update! screen :appear-textures textures)))

(defn get-big-fairy-texture [^TextureRegion etama ^long index]
  (if (< index 8)
    (TextureRegion. etama (* 96 index) 576 96 96)
    (case index
      8 (TextureRegion. etama 576 480 96 96)
      9 (TextureRegion. etama 672 480 96 96)
      10 (TextureRegion. etama 576 384 96 96)
      11 (TextureRegion. etama 672 384 96 96))))

(defn load-all-possible-item-textures! [screen]
  (let [^TextureRegion etama2 (:object (texture "etama2.png"))
        textures (for [i (range 16)]
                   (new-tr etama2 (* 24 i) 312 24 24))]
    (update! screen :item-textures textures)))

(defn load-all-possible-enemy-textures! [screen]
  (let [^TextureRegion etama (:object (texture "enemy.png"))
        textures (for [i (range 11)]
                   (for [j (range 5)]
                     (if (= j 4)
                       (get-big-fairy-texture etama i)
                       (new-tr ^TextureRegion etama (* 48 i) (+ (* 48 j) 384) 48 48))))]
    (update! screen :enemy-textures textures)))

(defn load-all-possible-explosion-textures! [screen]
  (let [^TextureRegion etama (:object (texture "etama2.png"))
        textures [(new-tr etama 192 24 96 96)
                  (new-tr etama 288 24 96 96)
                  (new-tr etama 0 120 96 96)
                  (new-tr etama 96 120 96 96)]]
    (update! screen :explosion-textures textures)))

(defn load-all-possible-big-bullet-textures! [screen]
  (let [^TextureRegion etama6 (:object (texture "big-bullets.png"))
        textures-2-2 (for [i (range 8)]
                       (for [j (range 6)]
                         (new-tr etama6 (* 48 i) (* 48 j) 48 48)))
        textures-big (for [i (range 4)]
                       (new-tr etama6 (* 96 i) (* 96 6) 96 96))]
    (do
      (update! screen :textures-big textures-2-2)
      (update! screen :textures-giant textures-big))))

(defn load-kaguya-textures! [screen]
  (let [^TextureRegion em2 (:object (texture "stg7enm2.png"))
        textures (for [i (range 3)]
                   (for [j (range 3)]
                     (new-tr em2 (* 120 i) (* 120 j) 120 120)))]
    (update! screen :kaguya textures)))



(defn preload-textures! [screen]
  (update! screen :player-texture (:object (texture "pl02.png")))
  (update! screen :etama2 (:object (texture "etama2.png")))
  (update! screen :front-texture (:object (texture "front.png")))
  (let [
        texs [(raw-tex "stage-3c-name.png") (raw-tex "stage-3c-txt1.png") (raw-tex "stage-3c-txt2.png")]]
    (update! screen :stage-textures {:3c texs}))
  (let [texs {:test-sanae (raw-tex "characters/test-sanae.png") :test-kaguya (raw-tex "characters/test-kaguya.png")}]
    (update! screen :face-textures texs))
  (update! screen :reimu-shot2 (new-tr (raw-tex "sanae-shots.png") 192 240 24 24))
  (update! screen :message-font (gen-message-font))
  (update! screen :all-black (raw-tex "all-black.png"))
  (update! screen :kaguya-sc-background {:grunge (raw-tex "backgrounds/grunge.png")
                                         :background (raw-tex "backgrounds/kaguya-bg.png")
                                         :front (raw-tex "backgrounds/kaguya-front.png")})
  (update! screen :bonus-textures {true (raw-tex "spellcard-bonus.png")
                                   false (raw-tex "bonus-failed.png")})
  (update! screen :pron-font (BitmapFont. (files! :internal "pron-18.fnt")))
  (update! screen :adobe-arabic {:small (BitmapFont. (files! :internal "adobe-arabic-40.fnt"))
                                 :middle (BitmapFont. (files! :internal "adobe-arabic-60.fnt"))
                                 :big (BitmapFont. (files! :internal "adobe-arabic-96.fnt"))})
  (update! screen :paper (new-tr (raw-tex "sanae-shots.png") 216 240 48 48))
  (update! screen :sanae-bomb (raw-tex "sanae-bomb.png"))
  (update! screen :gameover-assets {:background (raw-tex "gameover-background.png")
                                    :flash (raw-tex "gameover-flash.png")
                                    :title (raw-tex "gameover-title.png")})
  (update! screen :message-back (raw-tex "message-back.png"))
  (update! screen :option (new-tr ^TextureRegion (:object (texture "pl02.png")) 144 216 24 24))
  (update! screen :hexagram (:object (texture "hexagram.png"))))

(defn render-fn [screen entities]
  (clear!)
  (update! screen :timer (inc (:timer screen)))
  (if (:gameover-timer screen)
    (update! screen :gameover-timer (inc (:gameover-timer screen))))
  (if (empty? (:wait (:entities-grouped screen)))
    (update! screen :gtimer (inc (:gtimer screen))))
  (if (key-pressed? :p)
    (println (count entities)))
  (if (key-pressed? :escape)
    (pause! screen))
  (if (.isKeyPressed Gdx/input (key-code :s))
    (if (= (:current-renderer screen) :real)
      (update! screen :current-renderer :debug)
      (update! screen :current-renderer :real)))
  (update! screen :entities-grouped (group-by :type entities))
  (cond
   (:paused screen) (r/render-pause screen entities)
   (:gameover screen) (-> entities
                          (choose-renderer-and-render screen)
                          (r/render-and-update-gameover screen))
    :default (let [trans-fn (fn [x]
                   (transduce (comp (l/update-individuals-trans entities screen)
                                    (l/clean-dead-bosses-trans entities screen))
                              conj x))]
              (-> entities
                  (trans-fn)
                  (l/clean-entities screen)
                  (l/update-player-bullets screen)
                  (l/update-shooters screen)
                  (choose-renderer-and-render screen)))))


(defn game-background-camera [screen]
  (let [cam (doto (PerspectiveCamera. 150 (width screen) (height screen))
              (.translate 512 50 256)
              (.lookAt  768 50 256)
              (.rotate  -0.1 0 1 0))]
    (do
      (update! screen :3d-cam cam)
      (set! (.near cam) 1)
      (set! (.far cam) 2048)
      (.rotate cam -78 0 0 1)
      (.update cam)
      cam)))


(defn test-decals! [screen]
  (let [raw-tex (fn [str] (:object (texture str)))
        side1-tex (TextureRegion. (raw-tex "side.png") 0 0 512 512)
        side2-tex (TextureRegion. (raw-tex "side.png") 512 0 512 512)
        side1-tex-small (TextureRegion. (raw-tex "side1.png"))
        side2-tex-small (TextureRegion. (raw-tex "side2.png"))
        ^TextureRegion floor-tex (raw-tex "floor.png")
        camera (game-background-camera screen)
        dbatch (DecalBatch. (CameraGroupStrategy. ^Camera camera))
        scale 512
        sides1 (for [i (range 16)]
                (doto (Decal/newDecal (if (even? i) side1-tex side2-tex))
                  (.setScale 0.5)
                  (.setX (+ -128 (* 256 i)))
                  ))
        sides2 (for [i (range 16)]
                (doto (Decal/newDecal (if (even? i) side2-tex side1-tex))
                  (.setScale 0.5)
                  (.setX (+ -128 (* 256 i)))
                  (.setZ 512)))
        floors (for [i (range 8)]
                 (doto (Decal/newDecal floor-tex)
                   (.setZ 256)
                   (.setX (* 512 i))
                   (.setY -128)
                   (.rotateX 90)))]
    (do
      (update! screen :decal-batch dbatch)
      (update! screen :decals (concat sides1 sides2 floors)))))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (doseq [[k v] screen]
      (if-not (contains? {:ui-listeners 1, :input-listeners 1, :layers 1, :on-timer 1, :options 1, :update-fn! 1, :execute-fn-on-gl! 1, :execute-fn! 1} k)
        (update! screen k nil)))
    (update! screen :my-tex (TextureRegion. ^TextureRegion (:object (texture "etama.png")) 0 96 24 24))
    (load-all-possible-bullet-textures! screen)
    (pt/load-all-particle-pools! screen)
    (load-all-possible-bullet-appear-textures! screen)
    (load-all-possible-enemy-textures! screen)
    (load-all-possible-big-bullet-textures! screen)
    (load-all-possible-item-textures! screen)
    (load-all-possible-explosion-textures! screen)
    (test-decals! screen)
    (println "Main Screen Called")
    (load-kaguya-textures! screen)
    (update! screen :renderer (stage))
    (update! screen :paused false)
    (preload-textures! screen)
    (update! screen :hub-batch (SpriteBatch.))
    (update! screen :speedbatch (SpriteBatch.))
    (update! screen :shape-renderer (new ShapeRenderer))
    (update! screen :flame-effect
             (doto ^ParticleEffectPool$PooledEffect (.obtain ^ParticleEffectPool (pt/particle-particle-pool-for "magical-flame.pt"))))
    (update! screen :enemy-bar (raw-tex "enemy-bar.png"))
    (update! screen :star-effect
             (doto ^ParticleEffectPool$PooledEffect (.obtain ^ParticleEffectPool (pt/particle-particle-pool-for "sanae-bomb.pt"))))
    (update! screen :option-effects
             (for [i (range 4)]
               (.obtain ^ParticleEffectPool (pt/particle-particle-pool-for "sanae-option.pt"))))
    (update! screen :ortho-cam
             (doto (OrthographicCamera. 960 720)
               (.translate camera-offset-x camera-offset-y)))
    (update! screen :hub-cam
             (doto (OrthographicCamera. 960 720)
               (.translate (+ (/ game-width 2)) (+ (/ game-height 2)))))
    (update! screen :current-renderer :real)
    (update! screen :font (gen-font))
    (update! screen :timer 0)
    (update! screen :gtimer 0)
    (let [player (f/player :reimu :a)
          circle {:radius 10 :x 250.0 :y 250.0 :type :circle :vel (f/polar-vector 3 225)}
          dialog (f/single-dialog "See, there is nothing there!")
          fps-counter {:fps 0 :type :fps-counter :ngc true}]
      [player fps-counter]))
  :on-render
  render-fn)

(defn get-main-screen []
  main-screen)


(defgame scarletto-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

(comment defgame scarletto-game
  :on-create
  (fn [this]
    (.setScreen this (PrescreenScreen. (fn [] (magic/switch-to-title!))))))

(defn get-game []
  scarletto-game)
