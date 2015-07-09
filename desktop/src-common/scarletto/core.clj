(ns scarletto.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [scarletto.render :as r]
            [scarletto.logics :as l]
            [scarletto.font :refer :all]
            [scarletto.config :refer :all]
            [scarletto.factory :as f])
  (:import [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect TextureRegion]
           [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx Gdx]
           [com.badlogic.gdx.math Vector2]
           [com.badlogic.gdx.graphics FPSLogger]
           [com.badlogic.gdx.graphics OrthographicCamera]
           [com.badlogic.gdx.graphics.glutils ShapeRenderer]))

(defonce manager (asset-manager))
(set-asset-manager! manager)



(defn choose-renderer-and-render [entities screen]
  (let [current-renderer (:current-renderer screen)]
    (if (= current-renderer :real)
      (r/render-real screen entities)
      (r/render-debug screen entities))))

(defn load-all-possible-bullet-textures! [screen]
  (let [^TextureRegion etama (:object (texture "etama.png"))
        textures (for [i (range 16)]
                   (for [j (range 16)]
                     (TextureRegion. ^TextureRegion etama (* 24 i) (* 24 j) 24 24)))]
    (update! screen :bullet-textures textures)))

(defn load-all-possible-bullet-appear-textures! [screen]
  (let [^TextureRegion etama (:object (texture "etama.png"))
        textures (for [i (range 8)]
                   (TextureRegion. ^TextureRegion etama (* 48 i) 312 48 48))]
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
                   (TextureRegion. etama2 (* 24 i) 312 24 24))]
    (update! screen :item-textures textures)))

(defn load-all-possible-enemy-textures! [screen]
  (let [^TextureRegion etama (:object (texture "enemy.png"))
        textures (for [i (range 11)]
                   (for [j (range 5)]
                     (if (= j 4)
                       (get-big-fairy-texture etama i)
                       (TextureRegion. ^TextureRegion etama (* 48 i) (+ (* 48 j) 384) 48 48))))]
    (update! screen :enemy-textures textures)))

(defn load-all-possible-explosion-textures! [screen]
  (let [^TextureRegion etama (:object (texture "etama2.png"))
        textures [(TextureRegion. etama 192 24 96 96)
                  (TextureRegion. etama 288 24 96 96)
                  (TextureRegion. etama 0 120 96 96)
                  (TextureRegion. etama 96 120 96 96)]]
    (update! screen :explosion-textures textures)))

(defn load-all-possible-big-bullet-textures! [screen]
  (let [^TextureRegion etama6 (:object (texture "etama6.png"))
        textures-2-2 (for [i (range 8)]
                       (for [j (range 6)]
                         (TextureRegion. etama6 (* 48 i) (* 48 j) 48 48)))
        textures-big (for [i (range 4)]
                       (TextureRegion. etama6 (* 96 i) (* 96 6) 96 96))]
    (do
      (update! screen :textures-big textures-2-2)
      (update! screen :textures-giant textures-big))))

(defn preload-textures! [screen]
  (update! screen :player-texture (:object (texture "pl00.png")))
  (update! screen :etama2 (:object (texture "etama2.png")))
  (update! screen :front-texture (:object (texture "front.png")))
  (update! screen :reimu-shot2 (:object (texture "reimu-shot2.png")))
  (update! screen :paper (TextureRegion. ^TextureRegion (:object (texture "pl00.png")) 0 216 24 24))
  (update! screen :option (TextureRegion. ^TextureRegion (:object (texture "pl00.png")) 144 216 24 24)))

(defn render-fn [screen entities]
  (clear!)
  (update! screen :timer (inc (:timer screen)))
  (if (empty? (:wait (:entities-grouped screen)))
    (update! screen :gtimer (inc (:gtimer screen))))
  (if (.isKeyPressed Gdx/input (key-code :p))
    (println (count entities)))
  (if (.isKeyPressed Gdx/input (key-code :s))
    (if (= (:current-renderer screen) :real)
      (update! screen :current-renderer :debug)
      (update! screen :current-renderer :real)))
  (update! screen :entities-grouped (group-by :type entities))
  (let [trans-fn (fn [x]
                   (transduce (comp (l/update-individuals-trans entities screen)
                                    (l/clean-dead-bosses-trans entities screen))
                              conj x))]
    (-> entities
        (trans-fn)
        (l/clean-entities)
        (l/update-player-bullets screen)
        (l/update-shooters screen)
        (choose-renderer-and-render screen))))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :my-tex (TextureRegion. ^TextureRegion (:object (texture "etama.png")) 0 96 24 24))
    (load-all-possible-bullet-textures! screen)
    (load-all-possible-bullet-appear-textures! screen)
    (load-all-possible-enemy-textures! screen)
    (load-all-possible-big-bullet-textures! screen)
    (load-all-possible-item-textures! screen)
    (load-all-possible-explosion-textures! screen)
    (update! screen :renderer (stage))
    (preload-textures! screen)
    (update! screen :hub-batch (SpriteBatch.))
    (update! screen :shape-renderer (new ShapeRenderer))
    (update! screen :ortho-cam
             (doto (OrthographicCamera. 960 720)
               (.translate camera-offset-x camera-offset-y)))
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


(defgame scarletto-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
