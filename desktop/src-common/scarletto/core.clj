(ns scarletto.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [scarletto.render :as r]
            [scarletto.logics :as l]
            [scarletto.factory :as f])
  (:import [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect]
           [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx Gdx]
           [com.badlogic.gdx.math Vector2]
           [com.badlogic.gdx.graphics FPSLogger]
           [com.badlogic.gdx.graphics OrthographicCamera]
           [com.badlogic.gdx.graphics.glutils ShapeRenderer]))

(defonce manager (asset-manager))
(set-asset-manager! manager)


(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    (update! screen :shape-renderer (new ShapeRenderer))
    (update! screen :ortho-cam
             (doto (OrthographicCamera. 640 480)
               (.translate 303 (/ 455 2))))
    (update! screen :timer 0)
    (let [player (f/player :reimu :a)
          circle {:radius 10 :x 250.0 :y 250.0 :type :circle :vel (f/polar-vector 3 225)}]
      [player circle]))

  :on-render
  (fn [screen entities]
    (clear!)
    (update! screen :timer (inc (:timer screen)))
    (if (.isKeyPressed Gdx/input (key-code :p))
      (println (count entities)))
    (->> entities
         (l/update-entities screen)
         (l/update-shooters screen)
         (r/render-debug screen)))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :spawn-bullets (concat entities
                             (f/nway-shoot
                               (f/bullet-circle 5 200 200 (f/polar-vector 3 0))
                               60))
      nil))

  :on-key-down
  (fn [screen entities]
    (comment l/update-entities-input screen entities)))

(defscreen text-screen
  :on-show
  (fn [screen entities]
    (update! screen :camera (orthographic) :renderer (stage))
    (assoc (label "0" (color :white))
           :id :fps
           :x 5))

  :on-render
  (fn [screen entities]
    (->> (for [entity entities]
           (case (:id entity)
             :fps (doto entity (label! :set-text (str (game :fps))))
             entity))
         (render! screen)))

  :on-resize
  (fn [screen entities]
    (height! screen 300)))

(defgame scarletto-game
  :on-create
  (fn [this]
    (set-screen! this main-screen text-screen)))
