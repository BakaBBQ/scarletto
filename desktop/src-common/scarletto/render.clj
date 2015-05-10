(ns scarletto.render
  (:require [scarletto.factory :as f])
  (:import [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect]
           [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx Gdx]
           [com.badlogic.gdx.math Vector2]
           [com.badlogic.gdx.graphics FPSLogger]
           [com.badlogic.gdx.graphics OrthographicCamera]
           [com.badlogic.gdx.graphics.glutils ShapeRenderer ShapeRenderer$ShapeType]))


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

(defn render-debug
  "debug renderer for collisions"
  [{:keys [shape-renderer ortho-cam] :as screen} entities]
  (let [^ShapeRenderer renderer shape-renderer
        ^OrthographicCamera cam ortho-cam]
    (do
        (.update cam)
        (.setProjectionMatrix renderer (.combined cam))
        (.begin renderer (ShapeRenderer$ShapeType/Line))
	(.setColor renderer 1 1 1 1)
        (doseq [entity entities]
          (render-debug-entity entity renderer))
        (.end renderer)))
  entities)
