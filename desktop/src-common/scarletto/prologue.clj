(ns scarletto.prologue
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [scarletto.render :as r]
            [scarletto.font :refer :all]
            [clojure.core.typed :refer :all])
  (:import [com.badlogic.gdx.graphics.g2d SpriteBatch Batch ParticleEffect BitmapFont TextureRegion]
           [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx.scenes.scene2d Stage]))

;; so as to give a general outline to the scene....


;; my dream format

(comment 'dream-format

         [:preload "prologue1.jpg"
          :background "prologue1.jpg"
          :text ["blah"]
          :text ["blah"]])

;; so it is kind of like an AVG engine

;; and we also need to support coloring... does libgdx have that built-in ...?
;; -> okay libgdx has that built in....

(defn raw-texture ^TextureRegion [str]
  (:object (texture str)))

(def test-data
  [:preload :test "prologue1.jpg"
   :background :test])

(def test-data
  {
   0 [:preload :prologue1 "prologue1.jpg"]
   1 [:background :prologue1]
   })

(defn extract-from-data [data key]
  (if (get data key)
    (get data key)
    []))

(defn preload! [screen :- (Map Sym Any) sym :- Sym str :- String]
  (update! screen :preloaded-textures (assoc (:preloaded-textures screen) sym (raw-texture str)))
  [])

(defn update-global-interpreter [screen entities])

;; as for the data structure....


(defrecord Text [str timer ctime])
(defrecord Background [str timer ctime])

(defn setup-screen! [screen]
  (update! screen :preloaded-textures {}))

(defn ptexture ^TextureRegion [screen :- (Map Sym Any) sym :- Sym]
  (sym (:preloaded-textures screen)))

(defn new-background [screen :- (Map Sym Any) str :- String]
  (Background. str 0 (:timer screen)))

(defn new-text [screen str]
  (Text. str 0 (:timer screen)))

;; okay here it goes...
;; entities => [(text "blah")]

(defmulti render-entity (fn [screen ^SpriteBatch batch entities entity] (class entity))
  )

(defmulti update-entity (fn [screen entities entity]
                          (class entity)))

(defmethod update-entity Background [screen entities entity]
  (update :timer entity inc))

(defmethod render-entity Background [screen batch entities background]
  (let [^TextureRegion tex (ptexture screen (:str background))
        nothing (comment println screen)]
    (r/draw-on-batch batch tex 0 0)))

(defmethod render-entity Text [screen batch entities entity])

(comment defmethod render-entity :default [screen batch entities entity]
  )

(defn render-real
  [{:keys [^Stage renderer font] :as screen} entities]
  (let [
        ^SpriteBatch batch (.getBatch renderer)]
    (do
      (.begin batch)
      (doseq [entity entities]
        (render-entity screen batch entities entity))
      (.end batch)))
  entities)

(defmulti interpret-inst (fn [coll screen entities] (first coll)))

(defmethod interpret-inst :preload [coll screen entities]
  (let [arg1 (nth coll 1)
        arg2 (nth coll 2)]
    (do
      (preload! screen arg1 arg2)
      [])))

(defmethod interpret-inst :background [coll screen entities]
  (let [arg1 (nth coll 1)]
    [(new-background screen arg1)]))

(defmethod interpret-inst nil [coll screen entities]
  (do
    []))

(defn update-interpret [screen data entities]
  (let [data (extract-from-data data (:timer screen))]
    (concat entities (interpret-inst data screen entities))))

(defn render-fn [screen entities]
  (update! screen :timer (+ 1 (:timer screen)))
  (clear!)
  (->> entities
       (update-interpret screen test-data)
       (render-real screen)))

(defscreen prologue-screen
  :on-show
  (fn [screen entities]
    (update! screen :timer 0)
    (setup-screen! screen)
    (update! screen :renderer (stage))
    (update! screen :font (gen-font))
    [])
  :on-render
  (fn [screen entities]
    (render-fn screen (sort-by {Background 1 Text 2} entities))))
