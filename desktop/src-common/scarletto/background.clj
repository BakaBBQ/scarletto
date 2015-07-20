(ns scarletto.background
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [clojure.core.typed :refer :all]
            [play-clj.g2d :refer :all])
  (:import [com.badlogic.gdx.math Vector2]
           [com.badlogic.gdx.graphics.g3d.decals Decal DecalBatch CameraGroupStrategy]))



(defprotocol background
  (init! [x screen])
  (update! [x screen])
  (render! [x screen]))

(defn stage-3c [])
