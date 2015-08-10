(ns scarletto.core.desktop-launcher
  (:require [scarletto.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication LwjglApplicationConfiguration]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (let [cfg (LwjglApplicationConfiguration.)
        nothing (do
                  (set! (.title cfg) "v0.1.0")
                  (set! (.width cfg) 960)
                  (set! (.height cfg) 720)
                  (set! (.resizable cfg) false))]
    (do
      (LwjglApplication. scarletto-game cfg)
      (Keyboard/enableRepeatEvents true))))
