(ns scarletto.core.desktop-launcher
  (:require [scarletto.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. scarletto-game "scarletto" 960 720)
  (Keyboard/enableRepeatEvents true))
