(ns scarletto.font
  (:require [play-clj.core :refer :all])
  (:import [com.badlogic.gdx.graphics.g2d.freetype FreeTypeFontGenerator
            FreeTypeFontGenerator$FreeTypeFontParameter]
           [com.badlogic.gdx Gdx]))

(defn gen-font []
  (let [gen (FreeTypeFontGenerator. (.internal Gdx/files "THSpatial.ttf"))
        par (doto (FreeTypeFontGenerator$FreeTypeFontParameter.))]
    (set! (.borderWidth par) 1)
    (set! (.size par) 12)
    (.generateFont gen par)))
