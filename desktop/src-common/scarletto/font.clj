(ns scarletto.font
  (:require [play-clj.core :refer :all])
  (:import [com.badlogic.gdx.graphics.g2d.freetype FreeTypeFontGenerator]
           [com.badlogic.gdx.graphics.g2d BitmapFont$BitmapFontData]
           [com.badlogic.gdx.graphics.g2d.freetype FreeTypeFontGenerator$FreeTypeFontParameter FreeTypeFontGenerator$FreeTypeBitmapFontData]
           [com.badlogic.gdx Gdx]))

(defn gen-font []
  (let [gen (FreeTypeFontGenerator. (.internal Gdx/files "THSpatial.ttf"))
        par (doto (FreeTypeFontGenerator$FreeTypeFontParameter.))
        font (do
               (set! (.borderWidth par) 1)
               (set! (.size par) 12)
               ;;(set! (.incremental par) true)
               (.generateFont gen par))
        nothing (comment set! (.markupEnabled ^BitmapFont$BitmapFontData (.getData font)) true)]
    font))


(defn gen-font-by-name-and-size [n size]
  (let [gen (FreeTypeFontGenerator. (.internal Gdx/files n))
        par (doto (FreeTypeFontGenerator$FreeTypeFontParameter.))
        font (do
               (set! (.size par) size)
               (.generateFont gen par))
        nothing (comment set! (.markupEnabled ^BitmapFont$BitmapFontData (.getData font)) true)]
    font))

(defn gen-message-font []
  (gen-font-by-name-and-size "yahei.ttf" 20))
