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


(def default-chars "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ,.蓬莱山辉夜东风谷早苗这只是随便测试用的文字，。")

(defn gen-font-by-name-and-size [n size default-c]
  (let [gen (FreeTypeFontGenerator. (.internal Gdx/files n))
        par (doto (FreeTypeFontGenerator$FreeTypeFontParameter.))
        font (do
               (set! (.size par) size)
               (set! (.characters par) default-c)
               (.generateFont gen par))
        nothing (set! (.markupEnabled ^BitmapFont$BitmapFontData (.getData font)) true)]
    font))

(defn gen-message-font [c]
  (gen-font-by-name-and-size "yahei.ttf" 24 c))

(defn gen-message-big-font [c]
  (gen-font-by-name-and-size "yahei.ttf" 32 c))

(defn gen-ename-font []
  (gen-font-by-name-and-size "DejaVuSans.ttf" 14 default-chars))
