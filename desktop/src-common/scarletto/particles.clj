(ns scarletto.particles
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [clojure.core.typed :refer :all]
            [play-clj.g2d :refer :all])
  (:import [com.badlogic.gdx.graphics.g2d ParticleEffectPool ParticleEffect]
           [com.badlogic.gdx.files FileHandle]))

(defn set-particle-pool! [screen :- Any, sym : Sym, effect :- ParticleEffectPool]
  (update! screen sym effect))

(defn particle-effect [effect-file :- FileHandle, image-dir :- FileHandle]
  (doto (ParticleEffect.)
    (.load ^FileHandle effect-file ^FileHandle image-dir)))
