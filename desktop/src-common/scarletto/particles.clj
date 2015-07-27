(ns scarletto.particles
  (:refer-clojure :exclude [atom doseq let fn defn ref dotimes defprotocol loop for])
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [clojure.core.typed :refer :all])
  (:import [com.badlogic.gdx.graphics.g2d ParticleEffectPool ParticleEffect]
           [com.badlogic.gdx.files FileHandle]))

(defn set-particle-pool! [screen :- Any, sym :- Sym, effect :- ParticleEffectPool]
  (update! screen sym effect))

(defn particle-effect [effect-file :- FileHandle, image-dir :- FileHandle]
  (doto (ParticleEffect.)
    (.load ^FileHandle effect-file ^FileHandle image-dir)))

(defn init-particle-pool-blue! [screen :- Any]
  (let [blue-effect (particle-effect (files! :internal "maple-blue.pt") (files! :internal ""))
        effect-pool (ParticleEffectPool. blue-effect 1 20)]
    (set-particle-pool! (screen :blue-maples effect-pool))))

(defn init-particle-pool-for [screen :- Any, n :- Sym, particle-path :- String]
  (let [particle-effect (particle-effect (files! :internal particle-path) (files! :internal ""))
        effect-pool (ParticleEffectPool. particle-effect 1 20)]
    (set-particle-pool! screen n effect-pool)))

(defn particle-particle-pool-for [particle-path :- String]
  (let [particle-effect (particle-effect (files! :internal particle-path) (files! :internal ""))
        effect-pool (ParticleEffectPool. particle-effect 1 20)]
    effect-pool))

(defn load-all-particle-pools! [screen :- Any]
  (init-particle-pool-for screen :maple-blue "maple-blue.pt")
  (init-particle-pool-for screen :maple-green "maple-green.pt")
  (init-particle-pool-for screen :maple-pink "maple-pink.pt")
  (init-particle-pool-for screen :maple-red "maple-red.pt")
  (init-particle-pool-for screen :maple-yellow "maple-yellow.pt")
  (init-particle-pool-for screen :sanae-hit-maple "sanae-hit.pt")
  (init-particle-pool-for screen :magical-frame "magical-flame.pt")
  (init-particle-pool-for screen :frog "frog.pt"))

(defn load-title-particle-pool! [screen :- Any]
  (init-particle-pool-for screen :title-particle "title-particle.pt"))
