(ns scarletto.magic
  (:require [play-clj.core :refer :all]))

(defn get-game []
  ((resolve 'scarletto.core/get-game)))

(defn get-title []
  ((resolve 'scarletto.title/get-title-screen)))

(defn get-main []
  ((resolve 'scarletto.core/get-main-screen)))

(defn switch-to-title! []
  (let [t (get-title)]
    (set-screen! (get-game) (get-title))))

(defn switch-to-main! []
  (do
    (set-screen! (get-game) (get-main))))
