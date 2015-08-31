(ns scarletto.config)

(def window-title "Scarletto Dev")

(def game-width 960)
(def game-height 720)

(def stage-left-bound 69)
(def stage-right-bound 905)
(def stage-upper-bound 675)
(def stage-lower-bound 40)
(def reimu-speed [2 5])

(def camera-offset-x (- (/ game-width 2) stage-left-bound))
(def camera-offset-y (- (/ game-height 2) stage-lower-bound))

(def offset-stage-right-bound (- stage-right-bound stage-left-bound))
(def offset-stage-upper-bound (- stage-upper-bound stage-lower-bound))

(def invincible-time 180)
