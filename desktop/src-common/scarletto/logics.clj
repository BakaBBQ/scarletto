(ns scarletto.logics
  (:require [play-clj.core :refer :all]
            [scarletto.collide :as c]
            [scarletto.decorators :as d]
            [scarletto.factory :as f]
            [scarletto.lifted :as l])
  (:import [com.badlogic.gdx Gdx]
           [com.badlogic.gdx Input$Keys]
	   [com.badlogic.gdx.math Vector2 CatmullRomSpline]))

(defn fold-directions
  []
  [(cond
     (.isKeyPressed Gdx/input (key-code :left))
     -1
     (.isKeyPressed Gdx/input (key-code :right))
     1
     :else
     0)
   (cond
     (.isKeyPressed Gdx/input (key-code :up))
     1
     (.isKeyPressed Gdx/input (key-code :down))
     -1
     :else
     0)])

(defmulti update-entity
  (fn [entity entities screen]
    (:type entity)))



(defn set-player-x [p x]
  (assoc p :x
         (cond
          (< x 0) 0
          (> x 382) 382
          :else x)))

(defn set-player-y [p y]
  (assoc p :y
         (cond
          (< y 0) 0
          (> y 442) 442
          :else y)))

(defn update-focused-timer
  [p key-pressed]
  (if key-pressed
    (update-in p [:focused]
               (fn [x] (min 40 (inc x))))
    (update-in p [:focused]
               (fn [x] (max 0 (dec x))))))

(defmethod update-entity :player [entity entities screen]
  (let [slow-mode (key-pressed? :shift-left)
        speed-multiplyer (if slow-mode 1 2)
        offsets (map (partial * speed-multiplyer) (fold-directions))
        player entity
        x (:x player)
        y (:y player)]
    (-> entity
        (set-player-x (+ (first offsets) x))
        (set-player-y (+ (last offsets)  y))
        (update-focused-timer slow-mode)
        (c/collide-check entities))))

(defn update-death
  [entity]
  (let [hp (:hp entity)]
    (if hp
      (if (<= hp 0)
        (assoc entity :dead true)
        entity)
      entity)))

(defmethod update-entity :fps-counter [entity entities screen]
  (-> entity
      (update-in [:fps] (game :fps))))

(defmulti deal-damage
  (fn [entity hp]
    (:boss entity)))

(defmethod deal-damage :default [entity dmg]
  (-> entity
      (update-in [:hp] (fn [x] (- x dmg)))
      (update-death)))

(defn update-shooter-collide [entity entities screen]
  (let [all-bullets (filter (fn [x] (= (:type x) :pbullet)) entities)
        is-collide (filter
                    (fn [x] (c/player-bullet-collide-shooter? entity x))
                    all-bullets)]
          (if (and is-collide ((comp not empty?) is-collide))
            (let [dmg (apply + (map (fn [x] (:dmg x)) is-collide))]
              (deal-damage entity dmg))
            entity)))

(defmethod update-entity :shooter [entity entities screen]
  (-> entity
      (d/update-single-shooter entities screen)
      (update-shooter-collide entities screen)))

(defmethod update-entity :pbullet [entity entities screen]
  (let [all-shooters (filter (fn [x] (= (:type x) :shooter)) entities)
        is-collide  (some
                     (fn [x] (c/player-bullet-collide-shooter? entity x))
                     all-shooters)
        ^Vector2 vel (:vel entity)]
    (if is-collide
       (assoc entity :dead true)
       (-> entity
          (update-in [:x] (partial + (.x vel)))
          (update-in [:y] (partial + (.y vel)))))))

(defmethod update-entity :item [entity entities screen]
  (let [i entity
        t (:timer i)
        ;;magnitude-w-t (fn [x] (- 1 (/ x 60)))
        magnitude-w-t (fn [x] (- 1 (/ (Math/pow x 0.5) 4)))

        magnitude (magnitude-w-t t)
        ^Vector2 newspeed (f/update-speed (:vel entity) (partial * magnitude))

        player (first entities)
        d  (c/distance (:x entity) (:y entity) (:x player) (:y player))
        did-collide (< d 3.5)

        is-move-closer (or (< d 40) (> (:y player) 355) (:attract i))

        attractive-force (min d 6)

        attract-speed (f/vector-to i player attractive-force)

        vx (if is-move-closer
             (.x attract-speed)
              (if (< magnitude 0)
                0
                (.x newspeed)))
        vy (if is-move-closer
             (.y attract-speed)
             (if (< magnitude 0)
             (- 1)
             (.y newspeed)))
        ]
    (-> entity
        (update-in [:x] (partial + vx))
        (update-in [:y] (partial + vy))
        (assoc :dead did-collide)
        (assoc :collided did-collide)
        (assoc :attract is-move-closer))))

(defn update-rotation [entity]
  (let [rot (:rotation entity)]
    (if (and rot (:vectors entity))
      (update-in entity
                 [:vectors]
                 (partial map (fn [x] (f/rotate-vector x rot))))
      entity)))

(defmethod update-entity :default [entity entities screen]
  (let [^Vector2 vel (:vel entity)]
    (-> entity
      (update-in [:x] (partial + (.x vel)))
      (update-in [:y] (partial + (.y vel)))
      (update-rotation))))

(defmulti update-entity-input
  (fn [screen entity]
    [(entity :type) (:key screen)]))

(defmethod update-entity-input [:player (key-code :left)] [screen entity]
  (assoc entity :x (- ^double (:x entity) 3)))

(defmethod update-entity-input :default [screen entity]
  entity)

(defn get-dead-aftereffects
  [alive dead]
  (for [e dead
        :let [p (first alive)
              powered-up-player (update-in p [:power] (partial + 5))]
        :when (= (:tag e) :power)]
    (assoc alive 0 powered-up-player)))

(defn add-power [player amt]
  (update-in player [:power]
             (fn [x]
               (min
                400
                (+ x amt)))))

(defmulti get-dead-entities-effect
  (fn [e] (:dtag e)))

(defn dead? [e]
  (<= (:hp e) 0))

(defmethod get-dead-entities-effect :test [e]
  (let [x (:x e)
        y (:y e)
        si (f/item x y :power (f/polar-vector 5 90))
         expanded (l/expand-to si 30 3)]
    (if (dead? e)
      expanded
      [])))

(defmethod get-dead-entities-effect :default [e]
  [])

(defn get-dead-aftereffects
  [alive dead]
  (let [
        items (filter (fn [x] (= (:type x) :item)) dead)
        items-grouped (group-by (fn [x] (:tag x)) items)
        poweritems (:power items-grouped)
        p (first alive)
        power-accumulation (* 5 (count poweritems))
        powered-up-player (add-power p power-accumulation)

        player-replaced (assoc alive 0 powered-up-player)
        e-sideeffects (flatten
                       (for [e dead]
                        (get-dead-entities-effect e)))]
    (concat player-replaced e-sideeffects)))

(defn clean-entities
  "clean those entities that fly out of screen"
  [entities]
  (let [
        f (comp not
                (fn [e]
                  (let [^double x (:x e)
                        ^double y (:y e)
                        ^int r (:radius e)
                        b (* 1.5 r)]
                    (and
                     (not= (:ngc e) true)
                     (or
                      (< x (- b))
                      (< y (- b))
                      (> x (+ 382 b))
                      (> y (+ 442 b))
                      (:dead e))))))
        grouped (group-by f entities)

        alive (get grouped true)
        dead (get grouped false)

        r (get-dead-aftereffects alive dead)
        ]
    r))

(defn in-area-of-effect
  [e]
  (let [x (:x e)
        y (:y e)]
    (and
     (> x 0)
     (> y 0)
     (< x 382)
     (< y 442))))

(defn update-timer
  [e]
  (if (:timer e)
    (update-in e [:timer] inc)
    e))

(defn update-exempt-once
  [e]
  (if (and (:exempt-once e) (:ngc e))
    (if (in-area-of-effect e)
      (assoc e :exempt-once false :ngc false)
      e)
    e))

(defn update-individuals
  "only updates the every each entity"
  [entities screen]
  (for [e entities]
    (-> e
        (update-entity entities screen)
        (update-timer)
        (update-exempt-once))))

(defmulti get-option-bullets
  (fn [option player]
    (:tag option)))

(defn get-single-option-bullets
  [p ox oy]
  (let [px (:x p)
        py (:y p)
        rx (+ px ox)
        ry (+ py oy)]
    (f/player-bullet 4 rx ry (f/polar-vector 14 90) 2)))

(defn get-player-option-bullets
  [p]
  (let [option-pos (f/get-player-option-pos p)]
    (map (fn [v] (get-single-option-bullets p (.x v) (.y v)))
         option-pos)))

(defn get-player-bullets
  [p]
  (let [px (:x p)
        py (:y p)
        b1 (f/player-bullet 5 (- px 10) py (f/polar-vector 12 90) 3)
        b2 (f/player-bullet 5 (+ px 10) py (f/polar-vector 12 90) 3)]
    [b1 b2]))

(defn update-player-bullets
  [entities screen]
  (let [p (first entities)
        px (:x p)
        py (:y p)
        t (:timer p)
        pressed-shoot (key-pressed? :z)]
    (concat entities
            (if (and pressed-shoot (= (mod t 6) 0))
              (get-player-bullets p)
              [])
            (if (and pressed-shoot (= (mod t 4) 0))
              (get-player-option-bullets p)
              []))))

(defn update-entities
  [screen entities]
  (-> entities
      (update-individuals screen)
      (clean-entities)
      (update-player-bullets screen)))

(defn update-shooters
  [screen entities]
  (d/update-shooters entities screen))

(defn update-entities-input
  [screen entities]
  (for [e entities]
    (update-entity-input screen e)))
