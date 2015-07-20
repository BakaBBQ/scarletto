(ns scarletto.logics
  (:require [play-clj.core :refer :all]
            [scarletto.config :refer :all]
            [scarletto.collide :as c]
            [scarletto.decorators :as d]
            [scarletto.factory :as f]
            [scarletto.lifted :as l])
  (:import [com.badlogic.gdx Gdx]
           [com.badlogic.gdx Input$Keys]
	   [com.badlogic.gdx.math Vector2 CatmullRomSpline]))




(defn tag? [e tag] (= (:tag e) tag))
(defn type? [e type] (= (:type e) type))

(defn fold-directions
  "gives a vector of 2 elements, the first element ranges from -1 to 1 showing the x key direction, the second ranges from -1 to 1, showing the y key direction"
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

(defn in-area-of-effect
  "detects if an entity is within the visible stage"
  [e]
  (let [x (:x e)
        y (:y e)]
    (and
     (> x 0)
     (> y 0)
     (< x offset-stage-right-bound)
     (< y offset-stage-upper-bound))))

(defn update-exempt-once
  "update entities with the :exempt-once flag to toggle the flag if the flag exists"
  [e]
  (if (and (:exempt-once e) (:ngc e))
    (if (in-area-of-effect e)
      (assoc e :exempt-once false :ngc false)
      e)
    e))

(defmulti update-entity
  "main dispatch for entity update loop"
  (fn [entity entities screen]
    (:type entity)))

(defn set-player-x [p x]
  "set the player's x pos, auto corrects out of stage values"
  (let [rb offset-stage-right-bound]
    (assoc p :x
         (cond
          (< x 0) 0
          (> x rb) rb
          :else x))))

(defn set-player-y [p y]
   "set the player's y pos, auto corrects out of stage values"
  (assoc p :y
         (cond
          (< y 0) 0
          (> y offset-stage-upper-bound) offset-stage-upper-bound
          :else y)))

(defn update-focused-timer
  "alters the player's focused timer"
  [p key-pressed]
  (if key-pressed
    (update p :focused
               (fn [x] (min 40 (inc x))))
    (update p :focused
               (fn [x] (max 0 (dec x))))))

(defn dec-abs [n]
  "forces the number n to approach 0. e.g. 3 -> 2 , 2 -> 1 , -3 -> -2, 0 -> 0, 1 -> 0"
  (if (pos? n)
    (dec n)
    (if (neg? n)
      (inc n)
      n)))

(defn ensure-not-out-of-bound [lb rb n]
  "forces the number n to be lb < n < rb, if not, changes n to the closest value"
  (if (< n lb)
    lb
    (if (> n rb)
      rb
      n)))

(defn update-player-velocity
  "alters player's velocity attribute according to the key pressed, the velocity attribute determines the player's current graphics"
  [player directions]
  (if (= (mod (:timer player) 3) 0)
    (let [dx (first directions)]
    (update-in
     (case (int dx)
      ;; if there is no movement the counter should go down
      0 (update player :velocity dec-abs)
      1 (update player :velocity inc)
      -1 (update player :velocity dec))
     [:velocity] (partial ensure-not-out-of-bound -7 7)))
    player))

(defn dec-until-zero
  [n]
  (if (pos? n)
    (dec n)
    0))

;; i feel like I am still using the imperitive style... state is evil
(defn player-dead?
  [player]
  (let [d (:dead player)]
    (and d (pos? d))))

(defn update-player-dead
  [player]
  (if (:collide player)
    (-> player
        (assoc :dead 90)
        (update :lives dec-until-zero))
    (update player :dead dec-until-zero)))


(defn update-invincible-and-dead
  [player]
  (if (= (:dead player) 1)
    (assoc player :invincible invincible-time)
    player))

(defn update-dead-timer
  [player]
  (-> player
      (update :dead dec-until-zero)
      (update-invincible-and-dead)
      (update :invincible dec-until-zero)))

(defn update-player-when-movable
  [entity offsets dirs slow-mode entities]
  (-> entity
      (set-player-x (+ (first offsets) (:x entity)))
      (set-player-y (+ (last offsets) (:y entity)))
      (update-player-velocity dirs)
      (update-focused-timer slow-mode)
      (c/collide-check entities)
      (update-player-dead)
      (update-dead-timer)))

(defmethod update-entity :player [entity entities screen]
  (let [slow-mode (key-pressed? :shift-left)
        speed-multiplyer (if slow-mode (first reimu-speed) (last reimu-speed))
        dirs (fold-directions)
        offsets (map (partial * speed-multiplyer) dirs)
        player entity
        x (:x player)
        y (:y player)
        r (if-not (player-dead? entity)
            (update-player-when-movable entity offsets dirs slow-mode entities)
            (update-dead-timer entity))]
    (do
      r)))

;; so let's see.. if the player is collided... then we should mark it dead... wait.. then how can we add those power items...

;; the plan follows as this: mark it dead for seconds

(defmethod update-entity :fps-counter [entity entities screen]
  (update-in entity [:fps] (constantly (game :fps))))

(defn update-death
  "if the entity's hp is smaller than 0, then mark it with flag :dead"
  [entity]
  (let [hp (:hp entity)]
    (if hp
      (if (<= hp 0)
        (assoc entity :dead true)
        entity)
      entity)))

(defmulti deal-damage
  "dispatches deal-damage according to whether the shooter is a boss or not"
  (fn [entity hp]
    (:boss entity)))

(defmethod deal-damage true [entity dmg]
  (-> entity
      (update-in [:hp] (fn [x] (- x dmg)))))

(defmethod deal-damage :default [entity dmg]
  (-> entity
      (update-in [:hp] (fn [x] (- x dmg)))
      (update-death)))

(defn update-shooter-collide [entity entities screen]
  (let [entities-grouped (:entities-grouped screen)
        all-bullets (:pbullet entities-grouped)
        is-collide (filter
                    (fn [x] (c/player-bullet-collide-shooter? entity x))
                    all-bullets)]
          (if (and is-collide ((comp not empty?) is-collide))
            (let [dmg (apply + (map :dmg is-collide))]
              (deal-damage entity dmg))
            entity)))

(comment defn spell-card-in-effect? [entities]
  (not (empty? (filter (fn [x] (and (= (:type x) :sc))) entities))))

(defn update-if-boss [entity entities screen]
  "a special method dedicated to those bosses.... wait... I do not like this"
  (if (:boss entity)
    (let [entities-sc (:sc (:entities-grouped screen))
          starting-spell-cards (filter (fn [x] (zero? (:timer x))) entities-sc)]
      (if-not (empty? starting-spell-cards)
        (assoc entity :hp (:hp (first starting-spell-cards)))
        entity))
    entity))

(defmethod update-entity :shooter [entity entities screen]
  "main update method for shooters"
  (-> entity
      (d/update-single-shooter entities screen)
      (update-shooter-collide entities screen)
      (update-if-boss entities screen)))

(defmethod update-entity :pbullet [entity entities screen]
  (let [entities-grouped (:entities-grouped screen)
        all-shooters (:shooter entities-grouped)
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

        is-move-closer (or (< d 40) (> (:y player) 533) (:attract i))

        attractive-force (min (* 1.5 d) 8)

        attract-speed (f/vector-to i player attractive-force)

        vx (if is-move-closer
             (.x ^Vector2 attract-speed)
              (if (neg? magnitude)
                0
                (.x ^Vector2 newspeed)))
        vy (if is-move-closer
             (.y ^Vector2 attract-speed)
             (if (neg? magnitude)
             (- 1)
             (.y ^Vector2 newspeed)))
        ]
    (-> entity
        (update :x (partial + vx))
        (update :y (partial + vy))
        (assoc :dead did-collide :collided did-collide :attract is-move-closer)
        (update-exempt-once))))

(defn update-rotation [entity]
  (let [rot (:rotation entity)]
    (if (and rot (:vectors entity))
      (update entity :vectors
                 (partial map (fn [x] (f/rotate-vector x rot))))
      entity)))

(defmethod update-entity :wait [entity entities screen]
  (if (empty? (filter (fn [e] (or
                              (and
                               (= (:type e) :shooter)
                               (not (:boss e)))
                              (= (:type e) :dialog)
                              (= (:type e) :sc)))
                      entities))
    (assoc entity :ngc false :dead true)
    entity))

(defmethod update-entity :dialog [entity entities screen]
  (let [key (key-pressed? :z)
        ctrl (key-pressed? :control-left)
        t (:timer entity)
        fading? (integer? (:ftimer entity))]
    (cond
     (<= 0 t 9) entity
     (<= 10 t) (if-not fading?
                 (if key
                   (assoc entity :ftimer 0)
                   entity)
                 (if (> (:ftimer entity) 10)
                   (assoc entity :dead true :ngc false)
                   (update entity :ftimer inc))))))

(defmethod update-entity :sc [entity entities screen]
  entity)

(defmethod update-entity :explosion [entity entiteis screen]
  (let [t (:timer entity)]
    (if (> t 20)
      (assoc entity :dead true)
      entity)))

(defmethod update-entity :default [entity entities screen]
  (let [^Vector2 vel (:vel entity)]
    (-> entity
      (update :x + (.x vel))
      (update :y + (.y vel))
      (update-rotation))))

(defmulti update-entity-input
  (fn [screen entity]
    [(entity :type) (:key screen)]))

(defmethod update-entity-input [:player (key-code :left)] [screen entity]
  (update-in entity [:x] - 3))

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
  :dtag)

(defmethod get-dead-entities-effect :default [e]
  [])

(defn dead? [e]
  (<= (:hp e) 0))

(defmethod get-dead-entities-effect :test [e]
  (let [x (:x e)
        y (:y e)
        si (f/item x y :power (f/polar-vector 5 90))
        expanded (l/expand-to si 30 3)
        explosion [(f/explosion x y 0)]]
    (if (dead? e)
      (concat expanded explosion)
      [])))

(defn get-dead-aftereffects
  [alive dead]
  (let [
        items (filter (fn [x] (= (:type x) :item)) dead)
        items-grouped (group-by :tag items)
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
                        ^int r (or (:radius e) 0)

                        gc-down (or (:gc-down e) 0)
                        gc-up (or (:gc-up e) offset-stage-upper-bound)
                        b (* 1.5 r)]
                     (and
                       (not= (:ngc e) true)
                       (or
                        (:dead e)
                        (< x (- b))
                        (< y (- gc-down b))
                        (> x (+ offset-stage-right-bound b))
                        (> y (+ gc-up b))
                        )))))
        grouped (group-by f entities)

        alive (get grouped true)
        dead (get grouped false)

        r (get-dead-aftereffects alive dead)
        ]
    r))

(defn clean-entities-trans
  [entities])

(defn clean-dead-bosses [entities screen]
  (if
      (not (empty? (filter (fn [x]
                             (and
                              (:boss x)
                              (<= (:hp x) 0)))
                           (:shooter (:entities-grouped screen)))))
    (map (fn [x]
           (if (= (:type x) :sc)
             (f/bullet-circle 5 100 100 (f/polar-vector 0 10))
             x))
         entities)
    entities))


;;let me explain the logic here
;;
;; if (there is some boss with a hp <= 0) then
;;   create some circle bullets....
;; else
;;   do nothing


;; then for refactoring
;; if (there is some boss with a hp <= 0) then
;;   return a transducer
;; else
;;   return identity
(defn clean-dead-bosses-trans [entities screen]
  (if
      (not (empty? (filter (fn [x]
                             (and
                              (:boss x)
                              (<= (:hp x) 0)))
                           (:shooter (:entities-grouped screen)))))
    (map (fn [x]
           (if (= (:type x) :sc)
             (f/bullet-circle 5 100 100 (f/polar-vector 0 10))
             x)))
    identity))

(defn update-timer
  [e]
  (if (:timer e)
    (update e :timer inc)
    e))



(defn update-individuals
  "only updates the every each entity"
  [entities screen]
  (for [e entities]
    (-> e
        (update-entity entities screen)
        (update-timer))))

;; update-entity e entities screen
;; update-timer e
;; update-exempt-once e
(defn update-individuals-trans
  [entities screen]
  (let [f (fn [x] (-> x
                      (update-entity entities screen)
                      (update-timer)
                      (update-exempt-once)))]
    (map f)))

(defmulti get-option-bullets
  (fn [option player]
    (:tag option)))

(defn get-single-option-bullets
  [p ox oy]
  (let [px (:x p)
        py (:y p)
        rx (+ px ox)
        ry (+ py oy)]
    (f/player-bullet 4 rx ry (f/polar-vector 18 90) 2)))

(defn get-player-option-bullets
  [p]
  (let [option-pos (f/get-player-option-pos p)]
    (map (fn [^Vector2 v] (get-single-option-bullets p (.x v) (.y v)))
         option-pos)))

(defn get-player-bullets
  [p]
  (let [px (:x p)
        py (:y p)
        b1 (f/player-bullet 5 (- px 10) py (f/polar-vector 16 90) 3)
        b2 (f/player-bullet 5 (+ px 10) py (f/polar-vector 16 90) 3)]
    [b1 b2]))

(defn update-player-bullets
  [entities screen]
  (let [entities-grouped (:entities-grouped screen)
        p (first entities)
        px (:x p)
        py (:y p)
        t (:timer p)
        pressed-shoot (key-pressed? :z)
        can-shoot (and
                   (empty? (:dialog entities-grouped))
                   (not (f/player-dead? p)))
        do-shoot (and pressed-shoot can-shoot)]
    (concat entities
            (if (and do-shoot (= (mod t 6) 0))
              (get-player-bullets p)
              [])
            (if (and do-shoot (= (mod t 4) 0))
              (get-player-option-bullets p)
              []))))

(defn update-player-bullets-trans
  [entities screen]
  (let [entities-grouped (:entities-grouped screen)
        p (first entities)
        px (:x p)
        py (:y p)
        t (:timer p)
        pressed-shoot (key-pressed? :z)
        can-shoot (empty? (:dialog entities-grouped))
        do-shoot (and pressed-shoot can-shoot)]
    (fn [x]
      (concat x
            (if (and do-shoot (= (mod t 6) 0))
              (get-player-bullets p)
              [])
            (if (and do-shoot (= (mod t 4) 0))
              (get-player-option-bullets p)
              [])))))

(defn update-entities-trans
  [screen entities]
  (update! screen :entities-grouped (group-by :type entities))
  (fn [x]
    (-> x
      (update-individuals screen)
      (clean-entities)
      (update-player-bullets screen))))

(defn update-shooters
  [entities screen]
  (d/update-shooters entities screen))
