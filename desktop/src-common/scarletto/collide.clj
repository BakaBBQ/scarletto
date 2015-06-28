(ns scarletto.collide
  (:import [com.badlogic.gdx.math Vector2]))

(defn distance ^double [^double x1 ^double y1 ^double x2 ^double y2]
  (Math/hypot (- x2 x1) (- y2 y1)))

(defn pdistance [x y x1 y1 x2 y2]
  (let [a (- x x1)
        b (- y y1)
        c (- x2 x1)
        d (- y2 y1)
        dot (+ (* a c) (* b d))
        len-sq (+ (* c c) (* d d))
        param (if (zero? len-sq)
                -1
                (/ dot len-sq))
        xxyy (cond
              (neg? param)
              [x1 y1]
              (> param 1)
              [x2 y2]
              :else
              [(+ x1 (* param c)) (+ y1 (* param d))])
        xx (first xxyy)
        yy (last xxyy)
        dx (- x xx)
        dy (- y yy)]
    (Math/hypot dx dy)))

(defmulti bullet-collide-player?
  (fn [player bullet]
    (:type bullet)))

(defn- fold-lines
  [vector-coll]
  (let [repeated-coll (conj vector-coll (first vector-coll))]
    (partition 2 1 vector-coll)))

(defn- line-collide-with-player?
  [^Vector2  v1 ^Vector2 v2 player [^double bx ^double by]]
  (let [p player
        dis (pdistance (:x p) (:y p) (+ bx (.x v1)) (+ by (.y v1)) (+ bx (.x v2)) (+ by (.y v2)))]
    (<= dis ^int (:radius player))))

(defmethod bullet-collide-player? :polygon [player bullet]
  (let [b bullet
        p player
        total-radius (+ ^int (:radius b) ^int (:radius p))
        broadphase-d (distance (:x b) (:y b) (:x p) (:y p))]
    (and
     ;; both need to be qualified
     ;; the broadphase check should match
     ;; and the narrow phrase should match

     ;; narrow-phase:
     ;; check if each line collides with the player
     (<= broadphase-d total-radius)
     (some
      ;; if each line to the player's distance < player's radius
      (fn [l]
        (let [v1 (first l)
              v2 (second l)]
          (line-collide-with-player? v1 v2 p [(:x bullet) (:y bullet)])))
      (fold-lines (:vectors bullet))))))

(defn circle-intersect? [x1 y1 r1 x2 y2 r2]
  (let [total-radius (+ r1 r2)
        dis (distance x1 y1 x2 y2)]
    (<= dis total-radius)))

(defmethod bullet-collide-player? :circle [player bullet]
  (let [b bullet
        p player]
    (circle-intersect? (:x b) (:y b) (:radius b)
                       (:x p) (:y p) (:radius p))))

(defn player-bullet-collide-shooter? [pbullet shooter]
  (circle-intersect? (:x pbullet) (:y pbullet) (:radius pbullet)
                     (:x shooter) (:y shooter) (:radius shooter)))

(defmethod bullet-collide-player? :default [player bullet]
  false)

(defn collide-check
  ;; whenever the player collides with other entities, it needs to at least do something
  [player all-entities]
  (let [others (rest all-entities)]
    (assoc player :collide
               (some
                   (partial bullet-collide-player? player)
                   others))))
