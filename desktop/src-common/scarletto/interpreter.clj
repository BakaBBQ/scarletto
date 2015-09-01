(ns scarletto.interpreter
  (use [scarletto.factory :as f]
       [scarletto.config :refer :all])
  (require [play-clj.core :refer :all]
           [clojure.edn :as e]))


(defn load-data [filename]
  (let [f (files! :internal (str "data/" filename))
        s (.readString f)]
    (e/read-string s)))

(defmulti run-obj (fn [coll] (first coll)))

(defmethod run-obj :player [[e & args]]
  (merge (f/player :reimu :a) (first args)))
(defmethod run-obj :fps [e & args]
  {:fps 0 :type :fps-counter :ngc true})
(defmethod run-obj :cenemy [[e & args]]
  )
(defmethod run-obj :kaguya [[e & args]]
  (assoc
    (f/bullet-shooter
      :meow :n (/ (- stage-right-bound stage-left-bound) 2) 250)
    :dtag :test
    :exempt-once true
    :tag (rand-nth [:eientei-blue-aimed-bullet-one])
    :radius 12
    :mtag :test
    :boss true
    :name "Kaguya Houraisan"
    :hp 500))

(defmethod run-obj :spellcard [[e sc-tag hp name]]
  (let []
    (assoc (f/spellcard sc-tag sc-tag hp) :hp hp :graphics (not= "" name) :name name)))

(defmethod run-obj :enemy [[e & args]]
  (let [s (first args)
        d (load-data s)]
    (merge (f/bullet-shooter :blah :blah 0 0) d)))
(defmethod run-obj :dialog [[e & args]]
  (let []
    (merge (f/new-message) (first args))))
(defmethod run-obj :wait [[e & args]]
  (let []
    (merge (f/wait-until-all-clear) (first args))))

(defmethod run-obj :stage-text [[e & args]]
  (f/stage-text (first args)))

(defn interpret-init [init-coll]
  (map run-obj init-coll))

(defn interpret-schedule [schedule gtimer]
  (if (get schedule gtimer)
    (let [r (get schedule gtimer)]
      (if (= (flatten r) r)
        [(run-obj r)]
        (map run-obj r)))
    []))
