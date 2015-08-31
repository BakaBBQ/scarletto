(ns scarletto.interpreter
  (use [scarletto.factory :as f])
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
