(ns scarletto.core.desktop-launcher
  (:require [scarletto.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication LwjglApplicationConfiguration]
           [org.lwjgl.input Keyboard]
           [org.ninesyllables.scarletto DirUtils]
           [com.badlogic.gdx Files$FileType])
  (:gen-class))

(def re-keyval #"^\s*(\w+)\s*=?(.*)")
(def re-group #"^\s*\[([^\]]+)].*")
(defn clean-lines [coll]
  (drop-while #(re-find #"^\s*(#.*)?$" %) coll))
(defn partition-first [pred coll]
  (split-at (count (take-while pred coll)) coll))
(defn clean [s]
  (.trim (subs s 0 (if (neg? (.indexOf s "#")) (count s) (.indexOf s "#")))))
(defn parse-value [val]
  (let [val (clean val) c (str (first val))]
    (cond
      (= c "\"") (subs val 1 (dec (count val)))
      (or (= c "[") (number? val)) (read-string val)
      (= val "true") true
      (= val "false") false
    :else val))) ;;Dates!
(defn parse [toml]
  (loop [d {} lines (clojure.string/split-lines toml)]
    (let [line (first lines)]
      (if (nil? line)
        d ;;EOF
        (if-let [m (re-find re-group line)]
                    (let [g (partition-first #(re-find re-keyval %) (clean-lines (drop 1 lines)))]
            (recur (assoc d (nth m 1) (parse (clojure.string/join "\n" (first g))))
              (second g))) ;;[group]
          (recur
            (if-let [m (re-find re-keyval line)]
              (assoc d (nth m 1) (parse-value (nth m 2))) ;;key=value
              d) ;;ignore
            (drop 1 lines)))))))

(defn -main []
  (do
    (DirUtils/prepare)
    (let [cfg (LwjglApplicationConfiguration.)
          config-raw (slurp (DirUtils/getConfigPath))
          user-config (parse config-raw)
          window-config (get user-config "window")
          fullscreen (get window-config "fullscreen")
          title (get window-config "title")
          resolution (get window-config "resolution")]
      (do
        (set! (.title cfg) title)
        (set! (.width cfg) (first resolution))
        (set! (.height cfg) (second resolution))
        (set! (.resizable cfg) false)
        (doseq [s [16 32 64 128]
                :let [filename (str "icon" s ".png")]]
          (.addIcon cfg filename Files$FileType/Internal))
        (LwjglApplication. scarletto-game cfg)
        (Keyboard/enableRepeatEvents true)))))
