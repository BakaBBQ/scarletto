(defproject scarletto "0.1.0-SNAPSHOT"
  :description "bullet-hell shooter in clojure"
  :global-vars {*warn-on-reflection* true}

  :dependencies [[com.badlogicgames.gdx/gdx "1.6.4"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.6.4"]
                 [com.badlogicgames.gdx/gdx-box2d "1.6.4"]
                 [com.badlogicgames.gdx/gdx-freetype "1.6.4"]
                 [com.badlogicgames.gdx/gdx-box2d-platform "1.5.5"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-bullet "1.5.5"]
                 [com.badlogicgames.gdx/gdx-bullet-platform "1.5.5"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-freetype-platform "1.6.4"
                  :classifier "natives-desktop"]
                 [seesaw "1.4.5"]
                 [clj-toml "0.3.1"]
                 [com.badlogicgames.gdx/gdx-platform "1.6.4"
                  :classifier "natives-desktop"]
                 [criterium "0.4.3"]
                 [org.clojure/clojure "1.7.0-alpha5"]
                 [org.clojure/core.typed "0.3.7"]
                 [play-clj "0.4.6"]]

  :source-paths ["src" "src-common"]
  :java-source-paths ["src-java"]
  :javac-options ["-target" "1.7" "-source" "1.7" "-Xlint:-options"]
  :aot [scarletto.core.desktop-launcher]
  :main scarletto.core.desktop-launcher)
