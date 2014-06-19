(defproject game-of-life "0.1.0-SNAPSHOT"
  :description "First cut at Conway's game of life in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[speclj "2.6.1"]]}}
  :plugins [[speclj "2.6.1"]]
  :test-paths ["spec"]
  :jvm-opts ["-Dline.separator=\"\n\""])
