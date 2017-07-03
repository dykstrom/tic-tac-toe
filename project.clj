(defproject tic-tac-toe "0.1.0-SNAPSHOT"
  :description "Play the game of tic-tac-toe"
  :url "https://github.com/dykstrom/tic-tac-toe"
  :license {:name "GPLv3"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [ysera "1.0.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
