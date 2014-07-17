(defproject rooten "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2234"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [om "0.5.0"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [
      { :id "dev"
        :source-paths ["src"]
        :compiler {
          :output-to "app.js"
          :output-dir "out"
          :optimizations :none
          :source-map true}}
      { :id "release"
        :source-paths ["src"]
        :compiler {
          :output-to "js/app.js"
          :optimizations :advanced
          :pretty-print false
          :preamble ["react/react.min.js"]
          :externs ["react/externs/react.js"
                    "externs/jquery.js"]}}]})
