(defproject knn "0.1.0-SNAPSHOT"
  :description "An implementation of the kth nearest neighbour, in Clojure.
               Specialised to R^2, using a quadtree to store the training
               data."
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [incanter/incanter-core "1.5.5"]
                 [incanter/incanter-charts "1.5.5"]
                 [incanter-gorilla "0.1.0"]
                 [org.clojure/data.priority-map "0.0.5"]]

  :plugins [[lein-gorilla "0.3.3"]]

  :main ^:skip-aot knn.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
