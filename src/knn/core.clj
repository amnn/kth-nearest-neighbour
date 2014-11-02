(ns knn.core
  (:require [clojure.java.io :refer [reader]]
            [knn.quad-tree :as qt]))

(defn read-sample
  "Takes two filenames `pos-fname` and `label-fname` and produces a sequence of
  position-label pairs."
  [pos-fname label-fname]
  (with-open [pos-f (reader pos-fname)
              lab-f (reader label-fname)]
    (let [pos-lines (line-seq pos-f)
          lab-lines (line-seq lab-f)]
      (doall (map #(vector (read-string (str \[ % \]))
                           (read-string %2))
                  pos-lines lab-lines)))))
