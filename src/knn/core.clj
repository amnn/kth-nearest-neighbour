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

(defn classify
  "Given a quad tree describing the environment, create a function that
  classifies new positions with a label."
  [qt k]
  (fn [pos]
    (->> (qt/k-nn qt pos k)
         (map #(-> % key last))
         frequencies
         (apply max-key val)
         key)))

(defn sample->hyp
  "Take a sample and convert it into a hypothesis function using the kth
  nearest neighbour algorithm."
  [sample k]
  (-> (reduce qt/insert (qt/quad-tree) sample)
      (classify k)))
