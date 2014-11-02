(ns knn.core
  (:require [incanter.distributions :refer [draw]]
            [clojure.java.io :refer [reader]]
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
    (let [{pos 1.0 neg -1.0
           :or {pos 0 neg 0}}
          (->> (qt/k-nn qt pos k)
               (map #(-> % key last))
               frequencies)]
      (cond
        (= pos neg) (draw [1.0 -1.0])
        (< pos neg) -1.0
        (> pos neg)  1.0))))

(defn sample->hyp
  "Take a sample and convert it into a hypothesis function using the kth
  nearest neighbour algorithm."
  [sample k]
  (-> (reduce qt/insert (qt/quad-tree) sample)
      (classify k)))

(defn empirical-error
  "Indicates the experimental error of the classifier created from the
  `train` data against the `test` data, using the `k` nearest neighbours
  algorithm."
  [k train test]
  (let [N (double (count test))
        h (sample->hyp train k)]
    (/ (reduce (fn [err [pos lab]]
                 (if (not= (h pos) lab)
                   (inc err)
                   err))
               0 test)
       N)))
