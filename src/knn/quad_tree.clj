(ns knn.quad-tree
  (:require [clojure.data.priority-map :refer :all]))

(defrecord ^:private QuadTree
  [pos label tl tr bl br])

(defn- quad
  "Returns the sub-quad the position belongs to."
  [qt pos]
  (let [[dx dy] (map - pos (:pos qt))
        neg-dx? (neg? dx)
        neg-dy? (neg? dy)]
    (if neg-dy?
      (if neg-dx? :bl :br)
      (if neg-dx? :tl :tr))))

(defn- x-nbr
  "Lateral neighbour of a sub-quad"
  [sq]
  (case sq
    :tl :tr, :tr :tl
    :bl :br, :br :bl))

(defn- y-nbr
  "Vertical neighbour of a sub-quad"
  [sq]
  (case sq
    :tl :bl, :tr :br
    :bl :tl, :br :tr))

(defn- d-nbr
  "Diagonal neighbour of a sub-quad"
  [sq]
  (case sq
    :tl :br, :tr :bl
    :bl :tr, :br :tl))

(defn- diag?
  "Predicate to determine whether two sub-quads are diagonal to each other."
  [a b] (= a (d-nbr b)))

(defn insert
  "Insert a `pos`, `label` pair into the `qt` quad-tree."
  [qt [pos label :as plp]]
  (if qt
    (update-in qt [(quad qt pos)]
               insert plp)
    (->QuadTree pos label
                nil nil nil nil)))

(defn quad-tree
  "Given a list of position label pairs, returns the quad tree representing
  them."
  ([] nil)
  ([& plps]
   (reduce insert (quad-tree)
           (partition 2 plps))))

(defn- dist
  "Euclidean distance in R^2 between two points."
  [a b]
  (let [[dx dy] (map - a b)]
    (Math/sqrt (+ (* dx dx)
                  (* dy dy)))))

(defn- furthest
  "Returns the distance of the furthest neighbour."
  [nbrs]
  (if-let [kvp (->> nbrs rseq first)]
    (val kvp)
    0))

(defn k-nn
  "Returning the k nearest neighbours to `pos` in `qt`, if there are that many
  such neighbours."
  [qt [x y :as pos] k]
  (if qt
    (let [[cx cy :as pos-qt] (:pos qt)
          sq   (quad qt pos)
          dd   (dist pos pos-qt)
          nbrs (priority-map
                 (x-nbr sq) (Math/abs (- x cx))
                 (y-nbr sq) (Math/abs (- y cy))
                 (d-nbr sq) dd)]
      (into (priority-map)
            (take k (-> (reduce (fn [nns [sq* d]]
                                  (if (< (furthest nns) d) nns
                                    (->> (k-nn (sq* qt) pos k)
                                         (into nns) (take k)
                                         (into (empty nns)))))
                                (k-nn (sq qt) pos k) nbrs)
                        (assoc [pos-qt (:label qt)] dd)))))
    (priority-map)))
