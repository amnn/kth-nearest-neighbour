(ns knn.quad-tree)

(defrecord ^:private QuadTree
  [pos label tl tr bl br])

(defn- quad [qt pos]
  "Returns the sub-quad the position belongs to."
  (let [[dx dy] (map - pos (:pos qt))
        neg-dx? (neg? dx)
        neg-dy? (neg? dy)]
    (if neg-dy?
      (if neg-dx? :bl :br)
      (if neg-dx? :tl :tr))))

(defn insert [qt [pos label :as plp]]
  "Insert a `pos`, `label` pair into the `qt` quad-tree."
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
