;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.util
  (:use conexp.base
        conexp.layouts.base
        conexp.fca.lattices)
  (:require [clojure.contrib.graph :as graph]))

(ns-doc
 "Utilities for computing lattice layouts.")

;;;

(defn enclosing-rectangle
  "Returns left lower and right upper edge of the minimal rectangle
  containing all points. The coordinates are given in a vector of the
  form [x_min y_min x_max y_max]."
  [points]
  (when (empty? points)
    (illegal-argument (str "Cannot scale empty sequence of points.")))
  (let [[x0 y0] (first points),
        [x_min y_min x_max y_max] (loop [x_min x0
                                         y_min y0
                                         x_max x0
                                         y_max y0
                                         points (rest points)]
                                    (if (empty? points)
                                      [x_min y_min x_max y_max]
                                      (let [[x y] (first points)]
                                        (recur (min x x_min)
                                               (min y y_min)
                                               (max x x_max)
                                               (max y y_max)
                                               (rest points)))))]
    [x_min y_min x_max y_max]))

(defn- scale-points-to-rectangle
  "Scales the collection of points such that they fit in the
  rectangle given by [x1 y1] and [x2 y2]."
  [[x1 y1] [x2 y2] points]
  (let [[x_min y_min x_max y_max] (enclosing-rectangle points),
        [a_x, b_x] (if (= x_min x_max)
                     [0, (/ (+ x1 x2) 2)]
                     (let [slope (/ (- x1 x2) (- x_min x_max))]
                       [slope, (- x1 (* slope x_min))])),
        [a_y, b_y] (if (= y_min y_max)
                     [0, (/ (+ y1 y2) 2)]
                     (let [slope (/ (- y1 y2) (- y_min y_max))]
                       [slope, (- y1 (* slope y_min))]))]
    (map (fn [[x y]]
           [(+ (* a_x x) b_x), (+ (* a_y y) b_y)])
         points)))

(defn scale-layout
  "Scales given layout to rectangle [x1 y1], [x2 y2]."
  [[x1 y1] [x2 y2] layout]
  (let [points (seq (positions layout))]
    (make-layout (lattice layout)
                 (zipmap (map first points)
                         (scale-points-to-rectangle [x1 y1] [x2 y2]
                                                    (map second points)))
                 (connections layout))))

;;;

(defn- lattice->graph
  "Converts given lattice to it's corresponding graph with loops
  removed."
  [lattice]
  (-> (struct-map graph/directed-graph
        :nodes (base-set lattice)
        :neighbors (memoize
                    (fn [x]
                      (let [order (order lattice)]
                        (filter #(order [x %]) (base-set lattice))))))
      graph/remove-loops))

(defn layers
  "Returns the layers of the given lattice, that is sequence of points
  with equal depth, starting with the lowest layer."
  [lattice]
  (reverse (graph/dependency-list (lattice->graph lattice))))

(defn edges
  "Returns a sequence of pairs of vertices of lattice which are
  directly neighbored in lattice."
  [lattice]
  (set-of [x y] [x (base-set lattice),
                 y (base-set lattice),
                 :when (directly-neighboured? lattice x y)]))

(defn top-down-elements-in-layout
  "Returns the elements in layout ordered top down."
  [layout]
  (let [graph (struct-map graph/directed-graph
                :nodes (keys (positions layout))
                :neighbors (memoize (fn [x]
                                      (map second (filter (fn [[a b]] (= a x))
                                                          (connections layout))))))]
    (apply concat (graph/dependency-list graph))))

;;; grid adjustment

(defn- fit-point-to-grid
  "Moves given point [x y] to the next point on the grid given by the
  origin [x_origin y_origin] and the paddings x_pad and y_pad."
  [[x_origin y_origin] x_pad y_pad [x y]]
  [(+ x_origin (* x_pad (round (/ (- x x_origin) x_pad)))),
   (+ y_origin (* y_pad (round (/ (- y y_origin) y_pad))))])

(defn fit-layout-to-grid
  "Specifies a grid by a origin point and paddings x_pad and y_pad
  between two adjacent grid lines in x and y direction
  respectively. Returns the layout resulting from adjusting the given
  layout on this layout."
  [layout origin x_pad y_pad]
  (let [fit-point (partial fit-point-to-grid origin x_pad y_pad)]
    (update-positions layout
                      (persistent!
                       (reduce (fn [map [name [x y]]]
                                 (assoc! map name (fit-point [x y])))
                               (transient {})
                               (positions layout))))))

(defn discretize-layout
  "Adjusts the given layout to fit on a grid of x_cells cells in the x
  coordinate and y_cells cells in the y coordinate."
  [layout x_cells y_cells]
  (assert (< 0 x_cells))
  (assert (< 0 y_cells))
  (let [[x_min y_min x_max y_max] (enclosing-rectangle (vals (positions layout))),
        origin [x_min y_min],
        x_pad  (/ (- x_max x_min) x_cells),
        y_pad  (/ (- y_max y_min) y_cells)]
    (fit-layout-to-grid layout origin x_pad y_pad)))

;;;

nil
