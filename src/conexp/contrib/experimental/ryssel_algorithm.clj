;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.experimental.ryssel-algorithm
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications))

(ns-doc "An implementation of Ryssels Algorithm")

;;;

(defn- cover [base-set candidates A]
  (let [object-covers (minimum-set-covers
                       (difference base-set A)
                       (set-of (difference base-set N) | N candidates))]
    (map (fn [cover]
           (map #(difference base-set %) cover))
         object-covers)))

(defn ryssel-base
  "Returns the implications computed by Ryssels Algorithm, as a lazy sequence."
  [ctx]
  (let [gens (reduce! (fn [map x]     ;generating elements of attribute extents
                        (let [extent (aprime ctx #{x})]
                          (assoc! map extent
                            (conj (get map extent #{}) x))))
                      {}
                      (attributes ctx)),
        M    (set (keys gens))]       ;all attribute extents
    (->> (reduce into
                 ()
                 (pmap (fn [A]
                         (let [candidates (set-of U | U (disj M A),
                                                      :let [U-cap-A (intersection U A)]
                                                      :when (not (exists [V M]
                                                                   (and (proper-subset? V A)
                                                                        (subset? U-cap-A V))))),
                               candidates (difference candidates
                                                      (set-of (intersection X Y) | X candidates, Y candidates
                                                                                   :when (and (not (subset? X Y))
                                                                                              (not (subset? Y X))))),
                               covers     (cover (objects ctx) candidates A)]
                           (concat (for [m (gens A)
                                         :when (exists [N M]
                                                 (and (subset? A N)
                                                      (not= (gens N) #{m})))]
                                     #{m})
                                   (for [X covers]
                                     (set-of m | Y X, m (gens Y))))))
                       M))
         distinct
         (map #(make-implication % (context-attribute-closure ctx %))))))

;;;

nil
