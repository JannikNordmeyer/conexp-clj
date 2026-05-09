;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.residuum
  (:require [clojure.core.reducers :as r]
            [clojure.math.combinatorics :refer [permuted-combinations combinations]]
            [clojure.set :refer [difference union intersection subset?]]
            [conexp.base :refer :all]
            [conexp.math.markov :refer :all]
            [conexp.fca
             [contexts :refer [make-context incidence dual-context
                               attribute-derivation
                               context-attribute-closure
                               context-object-closure
                               object-derivation random-context
                               objects attributes
                               context? concept?
                               reduce-context
                               up-arrows down-arrows]]
             [exploration :refer :all]
             [fast :refer [with-binary-context
                           to-bitset
                           bitwise-context-attribute-closure
                           bitwise-object-derivation
                           bitwise-attribute-derivation concepts]]
             [implications :refer :all]
             [lattices :refer :all]

             [distributivity :refer [birkhoff-downset-completion birkhoff-upset-completion]]
             [posets :refer [order-ideal order-filter poset-upper-neighbours poset-lower-neighbours]]]

            [conexp.math.util :refer [eval-polynomial binomial-coefficient]]
            [conexp.io.contexts :refer [read-context]])
  (:import [conexp.fca.lattices Lattice]
           [java.util ArrayList BitSet]))





(defn incompatible-triples [lat]
  "Given lattice lat, compute triples (x,y,z)∈L³ (pairwise different)
  such that they do not fullfil the distributive property
  x∨(y∧z)=(x∨y)∧(x∨z)."
  (let [base-set (lattice-base-set lat)
        inf (inf lat)
        sup (sup lat)]
    (filter (fn [[x y z]] (not (= (sup x (inf y z ))
                                  (inf (sup x y) (sup x z)))))
            (combinations base-set 3)))
)

(defn incompatible-triples2 [lat]
  "Given lattice lat, compute triples (x,y,z)∈L³ (pairwise different)
  such that they do not fullfil the distributive property
  x∨(y∧z)=(x∨y)∧(x∨z)."
  (let [base-set (lattice-base-set lat)
        inf (inf lat)
        sup (sup lat)]
    (filter (fn [[x y z]] (not (= (inf x (sup y z ))
                                  (sup (inf x y) (inf x z)))))
            (for [x base-set y base-set z base-set] [x y z])))
)

(defn incompatible-triples3 [lat]
  "Given lattice lat, compute triples (x,y,z)∈L³ (pairwise different)
  such that they do not fullfil the distributive property
  x∨(y∧z)=(x∨y)∧(x∨z)."
  (let [base-set (lattice-base-set lat)
        inf (inf lat)
        sup (sup lat)]
    (filter (fn [[x y z]] (not (= (sup (sup (inf x y) (inf x z)) (inf y z))
                                  (inf (inf (sup x y) (sup x z)) (sup y z)))))
            (for [x base-set y base-set z base-set] [x y z])))
)

(defn minimal-hitting-set [triples]
  (let [triples (map set triples)
        best (atom nil)]

    (letfn [(covered? [chosen triple]
              (some chosen triple))

            (all-covered? [chosen]
              (every? #(covered? chosen %) triples))

            (search [chosen remaining]
              (when (or (nil? @best)
                        (< (count chosen) (count @best)))

                (if (all-covered? chosen)
                  (reset! best chosen)

                  (when-let [t (first (filter #(not (covered? chosen %)) triples))]
                    (doseq [e t]
                      (search (conj chosen e) (disj remaining e)))))))]

      (search #{} (set (mapcat identity triples)))
      @best))
)

(defn greedy-hitting-set [triples]
  (loop [remaining (map set triples)
         solution  #{}]

    (if (empty? remaining)
      solution

      (let [freqs (frequencies (mapcat identity remaining))
            best  (key (apply max-key val freqs))
            remaining' (remove #(contains? % best) remaining)]

        (recur remaining' (conj solution best))))))


(defn greedy-residuum [lat incompatibility-function]
  (let [incompatibility-relation (incompatibility-function lat)
        hitting-set (greedy-hitting-set incompatibility-relation)]
  (println (count incompatibility-relation))
  (println (count hitting-set))
  (make-lattice (difference (lattice-base-set lat) hitting-set) (lattice-order lat)))

)

(defn distributive-residuum [lat incompatibility-function]
  (let [incompatibility-relation (incompatibility-function lat)
        hitting-set (minimal-hitting-set incompatibility-relation)]
  (println (count incompatibility-relation))
  (println (count hitting-set))
  (make-lattice (difference (lattice-base-set lat) hitting-set) (lattice-order lat)))

)

(defn sublattice? [lat1 lat2]
  (let [base-set1 (lattice-base-set lat1)
        inf1 (inf lat1)
        sup1 (sup lat1)
        inf2 (inf lat2)
        sup2 (sup lat2)]
    (every? identity (for [x base-set1 y base-set1] (and (= (sup1 x y) (sup2 x y))
                                                         (= (inf1 x y) (inf2 x y))))))
)

(defn explicit-sublattice? [lat1 lat2]
  (let [base-set1 (lattice-base-set lat1)
        inf1 (inf lat1)
        sup1 (sup lat1)
        inf2 (inf lat2)
        sup2 (sup lat2)]
    (filter #(not (last %)) (for [x base-set1 y base-set1] [x y 
                                                      (and (= (sup1 x y) (sup2 x y))
                                                           (= (inf1 x y) (inf2 x y)))])))
)

(defn anonymize-lattice [lat]
  (let [base-set (lattice-base-set lat)
        order-fn (lattice-order lat)
        order-relation (filter order-fn (for [x base-set y base-set] [x y]))
        mapping (zipmap base-set (range))
      
        new-order (set (map #(vector (mapping (first %)) (mapping (second %))) order-relation))
        new-base-set (set (vals mapping))]
    
    (make-lattice new-base-set new-order))
)

(defn relabel
  [base-set relation]
  (let [;; assign each element a unique integer
        mapping (zipmap base-set (range))
        
        ;; replace elements in relation pairs
        new-relation (set
                       (map (fn [[a b]]
                              [(mapping a) (mapping b)])
                            relation))
        
        ;; new base set is just the integers
        new-base-set (set (vals mapping))]
    
    {:base-set new-base-set
     :relation new-relation
     :mapping mapping}))

(def lat (make-lattice #{"Top" "Bot" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"}
                       #{["Top" "Top"]
                         ["g" "Top"] ["g" "g"]
                         ["h" "Top"] ["h" "h"]
                         ["i" "Top"] ["i" "i"]
                         ["j" "Top"] ["j" "j"]
                         ["a" "Top"] ["a" "g"] ["a" "h"] ["a" "a"]
                         ["b" "Top"] ["b" "g"] ["b" "i"] ["b" "b"]
                         ["c" "Top"] ["c" "g"] ["c" "j"] ["c" "c"]
                         ["d" "Top"] ["d" "h"] ["d" "i"] ["d" "d"]
                         ["e" "Top"] ["e" "h"] ["e" "j"] ["e" "e"]
                         ["f" "Top"] ["f" "i"] ["f" "j"] ["f" "f"]
                         ["Bot" "Top"] ["Bot" "g"] ["Bot" "h"] ["Bot" "i"] ["Bot" "j"] ["Bot" "a"] 
                         ["Bot" "b"] ["Bot" "c"] ["Bot" "d"] ["Bot" "e"] ["Bot" "f"] ["Bot" "Bot"]}))

(def lat2 (make-lattice #{"Top" "Bot" "u" "v" "x" "y" "a" "b" "c"}
                        #{["Top" "Top"]
                          ["u" "Top"] ["u" "u"]
                          ["v" "Top"] ["v" "u"] ["v" "x"] ["v" "a"] ["v" "v"]
                          ["x" "Top"] ["x" "x"]
                          ["y" "Top"] ["y" "u"] ["y" "x"] ["y" "a"] ["y" "b"] ["y" "c"] ["y" "y"]
                          ["a" "Top"] ["a" "u"] ["a" "x"] ["a" "a"]
                          ["b" "Top"] ["b" "x"] ["b" "b"]
                          ["c" "Top"] ["c" "x"] ["c" "c"]
                          ["Bot" "Top"] ["Bot" "u"] ["Bot" "x"] ["Bot" "a"] ["Bot" "b"] ["Bot" "c"] ["Bot" "v"] ["Bot" "y"] ["Bot" "Bot"]}))

(def testlat (make-lattice #{"Top" "Bot" "a" "b" "c" "x"} #{["Top" "Top"] ["a" "Top"] ["a" "a"] ["b" "Top"] ["b" "b"] ["c" "Top"] ["c" "c"] ["x" "Top"] ["x" "b"] ["x" "c"] ["x" "x"] ["Bot" "Top"] ["Bot" "a"] ["Bot" "b"] ["Bot" "c"] ["Bot" "x"] ["Bot" "Bot"] }))


(defn test-sublattices [strs incompatiblilty-function]
  (doseq [ctxstr strs]
    (println ctxstr)
    (let [lat (concept-lattice (read-context (str "testing-data/" ctxstr)))
          sublattice (distributive-residuum lat incompatiblilty-function)]
      (println (distributive? sublattice))
      (println (sublattice? sublattice lat))
      (println "---------------")))

)

(defn testresiduum [n]
  (doseq [i (range n)]
    (let [ctx (random-context #{1 2 3 4 5 6 7 8 9 10 11 12} 0.3)
          lat (concept-lattice ctx)
          residuum (distributive-residuum lat incompatible-triples)]
      (println (sublattice? residuum lat))))
)

;(def l (anonymize-lattice (concept-lattice (random-context #{1 2 3 4 5 6 7 8 9 10 11 12} 0.3))))
;(def residuum (greedy-residuum l incompatible-triples))
;(sublattice? residuum l)
;(distributive residuum)