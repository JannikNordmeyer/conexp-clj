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
            [conexp.io.contexts :refer [read-context]]
            [clojure.java.io :as io])
  (:import [conexp.fca.lattices Lattice]
           [java.util ArrayList BitSet]))




(defn d1 [x y z lat] 
  "Verifies the identity x∨(y∧z)=(x∨y)∧(x∨z) on a triple of lattice elements."
    (let [inf (inf lat)
          sup (sup lat)]
      (= (sup x (inf y z ))
         (inf (sup x y) (sup x z))))
)

(defn d2 [x y z lat] 
  "Verifies the identity x∧(y∨z)=(x∧y)∨(x∧z) on a triple of lattice elements."
    (let [inf (inf lat)
          sup (sup lat)]
      (= (inf x (sup y z ))
         (sup (inf x y) (inf x z))))
)

(defn d3 [x y z lat] 
  "Verifies the identity (a ∨ b) ∧ (b ∨ c) ∧ (a ∨ c) = (a ∧ b) ∨ (b ∧ c) ∨ (a ∧ c) 
   on a tripe of lattice elements."
    (let [inf (inf lat)
          sup (sup lat)]
      (= (sup (sup (inf x y) (inf x z)) (inf y z))
                                  (inf (inf (sup x y) (sup x z)) (sup y z))))
)


(defn incompatible-triples [lat f]
  "Returns a Set of all triples (not respecting order) that do not satisfy the identity *f*."
    (let [base-set (lattice-base-set lat)]
    (filter (fn [[x y z]] (not (f x y z lat)))
            (combinations base-set 3)))
)


(defn hitting-set? [candidate relation]
  "Verifies whether the set *candidate* is a hitting set of the supplied relation.
   The relation must be supplies as a collection of sets on the universe as *candidate*."
  (every? #(not (empty? (intersection (set candidate) %))) relation)
)


(defn minimal-hitting-sets [relation]
  "Returns a collection of all cardinality-minimal hitting sets of the supplied relation.
  The relation must be supplied as a collection of sets."
  (let [universe (vec (reduce union relation))]
    (loop [n 1]
      (let [candidates (combinations universe n)
            hitting-sets (filter #(hitting-set? % relation) candidates)]
        (if (not (empty? hitting-sets))
          hitting-sets
          (recur (+ n 1))))))
)


(defn max-covering-elements [relation]
  "Accepts a relation as a collection of sets and returns a set of elements from the same universe
   that appear in a maximal amount of entries in the relation."
  (let [universe (reduce union relation)]
    (loop [remaining universe
           maximal #{}
           max-intersection-count 0]
      (let [current (first remaining)
            current-intersection-count (count (filter #(contains? % current) relation))]
        (if (not current)
          maximal   
          (if (< max-intersection-count current-intersection-count)
            (recur (rest remaining)
                   #{current}
                   current-intersection-count)
            (if (= max-intersection-count current-intersection-count)
              (recur (rest remaining)
                     (conj maximal current)
                     max-intersection-count)
              (recur (rest remaining)
                     maximal
                     max-intersection-count)))))))
)

(defn remove-covered [elements relation]
  "Accepts a relation as a set of sets and removes all relation entries that have an intersection with
   the set *elements*."
  (filter #(empty? (intersection % elements)) relation)
)


(defn greedy-hitting-sets [relation]
  (loop [hitting-sets #{}
         current-sets #{#{}}]
    (let [new-sets (for [current-set current-sets 
                         new-element (max-covering-elements (remove-covered current-set relation))] 
                     (conj current-set new-element))
          new-hitting-sets (set (filter #(hitting-set? % relation) new-sets))]
      (if (empty? new-sets)
        hitting-sets
        (recur (union hitting-sets new-hitting-sets)
               (difference (set new-sets) new-hitting-sets)))))
)



(defn read-non-isomorphic-lattices [address]
  (with-open [rdr (io/reader address)]
    (doall
      (for [line (line-seq rdr)]
        (covering-relation-from-string line)))))

(defn covering-relation-from-string [s]
  (loop [remaining (vec s)
         tuples #{}
         chunk-size 1]
    (if (empty? remaining)
      tuples
      (let [chunk (take chunk-size remaining)
            new-tuples (set (for [i (range chunk-size) 
                                  :when (= (nth chunk i) \1)] 
                              [i chunk-size]))]
        (recur (subvec remaining chunk-size)
               (union tuples new-tuples)
               (+ chunk-size 1)))))
)



;(read-non-isomorphic-lattices "testing-data/non-isomorphic-lattices/unlabelled-05.cats")


(defn transitive-closure [relation]
  (loop [r (set relation)]
    (let [new-pairs
          (set
           (for [[a b] r
                 [c d] r
                 :when (= b c)]
             [a d]))
          r' (into r new-pairs)]
      (if (= r r')
        r
        (recur r'))))
)

(defn add-reflexive [relation]
  (let [universe (reduce union relation)]
    (union relation (for [e universe] [e e])))
)

(defn lattice-from-covering-relation [relation]
  (make-lattice (reduce union relation) #(.contains (transitive-closure (add-reflexive relation)) [%1 %2]))
)




(defn sublattice? [lat1 lat2]
  "Verifies whether *lat1* is a sublattice of *lat2*."
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


