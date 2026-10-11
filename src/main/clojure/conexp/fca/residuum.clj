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
            [conexp.math.algebra :refer :all]
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
             [posets :refer :all]
             [distributivity :refer [birkhoff-downset-completion birkhoff-upset-completion]]
             [posets :refer [order-ideal order-filter poset-upper-neighbours poset-lower-neighbours]]]

            [conexp.math.util :refer [eval-polynomial binomial-coefficient]]
            [conexp.io.contexts :refer [read-context]]
            [clojure.java.io :as io])
  (:import [conexp.fca.lattices Lattice]
           [conexp.fca.posets Poset]
           [java.util ArrayList BitSet]))


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

(defn d1 [x y z lat] 
  "Verifies the identity x∨(y∧z)=(x∨y)∧(x∨z) on a triple of lattice elements.
  This predicate is not commutative."
    (let [inf (inf lat)
          sup (sup lat)]
      (= (sup x (inf y z))
         (inf (sup x y) (sup x z))))
)

(defn d2 [x y z lat] 
  "Verifies the identity x∧(y∨z)=(x∧y)∨(x∧z) on a triple of lattice elements.
   This predicate is not commutative."
    (let [inf (inf lat)
          sup (sup lat)]
      (= (inf x (sup y z ))
         (sup (inf x y) (inf x z))))
)

(defn d3 [x y z lat] 
  "Verifies the identity (a ∨ b) ∧ (b ∨ c) ∧ (a ∨ c) = (a ∧ b) ∨ (b ∧ c) ∨ (a ∧ c) 
   on a triple of lattice elements. This predicate is commutative."
    (let [inf (inf lat)
          sup (sup lat)]
      (= (sup (sup (inf x y) (inf x z)) (inf y z))
                                  (inf (inf (sup x y) (sup x z)) (sup y z))))
)

(defn d4 [x y z lat]
  "Verifies whether the supplied elements generate a destributive sublattice. 
   This predicate is commutative."
  (distributive? (generated-sublattice lat #{x y z}))
)




(defn incompatible-triples
  "Returns a Set of all triples (not respecting order) that do not satisfy the identity *f*.
  If the predicate *f* is commutative, the triples will be more efficiently represented as sets."
  ([lat f] (incompatible-triples lat f false))
  ([lat f commutative]
    (let [base-set (lattice-base-set lat)
          triples (if commutative (combinations base-set 3) (permuted-combinations base-set 3))]
      (set (map set (filter (fn [[x y z]] (not (f x y z lat)))
                          triples)))))
)



(defn respectant? [lat f]
  "Verifies if the predicate *f* is true on all triples of the supplied lattice."
  (= (count (incompatible-triples lat f)) 0)
)


(defn hitting-set? [candidate relation]
  "Verifies whether the set *candidate* is a hitting set of the supplied relation.
   The relation must be supplies as a collection of sets on the universe as *candidate*."
  (every? #(not (empty? (intersection (set candidate) %))) 
          relation)
)


(defn minimal-hitting-sets [relation]
  "Returns a collection of all cardinality-minimal hitting sets of the supplied relation.
  The relation must be supplied as a collection of sets."
  (if (empty? relation)
    #{}
    (let [universe (vec (reduce union relation))]
      (loop [n 1]
        (let [candidates (combinations universe n)
              hitting-sets (filter #(hitting-set? % relation) candidates)]
          (if (not (empty? hitting-sets))
            hitting-sets
            (recur (+ n 1)))))))
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
  "Returns all hitting sets that can be generated by iteratively choosing the element that covers
   the most tuples in the relation."
  (loop [hitting-sets #{}
         current-sets #{#{}}]
    (let [new-sets (for [current-set current-sets 
                         new-element (max-covering-elements (remove-covered current-set relation))] 
                     (conj current-set new-element))
          new-hitting-sets (set (filter #(hitting-set? % relation) new-sets))]
      (if (or (not (empty? hitting-sets)) (empty? new-sets))
        hitting-sets
        (recur (union hitting-sets new-hitting-sets)
               (difference (set new-sets) new-hitting-sets)))))
)


(defn residuum [f lat commutative]
  (let [incompatibility-relation (set (incompatible-triples lat f commutative))
        hitting-sets (minimal-hitting-sets incompatibility-relation)
        posets (for [h hitting-sets] (make-poset-nc (difference (lattice-base-set lat) h) 
                                                    (lattice-order lat)))]
    (for [p posets] (if (has-lattice-order? p) (make-lattice-nc (base-set p) (order p))
                                              p)))
)

(defn greedy-residuum [f lat commutative]
  (let [incompatibility-relation (set (incompatible-triples lat f commutative))
        hitting-sets (greedy-hitting-sets incompatibility-relation)
        posets (for [h hitting-sets] (make-poset-nc (difference (lattice-base-set lat) h) 
                                                    (lattice-order lat)))]
    (for [p posets] (if (has-lattice-order? p) (make-lattice-nc (base-set p) (order p))
                                              p)))
)

(defn syndrome [f lat commutative]
  (let [posets (residuum f lat commutative)
        lattices (filter has-lattice-order? posets)]
  [(count posets)
   (count lattices)
   (count (filter #(sublattice? % lat) lattices))
   (count (filter #(respectant? % f) lattices))
   (count (filter #(and (sublattice? % lat) (respectant? % f)) lattices))])
)

(defn greedy-syndrome [f lat commutative]
  (let [posets (greedy-residuum f lat commutative)
        lattices (filter has-lattice-order? posets)]
  [(count posets)
   (count lattices)
   (count (filter #(sublattice? % lat) lattices))
   (count (filter #(respectant? % f) lattices))
   (count (filter #(and (sublattice? % lat) (respectant? % f)) lattices))])
)



(defn perfect? [syndrome]
  (apply = syndrome)
)



(defn covering-relation-from-string [s]
  "Accepts a string representation of a covering relation in the Gebhardt format
   used in the non-isomorphic lattices dataset and returns a set of the covering relation."
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


(defn- transitive-closure [relation]
  "Accepts a relation in set form and returns the transitive closure thereof."
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

(defn- add-reflexive [relation]
  "Accepts a relation in set form and adds pairs to make the relation reflexive.
   The relation's universe is implicitely computed by checking all elements in the relations tuples."
  (let [universe (reduce union relation)]
    (union relation (for [e universe] [e e])))
)

(defn lattice-from-covering-relation [relation]
  "Accepts a covering relation in set form and returns the lattice represented by the relation."
  (make-lattice-nc (reduce union relation) 
                (transitive-closure (add-reflexive relation)))
)


(defn read-non-isomorphic-lattices [address]
  "Reads a file from the non-isomorphic lattices dataset and returns all lattices contained within."
  (with-open [rdr (io/reader address)]
    (doall
      (for [line (line-seq rdr)]
        (lattice-from-covering-relation (covering-relation-from-string line)))))
)



;(read-non-isomorphic-lattices "testing-data/non-isomorphic-lattices/unlabelled-05.cats")



(defn evaluate-non-isomorphic-lattices [f commutative address]
  (with-open [rdr (io/reader address)]
      (doseq [line (line-seq rdr)]
        (let [syn (syndrome f (lattice-from-covering-relation (covering-relation-from-string line)) commutative)]
          (println syn
                   (perfect? syn)
                   line))))
)

(defn greedy-evaluate-non-isomorphic-lattices [f commutative address]
  (with-open [rdr (io/reader address)]
      (doseq [line (line-seq rdr)]
        (let [syn (greedy-syndrome f (lattice-from-covering-relation (covering-relation-from-string line)) commutative)]
          (println syn
                   (perfect? syn)
                   line))))
)


(defn evaluate [in-address out-address]
  (with-open [r (io/reader in-address)
              w (io/writer out-address)]
    (.write w (str "String-Representation; "
                   "Size; "
                   "d1-Syndrome; "
                   "d1-Syndrome Perfect?; "
                   "greedy-d1-Syndome; "
                   "greedy-d1-Syndrome Perfect?; "
                   "d1-greedy-same?; "

                   "d2-Syndrome; "
                   "d2-Syndrome Perfect?; "
                   "greedy-d2-Syndome; "
                   "greedy-d2-Syndrome Perfect?; "
                   "d2-greedy-same?; "

                   "d3-Syndrome; "
                   "d3-Syndrome Perfect?; "
                   "greedy-d3-Syndome; "
                   "greedy-d3-Syndrome Perfect?; "
                   "d3-greedy-same?" 
                   "\n"))
    (doseq [line (line-seq r)]
      (let [lat (lattice-from-covering-relation (covering-relation-from-string line))
            d1-res (syndrome d1 lat false)
            d1-res-perfect (perfect? d1-res)
            d1-res-greedy (greedy-syndrome d1 lat false)
            d1-res-greedy-perfect (perfect? d1-res-greedy)
            d1-greedy-same (= d1-res d1-res-greedy)

            d2-res (syndrome d2 lat false)
            d2-res-perfect (perfect? d2-res)
            d2-res-greedy (greedy-syndrome d2 lat false)
            d2-res-greedy-perfect (perfect? d2-res-greedy)
            d2-greedy-same (= d2-res d2-res-greedy)

            d3-res (syndrome d3 lat true)
            d3-res-perfect (perfect? d3-res)
            d3-res-greedy (greedy-syndrome d3 lat true)
            d3-res-greedy-perfect (perfect? d3-res-greedy)
            d3-greedy-same (= d3-res d3-res-greedy)]
        (.write w (str line "; "
                       (count (lattice-base-set lat)) "; "
                       d1-res "; "
                       d1-res-perfect "; "
                       d1-res-greedy "; "
                       d1-res-greedy-perfect "; "
                       d1-greedy-same "; "

                       d2-res "; "
                       d2-res-perfect "; "
                       d2-res-greedy "; "
                       d2-res-greedy-perfect "; "
                       d2-greedy-same "; "
                       
                       d3-res "; "
                       d3-res-perfect "; "
                       d3-res-greedy "; "
                       d3-res-greedy-perfect "; "
                       d3-greedy-same
                       "\n")))))
)


;automated counterexample search for triple properties

(defn- contradicting-triples [lat premise-predicate conclusion-predicate]
  "Returns the set of Lattice triples that contradict the supplied implication of triple predicates."
  (let [base-set (lattice-base-set lat)
        triples  (permuted-combinations base-set 3)]
     (set (filter (fn [[x y z]] (and (premise-predicate x y z lat) (not (conclusion-predicate x y z lat))))
                           triples)))
)

(defn find-counterexample [premise-predicate conclusion-predicate address]
  "Searches the file at the supplied adress for a counterexample that contradicts the supplied implication."
  (with-open [rdr (io/reader address)]
  (loop []
    (let [line (.readLine rdr)
          lat (lattice-from-covering-relation (covering-relation-from-string line))
          contradictions (contradicting-triples lat premise-predicate conclusion-predicate)]
       (if line   
          (if (seq contradictions)
             [line contradictions]
             (recur))))))
)

(defn search-for-counterexamples [premise-predicate conclusion-predicate max-search-size]
  "Searches the non-isomorphic-lattice dataset for a counterexample of the supplied implication of predicates
   until the supplied maximum size."
  (loop [size 2]
    (let [search-results (find-counterexample premise-predicate 
                                              conclusion-predicate 
                                              (str "testing-data/non-isomorphic-lattices/unlabelled-"
                                                   size
                                                   ".cats"))]
      (if (<= size max-search-size) 
        (if search-results 
          search-results
          (recur (+ size 1))))))
)


(defn compute-predicates [latstr triple predicates]
  (let [lat (lattice-from-covering-relation (covering-relation-from-string latstr))
        [x y z] triple
        predicate-wrapper (fn [predicate] (predicate x y z lat))]
    (filter predicate-wrapper predicates))
)

(defn- conjoin-predicates [predicates]
  "Accepts a set of prediates and returns the conjuction on them as a function."
  (fn [& args]
    (every? #(apply % args) predicates))
)



(defn counterexample-search-handler [ctx known impl]
(println ctx)
  (let [[counterexample-lat counterexample-triples] (search-for-counterexamples (conjoin-predicates (premise impl)) 
                                                                               (conjoin-predicates (conclusion impl)) 12)]
    (do (if counterexample-lat (print (str impl " does not hold. Counterexample:\n" [counterexample-lat (first counterexample-triples)] "\n"))
                               (print (str "No counterexample found for implication " impl ". Assumed to be true.\n")))
      [[[counterexample-lat (first counterexample-triples)] (compute-predicates counterexample-lat 
                                                                               (first counterexample-triples)
                                                                               (attributes ctx))]]))
)


;(def pctx (make-context #{} #{d1 d2 d3 d4} #{}))
;(explore-attributes :context pctx :handler counterexample-search-handler)

(def lat (make-lattice #{1 2 3 4 5 6} #{[1 1]
                                        [2 1] [2 2]
                                        [3 1] [3 3]
                                        [4 1] [4 4]
                                        [5 1] [5 3] [5 4] [5 5]
                                        [6 1] [6 2] [6 3] [6 4] [6 5] [6 6]}))

(def lat2 (make-lattice #{"a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "top" "bot"}
                        #{["top" "top"]
                          ["g" "top"] ["g" "g"]
                          ["h" "top"] ["h" "h"]
                          ["i" "top"] ["i" "i"]
                          ["j" "top"] ["j" "j"]
                          ["d" "top"] ["d" "h"] ["d" "i"] ["d" "d"]
                          ["a" "top"] ["a" "g"] ["a" "h"] ["a" "a"]
                          ["b" "top"] ["b" "g"] ["b" "i"] ["b" "b"]
                          ["c" "top"] ["c" "g"] ["c" "j"] ["c" "c"]
                          ["e" "top"] ["e" "h"] ["e" "j"] ["e" "e"]
                          ["f" "top"] ["f" "i"] ["f" "j"] ["f" "f"]
                          ["bot" "bot"]["bot" "a"]["bot" "b"]["bot" "c"]["bot" "d"]["bot" "e"]["bot" "f"]["bot" "g"]["bot" "h"]["bot" "i"]["bot" "j"]["bot" "top"]}
                        ))

;(greedy-evaluate-non-isomorphic-lattices d3 "testing-data/non-isomorphic-lattices/unlabelled-09.cats")


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


;(def lat (concept-lattice (read-context "testing-data/triangles.cxt")))
;lat
;(count (incompatible-triples lat d1 false))

(def lat (concept-lattice (random-context #{1 2 3 4 5 6 7 8 9 10} 0.5)))
(greedy-syndrome d3 lat true)


(def counterexample (make-lattice #{0 7 1 4 15 13 6 3 12 2 11 9 5 14 16 10 8}
                                  #{[0 0]
  [0 7]
  [0 1]
  [0 15]
  [0 11]
  [0 14]
  [0 16]
  [0 8]
  [7 7]
  [7 15]
  [7 11]
  [7 14]
  [1 1]
  [1 11]
  [1 14]
  [1 8]
  [4 0]
  [4 7]
  [4 1]
  [4 4]
  [4 15]
  [4 13]
  [4 6]
  [4 3]
  [4 12]
  [4 2]
  [4 11]
  [4 9]
  [4 5]
  [4 14]
  [4 16]
  [4 10]
  [4 8]
  [15 15]
  [15 14]
  [13 15]
  [13 13]
  [13 6]
  [13 12]
  [13 5]
  [13 14]
  [13 16]
  [13 10]
  [13 8]
  [6 15]
  [6 6]
  [6 12]
  [6 14]
  [6 10]
  [3 7]
  [3 15]
  [3 6]
  [3 3]
  [3 12]
  [3 11]
  [3 9]
  [3 14]
  [3 10]
  [12 12]
  [12 14]
  [2 1]
  [2 12]
  [2 2]
  [2 11]
  [2 9]
  [2 5]
  [2 14]
  [2 8]
  [11 11]
  [11 14]
  [9 12]
  [9 11]
  [9 9]
  [9 14]
  [5 12]
  [5 5]
  [5 14]
  [5 8]
  [14 14]
  [16 15]
  [16 14]
  [16 16]
  [16 8]
  [10 15]
  [10 12]
  [10 14]
  [10 10]
  [8 14]
  [8 8]}))


(def ctx (make-context #{} #{"non-residual" "dystonic" "addorsed"} #{}))

(def ctx2 (make-context #{}  #{"d1" "d2" "d3" "d4" "strong d1" "strong d2"} #{}))
