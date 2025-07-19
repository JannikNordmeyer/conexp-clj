;; Copyright â the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.lattices
  "Basis datastructure and definitions for abstract lattices."
  (:use conexp.base
        conexp.math.algebra
        conexp.fca.contexts
        conexp.fca.posets)
  (:require [clojure.set :refer [difference union subset? intersection]]
            [clojure.math :as math])
  (:gen-class))

;;; Datastructure


;utility
(defn minimal-node[nodes order-fn]
  "Returns a Minimal Element from a Set of Objects Given an Order Function."
  (reduce (fn [min-node node]
            (if (order-fn node min-node);Chooses a node, then updated the node whenever a lesser one is found
              node
              min-node))
          (first nodes)
          (rest nodes))
)


(defn order-function-to-set [base-set order-fn]
  "Accepts a Base Set and an Order Function and Returns an Explicit Set Representation of the Order Relation."
  (filter #(order-fn (first %) (second %)) (for [x base-set y base-set] [x y]))
)





(deftype Lattice [base-set order-function inf sup]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.base-set this) (.base-set ^Lattice other))
         (let [order-this (.order this),
               order-other (.order other)]
           (or (= order-this order-other)
               (forall [x (.base-set this)
                        y (.base-set this)]
                 (<=> (order-this x y)
                      (order-other x y)))))))
  (hashCode [this]
    (hash-combine-hash Lattice base-set))
  ;;
  Order
  (base-set [this] base-set)
  (order [this]
    (fn order-fn
      ([pair] (order-function (first pair) (second pair)))
      ([x y] (order-function x y)))))




(defn inf
  "Returns a function computing the infimum in lattice."
  [^Lattice lattice]
  (.inf lattice))

(defn sup
  "Returns a function computing the supremum in lattice."
  [^Lattice lattice]
  (.sup lattice))

(defn lattice-base-set
  "Alternative base-set function to importing conexp.math.algebra"
  [^Lattice lattice]
  (base-set lattice))

(defn lattice-order
  "Alternative order function to importing conexp.math.algebra"
  [^Lattice lattice]
  (order lattice))

(defmethod print-method Lattice [^Lattice lattice, ^java.io.Writer out]
  (.write out
          ^String (str "Lattice on " (count (base-set lattice)) " elements.")))


;;; Constructors

(defmulti make-lattice-nc
  "Creates a new lattice from the given arguments, without any
  checks. Use with care."
  {:arglists '([base-set order-function]
               [base-set inf sup]
               [base-set order-function inf sup])}
  (fn [& args] (vec (map clojure-type args))))

(defmethod make-lattice-nc [clojure-coll clojure-coll] [base-set order]
  (let [order (set order)]
    (make-lattice-nc base-set (fn [x y] (contains? order [x y])))))

(defmethod make-lattice-nc [clojure-coll clojure-fn] [base-set order]
  (let [inf (memoize (fn inf [x y]
                       (loop [elements base-set]
                         (let [z (first elements)]
                           (if (empty? elements)
                             nil
                             (if (and (order z x)
                                      (order z y)
                                      (forall [a base-set]
                                              (=> (and (order a x) (order a y))
                                                  (order a z))))
                               z
                               (recur (rest elements)))))))),
        sup (memoize (fn sup [x y]
                       (loop [elements base-set]
                         (if (empty? elements)
                           nil
                           (let [z (first elements)]
                             (if (and (order x z)
                                      (order y z)
                                      (forall [a base-set]
                                              (=> (and (order x a) (order y a))
                                                  (order z a))))
                               z
                               (recur (rest elements))))))))]
    (make-lattice-nc base-set order inf sup)))

(defmethod make-lattice-nc [clojure-coll clojure-fn clojure-fn] [base-set inf sup]
  (make-lattice-nc base-set
                   (fn [x y] (= y (sup x y)))
                   inf
                   sup))

(defmethod make-lattice-nc [clojure-coll clojure-fn clojure-fn clojure-fn]
  [base-set order inf sup]
  (Lattice. (set base-set) order inf sup))

(defmethod make-lattice-nc :default [& args]
  (illegal-argument "The arguments " args " are not valid for a Lattice."))

(defn has-lattice-order?
  "Given a lattice checks if its order is indeed a lattice order."
  [lat]
  (let [<= (order lat)]
    (and (forall [x (base-set lat)]
           (<= x x))
         (forall [x (base-set lat),
                  y (base-set lat)]
           (=> (and (<= x y)
                    (<= y x))
               (= x y)))
         (forall [x (base-set lat),
                  y (base-set lat),
                  z (base-set lat)]
           (=> (and (<= x y)
                    (<= y z))
               (<= x z)))
         (forall [x (base-set lat),
                  y (base-set lat)]
           (and (singleton? (for [z (base-set lat)
                                  :when (and (<= x z)
                                             (<= y z)
                                             (forall [w (base-set lat)]
                                               (=> (and (<= x w)
                                                        (<= y w))
                                                   (<= w z))))]
                              z))
                (singleton? (for [z (base-set lat)
                                  :when (and (<= z x)
                                             (<= z y)
                                             (forall [w (base-set lat)]
                                               (=> (and (<= w x)
                                                        (<= w y))
                                                   (<= w z))))]
                              z)))))))

(defn make-lattice
  "Standard constructor for makeing lattice. Call with two arguments
  [base-set order] to construct the lattice by its order
  relation (given as a set of pairs or as a function of two
  arguments). Call with three arguments [base-set inf sup] to
  construct the lattice by its algebraic operations.

  Note: This function will test the resulting lattice for being one,
  which may take some time. If you don't want this, use
  make-lattice-nc."
  [& args]
  (let [lattice (apply make-lattice-nc args)]
    (when-not (has-lattice-order? lattice)
      (illegal-argument "Given arguments do not describe a lattice."))
    lattice))





(defn fat? [plat x k]
  "Verifies Whether the Size of the Downset of a Node *x* is Greater than or Equal to *k*."
  (<= k (count (order-ideal plat #{x})))
)

(defn thin? [plat x k]
  "Verifies Whether the Size of the Downset of a Node *x* is smaller than *k*."
  (not (fat? plat x k))
)

(defn minimal-fat-node [plat k]
  "Returns a Fat Node from a Partial Lattice, that is Minimal by Lattice Order."
  (let [fat-nodes (filter #(fat? plat % k) (base-set plat))]
    (minimal-node fat-nodes (order plat)))
)



;############################################################

;testing statements
;(use 'conexp.io.contexts)
;(def ctx (read-context "testing-data/Brunson-Club.ctx"))
;(def lat (concept-lattice ctx))
;(def testlat (anonymize-lattice lat))
;(use 'conexp.gui.draw)
;(def blocks (block-decomposition testlat (clojure.math/floor (clojure.math/sqrt (count (base-set testlat))))))
;(def comps (make-comp-structure testlat))



(defn block-decomposition 
  "Returns a Block Decomposition of a Partial Lattice with Block Size *k*. *k* defaults
   to the square root of the lattice size.
   The decomposition is represented a a tuple, where the first entry is a dictionary
   where each block header maps to a set of the nodes in its block, and the second entry
   is the residual block."
  ([plat] 
    (block-decomposition plat (clojure.math/floor(clojure.math/sqrt (count (base-set plat))))))
   ([plat k]
     (loop [remaining (base-set plat)
            current-plat plat
            current-block-header (minimal-fat-node plat k)
            blocks []]
       (if current-block-header
         (let [new-remaining (clojure.set/difference remaining (order-ideal current-plat #{current-block-header}))
               new-plat (make-lattice-nc new-remaining (order plat))]
           (recur new-remaining
                  new-plat
                  (minimal-fat-node new-plat k)
                  (conj blocks [current-block-header (order-ideal current-plat #{current-block-header})])))
         [(into {} blocks) remaining];add residual block
)))
)


(defn find-block [block-decomposition node]
  "Returns the block that contains *node* as a tuple of its header and its nodes.
   Returns nil, if the node is in the residual block, or not in the lattice at all."
  (if (.contains (second block-decomposition) node)
    [nil (second block-decomposition)]
    (let [blocks (first block-decomposition)]
      (some identity (for [block blocks] (if (.contains (second block) node) block)))))
)

(defn local-downset [plat blocks x]
  (clojure.set/intersection (order-ideal plat #{x}) (second (find-block blocks x)))

)


;nodes inplemented as triple (index, display-name, block-header)


(defn make-comp-structure [lat]
  "Returns two dictionaries used for the order comparision operation:

  1. A dictionary that maps each block header *h* to a dictionary that maps each node *n* in the lattice to h∧n .
  2. A dictionary that maps each node in the lattice to its local downset.

  Consult section 5.1 

  3. A dictionary that maps each subblock header *h* to a dictionary that maps each node *n* in the block, that *h* is the header of, to h∧n.

  4. A dictionary that maps each subblock header *h* to a dictionary that maps each pair of nodes in the subblock to their meet if it lies
     within the same subblock, *nil* otherwise. UNTESTED

  5. A dictionary taht maps element *x*, that is in a residual subblock, to its local downset in its respective subblock. UNTESTED"
  (let [meet (inf lat)
        [principal-blocks residual-block] (block-decomposition lat)
       ; principal-subblocks (apply concat (for [block principal-blocks] (first (block-decomposition (make-lattice-nc (second block) (order lat))))))
        ]

    [(into {} (for [header (keys principal-blocks)] [header (into {} (for [node (base-set lat)] [node (meet header node)]))]))

     (into {} (for [node (base-set lat)] [node (local-downset lat [principal-blocks residual-block] node)]))

     (into {} (for [block principal-blocks 
                    principal-subblock (first (block-decomposition (make-lattice-nc (second block) (order lat))))]

                   [(first principal-subblock) 
                    (into {} (for [node (second block)] [node (meet (first principal-subblock) node)]))]))

     (into {} (for [block principal-blocks 
                    principal-subblock (first (block-decomposition (make-lattice-nc (second block) (order lat))))]

                   [(first principal-subblock) 
                    (into {} (for [node1 (second principal-subblock) 
                                   node2 (second principal-subblock)] [[node1 node2] 
                                                                       ((inf (make-lattice-nc (second principal-subblock) (order lat))) node1 node2)]))]))

     (into {} (for [block principal-blocks]
                (let [subblock-decomposition (block-decomposition (make-lattice-nc (second block) (order lat)))
                      residual-subblock (second subblock-decomposition)]
                  (for [node residual-subblock] 
                    [node (clojure.set/intersection (order-ideal lat node) residual-subblock)]))))])
)



(defn partial-lattice-compare [plat comps blocks x y]
  "Computes the order comparison operation using the data structure produced by *make-comp-structure*.
   Consult section 5.2"

  (let [[h_i B_i] (find-block blocks x)
        [h_i' _] (find-block blocks y)
        [meets local-downsets] comps]


    (if h_i
      (let [y_i (get (get meets h_i) y)]
        (if (.contains B_i y_i)
          (if (.contains (get local-downsets y_i) x) true
                                                     false)
          false))

      (if (and (not h_i) (not h_i'))
        (if (.contains (get local-downsets y) x) true
                                                 false)
        (if (and (not h_i) h_i') false
                                 nil))))
)







(defn dual-lattice
  "Dualizes given lattice lat."
  [lat]
  (let [order (order lat)]
    (make-lattice-nc (base-set lat) (fn [x y] (order y x)))))

(defn distributive?
  "Checks (primitively) whether given lattice lat is distributive or not."
  [lat]
  (let [inf  (inf lat),
        sup  (sup lat),
        base (base-set lat)]
    (forall [x base,
             y base,
             z base]
      (= (sup x (inf y z))
         (inf (sup x y) (sup x z))))))

(defn modular?
  "Checks (primitively) whether given lattice lat is modular or not."
  [lat]
  (let [inf  (inf lat),
        sup  (sup lat),
        base (base-set lat),
        ordr (order lat)]
    (forall [x base,
             y base,
             z base]
      (=> (ordr x z)
          (= (sup x (inf y z))
             (inf (sup x y) z))))))

(defn lattice-one
  "Returns the one element of lattice lat."
  [lat]
  (when (empty? (base-set lat))
    (illegal-argument "The lattice is empty and therefore cannot have a maximal element."))
  (let [order (order lat)]
    (reduce (fn [x a]
              (if (order x a) a x))
            (base-set lat))))

(defn lattice-zero
  "Returns the zero element of lattice lat."
  [lat]
  (when (empty? (base-set lat))
    (illegal-argument "The lattice is empty and therefore cannot have a minimal element."))
  (let [order (order lat)]
    (reduce (fn [x a]
              (if (order a x) a x))
            (base-set lat))))

(defn lattice-upper-neighbours
  "Returns all direct upper neighbours of x in lattice lat."
  [lat x]
  (poset-upper-neighbours lat x))

(defn lattice-lower-neighbours
  "Returns all direct lower neighbours of y in lattice lat."
  [lat y]
  (poset-lower-neighbours lat y))

(defn lattice-atoms
  "Returns the lattice atoms of lat."
  [lat]
  (lattice-upper-neighbours lat (lattice-zero lat)))

(defn lattice-coatoms
  "Returns the lattice coatoms of lat."
  [lat]
  (lattice-lower-neighbours lat (lattice-one lat)))

(defn lattice-sup-irreducibles
  "Returns the sup-irreducible elements of lattice lat."
  [lat]
  (set-of y [y (base-set lat)
             :when (= 1 (count (lattice-lower-neighbours lat y)))]))

(defn lattice-inf-irreducibles
  "Returns the inf-irreducible elements of lattice lat."
  [lat]
  (set-of x [x (base-set lat)
             :when (= 1 (count (lattice-upper-neighbours lat x)))]))

(defn lattice-doubly-irreducibles
  "Returns all (i.e. sup and inf) irreducible elements of lattice lat."
  [lat]
  (intersection (lattice-sup-irreducibles lat)
                (lattice-inf-irreducibles lat)))


;;; FCA

(defn concept-lattice
  "Returns for a given context ctx its concept lattice."
  [ctx]
  (make-lattice-nc (set (concepts ctx))
                   (fn <= [[A _] [C _]]
                     (subset? A C))
                   (fn inf [[A _] [C _]]
                     (let [A+C (intersection A C)]
                       [A+C (object-derivation ctx A+C)]))
                   (fn sup [[_ B] [_ D]]
                     (let [B+D (intersection B D)]
                       [(attribute-derivation ctx B+D) B+D]))))

(defn standard-context
  "Returns the standard context of lattice lat."
  [lat]
  (make-context (lattice-sup-irreducibles lat)
                (lattice-inf-irreducibles lat)
                (fn [x y]
                  ((order lat) [x y]))))

(defmethod poset-context Lattice
  [lattice]
  (standard-context lattice))

(defn extract-context-from-bv
  "Extracts the objects, attributes and incidence of a concept
  lattice."
  [lat]
  (let [c (base-set lat)]
    (assert (every? (fn [[a b]] (and (set? a) (set? b))) c) "Lattice is not a concept lattice")
    (let [objects (apply max-key count (map first c))
          attributes (apply max-key count (map second c))
          i-fn (fn [a b] 
                 (some (fn [[ext int]] (and (contains? ext a)
                                           (contains? int b)))
                        c))]
      (make-context objects attributes i-fn))))


;;; TITANIC Implementation

(defn- minimum
  "Computes the minimum of weights in a given sequence of
  weights. order must be a linear order on elements plus max-val and
  max-val must be the maximal element to be considered."
  [order max-val elements]
  (loop [current-min max-val,
         elements    elements]
    (if (empty? elements)
      current-min
      (recur (if (order (first elements) current-min)
               (first elements)
               current-min)
             (rest elements)))))

(defn- titanic-closure
  "Computes the closure of X by the weights of its subsets. The
  following restrictions apply:

    - keys is the sequence of pairs of already computed key-sets
      together with their weights, ordered by their weights,
    - X is a member of keys.

  "
  [X X-weight base-set keys set-weight closure]
  (let [keys  (doall (map first (take-while #(not= (second %) X-weight) keys))),
        start (reduce #(union %1 (closure (disj X %2)))
                      X X)]
    (loop [Y        (transient start),
           elements (difference base-set start)]
      (if-not (empty? elements)
        (let [m    (first elements),
              X+m  (conj X m),
              add? (if-let [s (set-weight X+m)]
                     (= s X-weight)
                     (not (exists [K keys]
                            (and (contains? K m) (subset? K X+m)))))]
          (recur (if add? (conj! Y m) Y)
                 (rest elements)))
        (persistent! Y)))))

(defn- titanic-generate
  "Computes the next candidate set."
  [base-set key-set]
  (set-of next [A key-set,
                m (difference base-set A),
                :let [next (conj A m)]
                :when (forall [x A]
                        (contains? key-set (disj next x)))]))

(defn titanic
  "Implements the titanic algorithm. weigh must return for a
  collection of sets a hash-map mapping every set to its weight, with
  max-weight being the maximal weight possible. weight-order denotes
  the order on the weights. Note that this order has to be linear for
  this implementation to work."
  [base-set weigh max-weight weight-order]
  (loop [closure    {},
         set-weight (weigh [#{}]),
         key-set    #{[#{}, (set-weight #{})]}, ;keys are pairs of sets and weights here
         keys       key-set]
    (let [candidates   (titanic-generate base-set (set-of (first k) [k key-set])),
          set-weight   (into set-weight (weigh candidates)),
          closure      (into closure (map (fn [[X, X-weight]]
                                            [X (titanic-closure X X-weight base-set keys set-weight closure)])
                                          key-set)),
          next-key-set (set-of [X w] [X candidates,
                                      :let [w (set-weight X)]
                                      :when (not= w
                                                  ;;(subset-weight X)
                                                  (minimum weight-order max-weight
                                                           (map #(set-weight (disj X %)) X)))])]
      (if (seq next-key-set)            ;i.e. (not (empty? next-key-set))
        (recur closure,
               set-weight,
               next-key-set,
               (sort #(weight-order (second %1) (second %2))
                     (concat next-key-set keys)))
        (distinct (vals closure))))))

(defn titanic-keys
  "Nearly the same as titanic, but returns the key sets only."
  [base-set weigh max-weight weight-order]
  (loop [set-weight (weigh [#{}]),
         key-set    #{#{}},
         keys       [#{}]]
    (let [candidates   (titanic-generate base-set key-set),
          set-weight   (into set-weight (weigh candidates)),
          next-key-set (set-of X [X candidates,
                                  :when (not= (set-weight X)
                                              ;;(subset-weight X)
                                              (minimum weight-order max-weight
                                                       (map #(set-weight (disj X %)) X)))])]
      (if-not (empty? next-key-set)
        (recur set-weight
               next-key-set
               (concat next-key-set keys))
        keys))))

;;; Common use cases

(defn supports
  "Returns a function of one argument being a collection of sets of attributes,
  returning a hash-map from those sets to their supports in the given
  context, if they have at least minsupp elements. Otherwise they are
  associated with -1."
  [context minsupp]
  (let [num-of-objects (count (objects context)),
        minnum         (* minsupp num-of-objects),
        obj-deriv      (map-by-fn #(object-derivation context #{%}) (objects context))]
    (fn [att-sets]
      (map-by-fn
       (fn [att-set]
         (let [obj-count (count (filter #(subset? att-set (obj-deriv %))
                                        (objects context)))]
           (if (<= minnum obj-count)
             obj-count
             -1)))
       att-sets))))

(defn titanic-intents
  "Computes the intents of the given context via TITANIC."
  [context]
  (titanic (attributes context)
           (supports context 0)
           1.0
           <))

(defn titanic-iceberg-intent-seq
  "Computes the iceberg intent seq for given context and minimal
  support minsupp via TITANIC."
  [context minsupp]
  (let [intents (titanic (attributes context)
                         (supports context minsupp)
                         1.0
                         <)]
    (if (<= (* (count (objects context)) minsupp)
            (count (attribute-derivation context (attributes context))))
      intents
      (remove #{(attributes context)} intents))))

(defn iceberg-lattice
  "Returns for a given context ctx its iceberg lattice."
  ([ctx]
    (iceberg-lattice ctx 0))
  ([ctx minsupp]
    (let [intents  (titanic-iceberg-intent-seq ctx minsupp)
          concepts (map
                     #(vector (attribute-derivation ctx %) %)
                     intents)]
      (make-lattice-nc concepts
                       (fn <= [[A _] [C _]]
                         (subset? A C))
                       (fn inf [[A _] [C _]]
                         (let [A+C (intersection A C)]
                           [A+C (object-derivation ctx A+C)]))
                       (fn sup [[_ B] [_ D]]
                         (let [B+D (intersection B D)]
                           [(attribute-derivation ctx B+D) B+D]))))))

(defn generated-sublattice [lat generators]
  "Computes the sublattice of the specified lattice with the specified set of generators."
  (let [lat-join (sup lat)
        lat-meet (inf lat)]
    (loop [X generators]
      (let [X-new (union (into #{} (for [a X b X] (lat-join a b)))
                                     (into #{} (for [a X b X] (lat-meet a b))))]
        (if (= X X-new) (make-lattice X lat-meet lat-join)
                        (recur X-new)))))
)



(defn anonymize-lattice [lat]
  "Returns a Lattice with the same Order Relation is the Input, but with the Names of 
   the Nodes Replaced by Numeric Identifiers."
  (let [nodes (into [] (base-set lat))
        ord (order lat)]
    (make-lattice (range (count (base-set lat))) #(ord (get nodes %1) (get nodes %2))))
)

;test function:
(use 'conexp.io.contexts)
;"testing-data/Brunson-Club.ctx"
(defn test-comparisons [ctx-path]
  (let [ctx (read-context ctx-path)
        lat (concept-lattice ctx)
        testlat (anonymize-lattice lat)
        blocks (block-decomposition testlat)
        comps (make-comp-structure testlat)
        ord (order testlat)]
    (every? identity (for [x (base-set testlat) y (base-set testlat)] (= (ord x y) (partial-lattice-compare lat comps blocks x y)))))
)


nil


