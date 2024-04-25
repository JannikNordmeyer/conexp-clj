;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.attribute-exploration
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications)
  (:require [clojure.set :as set]))


(defn verify-counterexample [ctx current-impl true-impls counterexample]
  (let [[obj attrs] counterexample]

    (if (not (subset? attrs (attributes ctx)))
      (println "The new object contains unknown attributes.")

      (let [contradicted-impls (filter #(not (respects? attrs %)) true-impls )]
      (if (not (empty? contradicted-impls))
        (do (println "Your example does not respect the following confirmed implications:")
            (doseq [impl contradicted-impls]
              (println impl)))
        (if (respects? attrs current-impl)
          (println "Your example does not contradict the given implication.")
          true)))))
)



(defn ask-counterexample [ctx current-impl true-impls]
  (loop []
    (let [object (str (ask "\nEnter the name of the new object:"
                           #(read-string (str (read-line)))))
          attributes (load-string (str (ask (str "\nEnter a collection of attributes " 
                                               object " is incident to:")
                                          #(read-string (str (read-line))))))]
      (if (verify-counterexample ctx current-impl true-impls [object attributes])
        [object attributes]
        (recur))))
)


(defn explore-attributes [ctx]

  (loop [current-ctx ctx
         true-impls #{}]

    (let [canonical-base (into #{} (canonical-base current-ctx))
          next-impl (first (set/difference canonical-base true-impls))]

      (if next-impl

        (do
        (println "\n\nCurrent Context:")
        (println current-ctx)
        (println "Currently confirmed Implications:")
        (println true-impls)

        (if (yes-or-no? (str "\nDoes the implication " next-impl " hold? (yes/no)"))
          (recur current-ctx
                 (conj true-impls next-impl))
          (let [[new-obj new-attrs] (ask-counterexample current-ctx next-impl true-impls)]

            (recur (make-context (conj (objects current-ctx) new-obj)
                                 (attributes current-ctx)
                                 (set/union (incidence current-ctx) (into #{} (for [attr new-attrs] [new-obj attr]))))
                   true-impls))))

        [true-impls current-ctx]
)


)
  )
)






(def V "Vertebrate")
(def C "Can Fly")
(def B "Beak")
(def E "Lays Eggs")
(def W "Warmblooded")
(def F "Feathers")

;(def ctx (make-context #{} #{"Vertebrate" "Sauropsid" "Bird" "Lays Eggs" "Warmblooded" "Feathers"} #{}))

(def ctx (make-context #{"Crow" "Cod"} #{"V" "C" "B" "E" "W" "F"} #{["Crow" "V"] ["Crow" "C"] ["Crow" "B"] ["Crow" "E"] ["crow" "W"] ["Crow" "F"]
                                                                    ["Cod" "V"] ["Cod" "E"]}))

(def ctx-explored (make-context #{"Crab" "Crocodile" "Patypus" "Tiger" "Velociraptor" "Cod" "Crow"}
                                #{"Vertebrate" "Sauropsid" "Bird" "Lays Eggs" "Warmblooded" "Feathers"}
                                #{["Crow" "Vertebrate"] ["Crow" "Sauropsid"] ["Crow" "Bird"] ["Crow" "Lays Eggs"] ["crow" "Warmblooded"] ["Crow" "Feathers"]
                                  ["Cod" "Vertebrate"] ["Cod" "Lays Eggs"]
                                  ["Crab" "Lays Eggs"]
                                  ["Crocodile" "Lays Eggs"] ["Crocodile" "Sauropsid"] ["Crocodile" "Vertebrate"]
                                  ["Patypus" "Lays Eggs"] ["Patypus" "Warmblooded"] ["Patypus" "Vertebrate"]
                                  ["Tiger" "Vertebrate"] ["Tiger" "Warmblooded"]
                                  ["Velociraptor" "Feathers"] ["Velociraptor" "Lays Eggs"] ["Velociraptor" "Sauropsid"] ["Velociraptor" "Vertebrate"]}))
