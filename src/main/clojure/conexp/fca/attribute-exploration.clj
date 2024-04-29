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
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def abort "abort")
(def return "return")
(def yes "yes")
(def no "no")


(defn verify-counterexample [ctx current-impl true-impls counterexample]
  "Returns false and prints explanatory text, if the counterexample is consistent with the current state of exploration.
   Returns true otherwise."
  (let [[obj attrs] counterexample]

    (if (contains? (objects ctx) obj)
      (println (str "\nThe context already contains an object named " obj "."))
      (if (not (subset? attrs (attributes ctx)))
        (println "\nThe new object contains unknown attributes.")

        (let [contradicted-impls (filter #(not (respects? attrs %)) true-impls )]
          (if (not (empty? contradicted-impls))
            (do (println "\nYour example does not respect the following confirmed implications:")
              (doseq [impl contradicted-impls]
                (println impl)))
            (if (respects? attrs current-impl)
              (println "\nYour example does not contradict the given implication.")
              true))))))
)


(defn ask-counterexample [ctx current-impl true-impls]
  "Repeatedly asks for a new object and its attributes, until a valid counterexample is input."
  (loop []
    (let [obj-response (str (ask "\nEnter the name of the new object:"
                                 #(read-string (str (read-line)))))]
      (cond 
        (= obj-response abort) abort
        (= obj-response return) return
        :else (let [attr-response (disj (into #{} (str/split (str (ask (str "\nEnter all attributes " 
                                                                         obj-response " is incident to:")
                                                                    #(str (read-line))))
                                                          #" "))
                                        "")];remove empty string to enable input of no attributes
                (cond 
                  (= attr-response #{abort}) abort
                  (= attr-response #{return}) return
                  (verify-counterexample ctx current-impl true-impls [obj-response attr-response]) [obj-response attr-response]
                  :else (recur))))))
)

(defn abort-exploration [ctx true-impls]
  "Prints abort messages."
  (println "\nThe Exploration has been aborted.")
  (println "This is the most recent state of the explored context:")
  (println ctx)
  (println "Confirmed Implications:")
  (println true-impls)
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

        (let [input (str (ask (str "\nDoes the implication " next-impl " hold? (yes/no)")
                              #(read-string (str (read-line)))))]
          (cond
            (= input abort) (abort-exploration current-ctx true-impls)

            (= input yes) (recur current-ctx
                                 (conj true-impls next-impl))

            (= input no) (let [response (ask-counterexample current-ctx next-impl true-impls)]
                           (cond
                             (= response abort) (abort-exploration current-ctx true-impls)
                             (= response return) (recur current-ctx true-impls)
                             :else (recur (make-context (conj (objects current-ctx) 
                                                              (first response))
                                                        (attributes current-ctx)
                                                        (set/union (incidence current-ctx) 
                                                                   (into #{} (for [attr (second response)] [(first response) attr]))))
                                          true-impls)))
            :else (do (println "\nInvalid Input.")
                      (recur current-ctx true-impls))
              )))

        [true-impls current-ctx])))
)

(defmulti explore (fn [input] (class input)))

(defmethod explore conexp.fca.contexts.Formal-Context [input] (explore-attributes input))
(defmethod explore clojure.lang.PersistentHashSet [input] (explore-attributes (make-context #{} input #{})))



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
