;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.applications.election-results
  (:require [conexp.fca.contexts :refer :all]
            [conexp.fca.many-valued-contexts :refer :all]
            [clojure.data.json :as json]))

#(defn download-context [address]
  (let [answers-json (json/read-str (slurp (str "https://raw.githubusercontent.com/gockelhahn/qual-o-mat-data/master/data/" 
                                                address 
                                                "/answer.json")))
        party-json (json/read-str (slurp (str "https://raw.githubusercontent.com/gockelhahn/qual-o-mat-data/master/data/" 
                                              address 
                                              "/party.json")))
        statement-json (json/read-str (slurp (str "https://raw.githubusercontent.com/gockelhahn/qual-o-mat-data/master/data/" 
                                                  address 
                                                  "/statement.json")))
        opinion-json (json/read-str (slurp (str "https://raw.githubusercontent.com/gockelhahn/qual-o-mat-data/master/data/" 
                                                address 
                                                "/opinion.json")))
        answers (into {} (for [x answers-json] [(x "id") (x "message")]))
        party (into {} (for [x party-json] [(x "id") (x "name")]))
        statement (into {} (for [x statement-json] [(x "id") (x "label")]))
        incidence (for [x opinion-json] [(party (x "party")) (statement (x "statement")) (answers (x "answer"))])]

    (make-mv-context (vals party) (vals statement) incidence))

)

