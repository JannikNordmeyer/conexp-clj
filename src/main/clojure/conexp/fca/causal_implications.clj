(ns conexp.fca.causal-implications
  "Causal Implications for Formal Concept Analysis."
  (:require
   [conexp.base :refer :all]
   [conexp.io.contexts :refer :all]
   [conexp.fca.contexts :refer :all]
   [conexp.fca.implications :refer :all]
   [clojure.set :as set]))


(defn matched-record-pair? [ctx impl controlled-variables a b]
  "Returns true if a and b form a matched record pair in respect to the context and controlled variables,
   false otherwise."
  (let [premise (premise impl) 
        conclusion (conclusion impl)
        a-attributes (object-derivation ctx [a])
        b-attributes (object-derivation ctx [b])]

    ;check whether premise is present in exactly one of the objects
    (and (or (and (subset? premise a-attributes) (not (subset? premise b-attributes)))
             (and (subset? premise b-attributes) (not (subset? premise a-attributes))))
         ;check whether controlled variables have same realizations in both objects
         (subset? controlled-variables
                  (set/union (set/intersection a-attributes b-attributes)
                             (set/intersection (set/difference controlled-variables a-attributes) 
                                               (set/difference controlled-variables b-attributes))))))
)

(defn find-matched-record-pair [ctx impl controlled-variables objs-considered a]
  "Searches objs-considered for an object that forms a matched record pair with a,
   then returns a set containing a and that element. Returns the empty set, if no match is found."
  (let [objs (into [] objs-considered)]
  (if (= (count objs) 0)
           #{}
           (if (matched-record-pair? ctx impl controlled-variables a (first objs)) 
                 #{a (first objs)}
                 (find-matched-record-pair ctx impl controlled-variables (rest objs) a)))
  ))


(defn fair-data-set [ctx impl controlled-variables]
  "computes the fair data set of ctx in respect to impl by finding matched record pairs
   among the objects. Each object may only be matched with exactly one other object."
  (let [objs (objects ctx)]
    (filter seq 
            (reduce (fn[present-objs new-obj]
              (if (contains? (reduce set/union present-objs) new-obj)
                present-objs
                (conj present-objs (find-matched-record-pair 
                                           ctx 
                                           impl 
                                           controlled-variables 
                                           (set/difference objs (reduce set/union present-objs)) 
                                           new-obj))))
              #{} objs))
      ))

(defn fair-odds-ratio [ctx impl fair-data]
  "computes the odds ratio of an implication by dividing the number of matched pairs, 
   where the only the exposed element contains the conclusion by the number of matched pairs,
   where only the non-exposed object contains the conclusion. 
   (The divisor is capped at a minimum of 1)
   Only works on implications with single attributes as premise and conclusion."
  (let [premise-attr  (first (premise impl)), conclusion-attr (first (conclusion impl))]

    (/ (reduce +  (for [pair fair-data]
      (let [a  (first pair), b (first (rest pair))]
        (if (or (and (incident? ctx a premise-attr) 
                     (and (incident? ctx a conclusion-attr) 
                          (not (incident? ctx b conclusion-attr))))
                (and (incident? ctx b premise-attr) 
                     (and (incident? ctx b conclusion-attr) 
                          (not (incident? ctx a conclusion-attr)))))
        1
        0))))

   (max (reduce +  (for [pair fair-data]
      (let [a  (first pair), b (first (rest pair))]
        (if (or (and (incident? ctx a premise-attr) 
                     (and (not (incident? ctx a conclusion-attr)) 
                          (incident? ctx b conclusion-attr)))
                (and (incident? ctx b premise-attr) 
                     (and (not (incident? ctx b conclusion-attr)) 
                          (incident? ctx a conclusion-attr))))
        1
        0))))
        
        1)
    )))


(defn confidence-interval [ctx impl odds-ratio zconf]
  "Computes the confidence interval of the implication. odds-ratio is the regular odds ratio of
   the implication on its context, zconf is a standard normal deviate corresponding to the desired 
   level of confidence. (1.7 => 70% confidence)"
  (let [premise (premise impl) conclusion (conclusion impl)]
    [(Math/exp (+ (Math/log odds-ratio)
                  (* zconf (Math/sqrt (+ (/ 1 (imp/absolute-support ctx [(set/union premise conclusion) #{}]))
                                         (/ 1 (imp/absolute-support ctx [premise conclusion]))
                                         (/ 1 (imp/absolute-support ctx [conclusion premise])) 
                                         (/ 1 (imp/absolute-support ctx [#{} (set/union premise conclusion)])))))))
     (Math/exp (- (Math/log odds-ratio)
                  (* zconf (Math/sqrt (+ (/ 1 (imp/absolute-support ctx [(set/union premise conclusion) #{}]))
                                         (/ 1 (imp/absolute-support ctx [premise conclusion]))
                                         (/ 1 (imp/absolute-support ctx [conclusion premise])) 
                                         (/ 1 (imp/absolute-support ctx [#{} (set/union premise conclusion)])))))))] 
))

(defn fair-confidence-interval [ctx impl fair-odds-ratio fair-data zconf]
  "Computes the confidence interval of an implication on its fair data set.
   fair-odds-ratio is the implication's odds ratio on its fair data set,
   zconf is a standard normal deviate corresponding to the desired 
   level of confidence. (1.7 => 70% confidence)
   Only works on implications with single attributes as premise and conclusion."
  (let [premise-attr (first (premise impl)), conclusion-attr (first (conclusion impl))
        
        interval-component (* zconf (Math/sqrt (+ (/ 1 (max (reduce + 
                                                    (for [pair fair-data]
                                                    (let [a  (first pair), b (first (rest pair))]
                                                    (if (or (and (incident? ctx a premise-attr) 
                                                                 (and (incident? ctx a conclusion-attr) 
                                                                      (not (incident? ctx b conclusion-attr))))
                                                            (and (incident? ctx b premise-attr) 
                                                                 (and (incident? ctx b conclusion-attr) 
                                                                      (not (incident? ctx a conclusion-attr)))))
                                                     1
                                                     0))))
                                                  
                                                  1))
                                         (/ 1 (max (reduce +  
                                             (for [pair fair-data]
                                             (let [a  (first pair), b (first (rest pair))]
                                             (if (or (and (incident? ctx a premise-attr) 
                                                          (and (not (incident? ctx a conclusion-attr)) 
                                                               (incident? ctx b conclusion-attr)))
                                                     (and (incident? ctx b premise-attr) 
                                                          (and (not (incident? ctx b conclusion-attr)) 
                                                               (incident? ctx a conclusion-attr))))
                                              1
                                              0))))
                                              1))

                                        )))
        ]
    [(Math/exp (+ (Math/log fair-odds-ratio)
                  interval-component))
     (Math/exp (- (Math/log fair-odds-ratio)
                  interval-component))]
))


(defn relevant? [ctx variable response-variable zconfidence]
  "Computes whather a viariable is relevant to the response variable, by computing whether or not it
   is associated with the response variable.
   A variable p is associated with the response variable z, if the lower bound of the confidence interval of the 
   implication p -> z is greater than 1."
  (let [impl (->Implication #{variable} #{response-variable})]
    (> (last (confidence-interval ctx impl (odds-ratio ctx impl) zconfidence))
       1))
)

(defn irrelevant-variables [ctx vars response-variable zconfidence]
  "Returns a set that contains all variables in vars that are irrelevant to response-var."
  (set (filter #(not (relevant? ctx % response-variable zconfidence)) vars))
)

(defn exclusive-variables [ctx item-set thresh] 
  "Finds mutually exclusive variables to those in item-set. Returns tuples of the exclusive variables.
   Two variables a and b are mutually exclusive, if the absolute support for (a and b) or (b and not a) 
   is no larger than thresh."
  (set 
    (filter some? 
      (for [a item-set, b (attributes ctx)]
        (if (not (= a b))
          (if (or (<= (imp/absolute-support ctx [#{a b} #{}]) thresh) 
                  (<= (imp/absolute-support ctx [#{b} #{a}]) thresh))
            #{a b})))))
  )

(defn causal? [ctx impl irrelevant-vars zconf thresh]
  "Computes whether an implication is causal. An implication is causal, if the lower bound of its fair
   confidence interval is greather than 1." 
  (let [premise (premise impl) 
        conclusion (conclusion impl)
        E (reduce set/union (exclusive-variables ctx premise thresh)) 
        controlled-variables (set/difference (attributes ctx)  
                                             (set/union conclusion irrelevant-vars E premise))
        fair-data (fair-data-set ctx impl controlled-variables)
        fair-odds (fair-odds-ratio ctx impl fair-data)]

    (< 1 (last (fair-confidence-interval ctx impl fair-odds fair-data zconf)) )    
))

(defn generate-causal-rules [ctx premises response-var irrelevant-vars zconf thresh]
  "Generates all causal implications comprised of the premise in premises and the response variable.
   Returns only the premises of the causal implications."
  (filter #(causal? ctx (->Implication % #{response-var}) irrelevant-vars zconf thresh) premises)
  )

(defn find-redundant [ctx current-item-sets new-item-sets response-var]
  "Computes redundant rules by comparing the support of the premise to that of its subsets.
   If they have the same support, they cover the same objects, and the more specific rule redundant."
  (set (for [new new-item-sets, old current-item-sets]
    (if (and (subset? old new) 
             (= (imp/local-support ctx (->Implication  new #{response-var})) 
                (imp/local-support ctx (->Implication  old #{response-var}))))
     new  
))))

(defn causal-association-rule-discovery 
  ([ctx min-lsupp max-length response-var zconf]
   "Computes all causal implication rules with response-var as the conclusion. Returns only the premises of the causal 
    implications. Trivial implications are not considered."
   ;initial setup
   (let [frequent-vars (set (filter
                             #(> (imp/local-support ctx (->Implication #{%} #{response-var})) min-lsupp) 
                             (attributes ctx))) 
         ivars (irrelevant-variables ctx frequent-vars response-var zconf)]

      (causal-association-rule-discovery 
         ctx ;context
         #{} ;current causal rules
         (set/difference frequent-vars #{response-var})  ;frequent single variables
         (for [x (set/difference frequent-vars #{response-var})] #{x}) ;itemsets of the current iteration
         ivars ;irrelevant variables in respect to response-var
         min-lsupp ;minimum local support
         0 ;counter, counts up to max-length
         max-length ;maximum length of rules
         response-var ;response variable
         zconf ;confidence for significance test (1.7)
         )) 
 ) 
  
  ([ctx rule-set variables current ivars min-lsupp counter max-length response-var zconf]

    (if (= counter max-length)
      rule-set
      (let [new-causal-rules (generate-causal-rules ctx current response-var ivars zconf 1)
            new-item-sets (set (filter #(= (count %) (+ counter 2)) (for [c current, i variables] (conj c i))))]

      (causal-association-rule-discovery 
         ctx
         (set/union rule-set new-causal-rules)
         variables
         (set/difference (filter #(> (imp/local-support ctx (->Implication #{%} #{response-var})) min-lsupp) new-item-sets) 
                         (find-redundant ctx current new-item-sets response-var));filter item sets
         ivars
         min-lsupp
         (inc counter)
         max-length
         response-var
         zconf)
))))

