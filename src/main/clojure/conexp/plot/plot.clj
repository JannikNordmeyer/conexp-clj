(ns conexp.plot.plot
  "Plotting Functions for Visualization."
  (:require
   [conexp.base :refer :all]
   [conexp.fca.many-valued-contexts :refer :all]
   [conexp.io.many-valued-contexts :refer :all]
   [conexp.io.contexts :refer :all]
   [conexp.io.fcas :refer :all]
   [conexp.fca.contexts :refer :all])
  (:import
    [java.util Date]
    [org.jfree.chart ChartFactory ChartUtils JFreeChart ChartPanel]
    [org.jfree.chart.plot    PlotOrientation]
    [org.jfree.data.category CategoryDataset DefaultCategoryDataset]
    [org.jfree.data.general  PieDataset DefaultPieDataset]
    [org.jfree.data.time     RegularTimePeriod Millisecond Second Minute Hour Day Week Month Quarter Year
                             TimeSeries TimeSeriesCollection]
    [org.jfree.data.xy       XYDataset]
    [javax.swing JFrame]))


(defn plot-all-attribute-values [ctx]
  "Generates a Chart of the Values of all Attributes of the Context"
  (let [dataset (DefaultCategoryDataset.)
        incidence  (incidence ctx)
        attributes (attributes ctx)]

        (doseq [attr attributes]
           (let [values (filter #(= (get (first %) 1) attr) incidence)
                 freq (frequencies (map #(get % 1) values))]
               
                    (doseq [x (seq freq)] (.addValue dataset (get x 1) (get x 0) attr))))

                (ChartFactory/createBarChart (str "Attributes") 
                                             "Values"
                                             "Number of Occurrences" 
                                             dataset
                                             PlotOrientation/VERTICAL
                                             true true false)))

(defn plot-attribute-values [ctx attr]
  "Generates a Chart of the Values of attr."
  (let [dataset (DefaultCategoryDataset.)
        incidence  (incidence ctx)
        values (filter #(= (get (first %) 1) attr) incidence)
        freq (frequencies (map #(get % 1) values))]
        (doseq [x (seq freq)] (.addValue dataset (get x 1) (get x 0) attr))

        (ChartFactory/createBarChart (str "Attribute: " attr) 
                                     "Values"
                                     "Number of Occurrences" 
                                     dataset
                                     PlotOrientation/VERTICAL
                                     true true false)))

(defn render-chart [chart]
  "Displays any JFreeChart Chart."
  (let [panel (ChartPanel. chart)
        frame (JFrame.)]

     (.add frame panel)
     (.pack frame)
     (.setVisible frame true)))

(defn numeric? [ctx attr]
  "Verifies whether every value of attr is a number or can be converted to a number."
  (let [incidence  (incidence ctx)
        values (filter #(= (get (first %) 1) attr) incidence)]
       (every? true? (for [value values] (number? (read-string (str (get  value 1))))))))


(defn plot-attribute-value-intervals [ctx attr & {:keys [intervals num_intervals order] :as opts}]
  "Creates a Plot of the Context's Attributed Subdivided into Intervals.
   Resolves the Combination of Arguments and Calls plot-attribute-interval Accordingly."

  ;Forward supplied Intervals
  (if (:intervals opts) 
     (if (:order opts) 
        (plot-attribute-interval ctx attr (:intervals opts) (:order opts))
        (plot-attribute-interval ctx attr (:intervals opts)))
  
  ;Compute Intervals from supplied Interval Number
  (if (:num_intervals opts)
     (if (:order opts)

          (let [step (Math/ceil (/ (count order) (:num_intervals opts)))
                intervals (partition-all step order)]
            (println step)

          (plot-attribute-interval ctx attr intervals order))




          (let [incidence  (incidence ctx)
                values (filter #(= (get (first %) 1) attr) incidence)
                num-values (for [v values] (read-string (str (get v 1))))
                min-value (apply min num-values)
                max-value (apply max num-values)
                step (/ (- max-value min-value) (:num_intervals opts))
                intervals (for [x (range min-value max-value step)] [x (+ x step)] )]

          (plot-attribute-interval ctx attr intervals)))))
)

;(assert (and (integer? (:num_intervals opts)) (< 0 (:num_interval opts))) "Intervall needs to be a positive interger.")


;(render-chart (plot-attribute-value-intervals ctx "Copies" :intervals [[1 2] [1 3] [2 4]]))
;(render-chart (plot-attribute-value-intervals ctx "Copies" :num_intervals 3))

;(render-chart (plot-attribute-value-intervals ctx "Mana Cost" :intervals [["-" "W"]["1W" "1WWU"]["5" "3UU"]] :order order))
;(render-chart (plot-attribute-value-intervals ctx "Mana Cost" :num_intervals 3 :order order))

(defn plot-attribute-interval 
  ([ctx attr intervals] 
   "Creates a Plot of the Provided Attribute with Provided Intervals.
    Attribute must be numeric."
   (let [incidence  (incidence ctx)
         values (filter #(= (get (first %) 1) attr) incidence)
         num-values (for [v values] (read-string (str (get v 1))))
         dataset (DefaultCategoryDataset.)]

      (doseq [interval intervals] 
            (let [occurrences (if (= interval (last intervals))
                                 ;last interval is right closed
                                 (count (filter #(and (<= (first interval) %)(>= (last interval) %)) num-values)) 
                                 (count (filter #(and (<= (first interval) %)(> (last interval) %)) num-values)))]
               
               (if (= interval (last intervals))
                      (.addValue dataset occurrences (str "[" (first interval) "-" (last interval) "]") attr)
                      (.addValue dataset occurrences (str "[" (first interval) "-" (last interval) ")") attr))
              
      ))

      (ChartFactory/createBarChart (str "Attribute: " attr) 
                                   "Values"
                                   "Number of Occurrences" 
                                   dataset
                                   PlotOrientation/VERTICAL
                                   true true false)))

  ([ctx attr intervals order]
   "Creates a Plot of the Provided Attribute with Provided Intervals.
    Attribute must be non-numeric Attributes. Order needs to be Provided."
   (let [incidence  (incidence ctx)
         values (filter #(= (get (first %) 1) attr) incidence)
         str-values (for [v values] (get v 1))
         dataset (DefaultCategoryDataset.)]

      (assert (subset? (set str-values) (set order)) "Attribute contains values not in specified Order.")

      (doseq [interval intervals] 
             (let [ occurrences (count (filter #(and 
                                                  (<= (.indexOf order (first interval)) (.indexOf order %))
                                                  (<= (.indexOf order %) (.indexOf order (last interval))))
                                                str-values))]
               (.addValue dataset occurrences (str (first interval) "-" (last interval)) attr)))
                


      (ChartFactory/createBarChart (str "Attribute: " attr) 
                                   "Values"
                                   "Number of Occurrences" 
                                   dataset
                                   PlotOrientation/VERTICAL
                                   true true false)))
)

(defn generate-interval-scale

  ([ctx attr interval]
   "Generates context assigning each object to an attribute representing a range of lenth *interval*."
   (let [incidence  (incidence ctx)
         values (filter #(= (get (first %) 1) attr) incidence)
         num-values (for [v values] (read-string (str (get v 1))))
         min-value (apply min num-values)
         max-value (apply max num-values)

         ;Generate Ranges as Attributes
         attributes (for [x (range (- min-value interval) max-value interval)]
                     (str "(" x "-" (+ x interval) "]"))
         objects (distinct num-values)
         ;Assign Objects to Ranges/Attributes
         incidence (filter identity (for [x (range (- min-value interval) max-value interval)
                                         o objects]

                                      (if (and (< x o) ( <= o ( + x interval)))
                                           [o (str "(" x "-" (+ x interval) "]")])))]

   (make-context (set objects) (set attributes) (set incidence))))

  ([ctx attr interval order]
   "Generates a context asigning each object to a range containing *interval* objects."
   (let [incidence  (incidence ctx)
         values (filter #(= (get (first %) 1) attr) incidence)
         str-values (for [v values] (get v 1))

         ;Generate Ranges as Attributes
         attributes (for [x (range 0 (count order) interval)] (str (get order x) "-" (get order (- (+ x interval) 1))))
         objects order
         ;Assign Objects to Ranges/Attributes
         incidence (for [x (range 0 (count objects))] [(get objects x) (nth attributes (Math/floor (/ x interval)))])
         ]

   (make-context (set objects) (set attributes) (set incidence))))
)

(generate-interval-scale ctx "Copies" 2)
(generate-interval-scale ctx "Mana Cost" 2 order)


(def chart (plot-attribute-distribution ctx "Copies"))
(render-chart chart)

(def chart2 (plot-attribute-distributions ctx))
(render-chart chart2)

(def chart3 (plot-attribute-subdivide ctx "Copies" 3))
(render-chart chart3)

(def ichart (plot-attribute-interval ctx "CMC" 2))
(render-chart ichart)

(def order ["-" "U" "W" "1W" "1WWU" "5" "3UU"])

(def ichart2 (plot-attribute-interval ctx "Mana Cost" 2 order))
(render-chart ichart2)


(def ctx (make-mv-context #{"Brainstorm" "Swords to Plowshares" "Stoneforge Mystic" "Tundra" "Supreme Verdict" "Batterskull" "Force of Will"}
                             #{"Type" "CMC" "Copies" "Mana Cost"}
                             #{["Brainstorm" "Type" "Instant"] ["Brainstorm" "CMC" 1] ["Brainstorm" "Copies" "4"] ["Brainstorm" "Mana Cost" "U"]
                               ["Swords to Plowshares" "Type" "Instant"] ["Swords to Plowshares" "CMC" 1] ["Swords to Plowshares" "Copies" "4"] ["Swords to Plowshares" "Mana Cost" "W"]
                               ["Stoneforge Mystic" "Type" "Creature"] ["Stoneforge Mystic" "CMC" 2] ["Stoneforge Mystic" "Copies" "4"] ["Stoneforge Mystic" "Mana Cost" "1W"]
                               ["Tundra" "Type" "Land"] ["Tundra" "CMC" 0] ["Tundra" "Copies" "2"] ["Tundra" "Mana Cost" "-"]
                               ["Supreme Verdict" "Type" "Sorcery"] ["Supreme Verdict" "CMC" 4] ["Supreme Verdict" "Copies" "1"] ["Supreme Verdict" "Mana Cost" "1WWU"]
                               ["Batterskull" "Type" "Artifact"] ["Batterskull" "CMC" 5] ["Batterskull" "Copies" "1"] ["Batterskull" "Mana Cost" "5"]
                               ["Force of Will" "Type" "Instant"] ["Force of Will" "CMC" 5] ["Force of Will" "Copies" "4"] ["Force of Will" "Mana Cost" "3UU"]}))


