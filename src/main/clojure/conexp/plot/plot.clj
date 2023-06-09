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


(defn plot-attribute-distributions [ctx]
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

(defn plot-attribute-distribution [ctx attr]
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

(defn plot-attribute-subdivide [ctx attr segments]
  (assert (and (integer? segments) (< 1 segments)) "*Segments* needs to be an integer larger than 1.")
  (let [incidence  (incidence ctx)
         values (filter #(= (get (first %) 1) attr) incidence)
         num-values (for [v values] (read-string (str (get v 1))))
         min-value (apply min num-values)
         max-value (apply max num-values)]

        (plot-attribute-interval ctx attr (/ (- max-value min-value) segments))))

(defn plot-attribute-interval 
  ([ctx attr interval] 
   "Plots numeric attribute subdivided into specified interval."
   (assert (numeric? ctx attr) "Attribute appears to not be numeric.")
   (assert (< 0 interval) "Intervall needs to be a positive number.")
   (let [incidence  (incidence ctx)
         values (filter #(= (get (first %) 1) attr) incidence)
         num-values (for [v values] (read-string (str (get v 1))))
         min-value (apply min num-values)
         max-value (apply max num-values)
         dataset (DefaultCategoryDataset.)]

      (loop [n (- min-value interval)]
         (let [occurrence (count (filter #(and (< n %)(>= (+ n interval) %)) num-values))] 

            (.addValue dataset occurrence (str "(" n "-" (+ n interval) "]") attr)
            (if (< (+ n interval) max-value) (recur (+ n interval)))))

      (ChartFactory/createBarChart (str "Attribute: " attr) 
                                   "Values"
                                   "Number of Occurrences" 
                                   dataset
                                   PlotOrientation/VERTICAL
                                   true true false)))

  ([ctx attr interval order]
   "Plots non-numeric attribute subdivided into interval based on specified order."
   (assert (and (integer? interval) (< 0 interval)) "Intervall needs to be a positive interger.")
   (let [incidence  (incidence ctx)
         values (filter #(= (get (first %) 1) attr) incidence)
         str-values (for [v values] (get v 1))
         dataset (DefaultCategoryDataset.)]

      (assert (subset? (set str-values) (set order)) "Attribute contains values not in specified Order.")

      (loop [ n 0]
         (let [occurrence (count (filter #(and 
                                           (<= n (.indexOf order %))
                                           (<= (.indexOf order %) (+ n (- interval 1))))
                                         str-values))]

            (.addValue dataset occurrence (str (get order n) "-" (get order (+ n (- interval 1)))) attr)
            (if (< (+ n interval) (count order)) (recur (+ n interval)))))

      (ChartFactory/createBarChart (str "Attribute: " attr) 
                                   "Values"
                                   "Number of Occurrences" 
                                   dataset
                                   PlotOrientation/VERTICAL
                                   true true false))))


(defn generate-interval-scaling 

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

(generate-interval-scaling ctx "Copies" 2)
(generate-interval-scaling ctx "Mana Cost" 2 order)


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


