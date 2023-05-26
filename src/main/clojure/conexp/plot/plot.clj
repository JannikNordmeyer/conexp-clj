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


(defn plot-attribute [ctx attr]
  "Generates a JFreeChart Chart of the Values of attr."
  (let [dataset (DefaultCategoryDataset.)
        incidence  (incidence ctx)
        occurrences (filter #(= (get (first %) 1) attr) incidence)
        freq (frequencies (map #(get % 1) occurrences))]

        (doseq [x (seq freq)] (.addValue dataset (get x 1) (get x 0) attr))

        (ChartFactory/createBarChart (str "Attribute: " attr) 
                                     "Values"
                                     "Number of Occurrences" 
                                     dataset
                                     PlotOrientation/VERTICAL
                                     true true false)
))

(defn render-chart [chart]
  "Displays any JFreeChart Chart."
  (let [panel (ChartPanel. chart)
        frame (JFrame.)]

     (.add frame panel)
     (.pack frame)
     (.setVisible frame true)

   )
)

(def chart (plot-attribute ctx "Copies"))

(render-chart chart)




(def ctx (make-mv-context #{"Brainstorm" "Stoneforge Mystic" "Tundra" "Supreme Verdict"}
                          #{"Type" "CMC" "Copies"}
                          #{["Brainstorm" "Type" "Instant"] ["Brainstorm" "CMC" 1] ["Brainstorm" "Copies" 4]
                            ["Stoneforge Mystic" "Type" "Creature"] ["Stoneforge Mystic" "CMC" 2] ["Stoneforge Mystic" "Copies" 4]
                            ["Tundra" "Type" "Land"] ["Tundra" "CMC" 0] ["Tundra" "Copies" 2]
                            ["Supreme Verdict" "Type" "Sorcery"] ["Supreme Verdict" "CMC" 4] ["Supreme Verdict" "Copies" 1]}))


