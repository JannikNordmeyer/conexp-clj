(ns conexp.plot.plot
  "Plotting Functions for Visualization."
  (:require
   [conexp.base :refer :all]
   [conexp.fca.many-valued-contexts :refer :all])
  (:import
    [java.util Date]
    [org.jfree.chart ChartFactory ChartUtils JFreeChart]
    [org.jfree.chart.plot    PlotOrientation]
    [org.jfree.data.category CategoryDataset DefaultCategoryDataset]
    [org.jfree.data.general  PieDataset DefaultPieDataset]
    [org.jfree.data.time     RegularTimePeriod Millisecond Second Minute Hour Day Week Month Quarter Year
                             TimeSeries TimeSeriesCollection]
    [org.jfree.data.xy       XYDataset]))

(def ctx (make-mv-context #{"Brainstorm" "Stoneforge Mystic" "Tundra" "Supreme Verdict"}
                          #{"Type" "CMC" "Copies"}
                          #{["Brainstorm" "Type" "Instant"] ["Brainstorm" "CMC" 1] ["Brainstorm" "Copies" 4]
                            ["Stoneforge Mystic" "Type" "Creature"] ["Stoneforge Mystic" "CMC" 2] ["Stoneforge Mystic" "Copies" 4]
                            ["Tundra" "Type" "Land"] ["Tundra" "CMC" 0] ["Tundra" "Copies" 2]
                            ["Supreme Verdict" "Type" "Sorcery"] ["Supreme Verdict" "CMC" 4] ["Supreme Verdict" "Copies" 1]}))


