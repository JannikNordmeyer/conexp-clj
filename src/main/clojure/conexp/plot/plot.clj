(ns plot
   (:require 
    [libpython-clj.python :as py]
    [tech.v2.datatype :as dtype]
    [tech.v2.tensor :as dtt]
    [clojure.java.io :as io]
    [java.awt.image BufferedImage]
    [javax.imageio ImageIO]))



(require '[libpython-clj.python :as py])
(require '[tech.v2.datatype :as dtype])
(require '[tech.v2.tensor :as dtt])
(require '[clojure.java.io :as io])
(import '[java.awt.image BufferedImage])
(import '[javax.imageio ImageIO])



(ns conexp.fca.causal-implications
  "Causal Implications for Formal Concept Analysis."
  (:require
   [conexp.base :refer :all]
   [conexp.io.contexts :refer :all]
   [conexp.io.fcas :refer :all]
   [conexp.fca.contexts :refer :all]
   [clojure.set :as set]))


;;Uncomment this line to load a different version of your python shared library:
;;(alter-var-root #'libpython-clj.jna.base/*python-library* (constantly "python3.7m"))


(py/initialize!)
