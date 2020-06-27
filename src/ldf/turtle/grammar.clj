(ns ldf.turtle.grammar
  (:require [clojure.java.io :as io]))

(defmacro grammar []
  (slurp (io/resource "turtle.bnf")))
