(ns com.ilshad.ldf.turtle.grammar
  (:require [clojure.java.io :as io]))

(defmacro grammar []
  (slurp (io/resource "com/ilshad/ldf/turtle.bnf")))
