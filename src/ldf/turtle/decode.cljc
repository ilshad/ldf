(ns ldf.turtle.decode
  (:require [ldf.turtle.parser :as parser]))

(defn decode-turtle [text opts]
  #_(let [[_ & tree] (parser/parse text)]
    (reduce-tree tree {:opts opts :prefixes {}})))
