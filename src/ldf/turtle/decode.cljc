(ns ldf.turtle.decode
  (:require [ldf.turtle.parser :as parser]))

(defn decode-turtle [text opts]
  (->> (parser/parse text)
       (parser/transform opts)
       (filterv identity)))
