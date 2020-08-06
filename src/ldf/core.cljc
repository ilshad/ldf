(ns ldf.core
  (:require [ldf.turtle.encode :refer [encode-turtle]]
            [ldf.turtle.parser :refer [decode-turtle]]
            [ldf.prefix :as prefix]))

(defn make-opts [opts]
  (-> (or opts {})
      (update :namespaces (partial merge prefix/default-namespaces))
      (update :format     #(or % :turtle))))

(defn- encode* [data opts]
  (case (:format opts)
    :turtle (encode-turtle data opts)))

(defn- decode* [string opts]
  (case (:format opts)
    :turtle (decode-turtle string opts)))

(defn encode [data & [opts]]
  (encode* data (make-opts opts)))

(defn decode [string & [opts]]
  (decode* string (make-opts opts)))
