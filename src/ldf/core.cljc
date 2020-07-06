(ns ldf.core
  (:require [clojure.spec.alpha :as s]
            [ldf.turtle.encode :refer [encode-turtle]]
            [ldf.turtle.decode :refer [decode-turtle]]
            [ldf.prefixes :as prefixes]
            [ldf.spec :as spec]))

(defn make-opts [opts]
  (-> (or opts {})
      (update :prefixes  (partial merge prefixes/defaults))
      (update :prefixes? #(if (nil? %) true %))
      (update :format    #(or % :turtle))))

(defn- conform [statements]
  (let [data (s/conform ::spec/statements statements)]
    (if (= data ::s/invalid)
      (throw (ex-info "Invalid" (s/explain-data ::spec/statements statements)))
      data)))

(defn- encode* [statements opts]
  (case (:format opts)
    :turtle (encode-turtle statements opts)))

(defn- decode* [text opts]
  (case (:format opts)
    :turtle (decode-turtle text opts)))

(defn encode [statements & [opts]]
  (encode* (conform statements) (make-opts opts)))

(defn decode [text & [opts]]
  (decode* text (make-opts opts)))
