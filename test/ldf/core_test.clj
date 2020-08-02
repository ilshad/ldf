(ns ldf.core-test
  (:require [ldf.core :as ldf]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def string= #(= (string/trim %1) (string/trim %2)))

(defn edn-data []
  (edn/read-string (slurp "test/ldf/data.edn")))

(defn ttl-data []
  (slurp "test/ldf/data.ttl"))

(def example-prefixes
  {:_   "http://example.com/"
   :rel "http://www.perceive.net/schemas/relationship/"})

(defn encode-test-data []
  (ldf/encode (edn-data) {:prefixes example-prefixes
                          :base     "http://example.com/"}))

(def namespaces
  {""       "http://example.com/#"
   "rel"    "http://www.perceive.net/schemas/relationship/"
   "schema" "http://schema.org/"})

(defn decode-test-data []
  (ldf/decode (ttl-data) {:namespaces namespaces
                          :predicate-lists? false
                          :object-lists? false}))

(deftest encode-turtle-test
  (is (string= (encode-test-data) (ttl-data))))

(deftest decode-turtle-test
  )
