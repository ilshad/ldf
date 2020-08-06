(ns ldf.core-test
  (:require [ldf.core :as ldf]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def string= #(= (string/trim %1) (string/trim %2)))

(defn test-edn [] (edn/read-string (slurp "test/ldf/data.edn")))
(defn test-ttl [] (slurp "test/ldf/data.ttl"))

(def namespaces
  {""    "http://example.com/#"
   "rel" "http://www.perceive.net/schemas/relationship/"})

(def prefixes
  {""    "#"
   "rel" "http://www.perceive.net/schemas/relationship/"})

(def base "http://example.com/")

(defn encode-test-data []
  (ldf/encode (test-edn) {:namespaces namespaces
                          :prefixes prefixes
                          :base base}))

(defn decode-test-data []
  (ldf/decode (test-ttl) {:namespaces namespaces
                          :predicate-lists? false
                          :object-lists? false}))

(deftest encode-turtle-test
  (is (string= (encode-test-data) (test-ttl))))

(deftest decode-turtle-test
  (is (= (decode-test-data) (test-edn))))

(def opts-out
  {:namespaces {""     "http://example.com/#"
                "rel"  "http://www.perceive.net/schemas/relationship/"
                "foaf" "http://xmlns.com/foaf/0.1/"}
   :prefixes   {""     "#"
                "rel"  "http://www.perceive.net/schemas/relationship/"
                "foaf" "http://xmlns.com/foaf/0.1/"
                "xsd"  "http://www.w3.org/2001/XMLSchema#"}
   :base       "http://example.com/"})
