(ns com.ilshad.ldf-test
  (:require [com.ilshad.ldf :as ldf]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.edn :as edn]))

;;
;; Utils
;;

(def string= #(= (string/trim %1) (string/trim %2)))

;;
;; Test data
;;

(defn data-edn [] (edn/read-string (slurp "test/com/ilshad/data.edn")))
(defn data-in-ttl [] (slurp "test/com/ilshad/data_in.ttl"))
(defn data-out-ttl [] (slurp "test/com/ilshad/data_out.ttl"))

;;
;; Encode
;;

(def encode-opts
  {:base       "http://example.com/"
   :prefixes   {""     "#"
                "rel"  "http://www.perceive.net/schemas/relationship/"
                "foaf" "http://xmlns.com/foaf/0.1/"
                "xsd"  "http://www.w3.org/2001/XMLSchema#"}
   :namespaces {""     "http://example.com/#"
                "rel"  "http://www.perceive.net/schemas/relationship/"
                "foaf" "http://xmlns.com/foaf/0.1/"}})

(defn encode []
  (ldf/encode (data-edn) encode-opts))

(deftest encode-test
  (is (string= (encode) (data-out-ttl))))

;;
;; Decode
;;

(def decode-opts
  {:namespaces {""    "http://example.com/#"
                "foaf" "http://xmlns.com/foaf/0.1/"
                "rel" "http://www.perceive.net/schemas/relationship/"}
   :object-lists? false
   :predicate-lists? false})

(defn decode []
  (ldf/decode (data-in-ttl) decode-opts))

(deftest decode-test
  (is (= (decode) (data-edn))))
