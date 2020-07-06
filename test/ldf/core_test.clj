(ns ldf.core-test
  (:require [ldf.core :as ldf]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def string= #(= (string/trim %1) (string/trim %2)))

(defn example-1-edn []
  (edn/read-string (slurp "test/ldf/example_1.edn")))

(defn example-1-ttl []
  (slurp "test/ldf/example_1.ttl"))

(def example-1-prefixes
  {:_   "#"
   :rel "http://www.perceive.net/schemas/relationship/"})

(deftest encode-turtle-test
  (is (string= (ldf/encode (example-1-edn)
                           {:prefixes example-1-prefixes
                            :base     "http://example.com/"})
               (example-1-ttl))))

(deftest decode-turtle-test
  )
