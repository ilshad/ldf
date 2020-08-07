(ns ldf.turtle.spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::iri
  (s/or :expanded  string?
        :qualified qualified-keyword?
        :simple    simple-keyword?))

(s/def ::value string?)
(s/def ::lang  simple-keyword?)
(s/def ::type  ::iri)

(s/def ::literal
  (s/or :number  number?
        :boolean boolean?
        :quoted  (s/keys ::req-un [::value]
                         ::opt-un [::lang ::type])))

(s/def ::triple
  (s/cat :subject   ::iri
         :predicate (s/or :a #{:a} :iri ::iri)
         :object    (s/or :iri ::iri :literal ::literal)))

(s/def ::triples (s/coll-of ::triple))
