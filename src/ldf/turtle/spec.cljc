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

(s/def ::subject ::iri)

(s/def ::predicate
  (s/or :iri ::iri
        :a   #{:a}))

(s/def ::object
  (s/or :iri     ::iri
        :literal ::literal))

(s/def ::triple
  (s/cat :subject   ::subject
         :predicate ::predicate
         :object    ::object))

(s/def ::triples (s/coll-of ::triple))
