(ns ldf.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]))

(s/def ::iri
  (s/or ::just         (s/and string? (partial re-matches #"^<.*>$"))
        ::prefixed     qualified-keyword?
        ::empty-prefix simple-keyword?))

(s/def ::value string?)
(s/def ::lang  simple-keyword?)
(s/def ::type  ::iri)

(s/def ::literal
  (s/or ::string string?
        ::number number?
        ::inst   inst?
        ::quoted (s/keys ::req-un [::value]
                         ::opt-un [::lang ::type])))

(s/def ::blank-node
  (s/and simple-keyword? #(string/starts-with? (name %) "_")))

(s/def ::subject
  (s/or ::iri        ::iri
        ::blank-node ::blank-node))

(s/def ::predicate
  (s/or ::a   #{:a}
        ::iri ::iri))

(s/def ::object
  (s/or ::blank-node  ::blank-node
        ::iri         ::iri
        ::literal     ::literal
        ::object-list (s/coll-of ::object)))

(s/def ::statement
  (s/or ::triple-simple
        (s/cat :subject   ::subject
               :predicate ::predicate
               :object    ::object)

        ::triple-with-predicate-list
        (s/cat :subject        ::subject
               :predicate-list (s/coll-of
                                (s/cat :predicate ::predicate
                                       :object    ::object)))))

(s/def ::statements (s/coll-of ::statement))
