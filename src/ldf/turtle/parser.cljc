(ns ldf.turtle.parser
  (:require #?(:clj  [instaparse.core :as insta :refer [defparser]]
               :cljs [instaparse.core :as insta :refer-macros [defparser]])
            #?(:clj  [ldf.turtle.grammar :refer [grammar]]
               :cljs [ldf.turtle.grammar :refer-macros [grammar]])))

(defparser parser (grammar))

(defn parse [text]
  (let [result (parser text)]
    (if (insta/failure? result)
      (throw (ex-info "Parse Turtle Error" (insta/get-failure result)))
      result)))

(defn- ref-str [x]
  (if (string? x)
    (str "<" x ">")
    x))

(defn- splice-single [& xs]
  (if (= (count xs) 1)
    (first xs)
    (vec xs)))

(defn- rdf-literal [& xs]
  (if (= (count xs) 1)
    (first xs)
    (let [[value [tag _ tag-value]] xs]
      (case tag
        :langtag {:value value :lang (keyword tag-value)}
        xs))))

(def transform-map
  {:base         (fn [x] {:base x})
   :prefix       (fn [& xs]
                   {:prefix
                    {(if (= (first xs) ":") :_ (keyword (first xs)))
                     (last xs)}})
   :turtleDoc    (fn [& xs] (vec xs))
   :triples      (fn [subject [_ & xs]]
                   (if (= (count xs) 2)
                     [subject (first xs) (second xs)]
                     [subject (vec (map vec (partition 2 xs)))]))
   :iri          identity
   :subject      identity
   :predicate    identity
   :string       identity
   :literal      identity
   :object       identity
   :ref          ref-str
   :objectList   splice-single
   :RDFLiteral   rdf-literal
   :a            (constantly :a)
   :PrefixedName (fn [& xs]
                   (if (= (count xs) 2)
                     (keyword (second xs))
                     (keyword (first xs) (last xs))))})

(defn transform [tree]
  (insta/transform transform-map tree))
