(ns ldf.turtle.parser
  (:require [clojure.string :as string]
            #?(:clj  [instaparse.core :as insta :refer [defparser]]
               :cljs [instaparse.core :as insta :refer-macros [defparser]])
            #?(:clj  [ldf.turtle.grammar :refer [grammar]]
               :cljs [ldf.turtle.grammar :refer-macros [grammar]])))

(defparser parser (grammar))

(defn parse [text]
  (let [result (parser text)]
    (if (insta/failure? result)
      (throw (ex-info "Parse Turtle Error" (insta/get-failure result)))
      result)))

(defn- base! [env]
  (fn [x]
    (swap! env assoc :base x)
    nil))

(defn- prefix-value [xs env]
  [(if (= (count xs) 3)
     (first xs)
     "")
   (if (= (last xs) "<#>")
     (str (:base @env) "#")
     (last xs))])

(defn- prefix! [env opts]
  (fn [& xs]
    (let [[extern-prefix url] (prefix-value xs env)]
      (swap! env assoc-in [:resolvers extern-prefix]
        (if-let [intern-prefix (get-in opts [:prefixes url])]
          (partial keyword intern-prefix)
          (partial str url))))
    nil))

(defn- prefixed-name [env]
  (fn [& xs]
    (let [[extern-prefix name] (prefix-value xs env)]
      (prn :prefix extern-prefix name xs)
      (if-let [func (get-in @env [:resolvers extern-prefix])]
        (func name)
        (throw (ex-info (str "Invalid input: undefined prefix '"
                             extern-prefix "'")
                        {:prefixed-name xs}))))))

(defn- ref-str [env]
  (fn [x]
    (if (string? x)
      (if (string/starts-with? x "#")
        (if-let [func (get-in @env [:resolvers ""])]
          (func x)
          (:base @env))
        (str "<" x ">"))
      x)))

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

(defn- triples [subject [_ & xs]]
  (if (= (count xs) 2)
    [subject (first xs) (second xs)]
    [subject (vec (map vec (partition 2 xs)))]))

(def flip-map (partial reduce-kv #(assoc %1 %3 (name %2)) {}))

(defn transformers [opts]
  (let [env (atom {})]
    {:turtleDoc    (fn [& xs] (vec xs))
     :base         (base! env)
     :prefix       (prefix! env opts)
     :PrefixedName (prefixed-name env)
     :triples      triples
     :iri          identity
     :subject      identity
     :predicate    identity
     :string       identity
     :literal      identity
     :object       identity
     :ref          (ref-str env)
     :objectList   splice-single
     :RDFLiteral   rdf-literal
     :a            (constantly :a)}))

(defn transform [opts tree]
  (-> (transformers (update opts :prefixes flip-map))
      (insta/transform tree)))
