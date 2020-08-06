(ns ldf.turtle.parser
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            #?(:clj  [instaparse.core :as insta :refer [defparser]]
               :cljs [instaparse.core :as insta :refer-macros [defparser]])
            #?(:clj  [ldf.turtle.grammar :refer [grammar]]
               :cljs [ldf.turtle.grammar :refer-macros [grammar]])))

(defparser parser (grammar))

(defn- parse [string]
  (let [result (parser string)]
    (if (insta/failure? result)
      (throw (ex-info "Parse Turtle Error" (insta/get-failure result)))
      result)))

(defn- set-base! [env]
  (fn [string]
    (swap! env assoc :base string)
    nil))

(defn- set-prefix-resolver! [env opts]
  (fn [& xs]
    (swap! env assoc-in [:resolvers (first xs)]
      (if-let [ns (get-in opts [:namespaces (last xs)])]
        (if (= ns "")
          keyword
          (partial keyword ns))
        (partial str (last xs))))
    nil))

(defn- prefixed-name [env]
  (fn [& xs]
    (let [func (get-in @env [:resolvers (first xs)])]
      (func (last xs)))))

(defn- iri [env opts]
  (fn [string]
    (loop [[[uri label] & namespaces] (:namespaces opts)]
      (if uri
        (if (string/starts-with? string uri)
          (if (= string uri)
            string
            (let [relative (string/replace-first string uri "")]
              (if (= label "")
                (keyword relative)
                (keyword label relative))))
          (recur namespaces))
        string))))

(defn- object-list [& xs]
  (if (= (count xs) 1)
    (first xs)
    (vec xs)))

(defn- read-strings [& xs]
  (edn/read-string (apply str xs)))

(defn- rdf-literal [& xs]
  (if (= (count xs) 1)
    {:value (first xs)}
    (let [[value metadata] xs]
      (cond
        (coll? metadata)
        {:value value :lang (keyword (last metadata))}

        (and (keyword? metadata)
             ;; TODO: don't assume ns
             (#{:xsd/integer :xsd/decimal :xsd/double} metadata))
        (read-strings value)

        (and (string? metadata)
             (#{"http://www.w3.org/2001/XMLSchema#integer"
                "http://www.w3.org/2001/XMLSchema#decimal"
                "http://www.w3.org/2001/XMLSchema#double"} metadata))
        (read-strings value)

        :else {:value value :type metadata}))))

(defn- triples [subject [_ & xs]]
  (if (= (count xs) 2)
    [subject (first xs) (second xs)]
    [subject (mapv vec (partition 2 xs))]))

(def flip-map (partial reduce-kv #(assoc %1 %3 (name %2)) {}))

(defn- transformers [opts]
  (let [env (atom {})]
    {:turtleDoc      (fn [& xs] (vec xs))
     :base           (set-base! env)
     :prefix         (set-prefix-resolver! env opts)
     :PrefixedName   (prefixed-name env)
     :ref            (fn [s] (str (:base @env) s))
     :iri            (iri env opts)
     :triples        triples
     :objectList     object-list
     :RDFLiteral     rdf-literal
     :integer        read-strings
     :decimal        read-strings
     :double         read-strings
     :BooleanLiteral read-strings
     :a              (constantly :a)}))

(defn- transform [tree opts]
  (-> (transformers (update opts :namespaces flip-map))
      (insta/transform tree)))

(defn- flatten-predicate-lists [tree]
  (loop [nodes  tree
         acc    nil
         result []]
    (if acc
      (let [[triple & triples] acc]
        (recur nodes triples (conj result triple)))
      (if nodes
        (let [[node & nodes] nodes]
          (if node
            (if (= (count node) 2)
              (let [[subj pred-list] node
                    acc (map (fn [[pred obj]] [subj pred obj]) pred-list)]
                (recur nodes acc result))
              (recur nodes acc (conj result node)))
            (recur nodes acc result)))
        result))))

(defn- flatten-object-lists [tree]
  (loop [nodes  tree
         acc    nil
         result []]
    (if acc
      (let [[triple & triples] acc]
        (recur nodes triples (conj result triple)))
      (if nodes
        (let [[[subj pred objx :as node] & nodes] nodes]
          (if (vector? objx)
            (let [acc (map (fn [obj] [subj pred obj]) objx)]
              (recur nodes acc result))
            (recur nodes acc (conj result node))))
        result))))

(defn- normalize [tree opts]
  (cond-> (if (:predicate-lists? opts)
            (filterv identity tree)
            (flatten-predicate-lists tree))
    (not (:object-lists? opts)) flatten-object-lists))

(defn decode-turtle [string opts]
  (-> (parse string)
      (transform opts)
      (normalize opts)))
