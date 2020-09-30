(ns com.ilshad.ldf.turtle.parser
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            #?(:cljs [instaparse.core :as insta :refer-macros [defparser]]
               :clj  [instaparse.core :as insta :refer [defparser]])
            #?(:cljs [com.ilshad.ldf.turtle.grammar :refer-macros [grammar]]
               :clj  [com.ilshad.ldf.turtle.grammar :refer [grammar]])))

;;
;; Grammar
;;

(defparser parser (grammar))

(defn- parse [string]
  (let [result (parser string)]
    (if (insta/failure? result)
      (throw (ex-info "Parse Turtle Error" (insta/get-failure result)))
      result)))

;;
;; Namespaces and prefixes
;;

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

(defn- expand-ref [env]
  (fn [string]
    (str (when-not (re-seq #"@" string)
           (:base @env))
         string)))

;;
;; Blank Nodes
;;

(defn- blank-node? [x]
  (and (vector? x)
       (#{:ANON :BLANK_NODE_LABEL :blankNodePropertyList}
        (first x))))

(defn- gen-blank-node-id [tag env]
  (case tag
    :ANON 0
    (inc (apply max (:blank-nodes-index env)))))

(defn- traverse-blank-nodes [parent-id property-object-list env]
  (reduce
    (fn [env [pred obj]]
      (if (blank-node? obj)
        (let [child-pol (rest (second obj))
              child-id  (gen-blank-node-id child-pol env)]
          (traverse-blank-nodes child-id
                                (partition 2 child-pol)
                                (-> (update env :blank-nodes-index conj
                                      child-id)
                                    (update-in [:blank-nodes parent-id] conj
                                      [pred child-id]))))
        (update-in env [:blank-nodes parent-id] conj [pred obj])))
    env property-object-list))

(defn- save-blank-nodes [subj xs env]
  (swap! env
    (fn [env]
      (let [id  (gen-blank-node-id (first subj) env)
            pol (when (= (first subj) :blankNodePropertyList)
                  (rest (second subj)))]
        (traverse-blank-nodes id
                              (partition 2 (concat pol xs))
                              (update env :blank-nodes-index conj id)))))
  nil)

(defn- restore-blank-nodes [env]
  (fn [tree]
    (let [nodes (:blank-nodes @env)]
      (reduce
        (fn [tree id]
          (concat tree (map (fn [[pred obj]] [id pred obj])
                         (get nodes id))))
        tree
        (:blank-nodes-index @env)))))

;;
;; Transform parse tree
;;

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

(defn- triples [env]
  (fn [subj [_ & xs]]
    (if (blank-node? subj)
      (save-blank-nodes subj xs env)
      [subj (mapv vec (partition 2 xs))])))

(defn- document [env]
  (fn [& xs]
    {:tree (vec xs)
     :env  env}))

(def flip-map (partial reduce-kv #(assoc %1 %3 (name %2)) {}))

(def new-env
  {:blank-nodes       {}
   :blank-nodes-index [0]})

(defn- transformers [opts]
  (let [env (atom new-env)]
    {:turtleDoc             (document env)
     :base                  (set-base! env)
     :prefix                (set-prefix-resolver! env opts)
     :PrefixedName          (prefixed-name env)
     :ref                   (expand-ref env)
     :iri                   (iri env opts)
     :triples               (triples env)
     :objectList            object-list
     :RDFLiteral            rdf-literal
     :integer               read-strings
     :decimal               read-strings
     :double                read-strings
     :BooleanLiteral        read-strings
     :a                     (constantly :a)}))

(defn- transform [tree opts]
  (-> (transformers (update opts :namespaces flip-map))
      (insta/transform tree)))

;;
;; Flatten trees
;;

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

(defn- normalize [{:keys [tree env]} opts]
  (-> (if (:predicate-lists? opts)
            (filterv identity tree)
            (flatten-predicate-lists tree))
      ((restore-blank-nodes env))
      (cond-> (not (:object-lists? opts)) flatten-object-lists)))

;;
;; API
;;

(defn decode-turtle [string opts]
  (-> (parse string)
      (transform opts)
      (normalize opts)))
