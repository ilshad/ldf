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

(defn- next-blank-node-id [env]
  (let [label-ids (remove #{:_} (:blank-nodes-index env))]
    (keyword (str "_" (count label-ids)))))

(defn- ensure-blank-node-label [env label]
  (if (nil? (get-in env [:blank-nodes-labels label]))
    (let [id (next-blank-node-id env)]
      (-> (update env :blank-nodes-index conj id)
          (assoc-in [:blank-nodes-labels label] id)))
    env))

(defn- blank-node-label [env]
  (fn [& [_ label]]
    (-> (swap! env ensure-blank-node-label label)
        (get-in [:blank-nodes-labels label]))))

(defn- add-blank-node-property-list [env pol]
  (let [id  (next-blank-node-id env)
        pol (for [[pred obj] (mapv vec (partition 2 pol))] [id pred obj])]
    (-> (update env :blank-nodes-index conj id)
        (update :blank-node-property-lists conj {:id id :pol pol}))))

(defn- blank-node-property-list [env]
  (fn [& [[_ & pol]]]
    (-> (swap! env add-blank-node-property-list pol)
        :blank-node-property-lists
        last
        :id)))

(defn- restore-blank-node-property-list [env]
  (let [bnpls (:blank-node-property-lists @env)]
    (when-not (empty? bnpls)
      (swap! env assoc :blank-node-property-lists [])
      (mapcat :pol bnpls))))

;;
;; Main transforms
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

(defn- triple-groups [env]
  (fn [subj [_ & xs]]
    (conj (restore-blank-node-property-list env)
          [subj (mapv vec (partition 2 xs))])))

(def flip-map (partial reduce-kv #(assoc %1 %3 (name %2)) {}))

(defn- new-env []
  (atom {:blank-nodes-index         []
         :blank-nodes-labels        {}
         :blank-node-property-lists []}))

(defn- transformers [opts]
  (let [env (new-env)]
    {:turtleDoc             (fn [& xs] xs)
     :base                  (set-base! env)
     :prefix                (set-prefix-resolver! env opts)
     :PrefixedName          (prefixed-name env)
     :ref                   (expand-ref env)
     :BLANK_NODE_LABEL      (blank-node-label env)
     :blankNodePropertyList (blank-node-property-list env)
     :iri                   (iri env opts)
     :triples               (triple-groups env)
     :objectList            object-list
     :RDFLiteral            rdf-literal
     :integer               read-strings
     :decimal               read-strings
     :double                read-strings
     :BooleanLiteral        read-strings
     :ANON                  (constantly :_)
     :a                     (constantly :a)}))

(defn- transform [tree opts]
  (let [opts (update opts :namespaces flip-map)]
    (insta/transform (transformers opts) tree)))

;;
;; Flatten
;;

(defn- flatten-triple-groups [tree]
  (apply concat tree))

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

;;
;; API
;;

(defn decode-turtle [string opts]
  (-> (parse string)
      (transform opts)
      flatten-triple-groups
      flatten-predicate-lists
      flatten-object-lists))

