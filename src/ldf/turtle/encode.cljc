(ns ldf.turtle.encode
  (:require [clojure.string :as string]
            [ldf.turtle.spec :as spec]))

;;
;; Namespaces and prefixes
;;

(defn- intern-prefixes [opts]
  (assoc opts :prefixes*
    (reduce-kv
      (fn [result label uri]
        (assoc result
          (if (re-find #"^https?://" uri)
            uri
            (if-let [base (:base opts)]
              (str base uri)
              (throw
               (ex-info (str ":base option is required for prefix " uri)
                        {:prefix {label uri}}))))
          label))
      {} (:prefixes opts))))

(defn- uri->prefixed [string opts]
  (when (re-find #"^https?://" string)
    (loop [[[uri label] & prefixes] (:prefixes* opts)]
      (when uri
        (if (string/starts-with? string uri)
          (if (= string uri)
            string
            (str label ":" (string/replace-first string uri "")))
          (recur prefixes))))))

(defn- ns-not-found-msg [kw]
  (str "Base URI for "
       (if-let [ns (namespace kw)]
         (str "namespace '" ns "' ")
         "simple keywords ")
       "is not defined."))

(defn- encode-keyword [kw opts]
  (if-let [uri (get-in opts [:namespaces (or (namespace kw) "")])]
    (if-let [label (get-in opts [:prefixes* uri])]
      (str label ":" (name kw))
      (if (= uri (:base opts))
        (str "<" (name kw) ">")
        (str "<" uri (name kw) ">")))
    (throw (ex-info (ns-not-found-msg kw) {::spec/iri kw}))))

;;
;; Base syntax
;;

(defn- encode-iri [iri opts]
  (cond
    (string? iri)  (or (uri->prefixed iri opts) (str "<" iri ">"))
    (keyword? iri) (encode-keyword iri opts)
    :else          (throw (ex-info "Invalid IRI" {::spec/iri iri}))))

(defn- encode-quoted-literal [{:keys [value lang type]} opts]
  (str "\"" value "\""
       (or (and lang (str "@" (name lang)))
           (and type (str "^^" (encode-iri type opts))))))

(defn- encode-literal [x opts]
  (if (map? x)
    (encode-quoted-literal x opts)
    x))

(defn- encode-subject [subj opts]
  (encode-iri subj opts))

(defn- encode-predicate [pred opts]
  (if (= pred :a) ":a" (encode-iri pred opts)))

(defn- encode-object [obj opts]
  (if ((some-fn string? qualified-keyword? simple-keyword?) obj)
    (encode-iri obj opts)
    (encode-literal obj opts)))

(defn- encode-triples [triples opts]
  (map (fn [triple]
         (map #(%1 %2 opts)
           [encode-subject encode-predicate encode-object]
           triple))
    triples))

;;
;; Reduce collections
;;

(defn- collect-pred-lists [triples]
  (reduce
    (fn [result [subj pred obj]]
      (update result subj
        (fn [pred-list]
          (conj (or pred-list []) [pred obj]))))
    {} triples))

(defn- collect-obj-lists [pred-list]
  (reduce
    (fn [result [pred obj]]
      (update result pred conj obj))
    {} pred-list))

(defn- normalize-obj-lists [m]
  (reduce-kv
    (fn [result pred obj-list]
      (conj result
            (let [[obj & more] obj-list]
              (if more
                [pred obj-list]
                [pred obj]))))
    [] m))

(defn- normalize-lists [m]
  (reduce-kv
    (fn [result obj pred-list]
      (conj result
            (let [[[pred subj] & more] pred-list]
              (if more
                [obj (-> pred-list collect-obj-lists normalize-obj-lists)]
                [obj pred subj]))))
    [] m))

;;
;; Build final document
;;

(defn- print-objx [objx]
  (if (coll? objx)
    (apply str (interpose ", " objx))
    objx))

(defn- print-pred-list [pred-list]
  (apply str
    (interpose ";\n"
      (map (fn [[pred objx]] (str "    " pred " " (print-objx objx)))
        pred-list))))

(defn- print-triples [triples]
  (apply str
    (map (fn [[subj pred obj]]
           (if obj
             (str subj " " pred " " obj ".\n\n")
             (str subj "\n" (print-pred-list pred) ".\n\n")))
      triples)))

(defn- print-prefixes [opts]
  (apply str
    (map (fn [[label url]] (str "@prefix " label ": <" url ">.\n"))
      (:prefixes opts))))

(defn- print-base [opts]
  (when-let [base (:base opts)]
    (str "@base <" base ">.\n")))

(defn- print-turtle [triples opts]
  (str (print-base opts)
       (print-prefixes opts) "\n"
       (print-triples triples)))

;;
;; API
;;

(defn encode-turtle [data opts]
  (-> (encode-triples data (intern-prefixes opts))
      collect-pred-lists
      normalize-lists
      (print-turtle opts)))
