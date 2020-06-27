(ns ldf.turtle.encode
  (:require [ldf.spec :as spec]))

(defmulti form (fn [[tag value] context] tag))

(defmethod form :default [[tag value] _]
  (throw (ex-info "Not Implemented" {:tag tag :value value})))

(defmethod form ::spec/just    [[_ x] c] [x c])
(defmethod form ::spec/number  [[_ x] c] [x c])
(defmethod form ::spec/inst    [[_ x] c] [x c])
(defmethod form ::spec/iri     [[_ x] c] (form x c))
(defmethod form ::spec/literal [[_ x] c] (form x c))
(defmethod form ::spec/string  [[_ s] c] [(str "\"" s "\"") c])
(defmethod form ::spec/a       [_     c] ["a" c])

(defmethod form ::spec/quoted [[_ {:keys [value lang type] :as m}] c]
  [(str "\"" value "\""
         (cond
           lang (str "@" (name lang))
           type (str "^^" (form type c))
           :else ""))
   c])

(defmethod form ::spec/prefixed [[_ kw] c]
  (let [prefix (namespace kw)]
    (if-let [uri (get-in c [:opts :prefixes (keyword prefix)])]
      (if (-> c :opts :prefixes?)
        [(str prefix ":" (name kw)) (update c :prefixes conj (keyword prefix))]
        [(str "<" uri (name kw) ">") c])
      (throw (ex-info (str "Prefix for " kw " is not defined.") {:kw kw})))))

(defmethod form ::spec/empty-prefix [[_ kw] c]
  (if-let [url (get-in c [:opts :prefixes :_])]
    [(str kw) (update c :prefixes conj :_)]
    (throw (ex-info "Namespace for empty prefix is not defined." {}))))

(defn- reduce-map [m c keys]
  (reduce
    (fn [[result c] k]
      (let [[s c] (form (m k) c)]
        [(str result (when-not (empty? result) " ") s) c]))
    ["" c]
    keys))

(defn- reduce-object-list [objects c]
  (reduce
    (fn [[result c] object]
      (let [[s c] (form object c)]
        [(conj result s) c]))
    [[] c]
    objects))

(defn- reduce-predicate-list [items c]
  (reduce
    (fn [[result c] m]
      (let [[s c] (reduce-map m c [:predicate :object])]
        [(conj result s) c]))
    [[] c]
    items))

(defmethod form ::spec/object-list [[_ objects] c]
  (let [[strings c] (reduce-object-list objects c)]
    [(apply str (interpose ", " strings)) c]))

(defmethod form ::spec/triple-simple [[_ m] c]
  (let [[s c] (reduce-map m c [:subject :predicate :object])]
    [(str s ".\n\n") c]))

(defmethod form ::spec/triple-with-predicate-list [[_ m] c]
  (let [[subject c]    (form (:subject m) c)
        [predicates c] (reduce-predicate-list (:predicate-list m) c)]
    [(str subject "\n    "
          (apply str (interpose ";\n    " predicates)) ".\n\n")
     c]))

(defn- reduce-triples [triples context]
  (reduce
    (fn [[text context] triple]
      (let [[string context] (form triple context)]
        [(str text string) context]))
    ["" context]
    triples))

(defn- print-prefixes [context]
  (str
   (when-let [base (-> context :opts :base)]
     (str "@base <" base ">.\n"))
   (reduce
     (fn [result prefix]
       (str result "@prefix "
            (case prefix :_ "" (name prefix))
            ": <" (get-in context [:opts :prefixes prefix]) ">.\n"))
     "" (sort (:prefixes context)))
   "\n"))

(defn encode-turtle [triples opts]
  (let [[string context] (reduce-triples triples {:opts opts :prefixes #{}})]
    (str (print-prefixes context) string)))
