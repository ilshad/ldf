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
