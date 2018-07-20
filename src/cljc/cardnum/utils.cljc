(ns cardnum.utils)

(def INFINITY 2147483647)


(defn str->int [s]
  #?(:clj  (java.lang.Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn side-from-str [side-str]
  (keyword (.toLowerCase side-str)))

(defn faction-label
  "Returns faction of a card as a lowercase label"
  [card]
  (if (nil? (:faction card))
    "neutral"
    (-> card :faction .toLowerCase (.replace " " "-"))))

(defn other-side [side]
  (cond (= side :contestant) :challenger
        (= side :challenger) :contestant))
