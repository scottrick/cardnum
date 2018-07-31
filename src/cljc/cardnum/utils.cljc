(ns cardnum.utils
  (:require [clojure.string :refer [split split-lines join escape] :as s]
            [cardnum.cards :refer [all-cards] :as cards]
    #?@(:clj [[clj-time.core :as t] [clj-time.format :as f]])))

(def INFINITY 2147483647)

;;; Deck Parsing
(defn str->int [s]
  #?(:clj  (java.lang.Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn NaN?
  "Test if this number is nan"
  [x]
  #?(:clj  (false? (== x x))
     :cljs (js/isNaN x)))
  ; Nan is the only value for which equality is false

(defn identical-cards? [cards]
  (let [name (:title (first cards))]
    (every? #(= (:title %) name) cards)))

(defn filter-title [query cards]
  (if (empty? query)
    cards
    (let [lcquery (.toLowerCase query)]
      (filter #(or (not= (.indexOf (.toLowerCase (:title %)) lcquery) -1)
                   (not= (.indexOf (:normalizedtitle %) lcquery) -1))
              cards))))

(defn take-best-card
  "Returns a non-rotated card from the list of cards or a random rotated card from the list"
  [cards]
  (let [non-rotated (filter #(= (:title %)) cards)]
    (if (not-empty non-rotated)
      (first non-rotated)
      (first cards))))

(defn filter-exact-title [query cards]
  (let [lcquery (.toLowerCase query)]
    (filter #(or (= (.toLowerCase (:title %)) lcquery)
                 (= (:normalizedtitle %) lcquery))
            cards)))

(defn lookup
  "Lookup the card title (query) looking at all cards on specified alignment"
  [card]
  (let [q (.toLowerCase (:title card))
        id (:id card)
        cards @all-cards
        exact-matches (filter-exact-title q cards)]
    (cond (and id
               (first (filter #(= id (:trimCode %)) cards)))
          (let [id-matches (filter #(= id (:trimCode %)) cards)]
            (first (filter-exact-title q id-matches)))
          (not-empty exact-matches) (take-best-card exact-matches)
          :else
          (loop [i 2 matches cards]
            (let [subquery (subs q 0 i)]
              (cond (zero? (count matches)) card
                    (or (= (count matches) 1) (identical-cards? matches)) (take-best-card matches)
                    (<= i (count (:title card))) (recur (inc i) (filter-title subquery matches))
                    :else card))))))

(defn parse-line
  "Parse a single line of a deck string"
  [line]
  (let [clean (s/trim line)
        [_ qty-str card-name card-params]
        (re-matches #"(\d+)[^\s]*\s+([^\(^\[]+)([\(\[](.{0,7}))?" clean)]
    (if (and qty-str
             (not (NaN? (str->int qty-str)))
             card-name
             card-params)
      (assoc {} :qty (str->int qty-str) :card (s/trim card-name) :id (s/trim card-params))
      (if (and qty-str
               (not (NaN? (str->int qty-str)))
               card-name)
        (assoc {} :qty (str->int qty-str) :card (s/trim card-name))
        nil))))

(defn- line-reducer
  "Reducer function to parse lines in a deck string"
  [acc line]
  (if-let [card (parse-line line)]
    (conj acc card)
    acc))

(defn deck-string->list
  "Turn a raw deck string into a list of {:qty :title}"
  [deck-string]
  (reduce line-reducer [] (split-lines deck-string)))

(defn collate-deck
  "Takes a list of {:qty n :card title} and returns list of unique titles and summed n for same title"
  [card-list]
  ;; create a backing map of title to {:qty n :card title} and update the
  (letfn [(duphelper [currmap line]
            (let [title (:card line)
                  curr-qty (get-in currmap [title :qty] 0)
                  line (update line :qty #(+ % curr-qty))]
              (assoc currmap title line)))]
    (vals (reduce duphelper {} card-list))))

(defn lookup-deck
  "Takes a list of {:qty n :card title} and looks up each title and replaces it with the corresponding cardmap"
  [card-list]
  (let [card-list (collate-deck card-list)]
    ;; lookup each card and replace title with cardmap
    (map #(assoc % :card (lookup (assoc % :title (:card %)))) card-list)))

(defn parse-deck-string
  "Parses a string containing the decklist and returns a list of lines {:qty :card}"
  [deck-string]
  (let [raw-deck-list (deck-string->list deck-string)]
    (lookup-deck raw-deck-list)))

(defn side-from-str [side-str]
  (keyword (.toLowerCase side-str)))

(defn faction-label
  "Returns faction of a card as a lowercase label"
  [card]
  (if (nil? (:faction card))
    "neutral"
    (-> card :faction .toLowerCase (.replace " " "-"))))
