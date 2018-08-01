(ns game-test.utils
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [cardnum.cards :refer [all-cards]]))

(defn load-card [title]
  (let [conn (mg/connect {:host "127.0.0.1" :port 27017})
        db (mg/get-db conn "meccg")
        card (mc/find-maps db "cards" {:title title})
        ret (first card)]
    (mg/disconnect conn)
    ret))

(defn load-cards []
  (let [conn (mg/connect {:host "127.0.0.1" :port 27017})
        db (mg/get-db conn "meccg")
        cards (doall (mc/find-maps db "cards"))]
    (mg/disconnect conn)
    cards))

(defn qty [card amt]
  (let [loaded-card (if (string? card) (@all-cards card) card)]
    (when-not loaded-card
      (throw (Exception. (str card " not found in @all-cards"))))
    {:card loaded-card :qty amt}))

(defn make-deck [identity deck]
  {:identity identity
   :deck (map #(if (string? %) (qty % 1) %) deck)})

(defn default-contestant
  ([] (default-contestant [(qty "Hedge Fund" 3)]))
  ([deck] (make-deck "Custom Biotics: Engineered for Success" deck)))

(defn default-challenger
  ([] (default-challenger [(qty "Sure Gamble" 3)]))
  ([deck] (make-deck "The Professor: Keeper of Knowledge" deck)))
