(ns meccg.appstate
  (:require [cardnum.utils :refer [str->int]]))

(def app-state
  (atom {:active-page "/"
         :user (js->clj js/user :keywordize-keys true)
         :options (merge {:background "lobby-bg"
                          :deckstats "always"
                          :gamestats "always"
                          :sounds (let [sounds (js->clj (.getItem js/localStorage "sounds"))]
                                    (if (nil? sounds) true (= sounds "true")))
                          :sounds-volume (let [volume (js->clj (.getItem js/localStorage "sounds_volume"))]
                                           (if (nil? volume) 100 (str->int volume)))}
                         (:options (js->clj js/user :keywordize-keys true)))

         :cards-loaded false
         :sets [] :mwl []
         :decks [] :decks-loaded false
         :stats (:stats (js->clj js/user :keywordize-keys true))
         :games [] :gameid nil :messages []
         :channels {:general []}
         }))
