(ns web.saves
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [proj-dir map-values response]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]
            [cheshire.core :as json]
            [clojure.data.json :as j-data]
            [clojure.java.io :as io]
            [cardnum.cards :refer [all-cards]]
            [cardnum.decks :as decks]))

(defn saves-handler [req]
  (when-let [user (:user req)]
    (let [list (.list (io/file (io/file (str proj-dir "/all-saves/" (:username user)))))]
      (let [saves (for [line list]
                    (when (not (boolean (re-find #"Store" (str line))))
                      ;(println (str line))
                      (let [load (j-data/read-str
                                   (slurp (str "all-saves/" (:username user) "/" line))
                                   :key-fn keyword)
                            ;_ (println (str load))
                            save {:game-load (subs (str line) 0 (- (count (str line)) 6))
                                  :name (:game-name load)
                                  :title (:title load)
                                  :identity1 {:title (:id_title1 load)
                                              :set_code (:id_scode1 load)
                                              :ImageName (:id_image1 load)}
                                  :identity2 {:title (:id_title2 load)
                                              :set_code (:id_scode2 load)
                                              :ImageName (:id_image2 load)}
                                  :date (:game-date load)
                                  :game-save (:game-save load)}]
                        save)))]
        ;(println (str saves))
        (response 200 (json/generate-string saves))))))

(defn saves-create-handler [{{username :username} :user
                             deck                 :body}]
  (if (and username deck)
    (let [deck (-> deck
                   (update-in [:resources] (fn [cards] (mapv #(select-keys % [:qty :card :id]) cards)))
                   (update-in [:hazards] (fn [cards] (mapv #(select-keys % [:qty :card :id]) cards)))
                   (update-in [:sideboard] (fn [cards] (mapv #(select-keys % [:qty :card :id]) cards)))
                   (update-in [:characters] (fn [cards] (mapv #(select-keys % [:qty :card :id]) cards)))
                   (update-in [:pool] (fn [cards] (mapv #(select-keys % [:qty :card :id]) cards)))
                   (update-in [:fwsb] (fn [cards] (mapv #(select-keys % [:qty :card :id]) cards)))
                   (assoc :username username))]
      (response 200 (mc/insert-and-return db "decks" deck)))
    (response 401 {:message "Unauthorized"})))

(defn saves-save-handler [{{username :username} :user
                           deck                 :body}]
  (if (and username deck)
    (let [get-code (fn [c] (if (nil? (:id c))
                             c
                             (assoc c :code (str (:card c) " " (:id c)))))
          get-data (fn [c] (if (nil? (:id c))
                             (update-in c [:card] @all-cards)
                             (update-in c [:code] @all-cards)))
          get-swap (fn [c] (if (nil? (:id c))
                             c
                             (do
                               (dissoc c :card)
                               (assoc c :card (:code c)))))
          rid-code (fn [c] (if (nil? (:id c))
                             c
                             (dissoc c :code :id)))
          rid-card (fn [c] (nil? (:card c)))

          check-deck (-> deck
                         (map-values [:resources :hazards :sideboard
                                        :characters :pool :fwsb]
                                     #(mapv get-code %))
                         (map-values [:resources :hazards :sideboard
                                        :characters :pool :fwsb]
                                     #(mapv get-data %))
                         (map-values [:resources :hazards :sideboard
                                        :characters :pool :fwsb]
                                     #(mapv get-swap %))
                         (map-values [:resources :hazards :sideboard
                                        :characters :pool :fwsb]
                                     #(mapv rid-code %))
                         (update-in [:identity] #(@all-cards (:title %))))
          deck (-> deck
                   (map-values [:resources :hazards :sideboard
                                  :characters :pool :fwsb]
                               (fn [cards] (mapv #(select-keys % [:qty :card :id]) cards)))
                                      (assoc :username username))
          status (decks/calculate-deck-status check-deck)
          deck (assoc deck :status status)]
      (when (nil? (:identity check-deck))
        (println "NIL IDENTITY WHEN SAVING DECK")
        (println "Deck:" deck)
        (println "-----------------------------"))
      (if-let [deck-id (:_id deck)]
        (if (:identity deck)
          (do (mc/update db "decks"
                         {:_id (object-id deck-id) :username username}
                         {"$set" (dissoc deck :_id)})
            (response 200 {:message "OK"}))
          (response 409 {:message "Deck is missing identity"}))
        (response 409 {:message "Deck is missing _id"})))
    (response 401 {:message "Unauthorized"})))

(defn saves-delete-handler [{{username :username} :user
                             {game-save :game-save} :params}]
  (if (and username game-save)
    (if (and (.exists (io/file (str proj-dir "/all-saves/" username "/" game-save "g.json" )))
             (.exists (io/file (str proj-dir "/all-states/" username "/" game-save "s.json" ))))
      (do
      (io/delete-file (str proj-dir "/all-saves/" username "/" game-save "g.json" ))
      (io/delete-file (str proj-dir "/all-states/" username "/" game-save "s.json"))
      (response 200 {:message "Deleted"}))
      (response 403 {:message "Forbidden"}))
    (response 401 {:message "Unauthorized"})))
