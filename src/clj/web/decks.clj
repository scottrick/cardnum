(ns web.decks
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [map-values response]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]
            [cardnum.cards :refer [all-cards]]
            [cardnum.decks :as decks]))


(defn decks-handler [req]
  (if-let [user (:user req)]
    (response 200 (mc/find-maps db "decks" {:username (:username user)}))
    (response 200 (mc/find-maps db "decks" {:username "__demo__"}))))

(defn decks-create-handler [{{username :username} :user
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

(defn decks-save-handler [{{username :username} :user
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

(defn decks-delete-handler [{{username :username} :user
                             {id :id}             :params}]
  (if (and username id)
    (if (acknowledged? (mc/remove db "decks" {:_id (object-id id) :username username}))
      (response 200 {:message "Deleted"})
      (response 403 {:message "Forbidden"}))
    (response 401 {:message "Unauthorized"})))

