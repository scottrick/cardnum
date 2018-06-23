(in-ns 'game.core)

(declare all-active card-flag-fn? clear-turn-register! clear-wait-prompt create-deck hand-size keep-hand mulligan
         show-wait-prompt turn-message)

(def game-states (atom {}))

(defn- card-implemented
  "Checks if the card is implemented. Looks for a valid return from `card-def`.
  If implemented also looks for `:implementation` key which may contain special notes.
  Returns either:
    nil - not implemented
    :full - implemented fully
    msg - string with implementation notes"
  [card]
  (when-let [cdef (card-def card)]
    ;; Card is defined - hence implemented
    (if-let [impl (:implementation cdef)]
      (if (:recurring cdef) (str impl ". Recurring credits usage not restricted") impl)
      (if (:recurring cdef) "Recurring credits usage not restricted" :full))))

;;; Functions for the creation of games and the progression of turns.
(defn init-identity
  "Initialise the identity"
  [state side identity]
  (card-init state side identity)
  (when-let [baselink (:baselink identity)]
    (gain state side :link baselink)))

(defn- init-hands [state]
  (draw state :contestant 5 {:suppress-event true})
  (draw state :challenger 5 {:suppress-event true})
  (when (and (-> @state :contestant :identity :title)
             (-> @state :challenger :identity :title))
    (show-wait-prompt state :challenger "Contestant to keep hand or mulligan"))
  (doseq [side [:contestant :challenger]]
    (when (-> @state side :identity :title)
      (show-prompt state side nil "Keep hand?"
                   ["Keep" "Mulligan"]
                   #(if (= % "Keep")
                      (keep-hand state side nil)
                      (mulligan state side nil))))))

(defn- init-game-state
  "Initialises the game state"
  [{:keys [players gameid spectatorhands room] :as game}]
  (let [contestant (some #(when (= (:side %) "Contestant") %) players)
        challenger (some #(when (= (:side %) "Challenger") %) players)
        contestant-deck (create-deck (:deck contestant) (:user contestant))
        challenger-deck (create-deck (:deck challenger) (:user challenger))
        contestant-deck-id (get-in contestant [:deck :_id])
        challenger-deck-id (get-in challenger [:deck :_id])
        contestant-options (get-in contestant [:options])
        challenger-options (get-in challenger [:options])
        contestant-identity (assoc (or (get-in contestant [:deck :identity]) {:side "Contestant" :type "Identity"}) :cid (make-cid))
        contestant-identity (assoc contestant-identity :implementation (card-implemented contestant-identity))
        challenger-identity (assoc (or (get-in challenger [:deck :identity]) {:side "Challenger" :type "Identity"}) :cid (make-cid))
        challenger-identity (assoc challenger-identity :implementation (card-implemented challenger-identity))]
    (atom
      {:gameid gameid :log [] :active-player :challenger :end-turn true
       :room room
       :rid 0 :turn 0 :eid 0
       :sfx [] :sfx-current-id 0
       :stats {:time {:started (t/now)}}
       :options {:spectatorhands spectatorhands}
       :contestant {:user (:user contestant) :identity contestant-identity
              :options contestant-options
              :deck (zone :deck contestant-deck)
              :deck-id contestant-deck-id
              :hand []
              :discard [] :scored [] :rfg [] :play-area []
              :servers {:hq {} :rd {} :archives {}}
              :click 0 :credit 5 :bad-publicity 0 :has-bad-pub 0
              :toast []
              :hand-size {:base 5 :mod 0}
              :agenda-point 0
              :click-per-turn 3 :agenda-point-req 7 :keep false}
       :challenger {:user (:user challenger) :identity challenger-identity
                :options challenger-options
                :deck (zone :deck challenger-deck)
                :deck-id challenger-deck-id
                :hand []
                :discard [] :scored [] :rfg [] :play-area []
                :rig {:resource [] :muthereff [] :hazard []}
                :toast []
                :click 0 :credit 5 :run-credit 0 :link 0 :tag 0
                :memory {:base 4 :mod 0 :used 0}
                :hand-size {:base 5 :mod 0}
                :agenda-point 0
                :hq-access 1 :rd-access 1 :tagged 0
                :brain-damage 0 :click-per-turn 4 :agenda-point-req 7 :keep false}})))

(defn init-game
  "Initializes a new game with the given players vector."
  [game]
  (let [state (init-game-state game)
        contestant-identity (get-in @state [:contestant :identity])
        challenger-identity (get-in @state [:challenger :identity])]
    (init-identity state :contestant contestant-identity)
    (init-identity state :challenger challenger-identity)
    ;(swap! game-states assoc gameid state)
    (let [side :contestant]
      (wait-for (trigger-event-sync state side :pre-start-game)
                (let [side :challenger]
                  (wait-for (trigger-event-sync state side :pre-start-game)
                            (init-hands state)))))
    state))

(defn server-card
  ([title] (@all-cards title))
  ([title user]
   (@all-cards title)))

(defn make-card
  "Makes or remakes (with current cid) a proper card from an @all-cards card"
  ([card] (make-card card (make-cid)))
  ([card cid]
  (-> card
      (assoc :cid cid :implementation (card-implemented card))
      (dissoc :setname :text :_id :influence :number :influencelimit :factioncost))))

(defn reset-card
  "Resets a card back to its original state - retaining any data in the :persistent key"
  ([state side card]
   (update! state side (merge (make-card (get @all-cards (:title card)) (:cid card)) {:persistent card}))))

(defn create-deck
  "Creates a shuffled draw deck (R&D/Stack) from the given list of cards.
  Loads card data from server-side @all-cards map if available."
  ([deck] (create-deck deck nil))
  ([deck user]
   (shuffle (mapcat #(map (fn [card]
                            (let [server-card (or (server-card (:title card) user) card)
                                  c (assoc (make-card server-card) :art (:art card))]
                              (if-let [init (:init (card-def c))] (merge c init) c)))
                          (repeat (:qty %) (assoc (:card %) :art (:art %))))
                    (shuffle (vec (:cards deck)))))))

(defn make-rid
  "Returns a progressively-increasing integer to identify a new remote server."
  [state]
  (get-in (swap! state update-in [:rid] inc) [:rid]))

(defn make-eid
  [state]
  {:eid (:eid (swap! state update-in [:eid] inc))})

(defn make-result
  [eid result]
  (assoc eid :result result))

(defn mulligan
  "Mulligan starting hand."
  [state side args]
  (shuffle-into-deck state side :hand)
  (draw state side 5 {:suppress-event true})
  (let [card (get-in @state [side :identity])]
    (when-let [cdef (card-def card)]
      (when-let [mul (:mulligan cdef)]
        (mul state side (make-eid state) card nil))))
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "takes a mulligan")
  (trigger-event state side :pre-first-turn)
  (when (and (= side :contestant) (-> @state :challenger :identity :title))
    (clear-wait-prompt state :challenger)
    (show-wait-prompt state :contestant "Challenger to keep hand or mulligan"))
  (when (and (= side :challenger)  (-> @state :contestant :identity :title))
    (clear-wait-prompt state :contestant)))

(defn keep-hand
  "Choose not to mulligan."
  [state side args]
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "keeps their hand")
  (trigger-event state side :pre-first-turn)
  (when (and (= side :contestant) (-> @state :challenger :identity :title))
    (clear-wait-prompt state :challenger)
    (show-wait-prompt state :contestant "Challenger to keep hand or mulligan"))
  (when (and (= side :challenger)  (-> @state :contestant :identity :title))
    (clear-wait-prompt state :contestant)))

(defn end-phase-12
  "End phase 1.2 and trigger appropriate events for the player."
  ([state side args] (end-phase-12 state side (make-eid state) args))
  ([state side eid args]
   (turn-message state side true)
   (let [extra-clicks (get-in @state [side :extra-click-temp] 0)]
     (gain state side :click (get-in @state [side :click-per-turn]))
     (wait-for (trigger-event-sync state side (if (= side :contestant) :contestant-turn-begins :challenger-turn-begins))
               (do (when (= side :contestant)
                     (wait-for (draw state side 1 nil)
                               (trigger-event-simult state side eid :contestant-mandatory-draw nil nil)))

                   (cond

                     (neg? extra-clicks)
                     (lose state side :click (abs extra-clicks))

                     (pos? extra-clicks)
                     (gain state side :click extra-clicks))

                   (swap! state dissoc-in [side :extra-click-temp])
                   (swap! state dissoc (if (= side :contestant) :contestant-phase-12 :challenger-phase-12))
                   (when (= side :contestant)
                     (update-all-advancement-costs state side)))))))

(defn start-turn
  "Start turn."
  [state side args]

  ; Functions to set up state for undo-turn functionality
  (doseq [s [:challenger :contestant]] (swap! state dissoc-in [s :undo-turn]))
  (swap! state assoc :turn-state (dissoc @state :log))

  (when (= side :contestant)
    (swap! state update-in [:turn] inc))

  (doseq [c (filter :new (all-installed state side))]
    (update! state side (dissoc c :new)))

  (swap! state assoc :active-player side :per-turn nil :end-turn false)
  (swap! state assoc-in [side :register] nil)

  (let [phase (if (= side :contestant) :contestant-phase-12 :challenger-phase-12)
        start-cards (filter #(card-flag-fn? state side % phase true)
                            (all-active state side))]
    (swap! state assoc phase true)
    (trigger-event state side phase nil)
    (if (not-empty start-cards)
      (toast state side
                 (str "You may use " (clojure.string/join ", " (map :title start-cards))
                      (if (= side :contestant)
                        " between the start of your turn and your mandatory draw."
                        " before taking your first click."))
                 "info")
      (end-phase-12 state side args))))

(defn end-turn
  ([state side args] (end-turn state side (make-eid state) args))
  ([state side eid args]
   (let [max-hand-size (max (hand-size state side) 0)]
     (when (<= (count (get-in @state [side :hand])) max-hand-size)
       (turn-message state side false)
       (when (and (= side :challenger)
                  (neg? (hand-size state side)))
         (flatline state))
       (wait-for
         (trigger-event-sync state side (if (= side :challenger) :challenger-turn-ends :contestant-turn-ends))
         (do (when (= side :challenger)
               (trigger-event state side :post-challenger-turn-ends))
             (doseq [a (get-in @state [side :register :end-turn])]
               (resolve-ability state side (:ability a) (:card a) (:targets a)))
             (swap! state assoc-in [side :register-last-turn] (-> @state side :register))
             (doseq [card (all-active-installed state :challenger)]
               ;; Clear the added-virus-counter flag for each virus in play.
               ;; We do this even on the contestant's turn to prevent shenanigans with something like Gorman Drip and Surge
               (when (has-subtype? card "Virus")
                 (set-prop state :challenger card :added-virus-counter false))
               ;; Remove all-turn strength from characterbreakers.
               ;; We do this even on the contestant's turn in case the breaker is boosted due to Offer You Can't Refuse
               (when (has-subtype? card "Icebreaker")
                 (update! state side (update-in (get-card state card) [:pump] dissoc :all-turn))
                 (update-breaker-strength state :challenger card)))
             (swap! state assoc :end-turn true)
             (swap! state update-in [side :register] dissoc :cannot-draw)
             (swap! state update-in [side :register] dissoc :drawn-this-turn)
             (doseq [c (filter #(= :this-turn (:rezzed %)) (all-installed state :contestant))]
               (update! state side (assoc c :rezzed true)))
             (clear-turn-register! state)
             (swap! state dissoc :turn-events)
             (when-let [extra-turns (get-in @state [side :extra-turns])]
               (when (pos? extra-turns)
                 (start-turn state side nil)
                 (swap! state update-in [side :extra-turns] dec)
                 (let [turns (if (= 1 extra-turns) "turn" "turns")]
                   (system-msg state side (clojure.string/join ["will have " extra-turns " extra " turns " remaining."])))))
             (effect-completed state side eid)))))))
