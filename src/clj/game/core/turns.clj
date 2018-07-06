(in-ns 'game.core)

(declare all-active card-flag-fn? clear-turn-register! clear-wait-prompt create-pool create-deck create-board create-fw-dc create-location hand-size keep-hand interrupt mulligan
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
  ;;(draw state :contestant 8 {:suppress-event true})
  ;;(draw state :challenger 8 {:suppress-event true})
  (doseq [side [:contestant :challenger]]
    (when (-> @state side :identity :title)
      (show-prompt state side nil "Keep hand?"
                   ["Keep" "Mulligan" "Interrupt"]
                   #(if (= % "Keep")
                      (keep-hand state side nil)
                      (if (= % "Mulligan")
                        (mulligan state side nil)
                        (interrupt state side nil)))))))

(defn init-game
  "Initializes a new game with the given players vector."
  [{:keys [players gameid spectatorhands room] :as game}]
  (let [contestant (some #(when (= (:side %) "Contestant") %) players)
        challenger (some #(when (= (:side %) "Challenger") %) players)
        contestant-pool (create-pool "Contestant" (:deck contestant) (:user contestant))
        challenger-pool (create-pool "Challenger" (:deck challenger) (:user challenger))
        contestant-deck (create-deck "Contestant" (:deck contestant) (:user contestant))
        challenger-deck (create-deck "Challenger" (:deck challenger) (:user challenger))
        contestant-board (create-board "Contestant" (:deck contestant) (:user contestant))
        challenger-board (create-board "Challenger" (:deck challenger) (:user challenger))
        contestant-fw-dc (create-fw-dc "Contestant" (:deck contestant) (:user contestant))
        challenger-fw-dc (create-fw-dc "Challenger" (:deck challenger) (:user challenger))
        contestant-location (create-location "Contestant" (:deck contestant) (:user contestant))
        challenger-location (create-location "Challenger" (:deck challenger) (:user challenger))
        contestant-deck-id (get-in contestant [:deck :_id])
        challenger-deck-id (get-in challenger [:deck :_id])
        contestant-options (get-in contestant [:options])
        challenger-options (get-in challenger [:options])
        contestant-identity (assoc (or (get-in contestant [:deck :identity]) {:type "Identity"}) :cid (make-cid))
        contestant-identity (assoc contestant-identity :side "Contestant" :implementation (card-implemented contestant-identity))
        challenger-identity (assoc (or (get-in challenger [:deck :identity]) {:side "Challenger" :type "Identity"}) :cid (make-cid))
        challenger-identity (assoc challenger-identity :side "Challenger" :implementation (card-implemented challenger-identity))
        state (atom
                {:gameid gameid :log [] :active-player :challenger :end-turn true
                 :room room
                 :rid 0 :turn 0 :eid 0
                 :sfx [] :sfx-current-id 0
                 :options {:spectatorhands spectatorhands}
                 :contestant {:user (:user contestant) :identity contestant-identity
                              :options contestant-options
                              :deck (zone :deck contestant-deck)
                              :deck-id contestant-deck-id
                              :hand (zone :hand contestant-pool)
                              :sideboard (zone :sideboard contestant-board)
                              :fw-dc-sb (zone :fw-dc-sb contestant-fw-dc)
                              :location (zone :location contestant-location)
                              :discard [] :scored [] :rfg [] :play-area []
                              :locales {}
                              :rig {:resource [] :muthereff [] :hazard []}
                              :click 0 :credit 20 :bad-publicity 0 :has-bad-pub 0
                              :free_gi 0 :total_mp 0 :stage_pt 0
                              :char_mp 0 :ally_mp 0 :item_mp 0
                              :fact_mp 0 :kill_mp 0 :misc_mp 0
                              :toast []
                              :hand-size-base 8 :hand-size-modification 0
                              :agenda-point 0
                              :click-per-turn 100 :agenda-point-req 7 :keep false}
                 :challenger {:user (:user challenger) :identity challenger-identity
                              :options challenger-options
                              :deck (zone :deck challenger-deck)
                              :deck-id challenger-deck-id
                              :hand (zone :hand challenger-pool)
                              :sideboard (zone :sideboard challenger-board)
                              :fw-dc-sb (zone :fw-dc-sb challenger-fw-dc)
                              :location (zone :location challenger-location)
                              :discard [] :scored [] :rfg [] :play-area []
                              :locales {}
                              :rig {:resource [] :muthereff [] :hazard []}
                              :toast []
                              :click 0 :credit 20 :run-credit 0 :memory 4 :link 0 :tag 0
                              :free_gi 0 :total_mp 0 :stage_pt 0
                              :char_mp 0 :ally_mp 0 :item_mp 0
                              :fact_mp 0 :kill_mp 0 :misc_mp 0
                              :hand-size-base 8 :hand-size-modification 0
                              :agenda-point 0
                              :hq-access 1 :rd-access 1 :tagged 0
                              :brain-damage 0 :click-per-turn 100 :agenda-point-req 7 :keep false}})]
    (init-identity state :contestant contestant-identity)
    (init-identity state :challenger challenger-identity)
    (swap! game-states assoc gameid state)
    (let [side :contestant]
      (when-completed (trigger-event-sync state side :pre-start-game)
                      (let [side :challenger]
                        (when-completed (trigger-event-sync state side :pre-start-game)
                                        (init-hands state))))) @game-states))

(defn locale-card
  ([ImageName] (@all-cards ImageName))
  ([ImageName user]
   (@all-cards ImageName)))

(defn make-card
  "Makes or remakes (with current cid) a proper card from an @all-cards card"
  ([card] (make-card card (make-cid)))
  ([card cid]
  (-> card
      (assoc :cid cid :implementation (card-implemented card))
      (dissoc :text :_id :influence :number :influencelimit :factioncost))))

(defn reset-card
  "Resets a card back to its original state overlaid with any play-state data"
  ([state side card]
   (update! state side (merge card (make-card (get @all-cards (:ImageName card)) (:cid card))))))

(defn create-pool
  "Creates a the character pool for the character draft from the pool
  section of the deck.  These will be loaded into the hand"
  ([side deck] (create-pool side deck nil))
  ([side deck user]
   (mapcat #(map (fn [card]
                   (let [locale-card (or (locale-card (:ImageName card) user) card)
                         c (assoc (make-card locale-card) :side side :art (:art card))]
                     (if-let [init (:init (card-def c))] (merge c init) c)))
                 (repeat (:qty %) (assoc (:card %) :art (:art %))))
           (vec (:pool deck)))))

(defn create-deck
  "Creates a shuffled draw deck from the given list of cards.
  Loads card data from locale-side @all-cards map if available."
  ([side deck] (create-deck side deck nil))
  ([side deck user]
   (shuffle (mapcat #(map (fn [card]
                            (let [locale-card (or (locale-card (:ImageName card) user) card)
                                  c (assoc (make-card locale-card) :side side :art (:art card))]
                              (if-let [init (:init (card-def c))] (merge c init) c)))
                          (repeat (:qty %) (assoc (:card %) :art (:art %))))
                    (shuffle (vec (:cards deck)))))))

(defn create-board
  "Creates a shuffled draw deck from the given list of cards.
  Loads card data from locale-side @all-cards map if available."
  ([side deck] (create-board side deck nil))
  ([side deck user]
   (mapcat #(map (fn [card]
                   (let [locale-card (or (locale-card (:ImageName card) user) card)
                         c (assoc (make-card locale-card) :side side :art (:art card))]
                     (if-let [init (:init (card-def c))] (merge c init) c)))
                 (repeat (:qty %) (assoc (:card %) :art (:art %))))
           (vec (:sideboard deck)))))

(defn create-fw-dc
  "Creates a shuffled draw deck from the given list of cards.
  Loads card data from locale-side @all-cards map if available."
  ([side deck] (create-fw-dc side deck nil))
  ([side deck user]
   (mapcat #(map (fn [card]
                   (let [locale-card (or (locale-card (:ImageName card) user) card)
                         c (assoc (make-card locale-card) :side side :art (:art card))]
                     (if-let [init (:init (card-def c))] (merge c init) c)))
                 (repeat (:qty %) (assoc (:card %) :art (:art %))))
           (vec (:fwsb deck)))))

(defn create-location
  "Creates a shuffled draw deck from the given list of cards.
  Loads card data from locale-side @all-cards map if available."
  ([side deck] (create-location side deck nil))
  ([side deck user]
   (mapcat #(map (fn [card]
                   (let [locale-card (or (locale-card (:ImageName card) user) card)
                         c (assoc (make-card locale-card) :side side :art (:art card))]
                     (if-let [init (:init (card-def c))] (merge c init) c)))
                 (repeat (:qty %) (assoc (:card %) :art (:art %))))
           (vec (:location deck)))))

(defn make-rid
  "Returns a progressively-increasing integer to identify a new party locale."
  [state]
  (get-in (swap! state update-in [:rid] inc) [:rid]))

(defn make-eid
  [state]
  {:eid (:eid (swap! state update-in [:eid] inc))})

(defn make-result
  [eid result]
  (assoc eid :result result))

(defn interrupt
  "Mulligan starting hand."
  [state side args]
  ;;  (swap! state assoc-in [side :keep] true) ;; was true
  (system-msg state side "interrupts for placement")
  (show-prompt state side nil "Keep hand?"
               ["Keep" "Mulligan" "Interrupt"]
               #(if (= % "Keep")
                  (keep-hand state side nil)
                  (if (= % "Mulligan")
                    (mulligan state side nil)
                    (interrupt state side nil)))))

(defn mulligan
  "Mulligan starting hand."
  [state side args]
  (shuffle-into-deck state side :hand)
  (draw state side 8 {:suppress-event true}) ;; was true
  (let [card (get-in @state [side :identity])]
    (when-let [cdef (card-def card)]
      (when-let [mul (:mulligan cdef)]
        (mul state side (make-eid state) card nil))))
;;  (swap! state assoc-in [side :keep] true) ;; was true
  (system-msg state side "takes a mulligan")
  (show-prompt state side nil "Keep hand?"
               ["Keep" "Mulligan" "Interrupt"]
               #(if (= % "Keep")
                  (keep-hand state side nil)
                  (if (= % "Mulligan")
                    (mulligan state side nil)
                    (interrupt state side nil)))))
  ;;(trigger-event state side :pre-first-turn)
  ;;(when (and (= side :contestant) (-> @state :challenger :identity :title))
    ;;(clear-wait-prompt state :challenger)
    ;;(show-wait-prompt state :contestant "Challenger to keep hand or mulligan"))
  ;;(when (and (= side :challenger)  (-> @state :contestant :identity :title))
    ;;(clear-wait-prompt state :contestant)))

(defn keep-hand
  "Choose not to mulligan."
  [state side args]
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "keeps their hand")
  (trigger-event state side :pre-first-turn)
  (when (and (= side :contestant) (-> @state :challenger :identity :title))
    (clear-wait-prompt state :challenger))
  (when (and (= side :challenger)  (-> @state :contestant :identity :title))
    (clear-wait-prompt state :contestant)))

(defn end-phase-12
  "End phase 1.2 and trigger appropriate events for the player."
  [state side args]
  (turn-message state side true)
  (let [extra-clicks (or (get-in @state [side :extra-click-temp]) 0)]
    (gain state side :click (get-in @state [side :click-per-turn]))
    (when-completed (trigger-event-sync state side (if (= side :contestant) :contestant-turn-begins :challenger-turn-begins))
                    (do (when (= side :contestant)
                          ;;(draw state side)
                          ;;(trigger-event state side :contestant-mandatory-draw)
                          )

                        (cond

                          (< extra-clicks 0)
                          (lose state side :click (abs extra-clicks))

                          (> extra-clicks 0)
                          (gain state side :click extra-clicks))

                        (swap! state dissoc-in [side :extra-click-temp])
                        (swap! state dissoc (if (= side :contestant) :contestant-phase-12 :challenger-phase-12))
                        (when (= side :contestant)
                          (update-all-advancement-costs state side))))))

(defn start-turn
  "Start turn."
  [state side args]
  (when (= side :contestant)
    (swap! state update-in [:turn] inc))

  (doseq [c (filter #(:new %) (all-placed state side))]
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

;; --- For what goes on between Start/End ;)
;; Organization Phase
(defn untap-all
  [state side args]
  (system-msg state side "untaps")
  (gain state side :click -5)
  )
(defn org-phase
  [state side args]
  (system-msg state side "is done organizing, Long-event Phase")
  (gain state side :click -5)
  )
(defn m-h-phase
  [state side args]
  (system-msg state side "enters the Movement/Hazard Phase")
  (gain state side :click -5)
  )
;; Movement/Hazard Phase
(defn back-org
  [state side args]
  (system-msg state side "goes back to the Organization Phase")
  (gain state side :click 15)
  )
(defn next-m-h
  [state side args]
  (system-msg state side "goes to the next companies' M/H Phase")
  )
(defn site-phase
  [state side args]
  (system-msg state side "goes to the Site Phase")
  (gain state side :click -5)
  )
;; Site Phase
(defn back-m-h
  [state side args]
  (system-msg state side "goes back the M/H Phase")
  (gain state side :click 5)
  )
(defn next-site
  [state side args]
  (system-msg state side "goes to the next companies' Site Phase")
  )
(defn eot-phase
  [state side args]
  (system-msg state side "goes to the beginning of the End-of-turn")
  (gain state side :click -5)
  )
;; End of Turn Phase
(defn back-site
  [state side args]
  (system-msg state side "goes back the Site Phase")
  (gain state side :click 5)
  )
(defn eot-discard
  [state side args]
  (system-msg state side "acknowledges EOT discard")
  (gain state side :click -5)
  )
(defn eot-resolve
  [state side args]
  (system-msg state side "acknowledges End-of-turn")
  (gain state side :click -70)
  )

;;--- Hazard Phase Replies
;; Organization Phase
(defn reset-org
  [state side args]
  (gain state side :click -60)
  )
(defn wait-alert
  [state side args]
  (system-msg state side "WAIT! WAIT! WAIT!")
  )
;; Movement/Hazard Phase
(defn on-guard
  [state side args]
  (gain state side :click -5)
  )
(defn no-hazards
  [state side args]
  (system-msg state side "says no more hazards for this company")
  (if (= (get-in @state [side :click]) 45)
    (gain state side :click -10)
    (gain state side :click -5)
    )
  )
(defn reset-m-h
  [state side args]
  (if (= (get-in @state [side :click]) 35)
    (gain state side :click 10)
    (gain state side :click 45)
    )
  )
;; Site Phase
(defn reveal-o-g
  []
  )
(defn bluff-o-g
  []
  )
(defn reset-site
  []
  )
;; End of Turn Phase
(defn reset-done
  []
  )
(defn haz-play-done
  []
  )

(defn end-turn [state side args]
  (let [max-hand-size (max (hand-size state side) 0)]
    (when (<= (count (get-in @state [side :hand])) max-hand-size)
      (turn-message state side false)
      (if (= side :challenger)
        (do (when (neg? (hand-size state side))
              (flatline state))
            (trigger-event state side :challenger-turn-ends))
        (trigger-event state side :contestant-turn-ends))
      (gain state side :click -70)
      (doseq [a (get-in @state [side :register :end-turn])]
        (resolve-ability state side (:ability a) (:card a) (:targets a)))
      (swap! state assoc-in [side :register-last-turn] (-> @state side :register))
      (let [rig-cards (apply concat (vals (get-in @state [:challenger :rig])))
            hosted-cards (filter :placed (mapcat :hosted rig-cards))
            hosted-on-character (->> (get-in @state [:contestant :locales]) seq flatten (mapcat :characters) (mapcat :hosted))]
        (doseq [card (concat rig-cards hosted-cards hosted-on-character)]
          ;; Clear the added-virus-counter flag for each virus in play.
          ;; We do this even on the contestant's turn to prevent shenanigans with something like Gorman Drip and Surge
          (when (has-subtype? card "Virus")
            (set-prop state :challenger card :added-virus-counter false))))
      (swap! state assoc :end-turn true)
      (swap! state update-in [side :register] dissoc :cannot-draw)
      (swap! state update-in [side :register] dissoc :drawn-this-turn)
      (doseq [c (filter #(= :this-turn (:revealed %)) (all-placed state :contestant))]
        (update! state side (assoc c :revealed true)))
      (clear-turn-register! state)
      (swap! state dissoc :turn-events)
      (when-let [extra-turns (get-in @state [side :extra-turns])]
        (when (> extra-turns 0)
          (start-turn state side nil)
          (swap! state update-in [side :extra-turns] dec)
          (let [turns (if (= 1 extra-turns) "turn" "turns")]
            (system-msg state side (clojure.string/join ["will have " extra-turns " extra " turns " remaining."]))))))))
