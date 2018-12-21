(in-ns 'game.core)

(declare all-active card-flag-fn? clear-turn-register! clear-wait-prompt create-pool create-deck create-board create-fw-dc create-location
         in-hand? challenger-place contestant-place all-placed-challenger reveal untap hand-size keep-hand mulligan
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
    ; Card is defined - hence implemented
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
  ;(draw state :contestant 8 {:suppress-event true})
  ;(draw state :challenger 8 {:suppress-event true})
  (doseq [side [:contestant :challenger]]
    (when (-> @state side :identity :title)
      (system-msg state side "Drafting Begins"))))

(defn- init-game-state
  "Initializes the game state"
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
        contestant-identity (assoc (or (get-in contestant [:deck :identity]) {:side "Contestant" :type "Identity"}) :cid (make-cid))
        contestant-identity (assoc contestant-identity :side "Contestant" :implementation (card-implemented contestant-identity))
        challenger-identity (assoc (or (get-in challenger [:deck :identity]) {:side "Challenger" :type "Identity"}) :cid (make-cid))
        challenger-identity (assoc challenger-identity :side "Challenger" :implementation (card-implemented challenger-identity))
        contestant-dice-pick (get-in contestant [:deck :donate-dice])
        contestant-dice-size (get-in contestant [:deck :donate-size])
        challenger-dice-pick (get-in challenger [:deck :donate-dice])
        challenger-dice-size (get-in challenger [:deck :donate-size])
        ]
    (atom
      {:gameid gameid :log [] :active-player :challenger :end-turn true
       :room room :save-pref ""
       :rid 0 :turn 0 :eid 0
       :sfx [] :sfx-current-id 0
       :stats {:time {:started (t/now)}}
       :options {:spectatorhands spectatorhands}
       :contestant {:user (:user contestant) :identity contestant-identity
                    :deck-dice contestant-dice-pick
                    :deck-mmsz contestant-dice-size
                    :options contestant-options
                    :deck (zone :deck contestant-deck)
                    :deck-id contestant-deck-id
                    :hand (zone :hand contestant-pool)
                    :sideboard (zone :sideboard contestant-board)
                    :fw-dc-sb (zone :fw-dc-sb contestant-fw-dc)
                    :location (zone :location contestant-location)
                    :discard [] :scored [] :rfg [] :play-area [] :current []
                    :locales {:hq {} :rd {} :archives {} :sites {}}
                    :rig {:resource [] :radicle [] :hazard []}
                    :click 0 :credit 20 :bad-publicity 0 :has-bad-pub 0
                    :free_gi 0 :total_mp 0 :stage_pt 0
                    :char_mp 0 :ally_mp 0 :item_mp 0
                    :fact_mp 0 :kill_mp 0 :misc_mp 0
                    :toast [] :blind false :hold-card false :opt-key false
                    :hand-size-base 6 :hand-size-modification 2 :hpf false
                    :agenda-point 0
                    :click-per-turn 100 :agenda-point-req 7
                    :keep false :drew false :eot false}
       :challenger {:user (:user challenger) :identity challenger-identity
                    :deck-dice challenger-dice-pick
                    :deck-mmsz challenger-dice-size
                    :options challenger-options
                    :deck (zone :deck challenger-deck)
                    :deck-id challenger-deck-id
                    :hand (zone :hand challenger-pool)
                    :sideboard (zone :sideboard challenger-board)
                    :fw-dc-sb (zone :fw-dc-sb challenger-fw-dc)
                    :location (zone :location challenger-location)
                    :discard [] :scored [] :rfg [] :play-area [] :current []
                    :locales {:hq {} :rd {} :archives {} :sites {}}
                    :rig {:resource [] :radicle [] :hazard []}
                    :toast [] :blind false :hold-card false :opt-key false
                    :click 0 :credit 20 :run-credit 0 :memory 4 :link 0 :tag 0
                    :free_gi 0 :total_mp 0 :stage_pt 0
                    :char_mp 0 :ally_mp 0 :item_mp 0
                    :fact_mp 0 :kill_mp 0 :misc_mp 0
                    :hand-size-base 6 :hand-size-modification 2 :hpf false
                    :agenda-point 0
                    :hq-access 1 :rd-access 1 :tagged 0
                    :click-per-turn 100 :agenda-point-req 7
                    :keep false :drew false :eot false}})))

(defn load-game
  "Initializes a new game with the given players vector."
  [game save-pref]
  (let [state (atom game)
        contestant-identity (get-in @state [:contestant :identity])
        challenger-identity (get-in @state [:challenger :identity])]
    (init-identity state :contestant contestant-identity)
    (init-identity state :challenger challenger-identity)
    (swap! state assoc :save-pref save-pref)
    (let [side :contestant]
      (swap! state assoc-in [side :hold-card] false)
      (swap! state assoc-in [side :keep] true)
      (swap! state assoc-in [side :drew] true)
      (swap! state assoc :active-player side :per-turn nil :end-turn true)
      (let [offset (* -1 (get-in @state [side :click]))]
        (gain state side :click offset)))
    (let [side :challenger]
      (swap! state assoc-in [side :hold-card] false)
      (swap! state assoc-in [side :keep] true)
      (swap! state assoc-in [side :drew] true)
      (let [offset (* -1 (get-in @state [side :click]))]
        (gain state side :click offset)))
    state))

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
                            (init-hands state))))) state))

(defn server-card
  ([ImageName] (@all-cards ImageName))
  ([ImageName user]
   (@all-cards ImageName)))

(defn make-card
  "Makes or remakes (with current cid) a proper card from an @all-cards card"
  ([card] (make-card card (make-cid)))
  ([card cid]
   (-> card
       (assoc :cid cid :implementation (card-implemented card)))))

(defn reset-card
  "Resets a card back to its original state - retaining any data in the :persistent key"
  ([state side card]
   (update! state side (merge (make-card (get @all-cards (:ImageName card)) (:cid card)) {:persistent card}))))

(defn create-pool
  "Creates a the character pool for the character draft from the pool
  section of the deck.  These will be loaded into the hand"
  ([side deck] (create-pool side deck nil))
  ([side deck user]
   (mapcat #(map (fn [card]
                   (let [server-card (or (server-card (:ImageName card) user) card)
                         c (assoc (make-card server-card) :side side :id (:trimCode card))]
                     (if-let [init (:init (card-def c))] (merge c init) c)))
                 (repeat (:qty %) (assoc (:card %) :id (:trimCode %))))
           (vec (:pool deck)))))

(defn create-deck
  "Creates a shuffled draw deck from the given list of cards.
  Loads card data from locale-side @all-cards map if available."
  ([side deck] (create-deck side deck nil))
  ([side deck user]
   (shuffle (mapcat #(map (fn [card]
                            (let [server-card (or (server-card (:ImageName card) user) card)
                                  c (assoc (make-card server-card) :side side :id (:id card))]
                              (if-let [init (:init (card-def c))] (merge c init) c)))
                          (repeat (:qty %) (assoc (:card %) :id (:id %))))
                    (shuffle (vec (into (into (:resources deck) (:hazards deck)) (:characters deck))))))))

(defn create-board
  "Creates a shuffled draw deck from the given list of cards.
  Loads card data from locale-side @all-cards map if available."
  ([side deck] (create-board side deck nil))
  ([side deck user]
   (mapcat #(map (fn [card]
                   (let [server-card (or (server-card (:ImageName card) user) card)
                         c (assoc (make-card server-card) :side side :id (:id card))]
                     (if-let [init (:init (card-def c))] (merge c init) c)))
                 (repeat (:qty %) (assoc (:card %) :id (:id %))))
           (vec (:sideboard deck)))))

(defn create-fw-dc
  "Creates a shuffled draw deck from the given list of cards.
  Loads card data from locale-side @all-cards map if available."
  ([side deck] (create-fw-dc side deck nil))
  ([side deck user]
   (mapcat #(map (fn [card]
                   (let [server-card (or (server-card (:ImageName card) user) card)
                         c (assoc (make-card server-card) :side side :id (:id card))]
                     (if-let [init (:init (card-def c))] (merge c init) c)))
                 (repeat (:qty %) (assoc (:card %) :id (:id %))))
           (vec (:fwsb deck)))))

(defn create-location
  "Creates a shuffled draw deck from the given list of cards.
  Loads card data from locale-side @all-cards map if available."
  ([side deck] (create-location side deck nil))
  ([side deck user]
   (mapcat #(map (fn [card]
                   (let [server-card (or (server-card (:ImageName card) user) card)
                         c (assoc (make-card server-card) :side side :id (:id card))]
                     (if-let [init (:init (card-def c))] (merge c init) c)))
                 (repeat (:qty %) (assoc (:card %) :id (:id %))))
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

(defn mulligan
  "Mulligan/Draw Hand starting hand."
  [state side args]
  (swap! state assoc-in [side :drew] true)
  (shuffle-into-deck state side :hand)
  (draw state side 8 {:suppress-event true}) ; was true
  (system-msg state side "draws a new hand"))

;(trigger-event state side :pre-first-turn)
;(when (and (= side :contestant) (-> @state :challenger :identity :title))
;(clear-wait-prompt state :challenger)
;(show-wait-prompt state :contestant "Challenger to keep hand or mulligan"))
;(when (and (= side :challenger)  (-> @state :contestant :identity :title))
;(clear-wait-prompt state :contestant)))

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
  ([state side args] (end-phase-12 state side (make-eid state) args))
  ([state side eid args]
   (turn-message state side true)
   (let [extra-clicks (get-in @state [side :extra-click-temp] 0)]
     (gain state side :click (get-in @state [side :click-per-turn]))
     (wait-for (trigger-event-sync state side (if (= side :contestant) :contestant-turn-begins :challenger-turn-begins))
               (do ;(when (= side :contestant)
                 ;(wait-for (draw state side 1 nil)
                 ; (trigger-event-simult state side eid :contestant-mandatory-draw nil nil)))

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

  (swap! state assoc-in [:contestant :eot] false)
  (swap! state assoc-in [:challenger :eot] false)
  ; Functions to set up state for undo-turn functionality
  (doseq [s [:challenger :contestant]] (swap! state dissoc-in [s :undo-turn]))
  (swap! state assoc :turn-state (dissoc @state :log))

  (swap! state update-in [:turn] inc)

  (doseq [c (filter :new (all-placed state side))]
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

(defn opp-side [side]
  (if (= side :contestant)
    :challenger
    :contestant))

;;--- Hazard Phase Replies
;; Organization Phase
(defn reset-org
  [state side args]
  (let [offset (* -1 (get-in @state [side :click]))]
    (gain state side :click offset))
  )
(defn wait-alert
  [state side args]
  (system-msg state side "WAIT! WAIT! WAIT!")
  )
;; Movement/Hazard Phase
(defn on-guard
  [state side args]
  (resolve-ability state side
                   {:prompt "Select a site for on-guard card"
                    :choices {:req #(is-type? % "Site")}
                    :msg (msg "host " (:title target))
                    :effect (effect (resolve-ability (let [site target] {:req (req (zero? (count (:hosted target))))
                                                                         :prompt "Select a card for on-guard"
                                                                         :choices {:req #(in-hand? %)}
                                                                         :effect (req (contestant-place state side target site) ; install target onto card
                                                                                      )
                                                                         }) nil nil))} nil nil)
  (system-msg state side "plays an on-guard card")
  (gain state side :click -5)
  )
(defn no-hazards
  [state side args]
  (system-msg state side "says NO MORE HAZARDS for this company")
  (let [offset (- 35 (get-in @state [side :click]))]
    (gain state side :click offset))
  )
(defn reset-m-h
  [state side args]
  (let [offset (- 45 (get-in @state [side :click]))]
    (gain state side :click offset))
  )

;; Site Phase
(defn reveal-o-g
  [state side args]
  (resolve-ability state side
                   {:effect (effect (reveal target {:ignore-cost :all-costs :force true}))
                    :choices {:req (fn [t] (card-is? t :side side))}}
                   nil nil)
  (system-msg state side "reveals an on-guard card")
  (let [offset (- 20 (get-in @state [side :click]))]
    (gain state side :click offset))
  )
(defn pre-bluff
  [state side args]
  (system-msg state side "says on-guard is possible effect")
  (gain state side :click -3)
  )
(defn bluff-o-g
  [state side args]
  (resolve-ability state side
                   {:prompt "Select a card to move to your hand"
                    :effect (req (let [c (deactivate state side target)]
                                   (move state side c :hand)))
                    :choices {:req (fn [t] (card-is? t :side side))}}
                   nil nil)
  (system-msg state side "nope, returns a bluff to his hand")
  (gain state side :click -2)
  )
(defn reset-site
  [state side args]
  (let [offset (- 25 (get-in @state [side :click]))]
    (gain state side :click offset))
  )
;; End of Turn Phase
(defn reset-done
  [state side args]
  (let [offset (* -1 (get-in @state [side :click]))]
    (gain state side :click offset))
  )
(defn return-o-g
  [state side args]
  (resolve-ability state side
                   {:prompt "Select a card to move to your hand"
                    :effect (req (let [c (deactivate state side target)]
                                   (move state side c :hand)))
                    :choices {:req (fn [t] (card-is? t :side side))}}
                   nil nil)
  (system-msg state side "returns an on-guard card to his hand")
  )
(defn haz-play-done
  [state side args]
  (swap! state assoc-in [side :eot] true)
  (system-msg state side "has NOTHING ELSE")
  )

;; Organization Phase

(defn untap-all
  [state side args]
  (resolve-ability state side
                   {:effect (req
                              (if (= side :contestant)
                                (doseq [c (all-placed state side)]
                                  (when (and (not (:wounded c))
                                             (or (:tapped c) (:rotated c))
                                             (not (boolean (re-find #"Site" (:type c))))
                                             (not (boolean (re-find #"Region" (:type c))))
                                             (not (boolean (re-find #"Permanent" (:Secondary c))))
                                             )
                                             (untap state side c)))
                                (doseq [c (all-placed-challenger state side)]
                                  (when (and (not (:wounded c))
                                             (or (:tapped c) (:rotated c))
                                             (not (boolean (re-find #"Site" (:type c))))
                                             (not (boolean (re-find #"Region" (:type c))))
                                             (not (boolean (re-find #"Permanent" (:Secondary c))))
                                             )
                                             (untap state side c))))
                              )}
                   nil nil)

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
  (reset-m-h state (opp-side side) nil)
  )
; Movement/Hazard Phase
(defn back-org
  [state side args]
  (system-msg state side "goes back to the Organization Phase")
  (let [offset (- 100 (get-in @state [side :click]))]
    (gain state side :click offset))
  (reset-org state (opp-side side) nil)
  )
(defn next-m-h
  [state side args]
  (system-msg state side "goes to the next companies' M/H Phase")
  )
(defn site-phase
  [state side args]
  (system-msg state side "goes to the Site Phase")
  (gain state side :click -5)
  (reset-site state (opp-side side) nil)
  )
; Site Phase
(defn back-m-h
  [state side args]
  (system-msg state side "goes back the M/H Phase")
  (let [offset (- 85 (get-in @state [side :click]))]
    (gain state side :click offset))
  (reset-m-h state (opp-side side) nil)
  )
(defn next-site
  [state side args]
  (system-msg state side "goes to the next companies' Site Phase")
  )
(defn eot-phase
  [state side args]
  (system-msg state side "goes to the beginning of the End-of-turn")
  (gain state side :click -5)
  (reset-done state (opp-side side) nil)
  )
;; End of Turn Phase
(defn back-site
  [state side args]
  (system-msg state side "goes back the Site Phase")
  (let [offset (- 80 (get-in @state [side :click]))]
    (gain state side :click offset))
  (reset-site state (opp-side side) nil)
  )
(defn eot-discard
  [state side args]
  (swap! state assoc-in [side :eot] true)
  (system-msg state side "acknowledges EOT discard")
  (gain state side :click -5)
  )

(defn end-turn
  ([state side args] (end-turn state side (make-eid state) args))
  ([state side eid args]
   (turn-message state side false)
   (wait-for
     (trigger-event-sync state side (if (= side :challenger) :challenger-turn-ends :contestant-turn-ends))
     (do
       (gain state side :click -70)
       (doseq [a (get-in @state [side :register :end-turn])]
         (resolve-ability state side (:ability a) (:card a) (:targets a)))
       (swap! state assoc-in [side :register-last-turn] (-> @state side :register))
       (swap! state assoc :end-turn true)
       (swap! state update-in [side :register] dissoc :cannot-draw)
       (swap! state update-in [side :register] dissoc :drawn-this-turn)
       (doseq [c (filter #(= :this-turn (:revealed %)) (all-placed state :contestant))]
         (update! state side (assoc c :revealed true)))
       (clear-turn-register! state)
       (swap! state dissoc :turn-events)
       (when-let [extra-turns (get-in @state [side :extra-turns])]
         (when (pos? extra-turns)
           (start-turn state side nil)
           (swap! state update-in [side :extra-turns] dec)
           (let [turns (if (= 1 extra-turns) "turn" "turns")]
             (system-msg state side (clojure.string/join ["will have " extra-turns " extra " turns " remaining."])))))
       (effect-completed state side eid)))))

(defn not-first
  [state side args]
  (start-turn state side args)
  (swap! state update-in [:turn] dec)
  (swap! state assoc-in [:contestant :eot] true)
  (swap! state assoc-in [:challenger :eot] true)
  (system-msg state side "passes first turn")
  (let [offset (- 70 (get-in @state [side :click]))]
    (gain state side :click offset))
  (end-turn state side args)
  )