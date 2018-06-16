(in-ns 'game.core)

(declare expose-prevent)

;;; Site-specific helpers
(defn installed-access-trigger
  "Effect for triggering ambush on access.
  Ability is what happends upon access. If cost is specified Contestant needs to pay that to trigger."
  ([cost ability]
   (let [ab (if (> cost 0) (assoc ability :cost [:credit cost]) ability)
         prompt (if (> cost 0)
                  (req (str "Pay " cost " [Credits] to use " (:title card) " ability?"))
                  (req (str "Use " (:title card) " ability?")))]
     (installed-access-trigger cost ab prompt)))
  ([cost ability prompt]
   {:access {:req (req (and installed (>= (:credit contestant) cost)))
             :delayed-completion true
             :effect (effect (show-wait-prompt :challenger (str "Contestant to use " (:title card)))
                             (continue-ability
                              {:optional
                               {:prompt prompt
                                :yes-ability ability
                                :end-effect (effect (clear-wait-prompt :challenger))}}
                              card nil))}}))

(defn advance-ambush
  "Creates advanceable ambush structure with specified ability for specified cost"
  ([cost ability] (assoc (installed-access-trigger cost ability) :advanceable :always))
  ([cost ability prompt] (assoc (installed-access-trigger cost ability prompt)
                           :advanceable :always)))

(defn campaign
  "Creates a Campaign with X counters draining Y per-turn.
  Trashes itself when out of counters"
  [counters per-turn]
  (let [ability {:msg (str "gain " per-turn " [Credits]")
                 :counter-cost [:credit per-turn]
                 :once :per-turn
                 :req (req (:contestant-phase-12 @state))
                 :label (str "Gain " per-turn " [Credits] (start of turn)")
                 :effect (req (gain state :contestant :credit per-turn)
                              (when (zero? (get-in card [:counter :credit]))
                                (trash state :contestant card)))}]
    {:effect (effect (add-counter card :credit counters))
     :derezzed-events {:challenger-turn-ends contestant-rez-toast}
     :events {:contestant-turn-begins ability}
     :abilities [ability]}))

(defn as-trashed-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points after resolving the trash prompt."
  ([state side card n] (as-trashed-agenda state side card n nil))
  ([state side card n options]
  (or (move state :challenger (assoc (deactivate state side card) :agendapoints n) :scored options) ; if the challenger did not trash the card on access, then this will work
      (move state :challenger (assoc (deactivate state side card) :agendapoints n :zone [:discard]) :scored options)) ; allow force option in case of Blacklist/News Team
  (gain-agenda-point state side n)))

;;; Card definitions
(declare in-server?)

(def cards-assets
  {})