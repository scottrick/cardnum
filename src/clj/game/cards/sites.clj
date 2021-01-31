(ns game.cards.sites
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int]]
            [cardnum.cards :refer [all-cards]]))

;;; Site-specific helpers
(defn placed-access-trigger
  "Effect for triggering ambush on access.
  Ability is what happends upon access. If cost is specified Contestant needs to pay that to trigger."
  ([cost ability]
   (let [ab (if (pos? cost) (assoc ability :cost [:credit cost]) ability)
         prompt (if (pos? cost)
                  (req (str "Pay " cost " [Credits] to use " (:title card) " ability?"))
                  (req (str "Use " (:title card) " ability?")))]
     (placed-access-trigger cost ab prompt)))
  ([cost ability prompt]
   {:access {:req (req (and placed (>= (:credit contestant) cost)))
             :async true
             :effect (effect (show-wait-prompt :challenger (str "Contestant to use " (:title card)))
                             (continue-ability
                              {:optional
                               {:prompt prompt
                                :yes-ability ability
                                :end-effect (effect (clear-wait-prompt :challenger))}}
                              card nil))}}))

(defn advance-ambush
  "Creates advanceable ambush structure with specified ability for specified cost"
  ([cost ability] (assoc (placed-access-trigger cost ability) :advanceable :always))
  ([cost ability prompt] (assoc (placed-access-trigger cost ability prompt) :advanceable :always)))

(defn campaign
  "Creates a Campaign with X counters draining Y per-turn.
  Discards itself when out of counters"
  [counters per-turn]
  (let [ability {:msg (str "gain " per-turn " [Credits]")
                 :counter-cost [:credit per-turn]
                 :once :per-turn
                 :req (req (:contestant-phase-12 @state))
                 :label (str "Gain " per-turn " [Credits] (start of turn)")
                 :effect (req (when (zero? (get-counters card :credit))
                                (discard state :contestant card)))}]
    {:effect (effect (add-counter card :credit counters))
     :hidden-events {:challenger-turn-ends contestant-reveal-toast}
     :events {:contestant-turn-begins ability}
     :abilities [ability]}))

(defn as-discarded-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points after resolving the discard prompt."
  ([state side eid card n] (as-discarded-agenda state side eid card n nil))
  ([state side eid card n options]
  (or
    ; if the challenger did not discard the card on access, then this will work
    (move state :challenger (assoc (deactivate state side card) :agendapoints n) :scored options)
    ; allow force option in case of Blacklist/News Team
    (move state :challenger (assoc (deactivate state side card) :agendapoints n :zone [:discard]) :scored options))
   (wait-for (trigger-event-sync state side :as-agenda (assoc card :as-agenda-side side :as-agenda-points n))
             (do (gain-agenda-point state side n)
                 (effect-completed state side eid)))))

;;; Card definitions
(def card-definitions
  {})
