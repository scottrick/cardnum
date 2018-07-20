(ns game.cards.sites
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int other-side]]
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
                 :effect (req (take-credits state :contestant per-turn)
                              (when (zero? (get-counters card :credit))
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
  {"Adonis Campaign"
   (campaign 12 3)

   "Advanced Assembly Lines"
   {:effect (effect (gain-credits 3))
    :msg (msg "gain 3 [Credits]")
    :abilities [{:label "[Discard]: Place a non-agenda card from HQ"
                 :effect (effect (discard card {:cause :ability-cost})
                                 (contestant-place target nil))
                 :msg (msg (contestant-place-msg target))
                 :prompt "Select a non-agenda card to place from HQ"
                 :priority true
                 :req (req (not (:run @state)))
                 :choices {:req #(and (not (is-type? % "Operation"))
                                      (not (is-type? % "Agenda"))
                                      (= (:zone %) [:hand])
                                      (= (:side %) "Contestant"))}}]}

   "Aggressive Secretary"
   (advance-ambush 2 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                      :async true
                      :effect (req (let [agg (get-counters (get-card state card) :advancement)
                                         ab (-> discard-resource
                                                (assoc-in [:choices :max] agg)
                                                (assoc :prompt (msg "Choose " (quantify agg "resource") " to discard")
                                                       :async true
                                                       :effect (effect (discard-cards eid targets nil))
                                                       :msg (msg "discard " (join ", " (map :title targets)))))]
                                     (continue-ability state side ab card nil)))})

   "Alexa Belsky"
   {:abilities [{:label "[Discard]: Shuffle all cards in HQ into R&D"
                 :effect (effect (discard card {:cause :ability-cost})
                                 (show-wait-prompt :contestant "Challenger to decide whether or not to prevent Alexa Belsky")
                                 (resolve-ability
                                   {:prompt "Prevent Alexa Belsky from shuffling back in 1 card for every 2 [Credits] spent. How many credits?"
                                    :choices :credit
                                    :player :challenger
                                    :priority 2
                                    :msg (msg "shuffle "
                                              (quantify (- (count (:hand contestant)) (quot target 2)) "card")
                                              " in HQ into R&D")
                                    :effect (req (if (pos? (quot target 2))
                                                   (let [prevented (quot target 2)
                                                         unprevented (- (count (:hand contestant)) prevented)]
                                                     (doseq [c (take unprevented (shuffle (:hand contestant)))]
                                                       (move state :contestant c :deck))
                                                     (when (pos? unprevented)
                                                       (shuffle! state :contestant :deck))
                                                     (system-msg state :challenger
                                                                 (str "pays " target " [Credits] to prevent "
                                                                      (quantify prevented "random card")
                                                                      " in HQ from being shuffled into R&D")))
                                                   (shuffle-into-deck state :contestant :hand)))
                                    :end-effect (effect (clear-wait-prompt :contestant))}
                                   card nil))}]}

   "Alix T4LB07"
   {:events {:contestant-place {:effect (effect (add-counter card :power 1))}}
    :abilities [{:label "Gain 2 [Credits] for each counter on Alix T4LB07"
                 :cost [:click 1]
                 :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                 :effect (effect (discard card {:cause :ability-cost})
                                 (gain-credits (* 2 (get-counters card :power))))}]}

   "Allele Repression"
   {:implementation "Card swapping is manual"
    :advanceable :always
    :abilities [{:label "Swap 1 card in HQ and Archives for each advancement token"
                 :effect (effect (discard card {:cause :ability-cost}))
                 :msg (msg "swap " (get-counters card :advancement) " cards in HQ and Archives")}]}

   "Amani Senai"
   (letfn [(get-last-stolen-pts [state]
             (advancement-cost state :contestant (last (get-in @state [:challenger :scored]))))
           (get-last-scored-pts [state]
             (advancement-cost state :contestant (last (get-in @state [:contestant :scored]))))
           (senai-ability [trace-base-func]
             {:interactive (req true)
              :optional {:prompt "Trace with Amani Senai?"
                         :player :contestant
                         :yes-ability {:trace {:base (req (trace-base-func state))
                                               :successful
                                               {:choices {:req #(and (placed? %)
                                                                     (card-is? % :side :challenger))}
                                                :label "add an placed card to the Grip"
                                                :msg (msg "add " (:title target) " to the Challenger's Grip")
                                                :effect (effect (move :challenger target :hand true))}}}}})]
    {:events {:agenda-scored (senai-ability get-last-scored-pts)
              :agenda-stolen (senai-ability get-last-stolen-pts)}})

   "Anson Rose"
   (let [ability {:label "Place 1 advancement token on Anson Rose (start of turn)"
                  :once :per-turn
                  :effect (effect (system-msg (str "places 1 advancement counter on Anson Rose"))
                                  (add-prop card :advance-counter 1 {:placed true}))}]
     {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :flags {:contestant-phase-12 (req true)}
      :events {:contestant-turn-begins ability
               :reveal {:req (req (and (character? target)
                                    (pos? (get-counters card :advancement))))
                     :async true
                     :effect (req (let [character (get-card state target)
                                        charactername (:title character)]
                                    (show-wait-prompt state :challenger "Contestant to use Anson Rose")
                                    (continue-ability
                                      state side
                                      {:optional
                                       {:prompt (msg "Move advancement tokens from Anson Rose to " charactername "?")
                                        :yes-ability
                                        {:prompt "Choose how many advancement tokens to remove from Anson Rose"
                                         :choices {:number (req (get-counters card :advancement))}
                                         :effect (effect (add-prop :contestant character :advance-counter target {:placed true})
                                                         (add-prop :contestant card :advance-counter (- target) {:placed true})
                                                         (system-msg (str "uses Anson Rose to move " target
                                                                          " advancement tokens to " (card-str state character))))
                                         :end-effect (effect (clear-wait-prompt :challenger))}}}
                                      card nil)))}}
      :abilities [ability]})

   "API-S Keeper Isobel"
    (letfn [(counters-available? [state] (some #(pos? (get-counters % :advancement)) (all-placed state :contestant)))]
      {:flags {:contestant-phase-12 (req (counters-available? state))}
       :abilities [{:req (req (and (:contestant-phase-12 @state)
                                   (counters-available? state)))
                    :once :per-turn
                    :label "Remove an advancement token (start of turn)"
                    :prompt "Select a card to remove an advancement token from"
                    :choices {:req #(and (pos? (get-counters % :advancement))
                                         (placed? %))}
                    :effect (req (let [cnt (get-counters target :advancement)]
                                   (set-prop state side target :advance-counter (dec cnt))
                                   (gain-credits state :contestant 3)
                                   (system-msg state :contestant (str "uses API-S Keeper Isobel to remove an advancement token from "
                                                                (card-str state target) " and gains 3 [Credits]"))))}]})

   "Aryabhata Tech"
   {:events {:successful-trace {:msg "gain 1 [Credit] and force the Challenger to lose 1 [Credit]"
                                :effect (effect (gain-credits 1)
                                                (lose-credits :challenger 1))}}}

   "Bio-Ethics Association"
   (let [ability {:req (req unprotected)
                  :async true
                  :label "Do 1 net damage (start of turn)"
                  :once :per-turn
                  :msg "do 1 net damage"
                  :effect (effect (damage eid :net 1 {:card card}))}]
     {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :events {:contestant-turn-begins ability}
      :abilities [ability]})

   "Bioroid Work Crew"
   {:implementation "Timing restriction of ability use not enforced"
    :abilities [{:label "[Discard]: Place 1 card, paying all costs"
                 :req (req (= (:active-player @state) :contestant))
                 :prompt "Select a card in HQ to place"
                 :choices {:req #(and (not (is-type? % "Operation"))
                                      (in-hand? %)
                                      (= (:side %) "Contestant"))}
                 :effect (effect (discard card {:cause :ability-cost})
                                 (contestant-place target nil))
                 :msg (msg (contestant-place-msg target))}]}

   "Blacklist"
   {:effect (effect (lock-zone (:cid card) :challenger :discard))
    :leave-play (effect (release-zone (:cid card) :challenger :discard))}

   "Breached Dome"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :effect (req (let [c (first (get-in @state [:challenger :deck]))]
                            (system-msg state :contestant (str "uses Breached Dome to do one meat damage and to discard " (:title c)
                                                         " from the top of the Challenger's Stack"))
                            (mill state :contestant :challenger 1)
                            (damage state side eid :meat 1 {:card card})))}}

   "Brain-Taping Warehouse"
   {:events {:pre-reveal
             {:req (req (and (character? target)
                             (has-subtype? target "Bioroid")))
              :effect (effect (reveal-cost-bonus (- (:click challenger))))}}}

   "Broadcast Square"
   {:events {:pre-bad-publicity {:async true
                                 :trace {:base 3
                                         :successful {:msg "prevents all bad publicity"
                                                      :effect (effect (bad-publicity-prevent Integer/MAX_VALUE))}}}}}

   "Capital Investors"
   {:abilities [{:cost [:click 1]
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain-credits 2))}]}

   "Cerebral Overwriter"
   (advance-ambush 3 {:async true
                      :req (req (pos? (get-counters (get-card state card) :advancement)))
                      :msg (msg "do " (get-counters (get-card state card) :advancement) " brain damage")
                      :effect (effect (damage eid :brain (get-counters (get-card state card) :advancement) {:card card}))})

   "Chairman Hiro"
   {:effect (effect (lose :challenger :hand-size 2))
    :leave-play (effect (gain :challenger :hand-size 2))
    :discard-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Challenger's score area as an agenda worth 2 agenda points"
                   :async true
                   :effect (req (as-agenda state :challenger eid card 2))}}

   "Chief Slee"
   {:abilities [{:label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (system-msg (str "adds 1 power counter to Chief Slee")))}
                {:counter-cost [:power 5]
                 :cost [:click 1]
                 :async true
                 :msg "do 5 meat damage"
                 :effect (effect (damage eid :meat 5 {:card card}))}]}

   "C.I. Fund"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req (pos? (:credit contestant)))}
    :abilities [{:label "Move up to 3 [Credit] from credit pool to C.I. Fund"
                 :prompt "Choose how many [Credit] to move"
                 :once :per-turn
                 :choices {:number (req (min (:credit contestant) 3))}
                 :effect (effect (lose-credits target)
                                 (add-counter card :credit target))
                 :msg (msg "move " target " [Credit] to C.I. Fund")}
                {:label "Take all credits from C.I. Fund"
                 :cost [:credit 2]
                 :msg (msg "discard it and gain " (get-counters card :credit) " [Credits]")
                 :effect (effect (discard card {:cause :ability-cost})
                                 (take-credits (get-counters card :credit)))}]
    :events {:contestant-turn-begins {:req (req (>= (get-counters card :credit) 6))
                                :effect (effect (add-counter card :credit 2)
                                                (system-msg (str "adds 2 [Credits] to C.I. Fund")))}}}

   "City Surveillance"
   {:hidden-events {:contestant-turn-ends contestant-reveal-toast}
    :flags {:challenger-phase-12 (req (pos? (:credit challenger)))}
    :events {:challenger-turn-begins
             {:player :challenger
              :prompt "Pay 1 [Credits] or take 1 tag"
              :choices (req (concat (when (pos? (:credit challenger))
                                      ["Pay 1 [Credits]"])
                                    ["Take 1 tag"]))
              :msg "make the Challenger pay 1 [Credits] or take 1 tag"
              :async true
              :effect (req (case target
                             "Pay 1 [Credits]"
                             (do (system-msg state :challenger "pays 1 [Credits]")
                                 (pay state :challenger card :credit 1)
                                 (effect-completed state side eid))

                             (do (system-msg state :challenger "takes 1 tag")
                                 (gain-tags state :contestant eid 1))))}}}

   "Clone Suffrage Movement"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req (and (some #(is-type? % "Operation") (:discard contestant))
                                     unprotected))}
    :abilities [{:label "Add 1 operation from Archives to HQ"
                 :effect (effect (show-wait-prompt :challenger "Contestant to use Clone Suffrage Movement")
                                 (continue-ability
                                   {:prompt "Select an operation in Archives to add to HQ"
                                    :once :per-turn
                                    :show-discard true
                                    :choices {:req #(and (is-type? % "Operation")
                                                         (= (:zone %) [:discard]))}
                                    :msg (msg "add "
                                              (if (:seen target)
                                                (:title target)
                                                "a facedown card")
                                              " to HQ")
                                    :effect (effect (move target :hand))
                                    :end-effect (effect (clear-wait-prompt :challenger))}
                                   card nil))}]}

   "Clyde Van Rite"
   (let [ability {:req (req (or (pos? (:credit challenger))
                                (pos? (count (:deck challenger)))))
                  :player :challenger
                  :once :per-turn
                  :prompt "Pay 1 [Credits] or discard the top card of the Stack"
                  :choices (req (concat (when (pos? (:credit challenger))
                                          ["Pay 1 [Credits]"])
                                        (when (pos? (count (:deck challenger)))
                                          ["Discard top card"])))
                  :msg "make the Challenger pay 1 [Credits] or discard the top card of the Stack"
                  :effect (req (case target
                                 "Pay 1 [Credits]"
                                 (do (system-msg state side "pays 1 [Credits]")
                                     (pay state side card :credit 1))
                                 "Discard top card"
                                 (do (system-msg state side "discards the top card of the Stack")
                                     (mill state :challenger))))}]
     {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :flags {:contestant-phase-12 (req true)}
      :events {:contestant-turn-begins ability}
      :abilities [ability]})

   "Commercial Bankers Group"
   (let [ability {:req (req unprotected)
                  :label "Gain 3 [Credits] (start of turn)"
                  :once :per-turn
                  :msg "gain 3 [Credits]"
                  :effect (effect (gain-credits 3))}]
     {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :events {:contestant-turn-begins ability}
      :abilities [ability]})

   "Constellation Protocol"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12
            (req (let [a-token (->> (all-placed state :contestant)
                                    (filter character?)
                                    (filter #(pos? (get-counters % :advancement)))
                                    (remove empty?)
                                    first
                                    :title)]
                   (as-> (all-placed state :contestant) it
                     (filter character? it)
                     (filter can-be-advanced? it)
                     (remove empty? it)
                     (map :title it)
                     (split-with (partial not= a-token) it)
                     (concat (first it) (-> it rest first rest))
                     (count it)
                     (pos? it))))}
    :abilities [{:label "Move an advancement counter between Character"
                 :once :per-turn
                 :effect (req (show-wait-prompt state :challenger "Contestant to use Constellation Protocol")
                              (continue-ability
                                state side
                                {:choices {:req #(and (character? %)
                                                      (get-counters % :advancement))}
                                 :effect (req (let [from-character target]
                                                (continue-ability
                                                  state side
                                                  {:prompt "Move to where?"
                                                   :choices {:req #(and (character? %)
                                                                        (not= (:cid from-character) (:cid %))
                                                                        (can-be-advanced? %))}
                                                   :effect (effect (add-prop :contestant target :advance-counter 1)
                                                                   (add-prop :contestant from-character :advance-counter -1)
                                                                   (system-msg
                                                                     (str "uses Constellation Protocol to move an advancement token from "
                                                                          (card-str state from-character)
                                                                          " to "
                                                                          (card-str state target))))}
                                                  card nil)))
                                 :end-effect (effect (clear-wait-prompt :challenger))}
                                card nil))}]}

   "Contract Killer"
   {:advanceable :always
    :abilities [{:label "Discard a connection"
                 :async true
                 :cost [:click 1]
                 :req (req (>= (get-counters card :advancement) 2))
                 :choices {:req #(has-subtype? % "Connection")}
                 :msg (msg "discard " (:title target))
                 :effect (effect (discard card {:cause :ability-cost})
                                 (discard eid target nil))}
                {:label "Do 2 meat damage"
                 :async true
                 :cost [:click 1]
                 :req (req (>= (get-counters card :advancement) 2))
                 :msg "do 2 meat damage"
                 :effect (effect (discard card {:cause :ability-cost})
                                 (damage eid :meat 2 {:card card}))}]}

   "Contestantorate Town"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :additional-cost [:forfeit]
    :flags {:contestant-phase-12 (req (and (revealed? card)
                                     (->> (all-active-placed state :challenger)
                                          (filter radicle?)
                                          count
                                          pos?)))}
    :abilities [{:label "Discard a radicle"
                 :once :per-turn
                 :async true
                 :prompt "Select a radicle to discard with Contestantorate Town"
                 :choices {:req radicle?}
                 :msg (msg "discard " (:title target))
                 :effect (effect (discard eid target {:unpreventable true}))}]}

   "CPC Generator"
   {:events {:challenger-click-credit {:req (req (first-event? state side :challenger-click-credit))
                                   :msg "gain 1 [Credits]"
                                   :effect (effect (gain-credits :contestant 1))}}}

   "Cybernetics Court"
   {:in-play [:hand-size 4]}

   "Daily Business Show"
   {:events {:pre-contestant-draw
             {:msg "draw additional cards"
              ;; The req catches draw events that happened before DBS was revealed.
              :req (req (first-event? state :contestant :pre-contestant-draw))
              ;; The once and once-key force a single DBS to act on behalf of all revealed DBS's.
              :once :per-turn
              :once-key :daily-business-show-draw-bonus
              :effect (req (let [dbs (count (filter #(and (= "06086" (:code %))
                                                          (revealed? %))
                                                    (all-placed state :contestant)))]
                             (draw-bonus state side dbs)))}
             :post-contestant-draw
             {:req (req (first-event? state :contestant :post-contestant-draw))
              :once :per-turn
              :once-key :daily-business-show-put-bottom
              :async true
              :effect (req (let [dbs (count (filter #(and (= "06086" (:code %))
                                                          (revealed? %))
                                                    (all-placed state :contestant)))
                                 drawn (get-in @state [:contestant :register :most-recent-drawn])]
                             (show-wait-prompt state :challenger "Contestant to use Daily Business Show")
                             (wait-for (resolve-ability
                                         state side
                                         {:prompt (str "Select " (quantify dbs "card") " to add to the bottom of R&D")
                                          :msg (msg "add " (quantify dbs "card") " to the bottom of R&D")
                                          :choices {:max dbs
                                                    :req #(some (fn [c] (= (:cid c) (:cid %))) drawn)}
                                          :effect (req (doseq [c targets]
                                                         (move state side c :deck)))}
                                         card targets)
                                       (do (clear-wait-prompt state :challenger)
                                           (effect-completed state side eid)))))}}}

   "Dedicated Response Team"
   {:events {:successful-run-ends {:req (req tagged)
                                   :msg "do 2 meat damage"
                                   :async true
                                   :effect (effect (damage eid :meat 2 {:card card}))}}}

   "Dedicated Locale"
   {:recurring 2}

   "Director Haas"
   {:in-play [:click 1 :click-per-turn 1]
    :discard-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Challenger's score area as an agenda worth 2 agenda points"
                   :async true
                   :effect (req (as-agenda state :challenger eid card 2))}}

   "Docklands Crackdown"
   {:abilities [{:cost [:click 2]
                 :msg "add 1 power counter"
                 :effect (effect (add-counter card :power 1))}]
    :events {:pre-place {:req (req (and (pos? (get-counters card :power))
                                          (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (effect (place-cost-bonus [:credit (get-counters card :power)]))}
             :challenger-place {:silent (req true)
                              :req (req (and (pos? (get-counters card :power))
                                             (not (get-in @state [:per-turn (:cid card)]))))
                              :msg (msg "increase the place cost of " (:title target) " by " (get-counters card :power) " [Credits]")
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}

   "Drudge Work"
   {:effect (effect (add-counter card :power 3))
    :abilities [{:cost [:click 1]
                 :counter-cost [:power 1]
                 :async true
                 :choices {:req #(and (is-type? % "Agenda")
                                      (or (in-hand? %)
                                          (in-discard? %)))}
                 :msg (msg "reveal " (:title target)
                           (let [target-agenda-points (get-agenda-points state :contestant target)]
                             (when (pos? target-agenda-points)
                               (str ", gain " target-agenda-points " [Credits], ")))
                           " and shuffle it into R&D")
                 :effect (req (gain-credits state :contestant (get-agenda-points state :contestant target))
                              (move state :contestant target :deck)
                              (shuffle! state :contestant :deck)
                              (if (zero? (get-counters card :power))
                                (discard state side eid card nil)
                                (effect-completed state side eid)))}]}

   "Early Premiere"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req (some #(and (can-be-advanced? %)
                                            (in-locale? %))
                                      (all-placed state :contestant)))}
    :abilities [{:cost [:credit 1]
                 :label "Place 1 advancement token on a card that can be advanced in a locale"
                 :choices {:req #(and (can-be-advanced? %)
                                      (placed? %)
                                      (in-locale? %))} ; should be *in* a locale
                 :once :per-turn
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Echo Chamber"
   {:abilities [{:label "Add Echo Chamber to your score area as an agenda worth 1 agenda point"
                 :cost [:click 3]
                 :msg "add it to their score area as an agenda worth 1 agenda point"
                 :async true
                 :effect (req (as-agenda state :contestant eid card 1))}]}

   "Edge of World"
   (letfn [(character-count [state]
             (count (get-in (:contestant @state) [:locales (last (:locale (:run @state))) :characters])))]
     (placed-access-trigger 3 {:msg (msg "do " (character-count state) " brain damage")
                                  :async true
                                  :effect (effect (damage eid :brain (character-count state)
                                                          {:card card}))}))

   "Elizabeth Mills"
   {:effect (effect (lose :bad-publicity 1)) :msg "remove 1 bad publicity"
    :abilities [{:cost [:click 1] :label "Discard a location"
                 :msg (msg "discard " (:title target) " and take 1 bad publicity")
                 :choices {:req #(has-subtype? % "Location")}
                 :effect (effect (discard card {:cause :ability-cost})
                                 (discard target)
                                 (gain-bad-publicity :contestant 1))}]}

   "Elizas Toybox"
   {:abilities [{:cost [:click 3] :choices {:req #(not (:revealed %))}
                 :label "Reveal a card at no cost" :msg (msg "reveal " (:title target) " at no cost")
                 :effect (effect (reveal target {:ignore-cost :all-costs}))}]}

   "Encryption Protocol"
   {:events {:pre-discard {:req (req (placed? target))
                         :effect (effect (discard-cost-bonus 1))}}}

   "Estelle Moon"
   {:events {:contestant-place {:req (req (and (#{"Site" "Agenda" "Region"} (:type target))
                                           (is-party? (second (:zone target)))))
                            :effect (effect (add-counter card :power 1)
                                            (system-msg (str "places 1 power counter on Estelle Moon")))}}
    :abilities [{:label "Draw 1 card and gain 2 [Credits] for each power counter"
                 :effect (req (let [counters (get-counters card :power)
                                    credits (* 2 counters)]
                                (discard state side card {:cause :ability-cost})
                                (draw state side counters)
                                (gain-credits state side credits)
                                (system-msg state side (str "uses Estelle Moon to draw " counters
                                                            " cards and gain " credits " [Credits]"))))}]}

   "Eve Campaign"
   (campaign 16 2)

   "Executive Boot Camp"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req (some #(not (revealed? %)) (all-placed state :contestant)))}
    ; A card revealed by Executive Bootcamp is ineligible to receive the turn-begins event for this turn.
    :suppress {:contestant-turn-begins {:req (req (= (:cid target) (:ebc-revealed (get-card state card))))}}
    :events {:contestant-turn-ends {:req (req (:ebc-revealed card))
                              :effect (effect (update! (dissoc card :ebc-revealed)))}}
    :abilities [{:choices {:req (complement revealed?)}
                 :label "Reveal a card, lowering the cost by 1 [Credits]"
                 :msg (msg "reveal " (:title target))
                 :async true
                 :effect (req (reveal-cost-bonus state side -1)
                              (wait-for (reveal state side target {:no-warning true})
                                        (update! state side (assoc card :ebc-revealed (:cid target)))))}
                {:prompt "Choose an site to add to HQ"
                 :msg (msg "add " (:title target) " to HQ")
                 :activatemsg "searches R&D for an site"
                 :choices (req (cancellable (filter #(is-type? % "Site")
                                                    (:deck contestant))
                                            :sorted))
                 :cost [:credit 1]
                 :label "Search R&D for an site"
                 :effect (effect (discard card {:cause :ability-cost})
                                 (shuffle! :deck)
                                 (move target :hand))}]}

   "Executive Search Firm"
   {:abilities [{:prompt "Choose an Executive, Sysop, or Character to add to HQ"
                 :msg (msg "add " (:title target) " to HQ and shuffle R&D")
                 :activatemsg "searches R&D for an Executive, Sysop, or Character"
                 :choices (req (cancellable (filter #(or (has-subtype? % "Executive")
                                                         (has-subtype? % "Sysop")
                                                         (has-subtype? % "Character"))
                                                    (:deck contestant))
                                            :sorted))
                 :cost [:click 1]
                 :label "Search R&D for an Executive, Sysop, or Character"
                 :effect (effect (move target :hand)
                                 (shuffle! :deck))}]}

   "Exposé"
   {:advanceable :always
    :abilities [{:label "Remove 1 bad publicity for each advancement token on Exposé"
                 :msg (msg "remove " (get-counters card :advancement) " bad publicity")
                 :effect (effect (discard card {:cause :ability-cost})
                                 (lose :bad-publicity (get-counters card :advancement)))}]}

   "False Flag"
   (letfn [(tag-count [false-flag]
             (int (/ (get-counters false-flag :advancement) 2)))]
     {:advanceable :always
      :access {:req (req (pos? (get-counters (get-card state card) :advancement)))
               :msg (msg "give the challenger " (quantify (tag-count (get-card state card)) "tag"))
               :async true
               :effect (effect (gain-tags :contestant eid (tag-count (get-card state card))))}
      :abilities [{:cost [:click 1]
                   :advance-counter-cost 7
                   :label "Add False Flag to your score area as an agenda worth 3 agenda points"
                   :msg "add it to their score area as an agenda worth 3 agenda points"
                   :async true
                   :effect (req (as-agenda state :contestant eid card 3))}]})

   "Franchise City"
   {:events {:access {:req (req (is-type? target "Agenda"))
                      :msg "add it to their score area as an agenda worth 1 agenda point"
                      :async true
                      :effect (req (as-agenda state :contestant eid card 1))}}}

   "Full Immersion RecStudio"
   {:can-host (req (and (or (is-type? target "Site") (is-type? target "Agenda"))
                        (> 2 (count (:hosted card)))))
    :discard-cost-bonus (req (* 3 (count (:hosted card))))
    :abilities [{:label "Place an site or agenda on Full Immersion RecStudio"
                 :req (req (< (count (:hosted card)) 2))
                 :cost [:click 1]
                 :prompt "Select an site or agenda to place"
                 :choices {:req #(and (or (is-type? % "Site") (is-type? % "Agenda"))
                                      (in-hand? %)
                                      (= (:side %) "Contestant"))}
                 :msg "place and host an site or agenda"
                 :effect (req (contestant-place state side target card))}
                {:label "Place a previously-placed site or agenda on Full Immersion RecStudio (fixes only)"
                 :req (req (< (count (:hosted card)) 2))
                 :prompt "Select an placed site or agenda to host on Full Immersion RecStudio"
                 :choices {:req #(and (or (is-type? % "Site") (is-type? % "Agenda"))
                                      (placed? %)
                                      (= (:side %) "Contestant"))}
                 :msg "place and host an site or agenda"
                 :effect (req (host state side card target))}]}

   "Fumiko Yamamori"
   {:events {:psi-game-done {:req (req (not= (first targets) (second targets)))
                             :async true
                             :msg "do 1 meat damage"
                             :effect (effect (damage eid :meat 1 {:card card}))}}}

   "Gene Splcharacterr"
   {:advanceable :always
    :access {:req (req (pos? (get-counters (get-card state card) :advancement)))
             :msg (msg "do " (get-counters (get-card state card) :advancement) " net damage")
             :async true
             :effect (effect (damage eid :net (get-counters (get-card state card) :advancement)
                                      {:card card}))}
    :abilities [{:cost [:click 1]
                 :advance-counter-cost 3
                 :label "Add Gene Splicing to your score area as an agenda worth 1 agenda point"
                 :msg "add it to their score area as an agenda worth 1 agenda point"
                 :async true
                 :effect (req (as-agenda state :contestant eid card 1))}]}

   "Genetics Pavilion"
   {:msg "prevent the Challenger from drawing more than 2 cards during their turn"
    :effect (req (max-draw state :challenger 2)
                 (when (zero? (remaining-draws state :challenger))
                   (prevent-draw state :challenger)))
    :events {:challenger-turn-begins {:effect (effect (max-draw :challenger 2))}}
    :leave-play (req (swap! state update-in [:challenger :register] dissoc :max-draw :cannot-draw))}

   "Ghost Branch"
   (advance-ambush 0 {:async true
                      :req (req (pos? (get-counters (get-card state card) :advancement)))
                      :msg (msg "give the Challenger " (quantify (get-counters (get-card state card) :advancement) "tag"))
                      :effect (effect (gain-tags :contestant eid (get-counters (get-card state card) :advancement)))})

   "GRNDL Refinery"
   {:advanceable :always
    :abilities [{:label "Gain 4 [Credits] for each advancement token on GRNDL Refinery"
                 :cost [:click 1]
                 :msg (msg "gain " (* 4 (get-counters card :advancement)) " [Credits]")
                 :effect (effect (discard card {:cause :ability-cost})
                                 (gain-credits (* 4 (get-counters card :advancement))))}]}

   "Haas Arcology AI"
   {:advanceable :while-unrevealed
    :abilities [{:label "Gain [Click][Click]"
                 :once :per-turn
                 :msg "gain [Click][Click]"
                 :cost [:click 1]
                 :advance-counter-cost 1
                 :effect (effect (gain :click 2))}]}

   "Honeyfarm"
   {:flags {:rd-reveal (req true)}
    :access {:msg "force the Challenger to lose 1 [Credits]"
             :effect (effect (lose-credits :challenger 1))}}

   "Hostile Infrastructure"
   {:events {:challenger-discard {:async true
                            :req (req (some #(card-is? % :side :contestant) targets))
                            :msg (msg (str "do " (count (filter #(card-is? % :side :contestant) targets))
                                           " net damage"))
                            :effect (req (letfn [(do-damage [t]
                                                   (if-not (empty? t)
                                                     (wait-for (damage state side :net 1 {:card card})
                                                               (do-damage (rest t)))
                                                     (effect-completed state side eid)))]
                                           (do-damage (filter #(card-is? % :side :contestant) targets))))}}
    :abilities [{:msg "do 1 net damage"
                 :async true
                 :effect (effect (damage eid :net 1 {:card card}))}]}

   "Hyoubu Research Facility"
   {:events {:psi-bet-contestant {:once :per-turn
                            :msg (msg "gain " target " [Credits]")
                            :effect (effect (gain-credits :contestant target))}}}

   "Ibrahim Salem"
   (let [discard-ability (fn [card-type]
                         {:req (req (seq (filter #(is-type? % card-type) (:hand challenger))))
                          :prompt (str "Choose a " card-type " to discard")
                          :choices (req (filter #(is-type? % card-type) (:hand challenger)))
                          :effect (effect (discard target))
                          :msg (msg " discard " (:title target) " from the Challenger's Grip")})
         choose-ability {:label "Discard 1 card in the Challenger's Grip of a named type"
                         :once :per-turn
                         :req (req (seq (:hand challenger)))
                         :prompt "Choose a card type"
                         :choices ["Event" "Hazard" "Resource" "Radicle"]
                         :msg (msg "reveal " (join ", " (map :title (:hand challenger))) " and discard a " target)
                         :effect (effect (resolve-ability (discard-ability target) card nil))}]
     {:additional-cost [:forfeit]
      :flags {:contestant-phase-12 (constantly true)}
      :hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :abilities [choose-ability]})

   "Illegal Arms Factory"
   (let [ability {:msg "gain 1 [Credits] and draw 1 card"
                  :label "Gain 1 [Credits] and draw 1 card (start of turn)"
                  :once :per-turn
                  :async true
                  :req (req (:contestant-phase-12 @state))
                  :effect (effect (gain-credits 1)
                                  (draw eid 1 nil))}]
     {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :events {:contestant-turn-begins ability}
      :abilities [ability]
      :discard-effect {:req (req (= :locales (first (:previous-zone card)))
                               (= side :challenger))
                     :effect (effect (gain-bad-publicity :contestant 1)
                                     (system-msg :contestant (str "takes 1 bad publicity from Illegal Arms Factory")))}})

   "Indian Union Stock Exchange"
   (let [iuse {:req (req (not= (:faction target) (:faction (:identity contestant))))
               :msg "gain 1 [Credits]"
               :effect (effect (gain-credits 1))}]
     {:events {:play-operation iuse
               :reveal iuse}})

   "Isabel McGuire"
   {:abilities [{:label "Add an placed card to HQ"
                 :cost [:click 1]
                 :choices {:req placed?}
                 :msg (msg "move " (card-str state target) " to HQ")
                 :effect (effect (move target :hand))}]}

   "IT Department"
   {:abilities [{:counter-cost [:power 1]
                 :label "Add strength to a revealed Character"
                 :choices {:req #(and (character? %) (:revealed %))}
                 :req (req (pos? (get-counters card :power)))
                 :msg (msg "add strength to a revealed Character")
                 :effect (req (update! state side (update-in card [:it-targets (keyword (str (:cid target)))]
                                                             (fnil inc 0)))
                              (update-character-strength state side target))}
                {:cost [:click 1]
                 :msg "add 1 counter"
                 :effect (effect (add-counter card :power 1))}]
    :events (let [it {:req (req (:it-targets card))
                      :effect (req (update! state side (dissoc card :it-targets))
                                   (update-all-character state side))}]
              {:pre-character-strength {:req (req (get-in card [:it-targets (keyword (str (:cid target)))]))
                                  :effect (effect (character-strength-bonus
                                                    (* (get-in card [:it-targets (keyword (str (:cid target)))])
                                                       (inc (get-counters card :power))) target))}
               :challenger-turn-ends it
               :contestant-turn-ends it})}

   "Jackson Howard"
   {:abilities [{:cost [:click 1]
                 :msg "draw 2 cards"
                 :effect (effect (draw 2))}
                {:label "Shuffle up to 3 cards from Archives into R&D"
                 :activatemsg "removes Jackson Howard from the game"
                 :effect (effect (rfg-and-shuffle-rd-effect card 3))}]}

   "Jeeves Model Bioroids"
   (let [jeeves (effect (gain :click 1))
         ability {:label "Gain [Click]"
                  :msg "gain [Click]"
                  :once :per-turn
                  :effect jeeves}
         cleanup (effect (update! (dissoc card :seen-this-turn)))]
     {:abilities [ability]
      :leave-play cleanup
      :discard-effect {:effect cleanup}
      :events {:contestant-spent-click
               {:effect (req (when-not target
                               (print-stack-trace (Exception. (str "WHY JEEVES WHY: " targets))))
                             (update! state side (update-in card [:seen-this-turn (or target :this-is-a-hack)]
                                                            (fnil + 0) (second targets)))
                             (when (>= (get-in (get-card state card) [:seen-this-turn (or target :this-is-a-hack)]) 3)
                               (resolve-ability state side ability card nil)))}
               :contestant-turn-ends {:effect cleanup}}})

   "Kala Ghoda Real TV"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req true)}
    :abilities [{:msg "look at the top card of the Challenger's Stack"
                  :effect (effect (prompt! card (str "The top card of the Challenger's Stack is "
                                                     (:title (first (:deck challenger)))) ["OK"] {}))}
                {:label "[Discard]: Discard the top card of the Challenger's Stack"
                 :msg (msg "discard " (:title (first (:deck challenger))) " from the Challenger's Stack")
                 :effect (effect (discard card {:cause :ability-cost})
                                 (mill :challenger))}]}

   "Kuwinda K4H1U3"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req true)}
    :abilities [{:label "Trace X - do 1 brain damage (start of turn)"
                 :trace {:base (req (get-counters card :power))
                          :successful {:async true
                                       :msg "do 1 brain damage"
                                       :effect (effect (damage :challenger eid :brain 1 {:card card})
                                                       (discard card))}
                          :unsuccessful {:effect (effect (add-counter card :power 1)
                                                         (system-msg "adds 1 power counter to Kuwinda K4H1U3"))}}}]}

   "Lady Liberty"
   {:abilities [{:cost [:click 3]
                 :label "Add agenda from HQ to score area"
                 :req (req (let [counters (get-counters (get-card state card) :power)]
                             (some #(and (is-type? % "Agenda")
                                         (= counters (:agendapoints %)))
                                  (:hand contestant))))
                 :async true
                 :effect (req (show-wait-prompt state :challenger "Contestant to select an agenda for Lady Liberty")
                              (continue-ability
                                state side
                                {:prompt "Select an Agenda in HQ to move to score area"
                                 :choices {:req #(and (is-type? % "Agenda")
                                                      (= (:agendapoints %) (get-counters (get-card state card) :power))
                                                      (in-hand? %))}
                                 :msg (msg "add " (:title target) " to score area")
                                 :async true
                                 :effect (req (wait-for (as-agenda state :contestant target (:agendapoints target))
                                                        (let [latest (find-latest state target)]
                                                          (when-let [events (:events (card-def latest))]
                                                            (register-events state side events latest))
                                                          (clear-wait-prompt state :challenger)
                                                          (effect-completed state side eid))))}
                                card nil))}]
    :events {:contestant-turn-begins {:effect (effect (add-counter card :power 1))}}}

   "Lakshmi Smartfabrics"
   {:events {:reveal {:effect (effect (add-counter card :power 1))}}
    :abilities [{:req (req (seq (filter #(and (is-type? % "Agenda")
                                              (>= (get-counters card :power)
                                                  (:agendapoints %)))
                                        (:hand contestant))))
                 :label "X power counters: Reveal an agenda worth X points from HQ"
                 :effect (req (let [c (get-counters card :power)]
                                (resolve-ability
                                  state side
                                  {:prompt "Select an agenda in HQ to reveal"
                                   :choices {:req #(and (is-type? % "Agenda")
                                                        (>= c (:agendapoints %)))}
                                   :msg (msg "reveal " (:title target) " from HQ")
                                   :effect (req (let [title (:title target)
                                                      pts (:agendapoints target)]
                                                  (register-turn-flag! state side
                                                    card :can-steal
                                                    (fn [state side card]
                                                      (if (= (:title card) title)
                                                        ((constantly false)
                                                         (toast state :challenger "Cannot steal due to Lakshmi Smartfabrics." "warning"))
                                                        true)))
                                                  (add-counter state side card :power (- pts))))} card nil)))}]}

   "Launch Campaign"
   (campaign 6 2)

   "Levy University"
   {:abilities [{:prompt "Choose an Character"
                 :msg (msg "adds " (:title target) " to HQ")
                 :choices (req (cancellable (filter character? (:deck contestant)) :sorted))
                 :label "Search R&D for a piece of Character"
                 :cost [:click 1 :credit 1]
                 :effect (effect (move target :hand)
                                 (shuffle! :deck))}]}

   "Lily Lockwell"
   {:async true
    :effect (effect (draw eid 3 nil))
    :msg (msg "draw 3 cards")
    :abilities [{:label "Remove a tag to search R&D for an operation"
                 :prompt "Choose an operation to put on top of R&D"
                 :cost [:click 1]
                 :choices (req (cancellable (filter #(is-type? % "Operation") (:deck contestant)) :sorted))
                 :req (req (pos? (get-in @state [:challenger :tag])))
                 :effect (req (lose-tags state :contestant 1)
                              (let [c (move state :contestant target :play-area)]
                                (shuffle! state :contestant :deck)
                                (move state :contestant c :deck {:front true})
                                (system-msg state side (str "uses Lily Lockwell to put " (:title c) " on top of R&D"))))
                 :cancel-effect (effect (lose-tags :contestant 1)
                                        (shuffle! :contestant :deck)
                                        (system-msg (str "uses Lily Lockwell, but did not find an Operation in R&D")))}]}

   "Long-Term Investment"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :abilities [{:label "Move any number of [Credits] to your credit pool"
                 :req (req (>= (get-counters card :credit) 8))
                 :cost [:click 1]
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "gain " target " [Credits]")
                 :effect (effect (take-credits target))}]
    :events {:contestant-turn-begins {:effect (effect (add-counter card :credit 2)
                                                (system-msg (str "adds 2 [Credit] to Long-Term Investment")))}}}

   "Malia Z0L0K4"
   (let [re-enable-target (req (when-let [malia-target (:malia-target card)]
                                 (system-msg state side (str "uses "  (:title card) " to unblank "
                                                             (card-str state malia-target)))
                                 (enable-card state :challenger (get-card state malia-target))
                                 (when-let [reactivate-effect (:reactivate (card-def malia-target))]
                                   (resolve-ability state :challenger reactivate-effect (get-card state malia-target) nil))))]
     {:effect (effect (update! (assoc card :malia-target target))
                      (disable-card :challenger target))
      :msg (msg (str "blank the text box of " (card-str state target)))
      :choices {:req #(and (= (:side %) "Challenger") (placed? %) (radicle? %)
                           (not (has-subtype? % "Virtual")))}
      :leave-play re-enable-target
      :move-zone re-enable-target})

   "Marilyn Campaign"
   (let [ability {:msg "gain 2 [Credits]"
                  :counter-cost [:credit 2]
                  :once :per-turn
                  :req (req (:contestant-phase-12 @state))
                  :label (str "Gain 2 [Credits] (start of turn)")
                  :async true
                  :effect (req (take-credits state :contestant 2)
                               (if (zero? (get-counters (get-card state card) :credit))
                                 (discard state :contestant eid card {:unpreventable true})
                                 (effect-completed state :contestant eid)))}]
     {:effect (effect (add-counter card :credit 8))
      :hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :events {:contestant-turn-begins ability}
      :discard-effect {:req (req (= :locales (first (:previous-zone card))))
                     :async true
                     :effect (effect (show-wait-prompt :challenger "Contestant to use Marilyn Campaign")
                                     (continue-ability :contestant
                                       {:optional
                                        {:prompt "Shuffle Marilyn Campaign into R&D?"
                                         :priority 1
                                         :player :contestant
                                         :yes-ability {:msg "shuffle it back into R&D"
                                                       :effect (req (move state :contestant card :deck)
                                                                    (shuffle! state :contestant :deck)
                                                                    (effect-completed state side eid))}
                                         :end-effect (effect (clear-wait-prompt :challenger))}}
                                      card nil))}})

   "Mark Yale"
   {:events {:agenda-counter-spent {:msg "gain 1 [Credits]"
                                    :effect (effect (gain-credits 1))}}
    :abilities [{:label "Discard to gain 2 [Credits]"
                 :msg "gain 2 [Credits]"
                 :effect (effect (discard card {:cause :ability-cost})
                                 (gain-credits 2))}
                {:label "Spend an agenda counter to gain 2 [Credits]"
                 :effect (effect (continue-ability
                                   {:prompt "Select an agenda with a counter"
                                    :choices {:req #(and (is-type? % "Agenda")
                                                         (pos? (get-counters % :agenda)))}
                                    :msg (msg "spend an agenda token on " (:title target) " and gain 2 [Credits]")
                                    :effect (effect (add-counter target :agenda -1)
                                                    (gain-credits 2)
                                                    (trigger-event :agenda-counter-spent card))}
                                   card nil))}]}

   "Marked Accounts"
   (let [ability {:msg "take 1 [Credits]"
                  :label "Take 1 [Credits] (start of turn)"
                  :once :per-turn
                  :counter-cost [:credit 1]
                  :effect (effect (take-credits 1))}]
   {:abilities [ability
                {:cost [:click 1]
                 :msg "store 3 [Credits]"
                 :effect (effect (add-counter card :credit 3))}]
    :events {:contestant-turn-begins ability}})

   "MCA Austerity Policy"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :msg "to force the Challenger to lose a [Click] next turn and place a power counter on itself"
                 :effect (req (swap! state update-in [:challenger :extra-click-temp] (fnil dec 0))
                              (add-counter state side card :power 1))}
                {:cost [:click 1]
                 :counter-cost [:power 3]
                 :msg "gain 4 [Click] and discard itself"
                 :effect (effect (discard card {:cause :ability-cost
                                              :unpreventable true})
                                 (gain :click 4))}]}

   "Melange Mining Contestant."
   {:abilities [{:cost [:click 3]
                 :effect (effect (gain-credits 7))
                 :msg "gain 7 [Credits]"}]}

   "Mental Health Clinic"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain-credits 1))}]
     {:effect (effect (gain :challenger :hand-size 1))
      :leave-play (effect (lose :challenger :hand-size 1))
      :hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :events {:contestant-turn-begins ability}
      :abilities [ability]})

   "Mr. Stone"
   {:events {:challenger-gain-tag {:async true
                               :msg "do 1 meat damage"
                               :effect (effect (damage :contestant eid :meat 1 {:card card}))}}}

   "Mumba Temple"
   {:recurring 2}

   "Mumbad City Hall"
   {:abilities [{:label "Search R&D for an Alliance card"
                 :cost [:click 1]
                 :prompt "Choose an Alliance card to play or place"
                 :choices (req (cancellable (filter #(and (has-subtype? % "Alliance")
                                                          (if (is-type? % "Operation")
                                                            (<= (:cost %) (:credit contestant))
                                                            true))
                                                    (:deck contestant))
                                            :sorted))
                 :msg (msg "reveal " (:title target)
                           " from R&D and "
                           (if (= (:type target) "Operation") "play" "place")
                           " it")
                 :effect (req (shuffle! state side :deck)
                              (if (= (:type target) "Operation")
                                (play-instant state side target)
                                (contestant-place state side target nil nil)))}]}

   "Mumbad Construction Co."
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :events {:contestant-turn-begins {:effect (effect (add-prop card :advance-counter 1 {:placed true}))}}
    :abilities [{:cost [:credit 2]
                 :req (req (and (pos? (get-counters card :advancement))
                                (not-empty (all-active-placed state :contestant))))
                 :label "Move an advancement token to a faceup card"
                 :prompt "Select a faceup card"
                 :choices {:req revealed?}
                 :msg (msg "move an advancement token to " (card-str state target))
                 :effect (effect (add-prop card :advance-counter -1 {:placed true})
                                 (add-prop target :advance-counter 1 {:placed true}))}]}

   "Museum of History"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req (pos? (count (get-in @state [:contestant :discard]))))}
    :abilities [{:label "Shuffle cards in Archives into R&D"
                 :prompt (msg (let [mus (count (filter #(and (= "10019" (:code %))
                                                             (revealed? %))
                                                       (all-placed state :contestant)))]
                                (str "Select "
                                     (if (> mus 1) "a card " (str mus " cards "))
                                     "in Archives to shuffle into R&D")))
                 :choices {:req #(and (card-is? % :side :contestant)
                                      (= (:zone %) [:discard]))
                           :max (req (count (filter #(and (= "10019" (:code %))
                                                          (revealed? %))
                                                    (all-placed state :contestant))))}
                 :show-discard true
                 :priority 1
                 :once :per-turn
                 :once-key :museum-of-history
                 :msg (msg "shuffle "
                           (let [seen (filter :seen targets)
                                 n (count (filter #(not (:seen %)) targets))]
                             (str (join ", " (map :title seen))
                                  (when (pos? n)
                                    (str (when-not (empty? seen) " and ")
                                         (quantify n "card")))))
                           " into R&D")
                 :effect (req (doseq [c targets]
                                (move state side c :deck))
                              (shuffle! state side :deck))}]}

   "NASX"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain-credits 1))}]
     {:implementation "Manual - click NASX to add power counters"
      :hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :events {:contestant-turn-begins ability}
      :abilities [ability
                  {:label "Place 1 power counter"
                   :cost [:credit 1]
                   :effect (effect (add-counter card :power 1)
                                   (system-msg (str "places 1 power counter on NASX")))}
                  {:label "Place 2 power counters"
                   :cost [:credit 2]
                   :effect (effect (add-counter card :power 2)
                                   (system-msg (str "places 2 power counters on NASX")))}
                  {:label "[Discard] and gain 2 [Credits] for each power counter"
                   :cost [:click 1]
                   :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                   :effect (effect (discard card {:cause :ability-cost})
                                   (gain-credits (* 2 (get-counters card :power))))}]})

   "Net Analytics"
   (let [ability {:req (req (seq (filter #(some #{:tag} %) targets)))
                  :effect (effect (show-wait-prompt :challenger "Contestant to use Net Analytics")
                                  (continue-ability :contestant
                                    {:optional
                                     {:prompt "Draw from Net Analytics?"
                                      :yes-ability {:msg (msg "draw a card")
                                                    :effect (effect (draw :contestant 1))}
                                      :end-effect (effect (clear-wait-prompt :challenger))}}
                                    card nil))}]
     {:events {:challenger-lose-tag (assoc ability :req (req (= side :challenger)))
               :challenger-prevent (assoc ability :req (req (seq (filter #(some #{:tag} %) targets))))}})

   "Net Polcharacter"
   {:recurring (effect (set-prop card :rec-counter (:link challenger)))
    :effect (effect (set-prop card :rec-counter (:link challenger)))}

   "Neurostasis"
   (advance-ambush 3 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                      :async true
                      :effect (req (let [cnt (get-counters (get-card state card) :advancement)]
                                     (continue-ability
                                       state side
                                       {:prompt (msg "Choose " (quantify cnt "placed card") " to shuffle into the stack")
                                        :player :contestant
                                        :choices {:req #(and (placed? %)
                                                             (= (:side %) "Challenger"))
                                                  :max cnt}
                                        :msg (msg "shuffle " (join ", " (map :title targets)) " into the stack")
                                        :effect (req (doseq [c targets]
                                                       (move state :challenger c :deck))
                                                     (shuffle! state :challenger :deck))}
                                       card nil)))})

   "News Team"
   {:flags {:rd-reveal (req true)}
    :access {:msg (msg "force the Challenger take 2 tags or add it to their score area as an agenda worth -1 agenda point")
             :async true
             :effect (effect (continue-ability
                               {:player :challenger
                                :async true
                                :prompt "Take 2 tags or add News Team to your score area as an agenda worth -1 agenda point?"
                                :choices ["Take 2 tags" "Add News Team to score area"]
                                :effect (req (if (= target "Add News Team to score area")
                                               (do (system-msg state :challenger (str "adds News Team to their score area as an agenda worth -1 agenda point"))
                                                   (trigger-event state side :no-discard card)
                                                   (as-discarded-agenda state :challenger eid card -1 {:force true}))
                                               (do (system-msg state :challenger (str "takes 2 tags from News Team"))
                                                   (gain-tags state :challenger eid 2))))}
                               card targets))}}

   "NGO Front"
   (letfn [(builder [cost cred]
             {:advance-counter-cost cost
              :effect (effect (discard card {:cause :ability-cost})
                              (gain-credits cred))
              :label (str "[Discard]: Gain " cred " [Credits]")
              :msg (str "gain " cred " [Credits]")})]
     {:advanceable :always
      :abilities [(builder 1 5)
                  (builder 2 8)]})

   "Open Forum"
   {:events {:contestant-mandatory-draw
             {:interactive (req true)
              :msg (msg (if (-> contestant :deck count pos?)
                          (str "reveal and draw " (-> contestant :deck first :title) " from R&D")
                          "reveal and draw from R&D but it is empty"))
              :async true
              :effect (effect (draw 1)
                              (continue-ability
                                {:prompt "Choose a card in HQ to put on top of R&D"
                                 :async true
                                 :choices {:req #(and (in-hand? %)
                                                      (= (:side %) "Contestant"))}
                                 :msg "add 1 card from HQ to the top of R&D"
                                 :effect (effect (move target :deck {:front true})
                                                 (effect-completed eid))}
                                card nil))}}}

   "PAD Campaign"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain-credits 1))}]
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :events {:contestant-turn-begins ability}
    :abilities [ability]})

   "PAD Factory"
   {:abilities [{:cost [:click 1]
                 :label "Place 1 advancement token on a card"
                 :choices {:req placed?}
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :effect (req (add-prop state :contestant target :advance-counter 1 {:placed true})
                              (let [tgtcid (:cid target)]
                                (register-turn-flag! state side
                                  target :can-score
                                  (fn [state side card]
                                    (if (and (= tgtcid
                                                (:cid card))
                                             (>= (get-counters card :advancement)
                                                 (or (:current-cost card)
                                                     (:advancementcost card))))
                                      ((constantly false) (toast state :contestant "Cannot score due to PAD Factory." "warning"))
                                      true)))))}]}

   "Pālanā Agroplex"
   (let [ability {:msg "make each player draw 1 card"
                  :label "Make each player draw 1 card (start of turn)"
                  :once :per-turn
                  :effect (effect (draw 1) (draw :challenger))}]
     {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :flags {:contestant-phase-12 (req true)}
      :events {:contestant-turn-begins ability}
      :abilities [ability]})

   "Personalized Portal"
   {:events {:contestant-turn-begins
             {:effect (req (draw state :challenger 1)
                           (let [cnt (count (get-in @state [:challenger :hand]))
                                 credits (quot cnt 2)]
                             (gain-credits state :contestant credits)
                             (system-msg state :contestant
                                         (str "uses Personalized Portal to force the challenger to draw "
                                              "1 card and gains " credits " [Credits]"))))}}}

   "Plan B"
   (advance-ambush
    0
    {:req (req (pos? (get-counters (get-card state card) :advancement)))
     :effect (req (show-wait-prompt state :challenger "Contestant to select an agenda to score with Plan B")
                  (doseq [ag (filter #(is-type? % "Agenda") (:hand contestant))]
                    (update-advancement-cost state side ag))
                  (resolve-ability
                    state side
                    {:prompt "Select an Agenda in HQ to score"
                     :choices {:req #(and (is-type? % "Agenda")
                                          (<= (:current-cost %) (get-counters (get-card state card) :advancement))
                                          (in-hand? %))}
                     :msg (msg "score " (:title target))
                     :effect (effect (score (assoc target :advance-counter
                                                   (:current-cost target)))
                                     (clear-wait-prompt :challenger))}
                    card nil))}
    "Score an Agenda from HQ?")

   "Political Dealings"
   (letfn [(pdhelper [agendas n]
             {:optional
              {:prompt (msg "Reveal and place " (:title (nth agendas n)) "?")
               :yes-ability {:async true
                             :msg (msg "reveal " (:title (nth agendas n)))
                             :effect (req (wait-for (contestant-place
                                                      state side (nth agendas n) nil
                                                      {:place-state
                                                       (:place-state
                                                         (card-def (nth agendas n))
                                                         :unrevealed)})
                                                    (if (< (inc n) (count agendas))
                                                      (continue-ability state side (pdhelper agendas (inc n)) card nil)
                                                      (effect-completed state side eid))))}
               :no-ability {:async true
                            :effect (req (if (< (inc n) (count agendas))
                                           (continue-ability state side (pdhelper agendas (inc n)) card nil)
                                           (effect-completed state side eid)))}}})]
     {:events
      {:contestant-draw
       {:async true
        :req (req (let [drawn (get-in @state [:contestant :register :most-recent-drawn])
                        agendas (filter #(is-type? % "Agenda") drawn)]
                    (seq agendas)))
        :effect (req (let [drawn (get-in @state [:contestant :register :most-recent-drawn])
                           agendas (filter #(is-type? % "Agenda") drawn)]
                       (continue-ability state side (pdhelper agendas 0) card nil)))}}})

   "Primary Transmission Dish"
   {:recurring 3}

   "Private Contracts"
   {:effect (effect (add-counter card :credit 14))
    :abilities [{:cost [:click 1]
                 :counter-cost [:credit 2]
                 :msg "gain 2 [Credits]"
                 :effect (req (take-credits state :contestant 2)
                              (when (zero? (get-counters (get-card state card) :credit))
                                (discard state :contestant card)))}]}

   "Project Junebug"
   (advance-ambush 1 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                      :msg (msg "do " (* 2 (get-counters (get-card state card) :advancement)) " net damage")
                      :async true
                      :effect (effect (damage eid :net (* 2 (get-counters (get-card state card) :advancement))
                                              {:card card}))})

   "Psychic Field"
   (let [ab {:psi {:req (req placed)
                   :not-equal {:msg (msg "do " (count (:hand challenger)) " net damage")
                               :async true
                               :effect (effect (damage eid :net (count (:hand challenger)) {:card card}))}}}]
     {:expose ab :access ab})

   "Public Support"
   {:effect (effect (add-counter card :power 3))
    :hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :events {:contestant-turn-begins
             {:async true
              :effect (req (add-counter state side card :power -1)
                           (if (zero? (get-counters (get-card state card) :power))
                             (do (system-msg state :contestant "uses Public Support to add it to their score area as an agenda worth 1 agenda point")
                                 (as-agenda state :contestant eid (dissoc card :counter) 1))
                             (effect-completed state side eid)))}}}

   "Quarantine System"
   (letfn [(reveal-character [cnt] {:prompt "Select an Character to reveal"
                           :async true
                           :choices {:req #(and (character? %) (not (revealed? %)))}
                           :msg (msg "reveal " (:title target))
                           :effect (req (let [agenda (last (:rfg contestant))
                                              ap (:agendapoints agenda 0)]
                                          (reveal-cost-bonus state side (* ap -2))
                                          (reveal state side target {:no-warning true})
                                          (if (< cnt 3) (continue-ability state side (reveal-character (inc cnt)) card nil)
                                                        (effect-completed state side eid))))})]
     {:abilities [{:label "Forfeit agenda to reveal up to 3 Character with a 2 [Credit] discount per agenda point"
                   :req (req (pos? (count (:scored contestant))))
                   :cost [:forfeit]
                   :effect (req (continue-ability state side (reveal-character 1) card nil))}]})

   "Raman Rai"
   {:abilities [{:once :per-turn
                 :label "Lose [Click] and swap a card in HQ you just drew for a card in Archives"
                 :req (req (and (pos? (:click contestant))
                                (not-empty (turn-events state side :contestant-draw))))
                 :effect (req (let [drawn (get-in @state [:contestant :register :most-recent-drawn])]
                                (lose state :contestant :click 1)
                                (resolve-ability state side
                                  {:prompt "Choose a card in HQ that you just drew to swap for a card of the same type in Archives"
                                   :choices {:req #(some (fn [c] (= (:cid c) (:cid %))) drawn)}
                                   :effect (req (let [hqcard target
                                                      t (:type hqcard)]
                                                  (resolve-ability state side
                                                    {:show-discard true
                                                     :prompt (msg "Choose an " t " in Archives to reveal and swap into HQ for " (:title hqcard))
                                                     :choices {:req #(and (= (:side %) "Contestant")
                                                                          (= (:type %) t)
                                                                          (= (:zone %) [:discard]))}
                                                     :msg (msg "lose [Click], reveal " (:title hqcard) " from HQ, and swap it for " (:title target) " from Archives")
                                                     :effect (req (let [swappedcard (assoc hqcard :zone [:discard])
                                                                        archndx (character-index state target)
                                                                        arch (get-in @state [:contestant :discard])
                                                                        newarch (apply conj (subvec arch 0 archndx) swappedcard (subvec arch archndx))]
                                                                     (swap! state assoc-in [:contestant :discard] newarch)
                                                                     (swap! state update-in [:contestant :hand]
                                                                            (fn [coll] (remove-once #(= (:cid %) (:cid hqcard)) coll)))
                                                                     (move state side target :hand)))}
                                                   card nil)))}
                                 card nil)))}]}

   "Rashida Jaheem"
   (let [ability {:once :per-turn
                  :label "Gain 3 [Credits] and draw 3 cards (start of turn)"
                  :effect (effect (resolve-ability
                                    {:optional
                                     {:prompt "Discard Rashida Jaheem to gain 3 [Credits] and draw 3 cards?"
                                      :yes-ability {:async true
                                                    :msg "gain 3 [Credits] and draw 3 cards"
                                                    :effect (req (wait-for (discard state side card nil)
                                                                           (do (gain-credits state side 3)
                                                                               (draw state side eid 3 nil))))}}}
                                    card nil))}]
     {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
      :flags {:contestant-phase-12 (req true)}
      :events {:contestant-turn-begins ability}
      :abilities [ability]})

   "Reality Threedee"
   (let [ability {:effect (req (gain-credits state side (if tagged 2 1)))
                  :label "Gain credits (start of turn)"
                  :once :per-turn
                  :msg (msg (if tagged "gain 2 [Credits]" "gain 1 [Credits]"))}]
   {:effect (effect (gain-bad-publicity :contestant 1)
                    (system-msg "takes 1 bad publicity"))
    :hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :events {:contestant-turn-begins ability}
    :abilities [ability]})

   "Reconstruction Contract"
   {:events {:damage {:req (req (and (pos? (nth targets 2)) (= :meat target)))
                      :effect (effect (add-counter card :advancement 1)
                                      (system-msg "adds 1 advancement token to Reconstruction Contract"))}}
    :abilities [{:label "[Discard]: Move advancement tokens to another card"
                 :prompt "Select a card that can be advanced"
                 :choices {:req can-be-advanced?}
                 :effect (req (let [move-to target]
                                (resolve-ability
                                  state side
                                  {:prompt "Move how many tokens?"
                                   :choices {:number (req (get-counters card :advancement))
                                             :default (req (get-counters card :advancement))}
                                   :effect (effect (discard card {:cause :ability-cost})
                                                   (add-counter move-to :advancement target {:placed true})
                                                   (system-msg (str "discards Reconstruction Contract to move " target
                                                                    (pluralize " advancement token" target) " to "
                                                                    (card-str state move-to))))}
                                  card nil)))}]}

   "Reversed Accounts"
   {:advanceable :always
    :abilities [{:cost [:click 1]
                 :label "Force the Challenger to lose 4 [Credits] per advancement"
                 :msg (msg "force the Challenger to lose " (min (* 4 (get-counters card :advancement)) (:credit challenger)) " [Credits]")
                 :effect (effect (discard card {:cause :ability-cost})
                                 (lose-credits :challenger (* 4 (get-counters card :advancement))))}]}

   "Rex Campaign"
   (let [ability {:once :per-turn
                  :label "Remove 1 counter (start of turn)"
                  :effect (req (add-counter state side card :power -1)
                               (when (zero? (get-counters (get-card state card) :power))
                                 (discard state side card)
                                 (resolve-ability
                                   state side
                                   {:prompt "Remove 1 bad publicity or gain 5 [Credits]?"
                                    :choices ["Remove 1 bad publicity" "Gain 5 [Credits]"]
                                    :msg (msg (if (= target "Remove 1 bad publicity")
                                                "remove 1 bad publicity" "gain 5 [Credits]"))
                                    :effect (req (if (= target "Remove 1 bad publicity")
                                                   (lose state side :bad-publicity 1)
                                                   (gain-credits state side 5)))}
                                   card targets)))}]
   {:effect (effect (add-counter card :power 3))
    :hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :events {:contestant-turn-begins ability}
    :ability [ability]})

   "Ronald Five"
   {:events {:challenger-discard {:req (req (and (= (:side target) "Contestant")
                                           (pos? (:click challenger))))
                            :msg "force the challenger to lose 1 [Click]"
                            :effect (effect (lose :challenger :click 1))}}}

   "Ronin"
   {:advanceable :always
    :abilities [{:cost [:click 1]
                 :req (req (>= (get-counters card :advancement) 4))
                 :msg "do 3 net damage"
                 :async true
                 :effect (effect (discard card {:cause :ability-cost})
                                 (damage eid :net 3 {:card card}))}]}

   "Sandburg"
   {:effect (req (add-watch state :sandburg
                            (fn [k ref old new]
                              (let [credit (get-in new [:contestant :credit])]
                                (when (not= (get-in old [:contestant :credit]) credit)
                                  (update-all-character ref side)))))
                 (update-all-character state side))
    :events {:pre-character-strength {:req (req (and (character? target)
                                               (>= (:credit contestant) 10)))
                                :effect (effect (character-strength-bonus (quot (:credit contestant) 5) target))}}
    :leave-play (req (remove-watch state :sandburg)
                     (update-all-character state side))}

   "Sealed Vault"
   {:abilities [{:label "Store any number of [Credits] on Sealed Vault"
                 :cost [:credit 1]
                 :prompt "How many [Credits]?"
                 :choices {:number (req (- (:credit contestant) 1))}
                 :msg (msg "store " target " [Credits]")
                 :effect (effect (lose-credits target)
                                 (add-counter card :credit target))}
                {:label "Move any number of [Credits] to your credit pool"
                 :cost [:click 1]
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "gain " target " [Credits]")
                 :effect (effect (gain-credits target))}
                {:label "[Discard]: Move any number of [Credits] to your credit pool"
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "discard it and gain " target " [Credits]")
                 :effect (effect (discard card {:cause :ability-cost})
                                 (gain-credits target))}]}

   "Security Subcontract"
   {:abilities [{:choices {:req #(and (character? %)
                                      (revealed? %))}
                 :cost [:click 1]
                 :msg (msg "discard " (:title target) " to gain 4 [Credits]")
                 :label "Discard a revealed Character to gain 4 [Credits]"
                 :effect (effect (discard target {:cause :ability-cost})
                                 (gain-credits 4))}]}

   "Sensie Actors Union"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req unprotected)}
    :abilities [{:label "Draw 3 cards and add 1 card in HQ to the bottom of R&D"
                 :once :per-turn
                 :msg "draw 3 cards"
                 :effect (effect (draw 3)
                                 (resolve-ability
                                   {:prompt "Select a card in HQ to add to the bottom of R&D"
                                    :choices {:req #(and (= (:side %) "Contestant")
                                                         (in-hand? %))}
                                    :msg "add 1 card from HQ to the bottom of R&D"
                                    :effect (effect (move target :deck))}
                                  card nil))}]}

   "Locale Diagnostics"
   (let [ability {:effect (effect (gain-credits 2))
                  :once :per-turn
                  :label "Gain 2 [Credits] (start of turn)"
                  :msg "gain 2 [Credits]"}]
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :abilities [ability]
    :events {:contestant-turn-begins ability
             :contestant-place {:req (req (character? target))
                            :async true
                            :effect (req (wait-for (discard state side card nil)
                                                   (do (system-msg state :challenger "discards Locale Diagnostics")
                                                       (effect-completed state side eid))))}}})

   "Shannon Claire"
   {:abilities [{:cost [:click 1]
                 :msg "draw 1 card from the bottom of R&D"
                 :effect (effect (move (last (:deck contestant)) :hand))}
                {:label "[Discard]: Search R&D for an agenda"
                 :prompt "Choose an agenda to add to the bottom of R&D"
                 :msg (msg "reveal " (:title target) " from R&D and add it to the bottom of R&D")
                 :choices (req (cancellable (filter #(is-type? % "Agenda") (:deck contestant)) :sorted))
                 :effect (effect (discard card {:cause :ability-cost})
                                 (shuffle! :deck)
                                 (move target :deck))}
                {:label "[Discard]: Search Archives for an agenda"
                 :prompt "Choose an agenda to add to the bottom of R&D"
                 :msg (msg "reveal " (:title target) " from Archives and add it to the bottom of R&D")
                 :choices (req (cancellable (filter #(is-type? % "Agenda") (:discard contestant)) :sorted))
                 :effect (effect (discard card {:cause :ability-cost})
                                 (move target :deck))}]}

   "Shattered Remains"
   (advance-ambush 1 {:async true
                      :req (req (pos? (get-counters (get-card state card) :advancement)))
                      :effect (req (let [counters (get-counters (get-card state card) :advancement)]
                                     (continue-ability
                                       state side
                                       (-> discard-hazard
                                           (assoc-in [:choices :max] counters)
                                           (assoc :prompt (msg "Select " (quantify counters "piece") " of hazard to discard")
                                                  :effect (effect (discard-cards targets))
                                                  :msg (msg "discard " (join ", " (map :title targets)))))
                                       card nil)))})

   "Shi.Kyū"
   {:access
    {:async true
     :req (req (not= (first (:zone card)) :deck))
     :effect (effect (show-wait-prompt :challenger "Contestant to use Shi.Kyū")
                     (continue-ability
                       {:optional
                        {:prompt "Pay [Credits] to use Shi.Kyū?"
                         :yes-ability
                         {:prompt "How many [Credits] for Shi.Kyū?"
                          :choices :credit
                          :msg (msg "attempt to do " target " net damage")
                          :async true
                          :effect (req (let [dmg target]
                                         (clear-wait-prompt state :challenger)
                                         (continue-ability
                                           state :contestant
                                           {:player :challenger
                                            :prompt (str "Take " dmg " net damage or add Shi.Kyū to your score area as an agenda worth -1 agenda point?")
                                            :choices [(str "Take " dmg " net damage") "Add Shi.Kyū to score area"]
                                            :async true
                                            :effect (req (if (= target "Add Shi.Kyū to score area")
                                                           (do (system-msg state :challenger (str "adds Shi.Kyū to their score area as as an agenda worth -1 agenda point"))
                                                               (trigger-event state side :no-discard card)
                                                               (as-discarded-agenda state :challenger eid card -1 {:force true}))
                                                           (do (damage state :contestant eid :net dmg {:card card})
                                                               (system-msg state :challenger (str "takes " dmg " net damage from Shi.Kyū")))))}
                                           card targets)))}
                         :no-ability {:effect (effect (clear-wait-prompt :challenger))}}}
                       card targets))}}

   "Shock!"
   {:flags {:rd-reveal (req true)}
    :access {:msg "do 1 net damage"
             :async true
             :effect (effect (damage eid :net 1 {:card card}))}}

   "SIU"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req true)}
    :abilities [{:label "Trace 3 - Give the Challenger 1 tag"
                 :req (req (:contestant-phase-12 @state))
                 :async true
                 :effect (effect (discard card {:cause :ability-cost})
                                 (resolve-ability
                                  {:trace {:base 3
                                           :label "Trace 3 - Give the Challenger 1 tag"
                                           :successful {:msg "give the Challenger 1 tag"
                                                        :async true
                                                        :effect (effect (gain-tags :challenger eid 1))}}}
                                  card nil))}]}

   "Snare!"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :async true
             :effect (effect (show-wait-prompt :challenger "Contestant to use Snare!")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 4 [Credits] to use Snare! ability?"
                                 :end-effect (effect (clear-wait-prompt :challenger))
                                 :yes-ability {:async true
                                               :cost [:credit 4]
                                               :msg "do 3 net damage and give the Challenger 1 tag"
                                               :effect (req (wait-for (damage state side :net 3 {:card card})
                                                                      (gain-tags state :contestant eid 1)))}}}
                               card nil))}}

   "Space Camp"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :effect (effect (show-wait-prompt :challenger "Contestant to use Space Camp")
                             (continue-ability
                               {:optional
                                {:prompt "Place 1 advancement token with Space Camp?"
                                 :cancel-effect (req (clear-wait-prompt state :challenger)
                                                     (effect-completed state side eid))
                                 :yes-ability {:msg (msg "place 1 advancement token on " (card-str state target))
                                               :prompt "Select a card to place an advancement token on with Space Camp"
                                               :choices {:req can-be-advanced?}
                                               :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                                               (clear-wait-prompt :challenger))}
                                 :no-ability {:effect (req (clear-wait-prompt state :challenger)
                                                           (effect-completed state side eid))}}}
                               card nil))}}

   "Student Loans"
   {:events {:pre-play-instant
             {:req (req (and (is-type? target "Event") (seq (filter #(= (:title %) (:title target)) (:discard challenger)))))
              :effect (effect (system-msg :contestant (str "makes the challenger pay an extra 2 [Credits] due to Student Loans"))
                              (play-cost-bonus [:credit 2]))}}}

   "Sundew"
   {:implementation "it's all broken just don't even try man"}
    ; :events {:challenger-spent-click {:once :per-turn
    ;                               :msg (req (when (not this-locale) "gain 2 [Credits]"))
    ;                               :effect (req (when (not this-locale)
    ;                                              (gain-credits state :contestant 2)))}}

   "Synth DNA Modification"
   {:implementation "Manual fire once subroutine is broken"
    :abilities [{:msg "do 1 net damage"
                 :label "Do 1 net damage after AP subroutine broken"
                 :once :per-turn
                 :effect (effect (damage eid :net 1 {:card card}))}]}

   "Team Sponsorship"
   {:events {:agenda-scored {:label "Place a card from Archives or HQ"
                             :prompt "Select a card from Archives or HQ to place"
                             :show-discard true
                             :interactive (req true)
                             :async true
                             :choices {:req #(and (not (is-type? % "Operation"))
                                                  (= (:side %) "Contestant")
                                                  (#{[:hand] [:discard]} (:zone %)))}
                             :msg (msg (contestant-place-msg target))
                             :effect (effect (contestant-place eid target nil {:no-place-cost true}))}}}

   "Tech Startup"
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :flags {:contestant-phase-12 (req true)}
    :abilities [{:label "Place an site from R&D"
                 :prompt "Choose an site to place"
                 :msg (msg "place " (:title target))
                 :choices (req (filter #(is-type? % "Site") (:deck contestant)))
                 :effect (effect (discard card)
                                 (shuffle! :deck)
                                 (contestant-place target nil))}]}

   "TechnoCo"
   (letfn [(is-techno-target [card]
             (or (is-type? card "Resource")
                 (is-type? card "Hazard")
                 (and (is-type? card "Radicle") (has-subtype? card "Virtual"))))]
     {:events {:pre-place {:req (req (and (is-techno-target target)
                                            (not (second targets)))) ; not facedown
                             :effect (effect (place-cost-bonus [:credit 1]))}
               :challenger-place {:req (req (and (is-techno-target target)
                                               (not (second targets)))) ; not facedown
                                :msg "gain 1 [Credits]"
                                :effect (req (gain-credits state :contestant 1))}}})

   "Tenma Line"
   {:abilities [{:label "Swap 2 pieces of placed Character"
                 :cost [:click 1]
                 :prompt "Select two pieces of Character to swap positions"
                 :choices {:req #(and (placed? %)
                                      (character? %))
                           :max 2}
                 :effect (req (when (= (count targets) 2)
                                (swap-character state side (first targets) (second targets))))
                 :msg (msg "swap the positions of "
                           (card-str state (first targets))
                           " and "
                           (card-str state (second targets)))}]}

   "Test Ground"
   (letfn [(hide-card [advancements]
             {:async true
              :prompt "Hide a card"
              :choices {:req #(and (placed? %)
                                   (revealed? %))}
              :effect (req (hide state side target)
                           (if (pos? (dec advancements))
                             (continue-ability state side (hide-card (dec advancements)) card nil)
                             (effect-completed state side eid)))})]
     {:advanceable :always
      :abilities [{:label "Hide 1 card for each advancement token"
                   :req (req (pos? (get-counters card :advancement)))
                   :msg (msg "hide " (quantify (get-counters card :advancement) "card"))
                   :effect (req (let [advancements (get-counters card :advancement)]
                                  (discard state side card {:cause :ability-cost})
                                  (show-wait-prompt state :challenger (str "Contestant to hide "
                                                                       (quantify advancements "card")))
                                  (wait-for (resolve-ability state side (hide-card advancements) card nil)
                                            (clear-wait-prompt state :challenger))))}]})

   "The Board"
   (let [the-board {:req (req (and (= :challenger (:as-agenda-side target))
                                   (not= (:cid target) (:cid card))))
                    :effect (effect (lose :challenger :agenda-point 1))}]
         {:effect (effect (lose :challenger :agenda-point (count (:scored challenger))))
          :leave-play (effect (gain :challenger :agenda-point (count (:scored challenger))))
          :discard-effect {:when-inactive true
                         :req (req (:access @state))
                         :msg "add it to the Challenger's score area as an agenda worth 2 agenda points"
                         :async true
                         :effect (req (as-agenda state :challenger eid card 2))}
          :events {:agenda-stolen (dissoc the-board :req)
                   :as-agenda the-board
                   :pre-card-moved {:req (req (let [c (first targets)
                                                    c-cid (:cid c)]
                                                (some #(when (= c-cid (:cid %)) %) (:scored challenger))))
                                    :effect (req (gain state :challenger :agenda-point 1))}}})

   "The News Now Hour"
   {:events {:challenger-turn-begins {:effect (req (prevent-current state side))}}
    :effect (req (prevent-current state side))
    :leave-play (req (swap! state assoc-in [:challenger :register :cannot-play-current] false))}

   "The Root"
   {:recurring 3}

   "Thomas Haas"
   {:advanceable :always
    :abilities [{:label "Gain credits"
                 :msg (msg "gain " (* 2 (get-counters card :advancement)) " [Credits]")
                 :effect (effect (discard card {:cause :ability-cost})
                                 (gain-credits (* 2 (get-counters card :advancement))))}]}

   "Toshiyuki Sakai"
   (advance-ambush 0
    {:effect (effect (resolve-ability
                       {:prompt "Select an site or agenda in HQ"
                        :choices {:req #(and (or (is-type? % "Agenda")
                                                 (is-type? % "Site"))
                                             (in-hand? %))}
                        :msg "swap it for an site or agenda from HQ"
                        :effect (req (let [tidx (character-index state card)
                                           srvcont (get-in @state (cons :contestant (:zone card)))
                                           c (get-counters (get-card state card) :advancement)
                                           newcard (assoc target :zone (:zone card) :advance-counter c)
                                           newcont (apply conj (subvec srvcont 0 tidx) newcard (subvec srvcont tidx))]
                                       (resolve-ability state side
                                         {:effect (req (swap! state assoc-in (cons :contestant (:zone card)) newcont)
                                                       (swap! state update-in [:contestant :hand]
                                                         (fn [coll] (remove-once #(= (:cid %) (:cid newcard)) coll)))
                                                       (trigger-event state side :contestant-place newcard)
                                                       (move state side card :hand))} card nil)
                                       (resolve-prompt state :challenger {:choice "No action"})
                                       ; gets rid of prompt to discard Toshiyuki since it's back in HQ now
                                       (resolve-ability state :challenger
                                         {:optional
                                          {:player :challenger
                                           :priority true
                                           :prompt "Access the newly placed card?"
                                           :yes-ability {:effect (effect (access-card newcard))}}}
                                         card nil)))}
                      card nil))}
    "Swap Toshiyuki Sakai with an agenda or site from HQ?")

   "Turtlebacks"
   {:events {:locale-created {:msg "gain 1 [Credits]"
                              :effect (effect (gain-credits 1))}}}

   "Urban Renewal"
   {:effect (effect (add-counter card :power 3))
    :hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :events {:contestant-turn-begins
             {:async true
              :effect (req (add-counter state side card :power -1)
                           (if (zero? (get-counters (get-card state card) :power))
                             (wait-for (discard state side card nil)
                                       (do (system-msg state :contestant "uses Urban Renewal to do 4 meat damage")
                                           (damage state side eid :meat 4 {:card card})))
                             (effect-completed state side eid)))}}}

   "Victoria Jenkins"
   {:effect (req (lose state :challenger :click-per-turn 1)
                 (when (= (:active-player @state) :challenger)
                   (lose state :challenger :click 1)))
    :leave-play (req (gain state :challenger :click-per-turn 1)
                     (when (= (:active-player @state) :challenger)
                       (gain state :challenger :click 1)))
    :discard-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Challenger's score area as an agenda worth 2 agenda points"
                   :async true
                   :effect (req (as-agenda state :challenger eid card 2))}}

   "Warden Fatuma"
   (let [new-sub {:label "[Warden Fatuma] Force the Challenger to lose 1 [Click], if able"}]
     (letfn [(all-revealed-bios [state]
               (filter #(and (character? %)
                             (has-subtype? % "Bioroid")
                             (revealed? %))
                       (all-placed state :contestant)))
             (remove-one [cid state character]
               (remove-extra-subs state :contestant cid character))
             (add-one [cid state character]
               (add-extra-sub state :contestant cid character 0 new-sub))
             (update-all [state func]
               (doseq [i (all-revealed-bios state)]
                 (func state i)))]
       {:effect (req (system-msg
                       state :contestant
                       "uses Warden Fatuma to add \"[Subroutine] The Challenger loses [Click], if able\" before all other subroutines")
                  (update-all state (partial add-one (:cid card))))
        :leave-play (req (system-msg state :contestant "loses Warden Fatuma additional subroutines")
                      (update-all state (partial remove-one (:cid card))))
        :sub-effect {:msg "force the Challenger to lose 1 [Click], if able"
                     :effect (req (lose state :challenger :click 1))}
        :events {:reveal {:req (req (and (character? target)
                                      (has-subtype? target "Bioroid")))
                       :effect (req (add-one (:cid card) state (get-card state target)))}}}))

   "Watchdog"
   {:events {:pre-reveal {:req (req (and (character? target) (not (get-in @state [:per-turn (:cid card)]))))
                       :effect (effect (reveal-cost-bonus (- (:tag challenger))))}
             :reveal {:req (req (and (character? target) (not (get-in @state [:per-turn (:cid card)]))))
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}

   "Whampoa Reclamation"
   {:abilities [{:label "Discard 1 card from HQ: Add 1 card from Archives to the bottom of R&D"
                 :once :per-turn
                 :req (req (and (pos? (count (:hand contestant)))
                                (pos? (count (:discard contestant)))))
                 :async true
                 :effect (req (show-wait-prompt state :challenger "Contestant to use Whampoa Reclamation")
                              (wait-for (resolve-ability state side
                                                         {:prompt "Choose a card in HQ to discard"
                                                          :choices {:req #(and (in-hand? %) (= (:side %) "Contestant"))}
                                                          :effect (effect (discard target))}
                                                         card nil)
                                        (continue-ability
                                          state side
                                          {:prompt "Select a card in Archives to add to the bottom of R&D"
                                           :show-discard true
                                           :choices {:req #(and (in-discard? %) (= (:side %) "Contestant"))}
                                           :msg (msg "discard 1 card from HQ and add "
                                                     (if (:seen target) (:title target) "a card") " from Archives to the bottom of R&D")
                                           :effect (effect (move target :deck)
                                                           (clear-wait-prompt :challenger))}
                                          card nil)))}]}

   "Worlds Plaza"
   {:abilities [{:label "Place an site on Worlds Plaza"
                 :req (req (< (count (:hosted card)) 3))
                 :cost [:click 1]
                 :prompt "Select an site to place on Worlds Plaza"
                 :choices {:req #(and (is-type? % "Site")
                                      (in-hand? %)
                                      (= (:side %) "Contestant"))}
                 :msg (msg "host " (:title target))
                 :effect (req (contestant-place state side target card) ;; place target onto card
                              (reveal-cost-bonus state side -2)
                              (reveal state side (last (:hosted (get-card state card)))))}]}

   "Zaibatsu Loyalty"
   {:interactions {:prevent [{:type #{:expose}
                              :req (req true)}]}
    :hidden-events
    {:pre-expose
     {:async true
      :effect (req (let [etarget target]
                     (continue-ability
                       state side
                       {:optional {:req (req (not (revealed? card)))
                                   :player :contestant
                                   :prompt (msg "The Challenger is about to expose " (:title etarget) ". Reveal Zaibatsu Loyalty?")
                                   :yes-ability {:effect (effect (reveal card))}}}
                       card nil)))}}
    :abilities [{:msg "prevent 1 card from being exposed"
                 :cost [:credit 1]
                 :effect (effect (expose-prevent 1))}
                {:msg "prevent 1 card from being exposed"
                 :label "[Discard]: Prevent 1 card from being exposed"
                 :effect (effect (discard card {:cause :ability-cost})
                                 (expose-prevent 1))}]}

   "Zealous Judge"
   {:reveal-req (req tagged)
    :abilities [{:async true
                 :label "Give the Challenger 1 tag"
                 :cost [:click 1 :credit 1]
                 :msg (msg "give the Challenger 1 tag")
                 :effect (effect (gain-tags eid 1))}]}})
