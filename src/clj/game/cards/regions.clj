(ns game.cards.regions
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int other-side]]
            [cardnum.cards :refer [all-cards]]))

(def card-definitions
  {"Akitaro Watanabe"
   {:events {:pre-reveal-cost {:req (req (and (character? target)
                                           (= (card->locale state card) (card->locale state target))))
                            :effect (effect (reveal-cost-bonus -2))}}}

   "Amazon Industrial Zone"
   {:events
     {:contestant-place  {:optional {:req (req (and (character? target)
                                                (protecting-same-locale? card target)))
                                 :prompt "Reveal Character with reveal cost lowered by 3?" :priority 2
                                 :yes-ability {:effect (effect (reveal-cost-bonus -3) (reveal target))}}}}}

   "Arella Salvatore"
   (let [select-ability
         {:prompt "Select a card to place with Arella Salvatore"
          :choices {:req #(and (contestant-placeable-type? %)
                               (in-hand? %)
                               (= (:side %) "Contestant"))}
          :async true
          :cancel-effect (req (effect-completed state side eid))
          :effect (req (wait-for (contestant-place state :contestant target nil {:no-place-cost true :display-message false})
                                 (let [inst-target (find-latest state target)]
                                   (add-prop state :contestant inst-target :advance-counter 1 {:placed true})
                                   (system-msg state :contestant
                                               (str "uses Arella Salvatore to place and place a counter on "
                                                    (card-str state inst-target) ", ignoring all costs"))
                                   (effect-completed state side eid))))}]
     {:events
      {:agenda-scored
       {:req (req (and (= (:previous-zone target) (:zone card))))
        :interactive (req true)
        :silent (req (empty? (filter contestant-placeable-type? (:hand contestant))))
        :async true
        :effect (req (if (some contestant-placeable-type? (:hand contestant))
                       (continue-ability state side select-ability card nil)
                       (effect-completed state side eid)))}}})

   "Ash 2X3ZB9CY"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-locale)
                              :trace {:base 4
                                      :successful
                                      {:msg "prevent the Challenger from accessing cards other than Ash 2X3ZB9CY"
                                       :effect (req (max-access state side 0)
                                                    (when-not (:replace-access (get-in @state [:run :run-effect]))
                                                      (let [ash card]
                                                        (swap! state update-in [:run :run-effect]
                                                               #(assoc % :replace-access
                                                                       {:mandatory true
                                                                        :effect (effect (access-card ash))
                                                                        :card ash})))))}}}}}

   "Awakening Center"
   {:can-host (req (is-type? target "Character"))
    :abilities [{:label "Host a piece of Bioroid Character"
                 :cost [:click 1]
                 :prompt "Select a piece of Bioroid Character to host on Awakening Center"
                 :choices {:req #(and (character? %)
                                      (has-subtype? % "Bioroid")
                                      (in-hand? %))}
                 :msg "host a piece of Bioroid Character"
                 :effect (req (contestant-place state side target card {:no-place-cost true}))}
                {:req (req (and this-locale
                                (zero? (get-in @state [:run :position]))))
                 :label "Reveal a hosted piece of Bioroid Character"
                 :prompt "Choose a piece of Bioroid Character to reveal" :choices (req (:hosted card))
                 :msg (msg "lower the reveal cost of " (:title target) " by 7 [Credits] and force the Challenger to encounter it")
                 :effect (effect (reveal-cost-bonus -7) (reveal target)
                                 (update! (dissoc (get-card state target) :facedown))
                                 (register-events {:run-ends
                                                    {:effect (req (doseq [c (:hosted card)]
                                                                    (when (:revealed c)
                                                                      (discard state side c)))
                                                                  (unregister-events state side card))}} card))}]
    :events {:run-ends nil}}

   "Bamboo Dome"
   (letfn [(dome [dcard]
             {:prompt "Select a card to add to HQ"
              :async true
              :choices {:req #(and (= (:side %) "Contestant")
                                   (= (:zone %) [:play-area]))}
              :msg "move a card to HQ"
              :effect (effect (move target :hand)
                              (continue-ability (put dcard) dcard nil))})
           (put [dcard]
             {:prompt "Select first card to put back onto R&D"
              :async true
              :choices {:req #(and (= (:side %) "Contestant")
                                   (= (:zone %) [:play-area]))}
              :msg "move remaining cards back to R&D"
              :effect (effect (move target :deck {:front true})
                              (move (first (get-in @state [:contestant :play-area])) :deck {:front true})
                              (clear-wait-prompt :challenger)
                              (effect-completed eid))})]
    {:init {:root "R&D"}
     :place-req (req (filter #{"R&D"} targets))
     :abilities [{:cost [:click 1]
                  :req (req (>= (count (:deck contestant)) 3))
                  :async true
                  :msg (msg (str "reveal " (join ", " (map :title (take 3 (:deck contestant)))) " from R&D"))
                  :label "Reveal the top 3 cards of R&D. Secretly choose 1 to add to HQ. Return the others to the top of R&D, in any order."
                  :effect (req (doseq [c (take 3 (:deck contestant))]
                                 (move state side c :play-area))
                            (show-wait-prompt state :challenger "Contestant to use Bamboo Dome")
                            (continue-ability state side (dome card) card nil))}]})

   "Ben Musashi"
   (let [bm {:req (req (or (in-same-locale? card target)
                           (from-same-locale? card target)))
             :effect (effect (steal-cost-bonus [:net-damage 2]))}]
     {:discard-effect
              {:req (req (and (= :locales (first (:previous-zone card))) (:run @state)))
               :effect (effect (register-events {:pre-steal-cost (assoc bm :req (req (or (= (:zone target) (:previous-zone card))
                                                                                         (= (central->zone (:zone target))
                                                                                            (butlast (:previous-zone card))))))
                                                 :run-ends {:effect (effect (unregister-events card))}}
                                                (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost bm :run-ends nil}})

   "Berncharacter Mai"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-locale)
                              :trace {:base 5
                                      :successful {:msg "give the Challenger 1 tag"
                                                   :async true
                                                   :effect (effect (gain-tags :contestant eid 1))}
                                      :unsuccessful
                                      {:effect (effect (system-msg "discards Berncharacter Mai from the unsuccessful trace")
                                                       (discard card))}}}}}

  "Bio Vault"
  {:place-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :advanceable :always
   :abilities [{:label "[Discard]: End the run"
                :advance-counter-cost 2
                :req (req (:run @state))
                :msg "end the run. Bio Vault is discarded"
                :async true
                :effect (effect
                          (end-run)
                          (discard eid card {:cause :ability-cost}))}]}

   "Black Level Clearance"
   {:events {:successful-run
             {:interactive (req true)
              :req (req this-locale)
              :async true
              :effect (effect (continue-ability
                                {:prompt "Take 1 brain damage or jack out?"
                                 :player :challenger
                                 :choices ["Take 1 brain damage" "Jack out"]
                                 :effect (req (if (= target "Take 1 brain damage")
                                                (damage state side eid :brain 1 {:card card})
                                                (do (jack-out state side nil)
                                                    (swap! state update-in [:challenger :prompt] rest)
                                                    (close-access-prompt state side)
                                                    (handle-end-run state side)
                                                    (gain-credits state :contestant 5)
                                                    (draw state :contestant)
                                                    (system-msg state :contestant (str "gains 5 [Credits] and draws 1 card. Black Level Clearance is discarded"))
                                                    (discard state side card)
                                                    (effect-completed state side eid))))}
                               card nil))}}}

   "Breaker Bay Grid"
   {:events {:pre-reveal-cost {:req (req (in-same-locale? card target))
                            :effect (effect (reveal-cost-bonus -5))}}}

   "Bryan Stinson"
   {:abilities [{:cost [:click 1]
                 :req (req (and (< (:credit challenger) 6)
                                (pos? (count (filter #(and (is-type? % "Operation")
                                                          (has-subtype? % "Transaction")) (:discard contestant))))))
                 :label "Play a transaction operation from Archives, ignoring all costs, and remove it from the game"
                 :prompt "Choose a transaction operation to play"
                 :msg (msg "play " (:title target) " from Archives, ignoring all costs, and removes it from the game")
                 :choices (req (cancellable (filter #(and (is-type? % "Operation")
                                                          (has-subtype? % "Transaction")) (:discard contestant)) :sorted))
                 :effect (effect (play-instant nil (assoc-in target [:special :rfg-when-discarded] true) {:ignore-cost true})
                                 (move target :rfg))}]}

   "Calibration Testing"
   {:place-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :abilities [{:label "[Discard]: Place 1 advancement token on a card in this locale"
                 :async true
                 :effect (effect (continue-ability
                                   {:prompt "Select a card in this locale"
                                    :choices {:req #(in-same-locale? % card)}
                                    :async true
                                    :msg (msg "place an advancement token on " (card-str state target))
                                    :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                                    (discard eid card {:cause :ability-cost}))}
                                   card nil))}]}


   "Caprcharacter Nisei"
   {:events {:pass-character {:req (req (and this-locale
                                       (= (:position run) 1))) ; trigger when last character passed
                        :msg "start a Psi game"
                        :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}
             :run {:req (req (and this-locale
                                  (zero? (:position run)))) ; trigger on unprotected locale
                   :msg "start a Psi game"
                   :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}}
    :abilities [{:msg "start a Psi game"
                 :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}]}

   "ChiLo City Grid"
   {:events {:successful-trace {:req (req this-locale)
                                :async true
                                :effect (effect (gain-tags :contestant eid 1))
                                :msg "give the Challenger 1 tag"}}}

   "Code Replicator"
   {:abilities [{:label "[Discard]: Force the challenger to approach the passed piece of character again"
                 :req (req (and this-locale
                                (> (count (get-run-characters state)) (:position run))
                                (:revealed (get-in (:characters (card->locale state card)) [(:position run)]))))
                 :effect (req (let [charactername (:title (get-in (:characters (card->locale state card)) [(:position run)]))]
                                (discard state :contestant (get-card state card))
                                (swap! state update-in [:run] #(assoc % :position (inc (:position run))))
                                 (system-msg state :contestant (str "discards Code Replicator to make the challenger approach "
                                                              charactername " again"))))}]}

   "Contestantorate Troubleshooter"
   {:abilities [{:label "[Discard]: Add strength to a revealed Character protecting this locale" :choices :credit
                 :prompt "How many credits?"
                 :effect (req (let [boost target]
                                (resolve-ability
                                  state side
                                  {:choices {:req #(and (character? %)
                                                        (revealed? %))}
                                   :msg (msg "add " boost " strength to " (:title target))
                                   :effect (req (update! state side (assoc card :troubleshooter-target target
                                                                                :troubleshooter-amount boost))
                                                (discard state side (get-card state card))
                                                (update-character-strength state side target))} card nil)))}]
    :events {:pre-character-strength nil :challenger-turn-ends nil :contestant-turn-ends nil}
    :discard-effect
               {:effect (req (register-events
                               state side
                               (let [ct {:effect (req (unregister-events state side card)
                                                      (update! state side (dissoc card :troubleshooter-target))
                                                      (update-character-strength state side (:troubleshooter-target card)))}]
                                 {:pre-character-strength
                                                    {:req (req (= (:cid target) (:cid (:troubleshooter-target card))))
                                                     :effect (effect (character-strength-bonus (:troubleshooter-amount card) target))}
                                  :challenger-turn-ends ct :contestant-turn-ends ct}) card))}}

   "Crisium Grid"
   (let [suppress-event {:req (req (and this-locale (not= (:cid target) (:cid card))))}]
     {:suppress {:pre-successful-run suppress-event
                 :successful-run suppress-event}
      :events {:pre-successful-run {:silent (req true)
                                    :req (req this-locale)
                                    :effect (req (swap! state update-in [:run :run-effect] dissoc :replace-access)
                                                 (swap! state update-in [:run] dissoc :successful)
                                                 (swap! state update-in [:challenger :register :successful-run] #(next %)))}}})

   "Cyberdex Virus Suite"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :effect (effect (show-wait-prompt :challenger "Contestant to use Cyberdex Virus Suite")
                             (continue-ability
                               {:optional {:prompt "Purge virus counters with Cyberdex Virus Suite?"
                                           :yes-ability {:msg (msg "purge virus counters")
                                                         :effect (effect (clear-wait-prompt :challenger)
                                                                         (purge))}
                                           :no-ability {:effect (effect (clear-wait-prompt :challenger))}}}
                               card nil))}
    :abilities [{:label "[Discard]: Purge virus counters"
                 :msg "purge virus counters" :effect (effect (discard card) (purge))}]}

   "Daruma"
   (letfn [(choose-swap [to-swap]
             {:prompt (str "Select a card to swap with " (:title to-swap))
              :choices {:not-self true
                        :req #(and (= "Contestant" (:side %))
                                   (#{"Site" "Agenda" "Region"} (:type %))
                                   (or (in-hand? %) ; agenda, site or region from HQ
                                       (and (placed? %) ; card placed in a locale
                                            ;; central regions are not in a locale
                                            (not (#{:hq :rd :archives} (first (:zone %)))))))}
              :effect (req (wait-for (discard state :contestant card nil)
                                     (move state :contestant to-swap (:zone target) {:keep-locale-alive true})
                                     (move state :contestant target (:zone to-swap) {:keep-locale-alive true})
                                     (system-msg state :contestant
                                                 (str "uses Daruma to swap " (card-str state to-swap)
                                                      " with " (card-str state target)))
                                     (clear-wait-prompt state :challenger)))
              :cancel-effect (effect (clear-wait-prompt :challenger))})
           (ability [card]
             {:optional {:prompt "Discard Daruma to swap a card in this locale?"
                         :yes-ability {:async true
                                       :prompt "Select a card in this locale to swap"
                                       :choices {:req #(and (placed? %)
                                                            (in-same-locale? card %))
                                                 :not-self true}
                                       :effect (effect (continue-ability (choose-swap target) card nil))}
                         :no-ability {:effect (effect (clear-wait-prompt :challenger))}}})]
   {:events {:approach-locale {:async true
                               :effect (effect (show-wait-prompt :challenger "Contestant to use Daruma")
                                               (continue-ability :contestant (ability card) card nil))}}})

   "Dedicated Technician Team"
   {:recurring 2}

   "Defense Construct"
   {:advanceable :always
    :abilities [{:label "[Discard]: Add 1 facedown card from Archives to HQ for each advancement token"
                 :req (req (and run (= (:locale run) [:archives])
                                (pos? (get-counters card :advancement))))
                 :effect (effect (resolve-ability
                                   {:show-discard true
                                    :choices {:max (get-counters card :advancement)
                                              :req #(and (= (:side %) "Contestant")
                                                         (not (:seen %))
                                                         (= (:zone %) [:discard]))}
                                              :msg (msg "add " (count targets) " facedown cards in Archives to HQ")
                                    :effect (req (doseq [c targets]
                                                   (move state side c :hand)))}
                                  card nil)
                                 (discard card))}]}

   "Disposable HQ"
   (letfn [(dhq [n i]
             {:req (req (pos? i))
              :prompt "Select a card in HQ to add to the bottom of R&D"
              :choices {:req #(and (= (:side %) "Contestant")
                                   (in-hand? %))}
              :async true
              :msg "add a card to the bottom of R&D"
              :effect (req (move state side target :deck)
                           (if (< n i)
                             (continue-ability state side (dhq (inc n) i) card nil)
                             (do
                               (clear-wait-prompt state :challenger)
                               (effect-completed state side eid))))
              :cancel-effect (effect (clear-wait-prompt :challenger))})]
     {:flags {:rd-reveal (req true)}
      :access {:async true
               :effect (req (let [n (count (:hand contestant))]
                              (show-wait-prompt state :challenger "Contestant to finish using Disposable HQ")
                              (continue-ability state side
                                {:optional
                                 {:prompt "Use Disposable HQ to add cards to the bottom of R&D?"
                                  :yes-ability {:async true
                                                :msg "add cards in HQ to the bottom of R&D"
                                                :effect (effect (continue-ability (dhq 1 n) card nil))}
                                  :no-ability {:effect (effect (clear-wait-prompt :challenger))}}}
                               card nil)))}})

   "Drone Screen"
   {:events {:run {:req (req (and this-locale tagged))
                   :async true
                   :trace {:base 3
                           :successful
                           {:msg "do 1 meat damage"
                            :effect (effect (damage eid :meat 1 {:card card
                                                                 :unpreventable true}))}}}}}

   "Experiential Data"
   {:effect (req (update-character-in-locale state side (card->locale state card)))
    :events {:pre-character-strength {:req (req (protecting-same-locale? card target))
                                :effect (effect (character-strength-bonus 1 target))}}
    :hide-effect {:effect (req (update-character-in-locale state side (card->locale state card)))}
    :discard-effect {:effect (req (update-all-character state side))}}

   "Expo Grid"
   (let [ability {:req (req (some #(and (is-type? % "Site")
                                        (revealed? %))
                                  (get-in contestant (:zone card))))
                  :msg "gain 1 [Credits]"
                  :once :per-turn
                  :label "Gain 1 [Credits] (start of turn)"
                  :effect (effect (gain-credits 1))}]
   {:hidden-events {:challenger-turn-ends contestant-reveal-toast}
    :events {:contestant-turn-begins ability}
    :abilities [ability]})

   "Forced Connection"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :trace {:base 3
                     :successful {:msg "give the Challenger 2 tags"
                                  :async true
                                  :effect (effect (gain-tags :contestant eid 2))}}}}

   "Fractal Threat Matrix"
   {:implementation "Manual trigger each time all subs are broken"
    :abilities [{:label "Discard the top 2 cards from the Stack"
                 :msg (msg (let [deck (:deck challenger)]
                             (if (pos? (count deck))
                               (str "discard " (join ", " (map :title (take 2 deck))) " from the Stack")
                               "discard the top 2 cards from their Stack - but the Stack is empty")))
                 :effect (effect (mill :contestant :challenger 2))}]}

   "Georgia Emelyov"
   {:events {:unsuccessful-run {:req (req (= (first (:locale target))
                                             (second (:zone card))))
                                :async true
                                :msg "do 1 net damage"
                                :effect (effect (damage eid :net 1 {:card card}))}}
    :abilities [{:cost [:credit 2]
                 :label "Move to another locale"
                 :async true
                 :effect (effect (continue-ability
                                   {:prompt "Choose a locale"
                                    :choices (locale-list state)
                                    :msg (msg "move to " target)
                                    :effect (req (let [c (move state side card
                                                               (conj (locale->zone state target) :content))]
                                                   (unregister-events state side card)
                                                   (register-events state side (:events (card-def c)) c)))}
                                   card nil))}]}

   "Giordano Memorial Field"
   {:events
    {:successful-run
     {:interactive (req true)
      :async true
      :req (req this-locale)
      :msg "force the Challenger to pay or end the run"
      :effect (req (let [credits (:credit challenger)
                         cost (* 2 (count (:scored challenger)))
                         pay-str (str "pay " cost " [Credits]")
                         c-pay-str (capitalize pay-str)]
                     (show-wait-prompt state :contestant (str "Challenger to " pay-str " or end the run"))
                     (continue-ability
                       state :challenger
                       {:player :challenger
                        :async true
                        :prompt (msg "You must " pay-str " or end the run")
                        :choices (concat (when (>= credits cost)
                                           [c-pay-str])
                                         ["End the run"])
                        :effect (req (clear-wait-prompt state :contestant)
                                     (if (= c-pay-str target)
                                       (do (pay state :challenger card :credit cost)
                                           (system-msg state :challenger (str "pays " cost " [Credits]")))
                                       (do (end-run state side)
                                           (system-msg state :contestant "ends the run")))
                                     (effect-completed state side eid))}
                       card nil)))}}}

   "Heinlein Grid"
   {:abilities [{:req (req this-locale)
                 :label "Force the Challenger to lose all [Credits] from spending or losing a [Click]"
                 :msg (msg "force the Challenger to lose all " (:credit challenger) " [Credits]")
                 :once :per-run
                 :effect (effect (lose-credits :challenger :all)
                                 (lose :challenger :run-credit :all))}]}

   "Helheim Locales"
   {:abilities [{:label "Discard 1 card from HQ: All character protecting this locale has +2 strength until the end of the run"
                 :req (req (and this-locale (pos? (count run-characters)) (pos? (count (:hand contestant)))))
                 :async true
                 :effect (req (show-wait-prompt state :challenger "Contestant to use Helheim Locales")
                              (wait-for
                                (resolve-ability
                                  state side
                                  {:prompt "Choose a card in HQ to discard"
                                   :choices {:req #(and (in-hand? %) (= (:side %) "Contestant"))}
                                   :effect (effect (discard target) (clear-wait-prompt :challenger))} card nil)
                                (do (register-events
                                      state side
                                      {:pre-character-strength {:req (req (= (card->locale state card)
                                                                       (card->locale state target)))
                                                          :effect (effect (character-strength-bonus 2 target))}
                                       :run-ends {:effect (effect (unregister-events card))}} card)
                                    (continue-ability
                                      state side
                                      {:effect (req (update-character-in-locale
                                                      state side (card->locale state card)))} card nil))))}]
    :events {:pre-character-strength nil}}

   "Henry Phillips"
   {:implementation "Manually triggered by Contestant"
    :abilities [{:req (req (and this-locale tagged))
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain-credits 2))}]}

   "Hokusai Grid"
   {:events {:successful-run {:req (req this-locale)
                              :msg "do 1 net damage"
                              :async true
                              :effect (effect (damage eid :net 1 {:card card}))}}}

   "Intake"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :trace {:base 4
                     :label "add an placed resource or virtual radicle to the Grip"
                     :successful
                     {:async true
                      :effect (effect (show-wait-prompt :challenger "Contestant to resolve Intake")
                                      (continue-ability
                                        {:prompt "Select a resource or virtual radicle"
                                         :player :contestant
                                         :choices {:req #(and (placed? %)
                                                              (or (resource? %)
                                                                  (and (radicle? %)
                                                                       (has-subtype? % "Virtual"))))}
                                         :msg (msg "move " (:title target) " to the Grip")
                                         :effect (effect (move :challenger target :hand))
                                         :end-effect (req (clear-wait-prompt state :challenger)
                                                          (effect-completed state side eid))}
                                        card nil))}}}}

   "Jinja City Grid"
   (letfn [(place-character [character characters grids locale]
             (let [remaining (remove-once #(= (:cid %) (:cid character)) characters)]
             {:async true
              :effect (req (if (= "None" locale)
                             (continue-ability state side (choose-character remaining grids) card nil)
                             (do (system-msg state side (str "reveals that they drew " (:title character)))
                                 (wait-for (contestant-place state side character locale {:extra-cost [:credit -4]})
                                           (if (= 1 (count characters))
                                             (effect-completed state side eid)
                                             (continue-ability state side (choose-character remaining grids)
                                                               card nil))))))}))

           (choose-grid [character characters grids]
             (if (= 1 (count grids))
               (place-character character characters grids (-> (first grids) :zone second zone->name))
               {:async true
                :prompt (str "Choose a locale to place " (:title character))
                :choices (conj (mapv #(-> % :zone second zone->name) grids) "None")
                :effect (effect (continue-ability (place-character character characters grids target) card nil))}))

           (choose-character [characters grids]
             (if (empty? characters)
               nil
               {:async true
                :prompt "Choose an character to reveal and place, or None to decline"
                :choices (conj (mapv :title characters) "None")
                :effect (req (if (= "None" target)
                               (effect-completed state side eid)
                               (continue-ability state side
                                                 (choose-grid (some #(when (= target (:title %)) %) characters)
                                                              characters grids)
                                                 card nil)))}))]

     {:events {:contestant-draw {;; THIS IS A HACK: it prevents multiple Jinja from showing the "choose a locale to place into" sequence
                           :once :per-turn
                           :once-key :jinja-city-grid-draw
                           :async true
                           :effect (req (cond
                                          ;; If character were drawn, do the full routine.
                                          (some #(is-type? % "Character") (:most-recent-drawn contestant-reg))
                                          (let [characters (filter #(and (is-type? % "Character")
                                                                   (get-card state %))
                                                             (:most-recent-drawn contestant-reg))
                                                grids (filterv #(= "Jinja City Grid" (:title %))
                                                               (all-active-placed state :contestant))]
                                            (when (= :challenger (:active-player @state))
                                              (show-wait-prompt state :challenger "Contestant to resolve Jinja City Grid"))
                                            (if (not-empty characters)
                                              (continue-ability state side (choose-character characters grids) card nil)
                                              (effect-completed state side eid)))
                                          ;; else, if it's the challenger's turn, show a fake prompt so the challenger can't infer that character weren't drawn
                                          (= :challenger (:active-player @state))
                                          (continue-ability
                                            state :contestant
                                            {:prompt "You did not draw any character to use with Jinja City Grid"
                                             :choices ["Carry on!"]
                                             :prompt-type :bogus
                                             :effect nil}
                                            card nil)
                                          ;; otherwise, we done
                                          :else
                                          (effect-completed state side eid)))}
               :post-contestant-draw {:effect (req (swap! state dissoc-in [:per-turn :jinja-city-grid-draw])
                                             (when (= :challenger (:active-player @state))
                                               (clear-wait-prompt state :challenger)))}}})

   "Keegan Lane"
   {:abilities [{:label "[Discard], remove a tag: Discard a resource"
                 :req (req (and this-locale
                                (pos? (get-in @state [:challenger :tag]))
                                (not (empty? (filter #(is-type? % "Resource")
                                                     (all-active-placed state :challenger))))))
                 :msg (msg "remove 1 tag")
                 :effect (req (resolve-ability state side discard-resource card nil)
                              (discard state side card {:cause :ability-cost})
                              (lose-tags state :contestant 1))}]}

   "Khondi Plaza"
   {:recurring (effect (set-prop card :rec-counter (count (get-parties state))))
    :effect (effect (set-prop card :rec-counter (count (get-parties state))))}

   "K. P. Lynn"
   (let [abi {:prompt "Choose one"
              :player :challenger
              :choices ["Take 1 tag" "End the run"]
              :effect (req (if (= target "Take 1 tag")
                             (do (gain-tags state :challenger 1)
                                 (system-msg state :contestant (str "uses K. P. Lynn. Challenger chooses to take 1 tag")))
                             (do (end-run state side)
                                 (system-msg state :contestant (str "uses K. P. Lynn. Challenger chooses to end the run")))))}]
     {:events {:pass-character {:req (req (and this-locale (= (:position run) 1))) ; trigger when last character passed
                          :async true
                          :effect (req (continue-ability state :challenger abi card nil))}
               :run {:req (req (and this-locale
                                    (zero? (:position run)))) ; trigger on unprotected locale
                     :async true
                     :effect (req (continue-ability state :challenger abi card nil))}}})

   "Manta Grid"
   {:events {:successful-run-ends
             {:msg "gain a [Click] next turn"
              :req (req (and (= (first (:locale target)) (second (:zone card)))
                             (or (< (:credit challenger) 6) (zero? (:click challenger)))))
              :effect (req (swap! state update-in [:contestant :extra-click-temp] (fnil inc 0)))}}}

   "Marcus Batty"
   {:abilities [{:req (req this-locale)
                 :label "[Discard]: Start a Psi game"
                 :msg "start a Psi game"
                 :psi {:not-equal {:prompt "Select a revealed piece of Character to resolve one of its subroutines"
                                   :choices {:req #(and (character? %)
                                                        (revealed? %))}
                                   :msg (msg "resolve a subroutine on " (:title target))}}
                 :effect (effect (discard card))}]}

   "Mason Bellamy"
   {:implementation "Manually triggered by Contestant"
    :abilities [{:label "Force the Challenger to lose [Click] after an encounter where they broke a subroutine"
                 :req (req this-locale)
                 :msg "force the Challenger to lose [Click]"
                 :effect (effect (lose :challenger :click 1))}]}

   "Midori"
   {:abilities
    [{:req (req this-locale)
      :label "Swap the Character being approached with a piece of Character from HQ"
      :prompt "Select a piece of Character"
      :choices {:req #(and (character? %)
                           (in-hand? %))}
      :once :per-run
      :msg (msg "swap " (card-str state current-character) " with a piece of Character from HQ")
      :effect (req (let [hqcharacter target
                         c current-character]
                     (resolve-ability state side
                       {:effect (req (let [newcharacter (assoc hqcharacter :zone (:zone c))
                                           cndx (character-index state c)
                                           characters (get-in @state (cons :contestant (:zone c)))
                                           newcharacters (apply conj (subvec characters 0 cndx) newcharacter (subvec characters cndx))]
                                       (swap! state assoc-in (cons :contestant (:zone c)) newcharacters)
                                       (swap! state update-in [:contestant :hand]
                                              (fn [coll] (remove-once #(= (:cid %) (:cid hqcharacter)) coll)))
                                       (trigger-event state side :contestant-place newcharacter)
                                       (move state side c :hand)))} card nil)))}]}

   "Mumbad City Grid"
   {:abilities [{:req (req (let [num-character (count run-characters)]
                             (and this-locale
                                  (>= num-character 2)
                                  (< (:position run 0) num-character))))
                 :label "Swap the Character just passed with another piece of Character protecting this locale"
                 :effect (req (let [passed-character (nth (get-in @state (vec (concat [:contestant :locales] (:locale run) [:characters])))
                                                                                (:position run))
                                    character-zone (:zone passed-character)]
                                 (resolve-ability state :contestant
                                   {:prompt (msg "Select a piece of Character to swap with " (:title passed-character))
                                    :choices {:req #(and (= character-zone (:zone %)) (character? %))}
                                    :effect (req (let [fndx (character-index state passed-character)
                                                       sndx (character-index state target)
                                                       fnew (assoc passed-character :zone (:zone target))
                                                       snew (assoc target :zone (:zone passed-character))]
                                                   (swap! state update-in (cons :contestant character-zone)
                                                          #(assoc % fndx snew))
                                                   (swap! state update-in (cons :contestant character-zone)
                                                          #(assoc % sndx fnew))
                                                   (update-character-strength state side fnew)
                                                   (update-character-strength state side snew)
                                                   (system-msg state side (str "uses Mumbad City Grid to swap "
                                                                               (card-str state passed-character)
                                                                               " with " (card-str state target)))))}
                                                  card nil)))}]}

   "Mumbad Virtual Tour"
   {:implementation "Only forces discard if challenger has no Imps and enough credits in the credit pool"
    :flags {:must-discard (req (when placed
                               true))}
    :access {:req (req placed)
             :effect (req (let [discard-cost (discard-cost state side card)
                                no-salsette (remove #(= (:title %) "Salsette Slums") (all-active state :challenger))
                                slow-discard (any-flag-fn? state :challenger :slow-discard true no-salsette)]
                            (if (and (can-pay? state :challenger nil :credit discard-cost)
                                     (not slow-discard))
                              (do (toast state :challenger "You have been forced to discard Mumbad Virtual Tour" "info")
                                  (swap! state assoc-in [:challenger :register :force-discard] true))
                              (toast state :challenger
                                     (str "You must discard Mumbad Virtual Tour, if able, using any available means "
                                          "(Whizzard, Imp, Ghost Challenger, Net Celebrity...)")))))}
    :discard-effect {:when-inactive true
                   :effect (req (swap! state assoc-in [:challenger :register :force-discard] false))}}

   "Mwanza City Grid"
   (let [gain-creds-and-clear {:req (req (= (:from-locale target) (second (:zone card))))
                               :silent (req true)
                               :effect (req (let [cnt (total-cards-accessed run)
                                                  total (* 2 cnt)]
                                              (access-bonus state :challenger -3)
                                              (when cnt
                                                (gain-credits state :contestant total)
                                                (system-msg state :contestant
                                                            (str "gains " total " [Credits] from Mwanza City Grid")))))}
         boost-access-by-3 {:req (req (= target (second (:zone card))))
                            :msg "force the Challenger to access 3 additional cards"
                            :effect (req (access-bonus state :challenger 3))}]
     {:place-req (req (filter #{"HQ" "R&D"} targets))
      :events {:pre-access boost-access-by-3
               :end-access-phase gain-creds-and-clear}
      ;; TODO: as written, this may fail if mwanza is discarded outside of a run on its locale
      ;; (e.g. mwanza on R&D, run HQ, use polop to discard mwanza mid-run, shiro fires to cause RD
      :discard-effect                     ; if there is a run, mark mwanza effects to remain active until the end of the run
      {:req (req (:run @state))
       :effect (effect (register-events {:pre-access (assoc boost-access-by-3 :req (req (= target (second (:previous-zone card)))))
                                         :end-access-phase (assoc gain-creds-and-clear :req (req (= (:from-locale target) (second (:previous-zone card)))))
                                         :unsuccessful-run-ends {:effect (effect (unregister-events card))}
                                         :successful-run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}})

   "NeoTokyo Grid"
   (let [ng {:req (req (in-same-locale? card target))
             :once :per-turn
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits 1))}]
     {:events {:advance ng
               :advancement-placed ng}})

   "Nihongai Grid"
   {:events
    {:successful-run
     {:interactive (req true)
      :async true
      :req (req (and this-locale
                     (or (< (:credit challenger) 6)
                         (< (count (:hand challenger)) 2))
                     (not-empty (:hand contestant))))
      :effect (req (show-wait-prompt state :challenger "Contestant to use Nihongai Grid")
                   (let [top5 (take 5 (:deck contestant))]
                     (if (pos? (count top5))
                       (continue-ability state side
                         {:optional
                          {:prompt "Use Nihongai Grid to look at top 5 cards of R&D and swap one with a card from HQ?"
                           :yes-ability
                           {:async true
                            :prompt "Choose a card to swap with a card from HQ"
                            :choices top5
                            :effect (req (let [rdc target]
                                           (continue-ability state side
                                             {:async true
                                              :prompt (msg "Choose a card in HQ to swap for " (:title rdc))
                                              :choices {:req in-hand?}
                                              :msg "swap a card from the top 5 of R&D with a card in HQ"
                                              :effect (req (let [hqc target
                                                                 newrdc (assoc hqc :zone [:deck])
                                                                 deck (vec (get-in @state [:contestant :deck]))
                                                                 rdcndx (first (keep-indexed #(when (= (:cid %2) (:cid rdc)) %1) deck))
                                                                 newdeck (seq (apply conj (subvec deck 0 rdcndx) target (subvec deck rdcndx)))]
                                                             (swap! state assoc-in [:contestant :deck] newdeck)
                                                             (swap! state update-in [:contestant :hand]
                                                                    (fn [coll] (remove-once #(= (:cid %) (:cid hqc)) coll)))
                                                             (move state side rdc :hand)
                                                             (clear-wait-prompt state :challenger)
                                                             (effect-completed state side eid)))}
                                            card nil)))}
                           :no-ability {:effect (req (clear-wait-prompt state :challenger)
                                                     (effect-completed state side eid))}}}
                        card nil)
                       (do (clear-wait-prompt state :challenger)
                           (effect-completed state side eid)))))}}}

   "Oaktown Grid"
   {:events {:pre-discard {:req (req (in-same-locale? card target))
                         :effect (effect (discard-cost-bonus 3))}}}

   "Oberth Protocol"
   {:additional-cost [:forfeit]
    :events {:advance {:req (req (and (same-locale? card target)
                                      (= 1 (count (filter #(= (second (:zone %)) (second (:zone card)))
                                                          (map first (turn-events state side :advance)))))))
                       :msg (msg "place an additional advancement token on " (card-str state target))
                       :effect (effect (add-prop :contestant target :advance-counter 1 {:placed true}))}}}

   "Off the Grid"
   {:place-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :effect (req (prevent-run-on-locale state card (second (:zone card))))
    :events {:challenger-turn-begins {:effect (req (prevent-run-on-locale state card (second (:zone card))))}
             :successful-run {:req (req (= target :hq))
                              :effect (req (discard state :contestant card)
                                           (enable-run-on-locale state card
                                                                 (second (:zone card)))
                                           (system-msg state :contestant (str "discards Off the Grid")))}}
    :leave-play (req (enable-run-on-locale state card (second (:zone card))))}

   "Old Hollywood Grid"
   (let [ohg {:req (req (or (in-same-locale? card target)
                            (from-same-locale? card target)))
              :effect (req (register-persistent-flag!
                             state side
                             card :can-steal
                             (fn [state _ card]
                               (if-not (some #(= (:title %) (:title card)) (:scored challenger))
                                 ((constantly false)
                                    (toast state :challenger "Cannot steal due to Old Hollywood Grid." "warning"))
                                 true))))}]
     {:discard-effect
      {:req (req (and (= :locales (first (:previous-zone card))) (:run @state)))
       :effect (req (register-events
                      state side
                      {:pre-steal-cost (assoc ohg :req (req (or (= (:zone (get-nested-host target))
                                                                   (:previous-zone card))
                                                                (= (central->zone (:zone target))
                                                                   (butlast (:previous-zone card))))))
                       :run-ends {:effect (req (unregister-events state side (find-latest state card))
                                               (clear-persistent-flag! state side card :can-steal))}}
                      (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ohg
               :access {:effect (req (clear-persistent-flag! state side card :can-steal))}
               :run-ends nil}})

   "Overseer Matrix"
   (let [om {:req (req (in-same-locale? card target))
             :async true
             :effect (effect (show-wait-prompt :challenger "Contestant to use Overseer Matrix")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 1 [Credits] to use Overseer Matrix ability?"
                                 :player :contestant
                                 :end-effect (effect (clear-wait-prompt :challenger)
                                                     (effect-completed eid))
                                 :yes-ability {:cost [:credit 1]
                                               :msg "give the Challenger 1 tag"
                                               :async true
                                               :effect (req (gain-tags state :contestant eid 1))}}}
                               card nil))}]
     {:discard-effect
      {:req (req (and (= :locales (first (:previous-zone card)))
                      (:run @state)))
       :effect (effect (register-events {:challenger-discard (assoc om :req (req (or (= (:zone (get-nested-host target))
                                                                                  (:previous-zone card))
                                                                               (= (central->zone (:zone target))
                                                                                  (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:run-ends nil
               :challenger-discard om}})

   "Panic Button"
   {:init {:root "HQ"}
    :place-req (req (filter #{"HQ"} targets))
    :abilities [{:cost [:credit 1] :label "Draw 1 card" :effect (effect (draw))
                 :req (req (and run (= (first (:locale run)) :hq)))}]}

   "Port Anson Grid"
   {:msg "prevent the Challenger from jacking out unless they discard an placed resource"
    :effect (req (when this-locale
                   (prevent-jack-out state side)))
    :events {:run {:req (req this-locale)
                   :msg "prevent the Challenger from jacking out unless they discard an placed resource"
                   :effect (effect (prevent-jack-out))}
             :challenger-discard {:req (req (and this-locale (is-type? target "Resource")))
                            :effect (req (swap! state update-in [:run] dissoc :cannot-jack-out))}}}

   "Prisec"
   {:access {:req (req (placed? card))
             :async true
             :effect (effect (show-wait-prompt :challenger "Contestant to use Prisec")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 2 [Credits] to use Prisec ability?"
                                 :end-effect (effect (clear-wait-prompt :challenger))
                                 :yes-ability {:cost [:credit 2]
                                               :msg "do 1 meat damage and give the Challenger 1 tag"
                                               :async true
                                               :effect (req (wait-for (damage state side :meat 1 {:card card})
                                                                      (gain-tags state :contestant eid 1)))}}}
                               card nil))}}

   "Product Placement"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :msg "gain 2 [Credits]" :effect (effect (gain-credits :contestant 2))}}

   "Red Herrings"
   (let [ab {:req (req (or (in-same-locale? card target)
                           (from-same-locale? card target)))
             :effect (effect (steal-cost-bonus [:credit 5]))}]
     {:discard-effect
      {:req (req (and (= :locales (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab :run-ends nil}})

   "Research Station"
   {:init {:root "HQ"}
    :place-req (req (filter #{"HQ"} targets))
    :in-play [:hand-size 2]}

   "Ruhr Valley"
   {:events {:run {:req (req this-locale)
                   :effect (effect (lose :challenger :click 1))
                   :msg "force the Challenger to spend an additional [Click]"}
             :challenger-turn-begins {:req (req (> (:click-per-turn challenger) 1))
                                  :effect (req (enable-run-on-locale state card (second (:zone card))))}
             :challenger-spent-click {:req (req (<= 1 (:click challenger)))
                                  :effect (req (prevent-run-on-locale state card (second (:zone card))))}
             :leave-play (req (enable-run-on-locale state card (second (:zone card))))}}

   "Rutherford Grid"
   {:events {:pre-init-trace {:req (req this-locale)
                              :effect (effect (init-trace-bonus 2))}}}

   "Ryon Knight"
   {:abilities [{:label "[Discard]: Do 1 brain damage"
                 :msg "do 1 brain damage" :req (req (and this-locale (zero? (:click challenger))))
                 :async true
                 :effect (effect (discard card) (damage eid :brain 1 {:card card}))}]}

   "SanSan City Grid"
   {:effect (req (when-let [agenda (some #(when (is-type? % "Agenda") %)
                                         (:content (card->locale state card)))]
                   (update-advancement-cost state side agenda)))
    :events {:contestant-place {:req (req (and (is-type? target "Agenda")
                                           (in-same-locale? card target)))
                            :effect (effect (update-advancement-cost target))}
             :pre-advancement-cost {:req (req (in-same-locale? card target))
                                    :effect (effect (advancement-cost-bonus -1))}}}

   "Satellite Grid"
   {:effect (req (doseq [c (:characters (card->locale state card))]
                   (set-prop state side c :extra-advance-counter 1))
                 (update-all-character state side))
    :events {:contestant-place {:req (req (and (character? target)
                                           (protecting-same-locale? card target)))
                            :effect (effect (set-prop target :extra-advance-counter 1))}}
    :leave-play (req (doseq [c (:characters (card->locale state card))]
                       (update! state side (dissoc c :extra-advance-counter)))
                     (update-all-character state side))}

   "Self-destruct"
   {:place-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :abilities [{:req (req this-locale)
                 :label "[Discard]: Trace X - Do 3 net damage"
                 :effect (req (let [serv (card->locale state card)
                                    cards (concat (:characters serv) (:content serv))]
                                (discard state side card)
                                (doseq [c cards]
                                  (discard state side c))
                                (resolve-ability
                                  state side
                                  {:trace {:base (req (dec (count cards)))
                                           :successful {:msg "do 3 net damage"
                                                        :effect (effect (damage eid :net 3 {:card card}))}}}
                                  card nil)))}]}

   "Shell Contestantoration"
   {:abilities
    [{:cost [:click 1]
      :msg "store 3 [Credits]"
      :once :per-turn
      :effect (effect (add-counter card :credit 3))}
     {:cost [:click 1]
      :msg (msg "gain " (get-counters card :credit) " [Credits]")
      :once :per-turn
      :label "Take all credits"
      :effect (effect (take-credits (get-counters card :credit))
                      (set-prop card :counter {:credit 0}))}]}

   "Signal Jamming"
   {:abilities [{:label "[Discard]: Cards cannot be placed until the end of the run"
                 :msg (msg "prevent cards being placed until the end of the run")
                 :req (req this-locale)
                 :effect (effect (discard (get-card state card) {:cause :ability-cost}))}]
    :discard-effect {:effect (effect (register-run-flag! card :contestant-lock-place (constantly true))
                                   (register-run-flag! card :challenger-lock-place (constantly true))
                                   (toast :challenger "Cannot place until the end of the run")
                                   (toast :contestant "Cannot place until the end of the run"))}
    :events {:run-ends nil}}

   "Simone Diego"
   {:recurring 2}

   "Strongbox"
   (let [ab {:req (req (or (in-same-locale? card target)
                           (from-same-locale? card target)))
             :effect (effect (steal-cost-bonus [:click 1]))}]
     {:discard-effect
      {:req (req (and (= :locales (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab
               :run-ends nil}})

   "Surat City Grid"
   {:events
    {:reveal {:req (req (and (same-locale? card target)
                          (not (and (is-type? target "Region")
                                    (is-central? (second (:zone target)))))
                          (not= (:cid target) (:cid card))
                          (seq (filter #(and (not (revealed? %))
                                             (not (is-type? % "Agenda"))) (all-placed state :contestant)))))
           :effect (effect (resolve-ability
                             {:optional
                              {:prompt (msg "Reveal another card with Surat City Grid?")
                               :yes-ability {:prompt "Select a card to reveal"
                                             :choices {:req #(and (not (revealed? %))
                                                                  (not (is-type? % "Agenda")))}
                                             :msg (msg "reveal " (:title target) ", lowering the reveal cost by 2 [Credits]")
                                             :effect (effect (reveal-cost-bonus -2)
                                                             (reveal target))}}}
                            card nil))}}}

   "Tempus"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :effect (req (when (= (first (:zone card)) :deck)
                            (system-msg state :challenger (str "accesses Tempus"))))
             :trace {:base 3
                     :successful
                     {:msg "make the Challenger choose between losing [Click][Click] or suffering 1 brain damage"
                      :async true
                      :effect (req (let [tempus card]
                                     (if (< (:click challenger) 2)
                                       (do
                                         (system-msg state side "suffers 1 brain damage")
                                         (damage state side eid :brain 1 {:card tempus}))
                                       (do
                                         (show-wait-prompt state :contestant "Challenger to resolve Tempus")
                                         (continue-ability
                                           state :challenger
                                           {:prompt "Lose [Click][Click] or take 1 brain damage?"
                                            :player :challenger
                                            :choices ["Lose [Click][Click]" "Take 1 brain damage"]
                                            :async true
                                            :effect
                                            (req (clear-wait-prompt state :contestant)
                                                 (if (.startsWith target "Take")
                                                   (do
                                                     (system-msg state side (str "chooses to take 1 brain damage"))
                                                     (damage state side eid :brain 1 {:card tempus}))
                                                   (do
                                                     (system-msg state side "chooses to lose [Click][Click]")
                                                     (lose state :challenger :click 2)
                                                     (effect-completed state side eid))))}
                                           card nil)))))}}}}

   "The Twins"
   {:abilities [{:label "Reveal and discard a copy of the Character just passed from HQ"
                 :req (req (and this-locale
                                (> (count (get-run-characters state)) (:position run))
                                (:revealed (get-in (:characters (card->locale state card)) [(:position run)]))))
                 :effect (req (let [charactername (:title (get-in (:characters (card->locale state card)) [(:position run)]))]
                                (resolve-ability
                                  state side
                                  {:prompt "Select a copy of the Character just passed"
                                   :choices {:req #(and (in-hand? %)
                                                        (character? %)
                                                        (= (:title %) charactername))}
                                   :effect (req (discard state side (assoc target :seen true))
                                                (swap! state update-in [:run]
                                                       #(assoc % :position (inc (:position run)))))
                                   :msg (msg "discard a copy of " (:title target) " from HQ and force the Challenger to encounter it again")}
                                 card nil)))}]}

   "Tori Hanzō"
   {:events
    {:pre-resolve-damage
     {:once :per-run
      :async true
      :req (req (and this-locale
                     (= target :net)
                     (pos? (last targets))
                     (can-pay? state :contestant nil [:credit 2])))
      :effect (req (swap! state assoc-in [:damage :damage-replace] true)
                   (damage-defer state side :net (last targets))
                   (show-wait-prompt state :challenger "Contestant to use Tori Hanzō")
                   (continue-ability state side
                     {:optional
                      {:prompt (str "Pay 2 [Credits] to do 1 brain damage with Tori Hanzō?")
                       :player :contestant
                       :yes-ability {:async true
                                     :msg "do 1 brain damage instead of net damage"
                                     :effect (req (swap! state update-in [:damage] dissoc :damage-replace :defer-damage)
                                                  (clear-wait-prompt state :challenger)
                                                  (pay state :contestant card :credit 2)
                                                  (wait-for (damage state side :brain 1 {:card card})
                                                            (do (swap! state assoc-in [:damage :damage-replace] true)
                                                                (effect-completed state side eid))))}
                       :no-ability {:async true
                                    :effect (req (swap! state update-in [:damage] dissoc :damage-replace)
                                                 (clear-wait-prompt state :challenger)
                                                 (effect-completed state side eid))}}}
                     card nil))}
     :prevented-damage {:req (req (and this-locale
                                       (= target :net)
                                       (pos? (last targets))))
                        :effect (req (swap! state assoc-in [:per-run (:cid card)] true))}}}

   "Traffic Analyzer"
   {:events {:reveal {:req (req (and (protecting-same-locale? card target)
                                  (character? target)))
                   :interactive (req true)
                   :trace {:base 2
                           :successful {:msg "gain 1 [Credits]"
                                        :effect (effect (gain-credits 1))}}}}}

   "Tyrs Hand"
   {:abilities [{:label "[Discard]: Prevent a subroutine on a piece of Bioroid Character from being broken"
                 :req (req (and (= (butlast (:zone current-character)) (butlast (:zone card)))
                                (has-subtype? current-character "Bioroid")))
                 :effect (effect (discard card))
                 :msg (msg "prevent a subroutine on " (:title current-character) " from being broken")}]}

   "Underway Grid"
   {:implementation "Bypass prevention is not implemented"
    :events {:pre-expose {:req (req (same-locale? card target))
                          :msg "prevent 1 card from being exposed"
                          :effect (effect (expose-prevent 1))}}}

   "Valley Grid"
   {:implementation "Activation is manual"
    :abilities [{:req (req this-locale)
                 :label "Reduce Challenger's maximum hand size by 1 until start of next Contestant turn"
                 :msg "reduce the Challenger's maximum hand size by 1 until the start of the next Contestant turn"
                 :effect (req (update! state side (assoc card :times-used (inc (get card :times-used 0))))
                              (lose state :challenger :hand-size 1))}]
    :discard-effect {:req (req (and (= :locales (first (:previous-zone card))) (:run @state)))
                   :effect (req (when-let [n (:times-used card)]
                                  (register-events state side
                                                   {:contestant-turn-begins
                                                    {:msg (msg "increase the Challenger's maximum hand size by " n)
                                                     :effect (effect (gain :challenger :hand-size {:mod n})
                                                                     (unregister-events card)
                                                                     (update! (dissoc card :times-used)))}}
                                                   (assoc card :zone '(:discard)))))}
    :events {:contestant-turn-begins {:req (req (:times-used card))
                                :msg (msg "increase the Challenger's maximum hand size by "
                                          (:times-used card))
                                :effect (effect (gain :challenger :hand-size {:mod (:times-used card)})
                                                (update! (dissoc card :times-used)))}}}

   "Warroid Tracker"
   (letfn [(wt [card n t]
             {:prompt "Choose an placed card to discard due to Warroid Tracker"
              :async true
              :player :challenger
              :priority 2
              :choices {:req #(and (placed? %) (= (:side %) "Challenger"))}
              :effect (req (system-msg state side (str "discards " (card-str state target) " due to Warroid Tracker"))
                           (discard state side target {:unpreventable true})
                           (if (> n t)
                             (continue-ability state side (wt card n (inc t)) card nil)
                             (do (clear-wait-prompt state :contestant)
                                 (effect-completed state side eid)))
                           ;; this ends-the-run if WT is the only card and is discarded, and discards at least one challenger card
                           (when (zero? (count (cards-to-access state side (get-in @state [:run :locale]))))
                             (handle-end-run state side)))})]
   {:implementation "Does not handle UFAQ interaction with Singularity"
    :events {:challenger-discard {:async true
                            :req (req (let [target-zone (:zone target)
                                            target-zone (or (central->zone target-zone) target-zone)
                                            warroid-zone (:zone card)]
                                        (= (second warroid-zone)
                                           (second target-zone))))
                            :trace {:base 4
                                    :successful
                                    {:effect
                                     (req (let [n (count (all-placed state :challenger))
                                                n (if (> n 2) 2 n)]
                                            (if (pos? n)
                                              (do (system-msg
                                                    state side
                                                    (str "uses Warroid Tracker to force the challenger to discard "
                                                         (quantify n " placed card")))
                                                  (show-wait-prompt state :contestant "Challenger to choose cards to discard")
                                                  (resolve-ability state side (wt card n 1) card nil))
                                              (system-msg
                                                state side
                                                (str "uses Warroid Tracker but there are no placed cards to discard")))))}}}}})

   "Will-o-the-Wisp"
   {:events
    {:successful-run
     {:interactive (req true)
      :async true
      :req (req (and this-locale
                     (some #(has-subtype? % "Icebreaker") (all-active-placed state :challenger))))
      :effect (req (show-wait-prompt state :challenger "Contestant to use Will-o'-the-Wisp")
                   (continue-ability state side
                     {:optional
                      {:prompt "Discard Will-o'-the-Wisp?"
                       :choices {:req #(has-subtype? % "Icebreaker")}
                       :yes-ability {:async true
                                     :prompt "Choose an characterbreaker used to break at least 1 subroutine during this run"
                                     :choices {:req #(has-subtype? % "Icebreaker")}
                                     :msg (msg "add " (:title target) " to the bottom of the Challenger's Stack")
                                     :effect (effect (discard card)
                                                     (move :challenger target :deck)
                                                     (clear-wait-prompt :challenger)
                                                     (effect-completed eid))}
                       :no-ability {:effect (effect (clear-wait-prompt :challenger)
                                                    (effect-completed eid))}}}
                    card nil))}}}})
