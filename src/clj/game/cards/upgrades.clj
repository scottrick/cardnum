(ns game.cards.upgrades
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int]]
            [cardnum.cards :refer [all-cards]]))

(def card-definitions
  {"Akitaro Watanabe"
   {:events {:pre-rez-cost {:req (req (and (character? target)
                                           (= (card->server state card) (card->server state target))))
                            :effect (effect (rez-cost-bonus -2))}}}

   "Amazon Industrial Zone"
   {:events
     {:contestant-install  {:optional {:req (req (and (character? target)
                                                (protecting-same-server? card target)))
                                 :prompt "Rez Character with rez cost lowered by 3?" :priority 2
                                 :yes-ability {:effect (effect (rez-cost-bonus -3) (rez target))}}}}}

   "Ash 2X3ZB9CY"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-server)
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
                 :effect (req (contestant-install state side target card {:no-install-cost true}))}
                {:req (req (and this-server
                                (zero? (get-in @state [:run :position]))))
                 :label "Rez a hosted piece of Bioroid Character"
                 :prompt "Choose a piece of Bioroid Character to rez" :choices (req (:hosted card))
                 :msg (msg "lower the rez cost of " (:title target) " by 7 [Credits] and force the Challenger to encounter it")
                 :effect (effect (rez-cost-bonus -7) (rez target)
                                 (update! (dissoc (get-card state target) :facedown))
                                 (register-events {:run-ends
                                                    {:effect (req (doseq [c (:hosted card)]
                                                                    (when (:rezzed c)
                                                                      (trash state side c)))
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
                              (effect-completed eid dcard))})]

   {:init {:root "R&D"}
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
   (let [bm {:req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:net-damage 2]))}]
     {:trash-effect
              {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
               :effect (effect (register-events {:pre-steal-cost (assoc bm :req (req (or (= (:zone target) (:previous-zone card))
                                                                                         (= (central->zone (:zone target))
                                                                                            (butlast (:previous-zone card))))))
                                                 :run-ends {:effect (effect (unregister-events card))}}
                                                (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost bm :run-ends nil}})

   "Berncharacter Mai"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-server)
                              :trace {:base 5
                                      :successful {:msg "give the Challenger 1 tag"
                                                   :async true
                                                   :effect (effect (tag-challenger :challenger eid 1))}
                                      :unsuccessful
                                      {:effect (effect (system-msg "trashes Berncharacter Mai from the unsuccessful trace")
                                                       (trash card))}}}}}

  "Bio Vault"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :advanceable :always
   :abilities [{:label "[Trash]: End the run"
                :advance-counter-cost 2
                :req (req (:run @state))
                :msg "end the run. Bio Vault is trashed"
                :async true
                :effect (effect
                          (end-run)
                          (trash eid card {:cause :ability-cost}))}]}

   "Black Level Clearance"
   {:events {:successful-run
             {:interactive (req true)
              :req (req this-server)
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
                                                    (system-msg state :contestant (str "gains 5 [Credits] and draws 1 card. Black Level Clearance is trashed"))
                                                    (trash state side card)
                                                    (effect-completed state side eid))))}
                               card nil))}}}

   "Breaker Bay Grid"
   {:events {:pre-rez-cost {:req (req (in-same-server? card target))
                            :effect (effect (rez-cost-bonus -5))}}}

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
                 :effect (effect (play-instant nil (assoc-in target [:special :rfg-when-trashed] true) {:ignore-cost true})
                                 (move target :rfg))}]}

   "Calibration Testing"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :abilities [{:label "[Trash]: Place 1 advancement token on a card in this server"
                 :async true
                 :effect (effect (continue-ability
                                   {:prompt "Select a card in this server"
                                    :choices {:req #(in-same-server? % card)}
                                    :async true
                                    :msg (msg "place an advancement token on " (card-str state target))
                                    :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                                    (trash eid card {:cause :ability-cost}))}
                                   card nil))}]}


   "Caprcharacter Nisei"
   {:events {:pass-character {:req (req (and this-server
                                       (= (:position run) 1))) ; trigger when last character passed
                        :msg "start a Psi game"
                        :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}
             :run {:req (req (and this-server
                                  (zero? (:position run)))) ; trigger on unprotected server
                   :msg "start a Psi game"
                   :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}}
    :abilities [{:msg "start a Psi game"
                 :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}]}

   "ChiLo City Grid"
   {:events {:successful-trace {:req (req this-server)
                                :async true
                                :effect (effect (tag-challenger :challenger eid 1))
                                :msg "give the Challenger 1 tag"}}}

   "Code Replicator"
   {:abilities [{:label "[Trash]: Force the challenger to approach the passed piece of character again"
                 :req (req (and this-server
                                (> (count (get-run-characters state)) (:position run))
                                (:rezzed (get-in (:characters (card->server state card)) [(:position run)]))))
                 :effect (req (let [charactername (:title (get-in (:characters (card->server state card)) [(:position run)]))]
                                (trash state :contestant (get-card state card))
                                (swap! state update-in [:run] #(assoc % :position (inc (:position run))))
                                 (system-msg state :contestant (str "trashes Code Replicator to make the challenger approach "
                                                              charactername " again"))))}]}

   "Contestantorate Troubleshooter"
   {:abilities [{:label "[Trash]: Add strength to a rezzed Character protecting this server" :choices :credit
                 :prompt "How many credits?"
                 :effect (req (let [boost target]
                                (resolve-ability
                                  state side
                                  {:choices {:req #(and (character? %)
                                                        (rezzed? %))}
                                   :msg (msg "add " boost " strength to " (:title target))
                                   :effect (req (update! state side (assoc card :troubleshooter-target target
                                                                                :troubleshooter-amount boost))
                                                (trash state side (get-card state card))
                                                (update-character-strength state side target))} card nil)))}]
    :events {:pre-character-strength nil :challenger-turn-ends nil :contestant-turn-ends nil}
    :trash-effect
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
   (let [suppress-event {:req (req (and this-server (not= (:cid target) (:cid card))))}]
     {:suppress {:pre-successful-run suppress-event
                 :successful-run suppress-event}
      :events {:pre-successful-run {:silent (req true)
                                    :req (req this-server)
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
    :abilities [{:label "[Trash]: Purge virus counters"
                 :msg "purge virus counters" :effect (effect (trash card) (purge))}]}

   "Dedicated Technician Team"
   {:recurring 2}

   "Defense Construct"
   {:advanceable :always
    :abilities [{:label "[Trash]: Add 1 facedown card from Archives to HQ for each advancement token"
                 :req (req (and run (= (:server run) [:archives])
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
                                 (trash card))}]}

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
   {:events {:run {:req (req (and this-server tagged))
                   :async true
                   :trace {:base 3
                           :successful
                           {:msg "do 1 meat damage"
                            :effect (effect (damage eid :meat 1 {:card card
                                                                 :unpreventable true}))}}}}}

   "Experiential Data"
   {:effect (req (update-character-in-server state side (card->server state card)))
    :events {:pre-character-strength {:req (req (protecting-same-server? card target))
                                :effect (effect (character-strength-bonus 1 target))}}
    :derez-effect {:effect (req (update-character-in-server state side (card->server state card)))}
    :trash-effect {:effect (req (update-all-character state side))}}

   "Expo Grid"
   (let [ability {:req (req (some #(and (is-type? % "Site")
                                        (rezzed? %))
                                  (get-in contestant (:zone card))))
                  :msg "gain 1 [Credits]"
                  :once :per-turn
                  :label "Gain 1 [Credits] (start of turn)"
                  :effect (effect (gain-credits 1))}]
   {:derezzed-events {:challenger-turn-ends contestant-rez-toast}
    :events {:contestant-turn-begins ability}
    :abilities [ability]})

   "Forced Connection"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :trace {:base 3
                     :successful {:msg "give the Challenger 2 tags"
                                  :async true
                                  :effect (effect (tag-challenger :challenger eid 2))}}}}

   "Fractal Threat Matrix"
   {:implementation "Manual trigger each time all subs are broken"
    :abilities [{:label "Trash the top 2 cards from the Stack"
                 :msg (msg (let [deck (:deck challenger)]
                             (if (pos? (count deck))
                               (str "trash " (join ", " (map :title (take 2 deck))) " from the Stack")
                               "trash the top 2 cards from their Stack - but the Stack is empty")))
                 :effect (effect (mill :contestant :challenger 2))}]}

   "Georgia Emelyov"
   {:events {:unsuccessful-run {:req (req (= (first (:server target)) (second (:zone card))))
                                :async true
                                :msg "do 1 net damage"
                                :effect (effect (damage eid :net 1 {:card card}))}}
    :abilities [{:cost [:credit 2]
                 :label "Move to another server"
                 :async true
                 :effect (effect (continue-ability
                                   {:prompt "Choose a server"
                                    :choices (server-list state)
                                    :msg (msg "move to " target)
                                    :effect (req (let [c (move state side card
                                                               (conj (server->zone state target) :content))]
                                                   (unregister-events state side card)
                                                   (register-events state side (:events (card-def c)) c)))}
                                   card nil))}]}

   "Heinlein Grid"
   {:abilities [{:req (req this-server)
                 :label "Force the Challenger to lose all [Credits] from spending or losing a [Click]"
                 :msg (msg "force the Challenger to lose all " (:credit challenger) " [Credits]") :once :per-run
                 :effect (effect (lose-credits :challenger :all)
                                 (lose :challenger :run-credit :all))}]}

   "Helheim Servers"
   {:abilities [{:label "Trash 1 card from HQ: All character protecting this server has +2 strength until the end of the run"
                 :req (req (and this-server (pos? (count run-characters)) (pos? (count (:hand contestant)))))
                 :async true
                 :effect (req (show-wait-prompt state :challenger "Contestant to use Helheim Servers")
                              (wait-for
                                (resolve-ability
                                  state side
                                  {:prompt "Choose a card in HQ to trash"
                                   :choices {:req #(and (in-hand? %) (= (:side %) "Contestant"))}
                                   :effect (effect (trash target) (clear-wait-prompt :challenger))} card nil)
                                (do (register-events
                                      state side
                                      {:pre-character-strength {:req (req (= (card->server state card)
                                                                       (card->server state target)))
                                                          :effect (effect (character-strength-bonus 2 target))}
                                       :run-ends {:effect (effect (unregister-events card))}} card)
                                    (continue-ability
                                      state side
                                      {:effect (req (update-character-in-server
                                                      state side (card->server state card)))} card nil))))}]
    :events {:pre-character-strength nil}}

   "Henry Phillips"
   {:implementation "Manually triggered by Contestant"
    :abilities [{:req (req (and this-server tagged))
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain-credits 2))}]}

   "Hokusai Grid"
   {:events {:successful-run {:req (req this-server)
                              :msg "do 1 net damage"
                              :async true
                              :effect (effect (damage eid :net 1 {:card card}))}}}

   "Intake"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :interactive (req true)
             :trace {:base 4
                     :label "add an installed resource or virtual muthereff to the Grip"
                     :successful
                     {:async true
                      :effect (req (show-wait-prompt state :challenger "Contestant to resolve Intake")
                                   (continue-ability
                                     state :contestant
                                     {:prompt "Select a resource or virtual muthereff"
                                      :player :contestant
                                      :choices {:req #(and (installed? %)
                                                           (or (resource? %)
                                                               (and (muthereff? %)
                                                                    (has-subtype? % "Virtual"))))}
                                      :async true
                                      :msg (msg "move " (:title target) " to the Grip")
                                      :effect (effect (move :challenger target :hand))
                                      :end-effect (effect (clear-wait-prompt :challenger)
                                                          (effect-completed eid))}
                                     card nil))}}}}

   "Jinja City Grid"
   (letfn [(install-character [character characters grids server]
             (let [remaining (remove-once #(= (:cid %) (:cid character)) characters)]
             {:async true
              :effect (req (if (= "None" server)
                             (continue-ability state side (choose-character remaining grids) card nil)
                             (do (system-msg state side (str "reveals that they drew " (:title character)))
                                 (wait-for (contestant-install state side character server {:extra-cost [:credit -4]})
                                           (if (= 1 (count characters))
                                             (effect-completed state side eid)
                                             (continue-ability state side (choose-character remaining grids)
                                                               card nil))))))}))

           (choose-grid [character characters grids]
             (if (= 1 (count grids))
               (install-character character characters grids (-> (first grids) :zone second zone->name))
               {:async true
                :prompt (str "Choose a server to install " (:title character))
                :choices (conj (mapv #(-> % :zone second zone->name) grids) "None")
                :effect (effect (continue-ability (install-character character characters grids target) card nil))}))

           (choose-character [characters grids]
             (if (empty? characters)
               nil
               {:async true
                :prompt "Choose an character to reveal and install, or None to decline"
                :choices (conj (mapv :title characters) "None")
                :effect (req (if (= "None" target)
                               (effect-completed state side eid)
                               (continue-ability state side
                                                 (choose-grid (some #(when (= target (:title %)) %) characters)
                                                              characters grids)
                                                 card nil)))}))]

     {:events {:contestant-draw {:req (req (some #(is-type? % "Character")
                                           (:most-recent-drawn contestant-reg)))
                           ;; THIS IS A HACK: it prevents multiple Jinja from showing the "choose a server to install into" sequence
                           :once :per-turn
                           :once-key :jinja-city-grid-draw
                           :async true
                           :effect (req (let [characters (filter #(and (is-type? % "Character")
                                                                 (get-card state %))
                                                           (:most-recent-drawn contestant-reg))
                                              grids (filterv #(= "Jinja City Grid" (:title %))
                                                             (all-active-installed state :contestant))]
                                          (if (not-empty characters)
                                            (continue-ability state side (choose-character characters grids) card nil)
                                            (effect-completed state side eid))))}
               :post-contestant-draw {:effect (req (swap! state dissoc-in [:per-turn :jinja-city-grid-draw]))}}})

   "Keegan Lane"
   {:abilities [{:label "[Trash], remove a tag: Trash a resource"
                 :req (req (and this-server
                                (pos? (get-in @state [:challenger :tag]))
                                (not (empty? (filter #(is-type? % "Resource")
                                                     (all-active-installed state :challenger))))))
                 :msg (msg "remove 1 tag")
                 :effect (req (resolve-ability state side trash-resource card nil)
                              (trash state side card {:cause :ability-cost})
                              (lose state :challenger :tag 1))}]}

   "Khondi Plaza"
   {:recurring (effect (set-prop card :rec-counter (count (get-remotes state))))
    :effect (effect (set-prop card :rec-counter (count (get-remotes state))))}

   "K. P. Lynn"
   (let [abi {:prompt "Choose one"
              :player :challenger
              :choices ["Take 1 tag" "End the run"]
              :effect (req (if (= target "Take 1 tag")
                             (do (tag-challenger state :challenger 1)
                                 (system-msg state :contestant (str "uses K. P. Lynn. Challenger chooses to take 1 tag")))
                             (do (end-run state side)
                                 (system-msg state :contestant (str "uses K. P. Lynn. Challenger chooses to end the run")))))}]
     {:events {:pass-character {:req (req (and this-server (= (:position run) 1))) ; trigger when last character passed
                          :async true
                          :effect (req (continue-ability state :challenger abi card nil))}
               :run {:req (req (and this-server
                                    (zero? (:position run)))) ; trigger on unprotected server
                     :async true
                     :effect (req (continue-ability state :challenger abi card nil))}}})

   "Manta Grid"
   {:events {:successful-run-ends
             {:msg "gain a [Click] next turn"
              :req (req (and (= (first (:server target)) (second (:zone card)))
                             (or (< (:credit challenger) 6) (zero? (:click challenger)))))
              :effect (req (swap! state update-in [:contestant :extra-click-temp] (fnil inc 0)))}}}

   "Marcus Batty"
   {:abilities [{:req (req this-server)
                 :label "[Trash]: Start a Psi game"
                 :msg "start a Psi game"
                 :psi {:not-equal {:prompt "Select a rezzed piece of Character to resolve one of its subroutines"
                                   :choices {:req #(and (character? %)
                                                        (rezzed? %))}
                                   :msg (msg "resolve a subroutine on " (:title target))}}
                 :effect (effect (trash card))}]}

   "Mason Bellamy"
   {:implementation "Manually triggered by Contestant"
    :abilities [{:label "Force the Challenger to lose [Click] after an encounter where they broke a subroutine"
                 :req (req this-server)
                 :msg "force the Challenger to lose [Click]"
                 :effect (effect (lose :challenger :click 1))}]}

   "Midori"
   {:abilities
    [{:req (req this-server)
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
                                       (trigger-event state side :contestant-install newcharacter)
                                       (move state side c :hand)))} card nil)))}]}

   "Mumbad City Grid"
   {:abilities [{:req (req (let [num-character (count run-characters)]
                             (and this-server
                                  (>= num-character 2)
                                  (< (:position run 0) num-character))))
                 :label "Swap the Character just passed with another piece of Character protecting this server"
                 :effect (req (let [passed-character (nth (get-in @state (vec (concat [:contestant :servers] (:server run) [:characters])))
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
   {:implementation "Only forces trash if challenger has no Imps and enough credits in the credit pool"
    :flags {:must-trash (req (when installed
                               true))}
    :access {:req (req installed)
             :effect (req (let [trash-cost (trash-cost state side card)
                                no-salsette (remove #(= (:title %) "Salsette Slums") (all-active state :challenger))
                                slow-trash (any-flag-fn? state :challenger :slow-trash true no-salsette)]
                            (if (and (can-pay? state :challenger nil :credit trash-cost)
                                     (not slow-trash))
                              (do (toast state :challenger "You have been forced to trash Mumbad Virtual Tour" "info")
                                  (swap! state assoc-in [:challenger :register :force-trash] true))
                              (toast state :challenger
                                     (str "You must trash Mumbad Virtual Tour, if able, using any available means "
                                          "(Whizzard, Imp, Ghost Challenger, Net Celebrity...)")))))}
    :trash-effect {:when-inactive true
                   :effect (req (swap! state assoc-in [:challenger :register :force-trash] false))}}

   "Mwanza City Grid"
   (let [gain-creds {:req (req (and installed
                                    this-server
                                    (pos? (:cards-accessed run 0))))
                     :silent (req true)
                     :effect (req (let [cnt (:cards-accessed run)
                                        total (* 2 cnt)]
                                    (gain-credits state :contestant total)
                                    (system-msg state :contestant
                                                (str "gains " total " [Credits] from Mwanza City Grid"))))}]
     {:install-req (req (filter #{"HQ" "R&D"} targets))
      :events {:pre-access {:req (req (and installed
                                           ;; Pre-access server is same server as that Mwanza is in the root of
                                           (= target (second  (:zone card)))))
                            :msg "force the Challenger to access 3 additional cards"
                            :effect (effect (access-bonus 3))}
               :run-ends gain-creds}
      :trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:run-ends
                                         (assoc gain-creds :req (req (= (first (:server run))
                                                                        (second (:previous-zone card)))))
                                         :successful-run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}})

   "NeoTokyo Grid"
   (let [ng {:req (req (in-same-server? card target))
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
      :req (req (and this-server
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
   {:events {:pre-trash {:req (req (in-same-server? card target))
                         :effect (effect (trash-cost-bonus 3))}}}

   "Oberth Protocol"
   {:additional-cost [:forfeit]
    :events {:advance {:req (req (and (same-server? card target)
                                      (= 1 (count (filter #(= (second (:zone %)) (second (:zone card)))
                                                          (map first (turn-events state side :advance)))))))
                       :msg (msg "place an additional advancement token on " (card-str state target))
                       :effect (effect (add-prop :contestant target :advance-counter 1 {:placed true}))}}}

   "Off the Grid"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :effect (req (prevent-run-on-server state card (second (:zone card))))
    :events {:challenger-turn-begins {:effect (req (prevent-run-on-server state card (second (:zone card))))}
             :successful-run {:req (req (= target :hq))
                              :effect (req (trash state :contestant card)
                                           (enable-run-on-server state card
                                                                 (second (:zone card)))
                                           (system-msg state :contestant (str "trashes Off the Grid")))}}
    :leave-play (req (enable-run-on-server state card (second (:zone card))))}

   "Old Hollywood Grid"
   (let [ohg {:req (req (or (in-same-server? card target)
                            (from-same-server? card target)))
              :effect (req (register-persistent-flag!
                             state side
                             card :can-steal
                             (fn [state _ card]
                               (if-not (some #(= (:title %) (:title card)) (:scored challenger))
                                 ((constantly false)
                                    (toast state :challenger "Cannot steal due to Old Hollywood Grid." "warning"))
                                 true))))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
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
   (let [om {:req (req (in-same-server? card target))
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
                                               :effect (req (tag-challenger state :challenger eid 1))}}}
                               card nil))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card)))
                      (:run @state)))
       :effect (effect (register-events {:challenger-trash (assoc om :req (req (or (= (:zone (get-nested-host target))
                                                                                  (:previous-zone card))
                                                                               (= (central->zone (:zone target))
                                                                                  (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:run-ends nil
               :challenger-trash om}})

   "Panic Button"
   {:init {:root "HQ"}
    :abilities [{:cost [:credit 1] :label "Draw 1 card" :effect (effect (draw))
                 :req (req (and run (= (first (:server run)) :hq)))}]}

   "Port Anson Grid"
   {:msg "prevent the Challenger from jacking out unless they trash an installed resource"
    :effect (req (when this-server
                   (prevent-jack-out state side)))
    :events {:run {:req (req this-server)
                   :msg "prevent the Challenger from jacking out unless they trash an installed resource"
                   :effect (effect (prevent-jack-out))}
             :challenger-trash {:req (req (and this-server (is-type? target "Resource")))
                            :effect (req (swap! state update-in [:run] dissoc :cannot-jack-out))}}}

   "Prisec"
   {:access {:req (req (installed? card))
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
                                                                      (tag-challenger state :challenger eid 1)))}}}
                               card nil))}}

   "Product Placement"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :msg "gain 2 [Credits]" :effect (effect (gain-credits :contestant 2))}}

   "Red Herrings"
   (let [ab {:req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:credit 5]))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab :run-ends nil}})

   "Research Station"
   {:init {:root "HQ"}
    :in-play [:hand-size 2]}

   "Ruhr Valley"
   {:events {:run {:req (req this-server)
                   :effect (effect (lose :challenger :click 1))
                   :msg "force the Challenger to spend an additional [Click]"}
             :challenger-turn-begins {:req (req (> (:click-per-turn challenger) 1))
                                  :effect (req (enable-run-on-server state card (second (:zone card))))}
             :challenger-spent-click {:req (req (<= 1 (:click challenger)))
                                  :effect (req (prevent-run-on-server state card (second (:zone card))))}
             :leave-play (req (enable-run-on-server state card (second (:zone card))))}}

   "Rutherford Grid"
   {:events {:pre-init-trace {:req (req this-server)
                              :effect (effect (init-trace-bonus 2))}}}

   "Ryon Knight"
   {:abilities [{:label "[Trash]: Do 1 brain damage"
                 :msg "do 1 brain damage" :req (req (and this-server (zero? (:click challenger))))
                 :async true
                 :effect (effect (trash card) (damage eid :brain 1 {:card card}))}]}

   "SanSan City Grid"
   {:effect (req (when-let [agenda (some #(when (is-type? % "Agenda") %)
                                         (:content (card->server state card)))]
                   (update-advancement-cost state side agenda)))
    :events {:contestant-install {:req (req (and (is-type? target "Agenda")
                                           (in-same-server? card target)))
                            :effect (effect (update-advancement-cost target))}
             :pre-advancement-cost {:req (req (in-same-server? card target))
                                    :effect (effect (advancement-cost-bonus -1))}}}

   "Satellite Grid"
   {:effect (req (doseq [c (:characters (card->server state card))]
                   (set-prop state side c :extra-advance-counter 1))
                 (update-all-character state side))
    :events {:contestant-install {:req (req (and (character? target)
                                           (protecting-same-server? card target)))
                            :effect (effect (set-prop target :extra-advance-counter 1))}}
    :leave-play (req (doseq [c (:characters (card->server state card))]
                       (update! state side (dissoc c :extra-advance-counter)))
                     (update-all-character state side))}

   "Self-destruct"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :abilities [{:req (req this-server)
                 :label "[Trash]: Trace X - Do 3 net damage"
                 :effect (req (let [serv (card->server state card)
                                    cards (concat (:characters serv) (:content serv))]
                                (trash state side card)
                                (doseq [c cards]
                                  (trash state side c))
                                (resolve-ability
                                  state side
                                  {:trace {:base (req (dec (count cards)))
                                           :successful {:msg "do 3 net damage"
                                                        :effect (effect (damage eid :net 3 {:card card}))}}}
                                  card nil)))}]}

   "Shell Corporation"
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
   {:abilities [{:label "[Trash]: Cards cannot be installed until the end of the run"
                 :msg (msg "prevent cards being installed until the end of the run")
                 :req (req this-server)
                 :effect (effect (trash (get-card state card) {:cause :ability-cost}))}]
    :trash-effect {:effect (effect (register-run-flag! card :contestant-lock-install (constantly true))
                                   (register-run-flag! card :challenger-lock-install (constantly true))
                                   (toast :challenger "Cannot install until the end of the run")
                                   (toast :contestant "Cannot install until the end of the run"))}
    :events {:run-ends nil}}

   "Simone Diego"
   {:recurring 2}

   "Strongbox"
   (let [ab {:req (req (or (in-same-server? card target)
                           (from-same-server? card target)))
             :effect (effect (steal-cost-bonus [:click 1]))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab
               :run-ends nil}})

   "Surat City Grid"
   {:events
    {:rez {:req (req (and (same-server? card target)
                          (not (and (is-type? target "Region")
                                    (is-central? (second (:zone target)))))
                          (not= (:cid target) (:cid card))
                          (seq (filter #(and (not (rezzed? %))
                                             (not (is-type? % "Agenda"))) (all-installed state :contestant)))))
           :effect (effect (resolve-ability
                             {:optional
                              {:prompt (msg "Rez another card with Surat City Grid?")
                               :yes-ability {:prompt "Select a card to rez"
                                             :choices {:req #(and (not (rezzed? %))
                                                                  (not (is-type? % "Agenda")))}
                                             :msg (msg "rez " (:title target) ", lowering the rez cost by 2 [Credits]")
                                             :effect (effect (rez-cost-bonus -2)
                                                             (rez target))}}}
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
   {:abilities [{:label "Reveal and trash a copy of the Character just passed from HQ"
                 :req (req (and this-server
                                (> (count (get-run-characters state)) (:position run))
                                (:rezzed (get-in (:characters (card->server state card)) [(:position run)]))))
                 :effect (req (let [charactername (:title (get-in (:characters (card->server state card)) [(:position run)]))]
                                (resolve-ability
                                  state side
                                  {:prompt "Select a copy of the Character just passed"
                                   :choices {:req #(and (in-hand? %)
                                                        (character? %)
                                                        (= (:title %) charactername))}
                                   :effect (req (trash state side (assoc target :seen true))
                                                (swap! state update-in [:run]
                                                       #(assoc % :position (inc (:position run)))))
                                   :msg (msg "trash a copy of " (:title target) " from HQ and force the Challenger to encounter it again")}
                                 card nil)))}]}

   "Tori Hanzō"
   {:events
    {:pre-resolve-damage
     {:once :per-run
      :async true
      :req (req (and this-server
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
     :prevented-damage {:req (req (and this-server
                                       (= target :net)
                                       (pos? (last targets))))
                        :effect (req (swap! state assoc-in [:per-run (:cid card)] true))}}}

   "Traffic Analyzer"
   {:events {:rez {:req (req (and (protecting-same-server? card target)
                                  (character? target)))
                   :interactive (req true)
                   :trace {:base 2
                           :successful {:msg "gain 1 [Credits]"
                                        :effect (effect (gain-credits 1))}}}}}

   "Tyrs Hand"
   {:abilities [{:label "[Trash]: Prevent a subroutine on a piece of Bioroid Character from being broken"
                 :req (req (and (= (butlast (:zone current-character)) (butlast (:zone card)))
                                (has-subtype? current-character "Bioroid")))
                 :effect (effect (trash card))
                 :msg (msg "prevent a subroutine on " (:title current-character) " from being broken")}]}

   "Underway Grid"
   {:implementation "Bypass prevention is not implemented"
    :events {:pre-expose {:req (req (same-server? card target))
                          :msg "prevent 1 card from being exposed"
                          :effect (effect (expose-prevent 1))}}}

   "Valley Grid"
   {:implementation "Activation is manual"
    :abilities [{:req (req this-server)
                 :label "Reduce Challenger's maximum hand size by 1 until start of next Contestant turn"
                 :msg "reduce the Challenger's maximum hand size by 1 until the start of the next Contestant turn"
                 :effect (req (update! state side (assoc card :times-used (inc (get card :times-used 0))))
                              (lose state :challenger :hand-size 1))}]
    :trash-effect {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
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
             {:prompt "Choose an installed card to trash due to Warroid Tracker"
              :async true
              :player :challenger
              :priority 2
              :choices {:req #(and (installed? %) (= (:side %) "Challenger"))}
              :effect (req (system-msg state side (str "trashes " (card-str state target) " due to Warroid Tracker"))
                           (trash state side target {:unpreventable true})
                           (if (> n t)
                             (continue-ability state side (wt card n (inc t)) card nil)
                             (do (clear-wait-prompt state :contestant)
                                 (effect-completed state side eid)))
                           ;; this ends-the-run if WT is the only card and is trashed, and trashes at least one challenger card
                           (when (zero? (count (cards-to-access state side (get-in @state [:run :server]))))
                             (handle-end-run state side)))})]
   {:implementation "Does not handle UFAQ interaction with Singularity"
    :events {:challenger-trash {:async true
                            :req (req (= (-> card :zone second) (-> target :zone second)))
                            :trace {:base 4
                                    :successful
                                    {:effect
                                     (req (let [n (count (all-installed state :challenger))
                                                n (if (> n 2) 2 n)]
                                            (if (pos? n)
                                              (do (system-msg
                                                    state side
                                                    (str "uses Warroid Tracker to force the challenger to trash "
                                                         (quantify n " installed card")))
                                                  (show-wait-prompt state :contestant "Challenger to choose cards to trash")
                                                  (resolve-ability state side (wt card n 1) card nil))
                                              (system-msg
                                                state side
                                                (str "uses Warroid Tracker but there are no installed cards to trash")))))}}}}})

   "Will-o-the-Wisp"
   {:events
    {:successful-run
     {:interactive (req true)
      :async true
      :req (req (and this-server
                     (some #(has-subtype? % "Icebreaker") (all-active-installed state :challenger))))
      :effect (req (show-wait-prompt state :challenger "Contestant to use Will-o'-the-Wisp")
                   (continue-ability state side
                     {:optional
                      {:prompt "Trash Will-o'-the-Wisp?"
                       :choices {:req #(has-subtype? % "Icebreaker")}
                       :yes-ability {:async true
                                     :prompt "Choose an characterbreaker used to break at least 1 subroutine during this run"
                                     :choices {:req #(has-subtype? % "Icebreaker")}
                                     :msg (msg "add " (:title target) " to the bottom of the Challenger's Stack")
                                     :effect (effect (trash card)
                                                     (move :challenger target :deck)
                                                     (clear-wait-prompt :challenger)
                                                     (effect-completed eid card))}
                       :no-ability {:effect (effect (clear-wait-prompt :challenger)
                                                    (effect-completed eid card))}}}
                    card nil))}}}})
