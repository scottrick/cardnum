(ns game.cards.hazard
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int]]
            [cardnum.cards :refer [all-cards]]))

(def card-definitions
  {
   "Alone and Unadvised"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Aware of Their Ways"
   {:abilities [{:effect (req
                           (move state side card :discard)
                           (let [opp-side (if (= side :contestant)
                                            :challenger
                                            side)
                                 kount (count (get-in @state [opp-side :discard]))]
                             (loop [k (if (< kount 4) kount 4)]
                               (when (> k 0)
                                 (move state opp-side (rand-nth (get-in @state [opp-side :discard])) :play-area)
                                 (recur (- k 1))))
                             (resolve-ability state side
                                              {:prompt "Select a card to remove from the game"
                                               :choices {:req (fn [t] (card-is? t :side opp-side))}
                                               :effect (req (doseq [c (get-in @state [opp-side :play-area])]
                                                              (if (= c target)
                                                                (move state opp-side c :rfg)
                                                                (move state opp-side c :discard)
                                                                )))} nil nil)))}]}
   "Bring Our Curses Home"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Cast from the Order"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Covetous Thoughts"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Cruel Claw Perceived"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Despair of the Heart"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Diminish and Depart"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Dragons Curse"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Enchanted Stream"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Eyes of the Shadow"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Fear of Kin"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Fled into Darkness"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Flies and Spiders"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Foes Shall Fall"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Fools Bane"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Foolish Words"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "He is Lost to Us"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Heritage Forsaken"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Icy Touch"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "In the Grip of Ambition"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Inner Rot"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Longing for the West"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Lure of Creation"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Lure of Expedience"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Lure of Nature"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Lure of the Senses"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Many Burdens"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Memories Stolen"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Morgul-knife"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Nothing to Eat or Drink"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Out of Practice"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Pale Dream-maker"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Plague"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Politics"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Power Relinquished to Artifice"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Ransom"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Rebel-talk"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Revealed to all Watchers"
   {:abilities [{:label "Resolve"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:msg (msg "to draw 3 cards.")
                                                  :effect (effect (set-hand-aside :hand))} card nil)))}]}
   "Shut Yer Mouth"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "So Youve Come Back"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Something Else at Work"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Something Has Slipped"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Spells of the Barrow-wights"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "The Burden of Time"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "The Pale Sword"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Tookish Blood"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Wielders Curse"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Will You Not Come Down?"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Wound of Long Burden"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Acacia"
   {:events {:pre-purge {:effect (req (let [counters (number-of-virus-counters state)]
                                        (update! state side (assoc-in (get-card state card) [:special :numpurged] counters))))}
             :purge {:async true
                     :effect (effect (show-wait-prompt  :contestant "Challenger to decide if they will use Acacia")
                                     (continue-ability
                                       {:optional
                                        {:player :challenger
                                         :prompt "Use Acacia?"
                                         :yes-ability {:effect (req (let [counters (- (get-in (get-card state card) [:special :numpurged])
                                                                                      (number-of-virus-counters state))]
                                                                      (gain-credits state side counters)
                                                                      (system-msg state side (str "uses Acacia and gains " counters "[Credit]"))
                                                                      (discard state side card)
                                                                      (clear-wait-prompt state :contestant)
                                                                      (effect-completed state side eid)))}
                                         :no-ability {:effect (effect (clear-wait-prompt :contestant)
                                                                      (effect-completed eid))}}}
                                       card nil))}}}

   "Adjusted Matrix"
   {:implementation "Click Adjusted Matrix to use ability."
    :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-placed state :challenger))))
    :prompt "Choose Icebreaker on which to place Adjusted Matrix"
    :choices {:req #(and (= (:side %) "Challenger") (has-subtype? % "Icebreaker") (placed? %))}
    :msg (msg "host it on " (card-str state target))
    :effect (effect (update! (assoc target :subtype (combine-subtypes false (-> target :subtype) "AI")))
                    (host (get-card state target) (get-card state card)))
    :abilities [{:cost [:click 1]
                 :req (req run)
                 :msg "break character subroutine"}]
    :events {:pre-card-moved {:req (req (= (:cid target) (:cid card)))
                              :effect (effect (update! (assoc (-> card :host) :subtype (-> card :host :subtype (remove-subtypes-once ["AI"])))))}}}

   "Akamatsu Mem Chip"
   {:in-play [:memory 1]}

   "Archives Interface"
   {:events
    {:pre-access
     {:async true
      :interactive (req true)
      :req (req (and (= target :archives)
                     (not= (:max-access run) 0)
                     (not-empty (:discard contestant))))
      :effect (req (swap! state update-in [:contestant :discard] #(map (fn [c] (assoc c :seen true)) %))
                   (continue-ability state side
                     {:optional
                      {:prompt "Use Archives Interface to remove a card from the game instead of accessing it?"
                       :yes-ability {:prompt "Choose a card in Archives to remove from the game instead of accessing"
                                     :choices (req (:discard contestant))
                                     :msg (msg "remove " (:title target) " from the game")
                                     :effect (effect (move :contestant target :rfg))}}} card nil))}}}
   "Astrolabe"
   {:in-play [:memory 1]
    :events {:locale-created {:msg "draw 1 card"
                              :async true
                              :effect (effect (draw :challenger eid 1 nil))}}}

   "Autoscripter"
   {:events {:challenger-place {:silent (req true)
                              :req (req (and (is-type? target "Resource")
                                             (= (:active-player @state) :challenger)
                                             ;; only trigger when played a resourcem from grip
                                             (some #{:hand} (:previous-zone target))
                                             ;; check if didn't played a resource from the grip this turn
                                             (empty? (let [cards (map first (turn-events state side :challenger-place))
                                                           progs (filter #(is-type? % "Resource") cards)]
                                                          (filter #(some #{:hand} (:previous-zone %)) progs)))))
                              :msg "gain [Click]" :effect (effect (gain :click 1))}
             :unsuccessful-run {:effect (effect (discard card)
                                                (system-msg "discards Autoscripter"))}}}

   "Blackguard"
   {:in-play [:memory 2]
    :events {:expose
             {:msg (msg "attempt to force the reveal of " (:title target))
              :async true
              :effect (req (let [c target
                                 cdef (card-def c)
                                 cname (:title c)]
                             (if (:additional-cost cdef)
                               (do (show-wait-prompt state :challenger (str "Contestant to decide if they will reveal " cname))
                                   (continue-ability state side
                                     {:optional
                                      {:prompt (msg "Pay additional cost to reveal " cname "?")
                                       :player :contestant
                                       :yes-ability {:effect (effect (reveal :contestant c)
                                                                     (clear-wait-prompt :challenger))}
                                       :no-ability {:effect (effect (system-msg :contestant (str "declines to pay additional costs"
                                                                                       " and is not forced to reveal " cname))
                                                                    (clear-wait-prompt :challenger))}}}
                                    card nil))
                               (do (reveal state :contestant target)
                                   (effect-completed state side eid)))))}}}

   "Bookmark"
   {:abilities [{:label "Host up to 3 cards from your Grip facedown"
                 :cost [:click 1] :msg "host up to 3 cards from their Grip facedown"
                 :choices {:max 3
                           :req #(and (= (:side %) "Challenger")
                                      (in-hand? %))}
                 :effect (req (doseq [c targets]
                                 (host state side (get-card state card) c {:facedown true})))}
                {:label "Add all hosted cards to Grip" :cost [:click 1] :msg "add all hosted cards to their Grip"
                 :effect (req (doseq [c (:hosted card)]
                                (move state side c :hand)))}
                {:label "[Discard]: Add all hosted cards to Grip" :msg "add all hosted cards to their Grip"
                 :effect (req (doseq [c (:hosted card)]
                                (move state side c :hand))
                              (update! state side (dissoc card :hosted))
                              (discard state side (get-card state card) {:cause :ability-cost}))}]}

   "Box-E"
   {:in-play [:memory 2 :hand-size 2]}

   "Brain Cage"
   {:in-play [:hand-size 3]
    :effect (effect (damage eid :brain 1 {:card card}))}

   "Brain Chip"
   (let [challenger-points (fn [s] (max (get-in s [:challenger :agenda-point] 0) 0))]
     {:effect (req (gain state :challenger
                         :memory (challenger-points @state)
                         :hand-size (challenger-points @state))
                   (add-watch state (keyword (str "brainchip" (:cid card)))
                              (fn [k ref old new]
                                (let [bonus (- (challenger-points new) (challenger-points old))]
                                  (when-not (zero? bonus)
                                    (gain state :challenger
                                          :memory bonus
                                          :hand-size bonus))))))
      :leave-play (req (remove-watch state (keyword (str "brainchip" (:cid card))))
                       (lose state :challenger
                             :memory (challenger-points @state)
                             :hand-size (challenger-points @state)))})

   "Capstone"
   {:abilities [{:req (req (pos? (count (:hand challenger))))
                 :cost [:click 1]
                 :effect (req (let [handsize (count (:hand challenger))]
                                (resolve-ability state side
                                  {:prompt "Select any number of cards to discard from your Grip"
                                   :choices {:max handsize
                                             :req #(and (= (:side %) "Challenger")
                                                        (in-hand? %))}
                                   :effect (req (let [discarded (count targets)
                                                      remaining (- handsize discarded)]
                                                  (doseq [c targets]
                                                    (when (not (empty? (filter #(= (:title c) (:title %))
                                                                               (all-active-placed state :challenger))))
                                                      (draw state side)))
                                                  (discard-cards state side targets)
                                                  (system-msg state side
                                                    (str "spends [Click] to use Capstone to discard "
                                                      (join ", " (map :title targets)) " and draw "
                                                      (- (count (get-in @state [:challenger :hand])) remaining) " cards"))))}
                                 card nil)))}]}

   "Chop Bot 3000"
   {:flags {:challenger-phase-12 (req (>= 2 (count (all-placed state :challenger))))}
    :abilities [{:msg (msg "discard " (:title target))
                 :choices {:req #(and (= (:side %) "Challenger") (:placed %))
                           :not-self (req (:cid card))}
                 :effect (effect (discard target)
                                 (resolve-ability
                                   {:prompt "Draw 1 card or remove 1 tag" :msg (msg (.toLowerCase target))
                                    :choices ["Draw 1 card" "Remove 1 tag"]
                                    :effect (req (if (= target "Draw 1 card")
                                                   (draw state side)
                                                   (lose state side :tag 1)))} card nil))}]}

   "Clone Chip"
   {:abilities [{:prompt "Select a resource to place from your Heap"
                 :priority true :show-discard true
                 :req (req (and (not (seq (get-in @state [:challenger :locked :discard])))
                               (not (place-locked? state side))))
                 :choices {:req #(and (is-type? % "Resource")
                                      (= (:zone %) [:discard]))}
                 :effect (req (when (>= (:credit challenger) (:cost target))
                                    (do (challenger-place state side target)
                                        (discard state side card {:cause :ability-cost})
                                        (system-msg state side (str "uses " (:title card) " to place " (:title target))))))}]}

   "Comet"
   {:in-play [:memory 1]
    :events {:play-event {:req (req (first-event? state side :play-event))
                          :effect (req (system-msg state :challenger
                                                   (str "can play another event without spending a [Click] by clicking on Comet"))
                                       (update! state side (assoc card :comet-event true)))}}
    :abilities [{:req (req (:comet-event card))
                 :prompt "Select an Event in your Grip to play"
                 :choices {:req #(and (is-type? % "Event")
                                      (in-hand? %))}
                 :msg (msg "play " (:title target))
                 :effect (effect (play-instant target)
                                 (update! (dissoc (get-card state card) :comet-event)))}]}

   "Cortez Chip"
   {:abilities [{:prompt "Select a piece of Character"
                 :choices {:req character?}
                 :effect (req (let [character target]
                                (update! state side (assoc card :cortez-target character))
                                (discard state side (get-card state card) {:cause :ability-cost})
                                (system-msg state side
                                  (str "discards Cortez Chip to increase the reveal cost of " (card-str state character)
                                       " by 2 [Credits] until the end of the turn"))))}]
    :discard-effect {:effect (effect (register-events {:pre-reveal {:req (req (= (:cid target) (:cid (:cortez-target card))))
                                                               :effect (effect (reveal-cost-bonus 2))}
                                                     :challenger-turn-ends {:effect (effect (unregister-events card))}
                                                     :contestant-turn-ends {:effect (effect (unregister-events card))}}
                                                    (get-card state card)))}
    :events {:pre-reveal nil :challenger-turn-ends nil :contestant-turn-ends nil}}

   "Cyberdelia"
   {:implementation "Credit gain is manually triggered."
    :in-play [:memory 1]
    :abilities [{:msg "gain 1 [Credits] for breaking all subroutines on a piece of character"
                 :once :per-turn
                 :effect (effect (gain-credits 1))}]}

   "Cyberfeeder"
   {:recurring 1}

   "CyberSolutions Mem Chip"
   {:in-play [:memory 2]}

   "Cybsoft MacroDrive"
   {:recurring 1}

   "Daredevil"
   {:in-play [:memory 2]
    :events {:run-big {:once :per-turn
                       :req (req (first-event? state side :run-big))
                       :msg "draw two cards"
                       :effect (effect (draw 2))}}}

   "Dedicated Processor"
   {:implementation "Click Dedicated Processor to use ability"
    :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-placed state :challenger))))
    :hosting {:req #(and (has-subtype? % "Icebreaker")
                         (not (has-subtype? % "AI"))
                         (placed? %))}
    :abilities [{:cost [:credit 2]
                 :req (req run)
                 :effect (effect (pump (get-card state (:host card)) 4))
                 :msg (msg (str "pump the strength of " (get-in card [:host :title]) " by 4"))}]}

   "Deep Red"
   {:implementation "MU use restriction not enforced"
    :in-play [:memory 3]
    :events {:challenger-place
             {:optional
              {:req (req (has-subtype? target "Caïssa"))
               :prompt "Use Deep Red?" :priority 1
               :yes-ability {:async true
                             :effect (req (let [cid (:cid target)]
                                            (continue-ability state side
                                              {:async true
                                               :prompt "Choose the just-placed Caïssa to have Deep Red trigger its [Click] ability"
                                               :choices {:req #(= cid (:cid %))}
                                               :msg (msg "trigger the [Click] ability of " (:title target)
                                                         " without spending [Click]")
                                               :effect (req (gain state :challenger :click 1)
                                                            (play-ability state side {:card target :ability 0})
                                                            (effect-completed state side eid))}
                                             card nil)))}}}}}

   "Desperado"
   {:in-play [:memory 1]
    :events {:successful-run {:silent (req true)
                              :msg "gain 1 [Credits]" :effect (effect (gain-credits 1))}}}

   "Dinosaurus"
   {:abilities [{:label "Place a non-AI characterbreaker on Dinosaurus"
                 :req (req (empty? (:hosted card))) :cost [:click 1]
                 :prompt "Select a non-AI characterbreaker in your Grip to place on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (in-hand? %))}
                 :effect (effect (challenger-place target {:host-card card :no-mu true})
                                 (update! (assoc-in (get-card state card) [:special :dino-breaker] (:cid target))))}
                {:label "Host an placed non-AI characterbreaker on Dinosaurus"
                 :req (req (empty? (:hosted card)))
                 :prompt "Select an placed non-AI characterbreaker to host on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (placed? %))}
                 :msg (msg "host " (:title target))
                 :effect (req (free-mu state (:memoryunits target))
                              (->> target
                                (get-card state)
                                (host state side card)
                                (update-breaker-strength state side))
                              (update! state side (assoc-in (get-card state card) [:special :dino-breaker] (:cid target))))}]
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (first (:hosted card)))))
                                    :effect (effect (breaker-strength-bonus 2))}
             :card-moved {:req (req (= (:cid target) (get-in (get-card state card) [:special :dino-breaker])))
                          :effect (effect (update! (dissoc-in card [:special :dino-breaker]))
                                          (use-mu (:memoryunits target)))}}}

   "Doppelgänger"
   {:in-play [:memory 1]
    :events {:challenger-place
             {:req (req (= card target))
              :silent (req true)
              :effect (effect (update! (assoc card :dopp-active true)))}
             :challenger-turn-begins
             {:effect (effect (update! (assoc card :dopp-active true)))}
             :successful-run-ends
             {:interactive (req true)
              :optional
              {:req (req (:dopp-active card))
               :player :challenger
               :prompt "Use Doppelgänger to run again?"
               :yes-ability {:prompt "Choose a locale"
                             :async true
                             :choices (req runnable-locales)
                             :msg (msg "make a run on " target)
                             :makes-run true
                             :effect (effect (update! (dissoc card :dopp-active))
                                             (clear-wait-prompt :contestant)
                                             (run eid target))}}}}}

   "Dorm Computer"
   {:data {:counter {:power 4}}
    :abilities [{:counter-cost [:power 1]
                 :cost [:click 1]
                 :req (req (not run))
                 :prompt "Choose a locale"
                 :choices (req runnable-locales)
                 :msg "make a run and avoid all tags for the remainder of the run"
                 :makes-run true
                 :effect (effect (update! (assoc card :dorm-active true))
                                 (run target))}]
    :events {:pre-tag {:req (req (:dorm-active card))
                       :effect (effect (tag-prevent Integer/MAX_VALUE))
                       :msg "avoid all tags during the run"}
             :run-ends {:effect (effect (update! (dissoc card :dorm-active)))}}}

   "Dyson Fractal Generator"
   {:recurring 1}

   "Dyson Mem Chip"
   {:in-play [:memory 1 :link 1]}

   "e3 Feedback Implants"
   {:implementation "Usage restriction not enforced"
    :abilities [{:cost [:credit 1] :msg "break 1 additional subroutine"}]}

   "Ekomind"
   (let [update-base-mu (fn [state n] (swap! state assoc-in [:challenger :memory :base] n))]
     {:effect (req (update-base-mu state (count (get-in @state [:challenger :hand])))
                   (add-watch state :ekomind (fn [k ref old new]
                                               (let [hand-size (count (get-in new [:challenger :hand]))]
                                                 (when (not= (count (get-in old [:challenger :hand])) hand-size)
                                                   (update-base-mu ref hand-size))))))
      :leave-play (req (remove-watch state :ekomind))})

   "EMP Devcharacter"
   {:abilities [{:req (req (:run @state))
                 :msg "prevent the Contestant from revealing more than 1 piece of Character for the remainder of the run"
                 :effect (effect (register-events
                                   {:reveal {:req (req (character? target))
                                          :effect (effect (register-run-flag!
                                                            card :can-reveal
                                                            (fn [state side card]
                                                              (if (character? card)
                                                                ((constantly false)
                                                                 (toast state :contestant "Cannot reveal Character the rest of this run due to EMP Devcharacter"))
                                                                true))))}
                                    :run-ends {:effect (effect (unregister-events card))}} (assoc card :zone '(:discard)))
                                 (discard card {:cause :ability-cost}))}]
    :events {:reveal nil
             :run-ends nil}}

   "Feedback Filter"
   {:interactions {:prevent [{:type #{:net :brain}
                              :req (req true)}]}
    :abilities [{:cost [:credit 3]
                 :msg "prevent 1 net damage"
                 :effect (effect (damage-prevent :net 1))}
                {:label "[Discard]: Prevent up to 2 brain damage"
                 :msg "prevent up to 2 brain damage"
                 :effect (effect (discard card {:cause :ability-cost})
                                 (damage-prevent :brain 2))}]}

   "Flame-out"
   (let [turn-end {:async true
                   :effect (req (unregister-events state :challenger card)
                                (if-let [hosted (first (:hosted card))]
                                  (do
                                    (system-msg state :challenger (str "discards " (:title hosted) " from Flame-out"))
                                    (discard state side eid hosted nil))
                                  (effect-completed state side eid)))}]
   {:implementation "Credit usage restriction not enforced"
    :data {:counter {:credit 9}}
    :abilities [{:label "Take 1[Credits] from Flame-out"
                 :req (req (and (not-empty (:hosted card))
                                (pos? (get-counters card :credit))))
                 :counter-cost [:credit 1]
                 :effect (req (gain-credits state :challenger 1)
                              (system-msg state :challenger "takes 1[Credits] from Flame-out")
                              (register-events
                                state :challenger
                                {:challenger-turn-ends turn-end
                                 :contestant-turn-ends turn-end}
                                (get-card state card)))}
                {:label "Take all [Credits] from Flame-out"
                 :req (req (and (not-empty (:hosted card))
                                (pos? (get-counters card :credit))))
                 :effect (req (let [credits (get-counters card :credit)]
                                (gain-credits state :challenger credits)
                                (update! state :challenger (dissoc-in card [:counter :credit]))
                                (system-msg state :challenger (str "takes " credits "[Credits] from Flame-out"))
                                (register-events
                                  state :challenger
                                  {:challenger-turn-ends turn-end
                                   :contestant-turn-ends turn-end}
                                  (get-card state card))))}
                 {:label "Place a resource on Flame-out"
                 :req (req (empty? (:hosted card)))
                 :cost [:click 1]
                 :prompt "Select a resource in your Grip to place on Flame-out"
                 :choices {:req #(and (is-type? % "Resource")
                                      (in-hand? %))}
                 :effect (effect (challenger-place target {:host-card card})
                                 (update! (assoc-in (get-card state card) [:special :flame-out] (:cid target))))}
                {:label "Host an placed resource on Flame-out"
                 :req (req (empty? (:hosted card)))
                 :prompt "Select an placed resource to host on Flame-out"
                 :choices {:req #(and (is-type? % "Resource")
                                      (placed? %))}
                 :msg (msg "host " (:title target))
                 :effect (req (->> target
                                (get-card state)
                                (host state side card))
                              (update! state side (assoc-in (get-card state card) [:special :flame-out] (:cid target))))}]
    :events {:card-moved {:req (req (= (:cid target) (get-in (get-card state card) [:special :flame-out])))
                          :effect (effect (update! (dissoc-in card [:special :flame-out])))}
             :challenger-turn-ends nil
             :contestant-turn-ends nil}})

   "Forger"
   {:interactions {:prevent [{:type #{:tag}
                              :req (req true)}]}
    :in-play [:link 1]
    :abilities [{:msg "avoid 1 tag" :label "[Discard]: Avoid 1 tag"
                 :effect (effect (tag-prevent 1) (discard card {:cause :ability-cost}))}
                {:msg "remove 1 tag" :label "[Discard]: Remove 1 tag"
                 :effect (effect (discard card {:cause :ability-cost}) (lose :tag 1))}]}

   "Friday Chip"
   (let [ability {:msg (msg "move 1 virus counter to " (:title target))
                  :req (req (and (pos? (get-counters card :virus))
                                 (pos? (count-virus-resources state))))
                  :choices {:req is-virus-resource?}
                  :effect (req (add-counter state :challenger card :virus -1)
                               (add-counter state :challenger target :virus 1))}]
     {:abilities [{:effect (effect (update! (update-in card [:special :auto-accept] #(not %)))
                                   (toast (str "Friday Chip will now "
                                               (if (get-in card [:special :auto-accept]) "no longer " "")
                                               "automatically add counters.") "info"))
                   :label "Toggle auomatically adding virus counters"}]
      :effect (effect (toast "Tip: You can toggle automatically adding virus counters by clicking Friday Chip."))
      :events {:challenger-turn-begins ability
               :challenger-discard {:async true
                              :req (req (some #(card-is? % :side :contestant) targets))
                              :effect (req (let [amt-discarded (count (filter #(card-is? % :side :contestant) targets))
                                                 auto-ab {:effect (effect (system-msg :challenger
                                                                                      (str "places "
                                                                                           (quantify amt-discarded "virus counter")
                                                                                           " on Friday Chip"))
                                                                    (add-counter :challenger card :virus amt-discarded))}
                                                 sing-ab {:optional {:prompt "Place a virus counter on Friday Chip?"
                                                                     :yes-ability {:effect (effect (system-msg
                                                                                                     :challenger
                                                                                                     "places 1 virus counter on Friday Chip")
                                                                                                   (add-counter :challenger card :virus 1))}}}
                                                 mult-ab {:prompt "Place virus counters on Friday Chip?"
                                                          :choices {:number (req amt-discarded)
                                                                    :default (req amt-discarded)}
                                                          :effect (effect (system-msg :challenger
                                                                                      (str "places "
                                                                                           (quantify target "virus counter")
                                                                                           " on Friday Chip"))
                                                                          (add-counter :challenger card :virus target))}
                                                 ab (if (> amt-discarded 1) mult-ab sing-ab)
                                                 ab (if (get-in card [:special :auto-accept]) auto-ab ab)]
                                             (continue-ability state side ab card targets)))}}})

   "Gebrselassie"
   {:abilities [{:msg (msg "host it on an placed non-AI characterbreaker")
                 :cost [:click 1]
                 :choices {:req #(and (placed? %)
                                      (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI")))}
                 :effect (req (when-let [host (get-card state (:host card))]
                                (update! state side (dissoc-in host [:pump :all-turn]))
                                (update-breaker-strength state side host))
                              (host state side target card))}]
    :events {:pump-breaker {:silent (req true)
                            :req (req (= (:cid (second targets)) (:cid (:host card))))
                            :effect (effect (update! (update-in (second targets) [:pump :all-turn] (fnil #(+ % (first targets)) 0)))
                                            (update-breaker-strength (second targets)))}}
    :leave-play (req (when-let [host (get-card state (:host card))]
                       (update! state side (dissoc-in host [:pump :all-turn]))
                       (update-breaker-strength state side host)))}

   "GPI Net Tap"
   {:implementation "Discard and jack out effect is manual"
    :abilities [{:req (req (and (character? current-character) (not (revealed? current-character))))
                 :async true
                 :effect (effect (expose eid current-character))}]}

   "Grimoire"
   {:in-play [:memory 2]
    :events {:challenger-place {:silent (req true)
                              :req (req (has-subtype? target "Virus"))
                              :effect (effect (add-counter target :virus 1))}}}

   "Heartbeat"
   {:in-play [:memory 1]
    :interactions {:prevent [{:type #{:net :brain :meat}
                              :req (req true)}]}
    :abilities [{:msg (msg "prevent 1 damage, discarding a facedown " (:title target))
                 :choices {:req #(and (= (:side %) "Challenger") (:placed %))}
                 :priority 50
                 :effect (effect (discard target {:unpreventable true})
                                 (damage-prevent :brain 1)
                                 (damage-prevent :meat 1)
                                 (damage-prevent :net 1))}]}

   "Hippo"
   {:implementation "Subroutine and first encounter requirements not enforced"
    :abilities [{:label "Remove Hippo from the game: discard outermost piece of Character if all subroutines were broken"
                 :req (req (and run
                                (pos? (count run-characters))))
                 :async true
                 :effect (req (let [character (last run-characters)]
                                (system-msg
                                  state :challenger
                                  (str "removes Hippo from the game to discard " (card-str state character)))
                                (move state :challenger card :rfg)
                                (discard state :contestant eid character nil)))}]}

   "HQ Interface"
   {:in-play [:hq-access 1]}

   "Knobkierie"
   {:implementation "MU usage restriction not enforced"
    :in-play [:memory 3]
    :events {:successful-run
             {:req (req (and (first-event? state :challenger :successful-run)
                             (pos? (count-virus-resources state))))
              :optional
                   {:prompt "Place a virus counter?"
                    :yes-ability {:prompt "Select an placed virus resource"
                                  :choices {:req #(and (placed? %)
                                                       (has-subtype? % "Virus")
                                                       (is-type? % "Resource"))}
                                  :msg (msg "place 1 virus counter on " (:title target))
                                  :effect (effect (add-counter target :virus 1))}}}}}

   "Lemuria Codecracker"
   {:abilities [{:cost [:click 1 :credit 1]
                 :req (req (some #{:hq} (:successful-run challenger-reg)))
                 :choices {:req placed?}
                 :effect (effect (expose eid target))
                 :msg "expose 1 card"}]}

   "LLDS Memory Diamond"
   {:in-play [:link 1 :memory 1 :hand-size 1]}

   "LLDS Processor"
   {:events
     (let [llds {:effect (req (let [cards (:llds-target card)]
                                (update! state side (dissoc card :llds-target))
                                (doseq [c cards]
                                (update-breaker-strength state side
                                                         (find-cid (:cid c) (all-active-placed state :challenger))))))}]
       {:challenger-turn-ends llds :contestant-turn-ends llds
        :challenger-place {:silent (req true)
                         :req (req (has-subtype? target "Icebreaker"))
                         :effect (effect (update! (update-in card [:llds-target] #(conj % target)))
                                         (update-breaker-strength target))}
        :pre-breaker-strength {:req (req (some #(= (:cid target) (:cid %)) (:llds-target card)))
                               :effect (effect (breaker-strength-bonus 1))}})}

   "Lockpick"
   {:recurring 1}

   "Logos"
   {:in-play [:memory 1 :hand-size 1]
    :events {:agenda-scored
             {:player :challenger :prompt "Choose a card" :msg (msg "add 1 card to their Grip from their Stack")
              :choices (req (cancellable (:deck challenger)))
              :effect (effect (trigger-event :searched-stack nil)
                              (shuffle! :deck)
                              (move target :hand))}}}

   "Māui"
   {:in-play [:memory 2]
    :recurring (effect (set-prop card :rec-counter (count (:characters (get-in @state [:contestant :locales :hq])))))
    :effect (effect (set-prop card :rec-counter (count (:characters (get-in @state [:contestant :locales :hq])))))}

   "Maw"
   (let [ability {:label "Discard a card from HQ"
                  :req (req (and (= 1 (get-in @state [:challenger :register :no-discard-or-steal]))
                                 (pos? (count (:hand contestant)))
                                 (not= (first (:zone target)) :discard)))
                  :once :per-turn
                  :msg "force the Contestant to discard a random card from HQ"
                  :effect (req (let [card-to-discard (first (shuffle (:hand contestant)))
                                     card-seen? (= (:cid target) (:cid card-to-discard))
                                     card-to-discard (if card-seen? (assoc card-to-discard :seen true)
                                                                  card-to-discard)]
                                 ;; toggle access flag to prevent Hiro issue #2638
                                 (swap! state dissoc :access)
                                 (discard state :contestant card-to-discard)
                                 (swap! state assoc :access true)))}]
     {:in-play [:memory 2]
      :abilities [ability]
      :events {:post-access-card ability}})

   "Maya"
   {:in-play [:memory 2]
    :abilities [{:once :per-turn
                 :async true
                 :label "Move this accessed card to bottom of R&D"
                 :req (req (when-let [accessed-card (-> @state :challenger :prompt first :card)]
                             (in-deck? accessed-card)))
                 :msg "move the card just accessed to the bottom of R&D"
                 :effect (req (let [accessed-card (-> @state :challenger :prompt first :card)]
                                (move state :contestant accessed-card :deck)
                                (wait-for (tag-challenger state :challenger (make-eid state) 1)
                                          (close-access-prompt state side))))}
                {:once :per-turn
                 :label "Move a previously accessed card to bottom of R&D"
                 :effect (effect (resolve-ability
                                   {:async true
                                    ;; only allow targeting cards that were accessed this turn
                                    :choices {:req #(some (fn [accessed-card]
                                                            (= (:cid %) (:cid accessed-card)))
                                                          (map first (turn-events state side :access)))}
                                    :msg (msg "move " (:title target) " to the bottom of R&D")
                                    :effect (req (move state :contestant target :deck)
                                                 (tag-challenger state :challenger eid 1)
                                                 (swap! state update-in [side :prompt] rest)
                                                 (when-let [run (:run @state)]
                                                   (when (and (:ended run)
                                                              (empty? (get-in @state [:challenger :prompt])))
                                                     (handle-end-run state :challenger))))}
                                   card nil))}]}

   "MemStrips"
   {:implementation "MU usage restriction not enforced"
    :in-play [:memory 3]}

   "Mirror"
   {:in-play [:memory 2]
    :events {:successful-run
             {:async true
              :req (req (= target :rd))
              :effect (effect (continue-ability
                                {:prompt "Select a card and replace 1 spent [Recurring Credits] on it"
                                 :choices {:req #(< (get-counters % :recurring) (:recurring (card-def %) 0))}
                                 :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                 :effect (effect (add-prop target :rec-counter 1))}
                               card nil))}}}

   "Monolith"
   (let [mhelper (fn mh [n] {:prompt "Select a resource to place"
                             :choices {:req #(and (is-type? % "Resource")
                                                  (in-hand? %))}
                             :effect (req (place-cost-bonus state side [:credit -4])
                                          (challenger-place state side target nil)
                                            (when (< n 3)
                                              (resolve-ability state side (mh (inc n)) card nil)))})]
     {:interactions {:prevent [{:type #{:net :brain}
                                :req (req true)}]}
      :in-play [:memory 3]
      :effect (effect (resolve-ability (mhelper 1) card nil))
      :abilities [{:msg (msg "prevent 1 brain or net damage by discarding " (:title target))
                   :priority 50
                   :choices {:req #(and (is-type? % "Resource")
                                        (in-hand? %))}
                   :prompt "Choose a resource to discard from your Grip"
                   :effect (effect (discard target)
                                   (damage-prevent :brain 1)
                                   (damage-prevent :net 1))}]})

   "Muresh Bodysuit"
   {:events {:pre-damage {:once :per-turn :once-key :muresh-bodysuit
                          :req (req (= target :meat))
                          :msg "prevent the first meat damage this turn"
                          :effect (effect (damage-prevent :meat 1))}}}

   "Net-Ready Eyes"
   {:effect (effect (damage eid :meat 2 {:unboostable true :card card})) :msg "suffer 2 meat damage"
    :events {:run {:choices {:req #(and (placed? %)
                                        (has-subtype? % "Icebreaker"))}
                   :msg (msg "give " (:title target) " +1 strength")
                   :effect (effect (pump target 1 :all-run))}}}

   "NetChip"
   {:abilities [{:label "Place a resource on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-placed state :challenger)))]
                                (resolve-ability state side
                                  {:cost [:click 1]
                                   :prompt "Select a resource in your Grip to place on NetChip"
                                   :choices {:req #(and (is-type? % "Resource")
                                                        (challenger-can-place? state side % false)
                                                        (<= (:memoryunits %) n)
                                                        (in-hand? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (challenger-place target {:host-card card :no-mu true})
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-resources
                                                                   (cons (:cid target) (:hosted-resources card)))))}
                                 card nil)))}
                {:label "Host an placed resource on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-placed state :challenger)))]
                                (resolve-ability state side
                                  {:prompt "Select an placed resource to host on NetChip"
                                   :choices {:req #(and (is-type? % "Resource")
                                                        (<= (:memoryunits %) n)
                                                        (placed? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (host card target)
                                                   (free-mu (:memoryunits target))
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-resources
                                                                   (cons (:cid target) (:hosted-resources card)))))}
                                 card nil)))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-resources card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-resources
                                                          (remove #(= (:cid target) %) (:hosted-resources card))))
                                          (use-mu (:memoryunits target)))}}}

   "Obelus"
   {:in-play [:memory 1]
    :effect (req (gain state :challenger :hand-size {:mod (:tag challenger)})
                 (add-watch state :obelus
                   (fn [k ref old new]
                     (let [tagnew (get-in new [:challenger :tag] 0)
                           tagold (get-in old [:challenger :tag] 0)]
                       (when (> tagnew tagold)
                         (gain state :challenger :hand-size {:mod (- tagnew tagold)}))
                       (when (< tagnew tagold)
                         (lose state :challenger :hand-size {:mod (- tagold tagnew)}))))))
    :leave-play (req (remove-watch state :obelus)
                     (lose state :challenger :hand-size {:mod (:tag challenger)}))
    :events {:successful-run-ends {:once :per-turn
                                   :req (req (and (#{:rd :hq} (first (:locale target)))
                                                  (first-event? state side :successful-run-ends
                                                                #(#{:rd :hq} (first (:locale (first %)))))))
                                   :msg (msg "draw " (:cards-accessed target 0) " cards")
                                   :effect (effect (draw (:cards-accessed target 0)))}}}

   "Omni-drive"
   {:recurring 1
    :abilities [{:label "Place and host a resource of 1[Memory Unit] or less on Omni-drive"
                 :req (req (empty? (:hosted card)))
                 :cost [:click 1]
                 :prompt "Select a resource of 1[Memory Unit] or less to place on Omni-drive from your grip"
                 :choices {:req #(and (is-type? % "Resource")
                                      (<= (:memoryunits %) 1)
                                      (in-hand? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (challenger-place target {:host-card card :no-mu true})
                                 (update! (assoc (get-card state card) :Omnidrive-prog (:cid target))))}
                {:label "Host an placed resource of 1[Memory Unit] or less on Omni-drive"
                 :prompt "Select an placed resource of 1[Memory Unit] or less to host on Omni-drive"
                 :choices {:req #(and (is-type? % "Resource")
                                      (<= (:memoryunits %) 1)
                                      (placed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (free-mu (:memoryunits target))
                                 (update! (assoc (get-card state card) :Omnidrive-prog (:cid target))))}]
   :events {:card-moved {:req (req (= (:cid target) (:Omnidrive-prog (get-card state card))))
                          :effect (effect (update! (dissoc card :Omnidrive-prog))
                                          (use-mu (:memoryunits target)))}}}

   "Plascrete Carapace"
   {:data [:counter {:power 4}]
    :interactions {:prevent [{:type #{:meat}
                              :req (req true)}]}
    :abilities [{:counter-cost [:power 1]
                 :msg "prevent 1 meat damage"
                 :effect (req (damage-prevent state side :meat 1)
                              (when (zero? (get-counters (get-card state card) :power))
                                (discard state side card {:unpreventable true})))}]}

   "Polyhistor"
   (let [abi {:optional
              {:prompt "Draw 1 card to force the Contestant to draw 1 card?"
               :yes-ability {:msg "draw 1 card and force the Contestant to draw 1 card"
                             :effect (effect (draw :challenger 1)
                                             (draw :contestant 1))}
               :no-ability {:effect (req (system-msg state side (str "does not use Polyhistor"))
                                         (effect-completed state side eid))}}}]
     {:in-play [:link 1 :memory 1]
      :events {:pass-character {:req (req (and (= (:locale run) [:hq])
                                         (= (:position run) 1) ; trigger when last Character passed
                                         (pos? (count (:deck challenger)))))
                          :async true
                          :once :per-turn
                          :effect (req (continue-ability state :challenger abi card nil))}
               :run {:req (req (and (= (:locale run) [:hq])
                                    (zero? (:position run)) ; trigger on unprotected HQ
                                    (pos? (count (:deck challenger)))))
                     :async true
                     :once :per-turn
                     :effect (req (continue-ability state :challenger abi card nil))}}})

   "Prepaid VocharacterPAD"
   {:recurring 1}

   "Public Terminal"
   {:recurring 1}

   "Q-Coherence Chip"
   {:in-play [:memory 1]
    :events (let [e {:req (req (= (last (:zone target)) :resource))
                     :effect (effect (discard card)
                                     (system-msg (str "discards Q-Coherence Chip")))}]
              {:challenger-discard e :contestant-discard e})}

   "Qianju PT"
   {:flags {:challenger-phase-12 (req true)}
    :abilities [{:label "Lose [Click], avoid 1 tag (start of turn)"
                 :once :per-turn
                 :req (req (:challenger-phase-12 @state))
                 :effect (effect (update! (assoc card :qianju-active true)))
                 :msg "lose [Click] and avoid the first tag received until their next turn"}]
    :events {:contestant-turn-ends {:effect (effect (update! (dissoc card :qianju-active)))}
             :challenger-turn-begins {:req (req (:qianju-active card))
                                  :effect (effect (lose :click 1))}
             :pre-tag {:req (req (:qianju-active card))
                       :msg "avoid the first tag received"
                       :effect (effect (tag-prevent 1)
                                       (update! (dissoc card :qianju-active)))}}}

   "R&D Interface"
   {:in-play [:rd-access 1]}

   "Rabbit Hole"
   {:in-play [:link 1]
    :effect
    (effect (resolve-ability
             {:optional {:req (req (some #(when (= (:title %) "Rabbit Hole") %) (:deck challenger)))
                         :prompt "Place another Rabbit Hole?" :msg "place another Rabbit Hole"
                         :yes-ability {:effect (req (when-let [c (some #(when (= (:title %) "Rabbit Hole") %)
                                                                      (:deck challenger))]
                                                     (trigger-event state side :searched-stack nil)
                                                     (shuffle! state :challenger :deck)
                                                     (challenger-place state side c)))}}} card nil))}

   "Ramujan-reliant 550 BMI"
   {:interactions {:prevent [{:type #{:net :brain}
                              :req (req true)}]}
    :abilities [{:req (req (not-empty (:deck challenger)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-placed state :challenger)))]
                                (resolve-ability state side
                                  {:prompt "Choose how much damage to prevent"
                                   :priority 50
                                   :choices {:number (req (min n (count (:deck challenger))))}
                                   :msg (msg "discard " (join ", " (map :title (take target (:deck challenger))))
                                             " from their Stack and prevent " target " damage")
                                   :effect (effect (damage-prevent :net target)
                                                   (damage-prevent :brain target)
                                                   (mill :challenger target)
                                                   (discard card {:cause :ability-cost}))} card nil)))}]}

   "Recon Drone"
   ; eventmap uses reverse so we get the most recent event of each kind into map
   (letfn [(eventmap [s]
             (into {} (reverse (get s :turn-events))))]
     {:interactions {:prevent [{:type #{:net :brain :meat}
                                :req (req (:access @state))}]}
      :abilities [{:req (req (= (:cid (second (:pre-damage (eventmap @state))))
                                (:cid (first (:pre-access-card (eventmap @state))))))
                   :effect (effect (resolve-ability
                                     {:prompt "Choose how much damage to prevent"
                                      :priority 50
                                      :choices {:number (req (min (last (:pre-damage (eventmap @state)))
                                                                  (:credit challenger)))}
                                      :msg (msg "prevent " target " damage")
                                      :effect (effect (discard card {:cause :ability-cost})
                                                      (damage-prevent (first (:pre-damage (eventmap @state))) target)
                                                      (lose-credits target))}
                                     card nil))}]})

   "Record Reconstructor"
   {:events
    {:successful-run
     {:req (req (= (get-in @state [:run :locale]) [:archives]))
      :effect (req (let [rr card]
                     (swap! state assoc-in [:run :run-effect :replace-access]
                       {:effect (effect (resolve-ability
                                          {:prompt "Choose one faceup card to add to the top of R&D"
                                           :choices (req (filter #(:seen %) (:discard contestant)))
                                           :msg (msg "add " (:title target) " to the top of R&D")
                                           :effect (req (move state :contestant target :deck {:front true}))}
                                         rr nil))})))}}}

   "Reflection"
   {:in-play [:memory 1 :link 1]
    :events {:jack-out {:msg (msg "force the Contestant to reveal "
                                  (:title (first (shuffle (:hand contestant))))
                                  " from HQ")}}}

   "Replicator"
   (letfn [(hazard-and-in-deck? [target challenger]
             (and (is-type? target "Hazard")
                  (some #(= (:title %) (:title target)) (:deck challenger))))]
     {:events {:challenger-place
               {:interactive (req (hazard-and-in-deck? target challenger))
                :silent (req (not (hazard-and-in-deck? target challenger)))
                :optional {:prompt "Use Replicator to add a copy?"
                           :req (req (hazard-and-in-deck? target challenger))
                           :yes-ability {:msg (msg "add a copy of " (:title target) " to their Grip")
                                         :effect (effect (trigger-event :searched-stack nil)
                                                   (shuffle! :deck)
                                                   (move (some #(when (= (:title %) (:title target)) %)
                                                               (:deck challenger)) :hand))}}}}})


   "Respirocytes"
   (let [ability {:once :per-turn
                  :msg "draw 1 card and add a power counter to itself"
                  :effect (req (draw state :challenger)
                               (add-counter state side (get-card state card) :power 1)
                               (when (= (get-counters (get-card state card) :power) 3)
                                 (system-msg state :challenger "discards Respirocytes as it reached 3 power counters")
                                 (discard state side card {:unpreventable true})))}]
   {:effect (req (let [watch-id (keyword "respirocytes" (str (:cid card)))]
                   (update! state side (assoc card :respirocytes-watch-id watch-id))
                   (add-watch state watch-id
                            (fn [k ref old new]
                              (when (and (seq (get-in old [:challenger :hand]))
                                         (empty? (get-in new [:challenger :hand])))
                                (resolve-ability ref side ability card nil)))))
                 (damage state side eid :meat 1 {:unboostable true :card card}))
    :msg "suffer 1 meat damage"
    :discard-effect {:effect (req (remove-watch state (:respirocytes-watch-id card)))}
    :leave-play (req (remove-watch state (:respirocytes-watch-id card)))
    :events {:challenger-turn-begins {:req (req (empty? (get-in @state [:challenger :hand])))
                                  :effect (effect (resolve-ability ability card nil))}
             :contestant-turn-begins {:req (req (empty? (get-in @state [:challenger :hand])))
                                :effect (effect (resolve-ability ability card nil))}}})

   "Rubicon Switch"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :async true
                 :prompt "How many [Credits]?" :choices :credit
                 :effect (effect (system-msg (str "spends a [Click] and " target " [Credit] on Rubicon Switch"))
                                 (resolve-ability {:choices {:req #(and (character? %)
                                                                        (= :this-turn (:revealed %))
                                                                        (<= (:cost %) target))}
                                                   :effect (effect (hide target))
                                                   :msg (msg "hide " (:title target))} card nil))}]}

   "Security Chip"
   {:abilities [{:label "[Discard]: Add [Link] strength to a non-Cloud characterbreaker until the end of the run"
                 :msg (msg "add " (:link challenger) " strength to " (:title target) " until the end of the run")
                 :req (req (:run @state))
                 :prompt "Select one non-Cloud characterbreaker"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "Cloud"))
                                      (placed? %))}
                 :effect (effect (pump target (:link challenger) :all-run)
                                 (discard (get-card state card) {:cause :ability-cost}))}
                {:label "[Discard]: Add [Link] strength to any Cloud characterbreakers until the end of the run"
                 :msg (msg "add " (:link challenger) " strength to " (count targets) " Cloud characterbreakers until the end of the run")
                 :req (req (:run @state))
                 :prompt "Select any number of Cloud characterbreakers"
                 :choices {:max 50
                           :req #(and (has-subtype? % "Icebreaker")
                                      (has-subtype? % "Cloud")
                                      (placed? %))}
                 :effect (req (doseq [t targets]
                                (pump state side t (:link challenger) :all-run)
                                (update-breaker-strength state side t))
                              (discard state side (get-card state card) {:cause :ability-cost}))}]}

   "Security Nexus"
   {:implementation "Bypass is manual"
    :in-play [:memory 1 :link 1]
    :abilities [{:req (req (:run @state))
                 :once :per-turn
                 :async true
                 :msg "force the Contestant to initiate a trace"
                 :label "Trace 5 - Give the Challenger 1 tag and end the run"
                 :trace {:base 5
                         :successful {:msg "give the Challenger 1 tag and end the run"
                                      :effect (effect (tag-challenger :challenger eid 1)
                                                      (end-run))}
                         :unsuccessful {:msg "bypass the current Character"}}}]}

   "Severnius Stim Implant"
   {:abilities [{:cost [:click 1]
                 :prompt "Choose a locale to run with Severnius Stim Implant" :choices ["HQ" "R&D"]
                 :effect (req (let [n (count (:hand challenger))
                                    srv target]
                                (resolve-ability state side
                                  {:prompt "Choose at least 2 cards in your Grip to discard with Severnius Stim Implant"
                                   :choices {:max n :req #(and (= (:side %) "Challenger") (in-hand? %))}
                                   :msg (msg "discard " (count targets) " card" (if (not= 1 (count targets)) "s")
                                             " and access " (quot (count targets) 2) " additional cards")
                                   :effect (req (let [bonus (quot (count targets) 2)]
                                                   (discard-cards state side (make-eid state) targets
                                                                {:unpreventable true :suppress-event true})
                                                   (game.core/run state side srv nil card)
                                                   (register-events state side
                                                     {:pre-access
                                                      {:silent (req true)
                                                       :effect (effect (access-bonus bonus))}
                                                      :run-ends {:effect (effect (unregister-events card))}} card)))}
                                 card nil)))}]
    :events {:pre-access nil :run-ends nil}}

   "Şifr"
   {:in-play [:memory 2]
    :abilities [{:once :per-turn
                 :req (req (revealed? current-character))
                 :msg (msg "lower their maximum hand size by 1 and lower the strength of " (:title current-character) " to 0")
                 :effect (effect (lose :challenger :hand-size 1)
                                 (update! (assoc card :sifr-target current-character :sifr-used true))
                                 (update-character-strength current-character))}]
    :events {:challenger-turn-begins {:req (req (:sifr-used card))
                                  :effect (effect (gain :challenger :hand-size 1)
                                                  (update! (dissoc card :sifr-used)))}
             :pre-character-strength {:req (req (= (:cid target) (get-in card [:sifr-target :cid])))
                                :effect (req (let [character-str (:current-strength target)]
                                               (character-strength-bonus state side (- character-str) target)))}
             :run-ends {:effect (effect (update! (dissoc card :sifr-target)))}}}

   "Silencer"
   {:recurring 1}

   "Skulljack"
   {:effect (effect (damage eid :brain 1 {:card card}))
    :events {:pre-discard {:effect (effect (discard-cost-bonus -1))}}}

   "Spinal Modem"
   {:in-play [:memory 1]
    :recurring 2
    :events {:successful-trace {:req (req run)
                                :effect (effect (system-msg (str "suffers 1 brain damage from Spinal Modem"))
                                                (damage eid :brain 1 {:card card}))}}}

   "Sports Hopper"
   {:in-play [:link 1]
    :abilities [{:label "Draw 3 cards"
                 :msg "draw 3 cards"
                 :effect (effect (discard card {:cause :ability-cost}) (draw 3))}]}

   "Spy Camera"
   {:abilities [{:cost [:click 1]
                 :async true
                 :label "Look at the top X cards of your Stack"
                 :msg "look at the top X cards of their Stack and rearrange them"
                 :effect (req (show-wait-prompt state :contestant "Challenger to rearrange the top cards of their stack")
                              (let [n (count (filter #(= (:title %) (:title card))
                                                     (all-active-placed state :challenger)))
                                    from (take n (:deck challenger))]
                                (if (pos? (count from))
                                  (continue-ability state side (reorder-choice :challenger :contestant from '()
                                                                               (count from) from) card nil)
                                  (do (clear-wait-prompt state :contestant)
                                      (effect-completed state side eid)))))}
                {:label "[Discard]: Look at the top card of R&D"
                 :msg "discard it and look at the top card of R&D"
                 :effect (effect (prompt! card (str "The top card of R&D is " (:title (first (:deck contestant)))) ["OK"] {})
                                 (discard card {:cause :ability-cost}))}]}

   "The Gauntlet"
   {:implementation "Requires Challenger to manually (and honestly) set how many Character were broken directly protecting HQ"
    :in-play [:memory 2]
    :events {:post-successful-run {:req (req (and (= :hq target)
                                                  run))
                                   :silent (req true)
                                   :async true
                                   :effect (effect (continue-ability
                                                     {:prompt "How many Character protecting HQ did you break all subroutines on?"
                                                      ;; Makes number of character on locale (HQ) the upper limit.
                                                      ;; This should work since discarded character do not count according to UFAQ
                                                      :choices {:number (req (count (get-in @state [:contestant :locales :hq :characters])))}
                                                      :effect (effect (access-bonus target))}
                                                     card nil))}}}

   "The Personal Touch"
   {:hosting {:req #(and (has-subtype? % "Icebreaker")
                         (placed? %))}}

   "The Toolbox"
   {:in-play [:link 2 :memory 2]
    :recurring 2}

   "Titanium Ribs"
   {:events
    {:pre-resolve-damage
     {:async true
      :req (req (and (pos? (last targets))
                     (challenger-can-choose-damage? state)
                     (not (get-in @state [:damage :damage-replace]))))
      :effect (req (let [dtype target
                         src (second targets)
                         dmg (last targets)]
                     (when (> dmg (count (:hand challenger)))
                       (flatline state))
                     (when (= dtype :brain)
                       (swap! state update-in [:challenger :brain-damage] #(+ % dmg))
                       (swap! state update-in [:challenger :hand-size :mod] #(- % dmg)))
                     (show-wait-prompt state :contestant "Challenger to use Titanium Ribs to choose cards to be discarded")
                     (wait-for (resolve-ability
                                 state side
                                 {:async true
                                  :prompt (msg "Select " dmg " cards to discard for the " (name dtype) " damage")
                                  :player :challenger
                                  :choices {:max dmg :all true :req #(and (in-hand? %) (= (:side %) "Challenger"))}
                                  :msg (msg "discard " (join ", " (map :title targets)))
                                  :effect (req (clear-wait-prompt state :contestant)
                                               (doseq [c targets]
                                                 (discard state side c {:cause dtype :unpreventable true}))
                                               (trigger-event state side :damage-chosen)
                                               (damage-defer state side dtype 0)
                                               (effect-completed state side eid))}
                                 card nil)
                               (trigger-event-sync state side eid :damage dtype src dmg))))}
    :damage-chosen {:effect (effect (enable-challenger-damage-choice))}}
    :async true
    :effect (effect (enable-challenger-damage-choice)
                    (system-msg (str "suffers 2 meat damage from placing Titanium Ribs"))
                    (damage eid :meat 2 {:unboostable true :card card}))
    :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-challenger))}

   "Top Hat"
   (letfn [(ability [n]
             {:async true
              :mandatory true
              :prompt "Which card from the top of R&D would you like to access? (Card 1 is on top.)"
              :choices (take n ["1" "2" "3" "4" "5"])
              :effect (effect (system-msg (str "accesses the card at position " (str->int target) " of R&D"))
                              (access-card eid (nth (:deck contestant) (dec (str->int target))) "an unseen card"))})]
     {:events {:successful-run
               {:req (req (= target :rd))
                :interactive (req true)
                :optional {:prompt "Use Top Hat to choose one of the top 5 cards in R&D to access?"
                           :yes-ability {:effect (req (swap! state assoc-in [:run :run-effect :replace-access]
                                                             (ability (count (:deck contestant)))))}}}}})

   "Turntable"
   {:in-play [:memory 1]
    :events {:agenda-stolen
             {:interactive (req true)
              :req (req (not (empty? (:scored contestant))))
              :async true
              :effect (req
                        (let [stolen target]
                          (continue-ability
                            state side
                            {:optional
                             {:prompt (msg "Swap " (:title stolen) " for an agenda in the Contestant's score area?")
                              :yes-ability
                              {:async true
                               :effect (req
                                         (continue-ability
                                           state side
                                           {:prompt (str "Select a scored Contestant agenda to swap with " (:title stolen))
                                            :choices {:req #(in-contestant-scored? state side %)}
                                            :effect (req (let [scored target]
                                                           (swap-agendas state side scored stolen)
                                                           (system-msg state side (str "uses Turntable to swap "
                                                                                       (:title stolen) " for " (:title scored)))
                                                           (effect-completed state side eid)))}
                                           card targets))}}}
                            card targets)))}}}

   "Ubax"
   (let [ability {:req (req (:challenger-phase-12 @state))
                  :msg "draw 1 card"
                  :label "Draw 1 card (start of turn)"
                  :once :per-turn
                  :effect (effect (draw 1))}]
     {:in-play [:memory 1]
      :flags {:challenger-turn-draw true
              :challenger-phase-12 (req (< 1 (count (filter #(card-flag? % :challenger-turn-draw true)
                                                        (cons (get-in @state [:challenger :identity])
                                                              (all-active-placed state :challenger))))))}
      :events {:challenger-turn-begins ability}
      :abilities [ability]})

   "Unregistered S&W 35"
   {:abilities
    [{:cost [:click 2]
      :req (req (some #{:hq} (:successful-run challenger-reg)))
      :label "discard a Bioroid, Clone, Executive or Sysop"
      :prompt "Select a Bioroid, Clone, Executive, or Sysop to discard"
      :choices {:req #(and (revealed? %)
                           (or (has-subtype? % "Bioroid")
                               (has-subtype? % "Clone")
                               (has-subtype? % "Executive")
                               (has-subtype? % "Sysop"))
                           (or (and (= (last (:zone %)) :content) (is-party? (second (:zone %))))
                               (= (last (:zone %)) :onhost)))}
      :msg (msg "discard " (:title target)) :effect (effect (discard target))}]}

   "Vigil"
   (let [ability {:req (req (and (:challenger-phase-12 @state) (= (count (:hand contestant)) (hand-size state :contestant))))
                  :msg "draw 1 card"
                  :label "Draw 1 card (start of turn)"
                  :once :per-turn
                  :effect (effect (draw 1))}]
   {:in-play [:memory 1]
    :events {:challenger-turn-begins ability}
    :abilities [ability]})

   "Window"
   {:abilities [{:cost [:click 1] :msg "draw 1 card from the bottom of their Stack"
                 :effect (effect (move (last (:deck challenger)) :hand))}]}

   "Zamba"
   {:implementation "Credit gain is automatic"
    :in-play [:memory 2]
    :events {:expose {:effect (effect (gain-credits :challenger 1))
                      :msg "gain 1 [Credits]"}}}

   "Zer0"
   {:abilities [{:cost [:click 1 :net-damage 1]
                 :once :per-turn
                 :msg "gain 1 [Credits] and draw 2 cards"
                 :effect (effect (gain-credits 1)
                                 (draw 2))}]}})
