(in-ns 'game.core)

(def cards-hardware
  {"Acacia"
   {:events {:pre-purge {:effect (req (let [virus (filter #(has-subtype? % "Virus") (all-installed state :hero))
                                            counters (reduce + (map #(get-virus-counters state :hero %) virus))]
                                        (update! state side (assoc-in (get-card state card) [:special :numpurged] counters))))}
             :purge {:delayed-completion true
                     :effect (effect (show-wait-prompt  :minion "Runner to decide if they will use Acacia")
                                  (continue-ability {:optional
                                                     {:player :hero
                                                      :prompt "Use Acacia?"
                                                      :yes-ability {:effect (req (let [counters (get-in (get-card state card) [:special :numpurged])]
                                                                                   (gain state side :credit counters)
                                                                                   (system-msg state side (str "trashes Acacia and gains " counters "[Credit]"))
                                                                                   (trash state side card {:unpreventable true})
                                                                                   (clear-wait-prompt state :minion)
                                                                                   (effect-completed state side eid)))}
                                                      :no-ability {:effect (effect (clear-wait-prompt :minion)
                                                                                   (effect-completed eid))}}} card nil))}}}

   "Adjusted Matrix"
   {:implementation "Click Adjusted Matrix to use ability."
    :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-installed state :hero))))
    :prompt "Choose Icebreaker on which to install Adjusted Matrix"
    :choices {:req #(and (= (:side %) "Hero") (has-subtype? % "Icebreaker") (installed? %))}
    :msg (msg "host it on " (card-str state target))
    :effect (effect (update! (assoc target :subtype (combine-subtypes false (-> target :subtype) "AI")))
                    (host (get-card state target) (get-card state card)))
    :abilities [{:cost [:click 1]
                 :req (req run)
                 :msg "break ice subroutine"}]
    :events {:pre-card-moved {:req (req (= (:cid target) (:cid card)))
                              :effect (effect (update! (assoc (-> card :host) :subtype (-> card :host :subtype (remove-subtypes-once ["AI"])))))}}}

   "Akamatsu Mem Chip"
   {:in-play [:memory 1]}

   "Archives Interface"
   {:events
    {:pre-access
     {:delayed-completion true
      :interactive (req true)
      :req (req (and (= target :archives)
                     (not= (:max-access run) 0)
                     (not-empty (:discard minion))))
      :effect (req (swap! state update-in [:minion :discard] #(map (fn [c] (assoc c :seen true)) %))
                   (continue-ability state side
                     {:optional
                      {:delayed-completion true
                       :prompt "Use Archives Interface to remove a card from the game instead of accessing it?"
                       :yes-ability
                       {:delayed-completion true
                        :effect (effect (continue-ability
                                          {:prompt "Choose a card in Archives to remove from the game instead of accessing"
                                           :choices (req (:discard minion))
                                           :msg (msg "remove " (:title target) " from the game")
                                           :effect (effect (move :minion target :rfg))} card nil))}
                       :no-ability {:effect (req (effect-completed state side eid))}}} card nil))}}}

   "Astrolabe"
   {:in-play [:memory 1]
    :events {:server-created {:msg "draw 1 card"
                              :effect (effect (draw :hero))}}}

   "Autoscripter"
   {:events {:hero-install {:silent (req true)
                              :req (req (and (is-type? target "Program")
                                             (= (:active-player @state) :hero)
                                             ;; only trigger when played a programm from grip
                                             (some #{:hand} (:previous-zone target))
                                             ;; check if didn't played a program from the grip this turn
                                             (empty? (let [cards (map first (turn-events state side :hero-install))
                                                           progs (filter #(is-type? % "Program") cards)]
                                                          (filter #(some #{:hand} (:previous-zone %)) progs)))))
                              :msg "gain [Click]" :effect (effect (gain :click 1))}
             :unsuccessful-run {:effect (effect (trash card)
                                                (system-msg "trashes Autoscripter"))}}}

   "Blackguard"
   {:in-play [:memory 2]
    :events {:expose
             {:msg (msg "attempt to force the rez of " (:title target))
              :delayed-completion true
              :effect (req (let [c target
                                 cdef (card-def c)
                                 cname (:title c)]
                             (if (:additional-cost cdef)
                               (do (show-wait-prompt state :hero (str "Corp to decide if they will rez " cname))
                                   (continue-ability state side
                                     {:optional
                                      {:prompt (msg "Pay additional cost to rez " cname "?")
                                       :player :minion
                                       :yes-ability {:effect (effect (rez :minion c)
                                                                     (clear-wait-prompt :hero))}
                                       :no-ability {:effect (effect (system-msg :minion (str "declines to pay additional costs"
                                                                                       " and is not forced to rez " cname))
                                                                    (clear-wait-prompt :hero))}}}
                                    card nil))
                               (do (rez state :minion target)
                                   (effect-completed state side eid)))))}}}

   "Bookmark"
   {:abilities [{:label "Host up to 3 cards from your Grip facedown"
                 :cost [:click 1] :msg "host up to 3 cards from their Grip facedown"
                 :choices {:max 3
                           :req #(and (= (:side %) "Hero")
                                      (in-hand? %))}
                 :effect (req (doseq [c targets]
                                 (host state side (get-card state card) c {:facedown true})))}
                {:label "Add all hosted cards to Grip" :cost [:click 1] :msg "add all hosted cards to their Grip"
                 :effect (req (doseq [c (:hosted card)]
                                (move state side c :hand)))}
                {:label "[Trash]: Add all hosted cards to Grip" :msg "add all hosted cards to their Grip"
                 :effect (req (doseq [c (:hosted card)]
                                (move state side c :hand))
                              (update! state side (dissoc card :hosted))
                              (trash state side (get-card state card) {:cause :ability-cost}))}]}

   "Box-E"
   {:in-play [:memory 2 :hand-size-modification 2]}

   "Brain Cage"
   {:in-play [:hand-size-modification 3]
    :effect (effect (damage eid :brain 1 {:card card}))}

   "Brain Chip"
   (let [hero-points (fn [s] (max (or (get-in s [:hero :agenda-point]) 0) 0))]
     {:effect (req (gain state :hero
                         :memory (hero-points @state)
                         :hand-size-modification (hero-points @state))
                   (add-watch state (keyword (str "brainchip" (:cid card)))
                          (fn [k ref old new]
                            (let [bonus (- (hero-points new) (hero-points old))]
                              (when (not= 0 bonus)
                               (gain state :hero
                                     :memory bonus
                                     :hand-size-modification bonus))))))
      :leave-play (req (remove-watch state (keyword (str "brainchip" (:cid card))))
                       (lose state :hero
                             :memory (hero-points @state)
                             :hand-size-modification (hero-points @state)))})

   "Capstone"
   {:abilities [{:req (req (> (count (:hand hero)) 0))
                 :cost [:click 1]
                 :effect (req (let [handsize (count (:hand hero))]
                                (resolve-ability state side
                                  {:prompt "Select any number of cards to trash from your Grip"
                                   :choices {:max handsize
                                             :req #(and (= (:side %) "Hero")
                                                        (in-hand? %))}
                                   :effect (req (let [trashed (count targets)
                                                      remaining (- handsize trashed)]
                                                  (doseq [c targets]
                                                    (when (not (empty? (filter #(= (:title c) (:title %))
                                                                               (all-installed state :hero))))
                                                      (draw state side)))
                                                  (trash-cards state side targets)
                                                  (system-msg state side
                                                    (str "spends [Click] to use Capstone to trash "
                                                      (join ", " (map :title targets)) " and draw "
                                                      (- (count (get-in @state [:hero :hand])) remaining) " cards"))))}
                                 card nil)))}]}

   "Chop Bot 3000"
   {:flags {:hero-phase-12 (req (>= 2 (count (all-installed state :hero))))}
    :abilities [{:msg (msg "trash " (:title target))
                 :choices {:req #(and (= (:side %) "Hero") (:installed %))}
                 :effect (effect (trash target)
                                 (resolve-ability
                                   {:prompt "Draw 1 card or remove 1 tag" :msg (msg (.toLowerCase target))
                                    :choices ["Draw 1 card" "Remove 1 tag"]
                                    :effect (req (if (= target "Draw 1 card")
                                                   (draw state side)
                                                   (lose state side :tag 1)))} card nil))}]}

   "Clone Chip"
   {:abilities [{:prompt "Select a program to install from your Heap"
                 :priority true :show-discard true
                 :req (req (and (not (seq (get-in @state [:hero :locked :discard])))
                               (not (install-locked? state side))))
                 :choices {:req #(and (is-type? % "Program")
                                      (= (:zone %) [:discard]))}
                 :effect (req (when (>= (:credit hero) (:cost target))
                                    (do (hero-install state side target)
                                        (trash state side card {:cause :ability-cost})
                                        (system-msg state side (str "uses " (:title card) " to install " (:title target))))))}]}

   "Comet"
   {:in-play [:memory 1]
    :events {:play-event {:req (req (first-event? state side :play-event))
                          :effect (req (system-msg state :hero
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
   {:abilities [{:prompt "Select a piece of ICE"
                 :choices {:req ice?}
                 :effect (req (let [ice target]
                                (update! state side (assoc card :cortez-target ice))
                                (trash state side (get-card state card) {:cause :ability-cost})
                                (system-msg state side
                                  (str "trashes Cortez Chip to increase the rez cost of " (card-str state ice)
                                       " by 2 [Credits] until the end of the turn"))))}]
    :trash-effect {:effect (effect (register-events {:pre-rez {:req (req (= (:cid target) (:cid (:cortez-target card))))
                                                               :effect (effect (rez-cost-bonus 2))}
                                                     :hero-turn-ends {:effect (effect (unregister-events card))}
                                                     :minion-turn-ends {:effect (effect (unregister-events card))}}
                                                    (get-card state card)))}
    :events {:pre-rez nil :hero-turn-ends nil :minion-turn-ends nil}}

   "Cyberdelia"
   {:implementation "Credit gain is manually triggered."
    :in-play [:memory 1]
    :abilities [{:msg "gain 1 [Credits] for breaking all subroutines on a piece of ice"
                 :once :per-turn
                 :effect (effect (gain :credit 1))}]}

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
    :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-installed state :hero))))
    :hosting {:req #(and (has-subtype? % "Icebreaker")
                         (not (has-subtype? % "AI"))
                         (installed? %))}
    :abilities [{:cost [:credit 2]
                 :req (req run)
                 :effect (effect (pump (get-card state (:host card)) 4))
                 :msg (msg (str "pump the strength of " (get-in card [:host :title]) " by 4"))}]}

   "Deep Red"
   {:implementation "MU use restriction not enforced"
    :in-play [:memory 3]
    :events {:hero-install
             {:optional
              {:delayed-completion true
               :req (req (has-subtype? target "Caïssa"))
               :prompt "Use Deep Red?" :priority 1
               :yes-ability {:delayed-completion true
                             :effect (req (let [cid (:cid target)]
                                            (continue-ability state side
                                              {:delayed-completion true
                                               :prompt "Choose the just-installed Caïssa to have Deep Red trigger its [Click] ability"
                                               :choices {:req #(= cid (:cid %))}
                                               :msg (msg "trigger the [Click] ability of " (:title target)
                                                         " without spending [Click]")
                                               :effect (req (gain state :hero :click 1)
                                                            (play-ability state side {:card target :ability 0})
                                                            (effect-completed state side eid))}
                                             card nil)))}
               :no-ability {:effect (req (effect-completed state side eid))}}}}}

   "Desperado"
   {:in-play [:memory 1]
    :events {:successful-run {:silent (req true)
                              :msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "Dinosaurus"
   {:abilities [{:label "Install a non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card))) :cost [:click 1]
                 :prompt "Select a non-AI icebreaker in your Grip to install on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (in-hand? %))}
                 :effect (effect (gain :memory (:memoryunits target))
                                 (hero-install target {:host-card card})
                                 (update! (assoc (get-card state card) :dino-breaker (:cid target))))}
                {:label "Host an installed non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card)))
                 :prompt "Select an installed non-AI icebreaker to host on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (req (update-breaker-strength state side (host state side card target))
                              (update! state side (assoc (get-card state card) :dino-breaker (:cid target)))
                              (gain state side :memory (:memoryunits target)))}]
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (first (:hosted card)))))
                                    :effect (effect (breaker-strength-bonus 2))}
             :card-moved {:req (req (= (:cid target) (:dino-breaker (get-card state card))))
                          :effect (effect (update! (dissoc card :dino-breaker))
                                          (lose :memory (:memoryunits target)))}}}

   "Doppelgänger"
   {:in-play [:memory 1]
    :events {:hero-install
             {:req (req (= card target))
              :silent (req true)
              :effect (effect (update! (assoc card :dopp-active true)))}
             :hero-turn-begins
             {:effect (effect (update! (assoc card :dopp-active true)))}
             :successful-run-ends
             {:interactive (req true)
              :optional
              {:req (req (:dopp-active card))
               :player :hero
               :prompt "Use Doppelgänger to run again?"
               :yes-ability {:prompt "Choose a server"
                             :delayed-completion true
                             :choices (req runnable-servers)
                             :msg (msg "make a run on " target)
                             :makes-run true
                             :effect (effect (update! (dissoc card :dopp-active))
                                             (clear-wait-prompt :minion)
                                             (run eid target))}}}}}

   "Dorm Computer"
   {:data {:counter {:power 4}}
    :abilities [{:counter-cost [:power 1]
                 :cost [:click 1]
                 :req (req (not run))
                 :prompt "Choose a server"
                 :choices (req runnable-servers)
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
   {:effect (req (swap! state assoc-in [:hero :memory] (count (get-in @state [:hero :hand])))
                 (add-watch state :ekomind (fn [k ref old new]
                                             (let [hand-size (count (get-in new [:hero :hand]))]
                                               (when (not= (count (get-in old [:hero :hand])) hand-size)
                                                 (swap! ref assoc-in [:hero :memory] hand-size))))))
    :leave-play (req (remove-watch state :ekomind))}

   "EMP Device"
   {:abilities [{:req (req (:run @state))
                 :msg "prevent the Corp from rezzing more than 1 piece of ICE for the remainder of the run"
                 :effect (effect (register-events
                                   {:rez {:req (req (ice? target))
                                          :effect (effect (register-run-flag!
                                                            card :can-rez
                                                            (fn [state side card]
                                                              (if (ice? card)
                                                                ((constantly false)
                                                                 (toast state :minion "Cannot rez ICE the rest of this run due to EMP Device"))
                                                                true))))}
                                    :run-ends {:effect (effect (unregister-events card))}} (assoc card :zone '(:discard)))
                                 (trash card {:cause :ability-cost}))}]
    :events {:rez nil
             :run-ends nil}}

   "Feedback Filter"
   {:prevent {:damage [:net :brain]}
    :abilities [{:cost [:credit 3] :msg "prevent 1 net damage" :effect (effect (damage-prevent :net 1))}
                {:label "[Trash]: Prevent up to 2 brain damage"
                 :msg "prevent up to 2 brain damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage-prevent :brain 2))}]}

   "Forger"
   {:prevent {:tag [:all]}
    :in-play [:link 1]
    :abilities [{:msg "avoid 1 tag" :label "[Trash]: Avoid 1 tag"
                 :effect (effect (tag-prevent 1) (trash card {:cause :ability-cost}))}
                {:msg "remove 1 tag" :label "[Trash]: Remove 1 tag"
                 :effect (effect (trash card {:cause :ability-cost}) (lose :tag 1))}]}

   "GPI Net Tap"
   {:implementation "Trash and jack out effect is manual"
    :abilities [{:req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                 :delayed-completion true
                 :effect (effect (expose eid current-ice))}]}

   "Grimoire"
   {:in-play [:memory 2]
    :events {:hero-install {:silent (req true)
                              :req (req (has-subtype? target "Virus"))
                              :effect (effect (add-counter target :virus 1))}}}

   "Heartbeat"
   {:in-play [:memory 1]
    :prevent {:damage [:meat :net :brain]}
    :abilities [{:msg (msg "prevent 1 damage, trashing a facedown " (:title target))
                 :choices {:req #(and (= (:side %) "Hero") (:installed %))}
                 :priority 50
                 :effect (effect (trash target {:unpreventable true})
                                 (damage-prevent :brain 1)
                                 (damage-prevent :meat 1)
                                 (damage-prevent :net 1))}]}

   "HQ Interface"
   {:in-play [:hq-access 1]}

   "Lemuria Codecracker"
   {:abilities [{:cost [:click 1 :credit 1] :req (req (some #{:hq} (:successful-run hero-reg)))
                 :choices {:req installed?} :effect (effect (expose eid target))
                 :msg "expose 1 card"}]}

   "LLDS Memory Diamond"
   {:in-play [:link 1 :memory 1 :hand-size-modification 1]}

   "LLDS Processor"
   {:events
     (let [llds {:effect (req (let [cards (:llds-target card)]
                                (update! state side (dissoc card :llds-target))
                                (doseq [c cards]
                                (update-breaker-strength state side
                                                         (find-cid (:cid c) (all-installed state :hero))))))}]
       {:hero-turn-ends llds :minion-turn-ends llds
        :hero-install {:silent (req true)
                         :req (req (has-subtype? target "Icebreaker"))
                         :effect (effect (update! (update-in card [:llds-target] #(conj % target)))
                                         (update-breaker-strength target))}
        :pre-breaker-strength {:req (req (some #(= (:cid target) (:cid %)) (:llds-target card)))
                               :effect (effect (breaker-strength-bonus 1))}})}

   "Lockpick"
   {:recurring 1}

   "Logos"
   {:in-play [:memory 1 :hand-size-modification 1]
    :events {:agenda-scored
             {:player :hero :prompt "Choose a card" :msg (msg "add 1 card to their Grip from their Stack")
              :choices (req (cancellable (:deck hero)))
              :effect (effect (trigger-event :searched-stack nil)
                              (shuffle! :deck)
                              (move target :hand))}}}

   "Māui"
   {:in-play [:memory 2]
    :recurring (effect (set-prop card :rec-counter (count (:ices (get-in @state [:minion :servers :hq])))))
    :effect (effect (set-prop card :rec-counter (count (:ices (get-in @state [:minion :servers :hq])))))}

   "Maw"
   (let [ability {:label "Trash a card from HQ"
                  :req (req (and (first-event? state side :no-trash)
                                 (first-event? state side :no-steal)
                                 (pos? (count (:hand minion)))
                                 (not= (first (:zone target)) :discard)))
                  :once :per-turn
                  :msg "force the Corp to trash a random card from HQ"
                  :effect (req (let [card-to-trash (first (shuffle (:hand minion)))
                                     card-seen? (= (:cid target) (:cid card-to-trash))
                                     card-to-trash (if card-seen? (assoc card-to-trash :seen true)
                                                                  card-to-trash)]
                                 (trash state :minion card-to-trash)))}]
     {:in-play [:memory 2]
      :abilities [ability]
      :events {:no-trash ability
               :no-steal ability}})

   "Maya"
   {:in-play [:memory 2]
    :abilities [{:once :per-turn
                 :delayed-completion true
                 :label "Move this accessed card to bottom of R&D"
                 :req (req (when-let [c (:card (first (get-in @state [:hero :prompt])))]
                             (in-deck? c)))
                 :msg "move the card just accessed to the bottom of R&D"
                 :effect (req (let [c (:card (first (get-in @state [:hero :prompt])))]
                                (when (is-type? c "Agenda") ; trashing before the :access events actually fire; fire them manually
                                  (resolve-steal-events state side c))
                                (move state :minion c :deck)
                                (when-completed (tag-hero state :hero (make-eid state) 1)
                                                (close-access-prompt state side))))}
                {:once :per-turn
                 :label "Move a previously accessed card to bottom of R&D"
                 :effect (effect (resolve-ability
                                   {; only allow targeting cards that were accessed this turn -- not perfect, but good enough?
                                    :delayed-completion true
                                    :choices {:req #(some (fn [c] (= (:cid %) (:cid c)))
                                                          (map first (turn-events state side :access)))}
                                    :msg (msg "move " (:title target) " to the bottom of R&D")
                                    :effect (req (move state :minion target :deck)
                                                 (tag-hero state :hero eid 1)
                                                 (swap! state update-in [side :prompt] rest)
                                                 (when-let [run (:run @state)]
                                                   (when (and (:ended run) (empty? (get-in @state [:hero :prompt])))
                                                     (handle-end-run state :hero))))} card nil))}]}

   "MemStrips"
   {:implementation "MU usage restriction not enforced"
    :in-play [:memory 3]}

   "Mirror"
   {:in-play [:memory 2]
    :events {:successful-run
             {:delayed-completion true
              :req (req (= target :rd))
              :effect (effect (continue-ability
                                {:prompt "Select a card and replace 1 spent [Recurring Credits] on it"
                                 :choices {:req #(< (:rec-counter % 0) (:recurring (card-def %) 0))}
                                 :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                 :effect (effect (add-prop target :rec-counter 1))}
                               card nil))}}}

   "Monolith"
   (let [mhelper (fn mh [n] {:prompt "Select a program to install"
                             :choices {:req #(and (is-type? % "Program")
                                                  (in-hand? %))}
                             :effect (req (install-cost-bonus state side [:credit -4])
                                          (hero-install state side target nil)
                                            (when (< n 3)
                                              (resolve-ability state side (mh (inc n)) card nil)))})]
     {:prevent {:damage [:net :brain]}
      :in-play [:memory 3]
      :effect (effect (resolve-ability (mhelper 1) card nil))
      :abilities [{:msg (msg "prevent 1 brain or net damage by trashing " (:title target))
                   :priority 50
                   :choices {:req #(and (is-type? % "Program")
                                        (in-hand? %))}
                   :prompt "Choose a program to trash from your Grip"
                   :effect (effect (trash target)
                                   (damage-prevent :brain 1)
                                   (damage-prevent :net 1))}]})

   "Muresh Bodysuit"
   {:events {:pre-damage {:once :per-turn :once-key :muresh-bodysuit
                          :req (req (= target :meat))
                          :msg "prevent the first meat damage this turn"
                          :effect (effect (damage-prevent :meat 1))}}}

   "Net-Ready Eyes"
   {:effect (effect (damage eid :meat 2 {:unboostable true :card card})) :msg "suffer 2 meat damage"
    :events {:run {:choices {:req #(and (installed? %)
                                        (has-subtype? % "Icebreaker"))}
                   :msg (msg "give " (:title target) " +1 strength")
                   :effect (effect (pump target 1 :all-run))}}}

   "NetChip"
   {:abilities [{:label "Install a program on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-installed state :hero)))]
                                (resolve-ability state side
                                  {:cost [:click 1]
                                   :prompt "Select a program in your Grip to install on NetChip"
                                   :choices {:req #(and (is-type? % "Program")
                                                        (hero-can-install? state side % false)
                                                        (<= (:memoryunits %) n)
                                                        (in-hand? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (gain :memory (:memoryunits target))
                                                   (hero-install target {:host-card card})
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-programs
                                                                   (cons (:cid target) (:hosted-programs card)))))}
                                 card nil)))}
                {:label "Host an installed program on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-installed state :hero)))]
                                (resolve-ability state side
                                  {:prompt "Select an installed program to host on NetChip"
                                   :choices {:req #(and (is-type? % "Program")
                                                        (<= (:memoryunits %) n)
                                                        (installed? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (host card target)
                                                   (gain :memory (:memoryunits target))
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-programs
                                                                   (cons (:cid target) (:hosted-programs card)))))}
                                 card nil)))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs
                                                          (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (lose :memory (:memoryunits target)))}}}

   "Obelus"
   {:in-play [:memory 1]
    :effect (req (gain state :hero :hand-size-modification (:tag hero))
                 (add-watch state :obelus
                   (fn [k ref old new]
                     (let [tagnew (get-in new [:hero :tag])
                           tagold (get-in old [:hero :tag])]
                       (when (> tagnew tagold)
                         (gain state :hero :hand-size-modification (- tagnew tagold)))
                       (when (< tagnew tagold)
                         (lose state :hero :hand-size-modification (- tagold tagnew)))))))
    :leave-play (req (remove-watch state :obelus)
                     (lose state :hero :hand-size-modification (:tag hero)))
    :events {:successful-run-ends {:once :per-turn
                                   :req (req (let [successes (rest (turn-events state side :successful-run))]
                                               (and (#{[:rd] [:hq]} (:server target))
                                                    (empty? (filter #(#{'(:rd) '(:hq)} %) successes)))))
                                   :msg (msg "draw " (:cards-accessed target 0) " cards")
                                   :effect (effect (draw (:cards-accessed target 0)))}}}

   "Omni-drive"
   {:recurring 1
    :abilities [{:label "Install and host a program of 1[Memory Unit] or less on Omni-drive"
                 :req (req (empty? (:hosted card)))
                 :cost [:click 1]
                 :prompt "Select a program of 1[Memory Unit] or less to install on Omni-drive from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (<= (:memoryunits %) 1)
                                      (in-hand? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (hero-install target {:host-card card})
                                 (update! (assoc (get-card state card) :Omnidrive-prog (:cid target))))}
                {:label "Host an installed program of 1[Memory Unit] or less on Omni-drive"
                 :prompt "Select an installed program of 1[Memory Unit] or less to host on Omni-drive"
                 :choices {:req #(and (is-type? % "Program")
                                      (<= (:memoryunits %) 1)
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (gain :memory (:memoryunits target))
                                 (update! (assoc (get-card state card) :Omnidrive-prog (:cid target))))}]
   :events {:card-moved {:req (req (= (:cid target) (:Omnidrive-prog (get-card state card))))
                          :effect (effect (update! (dissoc card :Omnidrive-prog))
                                          (lose :memory (:memoryunits target)))}}}

   "Plascrete Carapace"
   {:data [:counter {:power 4}]
    :prevent {:damage [:meat]}
    :abilities [{:counter-cost [:power 1]
                 :msg "prevent 1 meat damage"
                 :effect (req (damage-prevent state side :meat 1)
                              (when (= (get-in card [:counter :power]) 0)
                                (trash state side card {:unpreventable true})))}]}

   "Polyhistor"
   (let [abi {:optional
              {:prompt "Draw 1 card to force the Corp to draw 1 card?"
               :yes-ability {:msg "draw 1 card and force the Corp to draw 1 card"
                             :effect (effect (draw :hero 1)
                                             (draw :minion 1))}
               :no-ability {:effect (req (system-msg state side (str "does not use Polyhistor"))
                                         (effect-completed state side eid))}}}]
     {:in-play [:link 1 :memory 1]
      :events {:pass-ice {:req (req (and (= (:server run) [:hq]) (= (:position run) 1) ; trigger when last ICE passed
                                         (pos? (count (:deck hero)))))
                          :delayed-completion true
                          :once :per-turn
                          :effect (req (continue-ability state :hero abi card nil))}
               :run {:req (req (and (= (:server run) [:hq]) (= (:position run) 0) ; trigger on unprotected HQ
                                    (pos? (count (:deck hero)))))
                     :delayed-completion true
                     :once :per-turn
                     :effect (req (continue-ability state :hero abi card nil))}}})

   "Prepaid VoicePAD"
   {:recurring 1}

   "Public Terminal"
   {:recurring 1}

   "Q-Coherence Chip"
   {:in-play [:memory 1]
    :events (let [e {:req (req (= (last (:zone target)) :program))
                     :effect (effect (trash card)
                                     (system-msg (str "trashes Q-Coherence Chip")))}]
              {:hero-trash e :minion-trash e})}

   "Qianju PT"
   {:flags {:hero-phase-12 (req true)}
    :abilities [{:label "Lose [Click], avoid 1 tag (start of turn)"
                 :once :per-turn
                 :req (req (:hero-phase-12 @state))
                 :effect (effect (update! (assoc card :qianju-active true)))
                 :msg "lose [Click] and avoid the first tag received until their next turn"}]
    :events {:minion-turn-ends {:effect (effect (update! (dissoc card :qianju-active)))}
             :hero-turn-begins {:req (req (:qianju-active card))
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
             {:optional {:req (req (some #(when (= (:title %) "Rabbit Hole") %) (:deck hero)))
                         :prompt "Install another Rabbit Hole?" :msg "install another Rabbit Hole"
                         :yes-ability {:effect (req (when-let [c (some #(when (= (:title %) "Rabbit Hole") %)
                                                                      (:deck hero))]
                                                     (trigger-event state side :searched-stack nil)
                                                     (shuffle! state :hero :deck)
                                                     (hero-install state side c)))}}} card nil))}

   "Ramujan-reliant 550 BMI"
   {:prevent {:damage [:net :brain]}
    :abilities [{:req (req (not-empty (:deck hero)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-installed state :hero)))]
                                (resolve-ability state side
                                  {:prompt "Choose how much damage to prevent"
                                   :priority 50
                                   :choices {:number (req (min n (count (:deck hero))))}
                                   :msg (msg "trash " (join ", " (map :title (take target (:deck hero))))
                                             " from their Stack and prevent " target " damage")
                                   :effect (effect (damage-prevent :net target)
                                                   (damage-prevent :brain target)
                                                   (mill :hero target)
                                                   (trash card {:cause :ability-cost}))} card nil)))}]}

   "Recon Drone"
   ; eventmap uses reverse so we get the most recent event of each kind into map
   (let [eventmap (fn [s] (into {} (reverse (get s :turn-events))))]
     {:abilities [{:req (req (and (true? (:access @state)) (= (:cid (second (:pre-damage (eventmap @state))))
                                                              (:cid (first (:post-access-card (eventmap @state)))))))
                :effect (effect (resolve-ability
                                  {:prompt "Choose how much damage to prevent"
                                   :priority 50
                                   :choices {:number (req (min (last (:pre-damage (eventmap @state)))
                                                               (:credit hero)))}
                                   :msg (msg "prevent " target " damage")
                                   :effect (effect (damage-prevent (first (:pre-damage (eventmap @state))) target)
                                                   (lose :credit target)
                                                   (trash card {:cause :ability-cost}))} card nil))}]
     :events    {:pre-access {:effect (req (doseq [dtype [:net :brain :meat]] (swap! state update-in [:prevent :damage dtype] #(conj % card))))}
                 :run-ends   {:effect (req (doseq [dtype [:net :brain :meat]] (swap! state update-in [:prevent :damage dtype] #(drop 1 %))))}}})

   "Record Reconstructor"
   {:events
    {:successful-run
     {:req (req (= (get-in @state [:run :server]) [:archives]))
      :effect (req (let [rr card]
                     (swap! state assoc-in [:run :run-effect :replace-access]
                       {:effect (effect (resolve-ability
                                          {:prompt "Choose one faceup card to add to the top of R&D"
                                           :choices (req (filter #(:seen %) (:discard minion)))
                                           :msg (msg "add " (:title target) " to the top of R&D")
                                           :effect (req (move state :minion target :deck {:front true}))}
                                         rr nil))})))}}}

   "Reflection"
   {:in-play [:memory 1 :link 1]
    :events {:jack-out {:msg (msg "force the Corp to reveal " (:title (first (shuffle (:hand minion)))) " from HQ")}}}

   "Replicator"
   {:events {:hero-install
             {:interactive (req (and (is-type? target "Hardware")
                                     (some #(= (:title %) (:title target)) (:deck hero))))
              :silent (req (not (and (is-type? target "Hardware")
                                     (some #(= (:title %) (:title target)) (:deck hero)))))
              :optional {:prompt "Use Replicator to add a copy?"
                         :req (req (and (is-type? target "Hardware") (some #(= (:title %) (:title target)) (:deck hero))))
                         :yes-ability {:msg (msg "add a copy of " (:title target) " to their Grip")
                                       :effect (effect (trigger-event :searched-stack nil)
                                                       (shuffle! :deck)
                                                       (move (some #(when (= (:title %) (:title target)) %)
                                                                   (:deck hero)) :hand))}}}}}

   "Respirocytes"
   (let [ability {:once :per-turn
                  :msg "draw 1 card and add a power counter to itself"
                  :effect (req (draw state :hero)
                               (add-counter state side (get-card state card) :power 1)
                               (when (= (get-in (get-card state card) [:counter :power]) 3)
                                 (system-msg state :hero "trashes Respirocytes as it reached 3 power counters")
                                 (trash state side card {:unpreventable true})))}]
   {:effect (req (let [watch-id (keyword "respirocytes" (str (:cid card)))]
                   (update! state side (assoc card :respirocytes-watch-id watch-id))
                   (add-watch state watch-id
                            (fn [k ref old new]
                              (when (and (seq (get-in old [:hero :hand]))
                                         (empty? (get-in new [:hero :hand])))
                                (resolve-ability ref side ability card nil)))))
                 (damage state side eid :meat 1 {:unboostable true :card card}))
    :msg "suffer 1 meat damage"
    :trash-effect {:effect (req (remove-watch state (:respirocytes-watch-id card)))}
    :leave-play (req (remove-watch state (:respirocytes-watch-id card)))
    :events {:hero-turn-begins {:req (req (empty? (get-in @state [:hero :hand])))
                                  :effect (effect (resolve-ability ability card nil))}
             :minion-turn-begins {:req (req (empty? (get-in @state [:hero :hand])))
                                :effect (effect (resolve-ability ability card nil))}}})

   "Rubicon Switch"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :delayed-completion true
                 :prompt "How many [Credits]?" :choices :credit
                 :effect (effect (system-msg (str "spends a [Click] and " target " [Credit] on Rubicon Switch"))
                                 (resolve-ability {:choices {:req #(and (ice? %)
                                                                        (= :this-turn (:rezzed %))
                                                                        (<= (:cost %) target))}
                                                   :effect (effect (derez target))
                                                   :msg (msg "derez " (:title target))} card nil))}]}

   "Security Chip"
   {:abilities [{:label "[Trash]: Add [Link] strength to a non-Cloud icebreaker until the end of the run"
                 :msg (msg "add " (:link hero) " strength to " (:title target) " until the end of the run")
                 :req (req (:run @state))
                 :prompt "Select one non-Cloud icebreaker"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "Cloud"))
                                      (installed? %))}
                 :effect (effect (pump target (:link hero) :all-run)
                                 (trash (get-card state card) {:cause :ability-cost}))}
                {:label "[Trash]: Add [Link] strength to any Cloud icebreakers until the end of the run"
                 :msg (msg "add " (:link hero) " strength to " (count targets) " Cloud icebreakers until the end of the run")
                 :req (req (:run @state))
                 :prompt "Select any number of Cloud icebreakers"
                 :choices {:max 50
                           :req #(and (has-subtype? % "Icebreaker")
                                      (has-subtype? % "Cloud")
                                      (installed? %))}
                 :effect (req (doseq [t targets]
                                (pump state side t (:link hero) :all-run)
                                (update-breaker-strength state side t))
                              (trash state side (get-card state card) {:cause :ability-cost}))}]}

   "Security Nexus"
   {:in-play [:memory 1 :link 1]
    :abilities [{:req (req (:run @state))
                 :once :per-turn
                 :delayed-completion true
                 :msg "force the Corp to initiate a trace"
                 :label "Trace 5 - Give the Runner 1 tag and end the run"
                 :trace {:base 5 :msg "give the Runner 1 tag and end the run"
                         :effect (effect (tag-hero :hero eid 1) (end-run))
                         :unsuccessful {:msg "bypass the current ICE"}}}]}

   "Severnius Stim Implant"
   {:abilities [{:cost [:click 1]
                 :prompt "Choose a server to run with Severnius Stim Implant" :choices ["HQ" "R&D"]
                 :effect (req (let [n (count (:hand hero))
                                    srv target]
                                (resolve-ability state side
                                  {:prompt "Choose at least 2 cards in your Grip to trash with Severnius Stim Implant"
                                   :choices {:max n :req #(and (= (:side %) "Hero") (in-hand? %))}
                                   :msg (msg "trash " (count targets) " card" (if (not= 1 (count targets)) "s")
                                             " and access " (quot (count targets) 2) " additional cards")
                                   :effect (req (let [bonus (quot (count targets) 2)]
                                                   (trash-cards state side targets)
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
                 :req (req (rezzed? current-ice))
                 :msg (msg "lower their maximum hand size by 1 and lower the strength of " (:title current-ice) " to 0")
                 :effect (effect (lose :hero :hand-size-modification 1)
                                 (update! (assoc card :sifr-target current-ice :sifr-used true))
                                 (update-ice-strength current-ice))}]
    :events {:hero-turn-begins {:req (req (:sifr-used card))
                                  :effect (effect (gain :hero :hand-size-modification 1)
                                                  (update! (dissoc card :sifr-used)))}
             :pre-ice-strength {:req (req (= (:cid target) (get-in card [:sifr-target :cid])))
                                :effect (req (let [ice-str (:current-strength target)]
                                               (ice-strength-bonus state side (- ice-str) target)))}
             :run-ends {:effect (effect (update! (dissoc card :sifr-target)))}}}

   "Silencer"
   {:recurring 1}

   "Skulljack"
   {:effect (effect (damage eid :brain 1 {:card card}))
    :events {:pre-trash {:effect (effect (trash-cost-bonus -1))}}}

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
                 :effect (effect (trash card {:cause :ability-cost}) (draw 3))}]}

   "Spy Camera"
   {:abilities [{:cost [:click 1]
                 :delayed-completion true
                 :label "Look at the top X cards of your Stack"
                 :msg "look at the top X cards of their Stack and rearrange them"
                 :effect (req (show-wait-prompt state :minion "Runner to rearrange the top cards of their stack")
                              (let [n (count (filter #(= (:title %) (:title card))
                                                     (all-installed state :hero)))
                                    from (take n (:deck hero))]
                                (if (pos? (count from))
                                  (continue-ability state side (reorder-choice :hero :minion from '()
                                                                               (count from) from) card nil)
                                  (do (clear-wait-prompt state :minion)
                                      (effect-completed state side eid card)))))}
                {:label "[Trash]: Look at the top card of R&D"
                 :msg "trash it and look at the top card of R&D"
                 :effect (effect (prompt! card (str "The top card of R&D is " (:title (first (:deck minion)))) ["OK"] {})
                                 (trash card {:cause :ability-cost}))}]}

   "The Gauntlet"
   {:implementation "Requires Runner to manually (and honestly) set how many ICE were broken directly protecting HQ"
    :in-play [:memory 2]
    :events {:successful-run {:req (req (and (= :hq target)
                                         run))
                              :silent (req true)
                              :delayed-completion true
                              :effect (effect (continue-ability
                                                {:prompt "How many ICE protecting HQ did you break all subroutines on?"
                                                 ;; Makes number of ice on server (HQ) the upper limit.
                                                 ;; This should work since trashed ice do not count according to UFAQ
                                                 :choices {:number (req (count (get-in @state [:minion :servers :hq :ices])))}
                                                 :effect (effect (access-bonus target))}
                                                card nil))}}}

   "The Personal Touch"
   {:hosting {:req #(and (has-subtype? % "Icebreaker")
                         (installed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}

   "The Toolbox"
   {:in-play [:link 2 :memory 2]
    :recurring 2}

   "Titanium Ribs"
   {:events
    {:pre-resolve-damage
     {:delayed-completion true
      :req (req (and (pos? (last targets))
                     (hero-can-choose-damage? state)
                     (not (get-in @state [:damage :damage-replace]))))
      :effect (req (let [dtype target
                         src (second targets)
                         dmg (last targets)]
                     (when (> dmg (count (:hand hero)))
                       (flatline state))
                     (when (= dtype :brain)
                       (swap! state update-in [:hero :brain-damage] #(+ % dmg))
                       (swap! state update-in [:hero :hand-size-modification] #(- % dmg)))
                     (show-wait-prompt state :minion "Runner to use Titanium Ribs to choose cards to be trashed")
                     (when-completed (resolve-ability state side
                                       {:delayed-completion true
                                        :prompt (msg "Select " dmg " cards to trash for the " (name dtype) " damage")
                                        :player :hero
                                        :choices {:max dmg :all true :req #(and (in-hand? %) (= (:side %) "Hero"))}
                                        :msg (msg "trash " (join ", " (map :title targets)))
                                        :effect (req (clear-wait-prompt state :minion)
                                                     (doseq [c targets]
                                                       (trash state side c {:cause dtype :unpreventable true}))
                                                     (trigger-event state side :damage-chosen)
                                                     (damage-defer state side dtype 0)
                                                     (effect-completed state side eid))}
                                      card nil)
                                     (do (trigger-event state side :damage dtype src dmg)
                                         (effect-completed state side eid)))))}
    :damage-chosen {:effect (effect (enable-hero-damage-choice))}}
    :delayed-completion true
    :effect (effect (enable-hero-damage-choice)
                    (system-msg (str "suffers 2 meat damage from installing Titanium Ribs"))
                    (damage eid :meat 2 {:unboostable true :card card}))
    :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-hero))}

   "Top Hat"
   (letfn [(ability [n]
             {:delayed-completion true
              :mandatory true
              :prompt "Which card from the top of R&D would you like to access? (Card 1 is on top.)"
              :choices (take n ["1" "2" "3" "4" "5"])
              :effect (effect (system-msg (str "accesses the card at position " (Integer/parseInt target) " of R&D"))
                              (handle-access eid [(nth (:deck minion) (dec (Integer/parseInt target)))] "an unseen card"))})]
     {:events {:successful-run
               {:req (req (= target :rd))
                :interactive (req true)
                :optional {:prompt "Use Top Hat to choose one of the top 5 cards in R&D to access?"
                           :yes-ability {:effect (req (swap! state assoc-in [:run :run-effect :replace-access]
                                                             (ability (count (:deck minion)))))}}}}})

   "Turntable"
   {:in-play [:memory 1]
    :events {:agenda-stolen
             {:interactive (req true)
              :req (req (not (empty? (:scored minion))))
              :delayed-completion true
              :effect (req
                        (let [stolen target]
                          (continue-ability
                            state side
                            {:optional
                             {:prompt (msg "Swap " (:title stolen) " for an agenda in the Corp's score area?")
                              :yes-ability
                              {:delayed-completion true
                               :effect (req
                                         (continue-ability
                                           state side
                                           {:prompt (str "Select a scored Corp agenda to swap with " (:title stolen))
                                            :choices {:req #(in-minion-scored? state side %)}
                                            :effect (req (let [scored target]
                                                           (swap-agendas state side scored stolen)
                                                           (system-msg state side (str "uses Turntable to swap "
                                                                                       (:title stolen) " for " (:title scored)))
                                                           (effect-completed state side eid card)))}
                                           card targets))}}}
                            card targets)))}}}

   "Ubax"
   (let [ability {:req (req (:hero-phase-12 @state))
                  :msg "draw 1 card"
                  :label "Draw 1 card (start of turn)"
                  :once :per-turn
                  :effect (effect (draw 1))}]
     {:in-play [:memory 1]
      :flags {:hero-turn-draw true
              :hero-phase-12 (req (< 1 (count (filter #(card-flag? % :hero-turn-draw true)
                                                        (cons (get-in @state [:hero :identity])
                                                              (all-installed state :hero))))))}
      :events {:hero-turn-begins ability}
      :abilities [ability]})

   "Unregistered S&W 35"
   {:abilities
    [{:cost [:click 2]
      :req (req (some #{:hq} (:successful-run hero-reg)))
      :label "trash a Bioroid, Clone, Executive or Sysop"
      :prompt "Select a Bioroid, Clone, Executive, or Sysop to trash"
      :choices {:req #(and (rezzed? %)
                           (or (has-subtype? % "Bioroid")
                               (has-subtype? % "Clone")
                               (has-subtype? % "Executive")
                               (has-subtype? % "Sysop"))
                           (or (and (= (last (:zone %)) :content) (is-remote? (second (:zone %))))
                               (= (last (:zone %)) :onhost)))}
      :msg (msg "trash " (:title target)) :effect (effect (trash target))}]}

   "Vigil"
   (let [ability {:req (req (and (:hero-phase-12 @state) (= (count (:hand minion)) (hand-size state :minion))))
                  :msg "draw 1 card"
                  :label "Draw 1 card (start of turn)"
                  :once :per-turn
                  :effect (effect (draw 1))}]
   {:in-play [:memory 1]
    :events {:hero-turn-begins ability}
    :abilities [ability]})

   "Window"
   {:abilities [{:cost [:click 1] :msg "draw 1 card from the bottom of their Stack"
                 :effect (effect (move (last (:deck hero)) :hand))}]}

   "Zamba"
   {:implementation "Credit gain is automatic"
    :in-play [:memory 2]
    :events {:expose {:effect (effect (gain :credit 1))
                      :msg "gain 1 [Credits]"}}}})
