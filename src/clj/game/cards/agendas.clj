(in-ns 'game.core)

(declare is-scored?)

(defn character-boost-agenda [subtype]
  (letfn [(count-character [contestant]
            (reduce (fn [c server]
                      (+ c (count (filter #(and (has-subtype? % subtype)
                                                (rezzed? %))
                                          (:characters server)))))
                    0 (flatten (seq (:servers contestant)))))]
    {:msg (msg "gain " (count-character contestant) " [Credits]")
     :interactive (req true)
     :effect (effect (gain :credit (count-character contestant))
                     (update-all-character))
     :swapped {:effect (req (update-all-character state side))}
     :events {:pre-character-strength {:req (req (has-subtype? target subtype))
                                 :effect (effect (character-strength-bonus 1 target))}}}))

(def cards-agendas
  {"15 Minutes"
   {:abilities [{:cost [:click 1] :msg "shuffle 15 Minutes into R&D"
                 :label "Shuffle 15 Minutes into R&D"
                 :effect (req (let [contestant-agendas (get-in contestant [:scored])
                                    agenda-owner (if (some #(= (:cid %) (:cid card)) contestant-agendas) :contestant :challenger)]
                                (gain-agenda-point state agenda-owner (- (:agendapoints card))))
                              ; refresh agendapoints to 1 before shuffle in case it was modified by e.g. The Board
                              (move state :contestant (dissoc (assoc card :agendapoints 1) :seen :rezzed) :deck {:front true})
                              (shuffle! state :contestant :deck))}]
    :flags {:has-abilities-when-stolen true}}

   "Accelerated Beta Test"
   (letfn [(abt [n i]
             (if (pos? i)
               {:delayed-completion true
                :prompt "Select a piece of Character from the Temporary Zone to install"
                :mutherfucker {:req #(and (= (:side %) "Contestant")
                                     (character? %)
                                     (= (:zone %) [:play-area]))}
                :effect (req (when-completed (contestant-install state side target nil
                                                           {:no-install-cost true :install-state :rezzed-no-cost})
                                             (let [card (get-card state card)]
                                               (unregister-events state side card)
                                               (if (not (:shuffle-occurred card))
                                                 (if (< n i)
                                                   (continue-ability state side (abt (inc n) i) card nil)
                                                   (do (doseq [c (get-in @state [:contestant :play-area])]
                                                         (system-msg state side "trashes a card")
                                                         (trash state side c {:unpreventable true}))
                                                       (effect-completed state side eid)))
                                                 (do (doseq [c (get-in @state [:contestant :play-area])]
                                                       (move state side c :deck))
                                                     (shuffle! state side :deck)
                                                     (effect-completed state side eid))))))
                :cancel-effect (req (doseq [c (get-in @state [:contestant :play-area])]
                                      (system-msg state side "trashes a card")
                                      (trash state side c {:unpreventable true})))}
               {:prompt "None of the cards are character. Say goodbye!"
                :mutherfucker ["I have no regrets"]
                :effect (req (doseq [c (get-in @state [:contestant :play-area])]
                               (system-msg state side "trashes a card")
                               (trash state side c {:unpreventable true})))}))]
     {:interactive (req true)
      :optional {:prompt "Look at the top 3 cards of R&D?"
                 :yes-ability {:delayed-completion true
                               :msg "look at the top 3 cards of R&D"
                               :effect (req (register-events state side
                                                             {:contestant-shuffle-deck
                                                              {:effect (effect (update! (assoc card :shuffle-occurred true)))}}
                                                             card)
                                            (let [n (count (filter character? (take 3 (:deck contestant))))]
                                              (doseq [c (take (min (count (:deck contestant)) 3) (:deck contestant))]
                                                (move state side c :play-area))
                                              (continue-ability state side (abt 1 n) card nil)))}}})

   "Advanced Concept Hopper"
   {:events
    {:run
     {:req (req (first-event? state side :run))
      :effect (effect (show-wait-prompt :challenger "Contestant to use Advanced Concept Hopper")
                      (continue-ability
                        {:player :contestant
                         :prompt "Use Advanced Concept Hopper to draw 1 card or gain 1 [Credits]?" :once :per-turn
                         :mutherfucker ["Draw 1 card" "Gain 1 [Credits]" "No action"]
                         :effect (req (case target
                                        "Gain 1 [Credits]"
                                        (do (gain state :contestant :credit 1)
                                            (system-msg state :contestant (str "uses Advanced Concept Hopper to gain 1 [Credits]")))
                                        "Draw 1 card"
                                        (do (draw state :contestant)
                                            (system-msg state :contestant (str "uses Advanced Concept Hopper to draw 1 card")))
                                        "No action"
                                        (system-msg state :contestant (str "doesn't use Advanced Concept Hopper")))
                                      (clear-wait-prompt state :challenger)
                                      (effect-completed state side eid card))} card nil))}}}

   "Ancestral Imager"
   {:events {:jack-out {:msg "do 1 net damage"
                        :effect (effect (damage :net 1))}}}

   "AR-Enhanced Security"
   {:events {:challenger-trash {:once :per-turn
                            :delayed-completion true
                            :req (req (some #(card-is? % :side :contestant) targets))
                            :msg "give the Challenger a tag for trashing a Contestant card"
                            :effect (effect (tag-challenger :challenger eid 1))}}}

   "Armored Servers"
   {:implementation "Challenger must trash cards manually when required"
    :effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:counter-cost [:agenda 1]
                 :req (req (:run @state))
                 :msg "make the Challenger trash a card from their Grip to jack out or break subroutines for the remainder of the run"}]}

   "AstroScript Pilot Program"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:counter-cost [:agenda 1] :msg (msg "place 1 advancement token on "
                                                      (card-str state target))
                 :mutherfucker {:req can-be-advanced?}
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Award Bait"
   {:access {:delayed-completion true
             :req (req (not-empty (filter #(can-be-advanced? %) (all-installed state :contestant))))
             :effect (effect (show-wait-prompt :challenger "Contestant to place advancement tokens with Award Bait")
                             (continue-ability
                               {:delayed-completion true
                                :mutherfucker ["0", "1", "2"]
                                :prompt "How many advancement tokens?"
                                :effect (req (let [c (Integer/parseInt target)]
                                               (continue-ability
                                                 state side
                                                 {:mutherfucker {:req can-be-advanced?}
                                                  :msg (msg "place " c " advancement tokens on " (card-str state target))
                                                  :cancel-effect (req (clear-wait-prompt state :challenger)
                                                                      (effect-completed state side eid))
                                                  :effect (effect (add-prop :contestant target :advance-counter c {:placed true})
                                                                  (clear-wait-prompt :challenger))} card nil)))}
                              card nil))}}

   "Bacterial Programming"
   (letfn [(hq-step [remaining to-trash to-hq]
             {:delayed-completion true
              :prompt "Select a card to move to HQ"
              :mutherfucker (conj (vec remaining) "Done")
              :effect (req (if (= "Done" target)
                             (do
                               (doseq [t to-trash]
                                 (trash state :contestant t {:unpreventable true}))
                               (doseq [h to-hq]
                                 (move state :contestant h :hand))
                               (continue-ability state :contestant (reorder-chocharacter :contestant (vec remaining)) card nil)
                               (system-msg state :contestant (str "uses Bacterial Programming to add " (count to-hq)
                                                            " cards to HQ, discard " (count to-trash)
                                                            ", and arrange the top cards of R&D")))
                             (do
                               (system-msg state :contestant (str "selected " (:title target) " to move to HQ"))
                               (continue-ability state :contestant (hq-step
                                                               (clojure.set/difference (set remaining) (set [target]))
                                                               to-trash
                                                               (conj to-hq target)) card nil))))})
           (trash-step [remaining to-trash]
             {:delayed-completion true
              :prompt "Select a card to discard"
              :mutherfucker (conj (vec remaining) "Done")
              :effect (req (if (= "Done" target)
                             (continue-ability state :contestant (hq-step remaining to-trash `()) card nil)
                             (do
                               (system-msg state :contestant (str "selected " (:title target) " to trash"))
                               (continue-ability state :contestant (trash-step
                                                               (clojure.set/difference (set remaining) (set [target]))
                                                               (conj to-trash target)) card nil))))})]
     (let [arrange-rd (effect (continue-ability
                                {:optional
                                 {:delayed-completion true
                                  :prompt "Arrange top 7 cards of R&D?"
                                  :yes-ability {:delayed-completion true
                                                :effect (req (let [c (take 7 (:deck contestant))]
                                                               (swap! state assoc-in [:run :shuffled-during-access :rd] true)
                                                               (show-wait-prompt state :challenger "Contestant to use Bacterial Programming")
                                                               (continue-ability state :contestant (trash-step c `()) card nil)))}}}
                                card nil))]
       {:effect arrange-rd
        :delayed-completion true
        :stolen {:delayed-completion true
                 :effect arrange-rd}
        :interactive (req true)}))

   "Bifrost Array"
   {:req (req (not (empty? (filter #(not= (:title %) "Bifrost Array") (:scored contestant)))))
    :optional {:prompt "Trigger the ability of a scored agenda?"
               :yes-ability {:prompt "Select an agenda to trigger the \"when scored\" ability of"
                             :mutherfucker {:req #(and (is-type? % "Agenda")
                                                  (not= (:title %) "Bifrost Array")
                                                  (= (first (:zone %)) :scored)
                                                  (when-scored? %)
                                                  (:abilities %))}
                             :msg (msg "trigger the \"when scored\" ability of " (:title target))
                             :effect (effect (continue-ability (card-def target) target nil))}}}

   "Brain Rewiring"
   {:effect (effect (show-wait-prompt :challenger "Contestant to use Brain Rewiring")
                    (resolve-ability
                      {:optional
                       {:prompt "Pay credits to add random cards from Challenger's Grip to the bottom of their Stack?"
                        :yes-ability {:prompt "How many credits?"
                                      :mutherfucker {:number (req (min (:credit contestant) (count (:hand challenger))))}
                                      :effect (req (when (pos? target)
                                                     (pay state :contestant card :credit target)
                                                     (let [from (take target (shuffle (:hand challenger)))]
                                                       (doseq [c from]
                                                         (move state :challenger c :deck))
                                                       (system-msg state side (str "uses Brain Rewiring to pay " target " [Credits] and add " target
                                                                                   " cards from the Challenger's Grip to the bottom of their Stack. "
                                                                                   "The Challenger draws 1 card"))
                                                       (draw state :challenger)
                                                       (clear-wait-prompt state :challenger))))}
                        :no-ability {:effect (effect (clear-wait-prompt :challenger))}}}
                     card nil))}

   "Braintrust"
   {:effect (effect (add-counter card :agenda (quot (- (:advance-counter card) 3) 2)))
    :silent (req true)
    :events {:pre-rez-cost {:req (req (character? target))
                            :effect (req (rez-cost-bonus state side (- (get-in card [:counter :agenda] 0))))}}}

   "Breaking News"
   {:delayed-completion true
    :effect (effect (tag-challenger :challenger eid 2))
    :silent (req true)
    :msg "give the Challenger 2 tags"
    :end-turn {:effect (effect (lose :challenger :tag 2))
               :msg "make the Challenger lose 2 tags"}}

   "CFC Excavation Contract"
   {:effect (req (let [bios (count (filter #(and (rezzed? %) (has-subtype? % "Bioroid")) (all-installed state :contestant)))
                       bucks (* bios 2)]
                   (gain state side :credit bucks)
                   (system-msg state side (str "gains " bucks " [Credits] from CFC Excavation Contract"))))}

   "Character Assassination"
   {:prompt "Select a resource to trash"
    :mutherfucker {:req #(and (installed? %)
                         (is-type? % "Resource"))}
    :msg (msg "trash " (:title target))
    :interactive (req true)
    :delayed-completion true
    :effect (effect (trash eid target {:unpreventable true}))}

   "Chronos Project"
   {:msg "remove all cards in the Challenger's Heap from the game"
    :interactive (req true)
    :effect (effect (move-zone :challenger :discard :rfg))}

   "Clone Retirement"
   {:msg "remove 1 bad publicity" :effect (effect (lose :bad-publicity 1))
    :silent (req true)
    :stolen {:msg "force the Contestant to take 1 bad publicity"
             :effect (effect (gain :contestant :bad-publicity 1))}}

   "Contestantorate Sales Team"
   (let [e {:effect (req (when (pos? (get-in card [:counter :credit] 0))
                           (gain state :contestant :credit 1)
                           (system-msg state :contestant (str "uses Contestantorate Sales Team to gain 1 [Credits]"))
                           (add-counter state side card :credit -1)))}]
     {:effect (effect (add-counter card :credit 10))
      :silent (req true)
      :events {:challenger-turn-begins e
               :contestant-turn-begins e}})

   "Contestantorate War"
   {:msg (msg (if (> (:credit contestant) 6) "gain 7 [Credits]" "lose all credits"))
    :interactive (req true)
    :effect (req (if (> (:credit contestant) 6)
                   (gain state :contestant :credit 7) (lose state :contestant :credit :all)))}

   "Crisis Management"
   (let [ability {:req (req tagged)
                  :delayed-completion true
                  :label "Do 1 meat damage (start of turn)"
                  :once :per-turn
                  :msg "do 1 meat damage"
                  :effect (effect (damage eid :meat 1 {:card card}))}]
     {:events {:contestant-turn-begins ability}
      :abilities [ability]})

   "Dedicated Neural Net"
    (let [psi-effect
           {:delayed-completion true
            :mandatory true
            :effect (req (if (not-empty (:hand contestant))
                           (do (show-wait-prompt state :challenger "Contestant to select cards in HQ to be accessed")
                               (continue-ability
                                 state :contestant
                                 {:prompt (msg "Select " (access-count state side :hq-access) " cards in HQ for the Challenger to access")
                                  :mutherfucker {:req #(and (in-hand? %) (card-is? % :side :contestant))
                                            :max (req (access-count state side :hq-access))}
                                  :effect (effect (clear-wait-prompt :challenger)
                                                  (continue-ability :challenger
                                                                    (access-helper-hq
                                                                      state (access-count state side :hq-access)
                                                                      ; access-helper-hq uses a set to keep track of which cards have already
                                                                      ; been accessed. Using the set difference we make the challenger unable to
                                                                      ; access non-selected cards from the contestant prompt
                                                                      (clojure.set/difference (set (:hand contestant)) (set targets)))
                                                                    card nil))}
                                 card nil))
                           (effect-completed state side eid card)))}]
       {:events {:successful-run {:interactive (req true)
                                  :psi {:req (req (= target :hq))
                                        :once :per-turn
                                        :not-equal {:effect (req (when-not (:replace-access (get-in @state [:run :run-effect]))
                                                                   (swap! state update-in [:run :run-effect]
                                                                          #(assoc % :replace-access psi-effect)))
                                                                 (effect-completed state side eid))}}}}})

   "Director Haas Pet Project"
   (letfn [(install-ability [server-name n]
             {:prompt "Select a card to install"
              :show-discard true
              :mutherfucker {:req #(and (= (:side %) "Contestant")
                                   (not (is-type? % "Operation"))
                                   (#{[:hand] [:discard]} (:zone %)))}
              :effect (req (contestant-install state side target server-name {:no-install-cost true})
                           (if (< n 2)
                             (continue-ability state side
                                               (install-ability (last (get-remote-names @state)) (inc n))
                                               card nil)
                             (effect-completed state side eid card)))
              :msg (msg (if (pos? n)
                          (contestant-install-msg target)
                          "create a new remote server, installing cards from HQ or Archives, ignoring all install costs"))})]
     {:optional {:prompt "Install cards in a new remote server?"
                 :yes-ability (install-ability "New remote" 0)}})

   "Domestic Sleepers"
   {:agendapoints-challenger (req (do 0))
    :abilities [{:cost [:click 3] :msg "place 1 agenda counter on Domestic Sleepers"
                 :req (req (not (:counter card)))
                 :effect (effect (gain-agenda-point 1)
                                 (set-prop card :counter {:agenda 1} :agendapoints 1))}]}

   "Eden Fragment"
   {:events {:pre-contestant-install
               {:req (req (and (is-type? target "Character")
                               (empty? (let [cards (map first (turn-events state side :contestant-install))]
                                         (filter #(is-type? % "Character") cards)))))
                :effect (effect (ignore-install-cost true))}
             :contestant-install
               {:req (req (and (is-type? target "Character")
                               (empty? (let [cards (map first (turn-events state side :contestant-install))]
                                         (filter #(is-type? % "Character") cards)))))
                :msg (msg "ignore the install cost of the first Character this turn")}}}

   "Efficiency Committee"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:cost [:click 1] :counter-cost [:agenda 1]
                 :effect (effect (gain :click 2)
                                 (register-turn-flag!
                                   card :can-advance
                                   (fn [state side card]
                                     ((constantly false)
                                       (toast state :contestant "Cannot advance cards this turn due to Efficiency Committee." "warning")))))
                 :msg "gain [Click][Click]"}]}

   "Elective Upgrade"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 2))
    :abilities [{:cost [:click 1] :counter-cost [:agenda 1]
                 :once :per-turn
                 :effect (effect (gain :click 2))
                 :msg "gain [Click][Click]"}]}

   "Encrypted Portals"
   (character-boost-agenda "Code Gate")

   "Escalate Vitriol"
   {:abilities [{:label "Gain 1 [Credit] for each Challenger tag"
                 :cost [:click 1]
                 :once :per-turn
                 :msg (msg "gain " (:tag challenger) " [Credits]")
                 :effect (effect (gain :credit (:tag challenger)))}]}

   "Executive Retreat"
   {:effect (effect (add-counter card :agenda 1)
                    (shuffle-into-deck :hand))
    :interactive (req true)
    :abilities [{:cost [:click 1] :counter-cost [:agenda 1] :msg "draw 5 cards" :effect (effect (draw 5))}]}

   "Explode-a-palooza"
   {:access {:delayed-completion true
             :effect (effect (show-wait-prompt :challenger "Contestant to use Explode-a-palooza")
                             (continue-ability
                               {:optional {:prompt "Gain 5 [Credits] with Explode-a-palooza ability?"
                                           :yes-ability {:msg "gain 5 [Credits]"
                                                         :effect (effect (gain :contestant :credit 5)
                                                                         (clear-wait-prompt :challenger))}
                                           :no-ability {:effect (effect (clear-wait-prompt :challenger))}}}
                               card nil))}}

   "False Lead"
   {:abilities [{:req (req (>= (:click challenger) 2)) :msg "force the Challenger to lose [Click][Click]"
                 :effect (effect (forfeit card) (lose :challenger :click 2))}]}

   "Fetal AI"
   {:access {:delayed-completion true
             :req (req (not= (first (:zone card)) :discard)) :msg "do 2 net damage"
             :effect (effect (damage eid :net 2 {:card card}))}
    :steal-cost-bonus (req [:credit 2])}

   "Firmware Updates"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:counter-cost [:agenda 1]
                 :mutherfucker {:req #(and (character? %) (can-be-advanced? %))}
                 :req (req (< 0 (get-in card [:counter :agenda] 0)))
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :once :per-turn
                 :effect (final-effect (add-prop target :advance-counter 1))}]}

   "Genetic Resequencing"
   {:mutherfucker {:req #(= (last (:zone %)) :scored)}
    :msg (msg "add 1 agenda counter on " (:title target))
    :effect (final-effect (add-counter target :agenda 1))
    :silent (req true)}

   "Geothermal Fracking"
   {:effect (effect (add-counter card :agenda 2))
    :silent (req true)
    :abilities [{:cost [:click 1]
                 :counter-cost [:agenda 1]
                 :msg "gain 7 [Credits] and take 1 bad publicity"
                 :effect (effect (gain :credit 7 :bad-publicity 1))}]}

   "Gila Hands Arcology"
   {:abilities [{:cost [:click 2] :effect (effect (gain :credit 3)) :msg "gain 3 [Credits]"}]}

   "Global Food Initiative"
   {:agendapoints-challenger (req (do 2))}

   "Glenn Station"
   {:abilities [{:label "Host a card from HQ on Glenn Station" :cost [:click 1]
                 :prompt "Choose a card to host on Glenn Station" :mutherfucker (req (:hand contestant))
                 :msg "host a card from HQ" :effect (effect (host card target {:facedown true}))}
                {:label "Add a card on Glenn Station to HQ" :cost [:click 1]
                 :prompt "Choose a card on Glenn Station" :mutherfucker (req (:hosted card))
                 :msg "add a hosted card to HQ" :effect (effect (move target :hand))}]}

   "Government Contracts"
   {:abilities [{:cost [:click 2] :effect (effect (gain :credit 4)) :msg "gain 4 [Credits]"}]}

   "Government Takeover"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 3)) :msg "gain 3 [Credits]"}]}

   "Graft"
   (letfn [(graft [n] {:prompt "Choose a card to add to HQ with Graft"
                       :delayed-completion true
                       :mutherfucker (req (cancellable (:deck contestant) :sorted))
                       :msg (msg "add " (:title target) " to HQ from R&D")
                       :cancel-effect (req (shuffle! state side :deck)
                                           (system-msg state side (str "shuffles R&D"))
                                           (effect-completed state side eid))
                       :effect (req (move state side target :hand)
                                    (if (< n 3)
                                      (continue-ability state side (graft (inc n)) card nil)
                                      (do (shuffle! state side :deck)
                                          (system-msg state side (str "shuffles R&D"))
                                          (effect-completed state side eid card))))})]
     {:delayed-completion true
      :msg "add up to 3 cards from R&D to HQ"
      :effect (effect (continue-ability (graft 1) card nil))})

   "Hades Fragment"
   {:flags {:contestant-phase-12 (req (and (not-empty (get-in @state [:contestant :discard])) (is-scored? state :contestant card)))}
    :abilities [{:prompt "Select a card to add to the bottom of R&D"
                 :show-discard true
                 :mutherfucker {:req #(and (= (:side %) "Contestant") (= (:zone %) [:discard]))}
                 :effect (effect (move target :deck))
                 :msg (msg "add " (if (:seen target) (:title target) "a card") " to the bottom of R&D")}]}

   "Helium-3 Deposit"
   {:interactive (req true)
    :mutherfucker ["0", "1", "2"]
    :prompt "How many power counters?"
    :effect (req (let [c (Integer/parseInt target)]
                   (continue-ability
                     state side
                     {:mutherfucker {:req #(< 0 (get-in % [:counter :power] 0))}
                      :msg (msg "add " c " power counters on " (:title target))
                      :effect (final-effect (add-counter target :power c))} card nil)))}

   "High-Risk Investment"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:cost [:click 1]
                 :counter-cost [:agenda 1]
                 :msg (msg "gain " (:credit challenger) " [Credits]")
                 :effect (effect (gain :credit (:credit challenger)))}]}

   "Hostile Takeover"
   {:msg "gain 7 [Credits] and take 1 bad publicity"
    :effect (effect (gain :credit 7 :bad-publicity 1))
    :interactive (req true)}

   "Hollywood Renovation"
   {:install-state :face-up
    :events {:advance
             {:req (req (= (:cid card) (:cid target)))
              :effect (req (let [n (if (>= (:advance-counter (get-card state card)) 6) 2 1)]
                             (continue-ability
                              state side
                              {:mutherfucker {:req #(and (not= (:cid %) (:cid card))
                                                    (can-be-advanced? %))}
                               :msg (msg "place " n " advancement tokens on "
                                         (card-str state target))
                               :effect (final-effect (add-prop :contestant target :advance-counter n {:placed true}))} card nil)))}}}

   "House of Knives"
   {:effect (effect (add-counter card :agenda 3))
    :silent (req true)
    :abilities [{:counter-cost [:agenda 1]
                 :msg "do 1 net damage"
                 :req (req (:run @state))
                 :once :per-run
                 :effect (effect (damage eid :net 1 {:card card}))}]}

   "Ikawah Project"
   {:steal-cost-bonus (req [:credit 2 :click 1])}

   "Illicit Sales"
   {:delayed-completion true
    :effect (req (when-completed
                   (resolve-ability state side
                     {:optional
                      {:prompt "Take 1 bad publicity from Illicit Sales?"
                       :yes-ability {:msg "take 1 bad publicity"
                                     :effect (effect (gain :bad-publicity 1))}
                       :no-ability {:effect (req (effect-completed state side eid))}}}
                     card nil)
                   (do (let [n (* 3 (+ (get-in @state [:contestant :bad-publicity]) (:has-bad-pub contestant)))]
                         (gain state side :credit n)
                         (system-msg state side (str "gains " n " [Credits] from Illicit Sales"))
                         (effect-completed state side eid)))))}

   "Improved Protein Source"
   {:msg "make the Challenger gain 4 [Credits]"
    :effect (effect (gain :challenger :credit 4))
    :interactive (req true)
    :stolen {:msg "make the Challenger gain 4 [Credits]"
             :effect (effect (gain :challenger :credit 4))}}

   "Improved Tracers"
   {:effect (req (update-all-character state side))
    :silent (req true)
    :swapped {:effect (req (update-all-character state side))}
    :events {:pre-character-strength {:req (req (has-subtype? target "Tracer"))
                                :effect (effect (character-strength-bonus 1 target))}
             :pre-init-trace {:req (req (has-subtype? target "Tracer"))
                              :effect (effect (init-trace-bonus 1))}}}

   "Labyrinthine Servers"
   {:prevent {:jack-out [:all]}
    :silent (req true)
    :effect (effect (add-counter card :power 2))
    :abilities [{:req (req (:run @state))
                 :counter-cost [:power 1]
                 :effect (req (let [ls (filter #(= "Labyrinthine Servers" (:title %)) (:scored contestant))]
                                (jack-out-prevent state side)
                                (when (zero? (reduce + (for [c ls] (get-in c [:counter :power]))))
                                  (swap! state update-in [:prevent] dissoc :jack-out))))
                 :msg "prevent the Challenger from jacking out"}]}

   "Lcharacternse Acquisition"
   {:interactive (req true)
    :prompt "Select an asset or upgrade to install from Archives or HQ"
    :show-discard true
    :mutherfucker {:req #(and (#{"Asset" "Upgrade"} (:type %))
                         (#{[:hand] [:discard]} (:zone %))
                         (= (:side %) "Contestant"))}
    :msg (msg "install and rez " (:title target) ", ignoring all costs")
    :effect (effect (contestant-install eid target nil {:install-state :rezzed-no-cost}))}

   "Mandatory Seed Replacement"
   (letfn [(msr [] {:prompt "Select two pieces of Character to swap positions"
                    :mutherfucker {:req #(and (installed? %) (character? %)) :max 2}
                    :effect (req (if (= (count targets) 2)
                                   (do (swap-character state side (first targets) (second targets))
                                       (resolve-ability state side (msr) card nil))
                                   (system-msg state :contestant (str "has finished rearranging Character"))))})]
     {:msg "rearrange any number of Character"
      :effect (effect (resolve-ability (msr) card nil))})

   "Mandatory Upgrades"
   {:msg "gain an additional [Click] per turn"
    :silent (req true)
    :effect (effect (gain :click 1 :click-per-turn 1))
    :swapped {:msg "gain an additional [Click] per turn"
              :effect (req (when (= (:active-player @state) :contestant)
                             (gain state :contestant :click 1))
                           (gain state :contestant :click-per-turn 1))}
    :leave-play (req (lose state :contestant :click 1 :click-per-turn 1))}

   "Market Research"
   {:interactive (req true)
    :req (req tagged)
    :effect (effect (add-counter card :agenda 1)
                    (set-prop card :agendapoints 3))}

   "Medical Breakthrough"
   {:silent (req true)
    :effect (effect (update-all-advancement-costs))
    :stolen {:effect (effect (update-all-advancement-costs))}
    :advancement-cost-bonus (req (- (count (filter #(= (:title %) "Medical Breakthrough")
                                                   (concat (:scored contestant) (:scored challenger))))))}

   "Merger"
   {:agendapoints-challenger (req (do 3))}

   "Meteor Mining"
   (let [mutherfucker ["Take Nothing" "Take 7 [Credits]"]]
    {:interactive (req true)
     :delayed-completion true
     :prompt "Pick what to take"
     :mutherfucker (req (if (> (:tag challenger) 1)
                     (conj mutherfucker "Give 7 Meat Damage")
                     mutherfucker))
     :effect (req (cond

                    (= target "Take 7 [Credits]")
                    (do (gain state side :credit 7)
                        (system-msg state side "takes 7 [Credits] from Meteor Mining")
                        (effect-completed state side eid))

                    (= target "Give 7 Meat Damage")
                    (do (damage state side eid :meat 7 {:card card})
                        (system-msg state side "gives 7 meat damage from Meteor Mining"))

                    (= target "Take Nothing")
                    (do (system-msg state side "did not take anything from Meteor Mining")
                        (effect-completed state side eid))))})

   "NAPD Contract"
   {:steal-cost-bonus (req [:credit 4])
    :advancement-cost-bonus (req (+ (:bad-publicity contestant)
                                    (:has-bad-pub contestant)))}

   "New Construction"
   {:install-state :face-up
    :events {:advance
             {:optional
              {:req (req (= (:cid card) (:cid target)))
               :prompt "Install a card from HQ in a new remote?"
               :yes-ability {:prompt "Select a card to install"
                             :mutherfucker {:req #(and (not (is-type? % "Operation"))
                                                  (not (is-type? % "Character"))
                                                  (= (:side %) "Contestant")
                                                  (in-hand? %))}
                             :msg (msg "install a card from HQ" (when (>= (:advance-counter (get-card state card)) 5)
                                       " and rez it, ignoring all costs"))
                             :effect (req (if (>= (:advance-counter (get-card state card)) 5)
                                            (do (contestant-install state side target "New remote"
                                                              {:install-state :rezzed-no-cost})
                                                (trigger-event state side :rez target))
                                            (contestant-install state side target "New remote")))}}}}}

   "Net Quarantine"
   (let [nq  {:effect (req (let [extra (int (/ (:challenger-spent target) 2))]
                             (when (pos? extra) (gain state side :credit extra)
                                                (system-msg state :contestant (str "uses Net Quarantine to gain " extra " [Credits]")))
                             (when (some? (get-in @state [:challenger :temp-link]))
                               (swap! state assoc-in [:challenger :link] (:temp-link challenger))
                               (swap! state dissoc-in [:challenger :temp-link]))))}]
   {:events
    {:trace     {:once :per-turn
                 :silent (req true)
                 :effect (req
                           (system-msg state :contestant "uses Net Quarantine to reduce Challenger's base link to zero")
                           (swap! state assoc-in [:challenger :temp-link] (:link challenger))
                           (swap! state assoc-in [:challenger :link] 0))}
    :successful-trace nq
    :unsuccessful-trace nq}})

   "NEXT Wave 2"
   {:delayed-completion true
    :not-when-scored true
    :effect (req (if (some #(and (rezzed? %) (character? %) (has-subtype? % "NEXT")) (all-installed state :contestant))
                   (continue-ability state side
                     {:optional
                      {:prompt "Do 1 brain damage with NEXT Wave 2?"
                       :yes-ability {:msg "do 1 brain damage"
                                     :effect (effect (damage eid :brain 1 {:card card}))}
                       :no-ability {:effect (req (effect-completed state side eid))}}}
                    card nil)
                   (effect-completed state side eid)))}

   "Nisei MK II"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 1))
    :abilities [{:req (req (:run @state))
                 :counter-cost [:agenda 1]
                 :msg "end the run"
                 :effect (effect (end-run))}]}

   "Oaktown Renovation"
   {:install-state :face-up
    :events {:advance {:req (req (= (:cid card) (:cid target)))
                       :msg (msg "gain " (if (>= (:advance-counter (get-card state card)) 5) "3" "2") " [Credits]")
                       :effect (req (gain state side :credit
                                          (if (>= (:advance-counter (get-card state card)) 5) 3 2)))}}}

   "Obokata Protocol"
   {:steal-cost-bonus (req [:net-damage 4])}

   "Paper Trail"
   {:trace {:base 6
            :msg "trash all connection and job resources"
            :effect (req (doseq [resource (filter #(or (has-subtype? % "Job")
                                                       (has-subtype? % "Connection"))
                                                  (all-installed state :challenger))]
                                   (trash state side resource)))}}

   "Personality Profiles"
   (let [pp {:req (req (pos? (count (:hand challenger))))
             :effect (effect (trash (first (shuffle (:hand challenger)))))
             :msg (msg "force the Challenger to trash " (:title (last (:discard challenger))) " from their Grip at random")}]
     {:events {:searched-stack pp
               :challenger-install (assoc pp :req (req (and (some #{:discard} (:previous-zone target))
                                                        (pos? (count (:hand challenger))))))}})

   "Philotic Entanglement"
   {:interactive (req true)
    :req (req (> (count (:scored challenger)) 0))
    :msg (msg "do " (count (:scored challenger)) " net damage")
    :effect (effect (damage eid :net (count (:scored challenger)) {:card card}))}

   "Posted Bounty"
   {:optional {:prompt "Forfeit Posted Bounty to give the Challenger 1 tag and take 1 bad publicity?"
               :yes-ability {:msg "give the Challenger 1 tag and take 1 bad publicity"
                             :delayed-completion true
                             :effect (effect (gain :bad-publicity 1)
                                             (tag-challenger :challenger eid 1)
                                             (forfeit card))}}}

   "Priority Requisition"
   {:interactive (req true)
    :mutherfucker {:req #(and (character? %) (not (rezzed? %)) (installed? %))}
    :effect (effect (rez target {:ignore-cost :all-costs}))}

   "Private Security Force"
   {:abilities [{:req (req tagged) :cost [:click 1] :effect (effect (damage eid :meat 1 {:card card}))
                 :msg "do 1 meat damage"}]}

   "Profiteering"
   {:interactive (req true)
    :mutherfucker ["0" "1" "2" "3"] :prompt "How many bad publicity?"
    :msg (msg "take " target " bad publicity and gain " (* 5 (Integer/parseInt target)) " [Credits]")
    :effect (final-effect (gain :credit (* 5 (Integer/parseInt target))
                                :bad-publicity (Integer/parseInt target)))}

   "Project Ares"
   (letfn [(trash-count-str [card]
             (quantify (- (:advance-counter card) 4) "installed card"))]
     {:silent (req true)
      :req (req (and (> (:advance-counter card) 4)
                     (pos? (count (all-installed state :challenger)))))
      :msg (msg "force the Challenger to trash " (trash-count-str card) " and take 1 bad publicity")
      :delayed-completion true
      :effect (effect (show-wait-prompt :contestant "Challenger to trash installed cards")
                      (continue-ability
                       :challenger
                       {:prompt (msg "Select " (trash-count-str card) " installed cards to trash")
                        :mutherfucker {:max (min (- (:advance-counter card) 4)
                                            (count (all-installed state :challenger)))
                                  :req #(and (= (:side %) "Challenger")
                                             (:installed %))}
                        :effect (final-effect (trash-cards targets)
                                              (system-msg (str "trashes " (join ", " (map :title targets))))
                                              (gain :contestant :bad-publicity 1))}
                       card nil)
                      (clear-wait-prompt :contestant))})

   "Project Atlas"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (max 0 (- (:advance-counter card) 3))))
    :abilities [{:counter-cost [:agenda 1]
                 :prompt "Choose a card"
                 :label "Search R&D and add 1 card to HQ"
                 ;; we need the req or the prompt will still show
                 :req (req (< 0 (get-in card [:counter :agenda] 0)))
                 :msg (msg "add " (:title target) " to HQ from R&D")
                 :mutherfucker (req (cancellable (:deck contestant) :sorted))
                 :cancel-effect (effect (system-msg "cancels the effect of Project Atlas"))
                 :effect (effect (shuffle! :deck)
                                 (move target :hand))}]}

   "Project Beale"
   {:interactive (req true)
    :agendapoints-challenger (req (do 2))
    :effect (req (let [n (quot (- (:advance-counter card) 3) 2)]
                    (set-prop state side card :counter {:agenda n} :agendapoints (+ 2 n))))}

   "Project Kusanagi"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (- (:advance-counter card) 2)))
    :abilities [{:counter-cost [:agenda 1]
                 :msg "make a piece of Character gain \"[Subroutine] Do 1 net damage\" after all its other subroutines for the remainder of the run"}]}

   "Project Vitruvius"
   {:silent (req true)
    :effect (effect (add-counter card :agenda (- (:advance-counter card) 3)))
    :abilities [{:counter-cost [:agenda 1]
                 :prompt "Choose a card in Archives to add to HQ"
                 :show-discard true
                 :mutherfucker {:req #(and (in-discard? %) (= (:side %) "Contestant"))}
                 :req (req (< 0 (get-in card [:counter :agenda] 0)))
                 :msg (msg "add " (if (:seen target)
                                    (:title target) "an unseen card ") " to HQ from Archives")
                 :effect (effect (move target :hand))}]}

   "Project Wotan"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:req (req (and (character? current-character)
                                (rezzed? current-character)
                                (has-subtype? current-character "Bioroid")))
                 :counter-cost [:agenda 1]
                 :msg (str "make the approached piece of Bioroid Character gain \"[Subroutine] End the run\""
                           "after all its other subroutines for the remainder of this run")}]}

   "Puppet Master"
   {:events {:successful-run
             {:interactive (req true)
              :delayed-completion true
              :effect (req (show-wait-prompt state :challenger "Contestant to use Puppet Master")
                           (continue-ability
                             state :contestant
                             {:prompt "Select a card to place 1 advancement token on"
                              :player :contestant
                              :mutherfucker {:req can-be-advanced?}
                              :cancel-effect (final-effect (clear-wait-prompt :challenger))
                              :msg (msg "place 1 advancement token on " (card-str state target))
                              :effect (final-effect (add-prop :contestant target :advance-counter 1 {:placed true})
                                                    (clear-wait-prompt :challenger))} card nil))}}}

   "Quantum Predictive Model"
   {:steal-req (req (not tagged))
    :access {:req (req tagged)
             :effect (effect (as-agenda card 1))
             :msg "add it to their score area and gain 1 agenda point"}}

   "Rebranding Team"
   (letfn [(get-assets [state contestant]
             (filter #(is-type? % "Asset") (concat (all-installed state :contestant)
                                                   (:deck contestant)
                                                   (:hand contestant)
                                                   (:discard contestant))))
           (add-ad [state side c]
             (update! state side (assoc c :subtype (combine-subtypes false ;append ad even if it is already an ad
                                                                     (:subtype c "")
                                                                     "Advertisement"))))]
     {:interactive (req true)
      :msg "make all assets gain Advertisement"
      :effect (req (doseq [c (get-assets state contestant)] (add-ad state side c)))
      :swapped {:msg "make all assets gain Advertisement"
                :effect (req (doseq [c (get-assets state contestant)] (add-ad state side c)))}
      :leave-play (req (doseq [c (get-assets state contestant)]
                         (update! state side (assoc c :subtype
                                                      (->> (split (or (:subtype c) "") #" - ")
                                                           (drop 1) ;so that all actual ads remain ads if agenda leaves play
                                                           (join " - "))))))})

   "Reeducation"
   (letfn [(contestant-final [chosen original]
             {:prompt (str "The bottom cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :mutherfucker ["Done" "Start over"]
              :delayed-completion true
              :msg (req (let [n (count chosen)]
                          (str "add " n " cards from HQ to the bottom of R&D and draw " n " cards.
                          The Challenger randomly adds " (if (<= n (count (:hand challenger))) n 0) " cards from their Grip
                          to the bottom of the Stack")))
              :effect (req (let [n (count chosen)]
                             (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :contestant c :deck))
                                 (draw state :contestant n)
                                 ; if contestant chooses more cards than challenger's hand, don't shuffle challenger hand back into Stack
                                 (when (<= n (count (:hand challenger)))
                                   (doseq [r (take n (shuffle (:hand challenger)))] (move state :challenger r :deck)))
                                 (clear-wait-prompt state :challenger)
                                 (effect-completed state side eid card))
                             (continue-ability state side (contestant-chocharacter original '() original) card nil))))})
           (contestant-chocharacter [remaining chosen original] ; Contestant chooses cards until they press 'Done'
             {:prompt "Choose a card to move to bottom of R&D"
              :mutherfucker (conj (vec remaining) "Done")
              :delayed-completion true
              :effect (req (let [chosen (cons target chosen)]
                             (if (not= target "Done")
                               (continue-ability state side (contestant-chocharacter (remove-once #(not= target %) remaining)
                                                                       chosen original) card nil)
                               (if (pos? (count (remove #(= % "Done") chosen)))
                                 (continue-ability state side (contestant-final (remove #(= % "Done") chosen) original) card nil)
                                 (do (system-msg state side "does not add any cards from HQ to bottom of R&D")
                                     (clear-wait-prompt state :challenger)
                                     (effect-completed state side eid card))))))})]
   {:delayed-completion true
    :effect (req (show-wait-prompt state :challenger "Contestant to add cards from HQ to bottom of R&D")
                 (let [from (get-in @state [:contestant :hand])]
                   (if (pos? (count from))
                     (continue-ability state :contestant (contestant-chocharacter from '() from) card nil)
                     (do (system-msg state side "does not add any cards from HQ to bottom of R&D")
                         (effect-completed state side eid card)))))})

   "Remote Data Farm"
   {:silent (req true)
    :msg "increase their maximum hand size by 2"
    :effect (effect (gain :hand-size-modification 2))
    :swapped {:msg "increase their maximum hand size by 2"
              :effect (effect (gain :hand-size-modification 2))}
    :leave-play (effect (lose :hand-size-modification 2))}

   "Research Grant"
   {:interactive (req true)
    :req (req (not (empty? (filter #(= (:title %) "Research Grant") (all-installed state :contestant)))))
    :delayed-completion true
    :effect (effect (continue-ability
                      {:prompt "Select another installed copy of Research Grant to score"
                       :mutherfucker {:req #(= (:title %) "Research Grant")}
                       :effect (effect (set-prop target :advance-counter (:advancementcost target))
                                       (score target))
                       :msg "score another installed copy of Research Grant"}
                     card nil))}

   "Restructured Datapool"
   {:abilities [{:cost [:click 1]
                 :trace {:base 2 :msg "give the Challenger 1 tag"
                         :delayed-completion true
                         :effect (effect (tag-challenger :challenger eid 1))}}]}

   "Self-Destruct Chips"
   {:silent (req true)
    :msg "decrease the Challenger's maximum hand size by 1"
    :effect (effect (lose :challenger :hand-size-modification 1))
    :swapped {:msg "decrease the Challenger's maximum hand size by 1"
              :effect (effect (lose :challenger :hand-size-modification 1))}
    :leave-play (effect (gain :challenger :hand-size-modification 1))}

   "Sensor Net Activation"
   {:effect (effect (add-counter card :agenda 1))
    :silent (req true)
    :abilities [{:counter-cost [:agenda 1]
                 :req (req (some #(and (has-subtype? % "Bioroid") (not (rezzed? %))) (all-installed state :contestant)))
                 :prompt "Choose a bioroid to rez, ignoring all costs"
                 :mutherfucker {:req #(and (has-subtype? % "Bioroid") (not (rezzed? %)))}
                 :msg (msg "rez " (card-str state target) ", ignoring all costs")
                 :effect (req (let [c target]
                                (rez state side c {:ignore-cost :all-costs})
                                (register-events state side
                                  {:contestant-turn-ends {:effect (effect (derez c)
                                                                    (unregister-events card))}
                                   :challenger-turn-ends {:effect (effect (derez c)
                                                                      (unregister-events card))}} card)))}]
      :events {:contestant-turn-ends nil :challenger-turn-ends nil}}

   "Sentinel Defense Program"
   {:events {:pre-resolve-damage {:req (req (and (= target :brain) (> (last targets) 0)))
                                  :msg "do 1 net damage"
                                  :effect (effect (damage eid :net 1 {:card card}))}}}

   "Show of Force"
   {:delayed-completion true
    :msg "do 2 meat damage"
    :effect (effect (damage eid :meat 2 {:card card}))}

   "SSL Endorsement"
   (let [add-credits (effect (add-counter card :credit 9))
         remove-credits {:optional {:req (req (pos? (get-in card [:counter :credit] -1)))
                                    :prompt "Gain 3 [Credits] from SSL Endorsement?"
                                    :yes-ability
                                    {:effect (req (when (pos? (get-in card [:counter :credit] -1))
                                                    (gain state :contestant :credit 3)
                                                    (system-msg state :contestant (str "uses SSL Endorsement to gain 3 [Credits]"))
                                                    (add-counter state side card :credit -3)))}}}]
     {:effect add-credits
      :stolen {:effect add-credits}
      :interactive (req true)
      :events {:contestant-turn-begins remove-credits}
      :flags {:has-events-when-stolen true}})

   "Standoff"
   (letfn [(stand [side]
             {:delayed-completion true
              :prompt "Choose one of your installed cards to trash due to Standoff"
              :mutherfucker {:req #(and (installed? %)
                                   (same-side? side (:side %)))}
              :cancel-effect (req (if (= side :challenger)
                                    (do (draw state :contestant)
                                        (gain state :contestant :credit 5)
                                        (clear-wait-prompt state :contestant)
                                        (system-msg state :challenger "declines to trash a card due to Standoff")
                                        (system-msg state :contestant "draws a card and gains 5 [Credits] from Standoff")
                                        (effect-completed state :contestant eid))
                                    (do (system-msg state :contestant "declines to trash a card from Standoff")
                                        (clear-wait-prompt state :challenger)
                                        (effect-completed state :contestant eid))))
              :effect (req (do (system-msg state side (str "trashes " (card-str state target) " due to Standoff"))
                               (clear-wait-prompt state (other-side side))
                               (trash state side target {:unpreventable true})
                               (show-wait-prompt state side (str (side-str (other-side side)) " to trash a card for Standoff"))
                               (continue-ability state (other-side side) (stand (other-side side)) card nil)))})]
     {:interactive (req true)
      :delayed-completion true
      :effect (effect (show-wait-prompt (str (side-str (other-side side)) " to trash a card for Standoff"))
                      (continue-ability :challenger (stand :challenger) card nil))})

   "Successful Field Test"
   (letfn [(sft [n max] {:prompt "Select a card in HQ to install with Successful Field Test"
                         :priority -1
                         :delayed-completion true
                         :mutherfucker {:req #(and (= (:side %) "Contestant")
                                              (not (is-type? % "Operation"))
                                              (in-hand? %))}
                         :effect (req (when-completed
                                        (contestant-install state side target nil {:no-install-cost true})
                                        (if (< n max)
                                          (continue-ability state side (sft (inc n) max) card nil)
                                          (effect-completed state side eid card))))})]
     {:delayed-completion true
      :msg "install cards from HQ, ignoring all costs"
      :effect (req (let [max (count (filter #(not (is-type? % "Operation")) (:hand contestant)))]
                     (continue-ability state side (sft 1 max) card nil)))})

   "Superior Cyberwalls"
   (character-boost-agenda "Barrier")

   "TGTBT"
   {:access {:msg "give the Challenger 1 tag"
             :delayed-completion true
             :effect (effect (tag-challenger :challenger eid 1))}}

   "The Cleaners"
   {:events {:pre-damage {:req (req (and (= target :meat)
                                         (= side :contestant)))
                          :msg "do 1 additional meat damage"
                          :effect (effect (damage-bonus :meat 1))}}}

   "The Future is Now"
   {:interactive (req true)
    :prompt "Choose a card to add to HQ" :mutherfucker (req (:deck contestant))
    :msg (msg "add a card from R&D to HQ and shuffle R&D")
    :effect (effect (shuffle! :deck)
                    (move target :hand) )}

   "The Future Perfect"
   {:access
    {:psi {:req (req (not installed))
           :not-equal {:msg (msg "prevent it from being stolen")
                       :effect (final-effect (register-run-flag! card :can-steal
                                                                 (fn [_ _ c] (not= (:cid c) (:cid card)))))}}}}

   "Underway Renovation"
   {:install-state :face-up
    :events {:advance {:req (req (= (:cid card) (:cid target))) 
                       :msg (msg (let [deck (:deck challenger)
                                       anydeck? (pos? (count deck)) 
                                       adv4? (>= (:advance-counter (get-card state card)) 4)] 
                         (cond
                           (and anydeck? adv4?)
                           (str "trash " (join ", " (map :title (take 2 deck))) " from the Challenger's stack")

                           (and anydeck? (not adv4?) )
                           (str "trash " (:title (first deck)) " from the Challenger's stack")

                           (false? anydeck?)  
                           "trash from the Challenger's stack but it is empty")))

                       :effect (effect (mill :challenger
                                             (if (>= (:advance-counter (get-card state card)) 4)  2 1)))}}}

   "Unorthodox Predictions"
   {:implementation "Prevention of subroutine breaking is not enforced"
    :delayed-completion false
    :prompt "Choose an Character type for Unorthodox Predictions" :mutherfucker ["Sentry", "Code Gate", "Barrier"]
    :msg (msg "prevent subroutines on " target " Character from being broken until next turn.")}

   "Utopia Fragment"
   {:events {:pre-steal-cost {:req (req (pos? (or (:advance-counter target) 0)))
                              :effect (req (let [counter (:advance-counter target)]
                                             (steal-cost-bonus state side [:credit (* 2 counter)])))}}}

   "Veterans Program"
   {:interactive (req true)
    :msg "lose 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}

   "Voting Machine Initiative"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:optional {:req (req (> (get-in card [:counter :agenda] 0)
                                         (:vmi-count card 0)))
                            :prompt "Cause the Challenger to lose [Click] at the start of their next turn?"
                            :yes-ability {:effect (effect (toast (str "The Challenger will lose " (inc (:vmi-count card 0))
                                                                      " [Click] at the start of their next turn") "info")
                                                    (update! (update-in card [:vmi-count] #(inc (or % 0)))))}}}]
    :events {:challenger-turn-begins {:req (req (pos? (:vmi-count card 0)))
                                  :msg (msg "force the Challenger to lose " (:vmi-count card) " [Click]")
                                  :effect (effect (lose :challenger :click (:vmi-count card))
                                                  (add-counter (dissoc card :vmi-count) :agenda (- (:vmi-count card))))}}}

   "Vulcan Coverup"
   {:interactive (req true)
    :msg "do 2 meat damage"
    :effect (effect (damage eid :meat 2 {:card card}))
    :stolen {:msg "force the Contestant to take 1 bad publicity"
             :effect (effect (gain :contestant :bad-publicity 1))}}

   "Water Monopoly"
   {:events {:pre-install {:req (req (and (is-type? target "Resource")
                                          (not (has-subtype? target "Virtual"))
                                          (not (second targets)))) ; not facedown
                           :effect (effect (install-cost-bonus [:credit 1]))}}}})
