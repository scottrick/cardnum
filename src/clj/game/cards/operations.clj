(in-ns 'game.core)

(def cards-operations
  {"24/7 News Cycle"
   {:req (req (pos? (count (:scored contestant))))
    :delayed-completion true
    :additional-cost [:forfeit]
    :effect (req (continue-ability
                   state side
                   {:prompt "Select an agenda in your score area to trigger its \"when scored\" ability"
                    :choices {:req #(and (is-type? % "Agenda")
                                         (when-scored? %)
                                         (is-scored? state :contestant %))}
                    :msg (msg "trigger the \"when scored\" ability of " (:title target))
                    :delayed-completion true
                    ;dissoc :end-turn for Breaking News
                    :effect (effect (continue-ability (dissoc (card-def target) :end-turn) target nil))}
                   card nil))}

   "Accelerated Diagnostics"
   (letfn [(ad [i n adcard]
             {:prompt "Select an operation to play"
              :choices {:req #(and (= (:side %) "Contestant")
                                   (is-type? % "Operation")
                                   (= (:zone %) [:play-area]))}
              :msg (msg "play " (:title target))
              :delayed-completion true
              :effect (req (when-completed (play-instant state side target {:no-additional-cost true})
                                           (if (and (not (get-in @state [:contestant :register :terminal])) (< i n))
                                             (continue-ability state side (ad (inc i) n adcard) adcard nil)
                                             (effect-completed state side eid))))})]
     {:delayed-completion true
      :implementation "Contestant has to manually cards back to R&D to correctly play a draw operation"
      :effect (req (let [n (count (filter #(is-type? % "Operation")
                                          (take 3 (:deck contestant))))]
                     (continue-ability state side
                                       {:msg "look at the top 3 cards of R&D"
                                        :delayed-completion true
                                        :effect (req (doseq [c (take 3 (:deck contestant))]
                                                       (move state side c :play-area))
                                                     (continue-ability state side (ad 1 n card) card nil))}
                                       card nil)))})

   "Ad Blitz"
   (let [abhelp (fn ab [n total]
                  {:prompt "Select an Advertisement to install and rez" :show-discard true
                   :delayed-completion true
                   :choices {:req #(and (= (:side %) "Contestant")
                                        (has-subtype? % "Advertisement")
                                        (or (in-hand? %)
                                            (= (:zone %) [:discard])))}
                   :effect (req (when-completed
                                  (contestant-install state side target nil {:install-state :rezzed})
                                  (if (< n total)
                                    (continue-ability state side (ab (inc n) total) card nil)
                                    (effect-completed state side eid))))})]
     {:prompt "How many Advertisements?"
      :delayed-completion true
      :choices :credit
      :msg (msg "install and rez " target " Advertisements")
      :effect (effect (continue-ability (abhelp 1 target) card nil))})

   "Aggressive Negotiation"
   {:req (req (:scored-agenda contestant-reg)) :prompt "Choose a card"
    :choices (req (cancellable (:deck contestant) :sorted))
    :effect (final-effect (move target :hand) (shuffle! :deck))
    :msg "search R&D for a card and add it to HQ"}

   "An Offer You Cant Refuse"
   {:delayed-completion false
    :prompt "Choose a server" :choices ["Archives" "R&D" "HQ"]
    :effect (req (let [serv target]
                   (show-wait-prompt state :contestant (str "Challenger to decide on running " target))
                   (continue-ability
                     state side
                     {:optional
                      {:prompt (msg "Make a run on " serv "?") :player :challenger
                       :yes-ability {:msg (msg "let the Challenger make a run on " serv)
                                     :effect (effect (clear-wait-prompt :contestant)
                                                     (game.core/run eid serv nil card))}
                       :no-ability {:effect (effect (clear-wait-prompt :contestant)
                                                    (as-agenda :contestant (some #(when (= (:cid card) (:cid %)) %) (:discard contestant)) 1))
                                    :msg "add it to their score area as an agenda worth 1 agenda point"}}}
                    card nil)))}

   "Anonymous Tip"
   {:msg "draw 3 cards"
    :effect (effect (draw 3))}

   "Archived Memories"
   {:effect (req (let [cid (:cid card)]
                   (resolve-ability state side
                     {:prompt "Select a card from Archives to add to HQ"
                      :show-discard true
                      :choices {:req #(and (not= (:cid %) cid)
                                           (= (:side %) "Contestant")
                                           (= (:zone %) [:discard]))}
                      :effect (final-effect (move target :hand)
                                            (system-msg (str "adds " (if (:seen target) (:title target) "an unseen card") " to HQ")))}
                    card nil)))}

   "Ark Lockdown"
   {:delayed-completion true
    :req (req (not-empty (:discard challenger)))
    :prompt "Name a card to remove all copies in the Heap from the game"
    :choices (req (cancellable (:discard challenger) :sorted))
    :msg (msg "remove all copies of " (:title target) " in the Heap from the game")
    :effect (req (doseq [c (filter #(= (:title target) (:title %)) (:discard challenger))]
                   (move state :challenger c :rfg))
                 (effect-completed state side eid card))}

   "Audacity"
   (let [audacity (fn au [n] {:prompt "Choose a card on which to place an advancement"
                              :delayed-completion true
                              :choices {:req can-be-advanced?}
                              :cancel-effect (req (effect-completed state side eid))
                              :msg (msg "place an advancement token on " (card-str state target))
                              :effect (req (add-prop state :contestant target :advance-counter 1 {:placed true})
                                           (if (< n 2)
                                             (continue-ability state side (au (inc n)) card nil)
                                             (effect-completed state side eid card)))})]
   {:delayed-completion true
    :req (req (let [h (:hand contestant)
                    p (:play-area contestant)]
                ;; this is needed to pass the req check for can-play? and again when card is actually played
                (if (some #(= (:cid %) (:cid card)) p)
                  (>= (count h) 2)
                  (>= (count h) 3))))
    :effect (req (system-msg state side "trashes all cards in HQ due to Audacity")
                 (doseq [c (:hand contestant)]
                   (trash state side c {:unpreventable true}))
                 (continue-ability state side (audacity 1) card nil))})

   "Back Channels"
   {:prompt "Select an installed card in a server to trash"
    :choices {:req #(and (= (last (:zone %)) :content)
                         (is-remote? (second (:zone %))))}
    :effect (final-effect (gain :credit (* 3 (get target :advance-counter 0))) (trash target))
    :msg (msg "trash " (card-str state target) " and gain "
              (* 3 (get target :advance-counter 0)) " [Credits]")}

   "Bad Times"
   {:implementation "Any required resource trashing is manual"
    :req (req tagged)
    :msg "force the Challenger to lose 2[mu] until the end of the turn"
    :effect (req (lose state :challenger :memory 2)
                 (when (< (:memory challenger) 0)
                  (system-msg state :challenger "must trash resources to free up [mu]")))
    :end-turn {:effect (req (gain state :challenger :memory 2)
                            (system-msg state :challenger "regains 2[mu]"))}}

   "Beanstalk Royalties"
   {:msg "gain 3 [Credits]"
    :effect (effect (gain :credit 3))}

   "Best Defense"
   {:delayed-completion true
    :req (req (not-empty (all-installed state :challenger)))
    :effect (req (let [t (:tag challenger)]
                   (continue-ability state side
                     {:prompt (msg "Choose a Challenger card with an install cost of " t " or less to trash")
                      :choices {:req #(and (installed? %)
                                           (<= (:cost %) t))}
                      :msg (msg "trash " (:title target))
                      :effect (effect (trash target))}
                    card nil)))}

   "Biased Reporting"
   {:delayed-completion true
    :req (req (not-empty (all-installed state :challenger)))
    :prompt "Choose a card type"
    :choices ["Muthereff" "Hardware" "Resource"]
    :effect (req (let [t target
                       num (count (filter #(is-type? % t) (all-installed state :challenger)))]
                   (show-wait-prompt state :contestant "Challenger to choose cards to trash")
                   (when-completed
                     (resolve-ability state :challenger
                       {:prompt (msg "Choose any number of cards of type " t " to trash")
                        :choices {:max num :req #(and (installed? %) (is-type? % t))}
                        :cancel-effect (final-effect (clear-wait-prompt :contestant))
                        :effect (req (doseq [c targets]
                                       (trash state :challenger c {:unpreventable true}))
                                     (gain state :challenger :credit (count targets))
                                     (system-msg state :challenger (str "trashes " (join ", " (map :title (sort-by :title targets)))
                                                                    " and gains " (count targets) " [Credits]"))
                                     (clear-wait-prompt state :contestant))}
                      card nil)
                     (do (let [n (* 2 (count (filter #(is-type? % t) (all-installed state :challenger))))]
                           (when (pos? n)
                             (gain state :contestant :credit n)
                             (system-msg state :contestant (str "uses Biased Reporting to gain " n " [Credits]")))
                           (effect-completed state side eid))))))}

   "Big Brother"
   {:req (req tagged)
    :msg "give the Challenger 2 tags"
    :delayed-completion true
    :effect (effect (tag-challenger :challenger eid 2))}

   "Bioroid Efficiency Research"
   {:implementation "Derez is manual"
    :choices {:req #(and (character? %)
                         (has-subtype? % "Bioroid")
                         (installed? %)
                         (not (rezzed? %)))}
    :msg (msg "rez " (card-str state target {:visible true}) " at no cost")
    :effect (final-effect (rez target {:ignore-cost :all-costs})
                          (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}

   "Biotic Labor"
   {:msg "gain [Click][Click]"
    :effect (effect (gain :click 2))}

   "Blue Level Clearance"
   {:msg "gain 5 [Credits] and draw 2 cards"
    :effect (effect (gain :credit 5) (draw 2))}

   "BOOM!"
   {:req (req (> (:tag challenger) 1))
    :delayed-completion true
    :msg "do 7 meat damage"
    :effect (effect (damage eid :meat 7 {:card card}))}

   "Casting Call"
   {:choices {:req #(and (is-type? % "Agenda")
                         (in-hand? %))}
    :delayed-completion true
    :effect (req (let [agenda target]
                   (continue-ability
                     state side {:prompt (str "Choose a server to install " (:title agenda))
                                 :choices (server-list state agenda)
                                 :effect (req (contestant-install state side agenda target {:install-state :face-up})
                                              ; find where the agenda ended up and host on it
                                              (let [agenda (some #(when (= (:cid %) (:cid agenda)) %)
                                                                 (all-installed state :contestant))]
                                                ; the operation ends up in :discard when it is played; to host it,
                                                ; we need (host) to look for it in discard.
                                                (host state side agenda (assoc card :zone [:discard]
                                                                                    :seen true :installed true))
                                                (system-msg state side (str "hosts Casting Call on " (:title agenda)))))}
                     card nil)))
    :events {:access {:req (req (= (:cid target) (:cid (:host card))))
                      :delayed-completion true
                      :effect (effect (tag-challenger :challenger eid 2)) :msg "give the Challenger 2 tags"}}}

   "Celebrity Gift"
   {:choices {:max 5
              :req #(and (= (:side %) "Contestant")
                         (in-hand? %))}
    :msg (msg "reveal " (join ", " (map :title (sort-by :title targets))) " and gain " (* 2 (count targets)) " [Credits]")
    :effect (final-effect (gain :credit (* 2 (count targets))))}

   "Cerebral Cast"
   {:req (req (:successful-run challenger-reg-last))
    :psi {:not-equal {:player :challenger :prompt "Take 1 tag or 1 brain damage?"
                      :choices ["1 tag" "1 brain damage"] :msg (msg "give the Challenger " target)
                      :delayed-completion true
                      :effect (req (if (= target "1 tag")
                                     (tag-challenger state side eid 1)
                                     (damage state side eid :brain 1 {:card card})))}}}

   "Cerebral Static"
   {:msg "disable the Challenger's identity"
    :effect (effect (disable-identity :challenger))
    :leave-play (effect (enable-identity :challenger))}

   "\"Clones are not People\""
   {:events {:agenda-scored {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :effect (effect (as-agenda :contestant card 1))}}}

   "Closed Accounts"
   {:req (req tagged)
    :msg (msg "force the Challenger to lose all " (:credit challenger) " [Credits]")
    :effect (effect (lose :challenger :credit :all))}

   "Commercialization"
   {:msg (msg "gain " (or (:advance-counter target) 0) " [Credits]")
    :choices {:req character?}
    :effect (final-effect (gain :credit (or (:advance-counter target) 0)))}

   "Consulting Visit"
   {:prompt  "Choose an Operation from R&D to play"
    :choices (req (cancellable
             (filter #(and (is-type? % "Operation")
                           (<= (:cost %) (:credit contestant)))
                      (:deck contestant))
             :sorted))
    :effect  (effect (shuffle! :deck)
                     (system-msg "shuffles their deck")
                     (play-instant target))
    :msg (msg "search R&D for " (:title target) " and play it")}

   "Contestantorate Shuffle"
   {:msg "shuffle all cards in HQ into R&D and draw 5 cards"
    :effect (effect (shuffle-into-deck :hand) (draw 5))}

   "Cyberdex Trial"
   {:msg "purge virus counters"
    :effect (effect (purge))}

   "Dark Tryst"
   {:effect (effect (draw 3)
                    (move (first (:play-area played)) :rfg))}

   "Dedication Ceremony"
   {:prompt "Select a faceup card"
    :choices {:req #(or (and (card-is? % :side :contestant)
                             (:rezzed %))
                        (and (card-is? % :side :challenger)
                             (or (installed? %)
                                 (:host %))
                             (not (facedown? %))))}
    :msg (msg "place 3 advancement tokens on " (card-str state target))
    :effect (req (add-prop state :contestant target :advance-counter 3 {:placed true})
                 (effect-completed state side eid card)
                 (let [tgtcid (:cid target)]
                   (register-turn-flag! state side
                     target :can-score
                     (fn [state side card]
                       (if (and (= (:cid card) tgtcid)
                                (>= (:advance-counter card) (or (:current-cost card) (:advancementcost card))))
                         ((constantly false) (toast state :contestant "Cannot score due to Dedication Ceremony." "warning"))
                         true)))))}

   "Defective Brainchips"
   {:events {:pre-damage {:req (req (= target :brain)) :msg "do 1 additional brain damage"
                          :once :per-turn :effect (effect (damage-bonus :brain 1))}}}

   "Distract the Masses"
   (let [shuffle-two {:delayed-completion true
                      :effect (effect (rfg-and-shuffle-rd-effect (find-cid (:cid card) (:discard contestant)) 2))}
         trash-from-hq {:delayed-completion true
                        :prompt "Select up to 2 cards in HQ to trash"
                        :choices {:max 2
                                  :req #(and (= (:side %) "Contestant")
                                             (in-hand? %))}
                        :msg (msg "trash " (quantify (count targets) "card") " from HQ")
                        :effect (req (when-completed
                                       (trash-cards state side targets nil)
                                       (continue-ability state side shuffle-two card nil)))
                        :cancel-effect (req (continue-ability state side shuffle-two card nil))}]
     {:delayed-completion true
      :msg "give The Challenger 2 [Credits]"
      :effect (effect (gain :challenger :credit 2)
                      (continue-ability trash-from-hq card nil))})

   "Diversified Portfolio"
   {:msg (msg "gain " (count (filter #(not (empty? %)) (map #(:content (second %)) (get-remotes @state))))
              " [Credits]")
    :effect (effect (gain :credit (count (filter #(not (empty? %)) (map #(:content (second %)) (get-remotes @state))))))}

   "Door to Door"
   {:events {:challenger-turn-begins
             {:trace {:base 1 :msg (msg (if tagged "do 1 meat damage" "give the Challenger 1 tag"))
                      :label "Do 1 meat damage if Challenger is tagged, or give the Challenger 1 tag"
                      :delayed-completion true
                      :effect (req (if tagged
                                     (damage state side eid :meat 1 {:card card})
                                     (tag-challenger state :challenger eid 1)))}}}}

   "Economic Warfare"
   {:req (req (and (:successful-run challenger-reg-last)
                   (can-pay? state :challenger nil :credit 4)))
    :msg "make the challenger lose 4 [Credits]"
    :effect (effect (lose :challenger :credit 4))}

   "Election Day"
   {:req (req (->> (get-in @state [:contestant :hand])
                   (filter #(not (= (:cid %) (:cid card))))
                   (count)
                   (pos?)))
    :msg (msg "trash all cards in HQ and draw 5 cards")
    :effect (effect (trash-cards (get-in @state [:contestant :hand]))
                    (draw 5))}

   "Enforced Curfew"
   {:msg "reduce the Challenger's maximum hand size by 1"
    :effect (effect (lose :challenger :hand-size-modification 1))
    :leave-play (effect (gain :challenger :hand-size-modification 1))}

   "Enforcing Loyalty"
   {:trace {:base 3
            :label "Trash a card not matching the faction of the Challenger's identity"
            :delayed-completion true
            :effect (req (let [f (:faction (:identity challenger))]
                           (continue-ability
                             state side
                             {:prompt "Select an installed card not matching the faction of the Challenger's identity"
                              :choices {:req #(and (installed? %) (not= f (:faction %)) (card-is? % :side :challenger))}
                              :msg (msg "trash " (:title target))
                              :effect (effect (trash target))}
                            card nil)))}}

   "Enhanced Login Protocol"
   (letfn [(elp-activated [state]
             (get-in @state [:contestant :register :elp-activated] false))
           (add-effect [state side]
             (swap! state assoc-in [:contestant :register :elp-activated] true)
             (click-run-cost-bonus state side [:click 1]))
           (remove-effect [state side]
             (click-run-cost-bonus state side [:click -1])
             (swap! state update-in [:contestant :register] dissoc :elp-activated))]
     {:effect (req (when (and (= :challenger (:active-player @state))
                              (not (:made-click-run challenger-reg)))
                     (add-effect state side)
                     (system-msg state side (str "uses Enhanced Login Protocol to add an additional cost of [Click]"
                                                 " to make the first run not through a card ability this turn"))))
      :events {:challenger-turn-begins {:msg "add an additional cost of [Click] to make the first run not through a card ability this turn"
                                    :effect (effect (add-effect))}
               :challenger-turn-ends {:req (req (elp-activated state))
                                  :effect (effect (remove-effect))}
               :run-ends {:req (req (and (elp-activated state)
                                         (:made-click-run challenger-reg)))
                          :effect (effect (remove-effect))}}
      :leave-play (req (when (elp-activated state)
                         (remove-effect state side)))})

   "Exchange of Information"
   {:req (req (and tagged
                   (seq (:scored challenger))
                   (seq (:scored contestant))))
    :delayed-completion true
    :effect (req
              (continue-ability
                state side
                {:prompt "Select a stolen agenda in the Challenger's score area to swap"
                 :choices {:req #(in-challenger-scored? state side %)}
                 :delayed-completion true
                 :effect (req
                           (let [stolen target]
                             (continue-ability
                               state side
                               {:prompt (msg "Select a scored agenda to swap for " (:title stolen))
                                :choices {:req #(in-contestant-scored? state side %)}
                                :effect (req (let [scored target]
                                               (swap-agendas state side scored stolen)
                                               (system-msg state side (str "uses Exchange of Information to swap "
                                                                           (:title scored) " for " (:title stolen)))
                                               (effect-completed state side eid card)))}
                               card nil)))}
                card nil))}

   "Fast Track"
   {:prompt "Choose an Agenda"
    :choices (req (cancellable (filter #(is-type? % "Agenda") (:deck contestant)) :sorted))
    :effect (effect (system-msg (str "adds " (:title target) " to HQ and shuffle R&D"))
                    (shuffle! :deck)
                    (move target :hand) )}

   "Financial Collapse"
   {:delayed-completion true
    :req (req (and (>= (:credit challenger) 6) (seq (filter #(is-type? % "Muthereff") (all-installed state :challenger)))))
    :effect (req (let [rcount (count (filter #(is-type? % "Muthereff") (all-installed state :challenger)))]
                   (if (pos? rcount)
                     (do (show-wait-prompt state :contestant "Challenger to trash a muthereff to prevent Financial Collapse")
                         (continue-ability
                           state side
                           {:prompt (msg "Trash a muthereff to prevent Financial Collapse?")
                            :choices ["Yes" "No"] :player :challenger
                            :delayed-completion true
                            :effect (final-effect (continue-ability
                                                    (if (= target "Yes")
                                                      {:player :challenger
                                                       :prompt "Select a muthereff to trash"
                                                       :choices {:req #(and (is-type? % "Muthereff") (installed? %))}
                                                       :effect (req (trash state side target {:unpreventable true})
                                                                    (system-msg state :challenger
                                                                                (str "trashes " (:title target)
                                                                                     " to prevent Financial Collapse"))
                                                                    (clear-wait-prompt state :contestant))}
                                                      {:effect (effect (lose :challenger :credit (* rcount 2))
                                                                       (clear-wait-prompt :contestant))
                                                       :msg (msg "make the Challenger lose " (* rcount 2) " [Credits]")})
                                                   card nil))} card nil))
                     (continue-ability
                       state side
                       {:effect (effect (lose :challenger :credit (* rcount 2)))
                        :msg (msg "make the Challenger lose " (* rcount 2) " [Credits]")} card nil))))}

   "Foxfire"
   {:trace {:base 7
            :prompt "Select 1 card to trash"
            :not-distinct true
            :choices {:req #(and (installed? %)
                                 (or (has-subtype? % "Virtual")
                                     (has-subtype? % "Link")))}
            :msg "trash 1 virtual muthereff or link"
            :effect (effect (trash target) (system-msg (str "trashes " (:title target))))}}

   "Freelancer"
   {:req (req tagged)
    :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
    :choices {:max 2
              :req #(and (installed? %)
                         (is-type? % "Muthereff"))}
    :effect (final-effect (trash-cards :challenger targets))}

   "Friends in High Places"
   (let [fhelper (fn fhp [n] {:prompt "Select a card in Archives to install with Friends in High Places"
                              :priority -1
                              :delayed-completion true
                              :show-discard true
                              :choices {:req #(and (= (:side %) "Contestant")
                                                   (not (is-type? % "Operation"))
                                                   (in-discard? %))}
                              :effect (req (when-completed
                                             (contestant-install state side target nil nil)
                                             (do (system-msg state side (str "uses Friends in High Places to "
                                                                             (contestant-install-msg target)))
                                                 (if (< n 2)
                                                   (continue-ability state side (fhp (inc n)) card nil)
                                                   (effect-completed state side eid card)))))})]
     {:delayed-completion true
      :effect (effect (continue-ability (fhelper 1) card nil))})

   "Genotyping"
   {:delayed-completion true
    :effect (effect (mill 2)
                    (system-msg "trashes the top 2 cards of R&D")
                    (rfg-and-shuffle-rd-effect eid (first (:play-area contestant)) 4))}

   "Green Level Clearance"
   {:msg "gain 3 [Credits] and draw 1 card"
    :effect (effect (gain :credit 3) (draw))}

   "Hard-Hitting News"
   {:req (req (:made-run challenger-reg-last))
    :trace {:base 4
            :delayed-completion true
            :msg "give the Challenger 4 tags"
            :label "Give the Challenger 4 tags"
            :effect (effect (tag-challenger :challenger eid 4))}}

   "Hasty Relocation"
   (letfn [(hr-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :delayed-completion true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :contestant c :deck {:front true}))
                                 (clear-wait-prompt state :challenger)
                                 (effect-completed state side eid card))
                             (continue-ability state side (hr-choice original '() 3 original)
                                               card nil)))})
           (hr-choice [remaining chosen n original]
             {:prompt "Choose a card to move next onto R&D"
              :choices remaining
              :delayed-completion true
              :effect (req (let [chosen (cons target chosen)]
                             (if (< (count chosen) n)
                               (continue-ability state side (hr-choice (remove-once #(not= target %) remaining)
                                                                        chosen n original) card nil)
                               (continue-ability state side (hr-final chosen original) card nil))))})]
     {:additional-cost [:mill 1]
      :delayed-completion true
      :msg "trash the top card of R&D, draw 3 cards, and add 3 cards in HQ to the top of R&D"
      :effect (req (draw state side 3)
                   (show-wait-prompt state :challenger "Contestant to add 3 cards in HQ to the top of R&D")
                   (let [from (get-in @state [:contestant :hand])]
                     (continue-ability state :contestant (hr-choice from '() 3 from) card nil)))})

   "Hatchet Job"
   {:trace {:base 5
            :choices {:req #(and (installed? %)
                                 (card-is? % :side :challenger)
                                 (not (has-subtype? % "Virtual")))}
            :msg "add an installed non-virtual card to the Challenger's grip"
            :effect (effect (move :challenger target :hand true))}}

   "Hedge Fund"
   {:msg "gain 9 [Credits]" :effect (effect (gain :credit 9))}

   "Hellion Alpha Test"
   {:req (req (:installed-muthereff challenger-reg-last))
    :trace {:base 2
            :choices {:req #(and (installed? %)
                                 (is-type? % "Muthereff"))}
            :msg "add a Muthereff to the top of the Stack"
            :effect (effect (move :challenger target :deck {:front true})
                            (system-msg (str "adds " (:title target) " to the top of the Stack")))
            :unsuccessful {:msg "take 1 bad publicity"
                           :effect (effect (gain :contestant :bad-publicity 1))}}}

   "Hellion Beta Test"
   {:req (req (:trashed-card challenger-reg-last))
    :trace {:base 2
            :label "Trash 2 installed non-resource cards"
            :choices {:max (req (min 2 (count (filter #(not (is-type? % "Resource")) (concat (all-installed state :contestant)
                                                                                            (all-installed state :challenger))))))
                      :all true
                      :req #(and (installed? %)
                                 (not (is-type? % "Resource")))}
            :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
            :effect (req (doseq [c targets]
                           (trash state side c)))
            :unsuccessful {:msg "take 1 bad publicity"
                           :effect (effect (gain :contestant :bad-publicity 1))}}}

   "Heritage Committee"
   {:delayed-completion true
    :effect (effect (draw 3)
                    (continue-ability
                      {:prompt "Select a card in HQ to put on top of R&D"
                       :choices {:req #(and (= (:side %) "Contestant")
                                            (in-hand? %))}
                       :msg "draw 3 cards and add 1 card from HQ to the top of R&D"
                       :effect (effect (move target :deck {:front true}))}
                      card nil))}

   "Housekeeping"
   {:events {:challenger-install {:player :challenger
                              :prompt "Select a card from your Grip to trash for Housekeeping" :once :per-turn
                              :choices {:req #(and (= (:side %) "Challenger")
                                                   (in-hand? %))}
                              :msg (msg "force the Challenger to trash " (:title target) " from their Grip")
                              :effect (effect (trash target {:unpreventable true}))}}}

   "Hunter Seeker"
   {:req (req (:stole-agenda challenger-reg-last))
    :delayed-completion true
    :prompt "Choose a card to trash"
    :choices {:req installed?}
    :msg (msg "trash " (card-str state target))
    :effect (effect (trash target))}

   "Interns"
   {:prompt "Select a card to install from Archives or HQ"
    :show-discard true
    :not-distinct true
    :choices {:req #(and (not (is-type? % "Operation"))
                         (= (:side %) "Contestant")
                         (#{[:hand] [:discard]} (:zone %)))}
    :effect (final-effect (contestant-install target nil {:no-install-cost true}))
    :msg (msg (contestant-install-msg target))}

   "Invasion of Privacy"
   (letfn [(iop [x]
             {:delayed-completion true
              :req (req (pos? (count (filter #(or (is-type? % "Muthereff")
                                                  (is-type? % "Event")) (:hand challenger)))))
              :prompt "Choose a muthereff or event to trash"
              :msg (msg "trash " (:title target))
              :choices (req (cancellable
                              (filter #(or (is-type? % "Muthereff")
                                           (is-type? % "Event")) (:hand challenger)) :sorted))
              :effect (req (trash state side target)
                           (if (pos? x)
                             (continue-ability state side (iop (dec x)) card nil)
                             (effect-completed state side eid card)))})]
     {:trace {:base 2 :msg "reveal the Challenger's Grip and trash up to X muthereffs or events"
              :effect (req (let [x (- target (second targets))]
                             (system-msg state :contestant
                                         (str "reveals the Challenger's Grip ( "
                                              (join ", " (map :title (sort-by :title (:hand challenger))))
                                              " ) and can trash up to " x " muthereffs or events"))
                             (continue-ability state side (iop (dec x)) card nil)))
              :unsuccessful {:msg "take 1 bad publicity" :effect (effect (gain :contestant :bad-publicity 1))}}})

   "IPO"
   {:msg "gain 13 [Credits]" :effect (effect (gain :credit 13))}

   "Lag Time"
   {:effect (effect (update-all-character))
    :events {:pre-character-strength {:effect (effect (character-strength-bonus 1 target))}}
    :leave-play (effect (update-all-character))}

   "Lateral Growth"
   {:delayed-completion true
    :msg "gain 4 [Credits]"
    :effect (effect (gain :credit 4)
                    (continue-ability {:player :contestant
                                       :prompt "Select a card to install"
                                       :choices {:req #(and (= (:side %) "Contestant")
                                                            (not (is-type? % "Operation"))
                                                            (in-hand? %))}
                                       :delayed-completion true
                                       :msg (msg (contestant-install-msg target))
                                       :effect (effect (contestant-install eid target nil nil))}
                                      card nil))}

   "Liquidation"
   {:delayed-completion true
    :effect (req (let [n (count (filter #(and (rezzed? %)
                                              (not (is-type? % "Agenda"))) (all-installed state :contestant)))]
                   (continue-ability state side
                     {:prompt "Select any number of rezzed cards to trash"
                      :choices {:max n
                                :req #(and (rezzed? %)
                                           (not (is-type? % "Agenda")))}
                      :msg (msg "trash " (join ", " (map :title (sort-by :title targets))) " and gain " (* (count targets) 3) " [Credits]")
                      :effect (req (doseq [c targets]
                                     (trash state side c))
                                   (gain state side :credit (* (count targets) 3)))}
                    card nil)))}

   "Load Testing"
   {:msg "make the Challenger lose [Click] when their next turn begins"
    :effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:challenger-turn-begins {:msg "make the Challenger lose [Click]"
                                  :effect (effect (lose :challenger :click 1)
                                                  (unregister-events card))}}}

   "Localized Product Line"
   {:prompt "Choose a card"
    :choices (req (cancellable (:deck contestant) :sorted))
    :delayed-completion true
    :effect (req (let [c (:title target)
                       cs (filter #(= (:title %) c) (:deck contestant))]
                   (continue-ability
                    state side
                    {:prompt "How many copies?"
                     :choices {:number (req (count cs))}
                     :msg (msg "add " (quantify target "cop" "y" "ies") " of " c " to HQ")
                     :effect (req (shuffle! state :contestant :deck)
                                  (doseq [c (take target cs)]
                                    (move state side c :hand)))}
                    card nil)))}

   "Manhunt"
   {:events {:successful-run {:interactive (req true)
                              :req (req (first-event? state side :successful-run))
                              :trace {:base 2 :msg "give the Challenger 1 tag"
                                      :delayed-completion true
                                      :effect (effect (tag-challenger :challenger eid 1))}}}}

   "Mass Commercialization"
   {:msg (msg "gain " (* 2 (count (filter #(pos? (+ (:advance-counter % 0) (:extra-advance-counter % 0)))
                                          (concat (all-installed state :challenger) (all-installed state :contestant))))) " [Credits]")
    :effect (effect (gain :credit (* 2 (count (filter #(pos? (+ (:advance-counter % 0) (:extra-advance-counter % 0)))
                                                      (concat (all-installed state :challenger) (all-installed state :contestant)))))))}

   "MCA Informant"
   {:implementation "Challenger must deduct 1 click and 2 credits, then trash host manually"
    :req (req (not-empty (filter #(has-subtype? % "Connection") (all-installed state :challenger))))
    :prompt "Choose a connection to host MCA Informant on it"
    :choices {:req #(and (= (:side %) "Challenger") (has-subtype? % "Connection") (installed? %))}
    :msg (msg "host it on " (card-str state target) ". The Challenger has an additional tag")
    :effect (req (host state side (get-card state target) (assoc card :zone [:discard] :seen true))
                 (swap! state update-in [:challenger :tag] inc))
    :leave-play (req (swap! state update-in [:challenger :tag] dec)
                     (system-msg state :contestant "trashes MCA Informant"))}

   "Medical Research Fundraiser"
   {:msg "gain 8 [Credits]. The Challenger gains 3 [Credits]"
    :effect (effect (gain :credit 8) (gain :challenger :credit 3))}

   "Midseason Replacements"
   {:req (req (:stole-agenda challenger-reg-last))
    :trace {:base 6
            :msg "give the Challenger X tags"
            :label "Give the Challenger X tags"
            :delayed-completion true
            :effect (effect (system-msg (str "gives the Challenger " (- target (second targets)) " tags"))
                            (tag-challenger :challenger eid (- target (second targets))))}}

   "Mushin No Shin"
   {:prompt "Select a card to install from HQ"
    :choices {:req #(and (#{"Asset" "Agenda" "Upgrade"} (:type %))
                         (= (:side %) "Contestant")
                         (in-hand? %))}
    :effect (req (contestant-install state side (assoc target :advance-counter 3) "New remote")
                 (effect-completed state side eid card)
                 (let [tgtcid (:cid target)]
                   (register-turn-flag! state side
                     card :can-rez
                     (fn [state side card]
                       (if (= (:cid card) tgtcid)
                         ((constantly false) (toast state :contestant "Cannot rez due to Mushin No Shin." "warning"))
                         true)))
                   (register-turn-flag! state side
                     card :can-score
                     (fn [state side card]
                       (if (and (= (:cid card) tgtcid)
                                (>= (:advance-counter card) (or (:current-cost card) (:advancementcost card))))
                         ((constantly false) (toast state :contestant "Cannot score due to Mushin No Shin." "warning"))
                         true)))))}

   "Mutate"
   {:additional-cost [:character 1]
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))

    :events {:contestant-trash {:effect (req (let [i (character-index state target)
                       [reveal r] (split-with (complement character?) (get-in @state [:contestant :deck]))
                       titles (->> (conj (vec reveal) (first r)) (filter identity) (map :title))]
                                           (system-msg state side (str "uses Mutate to trash " (:title target)))
                                           (when (seq titles)
                                             (system-msg state side (str "reveals " (clojure.string/join ", " titles) " from R&D")))
                                           (if-let [character (first r)]
                                             (let [newcharacter (assoc character :zone (:zone target) :rezzed true)
                                                   characters (get-in @state (cons :contestant (:zone target)))
                                                   newcharacters (apply conj (subvec characters 0 i) newcharacter (subvec characters i))]
                                               (swap! state assoc-in (cons :contestant (:zone target)) newcharacters)
                                               (swap! state update-in [:contestant :deck] (fn [coll] (remove-once #(not= (:cid %) (:cid newcharacter)) coll)))
                                               (trigger-event state side :contestant-install newcharacter)
                                               (card-init state side newcharacter {:resolve-effect false})
                                               (system-msg state side (str "uses Mutate to install and rez " (:title newcharacter) " from R&D at no cost"))
                                               (trigger-event state side :rez newcharacter))
                                             (system-msg state side (str "does not find any Character to install from R&D")))
                                           (shuffle! state :contestant :deck)
                                           (effect-completed state side eid card)
                                           (unregister-events state side card)))}}}

   "Neural EMP"
   {:req (req (:made-run challenger-reg-last))
    :msg "do 1 net damage"
    :effect (effect (damage eid :net 1 {:card card}))}

   "O₂ Shortage"
   {:delayed-completion true
    :effect (req (if (empty? (:hand challenger))
                   (do (gain state :contestant :click 2)
                       (system-msg state side (str "uses O₂ Shortage to gain [Click][Click]"))
                       (effect-completed state side eid))
                   (do (show-wait-prompt state :contestant "Challenger to decide whether or not to trash a card from their Grip")
                       (continue-ability state side
                         {:optional
                          {:prompt "Trash 1 random card from your Grip?"
                           :player :challenger
                           :yes-ability {:effect (effect (trash-cards :challenger (take 1 (shuffle (:hand challenger))))
                                                         (clear-wait-prompt :contestant))}
                           :no-ability {:msg "gain [Click][Click]"
                                        :effect (effect (gain :contestant :click 2)
                                                        (clear-wait-prompt :contestant))}}}
                        card nil))))}

   "Observe and Destroy"
   {:additional-cost [:tag 1]
    :req (req (and (pos? (:tag challenger))
                   (< (:credit challenger) 6)))
    :delayed-completion true
    :effect (effect (continue-ability
                      {:prompt "Select an installed card to trash"
                       :choices {:req installed?}
                       :msg (msg "remove 1 Challenger tag and trash " (:title target))
                       :effect (effect (trash target))}
                     card nil))}

   "Oversight AI"
   {:implementation "Trashing Character is manual"
    :choices {:req #(and (character? %) (not (rezzed? %)) (= (last (:zone %)) :characters))}
    :msg (msg "rez " (:title target) " at no cost")
    :effect (final-effect (rez target {:ignore-cost :all-costs})
                          (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}

   "Patch"
   {:choices {:req #(and (character? %) (rezzed? %))}
    :msg (msg "give +2 strength to " (card-str state target))
    :effect (final-effect (host target (assoc card :zone [:discard] :seen true :condition true))
                          (update-character-strength (get-card state target)))
    :events {:pre-character-strength {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (character-strength-bonus 2 target))}}}

   "Paywall Implementation"
   {:events {:successful-run {:msg "gain 1 [Credits]" :effect (effect (gain :contestant :credit 1))}}}

   "Peak Efficiency"
   {:msg (msg "gain " (reduce (fn [c server]
                                (+ c (count (filter (fn [character] (:rezzed character)) (:characters server)))))
                              0 (flatten (seq (:servers contestant))))
              " [Credits]")
    :effect (effect (gain :credit
                          (reduce (fn [c server]
                                    (+ c (count (filter (fn [character] (:rezzed character)) (:characters server)))))
                                  0 (flatten (seq (:servers contestant))))))}

   "Power Grid Overload"
   {:req (req (:made-run challenger-reg-last))
    :trace {:base 2
            :msg "trash 1 piece of hardware"
            :delayed-completion true
            :effect (req (let [max-cost (- target (second targets))]
                           (continue-ability state side
                                             {:choices {:req #(and (is-type? % "Hardware")
                                                                   (<= (:cost %) max-cost))}
                                              :msg (msg "trash " (:title target))
                                              :effect (effect (trash target))}
                                             card nil))
                         (system-msg state :contestant (str "trashes 1 piece of hardware with install cost less than or equal to " (- target (second targets)))))}}

   "Power Shutdown"
   {:req (req (:made-run challenger-reg-last))
    :prompt "Trash how many cards from the top R&D?"
    :choices {:number (req (apply max (map :cost (filter #(or (= "Resource" (:type %)) (= "Hardware" (:type %))) (all-installed state :challenger)))))}
    :msg (msg "trash " target " cards from the top of R&D")
    :delayed-completion true
    :effect (req (mill state :contestant target)
                 (let [n target]
                   (continue-ability state :challenger
                                     {:prompt "Select a Resource or piece of Hardware to trash"
                                      :choices {:req #(and (#{"Hardware" "Resource"} (:type %))
                                                           (<= (:cost %) n))}
                                      :msg (msg "trash " (:title target))
                                      :effect (effect (trash target))}
                                    card nil)))}

   "Precognition"
   {:delayed-completion true
    :msg "rearrange the top 5 cards of R&D"
    :effect (req (show-wait-prompt state :challenger "Contestant to rearrange the top cards of R&D")
                 (let [from (take 5 (:deck contestant))]
                   (if (pos? (count from))
                     (continue-ability state side (reorder-choice :contestant :challenger from '()
                                                                  (count from) from) card nil)
                     (do (clear-wait-prompt state :challenger)
                         (effect-completed state side eid card)))))}

   "Predictive Algorithm"
   {:events {:pre-steal-cost {:effect (effect (steal-cost-bonus [:credit 2]))}}}

   "Preemptive Action"
   {:effect (effect (rfg-and-shuffle-rd-effect (first (:play-area contestant)) 3))}

   "Priority Construction"
   (letfn [(install-card [chosen]
            {:prompt "Select a remote server"
             :choices (req (conj (vec (get-remote-names @state)) "New remote"))
             :delayed-completion true
             :effect (effect (contestant-install (assoc (move state side chosen :play-area) :advance-counter 3) target {:no-install-cost true}))})]
     {:delayed-completion true
      :prompt "Choose a piece of Character in HQ to install"
      :choices {:req #(and (in-hand? %) (= (:side %) "Contestant") (character? %))}
      :msg "install an Character from HQ and place 3 advancements on it"
      :cancel-effect (req (effect-completed state side eid))
      :effect (effect (continue-ability (install-card target) card nil))})

   "Product Recall"
   {:prompt "Select a rezzed asset or upgrade to trash"
    :choices {:req #(and (rezzed? %)
                         (or (is-type? % "Asset") (is-type? % "Upgrade")))}
    :effect (req (let [c target]
                   (trigger-event state side :pre-trash c)
                   (let [tcost (trash-cost state side c)]
                     (trash state side c)
                     (gain state :contestant :credit tcost)
                     (resolve-ability state side
                       {:msg (msg "trash " (card-str state c) " and gain " tcost " [Credits]")}
                      card nil)
                     (swap! state update-in [:bonus] dissoc :trash)
                     (effect-completed state side eid card))))}

   "Psychographics"
   {:req (req tagged)
    :choices :credit
    :prompt "How many credits?"
    :delayed-completion true
    :effect (req (let [c (min target (:tag challenger))]
                   (continue-ability state side
                                     {:msg (msg "place " c " advancement tokens on "
                                                (card-str state target))
                                      :choices {:req can-be-advanced?}
                                      :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                     card nil)))}

    "Psychokinesis"
    (letfn [(choose-card [cards]
             {:prompt "Select an agenda, asset, or upgrade to install"
              :choices (cons "None" cards)
              :delayed-completion true
              :effect (req (if-not (or (= target "None") (character? target) (is-type? target "Operation"))
                             (continue-ability state side (install-card target) card nil)
                             (system-msg state side "does not install an asset, agenda, or upgrade"))
                           (effect-completed state side eid card)
                           (clear-wait-prompt state :challenger))})
            (install-card [chosen]
             {:prompt "Select a remote server"
              :choices (req (conj (vec (get-remote-names @state)) "New remote"))
              :delayed-completion true
              :effect (effect (clear-wait-prompt :challenger)
                              (contestant-install (move state side chosen :play-area) target))})]
     {:msg "look at the top 5 cards of R&D"
      :delayed-completion true
      :effect (req (show-wait-prompt state :challenger "Contestant to look at the top cards of R&D")
                   (let [from (take 5 (:deck contestant))]
                     (continue-ability state side (choose-card from) card nil)))})

   "Punitive Counterstrike"
   {:trace {:base 5
            :delayed-completion true
            :msg (msg "do " (:stole-agenda challenger-reg-last 0) " meat damage")
            :effect (effect (damage eid :meat (:stole-agenda challenger-reg-last 0) {:card card}))}}

   "Reclamation Order"
   {:prompt "Select a card from Archives"
    :show-discard true
    :choices {:req #(and (= (:side %) "Contestant")
                         (not= (:title %) "Reclamation Order")
                         (= (:zone %) [:discard]))}
    :msg (msg "name " (:title target))
    :effect (req (let [title (:title target)
                       cards (filter #(= title (:title %)) (:discard contestant))
                       n (count cards)]
                   (continue-ability state side
                                     {:prompt (str "Choose how many copies of "
                                                   title " to reveal")
                                      :choices {:number (req n)}
                                      :msg (msg "reveal "
                                                (quantify target "cop" "y" "ies")
                                                " of " title
                                                " from Archives"
                                                (when (pos? target)
                                                  (str " and add "
                                                       (if (= 1 target) "it" "them")
                                                       " to HQ")))
                                      :effect (req (doseq [c (->> cards
                                                                  (sort-by :seen)
                                                                  reverse
                                                                  (take target))]
                                                     (move state side c :hand)))}
                                     card nil)))}

   "Recruiting Trip"
   (let [rthelp (fn rt [total left selected]
                  (if (pos? left)
                    {:prompt (str "Choose a Sysop (" (inc (- total left)) "/" total ")")
                     :choices (req (cancellable (filter #(and (has-subtype? % "Sysop")
                                                              (not (some #{(:title %)} selected))) (:deck contestant)) :sorted))
                     :msg (msg "put " (:title target) " into HQ")
                     :delayed-completion true
                     :effect (req (move state side target :hand)
                                  (continue-ability
                                    state side
                                    (rt total (dec left) (cons (:title target) selected))
                                    card nil))}
                    {:effect (effect (shuffle! :contestant :deck))
                     :msg (msg "shuffle R&D")}))]
   {:prompt "How many Sysops?"
    :delayed-completion true
    :choices :credit
    :msg (msg "search for " target " Sysops")
    :effect (effect (continue-ability (rthelp target target []) card nil))})

   "Red Planet Couriers"
   {:delayed-completion true
    :req (req (some #(can-be-advanced? %) (all-installed state :contestant)))
    :prompt "Select an installed card that can be advanced"
    :choices {:req can-be-advanced?}
    :effect (req (let [installed (get-all-installed state)
                       total-adv (reduce + (map :advance-counter
                                                (filter #(:advance-counter %) installed)))]
                   (doseq [c installed]
                     (update! state side (dissoc c :advance-counter)))
                   (set-prop state side target :advance-counter total-adv)
                   (update-all-character state side)
                   (system-msg state side (str "uses Red Planet Couriers to move " total-adv
                                               " advancement tokens to " (card-str state target)))
                   (effect-completed state side eid)))}

   "Replanting"
   (letfn [(replant [n]
             {:prompt "Select a card to install with Replanting"
              :delayed-completion true
              :choices {:req #(and (= (:side %) "Contestant")
                                   (not (is-type? % "Operation"))
                                   (in-hand? %))}
              :effect (req (when-completed (contestant-install state side target nil {:no-install-cost true})
                                           (if (< n 2)
                                             (continue-ability state side (replant (inc n)) card nil)
                                             (effect-completed state side eid card))))})]
     {:delayed-completion true
      :prompt "Select an installed card to add to HQ"
      :choices {:req #(and (= (:side %) "Contestant")
                           (installed? %))}
      :msg (msg "add " (card-str state target) " to HQ, then install 2 cards ignoring all costs")
      :effect (req (move state side target :hand)
                   (resolve-ability state side (replant 1) card nil))})

   "Restore"
   {:delayed-completion true
    :effect (effect (continue-ability {:prompt "Select a card in Archives to install & rez with Restore"
                                       :priority -1
                                       :delayed-completion true
                                       :show-discard true
                                       :choices {:req #(and (= (:side %) "Contestant")
                                                            (not (is-type? % "Operation"))
                                                            (in-discard? %))}
                                       :effect (req (when-completed
                                                      (contestant-install state side target nil {:install-state :rezzed})
                                                      (do (system-msg state side (str "uses Restore to "
                                                                                      (contestant-install-msg target)))
                                                          (let [leftover (filter #(= (:title target) (:title %)) (-> @state :contestant :discard))]
                                                            (when (seq leftover)
                                                              (doseq [c leftover]
                                                                (move state side c :rfg))
                                                              (system-msg state side (str "removes " (count leftover) " copies of " (:title target) " from the game"))))
                                                          (effect-completed state side eid card))))} card nil))}

   "Restoring Face"
   {:prompt "Select a Sysop, Executive or Clone to trash"
    :msg (msg "trash " (card-str state target) " to remove 2 bad publicity")
    :choices {:req #(and (rezzed? %)
                         (or (has-subtype? % "Clone")
                             (has-subtype? % "Executive")
                             (has-subtype? % "Sysop")))}
    :effect (final-effect (lose :bad-publicity 2) (trash target))}

   "Restructure"
   {:msg "gain 15 [Credits]"
    :effect (effect (gain :credit 15))}

   "Reuse"
   {:delayed-completion true
    :effect (req (let [n (count (:hand contestant))]
                   (continue-ability state side
                     {:prompt (msg "Select up to " n " cards in HQ to trash with Reuse")
                      :choices {:max n
                                :req #(and (= (:side %) "Contestant")
                                           (in-hand? %))}
                      :msg (msg (let [m (count targets)]
                                  (str "trash " (quantify m "card")
                                       " and gain " (* 2 m) " [Credits]")))
                      :effect (effect (trash-cards targets)
                                      (gain :credit (* 2 (count targets))))} card nil)))}

   "Rework"
   {:prompt "Select a card from HQ to shuffle into R&D"
    :choices {:req #(and (= (:side %) "Contestant")
                         (in-hand? %))}
    :msg "shuffle a card from HQ into R&D"
    :effect (final-effect (move target :deck) (shuffle! :deck))}

   "Rolling Brownout"
   {:msg "increase the play cost of operations and events by 1 [Credits]"
    :events {:play-event {:once :per-turn
                          :msg "gain 1 [Credits]"
                          :effect (effect (gain :contestant :credit 1))}
             :pre-play-instant {:effect (effect (play-cost-bonus [:credit 1]))}}}

   "Rover Algorithm"
   {:choices {:req #(and (character? %) (rezzed? %))}
    :msg (msg "host it as a condition counter on " (card-str state target))
    :effect (final-effect (host target (assoc card :zone [:discard] :seen true :condition true))
                          (update-character-strength (get-card state target)))
    :events {:pass-character {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (add-counter card :power 1))}
             :pre-character-strength {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (character-strength-bonus (get-in card [:counter :power] 0) target))}}}

   "Sacrifcharacter"
   {:req (req (pos? (:bad-publicity contestant)))
    :delayed-completion true
    :additional-cost [:forfeit]
    :effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:contestant-forfeit-agenda {:effect (req (let [bplost (min (:agendapoints (last (:rfg contestant))) (:bad-publicity contestant))]
                                                  (if (not (neg? bplost)) (do (lose state side :bad-publicity bplost)
                                                                              (gain state side :credit bplost)
                                                                              (system-msg state side (str "uses Sacrifcharacter to lose " bplost " bad publicity and gain " bplost " [Credits]")))
                                                                          (system-msg state side "uses Sacrifcharacter but gains no credits and loses no Bad Publicity"))
                                                  (effect-completed state side eid)
                                                  (unregister-events state side card)))}}}
   "Salems Hospitality"
   {:prompt "Name a Challenger card"
    :choices {:card-title (req (and (card-is? target :side "Challenger")
                                    (not (card-is? target :type "Identity"))))}
    :effect (req (system-msg state side
                             (str "uses Salem's Hospitality to reveal the Challenger's Grip ( "
                                  (join ", " (map :title (sort-by :title (:hand challenger))))
                                  " ) and trash any copies of " target))
                 (doseq [c (filter #(= target (:title %)) (:hand challenger))]
                   (trash state side c {:unpreventable true})))}

   "Scarcity of Muthereffs"
   {:msg "increase the install cost of muthereffs by 2"
    :events {:pre-install {:req (req (and (is-type? target "Muthereff")
                                          (not (second targets)))) ; not facedown
                           :effect (effect (install-cost-bonus [:credit 2]))}}}

   "Scorched Earth"
   {:req (req tagged)
    :delayed-completion true
    :msg "do 4 meat damage"
    :effect (effect (damage eid :meat 4 {:card card}))}

   "SEA Source"
   {:req (req (:successful-run challenger-reg-last))
    :trace {:base 3
            :msg "give the Challenger 1 tag"
            :label "Give the Challenger 1 tag"
            :delayed-completion true
            :effect (effect (tag-challenger :challenger eid 1))}}

   "Self-Growth Resource"
   {:req (req tagged)
    :prompt "Select two installed Challenger cards"
    :choices {:req #(and (installed? %)
                         (= "Challenger" (:side %)))
              :max 2}
    :msg (msg (str "move " (join ", " (map :title targets)) " to the Challenger's grip"))
    :effect (req (doseq [c targets]
                   (move state :challenger c :hand)))}

   "Servcharacter Outage"
   (letfn [(so-activated [state]
             (get-in @state [:contestant :register :so-activated] false))
           (add-effect [state side]
             (swap! state assoc-in [:contestant :register :so-activated] true)
             (run-cost-bonus state side [:credit 1]))
           (remove-effect [state side]
             (run-cost-bonus state side [:credit -1])
             (swap! state update-in [:contestant :register] dissoc :so-activated))]
     {:msg "add a cost of 1 [Credit] for the Challenger to make the first run each turn"
      :effect (req (when (and (= :challenger (:active-player @state))
                              (empty? (:made-run challenger-reg)))
                     (add-effect state side)))
      :events {:challenger-turn-begins {:msg "add an additional cost of 1 [Credit] to make the first run this turn"
                                    :effect (effect (add-effect))}
               :challenger-turn-ends {:req (req (so-activated state))
                                  :effect (effect (remove-effect))}
               :run-ends {:req (req (so-activated state))
                          :effect (effect (remove-effect))}}
      :leave-play (req (when (so-activated state)
                         (remove-effect state side)))})

   "Shipment from Kaguya"
   {:choices {:max 2 :req can-be-advanced?}
    :msg (msg "place 1 advancement token on " (count targets) " cards")
    :effect (req (doseq [t targets] (add-prop state :contestant t :advance-counter 1 {:placed true}))
                 (effect-completed state side eid card))}

   "Shipment from MirrorMorph"
   (let [shelper (fn sh [n] {:prompt "Select a card to install with Shipment from MirrorMorph"
                             :priority -1
                             :delayed-completion true
                             :choices {:req #(and (= (:side %) "Contestant")
                                                  (not (is-type? % "Operation"))
                                                  (in-hand? %))}
                             :effect (req (when-completed
                                            (contestant-install state side target nil nil)
                                            (if (< n 3)
                                              (continue-ability state side (sh (inc n)) card nil)
                                              (effect-completed state side eid card))))})]
     {:delayed-completion true
      :effect (effect (continue-ability (shelper 1) card nil))})

   "Shipment from SanSan"
   {:choices ["0", "1", "2"]
    :prompt "How many advancement tokens?"
    :delayed-completion true
    :effect (req (let [c (Integer/parseInt target)]
                   (continue-ability
                     state side
                     {:choices {:req can-be-advanced?}
                      :msg (msg "place " c " advancement tokens on " (card-str state target))
                      :effect (effect (add-prop :contestant target :advance-counter c {:placed true}))}
                     card nil)))}

   "Shipment from Tennin"
   {:delayed-completion true
    :req (req (not (:successful-run challenger-reg-last)))
    :choices {:req #(and (installed? %) (= (:side %) "Contestant"))}
    :msg (msg "place 2 advancement tokens on " (card-str state target))
    :effect (effect (add-prop target :advance-counter 2 {:placed true}))}

   "Shoot the Moon"
   {:choices {:req #(and (character? %) (not (rezzed? %)))
              :max (req (min (:tag challenger)
                             (reduce (fn [c server]
                                       (+ c (count (filter #(not (:rezzed %)) (:characters server)))))
                                     0 (flatten (seq (:servers contestant))))))}
    :req (req tagged)
    :effect (req (doseq [t targets] (rez state side t {:ignore-cost :all-costs}))
                 (effect-completed state side eid card))}

   "Snatch and Grab"
   {:trace {:msg "trash a connection"
            :base 3
            :choices {:req #(has-subtype? % "Connection")}
            :delayed-completion true
            :effect (req (let [c target]
                           (continue-ability
                             state side
                             {:prompt (msg "Take 1 tag to prevent " (:title c) " from being trashed?")
                              :choices ["Yes" "No"] :player :challenger
                              :delayed-completion true
                              :effect (effect (continue-ability
                                                (if (= target "Yes")
                                                  {:msg (msg "take 1 tag to prevent " (:title c)
                                                             " from being trashed")
                                                   :delayed-completion true
                                                   :effect (effect (tag-challenger eid 1 {:unpreventable true}))}
                                                  {:delayed-completion true
                                                   :effect (effect (trash eid c nil))
                                                   :msg (msg "trash " (:title c))})
                                                card nil))}
                             card nil)))}}

   "Special Report"
   {:prompt "Select any number of cards in HQ to shuffle into R&D"
    :choices {:max (req (count (:hand contestant)))
              :req #(and (= (:side %) "Contestant")
                         (in-hand? %))}
    :msg (msg "shuffle " (count targets) " cards in HQ into R&D and draw " (count targets) " cards")
    :effect (req (doseq [c targets]
                   (move state side c :deck))
                 (shuffle! state side :deck)
                 (draw state side (count targets))
                 (effect-completed state side eid card))}

   "Stock Buy-Back"
   {:msg (msg "gain " (* 3 (count (:scored challenger))) " [Credits]")
    :effect (effect (gain :credit (* 3 (count (:scored challenger)))))}

   "Sub Boost"
   {:choices {:req #(and (character? %) (rezzed? %))}
    :msg (msg "make " (card-str state target) " gain Barrier and \"[Subroutine] End the run\"")
    :effect (effect (update! (assoc target :subtype (combine-subtypes true (:subtype target) "Barrier")))
                    (update-character-strength target)
                    (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}

   "Subcontract"
   (letfn [(sc [i sccard]
             {:prompt "Select an operation in HQ to play"
              :choices {:req #(and (= (:side %) "Contestant")
                                   (is-type? % "Operation")
                                   (in-hand? %))}
              :delayed-completion true
              :msg (msg "play " (:title target))
              :effect (req (when-completed (play-instant state side target)
                                           (if (and (not (get-in @state [:contestant :register :terminal])) (< i 2))
                                               (continue-ability state side (sc (inc i) sccard) sccard nil)
                                               (effect-completed state side eid))))})]
     {:req (req tagged)
      :delayed-completion true
      :effect (effect (continue-ability (sc 1 card) card nil))})

   "Subliminal Messaging"
   (letfn [(subliminal []
             {:contestant-phase-12
              {:effect
               (req (if (not (:made-run challenger-reg-last))
                      (do (resolve-ability state side
                                           {:optional
                                            {:prompt "Add Subliminal Messaging to HQ?"
                                             :yes-ability {:effect (req (move state side card :hand)
                                                                        (system-msg state side "adds Subliminal Messaging to HQ"))}
                                             :no-ability {:effect (effect (register-events (subliminal) (assoc card :zone '(:discard))))}}}
                                           card nil)
                          (unregister-events state side card))
                      (do (unregister-events state side card)
                          (resolve-ability state side
                                           {:effect (effect (register-events (subliminal) (assoc card :zone '(:discard))))}
                                           card nil))))}})]
     {:msg "gain 1 [Credits]"
      :effect (effect (gain :credit 1)
                      (resolve-ability {:once :per-turn :once-key :subliminal-messaging
                                        :msg "gain [Click]"
                                        :effect (effect (gain :contestant :click 1))} card nil))
      :move-zone (req (if (= [:discard] (:zone card))
                        (register-events state side (subliminal) (assoc card :zone '(:discard)))
                        (unregister-events state side card)))
      :events {:contestant-phase-12 nil}})

   "Success"
   {:additional-cost [:forfeit]
    :effect (req (resolve-ability state side
                                  {:choices {:req can-be-advanced?}
                                   :msg (msg "advance " (card-str state target) " "
                                             (advancement-cost state side (last (:rfg contestant))) " times")
                                   :effect (req (dotimes [_ (advancement-cost state side (last (:rfg contestant)))]
                                                  (advance state :contestant target :no-cost)))} card nil))}

   "Successful Demonstration"
   {:req (req (:unsuccessful-run challenger-reg-last))
    :msg "gain 7 [Credits]"
    :effect (effect (gain :credit 7))}

   "Sunset"
   (letfn [(sun [serv]
             {:prompt "Select two pieces of Character to swap positions"
              :choices {:req #(and (= serv (rest (butlast (:zone %)))) (character? %))
                        :max 2}
              :delayed-completion true
              :effect (req (if (= (count targets) 2)
                             (do (swap-character state side (first targets) (second targets))
                                 (continue-ability state side (sun serv) card nil))
                             (do (system-msg state side "has finished rearranging Character")
                                 (effect-completed state side eid card))))})]
     {:prompt "Choose a server"
      :choices (req servers)
      :delayed-completion true
      :msg (msg "rearrange Character protecting " target)
      :effect (req (let [serv (next (server->zone state target))]
                     (continue-ability state side (sun serv) card nil)))})

   "Sweeps Week"
   {:effect (effect (gain :credit (count (:hand challenger))))
    :msg (msg "gain " (count (:hand challenger)) " [Credits]")}

   "Targeted Marketing"
   (let [gaincr {:req (req (= (:title target) (:marketing-target card)))
                 :effect (effect (gain :contestant :credit 10))
                 :msg (msg "gain 10 [Credits] from " (:marketing-target card))}]
     {:prompt "Name a Challenger card"
      :choices {:card-title (req (and (card-is? target :side "Challenger")
                                      (not (card-is? target :type "Identity"))))}
      :effect (effect (update! (assoc card :marketing-target target))
                      (system-msg (str "uses Targeted Marketing to name " target)))
      :events {:challenger-install gaincr
               :play-event gaincr}})

   "The All-Seeing I"
   (let [trash-all-muthereffs {:player :challenger
                              :effect (req (doseq [muthereff (get-in challenger [:rig :muthereff])]
                                             (trash state side muthereff)))
                              :msg (msg "trash all muthereffs")}]
       {:req (req tagged)
        :delayed-completion true
        :effect (effect
                 (continue-ability
                   (if-not (zero? (:bad-publicity contestant)) ;; If contestant's bad-pub is 0
                     {:optional {:player :challenger
                                 :prompt "Remove 1 bad publicity from the contestant to prevent all muthereffs from being trashed?"
                                 :yes-ability {:effect (effect (lose :contestant :bad-publicity 1))
                                               :player :contestant
                                               :msg (msg "lose 1 bad publicity, preventing all muthereffs from being trashed")}
                                 :no-ability trash-all-muthereffs}}
                    trash-all-muthereffs)
                  card targets))})

   "Threat Assessment"
   {:req (req (:trashed-card challenger-reg-last))
    :prompt "Select an installed Challenger card"
    :choices {:req #(and (= (:side %) "Challenger") (installed? %))}
    :delayed-completion true
    :effect (req (let [chosen target]
                   (show-wait-prompt state side "Challenger to resolve Threat Assessment")
                   (continue-ability state :challenger
                                     {:prompt (str "Add " (:title chosen) " to the top of the Stack or take 2 tags?")
                                      :choices [(str "Move " (:title chosen))
                                                "2 tags"]
                                      :delayed-completion true
                                      :effect (req (clear-wait-prompt state :contestant)
                                                   (move state :contestant (last (:discard contestant)) :rfg)
                                                   (if (.startsWith target "Move")
                                                     (do (system-msg state side (str "chooses to move " (:title chosen) " to the Stack"))
                                                       (move state :challenger chosen :deck {:front true})
                                                       (effect-completed state side eid))
                                                     (do (system-msg state side "chooses to take 2 tags")
                                                       (tag-challenger state :challenger eid 2))))}
                                     card nil)))}

   "Threat Level Alpha"
   {:trace {:base 1
            :label "Give the Challenger X tags"
            :delayed-completion true
            :effect (req (let [tags (-> @state :challenger :tag)]
                           (if (pos? tags)
                             (do (tag-challenger state :challenger eid tags)
                                 (system-msg state side (str "uses Threat Level Alpha to give the Challenger " tags " tags")))
                             (do (tag-challenger state :challenger eid 1)
                                 (system-msg state side "uses Threat Level Alpha to give the Challenger a tag")))))}}

   "Traffic Accident"
   {:req (req (>= (:tag challenger) 2))
    :msg "do 2 meat damage"
    :delayed-completion true
    :effect (effect (damage eid :meat 2 {:card card}))}

   "Transparency Initiative"
   {:choices {:req #(and (is-type? % "Agenda")
                         (installed? %)
                         (not (faceup? %)))}
    :effect (effect (update! (assoc target :seen true :rezzed true
                                           :subtype (combine-subtypes false (:subtype target) "Public")))
                    (host (get-card state target) (assoc card :zone [:discard] :seen true))
                    (register-events
                      {:advance {:req (req (= (:hosted card) (:hosted target)))
                                 :effect (effect (gain :credit 1)
                                                 (system-msg
                                                   (str "uses Transparency Initiative to gain 1 [Credit]")))}}
                      target))}

   "Trick of Light"
   {:choices {:req #(and (contains? % :advance-counter) (> (:advance-counter %) 0))}
    :delayed-completion true
    :effect (req (let [fr target tol card]
                   (continue-ability
                     state side
                     {:prompt "Move how many advancement tokens?"
                      :choices (take (inc (:advance-counter fr)) ["0" "1" "2"])
                      :delayed-completion true
                      :effect (req (let [c (Integer/parseInt target)]
                                     (continue-ability
                                       state side
                                       {:prompt  "Move to where?"
                                        :choices {:req #(and (not= (:cid fr) (:cid %))
                                                             (can-be-advanced? %))}
                                        :effect  (effect (add-prop :contestant target :advance-counter c {:placed true})
                                                         (add-prop :contestant fr :advance-counter (- c) {:placed true})
                                                         (system-msg (str "moves " c " advancement tokens from "
                                                                          (card-str state fr) " to " (card-str state target))))}
                                       tol nil)))}
                     card nil)))}

   "Ultraviolet Clearance"
   {:delayed-completion true
    :effect (req (gain state side :credit 10)
                 (draw state side 4)
                 (continue-ability state side
                   {:prompt "Choose a card in HQ to install"
                    :choices {:req #(and (in-hand? %) (= (:side %) "Contestant") (not (is-type? % "Operation")))}
                    :msg "gain 10 [Credits], draw 4 cards, and install 1 card from HQ"
                    :cancel-effect (req (effect-completed state side eid))
                    :effect (effect (contestant-install target nil))}
                  card nil))}

   "Violet Level Clearance"
   {:msg "gain 8 [Credits] and draw 4 cards"
    :effect (effect (gain :credit 8) (draw 4))}

   "Voter Intimidation"
   {:req (req (seq (:scored challenger)))
    :psi {:not-equal {:player :contestant
                      :prompt "Select a muthereff to trash"
                      :choices {:req #(and (installed? %)
                                           (is-type? % "Muthereff"))}
                      :msg (msg "trash " (:title target))
                      :effect (effect (trash target))}}}

   "Wake Up Call"
   {:req (req (:trashed-card challenger-reg-last))
    :prompt "Select a piece of hardware or non-virtual muthereff"
    :choices {:req #(or (hardware? %)
                        (and (muthereff? %) (not (has-subtype? % "Virtual"))))}
    :delayed-completion true
    :effect (req (let [chosen target
                       wake card]
                   (show-wait-prompt state side "Challenger to resolve Wake Up Call")
                   (continue-ability state :challenger
                                     {:prompt (str "Trash " (:title chosen) " or suffer 4 meat damage?")
                                      :choices [(str "Trash " (:title chosen))
                                                "4 meat damage"]
                                      :delayed-completion true
                                      :effect (req (clear-wait-prompt state :contestant)
                                                   (move state side (last (:discard contestant)) :rfg)
                                                   (if (.startsWith target "Trash")
                                                     (do (system-msg state side (str "chooses to trash " (:title chosen)))
                                                         (trash state side eid chosen nil))
                                                     (do (system-msg state side "chooses to suffer meat damage")
                                                         (damage state side eid :meat 4 {:card wake
                                                                                         :unboostable true}))))}
                                     card nil)))}

   "Wetwork Refit"
   {:choices {:req #(and (character? %)
                         (has-subtype? % "Bioroid")
                         (rezzed? %))}
    :msg (msg "give " (card-str state target) "\"[Subroutine] Do 1 brain damage\" before all its other subroutines")
    :effect (effect (update! (assoc target :subroutines (cons (do-brain-damage 1) (:subroutines target))))
                    (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))
    :leave-play (effect (update! (assoc (:host card) :subroutines (rest (:subroutines (:host card))))))}

   "Witness Tampering"
   {:msg "remove 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}})
