(ns game.cards.events
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int]]
            [cardnum.cards :refer [all-cards]]))

(defn- run-event
  ([] (run-event nil))
  ([run-ability] (run-event nil run-ability))
  ([cdef run-ability] (run-event cdef run-ability nil))
  ([cdef run-ability pre-run-effect]
   (run-event cdef run-ability pre-run-effect nil))
  ([cdef run-ability pre-run-effect post-run-effect]
   (merge {:prompt "Choose a locale"
           :choices (req runnable-locales)
           :effect (effect ((or pre-run-effect (effect)) eid card targets)
                           (run target run-ability card)
                           ((or post-run-effect (effect)) eid card targets))}
          cdef)))

(def card-definitions
  {"Account Siphon"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Contestant to lose " (min 5 (:credit contestant))
                                         " [Credits], gain " (* 2 (min 5 (:credit contestant)))
                                         " [Credits] and take 2 tags")
                               :async true
                               :effect (req (wait-for (tag-challenger state :challenger 2)
                                                      (do (gain-credits state :challenger (* 2 (min 5 (:credit contestant))))
                                                          (lose-credits state :contestant (min 5 (:credit contestant)))
                                                          (effect-completed state side eid))))}} card))}

   "Amped Up"
   {:msg "gain [Click][Click][Click] and suffer 1 brain damage"
    :effect (effect (gain :click 3) (damage eid :brain 1 {:unpreventable true :card card}))}

   "Another Day, Another Paycheck"
   {:events {:agenda-stolen
             {:trace {:base 0
                      :unsuccessful
                      {:effect (effect (gain-credits
                                         :challenger (+ (:agenda-point challenger) (:agenda-point contestant))))
                       :msg (msg (str "gain " (+ (:agenda-point challenger) (:agenda-point contestant)) " [Credits]"))}}}}}

   "Apocalypse"
   (let [contestant-discard {:async true
                     :effect (req (let [ai (all-placed state :contestant)
                                        onhost (filter #(= '(:onhost) (:zone %)) ai)
                                        unhosted (->> ai
                                                     (remove #(= '(:onhost) (:zone %)))
                                                     (sort-by #(vec (:zone %)))
                                                     (reverse))
                                        allcontestant (concat onhost unhosted)]
                                    (discard-cards state :challenger eid allcontestant)))}
         challenger-facedown {:effect (req (let [placedcards (all-active-placed state :challenger)
                                             ishosted (fn [c] (or (= ["onhost"] (get c :zone)) (= '(:onhost) (get c :zone))))
                                             hostedcards (filter ishosted placedcards)
                                             nonhostedcards (remove ishosted placedcards)]
                                         (doseq [oc hostedcards :let [c (get-card state oc)]]
                                           (flip-facedown state side c))
                                         (doseq [oc nonhostedcards :let [c (get-card state oc)]]
                                           (flip-facedown state side c))))}]
     {:req (req (and (some #{:hq} (:successful-run challenger-reg))
                     (some #{:rd} (:successful-run challenger-reg))
                     (some #{:archives} (:successful-run challenger-reg))))
      :async true
      ;; discard cards from right to left
      ;; otherwise, auto-killing locales would move the cards to the next locale
      ;; so they could no longer be discarded in the same loop
      :msg "discard all placed Contestant cards and turn all placed Challenger cards facedown"
      :effect (req (wait-for
                     (resolve-ability state side contestant-discard card nil)
                     (continue-ability state side challenger-facedown card nil)))})

   "Because I Can"
   (run-event
    {:choices (req (filter #(can-run-locale? state %) parties))}
    {:req (req (is-party? target))
     :replace-access {:msg "shuffle all cards in the locale into R&D"
                      :effect (req (doseq [c (:content run-locale)]
                                     (move state :contestant c :deck))
                                   (shuffle! state :contestant :deck))}})

   "Blackmail"
   (run-event
    {:req (req has-bad-pub)
     :msg "prevent Character from being revealed during this run"}
    nil
    (effect (register-run-flag!
              card
              :can-reveal
              (fn [state side card]
                (if (character? card)
                  ((constantly false)
                    (toast state :contestant "Cannot reveal Character on this run due to Blackmail"))
                  true)))))

   "Black Hat"
   {:trace {:base 4
            :unsuccessful {:effect (effect (register-events (:events (card-def card))
                                                            (assoc card :zone '(:discard))))}}
    :events {:pre-access {:req (req (#{:hq :rd} target))
                          :effect (effect (access-bonus 2))}
             :challenger-turn-ends {:effect (effect (unregister-events card))}}}

   "Bribery"
   {:prompt "How many credits?"
    :choices :credit
    :msg (msg "increase the reveal cost of the first unrevealed Character approached by " target " [Credits]")
    :effect (effect (resolve-ability (run-event) card nil))}

   "Brute-Force-Hack"
   {:implementation "Challenger must calculate the right number of credits including other game effects for the planned target Character"
    :prompt "How many [Credits]?" :choices :credit
    :effect (effect (system-msg (str "spends " target " [Credit] on Brute-Force-Hack"))
                    (resolve-ability {:choices {:req #(and (character? %)
                                                           (revealed? %)
                                                           (<= (:cost %) target))}
                                      :effect (effect (hide target))
                                      :msg (msg "hide " (:title target))} card nil))}

   "Build Script"
   {:msg "gain 1 [Credits] and draw 2 cards"
    :effect (effect (gain-credits 1) (draw 2))}

   "By Any Means"
   {:effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:challenger-turn-ends {:effect (effect (unregister-events card))}
             :access {:req (req (not= [:discard] (:zone target)))
                      :interactive (req true)
                      :async true
                      :msg (msg "discard " (:title target) " at no cost and suffer 1 meat damage")
                      :effect (req (wait-for (discard state side (assoc target :seen true) nil)
                                             (do (swap! state assoc-in [:challenger :register :discarded-card] true)
                                                 (damage state :challenger eid :meat 1 {:unboostable true}))))}}}

   "Calling in Favors"
   {:msg (msg "gain " (count (filter #(and (has-subtype? % "Connection") (is-type? % "Radicle"))
                                     (all-active-placed state :challenger))) " [Credits]")
    :effect (effect (gain-credits (count (filter #(and (has-subtype? % "Connection") (is-type? % "Radicle"))
                                                 (all-active-placed state :challenger)))))}

   "Career Fair"
   {:prompt "Select a radicle to place from your Grip"
    :choices {:req #(and (is-type? % "Radicle")
                         (in-hand? %))}
    :effect (effect (place-cost-bonus [:credit -3]) (challenger-place target))}

   "Careful Planning"
   {:prompt  "Choose a card in or protecting a party locale"
    :choices {:req #(is-party? (second (:zone %)))}
    :end-turn {:effect (effect (remove-icon card target))}
    :effect (effect (add-icon card target "CP" "red")
                    (system-msg (str "prevents the revealing of " (card-str state target)
                                     " for the rest of this turn via Careful Planning"))
                    (register-turn-flag! card :can-reveal
                                         (fn [state side card]
                                           (if (= (:cid card) (:cid target))
                                             ((constantly false)
                                               (toast state :contestant "Cannot reveal the rest of this turn due to Careful Planning"))
                                             true))))}

   "CBI Raid"
   (letfn [(cbi-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :async true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :contestant c :deck {:front true}))
                                 (clear-wait-prompt state :challenger)
                                 (effect-completed state side eid))
                             (continue-ability state side (cbi-choice original '() (count original) original)
                                               card nil)))})
           (cbi-choice [remaining chosen n original]
             {:prompt "Choose a card to move next onto R&D"
              :choices remaining
              :async true
              :effect (req (let [chosen (cons target chosen)]
                             (if (< (count chosen) n)
                               (continue-ability state side (cbi-choice (remove-once #(= target %) remaining)
                                                                        chosen n original) card nil)
                               (continue-ability state side (cbi-final chosen original) card nil))))})]
     {:req (req hq-runnable)
            :async true
            :effect (effect (run :hq {:replace-access
                                {:msg "force the Contestant to add all cards in HQ to the top of R&D"
                                 :async true
                                 :mandatory true
                                 :effect (req (show-wait-prompt state :challenger "Contestant to add all cards in HQ to the top of R&D")
                                              (let [from (:hand contestant)]
                                                (if (pos? (count from))
                                                  (continue-ability state :contestant (cbi-choice from '() (count from) from) card nil)
                                                  (do (clear-wait-prompt state :challenger)
                                                      (effect-completed state side eid)))))}} card))})

   "Code Siphon"
   {:req (req rd-runnable)
    :effect (effect (run :rd
                         {:replace-access
                          {:async true
                           :prompt "Choose a resource to place"
                           :msg (msg "place " (:title target) " and take 1 tag")
                           :choices (req (filter #(is-type? % "Resource") (:deck challenger)))
                           :effect (effect (trigger-event :searched-stack nil)
                                           (shuffle! :deck)
                                           (place-cost-bonus [:credit (* -3 (count (get-in contestant [:locales :rd :characters])))])
                                           (challenger-place target)
                                           (tag-challenger eid 1) )}} card))}

   "Cold Read"
   (let [end-effect {:prompt "Choose a resource that was used during the run to discard "
                     :choices {:req #(card-is? % :type "Resource")}
                     :msg (msg "discard " (:title target))
                     :effect (effect (discard target {:unpreventable true}))}]
     {:async true
      :prompt "Choose a locale"
      :recurring 4
      :choices (req runnable-locales)
      :effect (req (let [c (move state side (assoc card :zone '(:discard)) :play-area {:force true})]
                     (card-init state side c {:resolve-effect false})
                     (game.core/run state side (make-eid state) target
                                    {:end-run {:async true
                                               :effect (effect (discard c)
                                                               (continue-ability end-effect card nil))}}
                                    c)))})

   "Contaminate"
   {:effect (req (resolve-ability
                   state side
                   {:msg (msg "place 3 virus tokens on " (:title target))
                    :choices {:req #(and (placed? %)
                                         (= (:side %) "Challenger")
                                         (zero? (get-virus-counters state side %)))}
                    :effect (req (add-counter state :challenger target :virus 3))}
                   card nil))}

  "Contestantorate \"Grant\""
  {:events {:challenger-place {:silent (req true) ;; there are no current interactions where we'd want Grant to not be last, and this fixes a bug with Hayley
                             :req (req (first-event? state side :challenger-place))
                             :msg "force the Contestant to lose 1 [Credit]"
                             :effect (effect (lose-credits :contestant 1))}}}

   "Contestantorate Scandal"
   {:msg "give the Contestant 1 additional bad publicity"
    :implementation "No enforcement that this Bad Pub cannot be removed"
    :effect (req (swap! state update-in [:contestant :has-bad-pub] inc))
    :leave-play (req (swap! state update-in [:contestant :has-bad-pub] dec))}

   "Credit Crash"
   {:prompt "Choose a locale" :choices (req runnable-locales)
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:pre-access-card
             {:once :per-run
              :async true
              :req (req (not= (:type target) "Agenda"))
              :effect (req (let [c target
                                 cost (:cost c)
                                 title (:title c)]
                             (if (can-pay? state :contestant nil :credit cost)
                               (do (show-wait-prompt state :challenger "Contestant to decide whether or not to prevent the discard")
                                   (continue-ability state :contestant
                                     {:optional
                                      {:prompt (msg "Spend " cost " [Credits] to prevent the discard of " title "?")
                                       :player :contestant
                                       :yes-ability {:effect (req (lose-credits state :contestant cost)
                                                                  (system-msg state :contestant (str "spends " cost " [Credits] to prevent "
                                                                                               title " from being discarded at no cost"))
                                                                  (clear-wait-prompt state :challenger))}
                                       :no-ability {:msg (msg "discard " title " at no cost")
                                                    :async true
                                                    :effect (effect (clear-wait-prompt :challenger)
                                                                    (discard-no-cost eid c))}}}
                                    card nil))
                               (do (system-msg state side (str "uses Credit Crash to discard " title " at no cost"))
                                   (discard-no-cost state side eid c)))))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Credit Kiting"
   {:req (req (some #{:hq :rd :archives} (:successful-run challenger-reg)))
    :prompt "Select a card to place from your Grip"
    :choices {:req #(and (or (is-type? % "Hazard")
                             (is-type? % "Resource")
                             (is-type? % "Radicle"))
                         (in-hand? %))}
    :effect (effect (place-cost-bonus [:credit -8])
                    (challenger-place target)
                    (tag-challenger 1))}

   "Cyber Threat"
   {:prompt "Choose a locale"
    :choices (req runnable-locales)
    :async true
    :effect (req (let [serv target]
                   (continue-ability
                     state :contestant
                     {:optional
                      {:prompt (msg "Reveal a piece of Character protecting " serv "?")
                       :yes-ability {:prompt (msg "Select a piece of Character protecting " serv " to reveal")
                                     :player :contestant
                                     :choices {:req #(and (not (:revealed %))
                                                          (= (last (:zone %)) :characters))}
                                     :effect (req (reveal state :contestant target nil))}
                       :no-ability {:effect (effect (game.core/run eid serv nil card))
                                    :msg (msg "make a run on " serv " during which no Character can be revealed")}}}
                    card nil)))}

   "Data Breach"
   {:req (req rd-runnable)
    :async true
    :effect (req (let [db-eid (make-eid state)
                       events (:events (card-def card))]
                   (register-events state side
                                    (assoc-in events [:successful-run-ends :eid] db-eid)
                                    (assoc card :zone '(:discard)))
                   (wait-for (game.core/run state side db-eid :rd nil card)
                             (let [card (get-card state (assoc card :zone '(:discard)))]
                               (unregister-events state side card)
                               (when (:run-again card)
                                 (game.core/run state side db-eid :rd nil card))
                               (update! state side (dissoc card :run-again))))))
    :events {:successful-run-ends
             {:optional {:req (req (= [:rd] (:locale target)))
                         :prompt "Make another run on R&D?"
                         :yes-ability {:effect (effect (clear-wait-prompt :contestant)
                                                       (update! (assoc card :run-again true)))}}}}}

   "Day Job"
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]" :effect (effect (gain-credits 10))}

   "Déjà Vu"
   {:prompt "Choose a card to add to Grip" :choices (req (cancellable (:discard challenger) :sorted))
    :msg (msg "add " (:title target) " to their Grip")
    :effect (req (move state side target :hand)
                 (when (has-subtype? target "Virus")
                   (resolve-ability state side
                                    {:prompt "Choose a virus to add to Grip"
                                     :msg (msg "add " (:title target) " to their Grip")
                                     :choices (req (cancellable
                                                     (filter #(has-subtype? % "Virus") (:discard challenger)) :sorted))
                                     :effect (effect (move target :hand))} card nil)))}

   "Deep Data Mining"
   {:req (req rd-runnable)
    :effect (effect (run :rd nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus (max 0 (min 4 (available-mu state))))) }
             :run-ends {:effect (effect (unregister-events card))}}}

   "Demolition Run"
   {:req (req (or rd-runnable hq-runnable))
    :prompt "Choose a locale"
    :choices ["HQ" "R&D"]
    :effect (effect (run target nil card)
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard challenger)) :play-area)]
                                      (card-init state side c {:resolve-effect false})
                                      (register-events state side
                                                       {:run-ends {:effect (effect (discard c))}} c)))}
                     card nil))
    :events {:run-ends nil}
    :interactions {:discard-ability
                   {:label "[Demolition Run]: Discard card"
                    :msg (msg "discard " (:title target) " at no cost")
                    :async true
                    :effect (effect (discard-no-cost eid target))}}}

   "Deuces Wild"
   (let [all [{:effect (effect (gain-credits 3))
               :msg "gain 3 [Credits]"}
              {:effect (effect (draw 2))
               :msg "draw 2 cards"}
              {:effect (effect (lose :tag 1))
               :msg "remove 1 tag"}
              {:prompt "Select 1 piece of character to expose"
               :msg "expose 1 character and make a run"
               :choices {:req #(and (placed? %) (character? %))}
               :async true
               :effect (req (wait-for (expose state side target)
                                      (continue-ability
                                        state side
                                        {:prompt "Choose a locale"
                                         :choices (req runnable-locales)
                                         :async true
                                         :effect (effect (game.core/run eid target))}
                                        card nil)))}]
         choice (fn choice [abis]
                  {:prompt "Choose an ability to resolve"
                   :choices (map #(capitalize (:msg %)) abis)
                   :async true
                   :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                  (wait-for
                                    (resolve-ability state side chosen card nil)
                                    (if (= (count abis) 4)
                                      (continue-ability state side (choice (remove-once #(= % chosen) abis)) card nil)
                                      (effect-completed state side eid)))))})]
     {:async true
      :effect (effect (continue-ability (choice all) card nil))})

   "Compile"
   {:implementation "Trigger only on first encounter not enforced"
    :prompt "Choose a locale"
    :msg "make a run and place a resource on encounter with the first piece of Character"
    :choices (req runnable-locales)
    :async true
    :abilities [{:label "Place a resource using Compile"
                 :async true
                 :effect (effect (resolve-ability
                                   {:prompt "Place a resource from Stack or Heap?"
                                    :choices ["Stack" "Heap"]
                                    :msg (msg "place " (:title target))
                                    :effect (effect (resolve-ability
                                                      (let [chosen-source target]
                                                        {:prompt (str "Choose a resource in your " chosen-source " to place")
                                                         :choices (req (cancellable (filter #(is-type? % "Resource")
                                                                                            ((if (= chosen-source "Heap") :discard :deck) challenger))))
                                                         :effect (req (challenger-place state side (assoc-in target [:special :compile-placed] true) {:no-cost true})
                                                                      (when (= chosen-source "Stack")
                                                                        (shuffle! state :challenger :deck)))})
                                                      card nil))}
                                   card nil))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Compile in the Temporary Zone to place a Resource") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard challenger)) :play-area)]
                                         (card-init state side c {:resolve-effect false})))}
                      card nil))
    :events {:run-ends {:effect (req
                                 (let [compile-placed (first (filter #(get-in % [:special :compile-placed]) (game.core/all-placed state :challenger)))]
                                   (when (not (empty? compile-placed))
                                     (system-msg state side (str "moved " (:title compile-placed) " to the bottom of the Stack at the end of the run from Compile"))
                                     (move state :challenger compile-placed :deck)))
                                 (unregister-events state side card)
                                 (discard state side card))}}}

   "Dianas Hunt"
   {:implementation "One resource per encounter not enforced"
    :prompt "Choose a locale"
    :msg "make a run and place a resource on encounter with each Character"
    :choices (req runnable-locales)
    :async true
    :abilities [{:label "Place a resource using Diana's Hunt?"
                 :async true
                 :effect (effect (resolve-ability
                                   {:prompt "Choose a resource in your Grip to place"
                                    :choices {:req #(and (is-type? % "Resource")
                                                         (challenger-can-place? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "place " (:title target))
                                    :effect (req (let [diana-card (assoc-in target [:special :diana-placed] true)]
                                                   (challenger-place state side diana-card {:no-cost true})
                                                   (swap! state update :diana #(conj % diana-card))))}
                                   card nil))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Diana's Hunt in the Temporary Zone to place a Resource") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard challenger)) :play-area)]
                                      (card-init state side c {:resolve-effect false})
                                      (register-events state side
                                                       {:run-ends {:effect (req (let [hunt (:diana @state)]
                                                                                  (doseq [c hunt]
                                                                                    (let [placed (find-cid (:cid c) (all-placed state side))]
                                                                                      (when (get-in placed [:special :diana-placed])
                                                                                        (system-msg state side (str "discards " (:title c) " at the end of the run from Diana's Hunt"))
                                                                                        (discard state side placed {:unpreventable true}))))
                                                                                  (swap! state dissoc :diana)
                                                                                  (unregister-events state side card)
                                                                                  (discard state side c)))}} c)))}
                      card nil))
    :events {:run-ends nil}}

   "Diesel"
   {:msg "draw 3 cards" :effect (effect (draw 3))}

   "Dirty Laundry"
   (run-event
    {:end-run {:req (req (:successful run))
               :msg "gain 5 [Credits]"
               :effect (effect (gain-credits :challenger 5))}})

   "Diversion of Funds"
   {:req (req hq-runnable)
    :effect (effect (run :hq
                         {:req (req (= target :hq))
                          :replace-access
                          (let [five-or-all (fn [contestant] (min 5 (:credit contestant)))]
                            {:msg (msg "force the Contestant to lose " (five-or-all contestant)
                                       "[Credits], and gain " (five-or-all contestant) "[Credits]")
                             :effect (effect (lose-credits :contestant (five-or-all contestant))
                                             (gain-credits :challenger (five-or-all contestant)))})}
                      card))}

   "Drive By"
   {:choices {:req #(let [topmost (get-nested-host %)]
                     (and (is-party? (second (:zone topmost)))
                          (= (last (:zone topmost)) :content)
                          (not (:revealed %))))}
    :async true
    :effect (req (wait-for (expose state side target) ;; would be ncharacter if this could return a value on completion
                           (if async-result ;; expose was successful
                             (if (#{"Site" "Region"} (:type target))
                               (do (system-msg state :challenger (str "uses Drive By to discard " (:title target)))
                                   (discard state side (assoc target :seen true))
                                   (effect-completed state side eid))
                               (effect-completed state side eid))
                             (effect-completed state side eid))))}

   "Early Bird"
   (run-event
    {:msg (msg "make a run on " target " and gain [Click]")}
    nil
    (effect (gain :click 1)))

   "Easy Mark"
   {:msg "gain 3 [Credits]" :effect (effect (gain-credits 3))}

   "Embezzle"
   (letfn [(name-string [cards]
             (join " and " (map :title cards)))] ; either 'card' or 'card1 and card2'
    {:req (req hq-runnable)
     :effect (effect
              (run :hq {:req (req (= target :hq))
                        :replace-access
                        {:mandatory true
                         :msg (msg "reveal 2 cards from HQ and discard all "
                                   target (when (not= "Character" (:type target)) "s"))
                         :prompt "Choose a card type"
                         :choices ["Site" "Region" "Operation" "Character"]
                         :effect (req (let [chosen-type target
                                            cards-to-reveal (take 2 (shuffle (:hand contestant)))
                                            cards-to-discard (filter #(is-type? % chosen-type) cards-to-reveal)]
                                        (system-msg state side (str " reveals " (name-string cards-to-reveal) " from HQ"))
                                        (when-not (empty? cards-to-discard)
                                          (system-msg state side (str " discards " (name-string cards-to-discard)
                                                                      " from HQ and gain " (* 4 (count cards-to-discard)) "[Credits]"))
                                          (doseq [c cards-to-discard]
                                            (discard state :challenger (assoc c :seen true)))
                                          (gain-credits state :challenger (* 4 (count cards-to-discard))))))}}
                card))})

   "Emergency Shutdown"
   {:req (req (some #{:hq} (:successful-run challenger-reg)))
    :msg (msg "hide " (:title target))
    :choices {:req #(and (character? %)
                         (revealed? %))}
    :effect (effect (hide target))}

   "Emergent Creativity"
   (letfn [(ec [discard-cost to-discard]
             {:async true
             :prompt "Choose a hazard or resource to place"
             :msg (msg "discard " (if (empty? to-discard) "no cards" (join ", " (map :title to-discard)))
                       " and place " (:title target) " lowering the cost by " discard-cost)
             :choices (req (cancellable (filter #(or (is-type? % "Resource")
                                                     (is-type? % "Hazard"))
                                                (:deck challenger)) :sorted))
             :effect (req (trigger-event state side :searched-stack nil)
                          (shuffle! state side :deck)
                          (doseq [c to-discard]
                            (discard state side c {:unpreventable true}))
                          (place-cost-bonus state side [:credit (- discard-cost)])
                          (challenger-place state side target)
                          (effect-completed state side eid))})]
   {:prompt "Choose Hazard and Resources to discard from your Grip"
    :choices {:req #(and (or (is-type? % "Hazard")
                             (is-type? % "Resource"))
                         (in-hand? %))
              :max (req (count (:hand challenger)))}
    :cancel-effect (effect (resolve-ability (ec 0 []) card nil))
    :effect (req (let [discard-cost (apply + (map :cost targets))
                       to-discard targets]
                   (resolve-ability state side (ec discard-cost to-discard) card nil)))})

   "Employee Strike"
   {:msg "disable the Contestant's identity"
    :disable-id true
    :effect (effect (disable-identity :contestant))
    :leave-play (effect (enable-identity :contestant))}

   "Encore"
   {:req (req (and (some #{:hq} (:successful-run challenger-reg))
                   (some #{:rd} (:successful-run challenger-reg))
                   (some #{:archives} (:successful-run challenger-reg))))
    :effect (req (swap! state update-in [:challenger :extra-turns] (fnil inc 0))
                 (move state side (first (:play-area challenger)) :rfg))
    :msg "take an additional turn after this one"}

   "En Passant"
   {:req (req (:successful-run challenger-reg))
    :effect (req (let [runtgt (first (flatten (turn-events state side :run)))
                       serv (zone->name runtgt)]
                   (resolve-ability state side
                     {:prompt (msg "Choose an unrevealed piece of Character protecting " serv " that you passed on your last run")
                      :choices {:req #(and (character? %)
                                           (not (revealed? %)))}
                      :msg (msg "discard " (card-str state target))
                      :effect (req (discard state side target)
                                   (swap! state assoc-in [:challenger :register :discarded-card] true))}
                    card nil)))}

   "Escher"
   (letfn [(es [] {:prompt "Select two pieces of Character to swap positions"
                   :choices {:req #(and (placed? %) (character? %)) :max 2}
                   :effect (req (if (= (count targets) 2)
                                  (do (swap-character state side (first targets) (second targets))
                                      (resolve-ability state side (es) card nil))
                                  (system-msg state side "has finished rearranging Character")))})]
     {:req (req hq-runnable)
            :effect (effect (run :hq {:replace-access
                                {:msg "rearrange placed Character"
                                 :effect (effect (resolve-ability (es) card nil))}} card))})

   "Eureka!"
   {:effect (req (let [topcard (first (:deck challenger))
                       caninst (or (is-type? topcard "Hazard")
                                   (is-type? topcard "Resource")
                                   (is-type? topcard "Radicle"))]
                   (if caninst
                     (resolve-ability
                       state side
                       {:optional {:prompt (msg "Place " (:title topcard) "?")
                                   :yes-ability {:effect (effect (place-cost-bonus [:credit -10])
                                                                 (challenger-place topcard))}
                                   :no-ability {:effect (effect (discard topcard {:unpreventable true})
                                                                (system-msg (str "reveals and discards "
                                                                                 (:title topcard))))}}} card nil)
                     (do (discard state side topcard {:unpreventable true})
                         (system-msg state side (str "reveals and discards " (:title topcard)))))))}

   "Exclusive Party"
   {:msg (msg "draw 1 card and gain "
              (count (filter #(= (:title %) "Exclusive Party") (:discard challenger)))
              " [Credits]")
    :effect (effect (draw) (gain-credits (count (filter #(= (:title %) "Exclusive Party") (:discard challenger)))))}

   "Executive Wiretaps"
   {:msg (msg "reveal cards in HQ: " (join ", " (map :title (:hand contestant))))}

   "Exploit"
   {:req (req (and (some #{:hq} (:successful-run challenger-reg))
                   (some #{:rd} (:successful-run challenger-reg))
                   (some #{:archives} (:successful-run challenger-reg))))
    :prompt "Choose up to 3 pieces of Character to hide"
    :choices {:max 3 :req #(and (revealed? %) (character? %))}
    :msg (msg "hide " (join ", " (map :title targets)))
    :effect (req (doseq [c targets]
                   (hide state side c)))}

   "Exploratory Romp"
   (run-event
     {:replace-access {:prompt "Advancements to remove from a card in or protecting this locale?"
                       :choices ["0", "1", "2", "3"]
                       :async true
                       :effect (req (let [c (str->int target)]
                                      (show-wait-prompt state :contestant "Challenger to remove advancements")
                                      (continue-ability state side
                                        {:choices {:req #(and (contains? % :advance-counter)
                                                              (= (first (:locale run)) (second (:zone %))))}
                                         :msg (msg "remove " (quantify c "advancement token")
                                                   " from " (card-str state target))
                                         :effect (req (let [to-remove (min c (get-counters target :advancement))]
                                                        (add-prop state :contestant target :advance-counter (- to-remove))
                                                        (clear-wait-prompt state :contestant)
                                                        (effect-completed state side eid)))}
                                        card nil)))}})

   "Express Delivery"
   {:prompt "Choose a card to add to your Grip" :choices (req (take 4 (:deck challenger)))
    :msg "look at the top 4 cards of their Stack and add 1 of them to their Grip"
    :effect (effect (move target :hand) (shuffle! :deck))}

   "Falsified Credentials"
   {:prompt "Choose a type"
    :choices ["Agenda" "Site" "Region"]
    :msg (msg "to guess " target)
    :async true
    :effect (effect
             (continue-ability
              (let [chosen-type target]
                {:choices {:req #(let [topmost (get-nested-host %)]
                                   (and (is-party? (second (:zone topmost)))
                                        (= (last (:zone topmost)) :content)
                                        (not (revealed? %))))}
                 :async true
                 :effect (req             ;taken from Drive By - maybe refactor
                           (wait-for (expose state side target)
                                     (if (and async-result ;; expose was successful
                                              (= chosen-type (:type target)))
                                       (continue-ability
                                         state :challenger
                                         {:effect (effect (gain-credits 5))
                                          :msg "gain 5 [Credits] "}
                                         card nil)
                                       (effect-completed state side eid))))})
              card nil))}


   "Fear the Masses"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:async true
                               :mandatory true
                               :msg "force the Contestant to discard the top card of R&D"
                               :effect (req (mill state :contestant)
                                            (let [n (count (filter #(= (:title card) (:title %)) (:hand challenger)))]
                                              (if (pos? n)
                                                (continue-ability state side
                                                  {:prompt "Reveal how many copies of Fear the Masses?"
                                                   :choices {:number (req n)}
                                                   :effect (req (when (pos? target)
                                                                  (mill state :contestant target)
                                                                  (system-msg state side
                                                                              (str "reveals " target " copies of Fear the Masses,"
                                                                                   " forcing the Contestant to discard " target " cards"
                                                                                   " from the top of R&D"))))}
                                                 card nil)
                                                (effect-completed state side eid))))}} card))}

   "Feint"
   {:req (req hq-runnable)
    :implementation "Bypass is manual"
    :effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    ;; Don't need a msg since game will print that card access is prevented
    :events {:successful-run {:effect (effect (prevent-access))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Fisk Investment Seminar"
   {:msg "make each player draw 3 cards"
    :effect (effect (draw 3) (draw :contestant 3))}

   "Forged Activation Orders"
   {:choices {:req #(and (character? %)
                         (not (revealed? %)))}
    :effect (req (let [character target
                       serv (zone->name (second (:zone character)))
                       characterpos (character-index state character)]
                   (resolve-ability
                     state :contestant
                     {:prompt (msg "Reveal " (:title character) " at position " characterpos
                                   " of " serv " or discard it?") :choices ["Reveal" "Discard"]
                      :effect (effect (resolve-ability
                                        (if (and (= target "Reveal") (<= (reveal-cost state :contestant character) (:credit contestant)))
                                          {:msg (msg "force the reveal of " (:title character))
                                           :effect (effect (reveal :contestant character))}
                                          {:msg (msg "discard the Character at position " characterpos " of " serv)
                                           :effect (effect (discard :contestant character))})
                                        card nil))}
                     card nil)))}

   "Forked"
   {:implementation "Character discard is manual"
    :prompt "Choose a locale"
    :choices (req runnable-locales)
    :effect (effect (run target nil card))}

   "Frame Job"
   {:prompt "Choose an agenda to forfeit"
    :choices (req (:scored challenger))
    :effect (effect (forfeit target)
                    (gain-bad-publicity :contestant 1))
    :msg (msg "forfeit " (:title target) " and give the Contestant 1 bad publicity")}

   "Frantic Coding"
   {:async true
    :events {:challenger-shuffle-deck nil}
    :effect
    (req (let [topten (take 10 (:deck challenger))]
           (prompt! state :challenger card (str "The top 10 cards of the Stack are "
                                            (join ", " (map :title topten))) ["OK"] {})
           (continue-ability
             state side
             {:prompt "Place a resource?"
              :choices (conj (vec (sort-by :title (filter #(and (is-type? % "Resource")
                                                                (can-pay? state side nil
                                                                          (modified-place-cost state side % [:credit -5])))
                                                          topten))) "No place")
              :async true
              :effect (req (if (not= target "No place")
                             (do (register-events state side
                                                  {:challenger-shuffle-deck
                                                   {:effect (effect (update! (assoc card :shuffle-occurred true)))}}
                                                  (assoc card :zone '(:discard)))
                                 (place-cost-bonus state side [:credit -5])
                                 (let [to-discard (remove #(= (:cid %) (:cid target)) topten)]
                                   (wait-for (challenger-place state side target nil)
                                             (let [card (get-card state (assoc card :zone '(:discard)))]
                                               (if (not (:shuffle-occurred card))
                                                 (do (system-msg state side (str "discards " (join ", " (map :title to-discard))))
                                                     (doseq [c to-discard] (discard state side c {:unpreventable true}))
                                                     (effect-completed state side eid))
                                                 (do (system-msg state side "does not have to discard cards because the stack was shuffled")
                                                     (effect-completed state side eid)))))))
                             (do (doseq [c topten] (discard state side c {:unpreventable true}))
                                 (system-msg state side (str "discards " (join ", " (map :title topten)))))))} card nil)))}

   "\"Freedom Through Equality\""
   {:events {:agenda-stolen {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :async true
                             :effect (req (as-agenda state :challenger eid card 1))}}}

   "Freelance Coding Contract"
   {:choices {:max 5
              :req #(and (is-type? % "Resource")
                         (in-hand? %))}
    :msg (msg "discard " (join ", " (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :effect (req (doseq [c targets]
                   (discard state side c {:unpreventable true}))
                 (gain-credits state side (* 2 (count targets))))}

   "Game Day"
   {:msg (msg "draw " (- (hand-size state :challenger) (count (:hand challenger))) " cards")
    :effect (effect (draw (- (hand-size state :challenger) (count (:hand challenger)))))}

  "Glut Cipher"
  (let [contestant-choose {:show-discard true
                     :async true
                     :player :contestant
                     :prompt (msg "Select 5 cards from Archives to add to HQ")
                     :choices {:max 5
                               :all true
                               :req #(and (= (:side %) "Contestant")
                                          (= (:zone %) [:discard]))}
                     :msg (msg "move "
                               (let [seen (filter :seen targets)
                                     m (count  (remove :seen targets))]
                                 (str (join ", " (map :title seen))
                                      (when (pos? m)
                                        (str (when-not (empty? seen) " and ")
                                             (quantify m "unseen card")))
                                      " into HQ, then discard 5 cards")))
                     :effect (req (wait-for
                                    (resolve-ability state side
                                                     {:effect (req (doseq [c targets]
                                                                     (move state side c :hand)))}
                                                     card targets)
                                    (continue-ability state side
                                                      {:async true
                                                       :effect (req (doseq [c (take 5 (shuffle (:hand contestant)))]
                                                                      (discard state :contestant c))
                                                                    (clear-wait-prompt state :challenger)
                                                                    (effect-completed state :challenger eid))}
                                                      card nil)))}
        access-effect {:mandatory true
                       :async true
                       :req (req (>= (count (:discard contestant)) 5))
                       :effect (req (show-wait-prompt
                                      state :challenger
                                      "Contestant to choose which cards to pick up from Archives") ;; For some reason it just shows successful-run-trigger-message, but this works!?
                                    (continue-ability state side
                                                      contestant-choose
                                                      card nil))}]
    {:req (req archives-runnable)
     :makes-run true
     :effect (effect (run :archives
                          {:req (req (= target :archives))
                           :replace-access access-effect}
                          card))})

   "Government Investigations"
   {:flags {:psi-prevent-spend (req 2)}}

   "Hacktivist Meeting"
   {:implementation "Does not prevent reveal if HQ is empty"
    :events {:reveal {:req (req (and (not (character? target))
                                  (pos? (count (:hand contestant)))))
                   ;; FIXME the above condition is just a bandaid, proper fix would be preventing the reveal altogether
                   :msg "force the Contestant to discard 1 card from HQ at random"
                   :effect (effect (discard (first (shuffle (:hand contestant)))))}}}

   "High-Stakes Job"
   (run-event
    {:choices (req (let [unrevealed-character #(seq (filter (complement revealed?) (:characters (second %))))
                         bad-zones (keys (filter (complement unrevealed-character) (get-in @state [:contestant :locales])))]
                     (zones->sorted-names (remove (set bad-zones) (get-runnable-zones @state)))))}
    {:end-run {:req (req (:successful run))
               :msg "gain 12 [Credits]"
               :effect (effect (gain-credits :challenger 12))}})

   "Hostage"
   {:prompt "Choose a Connection"
    :choices (req (cancellable (filter #(has-subtype? % "Connection") (:deck challenger)) :sorted))
    :msg (msg "add " (:title target) " to their Grip and shuffle their Stack")
    :effect (req (let [connection target]
                   (trigger-event state side :searched-stack nil)
                   (resolve-ability
                     state side
                     {:prompt (str "Place " (:title connection) "?")
                      :choices ["Yes" "No"]
                      :effect (req (let [d target]
                                     (resolve-ability state side
                                       {:effect (req (shuffle! state side :deck)
                                                     (if (= "Yes" d)
                                                       (challenger-place state side connection)
                                                       (move state side connection :hand)))} card nil)))}
                     card nil)))}

   "Ive Had Worse"
   {:effect (effect (draw 3))
    :discard-effect {:when-inactive true
                   :req (req (#{:meat :net} target))
                   :effect (effect (draw :challenger 3)) :msg "draw 3 cards"}}

   "Immolation Script"
   {:req (req archives-runnable)
    :effect (effect (run :archives nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:pre-access
             {:async true
              :req (req (and (= target :archives)
                             ;; don't prompt unless there's at least 1 revealed Character matching one in Archives
                             (not-empty (clojure.set/intersection
                                          (into #{} (map :title (filter #(character? %) (:discard contestant))))
                                          (into #{} (map :title (filter #(revealed? %) (all-placed state :contestant))))))))
              :effect (req (continue-ability state side
                             {:async true
                              :prompt "Choose a piece of Character in Archives"
                              :choices (req (filter character? (:discard contestant)))
                              :effect (req (let [charactername (:title target)]
                                             (continue-ability state side
                                               {:async true
                                                :prompt (msg "Select a revealed copy of " charactername " to discard")
                                                :choices {:req #(and (character? %)
                                                                     (revealed? %)
                                                                     (= (:title %) charactername))}
                                                :msg (msg "discard " (card-str state target))
                                                :effect (req (discard state :contestant target)
                                                             (unregister-events state side card)
                                                             (effect-completed state side eid))} card nil)))}
                            card nil))}}}

   "Independent Thinking"
   (letfn [(cards-to-draw [targets]
             (* (count targets)
                (if (some #(and (not (facedown? %)) (has-subtype? % "Directive")) targets) 2 1)))]
     {:async true
      :prompt "Choose up to 5 placed cards to discard with Independent Thinking"
      :choices {:max 5
                :req #(and (placed? %)
                           (= (:side %) "Challenger"))}
      :effect (req (wait-for (discard-cards state side targets nil)
                             (draw state :challenger eid (cards-to-draw targets) nil)))
      :msg (msg "discard " (join ", " (map :title targets)) " and draw " (quantify (cards-to-draw targets) "card"))})

   "Indexing"
   {:req (req rd-runnable)
    :async true
    :effect (effect (run :rd
                         {:req (req (= target :rd))
                          :replace-access
                          {:msg "rearrange the top 5 cards of R&D"
                           :async true
                           :effect (req (show-wait-prompt state :contestant "Challenger to rearrange the top cards of R&D")
                                        (let [from (take 5 (:deck contestant))]
                                          (if (pos? (count from))
                                            (continue-ability state side (reorder-choice :contestant :contestant from '()
                                                                                         (count from) from) card nil)
                                            (do (clear-wait-prompt state :contestant)
                                                (effect-completed state side eid)))))}} card))}

   "Infiltration"
   {:prompt "Gain 2 [Credits] or expose a card?" :choices ["Gain 2 [Credits]" "Expose a card"]
    :effect (effect (continue-ability (if (= target "Expose a card")
                                        {:choices {:req placed?}
                                         :async true
                                         :effect (effect (expose eid target))}
                                         {:msg "gain 2 [Credits]" :effect (effect (gain-credits 2))})
                                      card nil))}

   "Information Sifting"
   (letfn [(access-pile [cards pile pile-size]
             {:prompt "Choose a card to access. You must access all cards."
              :choices [(str "Card from pile " pile)]
              :async true
              :effect (req (wait-for
                             (access-card state side (first cards))
                             (if (< 1 (count cards))
                               (continue-ability state side (access-pile (next cards) pile pile-size) card nil)
                               (do (swap! state assoc-in [:run :cards-accessed] pile-size)
                                   (effect-completed state side eid)))))})
           (which-pile [p1 p2]
             {:prompt "Choose a pile to access"
              :choices [(str "Pile 1 (" (count p1) " cards)") (str "Pile 2 (" (count p2) " cards)")]
              :async true
              :effect (req (let [choice (if (.startsWith target "Pile 1") 1 2)]
                             (clear-wait-prompt state :contestant)
                             (system-msg state side (str "chooses to access " target))
                             (continue-ability state side
                                (access-pile (if (= 1 choice) p1 p2) choice (count (if (= 1 choice) p1 p2)))
                                card nil)))})]
     (let [access-effect
           {:async true
            :mandatory true
            :effect (req (if (< 1 (count (:hand contestant)))
                           (do (show-wait-prompt state :challenger "Contestant to create two piles")
                               (continue-ability
                                 state :contestant
                                 {:async true
                                  :prompt (msg "Select up to " (dec (count (:hand contestant))) " cards for the first pile")
                                  :choices {:req #(and (in-hand? %) (card-is? % :side :contestant))
                                            :max (req (dec (count (:hand contestant))))}
                                  :effect (effect (clear-wait-prompt :challenger)
                                                  (show-wait-prompt :contestant "Challenger to select a pile")
                                                  (continue-ability
                                                    :challenger
                                                    (which-pile (shuffle targets)
                                                                (shuffle (vec (clojure.set/difference
                                                                                (set (:hand contestant)) (set targets)))))
                                                    card nil))
                                  } card nil))
                           (effect-completed state side eid)))}]
       {:req (req hq-runnable)
        :effect (effect (run :hq {:req (req (= target :hq))
                                  :replace-access access-effect}
                             card))}))

   "Inject"
   {:effect (req (doseq [c (take 4 (get-in @state [:challenger :deck]))]
                   (if (is-type? c "Resource")
                     (do (discard state side c {:unpreventable true})
                         (gain-credits state side 1)
                         (system-msg state side (str "discards " (:title c) " and gains 1 [Credits]")))
                     (do (move state side c :hand)
                         (system-msg state side (str "adds " (:title c) " to Grip"))))))}

   "Injection Attack"
   (run-event
    {:async true}
    nil
    nil
    (effect (continue-ability
             {:prompt "Select an characterbreaker"
              :choices {:req #(and (placed? %) (has-subtype? % "Icebreaker"))}
              :effect (effect (pump target 2 :all-run))}
             card nil)))

   "Inside Job"
   {:implementation "Bypass is manual"
    :prompt "Choose a locale"
    :choices (req runnable-locales)
    :effect (effect (run target nil card))}

   "Interdiction"
   (let [ab (effect (register-turn-flag!
                     card :can-reveal
                     (fn [state side card]
                       (if (and (= (:active-player @state) :challenger) (not (character? card)))
                         ((constantly false)
                          (toast state :contestant "Cannot reveal non-Character on the Challenger's turn due to Interdiction"))
                         true))))]
     {:msg "prevent the Contestant from revealing non-Character cards on the Challenger's turn"
      :effect ab
      :events {:challenger-turn-begins {:effect ab}}
      :leave-play (req (clear-all-flags-for-card! state side card))})

   "Itinerant Protesters"
   {:msg "reduce the Contestant's maximum hand size by 1 for each bad publicity"
    :effect (req (lose state :contestant :hand-size {:mod  (:bad-publicity contestant)})
                 (add-watch state :itin
                   (fn [k ref old new]
                     (let [bpnew (get-in new [:contestant :bad-publicity])
                           bpold (get-in old [:contestant :bad-publicity])]
                       (when (> bpnew bpold)
                         (lose state :contestant :hand-size {:mod (- bpnew bpold)}))
                       (when (< bpnew bpold)
                         (gain state :contestant :hand-size {:mod (- bpold bpnew)}))))))
    :leave-play (req (remove-watch state :itin)
                     (gain state :contestant :hand-size {:mod (:bad-publicity contestant)}))}

   "Knifed"
   {:implementation "Character discard is manual"
    :prompt "Choose a locale"
    :choices (req runnable-locales)
    :effect (effect (run target nil card))}

   "Kraken"
   {:req (req (:stole-agenda challenger-reg)) :prompt "Choose a locale" :choices (req locales)
    :msg (msg "force the Contestant to discard an Character protecting " target)
    :effect (req (let [serv (next (locale->zone state target))
                       servname target]
                   (resolve-ability
                     state :contestant
                     {:prompt (msg "Select a piece of Character in " target " to discard")
                      :choices {:req #(and (= (last (:zone %)) :characters)
                                           (= serv (rest (butlast (:zone %)))))}
                      :effect (req (discard state :contestant target)
                                   (system-msg state side (str "discards "
                                    (card-str state target))))}
                    card nil)))}

   "Lawyer Up"
   {:msg "remove 2 tags and draw 3 cards"
    :effect (effect (draw 3) (lose :tag 2))}

   "Lean and Mean"
   {:prompt "Choose a locale"
    :choices (req runnable-locales)
    :async true
    :msg (msg "make a run on " target (when (< (count (filter #(is-type? % "Resource") (all-active-placed state :challenger))) 4)
                                        ", adding +2 strength to all characterbreakers"))
    :effect (req (when (< (count (filter #(is-type? % "Resource") (all-active-placed state :challenger))) 4)
                   (doseq [c (filter #(has-subtype? % "Icebreaker") (all-active-placed state :challenger))]
                     (pump state side c 2 :all-run)))
                 (game.core/run state side (make-eid state) target nil card))}

   "Leave No Trace"
   (letfn [(get-revealed-cids [character]
             (map :cid (filter #(and (revealed? %) (is-type? % "Character")) character)))]
     {:prompt "Choose a locale"
      :msg "make a run and hide any Character that are revealed during this run"
      :choices (req runnable-locales)
      :async true
      :effect (req
                (let [old-character-cids (get-revealed-cids (all-placed state :contestant))]
                  (swap! state assoc :lnt old-character-cids)
                  (register-events state side (:events (card-def card)) (assoc card :zone '(:discard)))
                  (game.core/run state side (make-eid state) target nil card)))
      :events {:run-ends {:effect (req (let [new (set (get-revealed-cids (all-placed state :contestant)))
                                             old (set (:lnt @state))
                                             diff-cid (seq (clojure.set/difference new old))
                                             diff (map #(find-cid % (all-placed state :contestant)) diff-cid)]
                                         (doseq [character diff]
                                           (hide state side character))
                                         (when-not (empty? diff)
                                           (system-msg state side (str "hides " (join ", " (map :title diff)) " via Leave No Trace")))
                                         (swap! state dissoc :lnt)
                                         (unregister-events state side card)))}}})

   "Legwork"
   {:req (req hq-runnable)
    :effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Leverage"
   {:req (req (some #{:hq} (:successful-run challenger-reg)))
    :player :contestant
    :prompt "Take 2 bad publicity?"
    :choices ["Yes" "No"]
    :effect (req (if (= target "Yes")
                   (do (gain-bad-publicity state :contestant 2)
                       (system-msg state :contestant "takes 2 bad publicity"))
                   (do (register-events state side
                                        {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                      (damage-prevent :meat Integer/MAX_VALUE)
                                                                      (damage-prevent :brain Integer/MAX_VALUE))}
                                         :challenger-turn-begins {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard)))
                       (system-msg state :challenger "is immune to damage until the beginning of the Challenger's next turn"))))
    ; This :events is a hack so that the unregister-events above will fire.
    :events {:challenger-turn-begins nil :pre-damage nil}}

   "Levy AR Lab Access"
   {:msg "shuffle their Grip and Heap into their Stack and draw 5 cards"
    :effect (effect (shuffle-into-deck :hand :discard) (draw 5)
                    (move (first (:play-area challenger)) :rfg))}

   "Lucky Find"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain-credits 9))}

   "Mad Dash"
   {:prompt "Choose a locale"
    :choices (req runnable-locales)
    :async true
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:agenda-stolen {:silent (req true)
                             :effect (effect (update! (assoc card :steal true)))}
             :run-ends {:async true
                        :effect (req (if (:steal card)
                                       (wait-for (as-agenda state :challenger (get-card state card) 1)
                                                 (system-msg state :challenger
                                                             (str "adds Mad Dash to their score area as an agenda worth 1 agenda point")))
                                       (do (system-msg state :challenger
                                                       (str "suffers 1 meat damage from Mad Dash"))
                                           (damage state side eid :meat 1 {:card card})))
                                     (unregister-events state side card))}}}

   "Making an Entrance"
   (letfn [(entrance-discard [cards]
             {:prompt "Choose a card to discard"
              :choices (cons "None" cards)
              :async true
              :msg (req (when (not= target "None") (str "discard " (:title target))))
              :effect (req (if (= target "None")
                             (if (not-empty cards)
                               (continue-ability state side (reorder-choice :challenger :contestant cards '()
                                                                            (count cards) cards) card nil)
                               (do (clear-wait-prompt state :contestant)
                                   (effect-completed state side eid)))
                             (do (discard state side target {:unpreventable true})
                                 (continue-ability state side (entrance-discard (remove-once #(= % target) cards))
                                                   card nil))))})]
     {:msg "look at and discard or rearrange the top 6 cards of their Stack"
      :async true
      :effect (req (show-wait-prompt state :contestant "Challenger to rearrange the top cards of their stack")
                   (let [from (take 6 (:deck challenger))]
                     (continue-ability state side (entrance-discard from) card nil)))})

   "Marathon"
   (run-event
     {:choices (req (filter #(can-run-locale? state %) parties))}
     {:end-run {:effect (req (prevent-run-on-locale state card (:locale run))
                             (when (:successful run)
                               (system-msg state :challenger "gains 1 [Click] and adds Marathon to their grip")
                               (gain state :challenger :click 1)
                               (move state :challenger (assoc card :zone [:discard]) :hand)))}})


   "Mars for Martians"
   {:msg (msg "draw " (count (filter #(and (has-subtype? % "Clan") (is-type? % "Radicle"))
                                     (all-active-placed state :challenger)))
              " cards and gain " (:tag challenger) " [Credits]")
    :effect (effect (draw (count (filter #(and (has-subtype? % "Clan") (is-type? % "Radicle"))
                                         (all-active-placed state :challenger))))
                    (gain-credits (:tag challenger)))}

   "Mass Place"
   (let [mhelper (fn mi [n] {:prompt "Select a resource to place"
                             :choices {:req #(and (is-type? % "Resource")
                                                  (in-hand? %))}
                             :effect (req (challenger-place state side target)
                                            (when (< n 3)
                                              (resolve-ability state side (mi (inc n)) card nil)))})]
     {:effect (effect (resolve-ability (mhelper 1) card nil))})

   "Mining Accident"
   (letfn [(mining [] {:player :contestant
                       :async true
                       :prompt "Pay 5 [Credits] or take 1 Bad Publicity?"
                       :choices ["Pay 5 [Credits]" "Take 1 Bad Publicity"]
                       :effect (req (cond

                                      (and (= target "Pay 5 [Credits]") (can-pay? state :contestant nil :credit 5))
                                      (do (lose-credits state :contestant 5)
                                          (system-msg state side "pays 5 [Credits] from Mining Accident")
                                          (clear-wait-prompt state :challenger)
                                          (effect-completed state side eid))

                                      (= target "Pay 5 [Credits]")
                                      (do (can-pay? state :contestant "Mining Accident" :credit 5)
                                          (continue-ability state side (mining) card nil))

                                      (= target "Take 1 Bad Publicity")
                                      (do (gain-bad-publicity state :contestant 1)
                                          (system-msg state side "takes 1 bad publicity from Mining Accident")
                                          (clear-wait-prompt state :challenger)
                                          (effect-completed state side eid))))})]
   {:req (req (some #{:hq :rd :archives} (:successful-run challenger-reg)))
    :async true
    :effect (req (move state side (first (:play-area challenger)) :rfg)
                 (show-wait-prompt state :challenger "Contestant to choose to pay or take bad publicity")
                 (continue-ability state side (mining) card nil))
    :msg "make the Contestant pay 5 [Credits] or take 1 bad publicity"})

   "Möbius"
   {:req (req rd-runnable)
    :async true
    :effect (req (let [mob-eid (make-eid state)
                       events (:events (card-def card))]
                   (register-events state side
                                    (assoc-in events [:successful-run-ends :eid] mob-eid)
                                    (assoc card :zone '(:discard)))
                   (wait-for (game.core/run state side mob-eid :rd nil card)
                             (let [card (get-card state (assoc card :zone '(:discard)))]
                               (unregister-events state side card)
                               (when (:run-again card)
                                 (game.core/run state side mob-eid :rd nil card)
                                 (register-events state side {:successful-run
                                                              {:req (req (= target :rd))
                                                               :msg "gain 4 [Credits]"
                                                               :effect (effect (gain-credits 4)
                                                                               (unregister-events card))}}

                                               (assoc card :zone '(:discard))))
                            (update! state side (dissoc card :run-again))))))
    :events {:successful-run nil
             :successful-run-ends {:interactive (req true)
                                   :optional {:req (req (= [:rd] (:locale target)))
                                              :prompt "Make another run on R&D?"
                                              :yes-ability {:effect (effect (clear-wait-prompt :contestant)
                                                                            (update! (assoc card :run-again true)))}}}}}

   "Modded"
   {:prompt "Select a resource or piece of hazard to place from your Grip"
    :choices {:req #(and (or (is-type? % "Hazard")
                             (is-type? % "Resource"))
                         (in-hand? %))}
    :effect (effect (place-cost-bonus [:credit -3]) (challenger-place target))}

   "Net Celebrity"
   {:recurring 1}

   "Networking"
   {:msg "remove 1 tag"
    :effect (effect (lose :tag 1))
    :optional {:prompt "Pay 1 [Credits] to add Networking to Grip?"
               :yes-ability {:cost [:credit 1]
                             :msg "add it to their Grip"
                             :effect (effect (move (last (:discard challenger)) :hand))}}}

   "Notoriety"
   {:req (req (and (some #{:hq} (:successful-run challenger-reg))
                   (some #{:rd} (:successful-run challenger-reg))
                   (some #{:archives} (:successful-run challenger-reg))))
    :async true
    :effect (req (as-agenda state :challenger eid (first (:play-area challenger)) 1))
    :msg "add it to their score area as an agenda worth 1 agenda point"}

   "On the Lam"
   {:req (req (some #(is-type? % "Radicle") (all-active-placed state :challenger)))
    :prompt "Choose a radicle to host On the Lam"
    :choices {:req #(and (is-type? % "Radicle")
                         (placed? %))}
    :effect (effect (host target (assoc card :zone [:discard] :placed true))
                    (system-msg (str "hosts On the Lam on " (:title target))))
    :interactions {:prevent [{:type #{:net :brain :meat :tag}
                              :req (req true)}]}
    :abilities [{:label "[Discard]: Avoid 3 tags"
                 :msg "avoid up to 3 tags"
                 :effect (effect (tag-prevent 3)
                                 (discard card {:cause :ability-cost}))}
                {:label "[Discard]: Prevent up to 3 damage"
                 :msg "prevent up to 3 damage"
                 :effect (effect (damage-prevent :net 3)
                                 (damage-prevent :meat 3)
                                 (damage-prevent :brain 3)
                                 (discard card {:cause :ability-cost}))}]}

   "Out of the Ashes"
   (let [ashes-run {:prompt "Choose a locale"
                    :choices (req runnable-locales)
                    :async true
                    :effect (effect (run eid target nil card))}
         ashes-recur (fn ashes-recur [n]
                       {:prompt "Remove Out of the Ashes from the game to make a run?"
                        :choices ["Yes" "No"]
                        :effect (req (if (= target "Yes")
                                       (let [card (some #(when (= "Out of the Ashes" (:title %)) %) (:discard challenger))]
                                         (system-msg state side "removes Out of the Ashes from the game to make a run")
                                         (move state side card :rfg)
                                         (unregister-events state side card)
                                         (wait-for (resolve-ability state side ashes-run card nil)
                                                   (if (< 1 n)
                                                     (continue-ability state side (ashes-recur (dec n)) card nil)
                                                     (effect-completed state side eid))))))})
         ashes-flag {:challenger-phase-12 {:priority -1
                                       :once :per-turn
                                       :once-key :out-of-ashes
                                       :effect (effect (continue-ability
                                                         (ashes-recur (count (filter #(= "Out of the Ashes" (:title %))
                                                                                     (:discard challenger))))
                                                         card nil))}}]
   (run-event
    {:move-zone (req (if (= [:discard] (:zone card))
                       (register-events state side ashes-flag (assoc card :zone [:discard]))
                       (unregister-events state side card)))
     :events {:challenger-phase-12 nil}}
    nil))

   "Paper Tripping"
   {:msg "remove all tags" :effect (effect (lose :tag :all))}

   "Peace in Our Time"
   {:req (req (not (:scored-agenda contestant-reg)))
    :msg "gain 10 [Credits]. The Contestant gains 5 [Credits]"
    :effect (req (gain-credits state :challenger 10)
                 (gain-credits state :contestant 5)
                 (apply prevent-run-on-locale
                        state card (get-zones @state))
                 (register-events state side
                   {:challenger-turn-ends {:effect (req (apply enable-run-on-locale state card (get-zones @state)))}}
                  (assoc card :zone '(:discard))))
    :events {:challenger-turn-ends nil}}

   "Planned Assault"
   {:msg (msg "play " (:title target))
    :choices (req (cancellable (filter #(and (has-subtype? % "Run")
                                             (<= (:cost %) (:credit challenger))) (:deck challenger)) :sorted))
    :prompt "Choose a Run event" :effect (effect (trigger-event :searched-stack nil)
                                                 (shuffle! :deck)
                                                 (play-instant target {:no-additional-cost true}))}

   "Political Graffiti"
   (let [update-agenda-points (fn [state side target amount]
                               (set-prop state side (get-card state target) :agendapoints (+ amount (:agendapoints (get-card state target))))
                               (gain-agenda-point state side amount))]
     {:req (req archives-runnable)
      :events {:purge {:effect (effect (discard card {:cause :purge}))}}
      :discard-effect {:effect (req (let [current-side (get-scoring-owner state {:cid (:agenda-cid card)})]
                                    (update-agenda-points state current-side (find-cid (:agenda-cid card) (get-in @state [current-side :scored])) 1)))}
      :effect (effect (run :archives
                        {:req (req (= target :archives))
                         :replace-access
                         {:prompt "Select an agenda to host Political Graffiti"
                          :choices {:req #(in-contestant-scored? state side %)}
                          :msg (msg "host Political Graffiti on " (:title target) " as a hosted condition counter")
                          :effect (req (host state :challenger (get-card state target)
                                         ; keep host cid in :agenda-cid because `discard` will clear :host
                                         (assoc card :zone [:discard] :placed true :agenda-cid (:cid (get-card state target))))
                                       (update-agenda-points state :contestant target -1))}} card))})

   "Populist Rally"
   {:req (req (seq (filter #(has-subtype? % "Seedy") (all-active-placed state :challenger))))
    :msg "give the Contestant 1 fewer [Click] to spend on their next turn"
    :effect (effect (lose :contestant :click-per-turn 1)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:contestant-turn-ends {:effect (effect (gain :contestant :click-per-turn 1)
                                              (unregister-events card))}}}

   "Power Nap"
   {:effect (effect (gain-credits (+ 2 (count (filter #(has-subtype? % "Double")
                                                      (:discard challenger))))))
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard challenger)))) " [Credits]")}

   "Power to the People"
   {:effect (effect (register-events {:pre-steal-cost
                                      {:once :per-turn :effect (effect (gain-credits 7))
                                                       :msg "gain 7 [Credits]"}
                                      :challenger-turn-ends
                                      {:effect (effect (unregister-events card))}}
                    (assoc card :zone '(:discard))))
    :events {:pre-steal-cost nil :challenger-turn-ends nil}}

   "Prey"
   (run-event)

   "Process Automation"
   {:msg "gain 2 [Credits] and draw 1 card"
    :effect (effect (gain-credits 2) (draw 1))}

   "Push Your Luck"
   {:effect (effect (show-wait-prompt :challenger "Contestant to guess Odd or Even")
                    (resolve-ability
                      {:player :contestant :prompt "Guess whether the Challenger will spend an Odd or Even number of credits with Push Your Luck"
                       :choices ["Even" "Odd"]
                       :msg "force the Contestant to make a guess"
                       :effect (req (let [guess target]
                                      (clear-wait-prompt state :challenger)
                                      (resolve-ability
                                        state :challenger
                                        {:choices :credit :prompt "How many credits?"
                                         :msg (msg "spend " target " [Credits]. The Contestant guessed " guess)
                                         :effect (req (when (or (and (= guess "Even") (odd? target))
                                                                (and (= guess "Odd") (even? target)))
                                                        (system-msg state :challenger (str "gains " (* 2 target) " [Credits]"))
                                                        (gain-credits state :challenger (* 2 target))))} card nil)))}
                      card nil))}

   "Pushing the Envelope"
   (letfn [(hsize [s] (count (get-in s [:challenger :hand])))]
   {:msg (msg (if (<= (hsize @state) 2)
           "make a run, and adds +2 strength to placed characterbreakers"
           "make a run"))
    :prompt "Choose a locale"
    :choices (req runnable-locales)
    :async true
    :effect (req (when (<= (hsize @state) 2)
                   (let [breakers (filter #(has-subtype? % "Icebreaker") (all-active-placed state :challenger))]
                     (doseq [t breakers] (pump state side t 2 :all-run))))
                 (game.core/run state side (make-eid state) target))})

   "Quality Time"
   {:msg "draw 5 cards" :effect (effect (draw 5))}

   "Queens Gambit"
   {:choices ["0", "1", "2", "3"] :prompt "How many advancement tokens?"
    :effect (req (let [c (str->int target)]
                   (resolve-ability
                     state side
                     {:choices {:req #(and (is-party? (second (:zone %)))
                                           (= (last (:zone %)) :content)
                                           (not (:revealed %)))}
                      :msg (msg "add " c " advancement tokens on a card and gain " (* 2 c) " [Credits]")
                      :effect (effect (gain-credits (* 2 c))
                                      (add-prop :contestant target :advance-counter c {:placed true})
                                      (register-turn-flag! card :can-access
                                                           ;; prevent access of advanced card
                                                           (fn [_ _ card] (not (same-card? target card)))))}
                     card nil)))}

   "Quest Completed"
   {:req (req (and (some #{:hq} (:successful-run challenger-reg))
                   (some #{:rd} (:successful-run challenger-reg))
                   (some #{:archives} (:successful-run challenger-reg))))
    :choices {:req placed?} :msg (msg "access " (:title target))
    :effect (effect (access-card target))}

   "Rebirth"
   {:msg "change identities"
    :prompt "Choose an identity to become"
    :choices (req (let [is-swappable (fn [c] (and (= "Identity" (:type c))
                                             (= (-> @state :challenger :identity :faction) (:faction c))
                                             (not (.startsWith (:code c) "00")) ; only draft identities have this
                                             (not (= (:title c) (-> @state :challenger :identity :title)))))
                        swappable-ids (filter is-swappable (vals @all-cards))]
                    (cancellable swappable-ids :sorted)))

     :effect (req
               ;; Handle Ayla - Part 1
               (when (-> @state :challenger :identity :code (= "13012"))
                 (doseq [c (-> @state :challenger :identity :hosted)]
                   (move state side c :temp-nvram)))

               (move state side (last (:discard challenger)) :rfg)
               (disable-identity state side)

               ;; Manually reduce the challenger's link by old link
               (lose state :challenger :link (get-in @state [:challenger :identity :baselink]))

               ;; Move the selected ID to [:challenger :identity] and set the zone
               (swap! state update-in [side :identity]
                  (fn [x] (assoc (locale-card (:title target) (get-in @state [:challenger :user]))
                            :zone [:identity])))

               ;; enable-identity does not do everything that init-identity does
               (init-identity state side (get-in @state [:challenger :identity]))
               (system-msg state side "NOTE: passive abilities (Kate, Gabe, etc) will incorrectly fire
                if their once per turn condition was met this turn before Rebirth was played.
                Please adjust your game state manually for the rest of this turn if necessary")

               ;; Handle Ayla - Part 2
               (when-not (empty? (-> @state :challenger :temp-nvram))
                 (doseq [c (-> @state :challenger :temp-nvram)]
                   (host state side (get-in @state [:challenger :identity]) c {:facedown true}))))}

   "Recon"
   (run-event)

   "Reshape"
   {:prompt "Select two non-revealed Character to swap positions"
    :choices {:req #(and (placed? %) (not (revealed? %)) (character? %)) :max 2}
    :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
    :effect (req (when (= (count targets) 2)
                   (swap-character state side (first targets) (second targets))))}

   "Retrieval Run"
   {:req (req archives-runnable)
    :effect (effect (run :archives
                      {:req (req (= target :archives))
                       :replace-access
                       {:prompt "Choose a resource to place"
                        :msg (msg "place " (:title target))
                        :choices (req (filter #(is-type? % "Resource") (:discard challenger)))
                        :effect (effect (challenger-place target {:no-cost true}))}} card))}

   "Rigged Results"
   (letfn [(choose-character []
             {:prompt "Select a piece of Character to bypass"
              :choices {:req #(character? %)}
              :msg (msg "bypass " (card-str state target))
              :effect (effect (run (second (:zone target))))})
           (contestant-choice [spent]
             {:prompt "Guess how many credits were spent"
              :choices ["0" "1" "2"]
              :async true
              :effect (req (system-msg state :challenger (str "spends " spent "[Credit]. "
                                       (-> contestant :user :username) " guesses " target "[Credit]"))
                           (clear-wait-prompt state :challenger)
                           (lose-credits state :challenger spent)
                           (if (not= (str spent) target)
                             (continue-ability state :challenger (choose-character) card nil)
                             (effect-completed state side eid)))})
           (challenger-choice [cr]
             {:prompt "Spend how many credits?"
              :choices (take cr ["0" "1" "2"])
              :async true
              :effect (effect (show-wait-prompt :challenger "Contestant to guess")
                              (clear-wait-prompt :contestant)
                              (continue-ability :contestant (contestant-choice (str->int target)) card nil))})]
   {:effect (effect (show-wait-prompt :contestant "Challenger to spend credits")
                    (continue-ability (challenger-choice (inc (min 2 (:credit challenger)))) card nil))})

   "Rip Deal"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                                   {:async true
                                    :effect (req (let [n (min (-> @state :contestant :hand count) (access-count state side :hq-access))
                                                       heap (-> @state :challenger :discard count (- 1))]
                                                   (move state side (find-cid (:cid card) (:discard challenger)) :rfg)
                                                   (if (pos? heap)
                                                     (resolve-ability state side
                                                                      {:show-discard true
                                                                       :prompt (str "Choose " (min n heap) " card(s) to move from the Heap to your Grip")
                                                                       :async true
                                                                       :msg (msg "take " (join ", " (map :title targets)) " from their Heap to their Grip")
                                                                       :choices {:max (min n heap)
                                                                                 :all true
                                                                                 :req #(and (= (:side %) "Challenger")
                                                                                            (in-discard? %))}
                                                                       :effect (req (doseq [c targets] (move state side c :hand))
                                                                                    (do-access state side eid (:locale run) {:hq-root-only true}))} card nil)
                                                     (resolve-ability state side
                                                                      {:async true
                                                                       :msg (msg "take no cards from their Heap to their Grip")
                                                                       :effect (req (do-access state side eid (:locale run) {:hq-root-only true}))} card nil))))}} card))}

   "Rumor Mill"
   (letfn [(eligible? [card] (and (:uniqueness card)
                                  (or (card-is? card :type "Site")
                                      (card-is? card :type "Region"))
                                  (not (has-subtype? card "RegOLDion"))))
           (rumor [state] (filter eligible? (concat (all-placed state :contestant)
                                  (get-in @state [:contestant :hand])
                                  (get-in @state [:contestant :deck])
                                  (get-in @state [:contestant :discard]))))]
   {:leave-play (req (doseq [c (rumor state)]
                       (enable-card state :contestant c)))
    :effect (req (doseq [c (rumor state)]
                   (disable-card state :contestant c)))
    :events {:contestant-place {:req (req (eligible? target))
                            :effect (effect (disable-card :contestant target))}}})

   "Run Amok"
   {:implementation "Character discard is manual"
    :prompt "Choose a locale" :choices (req runnable-locales)
    :effect (effect (run target {:end-run {:msg " discard 1 piece of Character that was revealed during the run"}} card))}

   "Running Interference"
   (run-event
    {:events {:pre-reveal nil
              :run-ends nil}}
    nil
    nil
    (effect (register-events {:pre-reveal {:req (req (character? target))
                                        :effect (effect (reveal-cost-bonus (:cost target)))}
                              :run-ends {:effect (effect (unregister-events card))}}
                             (assoc card :zone '(:discard)))))

   "Satellite Uplink"
   {:choices {:max 2 :req placed?}
    :async true
    :effect (req (let [[card1 card2] targets]
                   (wait-for (expose state side card1)
                             (expose state side eid card2))))}

   "Scavenge"
   {:prompt "Select an placed resource to discard"
    :choices {:req #(and (is-type? % "Resource")
                         (placed? %))}
    :effect (req (let [discarded target tcost (- (:cost discarded)) st state si side]
                   (discard state side discarded)
                   (resolve-ability
                     state side
                     {:prompt "Select a resource to place from your Grip or Heap"
                      :show-discard true
                      :choices {:req #(and (is-type? % "Resource")
                                           (#{[:hand] [:discard]} (:zone %))
                                           (can-pay? st si nil (modified-place-cost st si % [:credit tcost])))}
                      :effect (effect (place-cost-bonus [:credit (- (:cost discarded))])
                                      (challenger-place target))
                      :msg (msg "discard " (:title discarded) " and place " (:title target))} card nil)))}

   "Scrubbed"
   {:events (let [sc {:effect (req (update! state side (dissoc card :scrubbed-target)))}]
                 {:encounter-character {:once :per-turn
                                  :effect (effect (update! (assoc card :scrubbed-target target))
                                                  (update-character-strength current-character))}
                  :pre-character-strength {:req (req (= (:cid target) (get-in card [:scrubbed-target :cid])))
                                     :effect (effect (character-strength-bonus -2 target))}
                  :run-ends sc})}

   "Showing Off"
   {:req (req rd-runnable)
    :effect (effect (run :rd
                      {:replace-access
                       {:msg "access cards from the bottom of R&D"
                        :async true
                        :effect (req
                                  ;; Not sure why this is done
                                  (wait-for (resolve-ability state side
                                                             {:effect (effect (register-events (:events (card-def card))
                                                                                               (assoc card :zone '(:discard))))}
                                                             card nil)
                                            (do-access state side eid (:locale run))))}} card))
    :events {:pre-access {:silent (req true)
                          :effect (req (swap! state assoc-in [:contestant :deck]
                                              (rseq (into [] (get-in @state [:contestant :deck])))))}
             :run-ends {:effect (req (swap! state assoc-in [:contestant :deck]
                                            (rseq (into [] (get-in @state [:contestant :deck]))))
                                     (unregister-events state side card))}}}

   "Singularity"
   (run-event
    {:choices (req (filter #(can-run-locale? state %) parties))}
    {:req (req (is-party? target))
     :replace-access {:mandatory true
                      :msg "discard all cards in the locale at no cost"
                      :effect (req (doseq [c (:content run-locale)]
                                     (discard state side c)))}})

   "Social Engineering"
   {:prompt "Select an unrevealed piece of Character"
    :choices {:req #(and (= (last (:zone %)) :characters) (not (revealed? %)) (character? %))}
    :effect (req (let [character target
                       serv (zone->name (second (:zone character)))]
              (resolve-ability
                 state :challenger
                 {:msg (msg "select the piece of Character at position " (character-index state character) " of " serv)
                  :effect (effect (register-events {:pre-reveal-cost
                                                    {:req (req (= target character))
                                                     :effect (req (let [cost (reveal-cost state side (get-card state target))]
                                                                    (gain-credits state :challenger cost)))
                                                     :msg (msg "gain " (reveal-cost state side (get-card state target)) " [Credits]")}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:pre-reveal-cost nil}
    :end-turn {:effect (effect (unregister-events card))}}

   "Spear Phishing"
   {:implementation "Bypass is manual"
    :prompt "Choose a locale"
    :choices (req runnable-locales)
    :effect (effect (run target nil card))}

   "Special Order"
   {:prompt "Choose an Icebreaker"
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (system-msg (str "adds " (:title target) " to their Grip and shuffles their Stack"))
                    (move target :hand))
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck challenger)) :sorted))}

   "Spooned"
   {:implementation "Character discard is manual"
    :prompt "Choose a locale"
    :choices (req runnable-locales)
    :effect (effect (run target nil card))}

   "Spot the Prey"
   {:prompt "Select 1 non-Character card to expose"
    :msg "expose 1 card and make a run"
    :choices {:req #(and (placed? %) (not (character? %)) (= (:side %) "Contestant"))}
    :async true
    :effect (req (wait-for (expose state side target)
                           (continue-ability
                             state side
                             {:prompt "Choose a locale"
                              :choices (req runnable-locales)
                              :async true
                              :effect (effect (game.core/run eid target))}
                             card nil)))}

   "Stimhack"
   (run-event
    nil
    {:end-run {:msg "take 1 brain damage"
               :effect (effect (damage eid :brain 1 {:unpreventable true :card card}))}}
    (effect (gain-run-credits 9)))

   "Sure Gamble"
   {:msg "gain 9 [Credits]" :effect (effect (gain-credits 9))}

   "Surge"
   {:msg (msg "place 2 virus tokens on " (:title target))
    :choices {:req #(and (has-subtype? % "Virus") (:added-virus-counter %))}
    :effect (req (add-counter state :challenger target :virus 2))}

   "SYN Attack"
   {:effect (req (if (< (count (:hand contestant)) 2)
                   (draw state :contestant 4)
                   (do (show-wait-prompt state :challenger "Contestant to choose an option for SYN Attack")
                       (resolve-ability state :contestant
                         {:prompt "Discard 2 cards or draw 4 cards?"
                          :choices ["Discard 2" "Draw 4"]
                          :effect (req (if (= target "Draw 4")
                                         (do (draw state :contestant 4)
                                             (system-msg state :contestant (str "draws 4 cards from SYN Attack"))
                                             (clear-wait-prompt state :challenger))
                                         (resolve-ability state :contestant
                                           {:prompt "Choose 2 cards to discard"
                                            :choices {:max 2 :req #(and (in-hand? %) (= (:side %) "Contestant"))}
                                            :effect (effect (discard-cards :contestant targets)
                                                            (system-msg :contestant (str "discards 2 cards from SYN Attack"))
                                                            (clear-wait-prompt :challenger))}
                                          card nil)))}
                        card nil))))}

   "System Outage"
   {:events {:contestant-draw {:req (req (not (first-event? state side :contestant-draw)))
                         :msg "force the Contestant to lose 1 [Credits]"
                         :effect (effect (lose-credits :contestant 1))}}}

   "System Seizure"
  {:effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
   :events {:pump-breaker {:silent (req true)
                           :req (req (or (and (has-flag? state side :current-run :system-seizure)
                                              (run-flag? state side (second targets) :system-seizure))
                                         (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (req (update! state side (update-in (second targets) [:pump :all-run] (fnil #(+ % (first targets)) 0)))
                                        (register-run-flag! state side card :system-seizure (fn [_ _ c] (= (:cid c) (:cid (second targets)))))
                                        (update-breaker-strength state side (second targets))
                                        (swap! state assoc-in [:per-turn (:cid card)] targets))}}
   :move-zone (req (when (= [:discard] (:zone card))
                     (unregister-events state side card)))}

   "Test Run"
   {:prompt "Place a resource from your Stack or Heap?"
    :choices (cancellable ["Stack" "Heap"])
    :msg (msg "place a resource from their " target)
    :effect (effect (resolve-ability
                      {:prompt "Choose a resource to place"
                       :choices (req (cancellable
                                       (filter #(is-type? % "Resource")
                                               ((if (= target "Heap") :discard :deck) challenger))))
                       :effect (effect (trigger-event :searched-stack nil)
                                       (shuffle! :deck)
                                       (challenger-place (assoc-in target [:special :test-run] true) {:no-cost true}))
                       :end-turn
                       {:req (req (get-in (find-cid (:cid target) (all-placed state :challenger)) [:special :test-run]))
                        :msg (msg "move " (:title target) " to the top of their Stack")
                        :effect (req (move state side (find-cid (:cid target) (all-placed state :challenger))
                                           :deck {:front true}))}}
                      card targets))}

   "The Makers Eye"
   {:req (req rd-runnable)
    :effect (effect (run :rd nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "The Noble Path"
   {:effect (req (doseq [c (:hand challenger)]
                   (discard state side c))
                 (register-events state side
                                  {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                (damage-prevent :meat Integer/MAX_VALUE)
                                                                (damage-prevent :brain Integer/MAX_VALUE))}
                                   :run-ends {:effect (effect (unregister-events card))}}
                                  (assoc card :zone '(:discard)))
                 (resolve-ability state side
                   {:prompt "Choose a locale"
                    :choices (req runnable-locales)
                    :msg (msg "discard their Grip and make a run on " target ", preventing all damage")
                    :effect (req (let [runtgt [(last (locale->zone state target))]
                                       characters (get-in @state (concat [:contestant :locales] runtgt [:characters]))]
                                   (swap! state assoc :per-run nil
                                                      :run {:locale runtgt :position (count characters)
                                                            :access-bonus 0 :run-effect nil})
                                   (gain-run-credits state :challenger (:bad-publicity contestant))
                                   (swap! state update-in [:challenger :register :made-run] #(conj % (first runtgt)))
                                   (trigger-event state :challenger :run runtgt)))} card nil))
    :events {:pre-damage nil :run-ends nil}}

   "The Prcharacter of Freedom"
   {:additional-cost [:connection 1]
    :msg "prevent the Contestant from advancing cards during their next turn"
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:rfg)))
                    (move (first (:play-area challenger)) :rfg))
    :events {:contestant-turn-begins
             {:effect (effect (register-turn-flag! card :can-advance
                                (fn [state side card]
                                  ((constantly false)
                                   (toast state :contestant "Cannot advance cards this turn due to The Prcharacter of Freedom." "warning"))))
                              (unregister-events card))}}}

   "Three Steps Ahead"
   {:end-turn {:effect (effect (gain-credits (* 2 (count (:successful-run challenger-reg)))))
               :msg (msg "gain " (* 2 (count (:successful-run challenger-reg))) " [Credits]")}}

   "Tinkering"
   {:prompt "Select a piece of Character"
    :choices {:req #(and (= (last (:zone %)) :characters) (character? %))}
    :effect (req (let [character target
                       serv (zone->name (second (:zone character)))
                       stypes (:subtype character)]
              (resolve-ability
                 state :challenger
                 {:msg (msg "make " (card-str state character) " gain Sentry, Code Gate, and Barrier until the end of the turn")
                  :effect (effect (update! (assoc character :subtype (combine-subtypes true (:subtype character) "Sentry" "Code Gate" "Barrier")))
                                  (update-character-strength (get-card state character))
                                  (add-icon card (get-card state character) "T" "green")
                                  (register-events {:challenger-turn-ends
                                                    {:effect (effect (remove-icon card (get-card state character))
                                                                     (update! (assoc (get-card state character) :subtype stypes)))}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:challenger-turn-ends nil}}

   "Trade-In"
   {:additional-cost [:hazard 1]
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:challenger-discard {:effect (effect (gain-credits (quot (:cost target) 2))
                                            (system-msg (str "discards " (:title target) " and gains " (quot (:cost target) 2) " [Credits]"))
                                            (continue-ability {:prompt "Choose a Hazard to add to your Grip from your Stack"
                                                               :choices (req (filter #(is-type? % "Hazard")
                                                                                     (:deck challenger)))
                                                               :msg (msg "add " (:title target) " to their Grip")
                                                               :effect (effect (trigger-event :searched-stack nil)
                                                                               (shuffle! :deck)
                                                                               (move target :hand)
                                                                               (unregister-events card))} card nil))}}}

   "Traffic Jam"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events {:pre-advancement-cost
             {:effect (req (advancement-cost-bonus
                             state side (count (filter #(= (:title %) (:title target)) (:scored contestant)))))}}}

   "Unplace"
   {:choices {:req #(and (placed? %)
                         (not (facedown? %))
                         (#{"Resource" "Hazard"} (:type %)))}
    :msg (msg "move " (:title target) " to their Grip")
    :effect (effect (move target :hand))}

   "Unscheduled Maintenance"
   {:events {:contestant-place {:req (req (character? target))
                            :effect (effect (register-turn-flag!
                                              card :can-place-character
                                              (fn [state side card]
                                                (if (character? card)
                                                  ((constantly false)
                                                   (toast state :contestant "Cannot place Character the rest of this turn due to Unscheduled Maintenance"))
                                                  true))))}}
    :leave-play (effect (clear-turn-flag! card :can-place-character))}

   "Vamp"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:async true
                               :prompt "How many [Credits]?" :choices :credit
                               :msg (msg "take 1 tag and make the Contestant lose " target " [Credits]")
                               :effect (effect (lose-credits :contestant target)
                                               (tag-challenger eid 1))}} card))}

   "Wanton Destruction"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Contestant to discard " target " cards from HQ at random")
                               :prompt "How many [Click] do you want to spend?"
                               :choices (req (map str (range 1 (inc (:click challenger)))))
                               :effect (req (let [n (str->int target)]
                                              (when (pay state :challenger card :click n)
                                                (discard-cards state :contestant (take n (shuffle (:hand contestant)))))))}} card))}

   "White Hat"
   (letfn [(finish-choice [choices]
             (let [choices (filter #(not= "None" %) choices)]
               (when (not-empty choices)
                {:effect (req (doseq [c choices]
                                (move state :contestant c :deck))
                              (shuffle! state :contestant :deck))
                 :msg (str "shuffle " (join ", " (map :title choices)) " into R&D")})))
           (choose-cards [hand chosen]
             {:prompt "Choose a card in HQ to shuffle into R&D"
              :player :challenger
              :choices (conj (vec (clojure.set/difference hand chosen))
                             "None")
              :async true
              :effect (req (if (and (empty? chosen)
                                    (not= "None" target))
                             (continue-ability state side (choose-cards hand (conj chosen target)) card nil)
                             (continue-ability state side (finish-choice (conj chosen target)) card nil)))})]
   {:req (req (some #{:hq :rd :archives} (:successful-run challenger-reg)))
    :trace {:base 3
            :unsuccessful
            {:async true
             :msg "reveal all cards in HQ"
             :effect (effect (continue-ability :challenger (choose-cards (set (:hand contestant)) #{}) card nil))}}})

   "Windfall"
   {:effect (effect (shuffle! :deck)
                    (resolve-ability
                      {:effect (req (let [topcard (first (:deck challenger))
                                          cost (:cost topcard)]
                                      (discard state side topcard)
                                      (when-not (is-type? topcard "Event")
                                        (gain-credits state side cost))
                                      (system-msg state side
                                                  (str "shuffles their Stack and discards " (:title topcard)
                                                       (when-not (is-type? topcard "Event")
                                                         (str " to gain " cost " [Credits]"))))))}
                     card nil))}})
