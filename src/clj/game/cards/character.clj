(ns game.cards.character
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int other-side]]
            [cardnum.cards :refer [all-cards]]))

;;;; Helper functions specific for Character

;;; Challenger abilites for breaking subs
(defn challenger-pay-or-break
  "Ability to break a subroutine by spending a radicle (Bioroids, Negotiator, etc)"
  [cost subs label]
  (let [cost-str (build-cost-str [cost])
        subs-str (quantify subs "subroutine")]
    {:cost cost
     :label (str label " " subs-str)
     :effect (req (system-msg state :challenger (str "spends " cost-str " to " label " " subs-str " on " (:title card))))}))

(defn challenger-break
  "Ability to break a subroutine by spending a radicle (Bioroids, Negotiator, etc)"
  [cost subs]
  (challenger-pay-or-break cost subs "break"))

(defn challenger-pay
  "Ability to pay to avoid a subroutine by spending a radicle (Popup Window, Turing, etc)"
  [cost subs]
  (challenger-pay-or-break cost subs "pay for"))

;;; General subroutines
(def end-the-run
  "Basic ETR subroutine"
  {:label "End the run"
   :msg "end the run"
   :effect (effect (end-run))})

(def end-the-run-if-tagged
  "ETR subroutine if tagged"
  {:label "End the run if the Challenger is tagged"
   :req (req tagged)
   :msg "end the run"
   :effect (effect (end-run))})

(defn give-tags
  "Basic give challenger n tags subroutine."
  [n]
  {:label (str "Give the Challenger " (quantify n "tag"))
   :msg (str "give the Challenger " (quantify n "tag"))
   :async true
   :effect (effect (gain-tags :contestant eid n))})

(def add-power-counter
  "Adds 1 power counter to the card."
  {:label "Add 1 power counter"
   :msg "add 1 power counter"
   :effect (effect (add-counter card :power 1))})

(defn trace-ability
  "Run a trace with specified base strength.
   If successful trigger specified ability"
  ([base {:keys [label] :as ability}]
   {:label (str "Trace " base " - " label)
    :trace {:base base
            :label label
            :successful ability}})
  ([base ability un-ability]
   (let [label (str (:label ability) " / " (:label un-ability))]
     {:label (str "Trace " base " - " label)
      :trace {:base base
              :label label
              :successful ability
              :unsuccessful un-ability}})))

(defn tag-trace
  "Trace ability for giving a tag, at specified base strength"
  ([base] (tag-trace base 1))
  ([base n] (trace-ability base (give-tags n))))

(defn gain-credits-sub
  "Gain specified amount of credits"
  [credits]
  {:label (str "Gain " credits " [Credits]")
   :msg (str "gain " credits " [Credits]")
   :effect (effect (gain-credits credits))})

(defn power-counter-ability
  "Does specified ability using a power counter."
  [{:keys [label message] :as ability}]
  (assoc ability :label (str "Hosted power counter: " label)
                 :msg (str message " using 1 power counter")
                 :counter-cost [:power 1]))

(defn do-psi
  "Start a psi game, if not equal do ability"
  ([{:keys [label] :as ability}]
  {:label (str "Psi Game - " label)
   :msg (str "start a psi game (" label ")")
   :psi {:not-equal ability}})
  ([{:keys [label-neq] :as neq-ability} {:keys [label-eq] :as eq-ability}]
   {:label (str "Psi Game - " label-neq " / " label-eq)
    :msg (str "start a psi game (" label-neq " / " label-eq ")")
    :psi {:not-equal neq-ability
          :equal     eq-ability}}))

(def take-bad-pub
  "Bad pub on reveal effect."
  (effect (gain-bad-publicity :contestant 1)
          (system-msg (str "takes 1 bad publicity from " (:title card)))))

(def challenger-loses-click
  "Challenger loses a click effect"
  (req (if (:challenger-phase-12 @state)
    ; this handles Jak Sinclair losing clicks before they are given
    (do (swap! state update-in [:challenger :extra-click-temp] (fnil dec 0))
        (toast state :challenger "Challenger loses a click at start of turn" "warning")
        (toast state :contestant "Challenger loses a click at start of turn" "warning"))
    (lose state :challenger :click 1))))

;;; For Advanceable Character
(defn get-advance-counters
  [card]
  (+ (get-counters card :advancement) (:extra-advance-counter card 0)))

(def advance-counters
  "Number of advancement counters - for advanceable Character."
  (req (get-advance-counters card)))

(def space-character-reveal-bonus
  "Amount of reveal reduction for the Space Character."
  (req (* -3 (get-advance-counters card))))

(defn space-character
  "Creates data for Space Character with specified abilities."
  [& abilities]
  {:advanceable :always
   :subroutines (vec abilities)
   :reveal-cost-bonus space-character-reveal-bonus})


;;; For Grail Character
(defn grail-in-hand
  "Req that specified card is a Grail card in the Contestant's hand."
  [card]
  (and (= (:side card) "Contestant")
       (in-hand? card)
       (has-subtype? card "Grail")))

(def reveal-grail
  "Ability for revealing Grail Character from HQ."
  {:label "Reveal up to 2 Grail Character from HQ"
   :choices {:max 2
             :req grail-in-hand}
   :msg (let [sub-label #(:label (first (:subroutines (card-def %))))]
          (msg "reveal " (join ", " (map #(str (:title %) " (" (sub-label %) ")") targets))))})

(def resolve-grail
  "Ability for resolving a subroutine on a Grail Character in HQ."
  {:label "Resolve a Grail Character subroutine from HQ"
   :choices {:req grail-in-hand}
   :effect (req (doseq [character targets]
                  (let [subroutine (first (:subroutines (card-def character)))]
                    (resolve-ability state side subroutine card nil))))})

(defn grail-character
  "Creates data for grail Character"
  [ability]
  {:abilities [reveal-grail]
   :subroutines [ability resolve-grail]})


;;; For NEXT Character
(defn next-character-count
  "Counts number of revealed NEXT Character - for use with NEXT Bronze and NEXT Gold"
  [contestant]
  (let [locales (flatten (seq (:locales contestant)))
        revealed-next? #(and (revealed? %) (has-subtype? % "NEXT"))]
    (reduce (fn [c locale] (+ c (count (filter revealed-next? (:characters locale))))) 0 locales)))


;;; For Montestanth Character
(defn montestanth [state side card new old]
  (update! state side (assoc card
                        :subtype-target new
                        :subtype (combine-subtypes true
                                                   (remove-subtypes (:subtype card) old)
                                                   new)))
  (update-character-strength state side card))

(defn montestanth-effect
  "Creates montestanth effect for Character. Montestanths from base type to other type"
  [base other]
  (req (if (odd? (get-counters (get-card state card) :advancement))
         (montestanth state side card other base)
         (montestanth state side card base other))))

(defn montestanth-character
  "Creates the data for montestanth Character with specified types and ability."
  [base other ability]
  (let [ab {:req (req (= (:cid card) (:cid target)))
            :effect (montestanth-effect base other)}]
    {:advanceable :always
     :effect (montestanth-effect base other)
     :subroutines [ability]
     :events {:advance ab :advancement-placed ab}}))


;;; For Constellation Character
(defn constellation-character
  "Generates map for Constellation Character with specified effect."
  [ability]
  {:subroutines [(assoc-in (trace-ability 2 ability) [:trace :kicker] (assoc ability :min 5))]})


;; For 7 Wonders Character
(defn wonder-sub
  "Checks total number of advancement counters on a piece of character against number"
  [card number]
  (<= number (get-advance-counters card)))

;;; Helper function for adding implementation notes to Character defined with functions
(defn- implementation-note
  "Adds an implementation note to the character-definition"
  [note character-def]
  (assoc character-def :implementation note))


;;;; Card definitions
(def card-definitions
  {"Aiki"
   {:subroutines [(do-psi {:label "Challenger draws 2 cards"
                           :msg "make the Challenger draw 2 cards"
                           :effect (effect (draw :challenger 2))})
                  (do-net-damage 1)]}

   "Aimor"
   {:subroutines [{:label "Discard the top 3 cards of the Stack. Discard Aimor."
                   :effect (req (when (not-empty (:deck challenger))
                                  (system-msg state :contestant
                                              (str "uses Aimor to discard "
                                                   (join ", " (map :title (take 3 (:deck challenger))))
                                                   " from the Challenger's Stack"))
                                  (mill state :contestant :challenger 3))
                                (when current-character
                                  (no-action state :contestant nil)
                                  (continue state :challenger nil))
                                (discard state side card)
                                (system-msg state side (str "discards Aimor")))}]}

   "Anansi"
   (let [contestant-draw {:optional {:prompt "Draw 1 card?"
                               :yes-ability {:async true
                                             :msg "draw 1 card"
                                             :effect (effect (draw eid 1 nil))}}}
         challenger-draw {:async true
                      :effect (req (show-wait-prompt state :contestant "Challenger to decide on card draw")
                                   (continue-ability state side
                                                     {:player :challenger
                                                      :optional
                                                      {:prompt "Pay 2 [Credits] to draw 1 card?"
                                                       :no-ability {:effect (effect (system-msg :challenger "does not draw 1 card")
                                                                                    (clear-wait-prompt :contestant))}
                                                       :yes-ability {:async true
                                                                     :effect (effect
                                                                               (system-msg :challenger "pays 2 [Credits] to draw 1 card")
                                                                               (lose-credits 2)
                                                                               (clear-wait-prompt :contestant)
                                                                               (draw eid 1 nil))}}}
                                                     card nil))}]
     {:implementation "Encounter-ends effect is manually triggered."
      :subroutines [{:msg "rearrange the top 5 cards of R&D"
                     :async true
                     :effect (req (show-wait-prompt state :challenger "Contestant to rearrange the top cards of R&D")
                                  (let [from (take 5 (:deck contestant))]
                                       (if (pos? (count from))
                                         (continue-ability state side (reorder-choice :contestant :challenger from '()
                                                                                      (count from) from)
                                                           card nil)
                                         (do (clear-wait-prompt state :challenger)
                                             (effect-completed state side eid)))))}
                    {:label "Draw 1 card; allow challenger to draw 1 card"
                     :async true
                     :effect (req (wait-for (resolve-ability state side contestant-draw card nil)
                                            (continue-ability state :challenger challenger-draw card nil)))}
                    (do-net-damage 1)]
      :abilities [(do-net-damage 3)]})

   "Archangel"
   {:flags {:rd-reveal (req true)}
    :access
    {:async true
     :req (req (not= (first (:zone card)) :discard))
     :effect (effect (show-wait-prompt :challenger "Contestant to decide to trigger Archangel")
                     (continue-ability
                       {:optional
                        {:prompt "Pay 3 [Credits] to force Challenger to encounter Archangel?"
                         :yes-ability {:cost [:credit 3]
                                       :async true
                                       :effect (effect (system-msg :contestant "pays 3 [Credits] to force the Challenger to encounter Archangel")
                                                       (clear-wait-prompt :challenger)
                                                       (continue-ability
                                                         :challenger {:optional
                                                                  {:player :challenger
                                                                   :prompt "You are encountering Archangel. Allow its subroutine to fire?"
                                                                   :priority 1
                                                                   :yes-ability {:async true
                                                                                 :effect (effect (play-subroutine eid {:card card :subroutine 0}))}
                                                                   :no-ability {:effect (effect (effect-completed eid))}}}
                                                         card nil))}
                         :no-ability {:effect (effect (system-msg :contestant "declines to force the Challenger to encounter Archangel")
                                                      (clear-wait-prompt :challenger))}}}
                       card nil))}
   :subroutines [(trace-ability 6 {:async true
                                   :effect (effect (show-wait-prompt :challenger "Contestant to select Archangel target")
                                                   (continue-ability {:choices {:req #(and (placed? %)
                                                                                           (card-is? % :side :challenger))}
                                                                      :label "Add 1 placed card to the Challenger's Grip"
                                                                      :msg "add 1 placed card to the Challenger's Grip"
                                                                      :effect (effect (clear-wait-prompt :challenger)
                                                                                      (move :challenger target :hand true)
                                                                                      (system-msg (str "adds " (:title target)
                                                                                                       " to the Challenger's Grip")))
                                                                      :cancel-effect (effect (clear-wait-prompt :challenger)
                                                                                             (effect-completed eid))}
                                                                     card nil))})]}

   "Archer"
   {:additional-cost [:forfeit]
    :subroutines [(gain-credits-sub 2)
                  discard-resource
                  end-the-run]}

   "Architect"
   {:flags {:undiscardable-while-revealed true}
    :subroutines [{:label "Look at the top 5 cards of R&D"
                   :prompt "Choose a card to place"
                   :priority true
                   :activatemsg "uses Architect to look at the top 5 cards of R&D"
                   :req (req (and (not (string? target))
                                  (not (is-type? target "Operation"))))
                   :not-distinct true
                   :choices (req (conj (take 5 (:deck contestant)) "No place"))
                   :effect (effect (system-msg (str "chooses the card in position "
                                                    (+ 1 (.indexOf (take 5 (:deck contestant)) target))
                                                    " from R&D (top is 1)"))
                                   (contestant-place (move state side target :play-area) nil {:no-place-cost true}))}
                  {:label "Place a card from HQ or Archives"
                   :prompt "Select a card to place from Archives or HQ"
                   :show-discard true
                   :priority true
                   :choices {:req #(and (not (is-type? % "Operation"))
                                        (#{[:hand] [:discard]} (:zone %))
                                        (= (:side %) "Contestant"))}
                   :effect (effect (contestant-place target nil))
                   :msg (msg (contestant-place-msg target))}]}

   "Ashigaru"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand contestant)) " subroutines")}]
    :subroutines [end-the-run]}

   "Assassin"
   {:subroutines [(trace-ability 5 (do-net-damage 3))
                  (trace-ability 4 discard-resource)]}

   "Asteroid Belt"
   (space-character end-the-run)

   "Authenticator"
   {:implementation "Encounter effect is manual"
    :abilities [(give-tags 1)]
    :challenger-abilities [{:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :challenger "takes 1 tag on encountering Authenticator to Bypass it")
                                     (gain-tags state :challenger eid 1 {:unpreventable true}))}]
    :subroutines [(gain-credits-sub 2)
                  end-the-run]}

   "Bailiff"
   {:implementation "Gain credit is manual"
    :abilities [(gain-credits-sub 1)]
    :subroutines [end-the-run]}

   "Bandwidth"
   {:subroutines [{:msg "give the Challenger 1 tag"
                   :async true
                   :effect (effect (gain-tags :contestant eid 1)
                                   (register-events
                                     {:successful-run {:effect (effect (lose-tags :contestant 1))
                                                       :msg "make the Challenger lose 1 tag"}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card))}]
    :events {:successful-run nil :run-ends nil}}

   "Bastion"
   {:subroutines [end-the-run]}

   "Battlement"
   {:subroutines [end-the-run]}

   "Blockchain"
   (letfn [(sub-count [contestant] (int (/ (count (filter #(and (is-type? % "Operation") (has-subtype? % "Transaction"))
                                                    (:discard contestant)))
                                     2)))]
     {:abilities [{:label "Gain subroutines"
                   :msg (msg (let [c (sub-count contestant)]
                               (str "gain " c (pluralize " subroutine" c))))}]
      :subroutines [{:label "Gain 1 [credits], Challenger loses 1 [credits]"
                     :msg "gain 1 [credits] and force the Challenger to lose 1 [credits]"
                     :effect (effect (gain-credits 1)
                                     (lose-credits :challenger 1))}
                    end-the-run]})

   "Bloodletter"
   {:subroutines [{:label "Challenger discards 1 resource or top 2 cards of their Stack"
                   :effect (req (if (empty? (filter #(is-type? % "Resource") (all-active-placed state :challenger)))
                                   (do (mill state :challenger 2)
                                       (system-msg state :challenger (str "discards the top 2 cards of their Stack")))
                                   (do (show-wait-prompt state :contestant "Challenger to choose an option for Bloodletter")
                                       (resolve-ability state :challenger
                                         {:prompt "Discard 1 resource or discard top 2 cards of the Stack?"
                                          :choices ["Discard 1 resource" "Discard top 2 of Stack"]
                                          :effect (req (if (and (= target "Discard top 2 of Stack") (> (count (:deck challenger)) 1))
                                                         (do (mill state :challenger 2)
                                                             (system-msg state :challenger (str "discards the top 2 cards of their Stack")))
                                                         (resolve-ability state :challenger discard-resource card nil))
                                                      (clear-wait-prompt state :contestant))}
                                        card nil))))}]}

   "Bloom"
   (let [character-index (fn [state i] (first (keep-indexed #(when (= (:cid %2) (:cid i)) %1)
                                                      (get-in @state (cons :contestant (:zone i))))))]
     {:subroutines
              [{:label "Place a piece of character from HQ protecting another locale, ignoring all costs"
                :prompt "Choose Character to place from HQ in another locale"
                :async true
                :choices {:req #(and (character? %)
                                     (in-hand? %))}
                :effect (req (let [this (zone->name (second (:zone card)))
                                   ncharacter target]
                               (continue-ability state side
                                                 {:prompt (str "Choose a location to place " (:title target))
                                                  :choices (req (remove #(= this %) (contestant-place-list state ncharacter)))
                                                  :async true
                                                  :effect (effect (contestant-place ncharacter target {:no-place-cost true}))}
                                                 card nil)))}
               {:label "Place a piece of character from HQ in the next innermost position, protecting this locale, ignoring all costs"
                :prompt "Choose Character to place from HQ in this locale"
                :async true
                :choices {:req #(and (character? %)
                                     (in-hand? %))}
                :effect (req (let [newcharacter (assoc target :zone (:zone card))
                                   bndx (character-index state card)
                                   characters (get-in @state (cons :contestant (:zone card)))
                                   newcharacters (apply conj (subvec characters 0 bndx) newcharacter (subvec characters bndx))]
                               (swap! state assoc-in (cons :contestant (:zone card)) newcharacters)
                               (swap! state update-in (cons :contestant (:zone target))
                                      (fn [coll] (remove-once #(= (:cid %) (:cid target)) coll)))
                               (card-init state side newcharacter {:resolve-effect false
                                                             :init-data true})
                               (trigger-event state side :contestant-place newcharacter)))}]})

   "Brainstorm"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand challenger)) " subroutines")}]
    :subroutines [(do-brain-damage 1)]}

   "Builder"
   {:abilities [{:label "Move Builder to the outermost position of any locale"
                 :cost [:click 1] :prompt "Choose a locale" :choices (req locales)
                 :msg (msg "move it to the outermost position of " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]
    :subroutines [{:label "Place 1 advancement token on an Character that can be advanced protecting this locale"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req #(and (character? %)
                                        (can-be-advanced? %))}
                   :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Bullfrog"
   {:subroutines [(do-psi {:label "Move Bullfrog to another locale"
                           :player :contestant
                           :prompt "Choose a locale"
                           :choices (req locales)
                           :msg (msg "move it to the outermost position of " target)
                           :effect (req (let [dest (locale->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc % :position (count (get-in contestant (conj dest :characters)))
                                                           :locale (rest dest))))
                                        (move state side card
                                              (conj (locale->zone state target) :characters)))})]}

   "Bulwark"
   {:effect take-bad-pub
    :abilities [{:msg "gain 2 [Credits] if there is an placed AI"
                 :req (req (some #(has-subtype? % "AI") (all-active-placed state :challenger)))
                 :effect (effect (gain-credits 2))}]
    :subroutines [(assoc discard-resource :player :challenger
                                       :msg "force the Challenger to discard 1 resource"
                                       :label "The Challenger discards 1 resource")
                  {:msg "gain 2 [Credits] and end the run"
                   :effect (effect (gain-credits 2)
                                   (end-run))}]}


   "Burke Bugs"
   {:subroutines [(trace-ability 0 (assoc discard-resource :not-distinct true
                                                        :player :challenger
                                                        :msg "force the Challenger to discard a resource"
                                                        :label "Force the Challenger to discard a resource"))]}

   "Caduceus"
   {:subroutines [(trace-ability 3 (gain-credits-sub 3))
                  (trace-ability 2 end-the-run)]}

   "Cell Portal"
   {:subroutines [{:msg "make the Challenger approach the outermost Character"
                   :effect (req (let [srv (first (:locale run))
                                      n (count (get-in @state [:contestant :locales srv :characters]))]
                                  (swap! state assoc-in [:run :position] n)
                                  (hide state side card)))}]}

   "Changeling"
   (montestanth-character "Barrier" "Sentry" end-the-run)

   "Checkpoint"
   {:effect take-bad-pub
    :subroutines [(trace-ability 5 {:label "Do 3 meat damage when this run is successful"
                                    :msg "do 3 meat damage when this run is successful"
                                    :effect (effect (register-events
                                                      {:successful-run
                                                       {:async true
                                                        :msg "do 3 meat damage"
                                                        :effect (effect (damage eid :meat 3 {:card card}))}
                                                       :run-ends {:effect (effect (unregister-events card))}}
                                                     card))})]
    :events {:successful-run nil :run-ends nil}}

   "Chetana"
   {:subroutines [{:msg "make each player gain 2 [Credits]"
                   :effect (effect (gain-credits :challenger 2)
                                   (gain-credits :contestant 2))}
                  (do-psi {:label "Do 1 net damage for each card in the Challenger's grip"
                           :effect (effect (damage eid :net (count (get-in @state [:challenger :hand])) {:card card}))
                           :msg (msg (str "do " (count (get-in @state [:challenger :hand])) " net damage"))})]}

   "Chimera"
   (let [turn-end-ability {:effect (effect (hide :contestant card)
                                           (update! (assoc (get-card state card) :subtype "Mythic")))}]
     {:prompt "Choose one subtype"
      :choices ["Barrier" "Code Gate" "Sentry"]
      :msg (msg "make it gain " target " until the end of the turn")
      :effect (effect (update! (assoc card
                                 :subtype-target target
                                 :subtype (combine-subtypes true (:subtype card) target)))
                      (update-character-strength card))
      :events {:challenger-turn-ends turn-end-ability
               :contestant-turn-ends turn-end-ability}
      :subroutines [end-the-run]})

   "Chiyashi"
   {:implementation "Discard effect when using an AI to break is activated manually"
    :abilities [{:label "Discard the top 2 cards of the Challenger's Stack"
                 :req (req (some #(has-subtype? % "AI") (all-active-placed state :challenger)))
                 :msg (msg (str "discard " (join ", " (map :title (take 2 (:deck challenger)))) " from the Challenger's Stack"))
                 :effect (effect (mill :contestant :challenger 2))}]
    :subroutines [(do-net-damage 2)
                  end-the-run]}

   "Chrysalis"
   {:flags {:rd-reveal (req true)}
    :subroutines [(do-net-damage 2)]
    :access {:async true
             :req (req (not= (first (:zone card)) :discard))
             :effect (effect (show-wait-prompt :contestant "Challenger to decide to break Chrysalis subroutine")
                             (continue-ability
                               :challenger {:optional
                                        {:player :challenger
                                         :prompt "You are encountering Chrysalis. Allow its subroutine to fire?"
                                         :priority 1
                                         :yes-ability {:effect (effect (clear-wait-prompt :contestant)
                                                                       (play-subroutine eid {:card card :subroutine 0}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :contestant)
                                                                      (effect-completed eid))}}}
                              card nil))}}

   "Chum"
   {:subroutines [{:label "Give +2 strength to next Character Challenger encounters"
                   :req (req this-locale)
                   :prompt "Select the Character the Challenger is encountering"
                   :choices {:req #(and (revealed? %) (character? %))}
                   :msg (msg "give " (:title target) " +2 strength")
                   :effect (req (let [character (:cid target)]
                                  (register-events state side
                                    {:pre-character-strength {:req (req (= (:cid target) character))
                                                        :effect (effect (character-strength-bonus 2 target))}
                                     :run-ends {:effect (effect (unregister-events card))}}
                                   card)
                                  (update-all-character state side)))}
                  (do-net-damage 3)]
    :events {:pre-character-strength nil :run-ends nil}}

   "Clairvoyant Monitor"
   {:subroutines [(do-psi {:label "Place 1 advancement token and end the run"
                           :player :contestant
                           :prompt "Select a target for Clairvoyant Monitor"
                           :msg (msg "place 1 advancement token on "
                                     (card-str state target) " and end the run")
                           :choices {:req placed?}
                           :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                           (end-run))})]}

   "Cobra"
   {:subroutines [discard-resource (do-net-damage 2)]}

   "Colossus"
   {:advanceable :always
    :subroutines [{:label "Give the Challenger 1 tag (Give the Challenger 2 tags)"
                   :async true
                   :msg (msg "give the Challenger " (if (wonder-sub card 3) "2 tags" "1 tag"))
                   :effect (effect (gain-tags :contestant eid (if (wonder-sub card 3) 2 1)))}
                  {:label "Discard 1 resource (Discard 1 resource and 1 radicle)"
                   :async true
                   :msg (msg "discard 1 resource" (when (wonder-sub card 3) " and 1 radicle"))
                   :effect (req (wait-for (resolve-ability state side discard-resource card nil)
                                          (if (wonder-sub card 3)
                                            (continue-ability
                                              state side
                                              {:prompt "Choose a radicle to discard"
                                               :msg (msg "discard " (:title target))
                                               :choices {:req #(and (placed? %)
                                                                    (is-type? % "Radicle"))}
                                               :cancel-effect (req (effect-completed state side eid))
                                               :effect (effect (discard target {:cause :subroutine}))}
                                              card nil)
                                            (effect-completed state side eid))))}]
    :strength-bonus advance-counters}

   "Conundrum"
   {:subroutines [(assoc discard-resource :player :challenger
                                       :msg "force the Challenger to discard 1 resource"
                                       :label "The Challenger discards 1 resource")
                  {:msg "force the Challenger to lose 1 [Click] if able"
                   :effect challenger-loses-click}
                  end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "AI") (all-active-placed state :challenger)) 3 0))}

   "Cortex Lock"
   {:subroutines [{:label "Do 1 net damage for each unused memory unit the Challenger has"
                   :msg (msg "do " (available-mu state) " net damage")
                   :effect (effect (damage eid :net (available-mu state) {:card card}))}]}

   "Crick"
   {:subroutines [{:label "place a card from Archives"
                   :prompt "Select a card to place from Archives"
                   :show-discard true
                   :priority true
                   :choices {:req #(and (not (is-type? % "Operation"))
                                        (= (:zone %) [:discard])
                                        (= (:side %) "Contestant"))}
                   :msg (msg (contestant-place-msg target))
                   :effect (effect (contestant-place target nil))}]
    :strength-bonus (req (if (= (second (:zone card)) :archives) 3 0))}

   "Curtain Wall"
   {:subroutines [end-the-run]
    :strength-bonus (req (let [characters (:characters (card->locale state card))]
                           (if (= (:cid card) (:cid (last characters))) 4 0)))
    :events (let [cw {:req (req (and (not= (:cid card) (:cid target))
                                     (= (card->locale state card) (card->locale state target))))
                      :effect (effect (update-character-strength card))}]
              {:contestant-place cw :discard cw :card-moved cw})}

   "Data Hound"
   (letfn [(dh-discard [cards]
             {:prompt "Choose a card to discard"
              :choices cards
              :async true
              :msg (msg "discard " (:title target))
              :effect (req (do (discard state side target {:unpreventable true})
                               (continue-ability
                                 state side
                                 (reorder-choice
                                   :challenger :challenger (remove-once #(= % target) cards)
                                   '() (count (remove-once #(= % target) cards))
                                   (remove-once #(= % target) cards))
                                 card nil)))})]
     {:subroutines [(trace-ability 2 {:async true
                                      :label "Look at the top of Stack"
                                      :msg "look at top X cards of Stack"
                                      :effect (req (show-wait-prompt state :challenger "Contestant to rearrange the top cards of the Challenger's Stack")
                                                   (let [c (- target (second targets))
                                                         from (take c (:deck challenger))]
                                                     (system-msg state :contestant
                                                                 (str "looks at the top " c " cards of Stack"))
                                                     (if (< 1 c)
                                                       (continue-ability state side (dh-discard from) card nil)
                                                       (do (system-msg state :contestant (str "discards " (:title (first from))))
                                                           (discard state side (first from) {:unpreventable true})
                                                           (clear-wait-prompt state :challenger)
                                                           (effect-completed state side eid)))))})]})

   "Data Loop"
   {:implementation "Encounter effect is manual"
    :subroutines [end-the-run-if-tagged
                  end-the-run]
    :challenger-abilities [{:label "Add 2 cards from your Grip to the top of the Stack"
                        :req (req (pos? (count (:hand challenger))))
                        :effect (req (let [n (min 2 (count (:hand challenger)))]
                                       (resolve-ability state side
                                         {:prompt (msg "Choose " n " cards in your Grip to add to the top of the Stack (first card targeted will be topmost)")
                                          :choices {:max n :all true
                                                    :req #(and (in-hand? %) (= (:side %) "Challenger"))}
                                          :effect (req (doseq [c targets]
                                                         (move state :challenger c :deck {:front true}))
                                                       (system-msg state :challenger (str "adds " n " cards from their Grip to the top of the Stack")))}
                                        card nil)))}]}

   "Data Mine"
   {:subroutines [{:msg "do 1 net damage"
                   :effect (req (damage state :challenger eid :net 1 {:card card})
                                (when current-character
                                  (no-action state side nil)
                                  (continue state side nil))
                                (discard state side card))}]}

   "Datapike"
   {:subroutines [{:msg "force the Challenger to pay 2 [Credits] if able"
                   :effect (effect (pay :challenger card :credit 2))}
                  end-the-run]}

   "Data Raven"
   {:implementation "Encounter effect is manual"
    :abilities [(give-tags 1)
                (power-counter-ability (give-tags 1))]
    :challenger-abilities [{:label "End the run"
                        :effect (req (end-run state :challenger)
                                     (system-msg state :challenger "chooses to end the run on encountering Data Raven"))}
                       {:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :challenger "chooses to take 1 tag on encountering Data Raven")
                                     (gain-tags state :challenger eid 1))}]
    :subroutines [(trace-ability 3 add-power-counter)]}

   "Data Ward"
   {:challenger-abilities [{:label "Pay 3 [Credits]"
                        :effect (req (pay state :challenger card :credit 3)
                                     (system-msg state :challenger "chooses to pay 3 [Credits] on encountering Data Ward"))}
                       {:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :challenger "chooses to take 1 tag on encountering Data Ward")
                                     (gain-tags state :challenger eid 1))}]
    :subroutines [end-the-run-if-tagged]}

   "DNA Tracker"
   {:subroutines [{:msg "do 1 net damage and make the Challenger lose 2 [Credits]"
                   :effect (req (wait-for (damage state side :net 1 {:card card})
                                          (lose-credits state :challenger 2)))}]}

   "Dracō"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target)
                    (update-character-strength card))
    :strength-bonus (req (get-counters card :power))
    :subroutines [(trace-ability 2 {:label "Give the Challenger 1 tag and end the run"
                                    :msg "give the Challenger 1 tag and end the run"
                                    :async true
                                    :effect (effect (gain-tags :contestant eid 1)
                                                    (end-run))})]}

   "Eli 1.0"
   {:subroutines [end-the-run]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Eli 2.0"
   {:subroutines [{:msg "draw 1 card" :effect (effect (draw))}
                  end-the-run]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Endless EULA"
   {:subroutines [end-the-run]
    :challenger-abilities [(challenger-pay [:credit 1] 1)
                       (challenger-pay [:credit 6] 6)]}

   "Enforcer 1.0"
   {:additional-cost [:forfeit]
    :subroutines [discard-resource
                  (do-brain-damage 1)
                  {:label "Discard a console"
                   :prompt "Select a console to discard"
                   :choices {:req #(has-subtype? % "Console")}
                   :msg (msg "discard " (:title target))
                   :effect (effect (discard target))}
                  {:msg "discard all virtual radicles"
                   :effect (req (doseq [c (filter #(has-subtype? % "Virtual") (all-active-placed state :challenger))]
                                  (discard state side c)))}]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Envelope"
   {:subroutines [(do-net-damage 1)
                  end-the-run]}

   "Enigma"
   {:subroutines [{:msg "force the Challenger to lose 1 [Click] if able"
                   :effect challenger-loses-click}
                  end-the-run]}

   "Errand Boy"
   {:subroutines [(gain-credits-sub 1)
                  {:msg "draw 1 card" :effect (effect (draw))}]}

   "Excalibur"
   {:subroutines [{:label "The Challenger cannot make another run this turn"
                   :msg "prevent the Challenger from making another run"
                   :effect (effect (register-turn-flag! card :can-run nil))}]}

   "Executive Functioning"
   {:subroutines [(trace-ability 4 (do-brain-damage 1))]}

   "Fairchild"
   {:subroutines [end-the-run
                  (do-brain-damage 1)]
    :challenger-abilities [(challenger-break [:credit 4] 1)]}

   "Fairchild 1.0"
   {:subroutines [{:label "Force the Challenger to pay 1 [Credits] or discard an placed card"
                   :msg "force the Challenger to pay 1 [Credits] or discard an placed card"
                   :player :challenger
                   :prompt "Choose one"
                   :choices ["Pay 1 [Credits]" "Discard an placed card"]
                   :effect (req (if (= target "Pay 1 [Credits]")
                                  (do (pay state side card :credit 1)
                                      (system-msg state side "pays 1 [Credits]"))
                                  (resolve-ability state :challenger discard-placed card nil)))}]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Fairchild 2.0"
   {:subroutines [{:label "Force the Challenger to pay 2 [Credits] or discard an placed card"
                   :msg "force the Challenger to pay 2 [Credits] or discard an placed card"
                   :player :challenger
                   :prompt "Choose one"
                   :choices ["Pay 2 [Credits]" "Discard an placed card"]
                   :effect (req (if (= target "Pay 2 [Credits]")
                                  (do (pay state side card :credit 2)
                                      (system-msg state side "pays 2 [Credits]"))
                                  (resolve-ability state :challenger discard-placed card nil)))}
                  (do-brain-damage 1)]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Fairchild 3.0"
   {:subroutines [{:label "Force the Challenger to pay 3 [Credits] or discard an placed card"
                   :msg "force the Challenger to pay 3 [Credits] or discard an placed card"
                   :player :challenger
                   :prompt "Choose one"
                   :choices ["Pay 3 [Credits]" "Discard an placed card"]
                   :effect (req (if (= target "Pay 3 [Credits]")
                                  (do (pay state side card :credit 3)
                                      (system-msg state side "pays 3 [Credits]"))
                                  (resolve-ability state :challenger discard-placed card nil)))}
                  {:label "Do 1 brain damage or end the run"
                   :prompt "Choose one"
                   :choices ["Do 1 brain damage" "End the run"]
                   :msg (msg (lower-case target))
                   :effect (req (if (= target "Do 1 brain damage")
                                  (damage state side eid :brain 1 {:card card})
                                  (end-run state side)))}]
    :challenger-abilities [(challenger-break [:click 3] 3)]}

   "Fenris"
   {:effect take-bad-pub
    :subroutines [(do-brain-damage 1)
                  end-the-run]}

   "Fire Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}

   "Flare"
   {:subroutines [(trace-ability 6 {:label "Discard 1 hazard, do 2 meat damage, and end the run"
                                    :msg "discard 1 hazard, do 2 meat damage, and end the run"
                                    :async true
                                    :effect (effect (continue-ability
                                                     {:prompt "Select a piece of hazard to discard"
                                                      :label "Discard a piece of hazard"
                                                      :choices {:req #(is-type? % "Hazard")}
                                                      :msg (msg "discard " (:title target))
                                                      :effect (req (wait-for
                                                                     (discard state side target {:cause :subroutine})
                                                                     (do (damage state side eid :meat 2 {:unpreventable true
                                                                                                         :card card})
                                                                         (end-run state side))))
                                                      :cancel-effect (effect (damage eid :meat 2 {:unpreventable true :card card})
                                                                             (end-run))}
                                                     card nil))})]}

   "Free Lunch"
   {:abilities [(power-counter-ability {:label "Challenger loses 1 [Credits]"
                                        :msg "make the Challenger lose 1 [Credits]"
                                        :effect (effect (lose-credits :challenger 1))})]
    :subroutines [add-power-counter]}

   "Galahad"
   (grail-character end-the-run)

   "Gatekeeper"
   (let [draw {:async true
               :prompt "Draw how many cards?"
               :choices {:number (req 3)
                         :max (req 3)
                         :default (req 1)}
               :msg (msg "draw " target "cards")
               :effect (effect (draw eid target nil))}
         reveal-and-shuffle {:prompt "Reveal and shuffle up to 3 agendas"
                             :show-discard true
                             :choices {:req #(and (= "Contestant" (:side %))
                                                  (or (= [:discard] (:zone %))
                                                      (= [:hand] (:zone %)))
                                                  (is-type? % "Agenda"))
                                       :max (req 3)}
                             :effect (req (doseq [c targets]
                                            (move state :contestant c :deck))
                                          (shuffle! state :contestant :deck))
                             :cancel-effect (effect (shuffle! :deck))
                             :msg (msg "add "
                                       (str (join ", " (map :title targets)))
                                       " to R&D")}
         draw-reveal-shuffle {:async true
                              :label "Draw cards, reveal and shuffle agendas"
                              :effect (req (wait-for (resolve-ability state side draw card nil)
                                                     (continue-ability state side reveal-and-shuffle card nil)))}]
    {:strength-bonus (req (if (= :this-turn (:revealed card)) 6 0))
     :subroutines [draw-reveal-shuffle
                   end-the-run]})

   "Gemini"
   (constellation-character (do-net-damage 1))

   "Grim"
   {:effect take-bad-pub
    :subroutines [discard-resource]}

   "Guard"
   {:implementation "Prevent bypass is manual"
    :subroutines [end-the-run]}

   "Gutenberg"
   {:subroutines [(tag-trace 7)]
    :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))}

   "Gyri Labyrinth"
   {:implementation "Hand size is not restored if discarded or hidden after firing"
    :subroutines [{:req (req (:run @state))
                   :label "Reduce Challenger's maximum hand size by 2 until start of next Contestant turn"
                   :msg "reduce the Challenger's maximum hand size by 2 until the start of the next Contestant turn"
                   :effect (effect (lose :challenger :hand-size 2)
                                   (register-events {:contestant-turn-begins
                                                     {:msg "increase the Challenger's maximum hand size by 2"
                                                      :effect (effect (gain :challenger :hand-size 2)
                                                                      (unregister-events card))}} card))}]
    :events {:contestant-turn-begins nil}}

   "Hadrians Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}

   "Hailstorm"
   {:subroutines [{:label "Remove a card in the Heap from the game"
                   :prompt "Choose a card in the Challenger's Heap"
                   :choices (req (:discard challenger))
                   :msg (msg "remove " (:title target) " from the game")
                   :effect (effect (move :challenger target :rfg))}
                  end-the-run]}

   "Harvester"
   {:subroutines [{:label "Challenger draws 3 cards and discards down to maximum hand size"
                   :msg "make the Challenger draw 3 cards and discard down to their maximum hand size"
                   :effect (req (draw state :challenger 3)
                                (let [delta (- (count (get-in @state [:challenger :hand])) (hand-size state :challenger))]
                                  (when (pos? delta)
                                    (resolve-ability
                                      state :challenger
                                      {:prompt (msg "Select " delta " cards to discard")
                                       :player :challenger
                                       :choices {:max delta
                                                 :req #(in-hand? %)}
                                       :effect (req (doseq [c targets]
                                                      (discard state :challenger c))
                                                    (system-msg state :challenger
                                                                (str "discards " (join ", " (map :title targets)))))}
                                      card nil))))}]}

   "Himitsu-Bako"
   {:abilities [{:msg "add it to HQ"
                :cost [:credit 1]
                :effect (effect (move card :hand))}]
    :subroutines [end-the-run]}

   "Hive"
   {:abilities [{:label "Gain subroutines"
                 :msg   (msg "gain " (min 5 (max 0 (- 5 (:agenda-point contestant 0)))) " subroutines")}]
    :subroutines [end-the-run]}

   "Heimdall 1.0"
   {:subroutines [(do-brain-damage 1)
                  end-the-run]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Heimdall 2.0"
   {:subroutines [(do-brain-damage 1)
                  {:msg "do 1 brain damage and end the run" :effect (effect (damage eid :brain 1 {:card card}) (end-run))}
                  end-the-run]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Herald"
   {:flags {:rd-reveal (req true)}
    :subroutines [(gain-credits-sub 2)
                  {:label "Pay 1 [Credits] to place 1 advancement token on a card that can be advanced"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req can-be-advanced?}
                   :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    :access {:async true
             :req (req (not= (first (:zone card)) :discard))
             :effect (effect (show-wait-prompt :contestant "Challenger to decide to break Herald subroutines")
                             (continue-ability
                               :challenger {:optional
                                        {:player :challenger
                                         :prompt "You are encountering Herald. Allow its subroutines to fire?"
                                         :priority 1
                                         :yes-ability {:effect (effect (clear-wait-prompt :contestant)
                                                                       (play-subroutine :contestant eid {:card card :subroutine 0})
                                                                       (play-subroutine :contestant eid {:card card :subroutine 1}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :contestant)
                                                                      (effect-completed eid))}}}
                              card nil))}}

   "Holmegaard"
   {:subroutines [(trace-ability 4 {:label "Challenger cannot access any cards this run"
                                    :msg "stop the Challenger from accessing any cards this run"
                                    :effect (effect (prevent-access))})
                  {:label "Discard an characterbreaker"
                   :prompt "Choose an characterbreaker to discard"
                   :msg (msg "discard " (:title target))
                   :choices {:req #(and (placed? %)
                                        (has? % :subtype "Icebreaker"))}
                   :effect (effect (discard target {:cause :subroutine})
                                   (clear-wait-prompt :challenger))}]}

   "Hortum"
   (letfn [(hort [n] {:prompt "Choose a card to add to HQ with Hortum"
                      :async true
                      :choices (req (cancellable (:deck contestant) :sorted))
                      :msg "add 1 card to HQ from R&D"
                      :cancel-effect (req (shuffle! state side :deck)
                                          (system-msg state side (str "shuffles R&D"))
                                          (effect-completed state side eid))
                      :effect (req (move state side target :hand)
                                   (if (< n 2)
                                     (continue-ability state side (hort (inc n)) card nil)
                                     (do (shuffle! state side :deck)
                                         (system-msg state side (str "shuffles R&D"))
                                         (effect-completed state side eid))))})]
     {:advanceable :always
      :subroutines [{:label "Gain 1 [Credits] (Gain 4 [Credits])"
                     :msg (msg "gain " (if (wonder-sub card 3) "4" "1") " [Credits]")
                     :effect (effect (gain-credits :contestant (if (wonder-sub card 3) 4 1)))}
                    {:label "End the run (Search R&D for up to 2 cards and add them to HQ, shuffle R&D, end the run)"
                     :async true
                     :effect (req (if (wonder-sub card 3)
                                    (wait-for
                                      (resolve-ability state side (hort 1) card nil)
                                      (do (end-run state side)
                                          (system-msg state side
                                                      (str "uses Hortum to add 2 cards to HQ from R&D, "
                                                           "shuffle R&D, and end the run"))))
                                    (do (end-run state side)
                                        (system-msg state side (str "uses Hortum to end the run"))
                                        (effect-completed state side eid))))}]})

   "Hourglass"
   {:subroutines [{:msg "force the Challenger to lose 1 [Click] if able"
                   :effect challenger-loses-click}]}

   "Howler"
   (let [character-index (fn [state i] (first (keep-indexed #(when (= (:cid %2) (:cid i)) %1)
                                                      (get-in @state (cons :contestant (:zone i))))))]
     {:subroutines
      [{:label "Place a piece of Bioroid Character from HQ or Archives"
        :prompt "Place Character from HQ or Archives?"
        :choices ["HQ" "Archives"]
        :effect (req (let [fr target]
                       (resolve-ability state side
                                        {:prompt "Choose a Bioroid Character to place"
                                         :choices (req (filter #(and (character? %)
                                                                     (has-subtype? % "Bioroid"))
                                                               ((if (= fr "HQ") :hand :discard) contestant)))
                                         :effect (req (let [newcharacter (assoc target :zone (:zone card) :revealed true)
                                                            hndx (character-index state card)
                                                            characters (get-in @state (cons :contestant (:zone card)))
                                                            newcharacters (apply conj (subvec characters 0 hndx) newcharacter (subvec characters hndx))]
                                                        (swap! state assoc-in (cons :contestant (:zone card)) newcharacters)
                                                        (swap! state update-in (cons :contestant (:zone target))
                                                               (fn [coll] (remove-once #(= (:cid %) (:cid target)) coll)))
                                                        (update! state side (assoc card :howler-target newcharacter))
                                                        (card-init state side newcharacter {:resolve-effect false
                                                                                      :init-data true})
                                                        (trigger-event state side :contestant-place newcharacter)))} card nil)))}]
      :events {:run-ends {:req (req (:howler-target card))
                          :effect (effect (discard card {:cause :self-discard})
                                          (hide (get-card state (:howler-target card))))}}})

   "Hudson 1.0"
   {:subroutines [{:msg "prevent the Challenger from accessing more than 1 card during this run"
                   :effect (effect (max-access 1))}]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Hunter"
   {:subroutines [(tag-trace 3)]}

   "Hydra"
   (letfn [(otherwise-tag [message ability]
             {:msg (msg (if (is-tagged? @state) message "give the Challenger 1 tag"))
              :label (str (capitalize message) " if the Challenger is tagged; otherwise, give the Challenger 1 tag")
              :async true
              :effect (req (if (is-tagged? @state)
                             (ability state :challenger eid card nil)
                             (gain-tags state :challenger eid 1)))})]
     {:subroutines [(otherwise-tag "do 3 net damage"
                                   (req (damage state :challenger :net 3 {:card card})))
                    (otherwise-tag "gain 5 [Credits]"
                                   (req (gain-credits state :contestant 5)
                                        (effect-completed state side eid)))
                    (otherwise-tag "end the run"
                                   (req (end-run state side eid)))]})

   "Ice Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}

   "Ichi 1.0"
   {:subroutines [discard-resource
                  (trace-ability 1 {:label "Give the Challenger 1 tag and do 1 brain damage"
                                    :msg "give the Challenger 1 tag and do 1 brain damage"
                                    :async true
                                    :effect (req (wait-for (damage state :challenger :brain 1 {:card card})
                                                           (gain-tags state :contestant eid 1)))})]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Ichi 2.0"
   {:subroutines [discard-resource
                  (trace-ability 3 {:label "Give the Challenger 1 tag and do 1 brain damage"
                                    :msg "give the Challenger 1 tag and do 1 brain damage"
                                    :async true
                                    :effect (req (wait-for (damage state :challenger :brain 1 {:card card})
                                                           (gain-tags state :contestant eid 1)))})]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Inazuma"
   {:abilities [{:msg "prevent the Challenger from breaking subroutines on the next piece of Character they encounter this run"}
                {:msg "prevent the Challenger from jacking out until after the next piece of Character"
                 :effect (effect (register-events
                                   {:pass-character {:effect (req (swap! state update-in [:run] dissoc :prevent-jack-out)
                                                            (unregister-events state side card))}} card)
                                 (prevent-jack-out))}]}

   "Information Overload"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:tag challenger 0) " subroutines")}
                (tag-trace 1)]
    :subroutines [discard-placed]}

   "IP Block"
   {:abilities [(assoc (give-tags 1)
                  :req (req (seq (filter #(has-subtype? % "AI") (all-active-placed state :challenger))))
                  :label "Give the Challenger 1 tag if there is an placed AI")]
    :subroutines [(tag-trace 3)
                  end-the-run-if-tagged]}

   "IQ"
   {:effect (req (add-watch state (keyword (str "iq" (:cid card)))
                            (fn [k ref old new]
                              (let [handsize (count (get-in new [:contestant :hand]))]
                                (when (not= (count (get-in old [:contestant :hand])) handsize)
                                  (update! ref side (assoc (get-card ref card) :strength-bonus handsize))
                                  (update-character-strength ref side (get-card ref card)))))))
    :subroutines [end-the-run]
    :strength-bonus (req (count (:hand contestant)))
    :reveal-cost-bonus (req (count (:hand contestant)))
    :leave-play (req (remove-watch state (keyword (str "iq" (:cid card)))))}

   "Ireress"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:bad-publicity contestant 0) " subroutines")}]
    :subroutines [{:msg "make the Challenger lose 1 [Credits]"
                   :effect (effect (lose-credits :challenger 1))}]}

   "Its a Trap!"
   {:expose {:msg "do 2 net damage"
             :async true
             :effect (effect (damage eid :net 2 {:card card}))}
    :subroutines [(assoc discard-placed :effect (req (discard state side target {:cause :subroutine})
                                                      (when current-character
                                                        (no-action state side nil)
                                                        (continue state side nil))
                                                      (discard state side card)))]}

   "Janus 1.0"
   {:subroutines [(do-brain-damage 1)]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Jua"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "prevent the Challenger from placing cards for the rest of the turn"
                 :effect (effect (register-turn-flag! card :challenger-lock-place (constantly true)))}]
    :subroutines [{:label "Choose 2 placed Challenger cards, if able. The Challenger must add 1 of those to the top of the Stack."
                   :req (req (>= (count (all-placed state :challenger)) 2))
                   :async true
                   :prompt "Select 2 placed Challenger cards"
                   :choices {:req #(and (= (:side %) "Challenger")
                                        (placed? %))
                             :max 2
                             :all true}
                   :msg (msg "add either " (card-str state (first targets)) " or " (card-str state (second targets)) " to the Stack")
                   :effect (req (when (= (count targets) 2)
                                     (show-wait-prompt state :contestant "Challenger to decide which card to move")
                                     (continue-ability
                                       state :challenger
                                        {:player :challenger
                                         :priority 1
                                         :prompt "Select a card to move to the Stack"
                                         :choices targets ;{:req (fn [x] (some #(= % x) targets))} - Alternative version
                                         :effect (req (let [c target]
                                                        (clear-wait-prompt state :contestant)
                                                        (move state :challenger c :deck {:front true})
                                                        (system-msg state :challenger (str "selected " (card-str state c) " to move to the Stack"))))}
                                         card nil)))}]}

   "Kakugo"
   {:events {:pass-character {:async true
                        :req (req (= target card))
                        :msg "do 1 net damage"
                        :effect (effect (damage eid :net 1 {:card card}))}}
    :subroutines [end-the-run]}

   "Kamali 1.0"
   (letfn [(better-name [kind] (if (= "hazard" kind) "piece of hazard" kind))
           (challenger-discard [kind]
             {:prompt (str "Select an placed " (better-name kind) " to discard")
              :label (str "Discard an placed " (better-name kind))
              :msg (msg "discard " (:title target))
              :async true
              :choices {:req #(and (placed? %)
                                   (is-type? % (capitalize kind)))}
              :cancel-effect (effect (system-msg (str "fails to discard an placed " (better-name kind)))
                                     (effect-completed eid))
              :effect (effect (discard eid target {:cause :subroutine}))})
           (sub-map [kind]
             {:player :challenger
              :async true
              :prompt "Choose one"
              :choices ["Take 1 brain damage" (str "Discard an placed " (better-name kind))]
              :effect (req (if (= target "Take 1 brain damage")
                             (do (system-msg state :contestant "uses Kamali 1.0 to give the Challenger 1 brain damage")
                                 (damage state :challenger eid :brain 1 {:card card}))
                             (continue-ability state :challenger (challenger-discard kind) card nil)))})
           (brain-discard [kind]
             {:label (str "Force the Challenger to take 1 brain damage or discard an placed " (better-name kind))
              :msg (str "force the Challenger to take 1 brain damage or discard an placed " (better-name kind))
              :async true
              :effect (req (show-wait-prompt state :contestant "Challenger to decide on Kamali 1.0 action")
                           (wait-for (resolve-ability state side (sub-map kind) card nil)
                                     (clear-wait-prompt state :contestant)))})]
     {:subroutines [(brain-discard "radicle")
                    (brain-discard "hazard")
                    (brain-discard "resource")]
      :challenger-abilities [(challenger-break [:click 1] 1)]})

   "Kitsune"
   {:subroutines [{:prompt "Select a card in HQ to force access"
                   :choices {:req in-hand?}
                   :label "Force the Challenger to access a card in HQ"
                   :msg (msg "force the Challenger to access " (:title target))
                   :effect (req (discard state side card)
                                (wait-for (trigger-event-sync state side :pre-access :hq)
                                          (wait-for (access-card state side target)
                                                    (let [from-hq (dec (access-count state side :hq-access))]
                                                      (continue-ability
                                                        state :challenger
                                                        (access-helper-hq
                                                          state from-hq
                                                          ;; access-helper-hq uses a set to keep track of which cards have already
                                                          ;; been accessed. by adding HQ root's contents to this set, we make the challenger
                                                          ;; unable to access those cards, as Kitsune intends.
                                                          (conj (set (get-in @state [:contestant :locales :hq :content])) target))
                                                        card nil)))))}]}

   "Komainu"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand challenger)) " subroutines")}]
    :subroutines [(do-net-damage 1)]}

   "Lab Dog"
   {:subroutines [(assoc discard-hazard :label "Force the Challenger to discard an placed piece of hazard"
                                        :player :challenger
                                        :msg (msg "force the Challenger to discard " (:title target))
                                        :effect (req (discard state side target)
                                                     (when current-character
                                                       (no-action state side nil)
                                                       (continue state side nil))
                                                     (discard state side card)))]}

   "Lancelot"
   (grail-character discard-resource)

   "Little Engine"
   {:subroutines [end-the-run
                  {:msg "make the Challenger gain 5 [Credits]"
                   :effect (effect (gain-credits :challenger 5))}]}

   "Lockdown"
   {:subroutines [{:label "The Challenger cannot draw cards for the remainder of this turn"
                   :msg "prevent the Challenger from drawing cards"
                   :effect (effect (prevent-draw))}]}

   "Loki"
   {:implementation "Encounter effects not implemented"
    :subroutines [{:label "End the run unless the Challenger shuffles their Grip into the Stack"
                   :effect (req (if (zero? (count (:hand challenger)))
                                    (do (end-run state side)
                                        (system-msg state :contestant (str "uses Loki to end the run")))
                                    (do (show-wait-prompt state :contestant "Challenger to decide to shuffle their Grip into the Stack")
                                        (resolve-ability state :challenger
                                          {:optional
                                           {:prompt "Reshuffle your Grip into the Stack?"
                                            :player :challenger
                                            :yes-ability {:effect (req (doseq [c (:hand challenger)]
                                                                         (move state :challenger c :deck))
                                                                       (shuffle! state :challenger :deck)
                                                                       (system-msg state :challenger (str "shuffles their Grip into their Stack"))
                                                                       (clear-wait-prompt state :contestant))}
                                            :no-ability {:effect (effect (end-run)
                                                                         (system-msg :challenger (str "doesn't shuffle their Grip into their Stack. Loki ends the run"))
                                                                         (clear-wait-prompt :contestant))}}}
                                         card nil))))}]}

   "Lotus Field"
   {:subroutines [end-the-run]
    :flags {:cannot-lower-strength true}}

   "Lycan"
   (montestanth-character "Sentry" "Code Gate" discard-resource)

   "Macrophage"
   {:subroutines [(trace-ability 4 {:label "Purge virus counters"
                                    :msg "purge virus counters"
                                    :effect (effect (purge))})
                  (trace-ability 3 {:label "Discard a virus"
                                    :prompt "Choose a virus to discard"
                                    :msg (msg "discard " (:title target))
                                    :choices {:req #(and (placed? %)
                                                         (has? % :subtype "Virus"))}
                                    :effect (effect (discard target {:cause :subroutine})
                                                    (clear-wait-prompt :challenger))})
                  (trace-ability 2 {:label "Remove a virus in the Heap from the game"
                                    :prompt "Choose a virus in the Heap to remove from the game"
                                    :choices (req (cancellable (filter #(has? % :subtype "Virus") (:discard challenger)) :sorted))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (effect (move :challenger target :rfg))})
                  (trace-ability 1 end-the-run)]}

   "Magnet"
   (letfn [(disable-hosted [state side c]
             (doseq [hc (:hosted (get-card state c))]
               (unregister-events state side hc)
               (update! state side (dissoc hc :abilities))))]
     {:async true
      :effect (req (let [magnet card]
                     (wait-for (resolve-ability
                                 state side
                                 {:req (req (some #(some (fn [h] (card-is? h :type "Resource")) (:hosted %))
                                                  (remove-once #(= (:cid %) (:cid magnet))
                                                               (filter character? (all-placed state contestant)))))
                                  :prompt "Select a Resource to host on Magnet"
                                  :choices {:req #(and (card-is? % :type "Resource")
                                                       (character? (:host %))
                                                       (not= (:cid (:host %)) (:cid magnet)))}
                                  :effect (effect (host card target))}
                                 card nil)
                               (disable-hosted state side card))))
      :hide-effect {:req (req (not-empty (:hosted card)))
                     :effect (req (doseq [c (get-in card [:hosted])]
                                    (card-init state side c {:resolve-effect false})))}
      :events {:challenger-place {:req (req (= (:cid card) (:cid (:host target))))
                                :effect (req (disable-hosted state side card)
                                          (update-character-strength state side card))}}
      :subroutines [end-the-run]})

   "Mamba"
   {:abilities [(power-counter-ability (do-net-damage 1))]
    :subroutines [(do-net-damage 1)
                  (do-psi add-power-counter)]}

   "Marker"
   {:subroutines [{:label "Give the next Character encountered \"End the run\" for the remainder of the run"
                   :msg (msg "give the next Character encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}

   "Markus 1.0"
   {:subroutines [discard-placed end-the-run]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Matrix Analyzer"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Place 1 advancement token on a card that can be advanced"
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req can-be-advanced?}
                 :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1))}]
    :subroutines [(tag-trace 2)]}

   "Mausolus"
   {:advanceable :always
    :subroutines [{:label "Gain 1 [Credits] (Gain 3 [Credits])"
                   :msg (msg "gain " (if (wonder-sub card 3) 3 1) "[Credits]")
                   :effect (effect (gain-credits (if (wonder-sub card 3) 3 1)))}
                  {:label "Do 1 net damage (Do 3 net damage)"
                   :async true
                   :msg (msg "do " (if (wonder-sub card 3) 3 1) " net damage")
                   :effect (effect (damage eid :net (if (wonder-sub card 3) 3 1) {:card card}))}
                  {:label "Give the Challenger 1 tag (and end the run)"
                   :async true
                   :msg (msg "give the Challenger 1 tag"
                             (when (wonder-sub card 3)
                               " and end the run"))
                   :effect (req (gain-tags state :contestant eid 1)
                                (when (wonder-sub card 3)
                                  (end-run state side)))}]}

   "Masvingo"
   {:implementation "Number of subs is manual"
    :advanceable :always
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :effect (effect (add-prop card :advance-counter 1))
    :subroutines [end-the-run]}

   "Merlin"
   (grail-character (do-net-damage 2))

   "Meridian"
   {:subroutines [{:label "Gain 4 [Credits] and end the run, unless the challenger adds Meridian to their score area as an agenda worth -1 agenda points"
                   :async true
                   :effect (req (show-wait-prompt state :contestant "Challenger to choose an option for Meridian")
                                (continue-ability
                                  state :challenger
                                  {:prompt "Choose one"
                                   :choices ["End the run" "Add Meridian to score area"]
                                   :player :challenger
                                   :async true
                                   :effect (req (if (= target "End the run")
                                                  (do (system-msg state :contestant (str "uses Meridian to gain 4 [Credits] and end the run"))
                                                      (clear-wait-prompt state :contestant)
                                                      (gain-credits state :contestant 4)
                                                      (end-run state :challenger eid))
                                                  (do (system-msg state :challenger (str "adds Meridian to their score area as an agenda worth -1 agenda points"))
                                                      (clear-wait-prompt state :contestant)
                                                      (wait-for (as-agenda state :challenger card -1)
                                                                (when current-character
                                                                  (no-action state side nil)
                                                                  (continue state side nil))
                                                                (effect-completed state side eid)))))}
                                  card nil))}]}

   "Meru Mati"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))}

   "Metamontestanth"
   {:subroutines [{:label "Swap two Character or swap two placed non-Character"
                   :msg "swap two Character or swap two placed non-Character"
                   :async true
                   :prompt "Choose one"
                   :choices ["Swap two Character" "Swap two non-Character"]
                   :effect (req (if (= target "Swap two Character")
                                  (continue-ability
                                    state side
                                    {:prompt "Select the two Character to swap"
                                     :async true
                                     :choices {:req #(and (placed? %) (character? %)) :max 2 :all true}
                                     :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                     :effect (req (when (= (count targets) 2)
                                                    (swap-character state side (first targets) (second targets))
                                                    (effect-completed state side eid)))}
                                    card nil)
                                  (continue-ability
                                    state side
                                    {:prompt "Select the two cards to swap"
                                     :async true
                                     :choices {:req #(and (placed? %) (not (character? %))) :max 2 :all true}
                                     :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                     :effect (req (when (= (count targets) 2)
                                                    (swap-placed state side (first targets) (second targets))
                                                    (effect-completed state side eid)))}
                                    card nil)))}]}

   "Mganga"
   {:subroutines [(do-psi {:label "do 2 net damage"
                           :async true
                           :player :contestant
                           :effect (req (wait-for (damage state :contestant :net 2 {:card card})
                                                  (discard state :contestant eid card nil)))}
                          {:label "do 1 net damage"
                           :async true
                           :player :contestant
                           :effect (req (wait-for (damage state :contestant :net 1 {:card card})
                                                  (discard state :contestant eid card nil)))})]}

   "Mind Game"
   {:subroutines [(do-psi {:label "Redirect the run to another locale"
                           :player :contestant
                           :prompt "Choose a locale"
                           :choices (req (remove #{(-> @state :run :locale central->name)} locales))
                           :msg (msg "redirect the run to " target)
                           :effect (req (let [dest (locale->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc % :position (count (get-in contestant (conj dest :characters)))
                                                           :locale (rest dest)))))})]
    :challenger-abilities [{:label "Add an placed card to the bottom of your Stack"
                        :prompt "Choose one of your placed cards"
                        :choices {:req #(and (placed? %)
                                             (= (:side %) "Challenger"))}
                        :effect (effect (move target :deck)
                                        (system-msg :challenger (str "adds " (:title target) " to the bottom of their Stack")))}]}

   "Minelayer"
   {:subroutines [{:msg "place an Character from HQ"
                   :choices {:req #(and (character? %)
                                        (in-hand? %))}
                   :prompt "Choose an Character to place from HQ"
                   :effect (req (contestant-place state side target (zone->name (first (:locale run))) {:no-place-cost true}))}]}

   "Formicary"
   {:optional {:prompt "Move Formicary?"
               :req (req (and (:run @state)
                   (zero? (:position run))
                   (not (contains? run :contestant-phase-43))
                   (not (contains? run :successful))))
               :yes-ability {:msg "reveal and move Formicary. The Challenger is now approaching Formicary."
                             :effect (req (move state side card
                                                [:locales (first (:locale run)) :characters]
                                                {:front true})
                                          (swap! state assoc-in [:run :position] 1))}
               :no-ability {:msg "reveal Formicary without moving it"}}
    :subroutines [{:label "End the run unless the Challenger suffers 2 net damage"
                   :async true
                   :effect (req (wait-for (resolve-ability
                                           state :challenger
                                           {:optional
                                            {:prompt "Suffer 2 net damage? (If not, end the run)"
                                             :yes-ability {:async true
                                                           :msg "let the Challenger suffer 2 net damage"
                                                           :effect (effect (damage eid :net 2 {:card card :unpreventable true}))}
                                             :no-ability end-the-run}}
                                           card nil)))}]}

   "Mirāju"
   {:abilities [{:label "Challenger broke subroutine: Redirect run to Archives"
                 :msg "make the Challenger continue the run on Archives. Mirāju is hidden"
                 :effect (req (swap! state update-in [:run]
                                     #(assoc % :position (count (get-in contestant [:locales :archives :characters]))
                                               :locale [:archives]))
                              (hide state side card))}]
    :subroutines [{:label "Draw 1 card, then shuffle 1 card from HQ into R&D"
                   :effect (req (wait-for (resolve-ability
                                            state side
                                            {:optional
                                             {:prompt "Draw 1 card?"
                                              :yes-ability {:async true
                                                            :msg "draw 1 card"
                                                            :effect (effect (draw eid 1 nil))}}}
                                            card nil)
                                          (resolve-ability
                                            state side
                                            {:prompt "Choose 1 card in HQ to shuffle into R&D"
                                             :choices {:req #(and (in-hand? %) (= (:side %) "Contestant"))}
                                             :msg "shuffle 1 card in HQ into R&D"
                                             :effect (effect (move target :deck)
                                                             (shuffle! :deck))}
                                            card nil)))}]}

   "Mlinzi"
   (letfn [(net-or-discard [net-dmg mill-cnt]
             {:label (str "Do " net-dmg " net damage")
              :effect (req (show-wait-prompt state :contestant "Challenger to choose an option for Mlinzi")
                           (resolve-ability
                             state :challenger
                             {:prompt "Take net damage or discard cards from the stack?"
                              :choices [(str "Take " net-dmg " net damage")
                                        (str "Discard the top " mill-cnt " cards of the stack")]
                              :effect (req (if (.startsWith target "Take")
                                             (do (system-msg state :contestant
                                                             (str "uses Mlinzi to do "
                                                                  net-dmg " net damage"))
                                                 (clear-wait-prompt state :contestant)
                                                 (damage state :challenger eid :net net-dmg {:card card}))
                                             (do (system-msg state :contestant
                                                             (str "uses Mlinzi to discard "
                                                                  (join ", " (map :title (take mill-cnt (:deck challenger))))
                                                                  " from the challenger's stack"))
                                                 (clear-wait-prompt state :contestant)
                                                 (mill state :challenger mill-cnt))))}
                             card nil))})]
     {:subroutines [(net-or-discard 1 2)
                    (net-or-discard 2 3)
                    (net-or-discard 3 4)]})

   "Mother Goddess"
   (let [ab (effect (update! (let [subtype (->> (mapcat :characters (flatten (seq (:locales contestant))))
                                                (filter #(and (revealed? %)
                                                              (not= (:cid card) (:cid %))))
                                                (mapcat #(split (:subtype %) #" - "))
                                                (cons "Mythic")
                                                distinct
                                                (join " - "))]
                               (assoc card :subtype-target (remove-subtypes subtype "Mythic")
                                           :subtype subtype))))
         mg {:req (req (character? target))
             :effect ab}]
     {:effect ab
      :subroutines [end-the-run]
      :events {:reveal mg
               :card-moved mg
               :hide mg
               :character-subtype-changed mg}})

   "Muckraker"
   {:effect take-bad-pub
    :subroutines [(tag-trace 1)
                  (tag-trace 2)
                  (tag-trace 3)
                  end-the-run-if-tagged]}

   "Najja 1.0"
   {:subroutines [end-the-run]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Nebula"
   (space-character discard-resource)

   "Negotiator"
   {:subroutines [(gain-credits-sub 2)
                  discard-resource]
    :challenger-abilities [(challenger-break [:credit 2] 1)]}

   "Nerine 2.0"
   {:subroutines [{:label "Do 1 brain damage and Contestant may draw 1 card"
                   :async true
                   :msg "do 1 brain damage"
                   :effect (req (wait-for (damage state :challenger :brain 1 {:card card})
                                          (resolve-ability
                                            state side
                                            {:optional
                                             {:prompt "Draw 1 card?"
                                              :yes-ability {:async true
                                                            :msg "draw 1 card"
                                                            :effect (effect (draw eid 1 nil))}}}
                                            card nil)))}]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Neural Katana"
   {:subroutines [(do-net-damage 3)]}

   "News Hound"
   {:subroutines [(tag-trace 3)
                  {:label "End the run if a Current is active"
                   :req (req (or (not (empty? (challenger :current)))
                                 (not (empty? (contestant :current)))))
                   :effect (effect (end-run)) :msg "end the run"}]}

   "NEXT Bronze"
   {:subroutines [end-the-run]
    :strength-bonus (req (next-character-count contestant))
    :events (let [nb {:req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "NEXT")))
                      :effect (effect (update-character-strength card))}]
              {:reveal nb
               :hide nb
               :discard nb
               :card-moved nb})}

   "NEXT Diamond"
   {:reveal-cost-bonus (req (- (next-character-count contestant)))
    :subroutines [(do-brain-damage 1)
                  {:prompt "Select a card to discard"
                   :label "Discard 1 placed Challenger card"
                   :msg (msg "discard " (:title target))
                   :choices {:req #(and (placed? %)
                                        (= (:side %) "Challenger"))}
                   :async true
                   :effect (req (discard state side eid target {:cause :subroutine}))}]}

   "NEXT Gold"
   {:subroutines [{:label "Do 1 net damage for each revealed NEXT character"
                   :msg (msg "do " (next-character-count contestant) " net damage")
                   :effect (effect (damage eid :net (next-character-count contestant) {:card card}))}
                  discard-resource]}

   "NEXT Opal"
   {:subroutines [{:label "Place a card from HQ, paying all costs"
                   :prompt "Choose a card in HQ to place"
                   :priority true
                   :choices {:req #(and (not (is-type? % "Operation"))
                                        (in-hand? %)
                                        (= (:side %) "Contestant"))}
                   :effect (effect (contestant-place target nil))
                   :msg (msg (contestant-place-msg target))}]}

   "NEXT Sapphire"
   {:subroutines [{:label "Draw up to X cards"
                   :prompt "Draw how many cards?"
                   :msg (msg "draw " target " cards")
                   :choices {:number (req (next-character-count contestant))
                             :default (req 1)}
                   :async true
                   :effect (effect (draw eid target nil))}
                  {:label "Add up to X cards from Archives to HQ"
                   :prompt "Select cards to add to HQ"
                   :show-discard  true
                   :choices {:req #(and (= "Contestant" (:side %))
                                        (= [:discard] (:zone %)))
                             :max (req (next-character-count contestant))}
                   :effect (req (doseq [c targets]
                                  (move state side c :hand)))
                   :msg (msg "add "
                             (let [seen (filter :seen targets)
                                   m (count (filter #(not (:seen %)) targets))]
                               (str (join ", " (map :title seen))
                                    (when (pos? m)
                                      (str (when-not (empty? seen) " and ")
                                           (quantify m "unseen card")))))
                             " to HQ")}
                  {:label "Shuffle up to X cards from HQ into R&D"
                   :prompt "Select cards to shuffle into R&D"
                   :choices {:req #(and (= "Contestant" (:side %))
                                        (= [:hand] (:zone %)))
                             :max (req (next-character-count contestant))}
                   :effect (req (doseq [c targets]
                                  (move state :contestant c :deck))
                                (shuffle! state :contestant :deck))
                   :cancel-effect (effect (shuffle! :contestant :deck))
                   :msg (msg "shuffle " (count targets) " cards from HQ into R&D")}]}

   "NEXT Silver"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain "
                           (count (filter #(and (is-type? % "Character")
                                                (has-subtype? % "NEXT"))
                                          (all-active-placed state :contestant)))
                           " subroutines")}]
    :subroutines [end-the-run]}

   "Nightdancer"
   {:subroutines [{:label (str "The Challenger loses [Click], if able. "
                               "You have an additional [Click] to spend during your next turn.")
                   :msg (str "force the challenger to lose a [Click], if able. "
                             "Contestant gains an additional [Click] to spend during their next turn")
                   :effect (req (lose state :challenger :click 1)
                                (swap! state update-in [:contestant :extra-click-temp] (fnil inc 0)))}]}

   "Oduduwa"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Place 1 advancement counter on Oduduwa"
                 :msg (msg "place 1 advancement counter on Oduduwa")
                 :effect (req (add-prop state side card :advance-counter 1 {:placed true}))}
                {:label "Place X advancement token on another piece of character"
                 :msg (msg "place " (get-counters card :advancement) " advancement token on " (card-str state target))
                 :choices {:req character?
                           :not-self true}
                 :effect (req (add-prop state side target :advance-counter (get-counters card :advancement) {:placed true}))}]
    :subroutines [end-the-run]}

   "Orion"
   (implementation-note "\"Resolve a subroutine...\" subroutine is not implemented"
                        (space-character discard-resource end-the-run))

   "Otoroshi"
   {:subroutines [{:async true
                   :label "Place 3 advancement tokens on placed card"
                   :msg "place 3 advancement tokens on placed card"
                   :prompt "Choose an placed Contestant card"
                   :choices {:req #(and (= (:side %) "Contestant")
                                        (placed? %))}
                   :effect (req (let [c target
                                      title (if (:revealed c)
                                              (:title c)
                                              "selected unrevealed card")]
                                  (add-counter state side c :advancement 3)
                                  (show-wait-prompt state side "Challenger to resolve Otoroshi")
                                  (continue-ability
                                    state side
                                    {:player :challenger
                                     :async true
                                     :prompt (str "Access " title " or pay 3 [Credits]?")
                                     :choices (concat ["Access card"]
                                                      (when (>= (:credit challenger) 3)
                                                        ["Pay 3 [Credits]"]))
                                     :msg (msg "force the Challenger to "
                                               (if (= target "Access card")
                                                 (str "access " title)
                                                 "pay 3 [Credits]"))
                                     :effect (req (clear-wait-prompt state :contestant)
                                                  (if (= target "Access card")
                                                    (access-card state :challenger eid c)
                                                    (pay-sync state :challenger eid card :credit 3)))}
                                    card nil)))}]}

   "Owl"
   {:subroutines [{:choices {:req #(and (placed? %)
                                        (is-type? % "Resource"))}
                   :label "Add placed resource to the top of the Challenger's Stack"
                   :msg "add an placed resource to the top of the Challenger's Stack"
                   :effect (effect (move :challenger target :deck {:front true})
                                   (system-msg (str "adds " (:title target) " to the top of the Challenger's Stack")))}]}

   "Pachinko"
   {:subroutines [end-the-run-if-tagged]}

   "Paper Wall"
   {:implementation "Discard on break is manual"
    :subroutines [end-the-run]}

   "Peeping Tom"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (= current-character card))
                 :label "Name a card type and reveal all cards in the Challenger's Grip"
                 :prompt "Choose a card type"
                 :choices ["Event" "Hazard" "Resource" "Radicle"]
                 :effect (req (let [n (count (filter #(is-type? % target) (:hand challenger)))]
                                (system-msg state side (str "uses Peeping Tom to name " target ", then reveals "
                                                            (join ", " (map :title (:hand challenger)))
                                                            " in the Challenger's Grip. Peeping Tom gains " n " subroutines"))))}]
    :challenger-abilities [{:label "End the run"
                        :effect (req (end-run state :challenger)
                                     (system-msg state :challenger "chooses to end the run"))}
                       {:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :challenger "chooses to take 1 tag from Peeping Tom")
                                     (gain-tags state :challenger eid 1))}]}

   "Pop-up Window"
   {:implementation "Encounter effect is manual. Challenger choice is not implemented"
    :abilities [(gain-credits-sub 1)]
    :subroutines [end-the-run]
    :challenger-abilities [(challenger-pay [:credit 1] 1)]}

   "Pup"
   {:subroutines [(do-net-damage 1)]
    :challenger-abilities [(challenger-pay [:credit 1] 1)]}

   "Quandary"
   {:subroutines [end-the-run]}

   "Quicksand"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (and this-locale (= (dec (:position run)) (character-index state card))))
                 :label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (update-all-character))}]
    :subroutines [end-the-run]
    :strength-bonus (req (get-counters card :power))}

   "Rainbow"
   {:subroutines [end-the-run]}

   "Ravana 1.0"
   {:subroutines [{:label "Resolve a subroutine on another piece of revealed bioroid Character"
                   :choices {:req #(and (revealed? %) (character? %) (has-subtype? % "Bioroid"))}
                   :msg (msg "resolve a subroutine on " (:title target))}]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Red Tape"
   {:subroutines [{:label "Give +3 strength to all Character for the remainder of the run"
                   :msg "give +3 strength to all Character for the remainder of the run"
                   :effect (effect (register-events
                                     {:pre-character-strength {:effect (effect (character-strength-bonus 3 target))}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card)
                                   (update-all-character))}]
    :events {:pre-character-strength nil :run-ends nil}}

   "Resistor"
   {:effect (req (add-watch state (keyword (str "resistor" (:cid card)))
                            (fn [k ref old new]
                              (let [tags (get-in new [:challenger :tag])]
                                (when (not= (get-in old [:challenger :tag]) tags)
                                  (update! ref side (assoc (get-card ref card) :strength-bonus tags))
                                  (update-character-strength ref side (get-card ref card)))))))
    :strength-bonus (req (:tag challenger))
    :leave-play (req (remove-watch state (keyword (str "resistor" (:cid card)))))
    :subroutines [(trace-ability 4 end-the-run)]}

   "Rototurret"
   {:subroutines [discard-resource end-the-run]}

   "Sadaka"
   (let [maybe-draw-effect
         {:async true
          :effect (req (show-wait-prompt state :challenger "Contestant to decide on Sadaka card draw action")
                       (continue-ability
                         state side
                         {:optional
                          {:player :contestant
                           :prompt "Draw 1 card?"
                           :yes-ability
                           {:async true
                            :effect (effect (clear-wait-prompt :challenger)
                                            (draw eid 1 nil))
                            :msg "draw 1 card"}
                           :no-ability {:effect (effect (clear-wait-prompt :challenger)
                                                        (effect-completed eid))}}}
                         card nil))}]
     {:subroutines [{:label "Look at the top 3 cards of R&D"
                     :req (req (not-empty (:deck contestant)))
                     :async true
                     :effect (req (let [top-cards (take 3 (:deck contestant))
                                        top-names (map :title top-cards)]
                                    (show-wait-prompt state :challenger "Contestant to decide on Sadaka R&D card actions")
                                    (continue-ability
                                      state side
                                      {:prompt (str "Top 3 cards of R&D: " (clojure.string/join ", " top-names))
                                       :choices ["Arrange cards" "Shuffle R&D"]
                                       :async true
                                       :effect
                                       (req (if (= target "Arrange cards")
                                              (wait-for
                                                (resolve-ability state side (reorder-choice :contestant top-cards) card nil)
                                                (do
                                                  (system-msg state :contestant (str "rearranges the top "
                                                                               (quantify (count top-cards) "card")
                                                                               " of R&D"))
                                                  (clear-wait-prompt state :challenger)
                                                  (continue-ability state side maybe-draw-effect card nil)))
                                              (do
                                                (shuffle! state :contestant :deck)
                                                (system-msg state :contestant (str "shuffles R&D"))
                                                (clear-wait-prompt state :challenger)
                                                (continue-ability state side maybe-draw-effect card nil))))}
                                      card nil)))}

                    {:label "Discard 1 card in HQ"
                     :async true
                     :effect
                     (req (show-wait-prompt state :challenger "Contestant to select cards to discard with Sadaka")
                          (wait-for
                            (resolve-ability
                              state side
                              {:prompt "Choose a card in HQ to discard"
                               :choices (req (cancellable (:hand contestant) :sorted))
                               :async true
                               :cancel-effect (effect (system-msg "chooses not to discard a card from HQ")
                                                      (effect-completed eid))
                               :effect (req (wait-for
                                              (discard state :contestant (make-eid state) target nil)
                                              (do
                                                (system-msg state :contestant "discards a card from HQ")
                                                (wait-for
                                                  (resolve-ability state side discard-radicle-sub card nil)
                                                  (effect-completed state side eid)))))}
                              card nil)
                            (do
                              (system-msg state :contestant "discards Sadaka")
                              (clear-wait-prompt state :challenger)
                              (when current-character
                                (no-action state side nil)
                                (continue state side nil))
                              (discard state :contestant eid card nil))))}]})

   "Sagittarius"
   (constellation-character discard-resource)

   "Salvage"
   {:advanceable :while-revealed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :subroutines [(tag-trace 2)]}

   "Sand Storm"
   {:subroutines [{:req (req (:run @state))
                   :label "Move Sand Storm and the run to another locale"
                   :prompt "Choose another locale and redirect the run to its outermost position"
                   :choices (req (cancellable locales))
                   :msg (msg "move Sand Storm and the run.  The Challenger is now running on " target ". Sand Storm is discarded")
                   :effect (req (let [dest (locale->zone state target)]
                                  (swap! state update-in [:run]
                                         #(assoc % :position (count (get-in contestant (conj dest :characters)))
                                                 :locale (rest dest)))
                                  (discard state side card {:unpreventable true})))}]}

   "Sandman"
   {:subroutines [{:label "Add an placed Challenger card to the grip"
                   :req (req (not-empty (all-placed state :challenger)))
                   :effect (effect (show-wait-prompt :challenger "Contestant to select Sandman target")
                                   (resolve-ability {:choices {:req #(and (placed? %)
                                                                           (= (:side %) "Challenger"))}
                                                      :msg (msg "to add " (:title target) " to the grip")
                                                      :effect (effect (clear-wait-prompt :challenger)
                                                                      (move :challenger target :hand true))
                                                      :cancel-effect (effect (clear-wait-prompt :challenger))}
                                                     card nil))}]}

   "Sapper"
   {:flags {:rd-reveal (req true)}
    :subroutines [discard-resource]
    :access {:async true
             :req (req (and (not= (first (:zone card)) :discard)
                            (some #(is-type? % "Resource") (all-active-placed state :challenger))))
             :effect (effect (show-wait-prompt :contestant "Challenger to decide to break Sapper subroutine")
                             (continue-ability
                               :challenger {:optional
                                        {:player :challenger
                                         :prompt "Allow Sapper subroutine to fire?"
                                         :priority 1
                                         :yes-ability {:effect (req (clear-wait-prompt state :contestant)
                                                                    (show-wait-prompt state :challenger "Contestant to discard a resource with Sapper")
                                                                    (play-subroutine state :contestant eid {:card card :subroutine 0}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :contestant)
                                                                      (effect-completed eid))}}}
                              card nil))}}

   "Searchlight"
   {:advanceable :always
    :subroutines [(tag-trace advance-counters)]}

   "Seidr Adaptive Barrier"
   (let [recalculate-strength (req (update-character-strength state side (get-card state card)))
         recalc-event {:req (req (= (:zone target) (:zone card)))
                       :effect recalculate-strength}]
     {:effect recalculate-strength
      :strength-bonus (req (count (:characters (card->locale state card))))
      :subroutines [end-the-run]
      :events {:card-moved recalc-event
               :contestant-place recalc-event}})

   "Self-Adapting Code Wall"
   {:subroutines [end-the-run]
    :flags {:cannot-lower-strength true}}

   "Sensei"
   {:subroutines [{:label "Give each other Character encountered \"End the run\" for the remainder of the run"
                   :msg (msg "give each other Character encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}

   "Shadow"
   {:advanceable :always
    :subroutines [(gain-credits-sub 2)
                  (tag-trace 3)]
    :strength-bonus advance-counters}

   "Sherlock 1.0"
   {:subroutines [(trace-ability 4 {:choices {:req #(and (placed? %)
                                                         (is-type? % "Resource"))}
                                    :label "Add an placed resource to the top of the Challenger's Stack"
                                    :msg (msg "add " (:title target) " to the top of the Challenger's Stack")
                                    :effect (effect (move :challenger target :deck {:front true}))})]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Sherlock 2.0"
   {:subroutines [(trace-ability 4 {:choices {:req #(and (placed? %)
                                                         (is-type? % "Resource"))}
                                    :label "Add an placed resource to the bottom of the Challenger's Stack"
                                    :msg (msg "add " (:title target) " to the bottom of the Challenger's Stack")
                                    :effect (effect (move :challenger target :deck))})
                  (give-tags 1)]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Shinobi"
   {:effect take-bad-pub
    :subroutines [(trace-ability 1 (do-net-damage 1))
                  (trace-ability 2 (do-net-damage 2))
                  (trace-ability 3 {:label "Do 3 net damage and end the run"
                                    :msg "do 3 net damage and end the run"
                                    :effect (effect (damage eid :net 3 {:card card})
                                                    (end-run))})]}

   "Shiro"
   {:subroutines [{:label "Rearrange the top 3 cards of R&D"
                   :msg "rearrange the top 3 cards of R&D"
                   :async true
                   :effect (req (show-wait-prompt state :challenger "Contestant to rearrange the top cards of R&D")
                                (let [from (take 3 (:deck contestant))]
                                  (if (pos? (count from))
                                    (continue-ability state side (reorder-choice :contestant :challenger from '()
                                                                                 (count from) from) card nil)
                                    (do (clear-wait-prompt state :challenger)
                                        (effect-completed state side eid)))))}
                  {:label "Force the Challenger to access the top card of R&D"
                   :async true
                   :effect (req (do-access state :challenger eid [:rd] {:no-root true}))}]}

   "Snoop"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (= current-character card))
                 :label "Reveal all cards in the Challenger's Grip"
                 :msg (msg "reveal the Challenger's Grip ( " (join ", " (map :title (:hand challenger))) " )")}
                {:req (req (pos? (get-counters card :power)))
                 :counter-cost [:power 1]
                 :label "Hosted power counter: Reveal all cards in Grip and discard 1 card"
                 :msg (msg "look at all cards in Grip and discard " (:title target)
                           " using 1 power counter")
                 :choices (req (cancellable (:hand challenger) :sorted))
                 :prompt "Choose a card to discard"
                 :effect (effect (discard target))}]
    :subroutines [(trace-ability 3 add-power-counter)]}

   "Snowflake"
   {:subroutines [(do-psi end-the-run)]}

   "Special Offer"
   {:subroutines [{:label "Gain 5 [Credits] and discard Special Offer"
                   :effect (req (gain-credits state :contestant 5)
                                (when current-character
                                  (no-action state side nil)
                                  (continue state side nil))
                                (discard state side card)
                                (system-msg state side (str "gains 5 [Credits] and discards Special Offer")))}]}

   "Spiderweb"
   {:subroutines [end-the-run]}

   "Surveyor"
   (let [x (req (* 2 (count (:characters (card->locale state card)))))
         recalculate-strength (req (update-character-strength state side (get-card state card)))
         recalc-event {:req (req (= (:zone target) (:zone card)))
                       :effect recalculate-strength}]
     {:effect recalculate-strength
      :strength-bonus x
      :subroutines [{:label "Trace X - Give the Challenger 2 tags"
                     :trace {:base x
                             :label "Give the Challenger 2 tags"
                             :successful (give-tags 2)}}
                    {:label "Trace X - End the run"
                     :trace {:base x
                             :label "End the run"
                             :successful end-the-run}}]
      :events {:card-moved recalc-event
               :contestant-place recalc-event}})

   "Susanoo-no-Mikoto"
   {:subroutines [{:req (req (not= (:locale run) [:discard]))
                   :msg "make the Challenger continue the run on Archives"
                   :effect (req (swap! state update-in [:run]
                                       #(assoc % :position (count (get-in contestant [:locales :archives :characters]))
                                                 :locale [:archives])))}]}

   "Swarm"
   {:effect take-bad-pub
    :advanceable :always
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :subroutines [discard-resource]
    :challenger-abilities [(challenger-pay [:credit 3] 1)]}

   "Swordsman"
   {:implementation "AI restriction not implemented"
    :subroutines [(do-net-damage 1)
                  {:prompt "Select an AI resource to discard"
                   :msg (msg "discard " (:title target))
                   :label "Discard an AI resource"
                   :effect (effect (discard target))
                   :choices {:req #(and (placed? %)
                                        (is-type? % "Resource")
                                        (has-subtype? % "AI"))}}]}

   "SYNC BRE"
   {:subroutines [(tag-trace 4)
                  (trace-ability 2 {:label "Challenger reduces cards accessed by 1 for this run"
                                    :async true
                                    :msg "reduce cards accessed for this run by 1"
                                    :effect (effect (access-bonus -1))})]}

   "Tapestry"
   {:subroutines [{:label "force the Challenger to lose 1 [Click], if able"
                   :msg "force the Challenger to lose 1 [Click]"
                   :effect challenger-loses-click}
                  {:msg "draw 1 card"
                   :effect (effect (draw))}
                  {:req (req (pos? (count (:hand contestant))))
                   :prompt "Choose a card in HQ to move to the top of R&D"
                   :choices {:req #(and (in-hand? %) (= (:side %) "Contestant"))}
                   :msg "add 1 card in HQ to the top of R&D"
                   :effect (effect (move target :deck {:front true}))}]}

   "Taurus"
   (constellation-character discard-hazard)

   "Thoth"
   {:implementation "Encounter effect is manual"
    :challenger-abilities [{:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :challenger "takes 1 tag on encountering Thoth")
                                     (gain-tags state :contestant eid 1))}]
    :subroutines [(trace-ability 4 {:label "Do 1 net damage for each Challenger tag"
                                    :async true
                                    :msg (msg "do " (:tag challenger) " net damage")
                                    :effect (effect (damage eid :net (:tag challenger) {:card card}))})
                  (trace-ability 4 {:label "Challenger loses 1 [Credits] for each tag"
                                    :async true
                                    :msg (msg "force the Challenger to lose " (:tag challenger) " [Credits]")
                                    :effect (effect (lose-credits :challenger (:tag challenger)))})]}

   "Thimblerig"
   {:flags {:contestant-phase-12 (req (>= (count (filter character? (all-placed state :contestant))) 2))}
    :implementation "Does not restrict usage of swap ability to start of turn or after pass"
    :abilities [{:label "Swap Thimblerig with a piece of character"
                 :prompt "Choose a piece of character to swap Thimblerig with"
                 :choices {:req character?
                           :not-self true}
                 :effect (effect (swap-character card target))}]
    :subroutines [end-the-run]}

   "Tithonium"
   {:alternative-cost [:forfeit]
    :implementation "Does not handle UFAQ for Pawn or Blackguard interaction"
    :cannot-host true
    :subroutines [discard-resource
                  end-the-run
                  {:label "Discard a radicle"
                   :msg (msg "discard " (:title target))
                   :async true
                   :choices {:req #(and (placed? %)
                                        (is-type? % "Radicle"))}
                   :effect (effect (discard target {:reason :subroutine}))}]}

   "TL;DR"
   {:subroutines [{:msg "duplicate subroutines on next piece of Character encountered this run"}]}

   "TMI"
   {:trace {:base 2
            :msg "keep TMI revealed"
            :label "Keep TMI revealed"
            :unsuccessful {:effect (effect (hide card))}}
    :subroutines [end-the-run]}

   "Tollbooth"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "make the Challenger pay 3 [Credits], if able"
                 :effect (effect (pay :challenger card :credit 3))}]
    :subroutines [end-the-run]}

   "Tour Guide"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (filter #(is-type? % "Site")
                                                  (all-active-placed state :contestant))) " subroutines")}]
    :subroutines [end-the-run]}

   "Tribunal"
   {:subroutines [{:msg "force the Challenger to discard 1 placed card"
                   :effect (effect (resolve-ability :challenger discard-placed card nil))}]}

   "Troll"
   {:implementation "Encounter effect is manual"
    :abilities [(trace-ability 2 {:label "Force the Challenger to lose [Click] or end the run"
                                  :msg "force the Challenger to lose [Click] or end the run"
                                  :player :challenger
                                  :prompt "Choose one"
                                  :choices ["Lose [Click]" "End the run"]
                                  :effect (req (if-not (and (= target "Lose [Click]")
                                                            (can-pay? state :challenger nil [:click 1]))
                                                 (do (end-run state side)
                                                     (system-msg state side "ends the run"))
                                                 (do (lose state side :click 1)
                                                     (system-msg state side "loses [Click]"))))})]}

   "Tsurugi"
   {:subroutines [end-the-run
                  (do-net-damage 1)]}

   "Turing"
   {:implementation "AI restriction not implemented"
    :subroutines [end-the-run]
    :strength-bonus (req (if (is-party? (second (:zone card))) 3 0))
    :challenger-abilities [(challenger-pay [:click 3] 1)]}

   "Turnpike"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "force the Challenger to lose 1 [Credits]"
                 :effect (effect (lose-credits :challenger 1))}]
    :subroutines [(tag-trace 5)]}

   "Tyrant"
   {:advanceable :while-revealed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :subroutines [end-the-run]}

   "Universal Connectivity Fee"
   {:subroutines [{:label "Force the Challenger to lose credits"
                   :msg (msg "force the Challenger to lose " (if tagged "all credits" "1 [Credits]"))
                   :effect (req (if tagged
                                  (do (lose-credits state :challenger :all)
                                      (lose state :challenger :run-credit :all)
                                      (when current-character
                                        (no-action state side nil)
                                        (continue state side nil))
                                      (discard state side card))
                                  (lose-credits state :challenger 1)))}]}

   "Upayoga"
   {:implementation "\"Resolve a subroutine...\" subroutine is not implemented"
    :subroutines [(do-psi {:label "Make the Challenger lose 2 [Credits]"
                           :msg "make the Challenger lose 2 [Credits]"
                           :effect (effect (lose-credits :challenger 2))})
                  {:msg "resolve a subroutine on a piece of revealed psi Character"}]}

   "Uroboros"
   {:subroutines [(trace-ability 4 {:label "Prevent the Challenger from making another run"
                                    :msg "prevent the Challenger from making another run"
                                    :effect (effect (register-turn-flag! card :can-run nil))})

                  (trace-ability 4 end-the-run)]}

   "Vanilla"
   {:subroutines [end-the-run]}

   "Veritas"
   {:subroutines [{:label "Contestant gains 2 [Credits]"
                   :msg "gain 2 [Credits]"
                   :effect (effect (gain-credits :contestant 2))}
                  {:label "Challenger loses 2 [Credits]"
                   :msg "force the Challenger to lose 2 [Credits]"
                   :effect (effect (lose-credits :challenger 2))}
                  (trace-ability 2 (give-tags 1))]}

   "Vikram 1.0"
   {:implementation "Resource prevention is not implemented"
    :subroutines [{:msg "prevent the Challenger from using resources for the remainder of this run"}
                  (trace-ability 4 (do-brain-damage 1))]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Viktor 1.0"
   {:subroutines [(do-brain-damage 1)
                  end-the-run]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Viktor 2.0"
   {:abilities [(power-counter-ability (do-brain-damage 1))]
    :subroutines [(trace-ability 2 add-power-counter)
                  end-the-run]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Viper"
   {:subroutines [(trace-ability 3 {:label "The Challenger loses 1 [Click] if able"
                                    :msg "force the Challenger to lose 1 [Click] if able"
                                    :effect challenger-loses-click})
                  (trace-ability 3 end-the-run)]}

   "Virgo"
   (constellation-character (give-tags 1))

   "Waiver"
   {:subroutines [(trace-ability 5 {:label "Reveal the Challenger's Grip and discard cards"
                                    :msg (msg "reveal all cards in the Challenger's Grip: " (join ", " (map :title (:hand challenger)))
                                              ". Cards with a play/place cost less than or equal to " (- target (second targets))
                                              " will be discarded")
                                    :effect (req (let [delta (- target (second targets))]
                                                   (doseq [c (:hand challenger)]
                                                     (when (<= (:cost c) delta)
                                                       (resolve-ability
                                                         state side
                                                         {:msg (msg "discard " (:title c))
                                                          :effect (effect (discard c))}
                                                         card nil)))))})]}

   "Wall of Static"
   {:subroutines [end-the-run]}

   "Wall of Thorns"
   {:subroutines [end-the-run
                  (do-net-damage 2)]}

   "Watchtower"
   {:subroutines [{:label "Search R&D and add 1 card to HQ"
                   :prompt "Choose a card to add to HQ"
                   :msg "add a card from R&D to HQ"
                   :choices (req (cancellable (:deck contestant) :sorted))
                   :cancel-effect (effect (system-msg "cancels the effect of Watchtower"))
                   :effect (effect (shuffle! :deck)
                                   (move target :hand))}]}

   "Weir"
   {:subroutines [{:label "force the Challenger to lose 1 [Click], if able"
                   :msg "force the Challenger to lose 1 [Click]"
                   :effect challenger-loses-click}
                  {:label "Challenger discards 1 card from their Grip"
                   :req (req (pos? (count (:hand challenger))))
                   :prompt "Choose a card to discard from your Grip"
                   :player :challenger
                   :choices (req (:hand challenger))
                   :not-distinct true
                   :effect (effect (discard :challenger target)
                                   (system-msg :challenger (str "discards " (:title target) " from their Grip")))}]}

   "Wendigo"
   (implementation-note
     "Resource prevention is not implemented"
     (montestanth-character "Code Gate" "Barrier"
                {:msg "prevent the Challenger from using a chosen resource for the remainder of this run"}))

   "Whirlpool"
   {:subroutines [{:msg "prevent the Challenger from jacking out"
                   :effect (req (when (and (is-party? (second (:zone card)))
                                           (> (count (concat (:characters (card->locale state card))
                                                             (:content (card->locale state card)))) 1))
                                  (prevent-jack-out state side))
                                (when current-character
                                  (no-action state side nil)
                                  (continue state side nil))
                                (discard state side card))}]}

   "Woodcutter"
   {:advanceable :while-revealed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :subroutines [(do-net-damage 1)]}

   "Wormhole"
   ;; TODO: create an ability for wormhole
   (implementation-note "Wormhole subroutine is not implemented"
                        (space-character))

   "Wotan"
   {:subroutines [end-the-run
                  (do-brain-damage 1)]
    :challenger-abilities [(challenger-pay [:click 2] 1)
                       (challenger-pay [:credit 3] 1)]}

   "Wraparound"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "Fracter") (all-active-placed state :challenger))
                           0 7))
    :events (let [wr {:silent (req true)
                      :req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "Fracter")))
                      :effect (effect (update-character-strength card))}]
              {:challenger-place wr :discard wr :card-moved wr})}

   "Yagura"
   {:subroutines [(do-net-damage 1)
                  {:msg "look at the top card of R&D"
                   :optional {:prompt (msg "Move " (:title (first (:deck contestant))) " to the bottom of R&D?")
                              :yes-ability {:effect (effect (move (first (:deck contestant)) :deck)
                                                            (do (system-msg state side "uses Yagura to move the top card of R&D to the bottom")))}
                              :no-ability {:effect (req (system-msg state :contestant (str "does not use Yagura to move the top card of R&D to the bottom")))}}}]}

   "Zed 1.0"
   {:implementation "Restriction on having spent [click] is not implemented"
    :subroutines [(do-brain-damage 1)]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Zed 2.0"
   {:implementation "Restriction on having spent [click] is not implemented"
    :subroutines [discard-hazard
                  (do-brain-damage 2)]
    :challenger-abilities [(challenger-break [:click 2] 2)]}})
