(in-ns 'game.core)

(declare trash-program trash-hardware trash-muthereff-sub trash-installed)

;;;; Helper functions specific for Character

;;; Challenger abilites for breaking subs
(defn challenger-break
  "Ability to break a subroutine by spending a muthereff (Bioroids, Negotiator, Turing etc)"
  [cost subs]
  (let [cost-str (build-cost-str [cost])
        subs-str (quantify subs "subroutine")]
    {:cost cost
     :label (str "break " subs-str)
     :effect (req (system-msg state :challenger (str "spends " cost-str " to break " subs-str " on " (:title card))))}))

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

(def give-tag
  "Basic give challenger 1 tag subroutine
   Mostly used with tag-trace"
  {:label "Give the Challenger 1 tag"
   :msg "give the Challenger 1 tag"
   :delayed-completion true
   :effect (effect (tag-challenger :challenger eid 1))})

(def add-power-counter
  "Adds 1 power counter to the card."
  {:label "Add 1 power counter"
   :msg "add 1 power counter"
   :effect (effect (add-counter card :power 1))})

(defn trace-ability
  "Run a trace with specified base strength.
   If successful trigger specified ability"
  [base ability]
  {:label (str "Trace " base " - " (:label ability))
   :trace (assoc ability :base base)})

(defn tag-trace
  "Trace ability for giving a tag, at specified base strength"
  [base]
  (trace-ability base give-tag))

(defn do-net-damage
  "Do specified amount of net-damage."
  [dmg]
  {:label (str "Do " dmg " net damage")
   :delayed-completion true
   :msg (str "do " dmg " net damage")
   :effect (effect (damage eid :net dmg {:card card}))})

(defn do-brain-damage
  "Do specified amount of brain damage."
  [dmg]
  {:label (str "Do " dmg " brain damage")
   :delayed-completion true
   :msg (str "do " dmg " brain damage")
   :effect (effect (damage eid :brain dmg {:card card}))})

(defn gain-credits
  "Gain specified amount of credits"
  [credits]
  {:label (str "Gain " credits " [Credits]")
   :msg (str "gain " credits " [Credits]")
   :effect (effect (gain :credit credits))})

(defn power-counter-ability
  "Does specified ability using a power counter."
  [{:keys [label msg] :as ability}]
  (assoc ability :label (str "Hosted power counter: " label)
                 :msg (str msg " using 1 power counter")
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
  "Bad pub on rez effect."
  (effect (gain :bad-publicity 1)
          (system-msg (str "takes 1 bad publicity from " (:title card)))))


;;; For Advanceable Character
(def advance-counters
  "Number of advancement counters - for advanceable Character."
  (req (+ (:advance-counter card 0) (:extra-advance-counter card 0))))

(def space-character-rez-bonus
  "Amount of rez reduction for the Space Character."
  (req (* -3 (+ (:advance-counter card 0) (:extra-advance-counter card 0)))))

(defn space-character
  "Creates data for Space Character with specified abilities."
  [& abilities]
  {:advanceable :always
   :subroutines (vec abilities)
   :rez-cost-bonus space-character-rez-bonus})


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
  "Counts number of rezzed NEXT Character - for use with NEXT Bronze and NEXT Gold"
  [contestant]
  (let [servers (flatten (seq (:servers contestant)))
        rezzed-next? #(and (rezzed? %) (has-subtype? % "NEXT"))]
    (reduce (fn [c server] (+ c (count (filter rezzed-next? (:characters server))))) 0 servers)))


;;; For Morph Character
(defn morph [state side card new old]
  (update! state side (assoc card
                        :subtype-target new
                        :subtype (combine-subtypes true
                                                   (remove-subtypes (:subtype card) old)
                                                   new)))
  (update-character-strength state side card))

(defn morph-effect
  "Creates morph effect for Character. Morphs from base type to other type"
  [base other]
  (req (if (odd? (get (get-card state card) :advance-counter 0))
         (morph state side card other base)
         (morph state side card base other))))

(defn morph-character
  "Creates the data for morph Character with specified types and ability."
  [base other ability]
  (let [ab {:req (req (= (:cid card) (:cid target)))
            :effect (morph-effect base other)}]
    {:advanceable :always
     :effect (morph-effect base other)
     :subroutines [ability]
     :events {:advance ab :advancement-placed ab}}))


;;; For Constellation Character
(defn constellation-character
  "Generates map for Constellation Character with specified effect."
  [ability]
  {:subroutines [(trace-ability 2 (assoc ability :kicker (assoc ability :min 5)))]})

;;; Helper function for adding implementation notes to Character defined with functions
(defn- implementation-note
  "Adds an implementation note to the character-definition"
  [note character-def]
  (assoc character-def :implementation note))


;;;; Card definitions
(def cards-character
  {"Aiki"
   {:subroutines [(do-psi {:label "Challenger draws 2 cards"
                           :msg "make the Challenger draw 2 cards"
                           :effect (effect (draw :challenger 2))})
                  (do-net-damage 1)]}

   "Aimor"
   {:subroutines [{:label "Trash the top 3 cards of the Stack. Trash Aimor."
                   :effect (req (when (not-empty (:deck challenger))
                                  (system-msg state :contestant
                                              (str "uses Aimor to trash "
                                                   (join ", " (map :title (take 3 (:deck challenger))))
                                                   " from the Challenger's Stack"))
                                  (mill state :challenger 3))
                                (when current-character
                                  (no-action state :contestant nil)
                                  (continue state :challenger nil))
                                (trash state side card)
                                (system-msg state side (str "trashes Aimor")))}]}

   "Archangel"
   {:access
    {:delayed-completion true
     :req (req (not= (first (:zone card)) :discard))
     :effect (effect (show-wait-prompt :challenger "Contestant to decide to trigger Archangel")
                     (continue-ability
                       {:optional
                        {:prompt "Pay 3 [Credits] to force Challenger to encounter Archangel?"
                         :yes-ability {:cost [:credit 3]
                                       :delayed-completion true
                                       :effect (effect (system-msg :contestant "pays 3 [Credits] to force the Challenger to encounter Archangel")
                                                       (clear-wait-prompt :challenger)
                                                       (continue-ability
                                                         :challenger {:optional
                                                                  {:player :challenger
                                                                   :prompt "You are encountering Archangel. Allow its subroutine to fire?"
                                                                   :priority 1
                                                                   :yes-ability {:delayed-completion true
                                                                                 :effect (effect (play-subroutine eid {:card card :subroutine 0}))}
                                                                   :no-ability {:effect (effect (effect-completed eid))}}}
                                                         card nil))}
                         :no-ability {:effect (effect (system-msg :contestant "declines to force the Challenger to encounter Archangel")
                                                      (clear-wait-prompt :challenger))}}}
                       card nil))}
   :subroutines [(trace-ability 6 {:delayed-completion true
                                   :effect (effect (show-wait-prompt :challenger "Contestant to select Archangel target")
                                                   (continue-ability {:choices {:req #(and (installed? %)
                                                                                           (card-is? % :side :challenger))}
                                                                      :label "Add 1 installed card to the Challenger's Grip"
                                                                      :msg "add 1 installed card to the Challenger's Grip"
                                                                      :effect (effect (clear-wait-prompt :challenger)
                                                                                      (move :challenger target :hand true)
                                                                                      (system-msg (str "adds " (:title target)
                                                                                                       " to the Challenger's Grip")))
                                                                      :cancel-effect (effect (clear-wait-prompt :challenger)
                                                                                             (effect-completed eid))}
                                                                     card nil))})]}

   "Archer"
   {:additional-cost [:forfeit]
    :subroutines [(gain-credits 2)
                  trash-program
                  end-the-run]}

   "Architect"
   {:flags {:untrashable-while-rezzed true}
    :subroutines [{:label "Look at the top 5 cards of R&D"
                   :prompt "Choose a card to install"
                   :priority true
                   :activatemsg "uses Architect to look at the top 5 cards of R&D"
                   :req (req (and (not (string? target))
                                  (not (is-type? target "Resource"))))
                   :not-distinct true
                   :choices (req (conj (take 5 (:deck contestant)) "No install"))
                   :effect (effect (system-msg (str "chooses the card in position "
                                                    (+ 1 (.indexOf (take 5 (:deck contestant)) target))
                                                    " from R&D (top is 1)"))
                                   (contestant-install (move state side target :play-area) nil {:no-install-cost true}))}
                  {:label "Install a card from HQ or Archives"
                   :prompt "Select a card to install from Archives or HQ"
                   :show-discard true
                   :priority true
                   :choices {:req #(and (not (is-type? % "Resource"))
                                        (#{[:hand] [:discard]} (:zone %))
                                        (= (:side %) "Contestant"))}
                   :effect (effect (contestant-install target nil))
                   :msg (msg (contestant-install-msg target))}]}

   "Ashigaru"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand contestant)) " subroutines")}]
    :subroutines [end-the-run]}

   "Assassin"
   {:subroutines [(trace-ability 5 (do-net-damage 3))
                  (trace-ability 4 trash-program)]}

   "Asteroid Belt"
   (space-character end-the-run)

   "Authenticator"
   {:implementation "Encounter effect is manual"
    :abilities [give-tag]
    :challenger-abilities [{:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :challenger "takes 1 tag on encountering Authenticator to Bypass it")
                                     (tag-challenger state :challenger eid 1 {:unpreventable true}))}]
    :subroutines [(gain-credits 2)
                  end-the-run]}

   "Bailiff"
   {:implementation "Gain credit is manual"
    :abilities [(gain-credits 1)]
    :subroutines [end-the-run]}

   "Bandwidth"
   {:subroutines [{:msg "give the Challenger 1 tag"
                   :delayed-completion true
                   :effect (effect (tag-challenger :challenger eid 1)
                                   (register-events
                                     {:successful-run {:effect (effect (lose :challenger :tag 1))
                                                       :msg "make the Challenger lose 1 tag"}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card))}]
    :events {:successful-run nil :run-ends nil}}

   "Bastion"
   {:subroutines [end-the-run]}

   "Battlement"
   {:subroutines [end-the-run]}

   "Bloodletter"
   {:subroutines [{:label "Challenger trashes 1 program or top 2 cards of their Stack"
                   :effect (req (if (empty? (filter #(is-type? % "Program") (all-installed state :challenger)))
                                   (do (mill state :challenger 2)
                                       (system-msg state :challenger (str "trashes the top 2 cards of their Stack")))
                                   (do (show-wait-prompt state :contestant "Challenger to choose an option for Bloodletter")
                                       (resolve-ability state :challenger
                                         {:prompt "Trash 1 program or trash top 2 cards of the Stack?"
                                          :choices ["Trash 1 program" "Trash top 2 of Stack"]
                                          :effect (req (if (and (= target "Trash top 2 of Stack") (pos? (count (:deck challenger))))
                                                         (do (mill state :challenger 2)
                                                             (system-msg state :challenger (str "trashes the top 2 cards of their Stack"))
                                                             (clear-wait-prompt state :contestant))
                                                         (resolve-ability state :challenger trash-program card nil)))}
                                        card nil))))}]}

   "Bloom"
   (let [character-index (fn [state i] (first (keep-indexed #(when (= (:cid %2) (:cid i)) %1)
                                                      (get-in @state (cons :contestant (:zone i))))))]
     {:subroutines
              [{:label "Install a piece of character from HQ protecting another server, ignoring all costs"
                :prompt "Choose Character to install from HQ in another server"
                :delayed-completion true
                :choices {:req #(and (character? %)
                                     (in-hand? %))}
                :effect (req (let [this (zone->name (second (:zone card)))
                                   ncharacter target]
                               (continue-ability state side
                                                 {:prompt (str "Choose a location to install " (:title target))
                                                  :choices (req (remove #(= this %) (contestant-install-list state ncharacter)))
                                                  :delayed-completion true
                                                  :effect (effect (contestant-install ncharacter target {:no-install-cost true}))}
                                                 card nil)))}
               {:label "Install a piece of character from HQ in the next innermost position, protecting this server, ignoring all costs"
                :prompt "Choose Character to install from HQ in this server"
                :delayed-completion true
                :choices {:req #(and (character? %)
                                     (in-hand? %))}
                :effect (req (let [newcharacter (assoc target :zone (:zone card))
                                   bndx (character-index state card)
                                   characters (get-in @state (cons :contestant (:zone card)))
                                   newcharacters (apply conj (subvec characters 0 bndx) newcharacter (subvec characters bndx))]
                               (swap! state assoc-in (cons :contestant (:zone card)) newcharacters)
                               (swap! state update-in (cons :contestant (:zone target))
                                      (fn [coll] (remove-once #(not= (:cid %) (:cid target)) coll)))
                               (card-init state side newcharacter {:resolve-effect false
                                                             :init-data true})
                               (trigger-event state side :contestant-install newcharacter)))}]})

   "Brainstorm"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand challenger)) " subroutines")}]
    :subroutines [(do-brain-damage 1)]}

   "Builder"
   {:abilities [{:label "Move Builder to the outermost position of any server"
                 :cost [:click 1] :prompt "Choose a server" :choices (req servers)
                 :msg (msg "move it to the outermost position of " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]
    :subroutines [{:label "Place 1 advancement token on an Character that can be advanced protecting this server"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req #(and (character? %)
                                        (can-be-advanced? %))}
                   :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Bullfrog"
   {:subroutines [(do-psi {:label "Move Bullfrog to another server"
                           :player :contestant
                           :prompt "Choose a server"
                           :choices (req servers)
                           :msg (msg "move it to the outermost position of " target)
                           :effect (req (let [dest (server->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc % :position (count (get-in contestant (conj dest :characters)))
                                                           :server (rest dest))))
                                        (move state side card
                                              (conj (server->zone state target) :characters)))})]}

   "Bulwark"
   {:effect take-bad-pub
    :abilities [{:msg "gain 2 [Credits] if there is an installed AI"
                 :req (req (some #(has-subtype? % "AI") (all-installed state :challenger)))
                 :effect (effect (gain :credit 2))}]
    :subroutines [(assoc trash-program :player :challenger
                                       :msg "force the Challenger to trash 1 program"
                                       :label "The Challenger trashes 1 program")
                  {:msg "gain 2 [Credits] and end the run"
                   :effect (effect (gain :credit 2)
                                   (end-run))}]}


   "Burke Bugs"
   {:subroutines [(trace-ability 0 (assoc trash-program :not-distinct true
                                                        :player :challenger
                                                        :msg "force the Challenger to trash a program"
                                                        :label "Force the Challenger to trash a program"))]}

   "Caduceus"
   {:subroutines [(trace-ability 3 (gain-credits 3))
                  (trace-ability 2 end-the-run)]}

   "Cell Portal"
   {:subroutines [{:msg "make the Challenger approach the outermost Character"
                   :effect (req (let [srv (first (:server run))
                                      n (count (get-in @state [:contestant :servers srv :characters]))]
                                  (swap! state assoc-in [:run :position] n)
                                  (derez state side card)))}]}

   "Changeling"
   (morph-character "Barrier" "Sentry" end-the-run)

   "Checkpoint"
   {:effect take-bad-pub
    :subroutines [(trace-ability 5 {:label "Do 3 meat damage when this run is successful"
                                    :msg "do 3 meat damage when this run is successful"
                                    :effect (effect (register-events
                                                      {:successful-run
                                                       {:delayed-completion true
                                                        :msg "do 3 meat damage"
                                                        :effect (effect (damage eid :meat 3 {:card card}))}
                                                       :run-ends {:effect (effect (unregister-events card))}}
                                                     card))})]
    :events {:successful-run nil :run-ends nil}}

   "Chetana"
   {:subroutines [{:msg "make each player gain 2 [Credits]" :effect (effect (gain :challenger :credit 2)
                                                                            (gain :contestant :credit 2))}
                  (do-psi {:label "Do 1 net damage for each card in the Challenger's grip"
                           :effect (effect (damage eid :net (count (get-in @state [:challenger :hand])) {:card card}))
                           :msg (msg (str "do " (count (get-in @state [:challenger :hand])) " net damage"))})]}

   "Chimera"
   (let [turn-end-ability {:effect (effect (derez :contestant card)
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
   {:implementation "Trash effect when using an AI to break is activated manually"
    :abilities [{:label "Trash the top 2 cards of the Challenger's Stack"
                 :req (req (some #(has-subtype? % "AI") (all-installed state :challenger)))
                 :msg (msg (str "trash " (join ", " (map :title (take 2 (:deck challenger)))) " from the Challenger's Stack"))
                 :effect (effect (mill :challenger 2))}]
    :subroutines [(do-net-damage 2)
                  end-the-run]}

   "Chrysalis"
   {:subroutines [(do-net-damage 2)]
    :access {:delayed-completion true
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
                   :req (req this-server)
                   :prompt "Select the Character the Challenger is encountering"
                   :choices {:req #(and (rezzed? %) (character? %))}
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
                           :choices {:req installed?}
                           :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                           (end-run))})]}

   "Cobra"
   {:subroutines [trash-program (do-net-damage 2)]}

   "Colossus"
   {:advanceable :always
    :subroutines [{:label "Give the Challenger 1 tag (Give the Challenger 2 tags)"
                   :delayed-completion true
                   :msg (msg "give the Challenger " (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) "1 tag" "2 tags"))
                   :effect (effect (tag-challenger :challenger eid (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 2)))}
                  {:label "Trash 1 program (Trash 1 program and 1 muthereff)"
                   :delayed-completion true
                   :msg (msg "trash 1 program" (when (< 2 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) " and 1 muthereff"))
                   :effect (req (when-completed (resolve-ability state side trash-program card nil)
                                                (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0)))
                                                  (effect-completed state side eid)
                                                  (continue-ability state side
                                                    {:prompt "Choose a muthereff to trash"
                                                     :msg (msg "trash " (:title target))
                                                     :choices {:req #(and (installed? %)
                                                                          (is-type? % "Muthereff"))}
                                                     :cancel-effect (req (effect-completed state side eid))
                                                     :effect (effect (trash target {:cause :subroutine}))}
                                                   card nil))))}]
    :strength-bonus advance-counters}

   "Conundrum"
   {:subroutines [(assoc trash-program :player :challenger
                                       :msg "force the Challenger to trash 1 program"
                                       :label "The Challenger trashes 1 program")
                  {:msg "force the Challenger to lose 1 [Click] if able"
                   :effect (effect (lose :challenger :click 1))}
                  end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "AI") (all-installed state :challenger)) 3 0))}

   "Cortex Lock"
   {:subroutines [{:label "Do 1 net damage for each unused memory unit the Challenger has"
                   :msg (msg "do " (:memory challenger) " net damage")
                   :effect (effect (damage eid :net (:memory challenger) {:card card}))}]}

   "Crick"
   {:subroutines [{:label "install a card from Archives"
                   :prompt "Select a card to install from Archives"
                   :show-discard true
                   :priority true
                   :choices {:req #(and (not (is-type? % "Resource"))
                                        (= (:zone %) [:discard])
                                        (= (:side %) "Contestant"))}
                   :msg (msg (contestant-install-msg target))
                   :effect (effect (contestant-install target nil))}]
    :strength-bonus (req (if (= (second (:zone card)) :archives) 3 0))}

   "Curtain Wall"
   {:subroutines [end-the-run]
    :strength-bonus (req (let [characters (:characters (card->server state card))]
                           (if (= (:cid card) (:cid (last characters))) 4 0)))
    :events (let [cw {:req (req (and (not= (:cid card) (:cid target))
                                     (= (card->server state card) (card->server state target))))
                      :effect (effect (update-character-strength card))}]
              {:contestant-install cw :trash cw :card-moved cw})}

   "Data Hound"
   (letfn [(dh-trash [cards]
             {:prompt "Choose a card to trash"
              :choices cards
              :delayed-completion true
              :msg (msg "trash " (:title target))
              :effect (req (do (trash state side target {:unpreventable true})
                               (continue-ability state side (reorder-choice
                                                              :challenger :challenger (remove-once #(not= % target) cards)
                                                              '() (count (remove-once #(not= % target) cards))
                                                              (remove-once #(not= % target) cards)) card nil)))})]
     {:subroutines [(trace-ability 2 {:delayed-completion true
                                      :label "Look at the top of Stack"
                                      :msg "look at top X cards of Stack"
                                      :effect (req (show-wait-prompt state :challenger "Contestant to rearrange the top cards of the Challenger's Stack")
                                                   (let [c (- target (second targets))
                                                         from (take c (:deck challenger))]
                                                     (system-msg state :contestant
                                                                 (str "looks at the top " c " cards of Stack"))
                                                     (if (< 1 c)
                                                       (continue-ability state side (dh-trash from) card nil)
                                                       (do (system-msg state :contestant (str "trashes " (:title (first from))))
                                                           (trash state side (first from) {:unpreventable true})
                                                           (clear-wait-prompt state :challenger)
                                                           (effect-completed state side eid card)))))})]})

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
                                (trash state side card))}]}

   "Datapike"
   {:subroutines [{:msg "force the Challenger to pay 2 [Credits] if able"
                   :effect (effect (pay :challenger card :credit 2))}
                  end-the-run]}

   "Data Raven"
   {:implementation "Encounter effect is manual"
    :abilities [give-tag
                (power-counter-ability give-tag)]
    :challenger-abilities [{:label "End the run"
                        :effect (req (end-run state :challenger)
                                     (system-msg state :challenger "chooses to end the run on encountering Data Raven"))}
                       {:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :challenger "chooses to take 1 tag on encountering Data Raven")
                                     (tag-challenger state :challenger eid 1))}]
    :subroutines [(trace-ability 3 add-power-counter)]}

   "Data Ward"
   {:challenger-abilities [{:label "Pay 3 [Credits]"
                        :effect (req (pay state :challenger card :credit 3)
                                     (system-msg state :challenger "chooses to pay 3 [Credits] on encountering Data Ward"))}
                       {:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :challenger "chooses to take 1 tag on encountering Data Ward")
                                     (tag-challenger state :challenger eid 1))}]
    :subroutines [end-the-run-if-tagged]}

   "DNA Tracker"
   {:subroutines [{:msg "do 1 net damage and make the Challenger lose 2 [Credits]"
                   :effect (req (when-completed (damage state side :net 1 {:card card})
                                                (lose state :challenger :credit 2)))}]}

   "Dracō"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target)
                    (update-character-strength card))
    :strength-bonus (req (get-in card [:counter :power] 0))
    :subroutines [(trace-ability 2 {:label "Give the Challenger 1 tag and end the run"
                                    :msg "give the Challenger 1 tag and end the run"
                                    :delayed-completion true
                                    :effect (effect (tag-challenger :challenger eid 1)
                                                    (end-run))})]}

   "Eli 1.0"
   {:subroutines [end-the-run]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Eli 2.0"
   {:subroutines [{:msg "draw 1 card" :effect (effect (draw))}
                  end-the-run]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Enforcer 1.0"
   {:additional-cost [:forfeit]
    :subroutines [trash-program
                  (do-brain-damage 1)
                  {:label "Trash a console"
                   :prompt "Select a console to trash"
                   :choices {:req #(has-subtype? % "Console")}
                   :msg (msg "trash " (:title target))
                   :effect (effect (trash target))}
                  {:msg "trash all virtual muthereffs"
                   :effect (req (doseq [c (filter #(has-subtype? % "Virtual") (all-installed state :challenger))]
                                  (trash state side c)))}]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Enigma"
   {:subroutines [{:msg "force the Challenger to lose 1 [Click] if able"
                   :effect (effect (lose :challenger :click 1))}
                  end-the-run]}

   "Errand Boy"
   {:subroutines [(gain-credits 1)
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
   {:subroutines [{:label "Force the Challenger to pay 1 [Credits] or trash an installed card"
                   :msg "force the Challenger to pay 1 [Credits] or trash an installed card"
                   :player :challenger
                   :prompt "Choose one"
                   :choices ["Pay 1 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 1 [Credits]")
                                  (do (pay state side card :credit 1)
                                      (system-msg state side "pays 1 [Credits]"))
                                  (resolve-ability state :challenger trash-installed card nil)))}]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Fairchild 2.0"
   {:subroutines [{:label "Force the Challenger to pay 2 [Credits] or trash an installed card"
                   :msg "force the Challenger to pay 2 [Credits] or trash an installed card"
                   :player :challenger
                   :prompt "Choose one"
                   :choices ["Pay 2 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 2 [Credits]")
                                  (do (pay state side card :credit 2)
                                      (system-msg state side "pays 2 [Credits]"))
                                  (resolve-ability state :challenger trash-installed card nil)))}
                  (do-brain-damage 1)]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Fairchild 3.0"
   {:subroutines [{:label "Force the Challenger to pay 3 [Credits] or trash an installed card"
                   :msg "force the Challenger to pay 3 [Credits] or trash an installed card"
                   :player :challenger
                   :prompt "Choose one"
                   :choices ["Pay 3 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 3 [Credits]")
                                  (do (pay state side card :credit 3)
                                      (system-msg state side "pays 3 [Credits]"))
                                  (resolve-ability state :challenger trash-installed card nil)))}
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
   {:subroutines [(trace-ability 6 {:label "Trash 1 hardware, do 2 meat damage, and end the run"
                                    :msg "trash 1 hardware, do 2 meat damage, and end the run"
                                    :delayed-completion true
                                    :effect (effect (continue-ability
                                                     {:prompt "Select a piece of hardware to trash"
                                                      :label "Trash a piece of hardware"
                                                      :choices {:req #(is-type? % "Hardware")}
                                                      :msg (msg "trash " (:title target))
                                                      :effect (req (when-completed
                                                                     (trash state side target {:cause :subroutine})
                                                                     (do (damage state side eid :meat 2 {:unpreventable true
                                                                                              :card card})
                                                                         (end-run state side))))
                                                      :cancel-effect (effect (damage eid :meat 2 {:unpreventable true :card card})
                                                                             (end-run))}
                                                     card nil))})]}

   "Free Lunch"
   {:abilities [(power-counter-ability {:label "Challenger loses 1 [Credits]"
                                        :msg "make the Challenger lose 1 [Credits]"
                                        :effect (effect (lose :challenger :credit 1))})]
    :subroutines [add-power-counter]}

   "Galahad"
   (grail-character end-the-run)

   "Gemini"
   (constellation-character (do-net-damage 1))

   "Grim"
   {:effect take-bad-pub
    :subroutines [trash-program]}

   "Guard"
   {:implementation "Prevent bypass is manual"
    :subroutines [end-the-run]}

   "Gutenberg"
   {:subroutines [(tag-trace 7)]
    :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))}

   "Gyri Labyrinth"
   {:implementation "Hand size is not restored if trashed or derezzed after firing"
    :subroutines [{:req (req (:run @state))
                   :label "Reduce Challenger's maximum hand size by 2 until start of next Contestant turn"
                   :msg "reduce the Challenger's maximum hand size by 2 until the start of the next Contestant turn"
                   :effect (effect (lose :challenger :hand-size-modification 2)
                                   (register-events {:contestant-turn-begins
                                                     {:msg "increase the Challenger's maximum hand size by 2"
                                                      :effect (effect (gain :challenger :hand-size-modification 2)
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
                                  (when (> delta 0)
                                    (resolve-ability
                                      state :challenger
                                      {:prompt (msg "Select " delta " cards to discard")
                                       :player :challenger
                                       :choices {:max delta
                                                 :req #(in-hand? %)}
                                       :effect (req (doseq [c targets]
                                                      (trash state :challenger c))
                                                    (system-msg state :challenger
                                                                (str "trashes " (join ", " (map :title targets)))))}
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
   {:subroutines [(gain-credits 2)
                  {:label "Pay 1 [Credits] to place 1 advancement token on a card that can be advanced"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req can-be-advanced?}
                   :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    :access {:delayed-completion true
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
                                    :effect (req (max-access state side 0)
                                                 (swap! state update-in [:run :run-effect] dissoc :replace-access))})
                  {:label "Trash an icebreaker"
                   :prompt "Choose an icebreaker to trash"
                   :msg (msg "trash " (:title target))
                   :choices {:req #(and (installed? %)
                                        (has? % :subtype "Icebreaker"))}
                   :effect (effect (trash target {:cause :subroutine})
                                   (clear-wait-prompt :challenger))}]}

   "Hortum"
   (letfn [(hort [n] {:prompt "Choose a card to add to HQ with Hortum"
                      :delayed-completion true
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
                                         (effect-completed state side eid card))))})]
     {:advanceable :always
      :subroutines [{:label "Gain 1 [Credits] (Gain 4 [Credits])"
                     :msg (msg "gain " (if (> (+ (:advance-counter card 0) (:extra-advance-counter card 0)) 2) "4" "1") " [Credits]")
                     :effect (effect (gain :contestant :credit (if (> (+ (:advance-counter card 0) (:extra-advance-counter card 0)) 2) 4 1)))}
                    {:label "End the run (Search R&D for up to 2 cards and add them to HQ, shuffle R&D, end the run)"
                     :delayed-completion true
                     :effect (req (if (> (+ (:advance-counter card 0) (:extra-advance-counter card 0)) 2)
                                    (when-completed (resolve-ability state side (hort 1) card nil)
                                                    (do (end-run state side)
                                                        (system-msg state side (str "uses Hortum to add 2 cards to HQ from R&D, "
                                                                                    "shuffle R&D, and end the run"))))
                                    (do (end-run state side)
                                        (system-msg state side (str "uses Hortum to end the run"))
                                        (effect-completed state side eid))))}]})

   "Hourglass"
   {:subroutines [{:msg "force the Challenger to lose 1 [Click] if able"
                   :effect (effect (lose :challenger :click 1))}]}

   "Howler"
   (let [character-index (fn [state i] (first (keep-indexed #(when (= (:cid %2) (:cid i)) %1)
                                                      (get-in @state (cons :contestant (:zone i))))))]
     {:subroutines
      [{:label "Install a piece of Bioroid Character from HQ or Archives"
        :prompt "Install Character from HQ or Archives?"
        :choices ["HQ" "Archives"]
        :effect (req (let [fr target]
                       (resolve-ability state side
                                        {:prompt "Choose a Bioroid Character to install"
                                         :choices (req (filter #(and (character? %)
                                                                     (has-subtype? % "Bioroid"))
                                                               ((if (= fr "HQ") :hand :discard) contestant)))
                                         :effect (req (let [newcharacter (assoc target :zone (:zone card) :rezzed true)
                                                            hndx (character-index state card)
                                                            characters (get-in @state (cons :contestant (:zone card)))
                                                            newcharacters (apply conj (subvec characters 0 hndx) newcharacter (subvec characters hndx))]
                                                        (swap! state assoc-in (cons :contestant (:zone card)) newcharacters)
                                                        (swap! state update-in (cons :contestant (:zone target))
                                                               (fn [coll] (remove-once #(not= (:cid %) (:cid target)) coll)))
                                                        (update! state side (assoc card :howler-target newcharacter))
                                                        (card-init state side newcharacter {:resolve-effect false
                                                                                      :init-data true})
                                                        (trigger-event state side :contestant-install newcharacter)))} card nil)))}]
      :events {:run-ends {:req (req (:howler-target card))
                          :effect (effect (trash card {:cause :self-trash})
                                          (derez (get-card state (:howler-target card))))}}})

   "Hudson 1.0"
   {:subroutines [{:msg "prevent the Challenger from accessing more than 1 card during this run"
                   :effect (effect (max-access 1))}]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Hunter"
   {:subroutines [(tag-trace 3)]}

   "Jua"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "prevent the Challenger from installing cards for the rest of the turn"
                 :effect (effect (lock-install (:cid card) :challenger)
                                 (register-events {:challenger-turn-ends
                                                   {:effect (effect (unlock-install (:cid card) :challenger)
                                                                    (unregister-events card))}}
                                                  card))}]
    :events {:challenger-turn-ends nil}
    :subroutines [{:label "Choose 2 installed Challenger cards, if able. The Challenger must add 1 of those to the top of the Stack."
                   :req (req (>= (count (all-installed state :challenger)) 2))
                   :delayed-completion true
                   :prompt "Select 2 installed Challenger cards"
                   :choices {:req #(and (= (:side %) "Challenger") (installed? %)) :max 2 :all true}
                   :msg (msg "add either " (card-str state (first targets)) " or " (card-str state (second targets)) " to the Stack")
                   :effect (req (when (= (count targets) 2)
                                     (show-wait-prompt state :contestant "Challenger to decide which card to move")
                                     (continue-ability
                                       state
                                       :challenger
                                        {:player :challenger
                                         :priority 1
                                         :prompt "Select a card to move to the Stack"
                                         :choices [(card-str state (first targets)) (card-str state (second targets))]
                                         :effect (req (let [c (installed-byname state :challenger target)]
                                                        (clear-wait-prompt state :contestant)
                                                        (move state :challenger c :deck {:front true})
                                                        (system-msg state :challenger (str "selected " (:title c) " to move to the Stack"))))}
                                         card nil)))}]}

   "Ice Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}

   "Ichi 1.0"
   {:subroutines [trash-program
                  (trace-ability 1 {:label "Give the Challenger 1 tag and do 1 brain damage"
                                    :msg "give the Challenger 1 tag and do 1 brain damage"
                                    :delayed-completion true
                                    :effect (req (when-completed (damage state :challenger :brain 1 {:card card})
                                                                 (tag-challenger state :challenger eid 1)))})]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Ichi 2.0"
   {:subroutines [trash-program
                  (trace-ability 3 {:label "Give the Challenger 1 tag and do 1 brain damage"
                                    :msg "give the Challenger 1 tag and do 1 brain damage"
                                    :delayed-completion true
                                    :effect (req (when-completed (damage state :challenger :brain 1 {:card card})
                                                                 (tag-challenger state :challenger eid 1)))})]
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
    :subroutines [trash-installed]}

   "IP Block"
   {:abilities [(assoc give-tag :req (req (not-empty (filter #(has-subtype? % "AI") (all-installed state :challenger))))
                                :label "Give the Challenger 1 tag if there is an installed AI")]
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
    :rez-cost-bonus (req (count (:hand contestant)))
    :leave-play (req (remove-watch state (keyword (str "iq" (:cid card)))))}

   "Ireress"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:bad-publicity contestant 0) " subroutines")}]
    :subroutines [{:msg "make the Challenger lose 1 [Credits]"
                   :effect (effect (lose :challenger :credit 1))}]}

   "Its a Trap!"
   {:expose {:msg "do 2 net damage"
             :delayed-completion true
             :effect (effect (damage eid :net 2 {:card card}))}
    :subroutines [(assoc trash-installed :effect (req (trash state side target {:cause :subroutine})
                                                      (when current-character
                                                        (no-action state side nil)
                                                        (continue state side nil))
                                                      (trash state side card)))]}

   "Janus 1.0"
   {:subroutines [(do-brain-damage 1)]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Kakugo"
   {:events {:pass-character {:delayed-completion true
                        :req (req (= target card))
                        :msg "do 1 net damage"
                        :effect (effect (damage eid :net 1 {:card card}))}}
    :subroutines [end-the-run]}

   "Kitsune"
   {:subroutines [{:prompt "Select a card in HQ to force access"
                   :choices {:req in-hand?}
                   :label "Force the Challenger to access a card in HQ"
                   :msg (msg "force the Challenger to access " (:title target))
                   :effect (req (trash state side card)
                                (when-completed (handle-access state side targets)
                                  (when-completed (trigger-event-sync state side :pre-access :hq)
                                    (let [from-hq (dec (access-count state side :hq-access))]
                                      (continue-ability
                                        state :challenger
                                        (access-helper-hq
                                          state from-hq
                                          ; access-helper-hq uses a set to keep track of which cards have already
                                          ; been accessed. by adding HQ root's contents to this set, we make the challenger
                                          ; unable to access those cards, as Kitsune intends.
                                          (conj (set (get-in @state [:contestant :servers :hq :content])) target))
                                       card nil)))))}]}

   "Komainu"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand challenger)) " subroutines")}]
    :subroutines [(do-net-damage 1)]}

   "Lab Dog"
   {:subroutines [(assoc trash-hardware :label "Force the Challenger to trash an installed piece of hardware"
                                        :player :challenger
                                        :msg (msg "force the Challenger to trash " (:title target))
                                        :effect (req (trash state side target)
                                                     (when current-character
                                                       (no-action state side nil)
                                                       (continue state side nil))
                                                     (trash state side card)))]}

   "Lancelot"
   (grail-character trash-program)

   "Lieutenant of Morgul"
   {:subroutines [end-the-run]}

   "Little Engine"
   {:subroutines [end-the-run
                  {:msg "make the Challenger gain 5 [Credits]" :effect (effect (gain :challenger :credit 5))}]}

   "Lockdown"
   {:subroutines [{:label "The Challenger cannot draw cards for the remainder of this turn"
                   :msg "prevent the Challenger from drawing cards" :effect (effect (prevent-draw))}]}

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
   (morph-character "Sentry" "Code Gate" trash-program)

   "Macrophage"
   {:subroutines [(trace-ability 4 {:label "Purge virus counters"
                                    :msg "purge virus counters"
                                    :effect (effect (purge))})
                  (trace-ability 3 {:label "Trash a virus"
                                    :prompt "Choose a virus to trash"
                                    :msg (msg "trash " (:title target))
                                    :choices {:req #(and (installed? %)
                                                         (has? % :subtype "Virus"))}
                                    :effect (effect (trash target {:cause :subroutine})
                                                    (clear-wait-prompt :challenger))})
                  (trace-ability 2 {:label "Remove a virus in the Heap from the game"
                                    :prompt "Choose a virus in the Heap to remove from the game"
                                    :choices (req (cancellable (filter #(has? % :subtype "Virus") (:discard challenger)) :sorted))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (effect (move :challenger target :rfg))})
                  (trace-ability 1 end-the-run)]}

   "Magnet"
   {:delayed-completion true
    :effect (req (let [magnet card]
                   (continue-ability
                     state side
                     {:req (req (some #(some (fn [h] (card-is? h :type "Program")) (:hosted %))
                                      (remove-once #(not= (:cid %) (:cid magnet)) (all-installed state contestant))))
                      :prompt "Select a Program to host on Magnet"
                      :choices {:req #(and (card-is? % :type "Program")
                                           (character? (:host %))
                                           (not= (:cid (:host %)) (:cid magnet)))}
                      :effect (req (let [hosted (host state side card target)]
                                     (unregister-events state side hosted)
                                     (update! state side (dissoc hosted :abilities))))}
                     card nil)))
    :events {:challenger-install {:req (req (= (:cid card) (:cid (:host target))))
                              :effect (req (doseq [c (get-in card [:hosted])]
                                             (unregister-events state side c)
                                             (update! state side (dissoc c :abilities)))
                                           (update-character-strength state side card))}}
    :subroutines [end-the-run]}

   "Mamba"
   {:abilities [(power-counter-ability (do-net-damage 1))]
    :subroutines [(do-net-damage 1)
                  (do-psi add-power-counter)]}

   "Marker"
   {:subroutines [{:label "Give the next Character encountered \"End the run\" for the remainder of the run"
                   :msg (msg "give the next Character encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}

   "Markus 1.0"
   {:subroutines [trash-installed end-the-run]
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
                   :msg (msg "gain " (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3) " [Credits]")
                   :effect (effect (gain :credit (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3)))}
                  {:label "Do 1 net damage (Do 3 net damage)"
                   :delayed-completion true
                   :msg (msg "do " (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3) " net damage")
                   :effect (effect (damage eid :net (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3) {:card card}))}
                  {:label "Give the Challenger 1 tag (and end the run)"
                   :delayed-completion true
                   :msg (msg "give the Challenger 1 tag"
                             (when (<= 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) " and end the run"))
                   :effect (req (tag-challenger state :challenger eid 1)
                                (when (<= 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0)))
                                  (end-run state side)))}]}

   "Merlin"
   (grail-character (do-net-damage 2))

   "Meru Mati"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))}

   "Metamorph"
   {:subroutines [{:label "Swap two Character or swap two installed non-Character"
                   :msg "swap two Character or swap two installed non-Character"
                   :delayed-completion true
                   :prompt "Choose one"
                   :choices ["Swap two Character" "Swap two non-Character"]
                   :effect (req (if (= target "Swap two Character")
                                  (continue-ability state side {:prompt "Select the two Character to swap"
                                                                :delayed-completion true
                                                                :choices {:req #(and (installed? %) (character? %)) :max 2 :all true}
                                                                :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                                                :effect (req (when (= (count targets) 2)
                                                                               (swap-character state side (first targets) (second targets))
                                                                               (effect-completed state side eid card)))} card nil)
                                  (continue-ability state side {:prompt "Select the two cards to swap"
                                                                :delayed-completion true
                                                                :choices {:req #(and (installed? %) (not (character? %))) :max 2 :all true}
                                                                :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                                                :effect (req (when (= (count targets) 2)
                                                                               (swap-installed state side (first targets) (second targets))
                                                                               (effect-completed state side eid card)))} card nil)))}]}

   "Mganga"
   {:subroutines [(do-psi {:label "do 2 net damage"
                           :delayed-completion true
                           :player :contestant
                           :effect (req (when-completed (damage state :contestant :net 2 {:card card})
                                                        (trash state :contestant eid card nil)))}
                          {:label "do 1 net damage"
                           :delayed-completion true
                           :player :contestant
                           :effect (req (when-completed (damage state :contestant :net 1 {:card card})
                                                        (trash state :contestant eid card nil)))})]}

   "Mind Game"
   {:subroutines [(do-psi {:label "Redirect the run to another server"
                           :player :contestant
                           :prompt "Choose a server"
                           :choices (req (remove #{(-> @state :run :server central->name)} servers))
                           :msg (msg "redirect the run to " target)
                           :effect (req (let [dest (server->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc % :position (count (get-in contestant (conj dest :characters)))
                                                           :server (rest dest)))))})]
    :challenger-abilities [{:label "Add an installed card to the bottom of your Stack"
                        :prompt "Choose one of your installed cards"
                        :choices {:req #(and (installed? %)
                                             (= (:side %) "Challenger"))}
                        :effect (effect (move target :deck)
                                        (system-msg :challenger (str "adds " (:title target) " to the bottom of their Stack")))}]}

   "Minelayer"
   {:subroutines [{:msg "install an Character from HQ"
                   :choices {:req #(and (character? %)
                                        (in-hand? %))}
                   :prompt "Choose an Character to install from HQ"
                   :effect (req (contestant-install state side target (zone->name (first (:server run))) {:no-install-cost true}))}]}

   "Mirāju"
   {:abilities [{:label "Challenger broke subroutine: Redirect run to Archives"
                 :msg "make the Challenger continue the run on Archives. Mirāju is derezzed"
                 :effect (req (swap! state update-in [:run]
                                     #(assoc % :position (count (get-in contestant [:servers :archives :characters]))
                                               :server [:archives]))
                              (derez state side card))}]
    :subroutines [{:label "Draw 1 card, then shuffle 1 card from HQ into R&D"
                   :effect (req (when-completed (resolve-ability state side
                                                  {:optional
                                                   {:delayed-completion true
                                                    :prompt "Draw 1 card?"
                                                    :yes-ability {:msg "draw 1 card"
                                                                  :effect (effect (draw))}
                                                    :no-ability {:effect (req (effect-completed state side eid))}}}
                                                 card nil)
                                                (resolve-ability state side
                                                  {:prompt "Choose 1 card in HQ to shuffle into R&D"
                                                   :choices {:req #(and (in-hand? %) (= (:side %) "Contestant"))}
                                                   :msg "shuffle 1 card in HQ into R&D"
                                                   :effect (effect (move target :deck)
                                                                   (shuffle! :deck))}
                                                 card nil)))}]}

   "Mother Goddess"
   (let [ab (effect (update! (let [subtype (->> (mapcat :characters (flatten (seq (:servers contestant))))
                                                (filter #(and (rezzed? %) (not= (:cid card) (:cid %))))
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
      :events {:rez mg :card-moved mg :derez mg :character-subtype-changed mg}})

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
   (space-character trash-program)

   "Negotiator"
   {:subroutines [(gain-credits 2)
                  trash-program]
    :challenger-abilities [(challenger-break [:credit 2] 1)]}

   "Nerine 2.0"
   {:subroutines [{:label "Do 1 brain damage and Contestant may draw 1 card"
                   :delayed-completion true
                   :msg "do 1 brain damage"
                   :effect (req (when-completed (damage state :challenger :brain 1 {:card card})
                                                (resolve-ability state side
                                                  {:optional
                                                   {:prompt "Draw 1 card?"
                                                    :yes-ability {:msg "draw 1 card"
                                                                  :effect (effect (draw))}
                                                    :no-ability {:effect (req (effect-completed state side eid))}}}
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
              {:rez nb :derez nb :trash nb :card-moved nb})}

   "NEXT Gold"
   {:subroutines [{:label "Do 1 net damage for each rezzed NEXT character"
                   :msg (msg "do " (next-character-count contestant) " net damage")
                   :effect (effect (damage eid :net (next-character-count contestant) {:card card}))}
                  trash-program]}

   "NEXT Opal"
   {:subroutines [{:label "Install a card from HQ, paying all costs"
                   :prompt "Choose a card in HQ to install"
                   :priority true
                   :choices {:req #(and (not (is-type? % "Resource"))
                                        (in-hand? %)
                                        (= (:side %) "Contestant"))}
                   :effect (effect (contestant-install target nil))
                   :msg (msg (contestant-install-msg target))}]}

   "NEXT Silver"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (filter #(and (is-type? % "Character")
                                                        (has-subtype? % "NEXT")
                                                        (rezzed? %))
                                                  (all-installed state :contestant))) " subroutines")}]
    :subroutines [end-the-run]}

   "Nightdancer"
   {:subroutines [{:label "The Challenger loses [Click], if able. You have an additional [Click] to spend during your next turn."
                   :msg "force the challenger to lose a [Click], if able. Contestant gains an additional [Click] to spend during their next turn"
                   :effect (req
                             (lose state :challenger :click 1)
                             (swap! state update-in [:contestant :extra-click-temp] (fnil inc 0)))}]}

   "Orc Tracker"
   {:abilities [{:label "Move"
                 :prompt "Choose a server" :choices (req servers)
                 :msg (msg "move another party " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}

   "Orion"
   ;; TODO: wormhole subroutine
   (implementation-note "\"Resolve a subroutine...\" subroutine is not implemented"
                        (space-character trash-program end-the-run))

   "Owl"
   {:subroutines [{:choices {:req #(and (installed? %)
                                        (is-type? % "Program"))}
                   :label "Add installed program to the top of the Challenger's Stack"
                   :msg "add an installed program to the top of the Challenger's Stack"
                   :effect (effect (move :challenger target :deck {:front true})
                                   (system-msg (str "adds " (:title target) " to the top of the Challenger's Stack")))}]}

   "Pachinko"
   {:subroutines [end-the-run-if-tagged]}

   "Paper Wall"
   {:implementation "Trash on break is manual"
    :subroutines [end-the-run]}

   "Pop-up Window"
   {:implementation "Encounter effect is manual. Challenger choice is not implemented"
    :abilities [(gain-credits 1)]
    :subroutines [end-the-run]
    :challenger-abilities [(challenger-break [:credit 1] 1)]}

   "Pup"
   {:subroutines [(do-net-damage 1)]
    :challenger-abilities [(challenger-break [:credit 1] 1)]}

   "Quandary"
   {:subroutines [end-the-run]}

   "Quicksand"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (and this-server (= (dec (:position run)) (character-index state card))))
                 :label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (update-all-character))}]
    :subroutines [end-the-run]
    :strength-bonus (req (get-in card [:counter :power] 0))}

   "Rainbow"
   {:subroutines [end-the-run]}

   "Ravana 1.0"
   {:subroutines [{:label "Resolve a subroutine on another piece of rezzed bioroid Character"
                   :choices {:req #(and (rezzed? %) (character? %) (has-subtype? % "Bioroid"))}
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
   {:subroutines [trash-program end-the-run]}

   "Sagittarius"
   (constellation-character trash-program)

   "Salvage"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [(tag-trace 2)]}

   "Sand Storm"
   {:subroutines [{:req (req (:run @state))
                   :label "Move Sand Storm and the run to another server"
                   :prompt "Choose another server and redirect the run to its outermost position"
                   :choices (req (cancellable servers))
                   :msg (msg "move Sand Storm and the run.  The Challenger is now running on " target ". Sand Storm is trashed")
                   :effect (req (let [dest (server->zone state target)]
                                (trash state side card {:unpreventable true})
                                (swap! state update-in [:run]
                                       #(assoc % :position (count (get-in contestant (conj dest :characters)))
                                                 :server (rest dest)))))}]}

   "Sapper"
   {:subroutines [trash-program]
    :access {:delayed-completion true
             :req (req (and (not= (first (:zone card)) :discard)
                            (some #(is-type? % "Program") (all-installed state :challenger))))
             :effect (effect (show-wait-prompt :contestant "Challenger to decide to break Sapper subroutine")
                             (continue-ability
                               :challenger {:optional
                                        {:player :challenger
                                         :prompt "Allow Sapper subroutine to fire?"
                                         :priority 1
                                         :yes-ability {:effect (req (clear-wait-prompt state :contestant)
                                                                    (show-wait-prompt state :challenger "Contestant to trash a program with Sapper")
                                                                    (play-subroutine state :contestant eid {:card card :subroutine 0}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :contestant)
                                                                      (effect-completed eid))}}}
                              card nil))}}

   "Searchlight"
   {:advanceable :always
    ;; Could replace this with (tag-trace advance-counters).
    :subroutines [{:label "Trace X - Give the Challenger 1 tag"
                   :trace {:base advance-counters
                           :delayed-completion true
                           :effect (effect (tag-challenger :challenger eid 1))
                           :msg "give the Challenger 1 tag"}}]}

   "Seidr Adaptive Barrier"
   {:effect (req (let [srv (second (:zone card))]
                   (add-watch state (keyword (str "sab" (:cid card)))
                              (fn [k ref old new]
                                (let [characters (count (get-in new [:contestant :servers srv :characters]))]
                                  (when (not= (count (get-in old [:contestant :servers srv :characters])) characters)
                                    (update! ref side (assoc (get-card ref card) :strength-bonus characters))
                                    (update-character-strength ref side (get-card ref card))))))))
    :strength-bonus (req (count (:characters (card->server state card))))
    :leave-play (req (remove-watch state (keyword (str "sab" (:cid card)))))
    :subroutines [end-the-run]}

   "Self-Adapting Code Wall"
   {:subroutines [end-the-run]
    :flags {:cannot-lower-strength true}}

   "Sensei"
   {:subroutines [{:label "Give each other Character encountered \"End the run\" for the remainder of the run"
                   :msg (msg "give each other Character encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}

   "Shadow"
   {:advanceable :always
    :subroutines [(gain-credits 2)
                  (tag-trace 3)]
    :strength-bonus advance-counters}

   "Sherlock 1.0"
   {:subroutines [{:label "Trace 4 - Add an installed program to the top of the Challenger's Stack"
                   :trace {:base 4
                           :choices {:req #(and (installed? %)
                                                (is-type? % "Program"))}
                           :msg (msg "add " (:title target) " to the top of the Challenger's Stack")
                           :effect (effect (move :challenger target :deck {:front true}))}}]
    :challenger-abilities [(challenger-break [:click 1] 1)]}

   "Sherlock 2.0"
   {:subroutines [{:label "Trace 4 - Add an installed program to the bottom of the Challenger's Stack"
                   :trace {:base 4
                           :choices {:req #(and (installed? %)
                                                (is-type? % "Program"))}
                           :msg     (msg "add " (:title target) " to the bottom of the Challenger's Stack")
                           :effect  (effect (move :challenger target :deck))}}
                  {:label  "Give the Challenger 1 tag"
                   :msg    "give the Challenger 1 tag"
                   :delayed-completion true
                   :effect (effect (tag-challenger :challenger eid 1))}]
    :challenger-abilities [(challenger-break [:click 2] 2)]}

   "Shinobi"
   {:effect take-bad-pub
    :subroutines [(trace-ability 1 (do-net-damage 1))
                  (trace-ability 2 (do-net-damage 2))
                  (trace-ability 3 {:label "Do 3 net damage and end the run"
                                    :msg "do 3 net damage and end the run"
                                    :effect (effect (damage eid :net 3 {:card card}) (end-run))})]}

   "Shiro"
   {:subroutines [{:label "Rearrange the top 3 cards of R&D"
                   :msg "rearrange the top 3 cards of R&D"
                   :delayed-completion true
                   :effect (req (show-wait-prompt state :challenger "Contestant to rearrange the top cards of R&D")
                                (let [from (take 3 (:deck contestant))]
                                  (if (pos? (count from))
                                    (continue-ability state side (reorder-choice :contestant :challenger from '()
                                                                                 (count from) from) card nil)
                                    (do (clear-wait-prompt state :challenger)
                                        (effect-completed state side eid card)))))}
                  {:label "Force the Challenger to access the top card of R&D"
                   :effect (req (doseq [c (take (get-in @state [:challenger :rd-access]) (:deck contestant))]
                                  (system-msg state :challenger (str "accesses " (:title c)))
                                  (handle-access state side [c])))}]}

   "Snoop"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (= current-character card))
                 :label "Reveal all cards in the Challenger's Grip"
                 :msg (msg "reveal the Challenger's Grip ( " (join ", " (map :title (:hand challenger))) " )")}
                {:req (req (> (get-in card [:counter :power] 0) 0))
                 :counter-cost [:power 1]
                 :label "Hosted power counter: Reveal all cards in Grip and trash 1 card"
                 :msg (msg "look at all cards in Grip and trash " (:title target)
                           " using 1 power counter")
                 :choices (req (cancellable (:hand challenger) :sorted))
                 :prompt "Choose a card to trash"
                 :effect (effect (trash target))}]
    :subroutines [(trace-ability 3 add-power-counter)]}

   "Snowflake"
   {:subroutines [(do-psi end-the-run)]}

   "Special Offer"
   {:subroutines [{:label "Gain 5 [Credits] and trash Special Offer"
                   :effect (req (gain state :contestant :credit 5)
                                (when current-character
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card)
                                (system-msg state side (str "gains 5 [Credits] and trashes Special Offer")))}]}

   "Spiderweb"
   {:subroutines [end-the-run]}

   "Susanoo-no-Mikoto"
   {:subroutines [{:req (req (not= (:server run) [:discard]))
                   :msg "make the Challenger continue the run on Archives"
                   :effect (req (swap! state update-in [:run]
                                       #(assoc % :position (count (get-in contestant [:servers :archives :characters]))
                                                 :server [:archives])))}]}

   "Swarm"
   {:effect take-bad-pub
    :advanceable :always
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [trash-program]
    :challenger-abilities [(challenger-break [:credit 3] 1)]}

   "Swordsman"
   {:implementation "AI restriction not implemented"
    :subroutines [(do-net-damage 1)
                  {:prompt "Select an AI program to trash"
                   :msg (msg "trash " (:title target))
                   :label "Trash an AI program"
                   :effect (effect (trash target))
                   :choices {:req #(and (installed? %)
                                        (is-type? % "Program")
                                        (has-subtype? % "AI"))}}]}

   "SYNC BRE"
   {:subroutines [(trace-ability 4 give-tag)
                  (trace-ability 2 {:label "Challenger reduces cards accessed by 1 for this run"
                                    :delayed-completion true
                                    :msg "reduce cards accessed for this run by 1"
                                    :effect (effect (access-bonus -1))})]}

   "Tapestry"
   {:subroutines [{:label "force the Challenger to lose 1 [Click], if able"
                   :msg "force the Challenger to lose 1 [Click]"
                   :req (req (pos? (:click challenger)))
                   :effect (effect (lose :challenger :click 1))}
                  {:msg "draw 1 card"
                   :effect (effect (draw))}
                  {:req (req (pos? (count (:hand contestant))))
                   :prompt "Choose a card in HQ to move to the top of R&D"
                   :choices {:req #(and (in-hand? %) (= (:side %) "Contestant"))}
                   :msg "add 1 card in HQ to the top of R&D"
                   :effect (effect (move target :deck {:front true}))}]}

   "Taurus"
   (constellation-character trash-hardware)

   "The Grimburgoth"
   {:subroutines [end-the-run]}

   "Thoth"
   {:implementation "Encounter effect is manual"
    :challenger-abilities [{:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :challenger "takes 1 tag on encountering Thoth")
                                     (tag-challenger state :challenger eid 1))}]
    :subroutines [(trace-ability 4 {:label "Do 1 net damage for each Challenger tag"
                                    :delayed-completion true
                                    :msg (msg "do " (:tag challenger) " net damage")
                                    :effect (effect (damage eid :net (:tag challenger) {:card card}))})
                  (trace-ability 4 {:label "Challenger loses 1 [Credits] for each tag"
                                    :delayed-completion true
                                    :msg (msg "force the Challenger to lose " (:tag challenger) " [Credits]")
                                    :effect (effect (lose :challenger :credit (:tag challenger)))})]}

   "Tithonium"
   {:alternative-cost [:forfeit]
    :implementation "Does not handle UFAQ for Pawn or Blackguard interaction"
    :cannot-host true
    :subroutines [trash-program
                  end-the-run
                  {:label "Trash a muthereff"
                   :msg (msg "trash " (:title target))
                   :delayed-completion true
                   :choices {:req #(and (installed? %)
                                        (is-type? % "Muthereff"))}
                   :effect (effect (trash target {:reason :subroutine}))}]}

   "TL;DR"
   {:subroutines [{:msg "duplicate subroutines on next piece of Character encountered this run"}]}

   "TMI"
   {:trace {:base 2
            :msg "keep TMI rezzed"
            :unsuccessful {:effect (effect (derez card))}}
    :subroutines [end-the-run]}

   "Tollbooth"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "make the Challenger pay 3 [Credits], if able"
                 :effect (effect (pay :challenger card :credit 3))}]
    :subroutines [end-the-run]}

   "Tour Guide"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (filter #(and (is-type? % "Asset") (rezzed? %))
                                                  (all-installed state :contestant))) " subroutines")}]
    :subroutines [end-the-run]}

   "Tribunal"
   {:subroutines [{:msg "force the Challenger to trash 1 installed card"
                   :effect (effect (resolve-ability :challenger trash-installed card nil))}]}

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
    :strength-bonus (req (if (is-remote? (second (:zone card))) 3 0))
    :challenger-abilities [(challenger-break [:click 3] 1)]}

   "Turnpike"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "force the Challenger to lose 1 [Credits]"
                 :effect (effect (lose :challenger :credit 1))}]
    :subroutines [(tag-trace 5)]}

   "Tyrant"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [end-the-run]}

   "Universal Connectivity Fee"
   {:subroutines [{:label "Force the Challenger to lose credits"
                   :msg (msg "force the Challenger to lose " (if tagged "all credits" "1 [Credits]"))
                   :effect (req (if tagged
                                  (do (lose state :challenger :credit :all :run-credit :all)
                                      (when current-character
                                        (no-action state side nil)
                                        (continue state side nil))
                                      (trash state side card))
                                  (lose state :challenger :credit 1)))}]}

   "Upayoga"
   {:implementation "\"Resolve a subroutine...\" subroutine is not implemented"
    :subroutines [(do-psi {:label "Make the Challenger lose 2 [Credits]"
                           :msg "make the Challenger lose 2 [Credits]"
                           :effect (effect (lose :challenger :credit 2))})
                  {:msg "resolve a subroutine on a piece of rezzed psi Character"}]}

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
                   :effect (effect (gain :contestant :credit 2))}
                  {:label "Challenger loses 2 [Credits]"
                   :msg "force the Challenger to lose 2 [Credits]"
                   :effect (effect (lose :challenger :credit 2))}
                  (trace-ability 2 give-tag)]}

   "Vikram 1.0"
   {:implementation "Program prevention is not implemented"
    :subroutines [{:msg "prevent the Challenger from using programs for the remainder of this run"}
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
                                    :effect (effect (lose :challenger :click 1))})
                  (trace-ability 3 end-the-run)]}

   "Virgo"
   (constellation-character give-tag)

   "Waiver"
   {:subroutines [(trace-ability 5 {:label "Reveal the Challenger's Grip and trash cards"
                                    :msg (msg "reveal all cards in the Challenger's Grip: " (join ", " (map :title (:hand challenger)))
                                              ". Cards with a play/install cost less than or equal to " (- target (second targets))
                                              " will be trashed")
                                    :effect (req (let [delta (- target (second targets))]
                                                   (doseq [c (:hand challenger)]
                                                     (when (<= (:cost c) delta)
                                                       (resolve-ability
                                                         state side
                                                         {:msg (msg "trash " (:title c))
                                                          :effect (effect (trash c))}
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
                   :req (req (pos? (:click challenger)))
                   :msg "force the Challenger to lose 1 [Click]"
                   :effect (effect (lose :challenger :click 1))}
                  {:label "Challenger trashes 1 card from their Grip"
                   :req (req (pos? (count (:hand challenger))))
                   :prompt "Choose a card to trash from your Grip"
                   :player :challenger
                   :choices (req (:hand challenger))
                   :not-distinct true
                   :effect (effect (trash :challenger target)
                                   (system-msg :challenger (str "trashes " (:title target) " from their Grip")))}]}

   "Wendigo"
   (implementation-note
     "Program prevention is not implemented"
     (morph-character "Code Gate" "Barrier"
                {:msg "prevent the Challenger from using a chosen program for the remainder of this run"}))

   "Whirlpool"
   {:subroutines [{:msg "prevent the Challenger from jacking out"
                   :effect (req (when (and (is-remote? (second (:zone card)))
                                           (> (count (concat (:characters (card->server state card))
                                                             (:content (card->server state card)))) 1))
                                  (prevent-jack-out state side))
                                (when current-character
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card))}]}

   "Woodcutter"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [(do-net-damage 1)]}

   "Wormhole"
   ;; TODO: create an ability for wormhole
   (implementation-note "Wormhole subroutine is not implemented"
                        (space-character))

   "Wotan"
   {:subroutines [end-the-run
                  (do-brain-damage 1)]
    :challenger-abilities [(challenger-break [:click 2] 1)
                       (challenger-break [:credit 3] 1)]}

   "Wraparound"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "Fracter") (all-installed state :challenger))
                           0 7))
    :events (let [wr {:silent (req true)
                      :req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "Fracter")))
                      :effect (effect (update-character-strength card))}]
              {:challenger-install wr :trash wr :card-moved wr})}

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
    :subroutines [trash-hardware
                  (do-brain-damage 2)]
    :challenger-abilities [(challenger-break [:click 2] 2)]}})
