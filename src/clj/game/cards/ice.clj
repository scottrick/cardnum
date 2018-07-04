(in-ns 'game.core)

(declare discard-resource discard-hazard discard-muthereff-sub discard-placed)

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
  "Bad pub on reveal effect."
  (effect (gain :bad-publicity 1)
          (system-msg (str "takes 1 bad publicity from " (:title card)))))


;;; For Advanceable Character
(def advance-counters
  "Number of advancement counters - for advanceable Character."
  (req (+ (:advance-counter card 0) (:extra-advance-counter card 0))))

(def space-character-reveal-bonus
  "Amount of reveal reduction for the Space Character."
  (req (* -3 (+ (:advance-counter card 0) (:extra-advance-counter card 0)))))

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
(def cards-ice
  {})