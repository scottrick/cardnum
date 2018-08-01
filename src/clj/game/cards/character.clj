(ns game.cards.character
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int]]
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
   :effect (effect (tag-challenger :challenger eid n))})

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
            :successful ability}})
  ([base {:keys [label] :as ability} {:keys [un-label] :as un-ability}]
   {:label (str "Trace " base " - " label " / " un-label)
    :trace {:base base
            :successful ability
            :unsuccessful un-ability}}))

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
  (req (if (odd? (get-counters (get-card state card) :advancement))
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
  {})
