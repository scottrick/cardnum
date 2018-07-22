(ns test.core
  (:require [game.utils :refer [remove-once has? merge-costs zone make-cid to-keyword capitalize
                                costs-to-symbol vdissoc distinct-by]]
            [game.macros :refer [effect req msg]]
            [clojure.string :refer [split-lines split join]]
            [game.core :as core :refer [all-cards]]
            [test.utils :refer [load-card load-cards qty default-contestant default-challenger
                                make-deck]]
            [test.macros :refer [do-game]]
            [clojure.test :refer :all]))

;;; Click action functions
(defn take-credits
  "Take credits for n clicks, or if no n given, for all remaining clicks of a side.
  If all clicks are used up, end turn and start the opponent's turn."
  ([state side] (take-credits state side nil))
  ([state side n]
    (let  [remaining-clicks (get-in @state [side :click])
           n (or n remaining-clicks)
           other (if (= side :contestant) :challenger :contestant)]
      (dotimes [i n] (core/click-credit state side nil))
      (if (= (get-in @state [side :click]) 0)
        (do (core/end-turn state side nil)
            (core/start-turn state other nil))))))

(defn new-game
  "Init a new game using given contestant and challenger. Keep starting hands (no mulligan) and start Contestant's turn."
  ([contestant challenger] (new-game contestant challenger nil))
  ([contestant challenger {:keys [mulligan start-as dont-start-turn dont-start-game] :as args}]
    (let [states (core/init-game
                   {:gameid 1
                    :players [{:side "Contestant"
                               :deck {:identity (@all-cards (:identity contestant))
                                      :cards (:deck contestant)}}
                              {:side "Challenger"
                               :deck {:identity (@all-cards (:identity challenger))
                                      :cards (:deck challenger)}}]})
          state (second (last states))]
      (when-not dont-start-game
        (if (#{:both :contestant} mulligan)
          (core/resolve-prompt state :contestant {:choice "Mulligan"})
          (core/resolve-prompt state :contestant {:choice "Keep"}))
        (if (#{:both :challenger} mulligan)
          (core/resolve-prompt state :challenger {:choice "Mulligan"})
          (core/resolve-prompt state :challenger {:choice "Keep"}))
        (when-not dont-start-turn (core/start-turn state :contestant nil))
        (when (= start-as :challenger) (take-credits state :contestant)))
      state)))

(defn load-all-cards []
  (reset! game.core/all-cards (into {} (map (juxt :title identity) (map #(assoc % :cid (make-cid)) (load-cards))))))
(load-all-cards)

;;; Card related functions
(defn find-card
  "Return a card with given title from given sequence"
  [title from]
  (some #(when (= (:title %) title) %) from))

(defn card-ability
  "Trigger a card's ability with its 0-based index. Refreshes the card argument before
  triggering the ability."
  ([state side card ability] (card-ability state side card ability nil))
  ([state side card ability targets]
   (core/play-ability state side {:card (core/get-card state card)
                                  :ability ability :targets targets})))

(defn card-subroutine
  "Trigger a piece of character's subroutine with the 0-based index."
  ([state side card ability] (card-subroutine state side card ability nil))
  ([state side card ability targets]
   (core/play-subroutine state side {:card (core/get-card state card)
                                     :subroutine ability :targets targets})))

(defn get-character
  "Get placed character protecting locale by position."
  [state locale pos]
  (get-in @state [:contestant :locales locale :characters pos]))

(defn get-content
  "Get card in a locale by position. If no pos, get all cards in the locale."
  ([state locale]
   (get-in @state [:contestant :locales locale :content]))
  ([state locale pos]
   (get-in @state [:contestant :locales locale :content pos])))

(defn get-resource
  "Get non-hosted resource by position."
  ([state] (get-in @state [:challenger :rig :resource]))
  ([state pos]
   (get-in @state [:challenger :rig :resource pos])))

(defn get-hazard
  "Get hazard by position."
  ([state] (get-in @state [:challenger :rig :hazard]))
  ([state pos]
   (get-in @state [:challenger :rig :hazard pos])))

(defn get-radicle
  "Get non-hosted radicle by position."
  [state pos]
  (get-in @state [:challenger :rig :radicle pos]))

(defn get-scored
  "Get a card from the score area. Can find by name or index.
  If no index or name provided, get the first scored agenda."
  ([state side] (get-scored state side 0))
  ([state side x]
   (if (number? x)
     ;; Find by index
     (get-in @state [side :scored x])
     ;; Find by name
     (when (string? x)
       (find-card x (get-in @state [side :scored]))))))

(defn get-counters
  "Get number of counters of specified type."
  [card type]
  (get-in card [:counter type] 0))

(defn play-from-hand
  "Play a card from hand based on its title. If placing a Contestant card, also indicate
  the locale to place into with a string."
  ([state side title] (play-from-hand state side title nil))
  ([state side title locale]
    (core/play state side {:card (find-card title (get-in @state [side :hand]))
                           :locale locale})))


;;; Run functions
(defn play-run-event
  "Play a run event with a replace-access effect on an unprotected locale.
  Advances the run timings to the point where replace-access occurs."
  [state card locale]
  (core/play state :challenger {:card card})
  (is (= [locale] (get-in @state [:run :locale])) "Correct locale is run")
  (is (get-in @state [:run :run-effect]) "There is a run-effect")
  (core/no-action state :contestant nil)
  (core/successful-run state :challenger nil)
  (is (get-in @state [:challenger :prompt]) "A prompt is shown")
  (is (get-in @state [:run :successful]) "Run is marked successful"))

(defn run-on
  "Start run on specified locale."
  [state locale]
  (core/click-run state :challenger {:locale locale}))

(defn run-continue
  "No action from contestant and continue for challenger to proceed in current run."
  [state]
  (core/no-action state :contestant nil)
  (core/continue state :challenger nil))

(defn run-phase-43
  "Ask for triggered abilities phase 4.3"
  [state]
  (core/contestant-phase-43 state :contestant nil)
  (core/successful-run state :challenger nil))

(defn run-successful
  "No action from contestant and successful run for challenger."
  [state]
  (core/no-action state :contestant nil)
  (core/successful-run state :challenger nil))

(defn run-jack-out
  "is done facing attack(s)"
  [state]
  (core/jack-out state :challenger nil))

(defn run-empty-locale
  "Make a successful run on specified locale, assumes no character in place."
  [state locale]
  (run-on state locale)
  (run-successful state))


;;; Misc functions
(defn score-agenda
  "Take clicks and credits needed to advance and score the given agenda."
  ([state _ card]
   (let [title (:title card)
         advancementcost (:advancementcost card)]
    (core/gain state :contestant :click advancementcost :credit advancementcost)
    (dotimes [n advancementcost]
      (core/advance state :contestant {:card (core/get-card state card)}))
    (is (= advancementcost (get-in (core/get-card state card) [:advance-counter])))
    (core/score state :contestant {:card (core/get-card state card)})
    (is (find-card title (get-in @state [:contestant :scored]))))))

(defn advance
  "Advance the given card."
  ([state card] (advance state card 1))
  ([state card n]
   (dotimes [_ n]
     (core/advance state :contestant {:card (core/get-card state card)}))))

(defn last-log-contains?
  [state content]
  (not (nil?
         (re-find (re-pattern content)
                  (get (last (get @state :log)) :text)))))

(defn second-last-log-contains?
  [state content]
  (not (nil?
         (re-find (re-pattern content)
                  (get (last (butlast (get @state :log))) :text)))))

(defn discard-from-hand
  "Discard specified card from hand of specified side"
  [state side title]
  (core/discard state side (find-card title (get-in @state [side :hand]))))

(defn discard-radicle
  "Discard specified card from rig of the challenger"
  [state title]
  (core/discard state :challenger (find-card title (get-in @state [:challenger :rig :radicle]))))

(defn starting-hand
  "Moves all cards in the player's hand to their draw pile, then moves the specified card names
  back into the player's hand."
  [state side cards]
  (doseq [c (get-in @state [side :hand])]
    (core/move state side c :deck))
  (doseq [ctitle cards]
    (core/move state side (find-card ctitle (get-in @state [side :deck])) :hand)))

(defn accessing
  "Checks to see if the challenger has a prompt accessing the given card title"
  [state title]
  (= title (-> @state :challenger :prompt first :card :title)))

(load "core-game")
