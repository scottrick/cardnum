(ns game.main
  (:require [cheshire.core :refer [parse-string generate-string]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [game.core :refer [card-is-public? game-states show-error-toast toast] :as core]
            [game.utils :refer [private-card]]
            [cardnum.cards :refer [all-cards]]
            [differ.core :as differ]))

(add-encoder java.lang.Object encode-str)

(def spectator-commands
  {"typing" core/typing
   "typingstop" core/typingstop})

(def commands
  {
   "ability" core/play-ability
   "access" core/successful-run
   "advance" core/advance
   "back-m-h" core/back-m-h
   "back-org" core/back-org
   "back-site" core/back-site
   "blind-hold" #(core/blind-hold %1 %2 (:card %3))
   "blind-zoom" core/blind-zoom
   "bluff-o-g" core/bluff-o-g
   "challenger-ability" core/play-challenger-ability
   "change" core/change
   "choice" core/resolve-prompt
   "close-deck" core/close-deck
   "close-fw-dc-sb" core/close-fw-dc-sb
   "close-location" core/close-location
   "close-sideboard" core/close-sideboard
   "hide-hand" core/hide-hand
   "concede" core/concede
   "contestant-ability" core/play-contestant-ability
   "contestant-phase-43" core/contestant-phase-43
   "continue" core/continue
   "credit" core/click-credit
   "discard-radicle" core/discard-radicle
   "draw" core/click-draw
   "dynamic-ability" core/play-dynamic-ability
   "end-phase-12" core/end-phase-12
   "end-turn" core/end-turn
   "eot-discard" core/eot-discard
   "eot-phase" core/eot-phase
   "equip" #(core/equip %1 %2 (:card %3))
   "flip" #(core/flip %1 %2 (:card %3))
   "haz-play-done" core/haz-play-done
   "hide" #(core/hide %1 %2 (:card %3))
   "invert" #(core/invert %1 %2 (:card %3))
   "jack-out" core/jack-out
   "keep" core/keep-hand
   "m-h-phase" core/m-h-phase
   "move-to-sb" #(core/move-to-sb %1 %2 (:card %3))
   "move" core/move-card
   "mulligan" core/mulligan
   "next-m-h" core/next-m-h
   "next-site" core/next-site
   "no-action" core/no-action
   "no-hazards" core/no-hazards
   "not-first" core/not-first
   "on-guard" core/on-guard
   "option-key-down" core/option-key-down
   "org-phase" core/org-phase
   "organize" #(core/organize %1 %2 (:card %3) nil)
   "place" #(core/equip %1 %2 (:card %3))
   "play" core/play
   "pre-bluff" core/pre-bluff
   "purge" core/do-purge
   "regionize" #(core/regionize %1 %2 (:card %3))
   "remove-tag" core/remove-tag
   "reset-done" core/reset-done
   "reset-m-h" core/reset-m-h
   "reset-org" core/reset-org
   "reset-site" core/reset-site
   "return-o-g" core/return-o-g
   "reveal-hand" core/reveal-hand
   "reveal-o-g" core/reveal-o-g
   "reveal" #(core/reveal %1 %2 (:card %3) nil)
   "rotate" #(core/rotate %1 %2 (:card %3))
   "run" core/click-run
   "score" #(core/score %1 %2 (game.core/get-card %1 (:card %3)))
   "select" core/select
   "shuffle" core/shuffle-deck
   "site-phase" core/site-phase
   "start-turn" core/start-turn
   "subroutine" core/play-subroutine
   "system-msg" #(core/system-msg %1 %2 (:msg %3))
   "tap" #(core/tap %1 %2 (:card %3))
   "toast" core/toast
   "transfer" #(core/transfer %1 %2 (:card %3))
   "untap-all" core/untap-all
   "untap" #(core/untap %1 %2 (:card %3))
   "view-deck" core/view-deck
   "view-fw-dc-sb" core/view-fw-dc-sb
   "view-location" core/view-location
   "view-sideboard" core/view-sideboard
   "wait-alert" core/wait-alert
   "wound" #(core/wound %1 %2 (:card %3))
   })

(defn strip [state]
  (-> state
    (dissoc :events :turn-events :per-turn :prevent :effect-completed :click-state :turn-state)
    (update-in [:contestant :register] dissoc :most-recent-drawn)
    (update-in [:challenger :register] dissoc :most-recent-drawn)))

(defn not-spectator?
  "Returns true if the specified user in the specified state is not a spectator"
  [state user]
  (let [contestant-id (get-in @state [:contestant :user :_id])
        challenger-id (get-in @state [:challenger :user :_id])]
    (and state ((set [contestant-id challenger-id]) (:_id user)))))

(defn- private-card-vector [state side cards]
  (vec (map (fn [card]
              (cond
                (not (card-is-public? state side card)) (private-card card)
                (:hosted card) (update-in card [:hosted] #(private-card-vector state side %))
                :else card))
            cards)))

(defn- make-private-challenger [state]
  (-> (:challenger @state)
      (update-in [:hand] #(private-card-vector state :challenger %))
      (update-in [:discard] #(private-card-vector state :challenger %))
      (update-in [:deck] #(private-card-vector state :challenger %))
      (update-in [:sideboard] #(private-card-vector state :challenger %))
      (update-in [:fw-dc-sb] #(private-card-vector state :challenger %))
      (update-in [:location] #(private-card-vector state :challenger %))
      (update-in [:rig :facedown] #(private-card-vector state :challenger %))
      (update-in [:rig :radicle] #(private-card-vector state :challenger %))))

(defn- make-private-contestant [state]
  (let [zones (concat [[:hand]] [[:discard]] [[:deck]] [[:sideboard]] [[:fw-dc-sb]] [[:location]]
                      (for [locale (keys (:locales (:contestant @state)))] [:locales locale :characters])
                      (for [locale (keys (:locales (:contestant @state)))] [:locales locale :content]))]
    (loop [s (:contestant @state)
           z zones]
      (if (empty? z)
        s
        (recur (update-in s (first z) #(private-card-vector state :contestant %)) (next z))))))

(defn- make-private-deck [state side deck]
  (if (:view-deck (side @state))
    deck
    (private-card-vector state side deck)))

(defn- make-private-sideboard [state side sideboard]
  (if (:view-sideboard (side @state))
    sideboard
    (private-card-vector state side sideboard)))

(defn- make-private-fw-dc-sb [state side sideboard]
  (if (:view-fw-dc-sb (side @state))
    sideboard
    (private-card-vector state side sideboard)))

(defn- make-private-location [state side location]
  (let [dc-sorted (if (:cut-region (side @state))
                 (filter #(= (:Region %) (:cut-region (side @state))) location)
                 nil)
        standard (if (:dc (side @state))
                   nil
                   (filter #(= (:dreamcard %) false) dc-sorted))]
    (if (:view-location (side @state))
      (if (:dc (side @state))
        dc-sorted
        standard)
      (private-card-vector state side dc-sorted))))

(defn- private-states
  "Generates privatized states for the Contestant, Challenger and any spectators from the base state.
  If `:spectatorhands` is on, all information is passed on to spectators as well."
  [state]
  ;; contestant, challenger, spectator
  (let [contestant-private (make-private-contestant state)
        challenger-private (make-private-challenger state)
        contestant-deck (update-in (:contestant @state) [:deck] #(make-private-deck state :contestant %))
        challenger-deck (update-in (:challenger @state) [:deck] #(make-private-deck state :challenger %))
        contestant-sideboard (update-in (:contestant @state) [:sideboard] #(make-private-sideboard state :contestant %))
        challenger-sideboard (update-in (:challenger @state) [:sideboard] #(make-private-sideboard state :challenger %))
        contestant-fw-dc-sb (update-in (:contestant @state) [:fw-dc-sb] #(make-private-fw-dc-sb state :contestant %))
        challenger-fw-dc-sb (update-in (:challenger @state) [:fw-dc-sb] #(make-private-fw-dc-sb state :challenger %))
        contestant-location (update-in (:contestant @state) [:location] #(make-private-location state :contestant %))
        challenger-location (update-in (:challenger @state) [:location] #(make-private-location state :challenger %))]
    [(assoc @state :challenger challenger-private :contestant contestant-deck
                   :contestant contestant-sideboard :contestant contestant-fw-dc-sb :contestant contestant-location)
     (assoc @state :contestant contestant-private :challenger challenger-deck
                   :challenger challenger-sideboard :challenger challenger-fw-dc-sb :challenger challenger-location)
     (if (get-in @state [:options :spectatorhands])
       (assoc @state :contestant contestant-deck :contestant contestant-sideboard
                     :contestant contestant-location :contestant contestant-sideboard
                     :challenger challenger-deck :challenger challenger-fw-dc-sb
                     :challenger challenger-location :challenger challenger-fw-dc-sb)
       (assoc @state :contestant contestant-private :challenger challenger-private))]))

(defn- reset-all-cards
  [cards]
  (let [;; split the cards into regular cards and alt-art cards
        [regular alt] ((juxt filter remove) #(not= "Alternates" (:set_code %)) cards)
        regular (into {} (map (juxt :title identity) regular))]
    (reset! all-cards regular)))

(defn public-states [state]
  (let [[new-contestant new-challenger new-spect] (private-states state)]
    {:challenger-state (strip new-challenger)
     :contestant-state   (strip new-contestant)
     :spect-state  (strip new-spect)}))

(defn public-diffs [old-state new-state]
  (let [[old-contestant old-challenger old-spect] (when old-state (private-states (atom old-state)))
        [new-contestant new-challenger new-spect] (private-states new-state)

        challenger-diff (differ/diff (strip old-challenger) (strip new-challenger))
        contestant-diff (differ/diff (strip old-contestant) (strip new-contestant))
        spect-diff (differ/diff (strip old-spect) (strip new-spect))]
    {:challenger-diff challenger-diff
     :contestant-diff contestant-diff
     :spect-diff spect-diff}))

(defn set-action-id
  "Creates a unique action id for each locale response - used in client lock"
  [state side]
  (swap! state update-in [side :aid] (fnil inc 0)))

(defn handle-action
  "Ensures the user is allowed to do command they are trying to do"
  [user command state side args]
  (if (not-spectator? state user)
    (do ((commands command) state side args)
        (set-action-id state side))
    (when-let [cmd (spectator-commands command)]
      (cmd state side args))))

(defn handle-concede
  "Concedes victory from the given player."
  [state side]
  (when (and state side)
    (core/concede state side nil)))

(defn handle-say
  "Adds a message from a user to the chat log."
  [state side user message]
  (when (and state side)
    (core/say state side {:user (select-keys user [:username :emailhash]) :text message})))

(defn handle-notification
  [state text]
  (when state
    (swap! state update-in [:log] #(conj % {:user "__system__" :text text}))))

(defn handle-announcement
  [state text]
  (when state
    (doseq [side [:challenger :contestant]]
      (toast state side text "warning" {:time-out 0 :close-button true}))))

(defn handle-typing
  [state side user typing]
  (when (and state side)
    (if typing
      (core/typing state side {:user user})
      (core/typingstop state side {:user user}))))

(defn handle-rejoin
  [state {:keys [_id username] :as user}]
  (when-let [side (cond
                    (= _id (get-in @state [:contestant :user :_id])) :contestant
                    (= _id (get-in @state [:challenger :user :_id])) :challenger
                    :else nil)]
    (swap! state assoc-in [side :user] user)
    (handle-notification state (str username " rejoined the game."))))
