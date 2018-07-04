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
  {"ability" core/play-ability
   "access" core/successful-run
   "advance" core/advance
   "change" core/change
   "choice" core/resolve-prompt
   "close-deck" core/close-deck
   "concede" core/concede
   "continue" core/continue
   "contestant-ability" core/play-contestant-ability
   "contestant-phase-43" core/contestant-phase-43
   "credit" core/click-credit
   "derez" #(core/derez %1 %2 (:card %3))
   "draw" core/click-draw
   "dynamic-ability" core/play-dynamic-ability
   "end-phase-12" core/end-phase-12
   "end-turn" core/end-turn
   "jack-out" core/jack-out
   "keep" core/keep-hand
   "move" core/move-card
   "mulligan" core/mulligan
   "no-action" core/no-action
   "play" core/play
   "purge" core/do-purge
   "remove-tag" core/remove-tag
   "rez" #(core/rez %1 %2 (:card %3) nil)
   "run" core/click-run
   "challenger-ability" core/play-challenger-ability
   "score" #(core/score %1 %2 (game.core/get-card %1 (:card %3)))
   "select" core/select
   "shuffle" core/shuffle-deck
   "start-turn" core/start-turn
   "subroutine" core/play-subroutine
   "system-msg" #(core/system-msg %1 %2 (:msg %3))
   "toast" core/toast
   "trash-muthereff" core/trash-muthereff
   "view-deck" core/view-deck})

(defn strip [state]
  (-> state
    (dissoc :events :turn-events :per-turn :prevent :damage :effect-completed :click-state :turn-state)
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
      (update-in [:rig :facedown] #(private-card-vector state :challenger %))
      (update-in [:rig :muthereff] #(private-card-vector state :challenger %))))

(defn- make-private-contestant [state]
  (let [zones (concat [[:hand]] [[:discard]] [[:deck]]
                      (for [server (keys (:servers (:contestant @state)))] [:servers server :characters])
                      (for [server (keys (:servers (:contestant @state)))] [:servers server :content]))]
    (loop [s (:contestant @state)
           z zones]
      (if (empty? z)
        s
        (recur (update-in s (first z) #(private-card-vector state :contestant %)) (next z))))))

(defn- make-private-deck [state side deck]
  (if (:view-deck (side @state))
    deck
    (private-card-vector state side deck)))

(defn- private-states
  "Generates privatized states for the Contestant, Challenger and any spectators from the base state.
  If `:spectatorhands` is on, all information is passed on to spectators as well."
  [state]
  ;; contestant, challenger, spectator
  (let [contestant-private (make-private-contestant state)
        challenger-private (make-private-challenger state)
        contestant-deck (update-in (:contestant @state) [:deck] #(make-private-deck state :contestant %))
        challenger-deck (update-in (:challenger @state) [:deck] #(make-private-deck state :challenger %))]
    [(assoc @state :challenger challenger-private
                   :contestant contestant-deck)
     (assoc @state :contestant contestant-private
                   :challenger challenger-deck)
     (if (get-in @state [:options :spectatorhands])
       (assoc @state :contestant contestant-deck :challenger challenger-deck)
       (assoc @state :contestant contestant-private :challenger challenger-private))]))

(defn- reset-all-cards
  [cards]
  (let [;; split the cards into regular cards and alt-art cards
        [regular alt] ((juxt filter remove) #(not= "Alternates" (:setname %)) cards)
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
     :contestant-diff   contestant-diff
     :spect-diff  spect-diff}))

(defn set-action-id
  "Creates a unique action id for each server response - used in client lock"
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
