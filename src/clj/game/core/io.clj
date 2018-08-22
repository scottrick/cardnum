(in-ns 'game.core)

(declare character-index parse-command show-error-toast)

(defn say
  "Prints a message to the log as coming from the given username. The special user string
  __system__ shows no user name."
  [state side {:keys [user text]}]
  (let [author (or user (get-in @state [side :user]))
        text (if (= (.trim text) "null") " null" text)]
    (if-let [command (parse-command text)]
      (when (and (not= side nil) (not= side :spectator))
        (command state side)
        (when-not (or (= text "/z") (= text "/o"))
          (swap! state update-in [:log] #(conj % {:user nil :text (str "[!]" (:username author) " uses a command: " text)}))
          )
        )
      (swap! state update-in [:log] #(conj % {:user author :text text})))
    (swap! state assoc :typing (remove #{(:username author)} (:typing @state)))))

(defn typing
  "Updates game state list with username of whoever is typing"
  [state side {:keys [user]}]
  (let [author (:username (or user (get-in @state [side :user])))]
    (swap! state assoc :typing (distinct (conj (:typing @state) author)))
    ;; say something to force update in client side rendering
    (say state side {:user "__system__" :text "typing"})))

(defn typingstop
  "Clears typing flag from game state for user"
  [state side {:keys [user text]}]
  (let [author (or user (get-in @state [side :user]))]
    (swap! state assoc :typing (remove #{(:username author)} (:typing @state)))
    ;; say something to force update in client side rendering
    (say state side {:user "__system__" :text "typing"})))

(defn system-say
  "Prints a system message to log (`say` from user __system__)"
  ([state side text] (system-say state side text nil))
  ([state side text {:keys [hr]}]
   (say state side {:user "__system__" :text (str text (when hr "[hr]"))})))

(defn system-msg
  "Prints a message to the log without a username."
  ([state side text] (system-msg state side text nil))
  ([state side text args]
   (let [username (get-in @state [side :user :username])]
     (system-say state side (str username " " text ".") args))))

(defn enforce-msg
  "Prints a message related to a rules enforcement on a given card.
  Example: 'Architect cannot be discarded while placed.'"
  [state card text]
  (say state nil {:user (get-in card [:title]) :text (str (:title card) " " text ".")}))

(defn toast
  "Adds a message to toast with specified severity (default as a warning) to the toast message list.
  If message is nil, removes first toast in the list.
  For options see http://codeseven.github.io/toastr/demo.html
  Currently implemented options:
    - msg-type (warning, info etc)
    - time-out (sets both timeOut and extendedTimeOut currently)
    - close-button
    - prevent-duplicates"
  ([state side message] (toast state side message "warning" nil))
  ([state side message msg-type] (toast state side message msg-type nil))
  ([state side message msg-type options]
   ;; Allows passing just the toast msg-type as the options parameter
   (if message
     ;; normal toast - add to list
     (swap! state update-in [side :toast] #(conj % {:msg message :type msg-type :options options}))
     ;; no message - remove top toast from list
     (swap! state update-in [side :toast] rest))))

(defn play-sfx
  "Adds a sound effect to play to the sfx queue.
  Each SFX comes with a unique ID, so each client can track for themselves which sounds have already been played.
  The sfx queue has size limited to 3 to limit the sound torrent tabbed out or lagged players will experience."
  [state side sfx]
  (when-let [current-id (get-in @state [:sfx-current-id])]
    (do
      (swap! state update-in [:sfx] #(take 3 (conj % {:id (inc current-id) :name sfx})))
      (swap! state update-in [:sfx-current-id] inc))))

;;; "ToString"-like methods
(defn card-str
  "Gets a string description of an placed card, reflecting whether it is revealed,
  in/protecting a locale, facedown, or hosted."
  ([state card] (card-str state card nil))
  ([state card {:keys [visible] :as args}]
  (str (if (card-is? card :side :contestant)
         ; Contestant card messages
         (str (if (or (revealed? card) visible) (:title card) (if (character? card) "Character" "a card"))
              ; Hosted cards do not need "in locale 1" messages, host has them
              (if-not (:host card)
                (str (if (character? card) " protecting " " in ")
                     ;TODO add naming of scoring area of contestant/challenger
                     (zone->name (second (:zone card)))
                     (if (character? card) (str " at position " (character-index state card))))))
         ; Challenger card messages
         (if (or (:facedown card) visible) "a facedown card" (:title card)))
       (if (:host card) (str " hosted on " (card-str state (:host card)))))))

(defn card-sb
  "Gets a string description of an placed card, reflecting whether it is revealed,
  in/protecting a locale, facedown, or hosted."
  [card]
  (str (:type card)))

(defn name-zone
  "Gets a string representation for the given zone."
  [side zone]
  (match (vec zone)
         [:hand] "Hand"
         [:discard] "Discard"
         [:deck] "Play Deck"
         [:sideboard] "Sideboard"
         [:fw-dc-sb] "FW-DC-SB"
         [:location] "Location Deck"
         [:rig _] "in play"
         [:locales :hq _] "the root of Hand"
         [:locales :rd _] "the root of Deck"
         [:locales :archives _] "the root of Discard"
         [:locales :sites _] "the root of Location"
         :else (zone->name (second zone))))

;;; In-game chat commands
(defn set-adv-counter [state side target value]
  (set-prop state side target :advance-counter value)
  (system-msg state side (str "sets advancement counters to " value " on "
                              (card-str state target)))
  (trigger-event state side :advancement-placed target))

(defn command-adv-counter [state side value]
  (resolve-ability state side
                   {:effect (effect (set-adv-counter target value))
                    :choices {:req (fn [t] (card-is? t :side side))}}
                   {:title "/adv-counter command"} nil))

(defn command-counter-smart [state side args]
  (resolve-ability
    state side
    {:effect (req (let [existing (:counter target)
                        value (if-let [n (string->num (first args))] n 0)
                        counter-type (cond (= 1 (count existing)) (first (keys existing))
                                     (can-be-advanced? target) :advance-counter
                                     (and (is-type? target "Agenda") (is-scored? state side target)) :agenda
                                     (and (card-is? target :side :challenger) (has-subtype? target "Virus")) :virus)
                        advance (= :advance-counter counter-type)]
                    (cond
                      advance
                      (set-adv-counter state side target value)

                      (not counter-type)
                      (toast state side
                             (str "Could not infer what counter type you mean. Please specify one manually, by typing "
                                  "'/counter TYPE " value "', where TYPE is advance, agenda, credit, power, or virus.")
                             "error" {:time-out 0 :close-button true})

                      :else
                      (do (set-prop state side target :counter (merge (:counter target) {counter-type value}))
                          (system-msg state side (str "sets " (name counter-type) " counters to " value " on "
                                                      (card-str state target)))))))
     :choices {:req (fn [t] (card-is? t :side side))}}
    {:title "/counter command"} nil))

(defn command-facedown [state side]
  (resolve-ability state side
                   {:prompt "Select a card to place facedown"
                    :choices {:req #(and (= (:side %) "Challenger")
                                         (in-hand? %))}
                    :effect (effect (challenger-place target {:facedown true}))}
                   {:title "/faceup command"} nil))

(defn command-counter [state side args]
  (cond
    (empty? args)
    (command-counter-smart state side `("1"))

    (= 1 (count args))
    (command-counter-smart state side args)

    :else
    (let [typestr (.toLowerCase (first args))
          value (if-let [n (string->num (second args))] n 1)
          one-letter (if (<= 1 (.length typestr)) (.substring typestr 0 1) "")
          two-letter (if (<= 2 (.length typestr)) (.substring typestr 0 2) one-letter)
          counter-type (cond (= "v" one-letter) :virus
                             (= "p" one-letter) :power
                             (= "c" one-letter) :credit
                             (= "ag" two-letter) :agenda
                             :else :advance-counter)
          advance (= :advance-counter counter-type)]
      (if advance
        (command-adv-counter state side value)
        (resolve-ability state side
                       {:effect (effect (set-prop target :counter (merge (:counter target) {counter-type value}))
                                        (system-msg (str "sets " (name counter-type) " counters to " value " on "
                                                         (card-str state target))))
                        :choices {:req (fn [t] (card-is? t :side side))}}
                       {:title "/counter command"} nil)))))

(defn command-revealall [state side value]
  (resolve-ability state side
                   {:optional {:prompt "Reveal all cards and turn cards in discard faceup?"
                               :yes-ability {:effect (req
                                                       (swap! state update-in [:contestant :discard] #(map (fn [c] (assoc c :seen true)) %))
                                                       (doseq [c (all-placed state side)]
                                                         (when-not (:revealed c)
                                                           (reveal state side c {:ignore-cost :all-costs :force true}))))}}}
                   {:title "/reveal-all command"} nil))

(defn command-revtop [state side value]
  (resolve-ability state side
                   {:effect (req
                              (let [prior (last (get-in @state [side :discard]))]
                                (update! state side (assoc prior :seen true))
                                (when-not (:revealed prior)
                                  (reveal state side prior {:ignore-cost :all-costs :force true}))))}
                   {:title "/revtop command"} nil))

(defn command-roll [state side value]
  (system-msg state side (str "rolls a " value " sided die and rolls a " (inc (rand-int value)))))

;  :background (:options @app-state))
;  (system-msg state side (str "rolls a " pick"-"(inc (rand-int 6))"+"size pick"-"(inc (rand-int 6))"+"size))))
; (get-in p [:user :options :deckstats])
; player (side @state)

(defn basic-roll [state side]
  (let [player (side @state)
        pick (get-in player [:user :options :dice-pick])
        size (get-in player [:user :options :dice-size])]
  (system-msg state side (str "rolls a " pick"-"(inc (rand-int 6))"+"size " " pick"-"(inc (rand-int 6))"+"size))))

(defn command-undo-click
  "Resets the game state back to start of the click"
  [state side]
  (when-let [click-state (:click-state @state)]
    (when (= (:active-player @state) side)
      (reset! state (dissoc (assoc click-state :log (:log @state) :click-state click-state) :run))
      (doseq [s [:challenger :contestant]]
        (toast state s "Game reset to start of click")))))

(defn command-undo-turn
  "Resets the entire game state to how it was at end-of-turn if both players agree"
  [state side]
  (when-let [turn-state (:turn-state @state)]
    (swap! state assoc-in [side :undo-turn] true)
    (when (and (-> @state :challenger :undo-turn) (-> @state :contestant :undo-turn))
      (reset! state (assoc turn-state :log (:log @state) :turn-state turn-state))
      (doseq [s [:challenger :contestant]]
        (toast state s "Game reset to start of turn")))))

(defn command-close-prompt [state side]
  (when-let [fprompt (-> @state side :prompt first)]
    (swap! state update-in [side :prompt] rest)
    (swap! state dissoc-in [side :selected])
    (effect-completed state side (:eid fprompt))))

(defn blind-zoom
  [state side args]
  (if (get-in @state [side :blind])
    (swap! state assoc-in [side :blind] false)
    (swap! state assoc-in [side :blind] true))
  )

(defn blind-hold
  [state side card]
  (swap! state assoc-in [side :hold-card] card)
  )

(defn option-key-down
  [state side args]
  (if (get-in @state [side :opt-key])
    (swap! state assoc-in [side :opt-key] false)
    (swap! state assoc-in [side :opt-key] true))
  )

(defn host-any-card
  [state side args]
  (resolve-ability state side
                   {:prompt "Select hosting card"
                    :choices {:req (fn [t] true)}
                    :msg (msg "host " (:title target))
                    :effect (req (let [c (get-card state target)] (resolve-ability state side
                                                                                   {:prompt "Select card to host"
                                                                                    :choices {:req (fn [t] true)}
                                                                                    :effect (effect (host c (get-card state target)))
                                                                                    ; host target onto card
                                                                                    } c nil )))} nil nil)
  (system-msg state side "host a card")
  )

(defn starter-path [state side st-path]
  (let [
        b-path1 (.replace st-path " b " " Border-land ")
        b-path2 (.replace b-path1 " b " " Border-land ")
        c-path1 (.replace b-path2 " c " " Coastal Sea ")
        c-path2 (.replace c-path1 " c " " Coastal Sea ")
        d-path1 (.replace c-path2 " d " " Dark-domain ")
        d-path2 (.replace d-path1 " d " " Dark-domain ")
        e-path1 (.replace d-path2 " e " " Desert ")
        e-path2 (.replace e-path1 " e " " Desert ")
        f-path1 (.replace e-path2 " f " " Free-domain ")
        f-path2 (.replace f-path1 " f " " Free-domain ")
        j-path1 (.replace f-path2 " j " " Jungle ")
        j-path2 (.replace j-path1 " j " " Jungle ")
        s-path1 (.replace j-path2 " s " " Shadow-land ")
        s-path2 (.replace s-path1 " s " " Shadow-land ")
        w-path1 (.replace s-path2 " w " " Wilderness ")
        w-path2 (.replace w-path1 " w " " Wilderness ")
        ]
    (system-msg state side w-path2)))

(defn parse-command [text]
  (let [[command & args] (split text #" ");"
        value (if-let [n (string->num (first args))] n 1)
        num   (if-let [n (-> args first (safe-split #"#") second string->num)] (dec n) 0)]
    (when (<= (count args) 2)
      (if (= (ffirst args) \#)
        (case command
          "/deck"       #(move %1 %2 (nth (get-in @%1 [%2 :hand]) num nil) :deck {:front true})
          "/discard"    #(move %1 %2 (nth (get-in @%1 [%2 :hand]) num nil) :discard)
          nil)
        (case command
          "/adv-counter" #(command-adv-counter %1 %2 value)
          "/bp"         #(swap! %1 assoc-in [%2 :bad-publicity] (max 0 value))
          "/card-info"  #(resolve-ability %1 %2
                                          {:effect (effect (system-msg (str "shows card-info of "
                                                                            (card-str state target)
                                                                            ": " (get-card state target))))
                                           :choices {:req (fn [t] (card-is? t :side %2))}}
                                          {:title "/card-info command"} nil)
          "/clear-win"  clear-win
          "/click"      #(swap! %1 assoc-in [%2 :click] (max 0 value))
          "/close-prompt" command-close-prompt
          "/counter"    #(command-counter %1 %2 args)
          "/credit"     #(swap! %1 assoc-in [%2 :credit] (max 0 value))
          "/deck"       #(toast %1 %2 "/deck number takes the format #n")
          "/discard"        #(resolve-ability %1 %2
                                          {:prompt "Select a card to discard"
                                           :effect (req (let [c (deactivate %1 %2 target)]
                                                          (move %1 %2 c :discard)))
                                           :choices {:req (fn [t] (card-is? t :side %2))}}
                                          {:title "/discard command"} nil)
          "/discard-n"    #(toast %1 %2 "/discard number takes the format #n")
          "/discard-random" #(move %1 %2 (rand-nth (get-in @%1 [%2 :hand])) :discard)
          "/draw"       #(draw %1 %2 (max 0 value))
          "/end-run"    #(when (= %2 :contestant) (end-run %1 %2))
          "/error"      show-error-toast
          "/handsize"   #(swap! %1 assoc-in [%2 :hand-size-modification] (- (max 0 value) (:hand-size-base %2)))
          "/hide"  #(resolve-ability %1 %2
                                        {:prompt "Select a card to hide"
                                         :effect (req (let [c (deactivate %1 %2 target)]
                                                        (hide %1 %2 c)))
                                         :choices {:req (fn [t] (card-is? t :side %2))}}
                                        {:title "/hide command"} nil)
          "/hide-hand"   #(hide-hand %1 %2)
          "/host"       #(host-any-card %1 %2 args)
          "/jack-out"   #(when (= %2 :challenger) (jack-out %1 %2 nil))
          "/link"       #(swap! %1 assoc-in [%2 :link] (max 0 value))
          "/memory"     #(swap! %1 assoc-in [%2 :memory] value)
          "/move-bottom"  #(resolve-ability %1 %2
                                            {:prompt "Select a card in hand to put on the bottom of your deck"
                                             :effect (effect (move target :deck))
                                             :choices {:req (fn [t] (and (card-is? t :side %2) (in-hand? t)))}}
                                            {:title "/move-bottom command"} nil)
          "/move-deck"   #(resolve-ability %1 %2
                                           {:prompt "Select a card to move to the top of your deck"
                                            :effect (req (let [c (deactivate %1 %2 target)]
                                                           (move %1 %2 c :deck {:front true})))
                                            :choices {:req (fn [t] (card-is? t :side %2))}}
                                           {:title "/move-deck command"} nil)
          "/move-hand"  #(resolve-ability %1 %2
                                          {:prompt "Select a card to move to your hand"
                                           :effect (req (let [c (deactivate %1 %2 target)]
                                                          (move %1 %2 c :hand)))
                                           :choices {:req (fn [t] (card-is? t :side %2))}}
                                          {:title "/move-hand command"} nil)
          "/move-site"  #(resolve-ability %1 %2
                                          {:prompt "Select a site to move to your Location Deck"
                                           :effect (req (let [c (deactivate %1 %2 target)]
                                                          (move %1 %2 c :location)))
                                           :choices {:req (fn [t] (card-is? t :side %2))}}
                                          {:title "/move-site command"} nil)
          "/move-sb"  #(resolve-ability %1 %2
                                          {:prompt "Select a site to move to your sideboard"
                                           :effect (req (let [c (deactivate %1 %2 target)]
                                                          (move %1 %2 c :sideboard)))
                                           :choices {:req (fn [t] (card-is? t :side %2))}}
                                          {:title "/move-sb command"} nil)
          "/move-fw-sb"  #(resolve-ability %1 %2
                                        {:prompt "Select a site to move to your FW-Sideboard"
                                         :effect (req (let [c (deactivate %1 %2 target)]
                                                        (move %1 %2 c :fw-dc-sb)))
                                         :choices {:req (fn [t] (card-is? t :side %2))}}
                                        {:title "/move-fw-sb command"} nil)
          "/o"          #(option-key-down %1 %2 nil)
          "/p"          #(resolve-ability %1 %2
                                          {:prompt "Select a site for starter movement"
                                           :effect (req (starter-path %1 %2 (:Path target)))
                                           :choices {:req (fn [t] (and (card-is? t :side %2)
                                                                       (= "Site" (:type t))
                                                                       (or  (= "Hero" (:alignment t))
                                                                            (= "Minion" (:alignment t)))))}}
                                          {:title "/p command"} nil)
          "/re-deck"    #(resolve-ability %1 %2
                                        {:effect (effect (shuffle-into-deck {} :discard))}
                                        {:title "/re-deck command"} nil)
          "/psi"        #(when (= %2 :contestant) (psi-game %1 %2
                                                      {:title "/psi command" :side %2}
                                                      {:equal  {:msg "resolve equal bets effect"}
                                                       :not-equal {:msg "resolve unequal bets effect"}}))
          "/reveal-hand"   #(reveal-hand %1 %2)
          "/reveal"        #(resolve-ability %1 %2
                                            {:effect (effect (reveal target {:ignore-cost :all-costs :force true}))
                                             :choices {:req (fn [t] (card-is? t :side %2))}}
                                            {:title "/reveal command"} nil)
          "/reveal-all"    #(when (= %2 :contestant) (command-revealall %1 %2 value))
          "/revtop"     #(when (= %2 :contestant) (command-revtop %1 %2 value))
          "/rfg"        #(resolve-ability %1 %2
                                          {:prompt "Select a card to remove from the game"
                                           :effect (req (let [c (deactivate %1 %2 target)]
                                                          (move %1 %2 c :rfg)))
                                           :choices {:req (fn [t] (card-is? t :side %2))}}
                                          {:title "/rfg command"} nil)
          "/rfgh"       #(resolve-ability %1 %2
                                          {:prompt "Select a card to rfg facedown"
                                           :effect (req (move %1 %2 (assoc target :hide true) :rfg))
                                           :choices {:req (fn [t] (card-is? t :side %2))}}
                                          {:title "/rfgh command"} nil)
          "/score"      #(resolve-ability %1 %2
                                          {:prompt "Select a card to score"
                                           :effect (req (let [c  target]
                                                          (move %1 %2 c :scored)))
                                           :choices {:req (fn [t] true)}}
                                          {:title "/score command"} nil)
          "/facedown"   #(command-facedown %1 %2)
          "/roll"       #(command-roll %1 %2 value)
          "/r"          #(basic-roll %1 %2)
          "/tag"        #(swap! %1 assoc-in [%2 :tag] (max 0 value))
          "/trace"      #(when (= %2 :contestant) (init-trace %1 %2
                                                        {:title "/trace command" :side %2}
                                                        {:base (max 0 value)
                                                         :msg "resolve successful trace effect"}))
          "/undo-click" #(command-undo-click %1 %2)
          "/undo-turn"  #(command-undo-turn %1 %2)
          "/z"          #(blind-zoom %1 %2 nil)
          nil)))))

(defn contestant-place-msg
  "Gets a message describing where a card has been placed from. Example: Interns. "
  [card]
  (str "place " (if (:seen card) (:title card) "an unseen card") " from " (name-zone :contestant (:zone card))))

(defn turn-message
  "Prints a message for the start or end of a turn, summarizing credits and cards in hand."
  [state side start-of-turn]
  (let [pre (if start-of-turn "started" "is ending")
        hand "Hand"
        cards (count (get-in @state [side :hand]))
        credits (get-in @state [side :credit])
        text (str pre " their turn " (:turn @state) " with " cards " cards")]
    (system-msg state side text {:hr (not start-of-turn)})))

(defn event-title
  "Gets a string describing the internal engine event keyword"
  [event]
  (if (keyword? event)
    (name event)
    (str event)))

(defn show-error-toast
  [state side]
  (when state
    (toast state side
           (str "Your last action caused a game error on the locale. You can keep playing, but there "
                "may be errors in the game's current state. Please click the button below to submit a report "
                "to our GitHub issues page.<br/><br/>Use /error to see this message again.")
           "exception"
           {:time-out 0 :close-button true})))

