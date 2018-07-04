(in-ns 'game.core)

;; These functions are called by main.clj in response to commands sent by users.

(declare card-str can-reveal? can-advance? contestant-place effect-as-handler enforce-msg gain-agenda-point get-party-names
         get-run-characters jack-out move name-zone play-instant purge resolve-select run has-subtype?
         challenger-place discard update-breaker-strength update-character-in-locale update-run-character win can-run?
         can-run-locale? can-score? play-sfx)

;;; Neutral actions
(defn play
  "Called when the player clicks a card from hand."
  [state side {:keys [card locale]}]
  (let [card (get-card state card)]
    (case (:type card)
      ("Event" "Operation") (play-instant state side card {:extra-cost [:click 0]})
      ("Hazard" "Muthereff" "Resource") (challenger-place state side (make-eid state) card {:extra-cost [:click 0]})
      ("Character" "Region" "Site" "Agenda") (contestant-place state side card locale {:extra-cost [:click 0] :action :contestant-click-place}))
    (trigger-event state side :play card)))

(defn shuffle-deck
  "Shuffle Play Deck."
  [state side {:keys [close] :as args}]
  (swap! state update-in [side :deck] shuffle)
  (if close
    (do
      (swap! state update-in [side] dissoc :view-deck)
      (system-msg state side "stops looking at their deck and shuffles it"))
    (system-msg state side "shuffles their deck")))

(defn click-draw
  "Click to draw."
  [state side args]
  (when (and (not (get-in @state [side :register :cannot-draw]))
             (pay state side nil :click 1 {:action :contestant-click-draw}))
    (system-msg state side "draws a card")
    (trigger-event state side (if (= side :contestant) :contestant-click-draw :challenger-click-draw) (->> @state side :deck (take 1)))
    (draw state side)
    (play-sfx state side "click-card")))

(defn click-credit
  "Click to gain 1 credit."
  [state side args]
  (when (pay state side nil :click 1 {:action :contestant-click-credit})
    (system-msg state side "spends [Click] to gain 1 [Credits]")
    (gain state side :credit 1)
    (trigger-event state side (if (= side :contestant) :contestant-click-credit :challenger-click-credit))
    (play-sfx state side "click-credit")))

(defn change
  "Increase/decrease a player's property (clicks, credits, MU, etc.) by delta."
  [state side {:keys [key delta]}]
  (let [kw (to-keyword key)
        kt (to-keyword :total_mp)
        ks (to-keyword :stage_pt)
        kg (to-keyword :free_gi)
        kh (to-keyword :hand-size-modification)]
    (if (or (= kw kt)
            (= kw ks)
            (= kw kg)
            (= kw kh))
      (if (neg? delta)
        (deduce state side [kw (- delta)])
        (swap! state update-in [side kw] (partial + delta)))
      (if (neg? delta)
        (do       (deduce state side [kw (- delta)])
                  (deduce state side [kt (- delta)]))
        (do         (swap! state update-in [side kw] (partial + delta))
                    (swap! state update-in [side kt] (partial + delta)))
        )
      )))
;;    (system-msg state side
  ;;              (str "sets " (.replace key "-" " ") " to " (get-in @state [side kw])
    ;;                 " (" (if (pos? delta) (str "+" delta) delta) ")"))))


(defn move-card
  "Called when the user drags a card from one zone to another."
  [state side {:keys [card locale]}]
  (let [c (get-card state card)
        ;; hack: if dragging opponent's card from play-area (Indexing), the previous line will fail
        ;; to find the card. the next line will search in the other player's play-area.
        c (or c (get-card state (assoc card :side (other-side (to-keyword (:side card))))))
        last-zone (last (:zone c))
        src (name-zone (:side c) (:zone c))
        from-str (when-not (nil? src)
                   (if (= :content last-zone)
                     (str " in " src) ; this string matches the message when a card is discarded via (discard)
                     (str " from their " src)))
        label (if (and (not= last-zone :play-area)
                       (not (and (= (:side c) "Challenger")
                                 (= last-zone :hand)
                                 (= locale "Grip")))
                       (or (and (= (:side c) "Challenger")
                                (not (:facedown c)))
                           (revealed? c)
                           (:seen c)
                           (= last-zone :deck)))
                (:title c)
                "a card")
        s (if (#{"HQ" "R&D" "Archives" "Sites"} locale) :contestant :challenger)]
    ;; allow moving from play-area always, otherwise only when same side, and to valid zone
    (when (and (not= src locale)
               (same-side? s (:side card))
               (or (= last-zone :play-area)
                   (same-side? side (:side card))))
      (case locale
        ("Heap" "Archives")
        (do (let [action-str (if (= (first (:zone c)) :hand) "discards " "discards ")]
              (discard state s c {:unpreventable true})
              (system-msg state side (str action-str label from-str))))
        ("Grip" "HQ")
        (do (move state s (dissoc c :seen :revealed) :hand {:force true})
            (system-msg state side (str "moves " label from-str " to their Hand")))
        ("Stack" "R&D")
        (do (move state s (dissoc c :seen :revealed) :deck {:front true :force true})
            (system-msg state side (str "moves " label from-str " to the top of their Play Deck")))
        ("Sites2" "Sites")
        (do (move state s (dissoc c :seen :revealed) :location {:front true :force true})
            (system-msg state side (str "moves " label from-str " to the their Location Deck")))
        nil))))

(defn concede [state side args]
  (system-msg state side "concedes")
  (win state (if (= side :contestant) :challenger :contestant) "Concede"))

(defn- finish-prompt [state side prompt card]
  (when-let [end-effect (:end-effect prompt)]
    (end-effect state side (make-eid state) card nil))
  ;; remove the prompt from the queue
  (swap! state update-in [side :prompt] (fn [pr] (filter #(not= % prompt) pr)))
  ;; This is a dirty hack to end the run when the last access prompt is resolved.
  (when (empty? (get-in @state [:challenger :prompt]))
    (when-let [run (:run @state)]
      (when (:ended run)
        (handle-end-run state :challenger)))
    (swap! state dissoc :access)))

(defn resolve-prompt
  "Resolves a prompt by invoking its effect funtion with the selected target of the prompt.
  Triggered by a selection of a prompt choice button in the UI."
  [state side {:keys [choice card] :as args}]
  (let [localecard (get-card state card)
        card (if (not= (:title card) (:title localecard))
               (@all-cards (:title card))
               localecard)
        prompt (first (get-in @state [side :prompt]))
        choices (:choices prompt)
        choice (if (= (:choices prompt) :credit)
                 (min choice (get-in @state [side :credit]))
                 choice)]
    (if (not= choice "Cancel")
      (if (:card-title choices) ; check the card has a :card-title function
        (let [title-fn (:card-title choices)
              found (some #(when (= (lower-case choice) (lower-case (:title % ""))) %) (vals @all-cards))]
          (if found
            (if (title-fn state side (make-eid state) (:card prompt) [found])
              (do ((:effect prompt) (or choice card))
                  (finish-prompt state side prompt card))
              (toast state side (str "You cannot choose " choice " for this effect.") "warning"))
            (toast state side (str "Could not find a card named " choice ".") "warning")))
        (do (when (= choices :credit) ; :credit prompts require payment
              (pay state side card :credit choice))
            (when (and (map? choices)
                       (:counter choices))
              ;; :Counter prompts deduct counters from the card
              (add-counter state side (:card prompt) (:counter choices) (- choice)))
            ;; trigger the prompt's effect function
            ((:effect prompt) (or choice card))
            (finish-prompt state side prompt card)))
      (do (if-let [cancel-effect (:cancel-effect prompt)]
            ;; trigger the cancel effect
            (cancel-effect choice)
            (effect-completed state side (:eid prompt) nil))
          (finish-prompt state side prompt card)))))

(defn select
  "Attempt to select the given card to satisfy the current select prompt. Calls resolve-select
  if the max number of cards has been selected."
  [state side {:keys [card] :as args}]
  (let [card (get-card state card)
        r (get-in @state [side :selected 0 :req])]
    (when (or (not r) (r card))
      (let [c (update-in card [:selected] not)]
        (update! state side c)
        (if (:selected c)
          (swap! state update-in [side :selected 0 :cards] #(conj % c))
          (swap! state update-in [side :selected 0 :cards]
                 (fn [coll] (remove-once #(not= (:cid %) (:cid card)) coll))))
        (let [selected (get-in @state [side :selected 0])]
          (when (= (count (:cards selected)) (or (:max selected) 1))
            (resolve-select state side)))))))

(defn- do-play-ability [state side card ability targets]
  (let [cost (:cost ability)]
    (when (or (nil? cost)
              (if (has-subtype? card "Run")
                (apply can-pay? state side (:title card) cost (get-in @state [:bonus :run-cost]))
                (apply can-pay? state side (:title card) cost)))
      (when-let [activatemsg (:activatemsg ability)]
        (system-msg state side activatemsg))
      (resolve-ability state side ability card targets))))

(defn play-ability
  "Triggers a card's ability using its zero-based index into the card's card-def :abilities vector."
  [state side {:keys [card ability targets] :as args}]
  (let [card (get-card state card)
        cdef (card-def card)
        abilities (:abilities cdef)
        ab (if (= ability (count abilities))
             ;; recurring credit abilities are not in the :abilities map and are implicit
             {:msg "take 1 [Recurring Credits]" :req (req (> (:rec-counter card) 0))
              :effect (req (add-prop state side card :rec-counter -1)
                             (gain state side :credit 1)
                           (when (has-subtype? card "Stealth")
                             (trigger-event state side :spent-stealth-credit card)))}
             (get-in cdef [:abilities ability]))]
    (when-not (:disabled card)
      (do-play-ability state side card ab targets))))

(defn play-auto-pump
  "Use the 'match strength with character' function of icebreakers."
  [state side args]
  (let [run (:run @state) card (get-card state (:card args))
        current-character (when (and run (> (or (:position run) 0) 0)) (get-card state ((get-run-characters state) (dec (:position run)))))
        pumpabi (some #(when (:pump %) %) (:abilities (card-def card)))
        pumpcst (when pumpabi (second (drop-while #(and (not= % :credit) (not= % "credit")) (:cost pumpabi))))
        strdif (when current-character (max 0 (- (or (:current-strength current-character) (:strength current-character))
                                           (or (:current-strength card) (:strength card)))))
        pumpnum (when strdif (int (Math/ceil (/ strdif (:pump pumpabi)))))]
    (when (and pumpnum pumpcst (>= (get-in @state [:challenger :credit]) (* pumpnum pumpcst)))
      (dotimes [n pumpnum] (resolve-ability state side (dissoc pumpabi :msg) (get-card state card) nil))
      (system-msg state side (str "spends " (* pumpnum pumpcst) " [Credits] to increase the strength of "
                                  (:title card) " to " (:current-strength (get-card state card)))))))

(defn play-copy-ability
  "Play an ability from another card's definition."
  [state side {:keys [card source index] :as args}]
  (let [card (get-card state card)
        source-abis (:abilities (cards (.replace source "'" "")))
        abi (when (< -1 index (count source-abis))
              (nth source-abis index))]
    (when abi
      (do-play-ability state side card abi nil))))

(def dynamic-abilities
  {"auto-pump" play-auto-pump
   "copy" play-copy-ability})

(defn play-dynamic-ability
  "Triggers an ability that was dynamically added to a card's data but is not necessarily present in its
  :abilities vector."
  [state side args]
  ((dynamic-abilities (:dynamic args)) state (keyword side) args))

(defn play-challenger-ability
  "Triggers a contestant card's challenger-ability using its zero-based index into the card's card-def :challenger-abilities vector."
  [state side {:keys [card ability targets] :as args}]
  (let [cdef (card-def card)
        ab (get-in cdef [:challenger-abilities ability])]
    (do-play-ability state side card ab targets)))

(defn play-subroutine
  "Triggers a card's subroutine using its zero-based index into the card's card-def :subroutines vector."
  ([state side args] (play-subroutine state side (make-eid state) args))
  ([state side eid {:keys [card subroutine targets] :as args}]
   (let [cdef (card-def card)
         sub (get-in cdef [:subroutines subroutine])
         cost (:cost sub)]
     (when (or (nil? cost)
               (apply can-pay? state side (:title card) cost))
       (when-let [activatemsg (:activatemsg sub)] (system-msg state side activatemsg))
       (resolve-ability state side eid sub card targets)))))

;;; Contestant actions
(defn discard-muthereff
  "Click to discard a muthereff."
  [state side args]
  (let [discard-cost (max 0 (- 2 (or (get-in @state [:contestant :discard-cost-bonus]) 0)))]
    (when-let [cost-str (pay state side nil :click 1 :credit discard-cost {:action :contestant-discard-muthereff})]
      (resolve-ability state side
                       {:prompt  "Choose a muthereff to discard"
                        :choices {:req (fn [card]
                                         (if (and (seq (filter (fn [c] (undiscardable-while-muthereffs? c)) (all-placed state :challenger)))
                                                  (> (count (filter #(is-type? % "Muthereff") (all-placed state :challenger))) 1))
                                           (and (is-type? card "Muthereff") (not (undiscardable-while-muthereffs? card)))
                                           (is-type? card "Muthereff")))}
                        :cancel-effect (effect (gain :credit discard-cost :click 1))
                        :effect  (effect (discard target)
                                         (system-msg (str (build-spend-msg cost-str "discard")
                                                          (:title target))))} nil nil))))

(defn do-purge
  "Purge viruses."
  [state side args]
  (when-let [cost (pay state side nil :click 3 {:action :contestant-click-purge})]
    (purge state side)
    (let [spent (build-spend-msg cost "purge")
          message (str spent "all virus counters")]
      (system-msg state side message))
    (play-sfx state side "virus-purge")))

(defn reveal
  "Reveal a contestant card."
  ([state side card] (reveal state side (make-eid state) card nil))
  ([state side card args]
   (reveal state side (make-eid state) card args))
  ([state side eid {:keys [disabled] :as card} {:keys [ignore-cost no-warning force no-get-card paid-alt] :as args}]
   (let [card (if no-get-card
                card
                (get-card state card))]
     (if (or force (can-reveal? state side card))
       (do
         (trigger-event state side :pre-reveal card)
         (if (or (#{"Site" "Character" "Region" "Resource" "Hazard"} (:type card))
                   (:place-revealed (card-def card)))
           (do (trigger-event state side :pre-reveal-cost card)
                 (let [cdef (card-def card)
                       cost (reveal-cost state side card)
                       costs (concat (when-not ignore-cost [:credit cost])
                                     (when (and (not= ignore-cost :all-costs)
                                                (not (:disabled card)))
                                       (:additional-cost cdef)))]
                   (when-let [cost-str (apply pay state side card costs)]
                     ;; Deregister the hidden-events before revealing card
                     (when (:hidden-events cdef)
                       (unregister-events state side card))
                     (if (not disabled)
                       (card-init state side (assoc card :revealed true :facedown false))
                       (update! state side (assoc card :revealed true :facedown false)))
                     (doseq [h (:hosted card)]
                       (update! state side (-> h
                                               (update-in [:zone] #(map to-keyword %))
                                               (update-in [:host :zone] #(map to-keyword %)))))
                     (system-msg state side (str (build-spend-msg cost-str "reveal" "reveals")
                                                 (:title card)))
                     (if (character? card)
                       (play-sfx state side "reveal-character")
                       (play-sfx state side "reveal-other"))
                     (trigger-event-sync state side eid :reveal card))))
           (effect-completed state side eid))
         (swap! state update-in [:bonus] dissoc :cost))
       (effect-completed state side eid)))))

(defn hide
  "Hide a contestant card."
  [state side card]
  (let [card (get-card state card)]
    (system-msg state side (str "hides " (:title card)))
    (update! state :contestant (deactivate state :contestant card true))
    (let [cdef (card-def card)]
      (when-let [hide-effect (:hide-effect cdef)]
        (resolve-ability state side hide-effect (get-card state card) nil))
      (when-let [dre (:hidden-events cdef)]
        (register-events state side dre card)))
    (trigger-event state side :hide card side)))

(defn rotate
  "Rotate a card."
  [state side card]
  (let [card (get-card state card)]
    (system-msg state side (str "rotates " (:title card)))
    (update! state side (assoc card :rotated true))))

(defn tap
  "Tap a card."
  [state side card]
  (let [card (get-card state card)]
    (system-msg state side (str "taps " (:title card)))
    (update! state side (assoc card :tapped true :wounded false))))

(defn untap
  "Untap a card."
  [state side card]
  (let [card (get-card state card)]
    (system-msg state side (str "untaps " (:title card)))
    (update! state side (dissoc card :tapped :wounded :inverted :rotated))))

(defn wound
  "Wounds character."
  [state side card]
  (let [card (get-card state card)]
    (system-msg state side (str "wounds " (:title card)))
    (update! state side (assoc card :wounded true))))

(defn invert
  "Inverts a resource."
  [state side card]
  (let [card (get-card state card)]
    (system-msg state side (str "inverts " (:title card)))
    (update! state side (assoc card :inverted true))))

(defn fix-tap
  [state side card]
  (if (:tapped card)
    (untap state side card)
    (tap state side card)))

(defn advance
  "Advance a contestant card that can be advanced.
   If you pass in a truthy value as the 4th parameter, it will advance at no cost (for the card Success)."
  ([state side {:keys [card]}] (advance state side card nil))
  ([state side card no-cost]
   (let [card (get-card state card)]
     (when (can-advance? state side card)
       (when-let [cost (pay state side card :click (if-not no-cost 1 0)
                            :credit (if-not no-cost 1 0) {:action :contestant-advance})]
         (let [spent   (build-spend-msg cost "advance")
               card    (card-str state card)
               message (str spent card)]
           (system-msg state side message))
         (update-advancement-cost state side card)
         (add-prop state side (get-card state card) :advance-counter 1)
         (play-sfx state side "click-advance"))))))

(defn score
  "Score an agenda. It trusts the card data passed to it."
  [state side args]
  (let [card (or (:card args) args)]
    (when (and (can-score? state side card)
               (empty? (filter #(= (:cid card) (:cid %)) (get-in @state [:contestant :register :cannot-score])))
               (>= (:advance-counter card 0) (or (:current-cost card) (:advancementcost card))))

      ;; do not card-init necessarily. if card-def has :effect, wrap a fake event
      (let [moved-card (move state :contestant card :scored)
            c (card-init state :contestant moved-card {:resolve-effect false
                                                 :init-data true})
            points (get-agenda-points state :contestant c)]
        (trigger-event-simult
          state :contestant (make-eid state) :agenda-scored
          {:first-ability {:effect (req (when-let [current (first (get-in @state [:challenger :current]))]
                                          (say state side {:user "__system__" :text (str (:title current) " is discarded.")})
                                          ; This is to handle Employee Strike with damage IDs #2688
                                          (when (:disable-id (card-def current))
                                            (swap! state assoc-in [:contestant :disable-id] true))
                                          (discard state side current)))}
           :card-ability (card-as-handler c)
           :after-active-player {:effect (req (let [c (get-card state c)
                                                    points (or (get-agenda-points state :contestant c) points)]
                                                (set-prop state :contestant (get-card state moved-card) :advance-counter 0)
                                                (system-msg state :contestant (str "scores " (:title c) " and gains "
                                                                             (quantify points "agenda point")))
                                                (swap! state update-in [:contestant :register :scored-agenda] #(+ (or % 0) points))
                                                (swap! state dissoc-in [:contestant :disable-id])
                                                (gain-agenda-point state :contestant points)
                                                (play-sfx state side "agenda-score")))}}
          c)))))

(defn no-action
  "The contestant indicates they have no more actions for the encounter."
  [state side args]
  (swap! state assoc-in [:run :no-action] true)
  (system-msg state side "has no further action")
  (trigger-event state side :no-action)
  (let [run-character (get-run-characters state)
        pos (get-in @state [:run :position])
        character (when (and pos (pos? pos) (<= pos (count run-character)))
              (get-card state (nth run-character (dec pos))))]
    (when (revealed? character)
      (trigger-event state side :encounter-character character)
      (update-character-strength state side character))))

;;; Challenger actions
(defn click-run
  "Click to start a run."
  [state side {:keys [locale] :as args}]
  (let [cost-bonus (get-in @state [:bonus :run-cost])
        click-cost-bonus (get-in @state [:bonus :click-run-cost])]
    (when (and (can-run? state side)
               (can-run-locale? state locale)
               (can-pay? state :challenger "a run" :click 1 cost-bonus click-cost-bonus))
      (swap! state assoc-in [:challenger :register :made-click-run] true)
      (run state side locale)
      (when-let [cost-str (pay state :challenger nil :click 1 cost-bonus click-cost-bonus)]
        (system-msg state :challenger
                    (str (build-spend-msg cost-str "make a run on") locale))
        (play-sfx state side "click-run")))))

(defn remove-tag
  "Click to remove a tag."
  [state side args]
  (let [remove-cost (max 0 (- 2 (or (get-in @state [:challenger :tag-remove-bonus]) 0)))]
    (when-let [cost-str (pay state side nil :click 1 :credit remove-cost)]
      (lose state side :tag 1)
      (system-msg state side (build-spend-msg cost-str "remove 1 tag"))
      (play-sfx state side "click-remove-tag"))))

(defn continue
  "The challenger decides to approach the next character, or the locale itself."
  [state side args]
  (when (get-in @state [:run :no-action])
    (let [run-character (get-run-characters state)
          pos (get-in @state [:run :position])
          cur-character (when (and pos (pos? pos) (<= pos (count run-character)))
                    (get-card state (nth run-character (dec pos))))
          next-character (when (and pos (< 1 pos) (<= (dec pos) (count run-character)))
                     (get-card state (nth run-character (- pos 2))))]
      (when-completed (trigger-event-sync state side :pass-character cur-character)
                      (do (update-character-in-locale
                            state side (get-in @state (concat [:contestant :locales] (get-in @state [:run :locale]))))
                          (swap! state update-in [:run :position] dec)
                          (swap! state assoc-in [:run :no-action] false)
                          (system-msg state side "continues the run")
                          (when cur-character
                            (update-character-strength state side cur-character))
                          (when next-character
                            (trigger-event-sync state side (make-eid state) :approach-character next-character))
                          (doseq [p (filter #(has-subtype? % "Icebreaker") (all-placed state :challenger))]
                            (update! state side (update-in (get-card state p) [:pump] dissoc :encounter))
                            (update-breaker-strength state side p)))))))

(defn view-deck
  "Allows the player to view their deck by making the cards in the deck public."
  [state side args]
  (system-msg state side "looks at their deck")
  (swap! state assoc-in [side :view-deck] true))

(defn close-deck
  "Closes the deck view and makes cards in deck private again."
  [state side args]
  (system-msg state side "stops looking at their deck")
  (swap! state update-in [side] dissoc :view-deck))

(defn view-sideboard
  "Allows the player to view their deck by making the cards in the deck public."
  [state side args]
  (system-msg state side "looks at their sideboard")
  (swap! state assoc-in [side :view-sideboard] true))

(defn close-sideboard
  "Closes the deck view and makes cards in deck private again."
  [state side args]
  (system-msg state side "stops looking at their sideboard")
  (swap! state update-in [side] dissoc :view-sideboard))

(defn view-location
  "Allows the player to view their deck by making the cards in the deck public."
  [state side region]
  (system-msg state side "looks at their location deck")
  (swap! state assoc-in [side :cut-region] region)
  (swap! state assoc-in [side :view-location] true))

(defn close-location
  "Closes the deck view and makes cards in deck private again."
  [state side args]
  (system-msg state side "stops looking at their location deck")
  (swap! state update-in [side] dissoc :cut-region)
  (swap! state update-in [side] dissoc :view-location))