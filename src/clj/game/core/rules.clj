(in-ns 'game.core)

(declare can-run? card-init card-str cards-can-prevent? close-access-prompt enforce-msg gain-agenda-point
         get-prevent-list get-agenda-points in-contestant-scored? placed? is-type? play-sfx prevent-draw make-result
         remove-old-current show-prompt system-say system-msg steal-trigger-events discard-cards
         undiscardable-while-revealed? update-all-character undiscardable-while-radicles? win win-decked)

;;;; Functions for applying core MECCG game rules.

;;; Playing cards.
(defn- complete-play-instant
  "Completes the play of the event / operation that the player can play for"
  [state side eid {:keys [title] :as card} cost-str ignore-cost]
  (let [c (move state side (assoc card :seen true) :play-area)
        cdef (card-def card)]
    (system-msg state side (str (if ignore-cost
                                  "play "
                                  (build-spend-msg cost-str "play"))
                                title
                                (when ignore-cost " at no cost")))
    (play-sfx state side "play-instant")
    (if (has-subtype? c "Current")
      (do (doseq [s [:contestant :challenger]]
            (remove-old-current state side s))
          (let [c (some #(when (= (:cid %) (:cid card)) %) (get-in @state [side :play-area]))
                moved-card (move state side c :current)]
            (card-init state side eid moved-card {:resolve-effect true
                                                  :init-data true})))
      (do (resolve-ability state side (assoc cdef :eid eid) card nil)
          (when-let [c (some #(when (= (:cid %) (:cid card)) %) (get-in @state [side :play-area]))]
            (move state side c :discard))
          (when (has-subtype? card "Terminal")
            (lose state side :click (-> @state side :click))
            (swap! state assoc-in [:contestant :register :terminal] true))))
    (trigger-event state side (if (= side :contestant) :play-operation :play-event) c)))

(defn play-instant
  "Plays an Event or Operation."
  ([state side card] (play-instant state side (make-eid state) card nil))
  ([state side eid? card?] (if (:eid eid?)
                             (play-instant state side eid? card? nil)
                             (play-instant state side (make-eid state) eid? card?)))
  ([state side eid card {:keys [targets ignore-cost extra-cost no-additional-cost]}]
   (swap! state update-in [:bonus] dissoc :play-cost)
   (wait-for (trigger-event-simult state side :pre-play-instant nil card)
             (when-not (seq (get-in @state [side :locked (-> card :zone first)]))
               (let [cdef (card-def card)
                     additional-cost (if (has-subtype? card "Triple")
                                       (concat (:additional-cost cdef) [:click 2])
                                       (:additional-cost cdef))
                     additional-cost (if (and (has-subtype? card "Double")
                                              (not (get-in @state [side :register :double-ignore-additional])))
                                       (concat (:additional-cost cdef) [:click 1])
                                       additional-cost)
                     additional-cost (if (and (has-subtype? card "Run")
                                              (get-in @state [:bonus :run-cost]))
                                       (concat additional-cost (get-in @state [:bonus :run-cost]))
                                       additional-cost)
                     total-cost (play-cost state side card
                                           (concat (when-not no-additional-cost additional-cost) extra-cost
                                                   [:credit (:cost card)]))
                     eid (if-not eid (make-eid state) eid)]
                 ;; ensure the instant can be played
                 (if (and (if-let [cdef-req (:req cdef)]
                            (cdef-req state side eid card targets) true) ; req is satisfied
                          (not (and (has-subtype? card "Current")
                                    (get-in @state [side :register :cannot-play-current])))
                          (not (and (has-subtype? card "Run")
                                    (not (can-run? state :challenger))))
                          (not (and (has-subtype? card "Priority")
                                    (get-in @state [side :register :spent-click])))) ; if priority, have not spent a click
                   ;; Wait on pay-sync to finish before triggering instant-effect
                   (wait-for (pay-sync state side card (if ignore-cost 0 total-cost) {:action :play-instant})
                             (if-let [cost-str async-result]
                               (complete-play-instant state side eid card cost-str ignore-cost)
                               ;; could not pay the card's prcharacter; mark the effect as being over.
                               (effect-completed state side eid)))
                   ;; card's req was not satisfied; mark the effect as being over.
                   (effect-completed state side eid)))))))

(defn max-draw
  "Put an upper limit on the number of cards that can be drawn in this turn."
  [state side n]
  (swap! state assoc-in [side :register :max-draw] n))

(defn remaining-draws
  "Calculate remaining number of cards that can be drawn this turn if a maximum exists"
  [state side]
  (when-let [max-draw (get-in @state [side :register :max-draw])]
    (let [drawn-this-turn (get-in @state [side :register :drawn-this-turn] 0)]
      (max (- max-draw drawn-this-turn) 0))))

(defn draw-bonus
  "Registers a bonus of n draws to the next draw (Daily Business Show)"
  [state side n]
  (swap! state update-in [:bonus :draw] (fnil #(+ % n) 0)))

(defn draw
  "Draw n cards from :deck to :hand."
  ([state side] (draw state side (make-eid state) 1 nil))
  ([state side n] (draw state side (make-eid state) n nil))
  ([state side n args] (draw state side (make-eid state) n args))
  ([state side eid n {:keys [suppress-event] :as args}]
   (swap! state update-in [side :register] dissoc :most-recent-drawn) ;clear the most recent draw in case draw prevented
   (trigger-event state side (if (= side :contestant) :pre-contestant-draw :pre-challenger-draw) n)
   (let [active-player (get-in @state [:active-player])
         n (+ n (get-in @state [:bonus :draw] 0))
         draws-wanted n
         draws-after-prevent (if (and (= side active-player) (get-in @state [active-player :register :max-draw]))
                                  (min n (remaining-draws state side))
                                  n)
         deck-count (count (get-in @state [side :deck]))]
     (when (and (= side :contestant) (> draws-after-prevent deck-count))
       (win-decked state))
     (when-not (and (= side active-player) (get-in @state [side :register :cannot-draw]))
       (let [drawn (zone :hand (take draws-after-prevent (get-in @state [side :deck])))]
         (swap! state update-in [side :hand] #(concat % drawn))
         (swap! state update-in [side :deck] (partial drop draws-after-prevent))
         (swap! state assoc-in [side :register :most-recent-drawn] drawn)
         (swap! state update-in [side :register :drawn-this-turn] (fnil #(+ % draws-after-prevent) 0))
         (swap! state update-in [:stats side :gain :card] (fnil + 0) n)
         (swap! state update-in [:bonus] dissoc :draw)
         (if (and (not suppress-event) (pos? deck-count))
           (wait-for
             (trigger-event-sync state side (if (= side :contestant) :contestant-draw :challenger-draw) draws-after-prevent)
             (trigger-event-sync state side eid (if (= side :contestant) :post-contestant-draw :post-challenger-draw) draws-after-prevent))
           (effect-completed state side eid))
         (when (safe-zero? (remaining-draws state side))
           (prevent-draw state side))))
     (when (< draws-after-prevent draws-wanted)
       (let [prevented (- draws-wanted draws-after-prevent)]
         (system-msg state (other-side side) (str "prevents "
                                                  (quantify prevented "card")
                                                  " from being drawn")))))))

;;; Damage
(defn flatline [state]
  (when-not (:winner state)
    (system-msg state :challenger "is flatlined")
    (win state :contestant "Flatline")))

(defn damage-count
  "Calculates the amount of damage to do, taking into account prevention and boosting effects."
  [state side dtype n {:keys [unpreventable unboostable] :as args}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:damage :damage-bonus dtype])) 0))
      (- (or (when-not unpreventable (get-in @state [:damage :damage-prevent dtype])) 0))
      (max 0)))

(defn damage-bonus
  "Registers a bonus of n damage to the next damage application of the given type."
  [state side dtype n]
  (swap! state update-in [:damage :damage-bonus dtype] (fnil #(+ % n) 0)))

(defn damage-prevent
  "Registers a prevention of n damage to the next damage application of the given type."
  [state side dtype n]
  (swap! state update-in [:damage :damage-prevent dtype] (fnil #(+ % n) 0)))

(defn damage-defer
  "Registers n damage of the given type to be deferred until later. (Chronos Protocol.)"
  ([state side dtype n] (damage-defer state side dtype n nil))
  ([state side dtype n {:keys [part-resolved] :as args}]
   (swap! state assoc-in [:damage :defer-damage dtype] {:n n
                                                        :part-resolved part-resolved})))

(defn get-defer-damage [state side dtype {:keys [unpreventable] :as args}]
  (let [{:keys [n part-resolved]} (get-in @state [:damage :defer-damage dtype])]
    (when (or part-resolved (not unpreventable)) n)))

(defn enable-challenger-damage-choice
  [state side]
  (swap! state assoc-in [:damage :damage-choose-challenger] true))

(defn enable-contestant-damage-choice
  [state side]
  (swap! state assoc-in [:damage :damage-choose-contestant] true))

(defn challenger-can-choose-damage?
  [state]
  (get-in @state [:damage :damage-choose-challenger]))

(defn contestant-can-choose-damage?
  [state]
  (get-in @state [:damage :damage-choose-contestant]))

(defn damage-choice-priority
  "Determines which side gets to act if either or both have the ability to choose cards for damage.
  Currently just for Chronos Protocol vs Titanium Ribs"
  [state]
  (let [active-player (get-in @state [:active-player])]
    (when (and (contestant-can-choose-damage? state) (challenger-can-choose-damage? state))
      (if (= active-player :contestant)
        (swap! state update-in [:damage] dissoc :damage-choose-challenger)
        (swap! state update-in [:damage] dissoc :damage-choose-contestant)))))

(defn resolve-damage
  "Resolves the attempt to do n damage, now that both sides have acted to boost or
  prevent damage."
  [state side eid type n {:keys [unpreventable unboostable card] :as args}]
  (swap! state update-in [:damage :defer-damage] dissoc type)
  (damage-choice-priority state)
  (wait-for (trigger-event-sync state side :pre-resolve-damage type card n)
            (do (when-not (or (get-in @state [:damage :damage-replace])
                              (challenger-can-choose-damage? state))
                  (let [n (if-let [defer (get-defer-damage state side type args)] defer n)]
                    (when (pos? n)
                      (let [hand (get-in @state [:challenger :hand])
                            cards-discarded (take n (shuffle hand))]
                        (when (= type :brain)
                          (swap! state update-in [:challenger :brain-damage] #(+ % n))
                          (swap! state update-in [:challenger :hand-size :mod] #(- % n)))
                        (when-let [discarded-msg (join ", " (map :title cards-discarded))]
                          (system-msg state :challenger (str "discards " discarded-msg " due to " (name type) " damage")))
                        (if (< (count hand) n)
                          (do (flatline state)
                              (discard-cards state side (make-eid state) cards-discarded
                                           {:unpreventable true})
                              (swap! state update-in [:stats :contestant :damage :all] (fnil + 0) n)
                              (swap! state update-in [:stats :contestant :damage type] (fnil + 0) n))
                          (do (discard-cards state side (make-eid state) cards-discarded
                                           {:unpreventable true :cause type})
                              (swap! state update-in [:stats :contestant :damage :all] (fnil + 0) n)
                              (swap! state update-in [:stats :contestant :damage type] (fnil + 0) n)
                              (trigger-event state side :damage type card n)))))))
                (swap! state update-in [:damage :defer-damage] dissoc type)
                (swap! state update-in [:damage] dissoc :damage-replace)
                (effect-completed state side eid))))

(defn damage
  "Attempts to deal n damage of the given type to the challenger. Starts the
  prevention/boosting process and eventually resolves the damage."
  ([state side type n] (damage state side (make-eid state) type n nil))
  ([state side type n args] (damage state side (make-eid state) type n args))
  ([state side eid type n {:keys [unpreventable unboostable card] :as args}]
   (swap! state update-in [:damage :damage-bonus] dissoc type)
   (swap! state update-in [:damage :damage-prevent] dissoc type)
   ;; alert listeners that damage is about to be calculated.
   (trigger-event state side :pre-damage type card n)
   (let [n (damage-count state side type n args)
         prevent (get-prevent-list state :challenger type)]
     (if (and (not unpreventable) (cards-can-prevent? state :challenger prevent type))
       ;; challenger can prevent the damage.
       (do (system-msg state :challenger "has the option to avoid damage")
           (show-wait-prompt state :contestant "Challenger to prevent damage" {:priority 10})
           (swap! state assoc-in [:prevent :current] type)
           (show-prompt
             state :challenger nil (str "Prevent any of the " n " " (name type) " damage?") ["Done"]
             (fn [_] (let [prevent (get-in @state [:damage :damage-prevent type])]
                       (when prevent (trigger-event state side :prevented-damage type prevent))
                       (system-msg state :challenger
                                   (if prevent (str "prevents " (if (= prevent Integer/MAX_VALUE) "all" prevent)
                                                    " " (name type) " damage") "will not prevent damage"))
                       (clear-wait-prompt state :contestant)
                       (resolve-damage state side eid type (max 0 (- n (or prevent 0))) args)))
             {:priority 10}))
       (resolve-damage state side eid type n args)))))


;;; Tagging
(defn tag-count
  "Calculates the number of tags to give, taking into account prevention and boosting effects."
  [state side n {:keys [unpreventable unboostable] :as args}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:tag :tag-bonus])) 0))
      (- (or (when-not unpreventable (get-in @state [:tag :tag-prevent])) 0))
      (max 0)))

(defn tag-prevent
  ([state side n] (tag-prevent state side (make-eid state) n))
  ([state side eid n]
   (swap! state update-in [:tag :tag-prevent] (fnil #(+ % n) 0))
   (trigger-event-sync state side eid (if (= side :contestant) :contestant-prevent :challenger-prevent) `(:tag ~n))))

(defn tag-remove-bonus
  "Applies a cost increase of n to removing tags with the click action. (SYNC.)"
  [state side n]
  (swap! state update-in [:challenger :tag-remove-bonus] (fnil #(+ % n) 0)))

(defn resolve-tag [state side eid n args]
  (trigger-event state side :pre-resolve-tag n)
  (if (pos? n)
    (do (gain state :challenger :tag n)
        (toast state :challenger (str "Took " (quantify n "tag") "!") "info")
        (trigger-event-sync state side eid :challenger-gain-tag n))
    (effect-completed state side eid)))

(defn gain-tags
  "Attempts to give the challenger n tags, allowing for boosting/prevention effects."
  ([state side n] (gain-tags state side (make-eid state) n nil))
  ([state side eid n] (gain-tags state side eid n nil))
  ([state side eid n {:keys [unpreventable unboostable card] :as args}]
   (swap! state update-in [:tag] dissoc :tag-bonus :tag-prevent)
   (trigger-event state side :pre-tag card)
   (let [n (tag-count state side n args)
         prevent (get-prevent-list state :challenger :tag)]
     (if (and (pos? n)
              (not unpreventable)
              (cards-can-prevent? state :challenger prevent :tag))
       (do (system-msg state :challenger "has the option to avoid tags")
           (show-wait-prompt state :contestant "Challenger to prevent tags" {:priority 10})
           (swap! state assoc-in [:prevent :current] :tag)
           (show-prompt
             state :challenger nil
             (str "Avoid " (when (< 1 n) "any of the ") (quantify n "tag") "?") ["Done"]
             (fn [_]
               (let [prevent (get-in @state [:tag :tag-prevent])
                     prevent-msg (if prevent
                                   (str "avoids "
                                        (if (= prevent Integer/MAX_VALUE) "all" prevent)
                                        (if (< 1 prevent) " tags" " tag"))
                                   "will not avoid tags")]
                 (system-msg state :challenger prevent-msg)
                 (clear-wait-prompt state :contestant)
                 (resolve-tag state side eid (max 0 (- n (or prevent 0))) args)))
             {:priority 10}))
       (resolve-tag state side eid n args)))))

(defn lose-tags
  ([state side n] (lose-tags state side (make-eid state) n))
  ([state side eid n]
   (if (= n :all)
     (do (swap! state update-in [:stats :challenger :lose :tag] (fnil + 0 0) (get-in @state [:challenger :tag]))
         (swap! state assoc-in [:challenger :tag] 0))
     (do (swap! state update-in [:stats :challenger :lose :tag] (fnil + 0) n)
         (deduct state :challenger [:tag n])))
   (trigger-event-sync state side eid :challenger-lose-tag n side)))


;;;; Bad Publicity
(defn bad-publicity-count
  "Calculates the number of bad publicity to give, taking into account prevention and boosting effects."
  [state side n {:keys [unpreventable unboostable] :as args}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:bad-publicity :bad-publicity-bonus])) 0))
      (- (or (when-not unpreventable (get-in @state [:bad-publicity :bad-publicity-prevent])) 0))
      (max 0)))

(defn bad-publicity-prevent [state side n]
  (swap! state update-in [:bad-publicity :bad-publicity-prevent] (fnil #(+ % n) 0))
  (trigger-event state side (if (= side :contestant) :contestant-prevent :challenger-prevent) `(:bad-publicity ~n)))

(defn resolve-bad-publicity [state side eid n args]
  (trigger-event state side :pre-resolve-bad-publicity n)
  (if (pos? n)
    (do (gain state :contestant :bad-publicity n)
        (toast state :contestant (str "Took " n " bad publicity!") "info")
        (trigger-event-sync state side eid :contestant-gain-bad-publicity n))
    (effect-completed state side eid)))

(defn gain-bad-publicity
  "Attempts to give the challenger n bad publicity, allowing for boosting/prevention effects."
  ([state side n] (gain-bad-publicity state side (make-eid state) n nil))
  ([state side eid n] (gain-bad-publicity state side eid n nil))
  ([state side eid n {:keys [unpreventable card] :as args}]
   (swap! state update-in [:bad-publicity] dissoc :bad-publicity-bonus :bad-publicity-prevent)
   (wait-for (trigger-event-sync state side :pre-bad-publicity card)
             (let [n (bad-publicity-count state side n args)
                   prevent (get-prevent-list state :contestant :bad-publicity)]
               (if (and (pos? n)
                        (not unpreventable)
                        (cards-can-prevent? state :contestant prevent :bad-publicity))
                 (do (system-msg state :contestant "has the option to avoid bad publicity")
                     (show-wait-prompt state :challenger "Contestant to prevent bad publicity" {:priority 10})
                     (swap! state assoc-in [:prevent :current] :bad-publicity)
                     (show-prompt
                       state :contestant nil
                       (str "Avoid " (when (< 1 n) "any of the ") n " bad publicity?") ["Done"]
                       (fn [_]
                         (let [prevent (get-in @state [:bad-publicity :bad-publicity-prevent])]
                           (system-msg state :contestant
                                       (if prevent
                                         (str "avoids "
                                              (if (= prevent Integer/MAX_VALUE) "all" prevent)
                                              " bad publicity")
                                         "will not avoid bad publicity"))
                           (clear-wait-prompt state :challenger)
                           (resolve-bad-publicity state side eid (max 0 (- n (or prevent 0))) args)))
                       {:priority 10}))
                 (resolve-bad-publicity state side eid n args))))))


;;; Discarding
(defn discard-radicle-bonus
  "Applies a cost increase of n to discarding a radicle with the click action. (SYNC.)"
  [state side n]
  (swap! state update-in [:contestant :discard-cost-bonus] (fnil #(+ % n) 0)))

(defn discard-prevent [state side type n]
  (swap! state update-in [:discard :discard-prevent type] (fnil #(+ % n) 0)))

(defn- resolve-discard-end
  ([state side eid card args] (resolve-discard-end state side eid card eid args))
  ([state side eid {:keys [disabled] :as card} oid
   {:keys [cause keep-locale-alive host-discarded] :as args}]
  (let [cdef (card-def card)
        moved-card (move state (to-keyword (:side card)) card :discard {:keep-locale-alive keep-locale-alive})]
    (swap! state update-in [:per-turn] dissoc (:cid moved-card))
    (swap! state update-in [:discard :discard-list] dissoc oid)
    (if-let [discard-effect (:discard-effect cdef)]
      (if (and (not disabled)
               (or (and (= (:side card) "Challenger")
                        (:placed card)
                        (not (:facedown card)))
                   (and (:revealed card)
                        (not host-discarded))
                   (and (:when-inactive discard-effect)
                        (not host-discarded))))
        (wait-for (resolve-ability state side discard-effect moved-card (list cause))
                  (effect-completed state side eid))
        (effect-completed state side eid))
      (effect-completed state side eid)))))

(defn- resolve-discard
  ([state side eid card args] (resolve-discard state side eid card eid args))
  ([state side eid {:keys [zone] :as card} oid
   {:keys [cause suppress-event] :as args}]
  (if (and (not suppress-event)
           (not= (last zone) :current)) ; Discarding a current does not trigger a discard event.
    (wait-for (trigger-event-sync state side (keyword (str (name side) "-discard")) card cause)
              (resolve-discard-end state side eid card oid args))
    (resolve-discard-end state side eid card args))))

(defn- prevent-discard
  ([state side card oid] (prevent-discard state side (make-eid state) card oid nil))
  ([state side card oid args] (prevent-discard state side (make-eid state) card oid args))
  ([state side eid {:keys [zone type] :as card} oid
    {:keys [unpreventable cause keep-locale-alive suppress-event] :as args}]
   (if (and card (not (some #{:discard} zone)))
     (cond

       (undiscardable-while-revealed? card)
       (do (enforce-msg state card "cannot be discarded while placed")
           (effect-completed state side eid))

       (and (= side :contestant)
            (undiscardable-while-radicles? card)
            (> (count (filter #(is-type? % "Radicle") (all-active-placed state :challenger))) 1))
       (do (enforce-msg state card "cannot be discarded while there are other radicles placed")
           (effect-completed state side eid))

       ;; Card is not enforced undiscardable
       :else
       (let [ktype (keyword (clojure.string/lower-case type))]
         (when (and (not unpreventable)
                    (not= cause :ability-cost))
           (swap! state update-in [:discard :discard-prevent] dissoc ktype))
         (let [type (->> ktype name (str "discard-") keyword)
               prevent (get-prevent-list state :challenger type)]
           ;; Check for prevention effects
           (if (and (not unpreventable)
                    (not= cause :ability-cost)
                    (cards-can-prevent? state :challenger prevent type card args))
             (do (system-msg state :challenger "has the option to prevent discard effects")
                 (show-wait-prompt state :contestant "Challenger to prevent discard effects" {:priority 10})
                 (show-prompt state :challenger nil
                              (str "Prevent the discarding of " (:title card) "?") ["Done"]
                              (fn [_]
                                (clear-wait-prompt state :contestant)
                                (if-let [_ (get-in @state [:discard :discard-prevent ktype])]
                                  (do (system-msg state :challenger (str "prevents the discarding of " (:title card)))
                                      (swap! state update-in [:discard :discard-prevent] dissoc ktype)
                                      (effect-completed state side eid))
                                  (do (system-msg state :challenger (str "will not prevent the discarding of " (:title card)))
                                      (swap! state update-in [:discard :discard-list oid] concat [card])
                                      (effect-completed state side eid))))
                              {:priority 10}))
             ;; No prevention effects: add the card to the discard-list
             (do (swap! state update-in [:discard :discard-list oid] concat [card])
                 (effect-completed state side eid))))))
     (effect-completed state side eid))))

(defn discard
  "Attempts to discard the given card, allowing for boosting/prevention effects."
  ([state side card] (discard state side (make-eid state) card nil))
  ([state side card args] (discard state side (make-eid state) card args))
  ([state side eid card {:keys [unpreventable cause suppress-event] :as args}]
   (wait-for (prevent-discard state side card eid args)
             (if-let [c (first (get-in @state [:discard :discard-list eid]))]
               (resolve-discard state side eid c args)
               (effect-completed state side eid)))))

(defn discard-cards
  ([state side cards] (discard-cards state side (make-eid state) cards nil))
  ([state side eid cards] (discard-cards state side eid cards nil))
  ([state side eid cards {:keys [suppress-event] :as args}]
   (letfn [(discardrec [cs]
             (if (not-empty cs)
               (wait-for (resolve-discard-end state side (get-card state (first cs)) eid args)
                         (discardrec (rest cs)))
               (effect-completed state side eid)))
           (preventrec [cs]
             (if (not-empty cs)
               (wait-for (prevent-discard state side (get-card state (first cs)) eid args)
                         (preventrec (rest cs)))
               (let [discardlist (get-in @state [:discard :discard-list eid])]
                 (wait-for (apply trigger-event-sync state side (keyword (str (name side) "-discard")) discardlist)
                           (discardrec discardlist)))))]
     (preventrec cards))))

(defn discard-no-cost
  [state side eid card & {:keys [seen unpreventable]
                          :or {seen true}}]
  (swap! state assoc-in [side :register :discarded-card] true)
  (discard state side eid (assoc card :seen seen) {:unpreventable unpreventable}))

;;; Agendas
(defn get-agenda-points
  "Apply agenda-point modifications to calculate the number of points this card is worth
  to the given player."
  [state side card]
  (let [base-points (:agendapoints card)
        challenger-fn (:agendapoints-challenger (card-def card))
        contestant-fn (:agendapoints-contestant (card-def card))]
    (cond
      (and (= side :challenger)
           (some? challenger-fn))
      (challenger-fn state side (make-eid state) card nil)
      (and (= side :contestant)
           (some? contestant-fn))
      (contestant-fn state side (make-eid state) card nil)
      :else
      base-points)))

(defn advancement-cost-bonus
  "Applies an advancement requirement increase of n the next agenda whose advancement requirement
  is being calculated. (SanSan City Grid.)"
  [state side n]
  (swap! state update-in [:bonus :advancement-cost] (fnil #(+ % n) 0)))

(defn advancement-cost [state side {:keys [advancementcost] :as card}]
  (if (some? advancementcost)
    (-> (if-let [costfun (:advancement-cost-bonus (card-def card))]
          (+ advancementcost (costfun state side (make-eid state) card nil))
          advancementcost)
        (+ (get-in @state [:bonus :advancement-cost] 0))
        (max 0))))

(defn update-advancement-cost
  "Recalculates the advancement requirement for the given agenda."
  [state side agenda]
  (swap! state update-in [:bonus] dissoc :advancement-cost)
  (trigger-event state side :pre-advancement-cost agenda)
  (update! state side (assoc agenda :current-cost (advancement-cost state side agenda))))

(defn update-all-advancement-costs [state side]
  (doseq [ag (->> (mapcat :content (flatten (seq (get-in @state [:contestant :locales]))))
                  (filter #(is-type? % "Agenda")))]
    (update-advancement-cost state side ag)))

(defn as-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points."
  ([state side card n] (as-agenda state side (make-eid state) card n))
  ([state side eid card n]
   (move state side (assoc (deactivate state side card) :agendapoints n) :scored)
   (wait-for (trigger-event-sync state side :as-agenda (assoc card :as-agenda-side side :as-agenda-points n))
             (do (gain-agenda-point state side n)
                 (effect-completed state side eid)))))

(defn forfeit
  "Forfeits the given agenda to the :rfg zone."
  ([state side card] (forfeit state side (make-eid state) card))
  ([state side eid card]
   ;; Remove all hosted cards first
   (doseq [h (:hosted card)]
     (discard state side
            (update-in h [:zone] #(map to-keyword %))
            {:unpreventable true :suppress-event true}))
   (let [card (get-card state card)]
     (system-msg state side (str "forfeits " (:title card)))
     (gain-agenda-point state side (- (get-agenda-points state side card)))
     (move state side card :rfg)
     (wait-for (trigger-event-sync state side (keyword (str (name side) "-forfeit-agenda")) card)
               (effect-completed state side eid)))))

(defn gain-agenda-point
  "Gain n agenda points and check for winner."
  [state side n]
  (gain state side :agenda-point n)
  (when (and (>= (get-in @state [side :agenda-point]) (get-in @state [side :agenda-point-req]))
             (not (get-in @state [side :cannot-win-on-points])))
    (win state side "Agenda")))


;;; Miscellaneous
(defn purge
  "Purges viruses."
  [state side]
  (trigger-event state side :pre-purge)
  (let [rig-cards (all-placed state :challenger)
        hosted-on-character (->> (get-in @state [:contestant :locales]) seq flatten (mapcat :characters) (mapcat :hosted))]
    (doseq [card (concat rig-cards hosted-on-character)]
      (when (or (has-subtype? card "Virus")
                (contains? (:counter card) :virus))
        (add-counter state :challenger card :virus (- (get-counters card :virus)))))
    (update-all-character state side))
  (trigger-event state side :purge))

(defn mill
  "Force the discard of n cards by discarding them."
  ([state side] (mill state side side 1))
  ([state side n] (mill state side side n))
  ([state from-side to-side n]
   (let [milltargets (take n (get-in @state [to-side :deck]))]
     (doseq [card milltargets]
       (discard-no-cost state from-side (make-eid state) card :seen false :unpreventable true)))))

;; Exposing
(defn expose-prevent
  [state side n]
  (swap! state update-in [:expose :expose-prevent] #(+ (or % 0) n)))

(defn- resolve-expose
  [state side eid target args]
  (system-msg state side (str "exposes " (card-str state target {:visible true})))
  (if-let [ability (:expose (card-def target))]
    (wait-for (resolve-ability state side ability target nil)
              (trigger-event-sync state side (make-result eid true) :expose target))
    (trigger-event-sync state side (make-result eid true) :expose target)))

(defn expose
  "Exposes the given card."
  ([state side target] (expose state side (make-eid state) target))
  ([state side eid target] (expose state side eid target nil))
  ([state side eid target {:keys [unpreventable] :as args}]
    (swap! state update-in [:expose] dissoc :expose-prevent)
    (if (revealed? target)
      (effect-completed state side eid) ; cannot expose faceup cards
      (wait-for (trigger-event-sync state side :pre-expose target)
                (let [prevent (get-prevent-list state :contestant :expose)]
                  (if (and (not unpreventable)
                           (cards-can-prevent? state :contestant prevent :expose))
                    (do (system-msg state :contestant "has the option to prevent a card from being exposed")
                        (show-wait-prompt state :challenger "Contestant to prevent the expose" {:priority 10})
                        (show-prompt state :contestant nil
                                     (str "Prevent " (:title target) " from being exposed?") ["Done"]
                                     (fn [_]
                                       (clear-wait-prompt state :challenger)
                                       (if-let [_ (get-in @state [:expose :expose-prevent])]
                                         (effect-completed state side (make-result eid false)) ;; ??
                                         (do (system-msg state :contestant "will not prevent a card from being exposed")
                                             (resolve-expose state side eid target args))))
                                     {:priority 10}))
                    (if-not (get-in @state [:expose :expose-prevent])
                      (resolve-expose state side eid target args)
                      (effect-completed state side (make-result eid false)))))))))

(defn reveal-hand
  "Reveals a side's hand to opponent and spectators."
  [state side]
  (swap! state assoc-in [side :openhand] true))

(defn conceal-hand
  "Conceals a side's revealed hand from opponent and spectators."
  [state side]
  (swap! state update-in [side] dissoc :openhand))

(defn clear-win
  "Clears the current win condition.  Requires both sides to have issued the command"
  [state side]
  (swap! state assoc-in [side :clear-win] true)
  (when (and (-> @state :challenger :clear-win) (-> @state :contestant :clear-win))
    (system-msg state side "cleared the win condition")
    (swap! state dissoc-in [:challenger :clear-win])
    (swap! state dissoc-in [:contestant :clear-win])
    (swap! state dissoc :winner :loser :winning-user :losing-user :reason :winning-deck-id :losing-deck-id :end-time)))

(defn win
  "Records a win reason for statistics."
  [state side reason]
  (when-not (:winner @state)
    (let [started (get-in @state [:stats :time :started])
          now (t/now)]
      (system-msg state side "wins the game")
      (play-sfx state side "game-end")
      (swap! state assoc-in [:stats :time :ended] now)
      (swap! state assoc-in [:stats :time :elapsed] (t/in-minutes (t/interval started now)))
      (swap! state assoc
             :winner side
             :loser (other-side side)
             :winning-user (get-in @state [side :user :username])
             :losing-user (get-in @state [(other-side side) :user :username])
             :reason reason :end-time (java.util.Date.)
             :winning-deck-id (get-in @state [side :deck-id])
             :losing-deck-id (get-in @state [(other-side side) :deck-id])))))

(defn win-decked
  "Records a win via decking the contestant."
  [state]
  (system-msg state :contestant "is decked")
  (win state :challenger "Decked"))
