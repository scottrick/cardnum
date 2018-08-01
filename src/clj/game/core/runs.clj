(in-ns 'game.core)

(declare any-flag-fn? clear-run-register! run-cleanup
         gain-run-credits update-character-in-locale update-all-character demote-all-characters
         get-agenda-points gain-agenda-point optional-ability
         get-party-names card-name can-access-loud can-steal?
         prevent-jack-out card-flag? can-run?)

;;; Steps in the run sequence
(defn run
  "Starts a run on the given locale, with the given card as the cause."
  ([state side locale] (run state side (make-eid state) locale nil nil))
  ([state side eid locale] (run state side eid locale nil nil))
  ([state side locale run-effect card] (run state side (make-eid state) locale run-effect card))
  ([state side eid locale run-effect card]
   (if (= side :contestant)
     (when (can-run? state :challenger)
       (let [s [(if (keyword? locale) locale (last (locale->zone state locale)))]
             characters (get-in @state (concat [:contestant :locales] s [:characters]))
             n (count characters)]
         ;; s is a keyword for the locale, like :hq or :party1
         (swap! state assoc :per-run nil
                :run {:locale s :position n :rerun n :access-bonus 0
                      :run-effect (assoc run-effect :card card)
                      :eid eid})
         (gain-run-credits state side (+ (get-in @state [:contestant :bad-publicity]) (get-in @state [:contestant :has-bad-pub])))
         (swap! state update-in [:challenger :register :made-run] #(conj % (first s)))
         (demote-all-characters state :contestant)
         (update-character-in-locale state side
                                     (get-in @state (concat [side :locales]
                                                            (get-in @state [:run :locale]))))
         (trigger-event-sync state :challenger (make-eid state) :run s)
         (when (>= n 2) (trigger-event state :challenger :run-big s n))))
     (when (can-run? state :contestant)
       (let [s [(if (keyword? locale) locale (last (locale->zone state locale)))]
             characters (get-in @state (concat [:challenger :locales] s [:characters]))
             n (count characters)]
         ;; s is a keyword for the locale, like :hq or :party1
         (swap! state assoc :per-run nil
                :run {:locale s :position n :rerun n :access-bonus 0
                      :run-effect (assoc run-effect :card card)
                      :eid eid})
         ;;(gain-run-credits state side (+ (get-in @state [:challenger :bad-publicity]) (get-in @state [:challenger :has-bad-pub])))
         (swap! state update-in [:contestant :register :made-run] #(conj % (first s)))
         (demote-all-characters state :challenger)
         (update-character-in-locale state side
                                     (get-in @state (concat [side :locales]
                                                            (get-in @state [:run :locale]))))
         (trigger-event-sync state :contestant (make-eid state) :run s)
         (when (>= n 2) (trigger-event state :contestant :run-big s n)))))
    ))

(defn gain-run-credits
  "Add temporary credits that will disappear when the run is over."
  [state side n]
  (swap! state update-in [:challenger :run-credit] + n)
  (gain-credits state :challenger n))

(defn access-end
  "Trigger events involving the end of the access phase, including :no-discard and :post-access-card"
  [state side eid c]
  (when-not (find-cid (:cid c) (get-in @state [:contestant :discard]))
    ;; Do not trigger :no-discard if card has already been discarded
    (trigger-event state side :no-discard c))
  (when (and (is-type? c "Agenda")
             (not (find-cid (:cid c) (get-in @state [:challenger :scored]))))
    (trigger-event state side :no-steal c))
  (when (and (get-card state c)
             ;; Don't increment :no-discard-or-steal if accessing a card in Archives
             (not= (:zone c) [:discard]))
    (swap! state update-in [:challenger :register :no-discard-or-steal] (fnil inc 0)))
  (trigger-event-sync state side eid :post-access-card c))

;;; Stealing agendas
(defn steal
  "Moves a card to the challenger's :scored area, triggering events from the completion of the steal."
  ([state side card] (steal state side (make-eid state) card))
  ([state side eid card]
   (let [c (move state :challenger (dissoc card :advance-counter :new) :scored {:force true})
         points (get-agenda-points state :challenger c)]
     (wait-for
       (trigger-event-simult
         state :challenger :agenda-stolen
         {:first-ability {:effect (req (system-msg state :challenger (str "steals " (:title c) " and gains "
                                                                      (quantify points "agenda point")))
                                       (swap! state update-in [:challenger :register :stole-agenda]
                                              #(+ (or % 0) (:agendapoints c 0)))
                                       (gain-agenda-point state :challenger points)
                                       (play-sfx state side "agenda-steal")
                                       (when (:run @state)
                                         (swap! state assoc-in [:run :did-steal] true))
                                       (when (card-flag? c :has-events-when-stolen true)
                                         (register-events state side (:events (card-def c)) c))
                                       (remove-old-current state side :contestant))}
          :card-ability (ability-as-handler c (:stolen (card-def c)))}
         c)
       (access-end state side eid card)))))

(defn- steal-agenda
  "Trigger the stealing of an agenda, now that costs have been paid."
  ([state side card] (steal-agenda state side (make-eid state) card))
  ([state side eid card]
   (let [cdef (card-def card)]
     (if (or (not (:steal-req cdef)) ((:steal-req cdef) state :challenger (make-eid state) card nil))
       (steal state :challenger eid card)
       (access-end state side eid card)))))

(defn steal-cost-bonus
  "Applies a cost to the next steal attempt. costs can be a vector of [:key value] pairs,
  for example [:credit 2 :click 1]."
  [state side costs]
  (swap! state update-in [:bonus :steal-cost] #(merge-costs (concat % costs))))

(defn steal-cost
  "Gets a vector of costs for stealing the given agenda."
  [state side card]
  (-> (when-let [costfun (:steal-cost-bonus (card-def card))]
        (costfun state side (make-eid state) card nil))
      (concat (get-in @state [:bonus :steal-cost]))
      merge-costs flatten vec))

;;; Accessing rules.
(defn access-cost-bonus
  "Applies a cost to the next access. costs can be a vector of [:key value] pairs,
  for example [:credit 2 :click 1]."
  [state side costs]
  (swap! state update-in [:bonus :access-cost] #(merge-costs (concat % costs))))

(defn access-cost
  "Gets a vector of costs for accessing the given card."
  [state side card]
  (-> (when-let [costfun (:access-cost-bonus (card-def card))]
        (costfun state side (make-eid state) card nil))
      (concat (get-in @state [:bonus :access-cost]))
      merge-costs flatten vec))

(defn access-non-agenda
  "Access a non-agenda. Show a prompt to discard for discardable cards."
  [state side eid c & {:keys [skip-trigger-event]}]
  (when-not skip-trigger-event
    (trigger-event state side :pre-discard c))
  (swap! state update-in [:stats :challenger :access :cards] (fnil inc 0))
  (if (not= (:zone c) [:discard]) ; if not accessing in Archives
    ;; The card has a discard cost (Site, Region)
    (let [card (assoc c :seen true)
          card-name (:title card)
          discard-cost (discard-cost state side c)
          can-pay (when discard-cost (or (can-pay? state :challenger nil :credit discard-cost)
                                       (can-pay-with-recurring? state :challenger discard-cost)))]
      ;; Show the option to pay to discard the card.
      (when-not (and (is-type? card "Operation")
                     ;; Don't show the option if Edward Kim's auto-discard flag is true.
                     (card-flag? card :can-discard-operation true))
        ;; If card has already been discarded this access don't show option to pay to discard (eg. Ed Kim)
        (when-not (find-cid (:cid card) (get-in @state [:contestant :discard]))
          (let [discard-ab-cards (->> (concat (all-active state :challenger)
                                            (get-in @state [:challenger :play-area]))
                                    (filter #(can-trigger? state :challenger (:discard-ability (:interactions (card-def %))) % [card])))
                ability-strs (map #(->> (card-def %) :interactions :discard-ability :label) discard-ab-cards)
                discard-cost-str (when can-pay
                                 [(str "Pay " discard-cost "[Credits] to discard")])
                ;; If the challenger is forced to discard this card (Neutralize All Threats)
                forced-to-discard? (and (or can-pay
                                          (seq discard-ab-cards))
                                      (or (get-in @state [:challenger :register :force-discard])
                                          (card-flag-fn? state side card :must-discard true)))
                discard-msg (when can-pay
                            (str discard-cost "[Credits] to discard " card-name " from " (name-zone :contestant (:zone card))))
                pay-str (when can-pay
                          (str (if forced-to-discard? "is forced to pay " "pays ") discard-msg))
                prompt-str (str "You accessed " card-name ".")
                no-action-str (when-not forced-to-discard?
                                ["No action"])
                choices (into [] (concat ability-strs discard-cost-str no-action-str))]
            (continue-ability
              state :challenger
              {:async true
               :prompt prompt-str
               :choices choices
               :effect (req (cond
                              (= target "No action")
                              (access-end state side eid c)

                              (.contains target "Pay")
                              (if (> discard-cost (get-in @state [:challenger :credit] 0))
                                (do (toast state side (str "You don't have the credits to pay for " card-name
                                                           ". Did you mean to first gain credits from placed cards?"))
                                    (access-non-agenda state side eid c :skip-trigger-event true))
                                (do (lose state side :credit discard-cost)
                                    (when (:run @state)
                                      (swap! state assoc-in [:run :did-discard] true)
                                      (when forced-to-discard?
                                        (swap! state assoc-in [:run :did-access] true)))
                                    (swap! state assoc-in [:challenger :register :discarded-card] true)
                                    (system-msg state side pay-str)
                                    (wait-for (discard state side card nil)
                                              (access-end state side eid c))))

                              (some #(= % target) ability-strs)
                              (let [idx (.indexOf ability-strs target)
                                    discard-ab-card (nth discard-ab-cards idx)
                                    cdef (-> (card-def discard-ab-card)
                                             :interactions
                                             :discard-ability)]
                                (when (:run @state)
                                  (swap! state assoc-in [:run :did-discard] true))
                                (wait-for (resolve-ability state side cdef discard-ab-card [card])
                                          (access-end state side eid c)))))}
              card nil)))))
    (access-end state side eid c)))

(defn- steal-pay-choice
  "Enables a vector of costs to be resolved in the order of choosing"
  [state side cost-map {:keys [title cid] :as card}]
  {:async true
   :prompt (str "Pay steal cost for " title "?")
   :choices (conj (vec (keys cost-map)) "No action")
   :effect (req
             (if (= target "No action")
               (continue-ability state :challenger
                                 {:async true
                                  :effect (req (when-not (find-cid cid (:deck contestant))
                                                 (system-msg state side (str "decides not to pay to steal " title)))
                                               (access-end state side eid card))}
                 card nil)
               (let [cost (cost-map target)
                     kw (first cost)
                     v (second cost)]
                 (if (and cost (can-pay? state side title [kw v]))
                   (wait-for
                     (pay-sync state side nil [kw v] {:action :steal-cost})
                     (do (system-msg state side (str "pays " target " to steal " title))
                         (if (> (count cost-map) 1)
                           (continue-ability
                             state side
                             (steal-pay-choice state :challenger (dissoc cost-map target) card)
                             card nil)
                           (steal-agenda state side eid card))))
                   (access-end state side eid card)))))})

(defn- access-agenda
  "Rules interactions for a challenger that has accessed an agenda and may be able to steal it."
  [state side eid c]
  (trigger-event state side :pre-steal-cost c)
  (swap! state update-in [:stats :challenger :access :cards] (fnil inc 0))
  (let [cost (steal-cost state side c)
        part-cost (partition 2 cost)
        cost-strs (map costs->symbol part-cost)
        cost-map (zipmap cost-strs part-cost)
        n (count cost-strs)
        card-name (:title c)
        can-pay-costs? (can-pay? state side card-name cost)
        cost-as-symbol (when (= 1 (count cost-strs)) (costs->symbol cost))
        ;; any discard abilities
        can-steal-this? (can-steal? state side c)
        discard-ab-cards (when (not= (:zone c) [:discard])
                         (->> (concat (all-active state :challenger)
                                      (get-in @state [:challenger :play-area]))
                              (filter #(can-trigger? state :challenger (get-in (card-def %) [:interactions :discard-ability]) % [c]))))
        ability-strs (map #(->> (card-def %) :interactions :discard-ability :label) discard-ab-cards)
        ;; strs
        steal-str (when (and can-steal-this? can-pay-costs?)
                    (if (seq cost-strs)
                      (if (= n 1)
                        [(str "Pay " cost-as-symbol " to steal")]
                        ["Pay to steal"])
                      ["Steal"]))
        no-action-str (when (or (nil? steal-str)
                                (not= steal-str ["Steal"]))
                        ["No action"])
        prompt-str (str "You accessed " card-name ".")
        choices (into [] (concat ability-strs steal-str no-action-str))]
    ;; Steal costs are additional costs and can be denied by the challenger.
    (continue-ability state :challenger
                      {:async true
                       :prompt prompt-str
                       :choices choices
                       :effect (req (cond
                                      ;; Can't steal or pay, or won't pay single additional cost to steal
                                      (= target "No action")
                                      (access-end state side eid c)

                                      ;; Steal normally
                                      (= target "Steal")
                                      (steal-agenda state :challenger eid c)

                                      ;; Pay single additiional cost to steal
                                      (.contains target "Pay")
                                      (if (> n 1)
                                        ;; Use the better function for multiple costs
                                        (continue-ability state :challenger
                                                          (steal-pay-choice state :challenger cost-map c)
                                                          c nil)
                                        ;; Otherwise, just handle everything right friggin here
                                        (wait-for (pay-sync state side nil cost {:action :steal-cost})
                                                  (do (system-msg state side
                                                                  (str "pays " cost-as-symbol " to steal " card-name))
                                                      (steal-agenda state side eid c))))

                                      ;; Use discard ability
                                      (some #(= % target) ability-strs)
                                      (let [idx (.indexOf ability-strs target)
                                            discard-ab-card (nth discard-ab-cards idx)
                                            cdef (-> (card-def discard-ab-card)
                                                     :interactions
                                                     :discard-ability)]
                                        (when (:run @state)
                                          (swap! state assoc-in [:run :did-discard] true))
                                        (wait-for (resolve-ability state side cdef discard-ab-card [c])
                                                  (do (trigger-event state side :no-steal c)
                                                      (access-end state side eid c))))))}
                      c nil)))

(defn- reveal-access?
  "Check if the card should be revealed on access"
  ;; If any special reveal message is wanted it can go in this function
  [state side {:keys [zone] :as card}]
  (let [cdef (card-def card)
        ;; Add more kw here as the maybe become relevant. Only think rd is relevant,
        ;; everything else should not be "unseen".
        reveal-kw (match (vec zone)
                         [:deck] :rd-reveal
                         [:hand] :hq-reveal
                         [:discard] :archives-reveal
                         :else :reveal)]
    ;; Check if the zone-reveal keyword exists in the flags property of the card definition
    (when-let [reveal-fn (get-in cdef [:flags reveal-kw])]
      (reveal-fn state side (make-eid state) card nil))))

(defn msg-handle-access
  "Generate the message from the access"
  [state side {:keys [zone] :as card} title]
  (let [msg (str "accesses " title
                 (when card
                   (str " from " (name-zone side zone))))]
    (system-msg state side msg)
    (when (reveal-access? state side card)
      (system-msg state side (str "must reveal they accessed " (:title card))))))

(defn- access-trigger-events
  "Trigger access effects, then move into discard/steal choice."
  [state side eid c title]
  (let [cdef (card-def c)
        c (assoc c :seen true)
        access-effect (when-let [acc (:access cdef)]
                        (ability-as-handler c acc))]
    (msg-handle-access state side c title)
    (wait-for (trigger-event-simult state side :access
                                    {:card-ability access-effect
                                     ;; Cancel other access handlers if the card moves zones because of a handler
                                     :cancel-fn (fn [state] (not (get-card state c)))}
                                    c)
              (if (get-card state c) ; make sure the card has not been moved by a handler
                (if (is-type? c "Agenda")
                  (access-agenda state side eid c)
                  ;; Accessing a non-agenda
                  (access-non-agenda state side eid c))
                (access-end state side eid c)))))

(defn- access-pay
  "Force the challenger to pay any costs to access this card, if any, before proceeding with access."
  [state side eid c title]
  (let [acost (access-cost state side c)
        ;; hack to prevent toasts when playing against Gagarin and accessing on 0 credits
        anon-card (dissoc c :title)
        cant-pay {:prompt "You can't pay the cost to access this card"
                  :choices ["OK"]
                  :async true
                  :effect (effect (access-end eid c))}]
    (cond
      ;; Check if a pre-access-card effect discarded the card (By Any Means)
      (not (get-card state c))
      (access-end state side eid c)

      ;; Either there were no access costs, or the challenger could pay them.
      (empty? acost)
      (access-trigger-events state side eid c title)

      (not-empty acost)
      ;; Await the payment of the costs; if payment succeeded, proceed with access.
      (wait-for (pay-sync state side anon-card acost)
                (if async-result
                  (access-trigger-events state side eid c title)
                  (resolve-ability state :challenger eid cant-pay c nil)))
      :else
      ;; The challenger cannot afford the cost to access the card
      (resolve-ability state :challenger eid cant-pay c nil))))

(defn access-card
  "Apply game rules for accessing the given card."
  ([state side card] (access-card state side (make-eid state) card nil))
  ([state side eid card] (access-card state side eid card (:title card)))
  ([state side eid card title]
    ;; Indicate that we are in the access step.
   (swap! state assoc :access true)
    ;; Reset counters for increasing costs of discard, steal, and access.
   (swap! state update-in [:bonus] dissoc :discard)
   (swap! state update-in [:bonus] dissoc :steal-cost)
   (swap! state update-in [:bonus] dissoc :access-cost)
    ;; First trigger pre-access-card, then move to determining if we can discard or steal.
   (wait-for (trigger-event-sync state side :pre-access-card card)
             (access-pay state side eid card title))))

(defn prevent-access
  "Prevents the challenger from accessing cards this run. This will cancel any run effects and not trigger access routines."
  [state _]
  (swap! state assoc-in [:run :prevent-access] true))

(defn max-access
  "Put an upper limit on the number of cards that can be accessed in this run. For Eater."
  [state side n]
  (swap! state assoc-in [:run :max-access] n))

(defn access-bonus
  "Increase the number of cards to be accessed during this run by n. Legwork, Maker's Eye.
  Not for permanent increases like RDI."
  [state side n]
  (swap! state update-in [:run :access-bonus] (fnil #(+ % n) 0)))

(defn access-count [state side kw]
  (let [run (:run @state)
        accesses (+ (get-in @state [:challenger kw]) (:access-bonus run 0))]
    (if-let [max-access (:max-access run)]
      (min max-access accesses) accesses)))


;;; Methods for allowing user-controlled multi-access in locales.

;; choose-access implements game prompts allowing the challenger to choose the order of access.
(defmulti choose-access (fn [cards locale] (get-locale-type (first locale))))

(defn access-helper-party [cards]
  {:prompt "Click a card to access it. You must access all cards in this locale."
   :choices {:req #(some (fn [c] (= (:cid %) (:cid c))) cards)}
   :async true
   :effect (req (wait-for (access-card state side target)
                          (if (< 1 (count cards))
                            (continue-ability state side (access-helper-party (filter #(not= (:cid %) (:cid target)) cards))
                                              card nil)
                            (effect-completed state side eid))))})

(defmethod choose-access :party [cards locale]
  {:async true
   :effect (req (if (and (>= 1 (count cards))
                         (not (any-flag-fn? state :challenger :slow-party-access true
                                            (concat (all-active state :challenger) (all-active state :contestant)))))
                  (access-card state side eid (first cards))
                  (continue-ability state side (access-helper-party cards) card nil)))})

(defn access-helper-hq-or-rd
  "Shows a prompt to access card(s) from the given zone.
  zone: :rd or :hq, for finding Regions to access.
  label: a string label to describe what is being accessed, e.g., 'Card from deck' -- 'deck' being the label.
  amount: how many accesses the challenger has remaining.
  select-fn: a function taking the already-accessed set as argument, and returning the next card to access
      from the given zone.
  title-fn: a function taking a card map being accessed and returning a string to print as the card's title, e.g.,
      'an unseen card from R&D' for an R&D run.
  already-accessed: a set of cards already accessed from this zone or its root."
  [state chosen-zone label amount select-fn title-fn already-accessed]
  (let [get-root-content (fn [state]
                           (filter #(not (contains? already-accessed %)) (get-in @state [:contestant :locales chosen-zone :content])))
        locale-name (central->name chosen-zone)
        unrevealed-region (str "Unrevealed region in " locale-name)
        card-from (str "Card from " label)]
    {:async true
     :prompt "Select a card to access."
     :choices (concat (when (pos? amount) [card-from])
                      (map #(if (revealed? %) (:title %) unrevealed-region)
                           (get-root-content state)))
     :effect (req (cond
                    (= target unrevealed-region)
                    ;; accessing an unrevealed region
                    (let [from-root (get-root-content state)
                          unrevealed (filter #(and (= (last (:zone %)) :content) (not (:revealed %)))
                                           from-root)]
                      (if (= 1 (count unrevealed))
                        ;; only one unrevealed region; access it and continue
                        (wait-for (access-card state side (first unrevealed))
                                  (if (or (pos? amount) (< 1 (count from-root)))
                                    (continue-ability
                                      state side
                                      (access-helper-hq-or-rd state chosen-zone label amount select-fn title-fn
                                                              (conj already-accessed (first unrevealed)))
                                      card nil)
                                    (effect-completed state side eid)))
                        ;; more than one unrevealed region. allow user to select with mouse.
                        (continue-ability
                          state side
                          {:async true
                           :prompt (str "Choose an region in " locale-name " to access.")
                           :choices {:req #(and (= (second (:zone %)) chosen-zone)
                                                (complement already-accessed))}
                           :effect (req (wait-for (access-card state side target)
                                                  (continue-ability
                                                    state side
                                                    (access-helper-hq-or-rd state chosen-zone label amount select-fn title-fn
                                                                            (conj already-accessed target))
                                                    card nil)))}
                          card nil)))
                    ;; accessing a card in deck
                    (= target card-from)
                    (let [accessed (select-fn already-accessed)]
                      (wait-for (access-card state side (make-eid state) accessed
                                             (title-fn accessed))

                                (let [from-root (get-root-content state)]
                                  (if (or (< 1 amount) (not-empty from-root))
                                    (continue-ability
                                      state side
                                      (access-helper-hq-or-rd state chosen-zone label (dec amount) select-fn title-fn
                                                              (if (-> @state :run :shuffled-during-access chosen-zone)
                                                                ;; if the zone was shuffled because of the access,
                                                                ;; the challenger "starts over" excepting any regions that were accessed
                                                                (do (swap! state update-in [:run :shuffled-during-access] dissoc chosen-zone)
                                                                    (set (filter #(= :locales (first (:zone %)))
                                                                                 already-accessed)))
                                                                (conj already-accessed accessed)))
                                      card nil)
                                    (effect-completed state side eid)))))
                    ;; accessing a revealed region
                    :else
                    (let [accessed (some #(when (= (:title %) target) %) (get-root-content state))]
                      (wait-for (access-card state side accessed)
                                (if (or (pos? amount) (< 1 (count (get-root-content state))))
                                  (continue-ability
                                    state side
                                    (access-helper-hq-or-rd state chosen-zone label amount select-fn title-fn
                                                            (conj already-accessed accessed))
                                    card nil)
                                  (effect-completed state side eid))))))}))

(defmethod choose-access :rd [cards locale]
  {:async true
   :effect (req (if (pos? (count cards))
                  (if (= 1 (count cards))
                    (access-card state side eid (first cards) "an unseen card")
                    (let [from-rd (access-count state side :rd-access)]
                      (continue-ability state side (access-helper-hq-or-rd
                                                     state :rd "deck" from-rd
                                                     ;; access the first card in deck that has not been accessed.
                                                     (fn [already-accessed] (first (drop-while already-accessed
                                                                                               (-> @state :contestant :deck))))
                                                     (fn [_] "an unseen card")
                                                     #{})
                                        card nil)))
                  (effect-completed state side eid)))})

(defmethod choose-access :hq [cards locale]
  {:async true
   :effect (req (if (pos? (count cards))
                  (if (and (= 1 (count cards))
                           (not (any-flag-fn? state :challenger :slow-hq-access true)))
                    (access-card state side eid (first cards))
                    (let [from-hq (min (access-count state side :hq-access)
                                       (-> @state :contestant :hand count))
                          ; Handle root only access - no cards to access in hand
                          from-hq (if (some #(= '[:hand] (:zone %)) cards) from-hq 0)]
                      (continue-ability state side (access-helper-hq-or-rd
                                                     state :hq "hand" from-hq
                                                     (fn [already-accessed] (some #(when-not (already-accessed %) %)
                                                                                  (shuffle (-> @state :contestant :hand))))
                                                     (fn [card] (:title card))
                                                     #{})
                                        card nil)))
                  (effect-completed state side eid)))})


(defn access-helper-hq
  "This is a helper for cards to invoke HQ access without knowing how to use the full access method. See Dedicated Neural Net."
  [state from-hq already-accessed]
  (access-helper-hq-or-rd state :hq "hand" from-hq
                          (fn [already-accessed] (some #(when-not (already-accessed %) %)
                                                       (shuffle (-> @state :contestant :hand))))
                          :title
                          already-accessed))


(defn- get-archives-accessible [state]
  ;; only include agendas and cards with an :access ability whose :req is true
  ;; (or don't have a :req, or have an :optional with no :req, or :optional with a true :req.)
  (filter #(let [cdef (card-def %)]
             ;; must also be :seen
             (and (:seen %)
                  (or (is-type? % "Agenda")
                      (should-trigger? state :contestant % nil (:access cdef)))))
          (get-in @state [:contestant :discard])))

(defn- get-archives-inactive [state]
  ;; get faceup cards with no access interaction
  (filter #(let [cdef (card-def %)]
             (and (:seen %)
                  (not (or (is-type? % "Agenda")
                           (should-trigger? state :contestant % nil (:access cdef))))))
          (get-in @state [:contestant :discard])))

(defn access-helper-archives [state amount already-accessed]
  (let [root-content (fn [already-accessed] (remove already-accessed (-> @state :contestant :locales :archives :content)))
        faceup-accessible (fn [already-accessed] (remove already-accessed (get-archives-accessible state)))
        facedown-cards (fn [already-accessed] (filter #(and (not (:seen %))
                                                            (not (already-accessed %)))
                                                      (-> @state :contestant :discard)))

        next-access (fn [state side eid already-accessed card]
                      (continue-ability state side (access-helper-archives state (dec amount) already-accessed)
                                        card nil))

        must-continue? (fn [already-accessed]
                         (and (< 1 amount)
                              (pos? (+ (count (root-content already-accessed))
                                       (count (faceup-accessible already-accessed))
                                       (count (facedown-cards already-accessed))))))]
    {:async true
     :prompt "Select a card to access. You must access all cards."
     :choices (concat (when (<= amount (count (filter (complement already-accessed) (get-archives-inactive state))))
                        [(str "Access " amount " inactive cards")])
                      (map :title (faceup-accessible already-accessed))
                      (map #(if (revealed? %) (:title %) "Unrevealed region in Archives") (root-content already-accessed))
                      (map (fn [_] (str "Facedown card in Archives")) (facedown-cards already-accessed)))
     :effect (req (cond
                    (.endsWith target "inactive cards")
                    ;; Interaction with Bacterial Resourceming. If we have X accesses remaining and <= X inactive cards
                    ;; in Archives, we don't have to access the remaining active cards.  This only happens if you choose
                    ;; to access at least one of the facedown cards added to Archives by Bacterial Resourceming.
                    (do (system-msg state side "accesses the remaining inactive cards in Archives")
                        (effect-completed state side eid))

                    (= target "Facedown card in Archives")
                    ;; accessing a card that was added to archives because of the effect of another card
                    (let [accessed (first (shuffle (facedown-cards already-accessed)))
                          already-accessed (conj already-accessed accessed)]
                      (wait-for (access-card state side accessed)
                                (if (must-continue? already-accessed)
                                  (next-access state side eid already-accessed card)
                                  (effect-completed state side eid))))

                    (= target "Unrevealed region in Archives")
                    ;; accessing an unrevealed region
                    (let [unrevealed (filter #(and (= (last (:zone %)) :content) (not (:revealed %)))
                                           (root-content already-accessed))]
                      (if (= 1 (count unrevealed))
                        ;; only one unrevealed region; access it and continue
                        (let [already-accessed (conj already-accessed (first unrevealed))]
                          (wait-for (access-card state side (first unrevealed))
                                    (if (must-continue? already-accessed)
                                      (next-access state side eid already-accessed card)
                                      (effect-completed state side eid))))
                        ;; more than one unrevealed region. allow user to select with mouse.
                        (continue-ability
                          state side
                          {:async true
                           :prompt "Choose an region in Archives to access."
                           :choices {:req #(and (= (second (:zone %)) :archives)
                                                (not (already-accessed %)))}
                           :effect (req (let [already-accessed (conj already-accessed target)]
                                          (wait-for (access-card state side target)
                                                    (if (must-continue? already-accessed)
                                                      (next-access state side eid already-accessed card)
                                                      (effect-completed state side eid)))))}
                          card nil)))

                    :else
                    ;; accessing a revealed region, or a card in archives
                    (let [accessed (some #(when (= (:title %) target) %)
                                         (concat (faceup-accessible already-accessed) (root-content already-accessed)))
                          already-accessed (conj already-accessed accessed)]
                      (wait-for (access-card state side accessed)
                                (if (must-continue? already-accessed)
                                  (next-access state side eid already-accessed card)
                                  (effect-completed state side eid))))))}))

(defmethod choose-access :archives [cards locale]
  {:async true
   :effect (req (let [cards (concat (get-archives-accessible state) (-> @state :contestant :locales :archives :content))
                      archives-count (+ (count (-> @state :contestant :discard)) (count (-> @state :contestant :locales :archives :content)))]
                  (if (not-empty cards)
                    (if (= 1 archives-count)
                      (access-card state side eid (first cards))
                      (continue-ability state side (access-helper-archives state archives-count #{}) card nil))
                    (effect-completed state side eid))))})

(defn get-all-hosted [hosts]
  (let [hosted-cards (mapcat :hosted hosts)]
    (if (empty? hosted-cards)
      hosted-cards
      (concat hosted-cards (get-all-hosted hosted-cards)))))


(defmulti cards-to-access
  "Gets the list of cards to access for the locale"
  (fn [state side locale] (get-locale-type (first locale))))

(defmethod cards-to-access :hq [state side locale]
  (concat (take (access-count state side :hq-access) (shuffle (get-in @state [:contestant :hand])))
          (get-in @state [:contestant :locales :hq :content])))

(defmethod cards-to-access :rd [state side locale]
  (concat (take (access-count state side :rd-access) (get-in @state [:contestant :deck]))
          (get-in @state [:contestant :locales :rd :content])))

(defmethod cards-to-access :archives [state side locale]
  (swap! state update-in [:contestant :discard] #(map (fn [c] (assoc c :seen true)) %))
  (concat (get-in @state [:contestant :discard]) (get-in @state [:contestant :locales :archives :content])))

(defmethod cards-to-access :party [state side locale]
  (let [contents (get-in @state [:contestant :locales (first locale) :content])]
    (filter (partial can-access-loud state side) (concat contents (get-all-hosted contents)))))

(defn do-access
  "Starts the access routines for the run's locale."
  ([state side eid locale] (do-access state side eid locale nil))
  ([state side eid locale {:keys [hq-root-only] :as args}]
   (wait-for (trigger-event-sync state side :pre-access (first locale))
             (do (let [cards (cards-to-access state side locale)
                       cards (if hq-root-only (remove #(= '[:hand] (:zone %)) cards) cards)
                       n (count cards)]
                   ;; Make `:did-access` true when reaching the access step (no replacement)
                   (when (:run @state) (swap! state assoc-in [:run :did-access] true))
                   (if (or (zero? n)
                           (safe-zero? (get-in @state [:run :max-access])))
                     (system-msg state side "accessed no cards during the run")
                     (do (swap! state assoc-in [:challenger :register :accessed-cards] true)
                         (wait-for (resolve-ability state side (choose-access cards locale) nil nil)
                                   (effect-completed state side eid))
                         (swap! state update-in [:run :cards-accessed] (fnil #(+ % n) 0)))))
                 (handle-end-run state side)))))

(defn replace-access
  "Replaces the standard access routine with the :replace-access effect of the card"
  [state side ability card]
  (wait-for (resolve-ability state side ability card nil)
            (run-cleanup state side)))

;;;; OLDER ACCESS ROUTINES. DEPRECATED.


;;; Ending runs.
(defn register-successful-run
  ([state side locale] (register-successful-run state side (make-eid state) locale))
  ([state side eid locale]
   (swap! state update-in [:challenger :register :successful-run] #(conj % (first locale)))
   (swap! state assoc-in [:run :successful] true)
   (wait-for (trigger-event-simult state side :pre-successful-run nil (first locale))
             (wait-for (trigger-event-simult state side :successful-run nil (first (get-in @state [:run :locale])))
                       (wait-for (trigger-event-simult state side :post-successful-run nil (first (get-in @state [:run :locale])))
                                 (effect-completed state side eid))))))

(defn- successful-run-trigger
  "The real 'successful run' trigger."
  [state side]
  (let [successful-run-effect (get-in @state [:run :run-effect :successful-run])
        card (get-in @state [:run :run-effect :card])]
    (when (and successful-run-effect
               (not (apply trigger-suppress state side :successful-run card)))
      (resolve-ability state side successful-run-effect (:card successful-run-effect) nil)))
  (wait-for (register-successful-run state side (get-in @state [:run :locale]))
            (let [the-run (:run @state)
                  locale (:locale the-run) ; bind here as the locale might have changed
                  run-effect (:run-effect the-run)
                  run-req (:req run-effect)
                  card (:card run-effect)
                  replace-effect (:replace-access run-effect)]
              (if (:prevent-access the-run)
                (do (system-msg state :challenger "is prevented from accessing any cards this run")
                    (resolve-ability state :challenger
                                     {:prompt "You are prevented from accessing any cards this run."
                                      :choices ["OK"]
                                      :effect (effect (handle-end-run))}
                                     nil nil))
                (if (and replace-effect
                         (or (not run-req)
                             (run-req state side (make-eid state) card [(first locale)])))
                  (if (:mandatory replace-effect)
                    (replace-access state side replace-effect card)
                    (swap! state update-in [side :prompt]
                           (fn [p]
                             (conj (vec p) {:msg "Use replacement effect instead of accessing cards?"
                                            :choices ["Replacement effect" "Access cards"]
                                            :effect #(if (= % "Replacement effect")
                                                       (replace-access state side replace-effect card)
                                                       (wait-for (do-access state side locale)
                                                                 (handle-end-run state side)))}))))
                  (wait-for (do-access state side locale)
                            (handle-end-run state side)))))))

(defn successful-run
  "Run when a run has passed all character and the challenger decides to access. The contestant may still get to act in 4.3."
  [state side args]
  (if (get-in @state [:run :contestant-phase-43])
    ;; if contestant requests phase 4.3, then we do NOT fire :successful-run yet, which does not happen until 4.4
    (do (swap! state dissoc :no-action)
        (system-msg state :contestant "wants to act before the run is successful")
        (show-wait-prompt state :challenger "Contestant's actions")
        (show-prompt state :contestant nil "Reveal and take actions before Successful Run" ["Done"]
                     (fn [args-contestant]
                       (clear-wait-prompt state :challenger)
                       (if-not (:ended (:run @state))
                        (show-prompt state :challenger nil "The run is now successful" ["Continue"]
                                     (fn [args-challenger] (successful-run-trigger state :challenger)))
                        (handle-end-run state side)))
                     {:priority -1}))
    (successful-run-trigger state side)))

(defn contestant-phase-43
  "The contestant indicates they want to take action after challenger hits Successful Run, before access."
  [state side args]
  (swap! state assoc-in [:run :contestant-phase-43] true)
  (swap! state assoc-in [:run :no-action] true)
  (system-msg state side "has no further action")
  (trigger-event state side :no-action))

(defn end-run
  "End this run, and set it as UNSUCCESSFUL"
  ([state side] (end-run state side (make-eid state)))
  ([state side eid]
   (let [run (:run @state)
         locale (first (get-in @state [:run :locale]))]
     (swap! state update-in [:challenger :register :unsuccessful-run] #(conj % locale))
     (swap! state assoc-in [:run :unsuccessful] true)
     (handle-end-run state side)
     (trigger-event-sync state side eid :unsuccessful-run run))))

(defn jack-out-prevent
  [state side]
  (swap! state update-in [:jack-out :jack-out-prevent] (fnil inc 0))
  (prevent-jack-out state side))

(defn- resolve-jack-out
  [state side eid]
  (end-run state side)
  (system-msg state side "is done facing attack(s)")
  (trigger-event-sync state side (make-result eid true) :jack-out))

(defn jack-out
  "The challenger decides to jack out."
  ([state side] (jack-out state side (make-eid state)))
  ([state side eid]
   (swap! state update-in [:jack-out] dissoc :jack-out-prevent)
   (wait-for (trigger-event-sync state side :pre-jack-out)
             (let [prevent (get-prevent-list state :contestant :jack-out)]
               (if (cards-can-prevent? state :contestant prevent :jack-out)
                 (do (system-msg state :contestant "has the option to prevent the Challenger from jacking out")
                     (show-wait-prompt state :challenger "Contestant to prevent the jack out" {:priority 10})
                     (show-prompt state :contestant nil
                                  (str "Prevent the Challenger from jacking out?") ["Done"]
                                  (fn [_]
                                    (clear-wait-prompt state :challenger)
                                    (if-let [_ (get-in @state [:jack-out :jack-out-prevent])]
                                      (effect-completed state side (make-result eid false))
                                      (do (system-msg state :contestant "will not prevent the Challenger from jacking out")
                                          (resolve-jack-out state side eid))))
                                  {:priority 10}))
                 (do (resolve-jack-out state side eid)
                     (effect-completed state side (make-result eid false))))))))

(defn- trigger-run-end-events
  [state side eid run]
  (cond
    ;; Successful
    (:successful run)
    (do
      (play-sfx state side "run-successful")
      (trigger-event-simult state side eid :successful-run-ends nil run))
    ;; Unsuccessful
    (:unsuccessful run)
    (do
      (play-sfx state side "run-unsuccessful")
      (trigger-event-sync state side eid :unsuccessful-run-ends run))
    ;; Neither
    :else
    (effect-completed state side eid)))

(defn run-cleanup
  "Trigger appropriate events for the ending of a run."
  [state side]
  (let [run (:run @state)
        locale (:locale run)
        eid (:eid run)]
    (swap! state assoc-in [:run :ending] true)
    (trigger-event state side :run-ends (first locale))
    (doseq [p (filter #(has-subtype? % "Icebreaker") (all-active-placed state :challenger))]
      (update! state side (update-in (get-card state p) [:pump] dissoc :all-run))
      (update! state side (update-in (get-card state p) [:pump] dissoc :encounter ))
      (update-breaker-strength state side p))
    (let [run-effect (get-in @state [:run :run-effect])]
      (when-let [end-run-effect (:end-run run-effect)]
        (resolve-ability state side end-run-effect (:card run-effect) [(first locale)])))
    (swap! state update-in [:challenger :credit] - (get-in @state [:challenger :run-credit]))
    (swap! state assoc-in [:challenger :run-credit] 0)
    (swap! state assoc :run nil)
    (update-all-character state side)
    (demote-all-characters state side)
    (swap! state dissoc :access)
    (clear-run-register! state)
    (trigger-run-end-events state side eid run)))

(defn handle-end-run
  "Initiate run resolution."
  [state side]
  (if-not (and (empty? (get-in @state [:challenger :prompt])) (empty? (get-in @state [:contestant :prompt])))
    (swap! state assoc-in [:run :ended] true)
    (run-cleanup state side)))

(defn close-access-prompt
  "Closes a 'You accessed _' prompt through a non-standard card effect like Imp."
  [state side]
  (let [prompt (-> @state side :prompt first)
        eid (:eid prompt)]
    (swap! state update-in [side :prompt] rest)
    (effect-completed state side eid)
    (when-let [run (:run @state)]
      (when (and (:ended run) (empty? (get-in @state [:challenger :prompt])) )
        (handle-end-run state :challenger)))))

(defn get-run-characters
  [state side]
  (get-in @state (concat [side :locales] (:locale (:run @state)) [:characters])))
