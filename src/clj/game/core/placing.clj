(in-ns 'game.core)

(declare available-mu free-mu host in-play? place-locked? make-rid reveal run-flag? locale-list placeable-locales locale->zone
         set-prop system-msg turn-flag? update-breaker-strength update-character-strength update-run-character use-mu)

;;;; Functions for the placeation and deactivation of cards.

;;; Deactivate a card
(defn- dissoc-card
  "Dissoc relevant keys in card"
  [card keep-counter]
  (let [c (dissoc card :current-strength :abilities :subroutines :challenger-abilities :revealed :special :new
                  :added-virus-counter :subtype-target :sifr-used :sifr-target)
        c (if keep-counter c (dissoc c :counter :rec-counter :advance-counter :extra-advance-counter))]
    (if (and (= (:side c) "Challenger") (not= (last (:zone c)) :facedown))
      (dissoc c :placed :facedown :counter :rec-counter :pump :locale-target) c)))

(defn- trigger-leave-effect
  "Triggers leave effects for specified card if relevant"
  [state side {:keys [disabled placed revealed facedown zone host] :as card}]
  (when-let [leave-effect (:leave-play (card-def card))]
    (when (and (not disabled)
               (not (and (= (:side card) "Challenger") host (not placed) (not facedown)))
               (or (and (= (:side card) "Challenger") placed (not facedown))
                   revealed
                   (and host (not facedown))
                   (= (first zone) :current)
                   (= (first zone) :scored)))
      (leave-effect state side (make-eid state) card nil))))

(defn deactivate
  "Deactivates a card, unregistering its events, removing certain attribute keys, and triggering
  some events."
  ([state side card] (deactivate state side card nil))
  ([state side card keep-counter]
   (unregister-events state side card)
   (trigger-leave-effect state side card)
   (when-let [mu (:memoryunits card)]
     (when (and (:placed card)
                (not (:facedown card)))
       (free-mu state mu)))
   (when (and (find-cid (:cid card) (all-active-placed state side))
              (not (:disabled card))
              (or (:revealed card)
                  (:placed card)))
     (when-let [in-play (:in-play (card-def card))]
       (apply lose state side in-play)))
   (dissoc-card card keep-counter)))


;;; Initialising a card
(defn- ability-init
  "Gets abilities associated with the card"
  [cdef]
  (let [abilities (if (:recurring cdef)
                    (conj (:abilities cdef) {:msg "Take 1 [Recurring Credits]"})
                    (:abilities cdef))]
    (for [ab abilities]
      (assoc (select-keys ab [:cost :pump :breaks]) :label (make-label ab)))))

(defn- contestant-ability-init
  "Gets abilities associated with the card"
  [cdef]
  (for [ab (:contestant-abilities cdef)]
    (assoc (select-keys ab [:cost]) :label (make-label ab))))

(defn- challenger-ability-init
  "Gets abilities associated with the card"
  [cdef]
  (for [ab (:challenger-abilities cdef)]
    (assoc (select-keys ab [:cost]) :label (make-label ab))))

(defn- subroutines-init
  "Initialised the subroutines associated with the card, these work as abilities"
  [cdef]
  (map-indexed (fn [idx sub]
                 {:label (make-label sub)
                  :from-cid nil
                  :data {:cdef-idx idx}})
               (:subroutines cdef)))

(defn card-init
  "Initializes the abilities and events of the given card."
  ([state side card] (card-init state side card {:resolve-effect true :init-data true}))
  ([state side card args] (card-init state side (make-eid state) card args))
  ([state side eid card {:keys [resolve-effect init-data] :as args}]
   (let [cdef (card-def card)
         recurring (:recurring cdef)
         abilities (ability-init cdef)
         run-abs (challenger-ability-init cdef)
         contestant-abs (contestant-ability-init cdef)
         subroutines (subroutines-init cdef)
         c (merge card
                  (when init-data (:data cdef))
                  {:abilities abilities
                   :subroutines subroutines
                   :challenger-abilities run-abs
                   :contestant-abilities contestant-abs})
         c (if (number? recurring) (assoc c :rec-counter recurring) c)
         c (if (string? (:strength c)) (assoc c :strength 0) c)]
     (when recurring
       (let [r (if (number? recurring)
                 (effect (set-prop card :rec-counter recurring))
                 recurring)]
         (register-events state side
                          {(if (= side :contestant) :contestant-phase-12 :challenger-phase-12)
                           {:effect r}} c)))
     (update! state side c)
     (when-let [events (:events cdef)]
       (register-events state side events c))
     (if (and resolve-effect (is-ability? cdef))
       (resolve-ability state side eid cdef c nil)
       (effect-completed state side eid))
     (when-let [in-play (:in-play cdef)]
       (apply gain state side in-play))
     (get-card state c))))


;;; Intalling a contestant card
(defn- contestant-can-place-reason
  "Checks if the specified card can be placed.
   Returns true if there are no problems
   Returns :regOLDion if RegOLDion check fails
   Returns :character if Character check fails
   !! NB: This should only be used in a check with `true?` as all return values are truthy"
  [state side card dest-zone]
  (cond
    ;; RegOLDion check
    (and (has-subtype? card "RegOLDion")
         (some #(has-subtype? % "RegOLDion") dest-zone))
    :regOLDion
    ;; Character place prevented by Unscheduled Maintenance
    (and (character? card)
         (not (turn-flag? state side card :can-place-character)))
    :character
    ;; Placing not locked
    (place-locked? state side) :lock-place
    ;; no restrictions
    :default true))

(defn- contestant-can-place?
  "Checks `contestant-can-place-reason` if not true, toasts reason and returns false"
  [state side card dest-zone]
  (let [reason (contestant-can-place-reason state side card dest-zone)
        reason-toast #(do (toast state side % "warning") false)
        title (:title card)]
    (case reason
      ;; pass on true value
      true true
      ;; failed regOLDion check
      :regOLDion
      (reason-toast (str "Cannot place " (:title card) ", limit of one RegOLDion per locale"))
      ;; failed place lock check
      :lock-place
      (reason-toast (str "Unable to place " title ", placing is currently locked"))
      ;; failed Character check
      :character
      (reason-toast (str "Unable to place " title ": can only place 1 piece of Character per turn")))))

(defn contestant-placeable-type?
  "Is the card of an acceptable type to be placed in a locale"
  [card]
  (some? (#{"Site" "Agenda" "Character" "Region" "Resource"} (:type card))))

(defn- contestant-place-site-agenda
  "Forces the contestant to discard an existing site or agenda if a second was just placed."
  [state side eid card dest-zone locale]
  (let [prev-card (some #(when (#{"Hazard"} (:type %)) %) dest-zone)]
    (if (and (#{"Hazard"} (:type card))
             prev-card
             (not (:host card)))
      (continue-ability state side {:prompt (str "The " (:title prev-card) " in " locale " will now be discarded.")
                                    :choices ["OK"]
                                    :async true
                                    :effect (req (system-msg state :contestant (str "discards " (card-str state prev-card)))
                                                 (if (get-card state prev-card) ; make sure they didn't discard the card themselves
                                                   (discard state :contestant eid prev-card {:keep-locale-alive true})
                                                   (effect-completed state :contestant eid)))}
                       nil nil)
      (effect-completed state side eid))))

(defn- contestant-place-message
  "Prints the correct place message."
  [state side card locale place-state cost-str]
  (let [card-name (if (or (= :revealed-no-cost place-state)
                          (= :face-up place-state)
                          (:revealed card))
                    (:title card)
                    (if (character? card) "Character" "a card"))
        locale-name (if (= locale "New party")
                      (str (party-num->name (get-in @state [:rid])) " (new party)")
                      locale)]
    (system-msg state side (str (build-spend-msg cost-str "place") card-name
                                (if (character? card) " protecting " " in ") locale-name))))

(defn contestant-place-list
  "Returns a list of targets for where a given card can be placed."
  [state card]
  (let [hosts (filter #(when-let [can-host (:can-host (card-def %))]
                        (and (revealed? %)
                             (can-host state :contestant (make-eid state) % [card])))
                      (all-placed state :contestant))]
    (concat hosts (placeable-locales state card))))

(defn- contestant-place-continue
  "Used by contestant-place to actually place the card, reveal it if it's supposed to be placed
  revealed, and calls :contestant-place in an awaitable fashion."
  [state side eid card locale {:keys [place-state host-card front] :as args} slot cost-str]
  (let [cdef (card-def card)
        dest-zone (get-in @state (cons :contestant slot))
        place-state (or place-state (:place-state cdef))
        c (-> card
              (assoc :advanceable (:advanceable cdef) :new true)
              (dissoc :seen :disabled))]
    (clear-place-cost-bonus state side)
    (when-not host-card
      (contestant-place-message state side c locale place-state cost-str))
    (play-sfx state side "place-contestant")

    (let [moved-card (if host-card
                       (host state side host-card (assoc c :placed true))
                       (move state side c slot {:front front}))]
      (when (is-type? c "Agenda")
        (update-advancement-cost state side moved-card))

      ;; Check to see if a second agenda/site was placed.
      (wait-for (contestant-place-site-agenda state side moved-card dest-zone locale)
                (letfn [(event [state side eid _]
                          (trigger-event-sync state side eid :contestant-place (get-card state moved-card)))]
                  (case place-state
                    ;; Ignore all costs. Pass eid to reveal.
                    :revealed-no-cost
                    (wait-for (event state side nil)
                              (reveal state side eid moved-card {:ignore-cost :all-costs}))

                    ;; Ignore reveal cost only. Pass eid to reveal.
                    :revealed-no-reveal-cost
                    (wait-for (event state side nil)
                              (reveal state side eid moved-card {:ignore-cost :reveal-costs}))

                    ;; Pay costs. Pass eid to reveal.
                    :revealed
                    (wait-for (event state side nil)
                              (reveal state side eid moved-card nil))

                    ;; "Face-up" cards. Trigger effect-completed manually.
                    :face-up
                    (if (:place-state cdef)
                      (wait-for (card-init state side
                                           (assoc (get-card state moved-card) :revealed true :seen true)
                                           {:resolve-effect false
                                            :init-data true})
                                (event state side eid nil))
                      (do (update! state side (assoc (get-card state moved-card) :revealed true :seen true))
                          (event state side eid nil)))

                    ;; All other cards. Trigger events, which will trigger effect-completed
                    (event state side eid nil))
                  (when-let [dre (:hidden-events cdef)]
                    (when-not (:revealed (get-card state moved-card))
                      (register-events state side dre moved-card))))))))

(defn- contestant-place-pay
  "Used by contestant-place to pay place costs, code continues in contestant-place-continue"
  [state side eid card locale {:keys [extra-cost no-place-cost host-card action] :as args} slot]
  (let [dest-zone (get-in @state (cons side slot))
        character-cost 0
        all-cost (concat extra-cost [:credit character-cost])
        end-cost (if no-place-cost 0 (place-cost state side card all-cost))
        end-fn #((clear-place-cost-bonus state side)
                 (effect-completed state side eid))]
    (if (and (contestant-can-place? state side card dest-zone)
             (not (place-locked? state :contestant)))
      (wait-for (pay-sync state side card end-cost {:action action})
                (if-let [cost-str async-result]
                  (if (= locale "New party")
                    (wait-for (trigger-event-sync state side :locale-created card)
                              (contestant-place-continue state side eid card locale args slot cost-str))
                    (contestant-place-continue state side eid card locale args slot cost-str))
                  (end-fn)))
      (end-fn))))

(defn organize
  ([state side card locale] (organize state side (make-eid state) card locale nil))
  ([state side card locale args] (organize state side (make-eid state) card locale args))
  ([state side eid card locale {:keys [host-card] :as args}]
   (cond
     ;; No locale selected; show prompt to select an place site (Interns, Lateral Growth, etc.)
     (not locale)
     (continue-ability state side (let [c (get-card state card)]
                       {:prompt (str "Choose a character for " (:title c) " to follow or not." )
                        :choices (concat ["New party"] (zones->sorted-names
                                                         (if (= (:side c) "Contestant") (get-party-zones state) (get-party-zones-challenger state))))
                        :async true
                        :effect (effect (organize eid c target args))})
                       card nil)
     ;; A card was selected as the locale; recurse, with the :host-card parameter set.
     (and (map? locale) (not host-card))
     (organize state side eid card locale (assoc args :host-card locale))
     ;; A locale was selected
     :else
     (let [slot (if host-card
                  (:zone host-card)
                  (conj (locale->zone state locale) (if (character? card) :characters :content)))
           dest-zone (get-in @state (cons :contestant slot))]
       ;; trigger :pre-contestant-place before computing place costs so that
       ;; event handlers may adjust the cost.
       (wait-for (trigger-event-sync state side :pre-contestant-place card {:locale locale :dest-zone dest-zone})
                 (contestant-place-pay state side eid card locale args slot))))))

(defn contestant-place
  "Places a card in the chosen locale. If locale is nil, asks for locale to place in.
  The args input takes the following values:
  :host-card - Card to host on
  :extra-cost - Extra place costs
  :no-place-cost - true if place costs should be ignored
  :action - What type of action placed the card
  :place-state - Can be :revealed-no-cost, :revealed-no-reveal-cost, :revealed, or :faceup"
  ([state side card locale] (contestant-place state side (make-eid state) card locale nil))
  ([state side card locale args] (contestant-place state side (make-eid state) card locale args))
  ([state side eid card locale {:keys [host-card] :as args}]
   (cond
     ;; No locale selected; show prompt to select an place site (Interns, Lateral Growth, etc.)
     (not locale)
     (continue-ability state side
                       {:prompt (str "Choose a location to place " (:title card))
                        :choices (contestant-place-list state card)
                        :async true
                        :effect (effect (contestant-place eid card target args))}
                       card nil)
     ;; A card was selected as the locale; recurse, with the :host-card parameter set.
     (and (map? locale) (not host-card))
     (contestant-place state side eid card locale (assoc args :host-card locale))
     ;; A locale was selected
     :else
     (let [slot (if host-card
                  (:zone host-card)
                  (conj (locale->zone state locale) (if (character? card) :characters :content)))
           dest-zone (get-in @state (cons :contestant slot))]
       ;; trigger :pre-contestant-place before computing place costs so that
       ;; event handlers may adjust the cost.
       (wait-for (trigger-event-sync state side :pre-contestant-place card {:locale locale :dest-zone dest-zone})
                 (contestant-place-pay state side eid card locale args slot))))))


;;; Placing a challenger card
(defn- challenger-can-place-reason
  "Checks if the specified card can be placed.
   Checks uniqueness of card and placed console.
   Returns true if there are no problems
   Returns :console if Console check fails
   Returns :unique if uniqueness check fails
   Returns :req if card-def :req check fails
   !! NB: This should only be used in a check with `true?` as all return values are truthy"
  [state side card facedown]
  (let [card-req (:req (card-def card))
        uniqueness (:uniqueness card)]
    (cond
      ;; Can always place a card facedown
      facedown true
      ;; Console check
      (and (has-subtype? card "Console")
           (some #(has-subtype? % "Console") (all-active-placed state :challenger)))
      :console
      ;; Placing not locked
      (place-locked? state :challenger) :lock-place
      ;; Uniqueness check
      (and uniqueness (in-play? state card)) :unique
      ;; Req check
      (and card-req (not (card-req state side (make-eid state) card nil))) :req
      ;; Nothing preventing place
      :default true)))

(defn challenger-can-place?
  "Checks `challenger-can-place-reason` if not true, toasts reason and returns false"
  [state side card facedown]
  (let [reason (challenger-can-place-reason state side card facedown)
        reason-toast #(do (toast state side % "warning") false)
        title (:title card)]
    (case reason
      ;; pass on true value
      true true
      ;; failed unique check
      :unique
      (reason-toast (str "Cannot place a second copy of " title " since it is unique. Please discard currently"
                         " placed copy first"))
      ;; failed place lock check
      :lock-place
      (reason-toast (str "Unable to place " title " since placing is currently locked"))
      ;; failed console check
      :console
      (reason-toast (str "Unable to place " title ": an placed console prevents the placeation of a replacement"))
      :req
      (reason-toast (str "Placeation requirements are not fulfilled for " title)))))

(defn- challenger-get-cost
  "Get the total place cost for specified card"
  [state side {:keys [cost] :as card}
   {:keys [extra-cost no-cost facedown] :as params}]
  (place-cost state side card
                (concat extra-cost (when (and (not no-cost) (not facedown)) [:credit cost]))))

(defn- challenger-place-message
  "Prints the correct msg for the card place"
  [state side card-title cost-str
   {:keys [no-cost host-card facedown custom-message] :as params}]
  (if facedown
    (system-msg state side "places a card facedown")
    (if custom-message
      (system-msg state side custom-message)
      (system-msg state side
                  (str (build-spend-msg cost-str "place") card-title
                       (when host-card (str " on " (card-str state host-card)))
                       (when no-cost " at no cost"))))))

(defn- handle-virus-counter-flag
  "Deal with setting the added-virus-counter flag"
  [state side placed-card]
  (if (and (has-subtype? placed-card "Virus")
           (pos? (get-counters placed-card :virus)))
    (update! state side (assoc placed-card :added-virus-counter true))))

(defn challenger-place
  "Places specified challenger card if able
  Params include extra-cost, no-cost, host-card, facedown and custom-message."
  ([state side card] (challenger-place state side (make-eid state) card nil))
  ([state side card params] (challenger-place state side (make-eid state) card params))
  ([state side eid card {:keys [host-card facedown] :as params}]
   (if (and (empty? (get-in @state [side :locked (-> card :zone first)]))
            (not (place-locked? state side)))
     (if-let [hosting (and (not host-card) (not facedown) (:hosting (card-def card)))]
       (continue-ability state side
                         {:choices hosting
                          :prompt (str "Choose a card to place " (:title card) " on")
                          :async true
                          :effect (effect (challenger-place eid card (assoc params :host-card target)))}
                         card nil)
       (do (trigger-event state side :pre-place card facedown)
           (let [cost (int 0)]
             (if (challenger-can-place? state side card facedown)
                 (let [c (if host-card
                           (host state side host-card card)
                           (move state side card
                                 [:rig (if facedown :facedown (to-keyword (:type card)))]))
                       c (assoc c :placed true :new true)
                       placed-card (if facedown
                                     (update! state side c)
                                     (card-init state side c {:resolve-effect false
                                                              :init-data true}))]
                   (challenger-place-message state side (:title card) nil params)
                   (play-sfx state side "place-challenger")
                   (when (is-type? card "Radicle")
                     (swap! state assoc-in [:challenger :register :placed-radicle] true))
                   (when (has-subtype? c "Icebreaker")
                     (update-breaker-strength state side c))
                   (trigger-event-simult state side eid :challenger-place
                                         {:card-ability (card-as-handler placed-card)}
                                         placed-card))
                 (effect-completed state side eid)))
           (clear-place-cost-bonus state side)))
     (effect-completed state side eid))))
