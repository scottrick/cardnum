(in-ns 'game.core)

;;;; Various functions for checking small "flag" values of cards, runs, players, etc.

(defn card-flag?
  "Checks the card to see if it has a :flags entry of the given flag-key, and with the given value if provided"
  ;; TODO: add a register for mutable state card flags, separate from this
  ([card flag-key]
   (let [cdef (card-def card)]
     (some? (get-in cdef [:flags flag-key]))))
  ([card flag-key value]
   (let [cdef (card-def card)]
     (= value (get-in cdef [:flags flag-key])))))

(defn card-flag-fn?
  "Checks the card to see if it has a :flags entry of the given flag-key, whose value is a four-argument
  function that returns the given value"
  ([state side card flag-key]
   (let [cdef (card-def card)
         func (get-in cdef [:flags flag-key])]
     (func state side (make-eid state) card nil)))
  ([state side card flag-key value]
   (let [cdef (card-def card)
         func (get-in cdef [:flags flag-key])]
     (and func (= (func state side (make-eid state) card nil) value)))))

(defn any-flag-fn?
  "Checks `card-flag-fn? on all placed cards on specified side for the value with the flag-key
  Default value of `cards` is `(all-active state side)`"
  ([state side flag-key value]
    (any-flag-fn? state side flag-key value (all-active state side)))
  ([state side flag-key value cards]
   (some #(card-flag-fn? state side % flag-key value) cards)))

(defn is-tagged?
  "Returns true if the challenger is tagged."
  [state]
  (or (pos? (get-in state [:challenger :tag]))
      (pos? (get-in state [:challenger :tagged]))
      (pos? (get-in state [:challenger :additional-tag]))))

;;; Generic flag functions
(defn- register-flag!
  "Register a flag of the specific type."
  [state side card flag-type flag condition]
  (swap! state update-in [:stack flag-type flag] #(conj % {:card card :condition condition})))

(defn- check-flag?
  "Flag condition will ask for permission to do something, e.g. :can-reveal, :can-advance
  If allowed, return true, if not allowed, return false. Therefore check for any false conditions.
  Returns true if no flags are present."
  [state side card flag-type flag]
  (let [conditions (get-in @state [:stack flag-type flag])]
    ;; check that every condition returns true
    (every? #((:condition %) state side card) conditions)))

(defn check-flag-types?
  "Checks flag that the specified flag types are permitting the flag"
  [state side card flag flag-types]
  (every? #(check-flag? state side card % flag) flag-types))

(defn get-preventing-cards
  "Returns all cards that are preventing specified flag, checking in the specified flag-types"
  [state side card flag flag-types]
  (let [conditions (mapcat #(get-in @state [:stack % flag]) flag-types)
        predicate (complement #((:condition %) state side card))]
    (map :card (filter predicate conditions))))

(defn has-flag?
  "Checks if the specified flag exists - used for Gene Conditioning Shoppe"
  [state side flag-type flag]
  (not-empty (get-in @state [:stack flag-type flag])))

(defn- clear-all-flags!
  "Clears all flags of specified type"
  [state flag-type]
  (swap! state assoc-in [:stack flag-type] nil))

(defn- clear-flag-for-card!
  "Remove all entries for specified card for flag-type and flag"
  [state side card flag-type flag]
  (swap! state update-in [:stack flag-type flag]
         (fn [flag-map] (remove #(= (get-cid %) (:cid card)) flag-map))))

;; Currently unused
(defn clear-all-flags-for-card!
  "Removes all flags set by the card - of any flag type"
  [state side card]
  (letfn [(clear-flag-type! [flag-type]
            (map #(clear-flag-for-card! state side card flag-type %)
                 (keys (get-in @state [:stack flag-type]))))]
    ;; Only care about the side-effects of this
    (map clear-flag-type! #{:current-run :current-turn :persistent})
    ;; Return the card again
    card))

;;; Run flag - cleared at end of run
(defn register-run-flag!
  "Registers a flag for the current run only. The flag gets cleared in end-run.
  Example: Blackmail flags the inability to reveal character."
  [state side card flag condition]
  (register-flag! state side card :current-run flag condition))

(defn run-flag?
  "Checks if any cards explicitly forbids the flag this run"
  [state side card flag]
  (check-flag? state side card :current-run flag))

(defn clear-run-register!
  "Clears the run-flag register."
  [state]
  (clear-all-flags! state :current-run))

(defn clear-run-flag!
  "Remove any entry associated with card for the given flag"
  [state side card flag]
  (clear-flag-for-card! state side card :current-run flag))

;;; Turn flag - cleared at end of turn
(defn register-turn-flag!
  "As register-run-flag, but for the entire turn."
  [state side card flag condition]
  (register-flag! state side card :current-turn flag condition))

(defn turn-flag?
  "Checks if any cards explicitly forbids the flag this turn"
  [state side card flag]
  (check-flag? state side card :current-turn flag))

(defn clear-turn-register! [state]
  (clear-all-flags! state :current-turn))

(defn clear-turn-flag!
  "Remove any entry associated with card for the given flag"
  [state side card flag]
  (clear-flag-for-card! state side card :current-turn flag))

;;; Persistent flag - has to be cleared manually
(defn register-persistent-flag!
  "A flag that persists until cleared."
  [state side card flag condition]
  (register-flag! state side card :persistent flag condition))

;; Currently unused after Efficiency Committee and Genetics refactor
(defn persistent-flag?
  "Checks if any cards explicitly forbids the flag"
  [state side card flag]
  (check-flag? state side card :persistent flag))

(defn clear-persistent-flag!
  "Remove any entry associated with card for the given flag"
  [state side card flag]
  (clear-flag-for-card! state side card :persistent flag))

;;; Functions related to locales that can be run
(defn prevent-run-on-locale
  "Adds specified locale to list of locales that cannot be run on.
  The causing card is also specified"
  [state card & locales]
  (doseq [locale locales]
    (swap! state assoc-in [:challenger :register :cannot-run-on-locale locale (:cid card)] true)))

(defn enable-run-on-locale
  "Removes specified locale from list of locale for the associated card.
  If other cards are associated with the same locale that locale will still be unable to be run
  on."
  [state card & locales]
  (doseq [locale locales]
    (let [card-map (get-in @state [:challenger :register :cannot-run-on-locale locale])
          reduced-card-map (dissoc card-map (:cid card))]
      (if (empty? reduced-card-map)
        ;; removes locale if no cards block it, otherwise updates the map
        (swap! state update-in [:challenger :register :cannot-run-on-locale] dissoc locale)
        (swap! state assoc-in [:challenger :register :cannot-run-on-locale locale]
               reduced-card-map)))))

(defn can-run-locale?
  "Returns true if the specified locale can be run on. Specified locale must be string form."
  [state locale]
  (not-any? #{locale}
            (map zone->name (keys (get-in @state [:challenger :register :cannot-run-on-locale])))))


;;; Functions for preventing specific game actions.
;;; TODO: look into migrating these to turn-flags and run-flags.
(defn prevent-draw [state side]
  (swap! state assoc-in [:challenger :register :cannot-draw] true))

(defn prevent-jack-out [state side]
  (swap! state assoc-in [:run :cannot-jack-out] true))

;; This function appears unused as well
(defn prevent-steal [state side]
  (swap! state assoc-in [:challenger :register :cannot-steal] true))

(defn prevent-current [state side]
  (swap! state assoc-in [:challenger :register :cannot-play-current] true))

(defn lock-zone [state side cid tside tzone]
  (swap! state update-in [tside :locked tzone] #(conj % cid)))

(defn release-zone [state side cid tside tzone]
  (swap! state update-in [tside :locked tzone] #(remove #{cid} %)))

;;; Small utilities for card properties.
(defn in-locale?
  "Checks if the specified card is placed in -- and not PROTECTING -- a locale"
  [card]
  (= (last (:zone card)) :content))

(defn in-hand?
  "Checks if the specified card is in the hand."
  [card]
  (= (:zone card) [:hand]))

(defn in-discard?
  "Checks if the specified card is in the discard pile."
  [card]
  (= (:zone card) [:discard]))

(defn is-scored?
  "Checks if the specified card is in the scored area of the specified player."
  [state side card]
  (some #(= (:cid %) (:cid card)) (get-in @state [side :scored])))

(defn when-scored?
  "Checks if the specified card is able to be used for a when-scored text ability"
  [card]
  (not (:not-when-scored (card-def card))))

(defn in-deck?
  "Checks if the specified card is in the draw deck."
  [card]
  (= (:zone card) [:deck]))

(defn facedown?
  "Checks if the specified card is facedown."
  [card]
  (or (= (:zone card) [:rig :facedown]) (:facedown card)))

(defn in-contestant-scored?
  "Checks if the specified card is in the Contestant score area."
  [state side card]
  (is-scored? state :contestant card))

(defn in-challenger-scored?
  "Checks if the specified card is in the Challenger score area."
  [state side card]
  (is-scored? state :challenger card))

(defn is-type?
  "Checks if the card is of the specified type, where the type is a string."
  [card type]
  (card-is? card :type type))

(defn has-subtype?
  "Checks if the specified subtype is present in the card, ignoring case."
  [card subtype]
  (letfn [(contains-sub? [card]
            (when-let [subs (:subtype card)]
              (includes? (lower-case subs) (lower-case subtype))))]
    (or (contains-sub? card)
        (contains-sub? (:persistent card)))))

(defn can-host?
  "Checks if the specified card is able to host other cards"
  [card]
  (or (not (revealed? card)) (not (:cannot-host (card-def card)))))

(defn character? [card]
  (is-type? card "Character"))

(defn resource? [card]
  (is-type? card "Resource"))

(defn hazard? [card]
  (is-type? card "Hazard"))

(defn radicle? [card]
  (is-type? card "Radicle"))

(defn revealed? [card]
  (:revealed card))

(defn faceup? [card]
  (or (:seen card) (:revealed card)))

(defn placed? [card]
  (or (:placed card) (= :locales (first (:zone card)))))

(defn active?
  "Checks if the card is active and should receive game events/triggers."
  [{:keys [zone] :as card}]
  (or (is-type? card "Identity")
      (= zone [:current])
      (and (card-is? card :side :contestant)
           (placed? card)
           (revealed? card))
      (and (card-is? card :side :challenger)
           (placed? card)
           (not (facedown? card)))))

(defn undiscardable-while-revealed? [card]
  (and (card-flag? card :undiscardable-while-revealed true) (revealed? card)))

(defn undiscardable-while-radicles? [card]
  (and (card-flag? card :undiscardable-while-radicles true) (placed? card)))

(defn place-locked?
  "Checks if placing is locked"
  [state side]
  (let [kw (keyword (str (name side) "-lock-place"))]
    (or (seq (get-in @state [:stack :current-run kw]))
        (seq (get-in @state [:stack :current-turn kw]))
        (seq (get-in @state [:stack :persistent kw])))))

(defn- can-reveal-reason
  "Checks if the contestant can reveal the card.
  Returns true if so, otherwise the reason:
  :side card is not on :contestant side
  :run-flag run flag prevents reveal
  :turn-flag turn flag prevents reveal
  :unique fails unique check
  :req does not meet reveal requirement"
  [state side card]
  (let [uniqueness (:uniqueness card)
        reveal-req (:reveal-req (card-def card))]
    (cond
      ;; Card on same side?
      (not (same-side? side (:side card))) :side
      ;; No flag restrictions?
      (not (run-flag? state side card :can-reveal)) :run-flag
      (not (turn-flag? state side card :can-reveal)) :turn-flag
      ;; Uniqueness check
      (and uniqueness (some #(and (:revealed %) (= (:code card) (:code %))) (all-placed state :contestant))) :unique
      ;; Reveal req check
      (and reveal-req (not (reveal-req state side (make-eid state) card nil))) :req
      ;; No problems - return true
      :default true)))

(defn can-reveal?
  "Checks if the card can be revealed. Toasts the reason if not."
  ([state side card] (can-reveal? state side card nil))
  ([state side card {:keys [ignore-unique] :as args}]
   (let [reason (can-reveal-reason state side card)
         reason-toast #(do (toast state side %) false)
         title (:title card)]
     (case reason
       ;; Do nothing special if true
       true true
       ;; No need to toast if on different side
       :side false
       ;; Flag restrictions - toast handled by flag
       :run-flag false
       :turn-flag false
       ;; Uniqueness
       :unique (or ignore-unique
                   (reason-toast (str "Cannot reveal a second copy of " title " since it is unique. Please discard the other"
                                      " copy first")))
       ;; Reveal requirement
       :req (reason-toast (str "Reveal requirements for " title " are not fulfilled"))))))

(defn can-steal?
  "Checks if the challenger can steal agendas"
  [state side card]
  (and (check-flag-types? state side card :can-steal [:current-turn :current-run])
       (check-flag-types? state side card :can-steal [:current-turn :persistent])))

(defn can-run?
  "Checks if the challenger is allowed to run"
  [state side]
  (let [cards (->> @state :stack :current-turn :can-run (map :card))]
    (if (empty? cards)
      true
      (do (toast state side (str "Cannot run due to " (join ", " (map :title cards))))
        false))))

(defn can-access?
  "Checks if the challenger can access the specified card"
  [state side card]
  (check-flag-types? state side card :can-access [:current-run :current-turn :persistent]))

(defn can-access-loud
  "Checks if the challenger can access the card, toasts card that is preventing it"
  [state side card]
  (let [cards (get-preventing-cards state side card :can-access [:current-run :current-turn :persistent])]
    (if (empty? cards)
      true
      (do (toast state side (str "Cannot access " (card-str state card) " because of " (join ", " (map :title cards))) "info")
          false))))

(defn can-advance?
  "Checks if the contestant can advance cards"
  [state side card]
  (check-flag-types? state side card :can-advance [:current-turn :persistent]))

(defn can-score?
  "Checks if the contestant can score cards"
  [state side card]
  (check-flag-types? state side card :can-score [:current-turn :persistent]))

(defn can-be-advanced?
  "Returns true if the card can be advanced"
  [card]
  (or (card-is? card :advanceable :always)
      ;; e.g. Tyrant, Woodcutter
      (and (card-is? card :advanceable :while-revealed)
           (revealed? card))
      ;; e.g. Haas Arcology AI
      (and (card-is? card :advanceable :while-unrevealed)
           (not (revealed? card)))
      (and (is-type? card "Agenda")
           (placed? card))))

(defn card-is-public? [state side {:keys [zone] :as card}]
  (if (= side :challenger)
    ;; public challenger cards: in hand and :openhand is true;
    ;; or placed/hosted and not facedown;
    ;; or scored or current or in heap
    (or (card-is? card :side :contestant)
        (and (:openhand (:challenger @state)) (in-hand? card))
        (and (or (placed? card) (:host card)) (not (facedown? card)))
        (#{:scored :discard :current} (last zone)))
    ;; public contestant cards: in hand and :openhand;
    ;; or placed and revealed;
    ;; or in :discard and :seen
    ;; or scored or current
    (or (card-is? card :side :challenger)
        (and (:openhand (:contestant @state)) (in-hand? card))
        (and (or (placed? card) (:host card))
             (or (is-type? card "Operation") (revealed? card)))
        (and (in-discard? card) (:seen card))
        (#{:scored :current} (last zone)))))

(defn ab-can-prevent?
  "Checks if the specified ability definition should prevent.
  Checks for a :req in the :prevent map of the card-def.
  Defaults to false if req check not met"
  ([state side card req-fn target args]
   (ab-can-prevent? state side (make-eid state) card req-fn target args))
  ([state side eid card req-fn target args]
   (cond
     req-fn (req-fn state side eid card (list (assoc args :prevent-target target)))
     :else false)))

(defn get-card-prevention
  "Returns card prevent abilities for a given type"
  [card type]
  (->> (-> card card-def :interactions :prevent)
       (filter #(contains? (:type %) type))))

(defn card-can-prevent?
  "Checks if a cards req (truthy test) can be met for this type"
  [state side card type target args]
  (let [abilities (get-card-prevention card type)]
    (some #(-> % false? not) (map #(ab-can-prevent? state side card (:req %) target args) abilities))))

(defn cards-can-prevent?
  "Checks if any cards in a list can prevent this type"
  ([state side cards type] (cards-can-prevent? state side cards type nil nil))
  ([state side cards type target args]
  (some #(true? %) (map #(card-can-prevent? state side % type target args) cards))))

(defn get-prevent-list
  "Get list of cards that have prevent for a given type"
  [state side type]
  (->> (all-active state side)
       (filter #(seq (get-card-prevention % type)))))
