(in-ns 'game.core)

(declare card-flag?)

;;; Ice strength functions
(defn character-strength-bonus
  "Increase the strength of the given character by n. Negative values cause a decrease."
  [state side n character]
  ;; apply the strength bonus if the bonus is positive, or if the character doesn't have the "can't lower strength" flag
  (when (or (pos? n) (not (card-flag? character :cannot-lower-strength true)))
    (swap! state update-in [:bonus :character-strength] (fnil #(+ % n) 0))))

(defn character-strength
  "Gets the modified strength of the given character."
  [state side {:keys [strength] :as card}]
  (+ (if-let [strfun (:strength-bonus (card-def card))]
       (+ strength (strfun state side (make-eid state) card nil))
       strength)
     (or (get-in @state [:bonus :character-strength]) 0)))

(defn update-character-strength
  "Updates the given character's strength by triggering strength events and updating the card."
  [state side character]
  (let [character (get-card state character) oldstren (or (:current-strength character) (:strength character))]
    (when (:revealed character)
      (swap! state update-in [:bonus] dissoc :character-strength)
      (trigger-event state side :pre-character-strength character)
      (update! state side (assoc character :current-strength (character-strength state side character)))
      (trigger-event state side :character-strength-changed (get-card state character) oldstren))))

(defn update-character-in-locale
  "Updates all character in the given locale's :characters field."
  [state side locale]
  (doseq [character (:characters locale)] (update-character-strength state side character) ))

(defn update-all-character
  "Updates all placed character."
  [state side]
  ;;(doseq [locale (get-in @state [:contestant :locales])]
   ;; (update-character-in-locale state side (second locale)))
   true)


;;; Icebreaker functions.
(defn breaker-strength-bonus
  "Increase the strength of the breaker by n. Negative values cause a decrease."
  [state side n]
  (swap! state update-in [:bonus :breaker-strength] (fnil #(+ % n) 0)))

(defn breaker-strength
  "Gets the modified strength of the given breaker."
  [state side {:keys [strength] :as card}]
  (when-not (nil? strength)
    ;; A breaker's current strength is the sum of its native strength,
    ;; the bonus reported by its :strength-bonus function,
    ;; the effects of per-encounter and per-run strength pumps,
    ;; and miscellaneous increases registered by third parties (Dinosaurus, others).
    (+ (if-let [strfun (:strength-bonus (card-def card))]
         (+ strength (strfun state side (make-eid state) card nil))
         strength)
       (get-in card [:pump :encounter] 0)
       (get-in card [:pump :all-run] 0)
       (get-in @state [:bonus :breaker-strength] 0))))

(defn update-breaker-strength
  "Updates a breaker's current strength by triggering updates and applying their effects."
  [state side breaker]
  (let [breaker (get-card state breaker)
        oldstren (or (:current-strength breaker) (:strength breaker))]
    (swap! state update-in [:bonus] dissoc :breaker-strength)
    (trigger-event state side :pre-breaker-strength breaker)
    (update! state side (assoc breaker :current-strength (breaker-strength state side breaker)))
    (trigger-event state side :breaker-strength-changed (get-card state breaker) oldstren)))

(defn pump
  "Increase a breaker's strength by n for the given duration of :encounter or :all-run"
  ([state side card n] (pump state side card n :encounter))
  ([state side card n duration]
   (update! state side (update-in card [:pump duration] (fnil #(+ % n) 0)))
   (update-breaker-strength state side (get-card state card))
   (trigger-event state side :pump-breaker n card)))

;;; Others
(defn character-index
  "Get the zero-based index of the given character in its locale's list of character, where index 0
  is the innermost character."
  [state character]
  (first (keep-indexed #(when (= (:cid %2) (:cid character)) %1) (get-in @state (cons :contestant (:zone character))))))
