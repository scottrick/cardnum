(in-ns 'game.core)

;;; Helper functions for Draft cards
(def draft-points-target
  "Set each side's agenda points target at 6, per draft format rules"
  (req (swap! state assoc-in [:challenger :agenda-point-req] 6)
       (swap! state assoc-in [:contestant :agenda-point-req] 6)))

(defn- has-most-faction?
  "Checks if the faction has a plurality of revealed / placed cards"
  [state side fc]
  (let [card-list (if (= side :contestant)
                    (filter :revealed (all-placed state :contestant))
                    (all-placed state :challenger))
        faction-freq (frequencies (map :faction card-list))
        reducer (fn [{:keys [max-count] :as acc} faction count]
                  (cond
                    ;; Has plurality update best-faction
                    (> count max-count)
                    {:max-count count :max-faction faction}
                    ;; Lost plurality
                    (= count max-count)
                    (dissoc acc :max-faction)
                    ;; Count is not more, do not change the accumulator map
                    :default
                    acc))
        best-faction (:max-faction (reduce-kv reducer {:max-count 0 :max-faction nil} faction-freq))]
    (= fc best-faction)))

;;; Card definitions
(def cards-identities
  {})