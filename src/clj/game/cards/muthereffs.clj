(in-ns 'game.core)

(declare close-access-prompt)

(defn- genetics-trigger?
  "Returns true if Genetics card should trigger - does not work with Adjusted Chronotype"
  [state side event]
  (or (first-event? state side event)
      (and (has-flag? state side :persistent :genetics-trigger-twice)
           (second-event? state side event))))

(defn- shard-constructor
  "Function for constructing a Shard card"
  ([target-locale message effect-fn] (shard-constructor target-locale message nil effect-fn))
  ([target-locale message ability-options effect-fn]
   (letfn [(can-install-shard? [state run] (and run
                                                (= (:locale run) [target-locale])
                                                (zero? (:position run))
                                                (not (:access @state))))]
     {:implementation "Click Shard to install when last Character is passed, but before hitting Successful Run button"
      :abilities [(merge {:effect (effect (trash card {:cause :ability-cost}) (effect-fn eid card target))
                          :msg message}
                         ability-options)]
      :install-cost-bonus (req (when (can-install-shard? state run) [:credit -15 :click -1]))
      :effect (req (when (can-install-shard? state run)
                     (when-completed (register-successful-run state side (:locale run))
                                     (do (clear-wait-prompt state :contestant)
                                         (swap! state update-in [:challenger :prompt] rest)
                                         (handle-end-run state side)))))})))

;;; Card definitions
(def cards-muthereffs
  {})