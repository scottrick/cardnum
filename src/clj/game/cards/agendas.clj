(in-ns 'game.core)

(declare is-scored?)

(defn character-boost-agenda [subtype]
  (letfn [(count-character [contestant]
            (reduce (fn [c server]
                      (+ c (count (filter #(and (has-subtype? % subtype)
                                                (revealed? %))
                                          (:characters server)))))
                    0 (flatten (seq (:servers contestant)))))]
    {:msg (msg "gain " (count-character contestant) " [Credits]")
     :interactive (req true)
     :effect (effect (gain :credit (count-character contestant))
                     (update-all-character))
     :swapped {:effect (req (update-all-character state side))}
     :events {:pre-character-strength {:req (req (has-subtype? target subtype))
                                 :effect (effect (character-strength-bonus 1 target))}}}))

(def cards-agendas
  {})