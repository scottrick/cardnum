(in-ns 'game.core)

(def cards-hazards
  {"Lure of the Senses"
   {:hosting {:req #(and (is-type? % "Character")
                         (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}})