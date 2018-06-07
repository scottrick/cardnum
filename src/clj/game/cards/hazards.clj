(in-ns 'game.core)

(def cards-hazards
  {"Lure of the Senses"
   {:hosting {:req #(and (is-type? % "Character")
                         (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}

   "Revealed to all Watchers"
   {:abilities [{:label "Resolve"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:msg (msg "to draw 3 cards.")
                                                  :effect (effect (set-hand-aside :hand))} card nil)))}]}

   })