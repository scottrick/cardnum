(in-ns 'game.core)

(def cards-hazards
  {
   "Alone and Unadvised"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Bring Our Curses Home"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Covetous Thoughts"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Despair of the Heart"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Dragon's Curse"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Foes Shall Fall"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "In the Grip of Ambition"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Inner Rot"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Longing for the West"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Lure of Creation"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Lure of Expedience"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Lure of Nature"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Lure of the Senses"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Pale Dream-maker"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
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
   "The Burden of Time"
   {:hosting {:req #(and (is-type? % "Character") (rezzed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   })