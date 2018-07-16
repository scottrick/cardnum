(in-ns 'game.core)

(def cards-hazards
  {
   "Alone and Unadvised"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Aware of Their Ways"
   {:abilities [{:effect (req
                           (move state side card :discard)
                           (let [opp-side (if (= side :contestant)
                                            :challenger
                                            side)
                                 kount (count (get-in @state [opp-side :discard]))]
                             (loop [k (if (< kount 4) kount 4)]
                               (when (> k 0)
                                 (move state opp-side (rand-nth (get-in @state [opp-side :discard])) :play-area)
                                 (recur (- k 1))))
                           (resolve-ability state side
                                            {:prompt "Select a card to remove from the game"
                                             :choices {:req (fn [t] (card-is? t :side opp-side))}
                                             :effect (req (doseq [c (get-in @state [opp-side :play-area])]
                                                            (if (= c target)
                                                              (move state opp-side c :rfg)
                                                              (move state opp-side c :discard)
                                                              )))} nil nil)))}]}
   "Bring Our Curses Home"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Cast from the Order"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Covetous Thoughts"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Cruel Claw Perceived"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Despair of the Heart"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Diminish and Depart"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Dragons Curse"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Enchanted Stream"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Eyes of the Shadow"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Fear of Kin"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Fled into Darkness"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Flies and Spiders"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Foes Shall Fall"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Fools Bane"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Foolish Words"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "He is Lost to Us"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Heritage Forsaken"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Icy Touch"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "In the Grip of Ambition"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Inner Rot"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Longing for the West"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Lure of Creation"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Lure of Expedience"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Lure of Nature"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Lure of the Senses"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Many Burdens"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Memories Stolen"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Morgul-knife"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Nothing to Eat or Drink"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Out of Practice"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Pale Dream-maker"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Plague"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Politics"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Power Relinquished to Artifice"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Ransom"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Rebel-talk"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
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
   "Shut Yer Mouth"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "So Youve Come Back"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Something Else at Work"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Something Has Slipped"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Spells of the Barrow-wights"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "The Burden of Time"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "The Pale Sword"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Tookish Blood"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Wielders Curse"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Will You Not Come Down?"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   "Wound of Long Burden"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}
   })