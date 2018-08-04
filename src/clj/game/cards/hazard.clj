(ns game.cards.hazard
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int]]
            [cardnum.cards :refer [all-cards]]))

(def card-definitions
  {
   "Alone and Unadvised"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
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
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Cast from the Order"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Covetous Thoughts"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Cruel Claw Perceived"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Despair of the Heart"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Diminish and Depart"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Dragons Curse"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Enchanted Stream"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Eyes of the Shadow"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Fear of Kin"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Fled into Darkness"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Flies and Spiders"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Foes Shall Fall"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Fools Bane"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Foolish Words"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "He is Lost to Us"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Heritage Forsaken"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Icy Touch"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "In the Grip of Ambition"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Inner Rot"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Long Dark Reach"
   {:abilities [{:label "Top seven"
                 :effect (req
                           (if (< 9 (count (get-in @state [side :deck])))
                             (loop [k 7]
                               (when (> k 0)
                                 (move state side (first (get-in @state [side :deck])) :play-area)
                                 (recur (- k 1)))))
                           )}
                {:label "Top shuffle"
                 :effect (req (move state side card :discard)
                              (shuffle! state side :play-area)
                              (doseq [c (get-in @state [side :play-area])]
                                (move state side c :deck))
                              )}]}
   "Longing for the West"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Lure of Creation"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Lure of Expedience"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Lure of Nature"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Lure of the Senses"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Many Burdens"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Memories Stolen"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Morgul-knife"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Nothing to Eat or Drink"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Out of Practice"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Pale Dream-maker"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Plague"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Politics"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Power Relinquished to Artifice"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Ransom"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Rebel-talk"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Revealed to all Watchers"
   {:abilities [{:label "Resolve"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:msg (msg "to draw 3 cards.")
                                                  :effect (effect (set-hand-aside :hand))} card nil)))}]}
   "Shut Yer Mouth"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "So Youve Come Back"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Something Else at Work"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Something Has Slipped"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Spells of the Barrow-wights"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "The Burden of Time"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "The Pale Sword"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Tookish Blood"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Wielders Curse"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Will You Not Come Down?"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   "Wound of Long Burden"
   {:hosting {:req #(and (is-type? % "Character") (revealed? %))}}
   })
