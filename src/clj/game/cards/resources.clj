(in-ns 'game.core)

(declare can-host?)

(def cards-resources
  {"A Flush of Air"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Elrond"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "A New Ringlord"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on your Avatar"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Against All Counsel"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on your Elf-lord"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Align Palantír"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ancient Knowledge"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a elf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ancient Skill and Wisdom"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on an elf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Arcane School"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Await the Advent of Allies"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a non-wizard"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Balin, Dwarf of All Trades"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Balin"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Blood of Huan"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character with a noble hound"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Bow of Alatar"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Alatar"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "By the Ringwraiths Word"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Captain of the House"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Glorfindel"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Choice of Lúthien"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Arwen"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Crimson Hood"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Balin"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dark Tryst"
   {:abilities [{:label "Resolve"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:msg (msg "to draw 3 cards.")
                                                  :effect (effect (draw 3) (move card :rfg))} card nil)))}]}
   "Dreams of Lore"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on the sage"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Durins Day"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a dwarf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarf-friend"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a non-dwarf, non-wizard"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Alchemist"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a dwarf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Art of War"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a dwarf leader"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Elf-friend"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a non-elf, non-wizard"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Fate of the Ithil-stone"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (msg "Which character represents the company")
                                                  :choices {:req #(and (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Fellowship"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "Discard this card if the company changes composition")
                                                            (msg "Place this card with a character/company"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Fosterhome of Royal Heirs"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Elrond"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Gandalfs Friend"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card with a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Gimlis Axe"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Gimili with an Axe"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Give Welcome to the Unexpected"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this on Gandalf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Great Achievement"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on your Avatar"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Great Bow of Lórien"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Legolas or Fanar bearing Galadhrim"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Great Friendship"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Gimli or Legolas"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Grey Embassy"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Gandalf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Grim Voiced and Grim Faced"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Bard Bowman"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Helm of Her Secrecy"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card with a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Herb-lore"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Radagast"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "His Beard Long and Forked"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a dwarf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Hobbit-lore"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Gandalf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Huntsmans Garb"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Alatar"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Into the Smoking Cone"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (msg "Which character represents the company")
                                                  :choices {:req #(and (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ironfoot"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Dáin II"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "King of Seven Houses"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on your Dwarf-lord"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "King under the Mountain"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a dwarf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Kinsman and Ambassador"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Círdan"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Knowledge of Ardas Treasures"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on your Dwarf-lord"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Knowledge of the Enemy"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card with a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Lady of Many Gifts"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Galadriel"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Lady of Many Songs"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Galadriel"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Lady of Many Visions"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Galadriel"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Last of the Seven"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Thráin"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Light of Lothlórien"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Galadriel"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Long-time Visitor"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Elrond"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Looking from Mind to Mind"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Lord of Dwarrowdelf"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a Dwarf-lord"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Lore of the Ages"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on an elf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Many-coloured Robes"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Saruman"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Master of Shapes"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Radagast"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Mithrandir"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (msg "Which character represents the company")
                                                  :choices {:req #(and (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Moon-runes"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Elrond"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "More Than Meets the Eye"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a Dwarf-lord"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Morinehtar"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (msg "Which character represents the company")
                                                  :choices {:req #(and (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "New Closeness to His Kin"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on an elf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Of the Race of Dale"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Bard Bowman, Bain, or Brand"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}

   "Open to the Summons"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on an agent"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Orders from Lugbúrz"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "Move to your hand to re-assign company")
                                                            (msg "Place this card with a company"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Oakenshield"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Thorin II"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Pallandos Apprentice"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Pallandos Hood"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Pallando"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Pocketed Robes"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Radagast"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Prince of Mirkwood"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Thranduil"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Promise of Treasure"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a non-avatar dwarf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Promptings of Wisdom"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a ranger"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Reforging"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on the sage"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Rescue Prisoners"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Return of the King"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Aragorn II"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ring of Air"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Elrond"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ring of Fire"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Gandalf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ring of Water"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Galadriel"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Rómestámo"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (msg "Which character represents the company")
                                                  :choices {:req #(and (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Royal Duo"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Galadriel"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Sacrifice of Form"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on your returned Wizard"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Saruman's Ring"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Saruman"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Shifter of Hues"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Radagast"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Son of Dáin"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Thorin III Stonehelm"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Son of Fundin"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Balin"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Son of Náin"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Dáin"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Son of Thráin"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Thorin"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Son of Thrór"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Thráin"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Squire of the Hunt"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Stave of Pallando"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Pallando"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Swordmaster"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Sworn to the Lord"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on your Lord"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Forge-master"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Grey Hat"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Gandalf"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Kings Hunting"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Thranduil"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Seas Prowess"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Círdan"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Three Are Together"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on one of the three"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Three Hunters"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on one of the three"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The White Wizard"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on your Wizard"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Windlord Found Me"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Thrall to the Voice"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on another character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (= (last (:zone %)) :characters)
                                                                         (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Three Golden Hairs"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "To Fealty Sworn"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a hobbit"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Towers Destroyed"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (msg "Which character represents the company")
                                                  :choices {:req #(and (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "True-hearted Man"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Boromir II"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Trusted Counsellor"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Value Food and Cheer"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Vein of Arda"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on the sage"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Very Active Governess"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Galadriel"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "War Preparations"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on the scout"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "When I Know Anything"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on the sage"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "When You Know More"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on the sage"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Winged Change-master"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on Radagast"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Wizards Myrmidon"
   {:abilities [{:label "Place"
                 :effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on a different character")
                                                            (msg "Place this card on a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}})