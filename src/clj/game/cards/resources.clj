(in-ns 'game.core)

(declare can-host?)

(def cards-resources
  {"A Flush of Air"
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   "Fellowship"
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   "Ironfoot"
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   "Moon-runes"
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   "New Closeness to His Kin"
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   "Royal Duo"
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   "True-hearted Man"
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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
   {:abilities [{:effect (req (let [r (get-card state card)
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