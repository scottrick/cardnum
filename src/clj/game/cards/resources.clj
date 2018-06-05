(in-ns 'game.core)

(declare can-host?)

(def cards-resources
  {"New Closeness to His Kin"
   {:abilities [{:effect (req (let [r (get-card state card)
                                    hosted? (character? (:host r))]
                                (resolve-ability state side
                                                 {:prompt (if hosted?
                                                            (msg "You may not play this on another Elf")
                                                            (msg "Place this card with an Elf"))
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
                                                            (msg "You may not play this on another agent")
                                                            (msg "Place this card with an agent"))
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
                                                            (msg "Place this card with a character"))
                                                  :choices {:req #(if (not hosted?)
                                                                    (and (= (last (:zone %)) :characters)
                                                                         (character? %)
                                                                         (can-host? %)))}
                                                  :msg (msg "host it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}})