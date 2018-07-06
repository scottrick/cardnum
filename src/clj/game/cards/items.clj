(in-ns 'game.core)

(declare can-host?)

(def cards-items
  {"A Little Gold Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Adamant Helmet"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Aiglos"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ancient Black Axe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Aranrûth"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Arrows Shorn of Ebony"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Athelas"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Battle-axe of the Fallen"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Beautiful Gold Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Bilbo's Magic Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Binding-ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Black Arrow"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Black Mace"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Black-hide Shield"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Black-mail Coat"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Blasting Fire"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Blazon of the Eye"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Book of Mazarbul"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Bow of Dragon-horn"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Bow of the Galadhrim"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Bow of Yew"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Bright Gold Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Broad-headed Spear"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Cloak of Many Colours"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Cram"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Crisfuin"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dagger of Westernesse"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Deadly Dart"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dragon-helm"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Durang"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Durin's Axe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Durin's Crown"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarf-chopper"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Axe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Chain-Shirt"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Fire"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Light-stone"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Ring of Barin's Tribe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Ring of Bávor's Tribe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Ring of Drúin's Tribe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Ring of Dwálin's Tribe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Ring of Thélor's Tribe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Dwarven Ring of Thrár's Tribe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Earth of Galadriel's Orchard"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Elenya"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Elf-stone"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Elven Cloak"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Elven Rope"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Emerald of Doriath"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Emerald of the Mariner"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Enruned Shield"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ent-draughts"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Essay on Ringcraft"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Fair Gold Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Forgotten Scrolls"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Foul-smelling Paste"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Fungi"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Gems of Arda"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Glamdring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Gleaming Gold Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Goblin Earth-plumb"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Gold Belt of Lórien"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Gold Ring that Sauron Fancies"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Great Bow of Yew"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Great-shield of Rohan"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Habergeon of Silver"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Hauberk of Bright Mail"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Healing Herbs"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Helm of Fear"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "High Helm"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Horn of Anor"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Iron Shield of Old"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Jewel of Beleriand"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Keys of Orthanc"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Keys to the White Towers"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Khazadshathûr"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Leaf Brooch"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Lesser Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Liquid Fire"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Lost Tome"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Courage"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Delusion"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Enigma"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Fury"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Guile"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Lies"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Lore"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Nature"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Savagery"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Shadows"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Stealth"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Weals"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magic Ring of Words"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Magical Harp"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Maranya"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Mechanical Bow"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Miner's Pick"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Minor Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Miruvor"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Mithril"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Narsil"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Necklace of Girion"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Necklace of Silver and Pearls"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Noldo-lantern"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Old Treasure"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Orc-draughts"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Orc-liquor"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Orcrist"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Palantír of Amon Sûl"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Palantír of Annúminas"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Palantír of Elostirion"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Palantír of Minas Tirith"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Palantír of Orthanc"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Palantír of Osgiliath"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Pale Enchanted Gold"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Paltry Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Perfect Gold Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Phial of Galadriel"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Pipe"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Pocket Handkerchief"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Poison"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Potion of Prowess"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Precious Gold Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Records Unread"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Red Arrow"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Red Book of Westmarch"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ring of Adamant"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ring of Ruby"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ring of Sapphire"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Sable Shield"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Sapling of the White Tree"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Saw-toothed Blade"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Scabbard of Chalcedony"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Scroll of Isildur"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Secret Book"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Shadow-cloak"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Shield of Iron-bound Ash"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Stabbing Tongue of Fire"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Star-glass"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Sting"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Strange Rations"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Sulhelka"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Sword of Gondolin"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Arkenstone"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Iron Crown"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Ithil-stone"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Least of Gold Rings"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Mithril-coat"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The One Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Oracle's Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Reviled Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "The Warding Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Thong of Fire"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Thrall-ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Thror's Map"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Thrór's Map"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Torque of Hues"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Trifling Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Troth-ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Twice-baked Cakes"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Ungolcrist"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Usriev of Treachery"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Valiant Sword"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Vile Fumes"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "War Mattock"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Waybread"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Whip"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {
                                                  :prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}

   "Whip of Many Thongs"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Wizard's Ring"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Wizard's Staff"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}
   "Wormsbane"
   {:hosting {:req #(and (character? %) (revealed? %))}
    :abilities [{:label "Transfer"
                 :effect (req (let [r (get-card state card)
                                    old-host (:host r)]
                                (resolve-ability state side
                                                 {:prompt (msg "Transfer " (:title card) " to a different character")
                                                  :choices {:req #(and (not (= (:cid %) (:cid old-host)))
                                                                       (= (last (:zone %)) :characters)
                                                                       (character? %)
                                                                       (can-host? %))}
                                                  :msg (msg "place it on " (card-str state target))
                                                  :effect (effect (host target card))} card nil)))}]}})