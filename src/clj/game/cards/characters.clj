(in-ns 'game.core)

(declare discard-resource discard-hazard discard-muthereff-sub discard-placed)

(def cards-characters
  {
   "Abur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Adrazar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Aegnor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Agog"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Airatano"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Aknazeh"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Anarin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Anborn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Andovon Pathdal"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Angamaitë"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Angbor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Annalena"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Aragorn II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Aramacar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Aransiros the Sublime"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Araphor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Arathorn II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Araudagul"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ardagor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ardana"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ardaron"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Arduin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Arduval"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Argirion"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Arhendhiril"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Arinmîr"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Arthrazoc"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Arwen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Aryen the Beastmaster"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ascarnil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ashmaar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Asternak"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Attack-Lord"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Azog"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Baduila"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bahadur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bain"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Balin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bard Bowman"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Barliman Butterbur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Baugúr"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Belegorn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Beorn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Beregond"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Beretar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bereth"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bergil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bertok"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bharâm"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bifur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bilbo"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bill Ferny"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bladorthin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bodyguard"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bofur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bolg"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bolg of the North"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bombur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Borin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Boromir II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Boron"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Brand"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Brandir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bróin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Brutal Retinue"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bulrakur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bûrat"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Burdîlgoth"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bûthrakaur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Caerlinc"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Calendal"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Cambragol"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Camring"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Carambor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Carlon"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Celeborn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Celeborn, Lord of Lórien"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Celebrían"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Celedhring"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ceorl"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Círdan"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Círdor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ciryaher"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Crook-legged Orc"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Curubor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dáin II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dâiruzôr"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dakalmog"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Damrod"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dár"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Daroc"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dâsakûn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Daurukh"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Deallus"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Denethor II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Derei"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Derufin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dervorin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dís"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Djerul"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Doeth (Durthak)"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dôgrib"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dolin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dom"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dorelas"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dori"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dragon's Disciple"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Drór"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Duilin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Duinhir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dunadan Mariner"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dúnhere"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dunlending Spy"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Durba"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Durlog"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dwalin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dwarven Miner"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dworin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Eärmacil II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Elendor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Elerína"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Elfhelm"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Elite Wose Hunter"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Elladan"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ellindiel"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Elrohir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Elrond"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Elven Handmaid"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Elwen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Emissary of the House"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Eodoric"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Éomer"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Éothain"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Éowyn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Eradan"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Erestor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Erkenbrand"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Eun"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Euog (Ulzog)"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Fanar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Faramir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Fatty Bolger"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Fëatur1"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Featur2"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Fíli"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Firiel"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Flói"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Folco Boffin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Forest-troll"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Fori the Beardless"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Forlong"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Fram Framson"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Frár"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Freca"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Frerin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Frodo"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Frôr V"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gabmog the Dimwitted"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Galadriel"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Galdor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Galgrinic"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Galion"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Galva"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gamling the Old"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Garulf"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gaurhir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gergeli"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ghán-buri-Ghán"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gildor Inglorion"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gilraen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gimli"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gimli, Son of Gloin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gisulf"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Glóin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Glorfindel II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Glorin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Goblin-Basher"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Goblin-miner"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Golasgil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Golodhros"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gorbag"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gorovod"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gorshûk"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gorthaur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Grachev Hos"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Grashûkh"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Grim-faced Northman"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Grimbeorn the Old"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Grimbold of Westfold"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Grimson the Fearless"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Grishnákh"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gulla"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Gûrthlug"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Guthláf"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hador"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Haeldwyn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Háin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Halbarad"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Halbarad of the North"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Haldalam"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Haldir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Háma"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hargrog"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Harngorin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Heladil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Heledwen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hembur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hendolen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Herion"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hill-troll"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hirgon"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hirluin the Fair"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hord"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Horseman in the Night"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Huinen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hungh"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Húrin the Tall"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Hurog"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ice-troll"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ill-favoured Fellow"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Imrahil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Imrazôr III"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ingold"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ioreth"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ivic"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Jaeru"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Jamir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Jehn Remak"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Jerrek"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Jûoma"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Kabadir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Karaag the Gnome"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Karhunkäsi"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Karvainen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Kavatha"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Kénwe Foryaren"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Khandash"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Khelekar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Khursh"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Kíli"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "King's Advisor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "King's Judge"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Klaen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Konihrabn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Kori"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Kragbogk the Berserker"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lagduf"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lardin Aril"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Laurrë Menelrana"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Layos"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Leamon"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Leärdionoth"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Legolas"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Legolas of Greenwood"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lieutenant of Angmar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lieutenant of Dol Guldur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lieutenant of Morgul"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Líndal"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Linsul"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lipór"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lobelia Sackville-Baggins"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lomëlindë"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lóni"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Los'pindel"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lothíriel"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lugdush"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lugronk"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Luitprand"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Mablung"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Malezar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Mallorn-dweller"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Manari Akaji"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Maran"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Marin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Mauhûr"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Mélorak"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Merry"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Midhir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Milo Burrows"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Míonid"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Miruimor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Moran"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Morelen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Morfuin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Mornaur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Morthaur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Morvran"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Morwen of Lossarnach"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Mountain-maggot"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Mountaineer"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Muzgash"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "My Precious"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nadash"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Náin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Náin, Son of Grór"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Náli"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Narin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nazog"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nestador"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nevido Smôd"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nîlûphel"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nimloth"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nob"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nori"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Norin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nuluzir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Odoacer"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ognor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Óin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Old Troll"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ologong"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ôm-buri-Ôm"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ôn-Eno"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ôn-Ikana"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc Archer"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc Brawler"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc Captain"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc Chieftain"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc Grunt"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc of the Claw"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc Shaman"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc Sniffler"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc Tracker"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orc Veteran"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ori"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orophin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Orthir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ostisen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Othar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Palandor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Pathfinder"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Peath"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Perchen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Peshtin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Pharacas"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Phorakôn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Pippin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Pitää Kalasta"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Pon Opar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Pôn-ora-Pôn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Prisoner from Ost-in-Edhil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Psousèn the Valorous"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Punakäsi"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Radbug"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Râisha"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Rána"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ranger of Arnor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Raudabern"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Relin II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Rilia"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Robin Smallburrow"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Róin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Rúmil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sakalthôr"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sakalure"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sakulbâr II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sam Gamgee"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sancho Proudfoot"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sangahyando"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sarkarxë"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Savak"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Seregul"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Seyran"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Shagrat"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Shámas"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Shipwright"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Silion"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sirnaur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Slûcrac"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sly Southerner"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Snaga"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sprautabern"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Squint-eyed Brute"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Strider"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Striuk'ir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Suldun"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sûlherok"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Súrion"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Swarthy Sneering Fellow"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tabaya Kas"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Taladhan"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Talmog"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tarcil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tarin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tartas Izain"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tarvaran"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Taurclax"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Taurion"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Taurnil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ted Sandyman"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Terilaen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tharúdan"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "The Angulion"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "The Grimburgoth"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "The Master"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "The Mouth"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Théoden"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Théodred"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Thorin II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Thorin III Stonehelm"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Thorongil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Thráin II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Thranduil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Threlin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Thulin II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tirial"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Troll Henchman"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Troll Lout"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Troll-chief"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tros Hesnef"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tûma"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tupór"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Tûranar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Uchel"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ufkral"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ufthak"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Uglúk"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ukôg the Lame"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ulkaur the Tongueless"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ullis"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ulrac"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ulred"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ulrith"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ulvun the Owlkeeper"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Umagaur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Unor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Urdak"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ûrêkhâd"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ûrezîr"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Vaal Gark"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Valandor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Valglin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Valkrist"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Vallin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Valmorgûl"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Valnaur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Valsûl"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Vanha"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Varadir"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Virsh"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Vishtâspa"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Vôteli"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Vygavril"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Wacho"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Waulfa"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Whil Whitfoot"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Wídfara"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Woedwyn"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Woffung"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Wolf"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Wood-elf"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Wormtongue"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Wûluag"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Wyrmslayer"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Yanos Kosvar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Yavëkamba"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Zohkad"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Zurtak"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   })