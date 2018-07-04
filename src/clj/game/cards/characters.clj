(in-ns 'game.core)

(declare discard-resource discard-hazard discard-muthereff-sub discard-placed)

(def cards-characters
  {"Adrazar"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Adûnaphel the Ringwraith"
   {}
   "Aegnor"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Akhôrahil the Ringwraith"
   {}
   "Alatar"
   {}
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
   "Arwen"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ascarnil"
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
   "Bergil"
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
   "Brand"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bróin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bûrat"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Bûthrakaur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Calendal"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Carambor"
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
   "Círdan"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Círdan the Shipwright"
   {}
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
   "Dáin"
   {}
   "Dáin II"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Damrod"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dâsakûn"
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
   "Dís"
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
   "Drór"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dunlending Spy"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dwalin"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Dwar the Ringwraith"
   {}
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
   "Elerína"
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
   "Elrond Half-elven"
   {}
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
   "Éomer"
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
   "Gandalf"
   {}
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
   "Goblin-Basher"
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
   "Hador"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Halbarad"
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
   "Hoarmûrath the Ringwraith"
   {}
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
   "Indûr the Ringwraith"
   {}
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
   "Khamûl the Ringwraith"
   {}
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
   "Kori"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lady Galadriel"
   {}
   "Lagduf"
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
   "Lóni"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Lugdush"
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
   "Mallorn-dweller"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Mauhûr"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Merry"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Míonid"
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
   "Nevido Smôd"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Nimloth"
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
   "Odoacer"
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
   "Ôm-buri-Ôm"
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
   "Pallando"
   {}
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
   "Pippin"
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
   "Radagast"
   {}
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
   "Ren the Ringwraith"
   {}
   "Robin Smallburrow"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Rúmil"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Sam Gamgee"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Saruman"
   {}
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
   "Súrion"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Taladhan"
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
   "Tharúdan"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "The Balrog"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "The Grimburgoth"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "The Mouth"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "The Witch-king"
   {}
   "Théoden"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Thorin"
   {}
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
   "Thráin"
   {}
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
   "Thranduil of Mirkwood"
   {}
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
   "Uchel"
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
   "Ulkaur the Tongueless"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Umagaur"
   {:abilities [{:label "Organize"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}
   "Ûvatha the Ringwraith"
   {}
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
   "Woffung"
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
                 :effect (effect (move card (conj (locale->zone state target) :characters)))}]}})