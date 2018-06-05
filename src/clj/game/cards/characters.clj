(in-ns 'game.core)

(declare trash-resource trash-hazard trash-muthereff-sub trash-installed)

(def cards-characters
  {"Adrazar"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Adûnaphel the Ringwraith"
   {}
   "Aegnor"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Akhôrahil the Ringwraith"
   {}
   "Alatar"
   {}
   "Anarin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Anborn"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Andovon Pathdal"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Annalena"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Aragorn II"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Aramacar"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Arhendhiril"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Arinmîr"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Arwen"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ascarnil"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Asternak"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Attack-Lord"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Azog"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Baduila"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Balin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bard Bowman"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Barliman Butterbur"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Belegorn"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Beorn"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Beregond"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Beretar"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bergil"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bifur"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bilbo"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bill Ferny"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bladorthin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bofur"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bolg"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bolg of the North"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bombur"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Borin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Boromir II"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Brand"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bróin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bûrat"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Bûthrakaur"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Calendal"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Carambor"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Celeborn"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Celeborn, Lord of Lórien"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Celebrían"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Celedhring"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Círdan"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Círdan the Shipwright"
   {}
   "Círdor"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ciryaher"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Crook-legged Orc"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dáin"
   {}
   "Dáin II"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Damrod"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dâsakûn"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Deallus"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Denethor II"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dís"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Doeth (Durthak)"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dôgrib"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dorelas"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dori"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Drór"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dunlending Spy"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dwalin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dwar the Ringwraith"
   {}
   "Dwarven Miner"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Dworin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Elerína"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Elladan"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ellindiel"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Elrohir"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Elrond"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Elrond Half-elven"
   {}
   "Elven Handmaid"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Elwen"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Emissary of the House"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Éomer"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Éowyn"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Eradan"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Erestor"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Erkenbrand"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Eun"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Euog (Ulzog)"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Fanar"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Faramir"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Fatty Bolger"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Fíli"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Firiel"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Flói"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Folco Boffin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Fori the Beardless"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Forlong"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Fram Framson"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Frár"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Frerin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Frodo"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Galadriel"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Galdor"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Galion"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Galva"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Gamling the Old"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Gandalf"
   {}
   "Gergeli"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ghán-buri-Ghán"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Gildor Inglorion"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Gimli"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Gimli, Son of Gloin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Gisulf"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Glóin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Glorfindel II"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Goblin-Basher"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Golodhros"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Gorbag"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Grishnákh"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Gulla"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Hador"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Halbarad"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Haldalam"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Haldir"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Háma"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Hendolen"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Herion"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Hill-troll"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Hoarmûrath the Ringwraith"
   {}
   "Horseman in the Night"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Huinen"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ill-favoured Fellow"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Imrahil"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Indûr the Ringwraith"
   {}
   "Ioreth"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ivic"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Jerrek"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Jûoma"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Khamûl the Ringwraith"
   {}
   "Kíli"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "King's Advisor"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "King's Judge"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Kori"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Lady Galadriel"
   {}
   "Lagduf"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Layos"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Leamon"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Legolas"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Legolas of Greenwood"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Lieutenant of Angmar"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Lieutenant of Dol Guldur"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Lieutenant of Morgul"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Lipór"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Lobelia Sackville-Baggins"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Lóni"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Lugdush"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Luitprand"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Mablung"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Mallorn-dweller"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Mauhûr"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Merry"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Míonid"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Mountain-maggot"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Mountaineer"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Muzgash"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "My Precious"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Náin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Náin, Son of Grór"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Náli"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Narin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Nevido Smôd"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Nimloth"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Nori"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Norin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Odoacer"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Óin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Old Troll"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ôm-buri-Ôm"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Orc Brawler"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Orc Captain"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Orc Chieftain"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Orc Sniffler"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Orc Tracker"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Orc Veteran"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ori"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Orophin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ostisen"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Othar"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Pallando"
   {}
   "Pathfinder"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Peath"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Perchen"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Pippin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Pon Opar"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Pôn-ora-Pôn"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Prisoner from Ost-in-Edhil"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Radagast"
   {}
   "Radbug"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Râisha"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ren the Ringwraith"
   {}
   "Robin Smallburrow"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Rúmil"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Sam Gamgee"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Saruman"
   {}
   "Shagrat"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Shámas"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Shipwright"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Sly Southerner"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Snaga"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Squint-eyed Brute"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Strider"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Súrion"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Taladhan"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Tarcil"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Tarin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Tharúdan"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "The Balrog"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "The Grimburgoth"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "The Mouth"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "The Witch-king"
   {}
   "Théoden"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Thorin"
   {}
   "Thorin II"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Thorin III Stonehelm"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Thráin"
   {}
   "Thráin II"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Thranduil"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Thranduil of Mirkwood"
   {}
   "Threlin"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Thulin II"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Troll Lout"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Troll-chief"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Tros Hesnef"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Tûma"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Tupór"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Uchel"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ufthak"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Uglúk"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ulkaur the Tongueless"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Umagaur"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Ûvatha the Ringwraith"
   {}
   "Vôteli"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Vygavril"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Wacho"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Woffung"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Wood-elf"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Wormtongue"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Wûluag"
   {:abilities [{:label "Move"
                 :prompt "Choose to follow" :choices (req served)
                 :msg (msg "follow " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}})