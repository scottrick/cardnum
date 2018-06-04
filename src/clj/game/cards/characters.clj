(in-ns 'game.core)

(declare trash-resource trash-hazard trash-muthereff-sub trash-installed)

(def cards-characters
  {"Lieutenant of Morgul"
   {:abilities [{:label "Move"
                 :prompt "Choose a place" :choices (req servers)
                 :msg (msg "move another party " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "Orc Tracker"
   {:abilities [{:label "Move"
                 :prompt "Choose a place" :choices (req servers)
                 :msg (msg "move another party " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}
   "The Grimburgoth"
   {:abilities [{:label "Move"
                 :prompt "Choose a place" :choices (req servers)
                 :msg (msg "move another party " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}})