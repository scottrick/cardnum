(in-ns 'game.core)

(declare trash-resource trash-hardware trash-muthereff-sub trash-installed)

(def cards-characters
  {"Orc Tracker"
   {:abilities [{:label "Move"
                 :prompt "Choose a place" :choices (req servers)
                 :msg (msg "move another party " target)
                 :effect (effect (move card (conj (server->zone state target) :characters)))}]}})