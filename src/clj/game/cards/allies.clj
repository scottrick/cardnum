(ns game.cards.allies
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int]]
            [cardnum.cards :refer [all-cards]]))

(def card-definitions
  {"\"Two-headed\" Troll"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "A More or Less Decent Giant"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Bill the Pony"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Black Horse"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Blackbole"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Cave Troll"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Creature of an Older World"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Evil Things Lingering"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Goldberry"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Gollum"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Great Bats"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Great Lord of Goblin-gate"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Great Troll"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Gwaihir"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Kheleglin"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Last Child of Ungoliant"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Leaflock"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Lindion the Oronín"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Mistress Lobelia"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Nasty Slimy Thing"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Nenseldë the Wingild"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Noble Hound"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Noble Steed"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Quickbeam"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Radagast's Black Bird"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Regiment of Black Crows"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Roäc the Raven"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Shadowfax"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Skinbark"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Stinker"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "The Balrog"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "The Warg-king"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Tom Bombadil"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "Treebeard"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "War-warg"
   {:hosting {:req #(and (character? %) (revealed? %))}}
   "War-wolf"
   {:hosting {:req #(and (character? %) (revealed? %))}}})