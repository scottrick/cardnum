(in-ns 'game.core)

(def cards-allies
  {"A More or Less Decent Giant"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Bill the Pony"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Black Horse"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Blackbole"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Cave Troll"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Creature of an Older World"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Evil Things Lingering"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Goldberry"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Gollum"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Great Bats"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Great Lord of Goblin-gate"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Great Troll"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Gwaihir"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Kheleglin"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Last Child of Ungoliant"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Leaflock"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Lindion the Oronín"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Mistress Lobelia"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Nasty Slimy Thing"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Nenseldë the Wingild"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Noble Hound"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Noble Steed"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Quickbeam"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Radagast's Black Bird"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Regiment of Black Crows"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Roäc the Raven"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Shadowfax"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Skinbark"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Stinker"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "The Balrog"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "The Warg-king"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Tom Bombadil"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "Treebeard"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "War-warg"
   {:hosting {:req #(and (character? %) (rezzed? %))}}
   "War-wolf"
   {:hosting {:req #(and (character? %) (rezzed? %))}}})