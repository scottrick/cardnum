(ns web.sites)

(def all-standard-sites
  [
   ;; REGIONS
   {:qty 3 :card "Andrast (TW)"}
   {:qty 3 :card "Andrast Coast (TW)"}
   {:qty 3 :card "Anduin Vales (TW)"}
   {:qty 3 :card "Anfalas (TW)"}
   {:qty 3 :card "Angmar (TW)"}
   {:qty 3 :card "Anórien (TW)"}
   {:qty 3 :card "Arthedain (TW)"}
   {:qty 3 :card "Bay of Belfalas (TW)"}
   {:qty 3 :card "Belfalas (TW)"}
   {:qty 3 :card "Brown Lands (TW)"}
   {:qty 3 :card "Cardolan (TW)"}
   {:qty 3 :card "Dagorlad (TW)"}
   {:qty 3 :card "Dorwinion (TW)"}
   {:qty 3 :card "Dunland (TW)"}
   {:qty 3 :card "Elven Shores (TW)"}
   {:qty 3 :card "Enedhwaith (TW)"}
   {:qty 3 :card "Eorstan (DF)"}
   {:qty 3 :card "Eriadoran Coast (TW)"}
   {:qty 3 :card "Fangorn (TW)"}
   {:qty 3 :card "Forochel (TW)"}
   {:qty 3 :card "Gap of Isen (TW)"}
   {:qty 3 :card "Gorgoroth (TW)"}
   {:qty 3 :card "Grey Mountain Narrows (TW)"}
   {:qty 3 :card "Gundabad (TW)"}
   {:qty 3 :card "Harondor (TW)"}
   {:qty 3 :card "Heart of Mirkwood (TW)"}
   {:qty 3 :card "High Pass (TW)"}
   {:qty 3 :card "Hollin (TW)"}
   {:qty 3 :card "Horse Plains (TW)"}
   {:qty 3 :card "Imlad Morgul (TW)"}
   {:qty 3 :card "Iron Hills (TW)"}
   {:qty 3 :card "Ithilien (TW)"}
   {:qty 3 :card "Khand (TW)"}
   {:qty 3 :card "Lamedon (TW)"}
   {:qty 3 :card "Lebennin (TW)"}
   {:qty 3 :card "Lindon (TW)"}
   {:qty 3 :card "Misty Mountains Northern Spur (DF)"}
   {:qty 3 :card "Misty Mountains Southern Spur (DF)"}
   {:qty 3 :card "Mouths of the Anduin (TW)"}
   {:qty 3 :card "Northern Rhovanion (TW)"}
   {:qty 3 :card "Númeriador (TW)"}
   {:qty 3 :card "Nurn (TW)"}
   {:qty 3 :card "Old Forest (FB)"}
   {:qty 3 :card "Old Pûkel Gap (TW)"}
   {:qty 3 :card "Old Pûkel-land (TW)"}
   {:qty 3 :card "Redhorn Gate (TW)"}
   {:qty 3 :card "Rhudaur (TW)"}
   {:qty 3 :card "Rohan (TW)"}
   {:qty 3 :card "Southern Mirkwood (TW)"}
   {:qty 3 :card "Southern Rhovanion (TW)"}
   {:qty 3 :card "Taur Rómen (FB)"}
   {:qty 3 :card "The Shire (TW)"}
   {:qty 3 :card "Udûn (TW)"}
   {:qty 3 :card "Western Mirkwood (TW)"}
   {:qty 3 :card "Withered Heath (TW)"}
   {:qty 3 :card "Wold & Foothills (TW)"}
   {:qty 3 :card "Woodland Realm (TW)"}

   ;; HERO SITES
   {:qty 1 :card "Amon Hen" :id "[H] (TW)"}
   {:qty 1 :card "Amon Lind" :id "[H] (FB)"}
   {:qty 1 :card "Bag End" :id "[H] (TW)"}
   {:qty 1 :card "Bandit Lair" :id "[H] (TW)"}
   {:qty 1 :card "Bar-en-Ibûn" :id "[M] (DF)"}
   {:qty 1 :card "Barad-dûr" :id "[H] (TW)"}
   {:qty 1 :card "Barak-shathûr" :id "[M] (DF)"}
   {:qty 1 :card "Barrow-downs" :id "[H] (TW)"}
   {:qty 1 :card "Beorn's House" :id "[H] (TW)"}
   {:qty 1 :card "Blue Mountain Dwarf-hold" :id "[H] (TW)"}
   {:qty 1 :card "Bree" :id "[H] (TW)"}
   {:qty 1 :card "Buhr Widu" :id "[H] (TD)"}
   {:qty 1 :card "Cameth Brin" :id "[H] (TW)"}
   {:qty 1 :card "Caras Amarth" :id "[H] (FB)"}
   {:qty 1 :card "Carn Dûm" :id "[H] (TW)"}
   {:qty 1 :card "Caves of Ûlund" :id "[H] (TW)"}
   {:qty 1 :card "Ceber Fanuin" :id "[H] (FB)"}
   {:qty 1 :card "Celebannon" :id "[H] (FB)"}
   {:qty 1 :card "Cerin Amroth" :id "[H] (FB)"}
   {:qty 1 :card "Cirith Gorgor" :id "[H] (AS)"}
   {:qty 1 :card "Cirith Ungol" :id "[H] (TW)"}
   {:qty 1 :card "Cor Angaladh" :id "[H] (FB)"}
   {:qty 1 :card "Dale" :id "[H] (TD)"}
   {:qty 1 :card "Dancing Spire" :id "[H] (TW)"}
   {:qty 1 :card "Dead Marshes" :id "[H] (TW)"}
   {:qty 1 :card "Dimrill Dale" :id "[H] (TW)"}
   {:qty 1 :card "Dol Amroth" :id "[H] (TW)"}
   {:qty 1 :card "Dol Guldur" :id "[H] (TW)"}
   {:qty 1 :card "Drúadan Forest" :id "[H] (TW)"}
   {:qty 1 :card "Dunharrow" :id "[H] (TW)"}
   {:qty 1 :card "Dunnish Clan-hold" :id "[H] (TW)"}
   {:qty 1 :card "Eagles' Eyrie" :id "[H] (TW)"}
   {:qty 1 :card "Easterling Camp" :id "[H] (TW)"}
   {:qty 4 :card "Edhellond" :id "[H] (TW)"}
   {:qty 1 :card "Edoras" :id "[H] (TW)"}
   {:qty 1 :card "Ettenmoors" :id "[H] (TW)"}
   {:qty 1 :card "Framsburg" :id "[H] (TD)"}
   {:qty 1 :card "Gaurblog Lug" :id "[M] (DF)"}
   {:qty 1 :card "Geann a-Lisch" :id "[H] (AS)"}
   {:qty 1 :card "Gladden Fields" :id "[H] (TW)"}
   {:qty 1 :card "Glittering Caves" :id "[H] (TW)"}
   {:qty 1 :card "Gobel Mírlond" :id "[H] (AS)"}
   {:qty 1 :card "Goblin-gate" :id "[H] (TW)"}
   {:qty 1 :card "Gold Hill" :id "[H] (TD)"}
   {:qty 1 :card "Gondmaeglom" :id "[H] (TD)"}
   {:qty 4 :card "Grey Havens" :id "[H] (TW)"}
   {:qty 1 :card "Haudh-in-Gwanûr" :id "[H] (DM)"}
   {:qty 1 :card "Henneth Annûn" :id "[H] (TW)"}
   {:qty 1 :card "Hermit's Hill" :id "[H] (DM)"}
   {:qty 1 :card "Himring" :id "[H] (TW)"}
   {:qty 1 :card "Irerock" :id "[H] (TW)"}
   {:qty 1 :card "Iron Hill Dwarf-hold" :id "[H] (TW)"}
   {:qty 1 :card "Isengard" :id "[H] (TW)"}
   {:qty 1 :card "Isle of the Ulond" :id "[H] (TD)"}
   {:qty 1 :card "Isles of the Dead that Live" :id "[H] (TW)"}
   {:qty 1 :card "Lake-town" :id "[H] (TW)"}
   {:qty 1 :card "Lond Galen" :id "[H] (TW)"}
   {:qty 1 :card "Long Marshes" :id "[H] (DF)"}
   {:qty 4 :card "Lórien" :id "[H] (TW)"}
   {:qty 1 :card "Lossadan Cairn" :id "[H] (TW)"}
   {:qty 1 :card "Lossadan Camp" :id "[H] (TW)"}
   {:qty 1 :card "Minas Morgul" :id "[H] (TW)"}
   {:qty 1 :card "Minas Tirith" :id "[H] (TW)"}
   {:qty 1 :card "Moria" :id "[H] (TW)"}
   {:qty 1 :card "Mount Doom" :id "[H] (TW)"}
   {:qty 1 :card "Mount Gram" :id "[H] (TW)"}
   {:qty 1 :card "Mount Gundabad" :id "[H] (TW)"}
   {:qty 1 :card "Mount Rerir" :id "[H] (FB)"}
   {:qty 1 :card "Nûrniag Camp" :id "[H] (AS)"}
   {:qty 1 :card "Nurunkhizdín" :id "[H] (DF)"}
   {:qty 1 :card "Old Forest" :id "[H] (TW)"}
   {:qty 1 :card "Ost-in-Edhil" :id "[H] (TW)"}
   {:qty 1 :card "Ovir Hollow" :id "[H] (TD)"}
   {:qty 1 :card "Pelargir" :id "[H] (TW)"}
   {:qty 1 :card "Raider-hold" :id "[H] (AS)"}
   {:qty 1 :card "Rhosgobel" :id "[H] (TW)"}
   {:qty 4 :card "Rhûbar" :id "[H] (FB)"}
   {:qty 4 :card "Rivendell" :id "[H] (TW)"}
   {:qty 1 :card "Ruined Signal Tower" :id "[H] (TW)"}
   {:qty 1 :card "Sarn Goriwing" :id "[H] (TW)"}
   {:qty 1 :card "Shelob's Lair" :id "[H] (TW)"}
   {:qty 1 :card "Shrel-Kain" :id "[H] (TW)"}
   {:qty 1 :card "Southron Oasis" :id "[H] (TW)"}
   {:qty 1 :card "Stone-circle" :id "[H] (TW)"}
   {:qty 1 :card "Telpëmar" :id "[H] (FB)"}
   {:qty 1 :card "Tharbad" :id "[H] (TD)"}
   {:qty 1 :card "The Carrock" :id "[H] (DF)"}
   {:qty 1 :card "The Gem-deeps" :id "[H] (DM)"}
   {:qty 1 :card "The Iron-deeps" :id "[H] (DM)"}
   {:qty 1 :card "The Lonely Mountain" :id "[H] (TW)"}
   {:qty 1 :card "The Pûkel-deeps" :id "[H] (DM)"}
   {:qty 1 :card "The Stones" :id "[H] (TW)"}
   {:qty 1 :card "The Sulfur-deeps" :id "[H] (DM)"}
   {:qty 1 :card "The Under-courts" :id "[H] (DM)"}
   {:qty 1 :card "The Under-galleries" :id "[H] (DM)"}
   {:qty 1 :card "The Under-gates" :id "[H] (DM)"}
   {:qty 1 :card "The Under-grottos" :id "[H] (DM)"}
   {:qty 1 :card "The Under-leas" :id "[H] (DM)"}
   {:qty 1 :card "The Under-vaults" :id "[H] (DM)"}
   {:qty 1 :card "The White Towers" :id "[H] (TW)"}
   {:qty 1 :card "The Wind Throne" :id "[H] (TW)"}
   {:qty 1 :card "The Worthy Hills" :id "[H] (AS)"}
   {:qty 1 :card "Thranduil's Halls" :id "[H] (TW)"}
   {:qty 1 :card "Tolfalas" :id "[H] (TW)"}
   {:qty 1 :card "Tom's House" :id "[H] (FB)"}
   {:qty 1 :card "Urlurtsu Nurn" :id "[H] (DM)"}
   {:qty 1 :card "Vale of Erech" :id "[H] (TW)"}
   {:qty 1 :card "Variag Camp" :id "[H] (TW)"}
   {:qty 1 :card "Weathertop" :id "[H] (TW)"}
   {:qty 1 :card "Wellinghall" :id "[H] (TW)"}
   {:qty 1 :card "Woodmen-town" :id "[H] (TW)"}
   {:qty 1 :card "Wose Passage-hold" :id "[H] (TW)"}
   {:qty 1 :card "Zarak Dûm" :id "[H] (TD)"}

   ;; MINION SITES
   {:qty 1 :card "Amon Hen" :id "[M] (LE)"}
   {:qty 1 :card "Amon Lind" :id "[M] (FB)"}
   {:qty 1 :card "Bag End" :id "[M] (LE)"}
   {:qty 1 :card "Bandit Lair" :id "[M] (LE)"}
   {:qty 1 :card "Bar-en-Ibûn" :id "[M] (DF)"}
   {:qty 1 :card "Barad-dûr" :id "[M] (LE)"}
   {:qty 1 :card "Barak-shathûr" :id "[M] (DF)"}
   {:qty 1 :card "Barrow-downs" :id "[M] (LE)"}
   {:qty 1 :card "Beorn's House" :id "[M] (LE)"}
   {:qty 1 :card "Blue Mountain Dwarf-hold" :id "[M] (LE)"}
   {:qty 1 :card "Bree" :id "[M] (LE)"}
   {:qty 1 :card "Buhr Widu" :id "[M] (LE)"}
   {:qty 1 :card "Cameth Brin" :id "[M] (LE)"}
   {:qty 1 :card "Caras Amarth" :id "[M] (FB)"}
   {:qty 4 :card "Carn Dûm" :id "[M] (LE)"}
   {:qty 1 :card "Caves of Ûlund" :id "[M] (LE)"}
   {:qty 1 :card "Ceber Fanuin" :id "[M] (FB)"}
   {:qty 1 :card "Celebannon" :id "[M] (FB)"}
   {:qty 1 :card "Cerin Amroth" :id "[M] (FB)"}
   {:qty 1 :card "Cirith Gorgor" :id "[M] (LE)"}
   {:qty 1 :card "Cirith Ungol" :id "[M] (LE)"}
   {:qty 1 :card "Cor Angaladh" :id "[M] (FB)"}
   {:qty 1 :card "Dale" :id "[M] (LE)"}
   {:qty 1 :card "Dancing Spire" :id "[M] (AS)"}
   {:qty 1 :card "Dead Marshes" :id "[M] (LE)"}
   {:qty 1 :card "Dimrill Dale" :id "[M] (LE)"}
   {:qty 1 :card "Dol Amroth" :id "[M] (LE)"}
   {:qty 4 :card "Dol Guldur" :id "[M] (LE)"}
   {:qty 1 :card "Drúadan Forest" :id "[M] (LE)"}
   {:qty 1 :card "Dunharrow" :id "[M] (LE)"}
   {:qty 1 :card "Dunnish Clan-hold" :id "[M] (LE)"}
   {:qty 1 :card "Eagles' Eyrie" :id "[M] (AS)"}
   {:qty 1 :card "Easterling Camp" :id "[M] (LE)"}
   {:qty 1 :card "Edhellond" :id "[M] (AS)"}
   {:qty 1 :card "Edoras" :id "[M] (LE)"}
   {:qty 1 :card "Ettenmoors" :id "[M] (LE)"}
   {:qty 1 :card "Framsburg" :id "[M] (AS)"}
   {:qty 1 :card "Gaurblog Lug" :id "[M] (DF)"}
   {:qty 4 :card "Geann a-Lisch" :id "[M] (LE)"}
   {:qty 1 :card "Gladden Fields" :id "[M] (LE)"}
   {:qty 1 :card "Glittering Caves" :id "[M] (LE)"}
   {:qty 1 :card "Gobel Mírlond" :id "[M] (LE)"}
   {:qty 1 :card "Goblin-gate" :id "[M] (LE)"}
   {:qty 1 :card "Gold Hill" :id "[M] (AS)"}
   {:qty 1 :card "Gondmaeglom" :id "[M] (LE)"}
   {:qty 1 :card "Grey Havens" :id "[M] (AS)"}
   {:qty 1 :card "Haudh-in-Gwanûr" :id "[M] (LE)"}
   {:qty 1 :card "Henneth Annûn" :id "[M] (LE)"}
   {:qty 1 :card "Hermit's Hill" :id "[M] (LE)"}
   {:qty 1 :card "Himring" :id "[M] (AS)"}
   {:qty 1 :card "Irerock" :id "[M] (AS)"}
   {:qty 1 :card "Iron Hill Dwarf-hold" :id "[M] (LE)"}
   {:qty 1 :card "Isengard" :id "[M] (LE)"}
   {:qty 1 :card "Isle of the Ulond" :id "[M] (AS)"}
   {:qty 1 :card "Isles of the Dead That Live" :id "[M] (AS)"}
   {:qty 1 :card "Lake-town" :id "[M] (LE)"}
   {:qty 1 :card "Lond Galen" :id "[M] (LE)"}
   {:qty 1 :card "Long Marshes" :id "[M] (DF)"}
   {:qty 1 :card "Lórien" :id "[M] (AS)"}
   {:qty 1 :card "Lossadan Cairn" :id "[M] (LE)"}
   {:qty 1 :card "Lossadan Camp" :id "[M] (LE)"}
   {:qty 4 :card "Minas Morgul" :id "[M] (LE)"}
   {:qty 1 :card "Minas Tirith" :id "[M] (LE)"}
   {:qty 1 :card "Moria" :id "[M] (LE)"}
   {:qty 1 :card "Mount Doom" :id "[M] (LE)"}
   {:qty 1 :card "Mount Gram" :id "[M] (LE)"}
   {:qty 1 :card "Mount Gundabad" :id "[M] (LE)"}
   {:qty 1 :card "Mount Rerir" :id "[M] (FB)"}
   {:qty 1 :card "Nûrniag Camp" :id "[M] (LE)"}
   {:qty 1 :card "Nurunkhizdín" :id "[M] (DF)"}
   {:qty 1 :card "Old Forest" :id "[M] (AS)"}
   {:qty 1 :card "Ost-in-Edhil" :id "[M] (LE)"}
   {:qty 1 :card "Ovir Hollow" :id "[M] (AS)"}
   {:qty 1 :card "Pelargir" :id "[M] (LE)"}
   {:qty 1 :card "Raider-hold" :id "[M] (LE)"}
   {:qty 1 :card "Rhosgobel" :id "[M] (AS)"}
   {:qty 1 :card "Rhûbar" :id "[M] (FB)"}
   {:qty 1 :card "Rivendell" :id "[M] (AS)"}
   {:qty 1 :card "Ruined Signal Tower" :id "[M] (LE)"}
   {:qty 1 :card "Sarn Goriwing" :id "[M] (LE)"}
   {:qty 1 :card "Shelob's Lair" :id "[M] (LE)"}
   {:qty 1 :card "Shrel-Kain" :id "[M] (LE)"}
   {:qty 1 :card "Southron Oasis" :id "[M] (LE)"}
   {:qty 1 :card "Stone-circle" :id "[M] (LE)"}
   {:qty 1 :card "Telpëmar" :id "[M] (FB)"}
   {:qty 1 :card "Tharbad" :id "[M] (LE)"}
   {:qty 1 :card "The Carrock" :id "[M] (DF)"}
   {:qty 1 :card "The Gem-deeps" :id "[M] (AS)"}
   {:qty 1 :card "The Iron-deeps" :id "[M] (AS)"}
   {:qty 1 :card "The Lonely Mountain" :id "[M] (LE)"}
   {:qty 1 :card "The Pûkel-deeps" :id "[M] (AS)"}
   {:qty 1 :card "The Stones" :id "[M] (LE)"}
   {:qty 1 :card "The Sulfur-deeps" :id "[M] (AS)"}
   {:qty 1 :card "The Under-courts" :id "[M] (AS)"}
   {:qty 1 :card "The Under-galleries" :id "[M] (AS)"}
   {:qty 1 :card "The Under-gates" :id "[M] (AS)"}
   {:qty 1 :card "The Under-grottos" :id "[M] (AS)"}
   {:qty 1 :card "The Under-leas" :id "[M] (AS)"}
   {:qty 1 :card "The Under-vaults" :id "[M] (AS)"}
   {:qty 1 :card "The White Towers" :id "[M] (LE)"}
   {:qty 1 :card "The Wind Throne" :id "[M] (LE)"}
   {:qty 1 :card "The Worthy Hills" :id "[M] (LE)"}
   {:qty 1 :card "Thranduil's Halls" :id "[M] (LE)"}
   {:qty 1 :card "Tolfalas" :id "[M] (AS)"}
   {:qty 1 :card "Tom's House" :id "[M] (FB)"}
   {:qty 1 :card "Urlurtsu Nurn" :id "[M] (LE)"}
   {:qty 1 :card "Vale of Erech" :id "[M] (LE)"}
   {:qty 1 :card "Variag Camp" :id "[M] (LE)"}
   {:qty 1 :card "Weathertop" :id "[M] (AS)"}
   {:qty 1 :card "Wellinghall" :id "[M] (AS)"}
   {:qty 1 :card "Woodmen-town" :id "[M] (LE)"}
   {:qty 1 :card "Wose Passage-hold" :id "[M] (LE)"}
   {:qty 1 :card "Zarak Dûm" :id "[M] (LE)"}

   ;; BALROG SITES
   {:qty 1 :card "Ancient Deep-hold" :id "[B] (BA)"}
   {:qty 1 :card "Barad-dûr" :id "[B] (BA)"}
   {:qty 1 :card "Carn Dûm" :id "[B] (BA)"}
   {:qty 1 :card "Cirith Gorgor" :id "[B] (BA)"}
   {:qty 1 :card "Cirith Ungol" :id "[B] (BA)"}
   {:qty 1 :card "Dol Guldur" :id "[B] (BA)"}
   {:qty 1 :card "Minas Morgul" :id "[B] (BA)"}
   {:qty 4 :card "Moria" :id "[B] (BA)"}
   {:qty 1 :card "Remains of Thangorodrim" :id "[B] (BA)"}
   {:qty 1 :card "The Drowning-deeps" :id "[B] (BA)"}
   {:qty 1 :card "The Gem-deeps" :id "[B] (BA)"}
   {:qty 1 :card "The Iron-deeps" :id "[B] (BA)"}
   {:qty 1 :card "The Pûkel-deeps" :id "[B] (BA)"}
   {:qty 1 :card "The Rusted-deeps" :id "[B] (BA)"}
   {:qty 1 :card "The Sulfur-deeps" :id "[B] (BA)"}
   {:qty 1 :card "The Under-courts" :id "[B] (BA)"}
   {:qty 1 :card "The Under-galleries" :id "[B] (BA)"}
   {:qty 4 :card "The Under-gates" :id "[B] (BA)"}
   {:qty 1 :card "The Under-grottos" :id "[B] (BA)"}
   {:qty 1 :card "The Under-leas" :id "[B] (BA)"}
   {:qty 1 :card "The Under-vaults" :id "[B] (BA)"}
   {:qty 1 :card "The Wind-deeps" :id "[B] (BA)"}

   ;; FALLEN SITES
   {:qty 4 :card "Isengard" :id "[F] (WH)"}
   {:qty 4 :card "Rhosgobel" :id "[F] (WH)"}
   {:qty 4 :card "The White Towers" :id "[F] (WH)"}

   ;; ELF SITES
   {:qty 4 :card "Grey Havens" :id "[E] (FB)"}
   {:qty 4 :card "Heart of the Halls (FB)"}
   {:qty 4 :card "Lórien" :id "[E] (FB)"}
   {:qty 4 :card "Rivendell" :id "[E] (FB)"}
   {:qty 4 :card "Thranduil's Halls" :id "[E] (FB)"}

   ;; DWARF SITES
   {:qty 4 :card "Blue Mountain Dwarf-hold" :id "[D] (DF)"}
   {:qty 4 :card "Iron Hill Dwarf-hold" :id "[D] (DF)"}

   ;; LORD SITES
   {:qty 1 :card "Edhellond" :id "[F] (FB)"}
   {:qty 1 :card "Grey Havens" :id "[F] (FB)"}
   {:qty 1 :card "Lórien" :id "[F] (FB)"}
   {:qty 1 :card "Rivendell" :id "[F] (FB)"}
   {:qty 1 :card "Thranduil's Halls" :id "[F] (FB)"}

   ;; DUAL SITES
   {:qty 1 :card "Durin's Tower" :id "(DF)"}
   {:qty 1 :card "Mines of Angûrath" :id "(DF)"}
   {:qty 1 :card "Mines of Falek-Dim" :id "(DF)"}
   {:qty 1 :card "Osteledan" :id "(FB)"}
   {:qty 1 :card "Ruins of Belegost" :id "(DF)"}
   {:qty 1 :card "Ruins of Nogrod" :id "(DF)"}
   {:qty 1 :card "The Drowning-Deeps" :id "[D] (DF)"}
   {:qty 1 :card "The Rusted-Deeps" :id "[D] (DF)"}
   {:qty 1 :card "The Wind-Deeps" :id "[D] (DF)"}
   ])