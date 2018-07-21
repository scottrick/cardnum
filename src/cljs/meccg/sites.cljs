(ns meccg.sites
  (:require [clojure.string :refer [split]]))

(def all-wizard-sites
  (str "
    1 Amon Hen [H] (TW)
    1 Amon Lind [H] (FB)
    1 Bag End [H] (TW)
    1 Bandit Lair [H] (TW)
    1 Bar-en-Ibûn [M] (DF)
    1 Barad-dûr [H] (TW)
    1 Barak-shathûr [M] (DF)
    1 Barrow-downs [H] (TW)
    1 Beorn's House [H] (TW)
    1 Blue Mountain Dwarf-hold [H] (TW)
    1 Bree [H] (TW)
    1 Buhr Widu [H] (TD)
    1 Cameth Brin [H] (TW)
    1 Caras Amarth [H] (FB)
    1 Carn Dûm [H] (TW)
    1 Caves of Ûlund [H] (TW)
    1 Ceber Fanuin [H] (FB)
    1 Celebannon [H] (FB)
    1 Cerin Amroth [H] (FB)
    1 Cirith Gorgor [H] (AS)
    1 Cirith Ungol [H] (TW)
    1 Cor Angaladh [H] (FB)
    1 Dale [H] (TD)
    1 Dancing Spire [H] (TW)
    1 Dead Marshes [H] (TW)
    1 Dimrill Dale [H] (TW)
    1 Dol Amroth [H] (TW)
    1 Dol Guldur [H] (TW)
    1 Drúadan Forest [H] (TW)
    1 Dunharrow [H] (TW)
    1 Dunnish Clan-hold [H] (TW)
    1 Eagles' Eyrie [H] (TW)
    1 Easterling Camp [H] (TW)
    4 Edhellond [H] (TW)
    1 Edoras [H] (TW)
    1 Ettenmoors [H] (TW)
    1 Framsburg [H] (TD)
    1 Gaurblog Lug [M] (DF)
    1 Geann a-Lisch [H] (AS)
    1 Gladden Fields [H] (TW)
    1 Glittering Caves [H] (TW)
    1 Gobel Mírlond [H] (AS)
    1 Goblin-gate [H] (TW)
    1 Gold Hill [H] (TD)
    1 Gondmaeglom [H] (TD)
    4 Grey Havens [H] (TW)
    1 Haudh-in-Gwanûr [H] (DM)
    1 Henneth Annûn [H] (TW)
    1 Hermit's Hill [H] (DM)
    1 Himring [H] (TW)
    1 Irerock [H] (TW)
    1 Iron Hill Dwarf-hold [H] (TW)
    1 Isengard [H] (TW)
    1 Isle of the Ulond [H] (TD)
    1 Isles of the Dead that Live [H] (TW)
    1 Lake-town [H] (TW)
    1 Lond Galen [H] (TW)
    1 Long Marshes [H] (DF)
    4 Lórien [H] (TW)
    1 Lossadan Cairn [H] (TW)
    1 Lossadan Camp [H] (TW)
    1 Minas Morgul [H] (TW)
    1 Minas Tirith [H] (TW)
    1 Moria [H] (TW)
    1 Mount Doom [H] (TW)
    1 Mount Gram [H] (TW)
    1 Mount Gundabad [H] (TW)
    1 Mount Rerir [H] (FB)
    1 Nûrniag Camp [H] (AS)
    1 Nurunkhizdín [H] (DF)
    1 Old Forest [H] (TW)
    1 Ost-in-Edhil [H] (TW)
    1 Ovir Hollow [H] (TD)
    1 Pelargir [H] (TW)
    1 Raider-hold [H] (AS)
    1 Rhosgobel [H] (TW)
    4 Rhûbar [H] (FB)
    4 Rivendell [H] (TW)
    1 Ruined Signal Tower [H] (TW)
    1 Sarn Goriwing [H] (TW)
    1 Shelob's Lair [H] (TW)
    1 Shrel-Kain [H] (TW)
    1 Southron Oasis [H] (TW)
    1 Stone-circle [H] (TW)
    1 Telpëmar [H] (FB)
    1 Tharbad [H] (TD)
    1 The Carrock [H] (DF)
    1 The Gem-deeps [H] (DM)
    1 The Iron-deeps [H] (DM)
    1 The Lonely Mountain [H] (TW)
    1 The Pûkel-deeps [H] (DM)
    1 The Stones [H] (TW)
    1 The Sulfur-deeps [H] (DM)
    1 The Under-courts [H] (DM)
    1 The Under-galleries [H] (DM)
    1 The Under-gates [H] (DM)
    1 The Under-grottos [H] (DM)
    1 The Under-leas [H] (DM)
    1 The Under-vaults [H] (DM)
    1 The White Towers [H] (TW)
    1 The Wind Throne [H] (TW)
    1 The Worthy Hills [H] (AS)
    1 Thranduil's Halls [H] (TW)
    1 Tolfalas [H] (TW)
    1 Tom's House [H] (FB)
    1 Urlurtsu Nurn [H] (DM)
    1 Vale of Erech [H] (TW)
    1 Variag Camp [H] (TW)
    1 Weathertop [H] (TW)
    1 Wellinghall [H] (TW)
    1 Woodmen-town [H] (TW)
    1 Wose Passage-hold [H] (TW)
    1 Zarak Dûm [H] (TD)
    "))

(def all-minion-sites
  (str "
    1 Amon Hen [M] (LE)
    1 Amon Lind [M] (FB)
    1 Bag End [M] (LE)
    1 Bandit Lair [M] (LE)
    1 Bar-en-Ibûn [M] (DF)
    1 Barad-dûr [M] (LE)
    1 Barak-shathûr [M] (DF)
    1 Barrow-downs [M] (LE)
    1 Beorn's House [M] (LE)
    1 Blue Mountain Dwarf-hold [M] (LE)
    1 Bree [M] (LE)
    1 Buhr Widu [M] (LE)
    1 Cameth Brin [M] (LE)
    1 Caras Amarth [M] (FB)
    4 Carn Dûm [M] (LE)
    1 Caves of Ûlund [M] (LE)
    1 Ceber Fanuin [M] (FB)
    1 Celebannon [M] (FB)
    1 Cerin Amroth [M] (FB)
    1 Cirith Gorgor [M] (LE)
    1 Cirith Ungol [M] (LE)
    1 Cor Angaladh [M] (FB)
    1 Dale [M] (LE)
    1 Dancing Spire [M] (AS)
    1 Dead Marshes [M] (LE)
    1 Dimrill Dale [M] (LE)
    1 Dol Amroth [M] (LE)
    4 Dol Guldur [M] (LE)
    1 Drúadan Forest [M] (LE)
    1 Dunharrow [M] (LE)
    1 Dunnish Clan-hold [M] (LE)
    1 Eagles' Eyrie [M] (AS)
    1 Easterling Camp [M] (LE)
    1 Edhellond [M] (AS)
    1 Edoras [M] (LE)
    1 Ettenmoors [M] (LE)
    1 Framsburg [M] (AS)
    1 Gaurblog Lug [M] (DF)
    4 Geann a-Lisch [M] (LE)
    1 Gladden Fields [M] (LE)
    1 Glittering Caves [M] (LE)
    1 Gobel Mírlond [M] (LE)
    1 Goblin-gate [M] (LE)
    1 Gold Hill [M] (AS)
    1 Gondmaeglom [M] (LE)
    1 Grey Havens [M] (AS)
    1 Haudh-in-Gwanûr [M] (LE)
    1 Henneth Annûn [M] (LE)
    1 Hermit's Hill [M] (LE)
    1 Himring [M] (AS)
    1 Irerock [M] (AS)
    1 Iron Hill Dwarf-hold [M] (LE)
    1 Isengard [M] (LE)
    1 Isle of the Ulond [M] (AS)
    1 Isles of the Dead That Live [M] (AS)
    1 Lake-town [M] (LE)
    1 Lond Galen [M] (LE)
    1 Long Marshes [M] (DF)
    1 Lórien [M] (AS)
    1 Lossadan Cairn [M] (LE)
    1 Lossadan Camp [M] (LE)
    4 Minas Morgul [M] (LE)
    1 Minas Tirith [M] (LE)
    1 Moria [M] (LE)
    1 Mount Doom [M] (LE)
    1 Mount Gram [M] (LE)
    1 Mount Gundabad [M] (LE)
    1 Mount Rerir [M] (FB)
    1 Nûrniag Camp [M] (LE)
    1 Nurunkhizdín [M] (DF)
    1 Old Forest [M] (AS)
    1 Ost-in-Edhil [M] (LE)
    1 Ovir Hollow [M] (AS)
    1 Pelargir [M] (LE)
    1 Raider-hold [M] (LE)
    1 Rhosgobel [M] (AS)
    1 Rhûbar [M] (FB)
    1 Rivendell [M] (AS)
    1 Ruined Signal Tower [M] (LE)
    1 Sarn Goriwing [M] (LE)
    1 Shelob's Lair [M] (LE)
    1 Shrel-Kain [M] (LE)
    1 Southron Oasis [M] (LE)
    1 Stone-circle [M] (LE)
    1 Telpëmar [M] (FB)
    1 Tharbad [M] (LE)
    1 The Carrock [M] (DF)
    1 The Gem-deeps [M] (AS)
    1 The Iron-deeps [M] (AS)
    1 The Lonely Mountain [M] (LE)
    1 The Pûkel-deeps [M] (AS)
    1 The Stones [M] (LE)
    1 The Sulfur-deeps [M] (AS)
    1 The Under-courts [M] (AS)
    1 The Under-galleries [M] (AS)
    1 The Under-gates [M] (AS)
    1 The Under-grottos [M] (AS)
    1 The Under-leas [M] (AS)
    1 The Under-vaults [M] (AS)
    1 The White Towers [M] (LE)
    1 The Wind Throne [M] (LE)
    1 The Worthy Hills [M] (LE)
    1 Thranduil's Halls [M] (LE)
    1 Tolfalas [M] (AS)
    1 Tom's House [M] (FB)
    1 Urlurtsu Nurn [M] (LE)
    1 Vale of Erech [M] (LE)
    1 Variag Camp [M] (LE)
    1 Weathertop [M] (AS)
    1 Wellinghall [M] (AS)
    1 Woodmen-town [M] (LE)
    1 Wose Passage-hold [M] (LE)
    1 Zarak Dûm [M] (LE)
    "))

(def all-balrog-sites
  (str "
    1 Ancient Deep-hold [B] (BA)
    1 Barad-dûr [B] (BA)
    1 Carn Dûm [B] (BA)
    1 Cirith Gorgor [B] (BA)
    1 Cirith Ungol [B] (BA)
    1 Dol Guldur [B] (BA)
    1 Minas Morgul [B] (BA)
    4 Moria [B] (BA)
    1 Remains of Thangorodrim [B] (BA)
    1 The Drowning-deeps [B] (BA)
    1 The Gem-deeps [B] (BA)
    1 The Iron-deeps [B] (BA)
    1 The Pûkel-deeps [B] (BA)
    1 The Rusted-deeps [B] (BA)
    1 The Sulfur-deeps [B] (BA)
    1 The Under-courts [B] (BA)
    1 The Under-galleries [B] (BA)
    4 The Under-gates [B] (BA)
    1 The Under-grottos [B] (BA)
    1 The Under-leas [B] (BA)
    1 The Under-vaults [B] (BA)
    1 The Wind-deeps [B] (BA)
    "))

(def all-fallen-sites
  (str "
    4 Isengard [F] (WH)
    4 Rhosgobel [F] (WH)
    4 The White Towers [F] (WH)
    "))

(def all-elf-sites
  (str "
    4 Grey Havens [E] (FB)
    4 Heart of the Halls (FB)
    4 Lórien [E] (FB)
    4 Rivendell [E] (FB)
    4 Thranduil's Halls [E] (FB)
    "))

(def all-dwarf-sites
  (str "
    4 Blue Mountain Dwarf-hold [D] (DF)
    4 Iron Hill Dwarf-hold [D] (DF)
    "))

(def all-lord-sites
  (str "
    1 Edhellond [F] (FB)
    1 Grey Havens [F] (FB)
    1 Lórien [F] (FB)
    1 Rivendell [F] (FB)
    1 Thranduil's Halls [F] (FB)
    "))

(def all-dual-sites
  (str "
    1 Osteledan
    1 Durin's Tower
    1 Mines of Angûrath
    1 Mines of Falek-Dim
    1 Ruins of Belegost
    1 Ruins of Nogrod
    1 The Drowning-Deeps
    1 The Rusted-Deeps
    1 The Wind-Deeps
    "))

(def all-regions
  (str "
    3 Andrast (TW)
    3 Andrast Coast (TW)
    3 Anduin Vales (TW)
    3 Anfalas (TW)
    3 Angmar (TW)
    3 Anórien (TW)
    3 Arthedain (TW)
    3 Bay of Belfalas (TW)
    3 Belfalas (TW)
    3 Brown Lands (TW)
    3 Cardolan (TW)
    3 Dagorlad (TW)
    3 Dorwinion (TW)
    3 Dunland (TW)
    3 Elven Shores (TW)
    3 Enedhwaith (TW)
    3 Eorstan (DF)
    3 Eriadoran Coast (TW)
    3 Fangorn (TW)
    3 Forochel (TW)
    3 Gap of Isen (TW)
    3 Gorgoroth (TW)
    3 Grey Mountain Narrows (TW)
    3 Gundabad (TW)
    3 Harondor (TW)
    3 Heart of Mirkwood (TW)
    3 High Pass (TW)
    3 Hollin (TW)
    3 Horse Plains (TW)
    3 Imlad Morgul (TW)
    3 Iron Hills (TW)
    3 Ithilien (TW)
    3 Khand (TW)
    3 Lamedon (TW)
    3 Lebennin (TW)
    3 Lindon (TW)
    3 Misty Mountains Northern Spur (DF)
    3 Misty Mountains Southern Spur (DF)
    3 Mouths of the Anduin (TW)
    3 Northern Rhovanion (TW)
    3 Númeriador (TW)
    3 Nurn (TW)
    3 Old Forest (FB)
    3 Old Pûkel Gap (TW)
    3 Old Pûkel-land (TW)
    3 Redhorn Gate (TW)
    3 Rhudaur (TW)
    3 Rohan (TW)
    3 Southern Mirkwood (TW)
    3 Southern Rhovanion (TW)
    3 Taur Rómen (FB)
    3 The Shire (TW)
    3 Udûn (TW)
    3 Western Mirkwood (TW)
    3 Withered Heath (TW)
    3 Wold & Foothills (TW)
    3 Woodland Realm (TW)
    "))