(ns meccg.sites
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [clojure.string :refer [split]]))

(def wizard-sites)

(def minion-sites)

(def balrog-sites)

(def fallen-sites)

(def elf-sites)

(def dwarf-sites)

(def all-regions2
  (str "3 Eorstan (DF)\n3 Andrast (TW)\n3 Misty Mountains Southern Spur (DF)\n3 Redhorn Gate (TW)\n3 Bay of Belfalas (TW)\n3 Horse Plains (TW)\n3 Wold & Foothills (TW)\n3 Western Mirkwood (TW)\n3 Woodland Realm (TW)\n3 Udûn (TW)\n3 Lamedon (TW)\n3 The Shire (TW)\n3 Iron Hills (TW)\n3 Mouths of the Anduin (TW)\n3 Heart of Mirkwood (TW)\n3 Andrast Coast (TW)\n3 Rhudaur (TW)\n3 Taur Rómen (FB)\n3 Dunland (TW)\n3 Withered Heath (TW)\n3 Misty Mountains Northern Spur (DF)\n3 Old Pûkel Gap (TW)\n3 Old Forest (FB)\n3 Dagorlad (TW)\n3 High Pass (TW)\n3 Anfalas (TW)\n3 Dorwinion (TW)\n3 Southern Rhovanion (TW)\n3 Angmar (TW)\n3 Gap of Isen (TW)\n3 Forochel (TW)\n3 Anórien (TW)\n3 Hollin (TW)\n3 Lindon (TW)\n3 Gorgoroth (TW)\n3 Rohan (TW)\n3 Númeriador (TW)\n3 Nurn (TW)\n3 Ithilien (TW)\n3 Anduin Vales (TW)\n3 Khand (TW)\n3 Harondor (TW)\n3 Fangorn (TW)\n3 Gundabad (TW)\n3 Lebennin (TW)\n3 Northern Rhovanion (TW)\n3 Arthedain (TW)\n3 Brown Lands (TW)\n3 Old Pûkel-land (TW)\n3 Grey Mountain Narrows (TW)\n3 Southern Mirkwood (TW)\n3 Enedhwaith (TW)\n3 Imlad Morgul (TW)\n3 Elven Shores (TW)\n3 Belfalas (TW)\n3 Cardolan (TW)\n3 Eriadoran Coast (TW)\n"))

(def all-regions
  (str "3 Eorstan (DF)
       3 Andrast (TW)
       3 Misty Mountains Southern Spur (DF)"))

(def all-regions4
  (str "3 Eorstan (DF)\n3 Andrast (TW)\n3 Misty Mountains Southern Spur (DF)"
       ))

(def all-regions3
  (str "3 Eorstan (DF)
       3 Andrast (TW)
       3 Misty Mountains Southern Spur (DF)
       3 Redhorn Gate (TW)
       3 Bay of Belfalas (TW)
       3 Horse Plains (TW)
       3 Wold & Foothills (TW)
       3 Western Mirkwood (TW)
       3 Woodland Realm (TW)
       3 Udûn (TW)
       3 Lamedon (TW)
       3 The Shire (TW)
       3 Iron Hills (TW)
       3 Mouths of the Anduin (TW)
       3 Heart of Mirkwood (TW)
       3 Andrast Coast (TW)
       3 Rhudaur (TW)
       3 Taur Rómen (FB)
       3 Dunland (TW)
       3 Withered Heath (TW)
       3 Misty Mountains Northern Spur (DF)
       3 Old Pûkel Gap (TW)
       3 Old Forest (FB)
       3 Dagorlad (TW)
       3 High Pass (TW)
       3 Anfalas (TW)
       3 Dorwinion (TW)
       3 Southern Rhovanion (TW)
       3 Angmar (TW)
       3 Gap of Isen (TW)
       3 Forochel (TW)
       3 Anórien (TW)
       3 Hollin (TW)
       3 Lindon (TW)
       3 Gorgoroth (TW)
       3 Rohan (TW)
       3 Númeriador (TW)
       3 Nurn (TW)
       3 Ithilien (TW)
       3 Anduin Vales (TW)
       3 Khand (TW)
       3 Harondor (TW)
       3 Fangorn (TW)
       3 Gundabad (TW)
       3 Lebennin (TW)
       3 Northern Rhovanion (TW)
       3 Arthedain (TW)
       3 Brown Lands (TW)
       3 Old Pûkel-land (TW)
       3 Grey Mountain Narrows (TW)
       3 Southern Mirkwood (TW)
       3 Enedhwaith (TW)
       3 Imlad Morgul (TW)
       3 Elven Shores (TW)
       3 Belfalas (TW)
       3 Cardolan (TW)
       3 Eriadoran Coast (TW)"
       ))