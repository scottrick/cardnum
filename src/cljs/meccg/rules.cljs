(ns meccg.rules
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [meccg.ajax :refer [GET]]))

(def app-state (atom {}))

(go (swap! app-state assoc :donators (:json (<! (GET "/data/donors")))))

(defn rules [cursor owner]
  (om/component
    (sab/html
      [:div.rules.panel.content-page.blue-shade
       [:h3 "Official Rules"]
       [:p "Rule Books & Rule Inserts for Expansions"]

       [:li [:a {:href "https://www.dropbox.com/s/4r97cj65ur2b5wl/1_Book_METW.pdf?dl=0" :target "_blank"} "METW Rulebook"]]
       [:li [:a {:href "https://www.dropbox.com/s/m1rok7xu5ej9qm9/2_Insert_METD.pdf?dl=0" :target "_blank"} "METD Insert"]]
       [:li [:a {:href "https://www.dropbox.com/s/p7ym8y4f8xuvse8/3_Insert_MEDM.pdf?dl=0" :target "_blank"} "MEDM Insert"]]
       [:li [:a {:href "https://www.dropbox.com/s/jjsjlbx30tybtsf/4_Book_MELE.pdf?dl=0" :target "_blank"} "MELE Rulebook"]]
       [:li [:a {:href "https://www.dropbox.com/s/898jqtcsiurwiqr/5_Insert_MEAS.pdf?dl=0" :target "_blank"} "MEAS Insert"]]
       [:li [:a {:href "https://www.dropbox.com/s/j5o9znu8iifb9mo/6_Insert_MEWH.pdf?dl=0" :target "_blank"} "MEWH Insert"]]
       [:li [:a {:href "https://www.dropbox.com/s/g07zysl1vui2y6h/7_Insert_MEBA.pdf?dl=0" :target "_blank"} "MEBA Insert"]]

       [:p "CRF and CoE issued Errata (currently thru ARV)"]

       [:li [:a {:href "https://www.dropbox.com/s/ibv5wn2nr0fi5zl/8_Terms.pdf?dl=0" :target "_blank"} "Terms"]]
       [:li [:a {:href "https://www.dropbox.com/s/l6bnzs95zka6tuy/9_Errata.pdf?dl=0" :target "_blank"} "Errata"]]
       [:li [:a {:href "https://councilofelrond.org/forum/viewtopic.php?f=103&t=3541" :target "_blank"} "ARV"]]

       [:p "Official Digests, Rulings & Clarifications"]

       [:li [:a {:href "https://www.dropbox.com/s/u2tb71hehuj6mq2/A_1_Digests_%28Ichabod%29.pdf?dl=0" :target "_blank"} "Digests (Ichabod)"]]
       [:li [:a {:href "https://www.dropbox.com/s/i1rtieg7vvz0cwj/A_2_Digests_%28Gnome%29.pdf?dl=0" :target "_blank"} "Digests (Gnome)"]]
       [:li [:a {:href "https://www.dropbox.com/s/ucgnvclhb1qc4kk/A_3_Digests_%28Van%29.pdf?dl=0" :target "_blank"} "Digests (Van)"]]
       [:li [:a {:href "https://www.dropbox.com/s/drk0jpopjddmwkl/B_CoE.pdf?dl=0" :target "_blank"} "CoE Rulings"]]

       [:p "Tournament Rules"]

       [:li [:a {:href "https://www.dropbox.com/s/zg1kpih5uvubh3x/C_Tournament.pdf?dl=0" :target "_blank"} "CoL Policy"]]

       [:p "Universal Rules Document (rules compendium)"]

       [:li [:a {:href "https://www.dropbox.com/s/6vvxvrlodjziw9u/D_URD_4_2.pdf?dl=0" :target "_blank"} "URD 4.2"]]

       [:p "Dreamcard \"Current\" Offical Rules"]

       [:li [:a {:href "https://www.dropbox.com/s/evfukzcc0vyk45u/E_DC_General_Rules_1_0.pdf?dl=0" :target "_blank"} "General Rules 1.0"]]
       [:li [:a {:href "https://www.dropbox.com/s/s1j2orouk25mjwh/F_DC_Lord-player_Rules_1_0.pdf?dl=0" :target "_blank"} "Lord-player Rules 1.0"]]

       [:p]
       [:p]
       [:p]
       [:h3 "Disclaimer"]
       [:p "Middle-earth:CCG and LotR are trademarks of Middle-earth Enterprises and/or Iron Crown Enterprises."]
       [:p "This website is not affiliated with Middle-earth Enterprises or Iron Crown Enterprises."]
       ])))

(om/root rules app-state {:target (. js/document (getElementById "rules"))})
