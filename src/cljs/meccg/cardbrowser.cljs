(ns meccg.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! >! sub pub] :as async]
            [clojure.string :as str]
            [meccg.appstate :refer [app-state]]
            [meccg.ajax :refer [GET]]
            [meccg.utils :refer [toastr-options banned-span restricted-span rotated-span influence-dots]]
            [cardnum.cards :refer [all-cards] :as cards]
            [cardnum.decks :as decks]))

(def cards-channel (chan))
(def pub-chan (chan))
(def notif-chan (pub pub-chan :topic))

(go (let [local-cards (js->clj (.parse js/JSON (.getItem js/localStorage "cards")) :keywordize-keys true)
          need-update? (not local-cards)
          cards (sort-by :code (:json (<! (GET "/data/cards"))))
          sets (:json (<! (GET "/data/sets")))
          mwl (:json (<! (GET "/data/mwl")))]
      (reset! cards/mwl mwl)
      (reset! cards/sets sets)
      (swap! app-state assoc :sets sets)
      (reset! all-cards cards)
      (swap! app-state assoc :cards-loaded true)
      (put! cards-channel cards)))

(defn make-span [text symbol class]
  (.replace text (apply str symbol) (str "<img src='" class "'style=\"width:16px;height:16px;\"></img>")))

(defn image-url
  ([card] (image-url card false))
  ([card allow-all-users]
   (str "/img/cards/" (:set_code card) "/" (:ImageName card))))

(defn add-symbols [card-text]
  (-> (if (nil? card-text) "" card-text)
      (make-span "Automatic-attacks" "img/dc/me_aa.png")
      (make-span "Automatic-attack" "img/dc/me_aa.png")
      (make-span "automatic-attacks" "img/dc/me_aa.png")
      (make-span "automatic-attack" "img/dc/me_aa.png")
      (make-span "Border-holds [B]" "img/dc/me_bh.png")
      (make-span "Border-lands [b]" "img/dc/me_bl.png")
      (make-span "Border-hold [B]" "img/dc/me_bh.png")
      (make-span "Border-land [b]" "img/dc/me_bl.png")
      (make-span "company vs. company combat" "img/dc/me_ccc.png")
      (make-span "CvCC" "img/dc/me_ccc.png")
      (make-span "corruption checks" "img/dc/me_cp.png")
      (make-span "corruption check" "img/dc/me_cp.png")
      (make-span "CC" "img/dc/me_cp.png")
      (make-span "corruption point (CP)" "img/dc/me_cp.png")
      (make-span "CP" "img/dc/me_cp.png")
      (make-span "corruption points" "img/dc/me_cp.png")
      (make-span "corruption point" "img/dc/me_cp.png")
      (make-span "Coastal Seas [ccc]" "img/dc/me_tc.png")
      (make-span "Coastal Seas [cc]" "img/dc/me_dc.png")
      (make-span "Coastal Seas [c]" "img/dc/me_cs.png")
      (make-span "Coastal Sea [c]" "img/dc/me_cs.png")
      (make-span "Dark-domains [d]" "img/dc/me_dd.png")
      (make-span "Dark-holds [D]" "img/dc/me_dh.png")
      (make-span "Dark-domain [d]" "img/dc/me_dd.png")
      (make-span "Dark-hold [D]" "img/dc/me_dh.png")
      (make-span "Darkhavens [V]" "img/dc/me_dha.png")
      (make-span "Darkhaven [V]" "img/dc/me_dha.png")
      (make-span "Darkhavens [K]" "img/dc/me_dha.png")
      (make-span "Darkhaven [K]" "img/dc/me_dha.png")
      (make-span "Darkhaven" "img/dc/me_dha.png")
      (make-span "Darkhaven" "img/dc/me_dha.png")
      (make-span "Deserts [ee]" "img/dc/me_ee.png")
      (make-span "Deserts" "img/dc/me_ee.png")
      (make-span "Desert [e]" "img/dc/me_er.png")
      (make-span "Desert" "img/dc/me_er.png")
      (make-span "Direct influence" "img/dc/me_di.png")
      (make-span "direct influence" "img/dc/me_di.png")
      (make-span "DI" "img/dc/me_di.png")
      (make-span "Free-domains [f]" "img/dc/me_fd.png")
      (make-span "Free-holds [F]" "img/dc/me_fh.png")
      (make-span "Free-domain [f]" "img/dc/me_fd.png")
      (make-span "Free-hold [F]" "img/dc/me_fh.png")
      (make-span "General influence" "img/dc/me_gi.png")
      (make-span "general influence" "img/dc/me_gi.png")
      (make-span "GI" "img/dc/me_gi.png")
      (make-span "Havens [H]" "img/dc/me_ha.png")
      (make-span "Haven [H]" "img/dc/me_ha.png")
      (make-span "Jungles [j]" "img/dc/me_ju.png")
      (make-span "Jungle [j]" "img/dc/me_ju.png")
      (make-span " MP." "img/dc/me_mp.png")
      (make-span " MP " "img/dc/me_mp.png")
      (make-span " mp." "img/dc/me_mp.png")
      (make-span " mp " "img/dc/me_mp.png")
      (make-span "marshalling points" "img/dc/me_mp.png")
      (make-span "marshalling point" "img/dc/me_mp.png")
      (make-span "Ruins & Lairs [R]" "img/dc/me_rl.png")
      (make-span "Shadow-holds [S]" "img/dc/me_sh.png")
      (make-span "Shadow-lands [s]" "img/dc/me_sl.png")
      (make-span "Shadow-hold [S]" "img/dc/me_sh.png")
      (make-span "Shadow-land [s]" "img/dc/me_sl.png")
      (make-span "SPs" "img/dc/me_sp.png")
      (make-span "SP" "img/dc/me_sp.png")
      (make-span "stage points" "img/dc/me_sp.png")
      (make-span "stage point" "img/dc/me_sp.png")
      (make-span "tap:" "img/dc/me_tap.png")
      (make-span "Tap " "img/dc/me_tap.png")
      (make-span " tapping" "img/dc/me_tap.png")
      (make-span " tap " "img/dc/me_tap.png")
      (make-span " tap." "img/dc/me_tap.png")
      (make-span "Wildernesses [w]" "img/dc/me_wi.png")
      (make-span "Wilderness [w]" "img/dc/me_wi.png")
      (make-span "Wildernesses [ww]" "img/dc/me_dw.png")
      (make-span "Wilderness [ww]" "img/dc/me_dw.png")
      (make-span "Wildernesses [www]" "img/dc/me_tw.png")
      (make-span "Wilderness [www]" "img/dc/me_tw.png")
      (make-span "[ccc]" "img/dc/me_tc.png")
      (make-span "[cc]" "img/dc/me_dc.png")
      (make-span "[ee]" "img/dc/me_ee.png")
      (make-span "[www]" "img/dc/me_tw.png")
      (make-span "[ww]" "img/dc/me_dw.png")
      (make-span "[b]" "img/dc/me_bl.png")
      (make-span "[c]" "img/dc/me_cs.png")
      (make-span "[d]" "img/dc/me_dd.png")
      (make-span "[e]" "img/dc/me_er.png")
      (make-span "[f]" "img/dc/me_fd.png")
      (make-span "[j]" "img/dc/me_ju.png")
      (make-span "[s]" "img/dc/me_sl.png")
      (make-span "[w]" "img/dc/me_wi.png")
      (make-span "[B]" "img/dc/me_bh.png")
      (make-span "[D]" "img/dc/me_dh.png")
      (make-span "[V]" "img/dc/me_dha.png")
      (make-span "[K]" "img/dc/me_dha.png")
      (make-span "[F]" "img/dc/me_fh.png")
      (make-span "[H]" "img/dc/me_ha.png")
      (make-span "[R]" "img/dc/me_rl.png")
      (make-span "[S]" "img/dc/me_sh.png")))

(defn non-game-toast
  "Display a toast warning with the specified message."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr type)]
    (f msg)))

(defn- card-text
  "Generate text html representation a card"
  [card cursor]
  [:div
   [:h4 (:full_set card) ": " (:title card)]
   [:div.text
    [:p [:span.type (str (:type card))] (if (= (.toLowerCase (:type card)) (:Secondary card))
                                          ""
                                          (str ": " (:Secondary card)))]
    [:pre {:dangerouslySetInnerHTML #js {:__html
                                         (loop [new-text (:text card)]
                                           (if (= new-text (add-symbols new-text))
                                             new-text
                                             (recur (add-symbols new-text))))}}]]])

(defn card-view [card owner]
  (reify
    om/IInitState
    (init-state [_] {:showText false})
    om/IRenderState
    (render-state [_ state]
      (let [cursor (om/get-state owner :cursor)]
        (sab/html
          [:div.card-preview.blue-shade
           (when (om/get-state owner :decorate-card)
             {:class (cond (:selected card) "selected")})
           (if (:showText state)
             (card-text card cursor)
             (when-let [url (image-url card true)]
               [:img {:src url
                      :alt (:title card)
                      :onClick #(do (.preventDefault %)
                                    (put! (:pub-chan (om/get-shared owner))
                                          {:topic :card-selected :data card})
                                    nil)
                      :onError #(-> (om/set-state! owner {:showText true}))
                      :onLoad #(-> % .-target js/$ .show)}]))])))))

(defn card-info-view [cursor owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (sab/html
        (let [selected-card (om/get-state owner :selected-card)]
          (if (nil? selected-card)
            [:div {:display "none"}]
            [:div
             [:h4 "Card text"]
             [:div.blue-shade.panel
              (card-text selected-card cursor)]]))))))

(def primary-order ["Character" "Resource" "Hazard" "Site" "Region"])
(def resource-secondaries ["Ally" "Faction" "Greater Item" "Major Item" "Minor Item" "Gold Ring Item" "Special Item"])
(def site-secondaries ["Greater Item" "Major Item" "Minor Item" "Gold Ring Item" "Information" "Hoard" "Scroll" "Palantír" "Battle-gear" "Non-battle-gear"])
(def shared-secondaries ["Permanent-event" "Short-event" "Long-event" "Permanent-event/Short-event" "Permanent-event/Long-event" "Short-event/Long-event"])
(def hazard-secondaries ["Creature" "Creature/Permanent-event" "Creature/Short-event" "Creature/Long-event"])
(def general-alignments ["Hero" "Minion" "Balrog" "Fallen-wizard" "Fallen/Lord" "Lord"
                         "Elf-lord" "Dwarf-lord" "Atani-lord" "War-lord" "Dragon-lord"
                         "Grey" "Dual"])
(def standard-havens ["Carn Dûm" "Dol Guldur" "Edhellond" "Geann a-Lisch"
                      "Grey Havens" "Lórien" "Minas Morgul" "Rivendell"])
(def standard-hero-havens ["Edhellond" "Grey Havens" "Lórien" "Rivendell"])
(def standard-minion-havens ["Carn Dûm" "Dol Guldur" "Geann a-Lisch" "Minas Morgul"])
(def dreamcard-havens ["Amaru" "Bozisha-Dar" "Carn Dûm" "Chey Goumal" "Dol Guldur"
                       "Edhellond" "Elanthia" "Evermist" "Geann a-Lisch" "Grey Havens"
                       "Hau Nysrin" "Inyalonî" "Lórien" "Minas Morgul" "Mornost" "Rhûbar"
                       "Rivendell" "Shapôl Udûn" "Taurondë" "Tower of Hargrog" "Valagalen"])
(def dreamcard-hero-havens ["Edhellond" "Elanthia" "Evermist" "Grey Havens" "Hau Nysrin"
                           "Inyalonî" "Lórien" "Rhûbar" "Rivendell" "Taurondë" "Valagalen"])
(def dreamcard-minion-havens ["Amaru" "Bozisha-Dar" "Carn Dûm" "Chey Goumal" "Dol Guldur"
                              "Geann a-Lisch" "Minas Morgul" "Mornost" "Shapôl Udûn" "Tower of Hargrog"])
(def set-order ["The Wizards" "The Dragons" "Dark Minions" "The Lidless Eye" "Against the Shadow" "The White Hand" "The Balrog"
                "Firstborn" "Durin's Folk" "The Necromancer" "Bay of Ormal" "Court of Ardor" "The Great Central Plains" "The Dominion of the Seven"
                "The Great Wyrms" "Kingdom of the North" "Morgoth's Legacy" "Nine Rings for Mortal Men" "The Northern Waste" "Red Nightfall"
                "Return of the Shadow" "The Sun Lands" "Treason of Isengard" "War of the Ring"])
(def race-options ["Wizard" "Dúnadan" "Hobbit" "Man" "Wose" "Umit"
                   "Dwarf" "Firebeard Dwarf" "Ironfist Dwarf" "Longbeard Dwarf"
                   "Elf" "Nando Elf" "Noldo Elf" "Silvan Elf" "Sinda Elf"
                   "Ringwraith" "Olog-hai Troll" "Troll" "Half-troll" "Uruk-hai Orc" "Half-orc" "Orc"
                   "Balrog" "Demon" "Dragon"])
(def fact-options ["Dúnadan" "Hobbit" "Dwarf" "Elf" "Man" "Wose" "Umit"
                   "Mercenary" "Slayer" "Slave" "Undead"
                   "Giant" "Troll" "Orc" "Balrog" "Demon" "Drake" "Dragon"
                   "Ent" "Animal" "Eagle" "Wolf" "Spider" "Special" "Other"])
(def skill-options ["Diplomat" "Warrior" "Ranger" "Scout" "Sage"])

(defn secondaries [primary]
  (case primary
    "All" (concat hazard-secondaries shared-secondaries resource-secondaries ["site"] ["region"])
    "Character" ["character" "Avatar" "Leader" "Agent"]
    "Resource" (concat resource-secondaries shared-secondaries)
    "Hazard" (concat hazard-secondaries shared-secondaries)
    "Site" site-secondaries
    "Region" ["region"]))

(defn alignments [primary]
  (case primary
    "All" (concat general-alignments ["Neutral"])
    "Character" (concat general-alignments ["Neutral"])
    "Resource" general-alignments
    "Hazard" ["Neutral"]
    "Site" general-alignments
    "Region" ["Neutral"]))

(defn options [list]
  (let [options (cons "All" list)]
    (for [option options]
      [:option {:value option :dangerouslySetInnerHTML #js {:__html option}}])))

(defn filter-cards [filter-value field cards]
  (if (= filter-value "All")
    cards
    (filter #(= (field %) filter-value) cards)))

(defn filter-sites [strict cards]
  (case strict
    "All"
    cards
    "Greater Item"
    (filter-cards true :GreaterItem cards)
    "Major Item"
    (filter-cards true :MajorItem cards)
    "Minor Item"
    (filter-cards true :MinorItem cards)
    "Gold Ring Item"
    (filter-cards true :GoldRing cards)
    "Information"
    (filter-cards true :Information cards)
    "Hoard"
    (filter-cards true :Hoard cards)
    "Scroll"
    (filter-cards true :Scroll cards)
    "Palantír"
    (filter-cards true :Palantiri cards)
    "Battle-gear"
    (filter-cards true :Gear cards)
    "Non-battle-gear"
    (filter-cards true :Non cards)
    ))

(defn filter-second [site-filter filter-value cards]
  (if site-filter
    (filter-sites filter-value cards)
    (filter-cards filter-value :Secondary cards)
    ))

(defn filter-dreamcards [should-filter cards]
  (if should-filter
    (filter-cards false :dreamcard cards)
    cards))

(defn filter-released [should-filter cards]
  (if should-filter
    (filter-cards true :released cards)
    cards))

(defn filter-title [query cards]
  (if (empty? query)
    cards
    (let [lcquery (.toLowerCase query)]
      (filter #(or (not= (.indexOf (.toLowerCase (:title %)) lcquery) -1)
                   (not= (.indexOf (:normalizedtitle %) lcquery) -1))
              cards))))

(defn sort-field [fieldname]
  (case fieldname
    "Set" #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %))
    "Name" (juxt :normalizedtitle #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %)))
    "Type" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %))
                    #((into {} (map-indexed (fn [i e] [e i]) primary-order)) (:type %)))
    "Align" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %))
                      #((into {} (map-indexed (fn [i e] [e i]) (concat general-alignments ["Neutral"]))) (:alignment %)))))

(defn selected-set-name [state]
  (-> (:set-filter state)
      (.replace "&nbsp;&nbsp;&nbsp;&nbsp;" "")
      (.replace " Set" "")))

(defn selected-set-dreamcards? [{:keys [sets]} state]
  (let [s (selected-set-name state)
        combined sets]
    (if (= s "All")
      false
      (->> combined
           (filter #(= s (:name %)))
           (first)
           (:dreamcards)))))

(defn selected-set-released? [{:keys [sets]} state]
  (let [s (selected-set-name state)
        combined sets]
    (if (= s "All")
      false
      (->> combined
           (filter #(= s (:name %)))
           (first)
           (:released)))))

(defn handle-scroll [e owner {:keys [page]}]
  (let [$cardlist (js/$ ".card-list")
        height (- (.prop $cardlist "scrollHeight") (.innerHeight $cardlist))]
    (when (> (.scrollTop $cardlist) (- height 600))
      (om/update-state! owner :page inc))))

(defn handle-search [e owner]
  (doseq [filter [:set-filter :secondary-filter :sort-filter :alignment-filter]]
    (om/set-state! owner filter "All"))
  (om/set-state! owner :search-query (.. e -target -value)))

(defn card-browser [{:keys [sets] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:search-query ""
       :sort-field "Name"
       :set-filter "All"
       :primary-filter "All"
       :alignment-filter "All"
       :secondary-filter "All"
       :haven-filter "All"
       :race-filter "All"
       :hide-dreamcards false
       :only-released true
       :page 1
       :filter-ch (chan)
       :selected-card nil})

    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [f (<! (om/get-state owner :filter-ch))]
              (om/set-state! owner (:filter f) (:value f))))))

    om/IDidMount
    (did-mount [_]
      (let [events (sub (:notif-chan (om/get-shared owner)) :card-selected (chan))]
        (go
          (loop [e (<! events)]
            (om/set-state! owner :selected-card (:data e))
            (recur (<! events))))))

    om/IRenderState
    (render-state [this state]
      (.focus (js/$ ".search"))
      (sab/html
        [:div.cardbrowser
         [:div.blue-shade.panel.filters
          (let [query (:search-query state)]
            [:div.search-box
             [:span.e.search-icon {:dangerouslySetInnerHTML #js {:__html "&#xe822;"}}]
             (when-not (empty? query)
               [:span.e.search-clear {:dangerouslySetInnerHTML #js {:__html "&#xe819;"}
                                      :on-click #(om/set-state! owner :search-query "")}])
             [:input.search {:on-change #(handle-search % owner)
                             :type "text" :placeholder "Search cards" :value query}]])

          [:div
           [:h4 "By"]
           [:select {:value (:sort-filter state)
                     :on-change #(om/set-state! owner :sort-field (.trim (.. % -target -value)))}
            (for [field ["Name" "Set" "Type" "Align"]]
              [:option {:value field} field])]]

          (let [format-pack-name (fn [name] (str "&nbsp;&nbsp;&nbsp;&nbsp;" name))
                hide-dreamcards (:hide-dreamcards state)
                released-only (:only-released state)
                dc-filtered (filter-dreamcards hide-dreamcards sets)
                sets-filtered (filter-released released-only dc-filtered)
                ;; Draft is specified as a cycle, but contains no set, nor is it marked as a bigbox
                ;; so we handled it specifically here for formatting purposes
                sets-list (map #(if (not (or (:bigbox %) (= (:name %) "Draft")))
                                  (update-in % [:name] format-pack-name)
                                  %)
                               sets-filtered)
                set-names (map :name
                               (sort-by (juxt :cycle_position :position)
                                        sets-list))]

            (if (= (:primary-filter state) "Site")
              (let [haven-options (if hide-dreamcards
                                    (case (:alignment-filter state)
                                      "All" standard-havens
                                      "Hero" standard-hero-havens
                                      "Minion" standard-minion-havens
                                      ("Balrog" "Fallen-wizard" "Fallen/Lord" "Lord"
                                        "Elf-lord" "Dwarf-lord" "Atani-lord"
                                        "War-lord" "Dragon-lord" "Grey" "Dual") [])
                                    (case (:alignment-filter state)
                                      "All" dreamcard-havens
                                      "Hero" dreamcard-hero-havens
                                      "Minion" dreamcard-minion-havens
                                      ("Balrog" "Fallen-wizard" "Fallen/Lord" "Lord"
                                        "Elf-lord" "Dwarf-lord" "Atani-lord"
                                        "War-lord" "Dragon-lord" "Grey" "Dual") [])
                                    )]
                (for [filter [["Set" :set-filter set-names]
                              ["Type" :primary-filter ["Character" "Resource" "Hazard" "Site" "Region"]]
                              ["Align" :alignment-filter (alignments (:primary-filter state))]
                              ["Strict" :secondary-filter (secondaries (:primary-filter state))]
                              ["Haven" :haven-filter haven-options]]]
                  [:div
                   [:h4 (first filter)]
                   [:select {:value ((second filter) state)
                             :on-change #(om/set-state! owner (second filter) (.. % -target -value))}
                    (options (last filter))]]))
              (if (or (= (:primary-filter state) "Character")
                      (= (:secondary-filter state) "Faction"))
                (let [type-options (if (= (:primary-filter state) "Character")
                                     race-options
                                     fact-options)]
                  (for [filter [["Set" :set-filter set-names]
                                ["Type" :primary-filter ["Character" "Resource" "Hazard" "Site" "Region"]]
                                ["Align" :alignment-filter (alignments (:primary-filter state))]
                                ["Strict" :secondary-filter (secondaries (:primary-filter state))]
                                ["Race" :race-filter type-options]
                                ]]
                    [:div
                     [:h4 (first filter)]
                     [:select {:value     ((second filter) state)
                               :on-change #(om/set-state! owner (second filter) (.. % -target -value))}
                      (options (last filter))]]))
                (for [filter [["Set" :set-filter set-names]
                              ["Type" :primary-filter ["Character" "Resource" "Hazard" "Site" "Region"]]
                              ["Align" :alignment-filter (alignments (:primary-filter state))]
                              ["Strict" :secondary-filter (secondaries (:primary-filter state))]]]
                  [:div
                   [:h4 (first filter)]
                   [:select {:value     ((second filter) state)
                             :on-change #(om/set-state! owner (second filter) (.. % -target -value))}
                    (options (last filter))]])
                )))

          [:div.hide-dreamcards-div
           [:label [:input.hide-dreamcards {:type "checkbox"
                                            :value true
                                            :checked (om/get-state owner :hide-dreamcards)
                                            :on-change #(let [hide (.. % -target -checked)]
                                                          (om/set-state! owner :hide-dreamcards hide)
                                                          (when (and hide (selected-set-dreamcards? cursor state))
                                                            (om/set-state! owner :set-filter "All"))
                                                          )}]
            "Hide Dreamcards"]]
          [:div.only-released-div
           [:label [:input.only-released {:type "checkbox"
                                            :value true
                                            :checked (om/get-state owner :only-released)
                                            :on-change #(let [hide (.. % -target -checked)]
                                                          (om/set-state! owner :only-released hide)
                                                          (when (and hide (selected-set-released? cursor state))
                                                            (om/set-state! owner :set-filter "All"))
                                                          )}]
            "Only Released"]]

          (om/build card-info-view cursor {:state {:selected-card (:selected-card state)}})
          ]

         [:div.card-list {:on-scroll #(handle-scroll % owner state)}
          (om/build-all card-view
                        (let [s (selected-set-name state)
                              [alt-filter cards] (cond
                                                   (= s "All") [nil @all-cards]
                                                   :else
                                                   [nil (filter #(= (:full_set %) s) @all-cards)])]
                          (->> cards
                               (filter-dreamcards (:hide-dreamcards state))
                               (filter-released (:only-released state))
                               (filter-cards (:primary-filter state) :type)
                               (filter-cards (:alignment-filter state) :alignment)
                               (filter-second (if (= (:primary-filter state) "Site") true false) (:secondary-filter state))
                               (filter-cards (:haven-filter state) :Haven)
                               (filter-cards (:race-filter state) :Race)
                               (filter-title (:search-query state))
                               (sort-by (sort-field (:sort-field state)))
                               (take (* (:page state) 28))))
                        {:key-fn #(str (:set_code %) (:code %) (:art %))
                         :fn #(assoc % :selected (and (= (:full_set %) (:full_set (:selected-card state)))
                                                      (= (:code %) (:code (:selected-card state)))))
                         :state {:cursor cursor :decorate-card true}
                         })]]))))

(om/root card-browser
         app-state
         {:shared {:notif-chan notif-chan
                   :pub-chan   pub-chan}
          :target (. js/document (getElementById "cardbrowser"))})
