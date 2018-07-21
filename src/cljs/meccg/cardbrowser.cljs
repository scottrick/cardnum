(ns meccg.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! >! sub pub] :as async]
            [clojure.string :as str]
            [cardnum.cards :refer [all-cards] :as cards]
            [cardnum.decks :as decks]
            [meccg.appstate :refer [app-state]]
            [meccg.account :refer [alt-art-name]]
            [meccg.ajax :refer [GET]]
            [meccg.utils :refer [toastr-options banned-span restricted-span rotated-span influence-dots]]
            [reagent.core :as r]))

(def cards-channel (chan))
(def pub-chan (chan))
(def notif-chan (pub pub-chan :topic))

(def browser-state (atom {}))

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
   (str "/img/cards/" (:setname card) "/" (:ImageName card))))

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
      (make-span "Coastal Seas [c]" "img/dc/me_cs.png")
      (make-span "Coastal Sea [c]" "img/dc/me_cs.png")
      (make-span "Dark-domains [d]" "img/dc/me_dd.png")
      (make-span "Dark-holds [D]" "img/dc/me_dh.png")
      (make-span "Dark-domain [d]" "img/dc/me_dd.png")
      (make-span "Dark-hold [D]" "img/dc/me_dh.png")
      (make-span "Darkhavens [V]" "img/dc/me_dha.png")
      (make-span "Darkhaven [V]" "img/dc/me_dha.png")
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
      (make-span "[w]" "img/dc/me_wi.png")))

(defn non-game-toast
  "Display a toast warning with the specified message."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr type)]
    (f msg)))

(defn- post-response [response]
  (if (= 200 (:status response))
    (let [new-alts (get-in response [:json :altarts] {})]
      (swap! app-state assoc-in [:user :options :alt-arts] new-alts)
      (non-game-toast "Updated Art" "success" nil))
    (non-game-toast "Failed to Update Art" "error" nil)))

(defn- card-text
  "Generate text html representation a card"
  [card]
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

(def primary-order ["Character" "Resource" "Hazard" "Site" "Region"])
(def resource-secondaries ["Ally" "Faction" "Greater Item" "Major Item" "Minor Item" "Gold Ring Item" "Special Item"])
(def shared-secondaries ["Permanent-event" "Short-event" "Long-event" "Permanent-event/Short-event" "Permanent-event/Long-event" "Short-event/Long-event"])
(def hazard-secondaries ["Creature" "Creature/Permanent-event" "Creature/Short-event" "Creature/Long-event"])
(def general-alignments ["Hero" "Minion" "Balrog" "Fallen-wizard" "Fallen/Lord" "Lord"
                         "Elf-lord" "Dwarf-lord" "Atani-lord" "War-lord" "Dragon-lord"
                         "Grey" "Dual"])
(def set-order ["The Wizards" "The Dragons" "Dark Minions" "The Lidless Eye" "Against the Shadow" "The White Hand" "The Balrog"
                "Firstborn" "Durin's Folk" "The Necromancer" "Bay of Ormal" "Court of Ardor" "The Central Plains" "Dominion"
                "The Great Wyrms" "Kingdom of the North" "Morgoth's Legacy" "Mortal Men" "The Northern Waste" "Red Nightfall"
                "Return of the Shadow" "The Sun Lands" "Treason of Isengard" "War of the Ring"])

(defn secondaries [primary]
  (case primary
    "All" (concat hazard-secondaries shared-secondaries resource-secondaries ["site"] ["region"])
    "Character" ["character" "Avatar" "Leader" "Agent"]
    "Resource" (concat resource-secondaries shared-secondaries)
    "Hazard" (concat hazard-secondaries shared-secondaries)
    "Site" ["site"]
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
      [:option {:value option :key option :dangerouslySetInnerHTML #js {:__html option}}])))

(defn filter-cards [filter-value field cards]
  (if (= filter-value "All")
    cards
    (filter #(= (field %) filter-value) cards)))

(defn filter-dreamcards [should-filter cards]
  (if should-filter
    (filter-cards false :dreamcard cards)
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
    "Name" (juxt :title #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %)))
    "Set" #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %))
    "Type" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %))
                    #((into {} (map-indexed (fn [i e] [e i]) primary-order)) (:type %)))
    "Align" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %))
                      #((into {} (map-indexed (fn [i e] [e i]) (concat general-alignments ["Neutral"]))) (:alignment %)))))

(defn card-info-view [s]
  (let [selected-card (:selected-card @s)]
    (if (nil? selected-card)
      [:div {:display "none"}]
      [:div
       [:h4 "Card text"]
       [:div.blue-shade.panel
        (card-text selected-card)]])))

(defn selected-set-name [s]
  (-> (:set-filter @s)
      (.replace "&nbsp;&nbsp;&nbsp;&nbsp;" "")
      (.replace " Set" "")))

(defn selected-set-dreamcards? [{:keys [sets]} s]
  (let [set (selected-set-name s)
        combined sets]
    (if (= set "All")
      false
      (->> combined
           (filter #(= set (:name %)))
           (first)
           (:dreamcards)))))

(defn handle-scroll [e s]
  (let [$cardlist (js/$ ".card-list")
        height (- (.prop $cardlist "scrollHeight") (.innerHeight $cardlist))]
    (when (> (.scrollTop $cardlist) (- height 600))
      (swap! s update-in [:page] (fnil inc 0)))))

(defn handle-search [e s]
  (doseq [filter [:set-filter :secondary-filter :sort-filter :alignment-filter]]
    (swap! s assoc filter "All"))
  (swap! s assoc :search-query (.. e -target -value)))

(defn card-view [card s]
  (let [cv (r/atom {:showText false})]
    (fn [card s]
      [:div.card-preview.blue-shade
       (when (:decorate-card @s)
         {:class (cond (= (:selected-card @s) card) "selected")})
       (if (:showText @cv)
         (card-text card)
         (when-let [url (image-url card true)]
           [:img {:src url
                  :alt (:title card)
                  :onClick #(do (.preventDefault %)
                                (swap! s assoc :selected-card card))
                  :onError #(-> (swap! cv assoc :showText true))
                  :onLoad #(-> % .-target js/$ .show)}]))])))

(defn card-list-view [s]
  (let [selected (selected-set-name s)
        [alt-filter cards] (cond
                             (= selected "All") [nil @all-cards]
                             :else
                             [nil (filter #(= (:full_set %) selected) @all-cards)])
        cards (->> cards
                   (filter-cards (:primary-filter @s) :type)
                   (filter-cards (:alignment-filter @s) :alignment)
                   (filter-cards (:secondary-filter @s) :Secondary)
                   (filter-dreamcards (:hide-dreamcards @s))
                   (filter-title (:search-query @s))
                   (sort-by (sort-field (:sort-field @s)))
                   (take (* (:page @s) 28)))]
    [:div.card-list {:on-scroll #(handle-scroll % s)}
     (doall
       (for [card cards]
         ^{:key (or (:display-name card) (:code card))}
         [card-view card s]))]))

(defn card-browser []
  (let [s (r/atom {:search-query ""
                   :sort-field "Name"
                   :set-filter "All"
                   :primary-filter "All"
                   :alignment-filter "All"
                   :secondary-filter "All"
                   :hide-dreamcards true
                   :page 1
                   :decorate-card true
                   :selected-card nil})
        sets (r/cursor app-state [:sets])
        cycles (r/cursor app-state [:cycles])]

    (r/create-class
      {:display-name "card-browser"

       :reagent-render
                     (fn []
                       (.focus (js/$ ".search"))
                       [:div.cardbrowser [:div.blue-shade.panel.filters
                                          (let [query (:search-query @s)]
                                            [:div.search-box
                                             [:span.e.search-icon {:dangerouslySetInnerHTML #js {:__html "&#xe822;"}}]
                                             (when-not (empty? query)
                                               [:span.e.search-clear {:dangerouslySetInnerHTML #js {:__html "&#xe819;"}
                                                                      :on-click #(swap! s assoc :search-query "")}])
                                             [:input.search {:on-change #(handle-search % s)
                                                             :type "text" :placeholder "Search cards" :value query}]])
                                          [:div
                                           [:h4 "By"]
                                           [:select {:value (:sort-filter @s)
                                                     :on-change #(swap! s assoc :sort-field (.trim (.. % -target -value)))}
                                            (for [field ["Name" "Set" "Type" "Align"]]
                                              ^{:key field}
                                              [:option {:value field} field])]
                                           ]

                                          (let [format-pack-name (fn [name] (str "&nbsp;&nbsp;&nbsp;&nbsp;" name))
                                                hide-dreamcards (:hide-dreamcards @s)
                                                sets-filtered (filter-dreamcards hide-dreamcards @sets)
                                                ;; Draft is specified as a cycle, but contains no set, nor is it marked as a bigbox
                                                ;; so we handled it specifically here for formatting purposes
                                                sets-list (map #(if (not (or (:bigbox %) (= (:name %) "Draft")))
                                                                  (update-in % [:name] format-pack-name)
                                                                  %)
                                                               sets-filtered)
                                                set-names (map :name
                                                               (sort-by (juxt :cycle_position :position)
                                                                        sets-list))]
                                            (doall
                                              (for [[title key f] [["Set" :set-filter set-names]
                                                                   ["Type" :primary-filter ["Character" "Resource" "Hazard" "Site" "Region"]]
                                                                   ["Align" :alignment-filter (alignments (:primary-filter @s))]
                                                                   ["Strict" :secondary-filter (secondaries (:primary-filter @s))]]]
                                                ^{:key title}
                                                [:div
                                                 [:h4 title]
                                                 [:select {:value (key @s)
                                                           :on-change #(swap! s assoc key (.. % -target -value))}
                                                  (options f)]])))

                                          [:div.hide-dreamcards-div
                                           [:label [:input.hide-dreamcards {:type "checkbox"
                                                                            :value true
                                                                            :checked (:hide-dreamcards @s)
                                                                            :on-change #(let [hide (.. % -target -checked)]
                                                                                          (swap! s assoc :hide-dreamcards hide)
                                                                                          (when (and hide (selected-set-dreamcards? @app-state s))
                                                                                            (swap! s assoc :set-filter "All")))}]
                                            "Hide Dreamcards"]]
                                          [card-info-view s]]

                        [card-list-view s]])})))