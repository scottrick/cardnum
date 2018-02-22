(ns meccg.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! >! sub pub] :as async]
            [meccg.appstate :refer [app-state]]
            [meccg.account :refer [alt-art-name]]
            [meccg.ajax :refer [GET]]))

(def cards-channel (chan))
(def pub-chan (chan))
(def notif-chan (pub pub-chan :topic))

;; Load in sets and mwl lists
(go (let [sets (:json (<! (GET "/data/sets")))]
      (swap! app-state assoc :sets sets)))

(go (let [cards (sort-by :title (:json (<! (GET "/data/cards"))))]
      (swap! app-state assoc :cards cards)
      (swap! app-state assoc :cards-loaded true)
      (put! cards-channel cards)))

(defn make-span [text symbol class]
  (.replace text (apply str symbol) (str "<img src='" class "'style=\"width:16px;height:16px;\"></img>")))

(defn show-alt-art?
  "Is the current user allowed to use alternate art cards and do they want to see them?"
  []
  (and
    (get-in @app-state [:options :show-alt-art] true)
    (get-in @app-state [:user :special] false)))

(defn image-url [card]
  (str "/img/cards/" (:setname card) "/" (:ImageName card)))

(defn expand-alts
  [acc card]
  (let [alt-card (get (:alt-arts @app-state) (:code card))
        alt-arts (keys (:alt_art alt-card))]
    (if (and alt-arts
             (show-alt-art?))
      (->> alt-arts
           (concat [""])
           (map (fn [art] (if art
                            (assoc card :art art)
                            card)))
           (map (fn [c] (if (:art c)
                          (assoc c :display-name (str (:display-name c) " [" (alt-art-name (:art c)) "]"))
                          c)))
           (concat acc))
      (conj acc card))))

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
      (make-span "Coastal Seas [c]" "img/dc/me_cs.png")
      (make-span "Coastal Sea [c]" "img/dc/me_cs.png")
      (make-span "Dark-domains [d]" "img/dc/me_dd.png")
      (make-span "Dark-holds [D]" "img/dc/me_dh.png")
      (make-span "Dark-domain [d]" "img/dc/me_dd.png")
      (make-span "Dark-hold [D]" "img/dc/me_dh.png")
      (make-span "Darkhavens [V]" "img/dc/me_dha.png")
      (make-span "Darkhaven [V]" "img/dc/me_dha.png")
      (make-span "Darkhaven" "img/dc/me_dha.png")
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
      (make-span "[b]" "img/dc/me_bl.png")
      (make-span "[c]" "img/dc/me_cs.png")
      (make-span "[d]" "img/dc/me_dd.png")
      (make-span "[f]" "img/dc/me_fd.png")
      (make-span "[j]" "img/dc/me_ju.png")
      (make-span "[s]" "img/dc/me_sl.png")
      (make-span "[w]" "img/dc/me_wi.png")))

(defn selected-alt-art [card cursor]
  (let [code (keyword (:code card))
        alt-card (get (:alt-arts @app-state) (name code) nil)
        selected-alts (:alt-arts (:options cursor))
        selected-art (keyword (get selected-alts code nil))
        card-art (:art card)]
    (and alt-card
         (cond
           (= card-art selected-art) true
           (and (nil? selected-art)
                (not (keyword? card-art))) true
           (and (= :default selected-art)
                (not (keyword? card-art))) true
           :else false))))

(defn- card-text
  "Generate text html representation a card"
  [card cursor]
  [:div
   [:h4 (:title card)]
   (when-let [memory (:memoryunits card)]
     (if (< memory 3)
       [:div.anr-icon {:class (str "mu" memory)} ""]
       [:div.heading (str "Memory: " memory) [:span.anr-icon.mu]]))
   (when-let [cost (:cost card)]
     [:div.heading (str "Cost: " cost)])
   (when-let [trash-cost (:trash card)]
     [:div.heading (str "Trash cost: " trash-cost)])
   (when-let [strength (:strength card)]
     [:div.heading (str "Strength: " strength)])
   (when-let [requirement (:advancementcost card)]
     [:div.heading (str "Advancement requirement: " requirement)])
   (when-let [agenda-point (:agendapoints card)]
     [:div.heading (str "Agenda points: " agenda-point)])
   (when-let [min-deck-size (:minimumdecksize card)]
     [:div.heading (str "Minimum deck size: " min-deck-size)])
   (when-let [influence-limit (:influencelimit card)]
     [:div.heading (str "Influence limit: " influence-limit)])
   [:div.text
    [:p [:span.type (str (:type card))] (if (= (.toLowerCase (:type card)) (:Secondary card))
                                          ""
                                          (str ": " (:Secondary card)))]
    [:pre {:dangerouslySetInnerHTML #js {:__html
                                         (add-symbols
                                           (add-symbols
                                             (add-symbols
                                               (add-symbols
                                                 (add-symbols
                                                   (add-symbols
                                                     (add-symbols
                                                       (add-symbols
                                                         (add-symbols
                                                           (add-symbols
                                                             (add-symbols
                                                               (add-symbols
                                                                 (:text card)))))))))))))}}]]
   ])

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
             {:class (cond (:selected card) "selected"
                           (selected-alt-art card cursor) "selected-alt")})
           (if (:showText state)
             (card-text card cursor)
             (when-let [url (image-url card)]
               [:img {:src url
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
(def resource-secondaries ["Ally" "Faction" "Greater Item" "Major Item" "Minor Item" "Special Item"])
(def shared-secondaries ["Long-event" "Permanent-event" "Permanent-event/Short-event" "Short-event"])
(def hazard-secondaries ["Creature" "Creature/Permanent-event" "Creature/Short-event"])
(def general-alignments ["Hero" "Minion" "Balrog" "Lord" "Fallen-wizard" "Elf-lord" "Dwarf-lord" "FW/DL" "Dual"])
(def set-order ["METW" "METD" "MEDM" "MELE" "MEAS" "MEWH" "MEBA" "MEFB" "MEDF"])

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
      [:option {:value option :dangerouslySetInnerHTML #js {:__html option}}])))

(defn filter-cards [filter-value field cards]
  (if (= filter-value "All")
    cards
    (filter #(= (field %) filter-value) cards)))

(defn filter-title [query cards]
  (if (empty? query)
    cards
    (let [lcquery (.toLowerCase query)]
      (filter #(or (not= (.indexOf (.toLowerCase (:title %)) lcquery) -1)
                   (not= (.indexOf (:normalizedtitle %) lcquery) -1))
              cards))))

(defn sort-field [fieldname]
  (case fieldname
    "Set" #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:setname %))
    "Name" (juxt :title #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:setname %)))
    "Primary" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:setname %))
                    #((into {} (map-indexed (fn [i e] [e i]) primary-order)) (:primary %)))
    "Alignment" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:setname %))
                      #((into {} (map-indexed (fn [i e] [e i]) (concat general-alignments ["Neutral"]))) (:side %)))))

(defn selected-set-name [state]
  (-> (:set-filter state)
      (.replace "&nbsp;&nbsp;&nbsp;&nbsp;" "")
      (.replace " Set" "")))

(defn handle-scroll [e owner {:keys [page]}]
  (let [$cardlist (js/$ ".card-list")
        height (- (.prop $cardlist "scrollHeight") (.innerHeight $cardlist))]
    (when (> (.scrollTop $cardlist) (- height 600))
      (om/update-state! owner :page inc))))

(defn handle-search [e owner]
  (doseq [filter [:set-filter :secondary-filter :sort-filter :side-filter]]
    (om/set-state! owner filter "All"))
  (om/set-state! owner :search-query (.. e -target -value)))

(defn card-browser [{:keys [sets] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:search-query ""
       :sort-field "Set"
       :set-filter "All"
       :primary-filter "All"
       :side-filter "All"
       :secondary-filter "All"
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
                             :title "text" :placeholder "Search cards" :value query}]])

          [:div
           [:h4 "Sort by"]
           [:select {:value (:sort-filter state)
                     :on-change #(om/set-state! owner :sort-field (.trim (.. % -target -value)))}
            (for [field ["Set" "Name" "Primary" "Alignment"]]
              [:option {:value field} field])]]

            (for [filter [["Set" :set-filter (map :code
                                                  (sort-by (juxt :position)
                                                           sets))]
                          ["Primary" :primary-filter ["Character" "Resource" "Hazard" "Site" "Region"]]
                          ["Alignment" :side-filter (alignments (:primary-filter state))]
                          ["Secondary" :secondary-filter (secondaries (:primary-filter state))]]]
              [:div
               [:h4 (first filter)]
               [:select {:value ((second filter) state)
                         :on-change #(om/set-state! owner (second filter) (.. % -target -value))}
                (options (last filter))]])

          (om/build card-info-view cursor {:state {:selected-card (:selected-card state)}})
          ]

         [:div.card-list {:on-scroll #(handle-scroll % owner state)}
          (om/build-all card-view
                        (let [s (-> (:set-filter state)
                                    (.replace "&nbsp;&nbsp;&nbsp;&nbsp;" ""))
                              list-sets (set (for [x sets :when (= (:code x) s)] (:code x)))
                              cards (if (= s "All")
                                      (:cards cursor)
                                      (if (= (.indexOf (:set-filter state) "Set") -1)
                                        (filter #(= (:setname %) s) (:cards cursor))
                                        (filter #(list-sets (:position %)) (:cards cursor))))]
                          (->> cards
                               (filter-cards (:primary-filter state) :type)
                               (filter-cards (:side-filter state) :side)
                               (filter-cards (:secondary-filter state) :Secondary)
                               (filter-title (:search-query state))
                               (sort-by (sort-field (:sort-field state)))
                               (take (* (:page state) 28))))
                        {:key :imageName})]]))))

(om/root card-browser
         app-state
         {:shared {:notif-chan notif-chan
                   :pub-chan   pub-chan}
          :target (. js/document (getElementById "cardbrowser"))})
