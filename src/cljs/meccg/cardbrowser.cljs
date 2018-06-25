(ns meccg.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! >! sub pub] :as async]
            [clojure.string :as str]
            [meccg.appstate :refer [app-state]]
            [meccg.account :refer [alt-art-name]]
            [meccg.ajax :refer [GET]]
            [meccg.utils :refer [toastr-options banned-span restricted-span rotated-span influence-dots]]
            [cardnum.cards :refer [all-cards] :as cards]
            [cardnum.decks :as decks]))

(def cards-channel (chan))
(def pub-chan (chan))
(def notif-chan (pub pub-chan :topic))

(go (let [server-version (get-in (<! (GET "/data/cards/version")) [:json :version])
          local-cards (js->clj (.parse js/JSON (.getItem js/localStorage "cards")) :keywordize-keys true)
          need-update? (or (not local-cards) (not= server-version (:version local-cards)))
          cards (sort-by :code
                         (if need-update?
                           (:json (<! (GET "/data/cards")))
                           (:cards local-cards)))
          sets (:json (<! (GET "/data/sets")))
          mwl (:json (<! (GET "/data/mwl")))
          latest_mwl (->> mwl
                          (map (fn [e] (update e :date_start #(js/Date.parse %))))
                          (sort-by :date_start)
                          (last))]
      (reset! cards/mwl latest_mwl)
      (reset! cards/sets sets)
      (swap! app-state assoc :sets sets)
      (when need-update?
        (.setItem js/localStorage "cards" (.stringify js/JSON (clj->js {:cards cards :version server-version}))))
      (reset! all-cards cards)
      (swap! app-state assoc :cards-loaded true)
      (put! cards-channel cards)))

(defn make-span [text symbol class]
  (.replace text (apply str symbol) (str "<img src='" class "'style=\"width:16px;height:16px;\"></img>")))

(defn show-alt-art?
  "Is the current user allowed to use alternate art cards and do they want to see them?"
  ([] (show-alt-art? false))
  ([allow-all-users]
   (and
     (get-in @app-state [:options :show-alt-art] true)
     (or allow-all-users
         (get-in @app-state [:user :special] false)))))

(defn image-url
  ([card] (image-url card false))
  ([card allow-all-users]
   (let [art (or (:art card) ; use the art set on the card itself, or fall back to the user's preferences.
                 (get-in @app-state [:options :alt-arts (keyword (:code card))]))
         alt-card (get (:alt-arts @app-state) (:code card))
         has-art (and (show-alt-art? allow-all-users)
                      art
                      (contains? (:alt_art alt-card) (keyword art)))
         version-path (if has-art
                        (get (:alt_art alt-card) (keyword art) (:code card))
                        (:image_url card))]
     (str "/img/cards/" (:setname card) "/" version-path))))

(defn- alt-version-from-string
  "Given a string name, get the keyword version or nil"
  [setname]
  (when-let [alt (some #(when (= setname (:name %)) %) (:alt-info @app-state))]
    (keyword (:version alt))))

(defn- expand-alts
  [only-version acc card]
  (let [alt-card (get (:alt-arts @app-state) (:code card))
        alt-only (alt-version-from-string only-version)
        alt-keys (keys (:alt_art alt-card))
        alt-arts (if alt-only
                   (filter #(= alt-only %) alt-keys)
                   alt-keys)]
    (if (and alt-arts
             (show-alt-art? true))
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

(defn- insert-alt-arts
  "Add copies of alt art cards to the list of cards. If `only-version` is nil, all alt versions will be added."
  [only-version cards]
  (reduce (partial expand-alts only-version) () (reverse cards)))

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

(defn non-game-toast
  "Display a toast warning with the specified message."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr type)]
    (f msg)))

(defn- post-response [cursor response]
  (if (= 200 (:status response))
    (let [new-alts (get-in response [:json :altarts] {})]
      (swap! app-state assoc-in [:user :options :alt-arts] new-alts)
      (non-game-toast "Updated Art" "success" nil))
    (non-game-toast "Failed to Update Art" "error" nil)))

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

(defn select-alt-art [card cursor]
  (when-let [art (:art card)]
    (let [code (keyword (:code card))
          alts (:alt-arts (:options cursor))
          new-alts (if (keyword? art)
                     (assoc alts code (name art))
                     (dissoc alts code))]
      (om/update! cursor [:options :alt-arts] new-alts)
      (meccg.account/post-options "/profile" (partial post-response cursor)))))

(defn- card-text
  "Generate text html representation a card"
  [card cursor]
  [:div
   [:h4 (:title card)]
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
             {:class (cond (:selected card) "selected"
                           (selected-alt-art card cursor) "selected-alt")})
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
(def resource-secondaries ["Ally" "Faction" "Greater Item" "Major Item" "Minor Item" "Special Item"])
(def shared-secondaries ["Long-event" "Permanent-event" "Permanent-event/Short-event" "Short-event"])
(def hazard-secondaries ["Creature" "Creature/Permanent-event" "Creature/Short-event"])
(def general-alignments ["Hero" "Minion" "Balrog" "Lord" "Fallen-wizard" "Elf-lord" "Dwarf-lord" "FW/DL" "Dual"])
(def set-order ["The Wizards" "The Dragons" "Dark Minions" "The Lidless Eye" "Against the Shadow"
                "The White Hand" "The Balrog" "Firstborn" "Durin's Folk"])

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

(defn filter-alt-art-cards [cards]
  (let [alt-arts (:alt-arts @app-state)]
    (filter #(contains? alt-arts (:code %)) cards)))

(defn filter-alt-art-set [setname cards]
  (when-let [alt-key (alt-version-from-string setname)]
    (let [sa (map first
                  (filter (fn [[k v]] (contains? (:alt_art v) alt-key)) (:alt-arts @app-state)))]
      (filter (fn [c] (some #(= (:code c) %) sa)) cards))))

(defn filter-cards [filter-value field cards]
  (if (= filter-value "All")
    cards
    (filter #(= (field %) filter-value) cards)))

(defn filter-rotated [should-filter cards]
  (if should-filter
    (filter-cards false :rotated cards)
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
    "Name" (juxt :title #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %)))
    "Primary" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %))
                    #((into {} (map-indexed (fn [i e] [e i]) primary-order)) (:type %)))
    "Alignment" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:full_set %))
                      #((into {} (map-indexed (fn [i e] [e i]) (concat general-alignments ["Neutral"]))) (:alignment %)))))

(defn selected-set-name [state]
  (-> (:set-filter state)
      (.replace "&nbsp;&nbsp;&nbsp;&nbsp;" "")
      (.replace " Cycle" "")))

(defn selected-set-rotated? [{:keys [sets cycles]} state]
  (let [s (selected-set-name state)
        combined (concat sets cycles)]
    (if (= s "All")
      false
      (->> combined
           (filter #(= s (:name %)))
           (first)
           (:rotated)))))

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
       :hide-rotated false
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
           [:h4 "Sort by"]
           [:select {:value (:sort-filter state)
                     :on-change #(om/set-state! owner :sort-field (.trim (.. % -target -value)))}
            (for [field ["Name" "Set" "Primary" "Alignment"]]
              [:option {:value field} field])]]

          (let [format-pack-name (fn [name] (str "&nbsp;&nbsp;&nbsp;&nbsp;" name))
                hide-rotated (:hide-rotated state)
                sets-filtered (filter-rotated hide-rotated sets)
                ;; Draft is specified as a cycle, but contains no set, nor is it marked as a bigbox
                ;; so we handled it specifically here for formatting purposes
                sets-list (map #(if (not (or (:bigbox %) (= (:name %) "Draft")))
                                  (update-in % [:name] format-pack-name)
                                  %)
                               sets-filtered)
                set-names (map :name
                               (sort-by (juxt :cycle_position :position)
                                        sets-list))
                alt-art-sets (concat `("Alt Art")
                                     (map #(format-pack-name (:name %))
                                          (sort-by :position (:alt-info @app-state))))]
            (for [filter [["Set" :set-filter (if (show-alt-art? true)
                                               (concat set-names alt-art-sets)
                                               set-names)]
                          ["Primary" :primary-filter ["Character" "Resource" "Hazard" "Site" "Region"]]
                          ["Alignment" :alignment-filter (alignments (:primary-filter state))]
                          ["Secondary" :secondary-filter (secondaries (:primary-filter state))]]]
              [:div
               [:h4 (first filter)]
               [:select {:value ((second filter) state)
                         :on-change #(om/set-state! owner (second filter) (.. % -target -value))}
                (options (last filter))]]))

          [:div.hide-rotated-div
           [:label [:input.hide-rotated {:type "checkbox"
                                         :value true
                                         :checked (om/get-state owner :hide-rotated)
                                         :on-change #(let [hide (.. % -target -checked)]
                                                       (om/set-state! owner :hide-rotated hide)
                                                       (when (and hide (selected-set-rotated? cursor state))
                                                         (om/set-state! owner :set-filter "All"))
                                                       )}]
            "Hide rotated cards"]]

          (om/build card-info-view cursor {:state {:selected-card (:selected-card state)}})
          ]

         [:div.card-list {:on-scroll #(handle-scroll % owner state)}
          (om/build-all card-view
                        (let [s (selected-set-name state)
                              cycle-sets (set (for [x sets :when (= (:cycle x) s)] (:name x)))
                              [alt-filter cards] (cond
                                                   (= s "All") [nil @all-cards]
                                                   (= s "Alt Art") [nil (filter-alt-art-cards @all-cards)]
                                                   (str/ends-with? (:set-filter state) " Cycle") [nil (filter #(cycle-sets (:full_set %)) @all-cards)]
                                                   (not (some #(= s (:name %)) (:sets @app-state))) [s (filter-alt-art-set s @all-cards)]
                                                   :else
                                                   [nil (filter #(= (:full_set %) s) @all-cards)])]
                          (->> cards
                               (filter-cards (:primary-filter state) :type)
                               (filter-cards (:alignment-filter state) :alignment)
                               (filter-cards (:secondary-filter state) :Secondary)
                               (filter-rotated (:hide-rotated state))
                               (filter-title (:search-query state))
                               (insert-alt-arts alt-filter)
                               (sort-by (sort-field (:sort-field state)))
                               (take (* (:page state) 28))))
                        {:key-fn #(str (:setname %) (:code %) (:art %))
                         :fn #(assoc % :selected (and (= (:full_set %) (:full_set (:selected-card state)))
                                                      (= (:code %) (:code (:selected-card state)))
                                                      (= (:art %) (:art (:selected-card state)))))
                         :state {:cursor cursor :decorate-card true}
                         })]]))))

(om/root card-browser
         app-state
         {:shared {:notif-chan notif-chan
                   :pub-chan   pub-chan}
          :target (. js/document (getElementById "cardbrowser"))})
