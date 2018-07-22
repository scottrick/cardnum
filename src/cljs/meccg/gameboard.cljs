(ns meccg.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [capitalize includes? join lower-case split]]
            [differ.core :as differ]
            [cardnum.utils :refer [str->int]]
            [cardnum.cards :refer [all-cards]]
            [meccg.appstate :refer [app-state]]
            [meccg.auth :refer [avatar] :as auth]
            [meccg.cardbrowser :refer [add-symbols] :as cb]
            [meccg.utils :refer [toastr-options influence-dot map-longest]]
            [meccg.ws :as ws]
            [om.core :as om :include-macros true]
            [om.dom :as dom]
            [sablono.core :as sab :include-macros true]))

(defonce game-state (atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defn image-url [{:keys [set_code ImageName] :as card}]
    (str "/img/cards/" (:set_code card) "/" (:ImageName card)))

(defn get-side [state]
  (let [user-id (:_id (:user @app-state))]
    (cond
      (= (get-in state [:challenger :user :_id]) user-id) :challenger
      (= (get-in state [:contestant :user :_id]) user-id) :contestant
      :else :spectator)))

(defn not-spectator? []
  (not= :spectator (get-side @game-state)))

(defn init-game [state]
  (let [side (get-side state)]
    (.setItem js/localStorage "gameid" (:gameid @app-state))
    (reset! game-state state)
    (swap! game-state assoc :side side)
    (reset! last-state @game-state)
    (reset! lock false)))

(defn launch-game [{:keys [state]}]
  (init-game state)
  (set! (.-onbeforeunload js/window) #(clj->js "Leaving this page will disconnect you from the game."))
  (-> "#gamelobby" js/$ .fadeOut)
  (-> "#gameboard" js/$ .fadeIn))

(declare toast)

(defn notify
  "Send a notification to the chat, and a toast to the current player of the specified severity"
  [text severity]
  (swap! game-state update-in [:log] #(conj % {:user "__system__" :text text}))
  (toast text severity nil))

(def zoom-channel (chan))

(defn check-lock?
  "Check if we can clear client lock based on action-id"
  []
  (let [aid [(:side @game-state) :aid]]
    (when (not= (get-in @game-state aid)
                (get-in @last-state aid))
      (reset! lock false))))

(defn handle-state [{:keys [state]}] (init-game state))

(defn handle-diff [{:keys [gameid diff]}]
  (when (= gameid (:gameid @game-state))
    (swap! game-state #(differ/patch @last-state diff))
    (check-lock?)
    (reset! last-state @game-state)))

(defn handle-timeout [{:keys [gameid]}]
  (when (= gameid (:gameid @game-state))
    (toast "Game closed due to inactivity" "error" {:time-out 0 :close-button true})))

(defn parse-state [state]
  (js->clj (.parse js/JSON state) :keywordize-keys true))

(ws/register-ws-handler! :meccg/state #(handle-state (parse-state %)))
(ws/register-ws-handler! :meccg/start #(launch-game (parse-state %)))
(ws/register-ws-handler! :meccg/diff #(handle-diff (parse-state %)))
(ws/register-ws-handler! :meccg/timeout #(handle-timeout (parse-state %)))

(def anr-icons {"[Credits]" "credit"
                "[$]" "credit"
                "[c]" "credit"
                "[Credit]" "credit"
                "[Click]" "click"
                "[Subroutine]" "subroutine"
                "[Recurring Credits]" "recurring-credit"
                "1[Memory Unit]" "mu1"
                "1[mu]" "mu1"
                "2[Memory Unit]" "mu2"
                "2[mu]" "mu2"
                "3[Memory Unit]" "mu3"
                "3[mu]" "mu3"
                "[Link]" "link"
                "[l]" "link"
                "[Memory Unit]" "mu"
                "[mu]" "mu"
                "[Discard]" "discard"
                "[t]" "discard"})


(defn send-command
  ([command] (send-command command nil))
  ([command {:keys [no-lock] :as args}]
   (when (or (not @lock) no-lock)
     (try (js/ga "send" "event" "game" command) (catch js/Error e))
     (when-not no-lock (reset! lock true))
     (ws/ws-send! [:meccg/action {:gameid-str (:gameid @game-state) :command command :args args}]))))

(defn send-msg [event owner]
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)
        $div (js/$ ".gameboard .messages")]
    (when-not (empty? text)
      (ws/ws-send! [:meccg/say {:gameid-str (:gameid @game-state) :msg text}])
      (.scrollTop $div (+ (.prop $div "scrollHeight") 500))
      (aset input "value" "")
      (.focus input))))

(defn send-typing [event owner]
  "Send a typing event to server for this user if it is not already set in game state"
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)]
    (if (empty? text)
      (ws/ws-send! [:meccg/typing {:gameid-str (:gameid @game-state) :typing false}])
      (when (not-any? #{(get-in @app-state [:user :username])} (:typing @game-state))
        (ws/ws-send! [:meccg/typing {:gameid-str (:gameid @game-state) :typing true}])))))

(defn mute-spectators [mute-state]
  (ws/ws-send! [:meccg/mute-spectators {:gameid-str (:gameid @game-state) :mute-state mute-state}]))

(defn concede []
  (ws/ws-send! [:meccg/concede {:gameid-str (:gameid @game-state)}]))

(defn build-exception-msg [msg error]
  (letfn [(build-report-url [error]
            (js/escape (str "Please describe the circumstances of your error here.\n\n\nStack Trace:\n```clojure\n"
                            error
                            "\n```")))]
    (str "<div>"
         msg
         "<br/>"
         "<button type=\"button\" class=\"reportbtn\" style=\"margin-top: 5px\" "
         "onclick=\"window.open('https://github.com/revealwits/meccg/issues/new?body="
         (build-report-url error)
         "');\">Report on GitHub</button></div>")))

(defn toast
  "Display a toast warning with the specified message.
  Sends a command to clear any server side toasts."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr (if (= "exception" type) "error" type))]
    (f (if (= "exception" type) (build-exception-msg msg (:last-error @game-state)) msg))
    (send-command "toast")))

(defn play-sfx
  "Plays a list of sounds one after another."
  [sfx soundbank]
  (when-not (empty? sfx)
    (when-let [sfx-key (keyword (first sfx))]
      (.volume (sfx-key soundbank) (/ (str->int (get-in @app-state [:options :sounds-volume])) 100))
      (.play (sfx-key soundbank)))
    (play-sfx (rest sfx) soundbank)))

(defn action-list [{:keys [type zone revealed advanceable advance-counter advancementcost current-cost] :as card}]
  (-> []
      (#(if (or (and (= type "Agenda")
                     (#{"locales" "onhost"} (first zone)))
                (= advanceable "always")
                (and revealed
                     (= advanceable "while-revealed"))
                (and (not revealed)
                     (= advanceable "while-unrevealed")))
          (cons "advance" %) %))
      (#(if (and (= type "Agenda") (>= advance-counter current-cost))
          (cons "score" %) %))
      (#(if (#{"Site" "Character" "Region"} type)
          (if-not revealed (cons "reveal" %) (cons "hide" %))
          %))))

(defn handle-abilities [{:keys [abilities facedown side type] :as card} owner]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))]
    (when-not (and (= side "Challenger") facedown)
      (cond
        ;; Open panel
        (or (> c 1)
            (some #{"hide" "advance"} actions)
            (and (= type "Character")
                 (not (:run @game-state))))  ; Horrible hack to check if currently in a run
        (-> (om/get-node owner "abilities") js/$ .toggle)
        ;; Trigger first (and only) ability / action
        (= c 1)
        (if (= (count abilities) 1)
          (send-command "ability" {:card card :ability 0})
          (send-command (first actions) {:card card}))))))

(defn handle-card-click [{:keys [type zone root] :as card} owner]
  (let [side (:side @game-state)]
    (when (not-spectator?)
      (cond
        ;; Selecting card
        (= (get-in @game-state [side :prompt 0 :prompt-type]) "select")
        (send-command "select" {:card card})
        ;; Card is an identity of player's side
        (and (= (:type card) "Identity")
             (= side (keyword (.toLowerCase (:side card)))))
        (handle-abilities card owner)
        ;; Challenger side
        (= side :challenger)
        (case (first zone)
          "hand" (if (:host card)
                   (when (:placed card)
                     (handle-abilities card owner))
                   (send-command "play" {:card card}))
          ("rig" "current" "onhost" "play-area") (handle-abilities card owner)
          ("locales") (when (and (= type "Character") (:revealed card))
                        ;; Character that should show list of abilities that send messages to fire sub
                        (-> (om/get-node owner "challenger-abilities") js/$ .toggle))
          nil)
        ;; Contestant side
        (= side :contestant)
        (case (first zone)
          "hand" (case type
                   ("Region" "Character") (if root
                                       (send-command "play" {:card card :locale root})
                                       (-> (om/get-node owner "locales") js/$ .toggle))
                   ("Agenda" "Site") (if (< (count (get-in @game-state [:contestant :locales])) 4)
                                        (send-command "play" {:card card :locale "New party"})
                                        (-> (om/get-node owner "locales") js/$ .toggle))
                   (send-command "play" {:card card}))
          ("locales" "scored" "current" "onhost") (handle-abilities card owner)
          "rig" (when (:contestant-abilities card)
                  (-> (om/get-node owner "contestant-abilities") js/$ .toggle))
          nil)))))

(defn in-play? [card]
  (let [dest (when (= (:side card) "Challenger")
               (get-in @game-state [:challenger :rig (keyword (.toLowerCase (:type card)))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn has?
  "Checks the string property of the card to see if it contains the given value"
  [card property value]
  (when-let [p (property card)]
    (> (.indexOf p value) -1)))

(defn has-subtype?
  "Checks if the specified subtype is present in the card."
  [card subtype]
  (or (has? card :subtype subtype)
      (when-let [persistent-subs (-> card :persistent :subtype)]
        (includes? persistent-subs subtype))))

(defn playable? [{:keys [title side zone cost type uniqueness abilities] :as card}]
  (let [my-side (:side @game-state)
        me (my-side @game-state)]
    (and (= (keyword (.toLowerCase side)) my-side)

         (cond

           (has-subtype? card "Double")
           (if (>= (:click me) 2) true false)

           (has-subtype? card "Triple")
           (if (>= (:click me) 3) true false)

           (= (:code card) "07036") ; Day Job
           (if (>= (:click me) 4) true false)

           (has-subtype? card "Priority")
           (if (get-in @game-state [my-side :register :spent-click]) false true)

           :else
           true)

         (and (= zone ["hand"])
              (or (not uniqueness) (not (in-play? card)))
              (or (#{"Agenda" "Site" "Region" "Character"} type) (>= (:credit me) cost))
              (pos? (:click me))))))

(defn spectator-view-hidden?
  "Checks if spectators are allowed to see hidden information, such as hands and face-down cards"
  []
  (and (get-in @game-state [:options :spectatorhands])
       (not (not-spectator?))))

(def ci-open "\u2664")
(def ci-seperator "\u2665")
(def ci-close "\u2666")

(defn is-card-item [item]
  (and (> (.indexOf item ci-seperator) -1)
       (= 0 (.indexOf item ci-open))))

(defn extract-card-info [item]
  (if (is-card-item item)
    [(.substring item 1 (.indexOf item ci-seperator))
     (.substring item (inc (.indexOf item ci-seperator)) (dec (count item)))]))

(defn create-span-impl [item]
  (if (= "[hr]" item)
    [:hr]
    (if (= "[!]" item)
      [:div.smallwarning "!"]
      (if-let [class (anr-icons item)]
        [:span {:class (str "anr-icon " class)}]
        (if-let [[title code] (extract-card-info item)]
          [:span {:class "fake-link" :id code} title]
          [:span item])))))

(defn get-non-alt-art [[title cards]]
  {:title title :code (:code (first cards))})

(defn prepare-cards []
  (->> @all-cards
    (filter #(not (:replaced_by %)))
    (group-by :title)
    (map get-non-alt-art)
    (sort-by #(count (:title %1)))
    (reverse)))

(def prepared-cards (memoize prepare-cards))

(def create-span (memoize create-span-impl))

(defn find-card-regex-impl [title]
  (str "(^|[^" ci-open "\\S])" title "(?![" ci-seperator "\\w]|([^" ci-open "]+" ci-close "))"))

(def find-card-regex (memoize find-card-regex-impl))

(defn card-image-token-impl [title code]
  (str "$1" ci-open title ci-seperator code ci-close))

(def card-image-token (memoize card-image-token-impl))

(defn card-image-reducer [text card]
  (.replace text (js/RegExp. (find-card-regex (:title card)) "g") (card-image-token (:title card) (:code card))))

(defn add-image-codes-impl [text]
  (reduce card-image-reducer text (prepared-cards)))

(def add-image-codes (memoize add-image-codes-impl))

(defn get-message-parts-impl [text]
  (let [with-image-codes (add-image-codes (if (nil? text) "" text))
        splitted (.split with-image-codes (js/RegExp. (str "(" ci-open "[^" ci-close "]*" ci-close ")") "g"))
        oldstyle (for [i splitted]
                   (seq (.split i (js/RegExp. (str "([1-3]\\[mu\\]|\\[[^\\]]*\\])") "g"))))]
    (flatten oldstyle)))

(def get-message-parts (memoize get-message-parts-impl))

(defn get-card-code [e]
  (let [code (str (.. e -target -id))]
    (when (pos? (count code))
      code)))

(defn card-preview-mouse-over [e channel]
  (.preventDefault e)
  (when-let [code (get-card-code e)]
    (when-let [card (some #(when (= (:code %) code) %) @all-cards)]
      (put! channel (assoc card :implementation :full))))
  nil)

(defn card-preview-mouse-out [e channel]
  (.preventDefault e)
  (when-let [code (get-card-code e)]
    (put! channel false))
  nil)

(defn log-pane [cursor owner]
  (reify
    ;; om/IInitState
    ;; (init-state [this] {:scrolling false})

    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (let [div (om/get-node owner "msg-list")
            ;; curr-msg-count (count (:log cursor))
            ;; prev-msg-count (count (:log prev-props))
            scrolltop (.-scrollTop div)
            height (.-scrollHeight div)]
        (when (or (zero? scrolltop)
                  (< (- height scrolltop (.height (js/$ ".gameboard .log"))) 500))
          (aset div "scrollTop" height))))
        ;;     is-scrolled (om/get-state owner :scrolling)
        ;;     scroll-top (.-scrollTop div)
        ;;     scroll-height (.-scrollHeight div)]
        ;; (when (or (and (zero? scroll-top)
        ;;                (not is-scrolled))
        ;;           (and (not= curr-msg-count prev-msg-count)
        ;;                (not is-scrolled)))
        ;;   (aset div "scrollTop" scroll-height))))

    om/IDidMount
    (did-mount [this]
      (-> ".log" js/$ (.resizable #js {:handles "w"})))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.log {:on-mouse-over #(card-preview-mouse-over % zoom-channel)
                  :on-mouse-out  #(card-preview-mouse-out % zoom-channel)}
        [:div.panel.blue-shade.messages {:ref "msg-list"}
                                         ;; :on-scroll #(let [currElt (.-currentTarget %)
                                         ;;                   scroll-top (.-scrollTop currElt)
                                         ;;                   scroll-height (.-scrollHeight currElt)
                                         ;;                   client-height (.-clientHeight currElt)
                                         ;;                   scrolling (< (+ scroll-top client-height) scroll-height)]
                                         ;;               (om/set-state! owner :scrolling scrolling))}
         (for [msg (:log cursor)]
           (when-not (and (= (:user msg) "__system__") (= (:text msg) "typing"))
             (if (= (:user msg) "__system__")
               [:div.system (for [item (get-message-parts (:text msg))] (create-span item))]
               [:div.message
                (om/build avatar (:user msg) {:opts {:size 38}})
                [:div.content
                 [:div.username (get-in msg [:user :username])]
                 [:div (for [item (get-message-parts (:text msg))] (create-span item))]]])))]
        (when (seq (remove nil? (remove #{(get-in @app-state [:user :username])} (:typing cursor))))
          [:div [:p.typing (for [i (range 10)] [:span " " influence-dot " "])]])
        (if-let [game (some #(when (= (:gameid cursor) (str (:gameid %))) %) (:games @app-state))]
          (when (or (not-spectator?)
                    (not (:mutespectators game)))
            [:form {:on-submit #(send-msg % owner)
                    :on-input #(send-typing % owner)}
             [:input {:ref "msg-input" :placeholder "Say something" :accessKey "l"}]]))]))))

(defn handle-dragstart [e cursor]
  (-> e .-target js/$ (.addClass "dragged"))
  (-> e .-dataTransfer (.setData "card" (.stringify js/JSON (clj->js @cursor)))))

(defn handle-drop [e locale]
  (-> e .-target js/$ (.removeClass "dragover"))
  (let [card (-> e .-dataTransfer (.getData "card") ((.-parse js/JSON)) (js->clj :keywordize-keys true))
        side (if (#{"HQ" "R&D" "Archives"} locale) "Contestant" "Challenger")]
    (send-command "move" {:card card :locale locale})))

(defn abs [n] (max n (- n)))

;; touch support
(defonce touchmove (atom {}))

(defn release-touch [card]
  (-> card (.removeClass "disable-transition"))
  (-> card (.css "position" ""))
  (-> card (.css "top" "")))

(defn update-card-position [card touch]
  (-> card (.css "left" (str (- (int (aget touch "pageX")) 30) "px")))
  (-> card (.css "top"  (str (- (int (aget touch "pageY")) 42) "px"))))

(defn get-card [e locale]
  (-> e .-target js/$ (.closest ".card-wrapper")))

(defn get-locale-from-touch [touch]
  (let [cX (.. touch -clientX)
        cY (.. touch -clientY)
        locale (-> (js/document.elementFromPoint cX cY)
                   js/$
                   (.closest "[data-locale]")
                   (.attr "data-locale"))]
    [locale (> (+ (abs (- (:x @touchmove) cX))
                  (abs (- (:y @touchmove) cY)))
               30)]))

(defn handle-touchstart [e cursor]
  (let [touch (aget (.. e -targetTouches) 0)
        [locale _] (get-locale-from-touch touch)
        card (get-card e locale)]
    (-> card (.addClass "disable-transition"))
    (reset! touchmove {:card (.stringify js/JSON (clj->js @cursor))
                       :x (.. touch -clientX)
                       :y (.. touch -clientY)
                       :start-locale locale})))

(defn handle-touchmove [e]
  (let [touch (aget (.. e -targetTouches) 0)
        card (get-card e (:start-locale @touchmove))]
    (-> card (.css "position" "fixed"))
    (update-card-position card touch)))

(defn handle-touchend [e]
  (let [touch (aget (.. e -changedTouches) 0)
        card (get-card e (:start-locale @touchmove))
        [locale moved-enough] (get-locale-from-touch touch)]
    (release-touch card)
    (when (and locale moved-enough (not= locale (:start-locale @touchmove)))
      (let [cardinfo (-> @touchmove :card ((.-parse js/JSON)) (js->clj :keywordize-keys true))]
        (send-command "move" {:card cardinfo :locale locale})))))

(defn ability-costs [ab]
  (when-let [cost (:cost ab)]
    (str (clojure.string/join
          ", " (for [c (partition 2 cost)]
                 (str (case (first c)
                        "credit" (str (second c) " [" (capitalize (name (first c))) "]")
                        (clojure.string/join "" (repeat (second c) (str "[" (capitalize (name (first c))) "]"))))))) ": ")))

(defn party->num [locale]
  (-> locale str (clojure.string/split #":party") last str->int))

(defn party->name [locale]
  (let [num (party->num locale)]
    (str "Locale " num)))

(defn central->name [zone]
  "Converts a central zone keyword to a string."
  (case (if (keyword? zone) zone (last zone))
    :hq "HQ"
    :rd "R&D"
    :archives "Archives"
    nil))

(defn zone->name [zone]
  "Converts a zone to a string."
  (or (central->name zone)
      (party->name zone)))

(defn zone->sort-key [zone]
  (case (if (keyword? zone) zone (last zone))
    :archives -3
    :rd -2
    :hq -1
    (str->int
     (last (clojure.string/split (str zone) #":party")))))

(defn zones->sorted-names [zones]
  (->> zones (sort-by zone->sort-key) (map zone->name)))

(defn get-parties [locales]
  (->> locales
       (filter #(not (#{:hq :rd :archives} (first %))))
       (sort-by #(zone->sort-key (first %)))))

(defn party-list [parties]
  (->> parties (map first) zones->sorted-names))

(defn card-counter-type [card]
  (let [counter-type (:counter-type card)]
    ;; Determine the appropriate type of counter for styling, falling back to
    ;; power counters when no other type can be inferred.
    (cond
      ;; If an placed card contains an annotation, use it.
      (and (:placed card)
           (not (nil? counter-type)))
      counter-type
      (= "Agenda" (:type card)) "Agenda"
      ;; Assume unplaced cards with counters are hosted on Personal
      ;; Workshop.
      (not (:placed card)) "Power"
      (not (:subtype card)) "Power"
      (> (.indexOf (:subtype card) "Virus") -1) "Virus"
      :else "Power")))

(defn facedown-card
  "Sab image element of a facedown card"
  ([side] (facedown-card side [] nil))
  ([side class-list alt-alt-text]
  (let [s (.toLowerCase side)
        alt (if (nil? alt-alt-text)
              (str "Facedown " s " card")
              alt-alt-text)
        tag (->> class-list
              vec
              (concat ["img" "card"])
              (join ".")
              keyword)]
    [tag {:src (str "/img/" s ".png")
          :alt alt}])))

(defn card-img
  "Build an image of the card (is always face-up). Only shows the zoomed card image, does not do any interaction."
  [{:keys [code title] :as cursor}]
  (om/component
   (when code
     (sab/html
      [:div.card-frame
       [:div.blue-shade.card {:on-mouse-enter #(put! zoom-channel cursor)
                              :on-mouse-leave #(put! zoom-channel false)}
        (when-let [url (image-url cursor)]
          [:div
           [:span.cardname title]
           [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]])]]))))

(defn face-down?
  "Returns true if the placed card should be drawn face down."
  [{:keys [side type facedown revealed host] :as card}]
  (if (= side "Contestant")
    (and (not= type "Operation")
         (not revealed)
         (not= (:side host) "Challenger"))
    facedown))

(defn card-zoom [card owner]
  (om/component
   (sab/html
    [:div.card-preview.blue-shade
     [:h4 (:title card)]
     (when-let [memory (:memoryunits card)]
       (if (< memory 3)
         [:div.anr-icon {:class (str "mu" memory)} ""]
         [:div.heading (str "Memory: " memory) [:span.anr-icon.mu]]))
     (when-let [cost (:cost card)]
       [:div.heading (str "Cost: " cost)])
     (when-let [discard-cost (:discard card)]
       [:div.heading (str "Discard cost: " discard-cost)])
     (when-let [strength (:strength card)]
       [:div.heading (str "Strength: " strength)])
     (when-let [requirement (:advancementcost card)]
       [:div.heading (str "Advancement requirement: " requirement)])
     (when-let [agenda-point (:agendatpoints card)]
       [:div.heading (str "Agenda points: " agenda-point)])
     (when-let [min-deck-size (:minimumdecksize card)]
       [:div.heading (str "Minimum deck size: " min-deck-size)])
     (when-let [influence-limit (:influencelimit card)]
       [:div.heading (str "Influence limit: " influence-limit)])
     (when-let [influence (:factioncost card)]
       (when-let [faction (:faction card)]
        [:div.heading "Influence "
         [:span.influence
          {:dangerouslySetInnerHTML #js {:__html (apply str (for [i (range influence)] "&#8226;"))}
           :class                   (-> faction .toLowerCase (.replace " " "-"))}]]))
     [:div.text
      [:p [:span.type (str (:type card))] (if (empty? (:subtype card))
                                            "" (str ": " (:subtype card)))]
      [:pre {:dangerouslySetInnerHTML #js {:__html (add-symbols (:text card))}}]]
     (when-let [url (image-url card)]
       [:img {:src url :alt (:title card) :onLoad #(-> % .-target js/$ .show)}])])))

(defn card-view [{:keys [zone code type abilities counter advance-counter advancementcost current-cost subtype
                         advanceable revealed strength current-strength title parties selected hosted
                         side rec-counter facedown locale-target subtype-target icon new challenger-abilities subroutines
                         contestant-abilities]
                  :as cursor}
                 owner {:keys [flipped] :as opts}]
  (om/component
   (sab/html
    [:div.card-frame
     [:div.blue-shade.card {:class (str (when selected "selected") (when new " new"))
                            :draggable (when (not-spectator?) true)
                            :on-touch-start #(handle-touchstart % cursor)
                            :on-touch-end   #(handle-touchend %)
                            :on-touch-move  #(handle-touchmove %)
                            :on-drag-start #(handle-dragstart % cursor)
                            :on-drag-end #(-> % .-target js/$ (.removeClass "dragged"))
                            :on-mouse-enter #(when (or (not (or (not code) flipped facedown))
                                                       (spectator-view-hidden?)
                                                       (= (:side @game-state) (keyword (.toLowerCase side))))
                                               (put! zoom-channel cursor))
                            :on-mouse-leave #(put! zoom-channel false)
                            :on-click #(handle-card-click cursor owner)}
      (when-let [url (image-url cursor)]
        (if (or (not code) flipped facedown)
          (let [facedown-but-known (or (not (or (not code) flipped facedown))
                                       (spectator-view-hidden?)
                                       (= (:side @game-state) (keyword (.toLowerCase side))))
                alt-str (if facedown-but-known (str "Facedown " title) nil)]
            (facedown-card side ["bg"] alt-str))
          [:div
           [:span.cardname title]
           [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]]))
      [:div.counters
       (when counter
         (map (fn [[type num-counters]]
                (when (pos? num-counters)
                  (let [selector (str "div.darkbg." (lower-case (name type)) "-counter.counter")]
                    [(keyword selector) num-counters])))
              counter))
       (when (pos? rec-counter) [:div.darkbg.recurring-counter.counter rec-counter])
       (when (pos? advance-counter) [:div.darkbg.advance-counter.counter advance-counter])]
      (when (and current-strength (not= strength current-strength))
        current-strength [:div.darkbg.strength current-strength])
      (when (get-in cursor [:special :extra-subs])
        [:div.darkbg.extra-subs \+])
      (when-let [{:keys [char color]} icon] [:div.darkbg.icon {:class color} char])
      (when locale-target [:div.darkbg.locale-target locale-target])
      (when subtype-target
        (let [colour-type (case subtype-target
                            ("Barrier" "Sentry") (lower-case subtype-target)
                            "Code Gate" "code-gate"
                            nil)
              label (if (includes? subtype-target " - ")
                      (->> (split subtype-target #" - ")
                           (map first)
                           (join " - "))
                      subtype-target)]
          [:div.darkbg.subtype-target {:class colour-type} label]))
      (when (and (= zone ["hand"]) (#{"Agenda" "Site" "Character" "Region"} type))
        (let [centrals ["Archives" "R&D" "HQ"]
              parties (concat (party-list parties) ["New party"])
              locales (case type
                        ("Region" "Character") (concat centrals parties)
                        ("Agenda" "Site") parties)]
          [:div.panel.blue-shade.locales-menu {:ref "locales"}
           (map (fn [label]
                  [:div {:on-click #(do (send-command "play" {:card @cursor :locale label})
                                        (-> (om/get-node owner "locales") js/$ .fadeOut))}
                   label])
                locales)]))
      (when (pos? (+ (count challenger-abilities) (count subroutines)))
        [:div.panel.blue-shade.challenger-abilities {:ref "challenger-abilities"}
         (map-indexed
          (fn [i ab]
            [:div {:on-click #(do (send-command "challenger-ability" {:card @cursor
                                                                  :ability i}))
                   :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}])
          challenger-abilities)
         (when (> (count subroutines) 1)
           [:div {:on-click #(send-command "system-msg"
                                           {:msg (str "indicates to fire all subroutines on " title)})}
            "Let all subroutines fire"])
         (map (fn [sub]
                [:div {:on-click #(send-command "system-msg"
                                                {:msg (str "indicates to fire the \"" (:label sub)
                                                           "\" subroutine on " title)})
                       :dangerouslySetInnerHTML #js {:__html (add-symbols (str "Let fire: \"" (:label sub) "\""))}}])
              subroutines)])
      (when (pos? (count contestant-abilities))
        [:div.panel.blue-shade.contestant-abilities {:ref "contestant-abilities"}
         (map-indexed
           (fn [i ab]
             [:div {:on-click #(do (send-command "contestant-ability" {:card @cursor
                                                                 :ability i}))
                    :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}])
           contestant-abilities)])
      (let [actions (action-list cursor)
            dynabi-count (count (filter :dynamic abilities))]
        (when (or (> (+ (count actions) (count abilities) (count subroutines)) 1)
                  (some #{"hide" "reveal" "advance"} actions)
                  (= type "Character"))
          [:div.panel.blue-shade.abilities {:ref "abilities"}
           (map (fn [action]
                  [:div {:on-click #(do (send-command action {:card @cursor}))} (capitalize action)])
                actions)
           (map-indexed
            (fn [i ab]
              (if (:dynamic ab)
                [:div {:on-click #(do (send-command "dynamic-ability" (assoc (select-keys ab [:dynamic :source :index])
                                                                             :card @cursor)))
                       :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}]
                [:div {:on-click #(do (send-command "ability" {:card @cursor
                                                               :ability (- i dynabi-count)})
                                      (-> (om/get-node owner "abilities") js/$ .fadeOut))
                       :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}]))
            abilities)
           (map-indexed
            (fn [i sub]
              [:div {:on-click #(do (send-command "subroutine" {:card @cursor :subroutine i})
                                    (-> (om/get-node owner "abilities") js/$ .fadeOut))
                     :dangerouslySetInnerHTML #js {:__html (add-symbols (str "[Subroutine]" (:label sub)))}}])
            subroutines)]))
      (when (#{"locales" "onhost"} (first zone))
        (cond
          (and (= type "Agenda") (>= advance-counter (or current-cost advancementcost)))
          [:div.panel.blue-shade.menu.abilities {:ref "agenda"}
           [:div {:on-click #(send-command "advance" {:card @cursor})} "Advance"]
           [:div {:on-click #(send-command "score" {:card @cursor})} "Score"]]
          (or (= advanceable "always") (and revealed (= advanceable "revealed-only")))
          [:div.panel.blue-shade.menu.abilities {:ref "advance"}
           [:div {:on-click #(send-command "advance" {:card @cursor})} "Advance"]
           [:div {:on-click #(send-command "reveal" {:card @cursor})} "Reveal"]]))]
     (when (pos? (count hosted))
       [:div.hosted
        (for [card hosted]
          (om/build card-view card {:opts {:flipped (face-down? card)}}))])])))

(defn drop-area [side locale hmap]
  (merge hmap {:on-drop #(handle-drop % locale)
               :on-drag-enter #(-> % .-target js/$ (.addClass "dragover"))
               :on-drag-leave #(-> % .-target js/$ (.removeClass "dragover"))
               :on-drag-over #(.preventDefault %)
               :data-locale locale}))

(defn close-popup [event owner ref msg shuffle? deck?]
  (-> (om/get-node owner ref) js/$ .fadeOut)
  (cond
    shuffle? (send-command "shuffle" {:close "true"})
    deck? (send-command "close-deck")
    msg (send-command "system-msg" {:msg msg}))
  (.stopPropagation event))

(defn label [cursor owner opts]
  (om/component
   (sab/html
    (let [fn (or (:fn opts) count)]
      [:div.header {:class (when (> (count cursor) 0) "darkbg")}
       (str (:name opts) " (" (fn cursor) ")")]))))

(defn- this-user?
  [player]
  (= (-> player :user :_id) (-> @app-state :user :_id)))

(defn build-hand-card-view
  [player parties wrapper-class]
  (let [side (get-in player [:identity :side])
        size (count (:hand player))]
    (sab/html
      (map-indexed
        (fn [i card]
          [:div {:class (str
                          (if (and (not= "select" (get-in player [:prompt 0 :prompt-type]))
                                   (this-user? player)
                                   (not (:selected card)) (playable? card))
                            "playable" "")
                          " "
                          wrapper-class)
                 :style {:left (* (/ 320 (dec size)) i)}}
           (if (or (this-user? player)
                   (:openhand player)
                   (spectator-view-hidden?))
             (om/build card-view (assoc card :parties parties))
             (facedown-card side))])
        (:hand player)))))

(defn hand-view [{:keys [player parties popup popup-direction] :as cursor} owner]
  (om/component
   (sab/html
    (let [side (get-in player [:identity :side])
          size (count (:hand player))
          name (if (= side "Contestant") "HQ" "Grip")]
      [:div.hand-container
       [:div.hand-controls
        [:div.panel.blue-shade.hand
         (drop-area (:side @game-state) name {:class (when (> size 6) "squeeze")})
         [:div
          (build-hand-card-view player parties "card-wrapper")]
         (om/build label (:hand player) {:opts {:name name}})]
        (when popup
          [:div.panel.blue-shade.hand-expand
           {:on-click #(-> (om/get-node owner "hand-popup") js/$ .fadeToggle)}
           "+"])]
       (when popup
         [:div.panel.blue-shade.popup {:ref "hand-popup" :class popup-direction}
          [:div
           [:a {:on-click #(close-popup % owner "hand-popup" nil false false)} "Close"]
           [:label (str size " card" (when (not= 1 size) "s") ".")]
           (build-hand-card-view player parties "card-popup-wrapper")
           ]])]))))

(defn show-deck [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
  (send-command "view-deck"))

(defn identity-view [player owner]
  (om/component
   (sab/html
    [:div.blue-shade.identity
     (om/build card-view (:identity player))])))

(defn deck-view [{:keys [identity deck] :as cursor} owner]
  (om/component
   (sab/html
    (let [is-challenger (= "Challenger" (:side identity))
          side (if is-challenger :challenger :contestant)
          name (if is-challenger "Stack" "R&D")
          ref (if is-challenger "stack" "rd")
          menu-ref (str ref "-menu")
          content-ref (str ref "-content")]
      [:div.blue-shade.deck
       (drop-area (:side @game-state) name
                  {:on-click #(-> (om/get-node owner menu-ref) js/$ .toggle)})
       (when (pos? (count deck))
         (facedown-card (:side identity) ["bg"] nil))
       (om/build label deck {:opts {:name name}})
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.menu {:ref menu-ref}
          [:div {:on-click #(do (send-command "shuffle")
                                (-> (om/get-node owner menu-ref) js/$ .fadeOut))} "Shuffle"]
          [:div {:on-click #(show-deck % owner ref)} "Show"]])
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.popup {:ref content-ref}
          [:div
           [:a {:on-click #(close-popup % owner content-ref "stops looking at their deck" false true)}
            "Close"]
           [:a {:on-click #(close-popup % owner content-ref "stops looking at their deck" true true)}
            "Close & Shuffle"]]
          (om/build-all card-view deck {:key :cid})])]))))

(defmulti discard-view #(get-in % [:identity :side]))

(defmethod discard-view "Challenger" [{:keys [discard] :as cursor} owner]
  (om/component
   (sab/html
    [:div.blue-shade.discard
     (drop-area :challenger "Heap" {:on-click #(-> (om/get-node owner "popup") js/$ .fadeToggle)})
     (when-not (empty? discard)
       (om/build card-view (last discard)))
     (om/build label discard {:opts {:name "Heap"}})
     [:div.panel.blue-shade.popup {:ref "popup" :class (if (= (:side @game-state) :challenger) "me" "opponent")}
      [:div
       [:a {:on-click #(close-popup % owner "popup" nil false false)} "Close"]]
      (om/build-all card-view discard {:key :cid})]])))

(defmethod discard-view "Contestant" [{:keys [discard locales] :as cursor} owner]
  (om/component
   (sab/html
    (let [faceup? #(or (:seen %) (:revealed %))
          draw-card #(if (faceup? %)
                       (om/build card-view %)
                       (if (or (= (:side @game-state) :contestant)
                               (spectator-view-hidden?))
                         [:div.unseen (om/build card-view %)]
                         (facedown-card "contestant")))]
      [:div.blue-shade.discard
       (drop-area :contestant "Archives" {:on-click #(-> (om/get-node owner "popup") js/$ .fadeToggle)})

       (when-not (empty? discard) (draw-card (last discard)))

       (om/build label discard {:opts {:name "Archives"
                                       :fn (fn [cursor] (let [total (count cursor)
                                                              face-up (count (filter faceup? cursor))]
                                                          ;; use non-breaking space to keep counts on same line.
                                                          (str face-up "↑ " (- total face-up) "↓")))}})

       [:div.panel.blue-shade.popup {:ref "popup" :class (if (= (:side @game-state) :challenger) "opponent" "me")}
        [:div
         [:a {:on-click #(close-popup % owner "popup" nil false false)} "Close"]
         [:label (let [total (count discard)
                       face-up (count (filter faceup? discard))]
                   (str total " cards, " (- total face-up) " face-down."))]]
        (for [c discard] (draw-card c))]]))))

(defn rfg-view [{:keys [cards name popup] :as cursor} owner]
  (om/component
   (sab/html
    (when-not (empty? cards)
      (let [size (count cards)]
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")
                                    :on-click (when popup #(-> (om/get-node owner "rfg-popup") js/$ .fadeToggle))}
         (map-indexed (fn [i card]
                        [:div.card-wrapper {:style {:left (* (/ 128 size) i)}}
                         [:div (om/build card-view card)]])
                      cards)
         (om/build label cards {:opts {:name name}})

         (when popup
           [:div.panel.blue-shade.popup {:ref "rfg-popup" :class "opponent"}
            [:div
             [:a {:on-click #(close-popup % owner "rfg-popup" nil false false)} "Close"]
             [:label (str size " card" (when (not= 1 size) "s") ".")]]
            (for [c cards] (om/build card-view c))])])))))

(defn play-area-view [{:keys [name player] :as cursor}]
  (om/component
   (sab/html
    (let [cards (:play-area player)
          size (count cards)
          side (get-in player [:identity :side])]
      (when-not (empty? cards)
        [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
         (map-indexed (fn [i card]
                        [:div.card-wrapper {:style {:left (* (/ 128 size) i)}}
                         (if (this-user? player)
                           (om/build card-view card)
                           (facedown-card side))])
                      cards)
         (om/build label cards {:opts {:name name}})])))))

(defn scored-view [{:keys [scored] :as cursor}]
  (om/component
   (sab/html
    (let [size (count scored)]
      [:div.panel.blue-shade.scored.squeeze
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:style {:left (* (/ 128 (dec size)) i)}}
                       [:div (om/build card-view card)]])
                    scored)
       (om/build label scored {:opts {:name "Scored Area"}})]))))

(defn controls
  "Create the control buttons for the side displays."
  ([key] (controls key 1 -1))
  ([key increment decrement]
   (sab/html
     [:div.controls
      [:button.small {:on-click #(send-command "change" {:key key :delta decrement}) :type "button"} "-"]
      [:button.small {:on-click #(send-command "change" {:key key :delta increment}) :type "button"} "+"]])))

(defmulti stats-view #(get-in % [:identity :side]))

(defmethod stats-view "Challenger" [{:keys [user click credit run-credit memory link tag
                                        brain-damage agenda-point tagged hand-size active]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :challenger)]
      [:div.panel.blue-shade.stats {:class (when active "active-player")}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str click " Click" (if (not= click 1) "s" "")) (when me? (controls :click))]
       [:div (str credit " Credit" (if (not= credit 1) "s" "")
                  (when (pos? run-credit)
                    (str " (" run-credit " for run)")))
        (when me? (controls :credit))]
       (let [{:keys [base mod used]} memory
             max-mu (+ base mod)
             unused (- max-mu used)]
         [:div (str unused " of " max-mu " MU unused")
          (when (neg? unused) [:div.warning "!"]) (when me? (controls :memory))])
       [:div (str link " Link Strength") (when me? (controls :link))]
       [:div (str agenda-point " Agenda Point" (when (not= agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str tag " Tag" (if (not= tag 1) "s" "")) (when (or (pos? tag) (pos? tagged)) [:div.warning "!"]) (when me? (controls :tag))]
       [:div (str brain-damage " Brain Damage")
        (when me? (controls :brain-damage))]
       (let [{:keys [base mod]} hand-size]
         [:div (str (+ base mod) " Max hand size")
          (when me? (controls :hand-size))])]))))

(defmethod stats-view "Contestant" [{:keys [user click credit agenda-point bad-publicity has-bad-pub hand-size active]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :contestant)]
      [:div.panel.blue-shade.stats {:class (when active "active-player")}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str click " Click" (if (not= click 1) "s" "")) (when me? (controls :click))]
       [:div (str credit " Credit" (if (not= credit 1) "s" "")) (when me? (controls :credit))]
       [:div (str agenda-point " Agenda Point" (when (not= agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str (+ bad-publicity has-bad-pub) " Bad Publicity")
        (when me? (controls :bad-publicity))]
       (let [{:keys [base mod]} hand-size]
         [:div (str (+ base mod) " Max hand size")
          (when me? (controls :hand-size))])]))))

(defn locale-view [{:keys [locale central-view run] :as cursor} owner opts]
  (om/component
   (sab/html
    (let [content (:content locale)]
      [:div.locale
       (let [characters (:characters locale)
             run-pos (:position run)
             current-character (when (and run (pos? run-pos) (<= run-pos (count characters)))
                           (nth characters (dec run-pos)))
             run-arrow (sab/html [:div.run-arrow [:div]])
             max-hosted (apply max (map #(count (:hosted %)) characters))]
         [:div.characters {:style {:width (when (pos? max-hosted)
                                      (+ 84 3 (* 42 (dec max-hosted))))}}
          (when-let [run-card (:card (:run-effect run))]
            [:div.run-card (om/build card-img run-card)])
          (for [character (reverse characters)]
            [:div.character {:class (when (not-empty (:hosted character)) "host")}
             (om/build card-view character {:opts {:flipped (not (:revealed character))}})
             (when (and current-character (= (:cid current-character) (:cid character)))
               run-arrow)])
          (when (and run (not current-character))
            run-arrow)])
       [:div.content
        (when central-view
          central-view)
        (when (not-empty content)
          (for [card content
                :let [is-first (= card (first content))]]
            [:div.locale-card {:class (str (when central-view "central ")
                                           (when (or central-view
                                                     (and (< 1 (count content))
                                                          (not is-first)))
                                             "shift"))}
             (om/build card-view card {:opts {:flipped (not (:revealed card))}})
             (when (and (not central-view) is-first)
               (om/build label content {:opts opts}))]))]]))))

(defmulti board-view #(get-in % [:player :identity :side]))

(defmethod board-view "Contestant" [{:keys [player run]}]
  (om/component
   (sab/html
    (let [locales (:locales player)
          s (:locale run)
          locale-type (first s)]
      [:div.contestant-board {:class (if (= (:side @game-state) :challenger) "opponent" "me")}
       (for [locale (reverse (get-parties locales))
             :let [num (party->num (first locale))]]
         (om/build locale-view {:locale (second locale)
                                :run (when (= locale-type (str "party" num)) run)}
                   {:opts {:name (party->name (first locale))}}))
       (om/build locale-view {:locale (:hq locales)
                              :central-view (om/build identity-view player)
                              :run (when (= locale-type "hq") run)})
       (om/build locale-view {:locale (:rd locales)
                              :central-view (om/build deck-view player)
                              :run (when (= locale-type "rd") run)})
       (om/build locale-view {:locale (:archives locales)
                              :central-view (om/build discard-view player)
                              :run (when (= locale-type "archives") run)})]))))

(defmethod board-view "Challenger" [{:keys [player run]}]
  (om/component
   (sab/html
    (let [is-me (= (:side @game-state) :challenger)
          centrals (sab/html
                    [:div.challenger-centrals
                     (om/build discard-view player)
                     (om/build deck-view player)
                     (om/build identity-view player)])]
      [:div.challenger-board {:class (if is-me "me" "opponent")}
       (when-not is-me centrals)
       (for [zone [:resource :hazard :radicle :facedown]]
         [:div
          (for [c (zone (:rig player))]
            [:div.card-wrapper {:class (when (playable? c) "playable")}
             (om/build card-view c)])])
       (when is-me centrals)]))))

(defn cond-button [text cond f]
  (sab/html
   (if cond
     [:button {:on-click f} text]
     [:button.disabled text])))

(defn handle-end-turn []
  (let [me ((:side @game-state) @game-state)
        {:keys [base mod]} (:hand-size me)
        max-size (max (+ base mod) 0)]
    (if (> (count (:hand me)) max-size)
      (toast (str "Discard to " max-size " card" (when (not= 1 max-size) "s")) "warning" nil)
      (send-command "end-turn"))))

(defn runnable-locales
  "List of locales the challenger can run on."
  [contestant challenger]
  (let [locales (keys (:locales contestant))
        restricted-locales (keys (get-in challenger [:register :cannot-run-on-locale]))]
    ;; remove restricted locales from all locales to just return allowed locales
    (remove (set restricted-locales) locales)))

(defn button-pane [{:keys [side active-player run end-turn challenger-phase-12 contestant-phase-12 contestant challenger me opponent] :as cursor} owner]
  (reify
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (when-let [autocomp (get-in cursor [side :prompt 0 :choices :autocomplete])]
        (-> "#card-title" js/$ (.autocomplete (clj->js {"source" autocomp})))))

    om/IRenderState
    (render-state [this state]
      (sab/html
        [:div.button-pane {:on-mouse-over #(card-preview-mouse-over % zoom-channel)
                           :on-mouse-out  #(card-preview-mouse-out % zoom-channel)}
         (if-let [prompt (first (:prompt me))]
           [:div.panel.blue-shade
            [:h4 (for [item (get-message-parts (:msg prompt))] (create-span item))]
            (if-let [n (get-in prompt [:choices :number])]
              [:div
               [:div.credit-select
                [:select#credit {:default-value (get-in prompt [:choices :default] 0)}
                 (for [i (range (inc n))]
                   [:option {:value i} i])]]
               [:button {:on-click #(send-command "choice"
                                                  {:choice (-> "#credit" js/$ .val str->int)})}
                "OK"]]
              (cond
                ;; choice of number of credits
                (= (:choices prompt) "credit")
                [:div
                 (when-let [base (:base prompt)]
                   ;; This is the initial trace prompt
                   (if (nil? (:strength prompt))
                     (if (= "contestant" (:player prompt))
                       ;; This is a trace prompt for the contestant, show challenger link + credits
                       [:div.info "Challenger: " (:link challenger) [:span {:class "anr-icon link"}]
                        " + " (:credit challenger) [:span {:class "anr-icon credit"}]]
                       ;; Trace in which the challenger pays first, showing base trace strength and contestant credits
                       [:div.info "Trace: " (when (:bonus prompt) (+ base (:bonus prompt)) base)
                        " + " (:credit contestant) [:span {:class "anr-icon credit"}]])
                     ;; This is a trace prompt for the responder to the trace, show strength
                     (if (= "contestant" (:player prompt))
                       [:div.info "vs Trace: " (:strength prompt)]
                       [:div.info "vs Challenger: " (:strength prompt) [:span {:class "anr-icon link"}]])))
                 [:div.credit-select
                  ;; Inform user of base trace / link and any bonuses
                  (when-let [base (:base prompt)]
                    (if (nil? (:strength prompt))
                      (if (= "contestant" (:player prompt))
                        (let [strength (when (:bonus prompt) (+ base (:bonus prompt)) base)]
                          [:span (str strength " + ")])
                        [:span (:link challenger) " " [:span {:class "anr-icon link"}] (str " + " )])
                      (if (= "contestant" (:player prompt))
                        [:span (:link challenger) " " [:span {:class "anr-icon link"}] (str " + " )]
                        (let [strength (when (:bonus prompt) (+ base (:bonus prompt)) base)]
                          [:span (str strength " + ")]))))
                  [:select#credit (for [i (range (inc (:credit me)))]
                                    [:option {:value i} i])] " credits"]
                 [:button {:on-click #(send-command "choice"
                                                    {:choice (-> "#credit" js/$ .val str->int)})}
                  "OK"]]

                ;; auto-complete text box
                (:card-title (:choices prompt))
                [:div
                 [:div.credit-select
                  [:input#card-title {:placeholder "Enter a card title"
                                      :onKeyUp #(when (= 13 (.-keyCode %))
                                                 (-> "#card-submit" js/$ .click)
                                                 (.stopPropagation %))}]]
                 [:button#card-submit {:on-click #(send-command "choice" {:choice (-> "#card-title" js/$ .val)})}
                  "OK"]]

                ;; choice of specified counters on card
                (:counter (:choices prompt))
                (let [counter-type (keyword (:counter (:choices prompt)))
                      num-counters (get-in prompt [:card :counter counter-type] 0)]
                  [:div
                   [:div.credit-select
                    [:select#credit (for [i (range (inc num-counters))]
                                      [:option {:value i} i])] " credits"]
                   [:button {:on-click #(send-command "choice"
                                                      {:choice (-> "#credit" js/$ .val str->int)})}
                    "OK"]])
                ;; otherwise choice of all present choices
                :else
                (for [c (:choices prompt)]
                  (when (not= c "Hide")
                    (if (string? c)
                      [:button {:on-click #(send-command "choice" {:choice c})}
                       (for [item (get-message-parts c)] (create-span item))]
                      (let [[title code] (extract-card-info (add-image-codes (:title c)))]
                        [:button {:class (when (:rotated c) :rotated)
                                  :on-click #(send-command "choice" {:card @c}) :id code} title]))))))]
           (if run
             (let [s (:locale run)
                   kw (keyword (first s))
                   locale (if-let [n (second s)]
                            (get-in contestant [:locales kw n])
                            (get-in contestant [:locales kw]))]
               (if (= side :challenger)
                 [:div.panel.blue-shade
                  (when-not (:no-action run) [:h4 "Waiting for Contestant's actions"])
                  (if (zero? (:position run))
                    (cond-button "Successful Run" (:no-action run) #(send-command "access"))
                    (cond-button "Continue" (:no-action run) #(send-command "continue")))
                  (cond-button "Jack Out" (not (:cannot-jack-out run))
                               #(send-command "jack-out"))]
                 [:div.panel.blue-shade
                  (when (zero? (:position run))
                    (cond-button "Action before access" (not (:no-action run))
                                 #(send-command "contestant-phase-43")))
                  (cond-button "No more action" (not (:no-action run))
                               #(send-command "no-action"))]))
             [:div.panel.blue-shade
              (if (= (keyword active-player) side)
                (when (and (zero? (:click me)) (not end-turn) (not challenger-phase-12) (not contestant-phase-12))
                  [:button {:on-click #(handle-end-turn)} "End Turn"])
                (when end-turn
                  [:button {:on-click #(send-command "start-turn")} "Start Turn"]))
              (when (and (= (keyword active-player) side)
                         (or challenger-phase-12 contestant-phase-12))
                [:button {:on-click #(send-command "end-phase-12")}
                 (if (= side :contestant) "Mandatory Draw" "Take Clicks")])
              (when (= side :challenger)
                [:div
                 (cond-button "Remove Tag"
                              (and (pos? (:click me))
                                   (>= (:credit me) (- 2 (or (:tag-remove-bonus me) 0)))
                                   (pos? (:tag me)))
                              #(send-command "remove-tag"))
                 [:div.run-button
                  (cond-button "Run" (and (pos? (:click me))
                                          (not (get-in me [:register :cannot-run])))
                               #(-> (om/get-node owner "locales") js/$ .toggle))
                  [:div.panel.blue-shade.locales-menu {:ref "locales"}
                   (map (fn [label]
                          [:div {:on-click #(do (send-command "run" {:locale label})
                                                (-> (om/get-node owner "locales") js/$ .fadeOut))}
                           label])
                        (zones->sorted-names (runnable-locales contestant challenger)))]]])
              (when (= side :contestant)
                (cond-button "Purge" (>= (:click me) 3) #(send-command "purge")))
              (when (= side :contestant)
                (cond-button "Discard Radicle" (and (pos? (:click me))
                                                   (>= (:credit me) (- 2 (or (:discard-cost-bonus me) 0)))
                                                   (or (pos? (:tagged opponent))
                                                       (pos? (:tag opponent))))
                             #(send-command "discard-radicle")))
              (cond-button "Draw" (and (pos? (:click me)) (not-empty (:deck me))) #(send-command "draw"))
              (cond-button "Gain Credit" (pos? (:click me)) #(send-command "credit"))]))]))))

(defn update-audio [{:keys [gameid sfx sfx-current-id] :as cursor} owner]
  ;; When it's the first game played with this state or when the sound history comes from different game, we skip the cacophony
  (let [sfx-last-played (om/get-state owner :sfx-last-played)]
    (when (and (get-in @app-state [:options :sounds])
               (not (nil? sfx-last-played))
               (= gameid (:gameid sfx-last-played)))
      ;; Skip the SFX from queue with id smaller than the one last played, queue the rest
      (let [sfx-to-play (reduce (fn [sfx-list {:keys [id name]}]
                                  (if (> id (:id sfx-last-played))
                                    (conj sfx-list name)
                                    sfx-list)) [] sfx)]
        (play-sfx sfx-to-play (om/get-state owner :soundbank)))))
  ;; Remember the most recent sfx id as last played so we don't repeat it later
  (when sfx-current-id
    (om/set-state! owner :sfx-last-played {:gameid gameid :id sfx-current-id})))

(def contestant-stats
  (let [s #(-> @game-state :stats :contestant)]
    [["Clicks Gained" #(-> (s) :gain :click)]
     ["Credits Gained" #(-> (s) :gain :credit)]
     ["Credits Lost" #(-> (s) :lose :credit)]
     ["Credits by Click" #(-> (s) :click :credit)]
     ["Cards Drawn" #(-> (s) :gain :card)]
     ["Cards Drawn by Click" #(-> (s) :click :draw)]
     ["Damage Done" #(-> (s) :damage :all)]
     ["Cards Revealed" #(-> (s) :cards :revealed)]]))

(def challenger-stats
  (let [s #(-> @game-state :stats :challenger)]
    [["Clicks Gained" #(-> (s) :gain :click)]
     ["Credits Gained" #(-> (s) :gain :credit)]
     ["Credits Lost" #(-> (s) :lose :credit)]
     ["Credits by Click" #(-> (s) :click :credit)]
     ["Cards Drawn" #(-> (s) :gain :card)]
     ["Cards Drawn by Click" #(-> (s) :click :draw)]
     ["Tags Gained" #(-> (s) :gain :tag)]
     ["Runs Made" #(-> (s) :runs :started)]
     ["Cards Accessed" #(-> (s) :access :cards)]]))

(defn show-stat
  "Determines statistic counter and if it should be shown"
  [side]
  (when-let [stat-fn (-> side second)]
    (let [stat (stat-fn)]
      (if (pos? stat) stat "-"))))

(defn build-game-stats
  "Builds the end of game statistics div & table"
  []
  (let [stats (map-longest list nil contestant-stats challenger-stats)]
    [:div
     [:table.win.table
      [:tr.win.th
       [:td.win.th "Contestant"] [:td.win.th]
       [:td.win.th "Challenger"] [:td.win.th]]
      (for [[contestant challenger] stats]
        [:tr [:td (first contestant)] [:td (show-stat contestant)]
         [:td (first challenger)] [:td (show-stat challenger)]])]]))

(defn build-win-box
  "Builds the end of game pop up game end"
  [game-state]
  [:div.win.centered.blue-shade
   [:div
    (:winning-user @game-state) " (" (-> @game-state :winner capitalize)
    (cond
      (= "Decked" (@game-state :reason capitalize))
      (str ") wins due to the Contestant being decked on turn " (:turn @game-state))

      (= "Flatline" (@game-state :reason capitalize))
      (str ") wins by flatline on turn " (:turn @game-state))

      (= "Concede" (@game-state :reason capitalize))
      (str ") wins by concession on turn " (:turn @game-state))

      :else
      (str ") wins by scoring agenda points on turn "  (:turn @game-state)))]
   [:div "Time taken: " (-> @game-state :stats :time :elapsed) " minutes"]
   [:br]
   (build-game-stats)
   [:button.win-right {:on-click #(swap! app-state assoc :win-shown true) :type "button"} "✘"]])

(defn gameboard [{:keys [side active-player run end-turn challenger-phase-12 contestant-phase-12 turn contestant challenger] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      (let [audio-sfx (fn [name] (list (keyword name)
                                       (new js/Howl (clj->js {:src [(str "/sound/" name ".ogg")
                                                                    (str "/sound/" name ".mp3")]}))))]
        {:soundbank
         (apply hash-map (concat
                          (audio-sfx "agenda-score")
                          (audio-sfx "agenda-steal")
                          (audio-sfx "click-advance")
                          (audio-sfx "click-card")
                          (audio-sfx "click-credit")
                          (audio-sfx "click-run")
                          (audio-sfx "click-remove-tag")
                          (audio-sfx "game-end")
                          (audio-sfx "place-contestant")
                          (audio-sfx "place-challenger")
                          (audio-sfx "play-instant")
                          (audio-sfx "reveal-character")
                          (audio-sfx "reveal-other")
                          (audio-sfx "run-successful")
                          (audio-sfx "run-unsuccessful")
                          (audio-sfx "virus-purge")))}))

    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [card (<! zoom-channel)]
              (om/set-state! owner :zoom card)))))

    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (when (get-in cursor [side :prompt 0 :show-discard])
        (-> ".me .discard .popup" js/$ .fadeIn))
      (if (= "select" (get-in cursor [side :prompt 0 :prompt-type]))
        (set! (.-cursor (.-style (.-body js/document))) "url('/img/gold_crosshair.png') 12 12, crosshair")
        (set! (.-cursor (.-style (.-body js/document))) "default"))
      (when (= "card-title" (get-in cursor [side :prompt 0 :prompt-type]))
        (-> "#card-title" js/$ .focus))
      (doseq [{:keys [msg type options]} (get-in cursor [side :toast])]
        (toast msg type options))
      (update-audio cursor owner))

    om/IRenderState
    (render-state [this state]
      (sab/html
       (when (and side contestant challenger)
         (let [me       (assoc ((if (= side :challenger) :challenger :contestant) cursor) :active (and (pos? turn) (= (keyword active-player) side)))
               opponent (assoc ((if (= side :challenger) :contestant :challenger) cursor) :active (and (pos? turn) (not= (keyword active-player) side)))]
           [:div.gameboard
            (when (and (:winner @game-state) (not (:win-shown @app-state)))
              (build-win-box game-state))
            [:div {:class (:background (:options @app-state))}]
            [:div.rightpane
             [:div.card-zoom
              (when-let [card (om/get-state owner :zoom)]
                (om/build card-zoom card))]
             ;; card implementation info
             (when-let [card (om/get-state owner :zoom)]
               (let [implemented (:implementation card)]
                 (case implemented
                   (:full "full") nil
                   [:div.panel.blue-shade.implementation
                    (case implemented
                      nil [:span.unimplemented "Unimplemented"]
                      [:span.impl-msg implemented])])))
             (om/build log-pane cursor)]

            [:div.centralpane
             (om/build board-view {:player opponent :run run})
             (om/build board-view {:player me :run run})]

            [:div.leftpane
             [:div.opponent
              (om/build hand-view {:player opponent :parties (get-parties (:locales contestant))
                                   :popup (= side :spectator) :popup-direction "opponent"})]

             [:div.inner-leftpane
              [:div.left-inner-leftpane
               [:div
                (om/build stats-view opponent)
                (om/build scored-view opponent)]
               [:div
                (om/build scored-view me)
                (om/build stats-view me)]]

              [:div.right-inner-leftpane
               [:div
                (om/build rfg-view {:cards (:rfg opponent) :name "Removed from the game" :popup true})
                (om/build rfg-view {:cards (:rfg me) :name "Removed from the game" :popup true})
                (om/build play-area-view {:player opponent :name "Temporary Zone"})
                (om/build play-area-view {:player me :name "Temporary Zone"})
                (om/build rfg-view {:cards (:current opponent) :name "Current" :popup false})
                (om/build rfg-view {:cards (:current me) :name "Current" :popup false})]
               (when-not (= side :spectator)
                 (om/build button-pane {:side side :active-player active-player :run run :end-turn end-turn :challenger-phase-12 challenger-phase-12 :contestant-phase-12 contestant-phase-12 :contestant contestant :challenger challenger :me me :opponent opponent}))]]

             [:div.me
              (om/build hand-view {:player me :parties (get-parties (:locales contestant))
                                   :popup true :popup-direction "me"})]]]))))))

(om/root gameboard game-state {:target (. js/document (getElementById "gameboard"))})
