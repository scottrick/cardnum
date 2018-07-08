(ns meccg.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [capitalize includes? join lower-case split]]
            [meccg.appstate :refer [app-state]]
            [meccg.auth :refer [avatar] :as auth]
            [meccg.cardbrowser :refer [add-symbols] :as cb]
            [meccg.deckbuilder :refer [influence-dot]]
            [meccg.standard :refer [standard-map]]
            [differ.core :as differ]
            [om.dom :as dom]))

(defonce game-state (atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defn image-url [{:keys [setname ImageName] :as card}]
    (str "/img/cards/" (:setname card) "/" (:ImageName card)))

(defn toastr-options
  "Function that generates the correct toastr options for specified settings"
  [options]
  (js-obj "closeButton" (:close-button options false)
          "debug" false
          "newestOnTop" false
          "progressBar" false
          "positionClass" "toast-card"
          ;; preventDuplicates - identical toasts don't stack when the property is set to true.
          ;; Duplicates are matched to the previous toast based on their message content.
          "preventDuplicates" (:prevent-duplicates options true)
          "onclick" nil
          "showDuration" 300
          "hideDuration" 1000
          ;; timeOut - how long the toast will display without user interaction
          "timeOut" (:time-out options 3000)
          ;; extendedTimeOut - how long the toast will display after a user hovers over it
          "extendedTimeOut" (:time-out options 1000)
          "showEasing" "swing"
          "hideEasing" "linear"
          "showMethod" "fadeIn"
          "hideMethod" "fadeOut"
          "tapToDismiss" (:tap-to-dismiss options true)))

(defn init-game [game side]
  (.setItem js/localStorage "gameid" (:gameid @app-state))
  (swap! game-state merge game)
  (swap! game-state assoc :side side)
  (swap! last-state #(identity @game-state)))

(defn launch-game [game]
  (let [user (:user @app-state)
        side (if (= (get-in game [:challenger :user :_id]) (:_id user))
               :challenger
               (if (= (get-in game [:contestant :user :_id]) (:_id user))
                 :contestant
                 :spectator))]
    (swap! app-state assoc :side side)
    (init-game game side))
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
(def socket (.connect js/io (str js/iourl "/lobby")))
(def socket-channel (chan))
(.on socket "meccg" #(put! socket-channel (js->clj % :keywordize-keys true)))
(.on socket "disconnect" #(notify "Connection to the locale lost. Attempting to reconnect."
                                  "error"))
(.on socket "reconnect" #(when (.-onbeforeunload js/window)
                           (notify "Reconnected to the locale." "success")
                           (.emit socket "meccg" #js {:action "reconnect" :gameid (:gameid @app-state)})))

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

(go (while true
      (let [msg (<! socket-channel)]
        (case (:type msg)
          "rejoin" (launch-game (:state msg))
          ("do" "notification" "quit") (do (swap! game-state (if (:diff msg) #(differ/patch @last-state (:diff msg))
                                                                 #(assoc (:state msg) :side (:side @game-state))))
                                           (swap! last-state #(identity @game-state)))
          nil)
        (reset! lock false))))

(defn send [msg]
  (.emit socket "meccg" (clj->js msg)))

(defn not-spectator? [game-state app-state]
  (#{(get-in @game-state [:contestant :user]) (get-in @game-state [:challenger :user])} (:user @app-state)))

(defn send-command
  ([command] (send-command command nil))
  ([command {:keys [no-lock] :as args}]
   (when (or (not @lock) no-lock)
     (try (js/ga "send" "event" "game" command) (catch js/Error e))
     (when-not no-lock (reset! lock true))
     (send {:action "do" :gameid (:gameid @game-state) :side (:side @game-state)
            :user (:user @app-state)
            :command command :args args}))))

(defn send-msg [event owner]
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)
        $div (js/$ ".gameboard .messages")]
    (when-not (empty? text)
      (send-command "say" {:user (:user @app-state) :text text})
      (.scrollTop $div (+ (.prop $div "scrollHeight") 500))
      (aset input "value" "")
      (.focus input))))

(defn send-typing [event owner]
  "Send a typing event to locale for this user if it is not already set in game state"
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)]
    (if (empty? text)
      (send-command "typingstop" {:user (:user @app-state) :no-lock true})
      (when (not-any? #{(get-in @app-state [:user :username])} (:typing @game-state))
        (send-command "typing" {:user (:user @app-state) :no-lock true})))))

(defn mute-spectators [mute-state]
  (send {:action "mute-spectators" :gameid (:gameid @app-state)
         :user (:user @app-state) :side (:side @game-state) :mutestate mute-state}))

(defn build-exception-msg [msg error]
  (letfn [(build-report-url [error]
            (js/escape (str "Please describe the circumstances of your error here.\n\n\nStack Trace:\n```clojure\n"
                            error
                            "\n```")))]
    (str "<div>"
         msg
         "<br/>"
         "<button type=\"button\" class=\"reportbtn\" style=\"margin-top: 5px\" "
         "onclick=\"window.open('https://github.com/rezwits/meccg/issues/new?body="
         (build-report-url error)
         "');\">Report on GitHub</button></div>")))

(defn toast
  "Display a toast warning with the specified message.
  Sends a command to clear any server side toasts."
  [msg type options]
  true)

(defn toast-old
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
      (.volume (sfx-key soundbank) (/ (js/parseInt (get-in @app-state [:options :sounds-volume])) 100))
      (.play (sfx-key soundbank)))
    (play-sfx (rest sfx) soundbank)))

(defn action-list [{:keys [type Secondary zone revealed tapped wounded rotated inverted advanceable advance-counter advancementcost current-cost] :as card}]
  (-> []
      (#(if (and (and (#{"Character" "Site" "Region"} type)
                      (#{"locales" "onhost"} (first zone)))
                 (not revealed))
          (cons "reveal" %) %))
      (#(if (and (#{"Character"} type)
                 (#{"locales" "onhost"} (first zone))
                 revealed)
          (cons "organize" %) %))
      (#(if (and (and (or (#{"Character"} type)
                          (#{"Ally"} Secondary))
                 (#{"locales" "onhost"} (first zone)))
                 (and revealed (not wounded)))
          (cons "wound" %) %))
      (#(if (and (and (or (#{"Character" "Site" "Region"} type)
                          (#{"Ally"} Secondary))
                      (#{"locales" "onhost"} (first zone)))
                 revealed
                 (or (not tapped) wounded))
          (cons "tap" %) %))
      (#(if (and (and (or (#{"Character" "Site" "Region"} type)
                          (#{"Ally"} Secondary))
                      (#{"locales" "onhost"} (first zone)))
                 (or tapped wounded))
          (cons "untap" %) %))
      (#(if (and (and (= type "Resource")
                      (some (partial = Secondary) ["Greater Item" "Major Item" "Minor Item" "Special Item"])
                      (#{"locales" "onhost"} (first zone)))
                 (and (not rotated) (or (not tapped) inverted)))
          (cons "transfer" %) %))
      (#(if (and (and (= type "Resource")
                      (some (partial = Secondary) ["Greater Item" "Major Item" "Minor Item" "Special Item" "Permanent-event" "Ally"])
                      (#{"locales" "onhost"} (first zone)))
                 (and (not rotated) (or (not tapped) inverted)))
          (cons "rotate" %) %))
      (#(if (and (and (= type "Resource")
                      (some (partial = Secondary) ["Greater Item" "Major Item" "Minor Item" "Special Item" "Permanent-event"])
                      (#{"locales" "onhost"} (first zone)))
                 (and (not inverted) (not rotated) tapped))
          (cons "invert" %) %))
      (#(if (and (and (= type "Resource")
                      (some (partial = Secondary) ["Greater Item" "Major Item" "Minor Item" "Special Item" "Permanent-event"])
                      (#{"locales" "onhost"} (first zone)))
                 (and (not tapped) (not rotated)))
          (cons "tap" %) %))
      (#(if (and (and (= type "Resource")
                      (some (partial = Secondary) ["Ally" "Greater Item" "Major Item" "Minor Item" "Special Item" "Permanent-event"])
                      (#{"locales" "onhost"} (first zone)))
                 (or tapped rotated))
          (cons "untap" %) %))))

(defn handle-abilities [{:keys [abilities facedown side type] :as card} owner]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))]
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
          (send-command (first actions) {:card card})))))

(defn handle-card-click [{:keys [type Secondary zone root] :as card} owner]
  (let [side (:side @game-state)]
    (when (not-spectator? game-state app-state)
      (cond
        ;; Selecting card
        (= (get-in @game-state [side :prompt 0 :prompt-type]) "select")
        (send-command "select" {:card card})
        ;; Card is an identity of player's side
        (and (= (:type card) "Identity")
             (= side (keyword (:side card))))
        (handle-abilities card owner)
        ;; Challenger side
        (= side :challenger)
        (case (first zone)
          "hand" (case type
                   ("Character") (if root
                                   (send-command "play" {:card card :locale root})
                                   (-> (om/get-node owner "locales") js/$ .toggle))
                   ("Region" "Site") (if (< (count (get-in @game-state [:challenger :locales])) 4)
                                       (send-command "play" {:card card :locale "New party"})
                                       (-> (om/get-node owner "locales") js/$ .toggle))
                   ("Resource") (if (some (partial = Secondary) ["Permanent-event"
                                                                 "Long-event" "Short-event"])
                                  (send-command "play" {:card card})
                                  (send-command "equip" {:card card}))
                   (send-command "play" {:card card}))
          ("rig" "current" "onhost" "play-area" "locales") (handle-abilities card owner)
          nil)
        ;; Contestant side
        (= side :contestant)
        (case (first zone)
          "hand" (case type
                   ("Character") (if root
                                   (send-command "play" {:card card :locale root})
                                   (-> (om/get-node owner "locales") js/$ .toggle))
                   ("Region" "Site") (if (< (count (get-in @game-state [:contestant :locales])) 4)
                                       (send-command "play" {:card card :locale "New party"})
                                       (-> (om/get-node owner "locales") js/$ .toggle))
                   ("Resource") (if (some (partial = Secondary) ["Permanent-event"
                                                                 "Long-event" "Short-event"])
                                  (send-command "play" {:card card})
                                  (send-command "equip" {:card card}))
                   (send-command "play" {:card card}))
          ("rig" "locales" "scored" "play-area" "current" "onhost") (handle-abilities card owner)
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
  "Checks if the specified subtype is present in the card.
  Mostly sugar for the has? function."
  [card subtype]
  (has? card :subtype subtype))

(defn playable? [{:keys [title side zone cost type uniqueness abilities] :as card}]
  (let [my-side (:side @game-state)
        me (my-side @game-state)]
    (and (= (keyword (.toLowerCase side)) my-side)

         (cond

           (has-subtype? card "Double")
           (if (>= (:click me) 2) true false)

           (has-subtype? card "Triple")
           (if (>= (:click me) 3) true false)

           (= (:fullCode card) "07036") ; Day Job
           (if (>= (:click me) 4) true false)

           (has-subtype? card "Priority")
           (if (get-in @game-state [my-side :register :spent-click]) false true)

           :else
           true)

         (and (= zone ["hand"])
              (or (not uniqueness) (not (in-play? card)))
              (or (#{"Site" "Region" "Character" "Resource"} type) (>= (:credit me) cost))
              (pos? (:click me))))))

(defn spectator-view-hidden?
  "Checks if spectators are allowed to see hidden information, such as hands and face-down cards"
  []
  (and (get-in @game-state [:options :spectatorhands])
       (not (not-spectator? game-state app-state))))

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

(defn create-face [text symbol class]
  (.replace text (apply str symbol) (str "<img src='" class "'style=\"width:20px;height:20px;\"></img>")))

(defn add-faces [card-text]
  (-> (if (nil? card-text) "" card-text)
      (create-face "roll-1" "img/dice1.png")
      (create-face "roll-2" "img/dice2.png")
      (create-face "roll-3" "img/dice3.png")
      (create-face "roll-4" "img/dice4.png")
      (create-face "roll-5" "img/dice5.png")
      (create-face "roll-6" "img/dice6.png")))

(defn add-regions [card-text]
  (-> (if (nil? card-text) "" card-text)
      (create-face "Borderland" "img/dc/me_bl.png")
      (create-face "Coastal Sea" "img/dc/me_cs.png")
      (create-face "Darkdomain" "img/dc/me_dd.png")
      (create-face "Freedomain" "img/dc/me_fd.png")
      (create-face "Jungle" "img/dc/me_ju.png")
      (create-face "Shadowland" "img/dc/me_sl.png")
      (create-face "Wilderness" "img/dc/me_wi.png")))

(defn create-span-impl [item]
  (if (= "[hr]" item)
    [:hr]
    (if (= "[!]" item)
      [:div.smallwarning "!"]
      (if-let [class (anr-icons item)]
        [:span {:class (str "anr-icon " class)}]
        (if-let [[title fullCode] (extract-card-info item)]
          [:span {:class "fake-link" :id fullCode} title]
          (if (boolean (re-find #"roll-" item))
            [:span {:dangerouslySetInnerHTML #js {:__html (add-faces (add-faces item))}}]
            [:span {:dangerouslySetInnerHTML #js {:__html (add-regions item)}}]))))))

(defn get-non-alt-art [[title cards]]
  {:title title :fullCode (:fullCode (first cards))})

(defn prepare-cards []
  (->> (:cards @app-state)
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

(defn card-image-token-impl [title fullCode]
  (str "$1" ci-open title ci-seperator fullCode ci-close))

(def card-image-token (memoize card-image-token-impl))

(defn card-image-reducer [text card]
  (.replace text (js/RegExp. (find-card-regex (:title card)) "g") (card-image-token (:title card) (:fullCode card))))

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
  (let [fullCode (str (.. e -target -id))]
    (when (pos? (count fullCode))
      fullCode)))

(defn card-preview-mouse-over [e channel]
  (.preventDefault e)
  (when-let [fullCode (get-card-code e)]
    (when-let [card (some #(when (= (:fullCode %) fullCode) %) (:cards @app-state))]
      (put! channel (assoc card :implementation :full))))
  nil)

(defn card-preview-mouse-out [e channel]
  (.preventDefault e)
  (when-let [fullCode (get-card-code e)]
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
        (if-let [game (some #(when (= (:gameid cursor) (:gameid %)) %) (:games @app-state))]
          (when (or (not-spectator? game-state app-state)
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
        side (if (#{"HQ" "R&D" "Archives" "Sites"} locale) "Contestant" "Challenger")]
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

(defn party->num [zone]
  (-> zone str (clojure.string/split #":party") last js/parseInt))

(defn party->name [zone]
  (let [num (party->num zone)]
    (str "Locale " num)))

(defn central->name [zone]
  "Converts a central zone keyword to a string."
  (case (if (keyword? zone) zone (last zone))
    :hq "HQ"
    :rd "R&D"
    :archives "Archives"
    :sites "Sites"
    nil))

(defn zone->name [zone]
  "Converts a zone to a string."
  (or (central->name zone)
      (party->name zone)))

(defn zone->sort-key [zone]
  (case (if (keyword? zone) zone (last zone))
    :sites -4
    :archives -3
    :rd -2
    :hq -1
    (js/parseInt
     (last (clojure.string/split (str zone) #":party")))))

(defn zones->sorted-names [zones]
  (->> zones (sort-by zone->sort-key) (map zone->name)))

(defn get-parties [locales]
  (->> locales
       (filter #(not (#{:hq :rd :archives :sites} (first %))))
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
    [tag {:src (str "/img/" s ".jpg")
          :alt alt}])))

(defn card-img
  "Build an image of the card (is always face-up). Only shows the zoomed card image, does not do any interaction."
  [{:keys [fullCode title] :as cursor}]
  (om/component
   (when fullCode
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
    (and (not= type "Resource")
         (not= type "Hazard"))
    facedown))

(defn card-zoom [card owner]
  (om/component
   (sab/html
    [:div.card-preview.blue-shade
     (when-let [url (image-url card)]
       [:img {:src url :alt (:title card) :onLoad #(-> % .-target js/$ .show)}])])))

(defn card-view [{:keys [zone fullCode type abilities counter advance-counter advancementcost current-cost subtype
                         advanceable revealed tapped rotated strength current-strength title parties selected hosted
                         side rec-counter facedown  locale-target subtype-target icon new challenger-abilities subroutines]
                  :as cursor}
                 owner {:keys [flipped location] :as opts}]
  (om/component
   (sab/html
    [:div.card-frame
     [:div.blue-shade.card {:class (str (when selected "selected") (when new " new"))
                            :draggable (when (not-spectator? game-state app-state) true)
                            :on-touch-start #(handle-touchstart % cursor)
                            :on-touch-end   #(handle-touchend %)
                            :on-touch-move  #(handle-touchmove %)
                            :on-drag-start #(handle-dragstart % cursor)
                            :on-drag-end #(-> % .-target js/$ (.removeClass "dragged"))
                            :on-mouse-enter #(when (or (not (or (not fullCode) flipped facedown))
                                                       (spectator-view-hidden?)
                                                       (= (:side @game-state) (keyword (.toLowerCase side))))
                                               (put! zoom-channel cursor))
                            :on-mouse-leave #(put! zoom-channel false)
                            :on-click #(handle-card-click @cursor owner)}
      (when-let [url (image-url cursor)]
        (if (or (not fullCode) flipped facedown)
          (let [facedown-but-known (or (not (or (not fullCode) flipped facedown))
                                       (spectator-view-hidden?)
                                       (= (:side @game-state) (keyword (.toLowerCase side))))
                alt-str (if facedown-but-known (str "Facedown " title) nil)]
            (if location
              (facedown-card "Locations")
              (facedown-card side ["bg"] alt-str)))
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
      (when-let [{:keys [char color]} icon] [:div.darkbg.icon {:class color} char])
      (when locale-target [:div.darkbg.locale-target locale-target])
      (when subtype-target
        (let [colour-type (case subtype-target
                            ("Barrier" "Sentry") (lower-case subtype-target)
                            "Code Gate" "Code-gate"
                            nil)
              label (if (includes? subtype-target " - ")
                      (->> (split subtype-target #" - ")
                           (map first)
                           (join " - "))
                      subtype-target)]
          [:div.darkbg.subtype-target {:class colour-type} label]))
      (when (and (= zone ["hand"]) (#{"Site" "Character" "Region"} type))
        (let [parties (concat (party-list parties) ["New party"])
              locales (case type
                        ("Site" "Region" "Character") parties)]
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
      (let [actions (action-list cursor)
            dynabi-count (count (filter :dynamic abilities))]
        (when (or (> (+ (count actions) (count abilities) (count subroutines)) 1)
                  (some #{"hide" "advance"} actions)
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
        (for [card hosted]
          [:div.hosted {:class (if (and (:tapped card) (not (:inverted card)))
                                 "tapped"
                                 (if (and (:inverted card) (not (:rotated card)))
                                   "inverted"
                                   (if (:rotated card)
                                     "rotated"
                                     nil)))}
          (om/build card-view card {:opts {:flipped (face-down? card)}})]))])))

(defn drop-area [side locale hmap]
  (merge hmap {:on-drop #(handle-drop % locale)
               :on-drag-enter #(-> % .-target js/$ (.addClass "dragover"))
               :on-drag-leave #(-> % .-target js/$ (.removeClass "dragover"))
               :on-drag-over #(.preventDefault %)
               :data-locale locale}))

(defn close-popup [event owner ref msg shuffle? location? board? fw-dc? deck?]
  (-> (om/get-node owner ref) js/$ .fadeOut)
  (cond
    location? (send-command "close-location")
    shuffle? (send-command "shuffle" {:close "true"})
    board? (send-command "close-sideboard")
    fw-dc? (send-command "close-fw-dc-sb")
    deck? (send-command "close-deck")
    msg (send-command "system-msg" {:msg msg}))
  (.stopPropagation event))

(defn label [cursor owner opts]
  (om/component
   (sab/html
    (let [fn (or (:fn opts) count)]
      [:div.header {:class (when (> (count cursor) 0) "darkbg")}
       (str (:name opts) " (" (fn cursor) ")")]))))

(defn label-without [cursor owner opts]
  (om/component
    (sab/html
      [:div.header {:class (when (> (count cursor) 0) "darkbg")}
       (str (:name opts))])))

(defn build-hand-card-view
  [player parties wrapper-class]
  (let [side (get-in player [:identity :side])
        size (count (:hand player))]
    (sab/html
      (map-indexed
        (fn [i card]
          [:div {:class (str
                          (if (and (not= "select" (get-in player [:prompt 0 :prompt-type]))
                                   (= (:user player) (:user @app-state))
                                   (not (:selected card)) (playable? card))
                            "playable" "")
                          " "
                          wrapper-class)
                 :style {:left (* (/ 320 (dec size)) i)}}
           (if (or (= (:user player) (:user @app-state))
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
         (om/build label (:hand player) {:opts {:name "Hand"}})]
        (when popup
          [:div.panel.blue-shade.hand-expand
           {:on-click #(-> (om/get-node owner "hand-popup") js/$ .fadeToggle)}
           "+"])]
       (when popup
         [:div.panel.blue-shade.popup {:ref "hand-popup" :class popup-direction}
          [:div
           [:a {:on-click #(close-popup % owner "hand-popup" nil false false false false false)} "Close"]
           [:label (str size " card" (when (not= 1 size) "s") ".")]
           (build-hand-card-view player parties "card-popup-wrapper")
           ]])]))))

(defn show-deck [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
  (send-command "view-deck"))

(defn show-sideboard [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
  (send-command "view-sideboard"))

(defn show-fw-dc-sb [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
  (send-command "view-fw-dc-sb"))

(defn show-sites [event owner ref menu terrain region]
  (if (or (= ref "Ch-regions") (= ref "Co-regions"))
    (do (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
        (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
        (-> (om/get-node owner menu) js/$ .toggle)
        (send-command "system-msg" {:msg (str terrain " " region)}))
    (do (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
        (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
        (-> (om/get-node owner menu) js/$ .toggle)
        (send-command "view-location" region))))

(defn show-map [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut))

(defn deck-view [{:keys [identity deck sideboard fw-dc-sb] :as cursor} owner]
  (om/component
   (sab/html
    (let [is-challenger (= "Challenger" (:side identity))
          side (if is-challenger :challenger :contestant)
          name (if is-challenger "Stack" "R&D")
          ref (if is-challenger "Ch-deck" "Ch-deck")
          menu-ref (str ref "-menu")
          content-ref (str ref "-content")
          deck-name (if is-challenger "Stack" "R&D")
          deck-ref (if is-challenger "stack" "rd")
          deck-menu-ref (str deck-ref "-menu")
          deck-content-ref (str deck-ref "-content")
          side-ref (if is-challenger "Ch-board" "Co-board")
          side-menu-ref (str side-ref "-menu")
          side-content-ref (str side-ref "-content")
          fwdc-ref (if is-challenger "Ch-dc-fw" "Co-dc-fw")
          fwdc-menu-ref (str fwdc-ref "-menu")
          fwdc-content-ref (str fwdc-ref "-content")]
      [:div.blue-shade.deck
       (drop-area (:side @game-state) deck-name
                  {:on-click #(-> (om/get-node owner menu-ref) js/$ .toggle)})
       (when (pos? (count deck))
         (facedown-card (:side identity) ["bg"] nil))
       (om/build label deck {:opts {:name "Play"}})
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.menu {:ref menu-ref}
          [:div {:on-click #(do (send-command "shuffle")
                                (-> (om/get-node owner menu-ref) js/$ .fadeOut))} "Shuffle"]
          [:div {:on-click #(show-deck % owner deck-ref)} "Show Deck"]
          [:div {:on-click #(show-sideboard % owner side-ref)} "Sideboard"]
          [:div {:on-click #(show-fw-dc-sb % owner fwdc-ref)} "FW-DC-SB"]])
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.popup {:ref deck-content-ref}
          [:div
           [:a {:on-click #(close-popup % owner deck-content-ref "stops looking at their deck" false false false false true)}
            "Close"]
           [:a {:on-click #(close-popup % owner deck-content-ref "stops looking at their deck" true false false false true)}
            "Close & Shuffle"]]
          (om/build-all card-view deck {:key :cid})])
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.popup {:ref side-content-ref}
          [:div
           [:a {:on-click #(close-popup % owner side-content-ref "stops looking at their sideboard" false false true false false)}
            "Close"](om/build-all card-view sideboard {:key :cid})]
          ]
         )
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.popup {:ref fwdc-content-ref}
          [:div
           [:a {:on-click #(close-popup % owner fwdc-content-ref "stops looking at their fallen-wizard sideboard" false false false true false)}
            "Close"](om/build-all card-view fw-dc-sb {:key :cid})]
          ]
         )
       ]))))

(defmulti discard-view #(get-in % [:identity :side]))

(defmethod discard-view "Challenger" [{:keys [discard locales] :as cursor} owner]
  (om/component
    (sab/html
      (let [faceup? #(or (:seen %) (:revealed %))
            draw-card #(if (faceup? %)
                         (om/build card-view %)
                         (if (or (= (:side @game-state) :challenger)
                                 (spectator-view-hidden?))
                           [:div.unseen (om/build card-view %)]
                           (facedown-card "challenger")))]
        [:div.blue-shade.discard
         (drop-area :challenger "Heap" {:on-click #(-> (om/get-node owner "popup") js/$ .fadeToggle)})

         (when-not (empty? discard) (draw-card (last discard)))

         (om/build label discard {:opts {:name "Discard"
                                         :fn (fn [cursor] (let [total (count cursor)
                                                                face-up (count (filter faceup? cursor))]
                                                            ;; use non-breaking space to keep counts on same line.
                                                            (str face-up "↑ " (- total face-up) "↓")))}})

         [:div.panel.blue-shade.popup {:ref "popup" :class (if (= (:side @game-state) :contestant) "opponent" "me")}
          [:div
           [:a {:on-click #(close-popup % owner "popup" nil false false false false false)} "Close"]
           [:label (let [total (count discard)
                         face-up (count (filter faceup? discard))]
                     (str total " cards, " (- total face-up) " face-down."))]]
          (for [c discard] (draw-card c))]]))))

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

       (om/build label discard {:opts {:name "Discard"
                                       :fn (fn [cursor] (let [total (count cursor)
                                                              face-up (count (filter faceup? cursor))]
                                                          ;; use non-breaking space to keep counts on same line.
                                                          (str face-up "↑ " (- total face-up) "↓")))}})

       [:div.panel.blue-shade.popup {:ref "popup" :class (if (= (:side @game-state) :challenger) "opponent" "me")}
        [:div
         [:a {:on-click #(close-popup % owner "popup" nil false false false false false)} "Close"]
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
                         [:div (if (:hide card)
                                 (facedown-card "challenger")
                                 (om/build card-view card))]])
                      cards)
         (om/build label cards {:opts {:name name}})

         (when popup
           [:div.panel.blue-shade.popup {:ref "rfg-popup" :class "opponent"}
            [:div
             [:a {:on-click #(close-popup % owner "rfg-popup" nil false false false false false)} "Close"]
             [:label (str size " card" (when (not= 1 size) "s") ".")]]
            (for [c cards] (if (:hide c)
                             (facedown-card "challenger")
                             (om/build card-view c)))])])))))

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
                         (if (= (:user player) (:user @app-state))
                           (om/build card-view card)
                           (facedown-card side))])
                      cards)
         (om/build label cards {:opts {:name name}})])))))

(defn score [cursor owner opts]
  (om/component
    (sab/html
      (let [fn (or (:fn opts) count)]
        [:div.header {:class "darkbg"}
         (str (:name opts))]))))

(defn scored-view [{:keys [scored] :as cursor}]
  (om/component
   (sab/html
    (let [size (count scored)]
      [:div.panel.blue-shade.scored.squeeze
       (map-indexed (fn [i card]
                      [:div.card-wrapper {:style {:left (* (/ 128 (dec size)) i)}}
                       [:div (om/build card-view card)]])
                    scored)
       (om/build score scored {:opts {:name "Marshalling Point Pile"}})]))))

(defn controls [key]
  (sab/html
   [:div.controls
    [:button.small {:on-click #(send-command "change" {:key key :delta 1}) :type "button"} "+"]
    [:button.small {:on-click #(send-command "change" {:key key :delta -1}) :type "button"} "-"]]))

(defmulti stats-view #(get-in % [:identity :side]))

(defmethod stats-view "Challenger" [{:keys [user free_gi char_mp ally_mp item_mp fact_mp kill_mp misc_mp
                                            total_mp stage_pt hand-size-base hand-size-modification active]} owner]
  (om/component
   (sab/html
    (let [me? (= (:side @game-state) :challenger)]
      [:div.panel.blue-shade.stats {:class (when active "active-player")}
       [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
       [:div (str free_gi " Free G.I.") (when me? (controls :free_gi))]
       [:div (str stage_pt " Stage pt" (if (not= stage_pt 1) "s" "")) (when me? (controls :stage_pt))]
       [:div (str total_mp " Total MP" (if (not= total_mp 1) "s" "")) (when me? (controls :total_mp))]
       [:div (str char_mp " Character MP" (if (not= char_mp 1) "s" "")) (when me? (controls :char_mp))]
       [:div (str ally_mp " Ally MP" (if (not= ally_mp 1) "s" "")) (when me? (controls :ally_mp))]
       [:div (str item_mp " Item MP" (if (not= item_mp 1) "s" "")) (when me? (controls :item_mp))]
       [:div (str fact_mp " Faction MP" (if (not= fact_mp 1) "s" "")) (when me? (controls :fact_mp))]
       [:div (str kill_mp " Kill MP" (if (not= kill_mp 1) "s" "")) (when me? (controls :kill_mp))]
       [:div (str misc_mp " Misc MP" (if (not= misc_mp 1) "s" "")) (when me? (controls :misc_mp))]
       [:div (str (+ hand-size-base hand-size-modification) " Max hand size")
        (when me? (controls :hand-size-modification))]]))))

(defmethod stats-view "Contestant" [{:keys [user free_gi char_mp ally_mp item_mp fact_mp kill_mp misc_mp
                                            total_mp stage_pt hand-size-base hand-size-modification active]} owner]
  (om/component
    (sab/html
      (let [me? (= (:side @game-state) :contestant)]
        [:div.panel.blue-shade.stats {:class (when active "active-player")}
         [:h4.ellipsis (om/build avatar user {:opts {:size 22}}) (:username user)]
         [:div (str free_gi " Free G.I.") (when me? (controls :free_gi))]
         [:div (str stage_pt " Stage pt" (if (not= stage_pt 1) "s" "")) (when me? (controls :stage_pt))]
         [:div (str total_mp " Total MP" (if (not= total_mp 1) "s" "")) (when me? (controls :total_mp))]
         [:div (str char_mp " Character MP" (if (not= char_mp 1) "s" "")) (when me? (controls :char_mp))]
         [:div (str ally_mp " Ally MP" (if (not= ally_mp 1) "s" "")) (when me? (controls :ally_mp))]
         [:div (str item_mp " Item MP" (if (not= item_mp 1) "s" "")) (when me? (controls :item_mp))]
         [:div (str fact_mp " Faction MP" (if (not= fact_mp 1) "s" "")) (when me? (controls :fact_mp))]
         [:div (str kill_mp " Kill MP" (if (not= kill_mp 1) "s" "")) (when me? (controls :kill_mp))]
         [:div (str misc_mp " Misc MP" (if (not= misc_mp 1) "s" "")) (when me? (controls :misc_mp))]
         [:div (str (+ hand-size-base hand-size-modification) " Max hand size")
          (when me? (controls :hand-size-modification))]]))))

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
            [:div.character {:class (if (and (:tapped character) (not (:wounded character)))
                                      "tapped"
                                      (if (:wounded character)
                                        "wounded"
                                        nil))}
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
            [:div.locale-card {:class (str (when (:tapped card) "tapped ")
                                           (when (or central-view
                                                     (and (< 1 (count content))
                                                          (not is-first)))
                                             "shift"))}
             (om/build card-view card {:opts {:flipped (not (:revealed card)) :location true}})
             (when (and (not central-view) is-first)
               (om/build label-without content {:opts opts}))]))]]))))

(defn location-view [{:keys [identity location] :as cursor} owner]
  (om/component
    (sab/html
      (let [is-challenger (= "Challenger" (:side identity))
            side (if is-challenger :challenger :contestant)
            map-name (if is-challenger "Sites2" "Sites")
            map-ref (if is-challenger "Ch-map" "Co-map-menu")
            map-menu-ref (str map-ref "-menu")
            map-content-ref (str map-ref "-content")
            site-name (if is-challenger "Location2" "Location")
            site-ref (if is-challenger "Ch-sites" "Co-sites")
            site-menu-ref (str site-ref "-menu")
            site-content-ref (str site-ref "-content")
            reg-name (if is-challenger "Regions2" "Regions")
            reg-ref (if is-challenger "Ch-regions" "Co-regions")
            reg-menu-ref (str reg-ref "-menu")
            reg-content-ref (str reg-ref "-content")]
        [:div.blue-shade.deck
         (drop-area (:side @game-state) map-name
                    {:on-click #(-> (om/get-node owner map-menu-ref) js/$ .toggle)})
         (facedown-card "Locations")
         (om/build label-without location {:opts {:name "Location"}})
         (when (= (:side @game-state) side)
           [:div.panel.blue-shade.menu {:ref map-menu-ref}
            [:div {:on-click #(show-map % owner reg-ref)} "Regions"]
            [:div {:on-click #(show-map % owner map-ref)} "Sites"]]
           )
         (when (= (:side @game-state) side)
           [:div.panel.blue-shade.popup {:ref reg-content-ref}
            [:div
             (standard-map show-sites owner reg-ref map-menu-ref)
             [:a {:on-click #(close-popup % owner reg-content-ref "stops looking at the map" false true false false false)}
              "Close"]]
            ])
           [:div.panel.blue-shade.popup {:ref map-content-ref}
            [:div
             (standard-map show-sites owner site-ref map-menu-ref)
             [:a {:on-click #(close-popup % owner map-content-ref "stops looking at the map" false true false false false)}
              "Close"]]
            ]
           [:div.panel.blue-shade.popup {:ref site-content-ref}
            [:div
             [:a {:on-click #(close-popup % owner site-content-ref "stops looking at a region" false true false false false)}
              "Close"]]
            (om/build-all card-view location {:key :cid})
            ]
         ]))))

(defmulti decks-view #(get-in % [:player :identity :side]))

(defmethod decks-view "Contestant" [{:keys [player run]}]
  (om/component
    (sab/html
      (let [locales (:locales player)
            s (:locale run)
            locale-type (first s)]
        [:div.contestant-board {:class (if (= (:side @game-state) :challenger) "opponent" "me")}
         (om/build locale-view {:locale (:sites locales)
                                :central-view (om/build location-view player)
                                :run (when (= locale-type "hq") run)})
         (om/build locale-view {:locale (:rd locales)
                                :central-view (om/build deck-view player)
                                :run (when (= locale-type "rd") run)})
         (om/build locale-view {:locale (:archives locales)
                                :central-view (om/build discard-view player)
                                :run (when (= locale-type "archives") run)})]))))

(defmethod decks-view "Challenger" [{:keys [player run]}]
  (om/component
    (sab/html
        [:div.challenger-board {:class (if (= (:side @game-state) :contestant) "opponent" "me")}
         (om/build location-view player)
         (om/build deck-view player)
         (om/build discard-view player)])))

(defmulti event-view #(get-in % [:player :identity :side]))

(defmethod event-view "Contestant" [{:keys [player run]}]
  (om/component
    (sab/html
      (let [is-me (= (:side @game-state) :contestant)]
        [:div.contestant-rig {:class (if is-me "me" "opponent")}
         (for [zone [:resource :hazard :muthereff :facedown]]
           [:div
            (for [c (zone (:rig player))]
              [:div.card-wrapper {:class (when (playable? c) "playable")}
               (om/build card-view c)])])
         ]))))

(defmethod event-view "Challenger" [{:keys [player run]}]
  (om/component
    (sab/html
      (let [is-me (= (:side @game-state) :challenger)]
        [:div.challenger-rig {:class (if is-me "me" "opponent")}
         (for [zone [:resource :hazard :muthereff :facedown]]
           [:div
            (for [c (zone (:rig player))]
              [:div.card-wrapper {:class (when (playable? c) "playable")}
               (om/build card-view c)])])
         ]))))

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
                     {:opts {:name (party->name (first locale))}}))]))))

(defmethod board-view "Challenger" [{:keys [player run]}]
  (om/component
    (sab/html
      (let [locales (:locales player)
            s (:locale run)
            locale-type (first s)]
        [:div.challenger-board {:class (if (= (:side @game-state) :contestant) "opponent" "me")}
         (for [locale (reverse (get-parties locales))
               :let [num (party->num (first locale))]]
           (om/build locale-view {:locale (second locale)
                                  :run (when (= locale-type (str "party" num)) run)}
                     {:opts {:name (party->name (first locale))}}))]))))

(defn cond-button [text cond f]
  (sab/html
   (if cond
     [:button {:on-click f} text]
     [:button.disabled text])))

(defn handle-end-phase []
  (let [me ((:side @game-state) @game-state)
        max-size (max (+ (:hand-size-base me) (:hand-size-modification me)) 0)]
    (if (> (count (:hand me)) max-size)
      (toast (str "Discard to " max-size " card" (when (not= 1 max-size) "s")) "warning" nil)
      (send-command "end-turn"))))

(defn handle-end-turn []
  (let [me ((:side @game-state) @game-state)
        max-size (max (+ (:hand-size-base me) (:hand-size-modification me)) 0)]
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
                                                  {:choice (-> "#credit" js/$ .val js/parseInt)})}
                "OK"]]
              (cond
                ;; choice of number of credits
                (= (:choices prompt) "credit")
                [:div
                 [:div.credit-select
                  ;; Inform user of base trace / link and any bonuses
                  (when-let [base (:base prompt)]
                    (let [bonus (:bonus prompt 0)
                          preamble (if (pos? bonus) (str base " + " bonus) (str base))]
                      [:span (str preamble " + ")]))
                  [:select#credit (for [i (range (inc (:credit me)))]
                                    [:option {:value i} i])] " credits"]
                 [:button {:on-click #(send-command "choice"
                                                    {:choice (-> "#credit" js/$ .val js/parseInt)})}
                  "OK"]]

                ;; auto-complete text box
                (:card-title (:choices prompt))
                [:div
                 [:div.credit-select
                  [:input#card-title {:placeholder "Enter a card title"
                                      :onKeyUp #(when (= 13 (.-keycode %))
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
                                                      {:choice (-> "#credit" js/$ .val js/parseInt)})}
                    "OK"]])
                ;; otherwise choice of all present choices
                :else
                (for [c (:choices prompt)]
                  (when (not= c "Hide")
                    (if (string? c)
                      [:button {:on-click #(send-command "choice" {:choice c})}
                       (for [item (get-message-parts c)] (create-span item))]
                      (let [[title fullCode] (extract-card-info (add-image-codes (:title c)))]
                        [:button {:class (when (:rotated c) :rotated)
                                  :on-click #(send-command "choice" {:card @c}) :id fullCode} title]))))))]
           (do
             [:div.panel.blue-shade
                  ;; --- Start Turn ---
              (if (and (not= (keyword active-player) side)
                       (zero? (:click me)) end-turn)
                (do
                [:div
                 [:button {:on-click #(send-command "start-turn")} "Start Turn"]
                 (cond-button "Untap All" nil nil)
                 (cond-button "Organization" nil nil)
                 (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                 (cond-button "M/H Phase" nil nil)
                 ])
                (if (and (zero? (:click opponent))
                         (zero? (:click me)))
                  [:div
                   (cond-button "Wait!!!" nil nil)
                   (cond-button "Nothing" nil nil)
                   (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                   (cond-button "Waiting" nil nil)
                   ]
                  )
                )
              (when (<= 90 (:click me) 100) ;; set to 100, opponent at 50
                [:div
                 (cond-button "Untap All" (= (:click me) 100) #(send-command "untap-all")) ;;-5
                 (cond-button "Organization" (= (:click me) 95) #(send-command "org-phase")) ;; -5
                 (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                 (cond-button "M/H Phase" (= (:click me) 90) #(send-command "m-h-phase")) ;; -5
                 ])
              (when (= (:click me) 85)
                [:div
                 [:button {:on-click #(send-command "back-org")} "Back to Organize"]
                 ;; set to 100, and opponent to 50
                 [:button {:on-click #(send-command "next-m-h")} "Next M/H"]
                 (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                 [:button {:on-click #(send-command "site-phase")} "Site Phase"] ;; -5
                 ])
              (when (= (:click me) 80)
                [:div
                 [:button {:on-click #(send-command "back-m-h")} "Back to M/H"]
                 ;; set to 85, and opponent to 45
                 [:button {:on-click #(send-command "next-site")} "Next Site"]
                 (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                 [:button {:on-click #(send-command "eot-phase")} "EOT Phase"]; -5
                 ])
              (when (< 65 (:click me) 80)
                [:div
                 [:button {:on-click #(send-command "back-site")} "Back to Site"]
                 ;; set to 80, and opponent to 25
                 (cond-button "EOT Discard" (= (:click me) 75) #(send-command "eot-discard"));; -5
                 (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                 (cond-button "End of Turn" (and (= (:click me) 70)
                                           (= (keyword active-player) side) (not end-turn)
                                              (not contestant-phase-12) (not challenger-phase-12)
                                           ) #(handle-end-turn))
                 ])

                ;;------BREAK to Hazard Player

              (cond ;;hazard click resets
                (and (> (:click me) 0) (<= 90 (:click opponent) 100)) (send-command "reset-org")
                (and (zero? (:click me)) (= (:click opponent) 85)) (send-command "reset-m-h")
                (and (<= 20 (:click me) 25) (= (:click opponent) 85)) (send-command "reset-m-h")
                (and (zero? (:click me)) (= (:click opponent) 80)) (send-command "reset-site")
                (and (<= 35 (:click me) 45) (= (:click opponent) 80)) (send-command "reset-site")
                (and (<= 20 (:click me) 45) (< 65 (:click opponent) 80)) (send-command "reset-done")
                )
              (when (<= 90 (:click opponent) 100);; set to 50, by me
                [:div
                 (cond-button "Wait!!!" (zero? (:click me)) #(send-command "wait-alert"))
                 (cond-button "Nothing" false nil)
                 (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                 (cond-button "Waiting" false nil)])
              (when (= (:click opponent) 85);; set to 45, by me
                [:div
                 (cond-button "On-guard" (= (:click me) 45) #(send-command "on-guard")) ;; -5
                 (cond-button "No Hazards" (or (= (:click me) 40) (= (:click me) 45)) #(send-command "no-hazards"))
                 ;; set to 35
                 (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                 (cond-button "Next M/H" (= (:click me) 35) #(send-command "reset-m-h"))
                 ;; set to 45, by me
                 ])
              (when (= (:click opponent) 80) ;; set to 25, by me
                [:div
                 ;;(when (> (:click me) 25) (send-command "reset-site"))
                 (cond-button "Reveal On-guard" (= (:click me) 25) #(send-command "reveal-o-g")) ;;-5
                 (cond-button "Bluff On-guard" (= (:click me) 25) #(send-command "bluff-o-g")) ;;-5
                 (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                 (cond-button "Next Site" (or (= (:click me) 20) (= (:click me) 25)) #(send-command "reset-site"))
                 ;; set to 25, by me
                 ])
              (when (< 65 (:click opponent) 80) ;; set to 0, by me
                [:div
                 ;;(when (> (:click me) 0) (send-command "reset-done"))
                 (cond-button "EOT Phase" false nil)
                 (cond-button "EOT Discard" false nil)
                 (cond-button "Draw" (not-empty (:deck me)) #(send-command "draw"))
                 (cond-button "Done" (zero? (:click me)) #(send-command "haz-play-done"))]) ;;-5
         ]))]))))

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
  (om/set-state! owner :sfx-last-played {:gameid gameid :id sfx-current-id}))

(defn gameboard [{:keys [side active-player run end-turn challenger-phase-12 contestant-phase-12 turn contestant challenger] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      (let [audio-sfx (fn [name] (list (keyword name)
                                       (new js/Howl (clj->js {:urls [(str "/sound/" name ".ogg")
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
       (when side
         (let [me       (assoc ((if (= side :challenger) :challenger :contestant) cursor) :active (and (pos? turn) (= (keyword active-player) side)))
               opponent (assoc ((if (= side :challenger) :contestant :challenger) cursor) :active (and (pos? turn) (not= (keyword active-player) side)))]
           [:div.gameboard
            (when (and (:winner @game-state) (not (:win-shown @app-state)))
              [:div.win.centered.blue-shade
               (:winning-user @game-state) " (" (-> @game-state :winner capitalize)
               (cond
                 (= "Decked" (@game-state :reason capitalize))
                 ") wins due to the Contestant being decked"

                 (= "Flatline" (@game-state :reason capitalize))
                 ") wins by flatlining the Challenger"

                 :else
                 ") wins by scoring agenda points")

               [:button.win-right {:on-click #(swap! app-state assoc :win-shown true) :type "button"} "x"]])
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
             (om/build event-view {:player opponent :run run})
             (om/build event-view {:player me :run run})
             (om/build board-view {:player me :run run})]

            [:div.leftpane
             [:div.opponent
              (om/build hand-view {:player opponent :parties (get-parties (:locales (if (= side :challenger) contestant challenger)))
                                   :popup (= side :spectator) :popup-direction "opponent"})]

             [:div.inner-leftpane
              [:div.left-inner-leftpane
               [:div
                (om/build stats-view opponent)
                (om/build scored-view opponent)
                ]
               [:div
                (om/build scored-view me)
                (om/build stats-view me)]
               ]

              [:div.right-inner-leftpane
               [:div
                (om/build decks-view {:player opponent :run run})
                (om/build rfg-view {:cards (:rfg opponent) :name "Removed from play/game" :popup true})
                (om/build rfg-view {:cards (:rfg me) :name "Removed from play/game" :popup true})
                (om/build play-area-view {:player opponent :name "Temporary Zone"})
                (om/build play-area-view {:player me :name "Temporary Zone"})
                (om/build rfg-view {:cards (:current opponent) :name "Current" :popup false})
                (om/build rfg-view {:cards (:current me) :name "Current" :popup false})]
               [:div
               (when-not (= side :spectator)
                 (om/build button-pane {:side side :active-player active-player :run run :end-turn end-turn :challenger-phase-12 challenger-phase-12 :contestant-phase-12 contestant-phase-12 :contestant contestant :challenger challenger :me me :opponent opponent}))
               (om/build decks-view {:player me :run run})]
               ]
              ]

             [:div.me
              (om/build hand-view {:player me :parties (get-parties (:locales (if (= side :challenger) challenger contestant)))
                                   :popup true :popup-direction "me"})]]]))))))

(om/root gameboard game-state {:target (. js/document (getElementById "gameboard"))})
