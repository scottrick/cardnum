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
            [differ.core :as differ]
            [om.dom :as dom]))

(defonce game-state (atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defn image-url [{:keys [side fullCode] :as card}]
  (let [art (or (:art card) ; use the art set on the card itself, or fall back to the user's preferences.
                (get-in @game-state [(keyword (lower-case side)) :user :options :alt-arts (keyword fullCode)]))
        art-options (:alt_art (get (:alt-arts @app-state) fullCode))
        special-user (get-in @game-state [(keyword (lower-case side)) :user :special])
        special-wants-art (get-in @game-state [(keyword (lower-case side)) :user :options :show-alt-art])
        viewer-wants-art (get-in @app-state [:options :show-alt-art])
        show-art (and special-user special-wants-art viewer-wants-art)
        art-available (and art-options (not-empty art-options))
        has-art (and art-options
                     art
                     (contains? art-options (keyword art)))
        version-path (if (and has-art show-art)
                       (get art-options (keyword art) (:fullCode card))
                       (:fullCode card))]
    (str "/img/cards/" (:setname card) "/" (:ImageName card))))

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
(.on socket "disconnect" #(notify "Connection to the server lost. Attempting to reconnect."
                                  "error"))
(.on socket "reconnect" #(when (.-onbeforeunload js/window)
                           (notify "Reconnected to the server." "success")
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
                "[Trash]" "trash"
                "[t]" "trash"})

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
  "Send a typing event to server for this user if it is not already set in game state"
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

(defn action-list [{:keys [type Secondary zone rezzed tapped wounded advanceable advance-counter advancementcost current-cost] :as card}]
  (-> []
      (#(if (and (and (#{"Character" "Site" "Region"} type)
                      (#{"servers" "onhost"} (first zone)))
                 (not rezzed))
          (cons "rez" %) %))
      (#(if (and (and (#{"Character"} type)
                 (#{"servers" "onhost"} (first zone)))
                 (and rezzed (not wounded)))
          (cons "wound" %) %))
      (#(if (and (and (#{"Character" "Site" "Region"} type)
                      (#{"servers" "onhost"} (first zone)))
                 rezzed
                 (or (not tapped) wounded))
          (cons "tap" %) %))
      (#(if (and (and (#{"Character" "Site" "Region"} type)
                      (#{"servers" "onhost"} (first zone)))
                 (or tapped wounded))
          (cons "untap" %) %))
      (#(if (and (and (= type "Resource")
                      (some (partial = Secondary) ["Ally" "Greater Item" "Major Item" "Minor Item" "Special Item"])
                      (#{"servers" "onhost"} (first zone)))
                 (not tapped))
          (cons "tap" %) %))
      (#(if (and (and (= type "Resource")
                      (some (partial = Secondary) ["Ally" "Greater Item" "Major Item" "Minor Item" "Special Item"])
                      (#{"servers" "onhost"} (first zone)))
                 tapped)
          (cons "untap" %) %))))

(defn handle-abilities [{:keys [abilities facedown side type] :as card} owner]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))]
      (cond
        ;; Open panel
        (or (> c 1)
            (some #{"derez" "advance"} actions)
            (and (= type "Character")
                 (not (:run @game-state))))  ; Horrible hack to check if currently in a run
        (-> (om/get-node owner "abilities") js/$ .toggle)
        ;; Trigger first (and only) ability / action
        (= c 1)
        (if (= (count abilities) 1)
          (send-command "ability" {:card card :ability 0})
          (send-command (first actions) {:card card})))))

(defn handle-card-click [{:keys [type zone root] :as card} owner]
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
                   ("Region" "Character") (if root
                                       (send-command "play" {:card card :server root})
                                       (-> (om/get-node owner "servers") js/$ .toggle))
                   ("Agenda" "Site") (if (< (count (get-in @game-state [:challenger :servers])) 4)
                                        (send-command "play" {:card card :server "New remote"})
                                        (-> (om/get-node owner "servers") js/$ .toggle))
                   (send-command "play" {:card card}))
          ("rig" "current" "onhost" "play-area" "servers") (handle-abilities card owner)
          nil)
        ;; Contestant side
        (= side :contestant)
        (case (first zone)
          "hand" (case type
                   ("Region" "Character") (if root
                                       (send-command "play" {:card card :server root})
                                       (-> (om/get-node owner "servers") js/$ .toggle))
                   ("Agenda" "Site") (if (< (count (get-in @game-state [:contestant :servers])) 4)
                                        (send-command "play" {:card card :server "New remote"})
                                        (-> (om/get-node owner "servers") js/$ .toggle))
                   (send-command "play" {:card card}))
          ("rig" "servers" "scored" "play-area" "current" "onhost") (handle-abilities card owner)
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
              (or (#{"Agenda" "Site" "Region" "Character" "Resource"} type) (>= (:credit me) cost))
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

(defn handle-drop [e server]
  (-> e .-target js/$ (.removeClass "dragover"))
  (let [card (-> e .-dataTransfer (.getData "card") ((.-parse js/JSON)) (js->clj :keywordize-keys true))
        side (if (#{"HQ" "R&D" "Archives"} server) "Contestant" "Challenger")]
    (send-command "move" {:card card :server server})))

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

(defn get-card [e server]
  (-> e .-target js/$ (.closest ".card-wrapper")))

(defn get-server-from-touch [touch]
  (let [cX (.. touch -clientX)
        cY (.. touch -clientY)
        server (-> (js/document.elementFromPoint cX cY)
                   js/$
                   (.closest "[data-server]")
                   (.attr "data-server"))]
    [server (> (+ (abs (- (:x @touchmove) cX))
                  (abs (- (:y @touchmove) cY)))
               30)]))

(defn handle-touchstart [e cursor]
  (let [touch (aget (.. e -targetTouches) 0)
        [server _] (get-server-from-touch touch)
        card (get-card e server)]
    (-> card (.addClass "disable-transition"))
    (reset! touchmove {:card (.stringify js/JSON (clj->js @cursor))
                       :x (.. touch -clientX)
                       :y (.. touch -clientY)
                       :start-server server})))

(defn handle-touchmove [e]
  (let [touch (aget (.. e -targetTouches) 0)
        card (get-card e (:start-server @touchmove))]
    (-> card (.css "position" "fixed"))
    (update-card-position card touch)))

(defn handle-touchend [e]
  (let [touch (aget (.. e -changedTouches) 0)
        card (get-card e (:start-server @touchmove))
        [server moved-enough] (get-server-from-touch touch)]
    (release-touch card)
    (when (and server moved-enough (not= server (:start-server @touchmove)))
      (let [cardinfo (-> @touchmove :card ((.-parse js/JSON)) (js->clj :keywordize-keys true))]
        (send-command "move" {:card cardinfo :server server})))))

(defn ability-costs [ab]
  (when-let [cost (:cost ab)]
    (str (clojure.string/join
          ", " (for [c (partition 2 cost)]
                 (str (case (first c)
                        "credit" (str (second c) " [" (capitalize (name (first c))) "]")
                        (clojure.string/join "" (repeat (second c) (str "[" (capitalize (name (first c))) "]"))))))) ": ")))

(defn remote->num [zone]
  (-> zone str (clojure.string/split #":remote") last js/parseInt))

(defn remote->name [zone]
  (let [num (remote->num zone)]
    (str "Server " num)))

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
      (remote->name zone)))

(defn zone->sort-key [zone]
  (case (if (keyword? zone) zone (last zone))
    :archives -3
    :rd -2
    :hq -1
    (js/parseInt
     (last (clojure.string/split (str zone) #":remote")))))

(defn zones->sorted-names [zones]
  (->> zones (sort-by zone->sort-key) (map zone->name)))

(defn get-remotes [servers]
  (->> servers
       (filter #(not (#{:hq :rd :archives} (first %))))
       (sort-by #(zone->sort-key (first %)))))

(defn remote-list [remotes]
  (->> remotes (map first) zones->sorted-names))

(defn card-counter-type [card]
  (let [counter-type (:counter-type card)]
    ;; Determine the appropriate type of counter for styling, falling back to
    ;; power counters when no other type can be inferred.
    (cond
      ;; If an installed card contains an annotation, use it.
      (and (:installed card)
           (not (nil? counter-type)))
      counter-type
      (= "Agenda" (:type card)) "Agenda"
      ;; Assume uninstalled cards with counters are hosted on Personal
      ;; Workshop.
      (not (:installed card)) "Power"
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
  "Returns true if the installed card should be drawn face down."
  [{:keys [side type facedown rezzed host] :as card}]
  (if (= side "Contestant")
    (and (not= type "Resource")
         (not= type "Hazard"))
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
     (when-let [trash-cost (:trash card)]
       [:div.heading (str "Trash cost: " trash-cost)])
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

(defn card-view [{:keys [zone fullCode type abilities counter advance-counter advancementcost current-cost subtype
                         advanceable rezzed tapped strength current-strength title remotes selected hosted
                         side rec-counter facedown  server-target subtype-target icon new challenger-abilities subroutines]
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
      (when server-target [:div.darkbg.server-target server-target])
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
      (when (and (= zone ["hand"]) (#{"Agenda" "Site" "Character" "Region"} type))
        (let [centrals ["Archives" "R&D" "HQ"]
              remotes (concat (remote-list remotes) ["New remote"])
              servers (case type
                        ("Agenda" "Site" "Region" "Character") remotes)]
          [:div.panel.blue-shade.servers-menu {:ref "servers"}
           (map (fn [label]
                  [:div {:on-click #(do (send-command "play" {:card @cursor :server label})
                                        (-> (om/get-node owner "servers") js/$ .fadeOut))}
                   label])
                servers)]))
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
                  (some #{"derez" "advance"} actions)
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
      (when (#{"servers" "onhost"} (first zone))
        (cond
          (and (= type "Agenda") (>= advance-counter (or current-cost advancementcost)))
          [:div.panel.blue-shade.menu.abilities {:ref "agenda"}
           [:div {:on-click #(send-command "advance" {:card @cursor})} "Advance"]
           [:div {:on-click #(send-command "score" {:card @cursor})} "Score"]]
          (or (= advanceable "always") (and rezzed (= advanceable "rezzed-only")))
          [:div.panel.blue-shade.menu.abilities {:ref "advance"}
           [:div {:on-click #(send-command "advance" {:card @cursor})} "Advance"]
           [:div {:on-click #(send-command "rez" {:card @cursor})} "Rez"]]))]
     (when (pos? (count hosted))
        (for [card hosted]
          [:div.hosted {:class (when (:tapped card) "tapped")}
          (om/build card-view card {:opts {:flipped (face-down? card)}})]))])))

(defn drop-area [side server hmap]
  (merge hmap {:on-drop #(handle-drop % server)
               :on-drag-enter #(-> % .-target js/$ (.addClass "dragover"))
               :on-drag-leave #(-> % .-target js/$ (.removeClass "dragover"))
               :on-drag-over #(.preventDefault %)
               :data-server server}))

(defn close-popup [event owner ref msg shuffle? sites? deck?]
  (-> (om/get-node owner ref) js/$ .fadeOut)
  (cond
    shuffle? (send-command "shuffle" {:close "true"})
    sites? (send-command "close-sites")
    deck? (send-command "close-deck")
    msg (send-command "system-msg" {:msg msg}))
  (.stopPropagation event))

(defn label [cursor owner opts]
  (om/component
   (sab/html
    (let [fn (or (:fn opts) count)]
      [:div.header {:class (when (> (count cursor) 0) "darkbg")}
       (str (:name opts) " (" (fn cursor) ")")]))))

(defn build-hand-card-view
  [player remotes wrapper-class]
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
             (om/build card-view (assoc card :remotes remotes))
             (facedown-card side))])
        (:hand player)))))

(defn hand-view [{:keys [player remotes popup popup-direction] :as cursor} owner]
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
          (build-hand-card-view player remotes "card-wrapper")]
         (om/build label (:hand player) {:opts {:name "Hand"}})]
        (when popup
          [:div.panel.blue-shade.hand-expand
           {:on-click #(-> (om/get-node owner "hand-popup") js/$ .fadeToggle)}
           "+"])]
       (when popup
         [:div.panel.blue-shade.popup {:ref "hand-popup" :class popup-direction}
          [:div
           [:a {:on-click #(close-popup % owner "hand-popup" nil false false false)} "Close"]
           [:label (str size " card" (when (not= 1 size) "s") ".")]
           (build-hand-card-view player remotes "card-popup-wrapper")
           ]])]))))

(defn show-deck [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
  (send-command "view-deck"))

(defn show-sideboard [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
  (send-command "view-sideboard"))

(defn show-sites [event owner ref menu terrain region]
  (if (or (= ref "Ch-regions") (= ref "Co-regions"))
    (do (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
        (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
        (-> (om/get-node owner menu) js/$ .toggle)
        (send-command "system-msg" {:msg (str terrain " " region)}))
    (do (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
        (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut)
        (-> (om/get-node owner menu) js/$ .toggle)
        (send-command "view-sites" region))))

(defn show-map [event owner ref]
  (-> (om/get-node owner (str ref "-content")) js/$ .fadeIn)
  (-> (om/get-node owner (str ref "-menu")) js/$ .fadeOut))

(defn deck-view [{:keys [identity deck sideboard] :as cursor} owner]
  (om/component
   (sab/html
    (let [is-challenger (= "Challenger" (:side identity))
          side (if is-challenger :challenger :contestant)
          name (if is-challenger "Stack" "R&D")
          ref (if is-challenger "Ch-menu" "Ch-menu")
          menu-ref (str ref "-menu")
          content-ref (str ref "-content")
          deck-name (if is-challenger "Stack" "R&D")
          deck-ref (if is-challenger "stack" "rd")
          deck-menu-ref (str deck-ref "-menu")
          deck-content-ref (str deck-ref "-content")
          side-ref (if is-challenger "Ch-board" "Co-board")
          side-menu-ref (str side-ref "-menu")
          side-content-ref (str side-ref "-content")]
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
          [:div {:on-click #(show-sideboard % owner side-ref)} "Sideboard"]])
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.popup {:ref deck-content-ref}
          [:div
           [:a {:on-click #(close-popup % owner deck-content-ref "stops looking at their deck" false false true)}
            "Close"]
           [:a {:on-click #(close-popup % owner deck-content-ref "stops looking at their deck" true false true)}
            "Close & Shuffle"]]
          (om/build-all card-view deck {:key :cid})])
       (when (= (:side @game-state) side)
         [:div.panel.blue-shade.popup {:ref side-content-ref}
          [:div
           [:a {:on-click #(close-popup % owner side-content-ref "stops looking at their sideboard" false false true)}
            "Close"](om/build-all card-view sideboard {:key :cid})]
          ]
         )]))))

(defmulti discard-view #(get-in % [:identity :side]))

(defmethod discard-view "Challenger" [{:keys [discard servers] :as cursor} owner]
  (om/component
    (sab/html
      (let [faceup? #(or (:seen %) (:rezzed %))
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
           [:a {:on-click #(close-popup % owner "popup" nil false false false)} "Close"]
           [:label (let [total (count discard)
                         face-up (count (filter faceup? discard))]
                     (str total " cards, " (- total face-up) " face-down."))]]
          (for [c discard] (draw-card c))]]))))

(defmethod discard-view "Contestant" [{:keys [discard servers] :as cursor} owner]
  (om/component
   (sab/html
    (let [faceup? #(or (:seen %) (:rezzed %))
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
         [:a {:on-click #(close-popup % owner "popup" nil false false false)} "Close"]
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
             [:a {:on-click #(close-popup % owner "rfg-popup" nil false false false)} "Close"]
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
                         (if (= (:user player) (:user @app-state))
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
       (om/build label scored {:opts {:name "Marshalling Point Pile"}})]))))

(defn controls [key]
  (sab/html
   [:div.controls
    [:button.small {:on-click #(send-command "change" {:key key :delta 1}) :type "button"} "+"]
    [:button.small {:on-click #(send-command "change" {:key key :delta -1}) :type "button"} "-"]]))

(defmulti stats-view #(get-in % [:identity :side]))

(defmethod stats-view "Challenger" [{:keys [user click credit run-credit memory link tag
                                        brain-damage agenda-point tagged hand-size-base
                                        hand-size-modification active]} owner]
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
       [:div (str memory " Memory Unit" (if (not= memory 1) "s" "")) (when (neg? memory) [:div.warning "!"]) (when me? (controls :memory))]
       [:div (str link " Link Strength") (when me? (controls :link))]
       [:div (str agenda-point " Agenda Point" (when (not= agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str tag " Tag" (if (not= tag 1) "s" "")) (when (or (pos? tag) (pos? tagged)) [:div.warning "!"]) (when me? (controls :tag))]
       [:div (str brain-damage " Brain Damage")
        (when me? (controls :brain-damage))]
       [:div (str (+ hand-size-base hand-size-modification) " Max hand size")
        (when me? (controls :hand-size-modification))]]))))

(defmethod stats-view "Contestant" [{:keys [user click credit agenda-point bad-publicity has-bad-pub
                                      hand-size-base hand-size-modification active]} owner]
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
       [:div (str (+ hand-size-base hand-size-modification) " Max hand size")
        (when me? (controls :hand-size-modification))]]))))

(defn server-view [{:keys [server central-view run] :as cursor} owner opts]
  (om/component
   (sab/html
    (let [content (:content server)]
      [:div.server
       (let [characters (:characters server)
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
             (om/build card-view character {:opts {:flipped (not (:rezzed character))}})
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
            [:div.server-card {:class (when (:tapped card) "tapped")}
             (om/build card-view card {:opts {:flipped (not (:rezzed card)) :location true}})
             (when (and (not central-view) is-first)
               (om/build label content {:opts opts}))]))]]))))

(defn general-map [owner ref menu]
  [:svg {:style {:version "1.1"
                 :id "Layer_1"
                 :x "0px" :y "0px"
                 ;;:enableBackground "new 0 0 702 468;"
                 :background-image "url('/img/map-alt@2x.png')"
                 :background-size "702px 468px"
                 :viewBox "0 0 702 468"
                 :xmlSpace "preserve"
                 :width "702px"
                 :height "468px"}}
   [:g {:id "regiones_x5F_vector_1_"}
    [:g {:on-click #(show-sites % owner ref menu "Shadowland" "Imlad Morgul")} [:path {:className "st0" :d "M505.4,302.6c-0.2-0.4-0.8-0.5-1.2-0.7c-1-0.7-1.7-1.9-2.6-2.6c-0.5-0.4-1.3-0.4-1.7-0.8c-0.4-0.4-0.4-2-0.7-2.5c-0.5-0.8-2.2-0.9-2.8-1.6c-1.7,0-2.6,1.9-4.2,2.5h-0.7c-1,0.3-2.4,1-3.6,1.2c-0.6,0.1-1-0.6-1.8-0.4c-1,0.3-1.8,1.3-2.8,1.8c-0.5,0.2-1.3,0.1-1.6,0.2c-0.1,0.1-0.2,0.2-0.4,0.2v0.5c-0.7,0.1-0.7,0.3-1.2,0.6c0,0.6,0,2.2,0.2,2.6v0.2c0.5-0.1,0.4-0.2,1-0.2c0,0.6,0.4,1.1,0.6,1.6c2.8,0,7.1,0.3,8.4,1.3c0.3-0.1,0.5-0.3,0.7-0.5c0.1-0.3-0.1-0.6,0.1-1c0.1-0.4,0.4-0.6,0.8-0.7c0.1-0.1,2.9-0.5,3-0.5c0.7,0.2,1.2,0.7,1.7,1.1c0.4,0.3,1.7,0.5,2.2,0.6c1.6,0.4,5.5-0.6,6.1,0h0.2c0-0.4-0.1-1.1,0.1-1.3L505.4,302.6L505.4,302.6z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Darkdomain" "Udûn")} [:path {:className "st0" :d "M500.5,272.5c-0.3-0.7-0.9-0.3-1-1.4c-1.4,0.6-2.2-0.6-3.2-1.1c-0.5-0.2-1.3-0.3-1.7-0.5h-1.3c0,0-0.4-0.2-0.5-0.2V269c-0.8,0-1.7-0.1-2.5-0.1c-0.1,0.1-2.3,0.8-2.5,0.8c-0.1,0.3-0.4,0.7-0.5,1.1c-0.7,2.5,0.2,4.5,1.8,5.2c0.6,0.2,1,0,1.7,0.2c0.8,0.3,1.6,1,2.3,1.3c0.8,0.4,1.5,0.3,2.3,0.7c0.4,0.2,0.5,0.8,0.8,1.1c0.2,0,0.3,0.1,0.5,0.1c0.9,0.6,0.6,1.2,2,1.7c0.2-0.2,0.2-0.2,0.5-0.2c-0.1-0.6-0.3-1.3-0.4-2c0.5-0.5,0.8-1.1,1.4-1.4c0-0.7-0.1-1.7,0.2-2.2L500.5,272.5L500.5,272.5z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Darkdomain" "Gorgoroth")} [:path {:className "st0" :d "M568.2,285.1c0-0.8-0.3-0.7-0.6-1.1c-1.5,0-1.8-0.2-2.8-0.5c-0.7-0.2-1.5,0.1-1.9,0.2c-0.7,0.2-1.7-0.1-2-0.2c-0.8-0.3-1.6,0.3-2.3-0.2c-0.9-0.1-1.1-0.9-1.1-1.9c0.6-0.6,0.9-1.3,1.7-1.8v-0.4c-0.8-0.1-2.4,0.4-3.4,0c-0.7-0.3-1-0.7-2-0.6c-0.2,0-0.5,0.4-0.7,0.5c-0.6,0.2-2.4,0.1-3.1-0.1c-0.5-0.1-0.9-0.7-1.3-0.8c-0.5-0.2-3.5-0.5-4.2-0.2c-0.5,0.2-1,0.8-1.6,1h-2.5c-0.8,0.2-1.6,0.6-2.4,0.8c-1.1,0.3-2-0.5-2.6-0.7c-0.9-0.4-2,0.3-2.5,0.5h-2.2c-0.1,0.1-0.2,0.4-0.4,0.5c-1.7,0.6-4.5-0.7-5.5-1c-1.5-0.4-3.3-0.3-4.7-0.6c-1.5-0.3-2.4,0.5-3.2,0.8h-1c-0.3,0.2-0.7,0.8-1.1,1c-1,0.4-2-0.3-3,0c-0.2,0.1-0.8,0.5-1.1,0.6c-1,0.4-2.2-0.2-3,0.2c-0.6,0.3-1.2,1.6-1.6,1.8c-0.6,0.4-1.9,0.1-2.4,0.5c-1.1,0.1-1.4-0.2-1.4-1.2c-0.4-0.2-0.6-0.2-0.7-0.6c-0.8,0.1-1.2,0.6-1.9,0.8c0.1,0.3,0.4,0.6,0.5,0.8c0.7,2.2-2.1,4-3,4.8c-0.1,0.1-0.2,2.2,0,2.8c0.1,0.4,3.3,3,3.7,3.1c0.2,1.2,0,1.3,0.6,2.4c0,0.3,0.1,0.6,0.1,0.8c0.2,0.5,1,0.5,1.3,0.8c0.3,0.4,0.4,0.8,0.8,1.1c0.5,0.3,1.4,0.2,1.9,0.6c0.3,0.2,0.3,0.6,0.6,0.8c0,0.4,0.1,0.4-0.2,0.6c0.2,0.6,0.8,0.7,1,1.2c0,1.1-0.1,2.2-0.1,3.2c0.1,0.2,0.2,1,0.4,1.2c0.2,0.3,0.6,0.4,0.7,0.7c0.1,0.3,0,0.6,0.1,0.8c0.3,0.5,1,0.8,1.4,0.8c0.2,0.4,0.9,1.6,0.7,1.8c-0.3,0.9-1.8,1.4-2.8,1.6c0.1,0.9,0.7,0.9,1.1,1.4c0.3,0.4,0.1,1.3,0.4,1.8c0.4,0.6,1.7,1.2,2.2,1.8c0.3,0.4,0.2,1.1,0.5,1.6c0.4,0.6,2.6,0.9,1.7,2.3c-0.5,1.3-3,1.4-4,2.3c-0.1,0.1,0.1,0.6,0.1,1.1c-0.4,0.3-0.5,0.6-0.8,1c-0.3,0.1-0.3-0.1-0.6,0c-0.4,0.2-1.5,1.4-1.9,1.7c-0.1,0.2,0,0.1-0.1,0.2v0.1c1.4,0.3,4.2-1,4.8-1.9c0.6-0.9,0.7-2.3,1.6-2.9c1-0.7,3-0.5,4.2-1.1c0.8-0.3,1.2-1.2,1.9-1.6c0.4-0.2,0.6,0,1.1-0.2c0.3-0.1,0.8-0.6,1.2-0.7c0.2,0,0.5-0.1,0.7-0.1c0.4-0.3,0.7-0.9,1.2-1.2c0.8-0.4,2-0.6,2.5-1.3c0.4-0.5,0.5-1.4,0.8-1.9c0.8-0.2,2-0.3,2.6,0.2c1.3,0,1.4,1.4,1.3,2.6c0.4,0.1,0.8,0.6,1.1,1c0.8-0.1,0.7-0.7,1.2-1.1c0.3-0.2,0.6-0.2,1-0.4c0-0.1,0.1-0.2,0.1-0.4h0.7c0.6-0.3,1.2-1.2,1.9-1.2c0-0.5,0.1-0.8-0.2-1c0.1-1.3,0.7-0.8,1.1-1.4c0-0.2,0.1-0.5,0.1-0.7c0.3-0.6,1.6-1.9,2.2-2.3c0.2-0.2,0.5,0,0.8-0.1c0.5-0.2,1.6-0.5,2.2-0.7h1.6c1.1-0.3,2.6-0.7,3.5-1.1c0.6-0.2,0.9,0,1.4-0.2c1.1-0.5,1.6-3.2,2.8-4.1c0.7-0.5,1.9-0.8,2.6-1.1h1c0.4-0.1,1.7-0.7,2-1c0.4-0.3,0.6-1,1-1.3c-0.4-0.4-4.2-2.1-4.9-1.7c-1.1,0-1.3-0.3-1.9-0.6c0-0.6-0.3-1.4,0-1.9c0.2-0.4,0.9-0.6,1.2-1c0.1-0.4,0.2-0.7,0.4-1.1c0.5-0.9,2.7-2.9,3.7-3.2h2.2c0-0.1,0.1-0.2,0.1-0.2h0.5v-0.2c0.5-0.3,1.1-0.2,1.6-0.5c0.5-0.3,0.7-1.2,1.6-1.6c0.3,0,0.6-0.1,1-0.1c0.3-0.1,0.8-0.6,1.1-0.7c0.6-0.3,1.2,0,1.9-0.2c0.7-0.3,1.6-0.7,2.3-1.1C568.5,286.1,568.5,285.5,568.2,285.1z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Darkdomain" "Nurn")} [:path {:className "st0" :d "M666.8,287.3c-0.7,0.1-0.7,0.5-1.2,0.7h-1.8c-0.3,0.1-0.7,0.4-1,0.5c-1.2,0.3-2.9,0-4.4,0c-0.6,0-0.8,0.4-1.4,0.2l-0.1-0.2H656c0-0.1-0.1-0.2-0.1-0.2h-1c-1.1-0.4-1.4-2.2-2.3-2.9c-0.5-0.4-1.4-0.2-2-0.5c-0.9-0.4-1.3-1.5-2-2c-1.1-0.8-3.4-1.9-3.8-3.2c-1.4,0.5-2.3,1.4-3.4,2.2c-0.6,0.5-1.4,0.5-2.2,0.8c-0.3,0.1-0.6,0.6-0.8,0.7h-0.8c-2.1,0.8-3.7-0.2-5.5-0.7c-1-0.3-2,0.4-2.6,0.2c-0.4-0.1-1.7-0.3-2.2-0.5c-0.6-0.2-1.1-0.7-1.8-0.8V281c-0.4-0.1-1.5-0.5-2.3-0.2c-0.4,0.2-3,0.5-3.7,0.2c-0.5-0.2-1.8-1.1-2.9-0.6c-0.2,0.2-0.3,0.4-0.5,0.6c-0.3,0.1-0.8,0.2-1,0.2c-1.1,0.3-2.3,0.1-3.2,0.5c-1,0.4-1.6,1.8-2.6,2.3c-1.2,0.5-3.5-0.4-4.3,0.1h-0.2c-0.1,1.2,0.1,2.7-0.4,3.7c-1.9,1.5-3.9,2.6-6.6,1.9c-0.5,0.6-1.3,1.1-1.7,1.8c-0.2,0.4-0.2,0.9-0.5,1.2c-0.4,0.5-2.3,1.3-2.8,1.3c-0.8-0.1-1.6-0.2-2.4-0.2c-0.9,0.3-2.6,0.5-3.6,0.7c-0.5,0.1-0.5-0.5-1.1-0.2c-0.3,0.1-0.6,0.6-0.8,0.7h-0.6c-0.2,0.1-0.5,0.4-0.7,0.5c-1,0.3-2.1,0.2-3,0.6c-0.3,0.1-1.5,1.1-1.8,1.4v0.2c-0.5,0.4-1.9,0.3-2.6,0.6c-0.5,0.2-1.1,0.8-1.6,1c-1.4,0.5-2.9-0.5-4.1,0.4c-0.5,0.4-0.3,1.2-0.6,1.9c-0.2,0.5-0.8,0.7-1.1,1.1c-2.8,0-5.3-2.2-7.1-1.7c-0.8,0.2-1.3-0.3-1.9,0.1c-0.6,0.3-1.5,1.9-2.2,2.3c-1.6,0.9-4.6,1-5.8,2.2c-0.8,0.9-1,3-2.2,3.6c-0.4,0.1-0.8,0.2-1.2,0.2c-1.4,0.7-3.5,1-5.2,1.6h-1.6c-0.5,0.2-1.5,0.4-1.9,0.7c-1.8,1.5-1.7,2.7-2.8,4.6c-0.3,0.5-1.3,1.5-1.9,1.7c-0.4,0.1-0.8-0.1-1.1,0.1c-0.2,0.2-0.2,0.5-0.4,0.7c-0.4,0.4-1.2,0.9-1.7,1.2c0,0.4-0.2,0.6-0.2,0.8c-0.1,0.5,0.2,0.8,0.2,1.3c-0.4,0.4-0.7,1.3-1.2,1.6c-0.5,0.3-0.8,0.2-1.2,0.6c-0.2,0.3,0.1,1-0.1,1.4c-0.5,1-2.2,1-2.5,2.2h-0.1c0.3,0.9,1.4,0.9,1.7,1.8c0.1,0.2-0.5,2.5-0.6,2.8c-0.2,0.4-0.7,0.6-0.8,1.2c-1.8,0.3,0.4,2.2,0.8,2.5c0,1.2-0.8,1.8-1.6,2.2h-1.1c-0.4,0.1-1.3,0.6-1.9,0.2c-1.4,0-1.9,1.1-2.6,1.8c0,0.4-0.1,0.4,0.1,0.7c-0.1,1-3.5,3.8-4.3,4.2c0,0.6,0.4,1.1,0.2,1.7c-0.2,0.7-1.2,1-1.6,1.6c-0.1,0.2-0.4,0.7-0.5,0.8v0.8c-0.6,0.5-1.1,1-1.7,1.6c0,0.4-0.5,1.1-0.2,1.7l0.4,0.1c0.2,0.3,0.1,1.4,0.1,1.4c-0.1,0.6-0.2,0.9-0.5,1.3c-0.1,0-0.2,0.1-0.4,0.1c0.1,0.9,0.2,1.8,0.2,2.8c-0.1,0.5-0.5,1.4-0.6,1.8c0,0.4,0.1,0.8,0.1,1.2c-0.2,0.6-0.7,0.9-0.7,1.7c-0.8,0.4-1.3,0.4-2,0.8c0.2,1.1,0.3,2.2,0.5,3.4c0.3,0.3,1,0.2,1.4,0.4c0.8,0.3,1.5,1.1,2.2,1.6c0,0.4,0.1,0.8,0.1,1.2c-0.6,0.6-1.3,1.5-2,1.9c-1,0.5-2.9,0.4-3.6,0.7c-2.2-0.1-1.5,0.5-2,1.4c-0.1,0.3-0.3,0.3-0.5,0.5h-0.2c0.6,0.4,3,1.3,4.2,0.8c0.8-0.3,2.1-1.1,3.4-0.8c0.9,0.2,1.9,0.4,2.4,1c0.3,0.4,0.5,1,1.1,1.2c1,0.3,2-0.1,2.9,0.2c1,0.4,1.7,1.7,2.2,2.6c0.7,0.1,0.8-0.1,1.3-0.4c0.7-0.3,3.3-0.6,4.2-0.2c0.1,0,1,0.7,1,0.7c0.5-0.6,1.8-1.6,3.2-1.2c0.7,0.2,1.3,0.7,1.9,1c0,0,2.3-0.2,2.5-0.2c2.5-0.9,6.1-1.1,8.3-2.4c0-0.3,0.1-0.4,0.1-0.6c-0.5-0.3-1.3-0.5-1.8-0.8c-0.4-0.3-0.4-0.8-1.1-1c-0.1-0.6-0.2-0.9-0.1-1.6c1-0.1,1.6-0.8,2.4-1.1h0.7c0.6-0.2,1.6-0.7,2.2-1c0.6-0.2,1,0,1.7-0.2c0.5-0.2,1-0.7,1.4-0.8h1.4c0.3-0.1,1.1-0.4,1.7-0.2c1.7,0.5,2.3,2.1,3.7,2.8c0.9,0.4,1.7-0.3,2.6,0c0.5,0.1,0.7,0.8,1.1,1c0.3,0.1,1.1-0.2,1.6-0.1c1.3,0.3,2.8,0.5,3.2,1.8c1,0,2-0.5,2.9-0.2l0.2,0.2h1.2c0.3,0.1,0.7,0.6,1,0.7h1.2c1.7,0.4,3-0.2,4.3-0.7c0.6-0.2,1.4,0.2,1.7,0.2c0.5,0.1,0.7-0.2,1-0.2h2.5c1.2-0.4,2.1-1.2,3.5-1.6c0.8-0.2,2.8,0.3,3.5,0.5h2.8c1.7,0.5,2.6,0,4-0.7c0-0.9,0-2.2-0.2-3c-0.2-0.7-1-1.9-0.6-3.1c0.5-1.9,2.8-3.5,4.1-4.8h0.1c-0.2-0.4-0.6-0.6-0.7-1c-0.2-0.7,0.2-1.2,0-1.8c-0.2-0.4-0.8-1.3-0.4-2c0.4-1,2.2-1.3,3.1-1.7c0.1-0.4,0-1,0.1-1.3c0.4-1,1.4-0.8,2.4-1.2c0.5-0.2,1.2-0.5,1.7-0.8c0.2-0.1,0.4-0.6,0.6-0.7c0.2-0.1,1.8-0.3,2-0.4c1.3-0.3,2.9,0.9,3.7,0.5l0.4-0.1c0-0.2,0-0.2-0.1-0.4c-0.5-0.4-1.4-0.7-2-0.8h-1.2c-0.6-0.2-1.2-0.6-1.7-0.8c0-0.5-0.2-1,0-1.3c0.4-1.2,2.3-1.3,3.5-1.8c0.6-0.2,1.1-0.9,1.7-1.1c0.9-0.3,2.1,0.1,2.6-0.5c1.4,0,2.8-0.1,3.7-0.4c-0.1-0.2,0-0.1-0.1-0.2c-0.5-0.3-2.6,0.1-3.5-0.4c-0.2-0.2-0.3-0.7-0.1-1c0.4-1.1,2.5-1.6,4-1.7c0.1-0.2,0-0.1,0.1-0.2v-0.5c-0.1-0.2-0.6-0.1-0.8-0.2c-0.2-0.1-0.4-0.5-0.6-0.6c0-1.1,0.8-1.4,1.4-1.8c-0.7-0.5-3,0-3.8-0.4l-0.4-0.5c-0.4-0.1-0.7-0.2-1.1-0.4c-0.2-0.1-0.3-0.4-0.6-0.5c-0.4,0-0.7-0.1-1.1-0.1c-0.3-0.1-0.5-0.6-0.8-0.8c-0.1-0.6-0.2-1,0.1-1.4c0.5-0.7,2.3-1.1,3-1.7c0.1-0.2,0.1-0.9,0.2-1.1c0.3-0.6,1-0.4,1.7-0.7c0.2-0.1,0.2-0.4,0.4-0.5c0.3-0.2,1.1-0.5,1.4-0.6c0.9-0.3,1.7,0.1,2.4-0.2c0.8-0.4,1.3-1.4,2.2-1.8c1-0.4,2.1,0.1,2.9,0c2.3-0.3,3.8,0.4,5.3,1.1c0.6,0.3,0.9-0.1,1.6,0.1c0.2,0.1,0.5,0.4,0.7,0.5c1,0.4,2.3-0.2,3.2,0c0.7,0.1,1.4,0.4,1.9,0.6h2.9c1.5,0.4,3.5,0.4,4.3,1.6c1.2-0.1,0.6-0.2,1.1-0.6c0.5-0.4,1.3,0,1.8-0.4c0.1-0.2,0.2-0.4,0.2-0.6c0.9-0.8,1.9-1.8,3.5-1.8c-0.1-1.4-0.2-2.1,0.2-3.4c0.2-0.6-0.2-0.7,0.1-1.2c0.5-0.8,2-0.3,2.8-0.8v-0.2c0.5-0.5,1-1.2,1.6-1.6c0.3-0.2,0.7-0.2,1-0.5c0-0.2,0.1-0.3,0.1-0.5h0.5c0.1-0.3,0.2-0.6,0.4-1c0.3-0.6,0.9-1.2,1.3-1.7c1-1.1,2.1-1,3.2-1.8c0.7-0.4,1.2-1.4,1.7-2c0.4-0.5,1.1-0.9,1.7-1.3c1.2-0.9,1.2-2.6,2.6-3.2c0.3-1.2,0.1-3,0.1-4c0-0.2,0.2-0.5,0.1-0.8c-0.1-0.5-0.6-1.1-0.7-1.6c-0.2-0.6-0.5-3.8-0.5-4.1c0.1-0.5,0.2-2.1,0.7-2.9c0.4-0.5,1.1-0.4,1.3-1.1C670.2,293.1,667,289.2,666.8,287.3z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Shadowland" "Horse Plains")} [:path {:className "st1" :d "M669,277.9c-0.1-0.7-0.9-1.2-1-1.8c0-0.3,0.2-0.6,0.1-1.1c-0.4-1.8,0.2-3.4,0.6-4.7c0.2-0.5,0.1-2.1-0.1-2.4v-0.8c-0.3-0.6-1-0.6-1.6-1c-0.7-0.4-1.2-1.4-1.9-1.8c-0.5-0.3-1-0.4-1.3-0.8c-0.8-1.2,0.1-2.9-0.2-4.2c-0.4-1.8-2.4-2.6-3.7-3.6c-0.6-0.4-1-1-1.7-1.4c0-0.7-0.2-0.9-0.4-1.3c-0.4-1.2,0.1-2-0.4-3.1c-0.2-0.4-0.6-0.8-0.7-1.2c-0.2-0.7,0-1.8-0.2-2.5c-0.2-0.4-0.9-0.7-1.1-1.1c-0.3-0.5-0.1-0.8-0.4-1.3c-0.1-0.1-0.4-0.4-0.5-0.5c-0.5-1.3,0.4-2.2-0.7-3.2c0-1.5,0.7-1.5,1.1-2.5c-0.4-0.5-0.6-1.2-1.1-1.6v-2.2c0.1-0.3,0.8-0.4,1-0.7v-1.2c0.2-0.6,1.1-1.1,1.3-1.7c0.1-0.8,0.2-1.6,0.4-2.4c0.4-0.9,1.3-0.8,2-1.3c0.4-0.4,0.7-0.8,1.1-1.2c0-0.3,0-0.4-0.1-0.6c-0.3-0.9-1.7-1.3-2.5-1.7c-0.2-0.8-0.6-1.7-0.8-2.5v-5.8c0.1-0.4,0.7-1.4,0.7-1.8c0-0.3-0.4-0.4-0.5-0.6c-0.2-0.5-0.1-2.1,0-2.3v-2c-0.4-1-4.1-2.6-5.2-3.4c-0.9-0.7-0.7-2.8-1.1-4.1c-0.7,0-1.8-0.2-2.3-0.1c-0.4,0.1-0.6,0.4-1.1,0.5c-0.1,1.3-1.1,1.9-1.6,2.9c-0.5,0.1-1,0.3-1.4,0.4c-0.2,1.5-1.5,2.3-2.8,2.8h-0.6c-0.2,0.3-0.3,0.6-0.5,0.8c-0.9,0.7-2.1,1.8-3.2,2.3c-1,0.4-2.5,0.2-3.4,0.7c-1.3,0.5-0.9,2.2-1.7,2.9c-0.4,0.3-1,0.4-1.4,0.7c0,0.1-0.1,0.2-0.1,0.4c-0.6,0.5-1.1,0.8-1.7,1.2c-0.3,0.4-0.6,0.7-1,1.1c-0.7,0.5-1.5,0.7-2.2,1.2c-0.3,0.3-0.1,0.8-0.4,1.1l-1,0.8c-0.4,0.2-0.9,0-1.4,0.2c-0.1,0.2-0.2,0.4-0.4,0.6c-0.3,0.2-0.9,0.1-1.2,0.4c-0.7,0.4-0.2,1.1-0.6,1.8c-0.4,0.7-1.5,1.3-2,1.8c0.1,0.4,0.3,0.5,0.4,0.7v2.2c-0.1,0.1-0.2,0.2-0.4,0.2c0,0.5-0.1,1-0.1,1.6c-0.2,0.6-2.2,2.3-2.9,2.5H614c-0.5,0.2-0.9,1-1.4,1.3c0,2-0.8,3.8-2.3,4.3h-2.2c-0.6,0.2-1.3,0.7-1.9,1h-0.7c-1,0.4-3.6,0.6-4.3-0.2h-1.8c-0.9,0.5-2.1,3.9-2.5,5c-0.1,0.3-0.5,0.5-0.6,0.7c0,0.3-0.1,0.6-0.1,0.8c-0.5,0.8-1.6,1.3-2,2c-0.2,0.4,0.1,0.8-0.5,1.2c-0.6,0.4-1.6,0.4-2.2,0.7c-0.6,0.3-1.1,1.4-1.4,2c-1.3,0-2.2,0-3-0.5c-0.4,0.4-1.1,0.6-1.3,1.2c-0.3,0.5-0.1,0.9-0.5,1.2c-0.4,0.4-0.9,0.2-1.2,0.7H583c-0.5,0.3-0.7,1.5-1,2c-0.3,0.5-0.9,0.9-1.2,1.4h-0.4c-0.1,0.9-0.9,1.4-1.3,2c-0.7,0-0.9,0.2-1.4,0.4c-0.4,0-0.8-0.1-1.2-0.1c-0.6,0.1-1.7,0.4-2,0.5c-1.8,0-3.5-0.1-5.3-0.1c-0.9,0.2-2,0.4-3,0.7c-0.3,0.1-0.6,0.5-1,0.6c-0.5,0-1-0.1-1.4-0.1c-0.5,0.1-1.2,0.4-1.7,0.5c-2.3,0.6-4-0.7-5.5-1.1c-0.4,0-0.7,0.1-1.1,0.1c-1.7-0.6-3.6-0.7-5.4-0.2h-1.6c-2,0.6-2.3,2.2-3.2,3.8c-0.2,0.4-2.1,1.2-2.5,1.3c1.2,1.4,3.3-0.1,5.3,0.5c0.5,0.1,1.5,0.3,1.9,0.6c0.1,0.2,0.2,0.3,0.4,0.5c0.6,0.3,1.1-0.2,1.6,0c0.1,0.1,0.4,0.4,0.6,0.5c0.1,0,2.3,0.1,2.3,0.1c0.9-0.3,2.1-1.7,4.1-1.1c2.8,0.9,3.7-0.2,6.2-1.1c0.7-0.3,2.1-1.2,3.4-0.7c2.3,0.8,4.9,1.4,7.2,2.3h2.6c2.2,0,4.1-0.8,5.8-1.3h0.7c1.6-0.6,2-2.5,4.2-3c1.1-0.3,1.4,0.6,2.2,0.6c1.9,0.1,4.6,0.7,6.5,1.2h1c0.6,0.2,1.2,0.8,2.2,0.5c0.6-0.2,1.2-0.5,1.9-0.7l1.1-0.1c0.3-0.1,0.6-0.6,1-0.7c0.6-0.2,1.1,0,1.7-0.2c0.6-0.3,1.3-1,2.2-1.2c0.8-0.2,1.4,0.2,2,0.2c0.4,0.7,0.9,1.3,1.3,1.9c1.1,0.1,2.2,0.2,3.4,0.4c0.1,0.1,0.5,0.5,0.7,0.5c0.1,0,0.8-0.3,1-0.4c0.3,0,1,0.3,1.4,0.1c0.8-0.3,1.5-1.1,2.3-1.3c0.3-0.1,3.5,0.3,3.7,0.4c0.1,0.2,0.2,0.3,0.2,0.5c0.2,0,0.5,0.1,0.7,0.1c0,0.1,0.1,0.2,0.1,0.4c0.6,0.3,1.7,0,2.4,0.2c0.7,0.3,1.8,1,2.4,1.6c2-0.1,6.8-3.4,8.2-0.7c1.8-0.2,2.4-1.8,4.1-1.9c0.3,0,0.2,0.2,0.4,0.2h1.2c0.6,0.2,1.8,0.4,2.4,0.6c0.8,0.2,1.2-0.3,1.6-0.5c0.7-0.3,2.1,0,2.8,0v0.5c1.1,0.1,1.3,0.8,2,1.1c0.9,0.3,1.8-0.1,2.5,0.1c0.5,0.2,0.9,0.8,1.3,1.1c0.1,2.4,0.3,4.5-1.2,5.8c-0.6,0.5-2.3,0.9-2.8,1.3c-2.1,0-5.2-0.4-6.5,0.5c0,0.6,0.1,1.2,0.1,1.8c-0.1,0.3-0.4,1.2-0.4,1.7c0.1,0.5,0.4,0.7,0.5,1.3c1.3,0.6,2.4,1.6,3.5,2.5h0.4c0.4,0.3,0.4,0.9,0.8,1.2c0.5,0.4,0.9,0.1,1.7,0.4c1.2,0.4,2.3,1.8,2.8,3c2.1,0.8,4.9,0.9,7.6,0.1c0.7,0,1.4-0.1,2-0.1c0.9-0.4,1.5-1.4,2-2.2c0.4-0.5,1.8-1.2,1.8-2.2c0-0.9-0.1-2.2-0.1-3.1C668.9,278.4,669.1,278.2,669,277.9z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Borderland" "Dorwinion")} [:path {:className "st2" :d "M642.2,125.8c-0.2,0.1-0.8,0.4-1.3,0.2c-1.1-0.3-2.5-0.9-3.5-1.3c-0.7,0-1.4-0.1-2-0.1c-0.1,0-0.3-0.3-0.4-0.4h-0.8c-0.2-0.1-1-0.4-1.2-0.5l-1.7-0.1c-0.5-0.1-1.4-0.3-1.9-0.2c-0.3,0-1.1,0.3-1.6,0.1c-0.1,0-0.4-0.3-0.5-0.4c-0.4,0-0.8,0.1-1.2,0.1c-0.4-0.1-3.3-0.3-4,0c-0.2,0.1-0.7,0.3-0.8,0.5v0.2h-0.7c-0.8,0.4-1.7,1.1-2.4,1.6c-0.6,0.4-1.4,0.3-2,0.7c-0.3,0.2-0.7,0.7-1.1,0.8c-1.2,0.4-3.2-0.2-4.2,0c-2.2,0.4-5.3,1-8.2,0.7c-1.5-0.1-3.4-0.5-4.8,0h-3.2c-0.5,0.2-1.2,0.7-1.8,0.8c-0.5,0.1-3.4-0.1-4-0.2c-0.7-0.1-0.8,0.4-1.4,0.2c0-0.1-0.1-0.2-0.1-0.2H586c-0.7-0.3-2.7-1.6-3.7-1c-0.3,0-1.9,0.9-2,1.1l-0.1,1c-0.4,0.3-0.9,0.6-1.3,1c-0.1,0.4-0.2,0.9-0.4,1.3c-0.5,0.7-1.6,1.1-2.3,1.6c-0.1,0.3,0,0.9-0.2,1.3c-0.4,0.4-0.9,0.7-1.3,1.1c-0.2,0.3-0.2,0.8-0.5,1.1c-0.6,0.5-1.7,0.5-2.2,0.7c-1.5-0.1-1.8,0.1-2.3,1.1h-0.4c-0.1,0.7-0.4,0.6-0.6,1.1v1.4c-0.2,0.5-1,0.9-1.2,1.3l-0.1,0.8c-0.1,0.3-0.6,0.7-0.7,1.1c-0.3,0.7-0.4,1.3-0.7,2c-0.1,0.3-0.6,0.7-0.7,1.1c-0.1,0.4,0.2,0.6,0.2,0.8c0.1,0.5-0.2,1-0.1,1.3v0.2c0.1,0,0.2,0.1,0.4,0.1c0.2,0.2,0.6,0.9,0.7,1.2c0,0.8,0.1,1.5,0.1,2.3c0.3,0.3,0.6,0.6,1,0.8c0.1,0.2,0.4,0.8,0.5,1v1.1c0.1,0.3,0.6,0.4,0.8,0.6c0.5,0.6,0.5,1.5,1.2,1.9c0,1.4-0.4,1.7-1.2,2.2c0,0.6,0,1.2-0.4,1.4c0.1,0.8,0.3,0.5,0.6,1c0.5,0.8,0,1.3-0.2,1.9c-0.1,0.3,0,0.5-0.1,0.7c0.1,0.8,0.5,0.8,0.7,1.4c0.5,0.7-0.2,1.3-0.4,1.7c-0.2,0.5,0.1,1.1,0,1.3v1c0,0-2.6,1.6-2.6,1.6h-2.6c-0.4,0.1-0.6,0.7-1,0.8c-0.5,0.2-0.8,0-1.3,0.2c-0.4,0.2-0.8,0.7-1.1,1c-0.1,0.1-0.4,0.1-0.5,0.2c-0.3,0.5-0.2,0.9-0.6,1.3c-1.3,0.9-3.3,0.8-4,2.3h-0.1v0.2c0.9-0.2,1.7-0.4,2.3,0.2c0.9-0.2,1.5-1.1,2.3-1.3c0.4-0.1,0.4,0.2,0.5,0.2c0.6,0.2,1.7-0.3,2.5-0.1c1.1,0.2,2.6-0.5,4.1-0.1c1,0.2,1.6,0.8,2.3,1.1c0.5,0.2,0.7-0.1,1.1,0c0.2,0.1,0.8,0.4,1.1,0.5c0.4,0,0.7,0.1,1.1,0.1c0.2,0.1,0.5,0.6,0.7,0.7c0.6,0.2,1.1-0.1,1.6,0.2c0.2,0.1,0.4,0.6,0.6,0.7c1,0,2.1,0.1,3.1,0.1c0.8,0.3,2.1,0.7,2.8,1.2c0.4,0.3,0.6,0.9,1,1.2c-0.1,1.5-1,1.9-2.2,2.4c0.2,0.5,2.3,1.5,2.9,1.8l0.2,2.2c0,0.1-0.2,0.4-0.1,0.7c0.2,0.7,0.9,1.7,0.1,2.5c-0.4,0.3-1.2,0.4-1.9,0.4v0.2c1.7,0,3-0.2,4.3,0.4c1,0.4,1.7,1.8,3.6,1.4c0.6-0.1,0.8-0.7,1.3-1c1.5-0.7,3.2-0.7,5.4-0.7c0-0.3-0.1-0.3-0.1-0.5c-1.5-0.1-3.3-1.2-2.9-3.1c-0.6-0.4-1.8-0.6-2.3-1.2c-0.2-0.4-0.3-0.9-0.5-1.3c-0.2,0-0.4-0.1-0.6-0.1c-0.8-0.5-1.3-1.6-1.6-2.6c0-0.1-0.2-3.2-0.2-3.2c0.1-0.4,0.4-0.8,0.1-1.3c-0.3-0.5-1.2-0.7-1.6-1.2c-0.7-0.9-1-3.7-0.1-4.8c0.6-0.7,1.6-0.7,2.5-1.1c0.4-0.1,1-0.6,1.3-0.7c0.4-0.2,0.8,0,1.2-0.2c0.3-0.2,0.6-0.7,1-0.8c0.5-0.3,0.9-0.2,1.4-0.5c1.1-0.6,4.7-3.9,5.2-4.9v-0.7c0.1-0.6,0.3-1.3,0.6-1.8c0.3-0.6,1.2-1,1.6-1.6h0.4v-0.4c0.7-0.4,1.9-0.6,2.4-1.2c0.6-0.7,0.9-3.5,1.4-4.4c2.3-0.1,2.7,0.3,4,1.6c1.6,0,2.6-0.7,3.8-1.1c1.3-0.5,2.9-0.3,4.7-0.2v-0.5c1-0.1,2-1,2.5-1.7h0.7c0.3-1.1,2.9-0.9,3.6-1.8c0.3-0.4,0.1-0.9,0.4-1.3c0.2-0.4,0.7-0.3,1.1-0.6c0.4-0.6,0.7-1.2,1.1-1.8c0.3-0.9,0.1-2,0.5-2.8c0.3-0.6,1.1-1.4,1.7-1.7c0.4-0.2,0.7,0,1-0.2c0.2-0.2,0.2-0.6,0.4-0.8c0.3-0.2,0.6-0.4,0.8-0.6c0.2-0.3,0.1-0.5,0.2-0.7c0.2-0.3,0.7-0.3,1.1-0.5c0.7-0.4,1.4-1.1,2.2-1.6c0-3.1,0.4-5.4,1.8-7.3c0.7-0.9,2.2-0.8,3.1-1.6c0.2-0.1,0-0.3,0.1-0.5c0.3-0.4,0.9-0.6,1.2-1c0-0.1,0.1-0.2,0.1-0.4c0.3-0.2,0.5-0.1,0.8-0.2c0.5-0.3,1.1-1,1.6-1.3c0.5-0.4,1.1-0.5,1.6-1h0.1C644.9,126.3,643.3,125.4,642.2,125.8z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Southern Rhovanion")} [:path {:className "st3" :d "M595.2,235.8c-0.2-0.5-0.1-1-0.5-1.2c-0.4-0.3-0.9-0.1-1.3-0.4c-0.5-0.3-0.5-1-0.8-1.6c-0.4-0.7-1.7-1.1-2.4-1.4c-0.4-0.9-0.4-2.1-1-2.9c0-0.8,0-1.3,0.4-1.7c0.1,0,0.2-0.1,0.4-0.1V226c0.1-0.1,0.7-0.6,0.8-0.7c0.4-0.2,0.8-0.1,1-0.5V224c0.3-0.5,1-0.8,1.6-1c0.1-0.2,0.3-0.7,0.4-0.8v-1c0.3-0.6,1.4-0.6,1.8-1.1c0.7-0.9,0.6-2.1,1.9-2.4c-0.1-0.1,0,0-0.1-0.1c-0.3-0.5-1-0.7-1.6-1c-0.1-0.4-0.6-1-0.6-1.1c0-0.2,0.1-0.4,0.1-0.6c0-0.9,0.1-2.2-0.4-2.9c-0.2-0.4-0.8-1-1.4-0.8c0,0-0.2,0.2-0.4,0.1c-0.3-0.1-1.3-0.9-1.8-1.1c-1-0.4-1.9,0-2.6-0.6c-4.5,0.1-2.6-1.4-3.8-4.1h-0.1c-0.2-0.3-1.7-1.9-2-2.2c0.1-1,0.2-1.7,0.7-2.5c0.2-0.4,0.9-0.6,1.1-1.1c-0.3-0.8,0.6-2.4,1.1-2.9c-0.4-1.9-3.7-1-4.9-0.8v-0.4c-0.2-0.1-0.5-0.2-0.7-0.2c-0.2-0.7-0.5-1.2-0.6-2c0.3-0.3,0.4-0.7,0.6-1c0.4-0.3,0.9-0.4,1.6-0.4c0-0.5-0.3-0.7-0.4-1.2c-0.1-0.5,0.4-0.8,0.1-1.3c-0.1-1.3-2.3-1.9-3.5-2c-0.1-0.4-0.5-1.2-0.1-1.7c0.3-1.1,1.8-1.1,2.6-1.6v-0.2c-0.8-0.4-1.9-1.2-3-1.3c-0.6-0.1-1.3,0.7-2,0.5c-0.6-0.2-1-0.9-1.6-1.2c-0.6-0.3-1.3-0.1-1.8-0.4c-0.3-0.1-0.4-0.6-0.7-0.7c-0.8-0.3-1.4,0-2.3-0.2c-1-0.3-1.9-1.2-3.1-1.3c-0.7-0.1-1.7,0.5-2.3,0.6c-0.6,0.1-2.6-0.5-3.4-0.1c-0.1,0.1-0.4,0.5-0.6,0.6c-0.5,0.3-0.8,0.2-1.3,0.5v0.2c-1,0.5-1.6-0.7-2.2-0.5v0.2c-0.1,0.1-0.4,0.2-0.5,0.2c-1,0.3-1.8-0.3-2.3-0.6c0-2.5,1.4-4.1,3.1-4.8c0.6-0.2,1.3-0.1,1.8-0.5c0.4-0.3,0.9-1.7,1.6-2.2c1.1-0.8,2.7-0.9,3.7-1.8h2.8c0.2-0.2,0.3-0.5,0.5-0.7c0.2-0.2,0.8-0.4,1-0.6v-1.4c0.1-0.1,0.5-0.6,0.6-0.7c-0.3-0.3-0.8-0.6-1-1.1c-0.7-2.1,1.6-2.2-0.2-3.4c0-0.8-0.1-1.8,0.4-2.5c0-1.1,0.5-1,1-1.6c0.4-0.6-1.4-1.8-1.6-2.3c-0.3-0.8,0-1.3-0.6-1.8c-0.2-0.4-0.2-0.1-0.4-0.2c-1-0.7-0.2-2-0.7-3.2c-0.3-0.6-1.1-1-1.3-1.7c-0.3-1.1,0.4-1.3-0.4-1.8c0.1-2,1.7-3.9,2.3-5.5c0.2-0.6,0-1,0.2-1.4c0.4-0.3,0.7-0.6,1.1-1c0.4-0.7,0.3-1.9,0.8-2.5c-0.2-0.5-0.6-0.5-0.8-0.8l-0.2-0.7c-0.8-1.1-2.7-1.9-3.8-2.6c-0.6-0.4-1-1.6-1.4-2.3c-0.9,0-1.9,0.2-2.9-0.1c-0.1,0-0.4-0.3-0.5-0.4h-1v-0.2c-0.3-0.2-1.1-0.4-1.3-0.5c-0.7-0.2-1.7,0.1-2.3,0.2c-1.2,0.3-3.2,0-4.1-0.2c-1-0.3-1.8,0.3-2.4,0.5c-0.4,0.1-1.1-0.1-1.3-0.1c-0.9-0.1-3.5,1.1-4.3,1.4c-0.4,0.2-1.2,0.6-1.8,0.2c-2.6,0-1.6,1-2.9,1.6h-1.4c-0.2,0.1-0.5,0.4-0.7,0.5c-1.7,0.6-3.8-0.4-4.7-0.8c-0.4-0.2-1.3,0-1.7-0.2c-1.7,0-1-0.5-1.4-1.6c-0.2-0.4-0.7-0.6-1-1c-0.7,0-1.4-0.1-2.2-0.1c-0.6,0.3-0.7,0.8-1.6,1c0,1,1.6,2.8,0.6,4.1c-0.1,0.4-0.4,0.6-0.8,0.7c-0.6,0.4-1.7,0-2.5,0.1c-2.7,0.6-4.1,0.3-6.2-0.6c-0.8-0.3-1.4-0.2-1.9-0.7H511c-0.2,1.5,0.6,1.8,1.1,2.8c0.4,0.8,0.2,1.8,0.6,2.6c0.2,0.5,0.8,0.8,1,1.4c0.5,1.4,0,3.1-0.8,3.6c0.1,0.4,0.2,1.3,0.5,1.7c0.1,0.2,0.4,0.1,0.6,0.2c0.7,0.5,0.7,1.6,0.7,2.8c-0.5,0.5-0.9,1.1-1.7,1.3c0.3,0.5,0.5,1.3,0.8,1.9c0.1,0.1,0.4,0.1,0.5,0.2c0.4,0.4,1.5,2.7,1.3,3.6c-0.1,0.3-0.4,0.7-0.5,1c-0.3,0.8-0.7,1.6-1.4,1.9c-0.9,0.4-2-0.1-3.1,0.2c-0.6,0.2-1.2,0.8-1.8,1.1c0,0.6,0.2,2-0.1,2.5c0,0.4-0.1,0.7-0.4,1c-0.3,0.3-4,1.6-4.9,1.3c-0.4-0.1-0.8-0.5-1.1-0.7c-1.3,2-1.6,3.5-3.6,5c0.1,1.1,1.1,1,1.7,1.7c1,1.1,2.8,2.8,4.2,3.4l1.3,0.1c0.9,0.4,2.3,1.4,2.8,2.3c1.2,2.3,0.4,5.5-1.2,6.7c-0.3,0.2-1.7,0.9-1.9,1h-1c-0.5,0.2-1.3,0.7-1.8,0.8h-1.9c-1.7,0-4,0.1-5,0.7c-1.3,0.8,0.1,3.1-0.8,4.3c-0.3,0.5-0.9,0.6-1.3,1.1c-0.6,0-1.3,0.1-1.6-0.2h-4.3c-0.2,0.1-0.7,2.1-1,2.4c-0.5,0.7-2,0.9-2.8,1.3c-0.1,0.4,0.1,1.4-0.1,1.9c-0.4,1.3-1.8,1.5-2.8,2.3c-0.5,0.4-0.9,1.1-1.6,1.4c0,0.8,0.2,1.2-0.2,1.7c-0.5,0.5-1.5,0.5-1.9,1.1c-0.5,0.7-0.7,1.4-1.2,2.2c-0.2,0.3-0.7,0.5-0.8,0.8c-0.2,0.6,0.2,0.7-0.1,1.2c-0.2,0.2-0.4,0.3-0.6,0.5c-0.1,0.4-0.2,0.8-0.2,1.2c-0.7,2.4,0.4,2.8,1.2,4.4c0,0.2,0.1,0.5,0.1,0.7c0.3,0.4,1,0.7,1.2,1.2c0,0.3,0.1,0.6,0.1,0.8c0.2,0.6,0.8,1,1.1,1.6c0.8,1.5,0.9,3,1.9,4.2c0.1,0,0.2,0.1,0.4,0.1c0.2,0.2,0.2,0.6,0.5,0.7c0.2,0,0.4,0.1,0.6,0.1c0.5,0.3,0.8,1,1.2,1.3c0.7,0.5,1.4,0.8,2.2,1.3c0.5,0.3,0.5,1,1,1.4c0.4,0.4,1.2,0.6,1.6,1.1c0.3,0.5,0.3,1,0.6,1.4c0.2,0.3,0.7,0.5,1,0.8c0.3,0.5,0.3,1.1,0.6,1.6c0.3,0.4,1.2,0.4,1.4,0.8c0.2,0.3,0.1,0.6,0.4,0.8c0.6,0.7,1.6,1.1,2.2,1.8c0.7,0.8,1.4,1.9,2.2,2.6c0.8,0.8,2,0.5,3.2,1c0.4,0.2,1,0.7,1.3,0.8c0.7,0.4,1.4,0.3,2,0.7c0.7,0.4,0.9,1.2,1.4,1.7c1.9,1.5,3.5,3.1,5.5,4.4c0.6,0.4,1.1,1.3,1.7,1.7c0.8,0.5,1.6,0.8,2.5,1.2c1.3,0.6,2.4,1.7,3.8,2.3c1.7,0.6,3.9,0.8,5.6,1.3c0.6,0.2,1.4,0,1.7,0c0.4,0.1,0.9,0.2,1.1,0.2h2.6c0.1,0,0.4,0.4,0.5,0.5c0.5,0,1-0.1,1.6-0.1c1.1,0.3,2.3,0.7,3.6,0.6c1.3,0.7,3.1,1.4,4.6,1.8c1.2,0.3,3.9-0.4,4.7,0c0.5,0.2,1.5,0.9,2,1.1c0.8,0.3,1.1-0.2,1.9,0c0.1,0,0.1,0.3,0.4,0.2c0.1-0.2,0.2-0.3,0.4-0.5c0.7,0,1.4-0.1,2.2-0.1c0.1,0,0.4-0.3,0.5-0.4c0.8,0,1.6,0.1,2.4,0.1c2-0.4,5.7,1,7.9,1.6c0.9,0.2,1.3-0.3,1.7-0.5c0.6-0.3,1.3,0.3,1.6,0.2c1.1-0.1,2.7-1,4-1.3c0.9,0,1.8,0.1,2.6,0.1c1.8,0,3.9,0,5.4-0.5c0.8-0.3,1.7,0.1,2.3-0.4c0.7-0.3,0.5-0.7,0.8-1.3c0.2-0.3,0.5-1,0.7-1.3c0.2-0.2,0.4-0.3,0.6-0.5c0.2-0.5,0-0.9,0.2-1.3c0.2-0.4,0.7-0.6,1-1c0.3,0,0.4,0,0.6-0.1c2.2,0,1.7-2.3,3.2-2.9c1.3-0.9,2.9,0.3,3.4,0.1c0.6-0.4,0.8-1.4,1.4-1.8c0.3-0.2,1.1,0,1.4-0.2c0.7-0.3,0.9-1.5,1.4-2c0.3-0.3,1.2-0.3,1.4-0.6c0.1-0.1,0.1-1,0.2-1.2c0.2-0.2,0.5-0.4,0.7-0.6c0-0.2,0.1-0.4,0.1-0.6c0.2-0.3,0.6-0.8,0.7-1.1c0.4-0.8,0.2-1.6,0.7-2.2C597.1,236.5,595.8,237.1,595.2,235.8z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Northern Rhovanion")} [:path {:className "st3" :d "M580.1,125.8c-0.5,0-1-0.1-1.4-0.1c-0.3-0.1-0.7-0.6-1-0.7c-0.3-0.2-0.6-0.1-1-0.2c-0.6-0.3-1-1.1-1.4-1.4c-0.2,0-0.4-0.1-0.6-0.1c-0.2-0.1-0.1-0.5-0.2-0.6c-0.2-0.2-0.5,0-0.8-0.1c-0.1-0.1-0.4-0.4-0.5-0.5c-1.2-0.6-3.5-0.3-4.8-0.6h-1.7c-0.3-0.1-1.1-0.3-1.4-0.5c-0.3-0.2-0.6-0.8-1.1-1c-0.3-0.1-0.9,0.3-1.3,0.1c-0.2-0.1-0.1-0.2-0.5-0.2c-0.1-0.3-0.4-0.4-0.5-0.5c-0.3-0.5-0.1-1.4-0.5-1.8c-0.2-0.3-0.7-0.3-1-0.5c-0.2-0.3-0.4-0.6-0.6-1c-0.6-0.4-1.3-0.3-1.8-0.7c-0.4-0.3-0.6-0.8-1.1-1.1c0-1.8-0.8-3.2-1.1-4.8c-0.9,0-1.4-0.3-1.8-0.8c-0.6-0.7-0.6-2.6,0-3.2c-0.1-1.3-1.3-1.2-1.8-2c-0.3-0.4-0.3-0.7-0.7-1c0-2.5,0.7-2,1.2-3.5c0.1-0.2,0-0.7,0-1.2c-0.6-0.1-0.5-0.4-0.8-0.6c-0.4-0.1-0.7-0.2-1.1-0.2c-0.3-0.1-0.6-0.4-1-0.6c-0.1-0.3-0.4-0.6-0.5-0.8c-0.3-0.6,0-1.3-0.4-1.8c-0.2-0.3-1.9-1-2.4-1.2c-0.2-0.6-0.7-1.2-0.8-1.8c-0.3-1.2,0.6-1.7,1-2.3c0.2-0.3,0-0.7,0.1-1.1c0.4-1,0.6-0.7,0.5-2.2c-0.6-0.3-1.4-0.8-2-1c-0.1-0.8-0.2-1.6-0.2-2.4c-0.1-0.3-0.6-0.5-0.7-0.7c-0.4-0.9,0.4-2.3,0-3.2c-0.8-1.8-3-2.4-4.3-3.7c-0.9-0.9-1.6-2.2-2.6-2.9c-0.1-1.4-0.4-2.7,0.1-3.7c-0.4-0.1-0.9-0.2-1.3-0.4c-0.7-0.4-1-1.4-1.7-1.9c-0.1-1.2,0.2-2.2,1.2-2.5c-0.2-0.7-2.9-1.5-3.7-1.8c-1.4-0.4-2.6-1-4.2-0.2c-0.1,0-0.2,0.4-0.2,0.5c-0.4-0.1-0.9-0.2-1.3-0.2c-0.6,0.1-1.5,0.5-1.9,0.6c-0.6,0-1.3-0.1-1.9-0.1c-0.3,0.1-1.5,1.2-1.7,1.4c-2.9,0-5.2-0.9-8-0.2c-0.6,0-1.2-0.1-1.8-0.1c-0.3,0.1-0.9,0.5-1.2,0.6c-0.9,0.3-1.7,0.3-2.4,0.7c-0.4,0.2-0.4,0.6-0.7,1c-0.3,0.3-0.7,0.1-1.2,0.4c-0.1,0-0.3,0.4-0.4,0.5h-0.1c-0.4,0.1-0.5-0.2-0.6-0.2c-0.5-0.1-0.7,0.2-1.2,0.2c-0.4,0.9-0.9,0.6-1.4,1.2c-0.2,0.2-0.1,0.5-0.2,0.7c-0.2,0.3-0.7,0.2-1.1,0.4c-0.2,0.1-0.3,0.6-0.5,0.7c-0.5,0.3-0.9,0.2-1.2,0.6c-0.5-0.1-0.3-0.2-0.5-0.2H496c-1.5,0-4.1-0.3-5,0c-0.8,0.3-1.6,0.9-2.4,1.2l-1.7,0.1c-0.1,0-0.6,0.5-0.7,0.5c-0.5,0.1-1.1-0.3-1.7,0h-0.2c0.5,1.4,1.7,2,2.2,3.4c0.4,1.3-1.3,3.5-1.7,4.3c1,0.9,1.5,2.1,2.3,3.2c0.4,0.6,1.2,1,1.6,1.8c0.2,0.7,0.3,1.4,0.5,2c0.6,1.1,2.2,1,3.1,1.8c0.4,0.4,0.3,0.8,0.5,1.4c0.2,0.6,1.1,2.3,1.4,2.8c0.3,0.4,1.7,1.7,2.2,1.9h0.5c0.2,0.3,0.4,0.6,0.6,0.8c0.3,0.2,0.6,0.2,0.7,0.5c0.3,0.6,0,1.3,0.2,2c0.5,1.5,2.2,4.3,0.2,5.5c0.5,2,5.1,1.6,6.1,3.6c0.3,0.6,0,1.1,0.2,1.8c0.3,0.9,0.7,2.5,1,3.5c0.3,1.2-0.1,2.2,0.2,3.2c0.3,0.8,1.1,1.3,1.7,1.9c0.4,0.4,0.7,1.3,1.1,1.7c0.7,0.7,2.2,1.1,3.1,1.7c0.4,0.3,0.8,0.8,1.2,1.1c0.1,1.9,0.2,3.7,1.6,4.7c0.5,0.4,1.4,0,2.2,0.2c2.2,0.7,3.4,2.3,4.2,4.3h1.4c0.1-0.1,0.1-0.4,0.2-0.5c0.2,0,0.5-0.1,0.7-0.1c0.2-0.1,0.5-0.4,0.6-0.5c0.6-0.2,2,0.2,2.8,0.2c0.4,0.4,1,0.8,1.3,1.2c0.3,0.4,0.3,0.9,0.6,1.3c0.4,0.2,1.3,0,1.9,0.2c0.3,0.1,0.5,0.6,0.8,0.7c1-0.3,2-0.1,2.8-0.4h1.6c0.6-0.2,0.9-1.2,1.7-1.4c0.9-0.3,2.2,0.2,3.2-0.1c1.2-0.4,2.4-1.3,3.7-1.7c0.8,0,1.6,0.1,2.4,0.1c0.4-0.1,1.2-0.5,1.6-0.6l1.7,0.2c0.6-0.2,1.3-0.7,2.3-0.4c0,0.1,0.1,0.2,0.1,0.2c0.8,0.2,2.1-0.5,3.1-0.1c1,0.3,2.1,0.9,3.1,1.2c1.2,0.4,2.7-0.6,3.1-0.6h1c0.3,0.2,0.7,2.3,1,2.8c0.1,0.1,1.7,1.3,1.8,1.3c0.5,0.2,0.8,0,1.2,0.2c0.4,0.2,0.4,1,0.7,1.3c0.5,0.6,1.4,0.7,1.8,1.3c0.5,0,0.8,0,1.1-0.2h1.9c0.4-0.2,0.3-0.6,0.6-1c0.3-0.4,1-0.6,1.3-1.1c0.2-0.2,0-0.5,0.1-0.8c0.3-0.9,1.4-1.9,2.5-2c0.3-1.7,1-1.6,1.7-2.6c0.1-0.3,0.2-0.6,0.2-0.8c0.3-0.4,1.1-0.6,1.6-0.8c0.1-0.2,0-0.1,0.1-0.2C580.4,126.1,580.3,125.9,580.1,125.8z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Iron Hills")} [:path {:className "st3" :d "M634.4,68h-0.7c-0.1,0.1-0.5,0.4-0.6,0.5c-0.5,0.2-0.8,0-1.3,0.2c-0.1,0.1-0.2,0.2-0.2,0.4c-0.4,0.2-0.8,0-1.3,0.2c-0.1,0.2-0.2,0.3-0.4,0.5c-0.4,0.2-1.4,0.3-1.7,0.4c-2.1,0.5-7.5-0.2-9.4-0.8h-2.2l-0.2-0.2c-1-0.2-2.1,0.9-2.8,1.2c-0.4,0.2-1.8-0.6-2.2-0.7c-2-0.6-5.8,0.8-7-0.8c-1.1,0-2.4,0.7-3.4,1.1c-0.8,0.3-2.2-0.1-3,0.2c-0.6,0.2-1.3,1-1.9,1.3c-1,0.4-1.6-0.2-2.8,0.2c-0.2,0.1-0.6,0.4-0.8,0.5h-1.8c-0.4,0.1-1,0.5-1.6,0.4c-0.8-0.2-1.3-0.5-2,0c-2.5-0.2-0.9-4.9-0.1-5.9c0.4-0.4,0.8-0.7,1.2-1.1c0.3-0.4,0.6-1,0.7-1.6c-3,0.2-6.3-0.6-9-1.6h-0.8c-0.6-0.2-1.4-0.5-1.9-0.7h-0.7c-0.2-0.1-0.6-0.5-0.8-0.6c0,0-3.6,0.5-3.8,0.6c-0.4,0.1-0.7,0.6-1.4,0.5c-1.1-0.3-2.3-0.8-3.2-1.2c-1.2-0.5-2.1,1-3.7,0.7c-1.9-0.3-7.3-0.1-9,0.2c-0.6,0.1-1.9-0.4-2.3-0.2c-0.5,0.1-1,0.7-1.6,0.8c-1.4,0.3-2.6-0.8-4.2-0.4c-1.1,0.3-2.6,0.5-3.8,0.8h-3.1c-0.9,0.3-2.4,0.8-3.4,1.3c0.1,0.6,0.8,1.1,1.3,1.3h1.1c0.3,0.1,0.6,0.6,1,0.8c-0.1,0.4,0.2,0.9,0,1.3c-0.1,0.2-0.5,0.4-0.6,0.7c-0.2,0.5,0.1,2,0.4,2.4c0.2,0.3,0.7,0.5,1,0.7c0.5,0.5,0.9,1.7,1.4,2c0.8,0.6,1.8,0.7,2.5,1.2c0.9,0.6,0.8,2.2,1.9,2.9c0,0.7,0.1,1.7-0.2,2.3c0.1,0.6,0.4,0.8,0.8,1.1c0,0.5,0.1,1.2-0.1,1.4c0.2,0.1,0.4,0.3,0.6,0.4v0.5h0.5c0.4,0.4,0.8,0.8,1.3,1.1c0,1.7,0,3.2-0.8,4.4c-0.2,0.4-1.1,1.1-0.8,1.6c0.1,1.4,1.5,1,2.4,1.6c0.3,0.2,0.5,0.6,0.8,0.8c0.1,0.9,0.3,1,0.2,2.2c1.2,0.6,2.1,1.1,3.1,1.7c0.6,2.1,0.4,3.5-1,4.7c0.1,0.3,0.1,0.5,0.2,0.7c0.2,0.3,0.5,0.2,0.8,0.4c0.6,0.4,2.3,2.9,1.1,4c0,2.1,1.6,1.3,2.3,2.5c0.4,0.7,0,1.6,0.4,2.5c0.1,0.4,0.6,1,0.7,1.3c0.1,0.3,0,0.6,0.1,0.8c0.1,0.2,0.1,0.4,0.2,0.5c0.5,0.3,1.1,0.3,1.6,0.6c0.6,0.4,0.7,1.1,1.6,1.3c0.1,0.1,1.6-0.7,1.9-0.8c0.2-0.3,0.5-0.6,0.7-1h1.1c1-0.5,0.5-1.7,2.8-1.7c0-2.5,0.6-1.5,1.2-2.8c0.1-0.6,0.2-1.1,0.4-1.7c0.2-0.4,0.7-0.5,1-0.8c0.6-0.7,1.2-1.5,1.7-2.3c1,0,2.1,0,2.8-0.2c0.4-1.7,0.9-2.8,0.8-4.6c1.1-0.1,1.4-1,2.2-1.4c0.4-0.2,0.7,0,1.1-0.2c0.2-0.3,0.5-0.6,0.7-0.8c0.5-0.3,0.8,0,1.3-0.2c0.6-0.3,0.9-1.2,1.4-1.6h0.5c0.6-0.3,1.3-1.1,2-1.3c0.5-0.2,0.9,0,1.3-0.2v-0.2h1c0.1-0.1,0.4-0.4,0.5-0.5c0.4-0.3,0.9-0.2,1.3-0.5c0.2-0.2,0.2-0.5,0.4-0.7c0.1-0.1,1.2-1,1.3-1.1c0.4-0.3,0.7-0.2,1-0.6c4.6,0,6.1,0,9,1.8c0,0.7-0.1,1.1-0.2,1.6c0.5,0.3,0.9,0.4,1.2,0.8c0.5,0,0.8-0.3,1.1-0.4h1.9c0.4-0.1,1.2-0.6,1.7-0.7c1.4-0.4,2.9,0.6,3.8,0.8c0.2,0,0.5-0.1,0.7-0.1c1.8-0.1,3.2-0.4,3.6-1.8c0.9,0,1.4-0.2,2-0.5c0.2,0,0.3,0.3,0.6,0.2c0.2-0.1,1-0.5,1.4-0.4c0.8,0.2,2.1,0.7,2.9,1c0.9,0.3,1.5-0.3,2.2-0.5h1.1c0,0,0.5-0.1,0.6-0.1v-0.2h1.2c0.4-0.1,1.1-0.2,1.6-0.4c0.1,0,0.3-0.3,0.4-0.4c0.6-0.2,0.9,0.2,1.3,0c0.7-0.4,1.1-1.4,1.7-1.8c0.3-0.1,0.6-0.2,1-0.2c0.2-0.2,0.3-0.5,0.6-0.7c-0.1-0.7-0.9-1.5-0.4-2c0-0.9,0.1-1.3,0.8-1.4c0.1-0.6-0.1-1.3,0.2-1.7c-0.1-0.7-1.2-1.3-1.7-1.6c-0.1-1.8-1-4,0.6-4.9c0-1.5-0.4-2.7,0-3.8c0.4-1.1,1.3-1.2,1.8-1.9l0.2-2.4c0.2-0.7,0.9-0.9,1-1.7C635.3,67.4,635,67.8,634.4,68z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Withered Heath")} [:path {:className "st3" :d "M614.2,41.2c-0.2-0.9,0.2-1.7,0.4-2.3c0.1-0.6-0.2-1-0.1-1.6c0.1-0.5,0.2-1.5,0.4-2.2c-0.4-0.1-0.9,0.1-1.3,0c-1.7-0.3-2.7-0.1-4.4-0.1h-1.8c-1.4-0.3-4.1-1.1-5.9-0.6c-0.3,0.1-0.9,0.4-1.2,0.5c-0.4,0.1-1.1-0.3-1.7-0.1c-0.6,0.2-1.4,0.5-2,0.7c-0.4,0.2-1.2-0.2-1.4-0.1c-0.4,0.1-1.3,0.3-1.6,0.4c-0.9,0.2-2.3-0.4-3.1-0.6h-2.8c-2.2-0.6-4.4-1-6.8-1c-0.5,0.3-1.2,0-1.8,0.2c-0.4,0.1-0.9,0.6-1.2,0.7c-0.3,0-0.6,0.1-0.8,0.1c-0.5,0.2-1,0.9-1.6,1.2c-1.1,0.6-2.2,0.7-3.4,1.2c-0.5,0.2-1,0.8-1.6,1c-0.4,0-0.7,0.1-1.1,0.1c-1.2,0.4-2.7,1-3.8,1.4h-0.8c-0.6,0.2-1.5,0.7-2.2,1c-1.5,0.5-2.8,0.5-4.3,1c-0.3,0.1-0.8,0.5-1.1,0.6c-0.8,0.3-1.5,0.3-2.3,0.7c-0.2,0.1-0.4,0.4-0.6,0.5h-1.1c-0.9,0.2-2.1,0.4-3.1,0.6c-0.2,0-0.5-0.1-0.7-0.1c-0.8,0.1-2.1,1-3.1,0.7c-0.9-0.2-1.7-0.7-2.4-1c-0.7-0.2-1.8,0.2-2.3,0.1c-0.5-0.1-4.4-0.3-4.9-0.1c-0.2,0.1-0.6,0.5-0.8,0.6c-0.9,0.3-2.3-0.2-2.6-0.2c-0.5-0.1-0.8,0.4-1.6,0.2c-0.4-0.1-0.8-0.5-1.1-0.6c-0.6-0.1-1.3-0.2-1.9-0.2c-1.7-0.5-4.4-1.6-6.6-1.6c-0.3,0,0,0.3-0.4,0.2c-0.6-0.1-1.2-0.4-1.8-0.4c-0.8,0.1-2.1,0.5-3,0.2c-0.5-0.1-1.3-0.5-1.8-0.6H512c-0.3-0.1-0.5-0.6-0.7-0.7c-0.4-0.2-3.7,0.3-4.1,0.5c-0.1,0.2-0.2,0.3-0.4,0.5c-0.8,0.3-1.7-0.3-2.2-0.4c-1-0.3-1.8,0.2-2.8,0c-0.9-0.2-3.8-1.4-4.7-1.2c-1.2,0.2-1.9,0.9-2.8,1.6c0,0.1-0.1,0.2-0.1,0.4l-1.4-0.1c-0.7,0.2-1.6,0.8-2.3,1.1c-0.5,0.2-0.9-0.1-1.3,0c-0.5,0.1-1.3,0.3-1.7,0.5h-0.6c-0.2,0.1-0.4,0.5-0.6,0.6c-0.7,0.4-1.2,0.5-1.8,0.8c0.1,1,0.5,1.1,0.7,1.8c0.5,1.5,0.3,4-0.7,4.7V52c0.5,0.2,1.1,0.7,1.6,0.8c0.7,0.3,1.3-0.2,1.8,0c0.1,0.1,0.2,0.4,0.4,0.5c0.6,0.3,1.6,0.3,2.2,0.5h4.9c0.8,0.2,1.8,0.7,2.5,1.1c0.8,0.4,3.3,1,2.3,2.4c-0.1,0.8-0.5,1-1.1,1.2c0.1,0.2,0,0.1,0.1,0.2c0.4,0.2,1.1-0.2,1.4-0.2c0,0,0.7,0.3,0.8,0.4c0.2,0,0.4-0.1,0.4-0.1h0.2c0.6,0.2,1.2,1,1.8,1.3c0.1,0.4,0.1,0.9,0.1,1.4h0.1v0.1c0.5,0,1.1-0.5,1.7-0.2c0.4,0.1,0.6,0.6,1.3,0.5c1.4-0.3,2.5-0.9,3.8-1.1c0.9-0.1,2.8,0.1,3.7,0.2c1.1,0.2,2.5-0.1,3.8,0c0.4,0,0.6,0.3,1,0.2c1-0.2,1.2-1.1,1.9-1.4c1.3-0.6,2.3,0.2,3.5-0.1c1.2-0.3,4.8-1.5,5.9-1.2c1.4,0.5,2.9,1.5,4.2,2c0.7,0.3,1.4,0.1,2.2,0.4c0.6,0.2,1.5,0.6,2,0.7c1.9,0.5,5.7-0.9,7.3-1.1c1-0.1,3,0.8,4.1,0.5c0.4-0.1,0.7-0.5,1-0.6c0.6,0,1.2-0.1,1.8-0.1l0.1-0.2c0.2,0,1.3,0.3,1.9,0.2c1.7,0,4.3,0,6.4,0h2.4c0.7-0.2,1.3-0.8,1.9-1c1.1-0.4,3.2,0.9,4,1.2c0.9,0.3,3.7-0.8,4.3-1c1.3-0.4,2.3,0.4,3,0.7c1,0.4,1.8,0.3,2.9,0.6c0.5,0.1,1.1,0.6,1.4,0.7c0.4,0.1,0.6-0.2,1.2,0c2,0.6,4.7,1.4,6.7,0.6c0.6-0.2,0.7-1,1.2-1.3c0.6-0.4,1.5-0.5,2-1c0.5-0.4,0.4-1.3,0.8-1.8c0.2-0.2,0.5-0.3,0.7-0.6c4.1,0,2.9-1.9,5-3.4c1.1-0.7,2.7-1.3,3.4-2.4c0.3-0.5,0.1-1,0.4-1.6c0,0,1.5-1.6,1.6-1.7c0.5-0.3,1.1-0.4,1.6-0.7c0.1-0.2,0.2-0.4,0.4-0.6c0.9-0.7,2.3-1.2,3.5-1.6c0.3-0.1,2.3-0.4,2.5-0.4c0.2,0.1,0.8,0.5,1.3,0.2c0.2-0.1,0.7-0.5,1-0.6C615.1,43.5,614.5,42.4,614.2,41.2z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Shadowland" "Grey Mountain Narrows")} [:path {:className "st1" :d "M503.3,63.2c-1.9,0.1-1.6-0.1-2.5-1c0-0.9,0.5-1.1,1.1-1.4c-0.4-0.6-3.3-0.4-3.8,0.1c-0.9-0.2-1.2-1.2-2.3-1.6c0-0.3,0.1-0.5,0.2-0.7c0.4-0.8,1.8-1,2.4-1.6c0.1-0.1,0,0,0.1-0.2c-1.2-0.5-2.2-1.3-3.4-1.7h-4.9c-0.7-0.2-1.7-0.4-2.5-0.6h-1.4c-0.6-0.2-1.9-0.5-2.4-0.8c-0.1-0.1-0.2-0.4-0.4-0.5c-0.2,0-0.3,0.1-0.5,0.1c-0.5-0.2-1.8-0.4-2.4-0.6c-0.9-0.2-1.4,0.3-1.9,0.5c-1.1,0.3-2-0.1-3.1,0.2c-0.4,0.1-0.8,0.6-1.1,0.7h-1.1c-0.3,0.1-3.1,0.3-4,0c-0.6-0.2-1.3-0.4-1.9-0.7c-0.1,0-0.3-0.3-0.4-0.4c-0.5-0.2-0.7,0.2-1.3,0c-0.5-0.2-1.2-1-1.7-1.2c-0.6-0.3-1.6,0.1-1.9,0c-0.4-0.1-1.3-0.4-2-0.2c-0.2,0-0.8,0.3-1.1,0.2c-0.2-0.1-0.6-0.2-1.2-0.2h-2.3c-0.3-0.1-0.8-0.5-1.1-0.6c-0.3,0-0.6,0.1-0.8,0.1c-0.4-0.1-1.2-0.5-1.6-0.6c-0.7-0.3-1.3,0-2-0.2c-0.4-0.1-0.9-0.5-1.6-0.4l-0.2,0.2h-1c-0.3,0.1-0.7,0.6-1.1,0.7c-0.5,0.1-0.9-0.2-1.2-0.2c-0.5-0.1-1.6,0.3-1.9,0.4c-0.4,0.1-0.5-0.2-0.7-0.2h-1.7c-0.2-0.1-1-0.4-1.2-0.5c-1-0.4-1.8,0.3-3,0c-0.6-0.2-1.2-0.4-1.8-0.6c-0.1-0.1-0.2-0.3-0.4-0.4c-1.8-0.6-3.3,0.9-4.3,1.2c-1,0.3-1.8-0.7-2.4-0.8c-0.4-0.1-4.1,0.3-4.4,0.4c-1.7,0.4-4.7-1.6-6.1-0.7c-1.1,0.1-0.5,0.5-1,0.8c-0.4,0.3-1.1,0.1-1.4,0.4c-0.5,0.3-0.7,1-1.4,1.2c-0.9,0.3-1.7-0.5-2.4-0.4c0,0.1-0.1,0.2-0.1,0.2c-0.5,0-1,0.1-1.4,0.1c-0.1,0.1-0.2,0.2-0.2,0.4h-0.6c0,0.1-0.1,0.2-0.1,0.4c-0.2,0.1-2.5-0.1-3.1,0.1l-1.2,0.6c0,0.2-0.1,0.3-0.1,0.5c-0.2,0.2-0.5,0.3-0.7,0.5c0,0.1-0.1,0.2-0.1,0.4h-0.8c-0.4,0.1-0.9,0.3-1.2,0.5c0.2,0.4,0.6,1.1,0.7,1.6c0.1,0.4-0.2,1.2,0.1,1.7v1.2l0.4,0.1c0.2,0.3,0.2,0.6,0.4,0.8c0.3,0.4,0.8,0.5,1.2,0.8c0.7,0.5,1.1,1.4,1.7,1.9c0.8,0.6,1.6-0.4,2.9,0.1c0.6,0.2,1.1,0.8,1.9,0.8c0.1-0.9,0.2-2,0.6-2.8c0.3-0.5,0.7-0.5,0.7-1.3c0.9-0.4,1.5-1.1,2.3-1.4c0.8-0.3,1.6-0.3,2.4-0.7c0.8-0.4,1.7-1.7,2.5-2h1c0.2-0.1,0.8-0.4,1-0.5c1.1-0.3,4.9,1.5,5.5,1.9c0.6,0.5,1,1.4,1.6,1.9c0.8,0.7,2.3,0.5,3.1,1.1c0.6-0.1,0.8-0.4,1.2-0.6c0.5-0.2,0.8,0,1.4-0.2c0.5-0.2,1.1-0.6,1.7-0.7c1-0.3,5.1-0.2,6,0.1h2c0.5,0.2,1.3,0.7,1.7,0.8c1.2,0.4,3.2-0.5,4.1,0.2c1,0,1.3-0.5,1.7-1.1c1.1,0,2.3-0.1,3,0.6c1.1,0,1.3,0.7,1.4,1.6h0.2c1.3-2.4,2.1-3.8,5.9-3.8c0.8,0,1.5,0,2,0.2c0.7,0.4,1.9,2.1,2.4,2.8c0.2,0,1.2,0.2,1.2,0.2c0.3-0.1,1.2-0.3,1.9,0c1.5,0.5,3.1,1.4,4.3,2.2c0.1,0.9,0.4,2.5,0.1,3.6c-0.1,0.5-0.5,0.8-0.6,1.3c0.1,0.1,0,0,0.1,0.2c2.1-0.1,4.5,0.4,6.4,0.8c1,0,2,0.1,3,0.1c0.6,0.2,2.8,0.6,3.7,0.2c0.3-0.1,1.3-0.4,1.6-0.5h0.8c0.5-0.2,1.2-0.9,1.7-1.1c0.6-0.3,1.2-0.2,1.6-0.2c1-0.2,2.2,0.3,3,0.1c0.6-0.1,1.7-0.4,2.4-0.2c0.2,0,0.9,0.3,1.3,0.1c0.2-0.1,0.3-0.4,0.5-0.5c0.6-0.3,1.2-0.4,1.7-1c0.5-0.6,0.8-1.5,1.6-1.9c0.7-0.4,2.2,0.1,2.9-0.4h0.1v-0.2C504.2,63.3,503.5,63.4,503.3,63.2z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Borderland" "Woodland Realm")} [:path {:className "st2" :d "M503.4,109.1c-0.2-0.8,0-1.7-0.4-2.3c-0.2-0.5-2-1.3-2.5-1.6c-0.7-0.3-1.2,0-1.8-0.2c-0.6-0.3-0.9-1.1-1.8-1.4c-0.1-0.5-0.3-1-0.2-1.8c0.3-0.3,0.7-0.7,0.8-1.1c0.3-0.7-0.4-2.7-0.8-3.2v-1.8c-0.3-0.8-1.4-0.8-2-1.2v-0.1c-0.3-0.2-0.4-0.7-0.7-1c-0.3-0.3-0.7-0.4-1-0.7c-0.3-0.4-0.3-0.8-0.6-1.2c-0.2-0.3-0.8-0.6-1-1c-0.3-0.6-0.2-1.2-0.6-1.8c-0.3-0.4-1-0.4-1.4-0.6c-0.6-0.3-1.4-1.1-1.8-1.7c-0.5-0.7-0.4-1.8-0.8-2.5c-0.2-0.4-0.6-1-1-1.3c-0.1,0-0.2-0.1-0.4-0.1c-0.3-0.4-0.3-0.9-0.6-1.3c-0.1,0-0.2-0.1-0.4-0.1c-0.2-0.3-0.2-0.6-0.4-0.8c-0.2-0.3-0.6-0.4-0.8-0.6c-0.2-0.2-0.2-0.5-0.4-0.7c0.1-1.4,1-2.7,1.7-4.1c0.3-0.4-0.5-0.9-0.7-1.2c-0.8-0.9-1.3-2.2-2.2-3.1c-0.5-0.3-1.5,0-2.2-0.1c-0.6-0.1-4.8-0.7-5-1H472c-2.3-0.9,0.5-4.3-0.6-5.9c-0.6-1-2.6-0.8-3.4-1.6h-3c-1.1-0.5-1.5-2.5-2.9-3c-0.3-0.2-0.4,0.1-0.8,0c-3.3-0.7-3.6,1.8-4.8,3.5c-0.2,0.3-0.4,0.2-0.6,0.6c-0.8,0.1-1.4,0.1-2-0.2c-0.2-0.4-0.2-1.2-0.5-1.6c-0.5-0.3-1.1-0.2-1.4-0.6c-0.8,0.2-1.4,0.8-2.3,1.1c-1.1,0.3-1.7-0.8-3.1-0.5c-1.4,0.3-2.7-0.4-4-0.8h-6.7c-1.4,0.1-4.3,2-5.5,1.6c-0.9-0.3-2.6-0.7-3.4-1.2c-0.8-0.5-1.2-1.7-1.9-2.2c-1.1-0.7-2.6-1.3-4-1.6c-0.4,0.4-1.3,0.3-1.8,0.6c-0.7,0.4-1.2,1.2-1.9,1.6h-0.6c-0.4,0.2-1,0.8-1.4,1h-0.8c-0.1,0.1-0.1,0.4-0.2,0.5c-0.8,0.5-1.5,0.7-1.9,1.6c-0.2,0.4,0,0.8-0.4,1.1c0.1,0.6,0.5,1,0.8,1.3c-0.1,1.8-1.6,2.7-2.6,3.6c0.1,0.5,0.3,0.6,0.4,1c0.4,1.3-0.6,1.7,0.6,2.8c1,0.8,3.2,1.1,3.6,2.4c0.5,1.5-0.1,4.1-0.8,4.6c0.1,1.6,1.2,2.3,1.3,4.2c1,0.5,1.9,1.5,2.8,2.2h0.5c0.3,0.3,0.4,0.9,0.7,1.2c0,1.1-0.3,1.3-0.7,1.9c-2.3-0.1-3.6-0.1-4.3,1.6c-0.3,0.7-0.1,1.4-0.5,1.9c0,1.2,0.3,2.3,1,2.8c0.3,0.2,0.6,0.2,0.8,0.4c0.3,0.2,0.3,0.6,0.7,0.7c1,0.3,1.6,0.3,2.3,0.8c0.8,0.6,0.6,1.7,1,2.8c0.3,0.8,1,2,0.4,3.1c-0.4,0.8-1.5,1.6-2.2,2.2c0,1.3,2.3,1.9,3.8,1.4c0.4-0.1,1-0.5,1.3-0.6h1.2c1.9,0,3.2,0.4,4.7,0.4c1.7,0,3.6-0.5,5-0.1c1.1,0.3,2.6-0.3,3.6,0.1c1.9,0.8,3.2-1.5,5.3-1c3.9,1,7.8,2.3,12.5,1.2c0.6-0.1,1.6,0.1,1.8,0c1.1-0.3,2.3-0.8,3.2-1.1c0.7-0.2,1.2-0.1,1.9-0.4c0.7-0.3,1.7-1,2.9-0.6c1.2,0.4,2.4,1.9,3.4,2.5c0.3,0,0.6,0.1,0.8,0.1c0.3,0.2,0.8,0.7,1.2,0.8c1.1,0.5,2.2,0.5,3.2,1.1c0.9,0.5,1.3,1.8,2.2,2.3c2.1,1.3,4.6,2.3,6.8,3.4c0.2-0.1,0.4-0.3,0.5-0.4l3.1-0.2c1-0.4,2.2-1.1,3.4-1.4c1.1-0.3,3,0.2,4.1,0.2c1,0,1.8-0.5,2.8-0.2c1.3,0.4,3,0.5,4.1,1c0.8,0.3,2-0.2,2.5,0c0.5,0.2,0.8,0.9,1.3,0.7C504.5,113.1,503.6,109.9,503.4,109.1z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Heart of Mirkwood")} [:path {:className "st3" :d "M517.3,127.6c-0.6-0.4-1.5,0-2.4-0.2c-0.8-0.2-2.1-1.1-2.5-1.7c-0.3-0.5-0.7-1.4-0.8-1.9c-0.2-0.9,0.4-1.7-0.1-2.4c-0.3-0.5-0.9-0.2-1.3-0.5c-0.5-0.4-1-0.7-1.6-1.1c-0.2-0.1-0.3-0.6-0.5-0.7h-0.4c-0.4-0.4-0.7-1.1-1.1-1.6c-0.3-0.2-0.6-0.5-1-0.7c-0.2-0.3-0.4-0.9-0.7-1.1c-0.5-0.3-1.1,0-1.6-0.2c-0.3-0.1-0.7-0.6-1-0.7c-0.7-0.3-1.2,0.2-1.9,0c-1.2-0.3-3-0.7-4.2-1.1c-0.8-0.3-1.3,0.3-1.7,0.4c-0.6,0.2-1.2-0.1-1.7,0c-1.2,0.2-2-0.6-3.4-0.2c-0.8,0.2-1.9,0.9-2.6,1.2c-0.9,0.4-1.8,0.1-3,0.4c-0.5,0.1-1,0.6-1.7,0.4c-1.5-0.4-3.1-1.3-4.3-2c-0.4-0.2-1.3-0.3-1.7-0.5c-0.1-0.2-0.2-0.3-0.4-0.5c-0.5-0.4-1-0.5-1.6-0.8c-0.4-0.3-1-1.5-1.4-1.9c-0.5-0.1-1-0.2-1.6-0.4c-1.3-0.6-2.7-1.1-4-1.8c-1.1-0.6-1.9-1.8-3.2-2.2c-0.4-0.2-1.1,0.3-1.4,0.5c0,0.3,0.9,2.7,1.1,2.9c0.4,0.8,1.4,0.7,1.9,1.3c1.2,1.3-1.1,3.3-1.7,3.7c-0.1,1.3-0.1,2.7-0.5,3.7c-0.3,0.7-0.9,1.4-1.1,2.3c-0.1,0.9-0.2,1.8-0.2,2.8c-0.1,0.4-0.7,0.9-0.8,1.3c-0.6,1.4-1,2.9-2.2,3.7c0,1.2,0.7,1.8,0.4,3.1c-0.4,1.7-2.1,3-3.6,3.6c-0.1,0.2,0,0.1-0.1,0.2c0.2,1.1,2,1.4,2.4,2.6c0,0.4-0.1,0.7-0.1,1.1c0.1,0.3,0.4,1.1,0.4,1.4c-0.1,0.9-0.8,1.9-0.4,3.2c0.1,0.4,0.6,0.7,0.7,1.1c0.1,0.4,0,0.7,0.1,1c0.2,0.4,0.7,0.7,0.8,1.2c0.3,0.9,0.2,3-0.1,3.6c-0.2,0.3-0.7,0.4-0.8,0.8v1.3c-0.2,0.5-0.7,1.6-1.1,1.9c-0.2,0.1-0.4,0.1-0.6,0.2c-0.2,0.3-0.4,0.6-0.6,0.8c0,0.7,0.2,0.9,0.6,1.2c-0.1,2.3-1.2,3-1.7,4.7c-0.3,1,1,2.8,0.5,4.4c-0.2,0.7-1,3-1.4,3.5c-0.3,0.4-0.8,0.5-1,1.1c-0.1,0.5,0.3,1.4,0.4,1.8c0.3,1.4-0.2,4.1-0.6,4.8c0,0.2,0,0.2,0.1,0.4c0.5,0.3,1.1,0.1,1.7,0.4c0.7,0.3,1.5,0.8,2.2,1.1c0.5,0.2,0.8,0,1.2,0.2c0.8,0.4,1.3,1.2,2,1.6c0.6,0.4,1.3,0.4,1.9,0.7c0.4,0.2,0.8,0.7,1.2,1c0.1,0.7,0,1.8,0.4,2.4c0.4,0.7,1.5,1.4,1.9,2.3c0.3,0.7,0.1,1.7,0.5,2.5c0.5,0.2,1,0.4,1.4,0.6h0.6c0.1,0.2,0.2,0.3,0.4,0.5c0.7,0.3,2.2-0.1,2.6-0.2c1-0.1,1.9-0.2,2.9-0.2c0.1,0,0.3-0.3,0.4-0.4c0.8-0.3,2.4-0.6,3.4-0.5c1.4,0.2,2.8,0.7,4,1.1c1.2,0.4,1.6-0.9,2.8-0.5c0.4,0.2,2.8,1.6,3.1,1.6c0.8,0,3.4-0.4,4.2,0.1c0.5,0,0.9,0,1.2-0.1c0.5-2.9-0.3-4.2,2.3-5.4c0.9-0.4,3-0.5,3.7-0.6h3.1c1.1-0.4,2.7-0.9,3.6-1.6c0.6-0.4,2-1.7,1.3-2.9c0-1.9-1.5-2.8-2.9-3.4c-0.8-0.3-1.3-0.1-1.9-0.5c-1.4-0.9-2.5-2.3-3.7-3.4c-0.7-0.6-2.6-1.2-2-3c0.3-1,1.2-1.1,1.8-1.7c0.7-0.6,0.8-1.8,1.3-2.5c0.5-0.6,1.1-1,1.6-1.7c1.1,0,2,0.5,3.1,0.2c0.7-0.2,1.6-0.6,2.3-0.8c0.2-0.6,0.2-1.9,0.2-2.9c0.3-0.3,0.4-0.7,0.7-1c0.6-0.6,3.5-1.6,4.4-1.1c1-0.2,0.9-1,1.4-1.6c0-1.7-1-2.2-1.7-3.2c-0.6-0.9-0.5-1.8-1.4-2.4c-0.1-1.8,0.5-2.1,2.3-2c-0.1-1.2-1.1-1-1.4-1.9c-0.2-0.2,0-0.6-0.1-1c-0.6-2.2,1.1-2.4,0.7-4c-0.2-0.8-1.1-1.3-1.3-1.9c0-0.7-0.1-1.4-0.1-2c-0.2-0.6-1-1.2-1.3-1.8c-0.7-1.6,0.4-3,0.8-3.8c1.5,0,1.8,0.6,2.8,1.1c0.6-0.2,1.5,0.5,2,0.7c0.5,0.2,1.8,0.2,1.9,0c1.4,0,3.5,0.3,4.4-0.2C521,133.3,518.4,128.2,517.3,127.6z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Western Mirkwood")} [:path {:className "st3" :d "M462.8,110.6c-0.6-0.5-0.8-1.6-1.1-2.4v-0.6c-0.2-0.3-0.5-0.5-0.7-1.1c-1.4,0-2.4,0.7-3.5,1h-3c-0.2,0-0.6,0.3-0.7,0.4c-0.5,0-1-0.1-1.6-0.1c0,0.1-0.1,0.2-0.1,0.2c-0.3,0-0.6-0.1-1-0.1c-1.7,0.1-3-0.2-4.3-0.6c-0.2,0-0.5,0.1-0.7,0.1c-0.2,0-1.8-0.2-2-0.4c-0.4-0.2-0.9-0.6-1.4-0.7c-1.3-0.3-2.5,0.9-3.4,1.2c-0.7,0.3-1.9-0.1-2.3-0.2c-1.1-0.3-2.7,0-3.6-0.2c-1-0.3-2.9,0-3.6,0.2h-3.4c-0.1,3.4-3,3.9-3.8,6.5c-0.3,0.9,0,1.7,0.1,2.5c1.1,0.6,2.4,1.4,3.6,1.7c-0.1,0.1,0,0-0.2,0.1c0.2,0.5,0.4,0.5,0.6,1c0.9,1.3-0.6,2.7-1,3.5c-0.2,0.5,0,1.1,0,1.8c0.7,0.7,1.4,1.4,2,2.2c0,1.9-1,1.9-1.8,2.8c-0.3,0.3-0.5,0.8-0.8,1.1c0,1.2,0.5,2,0.1,3.4c-0.4,1.3-1.9,1.8-2.5,2.8c-0.4,0.6-0.5,1.3-1,1.8c0,0.3,0,0.4,0.1,0.6c0.4,1.2,2,0.8,3.1,1.3c0.4,0.2,0.8,0.7,1.2,1c0,0.9-0.1,1.5-0.5,2v1.6c0.3,0.5,1.2,0.3,1.8,0.6c0.2,0.1,0.4,0.5,0.6,0.6c0.7,0.4,1.5,0.2,2.2,0.5c0.6,0.3,1.1,0.9,1.7,1.2c0.4,1.6,0,2-0.4,3.4c0.3,0.2,0.5,0.7,0.8,0.8c0.5,0.2,0.8,0.1,1.2,0.4c0.2,0.2,0.3,0.5,0.5,0.7c-0.1,0.6,0,0.4-0.5,0.4v0.1h-0.1c0,0.8,0.5,1.4,0.2,2.4c-0.4,1.4-2.8,3.9-3.7,4.9c0.1,0.5,2,3.7,2.3,4c0.5,0.3,1.2,0.4,1.4,1v1.4c0.2,0.8,1.1,1.1,1.3,1.8c0.1,0.4-0.1,0.9,0.1,1.2c0.2,0.5,2,1.7,2.5,1.9h2.3c0.3,0.1,0.8,0.5,1.1,0.6c1,0.4,2.1,0.5,3,1c-0.2,0.3-0.2,0.3-0.6,0.4v0.2c1.3,0.1,2.3,0.8,3.4,1.2c0.4,0,0.7,0.1,1.1,0.1c0.3,0.2,0.7,0.7,1.1,0.8c0.7,0.3,1.4,0.3,2,0.7c0.8-0.2,0.6-0.9,0.8-1.7c0.1-0.2,0.4-0.5,0.5-0.7c0.4-1.2-0.8-3.4-0.6-4.6c0.3-1.5,1.5-2.3,2-3.6c0.2-0.5,0.7-2.5,0.5-3.1c-0.1-0.2-0.4-0.4-0.5-0.6c-0.4-1.2,0.2-2.5,0.2-3.6c0.3-0.2,0.5-0.5,0.8-0.7c0-1.3,0.3-3-0.2-3.4c0.2-1.6,1.6-1.7,2.3-2.8c0.6-0.8,0-1.9,0.6-2.8c0.3-0.4,0.7-0.4,0.8-1c0.4-1.5-0.8-1.9-1.1-2.6v-0.7c-0.1-0.3-0.5-0.9-0.6-1.2c-0.4-1.1-0.2-2.8,0-3.8c0.1-0.5-0.2-0.6-0.2-0.8l0.1-1.4c-0.2-0.2-0.4-0.7-0.6-0.8c-0.8-0.5-1.4-0.4-1.7-1.3h-0.1c0-2.4,0.5-2.2,2-3c0.6-0.3,1-1.4,1.6-1.8c0-1.1-0.3-2.8-0.1-3.5c0.2-0.8,0.9-1.2,1.3-1.8c0.5-0.8,0.5-1.8,1-2.6c0.2-0.3,0.6-0.6,0.7-1c0-0.7,0.1-1.4,0.1-2.2c0.3-1.3,1-2.4,1.4-3.5c0.3-0.7-0.3-1.7,0.1-2.4c0-1.4,1.2-1.6,1.8-2.4c0.1-0.1,0,0,0.1-0.2C463.9,111.1,463.2,111,462.8,110.6z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Darkdomain" "Southern Mirkwood")} [:path {:className "st0" :d "M485.5,191.5c-0.6,0-1.1-0.5-1.8-0.2c-0.3,0.1-1.3,0.5-1.9,0.2c-1.1-0.4-3.8-1.6-5.8-1c-0.4,0.1-1.1,0.6-1.6,0.7h-1.6c-0.6,0.2-1.6,0.4-2.3,0.6c-1.3,0.3-4.5-1-5.2-1.4c-1.1-0.7-0.6-1.9-1.1-3.2c-0.3-0.9-1.4-1.3-1.8-2c-0.3-0.6-0.3-1.8-0.6-2.3c-0.3-0.8-1.6-0.9-2.3-1.3c-0.4-0.3-0.8-0.8-1.2-1.1c-1-0.6-2.3-0.7-3.5-1.2c-0.5-0.2-1.1-0.6-1.7-0.7h-3c-0.3-0.1-0.7-0.5-1-0.6c-0.9-0.4-2.2-0.3-3-1c-0.2-0.2-0.4-0.6-0.7-0.7c-1.6-0.8-3.4-0.9-4.9-1.7c-0.3-0.2-0.6-0.8-0.8-1h-1.2l-0.2-0.2h-0.2c-0.1,0-0.2,0.4-0.6,0.2c-0.9-0.3-1.9-0.9-2.4-1.6c-0.7,0.2-1.1,0.8-1.8,1.1c-1.5,0.6-4.1-0.9-4.6-1.7c-0.9,0-1.1,0.2-1.4,0.7H427c0.1,0.4,0,0.3,0.2,0.5c-0.2,0.8-1.5,1-2.6,1c-0.5,1.2-1.3,1.2-2.3,1.8c-0.4,0.2-0.7,0.8-1.1,1.1c0,1,0,1.8-0.5,2.3c-0.5,0.9-1.1,0.4-1.9,0.7c-1.6,0.7-1.9,3.1-4.3,3.1c-0.1,0.3-0.2,0.7-0.1,0.8c0.2,1.9,2.5,1.5,3.6,2.4c0.5,0.4,0.7,0.9,1,1.6c0.2,0.5,0.6,1.3,0.1,1.9c-0.2,0.6-0.3,0.5-0.8,0.7c-0.2,0.1-0.6,0.4-0.8,0.5h-2c-1.4,0.5-3,1.3-4.4,1.8c-0.6,0.2-1,0-1.6,0.2c-1,0.4-2,1.2-2.9,1.8c0,1.1,0.5,1.7,1.2,1.9c0,1.1,0.1,1.9-0.7,2.4c0,0.4,0,0.4,0.1,0.8c0.6,0.3,1.6,0.6,2.3,0.8c1.3,0.4,2-0.4,3.2-0.1c0.9,0.2,1.7,0.9,2.4,1.2c0.7,0.3,1.5,0.1,2.2,0.5c0.4,0.3,0.8,0.8,1.2,1.1c0.4,0.2,1.1,0.2,1.4,0.5c1.1,0,2.9,0.2,3.6-0.2c0.7-0.4,0.2-1.2,1.3-1.6c0.5-0.3,1.1,0.1,1.6-0.2c1.9,0,1.9,0.8,2.9,1.6c0,1.4-0.3,2.5,0.4,3.5c0.6,0.9,1.6,0.6,2.8,1.1c0.7,0.3,1.4,1.8,1.7,2.5c0.8,0.1,2.1-0.1,2.9,0c0.2,0,0.8,0.3,1.1,0.2c0.5-0.1,0.9-0.8,1.3-1c0.8-0.4,2.2-0.5,3-0.7h5.5c1.6-0.4,3.1-1.1,4.8-1.6c1.6-0.5,4.4,0.4,5.9,0.1c0.9-0.2,1.9-0.7,2.8-1c0.3-0.1,0.6,0.1,0.7,0.1c0.4,0,0.9-0.1,1.3-0.1c0.7,0.1,1.7,0.4,2.3,0.6c1.1,0.4,2.4-0.1,3.5,0.2c0.7,0.2,3,0.6,4.1,0.2c1.6-0.5,2.1-2.5,3-3.7c0.4-0.5,1.1-0.7,1.7-1.1c0.3-0.4,0.1-1.1,0.4-1.6c0.4-0.8,3.2-2.8,4.1-3.1c0.1-2.1,0.2-2.9,2-3.6c0.3-0.1,0.6,0,0.8-0.1c0.4-0.3,0.5-0.7,0.8-1.1C485.9,192.3,485.8,192,485.5,191.5z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Shadowland" "Brown Lands")} [:path {:className "st1" :d "M449.2,219.8c0.2-0.2,0.5-2,0.5-2.5c-0.3-0.3-0.2-0.6-0.5-1c-0.4-0.6-1.5-0.9-1.8-1.7c-0.3-0.7,0.3-1.5,0-2c-0.2-0.6-0.9-0.5-1.2-1c-0.3-0.4-0.3-1-0.5-1.4c-1.8,0-3.4-0.1-4.9,0.4c-0.4,0.1-1,0.5-1.4,0.7c-0.2,0.2-0.3,0.4-0.5,0.6c-0.3,0.1-4.7,0-4.8-0.1c-1.6-0.1-2.1-1.7-2.8-2.8c-2.4,0.1-2.8-1.1-3.6-2.6c-0.2-0.4-0.7-1.6-0.2-2.3c-0.1-0.6-0.2-0.6-0.5-1c-1.7-0.1-1.8,0.5-2.3,1.6c-2.2,0.5-5.1,0.7-7-0.1c-0.7-0.3-1.2-1.2-1.9-1.4c-0.6-0.3-1.3-0.1-1.9-0.4c-0.4-0.2-1-0.8-1.4-1c-0.8-0.3-1.7,0.2-2.3,0.2c-1.2,0.1-1.6-0.4-2.5-0.5c-0.6,1.2-2.2,1.6-3.1,2.5c-0.2,0.2-0.3,0.6-0.5,0.8c-0.4,0.5-1,0.7-1.3,1.2h-0.1c0.4,0.3,1,0.5,1.3,0.8c0.9,1.1-0.3,2.7-0.8,3.1c0.2,1.1,0.8,0.4,1.7,0.8c0.3,0.1,1,0.9,1.1,1.2v0.7c0,0,1.5,1.3,1.6,1.3c0.1,1.3,0.4,2.7-1,3.2c0.1,0.4,0.3,0.6,0.4,1.1c1.3,0.5,2.2,1.5,3.6,1.8h0.4c0.1,0,0.5-0.3,0.6-0.4h1.2c0.2-0.1,0.4-0.7,0.7-0.8c1.2-0.6,1.8,0,2.9-1.1c1.2,0,1.8,0.2,2.3,0.8c0,0,2.5-0.4,2.5-0.4c0.2-0.1,0.5-0.7,0.7-0.8c0.4-0.2,1.2,0.1,1.4,0c0.1-0.2,0.2-0.4,0.4-0.6c0.4-0.2,1.5-0.4,1.9-0.1c1,0.1,0.8,0.9,1.3,1.4c0.2,0.2,0.5,0.1,0.7,0.2c0.2,0.2,0.3,0.5,0.5,0.7c0.3,0.2,0.5,0.1,0.7,0.2c0.2,0.2,0.2,0.5,0.4,0.7c0.4,0.6,1.2,0.5,1.4,1.3c0.4,0.7-0.3,3.4-0.6,3.8c-0.2,0.3-0.3,0.4-0.8,0.5c0.2,0.3,1,0.9,1.3,1.1c0,0.9-0.2,2.5-0.7,2.9c0,0.4,0.1,0.4,0.2,0.7c1.4,0,1.9,0.2,3.1,0.4c1.1-1.1,2.3-2,3.2-3.2c1-0.1,2-0.2,3.1,0c0.1,0,0.6,0.2,1,0.1c0.3-0.1,0.5-0.5,0.7-0.6c0.5-0.2,0.9,0,1.4-0.2c0.5-0.2,1-0.8,1.4-1h1.1c0.5-0.1,1.3-0.3,1.8-0.6c0.6-0.3,1.1-1.2,1.7-1.6c0-0.8-0.5-0.9-0.6-1.3c-0.1-0.5,0.3-0.4,0.2-1c0-0.1-0.2-0.5-0.1-0.7C448.1,220.5,448.9,220.2,449.2,219.8z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Shadowland" "Dagorlad")} [:path {:className "st1" :d "M544.2,260.5c-0.2-0.1-0.6-0.6-0.8-0.7c-0.6-0.3-0.9,0.2-1.6,0l-0.2-0.2c-1.1-0.3-2.1,0.3-3.2,0c-0.5-0.1-1.2-0.6-1.7-0.7c-0.2,0-0.5,0.1-0.7,0.1c-0.1-0.2-0.2-0.3-0.2-0.5c-0.5-0.3-1.1,0-1.7-0.2c-0.2-0.1-0.4-0.4-0.6-0.5c-0.5-0.1-0.9,0.2-1.6,0c-1.3-0.3-2.8-0.7-4.1-1.1c-1.1-0.3-3-0.2-4.3-0.2H522c-0.7-0.3-1.8-0.6-2.5-0.8c-0.4,0-0.7,0.1-1.1,0.1c-0.1-0.1-0.4-0.5-0.6-0.6h-1.1c-0.2-0.1-0.6-0.4-0.8-0.5h-0.6c-0.1-0.2-0.2-0.3-0.4-0.5c-0.4-0.1-0.8-0.2-1.2-0.4c-0.5-0.3-0.8-0.9-1.3-1.2c-0.6-0.4-1.3-0.8-1.9-1.1H510c-0.8-0.4-1.3-1.6-2-2c-1.3-0.9-2.6-1.8-3.8-2.8c-0.3-0.4-0.6-0.7-0.8-1.1c-0.3-0.2-0.7-0.3-1-0.6c-0.3-0.4-0.5-0.9-0.8-1.2c-1.9-1.4-5.6-1.5-7.3-3.1c-0.5-0.5-0.9-1.2-1.3-1.8c-0.6-0.9-1.6-1.6-2.3-2.4c-0.8-0.9-2.1-1.4-2.8-2.4c-0.3-0.4-0.2-1.2-0.5-1.6c-0.2-0.2-0.7-0.3-0.8-0.6c-0.1-0.3-0.2-0.6-0.4-1c-0.4-0.6-1.3-1-1.8-1.6c-0.2-0.2-0.2-0.5-0.4-0.7c-1.6-1.6-4.1-2.4-5.4-4.2c-1-1.4-1.3-3-2-4.7c-0.2-0.4-0.8-0.8-1-1.2c-0.1-0.4-0.2-0.7-0.2-1.1c-0.3-0.2-0.6-0.5-0.8-0.7c-0.5-0.6-0.6-1.4-1-2c-0.2-0.1-0.3-0.2-0.5-0.4c-0.9-1.5-0.7-3.6,0-5.3c-0.9-0.3-2.4,0.2-3.5-0.1c-0.7-0.2-1.8-0.4-2.6-0.6c-1.1-0.2-1.9,0-2.9-0.2c-0.6-0.2-2.2-0.6-3.1-0.2c-0.5,0.2-1.3,0.7-1.8,0.8c-2,0-4.1-0.1-6.1-0.1c-1.7,0.5-3.2,1.4-5.3,1.6v0.4c0.6,0.4,1.2,0.9,1.4,1.6c0.3,0.8-0.4,1.5,0,2.2c0.2,0.3,0.6,0.3,0.8,0.5c0.6,0.4,1.1,1.6,1.3,2.4c0.6,2.2-1,3.3-1.4,4.4c-0.3,0.7,0.7,2-0.1,3.2c-0.7,1.1-2.2,2.1-3.6,2.5c-0.6,0.2-1,0-1.4,0.2c-0.3,0.1-0.6,0.5-0.8,0.6c-0.7,0.3-1.2,0.2-1.9,0.5c-0.2,0.1-0.7,0.5-1,0.6c-1.2,0.4-2.5-0.1-3.6,0.2c-0.5,0.2-0.8,0.8-1.3,1c0.4,0.5,0.3,0,0.8,0.2c0.6,0.2,1.8,1.3,2,1.9c0.3,0.5-0.2,1.6,0.1,2.3c0.4,0.3,0.7,0.6,1.1,1c0.3,0.5,0.1,1.4,0.4,1.9c0.1,0.1,0.4,0.3,0.5,0.5c0.3,0.7-0.2,1.9-0.2,2.3v0.2l0.2,0.5v2.3c0.3,0.2,0.6,0.5,0.8,0.7v1.3c0.1,0.4,0.6,0.6,0.7,1c0.2,0.7-0.3,1.9-0.1,2.4c0,0.1,0.4,0.4,0.5,0.5c0.2,0.4,0.3,1.9,0.1,2.5h-0.1c-0.3,0.9-1.1,1.1-1.4,1.8c-0.2,0.4-0.1,0.6-0.4,0.8c0.6,0.5,1.3,1.3,1.9,1.7c0,2.4,0.2,5.2-0.5,7c-0.2,0.5-0.8,0.9-1.1,1.3h-0.8c0,0.8,0.3,1.1,0.5,1.7c0.2,0.8,0,1.5,0.2,2.2c0.1,0.2,0.4,0.3,0.5,0.5v1.3c0.2,0.7,0.6,1.5,0.7,2.2c0.4,1.5-0.1,3.9,0.7,4.9c0.4,0.5,1.8,1.2,2.5,1.4h1.9c0.3,0.4,0.6,0.7,1,1.1c0.5,0,0.9,0,1.2,0.2c1-0.3,1.3-2,2-2.6c2.2-1.9,7-3.8,11-2.3V275h0.1c0.1-0.3-0.2-0.7-0.1-1.1c0-0.2,0.2-0.4,0.2-0.7c0.3-0.1,0.5-0.2,0.7-0.5c0-0.2-0.4-0.5-0.5-0.8c-0.3-0.8,0.5-1.6,0.7-2c0.4-0.2,0.8-0.1,1.3-0.2c0.3-0.1,0.6-0.5,0.8-0.6c0.9-0.3,1.8,0.2,2.3,0.4c0.2-0.2,0.4-0.4,0.6-0.5v-0.4c0.6-0.1,0.7-0.2,1.4-0.2c0-0.4,0.1-0.4,0-0.6v-0.4h0.2v0.7h0.7v-0.8c0.3,0.2,0.5,0.5,0.7,0.8c1.9-0.5,4.4-1.7,5.3-3.1c0.5-0.8,0.6-1.4,1.3-1.9c1.7-1.1,5.7-1,8.5-1c4,0,6.6-0.6,10.4-0.6c2.2,0,5.5-0.2,6.7,0.7c0.7,0,0.6,0.1,0.6-0.4h0.2v0.6c0.9,0,2.5,0.5,2.8,0.4c0.1,0.1,0.1,0.1,0.1,0.4c0.5,0,0.9-0.4,1.3-0.2c0.2,0.1,0.3,0.4,0.6,0.5h1.7c0.3,0.1,0.7,0.5,1.2,0.6c0.6,0.1,0.9-0.8,1.7-0.6l0.1,0.2c0.3,0.1,0.2-0.2,0.4-0.2c0,0,2.2,0.2,2.3,0.2v0.2c0.1,0.1,1.3,0.5,1.3,0.5c0.7,0.2,1.7-0.8,3-0.7c0.6,0,1.2,0.2,1.7,0.5c0.1,0.1,0.2,0.4,0.2,0.5c1,0.6,4.6,0,5.4-0.5c0.6,0.1,1.4,0.3,1.7,0.7c0.3,0.3-0.1,0.3,0.4,0.5c0.8,0.2,3-1.4,4.2-0.8c1.3,0,1.2,0.8,1.9,1.2c0.5,0.3,0.8,0.1,1.1,0.5c0.3-0.1,1-0.3,1.2-0.5c0.2-0.2,0.1-0.6,0.4-0.7h0.5c0.2-0.1,0.3-0.6,0.5-0.7h0.5c0.3-0.1,2.6-1.7,2.8-1.9c0.3-0.5,0.5-1.2,0.8-1.7C544.9,260.6,544.4,260.6,544.2,260.5z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Ithilien")} [:path {:className "st3" :d "M488.3,359.6c-0.1-0.4-0.2-0.7-0.4-1.1c-0.1-0.2-0.4,0-0.6-0.1c-0.2-0.1-1-1.5-1.1-1.8c0-0.6,0.1-1.3,0.1-1.9c-0.1-0.3-0.7-0.7-0.8-1.1c-0.2-0.5-0.7-5-0.6-5.6c0.1-0.6,1-2,1-2.4c0-0.2-0.4-0.5-0.5-0.7c-0.4-1.1,0.4-3.5,0.1-4.7c-0.1-0.4-0.5-2.8-0.5-3c0.1-0.4,0.6-1.2,0.5-1.8c-0.1-0.7-0.6-1.1-0.6-2c-0.6-0.3-1.3-0.8-1.9-1.2c0-2.1-1.1-2.3-1.7-3.6v-0.7c-0.1-0.2-0.5-0.5-0.6-0.7c-0.3-0.7-0.1-1.2-0.4-1.9c-0.1-0.2-0.4-0.6-0.5-0.8c-0.1-0.3,0.1-0.6,0.1-0.7c-0.2-0.6-0.9-0.9-1.1-1.4c-0.4-1.2,0.5-2,0.7-2.6v-1.4c0.1-0.4,0.5-0.8,0.4-1.3v-0.1l-0.2-0.1c-0.2-0.7,0.4-1.1,0.2-1.6c-0.1-0.3-0.5-0.7-0.7-0.8c0-1.3,0.4-1.8,0.6-2.6v-2.3c0-0.1,0.3-0.5,0.4-0.6c0.3-1,0-1.6,0.6-2.2c-0.1-0.9-0.9-1.1-1.7-1.1c-0.5-1-0.6-2.8-1-4c-0.3-0.9,0.3-1.5-0.1-2.3c-0.1-1-1.1-1.6-1.7-2.2c0-0.4,0-0.5,0.2-0.7c-0.1-1.4-1.1-1.2-1.6-2c-0.5-0.8-0.1-2.3-0.6-3.1c-0.4-0.6-1.6-0.6-2-1.2c-0.3-0.4,0-0.9-0.2-1.4c-0.2-0.4-0.9-0.3-1.2-0.6c-1.3-1.6-1.7-3.7-3.5-4.8c-0.2-0.1-0.4,0.2-0.7,0c-0.6-0.3-0.4-1.1-0.6-1.6c-0.1-0.3-1.9-1.5-2.3-1.6c-0.1-0.5-0.2-1.3,0.1-1.7c-0.1-1.5-0.6-0.6-1.7-1.1c-0.4-0.2-1-0.7-1.4-0.8c-2-0.7-6,0.8-7.1,1.6c-0.6,0.4-1,1.3-1.6,1.8c0,0.1-0.1,0.2-0.1,0.4c0.9,0.3,2.4,0,3.4,0.4c0.8,0.3,1.5,1,2.2,1.3c1.3,0.7,2.5,0.9,3.4,2c0.5,0.7,0.5,2,1.1,2.6c0.4,0.5,1.1,0.7,1.3,1.4v1.6c0.3,0.8,1,1.3,1.4,1.9c0.2,0.2,0,0.5,0.1,0.8c0.3,0.3,0.6,0.6,1,0.8v0.5c0.4,0.3,0.8,0.6,1.2,1c0,0.4,0,0.9,0.1,1.1c0,1.4,0.8,2.3,1.6,2.8c0,1.5,0,3.3,0.6,4.3c0.2,0.5,0.7,0.8,0.8,1.4c0.1,0.3,0.2,3.6,0.2,3.6c-0.1,0.2-0.4,0-0.4,0.5c0.1,1,0.5,1.9,0.7,2.6v4.9c-0.5,0.5-0.9,1.4-1.6,1.7c-0.5,0.2-0.8,0.1-1.2,0.4c-0.2,0.3-0.5,0.6-0.7,0.8c-0.3,0-0.6,0.1-0.8,0.1c-0.1,0.1-0.3,0.4-0.5,0.5H465c-0.8,0.3-1.1,1.5-2,1.8v1.6c0.1,0.3,0.6,0.4,0.8,0.6c0.4,0.3,0.7,0.9,1.1,1.3c0.5,0.7,1.6,1.2,2,1.9c0.5,0.9,0.5,2.7,0.8,3.7c0.2,0.6,0.8,1.2,1.1,1.8v0.5c0.2,0.4,0.8,0.7,1,1.1c0.3,0.8,0,4-0.2,4.4c-0.1,0.2-0.5,0.7-0.6,1c-0.4,1.2,0.1,2.3-0.4,3.2c-0.3,0.7-1.3,1.2-1.8,1.8c-0.4,0.6-0.6,1.3-1.1,1.8c-0.4,0.4-1.2,0.8-1.4,1.2c-0.1,0.4-0.2,0.8-0.2,1.2c-0.2,0.5-1,0.8-1.2,1.2c-0.1,0.5-0.2,1-0.2,1.6c0,0-3.1,2.4-3.1,2.4h-0.8c-0.3,0.1-0.7,0.5-1,0.6c-0.9,0.3-2.4,0.1-2.8,0.2c-0.8,0.3-1.8,0.9-2.5,1.2h-1.1c-0.7,0.2-1.4,0.4-2.2,0.6c-1.1,0.6-1.3,2.2-2.3,2.9c-0.7,0.5-2,0.3-2.6,0.7c-0.8,0.6-1.3,1.7-2.2,2.2c-0.7,0.4-1.2,0.4-1.7,1c-0.5,0.5-0.8,1.4-1.4,1.8c-0.4,0.2-0.8,0.1-1.2,0.4c-0.6,0.4-0.9,1.3-1.6,1.7v0.1c0.6,0.3,1.4,0.5,1.9,0.8c0.5,0.3,0.9,1,1.3,1.3c0.4,0.3,0.8,0.3,1.2,0.6c0.1,0.1,0.1,0.4,0.2,0.5c0.3,0.2,0.6,0,1,0.2c0.5,0.3,1.1,1,1.6,1.2c0.8,0.3,2.9-0.1,3.5,0.4c1.8,1.3,4.4,2.9,7.4,2.3h1.6c0.6-0.2,1.3-0.9,1.9-1.2c1.7-0.9,3.4-1.3,5.5-1.7c0.9-0.2,3.8,1.1,4.7,1.3c0.8,0.2,4.6,0,5-0.2c0.3-0.2,0.6-0.7,0.8-0.8c0.9-0.6,2-1.2,2.9-1.7c1.1-0.6,2.1-0.7,2.6-1.8c0.1-0.1,0.1-0.1,0.1-0.4c0.5-0.1,0.6-0.6,1.1-0.7c1-0.3,1.8-0.2,2.5-0.7c0.6-0.4,1-1.4,1.7-1.7c1-0.4,4.2,0.4,4.4,0.2c0.3-0.1,0.2-0.4,0.7-0.5c0-1.9,0.9-1.8,1.1-2.9C489.8,361.2,488.8,360.4,488.3,359.6z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Shadowland" "Khand")} [:path {:className "st1" :d "M669.1,310.8c-1.7,0.3-2,2.7-3.2,3.6c-0.8,0.6-1.6,1.2-2.2,1.9c-0.4,0.5-0.8,1.2-1.3,1.6c-1.1,0.7-2.2,0.4-3,1.6c-0.3,0.5-0.9,1.1-1.1,1.6c-0.1,0.4-0.2,0.8-0.2,1.2c-0.2,0.3-0.7,0.3-1,0.6c-0.1,0.2-0.2,0.5-0.2,0.7c-0.2,0.1-0.5-0.1-0.6-0.1c-0.7,0.4-1.3,1.5-2.2,1.9c-0.5,0.2-1.5,0.1-1.8,0.5c-0.3,0.4-0.2,1.9-0.2,2.4c0.3,0.3,0.6,0.9,1,1.1c0.4,0.2,0.7,0,1.1,0.2c0.1,0.2,0.2,0.3,0.4,0.5h1.6c0.5,0.1,1.6,0.4,2,0.7c0.4,0.3,1.4,1.5,1.6,2v1.9c0,0.1,0,0.3,0,0.5c-0.1,0.9,0.2,1.7-0.7,2.3c0.1,0.4,0.2,0.5,0.4,0.8c0.7,0.2,1.6,0.2,2.2,0.6c0.2,0.3,0.4,0.6,0.6,0.8c0,0.9-0.1,1.8-0.7,2c0.1,1,0.8,0.9,0.8,2.3c-0.3,0.3-0.4,0.7-1,0.8c-0.6,0.4-1.6,0-2,0.2H659v0.1c0.6,0.5,2.6,2.2,1.3,3.6c-0.4,0.9-1.6,0.7-2.4,1.2h-0.2v0.4c-0.7,0-1.1,0.1-1.6,0.4c-0.2,0.1-0.3,0.6-0.6,0.7c-1.3,0.5-2.9-0.6-4.1,0.1c-0.4,0.1-0.4,0.3-0.7,0.5c0,0.3-0.1,0.6-0.1,1c-0.6,0-3,0.5-3.5,0.8c-0.4,0.3-0.1,0.6-0.4,1.1c0,0-1.5,1.4-1.6,1.4c-0.7,0.5-1.5,1-2.4,1.2h-1.2c-1.6,0.6-3.2,1.3-5.5,1c-0.5-0.1-2,0.5-2.8,0c-1,0.2-1.7,1.3-2.5,1.8c-0.6,0.4-1.1,0.1-1.8,0.4c-0.6,0.3-0.8,1.2-1.3,1.6c-0.1,0.1-2.9,1.1-3.1,1.2c-0.1,0.8,0,2.5,0,3.7c-0.4,0.3-0.8,0.7-1.1,1.1c-0.5,0.8,0.5,3.8,0,4.4c-0.1,1.8-2,1.5-3.6,1.6c-0.2,0-0.8,0.3-1.2,0.1c-0.1-0.1-0.2-0.2-0.2-0.4c-0.5-0.2-1.5-0.4-1.9-0.5c-0.7-0.1-1.6,0.4-2.3,0.5v0.4c0.7,0.3,2.3,0.8,2.8,1.3c0.2,0.2,0.1,0.6,0.2,0.8c0,0,1.7,1.3,1.7,1.3c0.1,0.5,0.2,1-0.1,1.3c-0.1,0.9-0.7,1-1.3,1.4c-0.5,0.4-1,1.2-1.6,1.6c-0.4,0.2-0.9,0.1-1.3,0.4c-0.2,0.2-0.3,0.4-0.5,0.6c-0.3,0.2-0.5,0-0.7,0c0,0-1.6,0.5-1.8,0.6c-1.9,0.6-3.2-0.9-4.7,0c-1.3,0-2.2-0.5-2.6-1.3c-1,0-1.4,0.6-2,0h-0.7c0.3,0.8,0.7,1,0.7,2.2c-0.7,0.6-1.5,1.5-2.4,1.8c-0.5,0.2-0.5-0.4-1-0.2c-0.2,0-1.8,0.4-2,0.5c-0.3,0.2-0.7,0.6-1.1,0.7c-0.4,0.1-0.8,0-1.1,0.1c-1.8,0.1-1.5,1.9-2.4,2.8c-0.3,0.3-0.8,0.2-1.2,0.5c0,0.1-0.1,0.2-0.1,0.4c-0.5,0.6-1.4,1.6-2.2,1.9c-0.5,0.2-1.6,0.4-2.3,0.2c-0.1,0-0.4-0.3-0.6-0.2c-0.7,0.1-3.6,0.6-4.3,0.2c-0.1-0.1-0.1-0.4-0.2-0.5H581c-0.2-0.1-0.4-0.5-0.6-0.6c-0.4-0.3-0.7-0.4-1-0.8c-0.5,0.2-1.1,0.6-1.6,0.7h-0.1c-0.4,0.1-0.6-0.2-0.8-0.1c-0.2,0-0.5,0.3-0.8,0.2c-0.2,0-0.5-0.4-0.8-0.4c-0.8,0.1-1.9,0.5-2.6,0.7c-0.3,0.1-0.9-0.1-1.1-0.1c-0.6,0-1.8,0.2-2.2,0.4c-1,0.3-2.3-0.3-3.1-0.1c-1.7,0.3-5,0.9-6.5,0.1c-0.7,0-0.7,0.1-1,0.6c-0.8,0-2-0.4-3-0.1c-0.4,0.1-1.2,0.5-1.6,0.6c-0.2,0-0.5-0.1-0.7-0.1c-0.9,0-1.8,0.1-3,0.1c-0.4,0-0.8-0.1-1.2-0.1c-0.5,0.1-1,0.5-1.3,0.6c-0.7,0.3-2.2,0.3-2.9,0.5H544c-0.1,0.2-0.2,0.3-0.4,0.5c-1,0.5-2.7-0.1-3.6,0.4c-0.7,0.3-0.9,1-1.4,1.4c-1,0.7-2,1.2-3.1,1.8c0,0.6-0.1,2.5,0,2.9c0.1,0.3,0.4,0.9,0.2,1.6c-0.1,0.2-0.4,0.6-0.5,0.7c-0.1,0.2,0,1.1,0,1.3c-0.1,0.6,0.3,0.8,0.4,1.1c0.1,0.4-0.3,2.8-0.5,3.1c-0.3,0.7-1.5,1.7-2.2,2c-0.5,0.3-1.2,0.2-1.7,0.5c-0.9,0.6-1.6,2.4-2.3,3.4c-0.4,0.6-1.4,0.5-2.2,0.8c-0.6,0.3-0.9,1.1-1.3,1.6c-0.3,0.3-0.5,0.1-1,0.4c-0.4,0.2-0.8,0.8-1.2,1.1c0,0.7-0.3,1.8-0.4,2.3c1.1,1.5,0.1,3.8-0.4,5c-0.1,0.2,0.2,0.2,0.2,0.2c0.1,0.5,0.3,3.1,0.1,3.5c-0.3,0.6-1.2,1.2-1.4,1.8c-0.1,0.8-0.2,1.5-0.4,2.3c0.1,0.4,0.4,0.8,0.2,1.4c-0.1,0.7-0.4,2-0.2,2.8c0.1,0.5,0.3,0.9,0.4,1.4h0.1v0.1c1.5,0.3,3.7,0.1,5.4,0.1h145.2V311.4C671.9,311.4,669.3,311,669.1,310.8z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Harondor")} [:path {:className "st3" :d "M535.7,393.2c-0.3-0.1-0.3-0.6-0.6-0.7c-0.6-0.3-1.2-0.1-1.8-0.4c-0.1,0-0.3-0.3-0.4-0.4h-1.2c-0.1-0.1-0.1-0.5-0.2-0.6c-0.5-0.3-1.2,0.1-1.8-0.2c-0.5-0.2-1-0.9-1.3-1.3c-1.2,0.1-2.1,0.7-3.1,1.1c-0.2,0.1-3.4,0.3-3.5,0.2c-0.1-0.1-0.2-0.3-0.4-0.4c-0.4,0.4-1.1,0.3-1.7,0.6c-0.1,0.2-0.2,0.4-0.4,0.6c-0.3,0.1-0.7-0.1-0.8-0.1c-0.2,0.1-0.6,0.6-1.2,0.4c-0.4-0.2-1.7-0.5-2.4-0.2c-0.5,0.2-2.3,0.4-2.9,0.2c-0.4-0.1-1-0.5-1.4-0.6c-1.5-0.4-2,0.6-2.6,0.8c-0.5,0.2-1.9-0.2-2.4,0c-0.2,0-0.7,0.5-1.2,0.2v-0.2h-1.4c-0.2-0.1-0.2-0.5-0.5-0.6c-0.7-0.2-1.3,0.1-1.8-0.2c-0.2-0.1-0.3-0.6-0.5-0.7c-0.7-0.4-1.6-0.3-2.3-0.7c-0.5-0.3-0.6-1.2-1.1-1.4c-0.6-0.3-1.6,0.2-2.3-0.1c-0.5-0.2-1.1-0.5-1.9-0.5c-0.3-0.6-0.9-1.2-1-1.6c0-0.4,0.1-0.8,0.1-1.2c-0.2-0.5-0.7-0.7-1-1.1c-0.1-0.1-0.6-2.1-0.6-2.3c-0.3-0.1-0.6-0.2-0.8-0.2c0.1-1.1,0.3-0.5,0.6-1c0.1-0.3,0.1-1.2,0.2-1.4c-0.5-0.4-1.1-0.5-1.7-0.8c-1.1-0.7-1.7-1.6-3.1-2c-0.1-1,0-1.7,0.5-2.4c-0.9-0.7-1.5-1.9-2.6-2.3c0-0.5,0.1-1,0.1-1.6h0.2c0.1-0.2-0.2-0.8-0.2-1.3c0.5-0.4,1-1.3,1.6-1.6c0.8-0.4,1.4-0.4,2-0.8h0.2c-0.2-0.3-0.8-0.4-1.2-0.5c-0.2,0.3-0.3,0-0.6,0.2c0,0.1-0.1,0.2-0.1,0.4c-0.2,0.2-1,0.9-1.2,1.1c-0.5,0.4-1.2,0.4-1.6,0.8c-0.5,0-0.6,0-0.8-0.2c-0.6,0.5-2,2.2-2.5,2.5c-0.4,0.2-0.8,0-1.2,0.2c-1.8,0.9-3.4,2.5-5.5,3.1c-1.7,0.5-6.5-0.3-7.8-0.8c-0.5-0.2-1.6-0.8-2.6-0.5c-0.9,0.3-1.9,0.9-2.8,1.3c-0.6,0.3-1.1,0.1-1.6,0.5c-0.4,0.2-0.8,0.8-1.2,1c-0.9,0.3-5.8-0.1-6.5-0.2h-1.2c-0.7-0.2-0.9-1.1-1.4-1.4c-0.2-0.1-1.1-0.5-1.3-0.6c-1-0.4-1.6,0.1-2.6-0.2c-0.7-0.3-1.4-0.9-2-1.3c-0.4-0.3-0.9-0.2-1.3-0.5c-0.3-0.2-0.5-0.5-0.7-0.7c-1.3-1-2.7-2-4.2-2.4c-1,1.5-4.1,2.3-5.6,3.4c-0.5,0.3-0.8,1.3-1.2,1.8c-0.2,0.3-0.9,0.5-1.1,0.8c0,0.2-0.1,0.4-0.1,0.6l-1.6,1.4c-0.1,0.1-0.1,0.4-0.2,0.5c-0.3,0.3-0.9,0.4-1.2,0.7c-0.1,0.4-0.2,0.7-0.2,1.1c-0.2,0.3-0.7,0.7-0.8,1c-0.2,0.5-0.1,1-0.4,1.4c-0.3,0.5-1,0.7-1.2,1.3c-0.2,0.6,0.1,1.4,0.2,1.7c0.3,0.9,0.7,4.3,0.4,5.5c-0.3,1.1-1,2.4-1.3,3.6c-0.3,1.1-0.1,1.9-0.6,2.8c-0.3,0.5-1.1,0.9-1.3,1.4c-0.4,0.7-0.2,1.5-0.6,2.2c-0.6,0.9-1.2,1.9-1.8,2.8c-0.4,0.6-1.3,0.9-1.7,1.4c-0.4,0.6-0.3,2-1,2.4c-0.7,0.4-3,1-2.3,2.3c0.2,1.3,1.5,1.4,2,2.3c0.5,0.7,0.2,1.5,0.8,2.2c-0.1,0.5-0.1,0.6-0.5,0.7c0.1,0.9,0.6,0.8,0.8,1.3c0.2,0.5-0.3,1.6-0.1,2.3c0.3,1,0.8,3,1.3,3.7c0.4,0.5,1.2,0.8,1.6,1.3c0.3,0.4,0.6,1.6,0.7,2c0.4,1.2-0.1,4.4-0.5,5c-0.4,0.6-1.9,1.9-1.1,3.2c0,2.1,2,3.1,2.5,4.8c0.3,1-0.3,2.3,0.2,3v0.2h92.2c2.1,0,6.5,0.4,7.8-0.4c0.1-1.7,0-4.1,0.2-5.4c0-0.7-0.1-1.4-0.1-2.2c0.1-0.4,0.2-1.2,0.4-1.6c0.2-0.4,0.9-0.5,1.1-0.8v-0.6c0-0.1,0.3-0.3,0.4-0.4c0.2-0.6-0.3-1-0.4-1.7c0-0.6,0.1-1.8,0-2c0-0.2,0.2-0.4,0.5-0.6l0.1-1.6c-0.1-0.5-0.7-1.3-0.8-1.8c-0.3-1,0.2-2,0.2-3c0.3-0.3,0.5-1.1,1-1.3c0.7-0.4,1.6-0.2,2.3-0.6c0.6-0.4,0.7-1.5,1.3-1.8c0.5-0.3,1.1,0,1.6-0.2c0.4-0.3,0.5-0.9,0.8-1.3c0.5-0.8,1.5-1.9,2.3-2.4c1.3-0.9,3-1.2,3.6-2.8c0.2-0.5-0.2-1-0.4-1.6c-0.3-1.1,0.1-2.7,0.2-3.4c0.2-0.7-0.1-1.5,0-2.2c0.3-1.2-0.7-2,0-2.8c0.3-0.7,0.7-0.4,1.3-0.7c0.2-0.2,0.3-0.4,0.5-0.6c0.5-0.4,1.1-0.5,1.6-0.8c0.4-0.3,0.7-0.9,1.1-1.2C537.8,393.4,536.6,393.8,535.7,393.2z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Coastal Sea" "Mouths of the Anduin")} [:path {:className "st4" :d "M428.9,366.4c-0.1-0.3-0.1-0.8-0.1-1c-0.9,0.1-2.7,0-3.5,0.4c-0.6,0.2-1,1-1.7,1.3c-0.9,0.4-2.5,0.3-3.4,0.7c-0.9,0.4-1.6,1.9-2.4,2.3h-0.6c-0.3,0.2-0.6,0.7-1,0.8h-7.1c-1.5,0.5-2.5,1.6-3.7,2.3c0.1,0.8,0.7,0.7,1,1.3c0.2,0.3,0.4,1,0.1,1.3c-0.1,1.2-1.6,2.2-2.6,2.5c-0.9,0.2-1.3-0.6-2.2-0.4c-0.9,0.2-2.1,1-2.9,1.4c0.2,1.2,0.8,1.3,0.7,2.9c-0.2,0.2-0.4,0.4-0.5,0.6c-4.9,0.1-6.1-0.4-8-3.4c-1.8,0.1-0.8,1.9-1.2,3.4c-0.4,1.6-1.7,2.4-2.9,3.2c0.3,1.8,1,3.8,0,5.5c-0.5,0.8-2.1,1.1-2.5,1.8c-0.4,0.6-0.3,1.4-0.6,1.9c-0.4,0.6-1.2,1.6-1.8,2c-0.3,0.2-0.9,0.4-1.1,0.7c-0.1,0.3-0.2,0.6-0.2,0.8c-0.2,0.3-1.8,1.2-2.2,1.4v0.1c-0.4,0.2-0.8,0-1.1,0.1c-0.5,0.3-1.1,0.9-1.7,1.2c-0.8,0.3-1.4,0.2-2.3,0.5c-0.4,0.1-1.3,0.6-1.9,0.2h-1.8c-0.2,0.1-0.7,0.9-1,1.1h-0.4c-0.3,0.1-0.6,0.6-0.8,0.7c-0.5,0.2-0.8,0-1.2,0.2c-0.2,0.3-0.4,0.6-0.6,0.8c-1.3,0.9-2.5,2.3-3.4,3.6c-0.4,0.6-0.8,1.5-1.1,2.2c-0.3,0.6-0.1,1.5-0.5,1.9c-0.5,0.5-2,1.5-2.6,1.8c-0.8,0.4-1.6,0.3-2.3,0.7c-0.2,0.2-0.4,0.5-0.6,0.7H355c-0.2,0.1-0.2,0.5-0.4,0.6c-0.3,0.2-0.8,0.1-1.1,0.4c-0.1,0.1-0.1,0.4-0.2,0.5c-0.2,0.2-0.6,0.2-0.8,0.4c-0.2,0.2-0.4,0.5-0.6,0.7h-0.5c-0.2,0.1-0.2,0.5-0.4,0.6h-1c-0.4,0.1-1.3,0.7-1.6,1.1c-1.2,1.7-1.6,4-2.8,5.6c-0.9,1.2-3.2,2.2-3.7,3.5l-0.1,1.6c-0.8,2.7-3,6.9-5.2,8.2c0,0.3,0,0.4-0.1,0.6v0.2h81.1c-0.2-1-0.3-2-0.5-3c-0.3-0.7-1-1.2-1.4-1.8c-0.5-0.8-0.6-1.9-0.8-3c-0.5-2.4,1-3.3,1.6-4.6c0.5-1,0-4.1-0.4-4.8c-0.3-0.6-1.2-1-1.6-1.6c-0.4-0.6-0.8-1.6-1.1-2.3c-0.6-1.3-0.1-2.5-0.5-4c-0.2-0.7-0.7-1.3-0.8-2.2c0-0.4,0.1-0.8,0.1-1.2c-0.2-0.5-0.7-0.6-1-1c-1-1.2-1.8-2.1-1.7-4.1c0.8-0.7,1.9-2.6,3.1-2.8c0.3-2,2-2.5,2.9-3.8c0.4-0.6,1.2-1.8,1.4-2.4c0.2-0.6,0.1-1.2,0.4-1.7c0.4-0.7,1.2-1.2,1.6-2.2c0.3-0.9,0.2-1.8,0.5-2.8c0.3-1,1.5-3.1,1-4.8c-0.2-0.5-1-3.9-0.8-4.4c0.3-1.1,1.1-1.6,1.6-2.4c0.7-1.1,0.9-2.4,1.6-3.5c0.3-0.4,0.8-0.4,1.2-0.7c0.3-0.2,0.5-0.8,0.7-1.1c0.2-0.2,0.4-0.1,0.6-0.2c0.3-0.2,0.4-0.7,0.6-1c0.5-0.6,1.2-1.2,1.7-1.8c0.2-0.3,0.4-0.9,0.7-1.1c0.7-0.5,1.7-0.7,2.3-1.3C430.9,368,429.2,367,428.9,366.4z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Freedomain" "Belfalas")} [:path {:className "st5" :d "M410.4,340.8c-0.2-0.2-0.2-0.6-0.4-0.7c-0.3-0.3-0.9-0.5-1.2-0.7c-0.8-0.6-1.2-1.8-1.7-2.6c-1.7-0.7-4.2-0.7-5.9-1.3c-0.7,0-1.4-0.1-2-0.1c-0.8-0.3-2-1.5-3.2-1.2c-0.8,0.2-1.5,0.8-2.3,1.1c-1.1,0.3-2.1,0-3.2,0.4c-0.5,0.1-1,0.7-1.4,0.8c-0.7,0.3-1.2-0.1-1.8,0.2c-0.4,0.2-0.5,0.7-1,0.8c-1.6,0.5-3.5-1.5-4.7-1.1c-1.7,0.6-3.7,1.5-5.5,2h-1l-1.7,0.8c-0.5,0.4-0.7,1.2-1.2,1.7c-0.5,0.5-1.5,1.2-2.2,1.6c-1,0.6-2.2,0.5-3.4,1c-0.4,0.2-1,0.7-1.4,0.8h-0.7c-0.5,0.3-0.7,1.1-1,1.6c-0.5,0.8-1.6,0.8-2.4,1.3c-0.3,0.2-0.8,1-1,1.3c-0.1,0.4,0,0.8-0.1,1.1c-0.2,0.4-0.4,0.5-1,0.6c-0.2,0.6,0,1.2-0.1,1.8c-0.1,0.6-0.5,1.4-0.6,1.9l-0.1,1.4c-0.1,0.3-0.6,1.3-0.5,1.8c0.1,0.3,0.6,0.7,0.7,1c0.3,0.6,0,1.3,0.2,1.7c0,0.8-0.3,0.8-0.6,1.2c-1.3,0-2.3-0.2-3.2-0.4v-1.2h-0.4v0.5c-2,0-1.8,1.3-2.4,2.6c-0.2,0.4-0.6,1-1,1.2l-0.4-0.1c-0.6,0.4-0.6,2.4-1,2.9c-0.7,1.1-2.6,0.9-3.6,1.7c0,1.9,2.7,2.1,4.1,2.6c0.2,1.2-0.3,1.6-0.6,2.5c-0.5-0.3-2.2,0.1-2.8,0.6c-0.2,0.2-0.3,0.6-0.1,0.8v0.2c1.4,0,1.8-0.2,2.6-0.5h1.8c1.5,0,2.6-0.7,3-1.9h0.2v0.5c0.7-0.1,1.2-0.7,1.7-0.6c1.2,0.2,1.1,1.9,1.6,2.9c0.1,0.3,1.9,2,2.3,2.3c0.6,5.1-3.2,4.6-4.9,7c-0.3-0.2-0.1,0-0.2-0.4c-0.7,0.1-1.5,0.9-1.7,1.4c-0.2,0.5,0,0.9-0.2,1.3c-0.3,0.5-2.8,2.8-3.4,3v0.5c1.8,0,3.7,0.2,4.8-0.8c1.6,0,0.6,0.5,1.3,1h0.5c0.1,0.1,0.4,0.6,0.5,0.7c2.1,0,3.3-1.3,4.1-2.8c1.2,0,3.6,1,4.3,0.7c0.2-0.1,0.6-0.6,0.8-0.7c0.4-0.2,0.6,0,0.8-0.2c1.6-0.1,2.4,1,2.5,2.3c3.1,0.5,2.5,4,2.4,7.3c-0.3,0-0.4-0.1-0.6-0.1c-0.5,0.8-1.2,1.5-1.8,2.3c-0.3,0.2-0.6,0.4-0.8,0.6c-0.2,0.2-0.2,0.7-0.4,0.8c-0.3,0.3-0.9,0.1-1.2,0.4c-0.4,0.3-0.7,0.7-1.2,1v0.4c2.4,0,2.1-1.8,3.6-2.2c0.1,0,1.9,0,1.9,0c0.2,0.1,0.4,0.4,0.8,0.4c0.5-0.1,0.9-0.5,1.2-0.6c0.6-0.3,0.9,0,1.6-0.2c0.8-0.3,1.6-0.9,2.2-1.4c0.2-0.1,0.4-0.1,0.6,0.1c0.2-0.1,1.7-1.1,1.8-1.2c0.4-0.5,0.5-1,1-1.3c0.8-0.6,2.1-1.4,2.5-2.3v-1.1c0.3-1,1.2-1.1,2-1.7c0.5-0.3,0.5-0.9,1.1-1.1c0-0.7,0.1-1.4,0.1-2c-0.2-0.5-0.9-1.9-0.6-2.8c0.3-0.8,2.1-2.1,2.8-2.6c0-1.2,0-3.3,0.5-4.2c0.2-0.3,0.6-0.5,0.8-0.7c2.7-0.1,3.1,0.8,4,2.5c1.2,0.5,2.8,0.6,4.4,0.7c-0.2-1.2-1.1-0.6-1.9-1.1c-0.6-0.4-2-2.9-2.4-3.6h-0.5c-0.1-0.8-1-1.5-1.6-1.8l-0.2-1.3c0.1-0.5,0.6-0.9,0.7-1.4c0.2-0.9-0.1-1.8,0.2-2.5c0.5-1.1,1.5-2.3,2.2-3.2c0.2-0.4,0.1-0.7,0.2-1.2c0.2-0.6,0.9-1.5,1.3-1.8c1-0.7,2.3-0.5,3.2-1.3c0.2-0.2,0.3-0.6,0.5-0.8c0.4-0.5,1.1-0.8,1.6-1.3c0-0.1,0.1-0.2,0.1-0.4c0.9-0.7,3.3-1.1,4.2-1.4c0.8-0.3,1.6-1,2.6-1.1c-0.1-0.4-0.3-0.9-0.4-1.2c-0.1-0.5,0.3-1.2,0.2-1.3c-0.3-1.4-1-2.5-0.5-3.8c0.4-1,1.6-1.5,1.9-2.3c0.3-0.7,0-1.5,0.4-2.2c0.4-0.7,1.4-1,1.8-1.8c0.2-0.5,0-1.1,0.4-1.4C412.2,341.5,411.2,341.6,410.4,340.8z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Borderland" "Lamedon")} [:path {:className "st2" :d "M414.4,321c0-1.5-0.1-2.9-0.4-4.1c-0.2-0.9,0.6-1.5,0.2-2.3c-0.3-0.8-1.2-1.3-1-2.5c-0.6-0.4-1.6-0.5-2-1.1c-2.2,0.2-4,1-5.6,1.8c-0.1,0.2-0.2,0.3-0.4,0.5c-0.6,0.3-1.8,0.3-2.4,0.5h-1.4c-0.2,0.1-0.5,0.6-0.7,0.7c-0.4,0.2-0.8,0.2-1.2,0.4c0,0.1-0.1,0.2-0.1,0.2c-0.1,0-0.3-0.1-0.5-0.1c-1.3,0-3.9-1.1-5.4-0.6c-0.3,0.1-0.5,0.6-0.7,0.7c-0.7,0.4-1.4,0.3-2.3,0.2c-0.5-0.5-1.4-0.8-1.8-1.3c-0.2-0.3-0.4-1.2-0.7-1.4c-0.3,0-0.6-0.1-0.8-0.1c-0.6-0.4-1-1.2-1.2-1.9c-0.6-0.1-1.2-0.7-1.9-0.6c-0.9,0.1-1.9,0.4-2.8,0.6l-3.1-0.1c-0.6,0.1-3.3,0.6-4.2,0.2c-0.5-0.2-1.2-0.8-1.4-1.2c-1.2,0-1.2,0.4-1.9,0.7c-0.7,0.3-3.3,0.2-4.1,0.1c-0.7-1.4-1.7-1.7-2.3-2.9h-3c-1.4-0.4-2.6-1.4-4-1.8c-0.7-0.2-1.5,0.3-2.3,0.4c-0.1-1.3-1.7-1.3-2.4-2c-1.1,0.3-2,0.6-2.9,1.1c-0.1,0.1-0.3,0.4-0.5,0.5H347c-0.5,0.2-3.4,1-3.8,0.8c-0.3-0.1-0.5-0.5-0.8-0.5c-1.4,0.1-1.3,1.5-3.6,0.7c-1.5,0-3-0.1-4.4-0.1c-0.1,0-0.4,0.2-0.7,0.1c0-0.1-0.1-0.2-0.1-0.2c-0.6-0.1-0.5,0.5-1.2-0.1c-0.5,0.2-1,0.6-1.3,1.1h-0.1v0.1h0.1c0.3,0.9,3.2,2.2,2.2,3.8c0,0.6-0.2,0.8-0.7,1c0.1,1.3,0.5,1.3,0.7,2c0,0.6-0.1,1.1-0.1,1.7c-0.4,0.3-0.7,1-1.2,1.2c-0.4,0.2-0.8,0.1-1.1,0.2c0.2,0.6,0.6,0.7,1,1.1c0.6,0.7,1,1.7,1.6,2.4c0.3,0.5,1,0.6,1.3,1.1c1.7,0,3.4-0.1,4.7,0.5c0.3,0.1,0.5,0.6,0.7,0.7c0.4,0,0.8,0.1,1.2,0.1c0.7,0.2,1.4,0.7,2,1.1c0.5,1.1,0.5,2.7,1.2,3.6c0.8,1.1,2.6,1.4,3.4,2.5c0.2,0.4,0.1,0.7,0.2,1.2c0.2,0.6,0.9,1.7,0.2,2.5c0,1.7,1.3,1.2,1.3,2.8c0.6-0.2,1.3-0.3,1.9-0.5c0.6,0.7,1.6,0.9,2.4,1.4c0.3,0.2,0.4,0.7,0.7,1c0,0.7,0,1.5,0.1,2h0.1v0.1c0.1-0.2,0.1-0.6,0.1-1c0.5,0.3,0.2,1.1,0.6,1.4c0.2,0.2,0.5,0.5,0.7,0.6h0.6c0.9,0.4,2.1,3.1,2.6,4.1c0.5,0,2.5-1,2.8-1.3c0.5-0.7,0.6-1.4,1.3-1.8c0.4-0.1,0.7-0.2,1.1-0.2c0.7-0.3,1.7-0.9,2.5-1.2h1c0.1,0,0.5-0.4,0.6-0.5c0.6-0.3,0.9-0.2,1.4-0.5c1-0.6,1.3-2.1,2.3-2.6c0.7-0.4,1.8-1.1,2.5-1.3c0.7-0.2,1.2,0,1.9-0.2c1.4-0.5,4-2,5.5-1.7c0.6,0.1,1.2,0.5,1.7,0.7c0.4,0.2,0.6,0,0.8,0.2c1.2,0,1-0.6,1.8-0.8h1.3c0.5-0.2,1-0.8,1.6-1c1-0.3,2.8,0.1,3.7-0.2c0.5-0.2,1.1-0.9,1.7-1.1c0.9-0.3,3.1-0.2,3.4-0.4c0.9,0.1,0.5,0.6,1.1,0.8h0.8c0.1,0.1,0.2,0.4,0.2,0.5h1.9c0.6,0.2,1.7,0.6,2.4,0.8h1.2c0.6,0.2,1.4,0.6,2.2,0.7c0.4,0.8,0.8,1.6,1.2,2.4l1.4,0.1c0.3-2.2,0.4-4.4,0.8-6.4c0.1-0.4,0.3-2.2,0.2-2.5c0-0.2-0.2-0.3-0.1-0.6c0.2-0.5,0.9-1.1,1.1-1.7c0.4-1-0.6-2.3-0.2-3C412.8,322.6,413.7,321.5,414.4,321z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Freedomain" "Lebennin")} [:path {:className "st5" :d "M460.9,337.8c-0.1-0.1-0.5-0.2-0.6-0.4c-0.7-1.2-1.3-2.6-2.2-3.6c-0.7-0.8-2.1-1-2.9-1.7c-0.5-0.4-0.3-1.2-0.6-1.8c-0.1-0.2-0.5-0.4-0.6-0.6c-0.5-0.9-2-4.4-1.1-5.4c-0.2-0.9-1-0.9-1.4-1.6c-0.3-0.5-0.7-3.2-0.7-4h-0.1c-1.2,2.4-3.4,1.5-5.8,1c-0.6-0.1-1.3,0.2-1.9,0c-0.5-0.2-0.8-1-1.3-1.2h-1c-1.2-0.4-2.3-0.8-3-1.7c-2,0-4.3,0.1-5.2,1.2h-4.9c-0.2-0.1-0.3-0.6-0.5-0.7c0-0.7,0.1-0.9,0.4-1.3c-0.9-0.3-2.5,0.1-3.5-0.2c-0.3-0.1-0.7-0.6-1-0.7c-0.7-0.4-1-0.3-1.3-1.1c-0.7,0-0.8,0.3-1.2,0.5c-1.7,0.6-3.4-0.6-4.6,0.4c-0.3,0.5-0.6,3-0.4,4c0.1,0.5,0.5,1.6,0.2,2.4c-0.2,0.6-1.2,1.8-1.7,2.2c0,1.2,0.1,2.5,0.1,3.7c-0.2,0.5-0.8,0.8-1,1.2v1.8c-0.2,0.8-0.4,2-0.6,2.9v2.4c-0.3,1.2-0.4,2.8-1.1,3.6c0.2,1.2,1.7,1.1,2,2.2c0.7,2-0.4,4.3-1.1,5.4c-0.3,0.5-1,0.9-1.2,1.4v0.7c-0.2,0.6-0.7,1-1,1.4v0.5c-0.2,0.3-0.9,0.3-1.1,0.6c-0.7,1.2,1.5,3.1,0.4,4.2c0.1,0.7,0.4,0.6,0.8,0.8c0,1.1-0.4,1.1-0.5,1.9c-1.7,0.6-3.3,1.8-5,2.3c-0.5,0-1-0.1-1.6-0.1c0,0.1-0.1,0.2-0.1,0.4c-0.6,0.4-1.4,0.5-1.9,1c-0.7,0.6-0.9,1.6-1.8,2c-1.2,0.6-2.7,0.6-3.4,1.8c-0.4,0.7-0.3,1.7-0.7,2.4c-0.6,0.9-1.6,1.5-1.9,2.8c-0.3,1.2,0.2,2.5-0.5,3.2c0.6,1.3,1.8,1.5,2.6,2.5c0.4,0.5,0.7,1.8,1.1,2.4c0.9-0.1,1.2-0.6,1.8-1c1.4-0.9,3-1.4,4.8-0.8c0.4-0.4,1.1-0.8,1.6-1.1c0-0.6-0.1-0.7-0.4-1.1c-0.7-0.3-1.5-0.4-2.2-0.7c0-0.3,0-0.5,0.1-0.7c0.2-0.7,0.9-0.7,1.4-1.1c1.6-1,2.8-2.3,4.9-2.9c2.3,0,4.6-0.1,7-0.1c1.5-0.5,2.1-2.3,3.4-2.9c0.6-0.3,1.9-0.2,2.8-0.5c0.2-0.1,0.4-0.4,0.6-0.5c0.3,0,0.6-0.1,0.8-0.1c0.3-0.2,0.6-0.8,1-1c1.2-0.5,2.5-0.2,3.8-0.5c0.7-0.2,1.3-0.5,1.9-0.7c0.7-0.2,1.7,0.2,2.3-0.1c0.2-0.2,0.4-0.5,0.6-0.7c1.4-0.8,2.3-1.8,3.7-2.6c0.8-0.5,0.7-1.5,1.6-1.9h0.7c0.4-0.1,0.9-0.7,1.2-1c0.6-0.4,0.8-1.5,1.4-1.8c0.5-0.2,1,0,1.4-0.2c1.1-0.7,1.2-1.6,3.1-1.6v-0.7c0.1,0,0.2,0.1,0.4,0.1c0.1-0.1,0.1-0.5,0.2-0.6c0.3,0,0.6-0.1,1-0.1c0.6-0.3,1.7-1.8,2.2-2.4c0.7,0,1.2,0.2,1.8,0.4c1.4-0.2,2.8-0.3,4.2-0.5c0.6-0.2,1.1-0.7,1.8-0.8c0.2-0.7,0.7-1,1.1-1.6c0.8-1.2,1.2-2.5,2-3.6c0.3-0.4,0.8-0.6,1.2-1c0.4-0.4,0-0.9,0.7-1.4C462.9,340.6,461.3,338.2,460.9,337.8z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Freedomain" "Anórien")} [:path {:className "st5" :d "M463.6,331.1c-0.1-0.2-0.5-0.4-0.6-0.6V329c-0.1-0.3-3.8-4-4.2-4.3c-0.1-1.8-0.5-4.6,0.2-6c0.8-1.6,2.7-2.3,3.6-3.7c2.2,0,2.8-0.7,4.1-1.4c-0.1-1.8-0.4-4.8-0.8-6.4v-2c-0.1-0.5-0.6-0.8-0.7-1.3c-0.4-1.3-0.1-2.4-0.6-3.6c-0.2-0.5-0.8-0.7-1.1-1.1c-0.4-0.6-0.5-1.3-1-1.9c-0.3-0.2-0.6-0.5-0.8-0.7c-0.2-0.5,0.1-1.3-0.1-1.7c-0.4-0.7-1.6-1.9-1.9-2.5c-0.4-0.9-0.1-2-0.6-2.8c-0.8-1.1-2.5-1.6-2.5-3.5c-0.3-0.1-1-0.2-1.3-0.4c-0.6-0.4-0.9-1-1.7-1.2h-2c-0.6-0.1-2.5-0.4-3,0c-1.8,0-2.6-0.1-3.2-1.2l-1.8-0.2c-0.9,0.2-3.1,0.4-4,0c-0.6-0.2-1.4-0.8-2-1.1c-1.3-0.5-2.7-0.5-4-1.1c-1-0.4-2.1-1.3-3.1-1.7c-0.6-0.2-2.7-0.6-3.5-0.4c-0.4,0.1-1.4,0.5-2.2,0.2c-0.6-0.2-1.6-0.9-2.2-1c-0.8-0.1-1.8,0.7-3,0.4c-1-0.3-2.1-0.9-3.1-1.2c-1.9-0.5-3,0.7-4.7,0.4c-0.2,0-1.1,0.2-1.6,0c-0.2-0.1-0.5-0.4-0.6-0.5c-0.1,0-1.5,0-1.8,0.1c-0.2,0.1-0.7,0.5-1,0.6c-1.3,0.5-2.4,0.5-3.4,1.2c-1.1,0.8,0.3,2.1-0.1,3.6c-0.2,0.6-0.5,1.6-0.8,2.2c-0.2,0.4-0.8,0.6-1,1.1c-0.3,0.7,0,1.8-0.7,2.4c0.2,0.8,1,1,1.2,1.8c0.8,0,1.5-0.6,1.8-1.1c1.1,0,2.7,0.8,3.5,0.6c0.2-0.1,0.4-0.5,0.6-0.6h3.7c0.3,0.1,0.7,0.4,1,0.6v0.5c0.5,0.1,0.5,0.4,0.8,0.5c0,0,2.5-0.2,2.6-0.2c0.7-0.2,1.5-1,2.3-1.2c1.6-0.4,3.3,0.4,4.1,1c0.4,0.3,0.7,0.9,1.1,1.2c0.7,0.4,1.5,0.3,2.3,0.7c0.7,0.4,1.6,1.3,2,1.9c1.4,0,2.1-0.2,2.8-1.1c2.9,0,3.5,1.9,5.6,2.6c1,0.4,2.9,0.1,3.7,0.7c1.7,0,2.4,0.7,2.8,1.9c0.2,0.9-0.3,1.3,0.1,1.9c0.4,0.5,1.2,0.7,1.6,1.2h0.6c0.1,1.2,0.3,0.5,1.1,0.8c0.3,0.3,0.6,0.6,0.8,1c0.7,0.4,1.8,0,2.5,0.4c0.9,0.4,1.7,1.5,2.6,1.8c0.5,0.2,1.6-0.1,1.9-0.1c0.6,0,1.3,0.1,1.9,0.1h1c0.3,0.1,0.7,0.8,1,1h0.6c0.4,0.2,1.9,1,2.2,1.3c0.3,0.4,0.3,1.1,0.6,1.4c0,0.8-0.1,1.1-0.2,1.7c-0.8,0.2-1.7,0.7-2.8,0.5c-0.1,0.3-0.1,0.7-0.1,1.1c-0.9,0-1,0.1-1.4,0.5c0.4,0.6,1.1,0.9,1.3,1.7c0.2,0.3,0.2,1.8,0.1,2c-0.4,0.7-2.5,0.5-3.2,1c-0.7,0-1.7-0.1-2.3,0.1c-0.2,1.3,0.4,2.8,0.1,4.1c0.5,0.5,1.4,0.8,1.7,1.6c0.3,0.8-0.2,1.7,0,2.5c0.3,1.3,0.6,2.4,1.2,3.4c0.1,0.2,0.5,0.3,0.6,0.5c0.2,0.4,0.1,1.1,0.4,1.4c0.4,0.6,2.2,0.9,2.8,1.7c0.9,1.2,1.5,2.5,2.4,3.7c0.8,1.1,1.8,2.2,2.5,3.5h0.4c0.3-1.5,0.2-3.1,1-4.2c-0.2-1.2-1.3-2.2-1.8-3.2C463.6,332,463.6,331.5,463.6,331.1z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Borderland" "Rohan")} [:path {:className "st2" :d "M439.7,280.4v-0.6c-0.2-0.5-0.8-0.9-1-1.3c-0.4-0.8-0.2-1.8-0.6-2.6c0-0.1-0.3-0.4-0.4-0.5c0.4-1.8-0.3-4.2-0.7-5.9c-0.4-1.8-0.1-3.1-0.8-4.3c-0.3-0.4-0.7-0.3-1.2-0.6c-0.7-0.4-0.9-1.5-1.6-1.9v-3.4c0.3-0.9,0.9-1.5,1.3-2.2c0.2-0.3,0.5-1,0.6-1.2c0-0.1-0.1-0.2-0.1-0.4c0.1-0.1,0.4-0.4,0.5-0.5v-0.6c0.1,0,0.2-0.1,0.4-0.1c0-0.2,0.1-0.4,0.1-0.6c0.2-0.1,0.3-0.2,0.5-0.2c-0.2-0.3,1.2-2.2,1.4-2.6c-0.3-0.6-0.2-2.4-0.4-3v-1.2c-0.2-0.3-0.8-0.7-1-1.1v-1.2c-0.4-1.5-0.2-3.5-0.7-4.9v-1c-0.2-0.2-0.4-0.3-0.6-0.5c-0.8-0.8-1.1-1.4-1.1-3.1c-0.9,0.2-3.2,0.8-3.8,0.4c-1.3-0.1-1.4-1.3-2.4-1.7c-0.3-0.3-0.9,0.6-1.4,0.6c-0.5,0-1.2,0.7-1.8,1c-2.1,0.7-3.9,0.9-5.3,2.3c-0.8,0.8-1.4,1.8-2,2.6c-0.2,0.3-0.9,0.4-1.1,0.7c-0.2,0.3,0.1,0.7-0.1,1.1c-0.2,0.1-0.3,0.2-0.5,0.2c0,0.2-0.1,0.3-0.1,0.5c-0.3,0.4-1,0.7-1.3,1.2v0.4c-0.3,0.4-1,0.5-1.3,1c-0.8,1.1-1.6,2.1-2.4,3.1c-0.3,0.2-0.6,0.4-0.8,0.6c0,0.2-0.1,0.4-0.1,0.6c-0.3,0.4-0.8,0.8-1.1,1.2c-0.1,0.2,0,0.3-0.1,0.5c-0.8,0.7-3,1.8-4.1,2.2c-0.8,0.3-1.5,0.1-2.2,0.5c-0.8,0.4-2.3,0.8-3,1.2c-0.9,0.6-1.9,1.3-3,1.7h-1.6c-0.9,0.2-2.1,0.6-3.1,0.4c-0.5-0.1-1.3-0.5-1.9-0.4c-0.6,0.2-1.7,0-2.4,0.2c-1.1,0.3-3.3,0.7-4.2,0.7h-1.8c-1.2-0.3-5.9-1.9-7.3-1.3c-1-0.1-1.1-0.8-1.6-1.4c-0.3-0.4-0.9-0.7-1.3-1.1c-0.1-0.2-0.2-0.3-0.2-0.5h-0.6c-0.2-0.1-0.4-0.7-0.6-1v-0.1h-0.1v0.1c-0.3,0.2-0.3,0.4-0.6,0.6c0.1,0.6,0.5,1.5,0.8,1.9c-0.1,0.7-0.3,0.9-0.6,1.3c-1.1,0.4-2.9,0.3-4.3,0c-2-0.1-4-0.2-6-0.2c-1.3-0.3-2.3-0.7-4-0.7c0-0.3,0-0.3-0.1-0.5c0-0.3-0.1-0.4-0.1-0.6c-0.2-0.1-0.1,0-0.2-0.1c-0.9,0.1-1,0.5-1.6,0.7c-1,0.5-2.2,0.6-3,1.3c-0.6,0.5-0.9,1.3-1.3,1.9c-0.4,0.3-0.8,0.6-1.2,1c-0.6,0.8-1.1,1.8-1.9,2.4c0,0.5,0.1,1,0.1,1.4c-0.1,0.3-1.5,2.1-1.8,2.3c-0.5,0.3-1,0.2-1.6,0.5c-1,0.5-1.3,2.2-2.3,2.8h-0.6c-0.1,0.1-0.4,0.4-0.6,0.5c-1.3,0-2.6,0.1-3.8,0.1c-0.3,0.1-0.9,0.4-1.2,0.5c-0.5,0-1-0.1-1.4-0.1c-0.3,0.1-0.2,0.3-0.7,0.4v0.1h-0.1c0,0.5-0.1,1.4,0.1,1.7c-0.2,0.7-1,1.4-1.6,1.8v0.4c1.1,0,2-0.6,3.1-0.4c1.5,0.4,3.2,1.8,3.8,3.1h0.5c0.1,0.5,0.2,0.5,0.5,0.7c-0.1,0.4,0.1,0.2-0.2,0.4c0.1,0.6,0.7,0.8,1.3,0.8v0.4c1.6,0.3,3.9,0.2,5.2,1.1c0.9,0.7,0.7,2.5,1.6,3.2c0.4,0.4,1.2,0.7,1.6,1.2c0.3,0.4,0.7,1.2,1.1,1.4h0.7c0.7,0.4,1.6,1.2,2.3,1.6c0.2,0,0.5,0.1,0.7,0.1c0,0.2,0.1,0.3,0.1,0.5c0.2,0.1,1.1,0.5,1.1,0.5c1.5,0,1.1,1.1,1.8,1.9c0.6,0.4,1.6,0.5,2.2,1c0.3,0,0.4,0,0.6-0.1c0.1-0.2,0.1-0.4,0.1-0.7c0.3,0,0.6,0.1,0.8,0.1c0,0.3,0,0.5,0.1,0.7c0.7,0,1.1,0.1,1.3-0.4c0.5,0,0.4,0,0.6,0.4c0.4-0.2,0.6-0.6,1-0.7h0.7c0.4-0.2,0.9-0.9,1.3-1.1h0.7c0.5-0.2,1-0.7,1.4-0.8c0.5-0.2,0.8,0,1.2-0.2c0.2-0.1,0.3-0.6,0.5-0.7h0.8c0.6-0.2,1.5-0.5,2-0.7h1.4c0.3-0.1,0.5-0.4,1-0.5v0.4c0.5,0,0.4,0,0.6-0.2c0.3,0,0.4,0.2,0.5,0.2c0,0,1.1-0.4,1.2-0.5c0.4,0.1,0.4,0.4,0.5,0.5c0.1,0,0.9-0.2,1.4,0c0.3,0.1,0.6,0.5,0.8,0.6c0.4,0.1,0.8-0.1,1-0.1c0.2,0,3,0.3,3.2,0.4c1.9,0.7,4,2.1,6.1,2.9c1.4,0.5,4.1-0.4,5.3-0.5v-0.1h0.1c-0.1-0.4,0-0.3-0.2-0.5v-0.2c1.2,0,1.4,0.2,2.4,0h1.2c0.7-0.2,1.2-0.8,2.2-0.8c0.2-0.4,0.5-0.9,0.6-1.1c0.2-0.5-0.1-1.1,0.1-1.7c0.2-0.6,0.9-1.2,1.3-1.7c0.3-0.4,0.5-1.5,0.6-1.9c0.2-0.9-1-2-0.4-3.1c0.7-1.6,5.6-2.5,7.6-3.2c0.9-0.3,2.2,1.2,4.1,0.6c1-0.3,3.1-0.6,4.3-0.1c0.5,0.2,2.5,1.3,3.2,1.1c2.1-0.6,2.5-0.2,4.1,0.5c1.4,0.6,2.6-0.9,4.3-0.2c0.6,0.2,1.6,0.3,2.2,0.6c0.9,0.4,1.9,1.3,2.9,1.7c0.3,0,0.6,0.1,1,0.1c1.6,0.7,3.7,0.9,5,1.9h0.6C440.5,281.2,439.9,280.9,439.7,280.4z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Freedomain" "Fangorn")} [:path {:className "st3" :d "M376.1,237.4c-0.1-0.7-1-0.9-1.4-1.2c0-0.9,0.2-1.4,0.7-1.8c0-1.2-0.5-1.3-0.8-2c-0.4-0.7-0.3-1.2-0.6-2c0-0.1-1.5-2-1.7-2.3c-0.4-0.4-1.1-0.6-1.4-1c-0.5-0.5-0.4-1.1-1.2-1.3c-0.1-0.6-0.4-1.3-0.1-2.2c0.1-0.4,0.6-0.9,0.4-1.7c-0.2-0.7-1-1.1-1.3-1.7c-0.3-0.6,0.1-1.3-0.2-1.9c-0.2-0.2-0.4-0.3-0.6-0.5c-0.3-0.5-0.4-1-0.6-1.4c-0.6-0.2-1.8-0.6-2.2-0.6c-0.6,0.1-1.3,1.3-2.2,1.6c-1,0.3-2.3-0.4-2.9-0.6c-0.8-0.2-1.9,0-2.2,0c-0.5-0.1-1.2-0.4-1.7-0.5H355c0,0-0.5-0.6-0.5-0.6c-0.6-0.2-1.2,0.8-1.6,1c-0.7,0.3-1.7-0.1-2.2-0.2c-0.1,0.2-0.2,0.3-0.4,0.5c0.3,0.6,1.3,1.6,1.9,2c0.2,0.1,0.5,0.1,0.6,0.2c0.6,0.6,0.5,1.7,1.1,2.3c-0.2,1.3-1.2,1.4-1.6,2.2v0.7c-0.1,0.1-0.6,0.6-0.7,0.7c-0.6,0.3-1.3,0.2-1.8,0.6h-0.1v0.1c1.3,1.2,2,2.9,3.4,4c0,0.5,0,1-0.2,1.3c-0.2,0.4-1.4,1-1.8,1.1c-0.3-0.1-0.6-0.2-0.8-0.2c-0.7,0.2-1.1,1-1.9,1.2c-1.1,0.3-2-0.4-2.9,0.1h-0.2c0.1,0.5,0.5,0.5,0.6,1c0.7,2-0.8,3-1.7,3.8c0.1,0.9,0.4,1.2,0.5,2.2c0.5,0.2,1.1,0.7,1.6,0.8h0.8c0.2,0.1,0.2,0.7,0.4,0.8c0.1,0.1,0.6,0.2,0.7,0.2c0.1,0.2,0.1,0.4,0.2,0.5c0,2.2-0.6,1.2-1.7,2c-0.7,0.5-0.7,1.6-1.3,2.2c-0.1,0.1-1.3,1-1.3,1c-0.4,0.2-0.9,0-1.2,0.2c-0.9,0-0.6,0.4-1.2,0.7c0,0.7,0.2,0.8,0.2,1.4h0.4c-0.1,0.2,0,0.1-0.1,0.2c-0.1,0.9-3,2.7-3.8,1.6c-0.8,0.2-1.2,0.8-1.9,1.1c0.1,1,1.3,3.9,1.9,4.3c0.3,0.2,0.6,0.1,0.8,0.2c0.2,0.1,0.1,0.5,0.2,0.6c0.4,0,0.8,0.1,1.2,0.1c0.9,0.3,2.2,1.3,2.8,2c0.4,0,0.6,0.1,1,0.1c0.4-0.8,2.1-2.5,2.8-3.2c0.1,0,0.2-0.1,0.4-0.1c0.9-1.2,1.6-2.4,3-3.1c0.5-0.2,0.9-0.1,1.4-0.4c0.4-0.2,0.7-0.7,1.1-0.8c0.6-0.2,1.1-0.2,1.6-0.5c0.5,0.4,0.9,1.2,1.4,1.4c0.4,0,0.7,0.1,1.1,0.1c1,0.3,2.2,0.5,3.4,0.7h3c0.7,0.1,4.1,0.5,4.4,0.4h0.2c-0.1-0.6-0.4-0.5-0.6-0.8c-0.2-0.4-0.1-1.1-0.1-1.2c0.4-2.1,2.7-2.1,4.4-3c1.5-0.7,2.4-2.9,2.4-5.2c-0.4-0.4-1-0.8-1.2-1.4c-0.9-1.4,0.8-3.9,1.4-4.3c0.4-0.2,0.6,0.1,0.8,0c0.2-0.1,0.5-0.4,0.7-0.5C375.9,238,375.9,237.6,376.1,237.4z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Freedomain" "Wold & Foothills")} [:path {:className "st3" :d "M429.1,222.7c-0.3-0.9-1.7-1.8-2.5-2.2c0-0.3,0.1-0.6,0.1-0.8c-0.2,0-0.2,0-0.4,0.1c-0.7-0.1-1.4-1-2-1.4c-0.6,1.2-0.9,0.3-1.9,0.8c-0.4,0.2-0.2,0.5-1,0.5v0.5c-1.2,0-2.4,0.8-4,0.4c-0.3-0.1-0.8-0.5-1.3-0.4c-0.1,0-0.1,0.3-0.4,0.2c0-0.2,0-0.3-0.2-0.2c-0.8,0.1-1.6,1-2.2,1.3c-0.6,0.3-1.2-0.2-1.8,0c-1.5,0.4-2.2,0.5-3.5-0.4c-0.1-0.1-0.2-0.4-0.2-0.5h-0.7c-0.2-0.1-1.3-0.9-1.6-1.1h-0.1c-0.3-0.3-0.1-0.7-0.2-1.2c-0.1-0.4-0.7-0.5-0.5-1.3c0.2-0.9,1-1,1.3-1.8c-0.8-0.9-1.5-1.6-2-2.8c-0.3-0.1-1,0-1.3-0.1c-0.4-0.2-0.8-0.8-1.2-1.1c0-1.3,0-2.2,0.7-2.6c0-0.1-1.2-0.2-1.6-0.6c-1.4,0.1-1.4,0.9-2.3,1.2c-1,0.3-4.7-0.1-5.2-0.4c-1-0.6-1.9-2.4-2.3-3.5c0-0.7-0.1-1.4-0.1-2c-0.3-0.7-1.1-1-1.6-1.6c-0.2-0.3-0.2-0.6-0.4-0.8c-0.5-0.7-1.2-1.3-1.7-2c0-0.4-0.1-0.8-0.1-1.2c-0.6-2.7,0.5-5.8,2.3-7c-0.2-1.3-2.5-1.2-3.7-1.7c-0.3-0.1-0.4-0.5-0.6-0.6h-1.2c-0.8,0.3-2.2-0.2-3.6,0.1c-0.8,0.2-1-0.4-1.4-0.8c-0.3,0.5-1.8-0.3-2-0.4c-0.5,0-1,0.1-1.6,0.1c-1.6-0.3-3.3-0.6-4.9-1.1c-0.5-0.1-0.8,0.2-1,0.2c-0.6,0.2-2.1-0.5-2.4-0.8c-1.3,0-3.3,3-3.4,4.3c-0.7,0.4-1.5,0.7-2.3,1.1c-0.1,0.4-0.2,0.7-0.4,1.1c-0.3,0.3-0.8,0.4-1.1,0.7c-0.2,0.3,0,0.8-0.1,1.1c-0.2,1.2-1.3,0.7-2.2,1.2h-0.1c0.3,0.4,0.6,0.3,1,0.6c0.7,0.3,0.6,1,1,1.6c0,1.3-0.5,2.4-1.4,2.6c0.1,1.8,1.7,1.4,2.8,2c0.6,0.4,0.9,1.2,1.4,1.6c0.6,0.5,1.4,0.8,2,1.2c0.4,0.2,0.7,0.3,1,0.6c0.3,0.5,0.9,2,0.5,2.6c-0.1,1-0.7,1-1.2,1.6c-0.6,0-0.9,0.2-1.2,0.2c-0.7,0.1-1.2-0.2-2-0.1c0.2,0.5,2.5,2.3,3,2.6c0.2,0.1,0.5,0.1,0.6,0.2c0.4,0.4,0.4,1.2,1,1.6c0.7,0.4,1.8,0.3,2.4,0.7l1.6,2.4c0.1,0.6,0.2,1.2,0.4,1.8c0.5,1,2.3,3.2,1,4.9c0.2,1.6,2.2,2.2,3,3.4c0.1,0.2,0.2,0.5,0.2,0.7c0.2,0.3,0.8,0.5,1,0.8c0.4,0.8,0.4,1.8,0.8,2.6c0.2,0.4,1.9,2.2,0.7,3.2c0.1,0.7,0.6,0.7,0.8,1.3c0.3,0.8,0.1,1.9,0,2.6c-0.4,0.4-0.8,0.9-1.2,1.3c-0.4,0-0.7,0.1-1.1,0.1c-0.2,0.1-0.4,0.4-0.6,0.5c-0.1,0.4-0.2,0.8-0.4,1.1c0.1,1.2,1.1,1.2,1.3,2c0.3,1-0.4,1.8,0.2,2.3c-0.1,0.6-0.3,0.5-0.5,0.8c-0.1,0.4-0.2,0.9-0.4,1.3c-0.7,1.5-2,2.6-3.6,3.2c0,0.3,2.3,2.7,2.6,2.9c0.8,0.4,2.8,0.1,4.1,0.5c1,0.3,2.3,0.9,3.4,1.1c0.3,0.1,2.8,0,3.2-0.1c0.2-0.1,0.6-0.4,0.7-0.5h1.8l0.2-0.2h1.6c1.3,0,2.6,0.1,4,0h2.9V255c0.5-0.3,1.1-0.4,1.6-0.7c0.3-0.2,0.4-0.7,0.8-0.8c0.3,0,0.6-0.1,1-0.1c1.2-0.5,2.8-1.2,4.1-1.7c0.8-0.3,1.9-0.2,2.5-0.7c0.1-0.1,0.1-0.4,0.2-0.5c0.6-0.4,1.2-0.6,1.6-1.2v-0.4c0.2-0.2,0.4-0.3,0.6-0.5c0.1-0.3,0-0.6,0.1-0.8c0.3-0.4,0.7-0.6,1-1.1h0.5c0.1-0.7,2.8-4,3.4-4.6c0.2-0.2,0.6-0.2,0.7-0.4c0.5-0.5,0.4-1,0.7-1.6c0.5-0.4,1-0.7,1.4-1.1c0.2-0.3,0.2-0.7,0.5-1c0.3-0.2,0.6-0.3,0.8-0.5c0.4-0.5,0.7-1.3,1.1-1.7c0.3-0.3,0.8-0.3,1.2-0.5c0.6-0.3,1.1-0.9,1.7-1.2c0.7-0.3,2.1-0.3,2.9-0.6c1-0.4,1.3-1.3,2.9-1.3c0.2-1.6,1.3-2.9,1.2-4.7c-0.5-0.1-0.5-0.3-0.8-0.5c-0.1-0.4-0.1-0.9-0.1-1.4c0.2-0.2,0.4-0.8,0.6-1c0.2-0.1,0.4,0,0.5-0.1C428.5,224.5,428.3,223.1,429.1,222.7z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Borderland" "Anduin Vales")} [:path {:className "st2" :d "M433.6,170.2c-0.1-1.3-1.7-2.3-1.2-3.2c-0.2-0.9-1.2-1-1.7-1.7c-0.5-0.6-0.6-1.4-1-2c-0.2-0.3-0.6-0.4-0.8-0.6c-0.5-0.6-0.4-2.1-0.1-2.8c0.3-0.7,3-3.8,3.6-4.2l0.1-2.3c-1.1-0.2-1.5-0.9-2.3-1.4c0-1.1-0.2-2,0.5-2.6c-0.1-0.8-1.3-1.6-1.9-1.8h-1c-0.3-0.1-0.8-0.6-1.1-0.7c-1.1-0.5-2-0.2-2.5-1.6c-0.3-0.4-0.2-2.5,0.1-2.9c0-0.3,0-0.4-0.1-0.6c-0.7-0.4-1.7-0.3-2.4-0.7c-0.7-0.4-1.1-1.3-1.7-1.9c0-1.3,1.1-3.5,1.8-4.2c0.4-0.4,1.3-0.7,1.6-1.3c0.5-1.2-0.3-2.2,0-3.2c0.4-1.3,1.6-2.6,2.6-3.2c-0.1-0.6-1.8-2-2.3-2.3c0-1.9,0.7-3.2,1.6-4.3c0-2-2.5-2-3.6-2.9c-1.6-1.3-0.4-6,0.8-7.1h0.4c0.4-0.3,1-1,1.3-1.3c0.3-0.3,0.2-0.9,0.5-1.2c0-0.4,0-0.7-0.1-1c-1.9,0-3.2,0.9-4.9,0.6c-1.3-0.2-2.5-0.8-3.5-1.2c-0.2-0.8-0.3-1.7-0.2-2.5c0.8-0.8,1.5-1.9,2.4-2.5c0-0.4-0.8-3-1-3.2c-0.2-0.5-0.9-0.9-1.4-1.1h-1c-0.3-0.1-1.8-1.4-2-1.7c-0.3-0.4-0.9-1.7-1.1-2.2c-0.3-0.9,0.3-2.1,0.5-2.6c0.6-1.9,0.7-2.8,2.5-3.5c0.5-0.2,1.6-0.4,2.2-0.2v-0.1h0.1c-0.4-1.1-1.9-1.2-2.6-2c-0.3-0.3-1-1.4-1.1-1.7c-0.3-0.7,0-1.3-0.4-1.9c-0.2-0.3-0.6-0.7-0.8-1.2c-0.7-1.7,1.9-3.1,0.7-4.9c-0.4-0.8-1.8-1-2.5-1.6c-0.5-0.4-1.5-1.4-1.7-2v-1.9c-0.5-1.6-0.1-2.4,1-3.2c-0.3-0.5-1.7-1.3-2.3-1.4c-0.6-0.1-1.7,0.6-2.4-0.1c-1-0.1-1.1-0.8-1.6-1.4c-0.3-0.4-1.1-1-1.6-1.3h-0.4c-0.1-0.1-1.4-2.2-1.4-2.3c-0.7-2,0.5-3.5-1.7-4.3c-0.4-0.2-1.1-0.1-1.6-0.2c-0.5-0.1-1.3,0.3-2,0.4c-1.6,0-4.3-0.5-5.3,0.6h-4c-0.8,0.4-0.9,2.3-2.4,1.8h-1.1v0.2c-0.1,0.2,0,0.4-0.1,0.7c-0.1,0.3-0.5,0.5-0.6,0.7v0.2c-1,0.2-3.2,0.8-3.7,1.4c-0.1,0.3-0.2,0.6-0.2,1c-0.3,0.2-0.6,0.3-0.8,0.5c-0.1,0.1,0,0.3-0.1,0.5c-0.2,0.3-0.8,0.2-1.1,0.5c-0.9,1.2-1.1,2.7-2.3,3.6c-0.1,1.6,0.2,2.9,0.7,4.2c0.5,1.2,1.7,2.8,0.6,4.3c-0.4,0.8-1.5,1.1-2,1.8c-0.1,0.3-0.2,0.6-0.4,1c-0.3,0.4-1,0.7-1.2,1.2c-0.3,0.6-0.1,1.5-0.5,2c0.1,0.4,0.2,0.3,0.4,0.6c0.9-0.1,1.2-0.6,2-0.8c0.8-0.2,5.3,0.2,6,0.6c0.8,0.5,0.7,1.4,1.2,2.2c0,0.9-0.5,0.9-0.8,1.4c-0.3,0-0.4,0-0.6,0.1c-0.7,0-0.9,0.1-1.3,0.2c0.2,0.3,0.6,0.5,0.8,0.8c0.2,0.2,0.2,0.6,0.4,0.8c0.4,0.4,1.2,0.2,1.8,0.5c1.3,0.5,2.5,1.5,3.6,2.2v1.4c-0.2,0.2-0.3,0.6-0.5,0.8c-0.3,0.3-0.6,0.1-1.1,0.2c-0.7,0.3-1,1.1-1.7,1.3c0.3,0.5,1.2,1.2,1.8,1.4c0.4,0.2,0.7,0,1,0.2c0.5,0.3,0.5,1,0.8,1.4c0.1,0.2,0.4,0,0.6,0.1c0.3,0.2,0.7,1,1,1.3c0.3,0.4,1.3,1.3,0.8,2c0,2.4-2.8,1.2-4,2l-0.4,0.1c0.4,1.8,3.2,1.9,4.1,3.4c0,0.2,0.1,0.4,0.1,0.6c0.2,0.2,0.6,0.5,0.7,0.7c0.3,0.7-0.3,1.2,0,1.8c0.2,0.3,0.7,0.6,0.8,1c0.4,1-0.1,2.7-0.6,3.1c0,2.2,2,3.9,2.6,5.8c0.2,0.7-0.2,2.3-0.5,2.6c-0.1,0.1-0.4,0-0.5,0.1c-0.4,0.4-0.2,0.7-0.7,1c0.1,0.4,0.4,0.6,0.7,0.8c0,0.5,0,0.8-0.2,1.1c0.1,1,2.4,2.5,3.1,3c0,0.7,0.1,1.4,0.1,2c-0.1,0.3-0.4,0.3-0.4,0.7c0,0.1,0.3,0.4,0.4,0.5c0,1.1-0.5,2.1-1.2,2.5c-0.2,0.1-0.4-0.1-0.5,0.1c0,0.4-0.1,0.7-0.1,1.1c-0.2,0.1-0.3,0.2-0.5,0.4c-0.4,0.9,0.7,0.9-0.4,1.6c0,1.7,0.5,1.2,1,2.2c0.1,0.4,0.2,0.7,0.2,1.1c0.2,0.2,0.4,0.3,0.6,0.5c0.2,0.3,0.1,0.9,0.4,1.2c-0.2,1.6-1.2,1-1.7,1.7c-0.4,0.5-0.2,1.4-0.7,1.8c-1.7,1.4-5.3,0.8-7.4,0.8c0.1,1.6,1.3,1.2,2.4,1.8c1,0.5,1.9,1.4,1.9,3c-0.6,0.1-0.9,0.4-1.3,0.6c0.1,0.4,0.5,0.7,0.6,1.1c0.3,1.1-0.7,2.1-1.4,2.3c0,0.8,0.1,1.5-0.6,1.9c0.2,0.8,0.9,1.5,1.1,2.4c0.1,0.5-0.3,1.2-0.1,1.8c0.1,0.5,0.8,1.4,0.7,2c-0.1,0.4-0.2,1.1-0.4,1.4c-0.1,0.1-0.2,0.2-0.4,0.2c-0.1,0.5,0.4,0.7-0.1,1.4c-0.2,0.5-0.6,0.4-1.2,0.4v0.1h-0.1c0.1,0.6,0.3,0.7,0.7,1c0,0.7,0,1.5-0.4,1.8c-0.2,0.2-0.5,0.1-0.6,0.4c0,0.4-0.1,0.9-0.1,1.3c-0.2,0.4-0.8,0.4-1,0.7c-0.1,0.2,0.1,0.5-0.1,0.6c-0.1,0.1-0.4,0.1-0.5,0.1c0,0.8-0.2,1.3,0.2,1.6c-0.1,0.7-1,2.3-1.2,2.8c0,1,0.7,1.3,1.3,1.7c0.2,1.6-0.1,2.1-0.5,3.2c-0.3,0.9,0.2,2,0.4,2.4c0.2,0.7-0.3,1.1-0.4,1.4c-0.1,0.4,0,0.8-0.1,1.1c0,0.3,0.1,0.3,0.1,0.5h4.3c0.2,0.1,0.6,0.5,0.8,0.6h1.3c0.1,0.1,0.5,0.4,0.7,0.5c0.7,0.2,1.2,0.1,1.8,0.5c0.5,0.3,0.9,1.1,1.3,1.6c0,1.9-1.4,1.8-2,2.9c-0.3,0.5-0.1,1.3-0.1,1.6c0,0.3-0.5,0.6-0.6,0.8c-0.4,0.8,0.2,2.1,0.4,2.6c0.7,2.1,2.8,3.1,3.6,5.2c0.2,0.5-0.1,1.2,0.1,1.8c0.3,1,2,3.7,3.6,2.8c1.3,0,1.9-0.1,2.5-0.7c0.1-0.1,0.1-0.4,0.2-0.5c0.5-0.1,1-0.2,1.6-0.2c0.4-0.5,0.8-1,1.2-1.6c1.3-1.3,2.8-2.7,4.3-3.7c-0.2-1.3-1.8-2.3,0-3.5c0-0.8-0.7-1.4-1.2-1.8c-0.1-2.4,0.7-2.6,2.3-3.6c0.7-0.4,1.5-1.1,2.3-1.3c0.9-0.3,1.6-0.1,2.4-0.5c0.8-0.3,1.9-0.9,2.8-1.2c1.1-0.4,2.5,0.1,3.1-0.7c0.1-0.1-0.1-0.6-0.2-0.8c-0.5-0.7-1.2-0.6-2-1c-0.9-0.4-1.6-1.2-2.4-1.8c-0.1-1.2-0.2-1.8,0-3.1c0.3-0.2,0.6-0.5,1-0.6c0.4-0.1,0.8-0.2,1.2-0.2c0.8-0.5,1.3-1.8,2.2-2.3c0.6-0.4,1.9-0.6,2.3-1.1c0.2-0.5,0-1.5,0.4-2.2c0.6-1,2.3-1.1,2.8-2.2h0.5c0-0.2,0-0.2,0.1-0.4c0.4-0.9,1.3-0.5,2.2-1c0.5-0.2,0.7-0.9,0.8-1.4c1.5-0.4,2.6-0.3,4.1,0c0,0.3,0,0.3,0.1,0.5c0.3,0.4,0.6,0,1.1,0.2c0.1,0,0.4,0.4,0.5,0.5c0.7,0,1-0.1,1.4-0.4C433.5,171,433.8,170.5,433.6,170.2z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Shadowland" "Gundabad")} [:path {:className "st0" :d "M383,55c-0.4-0.4-0.7,0-1.3-0.2c-0.6-0.2-1-1-1.8-1.2c-0.8-0.2-2.5,1.1-3.6,1c-1.5-0.2-2.3-0.8-3.4-1.3c-0.4,0-0.8-0.1-1.2-0.1c-1.1-0.6-2.6-1.3-3-2.5h-3.4c-0.9,0.3-1.3,1.2-2,1.6c-0.4,0.2-0.7,0.1-1.1,0.4c-0.5,0.3-0.7,1.1-1.1,1.6c-0.4,0.6-3.4,1.5-4.8,1.2c-2.9-0.7-2.6-2.1-4.4-3.5c-0.8-0.6-2.3-1.6-3.2-1.9c-1.6-0.7-2.9,0.8-4.2,0.4c-0.5-0.2-0.6-0.9-1-1.1c-0.1-0.1-2.3-0.5-2.4-0.5c-0.2,0-0.5,0.2-0.6,0.2c-0.5,0.1-1.6-0.3-2.5,0c-0.2,0.1-0.6,0.4-1.1,0.4c-0.8-0.1-1.3-0.5-1.8-0.7h-3.7c-1.2,0.4-2,1.7-3.4,1.9c-0.3,1.5,0.5,2.7,1.2,3.7h0.4v0.4c1,0.8,2.1,1.9,4,1.9c0.2,0.4,0.4,0.7,0.7,1c0,0.5,0.1,0.7,0,1c0,0.4,0,1,0.1,1.2c0.6,0.5,1.9,0.8,2.6,1.1h0.8c0.3,0.2,0.2,0.5,0.4,0.8c0.2,0.3,0.6,0.4,0.8,0.7c1.6,0,1.9-0.9,3.1-1.3c0.6-0.4,2.2,0.1,2.8,0.5c0.2,0.2,0.4,0.5,0.6,0.7c0.7,0.3,1.3,0.2,1.9,0.6v0.2h1.3c0,0.1,0.1,0.2,0.1,0.4c0.5,0.4,1.2,0.5,1.6,1c0.2,0.3,0.2,0.8,0.5,1.1c0,1.4-0.5,1.6-0.8,2.5l-0.6,0.1c0.2,0.7,0.4,0.5,1,0.8c0.4,0.2,1.4,1.7,1.7,2.2c0.2,0.5,0,1.5,0.2,1.9c0.2,1,1.2,0.9,1.9,1.4c0.9,0.6,1.1,3.2,1.9,3.8c0.2,0,0.3,0.1,0.5,0.1c0.4,0.4,0.8,0.9,1.2,1.3c1.2,0.6,2.7,0.3,3.6,1.1c0.3,0.3,0.6,1.3,0.8,1.7c2.3,0,1.5-0.9,2.8-1.6c0.9-0.5,2.7-0.1,3.4,0.4c0.9-0.2,0.8-1.8,1.2-2.5c0.9-1.5,2.3-2.8,3.6-4c0-1.4-0.7-2-1.1-3.1c0-0.1,0.1-0.2,0.1-0.4c-0.1-0.3-0.3-0.8-0.5-1.2c-0.2-0.4-0.7-1.5-0.4-2c0-1.8,0.9-1.9,1.6-3c0.5-0.9,0.5-2,1.1-2.8c0.4-0.5,1.6-0.5,1.9-1.2c0.2-0.5,0-0.8,0.2-1.2c0.3-0.4,3-1.3,3.8-1.3c0-0.3,0.1-0.7,0.2-1c0.3-0.9,2-1.4,3.1-1.4C383.4,55.7,383.2,55.2,383,55z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "High Pass")} [:path {:className "st3" :d "M387.7,118.6c-0.1-0.1-0.4-0.1-0.5-0.4c-0.2-0.5,0-0.9-0.2-1.3c-0.1-0.2-0.6-0.2-0.7-0.4c-0.3-0.3-0.1-0.7-0.2-1.2c-0.6-1.6-0.5-2.3,0.4-3.7c0.2-0.4-0.8-1.6-1-2v-1.4c-0.2,0-0.3-0.1-0.5-0.1c-0.1-0.2,0.1-0.4,0.1-0.5c-0.5-0.8-2.5-1.8-3.5-2.3v-0.5c-0.3,0-0.5,0-0.7-0.1c-0.3-1.2-0.8-1.7-0.1-3c0.8-0.2,1-0.6,1.7-0.7c0.4-0.1,1,0.1,1.2,0.1c0.4-0.1,0.7-0.2,1.1-0.2v-0.1h-0.1c-0.1-0.4-0.1-0.4-0.5-0.5c-0.4-0.3-0.8-0.2-1.1,0c-0.1-0.2-0.1-0.2-0.4-0.4c-0.6-0.3-2.6,0.3-3.4,0.1c-0.3-0.1-0.4-0.4-0.6-0.6c-0.3-0.3-0.6-0.2-0.8-0.5c-1.5,0-1.5,0.6-2.3,1.2c-0.2,0.1-0.6,0.1-0.7,0.2c-0.2,0.4,0,1-0.2,1.4c-0.1,0.2-0.4,0.3-0.5,0.6c-0.4,0-0.5,0-0.7,0.2c-0.9-0.1-0.7-0.6-1.4-0.7c0-0.4,0.1-0.3,0-0.5c-0.1-0.2-0.9-0.8-1.1-0.8c-0.2,0-1,0.2-1.2,0.2c-0.9,0.1-2-0.2-2.9-0.1c-0.3,0.3-0.7,0.5-1,1c-1,0-1.5,0.2-2.4,0c-1.1-0.3-2.3-1.1-3.5-0.5c-0.6,0.3-0.8,1.1-1.3,1.4c-0.5,0.3-0.9,0.3-1.2,0.8h-0.2c0.4,0.4,0.9,0.7,1.1,1.3v1.2c0.1,0.3,0.3,0.4,0.4,0.8c0.4,0.1,0.7,0.2,1,0.4v2.3h-0.1c-0.4,0.5-1.2,1.6-1.8,1.9c-0.5,0.1-1,0.2-1.6,0.4c-0.2,0.1-0.3,0.6-0.5,0.7c-0.3,0.2-0.5,0.1-0.8,0.2c-0.2,0.2-0.4,0.5-0.6,0.7c0,0.6,0.5,2.9,0.7,3.2c0.2,0.2,0.5,0.4,0.7,0.6c0,0.3,0.1,0.6,0.1,0.8c0.3,0.7,0.8,1.5,1.2,2.2h0.7c0.1,0.7,0.6,1,0.7,1.6c0.2,1-0.8,2.4-0.6,2.9v0.7c0,0,2.3,1,2.4,1c0.4-0.1,0.7-0.8,1.1-1c0.7-0.3,1.4-0.1,2.2-0.4c0,0,1.5-0.7,1.6-0.7c0.4-0.3,0.3-1,0.7-1.3c0.5-0.4,1-0.5,1.3-1c0.8,0,1,0.2,1.6,0.4c0.4,0,0.9-0.1,1.3-0.1c0.3,0.1,0.7,0.5,1.1,0.6c0.7,0.2,1-0.3,1.4,0.1c1.8-0.2,2-2.5,3.4-3.1c2-0.9,4,0.5,5.5,0.6c-0.1,0.2,0,0.1-0.1,0.2c0.1,0.2,0,0.4,0.1,0.5h0.6c0,0,1.5,1.3,1.6,1.3v0.4c0.2,0,0.4,0.1,0.6,0.1c0.2,0.1,0.3,0.6,0.5,0.7c0.1,0,1.9-0.2,1.9-0.2c0.3-0.2,0-0.7,0.1-1c0.1-0.1,0.4-0.1,0.5-0.2c0.3-0.3,0.2-0.7,0.5-1.1c0.2-0.3,0.3-0.1,0.5-0.1c0.1-0.3,0.2-0.6,0.4-1C388,119.7,388.1,119,387.7,118.6z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Redhorn Gate")} [:path {:className "st3" :d "M366.6,182.9c-0.2-0.1-0.6,0-0.8-0.1c-0.1-0.2-0.2-0.3-0.2-0.5c-0.4-0.3-0.8-0.2-1.2-0.5c-0.2-0.2-0.5-0.6-0.7-0.7c-0.4-0.2-0.8-0.1-1.2-0.4c0-0.1-0.1-0.2-0.1-0.4h-0.5l-1.6-1.4c-0.2-0.3-0.2-0.8-0.5-1c-1.1-0.8-2.4,0-4-0.5c-0.9-0.2-1.2-1.1-1.9-1.4c-0.5-0.2-1.2,0-1.6-0.2c-0.3,0.2-0.3,0.4-0.6,0.6c-0.8,0.5-1.3,0.2-2,1c-0.1,0.1-0.1,0.1-0.1,0.4c-0.4-0.1-0.5-0.4-0.7-0.5c-0.7-0.2-1.9,0.5-2.9,0.2c-1.3-0.3-3.7-0.2-5.2-0.6c-0.9-0.2-1.7-0.1-2.3-0.6c-0.2,0-0.2,0-0.4,0.1c-0.1,0.1-0.1,0.1-0.1,0.4c-0.3,0-0.6-0.1-1-0.1v-0.1c-0.1-0.1-0.1-0.1-0.1-0.4c-0.6,0.1-1.1,0.2-1.7,0.2c-0.1,0.1-0.2,0.2-0.2,0.4c-0.8,0-1.5-0.1-2.3-0.1c-0.5,0.2-1,0.7-1.8,0.8c0.2,0.4,0.5,1.3,0.8,1.7c0,0.6-0.1,0.7-0.4,1.1c-0.4,0.1-0.5,0.2-1,0.2v0.4h0.4c0,0.3,0.1,0.6,0.1,0.8c0.8,0,1.6,0.8,2.4,0.6c0.1,0,0.3-0.3,0.4-0.4c0.3-0.1,2.9-0.1,3.4,0.2h0.7c0.2,0.8,0.2,1.1,0.1,1.9c-0.2,0-0.2,0-0.4,0.1c-0.5,0.2-0.7,0.8-1,1.2c-0.5,0.1-0.7,0.2-1.3,0.2c-0.1,3.3-7.3,3.1-9.1,1.3c-0.5,0-0.8,0.1-1.1,0.4H325v0.1h0.1c0.2,0.2,1.4,0.3,1.8,0.4c0,0.3,0,0.3-0.1,0.5c0,0.3,0,0.3,0.1,0.5c0.1,0.1,0.1,0.1,0.4,0.1c-0.1,0.5-0.3,0.5-0.7,0.7v1.2c-0.5,0.1-1,0.3-1.3,0.6c0,0.2-0.1,0.3-0.1,0.5h-0.8c-0.3,0.1-0.9,0.6-1.2,0.8c0.2,0.2,0.4,0.4,0.6,0.6c-0.1,0.5-0.2,0.5-0.5,0.7c0.7,3.5,8.7,1.1,9,5.9c0.8,0.2,1.2,1,1.8,1.3h0.6c0.4,0.2,0.9,0.9,1.4,1.1c0.5,0.2,1-0.3,1.3-0.1c0,0,0.2,0.3,0.2,0.4c1.4,0.5,3.3-1.8,4-2.2c0.6,0.2,2.2-0.2,2.8,0c0.4,0.1,1.5,0.5,1.8,0.6c0.3,0,0.6-0.1,0.8-0.1c0.1,0.1,0.2,0.2,0.2,0.4c0.1,0,2.6,0.4,2.6,0.4c0.5-0.1,1.1-0.7,1.6-0.8c0.9-0.3,2.3,0.1,3,0.1c0.2,0.2,0.3,0.2,0.4,0.6h1.6c-0.7-1.9,0.5-2.8,1.4-3.5c-0.2-0.6-0.9-0.8-1.2-1.3h-1.1c0-0.6,0-0.9-0.4-1.2c0.1-0.8,0.5-0.9,0.6-1.6c0.3,0,0.5,0,0.7-0.1h0.4c0.3-0.2,0.1-0.6,0.2-1c0.3-0.6,1.5-1.3,1.9-2h0.7v-1.1c0.1-0.2,2.8-1.4,3.2-1.6c0.1-0.3-0.1-0.5,0-0.8c0.3-0.9,1.2-1.1,1.1-2.3c0.7-0.1,0.7-0.5,1.2-0.7c0.7-0.3,1.7-0.5,2.4-0.6v-0.1C367.1,183.4,367,183.1,366.6,182.9z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Borderland" "Gap of Isen")} [:path {:className "st2" :d "M340.3,259.8c-0.6-0.2-0.9,0.2-1.4,0c-0.5-0.2-0.7-0.9-1.1-1.2c-0.4-0.3-0.8-0.3-1.1-0.6h-0.1c-0.5-0.6-0.4-1.6-0.8-2.3c-0.1-0.2-0.6-0.5-0.7-0.7c-0.2-0.4-0.1-0.9-0.4-1.3c-0.8-1.2-1.6-2.5-2.5-3.6c-0.4-0.5-0.4-1.4-0.6-1.8c-0.2-0.3,0-0.2-0.4-0.1c-0.2-0.6-0.2-1.2-0.6-1.7c-0.8-1-2.6-1-4.4-1v-0.4c-0.5,0.1-0.7,0.3-1,0.1c-0.9,0.2-0.3,0.6-0.7,1.2c-0.1,0.2-1.1,0.9-1.3,1c-0.3,0.2-1,0.1-1.3,0.4c-1.9-0.4-3.4-1.9-5.5-2c-0.3,0-0.5,0.3-0.7,0.4c-0.4,0.1-0.9-0.1-1.3,0.2c-0.2,0.1-0.1,0.4-0.4,0.5c-0.4,0.1-0.8,0.2-1.2,0.2c-0.6,0.3-1.4,1.5-2,1.8c-0.5,0.2-2.2,0.6-2.3,0.7c-0.1,0.2-0.2,0.5-0.2,0.7c-0.3,0.3-0.7,0.2-1,0.6c-0.2,0.5,0.1,2.1,0,3c-0.4,0.2-0.4,0.6-0.7,0.8c-0.3,0.2-0.8,0.1-1.1,0.4c-0.4,0.5-0.1,0.9-0.7,1.3c0.2,0.5,0.6,0.8,0.7,1.2c0.2,0.9-0.9,1.6-1.3,1.9c-0.7,0.5-0.9,1.6-1.4,2.3c-0.4,0.6-1,1-1.3,1.7c-0.2,0.5-0.1,0.9-0.4,1.4c-0.2,0.1-0.3,0.2-0.5,0.4c-0.3,0.7,0.3,1.6-0.2,2c0.1,0.2,0,0.1,0.1,0.2c0.2,0.2,2-0.5,2.4-0.4c2,0.5,5,0.2,8.2,0.2c1.1,0,3.1-0.4,3.6,0c1.1,0.1,1.8,1.2,2.4,1.8c-0.1,0.8-0.5,1.2-0.7,1.7c-0.1,0.2,0.2,1.5,0.1,1.9h1.1c0.1,0.6,0.2,1.1,0.1,1.8c0.8,0.1,1,0.7,1.2,1.3c1.6,0,3-1,3.4-2.3h1.1v-0.4h0.1v-0.1c0.8,0.1,1.1,0.3,1.6,0.5c0.8,0.3,1.3-0.3,1.9-0.2c0.3,0,0.3,0.3,0.7,0.1c0.6-0.2,0.3-0.5,0.6-1c0-0.1,1.7-1.2,1.8-1.2c0.1-0.6,0-2.1,0.4-2.5c0.5-1.1,3.6-1.2,4.8-1.6h2.9c1-0.3,2-1,2.5-1.8c0.2-0.4,0.2-0.8,0.6-1.1c0.5-0.4,1.1-0.3,1.7-0.6c0.3-0.1,0.4-0.5,0.7-0.7v-0.8C342.8,261.7,341.2,260.1,340.3,259.8z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Anfalas")} [:path {:className "st3" :d "M357.7,346.7c-0.2-0.6-0.4-1.1-0.6-1.7c-0.4-0.6-1.1-1.2-1.6-1.8H355v-0.4c-0.8-0.3-2.8-1.9-2-3.1c-0.1-1-1-0.8-1.4-1.4c-0.7-0.1-1-0.1-1.3,0.5c-1.1,0-1.7-0.1-2-0.8v-0.5c-0.1-0.2-0.4-0.1-0.6-0.2c-0.4-0.3-0.6-0.8-1-1.1c0-1,0.1-3.1-0.2-3.4v-1c-0.2-0.4-0.9-0.5-1.3-0.7c-0.6-0.4-2.6-2.3-2.9-2.9c-0.4-0.9-0.1-1.7-0.6-2.4c-0.5-0.8-2.3-0.9-3.1-1.3v-0.6c-0.8-0.1-1.6-0.2-2.4-0.4h-2.2c-0.7-0.3-1.4-1.2-2.2-1.4h-1.8v-0.2h-1.2c-0.1,0-0.4-0.3-0.5-0.4h-1.1c-1.1-0.3-2.5-0.8-3.5,0c-0.1,0-0.2,0.1-0.4,0.1v0.5c-0.6-0.1-0.4-0.3-0.7,0c-0.6,0-0.7-0.1-0.8-0.6c-0.6,0.1-0.6,0.2-1.2,0.2v0.5c-0.5,0.3-1.6,0.6-2,1.1c-1.5,0-3.1-0.7-3.8-1.6c-0.9,0.1-2.7,0.8-3.1,1.3c-2,0-1.9-0.8-3-1.6c-0.4-0.3-0.9-0.2-1.2-0.6c-0.3-0.4-0.3-0.9-0.7-1.2c-0.2-0.1-0.5-0.2-0.7-0.2c-0.2-0.1-0.2-0.5-0.4-0.6c-1.2-0.6-2.2,0.7-3.5,0.1c-0.3-0.1-0.6-0.7-0.8-0.8h-1.8c-0.3-0.1-0.6-0.4-0.8-0.5h-1c-0.6-0.2-0.8-0.9-1.3-1.2c0-1.1-0.3-2.6,0.1-3.6c0.2-0.4,0.7-0.7,0.8-1.2h0.4c0.1-0.5,0.2-0.7,0.6-1v-0.2c-1,0-2,0-2.5,0.5c-1,0.5-0.9,1.8-1.4,2.8c-0.7,1.1-1.1,0.9-2.4,1.3c-0.5,0.2-1.1,0.6-1.7,0.2c-0.4-0.2-0.5-0.5-1-0.7c0-1.7,0.5-2,1.1-3c-0.1-0.1-0.8-1.1-0.8-1.1h-1.3c-0.6-0.3-1.5-1.1-1.7-1.8c-0.5,0-1.5,0.1-1.7,0c-0.3-0.1-0.6-0.6-1-0.7c-0.5,0-1,0.1-1.4,0.1c-0.1-0.2-0.2-0.3-0.4-0.5c-0.9-0.6-1.9-0.8-2.8-1.4c0-0.1-0.1-0.2-0.1-0.4h-1.9c-0.6-0.2-1.3-0.5-2.2-0.6c-0.2,0.2-0.4,0.3-0.6,0.5c-0.1,0.1-0.1,0.1-0.1,0.4c-0.5,0.1-0.7,0.4-1.1,0.5h-1.3c0,0.1-0.1,0.2-0.1,0.4l-0.8-0.1c-0.3,0.1-0.6,0.4-0.8,0.5c-0.4,0-0.7,0.1-1.1,0.1c-0.8,0.3-1.8,1.7-2.3,2.4c-2.5,0-4.5,1.4-7,1c-0.5-0.1-1.2,0.1-1.6-0.1c-2.2,0.1-1.9,2.3-2.6,3.8c-0.2,0.3-0.8,0.5-1,0.8c-0.3,0.5,0.1,1.3-0.1,1.9c-0.1,0.2-0.5,0.2-0.6,0.4c-0.4,0.6,0.6,2.2-0.1,3.4c-0.3,0.4-0.6,0.5-1.3,0.5c0.1,1-0.1,1-0.2,1.7c-0.2,0.6,0.2,1.4,0.2,2.2c0.5,0.4,0.9,1.1,1.4,1.4c0,1.2-0.3,2.3,0,3.5c0.1,0.2,0.4,0.7,0.2,1.2c-0.1,0.2-0.4,0.4-0.5,0.5c0,0.1,0,1.6,0,1.7c0.2,1.1,1.3,1.7,1.6,2.8c0.2,0.9-0.6,1.6-0.7,2.2l-0.1,3.2c-0.1,0.3-0.6,0.6-0.7,0.8c0,0.1-0.2,1.4-0.2,1.4c-0.1,1.1,1.1,2.3,0.6,4c-0.1,0.3-0.5,0.6-0.6,0.8c0,0.5-0.1,1-0.1,1.4c-0.2,0.5-0.7,1-0.8,1.4c-0.4,1.3,0.3,3.2,0,4.1c-0.4,1.6-2.5,2.1-3.1,3.6c-0.4,0.8,0,2-0.4,3c-0.3,0.8-1.3,1.1-1.6,1.9c-0.2,0.6,0.3,0.8-0.1,1.2c0.2,1.4,2.2,1.9,2.9,2.9c0.7,0,2-0.3,2.3-0.2c0.2,0.1,0.3,0.2,0.7,0.2h1.2c0.5,0.2,0.9,0.8,1.3,1.1c0.3,0.2,0.9,0.4,1.1,0.7v0.5c0.2,0.3,0.7,0.6,0.8,0.8c0,0.4-0.1,0.9-0.1,1.3c0.1,0.1,0.2,0.2,0.4,0.2v0.8c0.1,0.3,0.4,0.8,0.5,1.1c0,1.1-0.1,2.2-0.1,3.4c0,0.1,0.3,0.4,0.4,0.5c0.3,0.9-0.7,1.9-0.4,3c0.3,1,0.8,2.2,1.2,3.1c0,0.3,0.1,0.6,0.1,0.8c0.2,0.4,0.8,0.3,1.1,0.6c0.5,0.4,0.6,1.3,1.1,1.7c0.3,0.2,0.7,0,1,0.2c0.1,0,0.2,0.4,0.2,0.5h0.6c0.5,0.3,1.1,0.9,1.7,1.1h0.7c0.5,0.2,1.8,0.9,2.9,0.5c1.2-0.5,2.3-1.1,3.5-1.7c0.1-0.7-0.8-1.9-0.4-2.5c0.1-0.5,0.2-0.5,0.4-0.8c0.7,0,0.8,0,1.4-0.1c0.7-1.4,1.5-1.3,1.7-3.5h0.4c0-0.2,0.1-0.5,0.1-0.7c0.7,0,1-0.5,1.2-1c0.3-0.5,0-1.5,0-2.3c0-1.1,1-1.9-0.1-2.8c0.1-0.9,1.5-2,2.5-1.7c0.2,0.1,0.7,0.4,1.2,0.2c0.7-0.2,1.5-0.6,2.3-0.7c0.1-0.2,0.1-0.5,0.2-0.7c0.2-0.3,1.3-0.9,1.7-1.1h0.7c0.6-0.3,1.7-0.6,2.3-1.1c0.3-0.4,0.6-0.7,0.8-1.1c0.4-0.2,0.6-0.1,0.8-0.5c2.1,0,1.8,1.2,2.8,1.4c3.4,1,7.6-1.2,10-2.3c0.1-0.3,0-0.2,0-0.5c0.5,0.1,2.2,0.2,2.9-0.2h0.6c-0.2-0.3,0-0.1-0.4-0.2v-0.1c0.6-0.2,1.1-0.4,1.3-1.1c0.1-0.2,0.1-0.1,0-0.2c0.2-1.2,1.1-0.4,2.2-0.8c1.2-0.4,1.9-1.2,3.6-1.3c0.4-1,1-1.7,1.7-2.4c0.2-0.2,0.2-0.5,0.4-0.7c1.1-1.5,2.5-3,4.7-3.4c0,0.3,0,0.5,0.1,0.6h0.6c0.6,0.3,1.2,0.9,1.9,1.1c0.5,0.1,0.9-0.4,1.3-0.1h1.4c0.1,0.3,0,0.8,0.1,1c0.2,0.1,0.6-0.1,1.1,0.1c1.5,0.6,2.3,0.6,4.1,0.2c1.1-0.2,1.9,0.2,2.8-0.1c0.2,0.4,0.6,1.1,1.1,1.3h0.8c0.1,0.1,0.2,0.5,0.4,0.6c0.5,0.2,1.1,0.3,1.6,0.2c0.4,1.4,2.8,2.2,4.7,1.3c0.4-0.2,0.2-0.4,0.5-0.7c0.5-0.7,1.4-1.2,2-1.7c0.3-0.2,0.5-0.7,0.8-1c0.3-0.2,0.6,0,1-0.2c0.3-0.2,0.7-0.7,1.1-0.8l1.3-0.1c1.9-0.6,3.4-0.9,6-0.8v0.5c0.6-0.3,1.1-0.8,1.8-1.1c0.3,0.4,0.6,0.7,0.8,1.1h0.2c-0.8-2.1,0.1-3.8,0.6-5.5c0-0.7,0.1-1.4,0.1-2.2c0.2-0.6,1-1.8,1.4-2.2C358.7,347.3,358,347.2,357.7,346.7z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Coastal Sea" "Bay of Belfalas")} [:path {:className "st4" :d "M370.1,392.9c-0.9-1-2.1-1.4-2.6-3c-1.1,0-1.3,0.6-2.2,0.8c-1.3,0.3-2.8-0.9-3.5-0.6c-0.5,0.2-0.3,0.6-0.6,1c-0.4,0.4-4,2.2-4.9,1.6c-0.6-0.1-0.6-0.3-0.8-0.7H355v-0.4c-2.5,0-4.7,1.1-7-0.4c0-2.5,1.9-3.1,3.2-4.3c0.5-0.5,0.4-1,0.7-1.8c0.2-0.5,1.7-2.4,2-2.6c0.4-0.2,0.9,0,1.4-0.1c0.4-0.1,1-0.4,1.4-0.6c0,0,1.3-1.2,1.3-1.2c0.3-0.4,0.3-1,0.6-1.4c-0.1-1.5-1.3-1.6-1.9-2.5c-0.3-0.5-0.4-1.2-0.7-1.7v-0.1c-0.3,0.1-0.6,0.5-0.8,0.7c-0.5-0.1-0.6-0.2-0.8-0.5c-0.9,0.3-2,0.8-2.9,1.1h-2.2c-0.4,0.1-1.3,0.6-1.8,0.2c-1.1,0.1-1.8,0-2-0.8h-0.1c-0.1-3.2,0.8-2.7,2.9-3.6c0-0.2,0-0.2-0.1-0.4c-0.8-0.5-1.9-0.5-2.6-1.1c-0.4-0.4-0.5-1.2-0.8-1.7c-0.1-2.3,0.9-2,2.5-2.5c0.4-0.1,0.6-0.4,1.1-0.5c0-0.3-0.2-0.8-0.1-1.2c0.1-0.2,0.2-0.6,0.2-1.1h0.4v0.7h0.2c0.1-0.4,0.2-0.7,0.4-1.1c-0.3,0-0.3,0-0.5-0.1h-0.1v-0.1c2.6-0.6,1.9-2.4,3.4-3.8c0-0.3-0.1-0.3-0.1-0.5c-1.4,0-1.5,0.4-2.4,0.7h-1.4c-1.7,0.5-3.6,1.5-4.4,2.9h-0.4c-0.2,1.3-1.7,2.6-3.1,2.5c-0.9,0-2.2-0.6-2.6-0.2c-0.6-0.2-1-0.6-1.6-0.8V363c-0.5,0-0.5-0.1-0.7,0.2c-0.9-0.3-3.4-1.7-3.8-2.4c-1.8,0-3.1,0.3-4.6,0.2h-1.3c-0.5-0.1-1.4-0.4-1.9-0.7c-0.3-0.1-0.6-0.6-0.8-0.7c-0.8,0-1.5-0.1-2.3-0.1c-0.3-0.1-0.7-0.8-1-1c-0.2-0.1-0.5,0-0.6-0.1c-3,0.3-3.8,4.6-5.8,6c-0.6,0.4-1.6-0.1-2.4,0.2c-0.4,0.1-0.8,0.7-1.2,0.8c-0.4,0.1-0.9-0.1-1.2,0.1c-0.4,0.3-0.3,1.1-0.7,1.4c-0.1,0-0.2-0.1-0.4-0.1c-0.2,0.3-0.4,0.6-0.6,0.8c-0.9,0.7-2.7,0.3-3.8,0.8c-0.5,0.2-1.2,0.7-1.7,1c-0.5,0.2-0.8,0-1.3,0.2c-0.3,0.1-0.6,0.5-0.8,0.6h-0.7c-0.6,0.2-1.5,0.7-2.2,1c-0.6,0.2-1.2-0.2-1.4-0.2h-2.8c-0.6-0.2-1.4-1-1.8-1.6c-1,0.3-1.3,1.3-2.2,1.8c-0.6,0.3-1.2,0.2-1.8,0.5c-0.2,0.1-0.6,0.5-0.8,0.6h-0.7c-0.5,0.2-0.7,1.1-1.2,1.4c-1.1,0.7-2.9,0.9-4.6,0.8v0.1h-0.1c0,0.5,0.2,1.4,0.1,2.2c-0.1,0.4-0.3,1.2-0.4,1.3c-0.1,0.6,0.3,1.4,0,2c-0.4,0.7-1.1,1.4-1.6,2c-0.3,0.4-0.2,0.8-0.5,1.3c-0.4,0.8-1.1,1.3-1.6,2c-0.3,0.5-1.2,0.6-1.6,1.1c-0.1,0.2,0.1,0.9,0.4,1.1c0,1.9-1.4,1.9-2.6,2.5c-0.6,0.3-1.1,0.8-1.8,1c-3.3,0.9-8.9-2.2-9.8-4.1c-0.3,0-0.4,0.1-0.6,0.1c-0.4,1.6-0.4,3.7,0.2,5.2c0.2,0.4,0.6,0.9,0.7,1.3c0.6,2.1-0.5,4.6-0.8,5.9c-0.2,0.8,0.4,1.7,0,2.3c-0.2,0.5-0.5,0.6-0.8,1c-0.5,0-0.7,0.2-1,0.2c-0.8,0.2-1.2-0.2-1.8-0.2c-0.1-0.4-0.4-0.5-0.5-0.8c-0.5-1.4,0-2.7-1.2-3.5c-1.9-1.2-5.3-1.4-7.9-0.6c-0.9,0.3-1.6-0.2-2-0.4c-0.2,0-0.6,0.3-0.7,0.4h-2.8c-0.4,0.2-1,0.7-1.4,0.8c-1.5,0.6-4.3-0.9-4.7-1.6c-0.5,0-0.7-0.1-0.8,0.2c-0.3,0.7,0.1,4.6,0.2,5.5c0.3,1.6-0.9,3.1-0.5,4.7c0.1,0.2,0.4,0.6,0.5,0.7c0.5,0.9,0.9,5.5,0.6,7.1c-0.2,0.8-0.2,1.6-0.5,2.3c-0.2,0.4-0.7,1-0.8,1.4c-0.4,1.2,0.6,2.7,0.8,3.6v3.1c0.1,0.5,0.5,1.4,0.5,1.9c0,1.4-1.5,2.9-2,4c-0.6,1.2-0.4,2.2-0.7,3.7c-0.4,1.5-1.1,2.8-1.1,4.6h104.4c0.5-2.3,3-3.4,4-5.4c0.6-1.1,1.2-2.4,1.6-3.6v-1.1c0.1-0.5,0.3-1.2,0.6-1.7c0.3-0.6,1.1-0.9,1.7-1.3c0.8-0.6,1.9-1.6,2.4-2.5c1.1-1.8,1.4-4.6,3.1-5.8c0.7-0.5,1.4-0.5,2.2-1c0.3-0.2,0.7-0.7,1.1-0.8h0.7c0.2-0.1,0.4-0.7,0.6-0.8c0.5-0.5,1.5-1.2,2-1.6c1.2-0.8,2.8-0.9,4.1-1.6c1-0.6,1-1.8,1.6-2.9c0.9-1.8,2-3.7,3.5-4.9c-0.1-0.5-0.7-1.1-1-1.4c0.1-2.5,1.2-1.6,2.6-2.5c0.3-0.2,0.6-0.8,1-1.1c0.2-0.2,0.5-0.1,0.7-0.2c1.1-0.7,1.7-2.2,2.5-3.1c0.2-0.1,0.3-0.2,0.5-0.2C370.7,396,370.5,393.5,370.1,392.9z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Andrast")} [:path {:className "st3" :d "M258,398.5c0.2-0.4,0.7-4.1,0.5-4.7c-0.2-0.4-0.8-1-1-1.4c-0.8-1.9-0.6-4.9,0.6-6c0-0.9-0.4-1.9-0.7-2.5c-0.2-0.1-0.3-0.2-0.5-0.2c-0.1-0.3-0.1-1.3-0.1-1.4c-0.2-1.1,0-2.4,0.1-3.1c0-0.3-0.1-0.6-0.1-1c-0.1-2,0-3.3-0.6-4.7c-0.1-0.7-0.2-1.4-0.4-2c-0.3-0.7-3-2.5-4.3-1.7h-2c-0.4-0.2-0.7-0.8-1.1-1.1c-0.8-0.6-1.6-1-2.3-1.8c-0.4-0.5-0.2-1.5-0.6-2c0.2-1.7,1.5-2.1,2-3.2c0-0.6,0.1-1.3,0.1-1.9c0.2-0.7,0.5-1.5,0.8-2c0.5-0.8,1.9-1.3,2.3-2.2c0.4-0.9,0.3-2.3,0.1-3.2c0-0.1-0.2-0.4-0.1-0.7c0.1-0.3,0.5-0.6,0.6-0.8c0.4-0.8,0.8-2.3,1.1-3.1c0.4-1.1-0.4-2.3-0.6-3.1c0-1,0.1-1.9,0.1-2.9c0.2-0.3,0.7-0.4,0.8-0.8v-1.3c0-1.1,0.2-2,0.7-2.6c-0.2-1-0.9-1.1-1.2-1.8c-0.3-0.6-0.2-1.3-0.6-1.8c0-1.1-0.2-2.1,0.6-2.4c-0.1-0.3-0.1-1-0.2-1.2c0-1.9-0.2-2.6-1.3-3.2c0-1.1-0.2-1.8-0.4-2.4c-0.2-0.9,0.7-3.5,1-4.1h1c-0.1-1.9-0.7-3,0.5-4.3c-0.2-1.6-1.6-0.8-2.5-1.4c-1.3-0.9-1.8-2-3.6-2.4c-0.8-0.2-1.9,0.3-2.8,0.1c-2.3-0.4-6-0.2-7.7-1.3c-1.3,0.1-2,1.9-3.6,2.3c-0.1,0.2,0,0.1-0.1,0.2c0.3,1.4,2.3,0.9,3.2,1.7c0.7,0.7,0.7,1.8,0.6,3c-0.4,0.4-0.7,1.1-1.3,1.3c-0.4,0.1-1-0.1-1.3,0.1h-0.2c0.3,0.3,1.1,0.2,1.4,0.5c0.3,0.3,0.3,1,0.6,1.3c0,0.4,0,0.9-0.1,1.1c-1.4,1.2-4.6,0.3-6.4,1.1c-0.3,0.1-0.4,0.5-0.6,0.7c-0.2,0.2-0.6,0.2-0.7,0.4c-0.5,0.6-0.3,1.4-0.8,1.9c-0.2,0.2-0.6,0.3-0.8,0.5c-0.1,0.2-0.2,0.4-0.4,0.6h-1.2c-0.3,0.1-0.7,0.7-1,0.8c-0.4,0.3-0.9,0.2-1.3,0.5c-0.1,0.1-0.1,0.4-0.2,0.5c-0.8,0.7-1.8,1.1-2.4,1.9c-0.3,0.4-0.4,0.9-0.8,1.2c-0.3,0.2-0.7,0-1.1,0.2c-0.4,0.2-1.3,0.9-1.9,0.8c-0.5-0.1-1.1-0.7-1.6-0.8c-0.5,1.2-1.7,1.6-2.8,2.2v0.4c1.8,0.4,3.9,0.8,4.8,2.2c0.2,0.7,0.4,1.4,0.6,2v1.9c0.3,0.1,0.6,0.2,0.8,0.2c0.2,0.2-0.1,0.6,0.1,0.8c0.4,0.5,1.2,0.7,1.8,1c0.4,0.2,0.7,0.8,1.1,1.1c0.2,0,0.3,0.1,0.5,0.1c0.3,0.3,0.1,0.8,0.4,1.2c0.4,0.7,1.3,1.4,1.8,2.2c0.2,0.2,0,0.5,0.1,0.8c0.1,0.3,0.7,0.7,0.8,1c0.5,0.9,0.1,2.5-0.2,3.4c-0.4,0.8-2.1,2.2-1.8,3.4c0.2,0.7,0.9,1,1.3,1.4c0.3,0.4,0.6,1.2,0.8,1.6c0.6,0.9-0.1,2,0.1,3c0.2,0.9,1,1.9,1.3,2.8v1.3c0.3,0.7,0.9,2.2,0.6,3.4c-0.2,0.9-0.9,2-1.2,2.8c-1.1,0-1.9,0.1-2.6,0.5c-1.2,0.6-2.6,4.4-2.4,6.1c-0.4,0.3-0.6,0.9-1,1.2c-0.5,0.4-1.3,0.6-1.7,1.2c-0.2,0.3-0.2,0.8-0.5,1.1c-0.1,0.1-0.4,0-0.5,0.1c-0.4,0.6-0.5,1.1-0.8,1.7c-0.1,0.2-1,1.2-1.2,1.3c-0.3,0.2-0.9,0.3-1.1,0.6c0,0.2-0.1,0.5-0.1,0.7c-0.3,0.4-1.6,1.7-2,2c0,1.8-0.1,3-0.5,4.4c-0.1,0.5-0.1,1.8,0.1,2.2c0,2-1.2,1.8-1.6,2.9c-0.2,0.6,0.2,1.4,0.1,1.8c-0.1,0.5-2.4,2.8-2.9,2.9c-0.2,0-0.4-0.1-0.6-0.1c-0.5,0.2-1,0.9-1.4,1.1h-2.2c-0.2,0.1-0.6,0.5-0.7,0.6c-0.5,0.3-1,0.2-1.4,0.5c-0.2,0.2-0.4,0.6-0.7,0.7l-0.7,0.1c-0.1,0.3-0.2,0.6-0.4,1c-0.5,0.1-1,0.2-1.4,0.2c-1.2-0.3-2.6-0.5-3.7,0.4c-0.4,0.3-0.4,0.9-0.7,1.4c-0.6,1-1.5,2.2-3,2.3v0.5c0.8,0,4.2,0.2,4.6,0.4c0.6,0.3,1.1,0.9,1.6,1.3c0.1,0.5,0.3,1.6,0,2c-0.2,0.5-1,0.9-1.6,1.1c0,0.7,0.2,1,0.6,1.3c0.8,0.6,2.6-0.2,3.8,0.1c0.3,0.1,0.9,0.3,1.2,0.5v0.2c0.4,0.2,1.6,0.3,1.9,0.4c0.9,0.2,1.2-0.6,1.7-0.8c0.3-0.1,0.6,0.1,0.7,0.1c0.1,0,0.4-0.5,0.5-0.6h0.4c0.7-0.5,0.5-1.3,1-2c0.4-0.7,1.3-1,1.7-1.7c1.8,0,4-0.2,5.8,0.2c0.7,0.2,1.6,0.2,2-0.1c0.1,0,0.2-0.1,0.4-0.1c0-0.9,0-1.2,0-2c1.5-0.3,2.2-3.7,3.1-4.9c0.5-0.7,2.2-0.8,3.1-1.1h0.8c0.3-0.2,0.5-0.8,0.8-1c0.6-0.3,1.1-0.4,1.6-0.8c0.3-0.3,0.4-0.8,0.7-1h0.7c0.1-0.1,0.3-0.4,0.5-0.5c0.2-1-0.3-1.7-0.1-2.6c0.1-0.5,0.4-1.2,0.5-1.8c0.8,0,1.7-0.6,2-1c0.1-0.1,0.1-0.5,0.2-0.6c0.3,0,0.6-0.1,1-0.1c0.3-0.2,0.5-0.9,0.8-1.1c0.3-0.2,0.6-0.1,0.8-0.4c1.1,0.1,0.5,0.5,1.1,1c0.4,0.1,0.7,0.2,1.1,0.2c0.9,0.4,1.6,1.1,2.5,1.4c1.3,0.4,2.9-0.9,4-1.1c0.5,0,1,0.1,1.4,0.1c1.2-0.3,0.7-0.5,2-0.1c0.6,0.2,1.3-0.1,1.6-0.2c0.7-0.2,2.8-0.1,3.5-0.2c0.6-0.1,1.5,0.3,1.9,0.5c0.7,0.2,1.3,0.1,1.9,0.4c1.9,0.9,2.6,2.5,3.1,4.8c0.3,0,0.3,0,0.5,0.1c0.6,0,0.5-0.1,0.8-0.4c-0.1-0.8-0.7-1.9-0.2-2.9C257.8,398.6,257.9,398.6,258,398.5z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Old Pûkel Gap")} [:path {:className "st3" :d "M255.4,308.9c0-0.2-0.1-0.4-0.1-0.6c-0.2-0.1-0.4-0.2-0.6-0.4c-0.3-0.5-2.6-3.3-3.2-3.6c-1.5-0.8-2.7-0.1-3.2-2c-0.2-0.1-0.1,0-0.2-0.1c-0.3,0-0.7,0-0.8,0.1c-0.1,0.1-0.1,0.1-0.1,0.4c-0.6-0.1-0.6-0.2-1-0.5c-0.8,1.1-2.1,1.7-3.1,2.5c-1.6,1.5-3.1,3.2-5,4.3c0.1,0.4,0.1,0.4,0.5,0.5c0.1,0,3.3,0.6,3.7,0.5c0.5-0.1,2.1-0.6,3.1-0.2c1.3,0.5,2.9,0.5,4.1,1.1c0,0,1.3,0.8,1.3,0.8c0.2,0.2,0.2,0.6,0.5,0.7c0.9,0.6,2,0.5,2.6,1.3c1-0.3,0.7-2.2,1.2-3c0.3-0.5,0.8-0.7,1.1-1.2C256.3,309.2,255.5,309.1,255.4,308.9z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Shadowland" "Angmar")} [:path {:className "st1" :d "M332.5,59.6c0-0.5,0.1-1,0.1-1.4c-0.3-0.5-1.6-0.3-2.2-0.6c-0.2-0.1-1.8-1.1-1.9-1.3v-0.4c-0.5-0.8-1.5-1.1-1.9-2c-0.3-0.7-0.1-1.6-0.4-2.3c-0.1-0.2-0.5-0.7-0.6-1c-1.4,0-2.9-0.3-4-0.7h-3.4c-0.4-0.1-1.2,0.2-1.7,0c-1.3-0.5-3.1-0.5-4.3-1.1c-0.5-0.2-0.9-1-1.4-1.3c-1.6-0.9-4.8,0.4-6.5-0.2c-1.3-0.5-2.8-1.3-4.2-1.8h-2.4c-0.8-0.3-1.3-1.1-2.4-1.3v-0.4c-1.3-0.1-3.1-0.2-4.2-0.7c-1.1-0.5-0.9-1.2-2.8-1.2v0.4h-0.1v0.1c-3.1,0-4.5-0.3-6.6-1.2c-0.1-0.1-3.6-0.2-3.6-0.2c-0.3,0.1-0.6,0.4-1.1,0.2c-1.1-0.3-2-0.9-3-1.3h-1.7c-1.2-0.3-2.6-0.5-3.4-1.2c-1-1.4,0.3-2.1-0.1-3.2c-0.1-0.4-0.6-0.5-0.8-0.7c-0.1-0.6-0.2-1.1-0.4-1.7c-0.2-0.3-0.7-0.5-1-1c-1.5,0-3,0.1-4.4,0.1c-2.2-0.5-4.5-1.3-6.8-1.8c-1-0.2-4.9-0.2-5.4-0.6c-0.7,0-1.5,0-2,0.1c0.4,1,1.6,3.5,1.3,5c-0.1,0.5-0.5,1.1-0.7,1.6c-0.8,0.2-1.4,0.8-2.3,0.8c1.1,2.1-0.4,2-1.1,3.5c-0.2,0.5,0,0.8-0.2,1.2c-0.2,0.1-0.3,0.2-0.5,0.4c-0.8,1.4-1.2,3-2.2,4.3c-0.7,1-2.1,1.2-3.1,1.9c-0.4,0.3-0.6,0.9-1,1.3c0.2,1.4,2.2,1,3.4,1.4c0.2,0.1,0.2,0.4,0.4,0.5c0.3,0.2,0.8,0.2,1.1,0.5c0.4,0.3,0.7,1,1.1,1.2c0.9,0.4,2.3,0.3,3.2,0.6c1.3,0.4,2.1,1.6,3.1,2.3c1.1,0.7,2.4,0.8,3.7,1.3c0.2,0.1,0.5,0.4,0.7,0.5c1.2,0.4,2.1-0.1,3.2,0.2c0.1,0,0.5,0.3,0.6,0.4h1c0.1,0.2,0.2,0.3,0.4,0.5c0.5,0.2,1,0,1.4,0.2c0.1,0.2,0.2,0.3,0.2,0.5c0.3,0,0.6,0.1,0.8,0.1c0.5,0.3,1.3,1.1,1.6,1.6c0.1,0.2,0,0.4,0.1,0.5c0.6,0.4,1.2,0.6,1.8,1c0.3,0.2,0.3,0.6,0.5,0.8c0.2,0.3,0.7,0.4,1,0.6c0.3,0.2,0.3,0.6,0.5,0.8c0.4,0.5,1.3,1,1.9,1.3v0.7c0.6,0,0.7,0.1,0.7,0.7c0.4-0.1,0.7-0.5,1.1-0.6c0.9-0.2,2.2,0,2.8-0.4c0.2-0.1,0.1-0.6,0.4-0.7c0.2,0,0.5,0.1,0.7,0.1c0.1-0.1,0.2-0.5,0.4-0.6c0.3-0.1,0.5,0.1,0.6,0.1c0.1,0,0.4-0.3,0.6-0.4c1.4-0.3,2.4,0.7,3.7,0.4c1.8-0.4,5.5-1.4,7.2-0.7c0.8,0.3,1.3,1.3,1.9,1.7h0.5c0.9,0.4,1.7,0.9,2.8,1.2c2.4,0.6,4.9-2.3,7.9-0.8c0.5,0.2,0.8,1.1,1.1,1.6c0.7-0.1,1.5-0.5,2.4-0.2c0.5,0.1,1.2,0.7,1.9,0.5c0.2-0.1,0.3-0.4,0.5-0.5c0.7,0,1.4,0.1,2,0.1c0.4-0.1,0.8-0.9,1.7-0.6c2.3,0.8,4.2,1,7.1,1.1V69c1-0.2,1.7-0.8,2.4-1.1c0.6,0,1.1-0.1,1.7-0.1c0.9-0.4,1.4-1.3,2.5-1.6c0.1-2.3,1.7-2.1,3.5-2.2v-0.4c1.1-0.5,2.8-1,3.5-1.9h3.6v-0.1C333.9,61.4,332.8,60.8,332.5,59.6z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Old Pûkel-land")} [:path {:className "st3" :d "M319.3,289.1c-0.6-0.3-1.2-1.2-1.8-1.6c0-1.7,0.9-1.4,1.8-2c0.1-0.4-0.2-0.1-0.4-0.5c0-0.5-0.1-1-0.1-1.4c-0.1-0.2-1.1-0.7-1.3-0.8c-0.1-0.3-0.2-0.7,0-1c0.1-1,0.6-0.9,1.1-1.4h0.1v-0.1c-0.5-0.3-0.8-0.6-1.2-0.8c-0.1-2.7-1.4-4-3.4-4.9c0-0.5-0.1-1.3,0.1-1.6c0.1,0,0.2-0.1,0.4-0.1c0.4-1-0.7-1.6,0.4-2.8c-0.2-0.5-0.6-0.7-0.8-1.1c-0.4,0-0.6,0.2-0.8,0.2c-0.6,0.1-2.6-0.5-3.7-0.2c-2.2,0.5-5.5,0.2-7.3-0.1c-0.5,0.3-1.1,0.1-1.7,0.4c-0.6,0.3-1.2,1-1.9,1.2c-0.4,0-0.8,0.1-1.2,0.1c-0.6,0.3-1,1-1.7,1.2c-2.3,0.9-5.1,1.1-7.1,0.8c-0.6-0.1-0.6,0.5-1,0.6l-3.1,0.1c-1.5-0.3-2.5-0.8-3.7-1.4c-0.2-0.1-0.5-0.4-0.7-0.5c-0.8-0.2-1.8,0.7-2.4,0.8c-0.3,0.1-0.7-0.1-0.8-0.1c-0.9-0.2-1.8,0.1-2.4,0.2c-1.1,0.3-2.2-0.6-2.6-0.4c-0.7,0.1-1,0.7-1.3,1.2c-1.2,0.1-3,0.2-4,0.8c-0.7,0.5-1,1.6-1.7,2.2c-0.4,0.3-0.8,0.8-1.2,1c-0.6,0.3-1.7,0.4-2,0.8c0,0.2-0.1,0.4-0.1,0.6c-0.3,0.3-0.7,0.3-1,0.6c-1,1.1-2.5,3.2-4,3.7c-0.2,0-0.4-0.1-0.6-0.1c-0.4,0.2-0.3,0.7-0.8,0.8c-0.3,0.2-2,0.3-2.5,0.1c-0.6-0.2-1.2-0.8-1.8-1c-0.6,0-1.1,0.1-1.7,0.1c-0.6-0.2-0.9-1.1-1.6-1.3c-1-0.4-1.6-0.2-2.2-1.1c-1.6,0.1-1.9,0.2-3.4-0.1c-0.3,0.7-0.6,1.1-1.3,1.4c0.3,2.3-0.6,2.7-2.3,3.4c-0.3,0.1-1.3-0.2-1.3-0.1c-0.3,0.3-0.6,1-1.1,1.2c-0.5,0.3-1.1,0.1-1.4,0.5c-1.8,2.4-2.6,5.6-4.1,8.4c-0.4,0.8-1.3,1.5-1.7,2.3c-0.3,0.6,0.1,1.3-0.1,2.2c-0.1,0.4-0.5,1.9-0.7,2.3c-0.1,0.2-0.9,0.8-1,1c-0.1,0.4-0.2,0.8-0.4,1.2c-0.4,0.5-1,0.5-1.7,0.7c-0.7,0.3-1.6,1-2.3,1.3c-0.5,0.2-1.3,0.1-1.7,0.5c-2.2-0.1-3.5,0.1-4,1.7c-0.4,0.7,0.3,1.9,0,2.9c-0.1,0.2-0.8,0.7-0.5,1.3c0,0.1,0.4,0.1,0.5,0.2c0.7,1.6-0.3,4.5,0.6,5.9c0.3,0.6,1.1,0.6,1.4,1.1c0.4,0.5,0.4,1.1,0.8,1.4c0.7,0.5,1.5,0.5,2.3,1c0.4,0.3,1.7,2.8,1.9,3.4c0.2,0.1,0.1,0,0.2,0.1c1.6-0.2,2.2-2.6,3.4-3.5c1-0.8,3.4-0.9,4.9-1.1v-0.1c-0.4-0.1-0.7-0.3-1.1-0.5c-0.1-0.3-0.2-0.6-0.4-0.8c0.1-0.4,0.1-0.8,0.4-1.1c0.6-0.9,2-0.8,3-1.3c-0.1-1.8-2-1.2-3-2c-0.4-0.3-0.5-1-0.8-1.3c0-2.2,1.2-1.9,2.4-2.9c0.3-0.2,0.8-0.7,1-1c0.1-0.2,0.2-0.5,0.2-0.7c0.4-0.1,0.7-0.2,1.1-0.4c1.3-0.7,2.3-1.9,3.4-2.8c0.5-0.4,1.3-0.6,1.8-1.1c0.7-0.7,1.2-1.7,2-2.3c0.8-0.6,1.7-1.1,2.5-1.8c0.3-0.3,0.4-0.9,0.7-1.1c0.7-0.5,2.6-0.9,3.5-1.1c-0.2-0.7-1.2-0.9-1.6-1.4c-0.8-1.1-1.1-2.4-1.9-3.5c0-1.9,3-3.4,5.5-2.8c0.9,0.2,2.7,0.9,4.1,0.5c0.5-0.1,1.1-0.6,1.7-0.7c0.9-0.3,4.5,0,5,0.2c0.4,0.2,0.7,0.6,1.2,0.7c0.5,0.1,1.1-0.2,1.4-0.2c1.5-0.3,4.9,0,6,0.2c2.1,0.5,6.8-0.6,8.2,0.1c0.3,0.2,0.6,0.7,0.7,1.1c2.5,0,4.3-0.7,6.2-1.6c0.1-0.3,0.1-0.3,0-0.6c0.5-0.3,1.3-0.3,1.9-0.6c0.4-0.2,0.9-0.7,1.4-0.8c1.9-0.5,5.4,0.9,6,1.7c0.9,0,1.4-0.4,2.2-0.6c1.2-0.3,4,0.1,4.8,0.4c1.2,0.4,1.7,1.6,2.9,2c2.4,1,4.4,0.7,5.5,3c2,0,3-1,4.3-1.7c0.8-0.4,1.8-0.3,2.6-0.6c0.8-0.3,1.3-0.8,2.2-1.1c0.1-0.4,0.1-0.9,0.4-1.2c0.2-0.6,0.9-0.6,1.4-1h0.1C321.4,289.4,320.2,289.5,319.3,289.1z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Enedhwaith")} [:path {:className "st3" :d "M305.5,253.2c0-1.5-0.5-1.8-0.6-2.9c-1.2-0.5-1.3-0.5-2-1.4c-0.7-0.4-1.8,0-2.6-0.2c-0.4-0.1-0.7-0.5-1.1-0.7c0-1.4,0.3-2.7,1.3-3.1c-0.3-1.1-2-0.9-3.1-1.2c-1.3-0.4-2.5-1.2-3.6-1.6h-1c-0.7-0.2-1.9-0.7-2.6-1c0-1.1,0.3-1.5,0.8-2c-0.2-2-3.8-1.6-5.8-1.9c-0.7-0.1-2,0.6-2.9,0.4c-0.7-0.2-1.6-1-2.5-1.3c-0.6-0.2-1.4,0.2-1.7,0.2c-1.1,0.3-2.6-0.6-3-1.1c-1.2,0.1-4,1.6-5.5,1.1c-0.2-0.1-0.8-0.4-1-0.5c-0.3-0.1-3.5-0.2-3.7-0.1c-0.5,0.1-1.3,0.6-2.3,0.4c-0.9-0.2-2.8-1-3.8-0.7c-0.4,0.1-0.9,0.6-1.2,0.7c-1,0.4-1.1-0.2-1.8,0.7c-2,0-2-1.1-3.1-1.8c-0.8-0.5-1.4-0.1-2.4-0.5c-0.9-0.3-3.2-2.7-3.2-3.8c-0.3,0.1-0.8,0.4-1.1,0.5l-0.7-0.1c-0.3,0.1-2.6,1.9-2.9,2.2c-0.6,0.5-0.6,1.7-1.1,2.3c-0.5,0.4-1,0.7-1.6,1.1c-0.8,1.1-0.9,2.6-2,3.4c-0.7,0.5-1.7,0-2.6,0.4c-0.7,0.2-0.7,1-1.2,1.3c-0.9,0.5-2.7,0.2-3.8,0.6c-1.6,0.5-2.2,2.4-3.5,3.2l-1.6,0.4c-0.2,0.1-0.6,0.5-0.8,0.6h-1.3c-0.5,0.2-0.8,1.1-1.2,1.4c-0.4,0.3-1,0.3-1.4,0.6c-0.4,0.2-0.8,1-1.1,1.3h-0.4c-0.2,0.8-0.8,0.9-1.1,1.6c-0.6,1.3-1.7,4.3-2.6,5.2c-0.5,0.5-1.2,0.3-1.9,0.7v0.2l-0.8,0.2c-0.4,0.4-0.7,0.9-1.1,1.3c-0.6,0.4-1,0.2-1.3,1c-0.2,0.5,0.2,0.9,0.2,1.1v0.8c0.3,0.7,0.7,2,1,2.9c0.4,1.3-0.1,2.9,0.6,3.8c0.2,0.6,0.8,0.6,1.3,1c0.5,0.3,0.8,1,1.3,1.3c0.6,0.3,1.3,0.2,1.9,0.5c0.7,0.3,1.1,1,1.8,1.3c0.1,0.4,0.4,0.7,0.2,1.3c0,0.1-0.2,0.4-0.1,0.7c0.2,0.7,0.9,1.2,1.2,1.8c0.3,0.5-0.1,1.1,0.1,1.6c-0.1,1.8-1.5,1.8-2.3,2.9c-0.5,0.6-0.5,1.4-1,2.2c-0.2,0.4-0.9,0.9-1.1,1.3c-0.5,1,0.3,4.1,0.5,4.6c0.3,1.1-0.5,1.6-0.2,2.6c0.2,0.8,1.2,2.5,0.7,4c-0.1,0.4-0.6,0.7-0.7,1.1v0.5l-0.6,0.5c-0.2,0.4,0,1.1-0.2,1.4c0,1.6-0.7,1-1.8,1.4c-0.2,0.1-0.2,0.4-0.4,0.5c-0.5,0.4-1.2,0.2-1.8,0.5c-0.2,0.1-0.5,0.5-0.7,0.6c-0.3,0-0.6-0.1-0.8-0.1c-0.6,0.2-0.9,1.1-1.6,1.3c-0.8,0.5-2.1-0.3-2.5-0.2c-0.6,0.1-0.7,0.6-1.7,0.6v0.1h-0.1c0.3,1.4,2.5,0.5,4,0.8c2.2,0.5,5.3-2.3,6.7,0.2c1.9-0.1,1.4-1.4,2.3-2c0.8-0.7,2-1.2,3-1.6c0.9-0.3,1.8-0.1,2.4-0.6c0.3-0.2,0.6-1.1,0.7-1.4h0.4c0.5-2,3.7-1,4.8-2.2c0.5-0.5,0.7-1.2,1.1-1.8c0.2-0.1,0.3-0.2,0.5-0.4c0.1-0.5,0.2-1,0.4-1.4c0.1-0.2,0.5-0.3,0.6-0.5c0.5-0.6,0.6-1.8,1-2.6c0.1-0.3,0.6-0.8,0.7-1.1c0.1-0.4,0.2-0.7,0.2-1.1c0.3-0.6,1.8-2.9,2.3-3.1c1.2-0.6,1.4-0.5,2-1.7c1.2,0,2.3-0.1,2.9-0.6c0.3-0.5,0-1.7,0.4-2.2c0.2-0.4,1.8-1.6,2.2-2.2c1,0,1.5,0.4,2.2,0.5c0.8,0.1,1.9-0.6,2.8-0.1c0.2,0.3,0.5,0.6,0.7,0.8c0.4,0.3,1,0.1,1.6,0.4c0.5,0.2,0.8,0.7,1.2,1c0.7,0.1,1.4,0.2,2.2,0.2c0.5,0.2,0.9,1,1.9,0.7c0.4-0.1,0.8-0.5,1.1-0.6h0.7c1.4-0.5,2.1-1.9,3.1-2.9c0.3-0.3,0.8-0.4,1.1-0.7c0.3-0.4,0.5-0.9,0.8-1.2c0.9-0.8,2.4-1,3.2-1.8c0.6-0.6,0.8-1.5,1.6-2c0.5-0.3,1.5-1,1.9-1.2c1.5-0.6,1.5,0.4,2.5-1.2c0.8,0,1.8-0.6,2.8-0.2c0.1,0,0.4,0.3,0.5,0.4c0.4,0.1,3.9-0.1,4.1-0.1c0.8-0.2,2-1,3.5-0.6c0.9,0.2,1.7,1,2.5,1.3c1.2,0.5,3.3,0.7,4.1-0.1c0.2-0.1,3,0.3,3.5,0.2c2.2-0.3,4.9-1,6.2-2.2c0.1-0.3,0.2-0.6,0.4-1c0.2-0.4,1-0.9,1.2-1.1c0.2-0.3,0.4-2.8,0.6-3.4c0.4-1.1,3-4.8,4-5.4c-0.1-1.1-0.6-1.2-0.5-2.4C303.8,254.9,304.2,253.8,305.5,253.2z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Dunland")} [:path {:className "st3" :d "M314.8,221.5c-1.1-0.3-2.2-0.5-3.1-1c-0.6-0.3-1.4-0.8-1.9-1.2c-0.1-0.2-0.2-0.3-0.2-0.5c-0.5-0.3-1,0-1.4-0.2c-0.8-0.5-1.5-1.9-2.5-2.2c-0.5-0.1-1.8,0.2-2.2,0.1c-0.7-0.2-1.1-0.9-1.8-1c-0.8-0.1-1.6,0.2-2.2,0.4c-1.3,0.3-3,0.4-4.6,0.4c-0.3-0.3-0.6-0.5-0.8-0.8c-1.7,0.1-2.6,1.2-3.8,1.7c-1.4,0.6-3-0.3-4-0.5c-0.6-0.1-1.4,0.5-2.3,0.2c-0.6-0.2-1-0.8-1.6-1.1c-1.9-1-3.4-0.9-4.1-3.2c-0.5-0.1-1.2,0.2-1.8,0c-0.6-0.2-1.1-0.7-1.9-0.4h-2.4c-0.4-0.2-0.4-0.8-1-1c-0.5-0.1-0.5,0.4-1,0.6c-1,0.4-2.1-0.6-2.4-1c-0.3,0-0.6,0.1-0.8,0.2c-0.1,0-0.2,0.1-0.4,0.1c1,3.9-2.9,4.7-5.4,5.8c-0.7,0.3-2.6,0.9-3.6,0.2c-1.2,0.1-2.5,1.4-2.8,2.4c-0.2,0.7,0,1.2-0.4,1.8c-0.3,0.5-1.1,0.5-1.6,1c-0.7,0.7-0.4,1.3-0.8,2c-0.1,1.6-1.4,1.3-2.3,2c-0.4,0.5-0.1,0.9-0.4,1.6c-0.2,0.5-0.8,0.8-1,1.4c-0.6,1.3,1.8,3.1,2.6,3.6c0.3,0.2,1.2-0.1,1.7,0.1c0.1,0.2,0.2,0.4,0.4,0.6c0.5-0.3,1.4,0.8,1.7,1.2c0.6-0.1,0.6-0.3,1-0.5h1.4c0.3-0.1,0.7-0.5,1.1-0.6c1.6-0.5,3.4,0.3,4.3,0.6c0.8,0.3,1.4-0.3,1.9-0.4c0.2,0,4.1,0,4.6,0.1c0.2,0.1,0.6,0.4,0.7,0.5c1.7,0.6,3.9-1.7,5.5-1.1c0.7,0.3,1.4,1,2.2,1.1c0.4,0.1,0.5-0.3,0.7-0.4c0.6,0,1.2,0.1,1.8,0.1c0.4,0.1,1.1,0.3,1.6,0.5c0.4,0.2,0.7,0.7,1.1,0.8c0.8,0.3,1.5-0.5,2.2-0.6c1.1-0.1,2.7,0.6,4,0.6v0.4c2.1,0,2.9,0.8,4,2c0,1.1-0.5,1-0.6,1.9c1.8,0.1,3.1,1.1,4.6,1.6c1.2,0.4,2.9,0.3,3.7,1c1.3-0.1,1.3-1.4,2.2-1.8c0.8-0.4,2-0.2,2.6-0.7c0.2-0.4,0.4-0.8,0.6-1.2c0.2-0.1,0.3-0.2,0.5-0.2c0.3-0.6,0-1.8,0.2-2.4c0.1-0.2,0.5-0.2,0.6-0.4c0.3-0.4,0.1-1.4,0.2-1.9c0.4-1.2,1.4-2.4,2.8-2.9c-0.2-0.9-0.9-1.8-1.3-2.5c-1.8,0-4.4,0.1-5.3-0.6c-0.4-0.3-0.6-0.8-1-1.2c0-0.4,0-0.8,0.1-1c0.2-0.6,0.7-0.6,1.1-1c2.1,0,4.5,0,6.2,0.2c0.7,0.1,2.2,0.2,2.6-0.1c0.6-0.3,0.3-0.9,0.7-1.3c0.6-0.5,1.3-0.6,2-1.1c0.3-0.2,0.5-0.6,0.8-0.8v-0.2C315.9,221.5,315.3,221.6,314.8,221.5z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Hollin")} [:path {:className "st3" :d "M336,174.4V174c-0.8,0-1.1-0.2-1.7-0.4h-1.2c-0.7-0.2-0.8-0.7-1.8-0.7c-0.2,0.7-1.1,1-1.9,0.6c-0.6-0.3-2-0.8-2.3-1.3v-0.6c-0.2-0.4-0.9-0.6-1.2-1h-3.1c-0.5-0.1-1.2-0.7-1.7-0.7c-0.2,0-0.9,0.3-1.3,0.1c-0.4-0.2-0.8-0.6-1.3-0.7c-0.3,0-0.6,0.2-1.1,0.1c-0.4-0.1-1-0.5-1.3-0.6c-0.7-0.2-1.2,0.3-1.6,0.5c-0.8,0.3-1.7-0.1-2.4,0c-0.8,0.2-1.6,0.5-2.8,0.5c-0.3,0.3-0.6,0.6-0.8,1c-0.6,0-0.8-0.1-1.1-0.4c-0.8,0-0.9,0.4-1.2,0.8h-1.4c-0.9,2.3-1.7,2.8-4.2,3.5c-0.9,0.2-1.9-0.2-2.5,0.2c-0.3,0.1-0.4,0.2-0.6,0.4c0,0.5-0.1,0.4,0.1,0.7c0,0.5-0.1,0.7-0.2,1c-0.8,0-0.9-0.2-1.7-0.2c-0.2,0.2-0.7,0.3-0.8,0.5c-0.3,0.4-0.3,1-0.8,1.4c-0.3,0.2-0.8,0.2-1.1,0.5c-0.3,0.2-0.3,0.8-0.5,1.1c-0.4,0.6-1.8,1.1-2.5,1.4c-0.4,1-0.5,2.3-1.4,2.8c-0.3,0.1-0.5-0.1-0.7,0.1c-0.6,0.6-0.9,1.7-1.7,2.2c-0.7,0.4-1.5,0.4-2.2,0.8c-0.9,0.4-1.2,1.9-1.1,3c-0.2,0.5-0.5,1-0.7,1.4c-0.3,0.1-1.2-0.1-1.6,0.1c-0.2,0.1-0.7,0.6-1,0.7c-0.5,0-1,0.1-1.4,0.1c-1.5,0.6-1.6,1.9-3.8,1.9c-0.3,0.6-0.9,2.2-1.4,2.5c-0.7,0.4-1.6,0.4-2.2,1c-0.7,0.7-1.4,1.8-1.9,2.6c-0.3,0.4-0.3,1-0.6,1.3c-0.2,0-0.3,0.1-0.5,0.1c-0.7,0.8-1.3,1.6-2,2.3c-0.4,0.3-1,0.4-1.3,0.8c-0.9,1.2-1,3-2,4.1v0.5c0.8,0.2,2.1,0.3,3,0.6c0.5-0.5,1.1-0.7,1.6-1.2c1.7,0,1.7,0.5,2.8,0.8c0.1,0.1,1.1-0.9,1.8-0.5c0.9,0.1,1,0.9,1.7,1.1c0.9,0.3,2.4-0.4,3.5-0.1c2,0.6,2.5,0.3,3.4,2.5c1.6,0.7,3.2,1.1,4.3,2.3c0.7,0,1.4-0.5,2.5-0.2c0.6,0.1,1.3,0.7,2.2,0.5c1.5-0.3,2.1-1.3,3.8-1.3v-0.5c0.7-0.1,1.7-0.2,2.3,0.1c0.3,0.2,0.3,0.6,0.7,0.7c0.3,0.1,3.3-0.6,3.6-0.7h1.3c0.6-0.2,1.1-0.4,1.6,0.2c0.3-0.2,0.1,0,0.1-0.4c0.8,0.1,0.7,0.7,1.2,1c0.8,0.3,1.8-0.2,2.6,0.1c1,0.4,1.8,1.4,2.6,1.9c0.5,0.3,1.1,0.2,1.6,0.5c0.4,0.2,1.4,1.1,1.8,1.4h0.4c-0.4-0.7-1.2-2.2-0.6-3.1c0-0.6,1.6-2.4,2-2.5c0.3,0,0.6,0.1,1,0.1c0.4-0.2,0.8-0.9,1.2-1.1c0.4-0.2,0.8,0.1,1.1,0c0.6-0.2,1.2-0.7,1.9-0.8v-0.1h-0.1v-0.2h-0.4c-0.1-0.5-0.1-0.4-0.5-0.6c0-1.7,0.3-1.9,1-2.9c1-0.1,1.9-0.2,2.9-0.4c-0.2-0.3,0-0.1-0.4-0.2c-0.1-0.8-0.6-0.9-0.8-1.6c-0.3-1,0-2.3,0-3.4c0.2-0.2,1.7-1.4,1.7-1.4v-0.5c0.2-0.4,0.8-1,1.1-1.3c0.7-0.5,3.1-0.5,3.8,0h1.8v-0.1h-0.1c-0.6-0.7-2.5-0.8-3.6-1.2c-1.9-0.6-3.9-2-4.7-3.7c-0.1-0.2-0.1-0.3,0-0.5c0-0.8-0.3-0.6-0.6-1h-0.5c0-0.3,0-0.4,0.1-0.6c0.4-0.8,3.7-1.6,4.7-2c0-0.3,0-0.3-0.1-0.5c-0.2-0.1-0.3,0-0.5,0.2c-0.3,0-0.5,0-0.7-0.1c0.1-0.4,0.1-0.4,0.2-0.6c-0.3-0.2-0.6-0.4-0.8-0.6c0-0.5,0.1-1.4-0.1-1.7c0-0.2,0-0.2,0.1-0.4c0.5-0.3,1-0.2,1.7-0.5c1.2-0.5,1.1-0.9,2.5-0.4c0.2,0,0.5,0.1,0.7,0.1c0,0,0.4,0.6,0.5,0.6c0.2,0.1,2.8,0.5,3.1,0.5c1.2-0.3,1.8-1,2.6-1.6v-0.5c0.8-0.1,0.8-0.5,1.6-0.6v-0.4c-2.6,0-3.1,0.4-5.2-0.5c-0.4-0.2-0.7-0.1-1.1-0.4c-0.2-0.1-0.2-0.5-0.4-0.6c-0.3-0.2-0.6-0.2-0.8-0.5h-0.1c-0.2-0.3-0.1-0.5-0.4-0.7c0-1.4,0.9-1.8,2-2c-0.1-0.5-0.5-1.7-0.1-2.3C329.8,175.7,334.5,174.3,336,174.4z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Rhudaur")} [:path {:className "st3" :d "M357.4,127.7c-0.1,0-0.2,0.1-0.4,0.1c-0.3-0.2-0.6-0.4-0.8-0.6c0-2.3-0.4-3.1,0.5-5c-0.5-0.5-1.3-1-1.7-1.6v-1c-0.2-0.7-1-1.3-1.3-1.9c-0.2-0.3-0.1-0.6-0.2-1c-0.2-0.3-0.7-0.6-0.8-1c-0.2-0.5-0.1-2.4,0.1-2.8c0.1-1.3,3.4-3.3,4.8-3.6c0.2-1,0.1-0.6-0.1-1.3l0.1-0.7c-0.2-0.4-0.9-0.6-1.1-1.1c0-0.3-0.1-0.6-0.1-1c-0.2-0.5-1.1-0.7-1.3-1c-0.3-0.4-0.3-0.9-0.6-1.3c-0.3-0.5-3.9-2-4.8-2.2c-0.2-0.5-0.9-1-0.5-1.7c0-1.3,1.2-2.2,2.4-1.9c-0.2-0.3-0.4-1-0.7-1.3c0-2.1,1.6-1.7,2.9-2.5c0.1-0.1,0,0,0.1-0.2c-2.8-1-7.5-0.7-9-3c-1.8,0.1-3.5,0.5-4.2,1.7c-1.2,0-1-0.1-1.7-0.5c0.2-1.5,1.7-2,2.3-3.1c0.6-1-0.7-1.7-0.2-2.9c0.1-0.2,0.6-0.9,0.8-1.1c0-0.5,0-0.9-0.1-1.2c-0.8,0-1.4-0.1-1.6,0.6v0.8c-0.1,0.2-0.5,0.2-0.6,0.4c-0.1,0.2,0,0.6-0.1,0.7c-1.2,1.6-2.7-0.1-4.1,0.5c-0.3,0.1-0.7,0.7-1,0.8c-0.7,0.4-1.7-0.2-2.2,0l-0.6,0.8c-0.5,0.3-1,0.4-1.4,0.7c0,0.1-0.1,0.2-0.1,0.4c-0.4,0.3-1.4,0.4-1.8,0.7c-0.5,0.4-0.7,1.2-1.4,1.4h-1c-0.1,0.1-0.5,0.4-0.7,0.5c-0.4,0.1-2.6-0.3-3-0.5c-0.3-0.1-0.6-0.6-0.8-0.7c-0.7-0.2-0.7,0.4-1.4,0.2c-1.1-0.2-2.1-0.6-2.9-1.1c-0.2-0.9-0.3-1.3-0.1-2.3c0.3-0.1,0.8-0.3,1.1-0.5c0.5-0.8,0-1.8,1.1-2c-0.1-0.2-0.2-0.3-0.4-0.5c-0.5,0-1.7-0.3-2.2-0.1c-0.5,0.1-1,0.6-1.7,0.7c-1.1,0.3-1.9,0-2.9,0.4c-0.3,0.1-0.8,0.6-1.1,0.7c-0.8,0.4-2.1-0.1-2.8,0.1c-0.1,0-0.4,0.3-0.5,0.4h-1.2c-0.2,0.1-0.5,0.5-0.7,0.6c-1.1,0.4-5.5-0.2-5.9-0.6c-0.5-0.2-0.7-0.8-1.1-1.2c0-1.2,0.8-2.3,1.7-2.5v-0.2c-0.7-0.3-1.5-0.6-2.2-0.8c-0.8-0.3-2.1,0-2.6-0.2c-0.3-0.1-0.4-0.5-0.8-0.7c-0.1-0.4-0.2-0.8-0.2-1.2c-0.5,0.2-4.8-0.7-5.2-1.1c-0.5-0.7-0.4-1.1-0.2-1.9c-0.8,0-1-0.3-1.7-0.5c-0.9-0.3-1.5,0.3-2,0.5c-1.2,0.3-2-0.3-2.8-0.5c-0.1-0.4-0.1-0.8-0.1-1.3c-2.4-0.4-3.1-1.2-4.4-2.6c-1.2,0.1-4.4,1.6-5.4,0.8c-1.5-0.4-1.6-2-2.3-2.4h-0.6c-0.1,0-0.6-0.6-0.6-0.7c-0.1-0.4-0.2-0.9-0.2-1.3c-0.2-0.4-1-0.5-1.3-0.8c-0.7-0.9-2-2.6-2.9-3.2c-0.8-0.6-1.9-1.1-2.4-2H262c0,3-1.4,3-2.6,4.6c-0.2,0.3-0.3,0.6-0.5,0.8c-0.5,0.5-1.3,0.8-1.7,1.3c-0.5,0.8-0.5,1.8-1,2.5c-0.7,1.1-2,1.9-3,2.8c-0.2,0.2-0.3,0.6-0.5,0.8c-0.4,0.4-0.9,0.7-1.3,1.1c-0.6,1-0.9,1.9-1.6,2.8c-0.4,0.5-1.7,1.1-2.2,1.8c-0.5,0.6-1,1.4-1.3,2.2c0,0.3-0.1,0.6-0.1,1c-0.2,0.5-0.7,0.9-1,1.3l-0.1,0.6c-0.3,0.4-1.5,1.6-1.7,2c-0.2,0.6,0,1.1-0.2,1.6c-0.3,0.5-0.9,0.8-1.2,1.2c-0.6,0.8-1,2.9-0.7,4.1c0.2,0.8,0.9,0.9,1.3,1.4c0.5,0.7,0.7,1.5,1.2,2.3c0.2,0.1,0.3,0.2,0.5,0.4c0.1,0.3,0.2,0.6,0.2,1c0.5,0.8,1.8,1,2.4,1.8c0.5,0.7,0.4,2.1,0.8,2.9c0.4,0.7,1.4,1.6,1.7,2.2c0.4,0.8,0.3,1.6,0.8,2.3c0.5,0.7,1.6,1.1,2,1.8c0.7,1.1,0.6,2.7,1.2,3.8c0.5,0.9,1.2,1.9,1.7,2.8c0.2,0.3,0.1,0.5,0.2,0.8c0.3,0.5,1.1,0.6,1.6,1c0.8,0.6,2,2.5,2.8,2.9c0.6,0.3,1.4,0.4,1.9,0.7c0.2,0.4,0.6,0.9,0.7,1.3c0.3,0.8,0,1.4,0,2.2c0,0.3,0.2,0.6,0.2,0.8c0.1,0.3-0.1,0.4-0.1,0.6c-0.1,0.7,0.1,1.4,0.2,2.2c0.1,0.3-0.1,0.8-0.1,1c0,0.2,0.3,1,0.4,1.3c0.2,1,0,1.9,0.2,2.8c0.3,1,0.9,2.2,1.2,3.2c0.3,1-0.3,1.7,0,2.5c0.6,1.6,1.5,1.6,2.4,2.6c0.5,0.6,0.7,1.6,1.3,2c1.1,0.8,2.7,1.2,3.5,2.3c0.2,0.4,0.2,0.9,0.5,1.2c-0.1,0.5-0.2,0.4-0.4,0.8c-1.4,0.3-3.4,0.1-4.8,0.6c-0.5,0.2-0.6,0.6-1.2,0.7v0.4c-0.4,0.2-0.7,0.3-1.1,0.5v0.4c0.6,0.1,1.3,0.5,1.8,0.5c0.4,0,1.3-0.2,1.9,0c0.6,0.2,1,0.8,1.4,1.1c1.4,0.9,2.6,1.9,4,2.9c0.5,0.4,1,1.2,1.6,1.6c0.6,0.4,1.4,0.4,2.2,0.7c1.7,0.8,3.9,1.4,5.6,2.3c0.6,0.3,1.2,1.1,1.8,1.4c0.8,0.5,1.8,0.8,2.5,1.4c0.6,0.6,0.9,1.5,1.7,2c1.2,0.8,2.5,1.5,3,2.9c0.8,2.3-0.7,3.8-0.7,5.5c0,0.8,0.4,1.7,0.2,2.3c-0.2,0.6-0.8,1.5-1.1,2c-0.1,0.3,0,0.6-0.1,0.7v0.2c0.6-0.4,1.1-1.1,1.6-1.7c0.4-0.4,1.4-1.2,1.9-1.6c0.2-0.2,0.3-0.6,0.5-0.7c0.4-0.3,1.1-0.2,1.4-0.5c0.4-0.3,0.4-1,0.8-1.3c0.8-0.5,1.8-0.2,2.9-0.5c0.9-0.2,1.4-0.8,2-1.1c0.4-0.2,0.7,0,1-0.2c0.6-0.4,0.5-0.9,0.8-1.6c0.5-0.9,1.5-0.5,2.2-1c0.1-0.2,0.2-0.4,0.4-0.6h1.9c0.3-0.1,0.5-0.6,1-0.7h1.9c0.9-0.2,2.3-0.2,3.2-0.5c0.6-0.2,1.8-0.8,2.6-0.5c0.3,0.1,0.4,0.6,0.7,0.7c0.6,0,1.2,0.1,1.8,0.1c0.6,0.1,1.1,0.5,1.6,0.6c0.9,0.3,1.5-0.3,2.3,0.1c0.1,0.1,0.2,0.2,0.2,0.4h2.2c2,0.4,2.7,0.7,3.4,2.5c0.4,0.1,0.8,0.5,1.3,0.4c0.6-0.2,1.5-0.7,2.5-0.5c0.8,0.2,0.9,0.5,1.3,0.7c0.6,0,1.3,0.1,1.9,0.1c0.9,0.3,1.7,1,2.4,1.4c0.3,0,0.4-0.4,1-0.5v-0.6c0.3,0,0.3,0.1,0.5,0c0.5,0,0.4-0.1,0.7-0.2v-0.4c0.8-0.1,2.1-0.8,2.5-1.4h0.1c-0.1-0.3-0.2-0.3-0.4-0.5c-1.8,0.2-2.3-0.5-3.1-1.4c0.2-1.3,0.5-1.2,0.8-2c0.6-0.2,1.6-0.5,2.3-0.6c0.5-0.1,1.1,0.1,1.9-0.2c0.6-0.3,1-1,1.7-1.3c0.4-0.2,0.6-0.1,1.1-0.2c0-0.1,0.1-0.2,0.1-0.2c0.5-0.2,1.1,0.1,1.4-0.1c0.7-1.1,0.2-1.9,1.8-2.4c-0.4-0.7-2.1-0.1-3.1-0.5c-0.2-0.1-0.5-0.4-0.7-0.5c-0.1-1.9,0.4-3.4,1.7-4.2c0.4-0.2,0.8-0.1,1.1-0.4c0.4-0.3,0.5-0.8,1.1-1v-0.4c-0.4,0-0.9,0-1.1-0.1c-1.1,0-1.3-0.4-1.8-1c0.1-1,1-3.5,1.4-4.3c0.3-0.6,1.4-1,2-1.4c0.2-0.1,0.1-0.5,0.2-0.6h0.5c0.1-0.2,0.2-0.3,0.4-0.5h0.5c0.7-0.4,0.8-1.7,1.6-2.2c-0.1-0.9-0.2-1.8-0.2-2.6c-0.2-0.3-0.6-0.4-0.7-0.8c-0.2-0.8,0.6-1.9,0.6-3.1h-0.4c-0.1-0.2,0-0.6-0.1-0.8c-0.2-0.4-0.8-0.7-1-1.1c-0.5-1.5,0.6-4.4,1.2-4.7h0.8c0.2-0.1,0.3-0.6,0.5-0.7c0.6-0.3,1.2-0.4,1.7-0.7c0.3-0.2,0.4-0.5,0.7-0.7c0-0.3,0-0.3,0.1-0.5C357.7,128.3,357.8,127.9,357.4,127.7z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Coastal Sea" "Andrast Coast")} [:path {:className "st4" :d "M232.4,423.2c-0.2-0.9-0.6-1.7-0.8-2.4c-0.4-1.6,1-3.5,1.3-4.9c0.3-1.4,0.1-5.2-0.2-6.1c-0.2-0.6-0.7-1.3-0.8-1.9c-0.2-0.9,0.2-1.8,0.4-2.4c0.4-1.5,0.1-3.5-0.1-4.8c-0.1-1,0.4-2.5-0.4-3.1v-0.1c-1.2,0.4-1.6,1.6-2.9,1.8c-0.1,0.4-0.4,0.8-0.4,1.3c0.1,0.6,0.4,1.4,0.1,2.2c-0.6,1.5-3.3,2.8-4.6,3.7c-0.2,0.1-0.4,0.6-0.6,0.7c-0.8,0.4-2,0-2.8,0.5c-1.4,0.5-1.4,2.3-2.2,3.5c-0.3,0.5-1.1,1-1.3,1.6c-0.2,0.4,0,0.9-0.1,1.2c0,0.9-0.3,0.8-0.5,1.3c-0.5,0-0.5,0-0.7,0.2h-7.7c-0.7,0.3-0.9,1.4-1.3,2c-0.4,0.6-1.4,1.6-2,1.9h-0.7c-0.5,0.2-0.8,0.8-1.4,1c-0.7,0-1.4-0.1-2-0.1c-0.2,0.1-0.4,0.6-1,0.4c-0.7-0.2-1.9-0.8-2.6-1c-0.9-0.2-1.6,0.1-2.4-0.2c-1.2-0.5-2-1.8-2-3.5c0.5-0.5,0.9-1,1.7-1.3c0-0.3,0-0.7-0.1-0.8c-0.6-1.3-3.3-0.1-4.8-0.6c-0.4-0.1-0.6-0.5-1-0.7c-0.1-0.7-0.3-1.6,0.1-2c0.4-0.8,1.7-0.6,2.2-1.3c1.1-1.5,1.3-2.8,3.2-3.6c1-0.4,2.7,0.1,3.5-0.2c1.5-0.7,2.8-2.1,4.4-2.6c0.8-0.3,1.7,0.1,2.5-0.2c0.3-0.1,0.7-0.6,1-0.7c0.4-0.2,0.7,0,1-0.2c0.6-0.4,1.4-1.3,1.7-1.9c0.2-0.5,0-1.1,0.1-1.7c0.2-0.6,0.9-1.3,1.3-1.6c0-0.6,0.3-1.6,0-2c0-1.4-0.7-2-1.7-2.4h-1.1c-0.8-0.3-2.3-0.8-3.1-1.1h-1.3c-0.4-0.1-1.4-0.4-1.8-0.5h-1.7c-1.8-0.5-4.7-0.4-6.7-0.8h-1.4c-3.3-0.8-7.3-0.7-11.5-0.7h-26.3c-0.1,0-0.5,0.3-0.6,0.4c-0.8,0-1.5,0.1-2.3,0.1c-0.7,0.3-1.1,1.1-1.7,1.6c-0.6,0.4-1.3,0.6-1.8,1.1c-0.8,0.9-1.2,2.4-2.2,3.1c-0.6,0.4-1.2,0.4-1.9,0.7c-0.4,0.2-1,0.8-1.4,1c-0.6,0.3-1.1,0.1-1.7,0.4c-0.7,0.3-1.2,1.2-1.8,1.6c-1,0.6-2.5,1.1-3.2,1.8c-0.7,0.6-2.3,1.9-3.2,2.2c-1.2,0.4-2.5,0.4-3.6,0.7H126c-2.1,0.6-4,1.6-6.2,2.2h-1.3c-1,0.3-2.8,0.6-3.8,0.8h-2.8c-2.3,0.6-5.2,1-7.6,1.6c-1.2,0.3-1.9,0.1-3.1,0.4c-1.4,0.4-3.5,0.8-4.9,1.3c-1,0.4-2,1.4-3,1.8c-0.6,0.2-1.5,0.1-2.2,0.2c-1.3,0.3-3.4,0.8-4.7,1.3c-0.8,0.3-1.3,1.2-2.2,1.4c-1.9,0.1-3.8,0.2-5.8,0.4h-2c-1.2,0.3-2.7,0.8-3.8,1.1H67c-1.3,0.3-2.7,0.6-4.1,1.1c-0.9,0.3-1.8,0.9-2.8,1.2l-1.7,0.1c-0.8,0.2-1.8,0.9-2.5,1.2c-0.3,0-0.6,0.1-0.8,0.1c-1,0.4-2,1.2-3,1.6c-1.7,0.7-3.9,1.1-5.4,2c-0.7,0.5-1.5,1.1-2.3,1.7l-1.3,1.4c-0.4,0.3-1,0.4-1.4,0.7c-0.7,0.5-1.5,1.3-2.4,1.7c-1,0.4-1.7,0.2-2.8,0.5c-1.6,0.5-3.3,1-5.5,1v10.3h198.1c0.2-1.8,0.7-2.9,1.1-4.4c0.4-1.6,0.2-3,0.8-4.3c0.5-1,1.6-2.2,1.9-3.2c0.2-0.6-0.3-0.9-0.4-1.2C232.5,425.8,232.5,424.5,232.4,423.2z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Coastal Sea" "Eriadoran Coast")} [:path {:className "st4" :d "M227,298.9c0,0-2.6,0.4-2.9,0.6c-0.9,0.7-1,2-2,2.5c-1.2,0.6-2.9,0.6-4.1,1.3c-0.7,0.4-1,1.2-1.4,1.8c-0.3,0.5-0.9,0.4-1.1,1.1c-0.9,0.1-1.4,0.4-2.3,0c-0.6-0.3-0.8-1.2-1.9-1c-0.9,0.2-1.6,0.7-2.4,1c-1,0.3-5.5-0.4-6.1-0.7c-0.5,0.1-1,0.6-1.4,0.4c-1,0-1.2-0.2-1.8-0.5c-0.1-0.7-0.1-1.2,0-1.9c0.2-0.2,0.6-0.5,0.7-0.8h2.5c0.6-0.3,2.6-1.7,3.6-0.5c1-0.2,1.2-0.9,1.9-1.3c0.7-0.4,1.6-0.3,2.4-0.6c0.8-0.4,1-1,2.4-1c0.1-0.9,0.2-1.8,0.4-2.6c0.3-0.6,1-1,1.2-1.7c0.3-1.1-0.4-2.1-0.6-2.8c-0.2-0.6,0-1.5-0.1-1.9c-0.2-0.9-0.5-4.7-0.2-5.6c0.2-0.7,0.7-1.3,1-1.8c1-1.9,1.3-3.3,3.2-4.3c0.1-1.7-0.5-1.6-1-2.8c-0.2-0.6,0-1.5-0.4-2c-0.6-1.2-2-1-3.1-1.6c-0.5-0.3-0.8-0.9-1.2-1.2c-0.6-0.4-1.5-0.7-2-1.1c-0.1-1.9-0.6-4.2-1.1-5.8c-0.2-0.8-0.1-1.9-0.5-2.5v-0.2c-1.2,0.4-2.1,1.3-3.1,1.8c-0.4,0.2-0.7,0-1.1,0.2c-0.2,0.1-0.4,0.4-0.6,0.5c-0.8,0.3-1.4-0.1-1.9,0.4c-0.9,0.8-2.5,3.2-3,4.3c-0.1,0.4-0.2,0.9-0.2,1.3c-0.2,0.5-0.8,0.5-1,1.2c-0.2,0.5,0.7,2.3,0.4,3.5c-0.2,0.7-0.9,1.8-1.4,2.3c-0.3,0.3-0.7,0.2-1,0.6c-0.7,1.1-0.6,2.8-1.7,3.5c-0.6,0.4-2.1,0.8-2.9,0.2c-0.9-0.1-0.7-0.5-1.1-1.1c-0.8-1.3-1.4-2.5,0-3.8c0-0.9,0-1.2-0.4-1.7c-1.2,0-2.5,0.7-3.8,0.2c-0.2-0.1-0.6-0.4-0.8-0.5c-0.9-0.2-1.6,0.1-2.3-0.4c-0.1-0.1-0.2-0.4-0.4-0.5c0-1,0.4-2.4,0-2.9v-0.1c-0.2-0.1-0.2,0.1-0.2,0.1c-1,0-2.1,1.1-2.4,1.8c-1.9,0-3.7,0.4-5.3-0.5c-0.3-0.2-0.5-0.6-0.8-0.8c-0.6-2.4,1.8-2.3,3.5-3.2c0.5-0.3,0.7-0.8,1.1-1.2c-0.2-1-1.3-1.2-1.8-1.9c-0.5-0.6-1.1-1.4-1.4-2c-0.3-0.7-0.2-1.9-0.6-2.5c0-1.4,1.4-2.9,2.2-3.6c-0.1-1-0.8-1.7-0.8-2.6c0.3-0.5,0.7-3.8,0.4-4.8c-0.2-0.8-1.1-1.1-1.6-1.7c-0.5-0.7-0.7-1.6-1.1-2.5c-1.3,0.1-2,1.8-1.6,3.5c0.7,2.8,0.7,5.3-1.3,6.6c0,0.7,0.1,1.4,0.1,2c-0.3,1.1-1,3.2-1.8,3.8c-0.5,0.4-1.2,0.3-1.8,0.6c-0.2,0.3-0.4,0.6-0.6,0.8c-0.7,0.4-1.3,0.4-1.9,0.8c-0.2,0.1-0.2,0.5-0.4,0.6c-0.7,0.6-1.2-0.6-1.7,1c-2.3,0.4-2.9,0.3-4-1.4c-0.3-0.5-0.8-0.8-1-1.4c-0.2-0.8-0.1-1.5-0.4-2.2c-0.1-0.3-0.5-0.8-0.6-1.1c-0.2-0.6,0.2-3.2,0.4-3.5c0-1,0.7-1.4,1.3-1.8c0-0.3-0.1-0.3-0.1-0.5c-0.4-0.1-1.1-0.2-1.4,0c-0.6,0-1.4,0-1.8-0.1c-0.3-0.8-0.2-2.1,0-2.9c0-0.7,0.1-1.4,0.1-2.2c0.5-1.4,1.8-2.3,2.3-3.7c0.4-1.2-0.9-2.2-0.2-3.1c-0.1-1.5-1.5-0.7-2.3-1.4c-0.4-0.2-0.4-0.5-0.6-0.8c-0.3-0.2-0.6-0.1-0.8-0.4h-0.5c-0.2,0.8-0.2,2.3-0.6,2.9c-0.5,1.3-3.4,1.2-4.8,1.7c-0.8,0.3-0.8,1.1-1.3,1.6c-0.4,0.3-1,0.4-1.3,0.8c-0.9,0-2.3-0.4-2.9-0.2c-0.3,0.1-1,0.5-1.4,0.2c-0.9,0-1.8-1.1-1.6-2.2c0.1-0.4,0.4-0.6,0.2-1c-0.7-1.5-4-0.5-4.1-3.7c-0.4,0.1-0.6,0.5-1,0.6c-0.7,0.2-1.6,0.1-2,0.5c-1.2,0.5-0.8,2-1.4,3c-0.1,0.1-1.6,1.3-1.8,1.4c-0.4,0.2-0.9,0.2-1.2,0.5c-0.4,0.4-0.7,1.7-0.8,2.3c-0.1,0.5,0.2,0.7,0,1.3c-0.2,0.7-0.8,1.2-1.2,1.8c-1.4,0.2-4.8,0.9-5.6,1.6c-0.5,0.3-1.3,2.2-1.4,2.9c-0.2,1,0.3,1.6-0.1,2.4c-0.2,0.4-1.3,1.2-1.7,1.4h-0.6c-0.5,0.3-0.6,1.4-1,1.8c0.1,0.9,0.6,1.6,0.7,2.4c0.1,0.8-2.1,3.8-2.6,4.1c-0.9,0.5-1.7,0.6-2.2,1.4c-0.9,1.5-1.3,3.2-2.3,4.7c-0.2,0.4-0.7,0.7-1,1.1c-0.4,0.7-0.6,1.4-1.1,2c-1.1,1.3-2.8,1.9-4,3.1c-0.7,0.8-1.9,2.3-2.3,3.2c-0.3,0.8-0.3,1.7-0.7,2.4c-0.8,1.3-2.2,2.1-3.4,3.1c-0.9,0.9-1.7,2-2.8,2.8c-0.6,0.5-1.5,0.5-2.2,0.8c-0.1,0.1-0.3,0.4-0.5,0.5c-0.4,0.2-0.9,0-1.3,0.2v0.2c-0.7,0.4-2.1,0.8-2.9,1.1h-2.2c-1.3,0.5-2.9,0.9-4.4,1.1c-1.7,0.2-4,0.1-5.6,0.7h-0.7c-2.8,1.3-5.7,2.7-8.6,3.8h-1.1c-0.7,0.2-1.5,0.2-2.4,0.5c-1.4,0.4-3.3,0.9-4.7,1.3c-0.7,0.2-1,0-1.7,0.2c-0.2,0.1-0.7,0.5-1,0.6c-0.7,0.3-1.3,0.2-2,0.5c-2.4,1-5.4,1.9-7.8,2.9h-0.7c-1,0.4-2.7,0.8-3.7,1.2c-0.5,0.2-0.9,0-1.4,0.2c-0.1,0.1-0.2,0.2-0.2,0.4c-0.4,0.2-0.7,0-1.1,0.2c-1.5,0.8-4.5,1.8-6.5,2v119.6c2.3,0,3.9-0.5,5.6-1c1-0.3,1.9-0.1,2.8-0.6c2.5-1.5,4.4-3.8,6.8-5.3c0.5-0.3,1-0.3,1.6-0.5c0.6-0.3,1.3-0.7,1.9-1c0.7-0.3,1.3-0.3,2-0.6c2-0.8,4.2-2.2,6.4-2.9c0.6,0,1.1-0.1,1.7-0.1c0.9-0.3,1.9-0.9,2.8-1.2l1.6-0.2c0.7-0.2,1.9-0.6,2.8-0.8h5.5c0.4-0.1,0.9-0.5,1.2-0.6c1-0.3,2.3-0.4,3.1-0.6c2-0.5,5.4,0.4,7.2-0.2c0.7-0.3,1.3-1.1,2.2-1.4c1.5-0.5,3.2-0.9,4.7-1.3c0.7-0.2,1.3,0,2-0.2c0.7-0.3,1.6-1.1,2.3-1.4c1.3-0.7,4.2-1.2,5.8-1.7c1.2-0.3,2-0.1,3.2-0.4c2.3-0.6,5.2-1,7.6-1.6h2.5c0.6-0.1,1.3-0.3,1.9-0.5h1.3c0.4-0.1,1-0.4,1.4-0.5c0.9-0.2,1.5,0,2.3-0.2c1.5-0.5,3-1.4,4.6-1.8h1.3c1.1-0.3,2.4-0.3,3.6-0.7c0.4-0.1,0.5-0.6,0.8-0.7h0.7c0.8-0.3,1.3-1.1,1.9-1.6c0.9-0.6,1.8-0.8,2.6-1.3c0.6-0.3,1-1.1,1.6-1.4c0.6-0.4,1.3-0.3,2-0.6c0.5-0.2,1.1-0.7,1.6-1c0.6-0.3,1.2-0.3,1.7-0.6c0.5-0.3,1.1-1.9,1.6-2.5c0.3-0.4,1.1-1.1,1.4-1.3h0.5c0.6-0.4,1.5-1.7,2.2-2h0.8c0.3-0.1,1.3-0.4,1.7-0.5h28.6c3.9,0,7.7-0.1,10.8,0.7h1.7c1.7,0.4,4.1,0.2,5.6,0.7c0.8,0,1.6,0.1,2.4,0.1c1.6,0.4,3.6,0.7,5,1.2c1.2,0.5,2.7,0.2,3.5,1.1c0.3,0,0.4-0.1,0.6-0.1c0-0.7-0.2-2,0.1-2.4c0.1-1.5,1.6-2,2.2-3c0.2-0.4,0.2-1,0.5-1.2c0.5-0.4,1.1-0.2,1.6-0.8c0.9-1.1,1.5-2.4,2.4-3.5c0.5-0.6,1.6-1.1,2-1.8c0.4-0.7,0.3-2,0.6-2.8c0.6-1.5,1.5-2.9,2.3-4.2c0.9-0.3,2-0.6,3-0.8c0.1-0.7,0.3-0.9,0.5-1.3c0.4-0.9-0.3-2.4-0.5-2.9l-0.1-1.4c-0.3-0.9-0.9-1.8-1.2-2.8V360c-0.3-1.1-1.7-2.8-2.5-3.6c0-2.3,1.3-3,2-4.4c0.2-0.4,0.1-0.8,0.4-1.1c-0.1-1.7-2-3.6-2.8-4.8c-0.3-0.5-0.3-1.2-0.8-1.4c-1-0.5-2.3-0.9-2.9-1.8c-0.8-1.2-1-4.6-1.8-5.4c-0.4-0.4-1.5-0.9-2.2-1.1h-1.2c-0.6-0.2-1-0.9-1.4-1.3l0.1-1.2c0.6-0.5,1.2-1.2,1.9-1.6h0.5c0.3-0.2,0.3-0.9,0.6-1.1c0.3-0.2,0.8-0.2,1.1-0.5c0.6,0.1,1.3,0.3,1.6,0.7c1.1-0.1,2.3-0.7,2.9-1.4c0.2-0.3,0.3-0.6,0.5-0.8c1.1-1,2.4-1.8,3.7-2.5c0.1-0.2,0-0.1,0.1-0.2c-0.2-1.3-1.1-2.9-2.2-3.5c-0.5-0.3-1.2-0.3-1.7-0.7c-0.4-0.3-0.6-1.1-1-1.6c-0.2-0.2-0.4-0.1-0.6-0.2c-0.6-0.5-1.1-1.2-1.4-1.9c-0.8-1.9,0.4-4.9-1.1-6c0-0.9,0.5-1.3,0.7-1.9c0.3-0.9-0.5-1.9-0.2-2.5c0.2-1.6,1.8-2.6,3.1-3.1h2.6c1.1-0.3,2.2-1.1,3.1-1.6h0.7c0.1-0.1,1.6-2.1,1.7-2.3c0.3-0.6,0.2-1.3,0.5-1.8C227.2,299,227.2,299.1,227,298.9z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Cardolan")} [:path {:className "st3" :d "M289.9,173c0.3-0.9,1.4-1.9,0.8-3.6c-0.5-1.5-2-1.7-3-2.6c-0.6-0.6-1-1.6-1.7-2c-0.6-0.5-1.4-0.6-2-1.1c-0.6-0.4-1.3-1.2-2-1.6h-0.6c-0.9-0.3-2.5-0.7-3.4-1.2c-0.3-0.1-0.7-0.6-1-0.7c-0.9-0.4-1.8-0.3-2.5-0.8c-0.7-0.5-1.2-1.3-1.8-1.8c-1.4-1-2.6-2.2-4.1-3.1c-0.8-0.5-2.5-0.6-3.5-0.5c-1.1-0.6-2.6-1.1-3.7-1.6c-0.5-0.2-1.1,0.1-1.4,0.1c-0.4,0-0.8-0.1-1.2-0.1c-1.8,0.7-2,2.8-3.5,3.6c-1,0.5-2.6,0.8-4.1,0.4c-0.4-0.1-1.9-0.6-2.5-0.4c-0.4,0.2-0.4,0.5-1.2,0.6c-0.2,0.8-0.5,1.4-0.8,2c-0.7,0.2-1.5,0.4-2.3,0.5c0.2,0.7,0.9,1.6,1.3,2.2c0,1-0.5,1.1-0.8,1.7c-2.2,0.1-6-0.2-7.1-1.6c-1.2,0.1-1.9,0.6-2.8,1.1c0,0.7,0.2,2.1-0.1,2.5c-0.3,2.6-4.8,2.6-7.1,1.7c-0.3-0.1-0.5-0.4-0.7-0.6c-0.5-0.4-1.1-0.3-1.7-0.6c-0.4-0.2-1.7-1.1-2-1.4c-0.5-0.4-0.6-1.6-1.2-1.9c-0.5-0.3-0.9-0.2-1.3-0.5c-0.6-0.4-0.9-1-1.3-1.7l-1-0.8c-0.7-1-1.5-1.9-2.2-2.9c-0.7,0-1.7-0.1-2.2-0.5c-0.8,0-2.6-0.2-3.1,0.1c-0.3,0.4-0.3,1.6,0,2v0.5c-0.2-0.1-0.1,0-0.2-0.1c-0.6,0.9-0.2,1.8-1.4,2.2c0,0.2,0,0.2-0.1,0.4c0,1.7-0.5,2.3-1.4,2.9c0,1.8-0.1,3.7-0.1,5.5c-0.1,0.3-0.4,0.5-0.5,0.7c-0.1,0.4-0.2,0.8-0.2,1.2c0,0.1-0.3,0.3-0.4,0.4v1.1c-0.2,0.7-0.5,1.1-1.1,1.4c0,0.6,0.9,2.8,0.5,3.6h-0.2c-0.4,0.5-0.8,1.2-1,1.8c-0.5,1.7,0.6,3,0.8,4c0,1,0.1,2.1,0.1,3.1c0.1,0.2,0.5,0.7,0.6,1.1v0.6c0.5,0.9,2.8,1.4,3.2,2.8c0.2,0.3-0.1,1.5-0.4,1.8c-0.2,0.5-0.5,0.4-0.8,0.7c-0.3,0.3-0.6,0.9-1,1.2c-0.9,0.7-2.9,1-4.1,1.3H202c-0.1,0.2-0.2,0.3-0.4,0.5c-0.4,0.2-0.8,0.1-1.2,0.4c-0.2,0.2-0.5,0.6-0.7,0.7h-0.5c-0.7,0.4-2.3,2.1-1.6,3.4v3.7c-0.5,1.2-1.8,0.6-3.1,1c-0.5,0.2-1.1,0.7-1.7,0.8c-0.4,0.1-1-0.1-1.2-0.1c-0.4-0.1-1,0.2-1.3,0.4c-0.7,0.3-1.2,0.9-2,1.1c-0.1,0.7-0.3,1.1-0.6,1.6c-2.1,0-3.6,0.2-4.8,1c0,1.7,1.4,2.7,1,4.1c-0.2,0.9-1.1,0.9-1.4,1.7c-0.7,1.1,0.5,3.2-0.2,4.4c-0.1,0.9-0.4,0.7-0.8,1.2c-1.2,1.4-2.5,1.9-4.7,1c-0.7-0.3-1-1.1-1.6-1.6c-0.8-0.8-2.5-2.1-3.6-2.5c-1.9-0.7-3.7,0.2-5.3,0.4c-1.5,0.1-3.9,0.1-4.9-0.2c-0.3-0.1-0.8,0.1-1,0.1c-0.5,0-1.5-0.1-2.2-0.1c-0.8,0-1.7-0.7-2.9-0.2c-0.3,0.1-0.6,0.6-0.8,0.7c-0.6,0.4-1.8,1-2.5,1.1c-0.8,0.1-2.2-0.6-2.9-0.7c-0.7-0.1-3.4,0.5-4.2,0.7c-0.1,0.3-0.2,0.6-0.2,0.8c0,0.4,0.5,1.1,0.5,1.6c-0.1,0.8-1.5,2.9-2,3.4c0,0.4-0.1,0.7-0.1,1.1c0.1,0.3,0.6,0.8,0.4,1.6c-0.1,0.4-0.6,1-0.4,1.8c0.1,0.5,0.6,1.4,0.5,1.8c-0.2,0.9-1.1,1.2-1.7,1.7c-1.5,1.3-3,3.6-4.8,4.6c0.1,0.8,0.3,0.9,0.6,1.3c0.4,0,0.9,0,1.1,0.1c1.3,0,1.3,0.9,2.2,1.3c0,0.6-0.1,2.4-0.1,2.4v0.2c0.7,0,1.2-0.3,1.6-0.7c1,0,1.1,0.5,1.9,0.7c0.2-0.2,0.7-0.2,1-0.5c0.4-0.4,0.5-1.2,0.8-1.7c0.3-0.2,1.7-0.6,2-0.7c0.4,0,0.8,0.1,1.2,0.1c0.5-0.2,1.4-0.7,1.9-0.8c0.1-0.6,0-2.8,0.4-3.4c0.2-0.3,0.6-0.7,1.2-0.6c0.5,0.1,1.6,0.7,2,1.1c0.2,0.1,0.2,0.5,0.4,0.6h0.5c0.2,0.1,0.4,0.4,0.6,0.5h1c0.2,0.1,0.5,0.6,0.7,0.7c0.2,0.1,0.6,0.1,0.7,0.1c0.1,0.4,0.4,0.8,0.2,1.3c-0.1,0.3-0.5,0.4-0.4,1c0.1,0.7,0.7,2,0.2,3c-0.5,1.1-1.8,2.3-2.2,3.2c-0.4,0.9,0,2.2-0.5,3c0.1,0.4,0.1,0.3,0.2,0.6c2.1,0,2.3,0.2,3.5,1c0,1.4-0.6,2.1-1.4,2.6c0,1.1-0.7,2.4-0.4,3.4c0.2,0.6,0.9,1.4,1.1,2c0,0.4-0.1,0.8-0.1,1.2c0.2,0.6,1,1.2,1.3,1.8c0.3,0.1,0.3,0.1,0.6,0c1.1-0.1,0.8-0.6,1.4-1c0.5-0.3,1,0,1.4-0.2c0.5-0.2,0.8-1,1.2-1.3h0.5c0.3-0.1,0.6-0.6,0.8-0.7h1.1c0.5-0.2,0.8-0.6,1.2-0.8c0.1-1.5,1.2-3,0.1-4.2c0-1.3,0.4-0.9,1.2-1.3c0.2-0.2,0.5-0.4,0.7-0.6c0.2-0.9,0.1-2,0.1-3c0-0.3,0.1-0.6,0.1-0.8c-0.2-0.7-0.7-2.1-0.4-3.2c0.4-1.7,1.9-1.7,1.9-3.7h0.1v-0.1c2.8,0,1.8,1.6,2.6,3.4c0.5,1.1,1.9,1.6,2.3,2.8c0.1,1.6,0.2,3.3,0.2,4.9c-0.1,0.3-0.5,0.2-0.4,0.8c0.2,1,0.8,1.5,1,2.6c-0.5,0.5-2,2.1-2.2,2.8c-0.3,0.5-0.1,2,0.1,2.5c0,0.1,0.9,1.3,1,1.3c0.3,0.2,0.5,0.1,0.7,0.4c0.6,0.9,1.1,1.5,1.8,2.2c0,2.6-2.9,3.8-4.7,4.3c0.2,0.6,2.6,1.5,3.4,1c1.2-0.1,2-1.1,2.5-1.9c0.3,0,0.3,0,0.5-0.1c1.6,0,2.3,0.2,2.8,1.3c-0.1,0.5-0.4,1.8-0.2,2.2c0.3,0.7,1,0,1.7,0.4v0.2c0.1,0.1,0.5,0.2,0.5,0.2c0.6,0.3,2.4,0,2.6-0.2c1.3-0.1,2.3-0.2,2.6,0.8c0.6,0.9,0,3.1-0.7,3.7c0,1.2,0.6,1.2,0.8,2.2c0.4,0,0.9,0.1,1.3,0.1c0.6-0.6,1.1-1,1.4-1.8c0.3-0.6,0.1-1.3,0.5-1.8c0.2-0.3,0.7-0.4,1.1-0.6c0.5-0.3,0.6-1.1,1.1-1.4c0-1-0.7-3.2-0.5-4c0.2-0.5,0.8-0.7,1.1-1.1c0.2-0.4,0.1-0.8,0.2-1.3c0.5-1.3,1.6-2.6,2.3-3.8h0.4c0.2-0.9,1.3-1.5,2.2-1.7c0.4,0,0.9-0.1,1.3-0.1c0.1-0.2,0.2-0.3,0.4-0.5c0.3,0,0.6-0.1,0.8-0.1c0.6-0.3,1.2-0.9,1.8-1.2c0.3-0.2,0.5-0.1,0.8-0.2c0.7-0.5,0.7-1.9,1.4-2.4c0.4-0.3,0.9-0.2,1.3-0.5c0.2-0.2,0.3-0.6,0.5-0.7c0.5-0.5,1.3-0.5,1.9-0.8c0-0.1,0.1-0.2,0.1-0.4c0.1,0,0.2,0.1,0.4,0.1c0.1,0,1.1-0.8,1.2-0.8c0.6-0.8,0.6-2,1.1-2.9c0.5-0.9,2.9-4.4,3.7-4.8c0.6-0.3,1.1-0.2,1.6-0.5c0.8-0.5,1.5-1.4,2.5-1.8c1.8-0.6,2.2-0.1,3.2-1.7c0.3-0.5,1.2-0.7,1.3-1.2c0.6-1.1,2.7-1.3,4-1.8h1.3c0.2-0.3,0.4-0.6,0.6-0.8c0.6-0.5,1.8-0.5,2.9-0.5c0.4-0.9,0.9-2.2,1.4-3c0.6-0.8,1.7-1.3,2.3-2.2c0.2-0.3,0.2-0.7,0.5-1.1c0.3-0.5,0.9-0.6,1.3-1c0.4-0.3,0.7-1,1.2-1.3c0.7-0.5,2.4-0.6,3-1.2c0.2-0.2,0.2-0.9,0.5-1.1c0.4-0.3,0.9-0.2,1.2-0.7c0.3-0.6,0.1-1.4,0.5-1.9c0.2-0.3,1.9-0.9,2.3-1.2c0.3-1.6,0.7-3,2-3.5c0.1-0.1,0.2-0.1,0.5-0.1c0-0.6,0.1-1.4,0.4-1.8c0.1-0.8,2.1-2.4,2.6-3.2c1.1,0,2.5,0.8,3.7,0.4c0.2-0.1,1.1-0.4,1.4-0.6c0.2-0.1,0.5-0.6,0.7-0.7c0.7-0.3,1.3-0.2,1.9-0.5c0.3-0.2,0.6-0.6,1-0.8c0-0.4,0-0.7-0.1-0.8c-0.3-0.2-0.4,0.1-0.8,0c-0.2-0.1-0.4-0.4-0.6-0.5c-0.9-0.3-1.7-0.3-2.4-0.8c-0.2-0.3-0.5-0.6-0.7-1c0.1-1,0.8-1.3,1.2-1.9c0.3-0.5,0.1-1.3,0.4-1.8c0.1-0.2,0.6-0.5,0.7-0.7c0.2-0.4,0.2-0.8,0.5-1.2c0.5-0.8,1.8-1.3,2.5-1.9c0.2-0.3,0.3-0.6,0.5-0.8c0.3-0.2,0.7-0.3,1-0.6c0.8-1,1.1-2.5,1.9-3.5c0.9-1,2.5-1.6,3.2-2.8c0.4-0.6,0.4-1.2,1-1.6c0.4-0.3,0.8-0.1,1.3-0.4c0.1-0.2,0.2-0.3,0.4-0.5h1.2c0.3-0.1,0.7-0.7,1-0.8c0.6-0.3,1.2-0.1,1.9-0.4c0.5-0.2,1.4-0.7,1.8-1.1c0.2-0.4-0.1-1.2,0.1-1.7c0.2-0.5,1-1.9,1.3-2.3c0.6-0.8,1.8-0.6,2.6-1.1c0.3-0.4,0.6-0.9,0.8-1.3c0.8-0.8,2-1.2,2.6-2.2l0.1-1.3c0.1-0.2,0.4-0.5,0.5-0.7v-0.8c0.1-0.4,0.5-0.9,0.6-1.2c0-0.3,0.1-0.6,0.1-1c0-0.1,0.3-0.3,0.4-0.4C290.3,175.5,289.6,174.1,289.9,173z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Freedomain" "The Shire")} [:path {:className "st5" :d "M211.7,138.7c-0.4-0.7-3-1-4.2-1.4c-1.2-0.4-2.1-1.3-3.5-1.4c-1.2-0.1-2.5,0.5-3.5,0.4c-0.6-0.1-2.8-0.5-4-0.1c-0.4,0.1-2.1,0.4-2.4,0.6c-0.3,0.2-0.7,0.6-1.1,0.7c-0.8,0.3-1.5,0.2-2.3,0.5c-0.5,0.2-0.9,0.7-1.7,0.7c-0.7,0-1.6-0.4-2.2-0.6c-0.7-0.2-1.1,0.2-1.9,0c-0.9-0.3-3.1-0.9-4.6-0.5c-0.3,0.1-0.6,0.4-0.8,0.5c-0.9,0.3-1.5-0.3-2.4,0c-0.6,0.2-2,0.5-2.5,0.8c-0.6,0.4-1,1.1-1.7,1.4c-1,0.5-2.3,0.4-3.4,1c-0.5,0.2-1,0.9-1.4,1.1h-1c-0.9,0.3-2.4,0.6-3.2,1.1c-0.7,1.7-0.3,3.7-0.2,5.4c-0.3,0.3-0.5,0.9-0.8,1.2c-0.6,0.5-1.5,0.4-2.2,0.8v0.2c-0.3,0.2-0.6,0.3-0.8,0.5c-0.1,0.1-0.1,0.4-0.2,0.5h-0.4c-0.5,0.4-0.5,1.4-1.1,1.8c0.3,1,1.9,1,2.6,1.6c0.2,0.3,0.3,0.6,0.5,1c0.3,0.4,1.4,1.6,1.8,1.9c0.7,0.7,1.9,0.7,2.8,1.2c0.3,0.2,0.3,0.6,0.6,0.8c0.5,0.4,1.2,0.3,1.7,0.7c0.2,0.2,0.2,0.6,0.5,0.7c0.4,0.3,0.9,0.1,1.3,0.4c0.3,0.2,2.7,2.7,2.9,3c0.1,0.4,0.2,0.9,0.2,1.3c0,0,0.7,1,0.8,1.1c0.4,0.5,1.1,0.5,1.8,0.8c0.6,0.3,1.2,0.9,1.7,1.3c0.4,0.4,0.7,0.8,1.1,1.2v0.8c0.6,0.5,1.1,1,1.7,1.4c0.3,0.4,0.7,1,0.8,1.4c0.1,0.4,0,0.7,0.1,1c0.3,0.5,1.1,0.6,1.6,1c0.4,0.3,0.4,0.7,0.7,1.1c0.3,0.4,1.2,1,1.7,1.2c0.9,0.4,1.8-0.1,2.6,0.1c0.9,0.2,2,0.6,3.2,0.4c0.4-0.1,1.2-0.2,1.8-0.1c1,0.2,2.7,0.9,4,0.5c0.9-0.3,0.9-0.8,1.4-1.2c0.3-0.2,1.2-0.2,1.6-0.5c1.5-0.9,2.6-1.5,4.8-1.8c-0.1-0.7-0.7-1.5-0.4-2.5c0.5-1.4,1.6-2.1,2-3.5c0.5-1.4,0.1-3.9,0.4-5.3c0-0.5-0.1-1-0.1-1.6c0.3-0.7,1.4-1.1,1.6-1.9c0.1-0.6-0.2-1.4,0.2-1.9c0.3-0.4,0.7-0.3,1-0.8c0.6-0.9-0.4-5.3-0.1-6.6c0.1-0.5,0.1-1.5,0.4-2c0.1-0.3,0.7-0.6,0.8-1c0-0.7,0.1-1.4,0.1-2c0.1-0.3,0.7-0.6,0.8-0.8v-1c0-0.2,0.3-0.7,0.4-0.8c0.1-0.6-0.5-0.9-0.5-1.6C211.2,141.7,212.6,139.7,211.7,138.7z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Forochel")} [:path {:className "st3" :d "M161.6,24.1c-0.3,0.1-0.8,0.6-1.1,0.7c-1,0.4-2.2-0.2-3,0.2c-0.5,0.3-0.9,0.7-1.4,1c-0.7,1.8-0.2,4.3-1.2,6c-0.6,1-5.4,3.3-7,3.4c-0.1,0.9-1.2,1.2-1.7,1.8c-0.5,0.6-1.5,1.7-2.2,2c-0.6,0.3-1,0.1-1.6,0.4c-0.1,0.1-0.2,0.4-0.2,0.5c-0.4,0.3-1.7,0-2.3,0.2c-0.6,0.2-0.4,0.5-1.6,0.5c-0.4,1.7-0.5,4.6,0.1,6.5c0,0.5,0.1,1,0.1,1.6c0.6,1.2,1.9,2.2,2.3,3.6V54c0,2.4-0.5,3.8-1,5.5c-0.4,1.4,0.2,2.5,0.5,3.5v5.2c-0.3,1.1-0.9,2.4-1.3,3.5c-0.3,0.8,0.3,1.5,0.4,1.9v1.1c0.1,0.3,0.4,0.6,0.5,0.8c0.3,0.9,0,1.6,0.1,2.3c0.1,0,1.6-1,1.7-1.1c0.3-0.4,0.2-1,0.6-1.2c0.7-0.5,1.6-0.5,2.4-1c0.3-0.2,2.7-2.5,2.9-2.8c0-0.4,0.1-0.8,0.1-1.2c0.4-1.2,0.8-2.6,1.8-3.2c1.1-0.8,2.8,0.2,4.2-0.4c0.4-0.2,1.4-0.6,1.7-1c0.4-0.5,0.7-1.3,1.3-1.6c0.2-0.1,3.1-0.8,3.2-0.7c0.4,0.1,1.8,0.4,2,0.4c1-0.2,1.9-0.9,2.8-1.2c0.2-0.1,2.2-0.2,2.2-0.2c0.1,0,0.2,0.3,0.4,0.2c0.2,0,0.5-0.5,0.7-0.6h0.7c0.3-0.1,0.7-0.3,1-0.5c0.2-0.1,0.5-0.6,0.7-0.7c1.3-0.7,2.9-0.1,4,0.2h1.6c0.4,0.1,0.9,0.6,1.2,0.7c0.5,0.2,0.9,0,1.2,0.4c2.8,0,2.1-0.7,3.4-2c0.6-0.7,1.7-0.9,2.3-1.6c0.4-0.4,0.3-1.2,0.6-1.7c0.4-0.6,1.3-0.8,1.9-1.2c1.2-0.9,2.3-2.2,3-3.6c0.8,0,1.3,0.5,2.2,0.2c1.4-0.4,2.9-0.8,3.7-1.8c1.7,0,1.6,0.4,2.3,1.2c1.2,0.9,4.6,1.2,5.8,0.1c1.9,0.3,3.8,2.3,4.9,3.5c0,0.8-0.1,1.4-0.5,1.8c0,0.9,0.4,0.9,0.7,1.4h0.4c0.1,0.4,0.2,1,0.4,1.3c0.3,0.5,3.1,2.4,3.8,2.5c0.9-1.2,1-3.2,2.5-3.8c0.7-0.3,1.3-0.1,2-0.4c0.9-0.3,1.7-1.2,2.6-1.4c0.9-0.3,1.6-0.1,2.5-0.4c0.5-0.1,1.2-0.6,1.8-0.7c0.8-0.2,1.4,0,2-0.4c0.6-0.3,0.9-1.1,1.4-1.4c0.3-0.2,1-0.4,1.2-0.5h1.2c1-0.3,2.7-0.5,3.6-1.1c0.2-0.2,0.2-0.5,0.4-0.7h0.2c0-0.2,0.1-0.3,0.1-0.5c0.1-0.1,0.5-0.2,0.6-0.4c0.3-0.3,0.2-0.8,0.5-1.2c0.3-0.4,0.7-0.9,1.1-1.3c1.1-1.5,3.4-1.9,4.4-3.5c0.6-0.9,0.7-2,1.2-2.9c0.4-0.6,1.1-1.5,1.4-2.2c0.2-0.5,0-0.8,0.2-1.1c-0.1-0.5-0.4-0.7-0.7-1c0-0.8,0.1-1.2,0.5-1.6c0.6-1.2,1.4-0.4,2.5-0.8c0.4-0.1,0.6-0.6,0.8-0.8c0-1.2-0.2-2.7-0.8-3.5c-0.4-0.6-1.4-0.7-2-1.1v-1.2c0.3-0.4,0.6-0.7,0.8-1.1c0.8-0.6,2.2-0.6,3.1-1c1.6-0.7,3-1.9,4.8-2.5v-0.1h0.1v-0.1H161.6z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Arthedain")} [:path {:className "st3" :d "M266.6,145.9c-0.1-0.3-0.2-0.6-0.4-1c-0.8-1.2-2.4-1.8-3-3.1c-0.3-0.7-0.6-1.6-0.5-2.6c0-0.2,0.2-0.6,0.1-1c-0.1-0.2-0.4-0.5-0.5-0.7c-0.5-1.6-0.6-3.1-1.1-4.7v-1.6c-0.3-1.6-0.2-3.4-0.5-4.6c-0.2-1.1,0.2-2-0.4-2.6c-0.3-0.7-1.6-0.6-2.2-1.1c-0.6-0.6-0.8-1.4-1.4-1.9c-0.8-0.6-2-1-2.5-1.8c-0.9-1.2-2.3-3.8-2.8-5.3c-0.2-0.6,0-1-0.2-1.6c-0.2-0.4-0.6-1.2-1-1.6c-0.4-0.4-1.2-0.6-1.6-1.1v-0.6c-0.1-0.1-0.4-0.3-0.5-0.5c-0.2-0.5-0.2-1.1-0.5-1.6c-0.6-0.9-1.5-1.7-1.9-2.8c-0.1-0.6-0.2-1.3-0.4-1.9c-0.4-0.7-1.5-0.7-1.9-1.3c-0.6-0.7-1.1-1.7-1.6-2.4c-0.3-0.5-0.3-1.1-0.7-1.6c-0.7-0.8-1.5-0.8-1.8-2.2c0-0.3,0.1-0.6,0.1-0.8c-0.4-2,0.2-3.5,1-4.7c0.3-0.2,0.6-0.5,1-0.7c0.4-0.5,0.1-1.4,0.5-2c0.4-0.7,1.3-1.1,1.7-1.8v-0.5c0.3-0.5,0.9-1,1.1-1.6c0.3-0.8,0.1-1.2,0.5-1.8c0.1-0.2,1.1-1.5,1.3-1.7v-0.1c0.2-0.1,0.4,0,0.5-0.1c0.4-0.4,0.5-1,1-1.3c0.3-0.2,0.9-0.5,1.1-0.8c0.2-0.3,0-0.7,0.2-1c0.2-0.2,0.4-0.3,0.6-0.5v-0.5c0.2-0.4,0.9-0.4,1.2-0.7c0.2-0.3,0.2-0.7,0.5-1c0.9-0.9,2.1-1.6,2.9-2.6c0.4-0.5,0.9-1.4,1.1-1.9c0.1-0.4-0.1-0.7,0.1-1c0.3-0.4,1-0.6,1.3-1c0.5-0.4,0.7-0.9,1.1-1.4c0.2-0.3,0.4-0.2,0.7-0.4c0.2-0.2,0.7-0.7,0.8-1c0.6-0.8,0.1-2,0.7-2.8c0-0.2,0-0.2-0.1-0.4c-1.5-1.2-4.7-1-6.7-1.7c-1.3-0.4-2.9-0.9-4.1-1.6c-0.9-0.5-1.4-1.6-2.5-2c-1.4-0.5-3.1-0.6-4.2-1.3c-0.7-0.5-1.4-1.5-2.2-1.8c-0.8-0.3-1.6,0-2.3-0.2c-0.4-0.2-0.7-0.9-1.3-0.7c-0.7,0.2-1.6,2.1-2.5,2.5c-1.4,0.7-3.5,0.4-4.9,1c-1,0.4-1.5,1.7-2.5,2.2c-0.7,0.3-1.7-0.1-2.6,0.2c-0.4,0.2-1.2,0.6-1.7,0.7l-1.6,0.1c-0.7,0.2-1.2,0.9-1.9,1.2c-1,0.4-2.5,0.3-3.2,1c-0.6,0.5-1.1,2.9-1.4,3.7c-2.7,0-2.9-0.7-4.4-1.6c-0.9-0.5-1.6-0.5-2.2-1.3c-0.3-0.5-0.2-0.9-0.5-1.4c-0.3-0.5-0.9-0.6-1.1-1.2c-0.3-0.5-0.2-2.1,0.1-2.4c-0.1-0.7-2.7-2.8-4-2.4c-0.5,0.2-1.5,1.1-2.4,0.7c0-0.1-0.1-0.2-0.1-0.2h-0.6c-0.2-0.1-0.7-0.4-0.8-0.5c-0.6-0.2-1.2,0-1.7-0.4c-0.3-0.2-0.8-0.9-1.2-1c-0.6,0-1,0.8-1.4,1c-0.5,0.2-3.2,0.6-3.2,0.6c-1.2,0.1-1.9,1.3-2.5,2c-0.2,0.2-1.3,1.2-1.4,1.3c-0.4,0.2-1,0.1-1.2,0.5c-0.1,0.4-0.2,0.7-0.2,1.1c-0.5,1.2-1.6,1.6-2.6,2.3c-0.1,0.4-0.2,0.5-0.2,1.1c-0.5,0.1-1.7,1.2-1.9,1.6c-1.7,0-3.5,0-4.7-0.6c-0.1-0.2-0.2-0.3-0.4-0.5c-0.5-0.2-1.1,0.1-1.4,0c-0.7-0.2-1.7-1.1-2.8-0.7c-1.2,0.4-2.2,1.5-3.4,1.9c-0.5,0-1,0.1-1.4,0.1c-0.7,0.2-1.5,0.7-2.2,1c-1.8,0.7-4.8-0.2-6.6,0.5c-1.3,0.4-1.9,1.9-3.1,2.4c-0.6,0.3-1,0-1.7,0.2c0,0.1-0.1,0.2-0.1,0.2c-0.6,0.2-1.8-0.1-2,0c-1.9,0.1-1.3,2.3-1.9,3.7c-0.3,0.6-1.2,1.4-1.7,1.9c-0.2,0.2-0.3,0.1-0.6,0.2c-0.4,0.3-0.5,0.9-0.8,1.2c-0.4,0.1-0.7,0.2-1.1,0.4c0,0.1-0.1,0.2-0.1,0.4c-0.5,0.2-1-0.1-1.3,0.2c-0.3,0.2-0.4,0.7-0.6,1c-0.3,0.4-1,0.5-1.3,1c-0.3,0.4-0.2,0.8-0.5,1.2c-0.4,0.6-1.2,1.3-1.4,1.9v3.1c-0.2,0.6-0.6,1.1-0.8,1.6c-0.3,0.8,0.2,1.7,0,2.4c-0.1,0.5-0.5,1.6-0.4,2c0.3,1,1.1,2,1.4,3.1c0.3,1.1-0.5,2-0.1,3c0.3,0.8,1.1,0.9,1.6,1.6c0.8,1.3,0.6,3.6,0.2,5.2c0.5,0.5,0.8,1.2,1.3,1.7c0.4,0.4,1.4,0.7,1.7,1.1c0.3,0.4,0.2,1.2,0.5,1.6c0.6,0.8,1.9,1,2.8,1.6c0.6,0.4,0.6,1.6,1,2.2c0.4,0.6,1.1,0.3,1.8,0.6c0.4,0.4,0.8,0.7,1.2,1.1c-0.1,0.5-0.5,0.9-0.4,1.4c0.1,0.2,0.5,0.6,0.6,0.7c0.3,0.6,0,1.1,0.2,1.7c0.2,0.4,0.9,0.8,1.1,1.2v0.6c0.2,0.2,0.4,0.3,0.6,0.5c0.9,1.2,2,2.5,2.4,4c0.3,0.9-0.2,1.7-0.4,2.2v1.6c-0.1,0.3-0.7,0.6-0.8,1c-0.1,1-0.2,2.1-0.2,3.1c-0.3,0.8-1,1.1-0.8,2c0.1,0.9-0.9,1.6-0.4,2c0,1.3-0.6,0.8-1.2,1.4c-0.2,0.2-0.1,0.8-0.2,1c-0.1,0.1-2.2,0.8-2.5,0.8c0.2,0.3,0.3,0.2,0.4,0.6c-1,0.1-1.5,0.6-2.3,0.8c-0.8,0.2-1.2,0-2,0.2c-0.8,0.2-1.7,0.9-2.4,1.2c-0.5,0.3-1.4,0.4-1.9,0.6h-0.8c-0.4,0.2-1,1.5-1.4,1.8c-0.4,0.3-1,0.4-1.3,0.7c-0.8,0.8-1.3,2.7-1.9,3.6c-0.4,0.4-0.9,0.7-1.3,1.1c-0.4,0.5-1,2-1.2,2.5c-0.2,0.5,0,0.9-0.2,1.3c-0.2,0.4-0.9,0.8-1.1,1.1c-0.3,0.5,0,1.2,0,1.8c0,0.7-0.1,1.4-0.1,2c0.1,0.6,0.5,1.2,0.4,2c-0.1,0.8,0,2.1-0.4,3c-0.3,0.8-1.1,1.5-1.3,2.3c-0.5,1.7,0,3.6-0.8,4.9c-0.4,0.6-1.4,1-1.8,1.7c0,0.3-0.1,0.6-0.1,0.8c-0.2,0.5-0.9,1.4-1.3,1.7c-0.6,0.4-2.3,0.6-2.8,1.1c-0.4,0.4-0.5,1.2-1,1.6c-0.3,0.3-0.6,0-1,0.2c-0.5,0.2-1.2,1.9-1.1,2.3c0,2.7,2.6,0.9,4.4,1.6c0.7,0.2,1.4,0.9,2,1.1c0.8,0.3,1.3-0.1,2,0.2c1,0.4,1.9,1.9,2.4,2.9c0.1,0.3,0,0.7,0.2,1c0.8,0.9,3.4,0.3,4.7,1.3c0.1,0.1,0.3,0.4,0.5,0.5h0.5c0.2,0.1,0.2,0.6,0.4,0.7c0.4,0.4,0.9,0.3,1.4,0.6c0.2,0.1,2.6,2.5,2.8,2.8c0.6,0.9,0.2,1.7,0.5,2.8c0.6,2.4,0.8,3.9-0.8,5.4c-0.5,0.4-0.5,1.1-1.1,1.4c0.2,1.1,1.5,0.9,2.2,1.4c0,0.1,0.1,0.2,0.1,0.4h0.5c1.2,0.7,0.7,2,1.2,3.2c0.1,0.3,0.5,0.4,0.7,0.6c0,0.2,0.1,0.4,0.1,0.6c0.2,0.3,0.7,0.3,1.1,0.5c0.2,0.3,0.4,0.6,0.6,0.8c-0.1,2.4-4.7,2.8-7.1,3c-0.1,0.4-0.1,1.2-0.4,1.6c-0.3,0.5-0.7,0.2-1.2,0.5c-0.2,0.3-0.5,0.6-0.7,0.8c-0.5,0.4-1.3,0.6-1.8,0.8c-0.4,2.3-1.4,3.6-3.6,4.1c0.1,0.6,0.4,1.1,0.6,1.6c0.6,0.1,2.2,0.4,2.6,0.1c2.4-0.1,2.6,2.8,1.3,4.1c0.2,0.9,1.1,1.1,1.4,1.8c0.2,0.5,0.8,2.4,0.7,3c-0.1,0.4-0.4,0.9-0.5,1.3c-0.1,0.6,0.2,1.3,0.1,1.9c1.1-0.2,2.1-1.6,2.8-2.3c-0.1-0.7-0.6-1.6-0.4-2.5c0.1-0.4,0.2-0.8,0.4-1.2c-0.1-0.6-0.5-1.4-0.2-2.2c0.4-1.1,1.4-1.9,1.8-2.9c0.4-1-0.3-1.7-0.4-2.6c1.2-1.2,1.1-1.7,3.1-2.3h2.2c0.2-0.1,0.4-0.4,0.8-0.2c0.6,0.2,1.7,1,2.6,0.7c1.6-0.4,2.3-1.4,4.1-1.8c1-0.2,1.9,0.2,2.4,0.4c1.2,0.4,2.6-0.2,3.7,0c0.9,0.2,2.5,0.6,3.5,0.4c1.6-0.3,4.2-1.1,6.2-0.5c1.6,0.5,3.3,2,4.6,3c0.5,0.4,0.5,1,1.1,1.2c0.4,0.3,1.5,0.4,2.2,0.4c0.3-0.3,0.8-0.8,1.2-1.1c0.1-1-0.3-4.6,0.4-5.4c0.2-0.4,0.9-0.9,1.3-1.1c-0.2-1.3-1.5-2.5-0.8-4.3c0.2-0.4,0.6-0.7,0.8-1.2c1.5-0.6,2.8-0.6,4.6-0.6c0-1.5,0.9-1.4,1.9-1.9c0.5-0.3,1.1-0.8,1.7-1c0.7-0.2,1.2,0.2,1.9,0c0.5-0.1,1-0.6,1.4-0.7h1.3c0.4-0.1,0.7-0.3,1.2-0.4c0-1.4-0.5-4.2-0.1-5.3c0.4-1,3.4-3.6,4.4-4c0.6,0,1.3-0.1,1.9-0.1c1.3-0.3,2.2-0.6,3.1-1h0.7c0.8-0.4,1.2-1.6,1.8-2.2c-0.2-0.7-0.4-0.3-1-0.6c-0.3-0.2-0.3-0.7-0.6-1c-0.3-0.3-0.8-0.3-1.1-0.6c-0.4-0.5-0.2-1.1-0.5-1.8c-0.1-0.2-0.5-0.3-0.6-0.6c-0.2-0.6,0.3-0.7,0.2-1.1c0-0.2-0.4-0.5-0.5-0.7c0-0.4,0.1-0.8,0.1-1.2c-0.2-0.7-0.8-1.4-1-2.3c-0.3-1,0-2.9,0.5-3.4V178c-1.8,0-2.1,0.9-3.2,1.4c-1,0.5-1.9,0.5-2.4,1.6c-1,0.1-2.7,0.4-3.8,0.1c-0.7-0.2-1.7-0.6-2.6-0.5c-0.7,0.1-1.9,0.6-2.8,0.4c-0.3-0.1-0.9-0.4-1.2-0.5c-0.9-0.2-2.3,0.1-3.2-0.2c-0.2-0.1-1.6-0.9-1.8-1.1c-0.5-0.4-0.7-1.2-1.2-1.6c-0.5-0.3-1.2-0.2-1.6-0.7c-0.6-0.8-0.5-2-1.1-2.9c-0.4-0.6-1.6-1-1.9-1.7c-0.2-0.4,0-0.7-0.2-1.1c-1-1.4-3.3-1.8-4.2-3c0-0.2-0.1-0.3-0.1-0.5c-0.2-0.2-0.4-0.3-0.6-0.5c-0.1-0.4-0.2-0.9-0.4-1.3c-0.4-0.6-1.1-1-1.6-1.6c-0.2-0.2-0.3-0.7-0.5-0.8H169c-0.5-0.2-1.4-0.5-1.9-0.8c-0.8-0.5-1.4-1.5-2.2-2c-0.6-0.4-1.8-0.6-2.5-1.1c-0.1-0.2-0.2-0.3-0.2-0.5c-0.3-0.2-0.6-0.3-0.8-0.5c-0.2-0.3-0.2-0.6-0.5-0.8c-0.2-0.2-0.6-0.2-0.7-0.4c-0.2-0.3-0.2-0.7-0.5-1c-0.4-0.3-1-0.2-1.4-0.5c-0.6-0.3-1-0.9-1.6-1.3c0-1.4,0.3-1.1,0.7-1.8c0.4-0.7,0.5-1.4,1-2c0.8-1,2.7-1.6,3.8-2.2c0-1.6-0.3-4.9,0.4-5.9c1.1-1.8,3.2-1.2,5.2-2c0.4-0.2,0.9-0.8,1.3-1c1.2-0.5,2.2-0.3,3.4-0.8c0.6-0.3,1-1.3,1.6-1.6c0.9-0.4,2.1-0.6,3-0.8h2.2c0.8-0.2,1.5-0.4,2.4-0.6c1.4-0.2,3.2,0.4,4.3,0.6h1.7c0.4,0.1,1.3,0.6,1.9,0.5c0.3-0.1,0.6-0.5,1-0.6c0.4,0,0.8-0.1,1.2-0.1c1.5-0.5,3.1-1.1,4.7-1.7c1.6,0,3.3-0.1,4.9-0.1l0.7,0.1c0.1,0,0.4-0.3,0.5-0.4c0.4-0.1,0.8,0.2,1.1,0.2c0.7,0.1,0.8-0.4,1.4-0.2c1.5,0.5,3,1.5,4.6,2c0.9,0.3,1.8,0.1,2.6,0.5c0.5,0.3,0.9,1.1,1.3,1.4c0.1,1.6-0.2,2.7-0.5,4.1c-0.1,0.5,0.2,0.8,0.2,1.1c0.2,0.8-0.2,1.3-0.4,1.7c0,0.6,0.1,1.1,0.1,1.7c-0.1,0.4-0.7,0.6-0.8,1v1.4c-0.1,0.4-0.6,0.6-0.7,1c-0.3,0.7-0.1,1.8-0.5,2.5c0,0.7,0,1.2,0.2,1.6c2.7,0,5.9-0.1,7.3,1.4c0.1,0.3,0.2,0.6,0.2,0.8h0.1c0.1,0.2,1.6,2,1.6,2h0.5c0.4,0.3,0.4,1.1,0.7,1.4c0.4,0.4,0.9,0.1,1.3,0.2c0.5,0.2,0.4,0.7,1.1,0.8c0.6,2.3,1.4,2,3.2,3.1c0.4,0.2,0.7,0.8,1.1,1h1.4c0.2,0.1,0.6,0.4,0.8,0.5c0.5,0.1,2.1-0.1,2.4-0.4c0.3-0.1,0.2-0.1,0.5-0.2c0.1-1-0.2-2.7,0.4-3.5c0.4-0.9,3.1-2.1,4.7-1.6c0.5,0.2,1,0.9,1.6,1.1h1.8c0.4,0.1,1.1,0.6,1.3,0.5h0.6c0-0.1,0.1-0.2,0.1-0.4c-0.9-0.8-1.2-1.5-1.2-3c0.1-0.1,0.2-0.2,0.2-0.4c0.4-0.2,1.5-0.3,1.8-0.6c1.5-0.1,0.9-0.9,1.6-1.7c0.4-0.5,0.8-0.3,1.4-0.6c0.1-0.2,0.2-0.3,0.4-0.5c1.4-0.6,4.5,1.1,5.8,0.5c1.5-0.7,1.9-2.6,3.2-3.4c0.3-0.2,0.6-0.1,0.8-0.2v-0.2c0.1-0.1,0.5-0.2,0.6-0.2l2.2,0.1c0.3-0.1,1-0.6,1.3-0.7h2.2c0.4-0.2,0.6-0.8,1-1c0.3,0,0.6-0.1,0.8-0.1c1-0.5,2.3-1,3.8-1v-0.5C268.7,147.6,267.2,146.6,266.6,145.9z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Wilderness" "Númeriador")} [:path {:className "st3" :d "M154.4,126.6c0.1-0.3,0.4-2,0.4-2c0-0.2-0.5-1.3-0.4-1.4h0.2c-0.2-1.2-2.9-3.6-3.6-4.7c-0.7-1-0.4-3.1-1.1-4.2c-0.3-0.5-2.3-0.7-2.8-1.3c-0.3-0.4-0.4-1.4-0.7-1.7c-0.2,0-0.4-0.1-0.6-0.1c-0.4-0.2-0.4-0.8-0.7-1.1c-0.4-0.3-0.8-0.3-1.1-0.7c-0.3-0.5-0.3-1.2-0.6-1.7c-0.7-1.1-2.8-1.9-3.4-3.2c-0.4-1,0.5-2,0.2-2.8c0-1.6-0.4-1.2-0.8-2c-0.2-0.4-0.1-0.6-0.4-0.8c-0.2-0.2-0.6-0.4-0.7-0.6c-0.2-0.5-0.4-2.3-0.2-2.8c0.1-0.3,0.4-0.7,0.2-1.2c-0.1-0.2-0.5-0.5-0.6-0.7c-0.2-0.4-0.9-2.6-1-2.9c-0.1-0.5,0.3-0.5,0.4-0.7c0.1-0.2-0.1-0.6-0.1-0.6c0-0.3,0.1-1.9,0.2-2.4c0.2-0.8,0.7-1.6,0.8-2.3c0.2-0.8-0.1-2-0.2-2.4c0.2-0.8,0.3-1.5,0.5-2.3c0.4-1.4-0.3-5.2-0.6-6.1c-0.1-0.2-0.5-2.3-0.4-2.6c0.2-0.4,0.8-1.2,1-1.7c0.5-1.1,0.4-2.5,0.5-4c0-0.4,0.1-0.7,0.1-1.1c-0.3-1.4-1.1-3.2-0.6-5.2c0.3-1.3,0.8-2.5,1.1-4c0.5-2.3-0.9-3.8-1.7-4.8c-0.7-0.9-1.5-3.5-1.4-5.2c-0.7,0.3-0.8,1.1-1.3,1.6c-0.5,0.4-2.3,0.6-2.9,1.1c-0.7,0.5-1.6,1.8-2.4,2.2c-0.6,0.2-1,0-1.4,0.2c-1.6,0.1-0.7,0.9-1.3,1.7c-0.4,0.5-1.2,0.6-1.4,1.3c0,0-3,0.4-3,0.4c-0.1,0.4-0.2,0.8-0.4,1.2c-0.3,0.4-2.7,1.7-3.5,0.8h-0.2c0.1,0.1,1.5,1.1,1.7,1.2c0,0.7,0.5,1,0.4,1.7c-0.2,1.2-1,2.2-1.3,3.2c-1.1,0.2-1.9-0.2-2.8-0.5c-1.4,1.5-4.2,1.1-6.4,1.6c0.1,0.2,0.3,0.3,0.5,0.5c0.1,0.2,0.3-0.3,0.8-0.1c0.6,0.2,1.6,0.5,2.2,0.7c0.2,1.5,1.4,4.6,0.1,5.5c0.1,0.3,1.2,1.5,1.4,1.8c0,0.8,0,1.4-0.4,1.8c-0.4,1.1-1.6,0.7-2.6,0.6v0.4c0.4,0.3,0.8,0.6,1.2,0.8v1.7c-0.5,0.5-0.8,1.2-1.4,1.6h-1.6c-0.4,0.1-1.4,0.4-1.9,0.5c0.3,0.5,0.9,0.6,1.1,1.2c0.3-0.2,1.2-1,1.9-0.6c0.2,0.1,0.4,0.5,0.6,0.6h1.2c0.8,0.2,2.7,0.8,1.8,1.9c-0.2,0.3-0.3,0.4-0.7,0.5c-0.1,0.2,0,0.1-0.1,0.2c0.4,0.3,1.3,0.2,1.7,0.6c0.8,0.8,0.8,1.9,1.1,3c-0.5,0.5-0.7,1.2-1.2,1.6c-0.9,0.6-2.4,1.1-3.5,0.5c-0.2-0.1-0.5-0.6-0.7-0.7c-0.5-0.2-0.8,0-1.2-0.2c-0.6-0.5-0.1-0.7-1.3-0.7c0.1,2.3,0.6,3.9-1.6,4.4c0,0.4,0.1,0.3,0,0.5c-0.3,0.5-2.9,0.9-3.5,1c0.1,0.2,0,0.1,0.1,0.2c0.5,0.3,1.4-0.1,2,0.1c0.3,0.1,0.9,0.4,1.2,0.6c0,0.6-0.1,0.8,0,1.3c-0.3,0.3-0.5,0.6-0.8,0.8c0.1,0.4,0.2,1,0,1.3V97c0.5-0.1,0.6-0.3,1-0.4c0.7-0.2,2.9,0.1,3.4,0.4c0.8,0.4,1.1,1.3,1.7,1.7h0.4c0.2,0.1,0.2,0.4,0.4,0.5c0.2,0.1,0.5,0.2,0.7,0.2c0.2,0.2,0.8,1.1,1.1,1.3h0.7c0.8,0.4,1.3,1.5,1.9,2c0,1.1-0.5,3.5-1.2,4c-0.2,0.1-0.6,0.1-0.8,0.2c0,0-0.2,0-0.1,0.1h0.1c0.1,0.3,0.8,0.9,1,1h0.7c0,0.2,0.1,0.4,0.1,0.6c0.2,0.1,0.4,0.2,0.6,0.2c0.3,0.3,0.2,0.9,0.5,1.2v1.7c-0.1,0.4-0.5,0.4-0.8,0.6c-0.5,0.4-1,1.2-1.6,1.4c-0.9,0.5-2.9,0.4-3.8,0.5v1.2c-0.5,0.2-0.5,0.6-1.1,0.7c0,0.3-0.1,0.2,0,0.4c-0.1,1.5,0.6,1.5,1.6,2c0.1,0.5,0.2,1.7,0.5,2c0.5,0.8,1.2-0.1,2.2,0.2c1,0.3,3.9,2.6,2.9,4.2c-0.4,1-2.5,1.3-3.5,1.3c0.3,0.3,0.7,0.8,1,1.1c0,0.8-0.1,1.2-0.6,1.6c0.1,0.2,0,0.1,0.1,0.2c0.7,0.5,1.3-0.4,2.2-0.2c1.3,0.3,2.3,1.1,3.2,1.7c0.1,0.5,0.4,1,0.1,1.4c-0.1,1.5-2.1,2-3.4,1.8c-0.1,0-0.5-0.2-0.8-0.1c-0.4,0.1-0.5,0.5-1.1,0.6c0,0.8,0.2,1.3,0.2,2.5c1.2,0.6,5.4,2.3,5.8,3.5c0.3,0.4-0.3,1.2-0.6,1.7c-1,0.2-3.6-0.3-4.3,0.2h-0.2c0.4,1.8,2.1,2,2.9,3.4c1.6,0,2.4,0,3.6-0.6c0.5-0.2,1.4-0.8,2.4-0.5c1.8,0.6,2.2,2.2,3.6,3.1c1.2,0.8,2.8,1.2,3.6,2.4c0.7-0.3,0.7-1.3,1.2-1.8c0.5-0.5,1.2-0.9,1.6-1.6c0.6-0.9,0.7-2.1,1.4-2.9c0.5-0.6,1.2-0.9,1.8-1.3c0.5-0.3,0.5-0.9,0.8-1.3c0.5-0.6,1-0.2,1.7-0.5c0.1,0,0.3-0.4,0.4-0.5h1c0.1-0.1,0.4-0.4,0.5-0.5h0.5c0.5-0.2,1.2-0.6,1.7-0.8c1-0.2,1.9-0.3,2.9-0.5c0.8-0.3,1.1-1.2,1.8-1.6c1.1-0.5,1.7-0.5,2.3-1.6c0.3-0.4,0.6-1.4,0.7-1.8v-1.8c0.1-0.4,0.7-0.8,0.8-1.1c0,0-0.2-0.6-0.1-0.8c0-0.3,0.3-0.5,0.4-0.7c0.1-0.4-0.2-1,0-1.6C153.8,127.2,154.3,126.9,154.4,126.6z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Freedomain" "Lindon")} [:path {:className "st5" :d "M137.2,228.6c-0.3-0.9-1.3-1.2-1.6-2.4c-0.2-1.1,1-1.7,0.7-2.4v-0.2c-1.8,0-3.1-0.1-4.3-0.6c0-0.2-0.1-0.4-0.1-0.6h-0.4v0.6c-0.3,0.2-0.4,0.3-0.6,0.6c-0.9,0.1-2.8,0.3-3.8-0.1c-0.3-0.1-0.3-0.4-0.7-0.5c0-0.4,0-1.4-0.2-1.7c-0.2-0.3-0.6-0.4-0.8-0.6c-1.6,0-1.4-0.1-2.3-0.7c0-0.3,0-0.4,0.1-0.6c0-0.3,0-0.5-0.1-0.7c-1.6,0.3-2,0.7-3.5-0.1c-0.2-0.6-0.1-1.3-0.4-2c-0.1-0.4-0.6-0.8-0.7-1.2c-0.5-1.5,0.2-3.5,1-4.2c-0.1-0.5-0.6-0.8-1-1.1c0-0.4,0.1-1.1-0.1-1.3c0.3-1.5,5.1-2.1,5.5-3.2c0.1-0.1,0,0,0.1-0.2h-0.1v-0.1c-2,0.2-6.5,2-8.2,0.8c-0.4-0.3-0.8-1-0.8-1.7c0-0.1,0.2-0.3,0.1-0.6c-0.2-0.4-0.6-0.1-1.1-0.2c-0.5-0.2-0.7-0.8-1.1-1h-2.5c0-0.2,0-0.2-0.1-0.4c-0.1-0.2,0-0.1-0.1-0.2c-0.4-0.3-1.5-0.6-2-0.7c-0.1-0.5-0.4-0.6-0.5-1c0-1.1-0.1-2.2-0.1-3.2c-0.1-0.3-0.5-0.7-0.6-1c0,0,0.2-1.8,0.2-1.9c0.2-0.7,1.1-1.2,1.2-1.9c0.1-0.8-0.8-1.2-1-1.8c-0.3-1.2,1.3-4,1.8-4.4c0.5-0.4,1.2-0.1,1.9-0.4c0.9-0.4,1.8-1.3,2.6-1.7h0.6c1.6-0.7,3.7-1.2,5.3-1.9c-0.1-3.6-0.2-4.7,2.4-6.1c0.7-0.4,1-1.4,1.7-1.8c0.8-0.4,2.2-0.6,2.8-1.3c0.4-0.5,0.4-1.3,0.7-1.8c0.6-0.5,1.1-1,1.7-1.6c0.4-0.6-0.1-0.9,0.6-1.2c-0.8-2.5,1.1-4.6,1.7-7.1c0.2-1-0.2-2-0.4-2.8c-0.5-2.5,0.3-4.9,1.3-5.9c0-0.3,0-0.6-0.1-0.7c-0.4-1-1.4-1-2.2-1.6c-0.2-0.2-0.3-0.6-0.6-0.7c-0.5-0.3-1-0.2-1.4-0.6c-0.7-0.7-1.3-2.1-2.3-2.4c-1.7-1.1-4.6,2.2-6.7,0.7c-1.6,0.1-0.7,1.1-1.6,1.7h-0.6c-1.6,0.9-2,1.3-4,1.3c0.3,0.6,1.6,1.1,2.3,1.3c0.1,0.3,0.1,1,0.2,1.3c0.1,0.2,0.5,0.2,0.7,0.4c0.7,0.6,1,1.5,1,2.9c-0.4,0.4-0.5,1-1,1.3c-0.4,0.3-1,0.4-1.3,0.8c-3,0-7.2-0.7-9.1,0.6c-0.2,0.1-2,1.8-2,1.9c-0.5,0.8,0.2,1.5-1.1,2c-1.3,0.6-3.4,0.1-4.6-0.2h-1.1c-0.7-0.2-1.8-0.3-2.5-0.6c-0.3-0.1-0.8-0.6-1.1-0.7c-0.6-0.3-1.2-0.2-1.8-0.5c-0.6-0.3-0.9-1.1-1.4-1.4c0-1.4-0.2-2.7,0.6-3.5c-0.4-0.3-0.7-0.6-1.1-0.8c0-0.9-0.1-1.4,0.5-1.7c-0.3-1.7-2.6-2.2-3.4-3.5c-0.2-0.2-0.2-0.7-0.2-1c-1.5,0.1-2.9,0.8-4.3,1.1c-0.3-0.3-0.8-0.5-1.1-0.7c0-2.5,1.1-2.4,2.8-3.2c0.7-0.3,1.2-1.2,1.9-1.6c0.4-0.2,0.9-0.1,1.2-0.4c1-1.3-0.6-3.2-0.1-4.7c0.3-0.9,1.7-1.8,2.5-2.2c0.3,0,0.6-0.1,1-0.1c0.1-0.1,0.5-0.4,0.6-0.5c0.6-0.2,1.6-0.3,2.2-0.5c0.4-0.1,0.8,0.2,1.1,0.1c0.4-0.1,1.1-0.4,1.7-0.2c0.9,0.2,1.8,0.8,2.6,1.1c0,0,2.5-0.8,2.8-1c0.2-0.1,0.3-0.4,0.5-0.6c0-0.2,0-0.2-0.1-0.4c-0.5-0.8-1.8-0.3-2.9-0.5c-1.1-0.2-3,0.2-4,0.2c-0.6,0-1.8,0.1-2.3-0.1c-0.4-0.1-0.4-0.3-0.7-0.5c-0.1-0.6-0.2-1.1-0.2-1.7c0.3-1.4,1.6-2.6,2.9-3c0.7-0.2,1,0,1.7-0.2c0.9-0.4,2.7-0.7,3.5,0.2c0.3-0.1,0.2,0,0.4-0.2h0.1v-0.1h-0.1c-0.4-0.7-2.4-0.8-3.1-1.3c-0.4-0.3-0.5-0.6-0.6-1.2c-0.8,0-1.7-0.1-2.5-0.1c-0.5-0.2-0.8-0.7-1.3-1c0-0.2,0-0.2,0.1-0.4c0-2.3,3-1.6,4.8-2.2c1-0.3,1.8-1.2,2.9-1.6v-0.2c-0.3-0.2-0.8-0.4-1.2-0.6c-0.4-0.9,0.1-2.4-0.5-3.2c-0.5-0.7-1.7-0.9-2.2-1.6c-0.4-0.6-0.3-1.5-0.6-2.3c0.3-0.3,1.7-1.8,1.8-2c0.1-0.3-0.3-0.6-0.1-1c0-0.1,0.9-1.1,1.1-1.2c0-0.4,0.1-0.5,0.1-0.8c-0.1-0.1-0.1-0.1-0.2-0.2H94c-0.5-0.2-0.9-0.6-1.4-0.7c0-0.3,0-0.3-0.1-0.5c0-0.1-0.1-0.2-0.1-0.4c-0.6,0-1.1,0-1.4,0.2h-3.2c-0.2-0.1-0.2-0.4-0.5-0.5c0-2,0.5-2.9,1.4-3.8c-0.1-0.2-1.3-0.6-1.7-1c-1.3-1.6-0.1-4,0.7-5c0.2-0.2,0.6-0.2,0.7-0.5c0.4-0.7,0.7-1.4,1.4-1.8c-0.2-0.4-2.6-0.7-3.1-1.1c-0.8-0.4-0.9-2.1-0.2-2.8c0-0.5-0.8-1.5-1.1-1.7c0.1-1.1,0.6-1.5,1.1-2.2c-0.4-0.1-0.8,0.1-1.2-0.2c-0.4-0.3-0.9-1.6-0.4-2.2c0.1-0.8,0.7-1,1.2-1.3v-0.4c-0.9-0.1-2.7-0.6-3.1-1.4c-0.5-0.9,0-1.2-1-1.9c0-1.6,0.1-2.9,0.6-4.1c0.2-0.6,0.9-0.6,1.2-1.1c0.5-0.7,0.6-1.5,1.1-2.2c0.3-0.4,0.7-0.5,1.1-0.8c0.5-0.4,0.8-1.2,1.3-1.6c1.2-0.9,3.3-2.1,5.2-2.2v-0.2c-0.5-0.1-1.5-0.1-1.8-0.4c-2.1,0.1-2-0.8-1.8-2.5c-0.2,0-0.2,0-0.4-0.1c-1.1-0.4-0.8-2.8-0.7-3.8c0.6-0.1,0.6-0.3,1.3-0.4c0.3-2.3-1.4-3.7-1.3-6.5c0.9-0.8,1.4-1,2.4-1.7c0.5-0.4,0.7-1,1.3-1.2c0.1-0.1,0.1-0.1,0.4-0.1c0-0.2,0-0.2,0.1-0.4c0.6-0.7,2.5-1.3,3.5-1.7h1.2c0.7-0.2,0.8-1.2,1.3-1.6c0.6-0.4,1.4-0.5,2-0.8c0.4-0.2,0.7-0.8,1.1-1.1c0.7-0.4,1.3,0,2-0.4c0.4-0.2,0.8-0.8,1.2-1.1c0.5-0.3,1.2-0.6,1.7-0.8c0.7-0.3,1.8-0.2,2.4-0.7c0.6-0.4,0.6-1.4,1.2-1.8c0.5-0.3,1.1-0.3,1.6-0.6c0.1-0.2,0.2-0.3,0.4-0.5c0.3-0.2,0.6-0.1,1.1-0.2c0.2-0.1,0.5-0.4,0.6-0.5h1l0.8-2.2c0.7-0.3,1.1-0.3,1.7-0.6c-0.3-2.1,0.8-2,2-2.9c0.5-0.4,0.9-0.8,1.4-1.1v-0.4c-1.2,0-1.7,0.4-2.5,0.7c-1.5,0.6-3.3-0.8-5-0.2c-1.5,0.5-3.3,2.9-4.7,3.8c-1,0.7-2.7,0.6-3.6,1.4c-0.2,0.2-0.1,0.5-0.2,0.8c-0.1,0.2-1,0.8-1.2,0.8c-1,0.4-2.2-0.2-3.1,0.4H98c-0.2,0.1-0.2,0.6-0.4,0.8c-0.3,0.3-0.8,0.5-1.1,0.8c-0.8,1.2-0.9,2.8-2,3.6c-0.5,0.4-1.1,0.6-1.7,1c0,0.1-0.1,0.2-0.1,0.4h-1.4c-0.1,0-0.1,0.4-0.2,0.5c-0.4,0.2-0.7,0.2-1.1,0.5c-1.3,0-1.1-1-1.9-1.4c-0.5-0.4-3.4,1.5-3.8,1.8c-0.7,0.2-1.4,0.5-2.2,0.7c-0.7,0.7-0.9,1.6-1.4,2.5c-0.3,0.5-1,0.8-1.3,1.3c-0.5,0.9-0.4,2-1,2.8c-0.6,0.7-1.9,1-2.6,1.6c-0.7,0.4-0.2,1.1-0.6,1.9c-0.2,0.3-0.6,0.4-0.7,0.8c-0.6,0-1.4-0.1-1.7-0.4c-0.4-0.2-0.3-0.3-0.5-0.7c-0.5-0.1-1.3-0.1-1.6,0c-0.7,0.3-0.4,0.6-0.8,1.2c-0.2,0.1-0.3,0.2-0.5,0.2c-0.3,0.5,0,1.1-0.2,1.6c-0.2,0.3-1.3,0.7-1.6,1.1c-0.3,0.3-0.1,0.7-0.2,1.1c-0.2,0.4-0.8,1.2-1.1,1.4c-0.2,0-0.3,0.1-0.5,0.1V65c-0.1,0.2-0.6,0.5-0.7,0.7c-0.4,0.7-0.5,1.4-1,1.9c-0.2,0.2-0.5,0.1-0.7,0.2c-0.5,0.3-1,1.2-1.4,1.6c-0.2,0.2-0.4,0.1-0.6,0.2c-0.5,0-0.5,0-0.7-0.2c-0.3,0-0.3,0.1-0.5,0.1c0.1,0.5,0.3,0.8,0.4,1.2c0.2,0.7-0.3,1.3-0.4,1.8c-0.1,0.9,0.3,2.6,0.6,3.2c0.1,0.2,0.4,0.2,0.5,0.4c0.2,0.4,0.1,0.7,0.4,1.1c0.2,0.3,0.6,0.3,0.8,0.6c2.4,0,4.4,0.5,6.4,1.1c0.6,0,1.2,0.1,1.8,0.1c0.6,0.3,1.2,1.1,1.8,1.3c0.6,0.2,1.5-0.2,1.8-0.2c0.7-0.2,3,0.2,3.1,0.2c0.6,0.1,0.9,0,1.2,0.2c0.3,0.2,0.4,0.4,0.6,0.6c-0.1,0.6-0.4,1.1-0.8,1.3h-0.6c-0.5,0.4-0.4,1.3-1.2,1.6c-1,0.4-2-0.1-3,0.5c-1.2,0.7-2,1.9-3.5,2.4h-1.4c-0.1,0-0.5,0.4-0.6,0.5h-1.1c-0.9,0.2-2.4,0.5-3.4,1.1c-0.2,0.1-0.3,0.5-0.5,0.7c-0.3,0.3-1,0.3-1.2,0.7c-0.6,1-1.1,2.1-2.2,2.6c-0.4,0.2-1.2,0.4-1.6,0.5h-1c-0.9,0.4-1.8,1-2.4,1.8c-0.4,0-0.6,0-0.8-0.1c-0.7-0.3-0.6-1.1-1.1-1.4c-0.4-0.3-0.8,0-1.3-0.2c-1.2-0.4-2.9-0.7-3.6-1.6c-1.6,0.4-0.7,2.3-1.1,4c-0.1,0.4-0.5,0.8-0.6,1.1c-0.3,0.7,0.5,2.1,0.6,2.6c0.2,0.9-0.2,1.9,0.1,2.6c0.6,1.2,1.7,2.4,2,3.8c0.2,0.9-0.2,1.8-0.4,2.5c-0.1,0.6,0,1.4,0.1,1.8c0.2,1.1-0.4,1.6-0.1,2.5c0.2,0.8,0.4,1.8,0.7,2.5c0.1,0.2,0.4,0.2,0.5,0.4c0.6,0.7,1.2,3.4,0.8,5c-0.2,0.6-0.3,1.1-0.5,1.7c-0.5,0.7-1.4,1.3-1.6,2.3c-0.1,0.9,0.1,2.2,0.4,3v1.9c0.4,1.5,0.9,3.2-0.1,4.6c0,0.4,1.5,3,1.7,3.2c0.8,0.6,2,0.5,2.6,1.3c0.8,1,0.9,3.2,2,3.8c1.3,0.7,4.3,0.1,4.8,1.6c0.8,2.2-1.1,3.7-1.8,4.9c-1.2,2-1.9,7.9-0.5,9.7c0.4,0.4,0.9,0.3,1.4,0.6c0.2,0.1,0.3,0.6,0.5,0.7h0.8c0.4,0.2,1.3,0.5,1.6,0.8c0.4,0.5,0.4,1.7,0.7,2.4c0.5,1.1,1.6,2.2,1.9,3.4c0.1,0.5-0.2,1-0.2,1.3c-0.2,0.9,0.2,2.5,0.6,3c0.4,0.6,1.5,0.7,2,1.2c0.7,0.6,0.9,1.9,1.7,2.3c1,0.5,2,0.5,2.8,1.2c0.6,0,3.7-1.3,4.2-1.7c0.4-0.3,1.1-1.4,1.3-1.9c0.2-1,0.3-1.9,0.5-2.9c0.4-1.1,1.3-2,1.7-3.2c0.3-1.2-0.4-4,0.4-4.9c0.1-0.3,0.2-0.3,0.4-0.5c2.2,0,2.1,2.3,3.5,3.1c0.4,0,0.8,0.1,1.2,0.1c1.5,0.3,3.3,0.2,4.4,0.8c0,0.5,0.2,0.8,0,1.1c0.1,2.6,3.2,2.9,4.7,4.1c0.1,0.1,1.2,1.4,1.3,1.6c2.4,0,3.7,0,5.6-0.7h0.6c0.7-0.3,0.8-1,1.6-1.2c0,0,1,0.1,1.2,0.2c1.1,0.6,1.8,2,3,2.6c0.1,0.2,0,0.1,0.1,0.2c0,1.1-0.5,1.6-1,2.3c-1.5,0.6-3.2,0.9-4.6,1.4h-0.8c-0.5,0.2-1.3,0.7-1.8,0.8h-1.3c-2.1,0.7-4.9,1.1-6.8,1.8h-1.4c-1.6,0.5-3.5,1-5,1.6c-0.5,0.2-1.3,0-1.8,0.2c-0.4,0.2-0.5,0.9-0.8,1.2c-0.4,0.4-2.9,1.5-3.5,1.7c-1.1,0.3-2.3-0.6-3-0.8c-0.2-0.1-3.8-0.5-3.8-0.5c-0.4,0.1-0.8,0.4-1.3,0.5c-0.1,0.8-0.2,1.5-0.4,2.3c-0.2,0.5-0.9,0.9-1.1,1.3c-0.7,1.5,0.2,3.9,1.1,4.6v1.8c-0.2,0.2-0.4,0.3-0.6,0.5c-0.5,0.7-1.1,2.3-1,3.6c0.2,1.6,1.6,3,2.2,4.3c0.2,0.5,0,0.9,0.2,1.4c0.3,0.7,1.4,1.6,1.1,2.8c-0.2,0.7-0.2,1.9-0.5,2.6c-0.4,1.1-1.5,1.7-1.9,2.9c-0.6,0.1-1.8,0.3-2.3,0.6c-0.6,0-0.6,0.1-1,0.4c0.1,0.4,0.1,0.3,0.2,0.6c0.9-0.1,1.9-0.2,2.4-0.7c1.8,0,2.2,2,3.1,3c-0.1,1.8-1,2.6-1,4.4c1,0.9,1.8,2.2,2.5,3.4c1.3,0,3.7-1,4.8-0.1c0.7,0.3,1.2,2.1,2.3,2.6c0.6,0.3,1.6,0.4,2.3,0.5h1.6c1.5,0.5,3.1,1.1,4.4,1.7c0.2,0.5,0.6,2.7,0.7,2.9c0,2.3,0.6,1.7,2.3,2.2c0.6,0.2,1.3,0.6,1.8,0.8c0.9,0.3,2.8-0.4,3.8,0c1.5,0.6,2.9,0.4,4.6,0.2h3.1c0.5-0.2,1.2-0.6,1.7-0.7c0.6,0,1.1-0.1,1.7-0.1c0.4-0.2,0.8-0.8,1.3-1c0.8-0.3,1.4,0.2,2.2,0c0.4-0.1,1-0.5,1.6-0.6c1.7-0.4,2,1.2,3.1,1.6c0.8,0.2,1.6-0.5,2.2-0.5c1.1,0.1,1.4,1,2.3,1.2c0.1,0.3,0.1,0.5,0.1,1c-0.3,0.3-0.5,0.5-1,0.7c0,0.1,0.1,0.2,0.1,0.2c0.8-0.1,0.8-0.5,1.3-0.7c0.9-0.4,1.8,0.2,2.6-0.1c0.4-0.1,1-0.6,1.3-0.7h0.6c0.1-0.1,0.3-0.4,0.5-0.5c0.7-0.2,2-0.2,2.3,0.1c1.2,0.1,1,0.9,1.4,1.7c0.3,0.5,1.1,0.8,1.4,1.3c1.6,0,2.6-0.8,3.8-1.2c0.7-0.2,1.5-0.1,1.8-0.7c0.1-0.3-0.1-1-0.1-1.6c0-0.7,0.2-1.1,0.4-1.6C137.8,230.4,137.3,229,137.2,228.6z"}]
     ]
    [:g {:on-click #(show-sites % owner ref menu "Coastal Sea" "Elven Shores")} [:path {:className "st4" :d "M130.4,237c-0.8-0.5-1.5-1.5-1.8-2.5c-0.9,0-1.7-0.2-2.4,0.2c-0.3,0.1-0.7,0.6-1,0.7c-0.1,0.1-2.2,0.2-2.2,0.2c-0.1,0-0.3-0.4-0.7-0.2c-0.9,0.4-1.8,1.6-3.1,0.7c-0.4-0.1-0.5-0.4-0.8-0.6c-0.1-0.5-0.2-1.3,0.1-1.7c0-0.3-0.1-0.3-0.1-0.5c-0.5,0.1-0.7,0.3-1.1,0.4c-1.4,0.4-3-0.8-3.5-1.6c-0.8,0.1-2.1,1.3-2.9,0.5c-1.2,0-1.3,0.6-2,1c-0.6,0.3-1.2,0-2,0.2c-0.7,0.2-1.6,0.6-2.3,0.8h-2.4c-1.2,0-3.7,0.4-4.7,0c-0.3-0.1-0.7-0.5-1.1-0.6c-0.5-0.1-1.5,0.3-1.8,0.4c-1.3,0.4-2-0.6-2.8-0.8c-0.6-0.2-0.9,0-1.3-0.2c-1.2,0-1.2-0.3-1.8-0.7c-1.8-1.3-0.7-3-1.7-4.8c-1.1-0.4-2.5-0.9-3.6-1.2h-2c-2-0.7-3.2-1.2-4.1-3c-1.2-0.5-2.9,0.1-4.6,0.1c-0.4-0.3-0.8-0.6-1.2-1c-0.4-0.5-0.6-1.2-1-1.7c-0.4-0.6-1.2-0.9-1.4-1.7c-0.6-1.8,1.2-3.7,0.7-4.6c-0.1-0.8-0.6-1.1-1-1.7c-0.5,0-0.8,0.1-1.1,0.4c-1.7,0-4.1,0.2-3.6-1.9c0.2-0.8,0.9-1.9,1.7-2.2c0.6-0.2,1.3,0.1,1.8-0.1c1.3-0.7,1.7-2.6,2-4.3c0.2-1-0.5-1.4-0.7-1.9c-0.1-0.4-0.2-0.9-0.2-1.3c-0.5-1.3-1.7-2.6-2.2-4.1c-0.5-1.6,0-4,0.6-5c0.2-0.3,0.8-0.6,0.8-1c0.2-0.9-1.1-1.8-1.3-2.5c-0.3-1.1,0-3.2,0.4-3.8c0.2-0.5,0.8-0.9,1-1.4c0.3-0.9,0-1.6,0.5-2.2c0.9-1.3,3.5-1.4,5.4-1h1.4c0.8,0.3,1.5,0.8,2.4,1.1c0.6-0.6,1.4-0.6,2.2-1.1c0.7-0.5,1.1-1.4,1.9-1.8c0.9-0.4,1.9-0.2,2.9-0.5c1.2-0.4,2.6-0.9,3.8-1.3h1.4c1.1-0.3,2.7-0.7,3.7-1.1H95c0.6-0.2,1.7-0.5,2.3-0.7h1.1c1.3-0.5,3-1.1,4.3-1.6h1c0.4-0.2,0.8-0.4,1.4-0.5c0.1-0.1,0,0,0.1-0.1c-0.1-1.1-1.2-1.4-1.7-2.2c-1,0.1-1.3,0.7-2,1c-0.8,0.3-5.4,1.1-6.6,0.7c-1.2-0.3-1.6-1.4-2.4-2c-0.4-0.3-0.9-0.2-1.3-0.5c-1.3-0.7-3.8-2.7-3.8-4.6c-2.8-0.9-4.9,0.2-6.2-2.4c-0.5,0.3-0.3,2.3-0.5,2.9c-0.4,1.2-1.3,2.4-1.7,3.6c-0.3,1-0.1,2.1-0.6,2.9c-0.8,1.3-1.6,2.4-3,3c-1,0.4-3,0.8-3.6,1.6c-1.5,0-1.3-0.9-2.2-1.3c-0.5-0.2-0.9,0-1.3-0.2c-0.4-0.2-1.3-0.7-1.6-1.1c-0.3-0.4-0.4-0.9-0.7-1.2c-0.9-0.8-2.2-1.5-2.8-2.6c-0.2-1-0.3-1.9-0.5-2.9c0-0.3,0.4-0.6,0.4-0.8c-0.1-1.2-1.4-2.3-1.8-3.4c-0.3-0.7-0.3-1.5-0.7-2c-0.5-0.3-1.3-0.1-1.8-0.4c-0.2-0.3-0.5-0.6-0.7-0.8c-0.6-0.4-1.2-0.3-1.6-0.8c-2.2-3.2-1.1-9.1,0.8-12.1c0.4-0.6,0.5-1.6,1.1-2c-0.1-1.6-1.2-0.8-2.5-1.2c-0.7-0.2-1.4-0.5-1.9-0.8c-1.5-1.1-1-2.4-1.9-3.7c-0.6-0.8-1.8-0.6-2.6-1.1c-0.6-0.4-0.6-1.4-1-2.2c-0.3-0.6-1-0.9-1.2-1.6c-0.6-0.9,0.7-2.4,0.6-3.7c-0.2-1.7-1.4-4.7-0.8-7.1c0.5-2,2.8-3.1,2-6c-0.2-1-1-1.6-1.3-2.4c-0.4-0.8-1-3.2-1-4.1c0-0.4,0.3-0.9,0.2-1.3c-0.1-0.5-0.3-1.6-0.1-2.3c0.1-0.5,0.6-1.4,0.4-2.3c-0.4-1.3-1.5-2.5-2-4c0-0.8-0.1-1.6-0.1-2.4c-0.2-0.6-0.8-1.7-0.6-2.6c0.1-0.5,0.5-1.1,0.6-1.6c0.3-1.2-0.1-2.3,0.2-3.4c0.3-0.8,1-1.4,1.4-2c1,0.1,1.8,0.2,2.5,0.6c0.2,0.1,0.5,0.5,0.7,0.6h0.7c0.5,0.2,1.3,0.5,1.8,0.7h0.6c0.3,0.2,0.9,1.2,1.3,1.1c0.2,0,0.7-0.7,1-0.8c1.1-0.5,2.5-0.3,3.5-0.8c0.7-0.4,0.8-1.2,1.2-1.9c0.4-0.7,2-1.7,2.6-2.2c0.9-0.6,2.4-0.9,3.6-1.2c0.4-0.1,0.6,0.2,0.8,0.1c0.3-0.1,0.7-0.6,1-0.7H70c0.6-0.2,1.8-1.7,2.5-2c0.9-0.5,2-0.5,3.2-0.8c0-0.1,0.1-0.2,0.1-0.2c-1.2-0.4-3.1,0.3-4.3-0.1c-0.4-0.1-0.5-0.7-0.8-0.8c-0.3-0.2-2-0.7-2.4-0.8c-2.4-0.7-5.7-0.3-7.4-1.6c-0.8-0.6-2.8-4.6-2.4-6.2c0-0.2,0.3-0.8,0.4-1.1c0.1-0.8-0.7-1.5-0.1-2.8c0.6-1.2,1.3-0.5,2.4-1c0.6-0.5,1.1-1,1.7-1.4c0.6-0.8,1-1.8,1.6-2.6c0.4-0.4,0.8-0.7,1.2-1.1c0.2-0.6,0.4-1.2,0.6-1.8c0.4-0.5,1.2-0.7,1.6-1.2c0.2-0.5,0.3-1,0.5-1.6c0.4-0.6,1-1.2,1.4-1.8c0.7-0.1,1.7-0.5,2.6-0.2c0.5,0.1,0.8,0.5,1.3,0.7c0.3-0.8,0.6-1.7,1.2-2.2c0.8-0.6,2-0.7,2.5-1.6c0.4-0.7,0.3-1.7,0.7-2.3c0.3-0.4,0.8-0.7,1.1-1.1c0.3-0.4,0.3-0.9,0.6-1.3c0.2-0.3,0.5-0.5,0.7-0.7c0.3-0.4,0.3-1,0.7-1.2c0.6-0.3,1.3-0.2,1.9-0.5c2-0.9,2.7-2.2,5.8-2c0.3,0.3,0.7,0.6,1,0.8c0.5,0.2,1.1-0.4,1.6-0.6h0.8c1.9-0.8,2.1-2.9,3.2-4.4c0.6-0.8,1.6-1.3,2.2-2c1.5,0,2.3-0.3,3.4-0.5c0.6-0.1,0.9,0.3,1.3,0c0.6-0.3,0.8-1.2,1.3-1.6c1-0.7,2.4-0.7,3.5-1.3c2.2-1.2,3.1-3.9,6.4-4.1v-0.2c-4,0-6.9,0.4-10,1.2c-1.9,0.5-4.5,0.2-6.7,0.2H94c-0.5,0.1-1.4,0.4-2.2,0.2c-2.5-0.6-4.1-1.7-6.1-2.5c-0.7-0.3-1.3,0-2.2-0.2c-1.3-0.4-3.2-0.5-4.6-0.8h-2.9c-1.4-0.4-3.5-0.4-5.3-0.4h-3.4c-1-0.3-3.9-2.5-4.4-3.4H31.2c-0.5,1.7-0.1,5.3-0.1,7.3v269.6c0,1.8-0.4,4.5,0.1,6c3-0.5,5.4-1.7,7.8-2.6h0.8c1-0.4,2.6-0.8,3.6-1.2h0.7c3.8-1.3,7.8-2.8,11.5-4.2c0.7-0.3,1.2,0,1.9-0.2c1-0.3,2.5-0.7,3.5-1.1h1.2c0.9-0.3,2.1-0.4,3.1-0.7c1.2-0.4,2.5-1.3,3.6-1.8c2.1-1,4.1-1.6,6.5-2.3H77c0.4-0.1,1.2-0.3,1.7-0.4h2.2c1.1-0.3,2.3-0.6,3.2-1h1.6c0.7-0.2,1.6-0.2,2.3-0.5c0.5-0.2,1.2-0.9,1.7-1.1h0.7c0.1-0.1,0.4-0.4,0.5-0.5c0.6-0.2,1.1-0.3,1.7-0.5c0.5-0.3,1.3-0.9,1.7-1.3c0.3-0.3,0.4-0.9,0.7-1.1c1.3-0.9,2.7-1.9,3.6-3.2c0.4-0.6,0.3-1.3,0.6-2c0.2-0.6,0.8-1.5,1.2-2c0.3-0.3,0.6-0.6,1-0.8c0.8-1.1,1.8-2.4,3-3.1c0.6-0.4,1.1-0.6,1.6-1.1c0.4-0.4,0.3-0.9,0.6-1.4c0.3-0.4,0.9-0.8,1.2-1.3c0.7-1.3,1-2.7,1.8-4c0.4-0.7,1.2-1.8,1.8-2.3c1.1-0.8,2.4-1.2,2.9-2.5c0.7-1.1-0.6-2.2-0.5-3.1c0.1-0.4,0.5-0.7,0.6-1c0.3-0.6,0.3-1.1,0.6-1.6c0.3-0.4,1.8-1.1,2.3-1.4c0-0.8-0.3-1.5,0-2.3c0.3-0.9,1.5-3.4,2.3-3.8c0.5-0.3,1.1-0.1,1.7-0.4c1.3-0.5,3.1-0.8,4.2-1.4c0.1-0.5-0.1-1.1,0-1.6c0.3-1.2,1-3.1,1.9-3.6h0.5c0,0,1.4-0.9,1.4-1c0.4-0.4,0.2-0.9,0.5-1.6c0.1-0.2,1.2-2,1.3-2.2c0.2-0.2,0.5-0.2,0.7-0.4h0.1C131.5,237.2,130.9,237.3,130.4,237z"}]
     ]
    ]
   ]
  )

(defn sites-view [{:keys [identity sites] :as cursor} owner]
  (om/component
    (sab/html
      (let [is-challenger (= "Challenger" (:side identity))
            side (if is-challenger :challenger :contestant)
            map-name (if is-challenger "Sites2" "Sites")
            map-ref (if is-challenger "Ch-menu" "Co-menu")
            map-menu-ref (str map-ref "-menu")
            map-content-ref (str map-ref "-content")
            site-name (if is-challenger "Sites2" "Sites")
            site-ref (if is-challenger "Ch-sites" "Co-sites")
            site-menu-ref (str site-ref "-menu")
            site-content-ref (str site-ref "-content")
            reg-name (if is-challenger "Regions2" "Regions")
            reg-ref (if is-challenger "Ch-regions" "Co-regions")
            reg-menu-ref (str reg-ref "-menu")
            reg-content-ref (str reg-ref "-content")]
        [:div.blue-shade.deck
         (drop-area (:side @game-state) name
                    {:on-click #(-> (om/get-node owner map-menu-ref) js/$ .toggle)})
         (facedown-card "Locations")
         (om/build label sites {:opts {:name map-name}})
         (when (= (:side @game-state) side)
           [:div.panel.blue-shade.menu {:ref map-menu-ref}
            [:div {:on-click #(show-map % owner reg-ref)} "Regions"]
            [:div {:on-click #(show-map % owner map-ref)} "Sites"]]
           )
         (when (= (:side @game-state) side)
           [:div.panel.blue-shade.popup {:ref reg-content-ref}
            [:div
             (general-map owner reg-ref map-menu-ref)
             [:a {:on-click #(close-popup % owner reg-content-ref "stops looking at the map" false true true)}
              "Close"]]
            ])
           [:div.panel.blue-shade.popup {:ref map-content-ref}
            [:div
             (general-map owner site-ref map-menu-ref)
             [:a {:on-click #(close-popup % owner map-content-ref "stops looking at the map" false true true)}
              "Close"]]
            ]
           [:div.panel.blue-shade.popup {:ref site-content-ref}
            [:div
             [:a {:on-click #(close-popup % owner site-content-ref "stops at a region" false true true)}
              "Close"]]
            (om/build-all card-view sites {:key :cid})
            ]
         ]))))

(defmulti decks-view #(get-in % [:player :identity :side]))

(defmethod decks-view "Contestant" [{:keys [player run]}]
  (om/component
    (sab/html
      (let [servers (:servers player)
            s (:server run)
            server-type (first s)]
        [:div.contestant-board {:class (if (= (:side @game-state) :challenger) "opponent" "me")}
         (om/build server-view {:server (:hq servers)
                                :central-view (om/build sites-view player)
                                :run (when (= server-type "hq") run)})
         (om/build server-view {:server (:rd servers)
                                :central-view (om/build deck-view player)
                                :run (when (= server-type "rd") run)})
         (om/build server-view {:server (:archives servers)
                                :central-view (om/build discard-view player)
                                :run (when (= server-type "archives") run)})]))))

(defmethod decks-view "Challenger" [{:keys [player run]}]
  (om/component
    (sab/html
        [:div.challenger-board {:class (if (= (:side @game-state) :contestant) "opponent" "me")}
         (om/build sites-view player)
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
      (let [servers (:servers player)
            s (:server run)
            server-type (first s)]
        [:div.contestant-board {:class (if (= (:side @game-state) :challenger) "opponent" "me")}
         (for [server (reverse (get-remotes servers))
               :let [num (remote->num (first server))]]
           (om/build server-view {:server (second server)
                                  :run (when (= server-type (str "remote" num)) run)}
                     {:opts {:name (remote->name (first server))}}))]))))

(defmethod board-view "Challenger" [{:keys [player run]}]
  (om/component
    (sab/html
      (let [servers (:servers player)
            s (:server run)
            server-type (first s)]
        [:div.challenger-board {:class (if (= (:side @game-state) :contestant) "opponent" "me")}
         (for [server (reverse (get-remotes servers))
               :let [num (remote->num (first server))]]
           (om/build server-view {:server (second server)
                                  :run (when (= server-type (str "remote" num)) run)}
                     {:opts {:name (remote->name (first server))}}))]))))

(defn cond-button [text cond f]
  (sab/html
   (if cond
     [:button {:on-click f} text]
     [:button.disabled text])))

(defn handle-end-turn []
  (let [me ((:side @game-state) @game-state)
        max-size (max (+ (:hand-size-base me) (:hand-size-modification me)) 0)]
    (if (> (count (:hand me)) max-size)
      (toast (str "Discard to " max-size " card" (when (not= 1 max-size) "s")) "warning" nil)
      (send-command "end-turn"))))

(defn runnable-servers
  "List of servers the challenger can run on."
  [contestant challenger]
  (let [servers (keys (:servers contestant))
        restricted-servers (keys (get-in challenger [:register :cannot-run-on-server]))]
    ;; remove restricted servers from all servers to just return allowed servers
    (remove (set restricted-servers) servers)))

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
           (if run
             (let [s (:server run)
                   kw (keyword (first s))
                   server (if-let [n (second s)]
                            (get-in contestant [:servers kw n])
                            (get-in contestant [:servers kw]))]
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
                               #(-> (om/get-node owner "servers") js/$ .toggle))
                  [:div.panel.blue-shade.servers-menu {:ref "servers"}
                   (map (fn [label]
                          [:div {:on-click #(do (send-command "run" {:server label})
                                                (-> (om/get-node owner "servers") js/$ .fadeOut))}
                           label])
                        (zones->sorted-names (runnable-servers contestant challenger)))]]])
              (when (= side :contestant)
                (cond-button "Purge" (>= (:click me) 3) #(send-command "purge")))
              (when (= side :contestant)
                (cond-button "Trash Muthereff" (and (pos? (:click me))
                                                   (>= (:credit me) (- 2 (or (:trash-cost-bonus me) 0)))
                                                   (or (pos? (:tagged opponent))
                                                       (pos? (:tag opponent))))
                             #(send-command "trash-muthereff")))
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
                          (audio-sfx "install-contestant")
                          (audio-sfx "install-challenger")
                          (audio-sfx "play-instant")
                          (audio-sfx "rez-character")
                          (audio-sfx "rez-other")
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
              (om/build hand-view {:player opponent :remotes (get-remotes (:servers (if (= side :challenger) contestant challenger)))
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
                (om/build rfg-view {:cards (:rfg opponent) :name "Removed from the game" :popup true})
                (om/build rfg-view {:cards (:rfg me) :name "Removed from the game" :popup true})
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
              (om/build hand-view {:player me :remotes (get-remotes (:servers (if (= side :challenger) challenger contestant)))
                                   :popup true :popup-direction "me"})]]]))))))

(om/root gameboard game-state {:target (. js/document (getElementById "gameboard"))})
