(ns meccg.account
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put!] :as async]
            [clojure.string :as s]
            [goog.dom :as gdom]
            [meccg.auth :refer [authenticated avatar] :as auth]
            [meccg.appstate :refer [app-state]]
            [meccg.ajax :refer [POST GET PUT]]
            [cardnum.cards :refer [all-cards]]))

(defn image-url [card-code version]
  (let [card (get (:cards @app-state) card-code)]
    (str "/img/cards/" (:set_code card) "/" (:ImageName card))))

(defn post-response [owner response]
  (if (= (:status response) 200)
    (om/set-state! owner :flash-message "Profile updated - Please refresh your browser")
    (case (:status response)
      401 (om/set-state! owner :flash-message "Invalid login or password")
      404 (om/set-state! owner :flash-message "No account with that email address exists")
      :else (om/set-state! owner :flash-message "Profile updated - Please refresh your browser"))))

(defn post-options [url callback]
  (let [params (:options @app-state)]
    (go (let [response (<! (PUT url params :json))]
          (callback response)))))

(defn handle-post [event owner url ref]
  (.preventDefault event)
  (om/set-state! owner :flash-message "Updating profile...")
  (swap! app-state assoc-in [:options :sounds] (om/get-state owner :sounds))
  (swap! app-state assoc-in [:options :keys-pick] (om/get-state owner :keys-pick))
  (swap! app-state assoc-in [:options :background] (om/get-state owner :background))
  (swap! app-state assoc-in [:options :dice-pick] (om/get-state owner :dice-pick))
  (swap! app-state assoc-in [:options :dice-size] (om/get-state owner :dice-size))
  (swap! app-state assoc-in [:options :sounds-volume] (om/get-state owner :volume))
  (swap! app-state assoc-in [:options :blocked-users] (om/get-state owner :blocked-users))
  (swap! app-state assoc-in [:options :gamestats] (om/get-state owner :gamestats))
  (swap! app-state assoc-in [:options :deckstats] (om/get-state owner :deckstats))
  (swap! app-state assoc-in [:options :language] (om/get-state owner :language))
  (swap! app-state assoc-in [:options :dc-erratum] (om/get-state owner :dc-erratum))
  (swap! app-state assoc-in [:options :ice-errata] (om/get-state owner :ice-errata))
  (.setItem js/localStorage "sounds" (om/get-state owner :sounds))
  (.setItem js/localStorage "sounds_volume" (om/get-state owner :volume))
  (post-options url (partial post-response owner)))

(defn add-user-to-block-list
  [owner user]
  (let [blocked-user-node (om/get-node owner "block-user-input")
        blocked-user (.-value blocked-user-node)
        my-user-name (:username user)
        current-blocked-list (om/get-state owner :blocked-users)]
    (set! (.-value blocked-user-node) "")
    (when (and (not (s/blank? blocked-user))
               (not= my-user-name blocked-user)
               (= -1 (.indexOf current-blocked-list blocked-user)))
      (om/set-state! owner :blocked-users (conj current-blocked-list blocked-user)))))

(defn remove-user-from-block-list
  [evt owner]
  (let [currElt (.-currentTarget evt)
        next-sib (gdom/getNextElementSibling currElt)
        user-name (gdom/getTextContent next-sib)
        current-blocked-list (om/get-state owner :blocked-users)]
    (when user-name
      (om/set-state! owner :blocked-users (vec (remove #(= % user-name) current-blocked-list))))))

(defn account-view [user owner]
  (reify
    om/IInitState
    (init-state [this] {:flash-message ""})

    om/IWillMount
    (will-mount [this]
      (om/set-state! owner :keys-pick (get-in @app-state [:options :keys-pick]))
      (om/set-state! owner :background (get-in @app-state [:options :background]))
      (om/set-state! owner :dice-pick (get-in @app-state [:options :dice-pick]))
      (om/set-state! owner :dice-size (get-in @app-state [:options :dice-size]))
      (om/set-state! owner :sounds (get-in @app-state [:options :sounds]))
      (om/set-state! owner :volume (get-in @app-state [:options :sounds-volume]))
      (om/set-state! owner :gamestats (get-in @app-state [:options :gamestats]))
      (om/set-state! owner :deckstats (get-in @app-state [:options :deckstats]))
      (om/set-state! owner :language (get-in @app-state [:options :language]))
      (om/set-state! owner :dc-erratum (get-in @app-state [:options :dc-erratum]))
      (om/set-state! owner :ice-errata (get-in @app-state [:options :ice-errata]))
      (om/set-state! owner :blocked-users (sort (get-in @app-state [:options :blocked-users] []))))

    om/IRenderState
    (render-state [this state]
      (sab/html
        [:div.account
         [:div.panel.blue-shade.content-page#profile-form {:ref "profile-form"}
          [:h2 "Settings"]
          [:form {:on-submit #(handle-post % owner "/profile" "profile-form")}
           [:section
            [:h3 "Avatar"]
            (om/build avatar user {:opts {:size 38}})
            [:a {:href "http://gravatar.com" :target "_blank"} "Change on gravatar.com"]]

           [:section
            [:h3 "Sounds"]
            [:div
             [:label [:input {:type "checkbox"
                              :value true
                              :checked (om/get-state owner :sounds)
                              :on-change #(om/set-state! owner :sounds (.. % -target -checked))}]
              "Enable sounds"]]
            [:div "Volume"
             [:input {:type "range"
                      :min 1 :max 100 :step 1
                      :on-change #(om/set-state! owner :volume (.. % -target -value))
                      :value (om/get-state owner :volume)
                      :disabled (not (om/get-state owner :sounds))}]]]

           [:section
            [:h3  "Hot Keys"]
            (for [option [{:name "[ for Zoom, ] for Send, and \\ for Auto"          :ref "default"}
                          {:name "<- for Zoom, -> for Send, alt/option for Auto"    :ref "classic"}]]
              [:div.radio
               [:label [:input {:type "radio"
                                :name "keys-pick"
                                :value (:ref option)
                                :on-change #(om/set-state! owner :keys-pick (.. % -target -value))
                                :checked (= (om/get-state owner :keys-pick) (:ref option))}]
                (:name option)]])]

           [:section
            [:h3  "Game board background"]
            (for [option [{:name "Barad-dûr by John Howe"              :ref "barad-dur-john-howe-bg"}
                          {:name "Bridge by Paul Monteagle"            :ref "bridge-paul-monteagle-bg"}
                          {:name "Edoras by Bakarov"                   :ref "edoras-bakarov-bg"}
                          {:name "Green Hill Morning by Ted Naismith"  :ref "ted-green-hill-morning-bg"}
                          {:name "Rogrog in Cardolan by Angus McBride" :ref "lobby-bg"}
                          {:name "Carn Dûm 1 (b&w)"                    :ref "carn-dum1bw-bg"}
                          {:name "Carn Dûm 2 (b&w)"                    :ref "carn-dum2bw-bg"}
                          {:name "Carn Dûm 3 (b&w)"                    :ref "carn-dum3bw-bg"}
                          {:name "Monochrome"                          :ref "monochrome-bg"}
                          ]]
              [:div.radio
               [:label [:input {:type "radio"
                                :name "background"
                                :value (:ref option)
                                :on-change #(om/set-state! owner :background (.. % -target -value))
                                :checked (= (om/get-state owner :background) (:ref option))}]
                (:name option)]])]

           [:section
            [:h3  "Which Lidless Eye Dice?"]
            (for [option [{:name "Black Flat Red Pips 16mm"       :ref "blck-16"}
                          {:name "Black Swirl Red Pips 18mm"      :ref "blacks-18"}
                          {:name "Silver Swirl Red Pips 16mm"     :ref "greys-16"}
                          {:name "Grey Swirl Red Pips 18mm"       :ref "greys-18"}
                          {:name "Dk. Gold Swirl Black Pips 16mm" :ref "gsdark-16"}
                          {:name "Lt. Gold Swirl Black Pips 18mm" :ref "gslight-18"}
                          {:name "Orange Flat Black Pips 16mm"    :ref "orgblack-16"}
                          {:name "Red Swirl Black Pips 16mm"      :ref "rsblack-16"}
                          {:name "Red Swirl Black Pips 18mm"      :ref "rsblack-18"}
                          {:name "Red Swirl White Pips 16mm"      :ref "rswhite-16"}]]
              [:div.radio
               [:label [:input {:type "radio"
                                :name "dice-pick"
                                :value (:ref option)
                                :on-change #(om/set-state! owner :dice-pick (.. % -target -value))
                                :checked (= (om/get-state owner :dice-pick) (:ref option))}]
                (:name option)]])]

           [:section
            [:h3  "Treat as which size?"]
            (for [option [{:name "treat as 16mm"      :ref "16mm"}
                          {:name "treat as 18mm"      :ref "18mm"}]]
              [:div.radio
               [:label [:input {:type "radio"
                                :name "dice-size"
                                :value (:ref option)
                                :on-change #(om/set-state! owner :dice-size (.. % -target -value))
                                :checked (= (om/get-state owner :dice-size) (:ref option))}]
                (:name option)]])]

           [:section
            [:h3 " Game Win/Lose Statistics "]
            (for [option [{:name "Always"                   :ref "always"}
                          {:name "Competitive Lobby Only"   :ref "competitive"}
                          {:name "None"                     :ref "none"}]]
              [:div
               [:label [:input {:type "radio"
                                :name "gamestats"
                                :value (:ref option)
                                :on-change #(om/set-state! owner :gamestats (.. % -target -value))
                                :checked (= (om/get-state owner :gamestats) (:ref option))}]
                (:name option)]])]

           [:section
            [:h3 " Deck Statistics "]
            (for [option [{:name "Always"                   :ref "always"}
                          {:name "Competitive Lobby Only"   :ref "competitive"}
                          {:name "None"                     :ref "none"}]]
              [:div
               [:label [:input {:type "radio"
                                :name "deckstats"
                                :value (:ref option)
                                :on-change #(om/set-state! owner :deckstats (.. % -target -value))
                                :checked (= (om/get-state owner :deckstats) (:ref option))}]
                (:name option)]])]

           [:section
            [:h3 " Language Preference"]
            [:select.language {:value (om/get-state owner :language)
                               :on-change #(om/set-state! owner :language (.. % -target -value))}
             (let [option [{:name "English"                   :ref "English"}
                           {:name "Español"                   :ref "Español"}
                           {:name "French"                    :ref "French"}
                           {:name "German"                    :ref "German"}
                           {:name "Japanese"                  :ref "Japanese"}]]
               (for [choice (sort-by :name option)]
                 [:option
                  {:value (:ref choice)}
                  (:name choice)])
               )
             ]]

           [:section
            [:h3 " Use DC Erratum (highest priority)"]
            (for [option [{:name "Always"                   :ref "always"}
                          {:name "Never"                    :ref "never"}]]
              [:div
               [:label [:input {:type "radio"
                                :name "dc-erratum"
                                :value (:ref option)
                                :on-change #(om/set-state! owner :dc-erratum (.. % -target -value))
                                :checked (= (om/get-state owner :dc-erratum) (:ref option))}]
                (:name option)]])]

           [:section
            [:h3 " Use ICE Errata (updated versions)"]
            (for [option [{:name "Always"                   :ref "always"}
                          {:name "Never"                    :ref "never"}]]
              [:div
               [:label [:input {:type "radio"
                                :name "ice-errata"
                                :value (:ref option)
                                :on-change #(om/set-state! owner :ice-errata (.. % -target -value))
                                :checked (= (om/get-state owner :ice-errata) (:ref option))}]
                (:name option)]])]

           [:section
            [:h3 " Blocked users"]
            [:div
             [:input {:on-key-down (fn [e]
                                     (when (= e.keyCode 13)
                                       (do
                                         (add-user-to-block-list owner user)
                                         (.preventDefault e))))
                      :ref "block-user-input"
                      :type "text" :placeholder "User name"}]
             [:button.block-user-btn {:type "button"
                                      :name "block-user-button"
                                      :on-click #(add-user-to-block-list owner user)}
              "Block user"]]
            (for [bu (om/get-state owner :blocked-users)]
              [:div.line
               [:button.small.unblock-user {:type "button"
                                            :on-click #(remove-user-from-block-list % owner)} "X" ]
               [:span.blocked-user-name (str "  " bu)]])]

           [:p
            [:button "Update Profile"]
            [:span.flash-message (:flash-message state)]]]]]))))


(defn account [{:keys [user]} owner]
  (om/component
   (when user
     (om/build account-view user))))

(om/root account app-state {:target (. js/document (getElementById "account"))})
