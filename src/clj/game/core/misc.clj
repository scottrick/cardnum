(in-ns 'game.core)

(declare set-prop get-nested-host get-nested-zone)

(defn get-zones [state]
  (keys (get-in state [:contestant :servers])))

(defn get-remote-zones [state]
  (filter is-remote? (get-zones state)))

(defn get-runnable-zones [state]
  (let [restricted-zones (keys (get-in state [:challenger :register :cannot-run-on-server]))]
    (remove (set restricted-zones) (get-zones state))))

(defn get-remotes [state]
  (select-keys (get-in state [:contestant :servers]) (get-remote-zones state)))

(defn get-remote-names [state]
  (zones->sorted-names (get-remote-zones state)))

(defn server-list [state card]
  (concat
    (if (#{"Asset" "Agenda"} (:type card))
      (get-remote-names @state)
      (zones->sorted-names (get-zones @state)))
    ["New remote"]))

(defn server->zone [state server party-head]
  (if (sequential? server)
    (vec (cons :servers server))
    (case server
      "HQ" [:servers :hq]
      "R&D" [:servers :rd]
      "Archives" [:servers :archives]
      "New remote" [:servers (keyword (str "remote" (make-rid state) " " (:title party-head)))]
      [:servers (->> server (str "remote") keyword)])))

(defn same-server? [card1 card2]
  "True if the two cards are IN or PROTECTING the same server."
  (let [zone1 (get-nested-zone card1)
        zone2 (get-nested-zone card2)]
    (= (second zone1) (second zone2))))

(defn protecting-same-server? [card character]
  "True if an character is protecting the server that the card is in or protecting."
  (let [zone1 (get-nested-zone card)
        zone2 (get-nested-zone character)]
    (and (= (second zone1) (second zone2))
         (= :characters (last zone2)))))

(defn in-same-server? [card1 card2]
  "True if the two cards are installed IN the same server, or hosted on cards IN the same server."
  (let [zone1 (get-nested-zone card1)
        zone2 (get-nested-zone card2)]
    (and (= zone1 zone2)
         (is-remote? (second zone1)) ; cards in centrals are in the server's root, not in the server.
         (= :content (last zone1)))))

(defn from-same-server? [upgrade target]
  "True if the upgrade is in the root of the server that the target is in."
  (= (central->zone (:zone target))
     (butlast (get-nested-zone upgrade))))

(defn all-installed
  "Returns a vector of all installed cards for the given side, including those hosted on other cards,
  but not including 'inactive hosting' like Personal Workshop."
  [state side]
  (if (= side :challenger)
    (let [top-level-cards (flatten (for [t [:program :hardware :muthereff]] (get-in @state [:challenger :rig t])))
          hosted-on-character (->> (:contestant @state) :servers seq flatten (mapcat :characters) (mapcat :hosted))]
      (loop [unchecked (concat top-level-cards (filter #(= (:side %) "Challenger") hosted-on-character)) installed ()]
        (if (empty? unchecked)
          (filter :installed installed)
          (let [[card & remaining] unchecked]
            (recur (filter identity (into remaining (:hosted card))) (into installed [card]))))))
    (let [servers (->> (:contestant @state) :servers seq flatten)
          content (mapcat :content servers)
          character (mapcat :characters servers)
          top-level-cards (concat character content)]
      (loop [unchecked top-level-cards installed ()]
        (if (empty? unchecked)
          (filter #(= (:side %) "Contestant") installed)
          (let [[card & remaining] unchecked]
            (recur (filter identity (into remaining (:hosted card))) (into installed [card]))))))))

(defn get-all-installed
  "Returns a list of all installed cards"
  [state]
  (concat (all-installed state :contestant) (all-installed state :challenger)))

(defn all-active
  "Returns a vector of all active cards for the given side. Active cards are either installed, the identity,
  currents, or the contestant's scored area."
  [state side]
  (if (= side :challenger)
    (cons (get-in @state [:challenger :identity]) (concat (get-in @state [:challenger :current]) (all-installed state side)))
    (cons (get-in @state [:contestant :identity]) (filter #(not (:disabled %))
                                                    (concat (all-installed state side)
                                                            (get-in @state [:contestant :current])
                                                            (get-in @state [:contestant :scored]))))))

(defn installed-byname
  "Returns a truthy card map if a card matching title is installed"
  [state side title]
  (some #(when (= (:title %) title) %) (all-installed state side)))

(defn in-play?
  "Returns a truthy card map if the given card is in play (installed)."
  [state card]
  (installed-byname state (to-keyword (:side card)) (:title card)))

(defn hand-size
  "Returns the current maximum handsize of the specified side."
  [state side]
  (let [side' (get @state side)
        base (get side' :hand-size-base 0)
        mod (get side' :hand-size-modification 0)]
    (+ base mod)))

(defn swap-agendas
  "Swaps the two specified agendas, first one scored (on contestant side), second one stolen (on challenger side)"
  [state side scored stolen]
  (let [contestant-ap-stolen (get-agenda-points state :contestant stolen)
        contestant-ap-scored (get-agenda-points state :contestant scored)
        challenger-ap-stolen (get-agenda-points state :challenger stolen)
        challenger-ap-scored (get-agenda-points state :challenger scored)
        contestant-ap-change (- contestant-ap-stolen contestant-ap-scored)
        challenger-ap-change (- challenger-ap-scored challenger-ap-stolen)]
    ;; Remove end of turn events for swapped out agenda
    (swap! state update-in [:contestant :register :end-turn]
           (fn [events] (filter #(not (= (:cid scored) (get-in % [:card :cid]))) events)))
    ;; Move agendas
    (swap! state update-in [:contestant :scored]
           (fn [coll] (conj (remove-once #(not= (:cid %) (:cid scored)) coll) stolen)))
    (swap! state update-in [:challenger :scored]
           (fn [coll] (conj (remove-once #(not= (:cid %) (:cid stolen)) coll)
                            (if-not (card-flag? scored :has-abilities-when-stolen true)
                              (dissoc scored :abilities :events) scored))))
    ;; Update agenda points
    (gain-agenda-point state :challenger challenger-ap-change)
    (gain-agenda-point state :contestant contestant-ap-change)
    ;; Set up abilities and events
    (let [new-scored (find-cid (:cid stolen) (get-in @state [:contestant :scored]))]
      (let [abilities (:abilities (card-def new-scored))
            new-scored (merge new-scored {:abilities abilities})]
        (update! state :contestant new-scored)
        (when-let [events (:events (card-def new-scored))]
          (unregister-events state side new-scored)
          (register-events state side events new-scored))
        (resolve-ability state side (:swapped (card-def new-scored)) new-scored nil)))
    (let [new-stolen (find-cid (:cid scored) (get-in @state [:challenger :scored]))]
      (deactivate state :contestant new-stolen))))

;;; Functions for icons associated with special cards - e.g. Femme Fatale
(defn add-icon
  "Adds an icon to a card. E.g. a Femme Fatale token.
  Card is the card adding the icon, target is card receiving the icon."
  [state side card target char color]
  ;; add icon
  (set-prop state side target :icon {:char char :color color :card card})
  ;; specify icon target on card
  (set-prop state side card :icon-target target))

(defn remove-icon
  "Remove the icon associated with the card and target."
  ([state side card] (remove-icon state side card (find-cid (-> card :icon-target :cid) (get-all-installed state))))
  ([state side card target]
   (set-prop state side target :icon nil)
   (set-prop state side card :icon-target nil)))
