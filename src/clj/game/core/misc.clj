(in-ns 'game.core)

(declare set-prop get-nested-host get-nested-zone all-active-installed)

(defn get-zones [state]
  (keys (get-in @state [:contestant :servers])))

(defn get-remote-zones [state]
  (filter is-remote? (get-zones state)))

(defn get-runnable-zones [state]
  (let [restricted-zones (keys (get-in @state [:challenger :register :cannot-run-on-server]))]
    (remove (set restricted-zones) (get-zones state))))

(defn get-remotes [state]
  (select-keys (get-in @state [:contestant :servers]) (get-remote-zones state)))

(defn get-remote-names [state]
  (zones->sorted-names (get-remote-zones state)))

(defn server-list
  "Get a list of all servers (including centrals)"
  [state]
  (zones->sorted-names (get-zones state)))

(defn installable-servers
  "Get list of servers the specified card can be installed in"
  [state card]
  (let [base-list (concat (server-list state) ["New remote"])]
    (if-let [install-req (-> card card-def :install-req)]
      ;; Install req function overrides normal list of install locations
      (install-req state :contestant card (make-eid state) base-list)
      ;; Standard list
      base-list)))


(defn server->zone [state server]
  (if (sequential? server)
    (vec (cons :servers server))
    (case server
      "HQ" [:servers :hq]
      "R&D" [:servers :rd]
      "Archives" [:servers :archives]
      "New remote" [:servers (keyword (str "remote" (make-rid state)))]
      [:servers (->> (split server #" ") last (str "remote") keyword)])))

(defn same-server?
  "True if the two cards are IN or PROTECTING the same server."
  [card1 card2]
  (let [zone1 (get-nested-zone card1)
        zone2 (get-nested-zone card2)]
    (= (second zone1) (second zone2))))

(defn protecting-same-server?
  "True if an character is protecting the server that the card is in or protecting."
  [card character]
  (let [zone1 (get-nested-zone card)
        zone2 (get-nested-zone character)]
    (and (= (second zone1) (second zone2))
         (= :characters (last zone2)))))

(defn in-same-server?
  "True if the two cards are installed IN the same server, or hosted on cards IN the same server."
  [card1 card2]
  (let [zone1 (get-nested-zone card1)
        zone2 (get-nested-zone card2)]
    (and (= zone1 zone2)
         (is-remote? (second zone1)) ; cards in centrals are in the server's root, not in the server.
         (= :content (last zone1)))))

(defn from-same-server?
  "True if the region is in the root of the server that the target is in."
  [region target]
  (= (central->zone (:zone target))
     (butlast (get-nested-zone region))))

(defn all-installed
  "Returns a vector of all installed cards for the given side, including those hosted on other cards,
  but not including 'inactive hosting' like Personal Workshop."
  [state side]
  (if (= side :challenger)
    (let [top-level-cards (flatten (for [t [:resource :hazard :muthereff :facedown]] (get-in @state [:challenger :rig t])))
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

(defn number-of-virus-counters
  "Returns number of actual virus counters (excluding virtual counters from Hivemind)"
  [state]
  (reduce + (map #(get-counters % :virus) (all-installed state :challenger))))

(defn all-active
  "Returns a vector of all active cards for the given side. Active cards are either installed, the identity,
  currents, or the contestant's scored area."
  [state side]
  (if (= side :challenger)
    (cons (get-in @state [:challenger :identity]) (concat (get-in @state [:challenger :current]) (all-active-installed state side)))
    (cons (get-in @state [:contestant :identity]) (remove :disabled
                                                    (concat (all-active-installed state side)
                                                            (get-in @state [:contestant :current])
                                                            (get-in @state [:contestant :scored]))))))

(defn all-active-installed
  "Returns a vector of active AND installed cards for the given side. This is all face-up installed cards."
  [state side]
  (let [installed (all-installed state side)]
   (if (= side :challenger)
     (remove :facedown installed)
     (filter :rezzed installed))))

(defn installed-byname
  "Returns a truthy card map if a card matching title is installed"
  [state side title]
  (some #(when (= (:title %) title) %) (all-active-installed state side)))

(defn in-play?
  "Returns a truthy card map if the given card is in play (installed)."
  [state card]
  (installed-byname state (to-keyword (:side card)) (:title card)))

;;; Stuff for handling {:base x :mod y} data structures

(defn base-mod-size
  "Returns the value of properties using the `base` and `mod` system"
  [state side prop]
  (let [base (get-in @state [side prop :base] 0)
        mod (get-in @state [side prop :mod] 0)]
    (+ base mod)))

(defn hand-size
  "Returns the current maximum hand-size of the specified side."
  [state side]
  (base-mod-size state side :hand-size))

(defn available-mu
  "Returns the available MU the challenger has"
  [state]
  (- (base-mod-size state :challenger :memory)
     (get-in @state [:challenger :memory :used] 0)))

(defn toast-check-mu
  "Check challenger has not exceeded, toast if they have"
  [state]
  (when (neg? (available-mu state))
    (toast state :challenger "You have exceeded your memory units!")))

(defn free-mu
  "Frees up specified amount of mu (reduces :used)"
  ([state _ n] (free-mu state n))
  ([state n]
   (deduct state :challenger [:memory {:used n}])))

(defn use-mu
  "Increases amount of mu used (increased :used)"
  ([state _ n] (use-mu state n))
  ([state n]
   (gain state :challenger :memory {:used n})))

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
           (fn [events] (filter #(not= (:cid scored) (get-in % [:card :cid])) events)))
    ;; Move agendas
    (swap! state update-in [:contestant :scored]
           (fn [coll] (conj (remove-once #(= (:cid %) (:cid scored)) coll) stolen)))
    (swap! state update-in [:challenger :scored]
           (fn [coll] (conj (remove-once #(= (:cid %) (:cid stolen)) coll)
                            (if-not (card-flag? scored :has-abilities-when-stolen true)
                              (dissoc scored :abilities :events) scored))))
    ;; Update agenda points
    (gain-agenda-point state :challenger challenger-ap-change)
    (gain-agenda-point state :contestant contestant-ap-change)
    ;; Set up abilities and events for new scored agenda
    (let [new-scored (find-cid (:cid stolen) (get-in @state [:contestant :scored]))
          abilities (:abilities (card-def new-scored))
          new-scored (merge new-scored {:abilities abilities})]
      (update! state :contestant new-scored)
      (when-let [events (:events (card-def new-scored))]
        (unregister-events state side new-scored)
        (register-events state side events new-scored))
      (resolve-ability state side (:swapped (card-def new-scored)) new-scored nil))
    ;; Set up abilities and events for new stolen agenda
    (when-not (card-flag? scored :has-events-when-stolen true)
      (let [new-stolen (find-cid (:cid scored) (get-in @state [:challenger :scored]))]
        (deactivate state :contestant new-stolen)))))

(defn remove-old-current
  "Removes the old current when a new one is played, or an agenda is stolen / scored"
  [state side current-side]
  (when-let [current (first (get-in @state [current-side :current]))] ; trash old current
    (if (get-in current [:special :rfg-when-trashed])
      (do (system-say state side (str (:title current) " is removed from the game."))
          (move state (other-side side) current :rfg))
      (do (system-say state side (str (:title current) " is trashed."))
          (trash state side current)))))

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
  ([state side card] (remove-icon state side card (:icon-target card)))
  ([state side card target]
   (when target (set-prop state side (find-latest state target) :icon nil))
   (set-prop state side (find-latest state card) :icon-target nil)))
