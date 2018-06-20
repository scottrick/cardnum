(in-ns 'game.core)

(declare add-icon remove-icon can-host?)

(def breaker-auto-pump
  "Updates an icebreaker's abilities with a pseudo-ability to trigger the
  auto-pump routine in core, IF we are encountering a revealed character with a subtype
  we can break."
  {:effect
   (req (let [abs (filter #(not= (:dynamic %) :auto-pump) (:abilities card))
              pumpabi (some #(when (:pump %) %) abs)
              pumpcst (when pumpabi (second (drop-while #(and (not= % :credit)
                                                              (not= % "credit"))
                                                        (:cost pumpabi))))
              current-character (when-not (get-in @state [:run :ending]) (get-card state current-character))
              strdif (when current-character (max 0 (- (or (:current-strength current-character)
                                                     (:strength current-character))
                                                 (or (:current-strength card)
                                                     (:strength card)))))
              pumpnum (when strdif (int (Math/ceil (/ strdif (:pump pumpabi)))))]
          (update! state side
                   (assoc card :abilities
                          (if (and pumpcst
                                   pumpnum
                                   (revealed? current-character)
                                   (or (some #(has-subtype? current-character %) (:breaks card))
                                       (= (first (:breaks card)) "All"))
                                   (pos? strdif))
                            (vec (cons {:dynamic :auto-pump
                                        :cost [:credit (* pumpcst pumpnum)]
                                        :label (str "Match strength of " (:title current-character))}
                                       abs))
                            abs)))))})

;; Takes a vector of character subtypes that can be broken (or ["All"] for
;; AI breakers) and a card definition, and returns a new card definition that
;; hooks up breaker-auto-pump to the necessary events.
;; IMPORTANT: Events on cdef take precedence, and should call
;; (:effect breaker-auto-pump) themselves.
(defn auto-icebreaker [breaks cdef]
  (assoc cdef :data (merge {:breaks breaks} (:data cdef))
              :events (merge {:run breaker-auto-pump
                              :pass-character breaker-auto-pump
                              :run-ends breaker-auto-pump
                              :character-strength-changed breaker-auto-pump
                              :character-subtype-changed breaker-auto-pump
                              :breaker-strength-changed breaker-auto-pump
                              :approach-character breaker-auto-pump }
                             (:events cdef))))

(defn cloud-icebreaker [cdef]
  (assoc cdef :effect (req (add-watch state (keyword (str "cloud" (:cid card)))
                        (fn [k ref old new]
                          (when (and (< (get-in old [:challenger :link]) 2)
                                     (> (get-in new [:challenger :link]) 1))
                            (gain state :challenger :memory (:memoryunits card)))
                          (when (and (> (get-in old [:challenger :link]) 1)
                                     (< (get-in new [:challenger :link]) 2))
                            (lose state :challenger :memory (:memoryunits card))))))
              :leave-play (req (remove-watch state (keyword (str "cloud" (:cid card))))
                               (when (> (get-in @state [:challenger :link]) 1)
                                 (lose state :challenger :memory (:memoryunits card))))
              :install-cost-bonus (req (when (> (get-in @state [:challenger :link]) 1)
                                         [:memory (* -1 (:memoryunits card))]))))

(defn- strength-pump
  "Creates a strength pump ability.
  Cost can be a credit amount or a list of costs e.g. [:credit 2]."
  ([cost strength] (strength-pump cost strength nil))
  ([cost strength all-run]
   {:msg (str "add " strength " strength" (when all-run " for the remainder of the run"))
    :cost [:credit cost]
    :effect (effect (pump card strength (or all-run :encounter)))
    :pump strength}))

(defn- break-sub
  "Creates a break subroutine ability.
  If n = 0 then any number of subs are broken."
  ([cost n] (break-sub cost n nil))
  ([cost n subtype] (break-sub cost n subtype nil))
  ([cost n subtype effect]
   {:msg (str "break "
              (when (> n 1) "up to ")
              (if (pos? n) n "any number of")
              (when subtype (str " " subtype))
              (pluralize " subroutine" n))
    :cost [:credit cost]
    :effect effect}))

;;; Breaker sets
(defn- cerberus
  "Breaker from the dog set"
  [type]
  (auto-icebreaker [type]
                   {:data {:counter {:power 4}}
                    :abilities [{:counter-cost [:power 1]
                                 :msg (str "break up to 2 " (lower-case type) " subroutines")}
                                (strength-pump 1 1)]}))

(defn- break-and-enter
  "Breakers from the Break and Entry set"
  [type]
  (cloud-icebreaker {:abilities [{:label (str "[Trash]: Break up to 3 " (lower-case type) "subroutines")
                                  :msg (str "break up to 3 " (lower-case type) " subroutines")
                                  :effect (effect (trash card {:cause :ability-cost}))}]
                      :events (let [cloud {:silent (req true)
                                           :req (req (has-subtype? target "Icebreaker"))
                                           :effect (effect (update-breaker-strength card))}]
                                {:challenger-install cloud :trash cloud :card-moved cloud})
                      :strength-bonus (req (count (filter #(has-subtype? % "Icebreaker")
                                                          (all-installed state :challenger))))}))

(defn- global-sec-breaker
  "GlobalSec breakers for Sunny"
  [type]
  (cloud-icebreaker (auto-icebreaker [type] {:abilities [(break-sub 2 0 (lower-case type))
                                                         (strength-pump 2 3)]})))

(defn- deva
  "Deva breakers"
  [name]
  (auto-icebreaker ["All"]
                   {:abilities [(break-sub 1 1 "Character")
                                (strength-pump 1 1)
                                {:req (req (seq (filter #(has-subtype? % "Deva") (:hand challenger))))
                                 :label "Swap with a deva resource from your Grip" :cost [:credit 2]
                                 :prompt (str "Select a deva resource in your Grip to swap with " name)
                                 :choices {:req #(and in-hand? (has-subtype? % "Deva"))}
                                 :msg (msg "swap in " (:title target) " from their Grip")
                                 :effect (req (if-let [hostcard (:host card)]
                                                (let [hosted (host state side (get-card state hostcard) target)]
                                                  (card-init state side hosted {:resolve-effect false
                                                                                :init-data true}))
                                                (let [devavec (get-in @state [:challenger :rig :resource])
                                                      devaindex (first (keep-indexed #(when (= (:cid %2) (:cid card)) %1) devavec))
                                                      newdeva (assoc target :zone (:zone card) :installed true)
                                                      newvec (apply conj (subvec devavec 0 devaindex) newdeva (subvec devavec devaindex))]
                                                  (lose state :challenger :memory (:memoryunits card))
                                                  (swap! state assoc-in [:challenger :rig :resource] newvec)
                                                  (swap! state update-in [:challenger :hand] (fn [coll] (remove-once #(not= (:cid %) (:cid target)) coll)))
                                                  (card-init state side newdeva {:resolve-effect false
                                                                                 :init-data true})))
                                              (move state side card :hand))}]}))

(defn- conspiracy
  "Install-from-heap breakers"
  [title type abilities]
  (let [install-prompt {:req (req (and (= (:zone card) [:discard])
                                       (revealed? current-character)
                                       (has-subtype? current-character type)
                                       (not (install-locked? state side))
                                       (not (some #(= title (:title %)) (all-installed state :challenger)))
                                       (not (get-in @state [:run :register :conspiracy (:cid current-character)]))))
                        :optional {:player :challenger
                                   :prompt (str "Install " title "?")
                                   :yes-ability {:effect (effect (unregister-events card)
                                                                 (challenger-install :challenger card))}
                                   :no-ability {:effect (req  ;; Add a register to note that the player was already asked about installing,
                                                              ;; to prevent multiple copies from prompting multiple times.
                                                              (swap! state assoc-in [:run :register :conspiracy (:cid current-character)] true))}}}
        heap-event (req (when (= (:zone card) [:discard])
                          (unregister-events state side card)
                          (register-events state side
                                           {:reveal install-prompt
                                            :approach-character install-prompt
                                            :run install-prompt}
                                           (assoc card :zone [:discard]))))]
    {:move-zone heap-event
     :events {:reveal nil
              :approach-character nil
              :run nil}
     :abilities abilities}))

(defn- central-breaker
  "'Cannot be used on a party locale' breakers"
  [type break pump]
  (let [central-req (req (or (not (:central-breaker card)) (#{:hq :rd :archives} (first (:locale run)))))]
    (auto-icebreaker [type]
                     {:abilities [(assoc break :req central-req)
                                  (assoc pump :req central-req)]
                      :effect (effect (update! (assoc card :central-breaker true)))})))

;;; Icebreaker definitions
(def cards-icebreakers
  {})