(ns game.cards.characterbreakers
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int]]
            [cardnum.cards :refer [all-cards]]))

(def breaker-auto-pump
  "Updates an characterbreaker's abilities with a pseudo-ability to trigger the
  auto-pump routine in core, IF we are encountering a rezzed character with a subtype
  we can break."
  {:effect
   (req (let [abs (filter #(not= (:dynamic %) :auto-pump) (:abilities card))
              pumpabi (some #(when (:pump %) %) abs)
              pumpcst (when pumpabi (second (drop-while #(and (not= % :credit)
                                                              (not= % "credit"))
                                                        (:cost pumpabi))))
              current-character (when-not (get-in @state [:run :ending]) (get-card state current-character))
              strdif (when (and (or (:current-strength current-character)
                                    (:strength current-character))
                                (or (:current-strength card)
                                    (:strength card)))
                       (max 0 (- (or (:current-strength current-character)
                                     (:strength current-character))
                                 (or (:current-strength card)
                                     (:strength card)))))
              pumpnum (when (and strdif
                                 (:pump pumpabi))
                        (int (Math/ceil (/ strdif (:pump pumpabi)))))]
          (update! state side
                   (assoc card :abilities
                          (if (and pumpcst
                                   pumpnum
                                   (rezzed? current-character)
                                   (or (some #(has-subtype? current-character %) (:breaks card))
                                       (= (first (:breaks card)) "All"))
                                   (pos? pumpnum))
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
(defn auto-characterbreaker [breaks cdef]
  (assoc cdef :data (merge {:breaks breaks} (:data cdef))
              :events (merge {:run breaker-auto-pump
                              :pass-character breaker-auto-pump
                              :run-ends breaker-auto-pump
                              :character-strength-changed breaker-auto-pump
                              :character-subtype-changed breaker-auto-pump
                              :breaker-strength-changed breaker-auto-pump
                              :approach-character breaker-auto-pump }
                             (:events cdef))))

(defn- wrestling-breaker
  "Laamb and Engolo. Makes currently encountered character gain chosen type until end of encounter."
  [cost character-type]
  {:once :per-turn
   :cost [:credit cost]
   :label (str "Make currently encountered character gain " character-type)
   :msg (msg "make " (:title current-character) " gain " character-type)
   :req (req (and current-character
                  (rezzed? current-character)
                  (not (has-subtype? current-character character-type))))
   :effect (req (let [character current-character
                      stargets (:subtype-target character)
                      stypes (:subtype character)
                      remove-subtype {:effect
                                      (effect (update! (assoc character
                                                              :subtype-target stargets
                                                              :subtype stypes))
                                              (unregister-events card)
                                              (register-events (:events (card-def card)) card))}]
                  (update! state side (assoc character
                                             :subtype-target (combine-subtypes true stargets character-type)
                                             :subtype (combine-subtypes true stypes character-type)))
                  (update-character-strength state side (get-card state character))
                  (register-events state side {:pass-character remove-subtype
                                               :run-ends remove-subtype} card)))})

(defn cloud-characterbreaker [cdef]
  (assoc cdef :effect (req (let [link (get-in @state [:challenger :link] 0)]
                             (when (>= link 2)
                               (free-mu state (:memoryunits card))))
                           (add-watch state (keyword (str "cloud" (:cid card)))
                                      (fn [k ref old new]
                                        (let [old-link (get-in old [:challenger :link] 0)
                                              new-link (get-in new [:challenger :link] 0)
                                              cloud-turned-on (and (< old-link 2)
                                                                   (>= new-link 2))
                                              cloud-turned-off (and (>= old-link 2)
                                                                    (< new-link 2))]
                                          (cond
                                            cloud-turned-on
                                            (free-mu state (:memoryunits card))

                                            cloud-turned-off
                                            (use-mu state (:memoryunits card)))))))
              :leave-play (req (remove-watch state (keyword (str "cloud" (:cid card))))
                               (let [link (get-in @state [:challenger :link] 0)]
                                 (when (>= link 2)
                                   ;; To counteract the normal freeing of MU on resource `:leave-play`
                                   (use-mu state (:memoryunits card)))))))

(defn- strength-pump
  "Creates a strength pump ability.
  Cost can be a credit amount or a list of costs e.g. [:credit 2]."
  ([cost strength] (strength-pump cost strength :encounter))
  ([cost strength duration]
   {:msg (str "add " strength " strength" (cond
                                            (= duration :all-run)
                                            " for the remainder of the run"
                                            (= duration :all-turn)
                                            " for the remainder of the turn"))
    :cost [:credit cost]
    :effect (effect (pump card strength duration))
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
  [character-type]
  (auto-characterbreaker [character-type]
                   {:data {:counter {:power 4}}
                    :abilities [{:counter-cost [:power 1]
                                 :msg (str "break up to 2 " (lower-case character-type) " subroutines")}
                                (strength-pump 1 1)]}))

(defn- break-and-enter
  "Breakers from the Break and Entry set"
  [character-type]
  (cloud-characterbreaker {:abilities [{:label (str "[Discard]: Break up to 3 " (lower-case character-type) "subroutines")
                                  :msg (str "break up to 3 " (lower-case character-type) " subroutines")
                                  :effect (effect (discard card {:cause :ability-cost}))}]
                      :events (let [cloud {:silent (req true)
                                           :req (req (has-subtype? target "Icebreaker"))
                                           :effect (effect (update-breaker-strength card))}]
                                {:challenger-install cloud :discard cloud :card-moved cloud})
                      :strength-bonus (req (count (filter #(has-subtype? % "Icebreaker")
                                                          (all-active-installed state :challenger))))}))

(defn- global-sec-breaker
  "GlobalSec breakers for Sunny"
  [character-type]
  (cloud-characterbreaker (auto-characterbreaker [character-type] {:abilities [(break-sub 2 0 (lower-case character-type))
                                                         (strength-pump 2 3)]})))

(defn- deva
  "Deva breakers"
  [card-name]
  (auto-characterbreaker ["All"]
                   {:abilities [(break-sub 1 1 "Character")
                                (strength-pump 1 1)
                                {:req (req (seq (filter #(has-subtype? % "Deva") (:hand challenger))))
                                 :label "Swap with a deva resource from your Grip"
                                 :cost [:credit 2]
                                 :prompt (str "Select a deva resource in your Grip to swap with " card-name)
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
                                                  (swap! state update-in [:challenger :hand] (fn [coll] (remove-once #(= (:cid %) (:cid target)) coll)))
                                                  (card-init state side newdeva {:resolve-effect false
                                                                                 :init-data true})))
                                              (move state side card :hand))}]}))

(defn- conspiracy
  "Install-from-heap breakers"
  [title character-type abilities]
  (let [install-prompt {:req (req (and (= (:zone card) [:discard])
                                       (rezzed? current-character)
                                       (has-subtype? current-character character-type)
                                       (not (install-locked? state :challenger))))
                        :async true
                        :effect (effect (continue-ability
                                          {:optional {:req (req (and (not-any? #(= title (:title %)) (all-active-installed state :challenger))
                                                                     (not (get-in @state [:run :register :conspiracy (:cid current-character)]))))
                                                      :player :challenger
                                                      :prompt (str "Install " title "?")
                                                      :yes-ability {:effect (effect (unregister-events card)
                                                                                    (challenger-install :challenger card))}
                                                      ;; Add a register to note that the player was already asked about installing,
                                                      ;; to prevent multiple copies from prompting multiple times.
                                                      :no-ability {:effect (req (swap! state assoc-in [:run :register :conspiracy (:cid current-character)] true))}}}
                                          card targets))}
        heap-event (req (when (= (:zone card) [:discard])
                          (unregister-events state side card)
                          (register-events state side
                                           {:rez install-prompt
                                            :approach-character install-prompt
                                            :run install-prompt}
                                           (assoc card :zone [:discard]))))]
    {:move-zone heap-event
     :events {:rez nil
              :approach-character nil
              :run nil}
     :abilities abilities}))

(defn- central-breaker
  "'Cannot be used on a remote server' breakers"
  [character-type break pump]
  (let [central-req (req (or (not (:central-breaker card)) (#{:hq :rd :archives} (first (:server run)))))]
    (auto-characterbreaker [character-type]
                     {:abilities [(assoc break :req central-req)
                                  (assoc pump :req central-req)]
                      :effect (effect (update! (assoc card :central-breaker true)))})))

(defn- ancient-greek-breaker
  "Adept, Sage and Savant. Strength depends on available memory units."
  [card-name abilities]
  {:abilities abilities
   :effect (req (add-watch state (keyword (str card-name (:cid card)))
                           (fn [k ref old new]
                             (when (not= (available-mu (atom old))
                                         (available-mu (atom new)))
                               (update-breaker-strength ref side card))))
                (update-breaker-strength state side card))
   :leave-play (req (remove-watch state (keyword (str card-name (:cid card)))))
   :strength-bonus (req (available-mu state))})

(defn- khumalo-breaker
  "Spends virus counters from any card to pump/break, gains virus counters for successful runs."
  [character-type]
  {:events {:successful-run {:silent (req true)
                             :effect (effect (system-msg "adds 1 virus counter to " (:title card))
                                             (add-counter card :virus 1))}}
   :abilities [{:label (str  "Break " character-type "subroutine(s)")
                :effect (req (wait-for (resolve-ability
                                         state side (pick-virus-counters-to-spend) card nil)
                                       (do (if-let [msg (:msg async-result)]
                                             (do (system-msg state :challenger
                                                             (str "spends " msg" to break " (:number async-result)
                                                                  " " character-type " subroutine(s)")))))))}
               {:label "Match strength of currently encountered character"
                :req (req (and current-character
                               (> (character-strength state side current-character)
                                  (or (:current-strength card) (:strength card)))))
                :effect (req (wait-for (resolve-ability
                                         state side
                                         (pick-virus-counters-to-spend
                                           (- (character-strength state side current-character)
                                              (or (:current-strength card) (:strength card))))
                                         card nil)
                                       (if-let [msg (:msg async-result)]
                                         (do (system-msg state :challenger (str "spends " msg " to add "
                                                                            (:number async-result) " strength"))
                                             (dotimes [_ (:number async-result)]
                                               (pump state side (get-card state card) 1))))))}
               {:label "Add strength"
                :effect (req (wait-for
                               (resolve-ability
                                 state side (pick-virus-counters-to-spend) card nil)
                               (if-let [msg (:msg async-result)]
                                 (do (system-msg state :challenger (str "spends " msg" to add "
                                                                    (:number async-result)
                                                                    " strength"))
                                     (dotimes [_ (:number async-result)]
                                       (pump state side (get-card state card) 1))))))}]})

;;; Icebreaker definitions
(def card-definitions
  {"Abagnale"
   (auto-characterbreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 2 2)
                                 {:label "Bypass Code Gate being encountered"
                                  :req (req (has-subtype? current-character "Code Gate"))
                                  :msg (msg "discard it and bypass " (:title current-character))
                                  :effect (effect (discard card {:cause :ability-cost}))}]})

   "Adept"
   (ancient-greek-breaker "adept" [{:cost [:credit 2]
                                    :req (req (or (has-subtype? current-character "Barrier")
                                                  (has-subtype? current-character "Sentry")))
                                    :msg "break 1 Sentry or Barrier subroutine"}])

   "Aghora"
   (deva "Aghora")

   "Alpha"
   (auto-characterbreaker ["All"]
                    {:abilities [{:cost [:credit 1]
                                  :req (req (= (:position run) (count run-characters)))
                                  :msg "break 1 subroutine on the outermost Character protecting this server"}
                                 (strength-pump 1 1)]})

   "Alias"
   (central-breaker "Sentry"
                    (break-sub 1 1 "Sentry")
                    (strength-pump 2 3))

   "Amina"
   (auto-characterbreaker ["Code Gate"]
                    {:abilities [(break-sub 2 3 "Code Gate")
                                 (strength-pump 2 3)
                                 {:label "Contestant loses 1 [Credits]"
                                  :req (req (and (has-subtype? current-character "Code Gate")
                                                 (rezzed? current-character)))
                                  :msg (msg "make the Contestant lose 1 [Credits]")
                                  :effect (effect (lose-credits :contestant 1))}]})

   "Ankusa"
   (auto-characterbreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "Barrier")
                                 (strength-pump 1 1)
                                 {:label "Add Barrier to HQ"
                                  :req (req (and (has-subtype? current-character "Barrier")
                                                 (rezzed? current-character)))
                                  :msg (msg "add " (:title current-character) " to HQ after breaking all its subroutines")
                                  :effect (req (let [c current-character]
                                                 (move state :contestant c :hand nil)
                                                 (continue state side nil)))}]})

   "Atman"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target))
    :abilities [(break-sub 1 1)]
    :strength-bonus (req (get-counters card :power))
    :events {:counter-added {:req (req (= :cid target) (:cid card))
                             :effect (effect (update-breaker-strength card))}}}

   "Aumakua"
   {:implementation "Add counters manually for access outside of a run or cards that replace access like Ash"
    ; We would need a :once :per-access key to make this work for Gang Sign etc.
    :abilities [(break-sub 1 1)
                {:label "Add a virus counter"
                 :effect (effect (system-msg "manually adds a virus counter to Aumakua")
                                 (add-counter card :virus 1))}]
    :strength-bonus (req (get-virus-counters state side card))
    :events {:run-ends {:req (req (and (not (or (get-in @state [:run :did-discard])
                                                (get-in @state [:run :did-steal])))
                                       (get-in @state [:run :did-access])))
                        :effect (effect (add-counter card :virus 1))}
             :expose {:effect (effect (add-counter card :virus 1))}
             :counter-added {:req (req (= :cid target) (:cid card))
                             :effect (effect (update-breaker-strength card))}}}

   "Aurora"
   (auto-characterbreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "Barrier")
                                 (strength-pump 2 3)]})

   "Baba Yaga"
   (let [host-click {:cost [:click 1]
                     :label "Install a non-AI characterbreaker on Baba Yaga"
                     :prompt "Choose a non-AI characterbreaker in your Grip to install on Baba Yaga"
                     :choices {:req #(and (has-subtype? % "Icebreaker")
                                          (not (has-subtype? % "AI"))
                                          (in-hand? %))}
                     :effect (effect (challenger-install target {:host-card card}))}
         host-free {:label "Host an installed non-AI characterbreaker on Baba Yaga"
                    :prompt "Choose an installed non-AI characterbreaker to host on Baba Yaga"
                    :choices {:req #(and (has-subtype? % "Icebreaker")
                                         (not (has-subtype? % "AI"))
                                         (installed? %))}
                    :effect (req (when (host state side card target)
                                   (gain :memory (:memoryunits target))))}
         gain-abis (req (let [new-abis (mapcat (fn [c] (map-indexed #(assoc %2 :dynamic :copy, :source (:title c)
                                                                               :index %1, :label (make-label %2))
                                                                    (filter #(not= :manual-state (:ability-type %))
                                                                            (:abilities (card-def c)))))
                                               (:hosted card))]
                          (update! state :challenger (assoc card :abilities (concat new-abis [host-click host-free])))))]
   {:abilities [host-click host-free]
    :hosted-gained gain-abis
    :hosted-lost gain-abis})

   "Battering Ram"
   (auto-characterbreaker ["Barrier"]
                    {:abilities [(break-sub 2 2 "Barrier")
                                 (strength-pump 1 1 :all-run)]})

   "Berserker"
   {:abilities [(break-sub 2 2 "Barrier")]
    :implementation "Number of subroutines on encountered Character has to be entered by challenger when Contestant chooses 'No More Action'"
    :events {:encounter-character {:req (req (and (= (:cid target) (:cid current-character))
                                            (has-subtype? target "Barrier")
                                            (rezzed? target)))
                             :async true
                             :effect (effect (continue-ability :challenger
                                               {:prompt "How many subroutines are on the encountered Barrier?"
                                                :choices {:number (req 10)}
                                                :async true
                                                :effect (effect (system-msg (str "pumps Berserker by " target " on encounter with the current Character"))
                                                                (pump card target))} card nil))}}}

   "BlacKat"
   {:implementation "Stealth credit restriction not enforced"
    :abilities [(break-sub 1 1 "Barrier")
                {:cost [:credit 1]
                 :msg "break up to 3 Barrier subroutines (using a stealth [Credits])"}
                (strength-pump 2 1)
                {:cost [:credit 2]
                 :msg "add 2 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 2)) :pump 2}]}

   "Black Orchestra"
   (conspiracy "Black Orchestra" "Code Gate"
               [{:cost [:credit 3]
                 :effect (effect (pump card 2)) :pump 2
                 :msg "add 2 strength and break up to 2 subroutines"}])

   "Blackstone"
   {:abilities [(break-sub 1 1 "Barrier")
                {:cost [:credit 3]
                 :msg "add 4 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 4 :all-run)) :pump 4}]}

   "Brahman"
   (auto-characterbreaker ["All"]
                    {:implementation "Adding non-virus resource to top of Stack is manual"
                     :abilities [(break-sub 1 2 "Character")
                                 (strength-pump 2 1)]})

   "Breach"
   (central-breaker "Barrier"
                    (break-sub 2 3 "Barrier")
                    (strength-pump 2 4))

   "Cerberus \"Cuj.0\" H3"
   (cerberus "Sentry")

   "Cerberus \"Rex\" H2"
   (cerberus "Code Gate")

   "Cerberus \"Lady\" H1"
   (cerberus "Barrier")

   "Chameleon"
   {:prompt "Choose one subtype"
    :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "choose " target)
    :effect (effect (update! (assoc card :subtype-target target)))
    :events {:challenger-turn-ends {:msg "add itself to Grip" :effect (effect (move card :hand))}}
    :abilities [{:cost [:credit 1] :msg (msg "break 1 " (:subtype-target card) " subroutine")}]}

   "Corroder"
   (auto-characterbreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 1 1)]})

   "Creeper"
   (cloud-characterbreaker
     (auto-characterbreaker ["Sentry"]
                      {:abilities [(break-sub 2 1 "Sentry")
                                   (strength-pump 1 1)]}))

   "Crowbar"
   (break-and-enter "Code Gate")

   "Crypsis"
   (auto-characterbreaker ["All"]
                    {:abilities [(break-sub 1 1 "Character" (effect (update! (assoc card :crypsis-broke true))))
                                 (strength-pump 1 1)
                                 {:cost [:click 1]
                                  :msg "place 1 virus counter"
                                  :effect (effect (add-counter card :virus 1))}]
                     :events (let [encounter-ends-effect
                                   {:req (req (:crypsis-broke card))
                                    :effect (req ((:effect breaker-auto-pump) state side eid card targets)
                                                 (if (pos? (get-counters card :virus))
                                                   (add-counter state side card :virus -1)
                                                   (discard state side card {:cause :self-discard}))
                                                 (update! state side (dissoc (get-card state card) :crypsis-broke)))}]
                               {:pass-character encounter-ends-effect
                                :run-ends encounter-ends-effect})
                     :move-zone (req (when (= [:discard] (:zone card))
                                       (update! state side (dissoc card :crypsis-broke))))})

   "Cyber-Cypher"
   (auto-characterbreaker ["Code Gate"]
                    {:prompt "Choose a server where this copy of Cyber-Cypher can be used:"
                     :msg (msg "target " target)
                     :choices (req servers)
                     :effect (effect (update! (assoc card :server-target target)))
                     :leave-play (effect (update! (dissoc card :server-target)))
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "Dagger"
   (auto-characterbreaker ["Sentry"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 5)]})

   "Dai V"
   (auto-characterbreaker ["All"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [{:cost [:credit 2]
                                  :msg "break all Character subroutines (using stealth [Credits])"}
                                 (strength-pump 1 1)]})

   "Darwin"
   {:flags {:challenger-phase-12 (req true)}
    :events {:purge {:effect (effect (update-breaker-strength card))}}
    :abilities [(break-sub 2 1 "Character")
                {:label "Place 1 virus counter (start of turn)"
                 :once :per-turn
                 :cost [:credit 1]
                 :msg "place 1 virus counter"
                 :req (req (:challenger-phase-12 @state))
                 :effect (effect (add-counter card :virus 1)
                                 (update-breaker-strength card))}]
    :strength-bonus (req (or (get-virus-counters state side card) 0))}

   "Demara"
   (auto-characterbreaker ["Barrier"]
                    {:abilities [(break-sub 2 2 "Barrier")
                                 (strength-pump 2 3)
                                 {:label "Bypass Barrier being encountered"
                                  :req (req (has-subtype? current-character "Barrier"))
                                  :msg (msg "discard it and bypass " (:title current-character))
                                  :effect (effect (discard card {:cause :ability-cost}))}]})

   "Deus X"
   {:interactions {:prevent [{:type #{:net}
                              :req (req true)}]}
    :abilities [{:msg "break any number of AP subroutines"
                 :effect (effect (discard card {:cause :ability-cost}))}
                {:msg "prevent any amount of net damage"
                 :effect (effect (discard card {:cause :ability-cost})
                                 (damage-prevent :net Integer/MAX_VALUE))}]}

   "Eater"
   (auto-characterbreaker ["All"]
                    {:abilities [{:cost [:credit 1]
                                  :msg "break Character subroutine and access 0 cards this run"
                                  :effect (effect (max-access 0))}
                                 (strength-pump 1 1)]})

   "Endless Hunger"
   {:abilities [{:label "Discard 1 installed card to break 1 \"End the run.\" subroutine"
                 :prompt "Select a card to discard for Endless Hunger"
                 :choices {:req #(and (= (:side %) "Challenger") (:installed %))}
                 :msg (msg "discard " (:title target)
                           " and break 1 \"[Subroutine] End the run.\" subroutine")
                 :effect (effect (discard target {:unpreventable true}))}]}
   "Engolo"
   (auto-characterbreaker
     ["Code Gate"]
     {:abilities [(break-sub 1 1 "Code Gate")
                  (strength-pump 2 4)
                  (wrestling-breaker 2 "Code Gate")]})

   "Faerie"
   (auto-characterbreaker ["Sentry"]
                    {:abilities [(break-sub 0 1 "Sentry" (effect (update! (assoc-in card [:special :faerie-used] true))))
                                 (strength-pump 1 1)]
                     :events {:pass-character {:req (req (get-in card [:special :faerie-used]))
                                         :effect (effect (discard card))}}})

   "Faust"
   {:abilities [{:label "Discard 1 card from Grip to break 1 subroutine"
                 :prompt "Select a card from your grip to discard for Faust"
                 :choices {:req in-hand?}
                 :msg (msg "discard " (:title target) " and break 1 subroutine")
                 :effect (effect (discard target {:unpreventable true}))}
                {:label "Discard 1 card from Grip to add 2 strength"
                 :prompt "Select a card from your grip to discard for Faust"
                 :choices {:req in-hand?}
                 :msg (msg "discard " (:title target) " and add 2 strength")
                 :effect (effect (discard target {:unpreventable true})
                                 (pump card 2))}]}

   "Fawkes"
   {:implementation "Stealth credit restriction not enforced"
    :abilities [(break-sub 1 1 "Sentry")
                {:label (str "X [Credits]: +X strength for the remainder of the run (using at least 1 stealth [Credits])")
                 :choices :credit
                 :prompt "How many credits?"
                 :effect (effect (pump card target :all-run))
                 :msg (msg "increase strength by " target " for the remainder of the run")}]}

   "Femme Fatale"
   (auto-characterbreaker ["Sentry"]
                    {:prompt "Select a piece of Character to target for bypassing"
                     :choices {:req character?}
                     :leave-play (req (remove-icon state side card))
                     :effect (req (let [character target
                                        serv (zone->name (second (:zone character)))]
                                    (add-icon state side card character "F" "blue")
                                    (system-msg state side
                                                (str "selects " (card-str state character)
                                                     " for Femme Fatale's bypass ability"))))
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1)]})

   "Flashbang"
   (auto-characterbreaker ["Sentry"]
                    {:abilities [(strength-pump 1 1)
                                 {:label "Derez a Sentry being encountered"
                                  :cost [:credit 6]
                                  :req (req (and (rezzed? current-character) (has-subtype? current-character "Sentry")))
                                  :msg (msg "derez " (:title current-character))
                                  :effect (effect (derez current-character))}]})

   "Force of Nature"
   (auto-characterbreaker ["Code Gate"]
                    {:abilities [(break-sub 2 2 "Code Gate")
                                 (strength-pump 1 1)]})

   "Garrote"
   (auto-characterbreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 1)]})

   "God of War"
   (auto-characterbreaker ["All"]
                    {:flags {:challenger-phase-12 (req true)}
                     :abilities [(strength-pump 2 1)
                                 {:counter-cost [:virus 1]
                                  :msg "break 1 subroutine"}
                                 {:label "Take 1 tag to place 2 virus counters (start of turn)"
                                  :once :per-turn
                                  :effect (req (wait-for (tag-challenger state :challenger 1)
                                                         (if (not (get-in @state [:tag :tag-prevent]))
                                                           (do (add-counter state side card :virus 2)
                                                               (system-msg state side
                                                                           (str "takes 1 tag to place 2 virus counters on God of War"))
                                                               (effect-completed state side eid))
                                                           (effect-completed state side eid))))}]})

   "Golden"
   (auto-characterbreaker ["Sentry"]
                    {:abilities [(break-sub 2 2 "Sentry")
                                 (strength-pump 2 4)
                                 {:label "Derez a Sentry and return Golden to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-character) (has-subtype? current-character "Sentry")))
                                  :msg (msg "derez " (:title current-character) " and return Golden to their Grip")
                                  :effect (effect (derez current-character)
                                                  (move card :hand))}]})

   "Gordian Blade"
   (auto-characterbreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1 :all-run)]})

   "Gingerbread"
   (auto-characterbreaker ["Tracer"]
                    {:abilities [(break-sub 1 1 "Tracer")
                                 (strength-pump 2 3)]})

   "GS Sherman M3"
   (global-sec-breaker "Barrier")

   "GS Shrike M2"
   (global-sec-breaker "Sentry")

   "GS Striker M1"
   (global-sec-breaker "Code Gate")

   "Houdini"
   {:abilities [(break-sub 1 1 "Code Gate")
                {:cost [:credit 2]
                 :msg "add 4 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 4 :all-run)) :pump 4}]}

   "Inti"
   (auto-characterbreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 2 1 :all-run)]})

   "Inversificator"
   (auto-characterbreaker ["Code Gate"]
                    {:implementation "No restriction on which pieces of Character are chosen"
                     :abilities [{:label "Swap the Code Gate you just passed with another Character"
                                  :once :per-turn
                                  :req (req (:run @state))
                                  :prompt "Select the Code Gate you just passed and another piece of Character to swap positions"
                                  :choices {:req #(and (installed? %) (character? %)) :max 2}
                                  :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                  :effect (req (when (= (count targets) 2)
                                                 (swap-character state side (first targets) (second targets))))}
                                 (break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "Knight"
   {:abilities [{:label "Host Knight on a piece of Character"
                 :effect (req (let [k (get-card state card)
                                    hosted (character? (:host k))
                                    characterpos (character-index state (get-card state (:host k)))]
                                (resolve-ability state side
                                 {:prompt (msg "Host Knight on a piece of Character" (when hosted " not before or after the current host Character"))
                                  :cost [:click 1]
                                  :choices {:req #(if hosted
                                                    (and (or (when (= (:zone %) (:zone (:host k)))
                                                               (not= 1 (abs (- (character-index state %) characterpos))))
                                                             (not= (:zone %) (:zone (:host k))))
                                                         (character? %)
                                                         (can-host? %)
                                                         (installed? %)
                                                         (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %))))
                                                    (and (character? %)
                                                         (installed? %)
                                                         (can-host? %)
                                                         (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %)))))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))} card nil)))}
                {:cost [:credit 2]
                 :req (req (character? (get-nested-host card)))
                 :msg "break 1 subroutine on the host Character"}]}

   "Laamb"
   (auto-characterbreaker
     ["Barrier"]
     {:abilities [(break-sub 2 0 "Barrier")
                  (strength-pump 3 6)
                  (wrestling-breaker 2 "Barrier")]})

   "Leviathan"
   (auto-characterbreaker ["Code Gate"]
                    {:abilities [(break-sub 3 3 "Code Gate")
                                 (strength-pump 3 5)]})

   "Lustig"
   (auto-characterbreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 5)
                                 {:label "Bypass Sentry being encountered"
                                  :req (req (has-subtype? current-character "Sentry"))
                                  :msg (msg "discard it and bypass " (:title current-character))
                                  :effect (effect (discard card {:cause :ability-cost}))}]})

   "Mammon"
   (auto-characterbreaker ["All"]
                    {:flags {:challenger-phase-12 (req (pos? (:credit challenger)))}
                     :abilities [{:label "X [Credits]: Place X power counters"
                                  :prompt "How many power counters to place on Mammon?" :once :per-turn
                                  :choices {:number (req (:credit challenger))}
                                  :req (req (:challenger-phase-12 @state))
                                  :effect (effect (lose-credits target)
                                                  (add-counter card :power target))
                                  :msg (msg "place " target " power counters on it")}
                                 {:counter-cost [:power 1]
                                  :label "Hosted power counter: Break Character subroutine"
                                  :msg "break 1 Character subroutine"}
                                 (strength-pump 2 2)]
                     :events {:challenger-turn-ends {:effect (effect (update! (assoc-in card [:counter :power] 0)))}}})

   "Mass-Driver"
   (auto-characterbreaker ["Code Gate"]
                    {:implementation "Prevention of subroutine resolution on next Character is manual"
                     :abilities [(break-sub 2 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "Maven"
   {:abilities [(break-sub 2 1 "Character")]
    :events (let [maven {:silent (req true)
                         :req (req (is-type? target "Resource"))
                         :effect (effect (update-breaker-strength card))}]
              {:challenger-install maven :discard maven :card-moved maven})
    :strength-bonus (req (count (filter #(is-type? % "Resource") (all-active-installed state :challenger))))}

   "Morning Star"
   {:abilities [(break-sub 1 0 "Barrier")]}

   "Mimic"
   {:abilities [(break-sub 1 1 "Sentry")]}

   "Mongoose"
   (auto-characterbreaker ["Sentry"]
                    {:implementation "Usage restriction is not implemented"
                     :abilities [(break-sub 1 2 "Sentry")
                                 (strength-pump 2 2)]})

   "MKUltra"
   (conspiracy "MKUltra" "Sentry"
               [{:cost [:credit 3]
                 :effect (effect (pump card 2)) :pump 2
                 :msg "add 2 strength and break up to 2 subroutines"}])

   "Musaazi"
   (khumalo-breaker "sentry")

   "NaNotK"
   (auto-characterbreaker ["Sentry"]
                    {:effect (req (add-watch state (keyword (str "nanotk" (:cid card)))
                                              (fn [k ref old new]
                                                (let [server (first (get-in @state [:run :server]))]
                                                  (when (or
                                                          ; run initiated or ended
                                                          (not= (get-in old [:run])
                                                                (get-in new [:run]))
                                                          ; server configuration changed (redirected or newly installed Character)
                                                          (not= (get-in old [:contestant :servers server :characters])
                                                                (get-in new [:contestant :servers server :characters])))
                                                    (update-breaker-strength ref side card))))))
                     :strength-bonus (req (if-let [numcharacter (count run-characters)] numcharacter 0))
                     :leave-play (req (remove-watch state (keyword (str "nanotk" (:cid card)))))
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 2)]})

   "Nfr"
   {:implementation "Adding power counter is manual"
    :abilities [{:label "Place 1 power counter on Nfr"
                 :msg "place 1 power counter on it"
                 :ability-type :manual-state
                 :effect (effect (add-counter card :power 1)
                                 (update-breaker-strength card))}
                (break-sub 1 1 "Barrier")]
    :strength-bonus (req (get-counters card :power))}

   "Ninja"
   (auto-characterbreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 5)]})

   "Omega"
   (auto-characterbreaker ["All"]
                    {:abilities [{:cost [:credit 1] :req (req (= 1 (:position run)))
                                  :msg "break 1 subroutine on the innermost Character protecting this server"}
                                 (strength-pump 1 1)]})

   "Overmind"
   (auto-characterbreaker ["All"]
                    {:effect (effect (add-counter card :power (available-mu state)))
                     :abilities [{:counter-cost [:power 1]
                                  :msg "break 1 subroutine"}
                                 (strength-pump 1 1)]})
   "Paperclip"
   (conspiracy "Paperclip" "Barrier"
               [{:label (str "X [Credits]: +X strength, break X subroutines")
                 :choices {:number (req (:credit challenger))
                           :default (req (if (:current-strength current-character)
                                           (max (- (:current-strength current-character)
                                                   (:current-strength card))
                                                1)
                                           1))}
                 :prompt "How many credits?"
                 :effect (effect (lose-credits target)
                                 (pump card target))
                 :msg (msg "spend " target " [Credits], increase strength by " target ", and break "
                           (quantify target "Barrier subroutine"))}])

   "Passport"
   (central-breaker "Code Gate"
                    (break-sub 1 1 "Code Gate")
                    (strength-pump 2 2))

   "Peacock"
   (auto-characterbreaker ["Code Gate"]
                    {:abilities [(break-sub 2 1 "Code Gate")
                                 (strength-pump 2 3)]})

   "Peregrine"
   (auto-characterbreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 3 3)
                                 {:label "Derez a Code Gate and return Peregrine to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-character) (has-subtype? current-character "Code Gate")))
                                  :msg (msg "derez " (:title current-character) " and return Peregrine to their Grip")
                                  :effect (effect (derez current-character)
                                                  (move card :hand))}]})

   "Persephone"
   (auto-characterbreaker ["Sentry"]
                    {:implementation "Requires challenger to input the number of subroutines allowed to resolve"
                     :abilities [(break-sub 2 1 "Sentry")
                                 (strength-pump 1 1)]
                     :events {:pass-character {:req (req (and (has-subtype? target "Sentry") (rezzed? target)) (pos? (count (:deck challenger))))
                                         :optional {:prompt (msg "Use Persephone's ability??")
                                                    :yes-ability {:prompt "How many subroutines resolved on the passed Character?"
                                                                  :async true
                                                                  :choices {:number (req 10)}
                                                                  :msg (msg (if (pos? target)
                                                                              (str "discard " (:title (first (:deck challenger))) " from their Stack and discard " target " cards from R&D")
                                                                              (str "discard " (:title (first (:deck challenger))) " from their Stack and nothing from R&D")))
                                                                  :effect (effect (mill :challenger)
                                                                                  (mill :challenger :contestant target))}}}}})

   "Pipeline"
   (auto-characterbreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1 :all-run)]})

   "Puffer"
   (auto-characterbreaker ["Sentry"]
                    {:implementation "Memory use must be manually tracked by the Challenger"
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1)
                                 {:cost [:click 1] :msg "place one power counter"
                                  :label "Place 1 power counter"
                                  :effect (effect (add-counter card :power 1)
                                                  (update-breaker-strength card))}
                                 {:cost [:click 1] :msg "remove one power counter"
                                  :label "Remove 1 power counter"
                                  :effect (effect (add-counter card :power -1)
                                                  (update-breaker-strength card))}]
                     :strength-bonus (req (get-counters card :power))})

   "Refractor"
   (auto-characterbreaker ["Code Gate"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 3)]})
   "Sadyojata"
   (deva "Sadyojata")

   "Sage"
   (ancient-greek-breaker "sage" [{:cost [:credit 2] :req (req (or (has-subtype? current-character "Barrier")
                                                                   (has-subtype? current-character "Code Gate")))
                                   :msg "break 1 Code Gate or Barrier subroutine"}])

   "Saker"
   (auto-characterbreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 2 2)
                                 {:label "Derez a Barrier and return Saker to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-character) (has-subtype? current-character "Barrier")))
                                  :msg (msg "derez " (:title current-character) " and return Saker to their Grip")
                                  :effect (effect (derez current-character)
                                                  (move card :hand))}]})

   "Savant"
   (ancient-greek-breaker "savant" [{:cost [:credit 2] :req (req (has-subtype? current-character "Sentry"))
                                     :msg "break 1 Sentry subroutine"}
                                    {:cost [:credit 2] :req (req (has-subtype? current-character "Code Gate"))
                                     :msg "break 2 Code Gate subroutines"}])

   "Snowball"
   (auto-characterbreaker ["Barrier"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 Barrier subroutine"
                                  :effect (effect (pump card 1 :all-run))}
                                 (strength-pump 1 1)]})

   "Sharpshooter"
   (auto-characterbreaker ["Destroyer"]
                    {:abilities [{:label "[Discard]: Break any number of Destroyer subroutines"
                                  :msg "break any number of Destroyer subroutines"
                                  :effect (effect (discard card {:cause :ability-cost}))}
                                 (strength-pump 1 2)]})

   "Shiv"
   (break-and-enter "Sentry")

   "Spike"
   (break-and-enter "Barrier")

   "Study Guide"
   {:abilities [(break-sub 1 1 "Code Gate")
                {:cost [:credit 2] :msg "place 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (update-breaker-strength card))}]
    :strength-bonus (req (get-counters card :power))}

   "Sūnya"
   {:implementation "Adding power counter is manual"
    :abilities [{:label "Place 1 power counter on Sūnya"
                 :ability-type :manual-state
                 :effect (effect (add-counter card :power 1)
                                 (system-msg (str "places 1 power counter on Sūnya"))
                                 (update-breaker-strength card))}
                (break-sub 2 1 "Sentry")]
    :strength-bonus (req (get-counters card :power))}

   "Switchblade"
   (auto-characterbreaker ["Sentry"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 0 "Sentry")
                                 (strength-pump 1 7)]})

   "Torch"
   (auto-characterbreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "Vamadeva"
   (deva "Vamadeva")

   "Wyrm"
   (auto-characterbreaker ["All"]
                    {:abilities [{:cost [:credit 3]
                                  :msg "break 1 subroutine on Character with 0 or less strength"}
                                 {:cost [:credit 1]
                                  :label "Give -1 strength to current Character"
                                  :req (req (rezzed? current-character))
                                  :msg (msg "give -1 strength to " (:title current-character))
                                  :effect (req (update! state side (update-in card [:wyrm-count] (fnil #(+ % 1) 0)))
                                               (update-character-strength state side current-character))}
                                 (strength-pump 1 1)]
                     :events (let [auto-pump (fn [state side eid card targets]
                                               ((:effect breaker-auto-pump) state side eid card targets))
                                   wy {:effect (effect (update! (dissoc card :wyrm-count))
                                                       (auto-pump eid (get-card state card) targets))}]
                               {:pre-character-strength {:req (req (and (= (:cid target) (:cid current-character))
                                                                  (:wyrm-count card)))
                                                   :effect (req (let [c (:wyrm-count (get-card state card))]
                                                                  (character-strength-bonus state side (- c) target)
                                                                  (auto-pump state side eid card targets)))}
                                :pass-character wy
                                :run-ends wy})})

   "Yusuf"
   (khumalo-breaker "barrier")

   "Yog.0"
   {:abilities [(break-sub 0 1 "Code Gate")]}

   "ZU.13 Key Master"
   (cloud-characterbreaker
     (auto-characterbreaker ["Code Gate"]
                      {:abilities [(break-sub 1 1 "Code Gate")
                                   (strength-pump 1 1)]}))})
