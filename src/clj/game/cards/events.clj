(in-ns 'game.core)

(defn- run-event
  ([] (run-event nil))
  ([run-ability] (run-event nil run-ability))
  ([cdef run-ability] (run-event cdef run-ability nil))
  ([cdef run-ability pre-run-effect]
   (run-event cdef run-ability pre-run-effect nil))
  ([cdef run-ability pre-run-effect post-run-effect]
   (merge {:prompt "Choose a server"
           :choices (req runnable-servers)
           :effect (effect ((or pre-run-effect (effect)) eid card targets)
                           (run target run-ability card)
                           ((or post-run-effect (effect)) eid card targets))}
          cdef)))

(def cards-events
  {"Account Siphon"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to lose " (min 5 (:credit minion))
                                         " [Credits], gain " (* 2 (min 5 (:credit minion)))
                                         " [Credits] and take 2 tags")
                               :delayed-completion true
                               :effect (req (when-completed (tag-hero state :hero 2)
                                                            (do (gain state :hero :credit (* 2 (min 5 (:credit minion))))
                                                                (lose state :minion :credit (min 5 (:credit minion)))
                                                                (effect-completed state side eid))))}} card))}

   "Amped Up"
   {:msg "gain [Click][Click][Click] and suffer 1 brain damage"
    :effect (effect (gain :click 3) (damage eid :brain 1 {:unpreventable true :card card}))}

   "Another Day, Another Paycheck"
   {:events {:agenda-stolen
             {:trace {:base 0
                      :unsuccessful {:effect (effect (gain :hero :credit
                                                           (+ (:agenda-point hero) (:agenda-point minion))))
                                     :msg (msg (str "gain " (+ (:agenda-point hero) (:agenda-point minion)) " [Credits]"))}}}}}

   "Apocalypse"
   {:req (req (and (some #{:hq} (:successful-run hero-reg))
                   (some #{:rd} (:successful-run hero-reg))
                   (some #{:archives} (:successful-run hero-reg))))
                           ;; trash cards from right to left
                           ;; otherwise, auto-killing servers would move the cards to the next server
                           ;; so they could no longer be trashed in the same loop
    :msg "trash all installed Corp cards and turn all installed Runner cards facedown"
    :effect (req (let [ai (all-installed state :minion)
                       onhost (filter #(= '(:onhost) (:zone %)) ai)
                       allminion (->> ai
                                    (remove #(= '(:onhost) (:zone %)))
                                    (sort-by #(vec (:zone %)))
                                    (reverse))]
                   ; Trash hosted cards first so they don't get trashed twice
                   (doseq [c onhost]
                     (trash state side c))
                   (doseq [c allminion]
                     (trash state side (get-card state c))))

                 ;; do hosted cards first so they don't get trashed twice
                 (doseq [c (all-installed state :hero)]
                   (when (or (= ["onhost"] (get c :zone)) (= '(:onhost) (get c :zone)))
                     (move state side c [:rig :facedown])
                     (if (:memoryunits c)
                       (gain state :hero :memory (:memoryunits c)))))
                 (doseq [c (all-installed state :hero)]
                   (when (not (or (= ["onhost"] (get c :zone)) (= '(:onhost) (get c :zone))))
                     (move state side c [:rig :facedown])
                     (if (:memoryunits c)
                       (gain state :hero :memory (:memoryunits c))))))}

   "Blackmail"
   (run-event
    {:req (req has-bad-pub)
     :msg "prevent ICE from being rezzed during this run"}
    nil
    (effect (register-run-flag!
              card
              :can-rez
              (fn [state side card]
                (if (ice? card)
                  ((constantly false)
                    (toast state :minion "Cannot rez ICE on this run due to Blackmail"))
                  true)))))

   "Bribery"
   {:prompt "How many credits?"
    :choices :credit
    :msg (msg "increase the rez cost of the first unrezzed ICE approached by " target " [Credits]")
    :effect (effect (resolve-ability (run-event) card nil))}

   "Brute-Force-Hack"
   {:implementation "Runner must calculate the right number of credits including other game effects for the planned target ICE"
    :prompt "How many [Credits]?" :choices :credit
    :effect (effect (system-msg (str "spends " target " [Credit] on Brute-Force-Hack"))
                    (resolve-ability {:choices {:req #(and (ice? %)
                                                           (rezzed? %)
                                                           (<= (:cost %) target))}
                                      :effect (effect (derez target))
                                      :msg (msg "derez " (:title target))} card nil))}

   "Build Script"
   {:msg "gain 1 [Credits] and draw 2 cards"
    :effect (effect (gain :credit 1) (draw 2))}

   "By Any Means"
   {:effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:hero-turn-ends {:effect (effect (unregister-events card))}
             :pre-access-card {:req (req (not= [:discard] (:zone target)))
                               :delayed-completion true
                               :msg (msg "trash " (:title target) " at no cost and suffer 1 meat damage")
                               :effect (req (when-completed (trash state side (assoc target :seen true) nil)
                                                            (do (swap! state assoc-in [:hero :register :trashed-card] true)
                                                                (damage state :hero eid :meat 1 {:unboostable true}))))}}}

   "Calling in Favors"
   {:msg (msg "gain " (count (filter #(and (has-subtype? % "Connection") (is-type? % "Resource"))
                                     (all-installed state :hero))) " [Credits]")
    :effect (effect (gain :credit (count (filter #(and (has-subtype? % "Connection") (is-type? % "Resource"))
                                                 (all-installed state :hero)))))}

   "Career Fair"
   {:prompt "Select a resource to install from your Grip"
    :choices {:req #(and (is-type? % "Resource")
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (hero-install target))}

   "Careful Planning"
   {:prompt  "Choose a card in or protecting a remote server"
    :choices {:req #(is-remote? (second (:zone %)))}
    :end-turn {:effect (effect (remove-icon card target))}
    :effect (effect (add-icon card target "CP" "red")
                    (system-msg (str "prevents the rezzing of " (card-str state target)
                                     " for the rest of this turn via Careful Planning"))
                    (register-turn-flag! card :can-rez
                                         (fn [state side card]
                                           (if (= (:cid card) (:cid target))
                                             ((constantly false)
                                               (toast state :minion "Cannot rez the rest of this turn due to Careful Planning"))
                                             true))))}

   "CBI Raid"
   (letfn [(cbi-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :delayed-completion true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :minion c :deck {:front true}))
                                 (clear-wait-prompt state :hero)
                                 (effect-completed state side eid card))
                             (continue-ability state side (cbi-choice original '() (count original) original)
                                               card nil)))})
           (cbi-choice [remaining chosen n original]
             {:prompt "Choose a card to move next onto R&D"
              :choices remaining
              :delayed-completion true
              :effect (req (let [chosen (cons target chosen)]
                             (if (< (count chosen) n)
                               (continue-ability state side (cbi-choice (remove-once #(not= target %) remaining)
                                                                        chosen n original) card nil)
                               (continue-ability state side (cbi-final chosen original) card nil))))})]
     {:req (req hq-runnable)
            :delayed-completion true
            :effect (effect (run :hq {:replace-access
                                {:msg "force the Corp to add all cards in HQ to the top of R&D"
                                 :delayed-completion true
                                 :effect (req (show-wait-prompt state :hero "Corp to add all cards in HQ to the top of R&D")
                                              (let [from (:hand minion)]
                                                (if (pos? (count from))
                                                  (continue-ability state :minion (cbi-choice from '() (count from) from) card nil)
                                                  (do (clear-wait-prompt state :hero)
                                                      (effect-completed state side eid card)))))}} card))})

   "Code Siphon"
   {:req (req rd-runnable)
    :effect (effect (run :rd
                         {:replace-access
                          {:delayed-completion true
                           :prompt "Choose a program to install"
                           :msg (msg "install " (:title target) " and take 1 tag")
                           :choices (req (filter #(is-type? % "Program") (:deck hero)))
                           :effect (effect (trigger-event :searched-stack nil)
                                           (shuffle! :deck)
                                           (install-cost-bonus [:credit (* -3 (count (get-in minion [:servers :rd :ices])))])
                                           (hero-install target)
                                           (tag-hero eid 1) )}} card))}

   "Cold Read"
   (let [end-effect {:prompt "Choose a program that was used during the run to trash "
                     :choices {:req #(card-is? % :type "Program")}
                     :msg (msg "trash " (:title target))
                     :effect (effect (trash target {:unpreventable true}))}]
     {:delayed-completion true
      :prompt "Choose a server"
      :recurring 4
      :choices (req runnable-servers)
      :effect (req (let [c (move state side (assoc card :zone '(:discard)) :play-area {:force true})]
                     (card-init state side c {:resolve-effect false})
                     (game.core/run state side (make-eid state) target
                                    {:end-run {:delayed-completion true
                                               :effect (effect (trash c)
                                                               (continue-ability end-effect card nil))}}
                                    c)))})

   "Corporate Scandal"
   {:msg "give the Corp 1 additional bad publicity"
    :implementation "No enforcement that this Bad Pub cannot be removed"
    :effect (req (swap! state update-in [:minion :has-bad-pub] inc))
    :leave-play (req (swap! state update-in [:minion :has-bad-pub] dec))}

   "Credit Crash"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:pre-access-card
             {:once :per-run
              :delayed-completion true
              :req (req (not= (:type target) "Agenda"))
              :effect (req (let [c target
                                 cost (:cost c)
                                 title (:title c)]
                             (if (can-pay? state :minion nil :credit cost)
                               (do (show-wait-prompt state :hero "Corp to decide whether or not to prevent the trash")
                                   (continue-ability state :minion
                                     {:optional
                                      {:delayed-completion true
                                       :prompt (msg "Spend " cost " [Credits] to prevent the trash of " title "?")
                                       :player :minion
                                       :yes-ability {:effect (req (lose state :minion :credit cost)
                                                                  (system-msg state :minion (str "spends " cost " [Credits] to prevent "
                                                                                               title " from being trashed at no cost"))
                                                                  (clear-wait-prompt state :hero))}
                                       :no-ability {:msg (msg "trash " title " at no cost")
                                                    :effect (effect (clear-wait-prompt :hero)
                                                                    (resolve-trash-no-cost c))}}}
                                    card nil))
                               (do (resolve-trash-no-cost state side c)
                                   (system-msg state side (str "uses Credit Crash to trash " title " at no cost"))
                                   (effect-completed state side eid)))))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Credit Kiting"
   {:req (req (some #{:hq :rd :archives} (:successful-run hero-reg)))
    :prompt "Select a card to install from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program")
                             (is-type? % "Resource"))
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -8])
                    (hero-install target)
                    (tag-hero 1))}

   "Cyber Threat"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :delayed-completion true
    :effect (req (let [serv target]
                   (continue-ability
                     state :minion
                     {:optional
                      {:prompt (msg "Rez a piece of ICE protecting " serv "?")
                       :yes-ability {:prompt (msg "Select a piece of ICE protecting " serv " to rez")
                                     :player :minion
                                     :choices {:req #(and (not (:rezzed %))
                                                          (= (last (:zone %)) :ices))}
                                     :effect (req (rez state :minion target nil))}
                       :no-ability {:effect (effect (game.core/run eid serv nil card))
                                    :msg (msg "make a run on " serv " during which no ICE can be rezzed")}}}
                    card nil)))}

   "Data Breach"
   {:req (req rd-runnable)
    :delayed-completion true
    :effect (req (let [db-eid (make-eid state)
                       events (:events (card-def card))]
                   (register-events state side
                                    (assoc-in events [:successful-run-ends :eid] db-eid)
                                    (assoc card :zone '(:discard)))
                   (when-completed (game.core/run state side db-eid :rd nil card)
                                   (let [card (get-card state (assoc card :zone '(:discard)))]
                                     (unregister-events state side card)
                                     (when (:run-again card)
                                       (game.core/run state side db-eid :rd nil card))
                                     (update! state side (dissoc card :run-again))))))
    :events {:successful-run-ends
             {:optional {:req (req (= [:rd] (:server target)))
                                              :prompt "Make another run on R&D?"
                                              :yes-ability {:effect (effect (clear-wait-prompt :minion)
                                                                            (update! (assoc card :run-again true)))}}}}}

   "Day Job"
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]" :effect (effect (gain :credit 10))}

   "Déjà Vu"
   {:prompt "Choose a card to add to Grip" :choices (req (cancellable (:discard hero) :sorted))
    :msg (msg "add " (:title target) " to their Grip")
    :effect (req (move state side target :hand)
                 (when (has-subtype? target "Virus")
                   (resolve-ability state side
                                    {:prompt "Choose a virus to add to Grip"
                                     :msg (msg "add " (:title target) " to their Grip")
                                     :choices (req (cancellable
                                                     (filter #(has-subtype? % "Virus") (:discard hero)) :sorted))
                                     :effect (effect (move target :hand))} card nil)))}

   "Deep Data Mining"
   {:req (req rd-runnable)
    :effect (effect (run :rd nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus (max 0 (min 4 (:memory hero))))) }
             :run-ends {:effect (effect (unregister-events card))}}}

   "Demolition Run"
   {:req (req (or rd-runnable hq-runnable))
    :prompt "Choose a server"
    :choices ["HQ" "R&D"]
    :abilities [{:msg (msg "trash " (:title (:card (first (get-in @state [side :prompt])))) " at no cost")
                 :effect (effect (trash-no-cost))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Demolition Run in the Temporary Zone to trash a card being accessed at no cost") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard hero)) :play-area)]
                                      (card-init state side c {:resolve-effect false})
                                      (register-events state side
                                                       {:run-ends {:effect (effect (trash c))}} c)))}
                     card nil))
    :events {:run-ends nil}}

   "Deuces Wild"
   (let [all [{:effect (effect (gain :credit 3))
               :msg "gain 3 [Credits]"}
              {:effect (effect (draw 2))
               :msg "draw 2 cards"}
              {:effect (effect (lose :tag 1))
               :msg "remove 1 tag"}
              {:prompt "Select 1 piece of ice to expose"
               :msg "expose 1 ice and make a run"
               :choices {:req #(and (installed? %) (ice? %))}
               :delayed-completion true
               :effect (req (when-completed (expose state side target)
                                            (continue-ability
                                              state side
                                              {:prompt "Choose a server"
                                               :choices (req runnable-servers)
                                               :delayed-completion true
                                               :effect (effect (game.core/run eid target))}
                                              card nil)))}]
         choice (fn choice [abis]
                  {:prompt "Choose an ability to resolve"
                   :choices (map #(capitalize (:msg %)) abis)
                   :delayed-completion true
                   :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                  (when-completed
                                    (resolve-ability state side chosen card nil)
                                    (if (= (count abis) 4)
                                      (continue-ability state side (choice (remove-once #(not= % chosen) abis)) card nil)
                                      (effect-completed state side eid)))))})]
     {:delayed-completion true
      :effect (effect (continue-ability (choice all) card nil))})

   "Dianas Hunt"
   {:implementation "One program per encounter not enforced"
    :prompt "Choose a server"
    :msg "make a run and install a program on encounter with each ICE"
    :choices (req runnable-servers)
    :delayed-completion true
    :abilities [{:label "Install a program using Diana's Hunt?"
                 :delayed-completion true
                 :effect (effect (resolve-ability
                                   {:prompt "Choose a program in your Grip to install"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (hero-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "install " (:title target))
                                    :effect (req (hero-install state side target {:no-cost true})
                                                 (swap! state update :diana #(conj % target)))}
                                   card nil))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Diana's Hunt in the Temporary Zone to install a Program") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard hero)) :play-area)]
                                      (card-init state side c {:resolve-effect false})
                                      (register-events state side
                                                       {:run-ends {:effect (req (let [hunt (:diana @state)]
                                                                                  (doseq [c hunt]
                                                                                    (when-let [installed (find-cid (:cid c) (all-installed state side))]
                                                                                      (system-msg state side (str "trashes " (:title c) " at the end of the run from Diana's Hunt"))
                                                                                      (trash state side installed {:unpreventable true})))
                                                                                  (swap! state dissoc :diana)
                                                                                  (unregister-events state side card)
                                                                                  (trash state side c)))}} c)))}
                      card nil))
    :events {:run-ends nil}}

   "Diesel"
   {:msg "draw 3 cards" :effect (effect (draw 3))}

   "Dirty Laundry"
   (run-event
    {:end-run {:req (req (:successful run))
               :msg "gain 5 [Credits]"
               :effect (effect (gain :hero :credit 5))}})

   "Drive By"
   {:choices {:req #(let [topmost (get-nested-host %)]
                     (and (is-remote? (second (:zone topmost)))
                          (= (last (:zone topmost)) :content)
                          (not (:rezzed %))))}
    :delayed-completion true
    :effect (req (when-completed (expose state side target) ;; would be nice if this could return a value on completion
                                 (if async-result ;; expose was successful
                                   (if (#{"Asset" "Upgrade"} (:type target))
                                     (do (system-msg state :hero (str "uses Drive By to trash " (:title target)))
                                         (trash state side (assoc target :seen true))
                                         (effect-completed state side eid))
                                     (effect-completed state side eid))
                                   (effect-completed state side eid))))}

   "Early Bird"
   (run-event
    {:msg (msg "make a run on " target " and gain [Click]")}
    nil
    (effect (gain :click 1)))

   "Easy Mark"
   {:msg "gain 3 [Credits]" :effect (effect (gain :credit 3))}

   "Emergency Shutdown"
   {:req (req (some #{:hq} (:successful-run hero-reg)))
    :msg (msg "derez " (:title target))
    :choices {:req #(and (ice? %)
                         (rezzed? %))}
    :effect (effect (derez target))}

   "Emergent Creativity"
   (letfn [(ec [trash-cost to-trash]
             {:delayed-completion true
             :prompt "Choose a hardware or program to install"
             :msg (msg "trash " (if (empty? to-trash) "no cards" (join ", " (map :title to-trash)))
                       " and install " (:title target) " lowering the cost by " trash-cost)
             :choices (req (cancellable (filter #(or (is-type? % "Program")
                                                     (is-type? % "Hardware"))
                                                (:deck hero)) :sorted))
             :effect (req (trigger-event state side :searched-stack nil)
                          (shuffle! state side :deck)
                          (doseq [c to-trash]
                            (trash state side c {:unpreventable true}))
                          (install-cost-bonus state side [:credit (- trash-cost)])
                          (hero-install state side target)
                          (effect-completed state side eid card))})]
   {:prompt "Choose Hardware and Programs to trash from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program"))
                         (in-hand? %))
              :max (req (count (:hand hero)))}
    :cancel-effect (effect (resolve-ability (ec 0 []) card nil))
    :effect (req (let [trash-cost (apply + (map :cost targets))
                       to-trash targets]
                   (resolve-ability state side (ec trash-cost to-trash) card nil)))})

   "Employee Strike"
   {:msg "disable the Corp's identity"
    :disable-id true
    :effect (effect (disable-identity :minion))
    :leave-play (effect (enable-identity :minion))}

   "Encore"
   {:req (req (and (some #{:hq} (:successful-run hero-reg))
                   (some #{:rd} (:successful-run hero-reg))
                   (some #{:archives} (:successful-run hero-reg))))
    :effect (req (swap! state update-in [:hero :extra-turns] (fnil inc 0))
                 (move state side (first (:play-area hero)) :rfg))
    :msg "take an additional turn after this one"}

   "En Passant"
   {:req (req (:successful-run hero-reg))
    :effect (req (let [runtgt (first (flatten (turn-events state side :run)))
                       serv (zone->name runtgt)]
                   (resolve-ability state side
                     {:prompt (msg "Choose an unrezzed piece of ICE protecting " serv " that you passed on your last run")
                      :choices {:req #(and (ice? %)
                                           (not (rezzed? %)))}
                      :msg (msg "trash " (card-str state target))
                      :effect (effect (trash target))}
                    card nil)))}

   "Escher"
   (letfn [(es [] {:prompt "Select two pieces of ICE to swap positions"
                   :choices {:req #(and (installed? %) (ice? %)) :max 2}
                   :effect (req (if (= (count targets) 2)
                                  (do (swap-ice state side (first targets) (second targets))
                                      (resolve-ability state side (es) card nil))
                                  (system-msg state side "has finished rearranging ICE")))})]
     {:req (req hq-runnable)
            :effect (effect (run :hq {:replace-access
                                {:msg "rearrange installed ICE"
                                 :effect (effect (resolve-ability (es) card nil))}} card))})

   "Eureka!"
   {:effect (req (let [topcard (first (:deck hero))
                       caninst (or (is-type? topcard "Hardware")
                                   (is-type? topcard "Program")
                                   (is-type? topcard "Resource"))]
                   (if caninst
                     (resolve-ability
                       state side
                       {:optional {:prompt (msg "Install " (:title topcard) "?")
                                   :yes-ability {:effect (effect (install-cost-bonus [:credit -10])
                                                                 (hero-install topcard))}
                                   :no-ability {:effect (effect (trash topcard {:unpreventable true})
                                                                (system-msg (str "reveals and trashes "
                                                                                 (:title topcard))))}}} card nil)
                     (do (trash state side topcard {:unpreventable true})
                         (system-msg state side (str "reveals and trashes " (:title topcard)))))))}

   "Exclusive Party"
   {:msg (msg "draw 1 card and gain "
              (count (filter #(= (:title %) "Exclusive Party") (:discard hero)))
              " [Credits]")
    :effect (effect (draw) (gain :credit (count (filter #(= (:title %) "Exclusive Party") (:discard hero)))))}

   "Executive Wiretaps"
   {:msg (msg "reveal cards in HQ: " (join ", " (map :title (:hand minion))))}

   "Exploit"
   {:req (req (and (some #{:hq} (:successful-run hero-reg))
                   (some #{:rd} (:successful-run hero-reg))
                   (some #{:archives} (:successful-run hero-reg))))
    :prompt "Choose up to 3 pieces of ICE to derez"
    :choices {:max 3 :req #(and (rezzed? %) (ice? %))}
    :msg (msg "derez " (join ", " (map :title targets)))
    :effect (req (doseq [c targets]
                   (derez state side c)))}

   "Exploratory Romp"
   (run-event
     {:replace-access {:prompt "Advancements to remove from a card in or protecting this server?"
                       :choices ["0", "1", "2", "3"]
                       :delayed-completion true
                       :effect (req (let [c (Integer/parseInt target)]
                                      (show-wait-prompt state :minion "Runner to remove advancements")
                                      (continue-ability state side
                                        {:choices {:req #(and (contains? % :advance-counter)
                                                              (= (first (:server run)) (second (:zone %))))}
                                         :msg (msg "remove " (quantify c "advancement token")
                                                   " from " (card-str state target))
                                         :effect (req (add-prop state :minion target :advance-counter (- c))
                                                      (clear-wait-prompt state :minion)
                                                      (effect-completed state side eid))}
                                        card nil)))}})

   "Express Delivery"
   {:prompt "Choose a card to add to your Grip" :choices (req (take 4 (:deck hero)))
    :msg "look at the top 4 cards of their Stack and add 1 of them to their Grip"
    :effect (effect (move target :hand) (shuffle! :deck))}

   "Falsified Credentials"
   {:prompt "Choose a type"
    :choices ["Agenda" "Asset" "Upgrade"]
    :delayed-completion true
    :effect (effect
             (continue-ability
              (let [chosen-type target]
                {:choices {:req #(let [topmost (get-nested-host %)]
                                   (and (is-remote? (second (:zone topmost)))
                                        (= (last (:zone topmost)) :content)
                                        (not (rezzed? %))))}
                 :delayed-completion true
                 :effect (req             ;taken from Drive By - maybe refactor
                          (when-completed (expose state side target)
                            (if (and async-result ;; expose was successful
                                     (= chosen-type (:type target)))
                              (continue-ability
                                  state :hero
                                  {:effect (effect (gain :credit 5))
                                   :msg "gain 5 [Credits] "}
                                  card nil)
                              (effect-completed state side eid))))})
              card nil))}


   "Fear the Masses"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:delayed-completion true
                               :mandatory true
                               :msg "force the Corp to trash the top card of R&D"
                               :effect (req (mill state :minion)
                                            (let [n (count (filter #(= (:title card) (:title %)) (:hand hero)))]
                                              (if (> n 0)
                                                (continue-ability state side
                                                  {:prompt "Reveal how many copies of Fear the Masses?"
                                                   :choices {:number (req n)}
                                                   :effect (req (when (> target 0)
                                                                  (mill state :minion target)
                                                                  (system-msg state side
                                                                              (str "reveals " target " copies of Fear the Masses,"
                                                                                   " forcing the Corp to trash " target " cards"
                                                                                   " from the top of R&D"))))}
                                                 card nil)
                                                (effect-completed state side eid card))))}} card))}

   "Feint"
   {:req (req hq-runnable)
    :implementation "Bypass is manual"
    :effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:msg "access 0 cards"
                              :effect (effect (max-access 0))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Fisk Investment Seminar"
   {:msg "make each player draw 3 cards"
    :effect (effect (draw 3) (draw :minion 3))}

   "Forged Activation Orders"
   {:choices {:req #(and (ice? %)
                         (not (rezzed? %)))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       icepos (ice-index state ice)]
                   (resolve-ability
                     state :minion
                     {:prompt (msg "Rez " (:title ice) " at position " icepos
                                   " of " serv " or trash it?") :choices ["Rez" "Trash"]
                      :effect (effect (resolve-ability
                                        (if (and (= target "Rez") (<= (rez-cost state :minion ice) (:credit minion)))
                                          {:msg (msg "force the rez of " (:title ice))
                                           :effect (effect (rez :minion ice))}
                                          {:msg (msg "trash the ICE at position " icepos " of " serv)
                                           :effect (effect (trash :minion ice))})
                                        card nil))}
                     card nil)))}

   "Forked"
   {:implementation "Ice trash is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}

   "Frame Job"
   {:prompt "Choose an agenda to forfeit"
    :choices (req (:scored hero))
    :effect (effect (forfeit target) (gain :minion :bad-publicity 1))
    :msg (msg "forfeit " (:title target) " and give the Corp 1 bad publicity")}

   "Frantic Coding"
   {:delayed-completion true
    :events {:hero-shuffle-deck nil}
    :effect
    (req (let [topten (take 10 (:deck hero))]
           (prompt! state :hero card (str "The top 10 cards of the Stack are "
                                            (join ", " (map :title topten))) ["OK"] {})
           (continue-ability
             state side
             {:prompt "Install a program?"
              :choices (conj (vec (sort-by :title (filter #(and (is-type? % "Program")
                                                                (can-pay? state side nil
                                                                          (modified-install-cost state side % [:credit -5])))
                                                          topten))) "No install")
              :delayed-completion true
              :effect (req (if (not= target "No install")
                             (do (register-events state side
                                                  {:hero-shuffle-deck
                                                   {:effect (effect (update! (assoc card :shuffle-occurred true)))}}
                                                  (assoc card :zone '(:discard)))
                                 (install-cost-bonus state side [:credit -5])
                                 (let [to-trash (remove #(= (:cid %) (:cid target)) topten)]
                                   (when-completed (hero-install state side target nil)
                                                   (let [card (get-card state (assoc card :zone '(:discard)))]
                                                     (if (not (:shuffle-occurred card))
                                                       (do (system-msg state side (str "trashes " (join ", " (map :title to-trash))))
                                                           (doseq [c to-trash] (trash state side c {:unpreventable true}))
                                                           (effect-completed state side eid))
                                                       (do (system-msg state side "does not have to trash cards because the stack was shuffled")
                                                           (effect-completed state side eid)))))))
                             (do (doseq [c topten] (trash state side c {:unpreventable true}))
                                 (system-msg state side (str "trashes " (join ", " (map :title topten)))))))} card nil)))}

   "\"Freedom Through Equality\""
   {:events {:agenda-stolen {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :effect (effect (as-agenda :hero card 1))}}}

   "Freelance Coding Contract"
   {:choices {:max 5
              :req #(and (is-type? % "Program")
                         (in-hand? %))}
    :msg (msg "trash " (join ", " (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :effect (req (doseq [c targets]
                   (trash state side c {:unpreventable true}))
                 (gain state side :credit (* 2 (count targets))))}

   "Game Day"
   {:msg (msg "draw " (- (hand-size state :hero) (count (:hand hero))) " cards")
    :effect (effect (draw (- (hand-size state :hero) (count (:hand hero)))))}

   "Government Investigations"
   {:flags {:psi-prevent-spend (req 2)}}

   "Hacktivist Meeting"
   {:implementation "Does not prevent rez if HQ is empty"
    :events {:rez {:req (req (and (not (ice? target)) (< 0 (count (:hand minion)))))
                   ;; FIXME the above condition is just a bandaid, proper fix would be preventing the rez altogether
                   :msg "force the Corp to trash 1 card from HQ at random"
                   :effect (effect (trash (first (shuffle (:hand minion)))))}}}

   "High-Stakes Job"
   (run-event
    {:choices (req (let [unrezzed-ice #(seq (filter (complement rezzed?) (:ices (second %))))
                         bad-zones (keys (filter (complement unrezzed-ice) (get-in @state [:minion :servers])))]
                     (zones->sorted-names (remove (set bad-zones) (get-runnable-zones @state)))))}
    {:end-run {:req (req (:successful run))
               :msg "gain 12 [Credits]"
               :effect (effect (gain :hero :credit 12))}})

   "Hostage"
   {:prompt "Choose a Connection"
    :choices (req (cancellable (filter #(has-subtype? % "Connection") (:deck hero)) :sorted))
    :msg (msg "add " (:title target) " to their Grip and shuffle their Stack")
    :effect (req (let [connection target]
                   (trigger-event state side :searched-stack nil)
                   (resolve-ability
                     state side
                     {:prompt (str "Install " (:title connection) "?")
                      :choices ["Yes" "No"]
                      :effect (req (let [d target]
                                     (resolve-ability state side
                                       {:effect (req (shuffle! state side :deck)
                                                     (if (= "Yes" d)
                                                       (hero-install state side connection)
                                                       (move state side connection :hand)))} card nil)))}
                     card nil)))}

   "Ive Had Worse"
   {:effect (effect (draw 3))
    :trash-effect {:when-inactive true
                   :req (req (#{:meat :net} target))
                   :effect (effect (draw :hero 3)) :msg "draw 3 cards"}}

   "Immolation Script"
   {:req (req archives-runnable)
    :effect (effect (run :archives nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:pre-access
             {:delayed-completion true
              :req (req (and (= target :archives)
                             ;; don't prompt unless there's at least 1 rezzed ICE matching one in Archives
                             (not-empty (clojure.set/intersection
                                          (into #{} (map :title (filter #(ice? %) (:discard minion))))
                                          (into #{} (map :title (filter #(rezzed? %) (all-installed state :minion))))))))
              :effect (req (continue-ability state side
                             {:delayed-completion true
                              :prompt "Choose a piece of ICE in Archives"
                              :choices (req (filter ice? (:discard minion)))
                              :effect (req (let [icename (:title target)]
                                             (continue-ability state side
                                               {:delayed-completion true
                                                :prompt (msg "Select a rezzed copy of " icename " to trash")
                                                :choices {:req #(and (ice? %)
                                                                     (rezzed? %)
                                                                     (= (:title %) icename))}
                                                :msg (msg "trash " (card-str state target))
                                                :effect (req (trash state :minion target)
                                                             (unregister-events state side card)
                                                             (effect-completed state side eid))} card nil)))}
                            card nil))}}}

   "Independent Thinking"
   (letfn [(cards-to-draw [targets]
             (* (count targets)
                (if (some #(and (not (facedown? %)) (has-subtype? % "Directive")) targets) 2 1)))]
     {:delayed-completion true
      :prompt "Choose up to 5 installed cards to trash with Independent Thinking"
      :choices {:max 5
                :req #(and (installed? %)
                           (= (:side %) "Hero"))}
      :effect (req (when-completed (trash-cards state side targets nil)
                                   (draw state :hero (cards-to-draw targets))))
      :msg (msg "trash " (join ", " (map :title targets)) " and draw " (quantify (cards-to-draw targets) "card"))})

   "Indexing"
   {:req (req rd-runnable)
    :delayed-completion true
    :effect (effect (run :rd
                         {:req (req (= target :rd))
                          :replace-access
                          {:msg "rearrange the top 5 cards of R&D"
                           :delayed-completion true
                           :effect (req (show-wait-prompt state :minion "Runner to rearrange the top cards of R&D")
                                        (let [from (take 5 (:deck minion))]
                                          (if (pos? (count from))
                                            (continue-ability state side (reorder-choice :minion :minion from '()
                                                                                         (count from) from) card nil)
                                            (do (clear-wait-prompt state :minion)
                                                (effect-completed state side eid card)))))}} card))}

   "Infiltration"
   {:prompt "Gain 2 [Credits] or expose a card?" :choices ["Gain 2 [Credits]" "Expose a card"]
    :effect (effect (continue-ability (if (= target "Expose a card")
                                        {:choices {:req installed?}
                                         :delayed-completion true
                                         :effect (effect (expose eid target))}
                                         {:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))})
                                      card nil))}

   "Information Sifting"
   (letfn [(access-pile [cards pile]
             {:prompt "Choose a card to access. You must access all cards."
              :choices [(str "Card from pile " pile)]
              :delayed-completion true
              :effect (req (when-completed
                             (handle-access state side [(first cards)])
                             (do (if (< 1 (count cards))
                                   (continue-ability state side (access-pile (next cards) pile) card nil)
                                   (effect-completed state side eid card)))))})
           (which-pile [p1 p2]
             {:prompt "Choose a pile to access"
              :choices [(str "Pile 1 (" (count p1) " cards)") (str "Pile 2 (" (count p2) " cards)")]
              :delayed-completion true
              :effect (req (let [choice (if (.startsWith target "Pile 1") 1 2)]
                             (clear-wait-prompt state :minion)
                             (system-msg state side (str "chooses to access " target))
                             (continue-ability state side
                                (access-pile (if (= 1 choice) p1 p2) choice)
                                card nil)))})]
     (let [access-effect
           {:delayed-completion true
            :mandatory true
            :effect (req (if (< 1 (count (:hand minion)))
                           (do (show-wait-prompt state :hero "Corp to create two piles")
                               (continue-ability
                                 state :minion
                                 {:delayed-completion true
                                  :prompt (msg "Select up to " (dec (count (:hand minion))) " cards for the first pile")
                                  :choices {:req #(and (in-hand? %) (card-is? % :side :minion))
                                            :max (req (dec (count (:hand minion))))}
                                  :effect (effect (clear-wait-prompt :hero)
                                                  (show-wait-prompt :minion "Runner to select a pile")
                                                  (continue-ability
                                                    :hero
                                                    (which-pile (shuffle targets)
                                                                (shuffle (vec (clojure.set/difference
                                                                                (set (:hand minion)) (set targets)))))
                                                    card nil))
                                  } card nil))
                           (effect-completed state side eid card)))}]
       {:req (req hq-runnable)
        :effect (effect (run :hq {:req (req (= target :hq))
                                  :replace-access access-effect}
                             card))}))

   "Inject"
   {:effect (req (doseq [c (take 4 (get-in @state [:hero :deck]))]
                   (if (is-type? c "Program")
                     (do (trash state side c {:unpreventable true})
                         (gain state side :credit 1)
                         (system-msg state side (str "trashes " (:title c) " and gains 1 [Credits]")))
                     (do (move state side c :hand)
                         (system-msg state side (str "adds " (:title c) " to Grip"))))))}

   "Injection Attack"
   (run-event
    {:delayed-completion true}
    nil
    nil
    (effect (continue-ability
             {:prompt "Select an icebreaker"
              :choices {:req #(and (installed? %) (has-subtype? % "Icebreaker"))}
              :effect (effect (pump target 2 :all-run))}
             card nil)))

   "Inside Job"
   {:implementation "Bypass is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}

   "Interdiction"
   (let [ab (effect (register-turn-flag!
                     card :can-rez
                     (fn [state side card]
                       (if (and (= (:active-player @state) :hero) (not (ice? card)))
                         ((constantly false)
                          (toast state :minion "Cannot rez non-ICE on the Runner's turn due to Interdiction"))
                         true))))]
     {:msg "prevent the Corp from rezzing non-ICE cards on the Runner's turn"
      :effect ab
      :events {:hero-turn-begins {:effect ab}}
      :leave-play (req (clear-all-flags-for-card! state side card))})

   "Itinerant Protesters"
   {:msg "reduce the Corp's maximum hand size by 1 for each bad publicity"
    :effect (req (lose state :minion :hand-size-modification (:bad-publicity minion))
                 (add-watch state :itin
                   (fn [k ref old new]
                     (let [bpnew (get-in new [:minion :bad-publicity])
                           bpold (get-in old [:minion :bad-publicity])]
                       (when (> bpnew bpold)
                         (lose state :minion :hand-size-modification (- bpnew bpold)))
                       (when (< bpnew bpold)
                         (gain state :minion :hand-size-modification (- bpold bpnew)))))))
    :leave-play (req (remove-watch state :itin)
                     (gain state :minion :hand-size-modification (:bad-publicity minion)))}

   "Knifed"
   {:implementation "Ice trash is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}

   "Kraken"
   {:req (req (:stole-agenda hero-reg)) :prompt "Choose a server" :choices (req servers)
    :msg (msg "force the Corp to trash an ICE protecting " target)
    :effect (req (let [serv (next (server->zone state target))
                       servname target]
                   (resolve-ability
                     state :minion
                     {:prompt (msg "Select a piece of ICE in " target " to trash")
                      :choices {:req #(and (= (last (:zone %)) :ices)
                                           (= serv (rest (butlast (:zone %)))))}
                      :effect (req (trash state :minion target)
                                   (system-msg state side (str "trashes "
                                    (card-str state target))))}
                    card nil)))}

   "Lawyer Up"
   {:msg "remove 2 tags and draw 3 cards"
    :effect (effect (draw 3) (lose :tag 2))}

   "Lean and Mean"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :delayed-completion true
    :msg (msg "make a run on " target (when (< (count (filter #(is-type? % "Program") (all-installed state :hero))) 4)
                                        ", adding +2 strength to all icebreakers"))
    :effect (req (when (< (count (filter #(is-type? % "Program") (all-installed state :hero))) 4)
                   (doseq [c (filter #(has-subtype? % "Icebreaker") (all-installed state :hero))]
                     (pump state side c 2 :all-run)))
                 (game.core/run state side (make-eid state) target nil card))}

   "Leave No Trace"
   {:prompt "Choose a server"
    :msg "make a run and derez any ICE that are rezzed during this run"
    :choices (req runnable-servers)
    :delayed-completion true
    :effect (req
              (let [old-ice (filter #(and (rezzed? %) (is-type? % "ICE")) (all-installed state :minion))]
                (swap! state assoc :lnt old-ice)
                (register-events state side (:events (card-def card)) (assoc card :zone '(:discard)))
                (game.core/run state side (make-eid state) target nil card)))
    :events {:run-ends {:effect (req (let [new (set (filter #(and (rezzed? %) (is-type? % "ICE")) (all-installed state :minion)))
                                           old (set (:lnt @state))
                                           diff (seq (clojure.set/difference new old))]
                                       (doseq [ice diff]
                                         (derez state side ice))
                                       (when-not (empty? diff)
                                         (system-msg state side (str "derezzes " (join ", " (map :title diff)) " via Leave No Trace")))
                                       (swap! state dissoc :lnt)
                                       (unregister-events state side card)))}}}

   "Legwork"
   {:req (req hq-runnable)
    :effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Leverage"
   {:req (req (some #{:hq} (:successful-run hero-reg)))
    :player :minion
    :prompt "Take 2 bad publicity?"
    :choices ["Yes" "No"]
    :effect (req (if (= target "Yes")
                   (do (gain state :minion :bad-publicity 2) (system-msg state :minion "takes 2 bad publicity"))
                   (do (register-events state side
                                        {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                      (damage-prevent :meat Integer/MAX_VALUE)
                                                                      (damage-prevent :brain Integer/MAX_VALUE))}
                                         :hero-turn-begins {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard)))
                       (system-msg state :hero "is immune to damage until the beginning of the Runner's next turn"))))
    ; This :events is a hack so that the unregister-events above will fire.
    :events {:hero-turn-begins nil :pre-damage nil}}

   "Levy AR Lab Access"
   {:msg "shuffle their Grip and Heap into their Stack and draw 5 cards"
    :effect (effect (shuffle-into-deck :hand :discard) (draw 5)
                    (move (first (:play-area hero)) :rfg))}

   "Lucky Find"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain :credit 9))}

   "Mad Dash"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :delayed-completion true
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:agenda-stolen {:silent (req true)
                             :effect (effect (update! (assoc card :steal true)))}
             :run-ends {:effect (req (if (:steal card)
                                       (do (as-agenda state :hero (get-card state card) 1)
                                           (system-msg state :hero
                                                       (str "adds Mad Dash to their score area as an agenda worth 1 agenda point")))
                                       (do (system-msg state :hero
                                                       (str "suffers 1 meat damage from Mad Dash"))
                                                       (damage state side eid :meat 1 {:card card})))
                                     (unregister-events state side card))}}}

   "Making an Entrance"
   (letfn [(entrance-trash [cards]
             {:prompt "Choose a card to trash"
              :choices (cons "None" cards)
              :delayed-completion true
              :msg (req (when (not= target "None") (str "trash " (:title target))))
              :effect (req (if (= target "None")
                             (if (not-empty cards)
                               (continue-ability state side (reorder-choice :hero :minion cards '()
                                                                            (count cards) cards) card nil)
                               (do (clear-wait-prompt state :minion)
                                   (effect-completed state side eid card)))
                             (do (trash state side target {:unpreventable true})
                                 (continue-ability state side (entrance-trash (remove-once #(not= % target) cards))
                                                   card nil))))})]
     {:msg "look at and trash or rearrange the top 6 cards of their Stack"
      :delayed-completion true
      :effect (req (show-wait-prompt state :minion "Runner to rearrange the top cards of their stack")
                   (let [from (take 6 (:deck hero))]
                     (continue-ability state side (entrance-trash from) card nil)))})

   "Mars for Martians"
   {:msg (msg "draw " (count (filter #(and (has-subtype? % "Clan") (is-type? % "Resource"))
                                     (all-installed state :hero)))
              " cards and gain " (:tag hero) " [Credits]")
    :effect (effect (draw (count (filter #(and (has-subtype? % "Clan") (is-type? % "Resource"))
                                         (all-installed state :hero))))
                    (gain :credit (:tag hero)))}

   "Mass Install"
   (let [mhelper (fn mi [n] {:prompt "Select a program to install"
                             :choices {:req #(and (is-type? % "Program")
                                                  (in-hand? %))}
                             :effect (req (hero-install state side target)
                                            (when (< n 3)
                                              (resolve-ability state side (mi (inc n)) card nil)))})]
     {:effect (effect (resolve-ability (mhelper 1) card nil))})

   "Mining Accident"
   (letfn [(mining [] {:player :minion
                       :delayed-completion true
                       :prompt "Pay 5 [Credits] or take 1 Bad Publicity?"
                       :choices ["Pay 5 [Credits]" "Take 1 Bad Publicity"]
                       :effect (req (cond

                                      (and (= target "Pay 5 [Credits]") (can-pay? state :minion nil :credit 5))
                                      (do (lose state :minion :credit 5)
                                          (system-msg state side "pays 5 [Credits] from Mining Accident")
                                          (clear-wait-prompt state :hero)
                                          (effect-completed state side eid))

                                      (= target "Pay 5 [Credits]")
                                      (do (can-pay? state :minion "Mining Accident" :credit 5)
                                          (continue-ability state side (mining) card nil))

                                      (= target "Take 1 Bad Publicity")
                                      (do (gain state :minion :bad-publicity 1)
                                          (system-msg state side "takes 1 Bad Publicity from Mining Accident")
                                          (clear-wait-prompt state :hero)
                                          (effect-completed state side eid))))})]
   {:req (req (some #{:hq :rd :archives} (:successful-run hero-reg)))
    :delayed-completion true
    :effect (req (move state side (first (:play-area hero)) :rfg)
                 (show-wait-prompt state :hero "Corp to choose to pay or take bad publicity")
                 (continue-ability state side (mining) card nil))
    :msg "make the Corp pay 5 [Credits] or take 1 Bad Publicity"})

   "Möbius"
   {:req (req rd-runnable)
    :delayed-completion true
    :effect (req (let [mob-eid (make-eid state)
                       events (:events (card-def card))]
                   (register-events state side
                                    (assoc-in events [:successful-run-ends :eid] mob-eid)
                                    (assoc card :zone '(:discard)))
                   (when-completed (game.core/run state side mob-eid :rd nil card)
                                   (let [card (get-card state (assoc card :zone '(:discard)))]
                                     (unregister-events state side card)
                                     (when (:run-again card)
                                       (game.core/run state side mob-eid :rd nil card)
                                       (register-events state side {:successful-run
                                                                   {:req (req (= target :rd))
                                                                    :msg "gain 4 [Credits]"
                                                                     :effect (effect (gain :credit 4)
                                                                                     (unregister-events card))}}

                                                        (assoc card :zone '(:discard))))
                                     (update! state side (dissoc card :run-again))))))
    :events {:successful-run nil
             :successful-run-ends {
                                   :interactive (req true)
                                   :optional {:req (req (= [:rd] (:server target)))
                                              :prompt "Make another run on R&D?"
                                              :yes-ability {:effect (effect (clear-wait-prompt :minion)
                                                                            (update! (assoc card :run-again true)))}}}}}

   "Modded"
   {:prompt "Select a program or piece of hardware to install from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program"))
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (hero-install target))}

   "Net Celebrity"
   {:recurring 1}

   "Networking"
   {:msg "remove 1 tag"
    :effect (effect (lose :tag 1))
    :optional {:prompt "Pay 1 [Credits] to add Networking to Grip?"
               :yes-ability {:cost [:credit 1]
                             :msg "add it to their Grip"
                             :effect (effect (move (last (:discard hero)) :hand))}}}

   "Notoriety"
   {:req (req (and (some #{:hq} (:successful-run hero-reg))
                   (some #{:rd} (:successful-run hero-reg))
                   (some #{:archives} (:successful-run hero-reg))))
    :effect (effect (as-agenda :hero (first (:play-area hero)) 1))
    :msg "add it to their score area as an agenda worth 1 agenda point"}

   "On the Lam"
   {:req (req (some #(is-type? % "Resource") (all-installed state :hero)))
    :prompt "Choose a resource to host On the Lam"
    :choices {:req #(and (is-type? % "Resource")
                         (installed? %))}
    :effect (effect (host target (assoc card :zone [:discard]))
                    (system-msg (str "hosts On the Lam on " (:title target))))
    :prevent {:tag [:all] :damage [:meat :net :brain]}
    :abilities [{:label "[Trash]: Avoid 3 tags"
                 :msg "avoid up to 3 tags"
                 :effect (effect (tag-prevent 3) (trash card {:cause :ability-cost}))}
                {:label "[Trash]: Prevent up to 3 damage"
                 :msg "prevent up to 3 damage"
                 :effect (effect (damage-prevent :net 3)
                                 (damage-prevent :meat 3)
                                 (damage-prevent :brain 3)
                                 (trash card {:cause :ability-cost}))}]}

   "Out of the Ashes"
   (let [ashes-run {:prompt "Choose a server"
                    :choices (req runnable-servers)
                    :delayed-completion true
                    :effect (effect (run eid target nil card))}
         ashes-recur (fn ashes-recur [n]
                       {:prompt "Remove Out of the Ashes from the game to make a run?"
                        :choices ["Yes" "No"]
                        :effect (req (if (= target "Yes")
                                       (let [card (some #(when (= "Out of the Ashes" (:title %)) %) (:discard hero))]
                                         (system-msg state side "removes Out of the Ashes from the game to make a run")
                                         (move state side card :rfg)
                                         (unregister-events state side card)
                                         (when-completed (resolve-ability state side ashes-run card nil)
                                                         (if (< 1 n)
                                                           (continue-ability state side (ashes-recur (dec n)) card nil)
                                                           (effect-completed state side eid card))))))})
         ashes-flag {:hero-phase-12 {:priority -1
                                       :once :per-turn
                                       :once-key :out-of-ashes
                                       :effect (effect (continue-ability
                                                         (ashes-recur (count (filter #(= "Out of the Ashes" (:title %))
                                                                                     (:discard hero))))
                                                         card nil))}}]
   (run-event
    {:move-zone (req (if (= [:discard] (:zone card))
                       (register-events state side ashes-flag (assoc card :zone [:discard]))
                       (unregister-events state side card)))
     :events {:hero-phase-12 nil}}
    nil))

   "Paper Tripping"
   {:msg "remove all tags" :effect (effect (lose :tag :all))}

   "Peace in Our Time"
   {:req (req (not (:scored-agenda minion-reg)))
    :msg "gain 10 [Credits]. The Corp gains 5 [Credits]"
    :effect (req (gain state :hero :credit 10)
                 (gain state :minion :credit 5)
                 (apply prevent-run-on-server
                        state card (get-zones @state))
                 (register-events state side
                   {:hero-turn-ends {:effect (req (apply enable-run-on-server state card (get-zones @state)))}}
                  (assoc card :zone '(:discard))))
    :events {:hero-turn-ends nil}}

   "Planned Assault"
   {:msg (msg "play " (:title target))
    :choices (req (cancellable (filter #(and (has-subtype? % "Run")
                                             (<= (:cost %) (:credit hero))) (:deck hero)) :sorted))
    :prompt "Choose a Run event" :effect (effect (trigger-event :searched-stack nil)
                                                 (shuffle! :deck)
                                                 (play-instant target {:no-additional-cost true}))}

   "Political Graffiti"
   (let [update-agenda-points (fn [state side target amount]
                               (set-prop state side (get-card state target) :agendapoints (+ amount (:agendapoints (get-card state target))))
                               (gain-agenda-point state side amount))]
     {:req (req archives-runnable)
      :events {:purge {:effect (effect (trash card))}}
      :trash-effect {:effect (req (let [current-side (get-scoring-owner state {:cid (:agenda-cid card)})]
                                    (update-agenda-points state current-side (find-cid (:agenda-cid card) (get-in @state [current-side :scored])) 1)))}
      :effect (effect (run :archives
                        {:req (req (= target :archives))
                         :replace-access
                         {:prompt "Select an agenda to host Political Graffiti"
                          :choices {:req #(in-minion-scored? state side %)}
                          :msg (msg "host Political Graffiti on " (:title target) " as a hosted condition counter")
                          :effect (req (host state :hero (get-card state target)
                                         ; keep host cid in :agenda-cid because `trash` will clear :host
                                         (assoc card :zone [:discard] :installed true :agenda-cid (:cid (get-card state target))))
                                       (update-agenda-points state :minion target -1))}} card))})

   "Populist Rally"
   {:req (req (seq (filter #(has-subtype? % "Seedy") (all-installed state :hero))))
    :msg "give the Corp 1 fewer [Click] to spend on their next turn"
    :effect (effect (lose :minion :click-per-turn 1)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:minion-turn-ends {:effect (effect (gain :minion :click-per-turn 1)
                                              (unregister-events card))}}}

   "Power Nap"
   {:effect (effect (gain :credit (+ 2 (count (filter #(has-subtype? % "Double")
                                                      (:discard hero))))))
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard hero)))) " [Credits]")}

   "Power to the People"
   {:effect (effect (register-events {:pre-steal-cost
                                      {:once :per-turn :effect (effect (gain :credit 7))
                                                       :msg "gain 7 [Credits]"}
                                      :hero-turn-ends
                                      {:effect (effect (unregister-events card))}}
                    (assoc card :zone '(:discard))))
    :events {:pre-steal-cost nil :hero-turn-ends nil}}

   "Prey"
   (run-event)

   "Process Automation"
   {:msg "gain 2 [Credits] and draw 1 card"
    :effect (effect (gain :credit 2) (draw 1))}

   "Push Your Luck"
   {:effect (effect (show-wait-prompt :hero "Corp to guess Odd or Even")
                    (resolve-ability
                      {:player :minion :prompt "Guess whether the Runner will spend an Odd or Even number of credits with Push Your Luck"
                       :choices ["Even" "Odd"]
                       :msg "force the Corp to make a guess"
                       :effect (req (let [guess target]
                                      (clear-wait-prompt state :hero)
                                      (resolve-ability
                                        state :hero
                                        {:choices :credit :prompt "How many credits?"
                                         :msg (msg "spend " target " [Credits]. The Corp guessed " guess)
                                         :effect (req (when (or (and (= guess "Even") (odd? target))
                                                                (and (= guess "Odd") (even? target)))
                                                        (system-msg state :hero (str "gains " (* 2 target) " [Credits]"))
                                                        (gain state :hero :credit (* 2 target))))} card nil)))}
                      card nil))}

   "Pushing the Envelope"
   (letfn [(hsize [s] (count (get-in s [:hero :hand])))]
   {:msg (msg (if (<= (hsize @state) 2)
           "make a run, and adds +2 strength to installed icebreakers"
           "make a run"))
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :delayed-completion true
    :effect (req (when (<= (hsize @state) 2)
                   (let [breakers (filter #(has-subtype? % "Icebreaker") (all-installed state :hero))]
                     (doseq [t breakers] (pump state side t 2 :all-run))))
                 (game.core/run state side (make-eid state) target))})

   "Quality Time"
   {:msg "draw 5 cards" :effect (effect (draw 5))}

   "Queens Gambit"
   {:choices ["0", "1", "2", "3"] :prompt "How many advancement tokens?"
    :effect (req (let [c (Integer/parseInt target)]
                   (resolve-ability
                     state side
                     {:choices {:req #(and (is-remote? (second (:zone %)))
                                           (= (last (:zone %)) :content)
                                           (not (:rezzed %)))}
                      :msg (msg "add " c " advancement tokens on a card and gain " (* 2 c) " [Credits]")
                      :effect (effect (gain :credit (* 2 c))
                                      (add-prop :minion target :advance-counter c {:placed true})
                                      (register-turn-flag! card :can-access
                                                           ;; prevent access of advanced card
                                                           (fn [_ _ card] (not (same-card? target card)))))}
                     card nil)))}

   "Quest Completed"
   {:req (req (and (some #{:hq} (:successful-run hero-reg))
                   (some #{:rd} (:successful-run hero-reg))
                   (some #{:archives} (:successful-run hero-reg))))
    :choices {:req installed?} :msg (msg "access " (:title target))
    :effect (effect (handle-access targets))}

   "Rebirth"
   {:msg "change identities"
    :prompt "Choose an identity to become"
    :choices (req (let [is-swappable (fn [c] (and (= "Identity" (:type c))
                                             (= (-> @state :hero :identity :faction) (:faction c))
                                             (not (.startsWith (:code c) "00")) ; only draft identities have this
                                             (not (= (:title c) (-> @state :hero :identity :title)))))
                        swappable-ids (filter is-swappable (vals @all-cards))]
                    (cancellable swappable-ids :sorted)))

     :effect (req
               ;; Handle Ayla - Part 1
               (when (-> @state :hero :identity :code (= "13012"))
                 (doseq [c (-> @state :hero :identity :hosted)]
                   (move state side c :temp-nvram)))

               (move state side (last (:discard hero)) :rfg)
               (disable-identity state side)

               ;; Manually reduce the hero's link by old link
               (lose state :hero :link (get-in @state [:hero :identity :baselink]))

               ;; Move the selected ID to [:hero :identity] and set the zone
               (swap! state update-in [side :identity]
                  (fn [x] (assoc (server-card (:title target) (get-in @state [:hero :user]))
                            :zone [:identity])))

               ;; enable-identity does not do everything that init-identity does
               (init-identity state side (get-in @state [:hero :identity]))
               (system-msg state side "NOTE: passive abilities (Kate, Gabe, etc) will incorrectly fire
                if their once per turn condition was met this turn before Rebirth was played.
                Please adjust your game state manually for the rest of this turn if necessary")

               ;; Handle Ayla - Part 2
               (when-not (empty? (-> @state :hero :temp-nvram))
                 (doseq [c (-> @state :hero :temp-nvram)]
                   (host state side (get-in @state [:hero :identity]) c {:facedown true}))))}

   "Recon"
   (run-event)

   "Reshape"
   {:prompt "Select two non-rezzed ICE to swap positions"
    :choices {:req #(and (installed? %) (not (rezzed? %)) (ice? %)) :max 2}
    :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
    :effect (req (when (= (count targets) 2)
                   (swap-ice state side (first targets) (second targets))))}

   "Retrieval Run"
   {:req (req archives-runnable)
    :effect (effect (run :archives
                      {:req (req (= target :archives))
                       :replace-access
                       {:prompt "Choose a program to install"
                        :msg (msg "install " (:title target))
                        :choices (req (filter #(is-type? % "Program") (:discard hero)))
                        :effect (effect (hero-install target {:no-cost true}))}} card))}

   "Rigged Results"
   (letfn [(choose-ice []
             {:prompt "Select a piece of ICE to bypass"
              :choices {:req #(ice? %)}
              :msg (msg "bypass " (card-str state target))
              :effect (final-effect (run (second (:zone target))))})
           (minion-choice [spent]
             {:prompt "Guess how many credits were spent"
              :choices ["0" "1" "2"]
              :delayed-completion true
              :effect (req (system-msg state :hero (str "spends " spent "[Credit]. "
                                       (-> minion :user :username) " guesses " target "[Credit]"))
                           (clear-wait-prompt state :hero)
                           (lose state :hero :credit spent)
                           (if (not= (str spent) target)
                             (continue-ability state :hero (choose-ice) card nil)
                             (effect-completed state side eid)))})
           (hero-choice [cr]
             {:prompt "Spend how many credits?"
              :choices (take cr ["0" "1" "2"])
              :delayed-completion true
              :effect (effect (show-wait-prompt :hero "Corp to guess")
                              (clear-wait-prompt :minion)
                              (continue-ability :minion (minion-choice (Integer/parseInt target)) card nil))})]
   {:effect (effect (show-wait-prompt :minion "Runner to spend credits")
                    (continue-ability (hero-choice (inc (min 2 (:credit hero)))) card nil))})

   "Rip Deal"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                                   {:delayed-completion true
                                    :effect (req (let [n (min (-> @state :minion :hand count) (access-count state side :hq-access))
                                                       heap (-> @state :hero :discard count (- 1))]
                                                   (move state side (find-cid (:cid card) (:discard hero)) :rfg)
                                                   (if (pos? heap)
                                                     (resolve-ability state side
                                                                      {:show-discard true
                                                                       :prompt (str "Choose " (min n heap) " card(s) to move from the Heap to your Grip")
                                                                       :delayed-completion true
                                                                       :msg (msg "take " (join ", " (map :title targets)) " from their Heap to their Grip")
                                                                       :choices {:max (min n heap)
                                                                                 :all true
                                                                                 :req #(and (= (:side %) "Hero")
                                                                                            (in-discard? %))}
                                                                       :effect (req (doseq [c targets] (move state side c :hand))
                                                                                    (do-access state side eid (:server run) {:hq-root-only true}))} card nil)
                                                     (resolve-ability state side
                                                                      {:delayed-completion true
                                                                       :msg (msg "take no cards from their Heap to their Grip")
                                                                       :effect (req (do-access state side eid (:server run) {:hq-root-only true}))} card nil))))}} card))}

   "Rumor Mill"
   (letfn [(eligible? [card] (and (:uniqueness card)
                                  (or (card-is? card :type "Asset")
                                      (card-is? card :type "Upgrade"))
                                  (not (has-subtype? card "Region"))))
           (rumor [state] (filter eligible? (concat (all-installed state :minion)
                                  (get-in @state [:minion :hand])
                                  (get-in @state [:minion :deck])
                                  (get-in @state [:minion :discard]))))]
   {:leave-play (req (doseq [c (rumor state)]
                       (enable-card state :minion c)))
    :effect (req (doseq [c (rumor state)]
                   (disable-card state :minion c)))
    :events {:minion-install {:req (req (eligible? target))
                            :effect (effect (disable-card :minion target))}}})

   "Run Amok"
   {:implementation "Ice trash is manual"
    :prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target {:end-run {:msg " trash 1 piece of ICE that was rezzed during the run"}} card))}

   "Running Interference"
   (run-event
    {:events {:pre-rez nil
              :run-ends nil}}
    nil
    nil
    (effect (register-events {:pre-rez {:req (req (ice? target))
                                        :effect (effect (rez-cost-bonus (:cost target)))}
                              :run-ends {:effect (effect (unregister-events card))}}
                             (assoc card :zone '(:discard)))))

   "Satellite Uplink"
   {:choices {:max 2 :req installed?}
    :delayed-completion true
    :effect (req (let [[card1 card2] targets]
                   (when-completed (expose state side card1)
                                   (expose state side eid card2))))}

   "Scavenge"
   {:prompt "Select an installed program to trash"
    :choices {:req #(and (is-type? % "Program")
                         (installed? %))}
    :effect (req (let [trashed target tcost (- (:cost trashed)) st state si side]
                   (trash state side trashed)
                   (resolve-ability
                     state side
                     {:prompt "Select a program to install from your Grip or Heap"
                      :show-discard true
                      :choices {:req #(and (is-type? % "Program")
                                           (#{[:hand] [:discard]} (:zone %))
                                           (can-pay? st si nil (modified-install-cost st si % [:credit tcost])))}
                      :effect (effect (install-cost-bonus [:credit (- (:cost trashed))])
                                      (hero-install target))
                      :msg (msg "trash " (:title trashed) " and install " (:title target))} card nil)))}

   "Scrubbed"
   {:events (let [sc {:effect (req (update! state side (dissoc card :scrubbed-target)))}]
                 {:encounter-ice {:once :per-turn
                                  :effect (effect (update! (assoc card :scrubbed-target target))
                                                  (update-ice-strength current-ice))}
                  :pre-ice-strength {:req (req (= (:cid target) (get-in card [:scrubbed-target :cid])))
                                     :effect (effect (ice-strength-bonus -2 target))}
                  :run-ends sc})}

   "Showing Off"
   {:req (req rd-runnable)
    :effect (effect (run :rd
                      {:replace-access
                       {:msg "access cards from the bottom of R&D"
                        :delayed-completion true
                        :effect (req (when-completed (resolve-ability state side
                                                       {:effect (effect (register-events (:events (card-def card))
                                                                                         (assoc card :zone '(:discard))))}
                                                      card nil)
                                                     (do-access state side eid (:server run))))}} card))
    :events {:pre-access {:silent (req true)
                          :effect (req (swap! state assoc-in [:minion :deck]
                                              (rseq (into [] (get-in @state [:minion :deck])))))}
             :run-ends {:effect (req (swap! state assoc-in [:minion :deck]
                                            (rseq (into [] (get-in @state [:minion :deck]))))
                                     (unregister-events state side card))}}}

   "Singularity"
   (run-event
    {:choices (req (filter #(can-run-server? state %) remotes))}
    {:req (req (is-remote? target))
     :replace-access {:mandatory true
                      :msg "trash all cards in the server at no cost"
                      :effect (req (doseq [c (get-in (:servers minion) (conj (:server run) :content))]
                                     (trash state side c)))}})

   "Social Engineering"
   {:prompt "Select an unrezzed piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (not (rezzed? %)) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))]
              (resolve-ability
                 state :hero
                 {:msg (msg "select the piece of ICE at position " (ice-index state ice) " of " serv)
                  :effect (effect (register-events {:pre-rez-cost
                                                    {:req (req (= target ice))
                                                     :effect (req (let [cost (rez-cost state side (get-card state target))]
                                                                    (gain state :hero :credit cost)))
                                                     :msg (msg "gain " (rez-cost state side (get-card state target)) " [Credits]")}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:pre-rez-cost nil}
    :end-turn {:effect (effect (unregister-events card))}}

   "Spear Phishing"
   {:implementation "Bypass is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}

   "Special Order"
   {:prompt "Choose an Icebreaker"
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (system-msg (str "adds " (:title target) " to their Grip and shuffles their Stack"))
                    (move target :hand))
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck hero)) :sorted))}

   "Spooned"
   {:implementation "Ice trash is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}

   "Spot the Prey"
   {:prompt "Select 1 non-ICE card to expose"
    :msg "expose 1 card and make a run"
    :choices {:req #(and (installed? %) (not (ice? %)) (= (:side %) "Minion"))}
    :delayed-completion true
    :effect (req (when-completed (expose state side target)
                                 (continue-ability
                                   state side
                                   {:prompt "Choose a server"
                                    :choices (req runnable-servers)
                                    :delayed-completion true
                                    :effect (effect (game.core/run eid target))}
                                   card nil)))}

   "Stimhack"
   (run-event
    nil
    {:end-run {:msg "take 1 brain damage"
               :effect (effect (damage eid :brain 1 {:unpreventable true :card card}))}}
    (effect (gain-run-credits 9)))

   "Sure Gamble"
   {:msg "gain 9 [Credits]" :effect (effect (gain :credit 9))}

   "Surge"
   {:msg (msg "place 2 virus tokens on " (:title target))
    :choices {:req #(and (has-subtype? % "Virus") (:added-virus-counter %))}
    :effect (req (add-counter state :hero target :virus 2))}

   "SYN Attack"
   {:effect (req (if (< (count (:hand minion)) 2)
                   (draw state :minion 4)
                   (do (show-wait-prompt state :hero "Corp to choose an option for SYN Attack")
                       (resolve-ability state :minion
                         {:prompt "Discard 2 cards or draw 4 cards?"
                          :choices ["Discard 2" "Draw 4"]
                          :effect (req (if (= target "Draw 4")
                                         (do (draw state :minion 4)
                                             (system-msg state :minion (str "draws 4 cards from SYN Attack"))
                                             (clear-wait-prompt state :hero))
                                         (resolve-ability state :minion
                                           {:prompt "Choose 2 cards to discard"
                                            :choices {:max 2 :req #(and (in-hand? %) (= (:side %) "Minion"))}
                                            :effect (effect (trash-cards :minion targets)
                                                            (system-msg :minion (str "discards 2 cards from SYN Attack"))
                                                            (clear-wait-prompt :hero))}
                                          card nil)))}
                        card nil))))}

   "System Outage"
   {:events {:minion-draw {:req (req (not (first-event? state side :minion-draw)))
                         :msg "force the Corp to lose 1 [Credits]"
                         :effect (effect (lose :minion :credit 1))}}}

   "System Seizure"
   (let [ss {:effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}]
     {:effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
      :events {:pump-breaker {:silent (req true)
                              :req (req (not (get-in @state [:per-turn (:cid card)])))
                              :effect (effect (update! (update-in (second targets) [:pump :all-run] (fnil #(+ % (first targets)) 0)))
                                              (update-breaker-strength (second targets)))}
               :pass-ice ss :run-ends ss}
      :move-zone (req (when (= [:discard] (:zone card))
                        (unregister-events state side card)))})

   "Test Run"
   {:prompt "Install a program from your Stack or Heap?"
    :choices (cancellable ["Stack" "Heap"])
    :msg (msg "install a program from their " target)
    :effect (effect (resolve-ability
                      {:prompt "Choose a program to install"
                       :choices (req (cancellable
                                       (filter #(is-type? % "Program")
                                               ((if (= target "Heap") :discard :deck) hero))))
                       :effect (effect (trigger-event :searched-stack nil)
                                       (shuffle! :deck)
                                       (hero-install (assoc-in target [:special :test-run] true) {:no-cost true}))
                       :end-turn
                       {:req (req (get-in (find-cid (:cid target) (all-installed state :hero)) [:special :test-run]))
                        :msg (msg "move " (:title target) " to the top of their Stack")
                        :effect (req (move state side (find-cid (:cid target) (all-installed state :hero))
                                           :deck {:front true}))}}
                      card targets))}

   "The Makers Eye"
   {:req (req rd-runnable)
    :effect (effect (run :rd nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "The Noble Path"
   {:effect (req (doseq [c (:hand hero)]
                   (trash state side c))
                 (register-events state side
                                  {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                (damage-prevent :meat Integer/MAX_VALUE)
                                                                (damage-prevent :brain Integer/MAX_VALUE))}
                                   :run-ends {:effect (effect (unregister-events card))}}
                                  (assoc card :zone '(:discard)))
                 (resolve-ability state side
                   {:prompt "Choose a server"
                    :choices (req runnable-servers)
                    :msg (msg "trash their Grip and make a run on " target ", preventing all damage")
                    :effect (req (let [runtgt [(last (server->zone state target))]
                                       ices (get-in @state (concat [:minion :servers] runtgt [:ices]))]
                                   (swap! state assoc :per-run nil
                                                      :run {:server runtgt :position (count ices)
                                                            :access-bonus 0 :run-effect nil})
                                   (gain-run-credits state :hero (:bad-publicity minion))
                                   (swap! state update-in [:hero :register :made-run] #(conj % (first runtgt)))
                                   (trigger-event state :hero :run runtgt)))} card nil))
    :events {:pre-damage nil :run-ends nil}}

   "The Price of Freedom"
   {:additional-cost [:connection 1]
    :msg "prevent the Corp from advancing cards during their next turn"
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:rfg)))
                    (move (first (:play-area hero)) :rfg))
    :events {:minion-turn-begins
             {:effect (effect (register-turn-flag! card :can-advance
                                (fn [state side card]
                                  ((constantly false)
                                   (toast state :minion "Cannot advance cards this turn due to The Price of Freedom." "warning"))))
                              (unregister-events card))}}}

   "Three Steps Ahead"
   {:end-turn {:effect (effect (gain :credit (* 2 (count (:successful-run hero-reg)))))
               :msg (msg "gain " (* 2 (count (:successful-run hero-reg))) " [Credits]")}}

   "Tinkering"
   {:prompt "Select a piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       stypes (:subtype ice)]
              (resolve-ability
                 state :hero
                 {:msg (msg "make " (card-str state ice) " gain Sentry, Code Gate, and Barrier until the end of the turn")
                  :effect (effect (update! (assoc ice :subtype (combine-subtypes true (:subtype ice) "Sentry" "Code Gate" "Barrier")))
                                  (update-ice-strength (get-card state ice))
                                  (register-events {:hero-turn-ends
                                                    {:effect (effect (update! (assoc (get-card state ice) :subtype stypes)))}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:hero-turn-ends nil}}

   "Trade-In"
   {:additional-cost [:hardware 1]
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:hero-trash {:effect (effect (gain :credit (quot (:cost target) 2))
                                            (system-msg (str "trashes " (:title target) " and gains " (quot (:cost target) 2) " [Credits]"))
                                            (continue-ability {:prompt "Choose a Hardware to add to your Grip from your Stack"
                                                               :choices (req (filter #(is-type? % "Hardware")
                                                                                     (:deck hero)))
                                                               :msg (msg "add " (:title target) " to their Grip")
                                                               :effect (effect (trigger-event :searched-stack nil)
                                                                               (shuffle! :deck)
                                                                               (move target :hand)
                                                                               (unregister-events card))} card nil))}}}

   "Traffic Jam"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events {:pre-advancement-cost
             {:effect (req (advancement-cost-bonus
                             state side (count (filter #(= (:title %) (:title target)) (:scored minion)))))}}}

   "Uninstall"
   {:choices {:req #(and (installed? %)
                         (not (facedown? %))
                         (#{"Program" "Hardware"} (:type %)))}
    :msg (msg "move " (:title target) " to their Grip")
    :effect (effect (move target :hand))}

   "Unscheduled Maintenance"
   {:events {:minion-install {:req (req (ice? target))
                            :effect (effect (register-turn-flag!
                                              card :can-install-ice
                                              (fn [state side card]
                                                (if (ice? card)
                                                  ((constantly false)
                                                   (toast state :minion "Cannot install ICE the rest of this turn due to Unscheduled Maintenance"))
                                                  true))))}}
    :leave-play (effect (clear-turn-flag! card :can-install-ice))}

   "Vamp"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:delayed-completion true
                               :prompt "How many [Credits]?" :choices :credit
                               :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                               :effect (effect (lose :minion :credit target)
                                               (tag-hero eid 1))}} card))}

   "Wanton Destruction"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to discard " target " cards from HQ at random")
                               :prompt "How many [Click] do you want to spend?"
                               :choices (req (map str (range 1 (inc (:click hero)))))
                               :effect (req (let [n (Integer/parseInt target)]
                                              (when (pay state :hero card :click n)
                                                (trash-cards state :minion (take n (shuffle (:hand minion)))))))}} card))}

   "Windfall"
   {:effect (effect (shuffle! :deck)
                    (resolve-ability
                      {:effect (req (let [topcard (first (:deck hero))
                                          cost (:cost topcard)]
                                      (trash state side topcard)
                                      (when-not (is-type? topcard "Event")
                                        (gain state side :credit cost))
                                      (system-msg state side
                                                  (str "shuffles their Stack and trashes " (:title topcard)
                                                       (when-not (is-type? topcard "Event")
                                                         (str " to gain " cost " [Credits]"))))))}
                     card nil))}})
