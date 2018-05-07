(in-ns 'test.core)

(deftest contestant-rez-unique
  ;; Rezzing a second copy of a unique Corp card
  (do-game
    (new-game (default-contestant [(qty "Caprice Nisei" 2)])
              (default-hero))
    (play-from-hand state :contestant "Caprice Nisei" "HQ")
    (play-from-hand state :contestant "Caprice Nisei" "R&D")
    (core/rez state :contestant (get-content state :hq 0))
    (is (:rezzed (get-content state :hq 0)) "First Caprice rezzed")
    (core/rez state :contestant (get-content state :rd 0))
    (is (not (:rezzed (get-content state :rd 0))) "Second Caprice could not be rezzed")))

(deftest hero-install-program
  ;; hero-install - Program; ensure costs are paid
  (do-game
    (new-game (default-contestant)
              (default-hero [(qty "Gordian Blade" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :hero "Gordian Blade")
    (let [gord (get-in @state [:hero :rig :program 0])]
      (is (= (- 5 (:cost gord)) (:credit (get-hero))) "Program cost was applied")
      (is (= (- 4 (:memoryunits gord)) (:memory (get-hero))) "Program MU was applied"))))

(deftest hero-installing-uniques
  ;; Installing a copy of an active unique Runner card is prevented
  (do-game
    (new-game (default-contestant)
              (default-hero [(qty "Kati Jones" 2) (qty "Scheherazade" 2)
                               (qty "Off-Campus Apartment" 1) (qty "Hivemind" 2)]))
    (take-credits state :contestant)
    (core/gain state :hero :click 1 :memory 2)
    (core/draw state :hero 2)
    (play-from-hand state :hero "Kati Jones")
    (play-from-hand state :hero "Off-Campus Apartment")
    (play-from-hand state :hero "Scheherazade")
    (let [oca (get-in @state [:hero :rig :resource 1])
          scheh (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero scheh 0)
      (prompt-select :hero (find-card "Hivemind" (:hand (get-hero))))
      (is (= "Hivemind" (:title (first (:hosted (refresh scheh))))) "Hivemind hosted on Scheherazade")
      (play-from-hand state :hero "Kati Jones")
      (is (= 1 (:click (get-hero))) "Not charged a click")
      (is (= 2 (count (get-in @state [:hero :rig :resource]))) "2nd copy of Kati couldn't install")
      (card-ability state :hero oca 0)
      (prompt-select :hero (find-card "Kati Jones" (:hand (get-hero))))
      (is (empty? (:hosted (refresh oca))) "2nd copy of Kati couldn't be hosted on OCA")
      (is (= 1 (:click (get-hero))) "Not charged a click")
      (play-from-hand state :hero "Hivemind")
      (is (= 1 (count (get-in @state [:hero :rig :program]))) "2nd copy of Hivemind couldn't install")
      (card-ability state :hero scheh 0)
      (prompt-select :hero (find-card "Hivemind" (:hand (get-hero))))
      (is (= 1 (count (:hosted (refresh scheh)))) "2nd copy of Hivemind couldn't be hosted on Scheherazade")
      (is (= 1 (:click (get-hero))) "Not charged a click"))))

(deftest deactivate-program
  ;; deactivate - Program; ensure MU are restored
  (do-game
    (new-game (default-contestant)
              (default-hero [(qty "Gordian Blade" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :hero "Gordian Blade")
    (let [gord (get-in @state [:hero :rig :program 0])]
      (core/trash state :hero gord)
      (is (= 4 (:memory (get-hero))) "Trashing the program restored MU"))))

(deftest agenda-forfeit-hero
  ;; forfeit - Don't deactivate agenda to trigger leave play effects if Runner forfeits a stolen agenda
  (do-game
    (new-game (default-contestant [(qty "Mandatory Upgrades" 1)])
              (default-hero [(qty "Data Dealer" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :hero "Data Dealer")
    (run-empty-server state "HQ")
    (prompt-choice :hero "Steal")
    (is (= 2 (:agenda-point (get-hero))))
    (card-ability state :hero (get-resource state 0) 0)
    (prompt-select :hero (get-scored state :hero 0))
    (is (= 1 (:click (get-hero))) "Didn't lose a click")
    (is (= 4 (:click-per-turn (get-hero))) "Still have 4 clicks per turn")))

(deftest agenda-forfeit-contestant
  ;; forfeit - Deactivate agenda to trigger leave play effects if Corp forfeits a scored agenda
  (do-game
    (new-game (default-contestant [(qty "Mandatory Upgrades" 1) (qty "Corporate Town" 1)])
              (default-hero))
    (play-from-hand state :contestant "Mandatory Upgrades" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (is (= 4 (:click-per-turn (get-contestant))) "Up to 4 clicks per turn")
    (play-from-hand state :contestant "Corporate Town" "New remote")
    (let [ctown (get-content state :remote2 0)]
      (core/rez state :contestant ctown)
      (prompt-select :contestant (get-scored state :contestant 0))
      (is (= 3 (:click-per-turn (get-contestant))) "Back down to 3 clicks per turn"))))

(deftest refresh-recurring-credits-hosted
  ;; host - Recurring credits on cards hosted after install refresh properly
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3) (qty "Hedge Fund" 3)])
              (default-hero [(qty "Compromised Employee" 1) (qty "Off-Campus Apartment" 1)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant 2)
    (play-from-hand state :hero "Off-Campus Apartment")
    (play-from-hand state :hero "Compromised Employee")
    (let [iwall (get-ice state :hq 0)
          apt (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero apt 1) ; use Off-Campus option to host an installed card
      (prompt-select :hero (find-card "Compromised Employee"
                                        (get-in @state [:hero :rig :resource])))
      (let [cehosted (first (:hosted (refresh apt)))]
        (card-ability state :hero cehosted 0) ; take Comp Empl credit
        (is (= 4 (:credit (get-hero))))
        (is (= 0 (:rec-counter (refresh cehosted))))
        (core/rez state :contestant iwall)
        (is (= 5 (:credit (get-hero))) "Compromised Employee gave 1 credit from ice rez")
        (take-credits state :hero)
        (take-credits state :contestant)
        (is (= 1 (:rec-counter (refresh cehosted)))
            "Compromised Employee recurring credit refreshed")))))

(deftest card-str-test-simple
  ;; ensure card-str names cards in simple situations properly
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3) (qty "Jackson Howard" 2)])
              (default-hero [(qty "Corroder" 1)
                               (qty "Clone Chip" 1)
                               (qty "Paparazzi" 1)
                               (qty "Parasite" 1)]))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (core/end-turn state :contestant nil)
    (core/start-turn state :hero nil)
    (play-from-hand state :hero "Corroder")
    (play-from-hand state :hero "Clone Chip")
    (play-from-hand state :hero "Paparazzi")
    (play-from-hand state :hero "Parasite")
    (let [hqiwall0 (get-ice state :hq 0)
          hqiwall1 (get-ice state :hq 1)
          rdiwall (get-ice state :rd 0)
          jh1 (get-content state :remote1 0)
          jh2 (get-content state :remote2 0)
          corr (get-in @state [:hero :rig :program 0])
          cchip (get-in @state [:hero :rig :hardware 0])
          pap (get-in @state [:hero :rig :resource 0])]
      (core/rez state :contestant hqiwall0)
      (core/rez state :contestant jh1)
      (prompt-select :hero (refresh hqiwall0))
      (is (= (core/card-str state (refresh hqiwall0)) "Ice Wall protecting HQ at position 0"))
      (is (= (core/card-str state (refresh hqiwall1)) "ICE protecting HQ at position 1"))
      (is (= (core/card-str state (refresh rdiwall)) "ICE protecting R&D at position 0"))
      (is (= (core/card-str state (refresh rdiwall) {:visible true})
             "Ice Wall protecting R&D at position 0"))
      (is (= (core/card-str state (refresh jh1)) "Jackson Howard in Server 1"))
      (is (= (core/card-str state (refresh jh2)) "a card in Server 2"))
      (is (= (core/card-str state (refresh corr)) "Corroder"))
      (is (= (core/card-str state (refresh cchip)) "Clone Chip"))
      (is (= (core/card-str state (refresh pap)) "Paparazzi"))
      (is (= (core/card-str state (first (:hosted (refresh hqiwall0))))
             "Parasite hosted on Ice Wall protecting HQ at position 0")))))

(deftest invalid-score-attempt
  ;; Test scoring with an incorrect number of advancement tokens
  (do-game
    (new-game (default-contestant [(qty "Ancestral Imager" 1)])
              (default-hero))
    (play-from-hand state :contestant "Ancestral Imager" "New remote")
    (let [ai (get-content state :remote1 0)]
      ;; Trying to score without any tokens does not do anything
      (is (not (find-card "Ancestral Imager" (:scored (get-contestant)))) "AI not scored")
      (is (not (nil? (get-content state :remote1 0))))
      (core/advance state :contestant {:card (refresh ai)})
      (core/score state :contestant {:card (refresh ai)})
      (is (not (nil? (get-content state :remote1 0)))))))

(deftest trash-contestant-hosted
  ;; Hosted Corp cards are included in all-installed and fire leave-play effects when trashed
  (do-game
    (new-game (default-contestant [(qty "Full Immersion RecStudio" 1) (qty "Worlds Plaza" 1) (qty "Director Haas" 1)])
              (default-hero))
    (play-from-hand state :contestant "Full Immersion RecStudio" "New remote")
    (let [fir (get-content state :remote1 0)]
      (core/rez state :contestant fir)
      (card-ability state :contestant fir 0)
      (prompt-select :contestant (find-card "Worlds Plaza" (:hand (get-contestant))))
      (let [wp (first (:hosted (refresh fir)))]
        (core/rez state :contestant wp)
        (card-ability state :contestant wp 0)
        (prompt-select :contestant (find-card "Director Haas" (:hand (get-contestant))))
        (let [dh (first (:hosted (refresh wp)))]
          (is (:rezzed dh) "Director Haas was rezzed")
          (is (= 0 (:credit (get-contestant))) "Corp has 0 credits")
          (is (= 4 (:click-per-turn (get-contestant))) "Corp has 4 clicks per turn")
          (is (= 3 (count (core/all-installed state :contestant))) "all-installed counting hosted Corp cards")
          (take-credits state :contestant)
          (run-empty-server state "Server 1")
          (prompt-select :hero dh)
          (prompt-choice :hero "Yes") ; trash Director Haas
          (prompt-choice :hero "Done")
          (is (= 3 (:click-per-turn (get-contestant))) "Corp down to 3 clicks per turn"))))))

(deftest trash-remove-per-turn-restriction
  ;; Trashing a card should remove it from [:per-turn] - Issue #1345
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 3)])
              (default-hero [(qty "Imp" 2) (qty "Scavenge" 1)]))
    (take-credits state :contestant)
    (core/gain state :hero :click 1)
    (play-from-hand state :hero "Imp")
    (let [imp (get-program state 0)]
      (run-empty-server state "HQ")
      (card-ability state :hero imp 0)
      (is (= 1 (count (:discard (get-contestant)))) "Accessed Hedge Fund is trashed")
      (run-empty-server state "HQ")
      (card-ability state :hero imp 0)
      (is (= 1 (count (:discard (get-contestant)))) "Card can't be trashed, Imp already used this turn")
      (prompt-choice :hero "OK")
      (play-from-hand state :hero "Scavenge")
      (prompt-select :hero imp)
      (prompt-select :hero (find-card "Imp" (:discard (get-hero)))))
    (let [imp (get-program state 0)]
      (is (= 2 (get-counters (refresh imp) :virus)) "Reinstalled Imp has 2 counters")
      (run-empty-server state "HQ")
      (card-ability state :hero imp 0))
    (is (= 2 (count (:discard (get-contestant)))) "Hedge Fund trashed, reinstalled Imp used on same turn")))

(deftest trash-seen-and-unseen
  ;; Trash installed assets that are both seen and unseen by hero
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 3)])
              (default-hero))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant 1)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "No")
    ;; run and trash the second asset
    (run-empty-server state "Server 2")
    (prompt-choice :hero "Yes")
    (take-credits state :hero 2)
    (play-from-hand state :contestant "PAD Campaign" "Server 1")
    (prompt-choice :contestant "OK")
    (is (= 2 (count (:discard (get-contestant)))) "Trashed existing asset")
    (is (:seen (first (get-in @state [:contestant :discard]))) "Asset trashed by hero is Seen")
    (is (not (:seen (second (get-in @state [:contestant :discard]))))
        "Asset trashed by contestant is Unseen")
    (is (not (:seen (get-content state :remote1 0))) "New asset is unseen")))

(deftest reinstall-seen-asset
  ;; Install a faceup card in Archives, make sure it is not :seen
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 1) (qty "Interns" 1)])
              (default-hero))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant 2)
    ;; run and trash the asset
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes")
    (is (:seen (first (get-in @state [:contestant :discard]))) "Asset trashed by hero is Seen")
    (take-credits state :hero 3)
    (play-from-hand state :contestant "Interns")
    (prompt-select :contestant (first (get-in @state [:contestant :discard])))
    (prompt-choice :contestant "New remote")
    (is (not (:seen (get-content state :remote2 0))) "New asset is unseen")))

(deftest all-installed-hero-test
  ;; Tests all-installed for programs hosted on ICE, nested hosted programs, and non-installed hosted programs
  (do-game
    (new-game (default-contestant [(qty "Wraparound" 1)])
              (default-hero [(qty "Omni-drive" 1) (qty "Personal Workshop" 1) (qty "Leprechaun" 1) (qty "Corroder" 1) (qty "Mimic" 1) (qty "Knight" 1)]))
    (play-from-hand state :contestant "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (core/rez state :contestant wrap)
      (take-credits state :contestant)
      (core/draw state :hero)
      (core/gain state :hero :credit 7)
      (play-from-hand state :hero "Knight")
      (play-from-hand state :hero "Personal Workshop")
      (play-from-hand state :hero "Omni-drive")
      (take-credits state :contestant)
      (let [kn (get-in @state [:hero :rig :program 0])
            pw (get-in @state [:hero :rig :resource 0])
            od (get-in @state [:hero :rig :hardware 0])
            co (find-card "Corroder" (:hand (get-hero)))
            le (find-card "Leprechaun" (:hand (get-hero)))]
        (card-ability state :hero kn 0)
        (prompt-select :hero wrap)
        (card-ability state :hero pw 0)
        (prompt-select :hero co)
        (card-ability state :hero od 0)
        (prompt-select :hero le)
        (let [od (refresh od)
              le (first (:hosted od))
              mi (find-card "Mimic" (:hand (get-hero)))]
          (card-ability state :hero le 0)
          (prompt-select :hero mi)
          (let [all-installed (core/all-installed state :hero)]
            (is (= 5 (count all-installed)) "Number of installed hero cards is correct")
            (is (not-empty (filter #(= (:title %) "Leprechaun") all-installed)) "Leprechaun is in all-installed")
            (is (not-empty (filter #(= (:title %) "Personal Workshop") all-installed)) "Personal Workshop is in all-installed")
            (is (not-empty (filter #(= (:title %) "Mimic") all-installed)) "Mimic is in all-installed")
            (is (not-empty (filter #(= (:title %) "Omni-drive") all-installed)) "Omni-drive is in all-installed")
            (is (not-empty (filter #(= (:title %) "Knight") all-installed)) "Knight is in all-installed")
            (is (empty (filter #(= (:title %) "Corroder") all-installed)) "Corroder is not in all-installed")))))))

(deftest log-accessed-names
  ;; Check that accessed card names are logged - except those on R&D, and no logs on archives
  (do-game
    (new-game
      (default-contestant [(qty "PAD Campaign" 7)])
      (default-hero))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (trash-from-hand state :contestant "PAD Campaign")
    (take-credits state :contestant)
    (run-empty-server state :hq)
    (prompt-choice :hero "No") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-server state :rd)
    (is (last-log-contains? state "an unseen card") "Accessed card name was not logged")
    (run-empty-server state :remote1)
    (prompt-choice :hero "No") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")))

(deftest counter-manipulation-commands
  ;; Test interactions of various cards with /counter and /adv-counter commands
  (do-game
    (new-game (default-contestant [(qty "Adonis Campaign" 1)
                             (qty "Public Support" 2)
                             (qty "Oaktown Renovation" 1)])
              (default-hero))
    ;; Turn 1 Corp, install oaktown and assets
    (core/gain state :contestant :click 4)
    (play-from-hand state :contestant "Adonis Campaign" "New remote")
    (play-from-hand state :contestant "Public Support" "New remote")
    (play-from-hand state :contestant "Public Support" "New remote")
    (play-from-hand state :contestant "Oaktown Renovation" "New remote")
    (let [adonis (get-content state :remote1 0)
          publics1 (get-content state :remote2 0)
          publics2 (get-content state :remote3 0)
          oaktown (get-content state :remote4 0)]
    (core/advance state :contestant {:card (refresh oaktown)})
    (core/advance state :contestant {:card (refresh oaktown)})
    (core/advance state :contestant {:card (refresh oaktown)})
    (is (= 8 (:credit (get-contestant))) "Corp 5+3 creds from Oaktown")
    (core/end-turn state :contestant nil)

    ;; Turn 1 Runner
    (core/start-turn state :hero nil)
    (take-credits state :hero 3)
    (core/click-credit state :hero nil)
    (core/end-turn state :hero nil)
    (core/rez state :contestant (refresh adonis))
    (core/rez state :contestant (refresh publics1))

    ;; Turn 2 Corp
    (core/start-turn state :contestant nil)
    (core/rez state :contestant (refresh publics2))
    (is (= 3 (:click (get-contestant))))
    (is (= 3 (:credit (get-contestant))) "only Adonis money")
    (is (= 9 (get-counters (refresh adonis) :credit)))
    (is (= 2 (get-counters (refresh publics1) :power)))
    (is (= 3 (get-counters (refresh publics2) :power)))

    ;; oops, forgot to rez 2nd public support before start of turn,
    ;; let me fix it with a /command
    (core/command-counter state :contestant ["power" 2])
    (prompt-select :contestant (refresh publics2))
    (is (= 2 (get-counters (refresh publics2) :power)))
    ;; Oaktown checks and manipulation
    (is (= 3 (:advance-counter (refresh oaktown))))
    (core/command-adv-counter state :contestant 2)
    (prompt-select :contestant (refresh oaktown))
    ;; score should fail, shouldn't be able to score with 2 advancement tokens
    (core/score state :contestant (refresh oaktown))
    (is (= 0 (:agenda-point (get-contestant))))
    (core/command-adv-counter state :contestant 4)
    (prompt-select :contestant (refresh oaktown))
    (is (= 4 (:advance-counter (refresh oaktown))))
    (is (= 3 (:credit (get-contestant))))
    (is (= 3 (:click (get-contestant))))
    (core/score state :contestant (refresh oaktown)) ; now the score should go through
    (is (= 2 (:agenda-point (get-contestant))))
    (take-credits state :contestant)

    ;; Turn 2 Runner
    ;; cheating with publics1 going too fast. Why? because I can
    (is (= 2 (get-counters (refresh publics1) :power)))
    (core/command-counter state :contestant ["power" 1])
    (prompt-select :contestant (refresh publics1))
    (is (= 1 (get-counters (refresh publics1) :power)))
    ;; let's adjust Adonis while at it
    (is (= 9 (get-counters (refresh adonis) :credit)))
    (core/command-counter state :contestant ["credit" 3])
    (prompt-select :contestant (refresh adonis))
    (is (= 3 (get-counters (refresh adonis) :credit)))
    (take-credits state :hero)

    ;; Turn 3 Corp
    (is (= 3 (:agenda-point (get-contestant)))) ; cheated PS1 should get scored
    (is (= 9 (:credit (get-contestant))))
    (is (= (:zone (refresh publics1) :scored)))
    (is (= (:zone (refresh publics2)) [:servers :remote3 :content]))
    (is (= (:zone (refresh adonis) :discard)))
    (take-credits state :contestant)

    ;; Turn 3 Runner
    (take-credits state :hero)

    ;; Turn 4 Corp
    (is (= 4 (:agenda-point (get-contestant)))) ; PS2 should get scored
    (is (= (:zone (refresh publics2) :scored)))
    (is (= 12 (:credit (get-contestant)))))))

(deftest run-bad-publicity-credits
  ;; Should not lose BP credits until a run is completely over. Issue #1721.
  (do-game
    (new-game (default-contestant [(qty "Cyberdex Virus Suite" 3)])
              (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
    (is (= 1 (:bad-publicity (get-contestant))) "Corp starts with 1 BP")
    (play-from-hand state :contestant "Cyberdex Virus Suite" "New remote")
    (play-from-hand state :contestant "Cyberdex Virus Suite" "R&D")
    (play-from-hand state :contestant "Cyberdex Virus Suite" "HQ")
    (take-credits state :contestant)
    (run-empty-server state :remote1)
    (prompt-choice :contestant "No")
    (prompt-choice :hero "Yes")
    (is (= 5 (:credit (get-hero))) "1 BP credit spent to trash CVS")
    (run-empty-server state :hq)
    (prompt-choice :contestant "No")
    (prompt-choice :hero "Yes")
    (is (= 5 (:credit (get-hero))) "1 BP credit spent to trash CVS")
    (run-empty-server state :rd)
    (prompt-choice :contestant "No")
    (prompt-choice :hero "Yes")
    (is (= 5 (:credit (get-hero))) "1 BP credit spent to trash CVS")))

(deftest run-psi-bad-publicity-credits
  ;; Should pay from Bad Pub for Psi games during run #2374
  (do-game
    (new-game (default-contestant [(qty "Caprice Nisei" 3)])
              (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
    (is (= 1 (:bad-publicity (get-contestant))) "Corp starts with 1 BP")
    (play-from-hand state :contestant "Caprice Nisei" "New remote")
    (take-credits state :contestant)
    (let [caprice (get-content state :remote1 0)]
      (core/rez state :contestant caprice)
      (run-on state "Server 1")
      (is (prompt-is-card? :contestant caprice) "Caprice prompt even with no ice, once hero makes run")
      (is (prompt-is-card? :hero caprice) "Runner has Caprice prompt")
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :hero "1 [Credits]")
      (is (= 5 (:credit (get-hero))) "Runner spend bad pub credit on psi game")
      (is (= 3 (:credit (get-contestant))) "Corp spent 2 on psi game"))))

(deftest purge-nested
  ;; Purge nested-hosted virus counters
  (do-game
    (new-game (default-contestant [(qty "Cyberdex Trial" 1)])
              (default-hero [(qty "Djinn" 1) (qty "Imp" 1) (qty "Leprechaun" 1)]))
    (take-credits state :contestant)
    (core/gain state :hero :credit 100)
    (play-from-hand state :hero "Leprechaun")
    (let [lep (get-program state 0)]
      (card-ability state :hero lep 0)
      (prompt-select :hero (find-card "Djinn" (:hand (get-hero))))
      (let [djinn (first (:hosted (refresh lep)))]
        (card-ability state :hero djinn 1)
        (prompt-select :hero (find-card "Imp" (:hand (get-hero))))
        (let [imp (first (:hosted (refresh djinn)))]
          (is (= 2 (get-counters imp :virus)) "Imp has 2 virus counters")
          (take-credits state :hero)
          (play-from-hand state :contestant "Cyberdex Trial")
          (is (= 0 (get-counters (refresh imp) :virus)) "Imp counters purged"))))))

(deftest multi-access-rd
  ;; multi-access of R&D sees all cards and upgrades
  (do-game
    (new-game (default-contestant [(qty "Keegan Lane" 1) (qty "Midway Station Grid" 1)
                             (qty "Sweeps Week" 1) (qty "Manhunt" 1)
                             (qty "Hedge Fund" 1) (qty "Big Brother" 1)])
              (default-hero [(qty "Medium" 1)]))
    (play-from-hand state :contestant "Keegan Lane" "R&D")
    (play-from-hand state :contestant "Midway Station Grid" "R&D")
    (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Sweeps Week" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Manhunt" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Big Brother" (:hand (get-contestant))) :deck)
    (core/rez state :contestant (get-content state :rd 1))
    (take-credits state :contestant)
    (play-from-hand state :hero "Medium")
    (let [keegan (get-content state :rd 0)
          msg (get-content state :rd 1)
          med (get-program state 0)]
      (core/command-counter state :hero ["virus" 2])
      (prompt-select :hero (refresh med))
      (run-empty-server state :rd)
      (prompt-choice :hero 2)
      (prompt-choice :hero "Card from deck")
      (is (= "Hedge Fund" (-> (get-hero) :prompt first :card :title)))
      (prompt-choice :hero "OK")
      (prompt-choice :hero "Unrezzed upgrade in R&D")
      (is (= "Keegan Lane" (-> (get-hero) :prompt first :card :title)))
      (prompt-choice :hero "No")
      (prompt-choice :hero "Card from deck")
      (is (= "Sweeps Week" (-> (get-hero) :prompt first :card :title)))
      (prompt-choice :hero "OK")
      (prompt-choice :hero "Midway Station Grid")
      (is (= "Midway Station Grid" (-> (get-hero) :prompt first :card :title)))
      (prompt-choice :hero "No")
      (prompt-choice :hero "Card from deck")
      (is (= "Manhunt" (-> (get-hero) :prompt first :card :title)))
      (prompt-choice :hero "OK")
      (is (not (:run @state)) "Run ended"))))

(deftest multi-steal-archives
  ;; stealing multiple agendas from archives
  (do-game
    (new-game (default-contestant [(qty "Breaking News" 3)])
              (default-hero))
    (trash-from-hand state :contestant "Breaking News")
    (trash-from-hand state :contestant "Breaking News")
    (trash-from-hand state :contestant "Breaking News")
    (take-credits state :contestant)
    (run-empty-server state :archives)
    (prompt-choice :hero "Breaking News")
    (prompt-choice :hero "Steal")
    (prompt-choice :hero "Breaking News")
    (prompt-choice :hero "Steal")
    (prompt-choice :hero "Breaking News")
    (prompt-choice :hero "Steal")
    (is (= 3 (count (:scored (get-hero)))) "3 agendas stolen")
    (is (empty (:discard (get-contestant))) "0 agendas left in archives")))
