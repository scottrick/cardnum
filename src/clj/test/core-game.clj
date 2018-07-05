(in-ns 'test.core)

(deftest contestant-reveal-unique
  ;; Revealing a second copy of a unique Contestant card
  (do-game
    (new-game (default-contestant [(qty "Caprcharacter Nisei" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Caprcharacter Nisei" "HQ")
    (play-from-hand state :contestant "Caprcharacter Nisei" "R&D")
    (core/reveal state :contestant (get-content state :hq 0))
    (is (:revealed (get-content state :hq 0)) "First Caprcharacter revealed")
    (core/reveal state :contestant (get-content state :rd 0))
    (is (not (:revealed (get-content state :rd 0))) "Second Caprcharacter could not be revealed")))

(deftest challenger-place-resource
  ;; challenger-place - Resource; ensure costs are paid
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Gordian Blade" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Gordian Blade")
    (let [gord (get-in @state [:challenger :rig :resource 0])]
      (is (= (- 5 (:cost gord)) (:credit (get-challenger))) "Resource cost was applied")
      (is (= (- 4 (:memoryunits gord)) (:memory (get-challenger))) "Resource MU was applied"))))

(deftest challenger-placing-uniques
  ;; Placing a copy of an active unique Challenger card is prevented
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Kati Jones" 2) (qty "Scheherazade" 2)
                               (qty "Off-Campus Apartment" 1) (qty "Hivemind" 2)]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 1 :memory 2)
    (core/draw state :challenger 2)
    (play-from-hand state :challenger "Kati Jones")
    (play-from-hand state :challenger "Off-Campus Apartment")
    (play-from-hand state :challenger "Scheherazade")
    (let [oca (get-in @state [:challenger :rig :muthereff 1])
          scheh (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger scheh 0)
      (prompt-select :challenger (find-card "Hivemind" (:hand (get-challenger))))
      (is (= "Hivemind" (:title (first (:hosted (refresh scheh))))) "Hivemind hosted on Scheherazade")
      (play-from-hand state :challenger "Kati Jones")
      (is (= 1 (:click (get-challenger))) "Not charged a click")
      (is (= 2 (count (get-in @state [:challenger :rig :muthereff]))) "2nd copy of Kati couldn't place")
      (card-ability state :challenger oca 0)
      (prompt-select :challenger (find-card "Kati Jones" (:hand (get-challenger))))
      (is (empty? (:hosted (refresh oca))) "2nd copy of Kati couldn't be hosted on OCA")
      (is (= 1 (:click (get-challenger))) "Not charged a click")
      (play-from-hand state :challenger "Hivemind")
      (is (= 1 (count (get-in @state [:challenger :rig :resource]))) "2nd copy of Hivemind couldn't place")
      (card-ability state :challenger scheh 0)
      (prompt-select :challenger (find-card "Hivemind" (:hand (get-challenger))))
      (is (= 1 (count (:hosted (refresh scheh)))) "2nd copy of Hivemind couldn't be hosted on Scheherazade")
      (is (= 1 (:click (get-challenger))) "Not charged a click"))))

(deftest deactivate-resource
  ;; deactivate - Resource; ensure MU are restored
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Gordian Blade" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Gordian Blade")
    (let [gord (get-in @state [:challenger :rig :resource 0])]
      (core/discard state :challenger gord)
      (is (= 4 (:memory (get-challenger))) "Discarding the resource restored MU"))))

(deftest agenda-forfeit-challenger
  ;; forfeit - Don't deactivate agenda to trigger leave play effects if Challenger forfeits a stolen agenda
  (do-game
    (new-game (default-contestant [(qty "Mandatory Regions" 1)])
              (default-challenger [(qty "Data Dealer" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Data Dealer")
    (run-empty-locale state "HQ")
    (prompt-choice :challenger "Steal")
    (is (= 2 (:agenda-point (get-challenger))))
    (card-ability state :challenger (get-muthereff state 0) 0)
    (prompt-select :challenger (get-scored state :challenger 0))
    (is (= 1 (:click (get-challenger))) "Didn't lose a click")
    (is (= 4 (:click-per-turn (get-challenger))) "Still have 4 clicks per turn")))

(deftest agenda-forfeit-contestant
  ;; forfeit - Deactivate agenda to trigger leave play effects if Contestant forfeits a scored agenda
  (do-game
    (new-game (default-contestant [(qty "Mandatory Regions" 1) (qty "Contestantorate Town" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Mandatory Regions" "New party")
    (score-agenda state :contestant (get-content state :party1 0))
    (is (= 4 (:click-per-turn (get-contestant))) "Up to 4 clicks per turn")
    (play-from-hand state :contestant "Contestantorate Town" "New party")
    (let [ctown (get-content state :party2 0)]
      (core/reveal state :contestant ctown)
      (prompt-select :contestant (get-scored state :contestant 0))
      (is (= 3 (:click-per-turn (get-contestant))) "Back down to 3 clicks per turn"))))

(deftest refresh-recurring-credits-hosted
  ;; host - Recurring credits on cards hosted after place refresh properly
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3) (qty "Hedge Fund" 3)])
              (default-challenger [(qty "Compromised Employee" 1) (qty "Off-Campus Apartment" 1)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant 2)
    (play-from-hand state :challenger "Off-Campus Apartment")
    (play-from-hand state :challenger "Compromised Employee")
    (let [iwall (get-character state :hq 0)
          apt (get-in @state [:challenger :rig :muthereff 0])]
      (card-ability state :challenger apt 1) ; use Off-Campus option to host an placed card
      (prompt-select :challenger (find-card "Compromised Employee"
                                        (get-in @state [:challenger :rig :muthereff])))
      (let [cehosted (first (:hosted (refresh apt)))]
        (card-ability state :challenger cehosted 0) ; take Comp Empl credit
        (is (= 4 (:credit (get-challenger))))
        (is (= 0 (:rec-counter (refresh cehosted))))
        (core/reveal state :contestant iwall)
        (is (= 5 (:credit (get-challenger))) "Compromised Employee gave 1 credit from character reveal")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (:rec-counter (refresh cehosted)))
            "Compromised Employee recurring credit refreshed")))))

(deftest card-str-test-simple
  ;; ensure card-str names cards in simple situations properly
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3) (qty "Jackson Howard" 2)])
              (default-challenger [(qty "Corroder" 1)
                               (qty "Clone Chip" 1)
                               (qty "Paparazzi" 1)
                               (qty "Parasite" 1)]))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (play-from-hand state :contestant "Jackson Howard" "New party")
    (play-from-hand state :contestant "Jackson Howard" "New party")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (core/end-turn state :contestant nil)
    (core/start-turn state :challenger nil)
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "Clone Chip")
    (play-from-hand state :challenger "Paparazzi")
    (play-from-hand state :challenger "Parasite")
    (let [hqiwall0 (get-character state :hq 0)
          hqiwall1 (get-character state :hq 1)
          rdiwall (get-character state :rd 0)
          jh1 (get-content state :party1 0)
          jh2 (get-content state :party2 0)
          corr (get-in @state [:challenger :rig :resource 0])
          cchip (get-in @state [:challenger :rig :hazard 0])
          pap (get-in @state [:challenger :rig :muthereff 0])]
      (core/reveal state :contestant hqiwall0)
      (core/reveal state :contestant jh1)
      (prompt-select :challenger (refresh hqiwall0))
      (is (= (core/card-str state (refresh hqiwall0)) "Ice Wall protecting HQ at position 0"))
      (is (= (core/card-str state (refresh hqiwall1)) "Character protecting HQ at position 1"))
      (is (= (core/card-str state (refresh rdiwall)) "Character protecting R&D at position 0"))
      (is (= (core/card-str state (refresh rdiwall) {:visible true})
             "Ice Wall protecting R&D at position 0"))
      (is (= (core/card-str state (refresh jh1)) "Jackson Howard in Locale 1"))
      (is (= (core/card-str state (refresh jh2)) "a card in Locale 2"))
      (is (= (core/card-str state (refresh corr)) "Corroder"))
      (is (= (core/card-str state (refresh cchip)) "Clone Chip"))
      (is (= (core/card-str state (refresh pap)) "Paparazzi"))
      (is (= (core/card-str state (first (:hosted (refresh hqiwall0))))
             "Parasite hosted on Ice Wall protecting HQ at position 0")))))

(deftest invalid-score-attempt
  ;; Test scoring with an incorrect number of advancement tokens
  (do-game
    (new-game (default-contestant [(qty "Ancestral Imager" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Ancestral Imager" "New party")
    (let [ai (get-content state :party1 0)]
      ;; Trying to score without any tokens does not do anything
      (is (not (find-card "Ancestral Imager" (:scored (get-contestant)))) "AI not scored")
      (is (not (nil? (get-content state :party1 0))))
      (core/advance state :contestant {:card (refresh ai)})
      (core/score state :contestant {:card (refresh ai)})
      (is (not (nil? (get-content state :party1 0)))))))

(deftest discard-contestant-hosted
  ;; Hosted Contestant cards are included in all-placed and fire leave-play effects when discarded
  (do-game
    (new-game (default-contestant [(qty "Full Immersion RecStudio" 1) (qty "Worlds Plaza" 1) (qty "Director Haas" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Full Immersion RecStudio" "New party")
    (let [fir (get-content state :party1 0)]
      (core/reveal state :contestant fir)
      (card-ability state :contestant fir 0)
      (prompt-select :contestant (find-card "Worlds Plaza" (:hand (get-contestant))))
      (let [wp (first (:hosted (refresh fir)))]
        (core/reveal state :contestant wp)
        (card-ability state :contestant wp 0)
        (prompt-select :contestant (find-card "Director Haas" (:hand (get-contestant))))
        (let [dh (first (:hosted (refresh wp)))]
          (is (:revealed dh) "Director Haas was revealed")
          (is (= 0 (:credit (get-contestant))) "Contestant has 0 credits")
          (is (= 4 (:click-per-turn (get-contestant))) "Contestant has 4 clicks per turn")
          (is (= 3 (count (core/all-placed state :contestant))) "all-placed counting hosted Contestant cards")
          (take-credits state :contestant)
          (run-empty-locale state "Locale 1")
          (prompt-select :challenger dh)
          (prompt-choice :challenger "Yes") ; discard Director Haas
          (prompt-choice :challenger "Done")
          (is (= 3 (:click-per-turn (get-contestant))) "Contestant down to 3 clicks per turn"))))))

(deftest discard-remove-per-turn-restriction
  ;; Discarding a card should remove it from [:per-turn] - Issue #1345
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 3)])
              (default-challenger [(qty "Imp" 2) (qty "Scavenge" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 1)
    (play-from-hand state :challenger "Imp")
    (let [imp (get-resource state 0)]
      (run-empty-locale state "HQ")
      (card-ability state :challenger imp 0)
      (is (= 1 (count (:discard (get-contestant)))) "Accessed Hedge Fund is discarded")
      (run-empty-locale state "HQ")
      (card-ability state :challenger imp 0)
      (is (= 1 (count (:discard (get-contestant)))) "Card can't be discarded, Imp already used this turn")
      (prompt-choice :challenger "OK")
      (play-from-hand state :challenger "Scavenge")
      (prompt-select :challenger imp)
      (prompt-select :challenger (find-card "Imp" (:discard (get-challenger)))))
    (let [imp (get-resource state 0)]
      (is (= 2 (get-counters (refresh imp) :virus)) "Replaced Imp has 2 counters")
      (run-empty-locale state "HQ")
      (card-ability state :challenger imp 0))
    (is (= 2 (count (:discard (get-contestant)))) "Hedge Fund discarded, replaced Imp used on same turn")))

(deftest discard-seen-and-unseen
  ;; Discard placed sites that are both seen and unseen by challenger
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 3)])
              (default-challenger))
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (take-credits state :contestant 1)
    (run-empty-locale state "Locale 1")
    (prompt-choice :challenger "No")
    ;; run and discard the second site
    (run-empty-locale state "Locale 2")
    (prompt-choice :challenger "Yes")
    (take-credits state :challenger 2)
    (play-from-hand state :contestant "PAD Campaign" "Locale 1")
    (prompt-choice :contestant "OK")
    (is (= 2 (count (:discard (get-contestant)))) "Discarded existing site")
    (is (:seen (first (get-in @state [:contestant :discard]))) "Site discarded by challenger is Seen")
    (is (not (:seen (second (get-in @state [:contestant :discard]))))
        "Site discarded by contestant is Unseen")
    (is (not (:seen (get-content state :party1 0))) "New site is unseen")))

(deftest replace-seen-site
  ;; Place a faceup card in Archives, make sure it is not :seen
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 1) (qty "Interns" 1)])
              (default-challenger))
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (take-credits state :contestant 2)
    ;; run and discard the site
    (run-empty-locale state "Locale 1")
    (prompt-choice :challenger "Yes")
    (is (:seen (first (get-in @state [:contestant :discard]))) "Site discarded by challenger is Seen")
    (take-credits state :challenger 3)
    (play-from-hand state :contestant "Interns")
    (prompt-select :contestant (first (get-in @state [:contestant :discard])))
    (prompt-choice :contestant "New party")
    (is (not (:seen (get-content state :party2 0))) "New site is unseen")))

(deftest all-placed-challenger-test
  ;; Tests all-placed for resources hosted on Character, nested hosted resources, and non-placed hosted resources
  (do-game
    (new-game (default-contestant [(qty "Wraparound" 1)])
              (default-challenger [(qty "Omni-drive" 1) (qty "Personal Workshop" 1) (qty "Leprechaun" 1) (qty "Corroder" 1) (qty "Mimic" 1) (qty "Knight" 1)]))
    (play-from-hand state :contestant "Wraparound" "HQ")
    (let [wrap (get-character state :hq 0)]
      (core/reveal state :contestant wrap)
      (take-credits state :contestant)
      (core/draw state :challenger)
      (core/gain state :challenger :credit 7)
      (play-from-hand state :challenger "Knight")
      (play-from-hand state :challenger "Personal Workshop")
      (play-from-hand state :challenger "Omni-drive")
      (take-credits state :contestant)
      (let [kn (get-in @state [:challenger :rig :resource 0])
            pw (get-in @state [:challenger :rig :muthereff 0])
            od (get-in @state [:challenger :rig :hazard 0])
            co (find-card "Corroder" (:hand (get-challenger)))
            le (find-card "Leprechaun" (:hand (get-challenger)))]
        (card-ability state :challenger kn 0)
        (prompt-select :challenger wrap)
        (card-ability state :challenger pw 0)
        (prompt-select :challenger co)
        (card-ability state :challenger od 0)
        (prompt-select :challenger le)
        (let [od (refresh od)
              le (first (:hosted od))
              mi (find-card "Mimic" (:hand (get-challenger)))]
          (card-ability state :challenger le 0)
          (prompt-select :challenger mi)
          (let [all-placed (core/all-placed state :challenger)]
            (is (= 5 (count all-placed)) "Number of placed challenger cards is correct")
            (is (not-empty (filter #(= (:title %) "Leprechaun") all-placed)) "Leprechaun is in all-placed")
            (is (not-empty (filter #(= (:title %) "Personal Workshop") all-placed)) "Personal Workshop is in all-placed")
            (is (not-empty (filter #(= (:title %) "Mimic") all-placed)) "Mimic is in all-placed")
            (is (not-empty (filter #(= (:title %) "Omni-drive") all-placed)) "Omni-drive is in all-placed")
            (is (not-empty (filter #(= (:title %) "Knight") all-placed)) "Knight is in all-placed")
            (is (empty (filter #(= (:title %) "Corroder") all-placed)) "Corroder is not in all-placed")))))))

(deftest log-accessed-names
  ;; Check that accessed card names are logged - except those on R&D, and no logs on archives
  (do-game
    (new-game
      (default-contestant [(qty "PAD Campaign" 7)])
      (default-challenger))
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (discard-from-hand state :contestant "PAD Campaign")
    (take-credits state :contestant)
    (run-empty-locale state :hq)
    (prompt-choice :challenger "No") ; Dismiss discard prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-locale state :rd)
    (is (last-log-contains? state "an unseen card") "Accessed card name was not logged")
    (run-empty-locale state :party1)
    (prompt-choice :challenger "No") ; Dismiss discard prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")))

(deftest counter-manipulation-commands
  ;; Test interactions of various cards with /counter and /adv-counter commands
  (do-game
    (new-game (default-contestant [(qty "Adonis Campaign" 1)
                             (qty "Public Support" 2)
                             (qty "Oaktown Renovation" 1)])
              (default-challenger))
    ;; Turn 1 Contestant, place oaktown and sites
    (core/gain state :contestant :click 4)
    (play-from-hand state :contestant "Adonis Campaign" "New party")
    (play-from-hand state :contestant "Public Support" "New party")
    (play-from-hand state :contestant "Public Support" "New party")
    (play-from-hand state :contestant "Oaktown Renovation" "New party")
    (let [adonis (get-content state :party1 0)
          publics1 (get-content state :party2 0)
          publics2 (get-content state :party3 0)
          oaktown (get-content state :party4 0)]
    (core/advance state :contestant {:card (refresh oaktown)})
    (core/advance state :contestant {:card (refresh oaktown)})
    (core/advance state :contestant {:card (refresh oaktown)})
    (is (= 8 (:credit (get-contestant))) "Contestant 5+3 creds from Oaktown")
    (core/end-turn state :contestant nil)

    ;; Turn 1 Challenger
    (core/start-turn state :challenger nil)
    (take-credits state :challenger 3)
    (core/click-credit state :challenger nil)
    (core/end-turn state :challenger nil)
    (core/reveal state :contestant (refresh adonis))
    (core/reveal state :contestant (refresh publics1))

    ;; Turn 2 Contestant
    (core/start-turn state :contestant nil)
    (core/reveal state :contestant (refresh publics2))
    (is (= 3 (:click (get-contestant))))
    (is (= 3 (:credit (get-contestant))) "only Adonis money")
    (is (= 9 (get-counters (refresh adonis) :credit)))
    (is (= 2 (get-counters (refresh publics1) :power)))
    (is (= 3 (get-counters (refresh publics2) :power)))

    ;; oops, forgot to reveal 2nd public support before start of turn,
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

    ;; Turn 2 Challenger
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
    (take-credits state :challenger)

    ;; Turn 3 Contestant
    (is (= 3 (:agenda-point (get-contestant)))) ; cheated PS1 should get scored
    (is (= 9 (:credit (get-contestant))))
    (is (= (:zone (refresh publics1) :scored)))
    (is (= (:zone (refresh publics2)) [:locales :party3 :content]))
    (is (= (:zone (refresh adonis) :discard)))
    (take-credits state :contestant)

    ;; Turn 3 Challenger
    (take-credits state :challenger)

    ;; Turn 4 Contestant
    (is (= 4 (:agenda-point (get-contestant)))) ; PS2 should get scored
    (is (= (:zone (refresh publics2) :scored)))
    (is (= 12 (:credit (get-contestant)))))))

(deftest run-bad-publicity-credits
  ;; Should not lose BP credits until a run is completely over. Issue #1721.
  (do-game
    (new-game (default-contestant [(qty "Cyberdex Virus Suite" 3)])
              (make-deck "Valencia Esteveveal: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant starts with 1 BP")
    (play-from-hand state :contestant "Cyberdex Virus Suite" "New party")
    (play-from-hand state :contestant "Cyberdex Virus Suite" "R&D")
    (play-from-hand state :contestant "Cyberdex Virus Suite" "HQ")
    (take-credits state :contestant)
    (run-empty-locale state :party1)
    (prompt-choice :contestant "No")
    (prompt-choice :challenger "Yes")
    (is (= 5 (:credit (get-challenger))) "1 BP credit spent to discard CVS")
    (run-empty-locale state :hq)
    (prompt-choice :contestant "No")
    (prompt-choice :challenger "Yes")
    (is (= 5 (:credit (get-challenger))) "1 BP credit spent to discard CVS")
    (run-empty-locale state :rd)
    (prompt-choice :contestant "No")
    (prompt-choice :challenger "Yes")
    (is (= 5 (:credit (get-challenger))) "1 BP credit spent to discard CVS")))

(deftest run-psi-bad-publicity-credits
  ;; Should pay from Bad Pub for Psi games during run #2374
  (do-game
    (new-game (default-contestant [(qty "Caprcharacter Nisei" 3)])
              (make-deck "Valencia Esteveveal: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant starts with 1 BP")
    (play-from-hand state :contestant "Caprcharacter Nisei" "New party")
    (take-credits state :contestant)
    (let [caprcharacter (get-content state :party1 0)]
      (core/reveal state :contestant caprcharacter)
      (run-on state "Locale 1")
      (is (prompt-is-card? :contestant caprcharacter) "Caprcharacter prompt even with no character, once challenger makes run")
      (is (prompt-is-card? :challenger caprcharacter) "Challenger has Caprcharacter prompt")
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :challenger "1 [Credits]")
      (is (= 5 (:credit (get-challenger))) "Challenger spend bad pub credit on psi game")
      (is (= 3 (:credit (get-contestant))) "Contestant spent 2 on psi game"))))

(deftest purge-nested
  ;; Purge nested-hosted virus counters
  (do-game
    (new-game (default-contestant [(qty "Cyberdex Trial" 1)])
              (default-challenger [(qty "Djinn" 1) (qty "Imp" 1) (qty "Leprechaun" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 100)
    (play-from-hand state :challenger "Leprechaun")
    (let [lep (get-resource state 0)]
      (card-ability state :challenger lep 0)
      (prompt-select :challenger (find-card "Djinn" (:hand (get-challenger))))
      (let [djinn (first (:hosted (refresh lep)))]
        (card-ability state :challenger djinn 1)
        (prompt-select :challenger (find-card "Imp" (:hand (get-challenger))))
        (let [imp (first (:hosted (refresh djinn)))]
          (is (= 2 (get-counters imp :virus)) "Imp has 2 virus counters")
          (take-credits state :challenger)
          (play-from-hand state :contestant "Cyberdex Trial")
          (is (= 0 (get-counters (refresh imp) :virus)) "Imp counters purged"))))))

(deftest multi-access-rd
  ;; multi-access of R&D sees all cards and regions
  (do-game
    (new-game (default-contestant [(qty "Keegan Lane" 1) (qty "Midway Station Grid" 1)
                             (qty "Sweeps Week" 1) (qty "Manhunt" 1)
                             (qty "Hedge Fund" 1) (qty "Big Brother" 1)])
              (default-challenger [(qty "Medium" 1)]))
    (play-from-hand state :contestant "Keegan Lane" "R&D")
    (play-from-hand state :contestant "Midway Station Grid" "R&D")
    (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Sweeps Week" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Manhunt" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Big Brother" (:hand (get-contestant))) :deck)
    (core/reveal state :contestant (get-content state :rd 1))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Medium")
    (let [keegan (get-content state :rd 0)
          msg (get-content state :rd 1)
          med (get-resource state 0)]
      (core/command-counter state :challenger ["virus" 2])
      (prompt-select :challenger (refresh med))
      (run-empty-locale state :rd)
      (prompt-choice :challenger 2)
      (prompt-choice :challenger "Card from deck")
      (is (= "Hedge Fund" (-> (get-challenger) :prompt first :card :title)))
      (prompt-choice :challenger "OK")
      (prompt-choice :challenger "Unrevealed region in R&D")
      (is (= "Keegan Lane" (-> (get-challenger) :prompt first :card :title)))
      (prompt-choice :challenger "No")
      (prompt-choice :challenger "Card from deck")
      (is (= "Sweeps Week" (-> (get-challenger) :prompt first :card :title)))
      (prompt-choice :challenger "OK")
      (prompt-choice :challenger "Midway Station Grid")
      (is (= "Midway Station Grid" (-> (get-challenger) :prompt first :card :title)))
      (prompt-choice :challenger "No")
      (prompt-choice :challenger "Card from deck")
      (is (= "Manhunt" (-> (get-challenger) :prompt first :card :title)))
      (prompt-choice :challenger "OK")
      (is (not (:run @state)) "Run ended"))))

(deftest multi-steal-archives
  ;; stealing multiple agendas from archives
  (do-game
    (new-game (default-contestant [(qty "Breaking News" 3)])
              (default-challenger))
    (discard-from-hand state :contestant "Breaking News")
    (discard-from-hand state :contestant "Breaking News")
    (discard-from-hand state :contestant "Breaking News")
    (take-credits state :contestant)
    (run-empty-locale state :archives)
    (prompt-choice :challenger "Breaking News")
    (prompt-choice :challenger "Steal")
    (prompt-choice :challenger "Breaking News")
    (prompt-choice :challenger "Steal")
    (prompt-choice :challenger "Breaking News")
    (prompt-choice :challenger "Steal")
    (is (= 3 (count (:scored (get-challenger)))) "3 agendas stolen")
    (is (empty (:discard (get-contestant))) "0 agendas left in archives")))
