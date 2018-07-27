(ns game-test.cards.characterbreakers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "characterbreakers"))

(deftest ^:skip-card-coverage
  auto-pump-breakers
  ;; Breaker get a dynamic ability that matches the strength of the encountered character
  (testing "Single pump"
   (do-game
     (new-game (default-contestant ["Masvingo"])
               (default-challenger ["Laamb"]))
     (play-from-hand state :contestant "Masvingo" "HQ")
     (core/reveal state :contestant (get-character state :hq 0))
     (take-credits state :contestant)
     (core/gain state :challenger :credit 5)
     (play-from-hand state :challenger "Laamb")
     (run-on state "HQ")
     (let [laamb (get-resource state 0)]
       (is (= 2 (:current-strength (refresh laamb))) "Laamb starts at 2 strength")
       (is (= 6 (:credit (get-challenger))) "Spent 4 to place")
       (core/play-dynamic-ability state :challenger {:dynamic "auto-pump" :card (refresh laamb)})
       (is (= 8 (:current-strength (refresh laamb))) "Laamb is at 8 strength")
       (is (= 3 (:credit (get-challenger))) "Spent 3 to pump"))))
  (testing "Multi pump"
   (do-game
     (new-game (default-contestant ["Masvingo"])
               (default-challenger ["Ankusa"]))
     (play-from-hand state :contestant "Masvingo" "HQ")
     (core/reveal state :contestant (get-character state :hq 0))
     (take-credits state :contestant)
     (core/gain state :challenger :credit 5)
     (play-from-hand state :challenger "Ankusa")
     (run-on state "HQ")
     (let [ank (get-resource state 0)]
       (is (zero? (:current-strength (refresh ank))) "Ankusa starts at 1 strength")
       (is (= 4 (:credit (get-challenger))) "Spent 6 to place")
       (core/play-dynamic-ability state :challenger {:dynamic "auto-pump" :card (refresh ank)})
       (is (= 3 (:current-strength (refresh ank))) "Ankusa is at 3 strength")
       (is (= 1 (:credit (get-challenger))) "Spent 3 to pump")))))

(deftest adept
  ;; Adept - +1 str for each unused MU
  (do-game
    (new-game (default-contestant) (default-challenger ["Adept" "Box-E"]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Adept")
    (let [ad (get-resource state 0)]
      (is (= 2 (core/available-mu state)))
      (is (= 4 (:current-strength (refresh ad))) "+2 strength for 2 unused MU")
      (play-from-hand state :challenger "Box-E")
      (is (= 4 (core/available-mu state)))
      (is (= 6 (:current-strength (refresh ad))) "+4 strength for 4 unused MU"))))

(deftest atman
  ;; Atman
  (testing "Placing with 0 power counters"
    (do-game
      (new-game (default-contestant) (default-challenger ["Atman"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Atman")
      (prompt-choice :challenger 0)
      (is (= 3 (core/available-mu state)))
      (let [atman (get-resource state 0)]
        (is (zero? (get-counters atman :power)) "0 power counters")
        (is (zero? (:current-strength atman)) "0 current strength"))))
  (testing "Placing with 2 power counters"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Atman"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Atman")
      (prompt-choice :challenger 2)
      (is (= 3 (core/available-mu state)))
      (let [atman (get-resource state 0)]
        (is (= 2 (get-counters atman :power)) "2 power counters")
        (is (= 2 (:current-strength atman)) "2 current strength")))))

(deftest aumakua
  ;; Aumakua - Gain credit on no-discard
  (testing "Gain counter on no discard"
    (do-game
      (new-game (default-contestant [(qty "PAD Campaign" 3)])
                (default-challenger ["Aumakua"]))
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Aumakua")
      (run-empty-locale state "Locale 1")
      (prompt-choice :challenger "No action")
      (is (= 1 (get-counters (get-resource state 0) :virus)) "Aumakua gains virus counter from no-discard")
      (core/gain state :challenger :credit 5)
      (run-empty-locale state "Locale 1")
      (prompt-choice-partial :challenger "Pay")
      (is (= 1 (get-counters (get-resource state 0) :virus)) "Aumakua does not gain virus counter from discard")))
  (testing "Gain counters on empty archives"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Aumakua"])
                {:start-as :challenger})
      (play-from-hand state :challenger "Aumakua")
      (run-empty-locale state :archives)
      (is (= 1 (get-counters (get-resource state 0) :virus)) "Aumakua gains virus counter from accessing empty Archives")))
  (testing "Neutralize All Threats interaction"
    (do-game
      (new-game (default-contestant [(qty "PAD Campaign" 3)])
                (default-challenger ["Aumakua" "Neutralize All Threats"]))
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Aumakua")
      (play-from-hand state :challenger "Neutralize All Threats")
      (core/gain state :challenger :credit 5)
      (run-empty-locale state "Locale 1")
      (is (zero? (get-counters (get-resource state 0) :virus)) "Aumakua does not gain virus counter from ABT-forced discard"))))

(deftest baba-yaga
  ;; Baba Yaga
  (do-game
    (new-game
      (default-contestant)
      (default-challenger ["Baba Yaga" "Faerie" "Yog.0" "Sharpshooter"]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Baba Yaga")
    (play-from-hand state :challenger "Sharpshooter")
    (let [baba (get-resource state 0)
          base-abicount (count (:abilities baba))]
      (card-ability state :challenger baba 0)
      (prompt-select :challenger (find-card "Faerie" (:hand (get-challenger))))
      (is (= (+ 2 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 2 subroutines from Faerie")
      (card-ability state :challenger (refresh baba) 0)
      (prompt-select :challenger (find-card "Yog.0" (:hand (get-challenger))))
      (is (= (+ 3 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 1 subroutine from Yog.0")
      (core/discard state :challenger (first (:hosted (refresh baba))))
      (is (= (inc base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga lost 2 subroutines from discarded Faerie")
      (card-ability state :challenger baba 1)
      (prompt-select :challenger (find-card "Sharpshooter" (:resource (:rig (get-challenger)))))
      (is (= 2 (count (:hosted (refresh baba)))) "Faerie and Sharpshooter hosted on Baba Yaga")
      (is (= 1 (core/available-mu state)) "1 MU left with 2 breakers on Baba Yaga")
      (is (= 4 (:credit (get-challenger))) "-5 from Baba, -1 from Sharpshooter played into Rig, -5 from Yog"))))

(deftest ^{:card-title "cerberus-\"rex\"-h2"}
  cerberus-rex-h2
  ;; Cerberus "Rex" H2 - boost 1 for 1 cred, break for 1 counter
  (do-game
   (new-game (default-contestant)
             (default-challenger ["Cerberus \"Rex\" H2"]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Cerberus \"Rex\" H2")
   (is (= 2 (:credit (get-challenger))) "2 credits left after place")
   (let [rex (get-resource state 0)]
     (is (= 4 (get-counters rex :power)) "Start with 4 counters")
     ;; boost strength
     (card-ability state :challenger rex 1)
     (is (= 1 (:credit (get-challenger))) "Spend 1 credit to boost")
     (is (= 2 (:current-strength (refresh rex))) "At strength 2 after boost")
     ;; break
     (card-ability state :challenger rex 0)
     (is (= 1 (:credit (get-challenger))) "No credits spent to break")
     (is (= 3 (get-counters (refresh rex) :power)) "One counter used to break"))))

(deftest chameleon
  ;; Chameleon - Place on contestant turn, only returns to hand at end of challenger's turn
  (testing "with Clone Chip"
    (do-game
      (new-game (default-contestant) (default-challenger ["Chameleon" "Clone Chip"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Clone Chip")
      (core/move state :challenger (find-card "Chameleon" (:hand (get-challenger))) :discard)
      (take-credits state :challenger)
      (is (zero? (count (:hand (get-challenger)))))
      ;; Place Chameleon on contestant turn
      (take-credits state :contestant 1)
      (let [chip (get-hazard state 0)]
        (card-ability state :challenger chip 0)
        (prompt-select :challenger (find-card "Chameleon" (:discard (get-challenger))))
        (prompt-choice :challenger "Sentry"))
      (take-credits state :contestant)
      (is (zero? (count (:hand (get-challenger)))) "Chameleon not returned to hand at end of contestant turn")
      (take-credits state :challenger)
      (is (= 1 (count (:hand (get-challenger)))) "Chameleon returned to hand at end of challenger's turn")))
  (testing "Returns to hand after hosting. #977"
    (do-game
      (new-game (default-contestant) (default-challenger [(qty "Chameleon" 2) "Scheherazade"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Chameleon")
      (prompt-choice :challenger "Barrier")
      (is (= 3 (:credit (get-challenger))) "-2 from playing Chameleon")
      ;; Host the Chameleon on Scheherazade that was just played (as in Personal Workshop/Hayley ability scenarios)
      (play-from-hand state :challenger "Scheherazade")
      (let [scheherazade (get-resource state 1)]
        (card-ability state :challenger scheherazade 1) ; Host an placed resource
        (prompt-select :challenger (find-card "Chameleon" (:resource (:rig (get-challenger)))))
        (is (= 4 (:credit (get-challenger))) "+1 from hosting onto Scheherazade")
        ;; Place another Chameleon directly onto Scheherazade
        (card-ability state :challenger scheherazade 0) ; Place and host a resource from Grip
        (prompt-select :challenger (find-card "Chameleon" (:hand (get-challenger))))
        (prompt-choice :challenger "Code Gate")
        (is (= 2 (count (:hosted (refresh scheherazade)))) "2 Chameleons hosted on Scheherazade")
        (is (= 3 (:credit (get-challenger))) "-2 from playing Chameleon, +1 from placing onto Scheherazade"))
      (is (zero? (count (:hand (get-challenger)))) "Both Chameleons in play - hand size 0")
      (take-credits state :challenger)
      (is (= 2 (count (:hand (get-challenger)))) "Both Chameleons returned to hand - hand size 2"))))

(deftest crypsis
  ;; Crypsis - Loses a virus counter after encountering character it broke
  (do-game
    (new-game (default-contestant ["Character Wall"])
              (default-challenger [(qty "Crypsis" 2)]))
    (play-from-hand state :contestant "Character Wall" "Archives")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 100)
    (play-from-hand state :challenger "Crypsis")
    (let [crypsis (get-resource state 0)]
      (card-ability state :challenger crypsis 2)
      (is (= 1 (get-counters (refresh crypsis) :virus))
          "Crypsis has 1 virus counter")
      (run-on state "Archives")
      (core/reveal state :contestant (get-character state :archives 0))
      (card-ability state :challenger (refresh crypsis) 0) ; Match strength
      (card-ability state :challenger (refresh crypsis) 1) ; Break
      (is (= 1 (get-counters (refresh crypsis) :virus))
          "Crypsis has 1 virus counter")
      (run-continue state)
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-on state "Archives")
      (card-ability state :challenger (refresh crypsis) 0) ; Match strength
      (card-ability state :challenger (refresh crypsis) 1) ; Break
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-challenger)))))
          "Crypsis was discarded"))
    (take-credits state :challenger)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Crypsis")
    (let [crypsis (get-resource state 0)]
      (run-on state "Archives")
      (card-ability state :challenger (refresh crypsis) 0) ; Match strength
      (card-ability state :challenger (refresh crypsis) 1) ; Break
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has nil virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-challenger)))))
          "Crypsis was discarded"))))

(deftest darwin
  ;; Darwin - starts at 0 strength
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Darwin"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Darwin")
    (let [darwin (get-resource state 0)]
      (is (zero? (get-counters (refresh darwin) :virus)) "Darwin starts with 0 virus counters")
      (is (zero? (:current-strength (refresh darwin ))) "Darwin starts at 0 strength")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (card-ability state :challenger (refresh darwin) 1) ; add counter
      (is (= 1 (get-counters (refresh darwin) :virus)) "Darwin gains 1 virus counter")
      (is (= 1 (:current-strength (refresh darwin ))) "Darwin is at 1 strength"))))

(deftest deus-x
  (testing "vs Multiple Hostile Infrastructure"
    (do-game
      (new-game
        (default-contestant [(qty "Hostile Infrastructure" 3)])
        (default-challenger [(qty "Deus X" 3) (qty "Sure Gamble" 2)]))
      (play-from-hand state :contestant "Hostile Infrastructure" "New party")
      (play-from-hand state :contestant "Hostile Infrastructure" "New party")
      (play-from-hand state :contestant "Hostile Infrastructure" "New party")
      (core/gain state :contestant :credit 10)
      (core/reveal state :contestant (get-content state :party1 0))
      (core/reveal state :contestant (get-content state :party2 0))
      (core/reveal state :contestant (get-content state :party3 0))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Deus X")
      (run-empty-locale state "Locale 1")
      (prompt-choice-partial :challenger "Pay")
      (let [dx (get-resource state 0)]
        (card-ability state :challenger dx 1)
        (prompt-choice :challenger "Done")
        (is (= 2 (count (:hand (get-challenger)))) "Deus X prevented one Hostile net damage"))))
  (testing "vs Multiple sources of net damage"
    (do-game
      (new-game
        (make-deck "Cardnum: Personal Evolution" [(qty "Fetal AI" 6)])
        (default-challenger [(qty "Deus X" 3) (qty "Sure Gamble" 2)]))
      (play-from-hand state :contestant "Fetal AI" "New party")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Deus X")
      (run-empty-locale state "Locale 1")
      (let [dx (get-resource state 0)]
        (card-ability state :challenger dx 1)
        (prompt-choice :challenger "Done")
        (prompt-choice-partial :challenger "Pay")
        (is (= 3 (count (:hand (get-challenger)))) "Deus X prevented net damage from accessing Fetal AI, but not from Personal Evolution")
        (is (= 1 (count (:scored (get-challenger)))) "Fetal AI stolen")))))

(deftest faerie
  ;; Faerie - discard after encounter is over, not before.
  (do-game
    (new-game
      (default-contestant ["Caduceus"])
      (default-challenger ["Faerie"]))
    (play-from-hand state :contestant "Caduceus" "Archives")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Faerie")
    (let [fae (get-resource state 0)]
      (run-on state :archives)
      (core/reveal state :contestant (get-character state :archives 0))
      (card-ability state :challenger fae 0)
      (is (refresh fae) "Faerie not discarded until encounter over")
      (run-continue state)
      (is (find-card "Faerie" (:discard (get-challenger))) "Faerie discarded"))))

(deftest faust
  (testing "Basic test: Pump by discarding"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Faust" (qty "Sure Gamble" 3)]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Faust")
      (let [faust (get-resource state 0)]
        (card-ability state :challenger faust 1)
        (prompt-select :challenger (find-card "Sure Gamble" (:hand (get-challenger))))
        (is (= 4 (:current-strength (refresh faust))) "4 current strength")
        (is (= 1 (count (:discard (get-challenger)))) "1 card discarded"))))
  (testing "Pump does not trigger discard prevention. #760"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Faust"
                                 "Sacrificial Construct"
                                 "Fall Guy"
                                 "Astrolabe"
                                 "Gordian Blade"
                                 "Armitage Codebusting"]))
      (take-credits state :contestant)
      (core/draw state :challenger 1)
      (play-from-hand state :challenger "Faust")
      (play-from-hand state :challenger "Fall Guy")
      (play-from-hand state :challenger "Sacrificial Construct")
      (is (= 2 (count (get-radicle state))) "Radicles placed")
      (let [faust (get-resource state 0)]
        (card-ability state :challenger faust 1)
        (prompt-select :challenger (find-card "Astrolabe" (:hand (get-challenger))))
        (is (empty? (:prompt (get-challenger))) "No discard-prevention prompt for hazard")
        (card-ability state :challenger faust 1)
        (prompt-select :challenger (find-card "Gordian Blade" (:hand (get-challenger))))
        (is (empty? (:prompt (get-challenger))) "No discard-prevention prompt for resource")
        (card-ability state :challenger faust 1)
        (prompt-select :challenger (find-card "Armitage Codebusting" (:hand (get-challenger))))
        (is (empty? (:prompt (get-challenger))) "No discard-prevention prompt for radicle")))))

(deftest femme-fatale
  ;; Femme Fatale counter test
  (do-game
   (new-game (default-contestant ["Character Wall"])
             (default-challenger [(qty "Femme Fatale" 2)]))
   (play-from-hand state :contestant "Character Wall" "HQ")
   (take-credits state :contestant)
   (core/gain state :challenger :credit 18)
   (let [iw (get-character state :hq 0)]
    (play-from-hand state :challenger "Femme Fatale")
    (prompt-select :challenger iw)
    (is (:icon (refresh iw)) "Character Wall has an icon")
    (core/discard state :challenger (get-resource state 0))
    (is (not (:icon (refresh iw))) "Character Wall does not have an icon after Femme discarded")
    (play-from-hand state :challenger "Femme Fatale")
    (prompt-select :challenger iw)
    (is (:icon (refresh iw)) "Character Wall has an icon")
    (core/discard state :contestant iw)
    (is (not (:icon (refresh iw))) "Character Wall does not have an icon after itself discarded"))))

(deftest god-of-war
  ;; God of War - Take 1 tag to place 2 virus counters
  (do-game
   (new-game (default-contestant)
             (default-challenger ["God of War"]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "God of War")
   (take-credits state :challenger)
   (take-credits state :contestant)
   (let [gow (get-resource state 0)]
     (card-ability state :challenger gow 2)
     (is (= 1 (:tag (get-challenger))))
     (is (= 2 (get-counters (refresh gow) :virus)) "God of War has 2 virus counters"))))

(deftest inversificator
  ;; Inversificator shouldn't hook up events for unrevealed character
  (do-game
    (new-game (default-contestant ["Turing" "Kakugo"])
              (default-challenger ["Inversificator" "Sure Gamble"]))
    (play-from-hand state :contestant "Kakugo" "HQ")
    (play-from-hand state :contestant "Turing" "HQ")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Inversificator")
    (let [inv (get-resource state 0)
          tur (get-character state :hq 1)]
      (is (= 1 (count (:hand (get-challenger)))) "Challenger starts with 1 card in hand")
      (run-on state :hq)
      (core/reveal state :contestant (refresh tur))
      (run-continue state)
      (card-ability state :challenger (refresh inv) 0)
      (prompt-select :challenger (get-character state :hq 1))
      (prompt-select :challenger (get-character state :hq 0))
      (run-jack-out state)
      (is (= 1 (count (:hand (get-challenger)))) "Challenger still has 1 card in hand")
      (run-on state :hq)
      (run-continue state)
      (is (= 1 (count (:hand (get-challenger)))) "Kakugo doesn't fire when unrevealed"))))

(deftest mammon
  ;; Mammon - Pay to add X power counters at start of turn, all removed at end of turn
  (do-game
   (new-game (default-contestant)
             (default-challenger ["Mammon"]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Mammon")
   (take-credits state :challenger)
   (take-credits state :contestant)
   (let [mam (get-resource state 0)]
     (card-ability state :challenger mam 0)
     (prompt-choice :challenger 3)
     (is (= 2 (:credit (get-challenger))) "Spent 3 credits")
     (is (= 3 (get-counters (refresh mam) :power)) "Mammon has 3 power counters")
     (take-credits state :challenger)
     (is (zero? (get-counters (refresh mam) :power)) "All power counters removed"))))

(deftest musaazi
  ;; Musaazi gains virus counters on successful runs and can spend virus counters from any placed card
  (do-game
    (new-game (default-contestant ["Lancelot"])
              (default-challenger ["Musaazi" "Imp"]))
    (play-from-hand state :contestant "Lancelot" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Musaazi")
    (play-from-hand state :challenger "Imp")
    (let [lancelot (get-character state :hq 0)
          musaazi (get-resource state 0)
          imp (get-resource state 1)]
      (run-empty-locale state "Archives")
      (is (= 1 (get-counters (refresh musaazi) :virus)) "Musaazi has 1 virus counter")
      (is (= 1 (:current-strength (refresh musaazi))) "Initial Musaazi strength")
      (is (= 2 (get-counters (refresh imp) :virus)) "Initial Imp virus counters")
      (run-on state "HQ")
      (core/reveal state :contestant lancelot)
      (card-ability state :challenger musaazi 1) ; match strength
      (prompt-select :challenger imp)
      (is (= 1 (get-counters (refresh imp) :virus)) "Imp lost 1 virus counter to pump")
      (is (= 2 (:current-strength (refresh musaazi))) "Musaazi strength 2")
      (is (empty? (:prompt (get-challenger))) "No prompt open")
      (card-ability state :challenger musaazi 0)
      (prompt-select :challenger musaazi)
      (prompt-select :challenger imp)
      (prompt-choice :challenger "Done")
      (is (= 0 (get-counters (refresh imp) :virus)) "Imp lost its final virus counter")
      (is (= 0 (get-counters (refresh imp) :virus)) "Musaazi lost its virus counter"))))

(deftest na'not'k
  ;; Na'Not'K - Strength adjusts accordingly when character placed during run
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Architect" "Eli 1.0"])
                (default-challenger ["Na'Not'K"]))
      (play-from-hand state :contestant "Architect" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Na'Not'K")
      (let [nanotk (get-resource state 0)
            architect (get-character state :hq 0)]
        (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
        (run-on state "HQ")
        (core/reveal state :contestant architect)
        (is (= 2 (:current-strength (refresh nanotk))) "1 character on HQ")
        (card-subroutine state :contestant (refresh architect) 1)
        (prompt-select :contestant (find-card "Eli 1.0" (:hand (get-contestant))))
        (prompt-choice :contestant "HQ")
        (is (= 3 (:current-strength (refresh nanotk))) "2 character on HQ")
        (run-jack-out state)
        (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))
  (testing "Strength adjusts accordingly when run redirected to another locale"
    (do-game
      (new-game (default-contestant ["Susanoo-no-Mikoto" "Crick" "Cortex Lock"])
                (default-challenger ["Na'Not'K"]))
      (play-from-hand state :contestant "Cortex Lock" "HQ")
      (play-from-hand state :contestant "Susanoo-no-Mikoto" "HQ")
      (play-from-hand state :contestant "Crick" "Archives")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Na'Not'K")
      (let [nanotk (get-resource state 0)
            susanoo (get-character state :hq 1)]
        (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
        (run-on state "HQ")
        (core/reveal state :contestant susanoo)
        (is (= 3 (:current-strength (refresh nanotk))) "2 character on HQ")
        (card-subroutine state :contestant (refresh susanoo) 0)
        (is (= 2 (:current-strength (refresh nanotk))) "1 character on Archives")
        (run-jack-out state)
        (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength")))))

(deftest overmind
  ;; Overmind - Start with counters equal to unused MU
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Overmind" (qty "Akamatsu Mem Chip" 2)]))
    (take-credits state :contestant)
    (take-credits state :challenger 1)
    (play-from-hand state :challenger "Akamatsu Mem Chip")
    (play-from-hand state :challenger "Akamatsu Mem Chip")
    (is (= 6 (core/available-mu state)))
    (play-from-hand state :challenger "Overmind")
    (is (= 5 (core/available-mu state)))
    (let [ov (get-resource state 0)]
      (is (= 5 (get-counters (refresh ov) :power)) "Overmind has 5 counters"))))

(deftest paperclip
  ;; Paperclip - prompt to place on encounter, but not if another is placed
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Vanilla"])
                (default-challenger [(qty "Paperclip" 2)]))
      (play-from-hand state :contestant "Vanilla" "Archives")
      (take-credits state :contestant)
      (discard-from-hand state :challenger "Paperclip")
      (run-on state "Archives")
      (core/reveal state :contestant (get-character state :archives 0))
      (prompt-choice :challenger "Yes") ; place paperclip
      (run-continue state)
      (run-successful state)
      (is (not (:run @state)) "Run ended")
      (discard-from-hand state :challenger "Paperclip")
      (run-on state "Archives")
      (is (empty? (:prompt (get-challenger))) "No prompt to place second Paperclip")))
  (testing "firing on facedown character shouldn't crash"
    (do-game
      (new-game (default-contestant ["Vanilla"])
                (default-challenger ["Paperclip"]))
      (play-from-hand state :contestant "Vanilla" "Archives")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Paperclip")
      (run-on state "Archives")
      (card-ability state :challenger (get-resource state 0) 0)
      (prompt-choice :challenger 0)))
  (testing "do not show a second place prompt if user said No to first, when multiple are in heap"
    (do-game
      (new-game (default-contestant [(qty "Vanilla" 2)])
                (default-challenger [(qty "Paperclip" 3)]))
      (play-from-hand state :contestant "Vanilla" "Archives")
      (play-from-hand state :contestant "Vanilla" "Archives")
      (take-credits state :contestant)
      (discard-from-hand state :challenger "Paperclip")
      (discard-from-hand state :challenger "Paperclip")
      (discard-from-hand state :challenger "Paperclip")
      (run-on state "Archives")
      (core/reveal state :contestant (get-character state :archives 1))
      (prompt-choice :challenger "No")
      (is (empty? (:prompt (get-challenger))) "No additional prompts to reveal other copies of Paperclip")
      (run-continue state)
      ;; we should get the prompt on a second character even after denying the first.
      (core/reveal state :contestant (get-character state :archives 0))
      (prompt-choice :challenger "No")
      (is (empty? (:prompt (get-challenger))) "No additional prompts to reveal other copies of Paperclip")
      (core/jack-out state :challenger)
      ;; Run again, make sure we get the prompt to place again.
      (run-on state "Archives")
      (prompt-choice :challenger "No")
      (is (empty? (:prompt (get-challenger))) "No additional prompts to reveal other copies of Paperclip"))))

(deftest peregrine
  ;; Peregrine - 2c to return to grip and hide an encountered code gate
  (do-game
    (new-game (default-contestant ["Paper Wall" (qty "Bandwidth" 2)])
              (default-challenger ["Peregrine"]))
    (play-from-hand state :contestant "Bandwidth" "Archives")
    (play-from-hand state :contestant "Bandwidth" "Archives")
    (play-from-hand state :contestant "Paper Wall" "Archives")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 2)
    (play-from-hand state :challenger "Peregrine")
    (let [bw1 (get-character state :archives 0)
          pw (get-character state :archives 2)
          per (get-resource state 0)]
      (run-on state "Archives")
      (core/reveal state :contestant pw)
      (core/reveal state :contestant bw1)
      (card-ability state :challenger per 2)
      (is (and (= 2 (:credit (get-challenger))) (empty? (:hand (get-challenger)))) "Can't use Peregrine on a barrier")
      (run-continue state)
      (card-ability state :challenger per 2)
      (is (and (= 2 (:credit (get-challenger))) (empty? (:hand (get-challenger)))) "Can't use Peregrine on unrevealed code gate")
      (run-continue state)
      (card-ability state :challenger per 2)
      (is (zero? (:credit (get-challenger))) "Spent 2 credits")
      (is (= 1 (count (:hand (get-challenger)))) "Peregrine returned to grip")
      (is (not (:revealed (refresh bw1))) "Bandwidth hidden"))))

(deftest persephone
  ;; Persephone's ability discards cards from R&D, triggering AR-Enhanced Security
  ;; See #3187
  (do-game
    (new-game
      (default-contestant ["Zed 1.0" (qty "Zed 2.0" 3) "AR-Enhanced Security"])
      (default-challenger [(qty "Persephone" 10)]))
    (core/move state :contestant (find-card "Zed 2.0" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Zed 2.0" (:hand (get-contestant))) :deck)
    (play-from-hand state :contestant "AR-Enhanced Security" "New party")
    (score-agenda state :contestant (get-content state :party1 0))
    (play-from-hand state :contestant "Zed 1.0" "Archives")
    (core/reveal state :contestant (get-character state :archives 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Persephone")
    (run-on state "Archives")
    (run-continue state)
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger 2)
    (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")
    (take-credits state :challenger)
    ;; Gotta move the discarded cards back to the deck
    (core/move state :contestant (find-card "Zed 2.0" (:discard (get-contestant))) :deck)
    (core/move state :contestant (find-card "Zed 2.0" (:discard (get-contestant))) :deck)
    (take-credits state :contestant)
    (run-on state "Archives")
    (run-continue state)
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger 2)
    (is (= 2 (:tag (get-challenger))) "Challenger took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")))

(deftest shiv
  ;; Shiv - Gain 1 strength for each placed breaker; no MU cost when 2+ link
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Nasir Meidan: Cyber Explorer" ["Shiv" (qty "Inti" 2)
                                                 "Access to Globalsec"]))
    (is (= 1 (:link (get-challenger))) "1 link")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Shiv")
    (let [shiv (get-resource state 0)]
      (is (= 1 (:current-strength (refresh shiv))) "1 placed breaker; 1 strength")
      (play-from-hand state :challenger "Inti")
      (is (= 2 (:current-strength (refresh shiv))) "2 placed breakers; 2 strength")
      (play-from-hand state :challenger "Inti")
      (is (= 3 (:current-strength (refresh shiv))) "3 placed breakers; 3 strength")
      (is (= 1 (core/available-mu state)) "3 MU consumed")
      (play-from-hand state :challenger "Access to Globalsec")
      (is (= 2 (:link (get-challenger))) "2 link")
      (is (= 2 (core/available-mu state)) "Shiv stops using MU when 2+ link"))))

(deftest snowball
  ;; Snowball - Strength boost until end of run when used to break a subroutine
  (do-game
   (new-game (default-contestant ["Spiderweb" "Fire Wall" "Hedge Fund"])
             (default-challenger ["Snowball"]))
   (play-from-hand state :contestant "Hedge Fund")
   (play-from-hand state :contestant "Fire Wall" "HQ")
   (play-from-hand state :contestant "Spiderweb" "HQ")
   (take-credits state :contestant)
   (core/gain state :challenger :credit 10)
   (play-from-hand state :challenger "Snowball")
   (let [sp (get-character state :hq 1)
         fw (get-character state :hq 0)
         snow (get-resource state 0)]
     (run-on state "HQ")
     (core/reveal state :contestant sp)
     (core/reveal state :contestant fw)
     (card-ability state :challenger snow 1) ; match strength
     (is (= 2 (:current-strength (refresh snow))))
     (card-ability state :challenger snow 0) ; strength matched, break a sub
     (card-ability state :challenger snow 0) ; break a sub
     (is (= 4 (:current-strength (refresh snow))) "Broke 2 subs, gained 2 more strength")
     (run-continue state)
     (is (= 3 (:current-strength (refresh snow))) "Has +2 strength until end of run; lost 1 per-encounter boost")
     (card-ability state :challenger snow 1)
     (card-ability state :challenger snow 1) ; match strength
     (is (= 5 (:current-strength (refresh snow))) "Matched strength, gained 2")
     (card-ability state :challenger snow 0) ; strength matched, break a sub
     (is (= 6 (:current-strength (refresh snow))) "Broke 1 sub, gained 1 more strength")
     (run-continue state)
     (is (= 4 (:current-strength (refresh snow))) "+3 until-end-of-run strength")
     (run-jack-out state)
     (is (= 1 (:current-strength (refresh snow))) "Back to default strength"))))

(deftest study-guide
  ;; Study Guide - 2c to add a power counter; +1 strength per counter
  (do-game
   (new-game (default-contestant)
             (default-challenger ["Study Guide" "Sure Gamble"]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Sure Gamble")
   (play-from-hand state :challenger "Study Guide")
   (let [sg (get-resource state 0)]
     (card-ability state :challenger sg 1)
     (is (= 4 (:credit (get-challenger))) "Paid 2c")
     (is (= 1 (get-counters (refresh sg) :power)) "Has 1 power counter")
     (is (= 1 (:current-strength (refresh sg))) "1 strength")
     (card-ability state :challenger sg 1)
     (is (= 2 (:credit (get-challenger))) "Paid 2c")
     (is (= 2 (get-counters (refresh sg) :power)) "Has 2 power counters")
     (is (= 2 (:current-strength (refresh sg))) "2 strength"))))

(deftest wyrm
  ;; Wyrm reduces strength of character
  (do-game
   (new-game (default-contestant ["Character Wall"])
             (default-challenger ["Wyrm"]))
   (play-from-hand state :contestant "Character Wall" "HQ")
   (take-credits state :contestant)
   (play-from-hand state :challenger "Wyrm")
   (run-on state "HQ")
   (let [character-wall (get-character state :hq 0)
         wyrm (get-resource state 0)]
     (core/reveal state :contestant character-wall)
     (card-ability state :challenger wyrm 1)
     (is (zero? (:current-strength (refresh character-wall))) "Strength of Character Wall reduced to 0")
     (card-ability state :challenger wyrm 1)
     (is (= -1 (:current-strength (refresh character-wall))) "Strength of Character Wall reduced to -1"))))

(deftest yusuf
  ;; Yusuf gains virus counters on successful runs and can spend virus counters from any placed card
  (do-game
    (new-game (default-contestant ["Fire Wall"])
              (default-challenger ["Yusuf" "Cache"]))
    (play-from-hand state :contestant "Fire Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Yusuf")
    (play-from-hand state :challenger "Cache")
    (let [fire-wall (get-character state :hq 0)
          yusuf (get-resource state 0)
          cache (get-resource state 1)]
      (run-empty-locale state "Archives")
      (is (= 1 (get-counters (refresh yusuf) :virus)) "Yusuf has 1 virus counter")
      (is (= 3 (:current-strength (refresh yusuf))) "Initial Yusuf strength")
      (is (= 3 (get-counters (refresh cache) :virus)) "Initial Cache virus counters")
      (run-on state "HQ")
      (core/reveal state :contestant fire-wall)
      (card-ability state :challenger yusuf 1) ; match strength
      (prompt-select :challenger cache)
      (prompt-select :challenger yusuf)
      (is (= 2 (get-counters (refresh cache) :virus)) "Cache lost 1 virus counter to pump")
      (is (= 5 (:current-strength (refresh yusuf))) "Yusuf strength 5")
      (is (= 0 (get-counters (refresh yusuf) :virus)) "Yusuf lost 1 virus counter to pump")
      (is (empty? (:prompt (get-challenger))) "No prompt open")
      (card-ability state :challenger yusuf 0)
      (prompt-select :challenger cache)
      (prompt-choice :challenger "Done")
      (is (= 1 (get-counters (refresh cache) :virus)) "Cache lost its final virus counter"))))
