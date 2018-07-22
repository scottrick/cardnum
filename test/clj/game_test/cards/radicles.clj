(ns game-test.cards.radicles
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "radicles"))

(deftest activist-support
  ;; Activist Support - Take tag if you have none; Contestant gains bad pub if they have none
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Activist Support"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Activist Support")
    (is (zero? (:tag (get-challenger))))
    (take-credits state :challenger)
    (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag; had none")
    (is (zero? (:bad-publicity (get-contestant))))
    (take-credits state :contestant)
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant took 1 bad pub; had none")
    (take-credits state :challenger)
    (is (= 1 (:tag (get-challenger))) "Challenger had 1 tag; didn't take another")
    (take-credits state :contestant)
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant had 1 bad pub; didn't take another")))

(deftest adjusted-chronotype
  ;; Ensure adjusted chronotype gains only 1 click when 2 clicks are lost
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Adjusted Chronotype" (qty "Beach Party" 2)]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Adjusted Chronotype")
      (play-from-hand state :challenger "Beach Party")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 4 (:click (get-challenger))) "Should have lost 1 click and gained 1 click")
      (play-from-hand state :challenger "Beach Party")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 3 (:click (get-challenger))) "Should have lost 2 clicks and gained 1 click")))
  (testing "Chronotype to cancel out MCA click loss"
    (do-game
      (new-game
        (default-contestant ["MCA Austerity Policy"])
        (default-challenger ["Adjusted Chronotype"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Adjusted Chronotype")
      (take-credits state :challenger)
      (play-from-hand state :contestant "MCA Austerity Policy" "New remote")
      (let [mca (get-content state :remote1 0)]
        (core/rez state :contestant mca)
        (card-ability state :contestant mca 0)
        (is (= 1 (get-counters (refresh mca) :power)))
        (take-credits state :contestant)
        ; challenger does not lose a click
        (is (= 4 (:click (get-challenger)))))))
  (testing "Ensure adjusted chronotype gains 2 clicks when 2 clicks are lost and GCS is installed"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Adjusted Chronotype"
                                 (qty "Beach Party" 3)
                                 "Gene Conditioning Shoppe"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Adjusted Chronotype")
      (play-from-hand state :challenger "Beach Party")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 4 (:click (get-challenger))) "Should have lost 1 click and gained 1 click")
      (play-from-hand state :challenger "Beach Party")
      (play-from-hand state :challenger "Gene Conditioning Shoppe")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 4 (:click (get-challenger))) "Should have lost 2 clicks and gained 2 clicks")
      (play-from-hand state :challenger "Beach Party")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 3 (:click (get-challenger))) "Should have lost 3 clicks and gained 2 clicks"))))

(deftest aesop's-pawnshop
  ;; Tests use cases for Aesop's Pawnshop
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Aesop's Pawnshop" "Cache"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Aesop's Pawnshop")
    (play-from-hand state :challenger "Cache")
    (let [orig-credits (:credit (get-challenger))
          ap (get-radicle state 0)
          cache (get-resource state 0)]
      (card-ability state :challenger ap 0)
      (prompt-select :challenger cache)
      (card-ability state :challenger ap 0)
      (prompt-select :challenger ap)
      (let [ap (get-radicle state 0)
            cache (get-in @state [:challenger :discard 0])]
        (is (= (+ 3 orig-credits) (:credit (get-challenger))) "Should have only gained 3 credits")
        (is (not= cache nil) "Cache should be in Heap")
        (is (not= ap nil) "Aesops should still be installed")))))

(deftest all-nighter
  ;; All-nighter - Click/discard to gain 2 clicks
  (do-game
    (new-game (default-contestant)
              (default-challenger ["All-nighter"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "All-nighter")
    (is (= 3 (:click (get-challenger))))
    (card-ability state :challenger (get-radicle state 0) 0)
    (is (= 4 (:click (get-challenger))) "Spent 1 click; gained 2 clicks")
    (is (= 1 (count (:discard (get-challenger)))) "All-nighter is discarded")))

(deftest bank-job
  ;; Bank Job
  (testing "Manhunt trace happens first"
    (do-game
      (new-game (default-contestant ["Manhunt" "PAD Campaign"])
                (default-challenger ["Bank Job"]))
      (play-from-hand state :contestant "Manhunt")
      (play-from-hand state :contestant "PAD Campaign" "New remote")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Bank Job")
      (run-empty-server state "Server 1")
      (prompt-choice :contestant 2) ; Manhunt trace active
      (prompt-choice :challenger 0)
      (prompt-choice :challenger "Replacement effect")
      (is (= "Bank Job" (:title (:card (first (get-in @state [:challenger :prompt])))))
          "Bank Job prompt active")
      (prompt-choice :challenger 8)
      (is (empty? (get-radicle state)) "Bank Job discarded after all credits taken")
      (is (= 1 (count (:discard (get-challenger)))))))
  (testing "Choose which to use when 2+ copies are installed"
    (do-game
      (new-game (default-contestant ["PAD Campaign"])
                (default-challenger [(qty "Bank Job" 2)]))
      (play-from-hand state :contestant "PAD Campaign" "New remote")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Bank Job")
      (run-empty-server state "Server 1")
      (prompt-choice :challenger "Replacement effect")
      (prompt-choice :challenger 4)
      (play-from-hand state :challenger "Bank Job")
      (let [bj1 (get-radicle state 0)
            bj2 (get-radicle state 1)]
        (is (= 4 (get-counters (refresh bj1) :credit)) "4 credits remaining on 1st copy")
        (run-empty-server state "Server 1")
        (prompt-choice :challenger "Replacement effect")
        (prompt-select :challenger bj2)
        (prompt-choice :challenger 6)
        (is (= 13 (:credit (get-challenger))))
        (is (= 2 (get-counters (refresh bj2) :credit)) "2 credits remaining on 2nd copy"))))
  (testing "Security Testing takes priority"
    (do-game
      (new-game (default-contestant ["PAD Campaign"])
                (default-challenger ["Bank Job" "Security Testing"]))
      (play-from-hand state :contestant "PAD Campaign" "New remote")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Security Testing")
      (play-from-hand state :challenger "Bank Job")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (prompt-choice :challenger "Server 1")
      (is (= 6 (:credit (get-challenger))))
      (run-empty-server state "Server 1")
      (is (empty? (:prompt (get-challenger))) "No Bank Job replacement choice")
      (is (= 8 (:credit (get-challenger))) "Security Testing paid 2c"))))

(deftest bazaar
  ;; Bazaar - Only triggers when installing from Grip
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Street Peddler"
                               "Bazaar"
                               (qty "Spy Camera" 6)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler" "Bazaar" "Spy Camera" "Spy Camera" "Spy Camera"])
    (play-from-hand state :challenger "Bazaar")
    (play-from-hand state :challenger "Street Peddler")
    (let [peddler (get-radicle state 1)]
      (card-ability state :challenger peddler 0)
      (prompt-card :challenger (first (:hosted peddler)))
      (is (empty? (:prompt (get-challenger))) "No Bazaar prompt from install off Peddler"))))

(deftest beach-party
  ;; Beach Party - Lose 1 click when turn begins; hand size increased by 5
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Beach Party"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Beach Party")
    (is (= 10 (core/hand-size state :challenger)) "Max hand size increased by 5")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (is (= 3 (:click (get-challenger))) "Lost 1 click at turn start")))

(deftest bhagat
  ;; Bhagat - only trigger on first run
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3) (qty "Architect" 3)])
              (default-challenger ["Bhagat"]))
    (starting-hand state :contestant [])
    (take-credits state :contestant)
    (run-empty-server state :hq)
    (play-from-hand state :challenger "Bhagat")
    (run-empty-server state :hq)
    (is (empty? (:discard (get-contestant))) "Bhagat did not trigger on second successful run")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (run-empty-server state :hq)
    (is (= 1 (count (:discard (get-contestant)))) "Bhagat milled one card")))

(deftest chrome-parlor
  ;; Chrome Parlor - Prevent all meat/brain dmg when installing cybernetics
  (do-game
    (new-game (default-contestant ["Traffic Accident"])
              (default-challenger ["Chrome Parlor" "Titanium Ribs"
                               "Brain Cage" (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Chrome Parlor")
    (play-from-hand state :challenger "Titanium Ribs")
    (is (empty? (:prompt (get-challenger))) "Damage prevented, no Ribs prompt to choose cards")
    (is (= 3 (count (:hand (get-challenger)))))
    (play-from-hand state :challenger "Brain Cage")
    (is (= 2 (count (:hand (get-challenger)))) "No cards lost")
    (is (zero? (:brain-damage (get-challenger))))
    (is (= 8 (core/hand-size state :challenger)) "Challenger hand size boosted by Brain Cage")
    (take-credits state :challenger)
    (core/gain state :challenger :tag 2)
    (core/discard state :challenger (get-hazard state 0))
    (play-from-hand state :contestant "Traffic Accident")
    (is (= 3 (count (:discard (get-challenger)))) "Conventional meat damage not prevented by Parlor")))

(deftest compromised-employee
  ;; Compromised Employee - Gain 1c every time Contestant rezzes Character
  (do-game
    (new-game (default-contestant [(qty "Pup" 2) "Launch Campaign"])
              (default-challenger ["Compromised Employee"]))
    (play-from-hand state :contestant "Pup" "HQ")
    (play-from-hand state :contestant "Pup" "R&D")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Compromised Employee")
    (let [ce (get-radicle state 0)]
      (is (= 1 (get-counters (refresh ce) :recurring)) "Has 1 recurring credit")
      (core/rez state :contestant (get-character state :hq 0))
      (is (= 4 (:credit (get-challenger))) "Gained 1c from Character rez")
      (core/rez state :contestant (get-character state :rd 0))
      (is (= 5 (:credit (get-challenger))) "Gained 1c from Character rez")
      (core/rez state :contestant (get-content state :remote1 0))
      (is (= 5 (:credit (get-challenger))) "Site rezzed, no credit gained"))))

(deftest councilman
  ;; Councilman reverses the rezz and prevents re-rezz
  (do-game
    (new-game (default-contestant ["Jackson Howard"])
              (default-challenger ["Councilman"]))
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Councilman")
    (let [jesus (get-content state :remote1 0)
          judas (get-radicle state 0)]
      (core/rez state :contestant jesus)
      ;; Challenger triggers Councilman
      (card-ability state :challenger judas 0)
      (prompt-select :challenger jesus)
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard no longer rezzed")
      (core/rez state :contestant (refresh jesus))
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard cannot be rezzed")
      (take-credits state :challenger)
      ;; Next turn
      (core/rez state :contestant (refresh jesus))
      (is (core/rezzed? (refresh jesus)) "Jackson Howard can be rezzed next turn"))))

(deftest-pending councilman-zone-change
  ;; Rezz no longer prevented when card changes zone (issues #1571)
  (do-game
    (new-game (default-contestant ["Jackson Howard"])
              (default-challenger ["Councilman"]))
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Councilman")
    (take-credits state :challenger)
    (let [jesus (get-content state :remote1 0)
          judas (get-radicle state 0)]
      (core/rez state :contestant jesus)
      ;; Challenger triggers Councilman
      (card-ability state :challenger judas 0)
      (prompt-select :challenger jesus)
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard no longer rezzed")
      (core/move state :contestant (refresh jesus) :hand))
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (let [jesus (get-content state :remote2 0)]
      (core/rez state :contestant jesus)
      (is (core/rezzed? (refresh jesus)) "Jackson Howard can be rezzed after changing zone"))))

(deftest counter-surveillance
  ;; Counter-Surveillance
  (testing "Discard to run, on successful run access cards equal to Tags and pay that amount in credits"
    (do-game
      (new-game (default-contestant [(qty "Hedge Fund" 3)])
                (default-challenger ["Counter Surveillance"]))
      (take-credits state :contestant)
      (core/gain state :challenger :tag 2)
      (play-from-hand state :challenger "Counter Surveillance")
      (is (= 4 (:credit (get-challenger))) "Challenger has 4 credits")
      (let [cs (get-radicle state 0)]
        (card-ability state :challenger cs 0)
        (prompt-choice :challenger "HQ")
        (run-successful state)
        (is (= [:hq] (get-in @state [:challenger :register :successful-run])))
        (prompt-choice :challenger "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-challenger) :prompt first :msg)))
        (prompt-choice :challenger "No action")
        (prompt-choice :challenger "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-challenger) :prompt first :msg)))
        (prompt-choice :challenger "No action")
        (is (= 1 (count (:discard (get-challenger)))) "Counter Surveillance discarded")
        (is (= 2 (:credit (get-challenger))) "Challenger has 2 credits"))))
  (testing "Test Obelus does not trigger before Counter Surveillance accesses are done. Issues #2675"
    (do-game
      (new-game (default-contestant [(qty "Hedge Fund" 3)])
                (default-challenger ["Counter Surveillance" "Obelus" (qty "Sure Gamble" 3)]))
      (starting-hand state :challenger ["Counter Surveillance" "Obelus"])
      (take-credits state :contestant)
      (core/gain state :challenger :tag 2)
      (core/gain state :challenger :credit 2)
      (is (= 7 (:credit (get-challenger))) "Challenger has 7 credits")
      (play-from-hand state :challenger "Counter Surveillance")
      (play-from-hand state :challenger "Obelus")
      (is (= 2 (:credit (get-challenger))) "Challenger has 2 credits") ; Challenger has enough credits to pay for CS
      (let [cs (get-radicle state 0)]
        (card-ability state :challenger cs 0)
        (prompt-choice :challenger "HQ")
        (run-successful state)
        (is (= [:hq] (get-in @state [:challenger :register :successful-run])))
        (is (zero? (count (:hand (get-challenger)))) "Challenger did not draw cards from Obelus yet")
        (prompt-choice :challenger "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-challenger) :prompt first :msg)))
        (is (zero? (count (:hand (get-challenger)))) "Challenger did not draw cards from Obelus yet")
        (prompt-choice :challenger "No action")
        (prompt-choice :challenger "Card from hand")
        (is (= "You accessed Hedge Fund." (-> (get-challenger) :prompt first :msg)))
        (prompt-choice :challenger "No action")
        (is (= 2 (count (:hand (get-challenger)))) "Challenger did draw cards from Obelus after all accesses are done")
        (is (= 1 (count (:discard (get-challenger)))) "Counter Surveillance discarded")
        (is (zero? (:credit (get-challenger))) "Challenger has no credits")))))

(deftest daily-casts
  ;; Play and tick through all turns of daily casts
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Daily Casts" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Daily Casts")
    (let [dc (get-radicle state 0)]
      ;; Number of credits
      (is (= 8 (get-counters dc :credit)))
      (is (= 2 (get-in @state [:challenger :credit])))
      ;; End turn
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 6 (get-counters (refresh dc) :credit)))
      (is (= 7 (get-in @state [:challenger :credit])))
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 4 (get-counters (refresh dc) :credit)))
      (is (= 13 (get-in @state [:challenger :credit])))
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 2 (get-counters (refresh dc) :credit)))
      (is (= 19 (get-in @state [:challenger :credit])))
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (nil? (get-radicle state 0))))))

(deftest data-folding
  ;; Data Folding - Gain 1c at start of turn if 2+ unused MU
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Data Folding" "Hyperdriver"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Data Folding")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (is (= 4 (core/available-mu state)) "At least 2 unused MU")
    (is (= 6 (:credit (get-challenger))) "Gained 1c at turn start")
    (play-from-hand state :challenger "Hyperdriver")
    (take-credits state :challenger)
    (is (= 1 (core/available-mu state)) "Only 1 unused MU")
    (is (= 8 (:credit (get-challenger))))
    (take-credits state :contestant)
    (is (= 8 (:credit (get-challenger))) "No credits gained at turn start")))

(deftest ddos
  ;; Prevent rezzing of outermost character for the rest of the turn
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3)])
              (default-challenger ["DDoS"]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "DDoS")
    (let [ddos (get-radicle state 0)
          iwall (get-character state :hq 1)]
      (card-ability state :challenger ddos 0)
      (is (= (:title ddos) (get-in @state [:challenger :discard 0 :title])))
      (run-on state "HQ")
      (core/rez state :contestant iwall)
      (is (not (:rezzed (refresh iwall))))
      (run-jack-out state)
      (run-on state "HQ")
      (core/rez state :contestant iwall)
      (is (not (:rezzed (refresh iwall))))
      (run-jack-out state)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-on state "HQ")
      (core/rez state :contestant iwall)
      (is (:rezzed (refresh iwall))))))

(deftest decoy
  ;; Decoy - Discard to avoid 1 tag
  (do-game
    (new-game (default-contestant ["SEA Source"])
              (default-challenger ["Decoy"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Decoy")
    (run-empty-server state :archives)
    (take-credits state :challenger)
    (play-from-hand state :contestant "SEA Source")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (= 1 (count (:prompt (get-challenger)))) "Challenger prompted to avoid tag")
    (card-ability state :challenger (get-radicle state 0) 0)
    (is (= 1 (count (:discard (get-challenger)))) "Decoy discarded")
    (is (zero? (:tag (get-challenger))) "Tag avoided")))

(let [choose-challenger
      (fn [name state prompt-map]
                      (let [the-choice (some #(when (= name (:title %)) %) (:choices (prompt-map :challenger)))]
                        (core/resolve-prompt state :challenger {:card the-choice})))
      ;; Start id
      sunny "Sunny Lebeau: Security Specialist"
      ;; List of all G-Mod identities
      geist "Armand \"Geist\" Walker: Tech Lord"
      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"
      maxx "MaxX: Maximum Punk Rock"]

  (deftest dj-fenris
    ;; DJ Fenris - host 1 g-mod id not in faction on DJ Fenris
    (testing "Hosting Chaos Theory"
      ;; Ensure +1 MU is handled correctly
      (do-game
        (new-game (default-contestant)
                  ;; Challenger id is Gabe, make sure Geist is not in list (would be first)
                  (make-deck sunny ["DJ Fenris"]) {:start-as :challenger})
        (play-from-hand state :challenger "DJ Fenris")
        (is (= (first (prompt-titles :challenger)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :challenger))
                    [geist chaos reina maxx]))
        (is (not-any? #(some #{%} (prompt-titles :challenger))
                      [professor whizzard jamie kate kit]))
        (choose-challenger chaos state prompt-map)
        (is (= chaos (get-in (get-radicle state 0) [:hosted 0 :title])) "Chaos Theory hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-challenger)))) "Still Sunny, id not changed")
        (is (= 2 (:link (get-challenger))) "2 link from Sunny")
        (is (= 5 (core/available-mu state)) "+1 MU from Chaos Theory")
        ;; Discard DJ Fenris
        (discard-radicle state "DJ Fenris")
        (is (= chaos (get-in (get-challenger) [:rfg 0 :title])) "Chaos Theory moved to RFG")
        (is (= 1 (count (:discard (get-challenger)))) "1 card in heap: DJ Fenris")
        (is (= 4 (core/available-mu state)) "+1 MU from Chaos Theory removed")
        ;; Recover DJ Fenris
        (core/move state :challenger (get-in (get-challenger) [:discard 0]) :hand)
        (core/gain state :challenger :credit 3)
        ;; Re-play DJ Fenris
        (play-from-hand state :challenger "DJ Fenris")
        (choose-challenger chaos state prompt-map)
        ;; Try moving CT to hand
        (game.core/move state :challenger (get-in (get-radicle state 0) [:hosted 0]) :hand)
        (is (= chaos (get-in (get-challenger) [:rfg 0 :title])) "Chaos Theory moved to RFG")
        (is (zero? (count (:hand (get-challenger)))) "Chaos Theory _not_ moved to hand")
        (is (= 4 (core/available-mu state)) "+1 MU from Chaos Theory removed")))
    (testing "Hosting Geist"
      ;; Ensure Geist effect triggers
      (do-game
        (new-game (default-contestant)
                  ;; Challenger id is Gabe, make sure Geist is not in list (would be first)
                  (make-deck sunny ["DJ Fenris" (qty "All-nighter" 3) (qty "Sure Gamble" 3)]) {:start-as :challenger})
        (starting-hand state :challenger ["DJ Fenris" "All-nighter" "All-nighter"])
        (play-from-hand state :challenger "All-nighter")
        (play-from-hand state :challenger "All-nighter")
        (play-from-hand state :challenger "DJ Fenris")
        (is (= (first (prompt-titles :challenger)) geist) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :challenger))
                    [geist chaos reina maxx]))
        (is (not-any? #(some #{%} (prompt-titles :challenger))
                      [professor whizzard jamie kate kit]))
        (choose-challenger geist state prompt-map)
        (is (= geist (get-in (get-radicle state 2) [:hosted 0 :title])) "Geist hosted on DJ Fenris")
        (is (= sunny (:title (:identity (get-challenger)))) "Still Sunny, id not changed")
        (is (= 2 (:link (get-challenger))) "2 link from Sunny, no extra link from Geist")
        (let [hand-count (count (:hand (get-challenger)))]
          (card-ability state :challenger (get-radicle state 0) 0) ; Use All-nighter
          (is (= (+ 1 hand-count) (count (:hand (get-challenger))))
              "Drew one card with Geist when using All-nighter discard ability")
          (discard-radicle state "DJ Fenris")
          (is (= geist (get-in (get-challenger) [:rfg 0 :title])) "Geist moved to RFG")
          (is (= 2 (count (:discard (get-challenger)))) "2 cards in heap: All-nighter and DJ Fenris")
          (card-ability state :challenger (get-radicle state 0) 0) ; Use All-nighter (again)
          (is (= (+ 1 hand-count) (count (:hand (get-challenger))))
              "Did not draw another card - Geist ability removed when DJ Fenris was discarded"))))))

(deftest donut-taganes
  ;; Donut Taganes - add 1 to play cost of Operations & Events when this is in play
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Donut Taganes" "Easy Mark"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Donut Taganes")
    (is (= 2 (:credit (get-challenger))) "Donut played for 3c")
    (play-from-hand state :challenger "Easy Mark")
    (is (= 4 (:credit (get-challenger))) "Easy Mark only gained 2c")
    (take-credits state :challenger)
    (is (= 8 (:credit (get-contestant))) "Contestant has 8c")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 11 (:credit (get-contestant))) "Contestant has 11c")))

(deftest dummy-box
  ;; Dummy Box - discard a card from hand to prevent contestant discarding installed card
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Dummy Box" 1) (qty "Cache" 1) (qty "Clot" 1)]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Dummy Box")
      (play-from-hand state :challenger "Cache")
      (take-credits state :challenger)
      (core/discard state :challenger (get-resource state 0))
      (is (not-empty (:prompt (get-challenger))) "Dummy Box prompting to prevent resource discard")
      (card-ability state :challenger (get-radicle state 0) 2)
      (prompt-select :challenger (find-card "Clot" (:hand (get-challenger))))
      (prompt-choice :challenger "Done")
      (is (= 1 (count (:discard (get-challenger)))) "Clot discarded")
      (is (empty? (:hand (get-challenger))) "Card discarded from hand")
      (is (= 1 (count (get-resource state))) "Cache still installed")
      (is (= 1 (count (get-radicle state))) "Dummy Box still installed")))
  (testing "doesn't prevent resource deletion during purge"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Dummy Box" 1) (qty "Cache" 1) (qty "Clot" 1)]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Dummy Box")
      (play-from-hand state :challenger "Clot")
      (take-credits state :challenger)
      (core/purge state :contestant)
      (is (empty? (:prompt (get-challenger))) "Dummy Box not prompting to prevent purge discard"))))

(deftest eden-shard
  ;; Eden Shard - Install from Grip in lieu of accessing R&D; discard to make Contestant draw 2
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Eden Shard"]))
      (starting-hand state :contestant ["Hedge Fund"])
      (take-credits state :contestant)
      (is (= 1 (count (:hand (get-contestant)))))
      (run-on state :rd)
      (core/no-action state :contestant nil)
      (play-from-hand state :challenger "Eden Shard")
      (is (= 5 (:credit (get-challenger))) "Eden Shard installed for 0c")
      (is (not (:run @state)) "Run is over")
      (card-ability state :challenger (get-radicle state 0) 0)
      (is (= 3 (count (:hand (get-contestant)))) "Contestant drew 2 cards")
      (is (= 1 (count (:discard (get-challenger)))) "Eden Shard discarded")))
  (testing "Do not install when accessing cards"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Eden Shard"]))
      (starting-hand state :contestant ["Hedge Fund"])
      (take-credits state :contestant)
      (is (= 1 (count (:hand (get-contestant)))))
      (run-empty-server state :rd)
      (play-from-hand state :challenger "Eden Shard")
      (is (not (get-radicle state 0)) "Eden Shard not installed")
      (is (= 1 (count (:hand (get-challenger)))) "Eden Shard not installed"))))

(deftest fan-site
  ;; Fan Site - Add to score area as 0 points when Contestant scores an agenda
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Hostile Takeover"])
                (default-challenger ["Fan Site"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Fan Site")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (score-agenda state :contestant (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-challenger))))
      (is (= 1 (count (:scored (get-challenger)))) "Fan Site added to Challenger score area")))
  (testing "Don't trigger after swap with Exchange of Information. Issue #1824"
    (do-game
      (new-game (default-contestant [(qty "Hostile Takeover" 2) "Exchange of Information"])
                (default-challenger ["Fan Site"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Fan Site")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (score-agenda state :contestant (get-content state :remote1 0))
      (core/tag-challenger state :challenger 1)
      (play-from-hand state :contestant "Exchange of Information")
      (prompt-select :contestant (find-card "Fan Site" (:scored (get-challenger))))
      (prompt-select :contestant (find-card "Hostile Takeover" (:scored (get-contestant))))
      (is (= 1 (:agenda-point (get-challenger))))
      (is (zero? (:agenda-point (get-contestant))))
      (is (find-card "Fan Site" (:scored (get-contestant))) "Fan Site swapped into Contestant score area")
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (score-agenda state :contestant (get-content state :remote2 0))
      (is (find-card "Fan Site" (:scored (get-contestant))) "Fan Site not removed from Contestant score area")))
  (testing "Challenger can forfeit Fan Site"
    (do-game
      (new-game (default-contestant ["Hostile Takeover"])
                (default-challenger ["Fan Site" "Data Dealer"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Fan Site")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (score-agenda state :contestant (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-challenger))))
      (is (= 1 (count (:scored (get-challenger)))) "Fan Site added to Challenger score area")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Data Dealer")
      (let [credits (:credit (get-challenger))]
        (card-ability state :challenger (get-radicle state 0) 0)
        (prompt-select :challenger (get-scored state :challenger 0))
        (is (zero? (count (:scored (get-challenger)))) "Fan Site successfully forfeit to Data Dealer")
        (is (= (+ credits 9) (:credit (get-challenger))) "Gained 9 credits from Data Dealer")))))

(deftest fester
  ;; Fester - Contestant loses 2c (if able) when purging viruses
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Fester"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Fester")
    (take-credits state :challenger)
    (core/lose state :contestant :credit 5)
    (core/gain state :contestant :click 3)
    (is (= 3 (:credit (get-contestant))))
    (core/purge state :contestant)
    (is (= 1 (:credit (get-contestant))) "Lost 2c when purging")
    (core/purge state :contestant)
    (is (= 1 (:credit (get-contestant))) "Lost no credits when purging, only had 1c")))

(deftest film-critic
  ;; Film Critic
  (testing "Prevent Contestant-discarded execs going to Challenger scored. Issues #1181/#1042"
    (do-game
      (new-game (default-contestant [(qty "Director Haas" 3) (qty "Project Vitruvius" 3) "Hedge Fund"])
                (default-challenger ["Film Critic"]))
      (play-from-hand state :contestant "Project Vitruvius" "New remote")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Film Critic")
      (let [fc (first (get-radicle state))]
        (run-empty-server state "Server 1")
        (prompt-choice :challenger "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :challenger)
        (discard-from-hand state :contestant "Director Haas")
        (is (= 1 (count (:discard (get-contestant)))) "Director Haas stayed in Archives")
        (is (zero? (:agenda-point (get-challenger))) "No points gained by Challenger")
        (is (empty? (:scored (get-challenger))) "Nothing in Challenger scored"))))
  (testing "Fetal AI interaction"
    (do-game
      (new-game (default-contestant [(qty "Fetal AI" 3)])
                (default-challenger ["Film Critic" (qty "Sure Gamble" 3)]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Film Critic")
      (let [fc (first (get-radicle state))]
        (run-empty-server state "HQ")
        ;; should not have taken damage yet
        (is (= 3 (count (:hand (get-challenger)))) "No damage dealt yet")
        (prompt-choice :challenger "Yes")
        (is (= 3 (count (:hand (get-challenger)))) "No damage dealt")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (card-ability state :challenger fc 0)
        (is (= 1 (count (:scored (get-challenger)))) "Agenda added to challenger scored")
        (is (= 3 (count (:hand (get-challenger)))) "No damage dealt"))))
  (testing "Do not take a net damage when a hosted agenda is discarded due to film critic discard #2382"
    (do-game
      (new-game (default-contestant [(qty "Hostile Infrastructure" 3) "Project Vitruvius"])
                (default-challenger ["Film Critic" (qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
      (play-from-hand state :contestant "Project Vitruvius" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Film Critic")
      (let [fc (first (get-radicle state))]
        (run-empty-server state :remote2)
        (prompt-choice :challenger "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :challenger)
        (core/gain state :contestant :credit 10)
        (core/discard-radicle state :contestant nil)
        (prompt-select :contestant fc)
        (is (= 1 (count (:discard (get-challenger)))) "FC discarded")
        (is (= 1 (count (:discard (get-contestant)))) "Agenda discarded")
        (is (= 3 (count (:hand (get-challenger)))) "No damage dealt"))))
  (testing "required hosted cards to be an agenda before firing ability"
    (do-game
      (new-game (default-contestant ["MCA Informant"])
                (default-challenger ["Film Critic"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Film Critic")
      (let [fc (first (get-radicle state))]
        (take-credits state :challenger)
        (play-from-hand state :contestant "MCA Informant")
        (prompt-select :contestant fc)
        (is (= 1 (count (:hosted (refresh fc)))) "MCA Informant hosted on FC")
        (take-credits state :contestant)
        (card-ability state :challenger fc 0)
        (is (= 1 (count (:hosted (refresh fc)))) "MCA Informant still hosted on FC")))))

(deftest find-the-truth
  ;; Find the Truth
  (testing "Basic test - On successful run see the top card from R&D before access"
    (do-game
      (new-game
        (default-contestant [(qty "Restructure" 10)])
        (default-challenger ["Find the Truth"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Find the Truth")
      (run-on state "HQ")
      (run-successful state)
      (is (= "Use Find the Truth to look at the top card of R&D?" (-> @state :challenger :prompt first :msg)) "FTT prompt")
      (prompt-choice :challenger "Yes")
      (is (= "The top card of R&D is Restructure" (-> @state :challenger :prompt first :msg)) "FTT shows card on R&D")
      (prompt-choice :challenger "Yes")))
  (testing "Equivocation & FTT - should get order of choice"
    (do-game
      (new-game
        (default-contestant [(qty "Restructure" 10)])
        (default-challenger ["Equivocation" "Find the Truth"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Equivocation")
      (play-from-hand state :challenger "Find the Truth")
      (run-empty-server state :rd)
      (prompt-choice :challenger "Find the Truth")
      (is (= "Use Find the Truth to look at the top card of R&D?" (-> @state :challenger :prompt first :msg)) "FTT prompt")
      (prompt-choice :challenger "Yes")
      (is (= "The top card of R&D is Restructure" (-> @state :challenger :prompt first :msg)) "FTT shows card")
      (prompt-choice :challenger "Yes") ; Equivocation prompt
      (is (= "Reveal the top card of R&D?" (-> @state :challenger :prompt first :msg)) "Equivocation Prompt")
      (prompt-choice :challenger "Yes")))
  (testing "Find The Truth should completed before Marilyn discard is forced"
    (do-game
      (new-game
        (default-contestant ["Marilyn Campaign" (qty "Vanilla" 10)])
        (default-challenger ["Find the Truth" "Neutralize All Threats"]))
      (starting-hand state :contestant ["Marilyn Campaign"])
      (play-from-hand state :contestant "Marilyn Campaign" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (is (= 3 (:credit (get-contestant))))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Find the Truth")
      (play-from-hand state :challenger "Neutralize All Threats")
      (run-on state :remote1)
      (run-successful state)
      (is (= "Use Find the Truth to look at the top card of R&D?" (-> @state :challenger :prompt first :msg)) "FTT prompt")
      (is (= "Waiting for Challenger to resolve successful-run triggers" (-> @state :contestant :prompt first :msg)) "No Marilyn Shuffle Prompt")
      (prompt-choice :challenger "Yes")
      (is (= "The top card of R&D is Vanilla" (-> @state :challenger :prompt first :msg)) "FTT shows card")
      (is (= "Waiting for Challenger to resolve successful-run triggers" (-> @state :contestant :prompt first :msg)) "No Marilyn Shuffle Prompt")
      (prompt-choice :challenger "No action")
      (prompt-choice-partial :challenger "Pay")
      (is (= "Waiting for Contestant to use Marilyn Campaign" (-> @state :challenger :prompt first :msg)) "Now Contestant gets shuffle choice")
      (is (= "Shuffle Marilyn Campaign into R&D?" (-> @state :contestant :prompt first :msg)) "Now Contestant gets shuffle choice")
      (is (= 2 (:credit (get-challenger)))) #_ discarded_marilyn)))

(deftest gang-sign
  ;; Gang Sign
  (testing "accessing from HQ, not including root. Issue #2113"
    (do-game
      (new-game (default-contestant [(qty "Hostile Takeover" 3) (qty "Braintrust" 2) "Crisium Grid"])
                (default-challenger [(qty "Gang Sign" 2) "HQ Interface"]))
      (play-from-hand state :contestant "Crisium Grid" "HQ")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 100)
      (play-from-hand state :challenger "Gang Sign")
      (play-from-hand state :challenger "Gang Sign")
      (play-from-hand state :challenger "HQ Interface")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (score-agenda state :contestant (get-content state :remote1 0))
      (prompt-choice :challenger "Gang Sign") ; simultaneous effect resolution
      (let [gs1 (-> (get-challenger) :prompt first)]
        (is (= (:choices gs1) ["Card from hand"]) "Gang Sign does not let Challenger access region in HQ root")
        (prompt-choice :challenger "Card from hand")
        (prompt-choice :challenger "Steal")
        (is (= (:card gs1) (-> (get-challenger) :prompt first :card)) "Second access from first Gang Sign triggered")
        (prompt-choice :challenger "Card from hand")
        (prompt-choice :challenger "Steal")
        (is (not= (:card gs1) (-> (get-challenger) :prompt first :card)) "First access from second Gang Sign triggered")
        (prompt-choice :challenger "Card from hand")
        (prompt-choice :challenger "Steal")
        (prompt-choice :challenger "Card from hand")
        (prompt-choice :challenger "Steal"))))
  (testing "accessing from HQ, not including root. Issue #2113"
    (do-game
      (new-game (default-contestant ["Hostile Takeover" "Snare!"])
                (default-challenger ["Gang Sign"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Gang Sign")
      (take-credits state :challenger)
      (play-and-score state "Hostile Takeover")
      (prompt-choice :challenger "Card from hand")
      ;; Challenger has "wait for Snare, wait for on-access" prompts.
      (is (= 2 (count (:prompt (get-challenger)))) "Challenger only has the Waiting prompt, not Snare!'s pay-prompt")
      ;; Core has "pay for Snare, wait for agenda-scored" prompts.
      (is (= 2 (count (:prompt (get-contestant)))) "Contestant has the prompt to use Snare!"))))

(deftest gene-conditioning-shoppe
  ;; Gene Conditioning Shoppe - set :genetics-trigger-twcharacter flag
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Hedge Fund" 3)])
                (default-challenger ["Gene Conditioning Shoppe"
                                 "Adjusted Chronotype"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Adjusted Chronotype")
      (is (not (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter)))
      (play-from-hand state :challenger "Gene Conditioning Shoppe")
      (is (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter))
      (core/discard state :challenger (get-radicle state 1))
      (is (not (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter)))))
  (testing "set :genetics-trigger-twcharacter flag - ensure redundant copies work"
    (do-game
      (new-game (default-contestant [(qty "Hedge Fund" 3)])
                (default-challenger [(qty "Gene Conditioning Shoppe" 2)
                                 "Adjusted Chronotype"]))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Adjusted Chronotype")
      (let [adjusted-chronotype (get-radicle state 0)]
        (is (not (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter)))
        (play-from-hand state :challenger "Gene Conditioning Shoppe")
        (play-from-hand state :challenger "Gene Conditioning Shoppe")
        (let [gcs1 (get-radicle state 1)
              gcs2 (get-radicle state 2)]
          (is (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter))
          (core/discard state :challenger gcs1)
          (is (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter))
          (core/discard state :challenger gcs2)
          (is (not (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter))))))))

(deftest globalsec-security-clearance
  ;; Globalsec Security Clearance - Ability, click lost on use
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Globalsec Security Clearance"]))
    (take-credits state :contestant)
    (core/gain state :challenger :link 2)
    (play-from-hand state :challenger "Globalsec Security Clearance")
    (take-credits state :challenger)
    (starting-hand state :contestant ["Hedge Fund"]) ; Hedge Fund on top
    (take-credits state :contestant)
    (is (:challenger-phase-12 @state) "Challenger in Step 1.2")
    (let [gsec (get-radicle state 0)]
      (card-ability state :challenger gsec 0)
      (is (pos? (.indexOf (-> (get-challenger) :prompt first :msg) "Hedge Fund")) "GSec revealed Hedge Fund")
      (core/end-phase-12 state :challenger nil)
      (is (= 3 (:click (get-challenger))) "Challenger lost 1 click from Globalsec Security Clearance"))))

(deftest grifter
  ;; Grifter - Gain 1c if you made a successful run this turn, otherwise discard it
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Grifter"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Grifter")
    (run-empty-server state :hq)
    (take-credits state :challenger)
    (is (= 6 (:credit (get-challenger))) "Gained 1c for a successful run during the turn")
    (take-credits state :contestant)
    (run-on state :hq)
    (run-jack-out state)
    (take-credits state :challenger)
    (is (= 1 (count (:discard (get-challenger)))) "No successful runs; Grifter is discarded")))

(deftest guru-davinder
  ;; Guru Davinder - no prompt/discard for 'preventing' 0 damage
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Punitive Counterstrike"])
                (default-challenger ["Guru Davinder"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Guru Davinder")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Punitive Counterstrike")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (empty? (get-in @state [:challenger :prompt]))
          "There is no prompt for 0 damage")))
  (testing "cannot steal Obokata while installed"
    (do-game
      (new-game (make-deck "Cardnum: Personal Evolution" [(qty "Obokata Protocol" 10)])
                (default-challenger ["Guru Davinder" (qty "Sure Gamble" 4)]))
      (play-from-hand state :contestant "Obokata Protocol" "New remote")
      (take-credits state :contestant)
      (core/gain state :challenger :agenda-point 6)
      (play-from-hand state :challenger "Guru Davinder")
      (run-empty-server state "Server 1")
      (prompt-choice :challenger "No action")
      (is (zero? (count (:discard (get-challenger)))) "Challenger did not pay damage")
      (is (not= :challenger (:winner @state)) "Challenger has not won"))))

(deftest hard-at-work
  ;; Hard at Work - Gain 2c and lose 1 click when turn begins
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Hard at Work"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Hard at Work")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (is (= 5 (:credit (get-challenger))) "Gained 2c")
    (is (= 3 (:click (get-challenger))) "Lost 1 click")))

(deftest character-carver
  ;; Ice Carver - lower character strength on encounter
  (do-game
    (new-game (default-contestant ["Ice Wall"])
              (default-challenger ["Ice Carver"]))
    (play-from-hand state :contestant "Ice Wall" "Archives")
    (take-credits state :contestant 2)
    (let [iwall (get-character state :archives 0)]
      (core/rez state :contestant iwall)
      (play-from-hand state :challenger "Ice Carver")
      (run-on state "Archives")
      (is (zero? (:current-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh iwall))) "Ice Wall strength at 1 after encounter"))))

(deftest investigative-journalism
  ;; Investigative Journalism - 4 clicks and discard to give the Contestant 1 bad pub
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Investigative Journalism"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Investigative Journalism")
    (is (empty? (get-radicle state)) "Contestant has no bad pub, couldn't install")
    (core/gain state :contestant :bad-publicity 1)
    (play-from-hand state :challenger "Investigative Journalism")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (card-ability state :challenger (get-radicle state 0) 0)
    (is (zero? (:click (get-challenger))) "Spent 4 clicks")
    (is (= 1 (count (:discard (get-challenger)))) "IJ is discarded")
    (is (= 2 (:bad-publicity (get-contestant))) "Contestant took 1 bad publicity")))

(deftest jackpot!
  ;; Jackpot! - whenever a card enters your score area, discard Jackpot to pull off credits
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Braintrust"])
                (default-challenger ["Jackpot!"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Jackpot!")
      (let [jak (get-radicle state 0)]
        (is (zero? (get-counters (refresh jak) :credit)) "Jackpot! starts with 0 credits")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 2 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn (2nd turn)")
        (run-empty-server state "HQ")
        (prompt-choice :challenger "Steal")
        (is (= 2 (:agenda-point (get-challenger))) "Challenger steals Braintrust")
        (prompt-choice :challenger "Yes")
        (is (= 12 (:credit (get-challenger))) "Challenger starts with 12 credits")
        (prompt-choice :challenger 2)
        (is (= 14 (:credit (get-challenger))) "Challenger gains 2 credits")
        (is (= 1 (count (:discard (get-challenger)))) "Jackpot! discarded"))))
  (testing "should fire when moving agendas from Film Critic to scored area"
    (do-game
      (new-game (default-contestant ["Project Vitruvius"])
                (default-challenger ["Jackpot!" "Film Critic"]))
      (play-from-hand state :contestant "Project Vitruvius" "New remote")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Film Critic")
      (play-from-hand state :challenger "Jackpot!")
      (let [fc (get-radicle state 0)
            jak (get-radicle state 1)]
        (run-empty-server state "Server 1")
        (prompt-choice :challenger "Yes")
        (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (card-ability state :challenger fc 0)
        (prompt-choice :challenger "Yes")
        (prompt-choice :challenger 1)
        (is (= 1 (count (:scored (get-challenger)))) "Moved agenda to scored area")
        (is (= 1 (count (:discard (get-challenger)))) "Jackpot! discarded")
        (is (empty? (:hosted (refresh fc))) "Removed agenda hosted on FC"))))
  (testing "should fire when discarding Chairman Hiro"
    (do-game
      (new-game (default-contestant ["Chairman Hiro"])
                (default-challenger ["Jackpot!"]))
      (play-from-hand state :contestant "Chairman Hiro" "New remote")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Jackpot!")
      (let [jak (get-radicle state 0)]
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (get-counters (refresh jak) :credit)) "Jackpot! gains 1 credit per turn")
        (run-empty-server state "Server 1")
        (prompt-choice-partial :challenger "Pay") ;discard CH
        (prompt-choice :challenger "Yes") ;discard Jackpot!
        (prompt-choice :challenger 1)
        (is (= 3 (:credit (get-challenger))) "Challenger gains 1 credit")
        (is (= 1 (count (:scored (get-challenger)))) "Chairman Hiro in score area")
        (is (= 1 (count (:discard (get-challenger)))) "Jackpot! discarded")))))

(deftest jak-sinclair
  ;; Jak Sinclair
  (testing "Lost clicks carry through to when turn starts fully #1764"
    (do-game
      (new-game (default-contestant [(qty "Enigma" 3)])
                (default-challenger [(qty "Jak Sinclair" 3)]))
      (play-from-hand state :contestant "Enigma" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Jak Sinclair")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (let [eni (get-character state :hq 0)
            jak (get-radicle state 0)]
        (core/rez state :contestant eni)
        (is (:challenger-phase-12 @state) "Challenger in Step 1.2")
        (card-ability state :challenger jak 0)
        (prompt-choice :challenger "HQ")
        (card-subroutine state :contestant (refresh eni) 0)
        (run-successful state)
        (core/end-phase-12 state :challenger nil)
        (is (= 3 (:click (get-challenger))) "Enigma took a click")))))

(deftest john-masanori
  ;; John Masanori - Draw 1 card on first successful run, take 1 tag on first unsuccessful run
  (do-game
    (new-game (default-contestant ["Crisium Grid"])
              (default-challenger [(qty "John Masanori" 3)
                               (qty "Sure Gamble" 3)
                               "Fall Guy"]))
    (play-from-hand state :contestant "Crisium Grid" "HQ")
    (core/rez state :contestant (get-content state :hq 0))
    (take-credits state :contestant)
    (core/gain state :challenger :click 2 :credit 2)
    (play-from-hand state :challenger "John Masanori")
    (is (= 4 (count (:hand (get-challenger)))))
    (run-empty-server state "HQ")
    (prompt-choice-partial :challenger "Pay") ; discard crisium #2433
    (run-empty-server state "Archives")
    (is (= 5 (count (:hand (get-challenger)))) "1 card drawn from first successful run")
    (run-empty-server state "Archives")
    (is (= 5 (count (:hand (get-challenger)))) "No card drawn from second successful run")
    (run-on state "HQ")
    (run-jack-out state)
    (is (= 1 (:tag (get-challenger))) "1 tag taken from first unsuccessful run")
    (run-on state "HQ")
    (run-jack-out state)
    (is (= 1 (:tag (get-challenger))) "No tag taken from second unsuccessful run")))

(deftest joshua-b.
  ;; Joshua B. - Take 1 tag at turn end if you choose to gain the extra click
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Joshua B."]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Joshua B.")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (is (zero? (:click (get-challenger))) "Challenger has 0 clicks")
    (is (:challenger-phase-12 @state) "Challenger is in Step 1.2")
    (card-ability state :challenger (get-radicle state 0) 0)
    (is (= 1 (:click (get-challenger))) "Gained extra click from Joshua")
    (core/end-phase-12 state :challenger nil)
    (is (= 5 (:click (get-challenger))) "Gained normal clicks as well")
    (take-credits state :challenger)
    (is (= 1 (:tag (get-challenger))) "Took 1 tag")))

(deftest kati-jones
  ;; Kati Jones - Click to store and take
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Kati Jones"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Kati Jones")
    (is (= 3 (:credit (get-challenger))))
    (let [kati (get-radicle state 0)]
      (card-ability state :challenger kati 0)
      (is (= 2 (:click (get-challenger))))
      (is (= 3 (get-counters (refresh kati) :credit)) "Store 3cr on Kati")
      (card-ability state :challenger kati 0)
      (is (= 2 (:click (get-challenger))) "Second use of Kati should not be allowed")
      (is (= 3 (get-counters (refresh kati) :credit)) "Second use of Kati should not be allowed")
      (take-credits state :challenger 2)
      (is (= 5 (:credit (get-challenger))) "Pass turn, take 2cr")
      (take-credits state :contestant)
      (card-ability state :challenger kati 0)
      (is (= 6 (get-counters (refresh kati) :credit)) "Store 3cr more on Kati")
      (take-credits state :challenger 3)
      (is (= 8 (:credit (get-challenger))) "Pass turn, take 3cr")
      (take-credits state :contestant)
      (card-ability state :challenger (refresh kati) 1)
      (is (= 14 (:credit (get-challenger))) "Take 6cr from Kati")
      (is (zero? (get-counters (refresh kati) :credit)) "No counters left on Kati"))))

(deftest lewi-guilherme
  ;; Lewi Guilherme - lower contestant hand size by 1, pay 1 credit when turn begins or discard
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Lewi Guilherme" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Lewi Guilherme")
    (is (= -1 (get-in (get-contestant) [:hand-size :mod])) "Contestant hand size reduced by 1")
    (take-credits state :challenger)
    (core/lose state :challenger :credit 6)
    (is (= 2 (:credit (get-challenger))) "Credits are 2")
    (take-credits state :contestant)
    (prompt-choice :challenger "Yes")
    (is (= 1 (:credit (get-challenger))) "Lost a credit from Lewi")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (prompt-choice :challenger "No")
    (is (= 1 (count (:discard (get-challenger)))) "First Lewi discarded")
    (is (zero? (get-in (get-contestant) [:hand-size :mod])) "Contestant hand size normal again")
    (play-from-hand state :challenger "Lewi Guilherme")
    (take-credits state :challenger)
    (core/lose state :challenger :credit 8)
    (is (zero? (:credit (get-challenger))) "Credits are 0")
    (take-credits state :contestant)
    (prompt-choice :challenger "Yes")
    (is (= 2 (count (:discard (get-challenger)))) "Second Lewi discarded due to no credits")))

(deftest logic-bomb
  ;; Logic Bomb
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Ice Wall" 2)])
                (default-challenger ["Logic Bomb"]))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/rez state :contestant (get-character state :hq 0))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Logic Bomb")
      (run-on state :hq)
      (is (= 2 (:click (get-challenger))) "Should still have 2 clicks")
      (card-ability state :challenger (get-radicle state 0) 0)
      (is (zero? (:click (get-challenger))) "Should now have 0 clicks")
      (is (= 1 (count (:discard (get-challenger)))) "Logic Bomb should be discarded")
      (is (last-log-contains? state "uses Logic Bomb"))
      (is (last-log-contains? state "\\[Click\\]\\[Click\\]") "Log should mention 2 clicks")))
  (testing "if the challenger has no clicks left"
    (do-game
      (new-game (default-contestant [(qty "Ice Wall" 2)])
                (default-challenger ["Logic Bomb"]))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/rez state :contestant (get-character state :hq 0))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Logic Bomb")
      (core/click-credit state :challenger nil)
      (core/click-credit state :challenger nil)
      (run-on state :hq)
      (is (zero? (:click (get-challenger))) "Should have 0 clicks")
      (card-ability state :challenger (get-radicle state 0) 0)
      (is (zero? (:click (get-challenger))) "Should still have 0 clicks")
      (is (= 1 (count (:discard (get-challenger)))) "Logic Bomb should be discarded")
      (is (last-log-contains? state "uses Logic Bomb"))
      (is (not (last-log-contains? state "\\[Click\\]")) "Log shouldn't mention any clicks"))))

(deftest london-library
  ;; Install non-virus resources on London library. Includes #325/409
  (do-game
    (new-game (default-contestant)
              (default-challenger ["London Library" "Darwin" "Study Guide"
                               "Chameleon" "Femme Fatale"]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 2)
    (play-from-hand state :challenger "London Library")
    (let [lib (get-radicle state 0)]
      (is (zero? (count (:hosted (refresh lib)))) "0 resources hosted")
      (card-ability state :challenger lib 0) ; Install a non-virus resource on London Library
      (prompt-select :challenger (find-card "Femme Fatale" (:hand (get-challenger))))
      (prompt-choice :challenger "Done") ; Cancel out of Femme's bypass
      (is (= 1 (count (:hosted (refresh lib)))) "1 resource hosted")
      (card-ability state :challenger lib 0)
      (prompt-select :challenger (find-card "Study Guide" (:hand (get-challenger))))
      (is (= 2 (count (:hosted (refresh lib)))) "2 resources hosted")
      (let [sg (second (:hosted (refresh lib)))]
        (is (zero? (:current-strength (refresh sg))) "Study Guide at 0 strength")
        (card-ability state :challenger sg 1) ; Place 1 power counter
        (is (= 1 (:current-strength (refresh sg))) "Study Guide at 1 strength"))
      (card-ability state :challenger lib 0)
      (prompt-select :challenger (find-card "Chameleon" (:hand (get-challenger))))
      (prompt-choice :challenger "Sentry")
      (is (= 3 (count (:hosted (refresh lib)))) "3 resources hosted")
      (is (= 2 (:click (get-challenger))) "At 2 clicks")
      (card-ability state :challenger lib 0)
      (prompt-select :challenger (find-card "Darwin" (:hand (get-challenger)))) ; Darwin is a virus
      (is (= 3 (count (:hosted (refresh lib)))) "Still 3 resources hosted")
      (is (= 2 (:click (get-challenger))) "Failed Darwin didn't use a click")
      (is (= 1 (count (:hand (get-challenger)))))
      (card-ability state :challenger lib 1) ; Add a resource hosted on London Library to your Grip
      (prompt-card :challenger nil)
      (prompt-select :challenger (find-card "Study Guide" (:hosted (refresh lib))))
      (is (= 2 (count (:hand (get-challenger)))) "Return Study Guide to hand")
      (is (= 2 (count (:hosted (refresh lib)))) "2 resources hosted")
      (card-ability state :challenger lib 0)
      (prompt-select :challenger (find-card "Study Guide" (:hand (get-challenger))))
      (is (= 3 (count (:hosted (refresh lib)))) "3 resources hosted")
      (is (zero? (count (:discard (get-challenger)))) "Nothing in archives yet")
      (take-credits state :challenger)
      (is (zero? (count (:hosted (refresh lib)))) "All resources discarded when turn ends")
      (is (= 2 (count (:hand (get-challenger)))) "Darwin never got played, Chameleon returned to hand")
      (is (= 2 (count (:discard (get-challenger)))) "Femme Fatale and Study Guide discarded"))))

(deftest muertos-gang-member
  ;; Muertos Gang Member - Install and Discard
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Tollbooth" "Ice Wall"])
                (default-challenger [(qty "Hedge Fund" 3) "Muertos Gang Member"]))
      (play-from-hand state :contestant "Tollbooth" "HQ")
      (play-from-hand state :contestant "Ice Wall" "Archives")
      (take-credits state :contestant)
      (let [toll (get-character state :hq 0)
            iw (get-character state :archives 0)]
        (core/rez state :contestant iw)
        (core/move state :challenger (find-card "Hedge Fund" (:hand (get-challenger))) :deck)
        (play-from-hand state :challenger "Muertos Gang Member")
        (prompt-select :contestant (refresh iw))
        (is (not (:rezzed (refresh iw))) "Ice Wall derezzed")
        (is (= 2 (count (:hand (get-challenger)))) "2 cards in Challenger's hand")
        (let [muer (get-radicle state 0)]
          (card-ability state :challenger muer 0)
          (is (= 3 (count (:hand (get-challenger)))) "Challenger drew a card from Muertos")
          (prompt-select :contestant toll)
          (is (:rezzed (refresh toll)) "Tollbooth was rezzed")))))
  (testing "Account for Reina interaction, #1098"
    (do-game
      (new-game (default-contestant ["Tollbooth" "Ice Wall"])
                (make-deck "Reina Roja: Freedom Fighter" [(qty "Hedge Fund" 3)
                                                          "Muertos Gang Member"]))
      (play-from-hand state :contestant "Tollbooth" "HQ")
      (play-from-hand state :contestant "Ice Wall" "Archives")
      (let [toll (get-character state :hq 0)
            iw (get-character state :archives 0)]
        (core/rez state :contestant iw)
        (take-credits state :contestant)
        (core/lose state :contestant :credit 100)
        (core/move state :challenger (find-card "Hedge Fund" (:hand (get-challenger))) :deck)
        (play-from-hand state :challenger "Muertos Gang Member")
        (prompt-select :contestant (refresh iw))
        (is (not (:rezzed (refresh iw))) "Ice Wall derezzed")
        (is (= 2 (count (:hand (get-challenger)))) "2 cards in Challenger's hand")
        (let [muer (get-radicle state 0)]
          (card-ability state :challenger muer 0)
          (is (= 3 (count (:hand (get-challenger)))) "Challenger drew a card from Muertos")
          (prompt-select :contestant toll)
          (is (:rezzed (refresh toll)) "Tollbooth was rezzed")
          (is (zero? (:credit (get-contestant))) "Contestant has 0 credits"))))))

(deftest net-mercur
  ;; Net Mercur - Gains 1 credit or draw 1 card when a stealth credit is used
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Net Mercur" "Silencer" "Ghost Challenger"]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 4 :credit 10)
    (play-from-hand state :challenger "Silencer")
    (play-from-hand state :challenger "Net Mercur")
    (play-from-hand state :challenger "Ghost Challenger")
    (let [sil (get-hazard state 0)
          nm (get-radicle state 0)
          gr (get-radicle state 1)]
      (card-ability state :challenger gr 0)
      (is (empty? (:prompt (get-challenger))) "No Net Mercur prompt from stealth spent outside of run")
      (run-on state :hq)
      (card-ability state :challenger sil 0)
      (prompt-choice :challenger "Place 1 [Credits]")
      (is (= 1 (get-counters (refresh nm) :credit)) "1 credit placed on Net Mercur")
      (card-ability state :challenger gr 0)
      (is (empty? (:prompt (get-challenger))) "No Net Mercur prompt for 2nd stealth in run")
      (run-jack-out state)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-on state :hq)
      (card-ability state :challenger nm 0)
      (is (= "Net Mercur" (:title (:card (first (get-in @state [:challenger :prompt]))))) "Net Mercur triggers itself"))))

(deftest network-exchange
  ;; Character install costs 1 more except for inner most
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Paper Wall" 3)])
                (default-challenger ["Network Exchange"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Network Exchange")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Paper Wall" "HQ")
      (is (= 8 (:credit (get-contestant))) "Paid 0 to install Paper Wall")
      (play-from-hand state :contestant "Paper Wall" "HQ")
      (is (= 6 (:credit (get-contestant))) "Paid 1 extra  to install Paper Wall")
      (play-from-hand state :contestant "Paper Wall" "HQ")
      (is (= 3 (:credit (get-contestant))) "Paid 1 extra  to install Paper Wall")))
  (testing "Architect 1st sub should ignore additional install cost"
    (do-game
      (new-game (default-contestant [(qty "Architect" 3)])
                (default-challenger ["Network Exchange"]))
      (play-from-hand state :contestant "Architect" "HQ")
      (take-credits state :contestant) ; contestant has 7 credits
      (play-from-hand state :challenger "Network Exchange")
      (take-credits state :challenger)
      (let [architect (get-character state :hq 0)]
        (core/rez state :contestant architect)
        (is (= 3 (:credit (get-contestant))) "Contestant has 3 credits after rez")
        (core/move state :contestant (find-card "Architect" (:hand (get-contestant))) :deck)
        (card-subroutine state :contestant architect 0)
        (prompt-card :contestant (find-card "Architect" (:deck (get-contestant))))
        (prompt-choice :contestant "HQ")
        (is (= 3 (:credit (get-contestant))) "Contestant has 7 credits")))))

(deftest neutralize-all-threats
  ;; Neutralize All Threats - Access 2 cards from HQ, force discard first accessed card with a discard cost
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 2) "Breaker Bay Grid" "Elizabeth Mills"])
              (default-challenger ["Neutralize All Threats"]))
    (play-from-hand state :contestant "Breaker Bay Grid" "New remote")
    (play-from-hand state :contestant "Elizabeth Mills" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Neutralize All Threats")
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Card from hand")
    (prompt-choice :challenger "No action") ; access first Hedge Fund
    (prompt-choice :challenger "Card from hand")
    (prompt-choice :challenger "No action") ; access second Hedge Fund
    (run-empty-server state "Server 1")
    (prompt-choice-partial :challenger "Pay")
    (is (= 3 (:credit (get-challenger))) "Forced to pay 2c to discard BBG")
    (is (= 1 (count (:discard (get-contestant)))) "Breaker Bay Grid discarded")
    (run-empty-server state "Server 2")
    (is (not (empty? (:prompt (get-challenger)))) "Challenger prompt to discard Elizabeth Mills")))

(deftest new-angeles-city-hall
  ;; New Angeles City Hall - Avoid tags; discard when agenda is stolen
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["SEA Source" "Breaking News"])
                (default-challenger ["New Angeles City Hall"]))
      (play-from-hand state :contestant "Breaking News" "New remote")
      (take-credits state :contestant 2)
      (play-from-hand state :challenger "New Angeles City Hall")
      (let [nach (get-radicle state 0)]
        (run-empty-server state "Archives")
        (take-credits state :challenger)
        (is (= 6 (:credit (get-challenger))))
        (play-from-hand state :contestant "SEA Source")
        (prompt-choice :contestant 0) ; default trace
        (prompt-choice :challenger 0) ; Challenger won't match
        (card-ability state :challenger nach 0)
        (prompt-choice :challenger "Done")
        (is (zero? (:tag (get-challenger))) "Avoided SEA Source tag")
        (is (= 4 (:credit (get-challenger))) "Paid 2 credits")
        (take-credits state :contestant)
        (run-empty-server state "Server 1")
        (prompt-choice :challenger "Steal")
        (is (= 1 (:agenda-point (get-challenger))))
        (is (empty? (get-radicle state)) "NACH discarded by agenda steal"))))
  (testing "don't gain Siphon credits until opportunity to avoid tags has passed"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Account Siphon" "New Angeles City Hall"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "New Angeles City Hall")
      (play-run-event state (first (:hand (get-challenger))) :hq)
      (prompt-choice :challenger "Replacement effect")
      (let [nach (get-radicle state 0)]
        (is (= 4 (:credit (get-challenger))) "Have not gained Account Siphon credits until tag avoidance window closes")
        (card-ability state :challenger nach 0)
        (card-ability state :challenger nach 0)
        (prompt-choice :challenger "Done")
        (is (zero? (:tag (get-challenger))) "Tags avoided")
        (is (= 10 (:credit (get-challenger))) "10 credits siphoned")
        (is (= 3 (:credit (get-contestant))) "Contestant lost 5 credits")))))

(deftest no-one-home
  ;; Prevent first tag or net damage of the turn if you beat trace0, then discard
  (do-game
    (new-game (default-contestant ["Data Mine" "SEA Source" "Scorched Earth"])
              (default-challenger [(qty "No One Home" 3) (qty "Sure Gamble" 2)]))
    (play-from-hand state :contestant "Data Mine" "Server 1")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "No One Home")
    (let [dm (get-character state :remote1 0)
          noh (get-radicle state 0)]
      (run-on state "Server 1")
      (core/rez state :contestant dm)
      (card-subroutine state :contestant dm 0)
      (card-ability state :challenger noh 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (prompt-choice :challenger "Done")
      (is (= 3 (count (:hand (get-challenger)))) "1 net damage prevented")
      (run-successful state)
      (play-from-hand state :challenger "No One Home")
      (take-credits state :challenger)
      (play-from-hand state :contestant "SEA Source")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 1 (count (:prompt (get-challenger)))) "Challenger prompted to avoid tag")
      (card-ability state :challenger (get-radicle state 0) 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (prompt-choice :challenger "Done")
      (is (= 3 (count (:discard (get-challenger)))) "Two NOH discarded, 1 gamble played")
      (is (zero? (:tag (get-challenger))) "Tags avoided")
      (take-credits state :contestant)
      (play-from-hand state :challenger "No One Home")
      (take-credits state :challenger)
      (core/gain state :challenger :tag 1)
      (core/gain state :contestant :credit 4)
      (play-from-hand state :contestant "Scorched Earth")
      (is (zero? (count (:prompt (get-challenger)))) "Challenger not prompted to avoid meat damage"))))

(deftest off-campus-apartment
  ;; Off-Campus Apartment
  (testing "ability shows a simultaneous resolution prompt when appropriate"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Street Peddler" "Off-Campus Apartment"
                                 "Underworld Contact" (qty "Spy Camera" 6)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Street Peddler" "Off-Campus Apartment" "Underworld Contact"])
      (play-from-hand state :challenger "Off-Campus Apartment")
      (let [oca (get-radicle state 0)]
        (card-ability state :challenger oca 0)
        (prompt-select :challenger (find-card "Underworld Contact" (:hand (get-challenger))))
        (is (= 2 (count (:hand (get-challenger)))) "Drew a card from OCA")
        (card-ability state :challenger oca 0)
        (prompt-select :challenger (find-card "Street Peddler" (:hand (get-challenger))))
        ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
        (is (= 2 (-> (get-challenger) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
        (prompt-choice :challenger "Off-Campus Apartment")
        (is (= 2 (count (:hand (get-challenger)))) "Drew a card from OCA"))))
  (testing "second ability does not break cards that are hosting others, e.g., Street Peddler"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Street Peddler" 2) "Off-Campus Apartment" (qty "Spy Camera" 6)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Street Peddler" "Street Peddler" "Off-Campus Apartment"])
      (core/move state :challenger (find-card "Street Peddler" (:hand (get-challenger))) :deck {:front true})
      (play-from-hand state :challenger "Off-Campus Apartment")
      (let [oca (get-radicle state 0)]
        (card-ability state :challenger oca 0)
        (prompt-select :challenger (find-card "Street Peddler" (:hand (get-challenger))))
        (prompt-choice :challenger "Street Peddler")
        (let [ped1 (first (:hosted (refresh oca)))]
          (card-ability state :challenger ped1 0)
          (prompt-card :challenger (-> (get-challenger) :prompt first :choices second)) ; choose Street Peddler
          (card-ability state :challenger (refresh oca) 1)
          (prompt-select :challenger (get-radicle state 1))
          (let [ped2 (first (:hosted (refresh oca)))]
            (card-ability state :challenger ped2 0)
            (prompt-card :challenger (-> (get-challenger) :prompt first :choices first)) ; choose Spy Camera
            ;; the fact that we got this far means the bug is fixed
            (is (= 1 (count (get-hazard state))) "Spy Camera installed")))))))

(deftest offcharacterr-frank
  ;; Offcharacterr Frank - meat damage to discard 2 from HQ
  (do-game
      (new-game (default-contestant ["Swordsman" (qty "Hedge Fund" 2)])
                (default-challenger ["Offcharacterr Frank" "Skulljack" (qty "Respirocytes" 4)]))
   (play-from-hand state :contestant "Swordsman" "Archives")
   (take-credits state :contestant)
   (starting-hand state :challenger ["Offcharacterr Frank" "Skulljack" "Respirocytes" "Respirocytes" "Respirocytes" "Respirocytes"])
   (play-from-hand state :challenger "Offcharacterr Frank")
   (card-ability state :challenger (get-radicle state 0) 0)
   (is (zero? (count (:discard (get-contestant)))) "Nothing discarded from HQ")
   (play-from-hand state :challenger "Skulljack")
   (is (= 3 (count (:hand (get-challenger)))) "Took 1 brain damage")
   (card-ability state :challenger (get-radicle state 0) 0)
   (is (zero? (count (:discard (get-contestant)))) "Nothing discarded from HQ")
   (let [sm (get-character state :archives 0)]
     (run-on state :archives)
     (core/rez state :contestant sm)
     (card-subroutine state :contestant sm 0)
     (run-jack-out state))
   (is (= 2 (count (:hand (get-challenger)))) "Took 1 net damage")
   (card-ability state :challenger (get-radicle state 0) 0)
   (is (zero? (count (:discard (get-contestant)))) "Nothing discarded from HQ")
   (play-from-hand state :challenger "Respirocytes")
   (is (zero? (count (:hand (get-challenger)))) "Took 1 meat damage")
   (card-ability state :challenger (get-radicle state 0) 0)
   (is (= 2 (count (:discard (get-contestant)))) "Two cards discarded from HQ")))

(deftest pad-tap
  ;; PAD Tap
  (do-game
    (new-game (default-contestant ["Melange Mining Contestant."])
              (default-challenger ["PAD Tap"]))
    (play-from-hand state :contestant "Melange Mining Contestant." "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "PAD Tap")
    (let [mel (get-content state :remote1 0)
          tap (get-radicle state 0)]
      (take-credits state :challenger)
      (let [credits (:credit (get-challenger))]
        (core/click-credit state :contestant nil)
        (is (zero? (-> (get-challenger) :prompt count)) "Challenger should have no prompts from PAD Tap")
        (is (= credits (:credit (get-challenger))) "Challenger shouldn't gain PAD Tap credits from clicking for a credit"))
      (let [credits (:credit (get-challenger))]
        (core/rez state :contestant mel)
        (core/gain state :contestant :click 10)
        (card-ability state :contestant mel 0)
        (is (= (+ credits 1) (:credit (get-challenger))) "Challenger should gain 1 credit from PAD Tap triggering from Melange Mining Contestant. ability")
        (card-ability state :contestant mel 0) ;; Triggering Melange a second time
        (is (zero? (-> (get-challenger) :prompt count)) "Challenger should have no prompts from PAD Tap"))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (zero? (-> (get-challenger) :discard count)) "Challenger should have 0 cards in Heap")
      (let [credits (:credit (get-contestant))
            clicks (:click (get-contestant))]
        (card-side-ability state :contestant tap 0)
        (is (= (- credits 3) (:credit (get-contestant))) "PAD Tap ability should cost Contestant 3 credits")
        (is (= (- clicks 1) (:click (get-contestant))) "PAD Tap ability should cost Contestant 1 click")))))

(deftest paige-piper
  ;; Paige Piper
  (testing "interaction with Frantic Coding. Issue #2190"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Paige Piper" (qty "Frantic Coding" 2) (qty "Sure Gamble" 3)
                                 (qty "Gordian Blade" 2) "Ninja" (qty "Bank Job" 3) (qty "Indexing" 2)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Paige Piper" "Frantic Coding" "Frantic Coding"])
      (play-from-hand state :challenger "Paige Piper")
      (prompt-choice :challenger "No")
      (take-credits state :challenger) ; now 8 credits
      (take-credits state :contestant)
      (play-from-hand state :challenger "Frantic Coding")
      (prompt-choice :challenger "No action")
      (prompt-card :challenger (find-card "Gordian Blade" (:deck (get-challenger))))
      (is (= 1 (count (get-resource state))) "Installed Gordian Blade")
      (prompt-choice :challenger "Yes")
      (prompt-choice :challenger "0")
      (is (= 1 (count (:discard (get-challenger)))) "Paige Piper intervention stopped Frantic Coding from discarding 9 cards")
      (is (= 5 (:credit (get-challenger))) "No charge to install Gordian")
      ;; a second Frantic Coding will not trigger Paige (once per turn)
      (play-from-hand state :challenger "Frantic Coding")
      (prompt-choice :challenger "No action")
      (prompt-card :challenger (find-card "Ninja" (:deck (get-challenger))))
      (is (= 2 (count (get-resource state))) "Installed Ninja")
      (is (= 11 (count (:discard (get-challenger)))) "11 cards in heap")
      (is (= 2 (:credit (get-challenger))) "No charge to install Ninja"))))

(deftest patron
  ;; Patron
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Jackson Howard"])
                (default-challenger [(qty "Patron" 4) (qty "Easy Mark" 4)]))
      (play-from-hand state :contestant "Jackson Howard" "New remote")
      (take-credits state :contestant 2)
      (play-from-hand state :challenger "Patron")
      (let [p (get-radicle state 0)]
        (take-credits state :challenger 3)
        (take-credits state :contestant)
        (prompt-choice :challenger "Server 1")
        (is (= 4 (count (:hand (get-challenger)))) "Starts with 4 cards")
        (run-empty-server state "Server 1")
        (is (= 6 (count (:hand (get-challenger)))) "Drew 2 cards")
        (run-empty-server state "Server 1")
        (prompt-choice :challenger "No")
        (is (= 6 (count (:hand (get-challenger)))) "Drew no cards")
        (play-from-hand state :challenger "Easy Mark")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (prompt-choice :challenger "Server 1")
        (run-empty-server state "Archives")
        (is (= 5 (count (:hand (get-challenger)))) "Did not draw cards when running other server"))))
  (testing "Manually selecting during Step 1.2 does not show a second prompt at start of turn. Issue #1744."
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Patron" 3) (qty "Jak Sinclair" 3)]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (starting-hand state :challenger ["Patron" "Jak Sinclair"])
      (play-from-hand state :challenger "Patron")
      (play-from-hand state :challenger "Jak Sinclair")
      (take-credits state :challenger)
      (let [p (get-radicle state 0)
            j (get-radicle state 1)]
        (take-credits state :contestant)
        (is (:challenger-phase-12 @state) "Challenger in Step 1.2")
        (card-ability state :challenger p 0)
        (prompt-choice :challenger "Archives")
        (card-ability state :challenger j 0)
        (prompt-choice :challenger "Archives")
        (run-successful state)
        (core/end-phase-12 state :challenger nil)
        (is (empty? (:prompt (get-challenger))) "No second prompt for Patron - used already")))))

(deftest professional-contacts
  ;; Professional Contacts - Click to gain 1 credit and draw 1 card
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Professional Contacts" 3)
                               (qty "Sure Gamble" 2)
                               (qty "Shiv" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Professional Contacts")
    (let [proco (get-radicle state 0)]
      (card-ability state :challenger proco 0)
      (is (= 2 (:click (get-challenger))) "Spent 1 click")
      (is (= 1 (:credit (get-challenger))) "Gained 1 credit")
      (is (= 5 (count (:hand (get-challenger)))) "Drew 1 card")
      (card-ability state :challenger proco 0)
      (is (= 1 (:click (get-challenger))) "Spent 1 click")
      (is (= 2 (:credit (get-challenger))) "Gained 1 credit")
      (is (= 6 (count (:hand (get-challenger)))) "Drew 1 card"))))

(deftest reclaim
  ;; Reclaim - discard Reclaim, discard card from grip, install resource, hazard, or virtual radicle from heap
  (testing "Basic behavior"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Reclaim" "Mimic" "Clone Chip"]))
      (take-credits state :contestant)
      (core/move state :challenger (find-card "Mimic" (:hand (get-challenger))) :discard)
      (play-from-hand state :challenger "Reclaim")
      (is (empty? (get-resource state)) "No resources installed")
      (is (= 5 (:credit (get-challenger))) "Challenger starts with 5c.")
      (card-ability state :challenger (get-radicle state 0) 0)
      (prompt-card :challenger (find-card "Clone Chip" (:hand (get-challenger))))
      (prompt-card :challenger (find-card "Mimic" (:discard (get-challenger))))
      (is (= 1 (count (get-resource state))) "1 Resource installed")
      (is (= 2 (:credit (get-challenger))) "Challenger paid install cost")))
  (testing "No cards in hand"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Reclaim"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Reclaim")
      (card-ability state :challenger (get-radicle state 0) 0)
      (is (empty? (:prompt (get-challenger))) "No Reclaim prompt")))
  (testing "Can install discarded card"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Reclaim" "Mimic"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Reclaim")
      (is (empty? (get-resource state)) "No resources installed")
      (is (= 5 (:credit (get-challenger))) "Challenger starts with 5c.")
      (card-ability state :challenger (get-radicle state 0) 0)
      (prompt-card :challenger (find-card "Mimic" (:hand (get-challenger))))
      (prompt-card :challenger (find-card "Mimic" (:discard (get-challenger))))
      (is (= 1 (count (get-resource state))) "1 Resource installed")
      (is (= 2 (:credit (get-challenger))) "Challenger paid install cost")))
  (testing "Can't afford to install card"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Reclaim" "Alpha"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Reclaim")
      (card-ability state :challenger (get-radicle state 0) 0)
      (is (empty? (get-resource state)) "No resources installed")
      (is (= 5 (:credit (get-challenger))) "Challenger starts with 5c.")
      (card-ability state :challenger (get-radicle state 0) 0)
      (prompt-card :challenger (find-card "Alpha" (:hand (get-challenger))))
      (prompt-card :challenger (find-card "Alpha" (:discard (get-challenger))))
      (is (empty? (get-resource state)) "Did not install resource")
      (is (= 5 (:credit (get-challenger))) "Challenger did not spend credits"))))

(deftest rolodex
  ;; Rolodex - Full test
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Rolodex" "Sure Gamble" "Desperado"
                               "Diesel" "Corroder" "Patron"]))
    (starting-hand state :challenger ["Rolodex"])
    (is (= 1 (count (:hand (get-challenger)))))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Rolodex")
    (prompt-card :challenger (find-card "Sure Gamble" (:deck (get-challenger))))
    (prompt-card :challenger (find-card "Desperado" (:deck (get-challenger))))
    (prompt-card :challenger (find-card "Diesel" (:deck (get-challenger))))
    (prompt-card :challenger (find-card "Corroder" (:deck (get-challenger))))
    (prompt-card :challenger (find-card "Patron" (:deck (get-challenger))))
    ;; try starting over
    (prompt-choice :challenger "Start over")
    (prompt-card :challenger (find-card "Patron" (:deck (get-challenger))))
    (prompt-card :challenger (find-card "Corroder" (:deck (get-challenger))))
    (prompt-card :challenger (find-card "Diesel" (:deck (get-challenger))))
    (prompt-card :challenger (find-card "Desperado" (:deck (get-challenger))))
    (prompt-card :challenger (find-card "Sure Gamble" (:deck (get-challenger)))) ;this is the top card on stack
    (prompt-choice :challenger "Done")
    (is (= "Sure Gamble" (:title (first (:deck (get-challenger))))))
    (is (= "Desperado" (:title (second (:deck (get-challenger))))))
    (is (= "Diesel" (:title (second (rest (:deck (get-challenger)))))))
    (is (= "Corroder" (:title (second (rest (rest (:deck (get-challenger))))))))
    (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-challenger)))))))))
    (core/discard state :challenger (get-radicle state 0))
    (is (last-log-contains? state "Sure Gamble, Desperado, Diesel")
        "Rolodex did log discarded card names")
    (is (= 4 (count (:discard (get-challenger)))) "Rolodex mills 3 cards when discarded")
    (is (= "Corroder" (:title (first (:deck (get-challenger))))))))

(deftest rosetta-2.0
  ;; Rosetta 2.0 remove an installed resource from the game and install one from the heap lower install cost
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Rosetta 2.0" "Corroder" "Gordian Blade"]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Rosetta 2.0" "Corroder"])
    (core/gain state :challenger :credit 2)
    (play-from-hand state :challenger "Rosetta 2.0")
    (play-from-hand state :challenger "Corroder")
    (is (= 3 (core/available-mu state)) "Corrder cost 1 mu")
    (is (= 2 (:credit (get-challenger))) "Starting with 2 credits")
    (card-ability state :challenger (get-radicle state 0) 0)
    (prompt-select :challenger (get-resource state 0))
    (prompt-choice :challenger (find-card "Gordian Blade" (:deck (get-challenger))))
    (is (= 3 (core/available-mu state)) "Gordian cost 1 mu, Corroder freed")
    (is (zero? (:credit (get-challenger))) "Ending with 0 credits")
    (is (= 1 (count (:rfg (get-challenger)))) "Corroder removed from game")
    (is (= 1 (count (get-resource state))) "One resource installed")
    (is (= "Gordian Blade" (:title (get-resource state 0))) "Gordian installed")))

(deftest sacrificial-construct
  ;; Sacrificial Construct - Discard to prevent discard of installed resource or hazard
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Sacrificial Construct" 2) "Cache"
                               "Motivation" "Astrolabe"]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 1)
    (play-from-hand state :challenger "Sacrificial Construct")
    (play-from-hand state :challenger "Sacrificial Construct")
    (play-from-hand state :challenger "Cache")
    (play-from-hand state :challenger "Motivation")
    (play-from-hand state :challenger "Astrolabe")
    (take-credits state :challenger)
    (core/discard state :challenger (get-radicle state 2))
    (is (empty? (:prompt (get-challenger))) "Sac Con not prompting to prevent radicle discard")
    (core/discard state :challenger (get-resource state 0))
    (card-ability state :challenger (get-radicle state 0) 0)
    (is (= 2 (count (:discard (get-challenger)))) "Sac Con discarded")
    (is (= 1 (count (get-resource state))) "Cache still installed")
    (core/discard state :challenger (get-hazard state 0))
    (card-ability state :challenger (get-radicle state 0) 0)
    (is (= 3 (count (:discard (get-challenger)))) "Sac Con discarded")
    (is (= 1 (count (get-hazard state))) "Astrolabe still installed")))

(deftest safety-first
  ;; Safety First - Reduce hand size by 2, draw 1 at turn end if below maximum
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Safety First" 3) (qty "Cache" 3)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Safety First" "Safety First" "Cache"])
    (play-from-hand state :challenger "Safety First")
    (is (= 3 (core/hand-size state :challenger)) "Max hand size reduced by 2")
    (take-credits state :challenger)
    (is (= 3 (count (:hand (get-challenger)))) "Drew 1 card at end of turn")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 3 (count (:hand (get-challenger)))) "Drew no cards, at maximum")))

(deftest scrubber
  ;; Scrubber
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["The Board"])
                (default-challenger ["Scrubber"]))
      (play-from-hand state :contestant "The Board" "New remote")
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (is (= 1 (-> (get-challenger) :prompt first :choices count)) "Challenger doesn't have enough credits to discard")
      (prompt-choice :challenger "No action")
      (play-from-hand state :challenger "Scrubber")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 5 (:credit (get-challenger))) "Challenger should only have 5 credits in pool")
      (run-empty-server state "Server 1")
      (is (= 2 (-> (get-challenger) :prompt first :choices count)) "Challenger can use Scrubber credits to discard")
      (let [scrubber (get-radicle state 0)]
        (card-ability state :challenger scrubber 0)
        (card-ability state :challenger scrubber 0))
      (prompt-choice-partial :challenger "Pay")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger should discard The Board and gain 2 agenda points")))
  (testing "when under discard cost but can up with recurring credits"
    (do-game
      (new-game (default-contestant ["The Board"])
                (default-challenger ["Scrubber" "Skulljack" "Sure Gamble"]))
      (play-from-hand state :contestant "The Board" "New remote")
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (is (= 1 (-> (get-challenger) :prompt first :choices count)) "Challenger doesn't have enough credits to discard")
      (prompt-choice :challenger "No action")
      (play-from-hand state :challenger "Scrubber")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Skulljack")
      (core/gain state :challenger :credit 1)
      (is (= 4 (:credit (get-challenger))) "Challenger should only have 4 credits in pool")
      (run-empty-server state "Server 1")
      (is (= 6 (core/discard-cost state :challenger (get-content state :remote1 0))) "The Board should cost 6 to discard")
      (is (= 2 (-> (get-challenger) :prompt first :choices count)) "Challenger can use Scrubber credits to discard")
      (prompt-choice-partial :challenger "Pay") ;; Whoops, challenger forgot to actually get the credits from Scrubber
      (is (= 6 (core/discard-cost state :challenger (get-content state :remote1 0))) "Skulljack shouldn't trigger a second time")
      (is (= 2 (-> (get-challenger) :prompt first :choices count)) "Challenger can still use Scrubber credits the second time around")
      (let [scrubber (get-radicle state 0)]
        (card-ability state :challenger scrubber 0)
        (card-ability state :challenger scrubber 0))
      (prompt-choice-partial :challenger "Pay") ;; Now the challenger has actually gained the Scrubber credits
      (is (= 2 (:agenda-point (get-challenger))) "Challenger should discard The Board and gain 2 agenda points"))))

(deftest salsette-slums
  ;; Salsette Slums - Once per turn, when the discard cost of a card is paid, optionally remove from the game
  (do-game
    (new-game (default-contestant ["Hostile Infrastructure" "Tech Startup" "Thomas Haas"
                             (qty "Hedge Fund" 3)])
              (default-challenger [(qty "Salsette Slums" 2) (qty "Sure Gamble" 3)]))
    ;; Use Hostile Infrastructure to ensure on-discard effects don't fire.
    (core/move state :contestant (find-card "Hostile Infrastructure" (:deck (get-contestant))) :hand)
    (core/move state :contestant (find-card "Tech Startup" (:deck (get-contestant))) :hand)
    (core/move state :contestant (find-card "Thomas Haas" (:deck (get-contestant))) :hand)
    (play-from-hand state :contestant "Tech Startup" "New remote")
    (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
    (play-from-hand state :contestant "Thomas Haas" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Salsette Slums")
    (play-from-hand state :challenger "Salsette Slums")
    (core/gain state :challenger :credit 2)
    (core/gain state :challenger :click 4)
    (let [ts1 (get-content state :remote1 0)
          hostile2 (get-content state :remote2 0)
          th3 (get-content state :remote3 0)
          salsette1 (get-radicle state 0)
          salsette2 (get-radicle state 1)]
      (is (= 3 (count (:hand (get-challenger)))) "Challenger started this part with three cards in hand")
      (core/rez state :contestant hostile2)
      (run-empty-server state "Server 1")
      (is (not (empty? (:prompt (get-challenger)))) "Prompting to discard.")
      (card-ability state :challenger salsette1 0)
      (is (empty? (:prompt (get-challenger))) "All prompts done")
      (is (= 3 (count (:hand (get-challenger)))) "On-discard ability of other Hostile didn't fire")
      (is (= (:cid ts1) (:cid (last (:rfg (get-contestant))))) "Tech Startup was removed from game")
      (is (= 2 (:credit (get-challenger))) "Challenger paid the discard cost.")
      (is (not (:run @state)) "Run is over")
      (run-empty-server state :remote2)
      (is (not (empty? (:prompt (get-challenger)))) "Prompting to discard")
      ;; Only able to use the ability once per turn
      (card-ability state :challenger salsette1 0)
      (is (not (empty? (:prompt (get-challenger)))) "Still prompting to discard")
      (is (:run @state) "Run is still occurring")
      ;; Can't use the ability if you can't afford to discard
      (card-ability state :challenger salsette2 0)
      (is (not (empty? (:prompt (get-challenger)))) "Still prompting to discard")
      (is (:run @state) "Run is still occurring")
      (prompt-choice :challenger "No action")
      ;; Test the "oops I forgot" ability (challenger feels bad that they forgot to use Slums when a Hostile is out)
      (run-empty-server state :remote3)
      (prompt-choice-partial :challenger "Pay")
      ;; Can only use that first Slums once
      (card-ability state :challenger salsette1 1)
      (is (empty? (:prompt (get-challenger))) "Not prompting the challenger")
      (is (not (= (:cid th3) (:cid (last (:rfg (get-contestant)))))) "Card was not removed from the game")
      (card-ability state :challenger salsette2 1)
      (is (not (empty? (:prompt (get-challenger)))) "Prompting the challenger to choose a card")
      (prompt-select :challenger (find-card "Thomas Haas" (:discard (get-contestant))))
      (is (= (:cid th3) (:cid (last (:rfg (get-contestant))))) "Card was removed from the game"))
    ;; Set things up so we can discard the Hostile and then make sure we can't "oops I forgot on a later turn"
    (core/gain state :challenger :credit 5)
    (run-empty-server state :remote2)
    (prompt-choice-partial :challenger "Pay")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (let [salsette1 (get-radicle state 0)
          hostile2 (get-content state :remote2 0)]
      (card-ability state :challenger salsette1 1)
      (prompt-select :challenger (find-card "Hostile Infrastructure" (:discard (get-contestant))))
      (is (not (= (:cid hostile2) (:cid (last (:rfg (get-contestant)))))) "Did not remove card from game"))))

(deftest security-testing
  ;; Security Testing
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Jackson Howard"])
                (default-challenger ["Security Testing"]))
      (play-from-hand state :contestant "Jackson Howard" "New remote")
      (take-credits state :contestant 2)
      (play-from-hand state :challenger "Security Testing")
      (let [st (get-radicle state 0)]
        (take-credits state :challenger 3)
        (take-credits state :contestant)
        (prompt-choice :challenger "Server 1")
        (run-empty-server state "Server 1")
        (is (= 10 (:credit (get-challenger))) "Gained 2 credits from Security Testing")
        (run-empty-server state "Server 1")
        (prompt-choice :challenger "No")
        (is (= 10 (:credit (get-challenger))) "Did not gain credits on second run")
        (take-credits state :challenger 2)
        (take-credits state :contestant)
        (prompt-choice :challenger "Server 1")
        (run-empty-server state "Archives")
        (is (= 12 (:credit (get-challenger))) "Did not gain credits when running other server"))))
  (testing "with multiple copies"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Security Testing" 2)]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Security Testing")
      (play-from-hand state :challenger "Security Testing")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (prompt-choice :challenger "Archives")
      (prompt-choice :challenger "R&D")
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-challenger))) "Gained 2 credits")
      (run-empty-server state "R&D")
      (is (= 11 (:credit (get-challenger)))))))

(deftest spoilers
  ;; Spoilers - Mill the Contestant when it scores an agenda
  (do-game
    (new-game (default-contestant ["Hostile Takeover" "Hedge Fund"])
              (default-challenger ["Spoilers"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Spoilers")
    (take-credits state :challenger)
    (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
    (is (= 1 (count (:deck (get-contestant)))))
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (score-agenda state :contestant ht)
      (is (= 1 (count (:discard (get-contestant)))))
      (is (zero? (count (:deck (get-contestant)))) "Last card from R&D milled"))))

(deftest stim-dealer
  ;; Stim Dealer - Take 1 brain damage when it accumulates 2 power counters
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Stim Dealer" "Sure Gamble" "Feedback Filter"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Feedback Filter")
    (play-from-hand state :challenger "Stim Dealer")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (let [sd (get-radicle state 0)]
      (is (= 1 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-challenger))) "Gained 1 click")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 2 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-challenger))) "Gained 1 click")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (zero? (get-counters (refresh sd) :power)) "Lost all counters")
      (is (empty? (:prompt (get-challenger))) "No Feedback Filter brain dmg prevention possible")
      (is (= 1 (:brain-damage (get-challenger))) "Took 1 brain damage")
      (is (= 4 (:click (get-challenger))) "Didn't gain extra click"))))

(deftest street-peddler
  ;; Street Peddler
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Street Peddler" "Gordian Blade"
                                 "Torch" (qty "Sure Gamble" 2)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Street Peddler" "Sure Gamble"])
      (play-from-hand state :challenger "Street Peddler")
      (let [sp (get-radicle state 0)]
        (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
        (card-ability state :challenger sp 0)
        (prompt-card :challenger (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (= "Gordian Blade" (:title (get-resource state 0)))
            "Gordian Blade was installed")
        (is (= 3 (core/available-mu state)) "Gordian cost 1 mu"))))
  (testing "Can't afford install"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Street Peddler" (qty "Gordian Blade" 3)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Street Peddler"])
      (play-from-hand state :challenger "Street Peddler")
      (let [sp (get-radicle state 0)]
        (card-ability state :challenger sp 0)
        (core/lose state :challenger :credit 3)
        (is (= 2 (count (:choices (first (:prompt (get-challenger))))))
            "1 card and 1 cancel option on Street Peddler")
        (prompt-card :challenger (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (zero? (count (get-resource state)))
            "Gordian Blade was not installed")
        (is (and (:installed (refresh sp)) (= 3 (count (:hosted (refresh sp))))
                 "Street Peddler still installed with 3 hosted cards")))))
  (testing "Interaction with Kate discount"
    (do-game
      (new-game (default-contestant)
                (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" ["Street Peddler"
                                                                     "Gordian Blade"
                                                                     (qty "Sure Gamble" 2)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Street Peddler"])
      (play-from-hand state :challenger "Street Peddler")
      (let [sp (get-radicle state 0)]
        ;; should still be able to afford Gordian w/ Kate discount
        (core/lose state :challenger :credit 3)
        (card-ability state :challenger sp 0)
        (is (= 2 (count (:choices (first (:prompt (get-challenger))))))
            "Only 1 choice (plus Cancel) to install off Peddler")
        (prompt-card :challenger (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
        (is (= "Gordian Blade" (:title (get-resource state 0)))
            "Gordian Blade was installed")
        (is (= 3 (core/available-mu state)) "Gordian cost 1 mu"))))
  (testing "Resources should cost memory. Issue #708"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Street Peddler" (qty "Corroder" 3)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Street Peddler"])
      (play-from-hand state :challenger "Street Peddler")
      (is (= 4 (core/available-mu state)) "No memory cost for hosting on Street Peddler")
      (let [sp (get-radicle state 0)]
        (is (= "Corroder" (:title (first (:hosted sp)))) "Street Peddler is hosting Corroder")
        (card-ability state :challenger sp 0)
        (prompt-card :challenger (first (:hosted sp))) ; choose to install Gordian
        (is (= "Corroder" (:title (get-resource state 0)))
            "Corroder was installed")
        (is (= 3 (core/available-mu state)) "Corroder cost 1 mu"))))
  (testing "Muertos/Brain Chip uninstall effect not fired when removed off peddler/hosting Issue #2294, #2358"
    (do-game
      (new-game (default-contestant ["Jackson Howard"])
                (default-challenger [(qty "Street Peddler" 2) "Muertos Gang Member" "Brain Chip"]))
      (core/move state :challenger (find-card "Muertos Gang Member" (:hand (get-challenger))) :deck {:front true})
      (core/move state :challenger (find-card "Brain Chip" (:hand (get-challenger))) :deck {:front true})
      (core/move state :challenger (find-card "Street Peddler" (:hand (get-challenger))) :deck {:front true})
      (play-from-hand state :contestant "Jackson Howard" "New remote")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Street Peddler")
      (core/gain state :challenger :agenda-point 1)
      (let [jh (get-content state :remote1 0)
            sp (get-radicle state 0)]
        (core/rez state :contestant jh)
        (card-ability state :challenger sp 0)
        (prompt-card :challenger (find-card "Street Peddler" (:hosted sp))) ; choose to another Peddler
        (is (empty? (:prompt (get-contestant))) "Contestant not prompted to rez Jackson")
        (is (= 4 (core/available-mu state)) "Challenger has 4 MU"))))
  (testing "Discarding hazard should not reduce :in-play values"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Street Peddler" (qty "HQ Interface" 3)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Street Peddler"])
      (play-from-hand state :challenger "Street Peddler")
      (let [sp (get-radicle state 0)]
        (card-ability state :challenger sp 0)
        (prompt-card :challenger (first (:hosted sp))) ; choose to install HQ Interface
        (is (= 2 (:hq-access (get-challenger)))
            "HQ Access increased by 1 from installed HQI and not reduced by the 2 discarded ones"))))
  (testing "Installing Parasite with only 1cr. Issue #491."
    (do-game
      (new-game (default-contestant [(qty "Pop-up Window" 3)])
                (default-challenger ["Street Peddler" (qty "Parasite" 3)]))
      (play-from-hand state :contestant "Pop-up Window" "HQ")
      (take-credits state :contestant 2)
      (starting-hand state :challenger ["Street Peddler"])
      (core/lose state :challenger :credit 4) ; go down to 1 credit
      (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
      (play-from-hand state :challenger "Street Peddler")
      (let [sp (get-radicle state 0)
            pu (get-character state :hq 0)]
        (core/rez state :contestant pu)
        (card-ability state :challenger sp 0)
        (prompt-card :challenger (first (:hosted sp))) ; choose to install Parasite
        (is (= "Parasite" (:title (:card (first (get-in @state [:challenger :prompt])))))
            "Parasite target prompt")
        (prompt-select :challenger pu)
        (is (= 4 (count (:discard (get-challenger)))) "3 Parasite, 1 Street Peddler in heap")
        (is (= 1 (count (:discard (get-contestant)))) "Pop-up Window in archives"))))
  (testing "Tech Trader install"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Street Peddler"
                                 "Tech Trader"]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Street Peddler"])
      (play-from-hand state :challenger "Street Peddler")
      (let [sp (get-radicle state 0)]
        (is (= 1 (count (:hosted sp))) "Street Peddler is hosting 1 card")
        (card-ability state :challenger sp 0)
        (prompt-card :challenger (find-card "Tech Trader" (:hosted sp))) ; choose to install Tech Trader
        (is (= "Tech Trader" (:title (get-radicle state 0)))
            "Tech Trader was installed")
        (is (= 5 (:credit (get-challenger))) "Did not gain 1cr from Tech Trader ability")))))

(deftest-pending street-peddler-discard-while-choosing-card
  ;; Street Peddler - discarding Street Peddler while choosing which card to
  ;; discard should dismiss the choice prompt. Issue #587.
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Street Peddler"
                               "Gordian Blade"
                               "Torch"
                               (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler" "Sure Gamble"])
    (play-from-hand state :challenger "Street Peddler")
    (let [street-peddler (get-radicle state 0)]
      (is (= 3 (count (:hosted street-peddler))) "Street Peddler is hosting 3 cards")
      (card-ability state :challenger street-peddler 0)
      (discard-radicle state "Street Peddler")
      (is (zero? (count (get-in @state [:challenger :prompt])))))))

(deftest symmetrical-visage
  ;; Symmetrical Visage - Gain 1 credit the first time you click to draw each turn
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Symmetrical Visage" 3)
                                 (qty "Sure Gamble" 3)
                                 "Fall Guy"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Symmetrical Visage")
      (is (= 3 (:credit (get-challenger))))
      (core/click-draw state :challenger nil)
      (is (= 4 (:credit (get-challenger))) "Gained 1 credit from first click spent to draw")
      (core/click-draw state :challenger nil)
      (is (= 4 (:credit (get-challenger))) "No credit gained from second click spent to draw")))
  (testing "Gain 1 credit the first and second time you click to draw each turn when GCS is installed"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Symmetrical Visage" 3)
                                 (qty "Gene Conditioning Shoppe" 3)
                                 "Fall Guy"]))
      (take-credits state :contestant)
      (core/gain state :challenger :click 1)
      (play-from-hand state :challenger "Symmetrical Visage")
      (is (= 3 (:credit (get-challenger))))
      (play-from-hand state :challenger "Gene Conditioning Shoppe")
      (is (= 1 (:credit (get-challenger))))
      (core/click-draw state :challenger nil)
      (is (= 2 (:credit (get-challenger))) "Gained 1 credit from first click spent to draw")
      (core/click-draw state :challenger nil)
      (is (= 3 (:credit (get-challenger)))
          "Gained 1 credit from second click spent to draw with Gene Conditioning Shoppe")
      ;; Move Fall Guy back to deck
      (core/move state :challenger (find-card "Fall Guy" (:hand (get-challenger))) :deck)
      (core/click-draw state :challenger nil)
      (is (= 3 (:credit (get-challenger)))
          "No credit gained from third click spent to draw with Gene Conditioning Shoppe"))))

(deftest synthetic-blood
  ;; Synthetic Blood - The first time you take damage each turn, draw one card
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
                (default-challenger [(qty "Synthetic Blood" 3)
                                 (qty "Sure Gamble" 3)
                                 "Fall Guy"]))
      (play-from-hand state :contestant "Data Mine" "HQ")
      (play-from-hand state :contestant "Data Mine" "HQ")
      (take-credits state :contestant)
      (let [first-dm (get-character state :hq 1)
            second-dm (get-character state :hq 0)]
        (play-from-hand state :challenger "Synthetic Blood")
        (run-on state "HQ")
        (core/rez state :contestant first-dm)
        (card-subroutine state :contestant first-dm 0)
        (is (= 4 (count (:hand (get-challenger)))) "1 card drawn when receiving damage (1st time)")
        (run-continue state)
        (core/rez state :contestant second-dm)
        (card-subroutine state :contestant second-dm 0)
        (is (= 3 (count (:hand (get-challenger)))) "no card drawn when receiving damage (2nd time)"))))
  (testing "The first and second time you take damage each turn (with GCS installed), draw one card"
    (do-game
      (new-game (default-contestant [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
                (default-challenger [(qty "Synthetic Blood" 3)
                                 "Sure Gamble"
                                 (qty "Gene Conditioning Shoppe" 3)]))
      (play-from-hand state :contestant "Data Mine" "HQ")
      (play-from-hand state :contestant "Data Mine" "HQ")
      (take-credits state :contestant)
      (let [first-dm (get-character state :hq 1)
            second-dm (get-character state :hq 0)]
        (play-from-hand state :challenger "Synthetic Blood")
        (play-from-hand state :challenger "Gene Conditioning Shoppe")
        (run-on state "HQ")
        (core/rez state :contestant first-dm)
        (card-subroutine state :contestant first-dm 0)
        (is (= 3 (count (:hand (get-challenger)))) "1 card drawn when receiving damage (1st time)")
        (run-continue state)
        (core/rez state :contestant second-dm)
        (card-subroutine state :contestant second-dm 0)
        (is (= 3 (count (:hand (get-challenger)))) "1 card drawn when receiving damage (2nd time)")))))

(deftest technical-writer
  ;; Technical Writer - Gain 1c per resource/hazard install; click/discard to take all credits
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Technical Writer" (qty "Faerie" 2)
                               "Vigil" "Same Old Thing"]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 2)
    (play-from-hand state :challenger "Technical Writer")
    (let [tw (get-radicle state 0)]
      (play-from-hand state :challenger "Faerie")
      (is (= 1 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :challenger "Faerie")
      (is (= 2 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :challenger "Vigil")
      (is (= 3 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :challenger "Same Old Thing")
      (is (= 3 (get-counters (refresh tw) :credit)) "No credit gained for radicle install")
      (card-ability state :challenger tw 0)
      (is (= 6 (:credit (get-challenger))) "Gained 3 credits")
      (is (zero? (:click (get-challenger))) "Spent 1 click")
      (is (= 1 (count (:discard (get-challenger)))) "Technical Writer discarded"))))

(deftest the-helpful-ai
  ;; The Helpful AI - +1 link; discard to give an characterbreaker +2 str until end of turn
  (do-game
    (new-game (default-contestant)
              (default-challenger ["The Helpful AI" "Corroder"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "The Helpful AI")
    (is (= 1 (:link (get-challenger))) "Gained 1 link")
    (play-from-hand state :challenger "Corroder")
    (let [corr (get-resource state 0)]
      (card-ability state :challenger (get-radicle state 0) 0)
      (prompt-select :challenger corr)
      (is (= 4 (:current-strength (refresh corr))) "Corroder has +2 strength")
      (is (= 1 (count (:discard (get-challenger)))) "Helpful AI discarded")
      (is (zero? (:link (get-challenger))))
      (take-credits state :challenger)
      (is (= 2 (:current-strength (refresh corr))) "Corroder back to default strength"))))

(deftest the-source
  ;; The Source - Increase advancement requirement of agendas by 1; 3c additional cost to steal
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 2)])
              (default-challenger [(qty "The Source" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "The Source")
    (run-empty-server state :remote1)
    (prompt-choice-partial :challenger "Pay") ; pay 3c extra to steal
    (is (= 4 (:credit (get-challenger))) "Paid 3c to steal")
    (is (= 2 (count (:discard (get-challenger)))) "The Source is discarded")
    (play-from-hand state :challenger "The Source")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote2 0)]
      (core/advance state :contestant {:card (refresh ht)})
      (core/advance state :contestant {:card (refresh ht)})
      (core/score state :contestant {:card (refresh ht)})
      (is (empty? (:scored (get-contestant))) "Hostile Takeover can't be scored with 2 adv")
      (core/gain state :contestant :click 1)
      (core/advance state :contestant {:card (refresh ht)})
      (core/score state :contestant {:card (refresh ht)})
      (is (= 1 (:agenda-point (get-contestant))) "Hostile Takeover scored with 3 adv")
      (is (= 3 (count (:discard (get-challenger)))) "The Source is discarded"))))

(deftest the-supplier
  ;; The Supplier
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["The Supplier"
                                 "Plascrete Carapace"
                                 "Utopia Shard"
                                 "Hedge Fund"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "The Supplier")
      (let [ts (get-radicle state 0)]
        (card-ability state :challenger ts 0)
        (prompt-select :challenger (find-card "Plascrete Carapace" (:hand (get-challenger))))
        (card-ability state :challenger ts 0)
        (is (= 1 (count (-> @state :challenger :prompt first :choices))))
        (prompt-select :challenger (find-card "Utopia Shard" (:hand (get-challenger))))
        (is (= 2 (count (:hosted (refresh ts)))) "The Supplier is hosting 2 cards")
        (take-credits state :challenger)
        (take-credits state :contestant)
        ;; Utopia Shard cannot be afforded and should not be in the prompt
        (prompt-select :challenger (find-card "Plascrete Carapace" (:hosted (refresh ts))))
        (is (= 2 (:credit (get-challenger)))
            "Challenger charged 1 credit to install Plascrete off The Supplier")
        (take-credits state :challenger)
        (is (= 6 (:credit (get-challenger))) "Challenger ends turn with 5 credits")
        (is (= 1 (count (:hosted (refresh ts)))) "One card still on The Supplier"))))
  (testing "Interaction with Kate discount. Issue #578."
    (do-game
      (new-game (default-contestant)
                (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                           ["The Supplier"
                            "Plascrete Carapace"
                            "Kati Jones"
                            "Hedge Fund"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "The Supplier")
      (let [ts (get-radicle state 0)]
        (card-ability state :challenger ts 0)
        (prompt-select :challenger (find-card "Plascrete Carapace" (:hand (get-challenger))))
        (core/lose state :challenger :credit (:credit (get-challenger)))
        (core/end-turn state :challenger nil)
        (take-credits state :contestant)
        (prompt-select :challenger (find-card "Plascrete Carapace" (:hosted (refresh ts))))
        (is (zero? (:credit (get-challenger))) "Kate discount applied")
        (is (= 1 (count (get-radicle state))) "Plascrete installed"))))
  (testing "Brain chip mem is deducted when it is hosted and Supplier is discarded. Issue #2358"
    (do-game
      (new-game (default-contestant [(qty "Hostile Takeover" 2)])
                (default-challenger ["The Supplier"
                                 "Brain Chip"]))
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (take-credits state :contestant)
      (is (= 4 (core/available-mu state)) "Challenger has 4 MU")
      (play-from-hand state :challenger "The Supplier")
      (let [ts (get-radicle state 0)]
        (card-ability state :challenger ts 0)
        (prompt-select :challenger (find-card "Brain Chip" (:hand (get-challenger))))
        (is (= 4 (core/available-mu state)) "Challenger has 4 MU")
        (run-empty-server state "Server 1")
        (prompt-choice :challenger "Steal")
        (take-credits state :challenger)
        (core/gain state :challenger :tag 1)
        (core/discard-radicle state :contestant nil)
        (prompt-select :contestant (get-radicle state 0))
        (is (= 2 (count (:discard (get-challenger)))))
        (is (= 4 (core/available-mu state)) "Challenger has 4 MU")))))

(deftest tech-trader
  ;; Basic test
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Tech Trader" "Fall Guy"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Tech Trader")
    (play-from-hand state :challenger "Fall Guy")
    (is (= 4 (:credit (get-challenger))))
    (let [fall (get-radicle state 1)]
      (card-ability state :challenger fall 1)
      (is (= 7 (:credit (get-challenger)))))))

(deftest the-archivist
  ;; The Archivist
  (do-game
    (new-game (default-contestant ["Global Food Initiative" "Private Security Force"])
              (default-challenger ["The Archivist"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "The Archivist")
    (is (zero? (:bad-publicity (get-contestant))) "Contestant should start with 0 bad publicity")
    (take-credits state :challenger)
    (play-and-score state "Global Food Initiative")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant should get 1 bad publicity from The Archivist")
    (play-and-score state "Private Security Force")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (= 2 (:bad-publicity (get-contestant))) "Contestant should get 1 bad publicity from The Archivist")))

(deftest the-black-file
  ;; The Black File - Prevent Contestant from winning by agenda points
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Vanity Project" 3) (qty "Sure Gamble" 3)])
                (default-challenger ["The Black File"]))
      (starting-hand state :contestant ["Vanity Project"])
      (core/gain state :contestant :agenda-point 3)
      (take-credits state :contestant)
      (play-from-hand state :challenger "The Black File")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Vanity Project" "New remote")
      (score-agenda state :contestant (get-content state :remote1 0))
      (is (= 7 (:agenda-point (get-contestant))))
      (is (not (:winner @state)) "No registered Contestant win")
      (take-credits state :contestant)
      (let [bf (get-radicle state 0)]
        (is (= 1 (get-counters (refresh bf) :power)) "1 power counter on The Black File")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 2 (get-counters (refresh bf) :power)) "2 power counters on The Black File")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (count (:rfg (get-challenger)))) "The Black File removed from the game")
        (is (= :contestant (:winner @state)) "Contestant wins")
        (is (= "Agenda" (:reason @state)) "Win condition reports agendas"))))
  (testing "Contestant can still win by flatlining Challenger"
    (do-game
      (new-game (default-contestant [(qty "Vanity Project" 3) (qty "Scorched Earth" 3)])
                (default-challenger ["The Black File"]))
      (starting-hand state :contestant ["Vanity Project" "Scorched Earth"])
      (core/gain state :contestant :agenda-point 3)
      (take-credits state :contestant)
      (play-from-hand state :challenger "The Black File")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Vanity Project" "New remote")
      (score-agenda state :contestant (get-content state :remote1 0))
      (is (= 7 (:agenda-point (get-contestant))))
      (is (not (:winner @state)) "No registered Contestant win")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "Scorched Earth")
      (is (= :contestant (:winner @state)) "Contestant wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest temujin-contract
  ;; Temüjin Contract
  (testing "Multiple times in one turn. Issue #1952"
    (do-game
      (new-game (default-contestant)
                (make-deck "Silhouette: Stealth Operative" ["Temüjin Contract"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Temüjin Contract")
      (prompt-choice :challenger "Archives")
      (run-empty-server state "Archives")
      (is (= 5 (:credit (get-challenger))) "Gained 4cr")
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-challenger))) "Gained 4cr")
      (is (= 12 (get-counters (get-radicle state 0) :credit)) "Temjin has 12 credits remaining"))))

(deftest the-turning-wheel
  ;; The Turning Wheel
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Hostile Takeover" "Ice Wall" "Ice Wall"])
                (default-challenger ["The Turning Wheel"]))
      (core/move state :contestant (find-card "Ice Wall" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Hostile Takeover" (:hand (get-contestant))) :deck)
      (take-credits state :contestant)
      (play-from-hand state :challenger "The Turning Wheel")
      (core/gain state :challenger :click 10 :credit 10)
      (let [ttw (get-radicle state 0)]
        (run-empty-server state "R&D")
        (prompt-choice :challenger "No action")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "R&D")
        (prompt-choice :challenger "No action")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "R&D")
        (card-ability state :challenger ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (is (= 1 (-> @state :run :access-bonus)) "Challenger should access 1 additional card"))))
  (testing "Access bonus shouldn't carry over to other runs if prematurely ended after spending TTW counters. #3598"
    (do-game
      (new-game (default-contestant ["Nisei MK II"])
                (default-challenger ["The Turning Wheel"]))
      (play-and-score state "Nisei MK II")
      (is (= 1 (get-counters (get-scored state :contestant 0) :agenda)))
      (take-credits state :contestant)
      (play-from-hand state :challenger "The Turning Wheel")
      (core/gain state :challenger :click 10 :credit 10)
      (let [nisei (get-scored state :contestant 0)
            ttw (get-radicle state 0)]
        (run-empty-server state "HQ")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "HQ")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "R&D")
        (card-ability state :challenger ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (is (= 1 (-> @state :run :access-bonus)) "Challenger should access 1 additional card")
        (card-ability state :contestant nisei 0)
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter from contestant using Nisei counter")
        (run-on state "R&D")
        (is (zero? (-> @ state :run :access-bonus)) "Access bonus should be reset on new run"))))
  (testing "Spending counters shouldn't increase accesses when running a non-R&D/HQ server"
    (do-game
      (new-game (default-contestant ["Hostile Takeover" "Ice Wall"])
                (default-challenger ["The Turning Wheel"]))
      (core/move state :contestant (find-card "Ice Wall" (:hand (get-contestant))) :deck)
      (discard-from-hand state :contestant "Hostile Takeover")
      (take-credits state :contestant)
      (play-from-hand state :challenger "The Turning Wheel")
      (core/gain state :challenger :click 10 :credit 10)
      (let [ttw (get-radicle state 0)]
        (run-empty-server state "R&D")
        (prompt-choice :challenger "No action")
        (is (= 1 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-empty-server state "R&D")
        (prompt-choice :challenger "No action")
        (is (= 2 (get-counters (refresh ttw) :power)) "The Turning Wheel should gain 1 counter")
        (run-on state "Archives")
        (card-ability state :challenger ttw 0)
        (is (zero? (get-counters (refresh ttw) :power)) "Using The Turning Wheel ability costs 2 counters")
        (is (= 1 (-> @state :run :access-bonus)) "Challenger should access 1 additional card")
        (run-successful state)
        (is (zero? (-> @state :run :access-bonus)) "Access bonuses are zeroed out when attacked server isn't R&D or HQ")))))

(deftest theophilius-bagbiter
  ;; Theophilius Bagbiter - hand size is equal to credit pool
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Theophilius Bagbiter"]))
    (take-credits state :contestant)
    (is (= 5 (:credit (get-challenger))) "Challenger starts with 5c")
    (play-from-hand state :challenger "Theophilius Bagbiter")
    (is (zero? (:credit (get-challenger))) "Challenger loses all credits on install")
    (is (= 1 (count (get-radicle state))) "Theophilius Bagbiter installed")
    (is (zero? (core/hand-size state :challenger)) "Max hand size is 0")
    (core/gain state :challenger :credit 7)
    (is (= 7 (:credit (get-challenger))) "Challenger has 7c")
    (is (= 7 (core/hand-size state :challenger)) "Max hand size is 7")
    (core/discard-radicle state :challenger nil)
    (prompt-select :challenger (get-radicle state 0))
    (is (= 1 (count (:discard (get-challenger)))) "Theo is discarded")
    (is (empty? (get-radicle state)) "No radicles installed")
    (is (= 5 (core/hand-size state :challenger)) "Max hand size is reset to default")))

(deftest tri-maf-contact
  ;; Tri-maf Contact - Click for 2c once per turn; take 3 meat dmg when discarded
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Tri-maf Contact" (qty "Cache" 3) "Shiv"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Tri-maf Contact")
    (let [tmc (get-radicle state 0)]
      (card-ability state :challenger tmc 0)
      (is (= 5 (:credit (get-challenger))) "Gained 2c")
      (is (= 2 (:click (get-challenger))) "Spent 1 click")
      (card-ability state :challenger tmc 0)
      (is (= 5 (:credit (get-challenger))) "No credits gained; already used this turn")
      (core/move state :challenger tmc :hand)
      (is (= 5 (count (:hand (get-challenger)))) "No meat damage")
      (play-from-hand state :challenger "Tri-maf Contact")
      (core/gain state :challenger :tag 1)
      (take-credits state :challenger)
      (core/discard-radicle state :contestant nil)
      (prompt-select :contestant (get-radicle state 0))
      (is (= 4 (count (:discard (get-challenger)))) "Took 3 meat damage"))))

(deftest virus-breeding-ground
  ;; Virus Breeding Ground - Gain counters
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Virus Breeding Ground"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Virus Breeding Ground")
      (let [vbg (get-radicle state 0)]
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :challenger 3)
        (take-credits state :contestant)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (take-credits state :challenger 3)
        (take-credits state :contestant)
        (is (= 2 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn"))))
  (testing "Can move to resources pumped by Hivemind"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Virus Breeding Ground" "Hivemind" "Aumakua"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Virus Breeding Ground")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Hivemind")
      (play-from-hand state :challenger "Aumakua")
      (let [aum (get-resource state 1)
            vbg (get-radicle state 0)]
        (is (zero? (get-counters aum :virus)) "Aumakua starts with 0 counters (excluding Hivemind)")
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :challenger vbg 0)
        (prompt-select :challenger aum)
        (is (= 1 (get-counters (refresh aum) :virus)) "Aumakua gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))
  (testing "Move counters"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Virus Breeding Ground" "Hivemind"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Virus Breeding Ground")
      (play-from-hand state :challenger "Hivemind")
      (let [hive (get-resource state 0)
            vbg (get-radicle state 0)]
        (is (= 1 (get-counters hive :virus)) "Hivemind starts with 1 counter")
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :challenger 3)
        (take-credits state :contestant)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :challenger vbg 0)
        (prompt-select :challenger hive)
        (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))
  (testing "Move counters to a non-virus radicle"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Virus Breeding Ground" "Crypt"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Virus Breeding Ground")
      (play-from-hand state :challenger "Crypt")
      (let [vbg (get-radicle state 0)
            crypt (get-radicle state 1)]
        (is (zero? (get-counters crypt :virus)) "Crypt starts with 0 counters")
        (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
        (card-ability state :challenger (refresh vbg) 0)
        (prompt-select :challenger (refresh crypt))
        (prompt-choice :challenger "Done")
        (is (zero? (get-counters (refresh crypt) :virus)) "Crypt doesn't gain a counter")
        (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground doesn't lose a counter")
        (run-on state "Archives")
        (run-successful state)
        (prompt-choice :challenger "Yes")
        (is (= 1 (get-counters (refresh crypt) :virus)) "Crypt gained a counter")
        (card-ability state :challenger (refresh vbg) 0)
        (prompt-select :challenger (refresh crypt))
        (is (= 2 (get-counters (refresh crypt) :virus)) "Crypt gained 1 counter")
        (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))

(deftest wasteland
  ;; Wasteland - Gain 1c the first time you discard an installed card of yours each turn
  (do-game
    (new-game (default-contestant ["PAD Campaign"])
              (default-challenger ["Wasteland" "Faust" (qty "Fall Guy" 4)]))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :click 2)
    (core/gain state :challenger :credit 4)
    (core/draw state :challenger)
    (play-from-hand state :challenger "Faust")
    (play-from-hand state :challenger "Wasteland")
    (is (= 4 (:credit (get-challenger))) "Challenger has 4 credits")
    (run-empty-server state "Server 1")
    (prompt-choice-partial :challenger "Pay") ; Discard PAD campaign
    (is (zero? (:credit (get-challenger))) "Gained nothing from Wasteland on contestant discard")
    ; discard from hand first which should not trigger #2291
    (let [faust (get-resource state 0)]
      (card-ability state :challenger faust 1)
      (prompt-card :challenger (first (:hand (get-challenger)))))
    (is (zero? (:credit (get-challenger))) "Gained nothing from Wasteland")
    (play-from-hand state :challenger "Fall Guy")
    (play-from-hand state :challenger "Fall Guy")
    (play-from-hand state :challenger "Fall Guy")
    (card-ability state :challenger (get-radicle state 1) 1)
    (is (= 1 (count (:discard (get-challenger)))) "Fall Guy discarded")
    (is (= 3 (:credit (get-challenger))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (take-credits state :challenger)
    (card-ability state :challenger (get-radicle state 1) 1)
    (is (= 2 (count (:discard (get-challenger)))) "Fall Guy discarded")
    (is (= 6 (:credit (get-challenger))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (card-ability state :challenger (get-radicle state 1) 1)
    (is (= 3 (count (:discard (get-challenger)))) "Fall Guy discarded")
    (is (= 8 (:credit (get-challenger))) "Gained 2c from Fall Guy but no credits from Wasteland")))

(deftest xanadu
  ;; Xanadu - Increase all Character rez cost by 1 credit
  (do-game
    (new-game (default-contestant [(qty "Paper Wall" 2) "Launch Campaign"])
              (default-challenger ["Xanadu"]))
    (play-from-hand state :contestant "Paper Wall" "HQ")
    (play-from-hand state :contestant "Paper Wall" "R&D")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Xanadu")
    (let [pw1 (get-character state :hq 0)
          pw2 (get-character state :rd 0)
          lc (get-content state :remote1 0)]
      (core/rez state :contestant pw1)
      (is (= 4 (:credit (get-contestant))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :contestant pw2)
      (is (= 3 (:credit (get-contestant))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :contestant lc)
      (is (= 2 (:credit (get-contestant))) "Paid 1 to rez Launch Campaign; no effect on non-Character"))))

(deftest zona-sul-shipping
  ;; Zona Sul Shipping - Gain 1c per turn, click to take all credits. Discard when tagged
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Zona Sul Shipping"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Zona Sul Shipping")
    (let [zss (get-radicle state 0)]
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 1 (get-counters (refresh zss) :credit)) "Zona Sul holds 1c")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 2 (get-counters (refresh zss) :credit)) "Zona Sul holds 2c")
      (card-ability state :challenger zss 0)
      (is (= 12 (:credit (get-challenger))) "Took 2c off Zona Sul")
      (is (= 3 (:click (get-challenger))) "Spent 1 click")
      (core/gain state :challenger :tag 1)
      (is (= 1 (count (:discard (get-challenger)))) "Zona Sul discarded when tag taken"))))
