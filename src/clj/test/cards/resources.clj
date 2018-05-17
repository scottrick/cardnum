(ns test.cards.resources
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest activist-support
  ;; Activist Support - Take tag if you have none; Contestant gains bad pub if they have none
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Activist Support" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Activist Support")
    (is (= 0 (:tag (get-challenger))))
    (take-credits state :challenger)
    (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag; had none")
    (is (= 0 (:bad-publicity (get-contestant))))
    (take-credits state :contestant)
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant took 1 bad pub; had none")
    (take-credits state :challenger)
    (is (= 1 (:tag (get-challenger))) "Challenger had 1 tag; didn't take another")
    (take-credits state :contestant)
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant had 1 bad pub; didn't take another")))

(deftest adjusted-chronotype
  ;; Ensure adjusted chronotype gains only 1 click when 2 clicks are lost
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Adjusted Chronotype" 1) (qty "Beach Party" 2)]))
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

(deftest adjusted-chronotype-mca
  ;; Chronotype to cancel out MCA click loss
  (do-game
    (new-game
      (default-contestant [(qty "MCA Austerity Policy" 1)])
      (default-challenger [(qty "Adjusted Chronotype" 1)]))
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

(deftest adjusted-chronotype-gcs
  ;; Ensure adjusted chronotype gains 2 clicks when 2 clicks are lost and GCS is installed
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Adjusted Chronotype" 1)
                              (qty "Beach Party" 3)
                              (qty "Gene Conditioning Shoppe" 1)]))
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
   (is (= 3 (:click (get-challenger))) "Should have lost 3 clicks and gained 2 clicks")))

(deftest aesops-pawnshop
  ;; Tests use cases for Aesop's Pawnshop
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Aesop's Pawnshop" 1) (qty "Cache" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Aesop's Pawnshop")
    (play-from-hand state :challenger "Cache")
    (let [orig-credits (:credit (get-challenger))
          ap (get-in @state [:challenger :rig :resource 0])
          cache (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger ap 0)
      (prompt-select :challenger cache)
      (card-ability state :challenger ap 0)
      (prompt-select :challenger ap)
      (let [ap (get-in @state [:challenger :rig :resource 0])
            cache (get-in @state [:challenger :discard 0])]
        (is (= (+ 3 orig-credits) (:credit (get-challenger))) "Should have only gained 3 credits")
        (is (not= cache nil) "Cache should be in Heap")
        (is (not= ap nil) "Aesops should still be installed")))))

(deftest all-nighter
  ;; All-nighter - Click/trash to gain 2 clicks
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "All-nighter" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "All-nighter")
    (is (= 3 (:click (get-challenger))))
    (card-ability state :challenger (get-resource state 0) 0)
    (is (= 4 (:click (get-challenger))) "Spent 1 click; gained 2 clicks")
    (is (= 1 (count (:discard (get-challenger)))) "All-nighter is trashed")))

(deftest bank-job-manhunt
  ;; Bank Job - Manhunt trace happens first
  (do-game
    (new-game (default-contestant [(qty "Manhunt" 1) (qty "PAD Campaign" 1)])
              (default-challenger [(qty "Bank Job" 1)]))
    (play-from-hand state :contestant "Manhunt")
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Bank Job")
    (run-empty-server state "Server 1")
    (prompt-chocharacter :contestant 2) ; Manhunt trace active
    (prompt-chocharacter :challenger 0)
    (prompt-chocharacter :challenger "Run ability")
    (is (= "Bank Job" (:title (:card (first (get-in @state [:challenger :prompt])))))
        "Bank Job prompt active")
    (prompt-chocharacter :challenger 8)
    (is (empty? (get-in @state [:challenger :rig :resource])) "Bank Job trashed after all credits taken")
    (is (= 1 (count (:discard (get-challenger)))))))

(deftest bank-job-multiple-copies
  ;; Bank Job - Choose which to use when 2+ copies are installed
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 1)])
              (default-challenger [(qty "Bank Job" 2)]))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Bank Job")
    (run-empty-server state "Server 1")
    (prompt-chocharacter :challenger "Run ability")
    (prompt-chocharacter :challenger 4)
    (play-from-hand state :challenger "Bank Job")
    (let [bj1 (get-resource state 0)
          bj2 (get-resource state 1)]
      (is (= 4 (get-counters (refresh bj1) :credit)) "4 credits remaining on 1st copy")
      (run-empty-server state "Server 1")
      (prompt-chocharacter :challenger "Run ability")
      (prompt-select :challenger bj2)
      (prompt-chocharacter :challenger 6)
      (is (= 13 (:credit (get-challenger))))
      (is (= 2 (get-counters (refresh bj2) :credit)) "2 credits remaining on 2nd copy"))))

(deftest bank-job-sectesting
  ;; Bank Job - Security Testing takes priority
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 1)])
              (default-challenger [(qty "Bank Job" 1) (qty "Security Testing" 1)]))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Security Testing")
    (play-from-hand state :challenger "Bank Job")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (prompt-chocharacter :challenger "Server 1")
    (is (= 6 (:credit (get-challenger))))
    (run-empty-server state "Server 1")
    (is (empty? (:prompt (get-challenger))) "No Bank Job replacement chocharacter")
    (is (= 8 (:credit (get-challenger))) "Security Testing paid 2c")))

(deftest bazaar-grip-only
  ;; Bazaar - Only triggers when installing from Grip
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Street Peddler" 1)
                               (qty "Bazaar" 1)
                               (qty "Spy Camera" 6)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler" "Bazaar" "Spy Camera" "Spy Camera" "Spy Camera"])
    (play-from-hand state :challenger "Bazaar")
    (play-from-hand state :challenger "Street Peddler")
    (let [peddler (get-resource state 1)]
      (card-ability state :challenger peddler 0)
      (prompt-card :challenger (first (:hosted peddler)))
      (is (empty? (:prompt (get-challenger))) "No Bazaar prompt from install off Peddler"))))

(deftest beach-party
  ;; Beach Party - Lose 1 click when turn begins; hand size increased by 5
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Beach Party" 1)]))
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
              (default-challenger [(qty "Bhagat" 1)]))
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
    (new-game (default-contestant [(qty "Traffic Accident" 1)])
              (default-challenger [(qty "Chrome Parlor" 1) (qty "Titanium Ribs" 1)
                               (qty "Brain Cage" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Chrome Parlor")
    (play-from-hand state :challenger "Titanium Ribs")
    (is (empty? (:prompt (get-challenger))) "Damage prevented, no Ribs prompt to choose cards")
    (is (= 3 (count (:hand (get-challenger)))))
    (play-from-hand state :challenger "Brain Cage")
    (is (= 2 (count (:hand (get-challenger)))) "No cards lost")
    (is (= 0 (:brain-damage (get-challenger))))
    (is (= 8 (core/hand-size state :challenger)) "Challenger hand size boosted by Brain Cage")
    (take-credits state :challenger)
    (core/gain state :challenger :tag 2)
    (core/trash state :challenger (get-hardware state 0))
    (play-from-hand state :contestant "Traffic Accident")
    (is (= 3 (count (:discard (get-challenger)))) "Conventional meat damage not prevented by Parlor")))

(deftest compromised-employee
  ;; Compromised Employee - Gain 1c every time Contestant rezzes Character
  (do-game
    (new-game (default-contestant [(qty "Pup" 2) (qty "Launch Campaign" 1)])
              (default-challenger [(qty "Compromised Employee" 1)]))
    (play-from-hand state :contestant "Pup" "HQ")
    (play-from-hand state :contestant "Pup" "R&D")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Compromised Employee")
    (let [ce (get-resource state 0)]
      (is (= 1 (:rec-counter (refresh ce))) "Has 1 recurring credit")
      (core/rez state :contestant (get-character state :hq 0))
      (is (= 4 (:credit (get-challenger))) "Gained 1c from Character rez")
      (core/rez state :contestant (get-character state :rd 0))
      (is (= 5 (:credit (get-challenger))) "Gained 1c from Character rez")
      (core/rez state :contestant (get-content state :remote1 0))
      (is (= 5 (:credit (get-challenger))) "Asset rezzed, no credit gained"))))

(deftest councilman
  ;; Councilman reverses the rezz and prevents re-rezz
  (do-game
    (new-game (default-contestant [(qty "Jackson Howard" 1)])
              (default-challenger [(qty "Councilman" 1)]))
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Councilman")
    (let [jesus (get-content state :remote1 0)
          judas (get-resource state 0)]
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

(deftest counter-surveillance
  ;; Trash to run, on successful run access cards equal to Tags and pay that amount in credits
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 3)])
              (default-challenger [(qty "Counter Surveillance" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :tag 2)
    (play-from-hand state :challenger "Counter Surveillance")
    (-> @state :challenger :credit (= 4) (is "Challenger has 4 credits"))
    (let [cs (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger cs 0)
      (prompt-chocharacter :challenger "HQ")
      (run-successful state)
      (-> (get-challenger) :register :successful-run (= [:hq]) is)
      (prompt-chocharacter :challenger "Card from hand")
      (-> (get-challenger) :prompt first :msg (= "You accessed Hedge Fund") is)
      (prompt-chocharacter :challenger "OK")
      (prompt-chocharacter :challenger "Card from hand")
      (-> (get-challenger) :prompt first :msg (= "You accessed Hedge Fund") is)
      (prompt-chocharacter :challenger "OK")
      (-> @state :challenger :discard count (= 1) (is "Counter Surveillance trashed"))
      (-> @state :challenger :credit (= 2) (is "Challenger has 2 credits")))))

(deftest counter-surveillance-obelus
  ;; Test Obelus does not trigger before Counter Surveillance accesses are done. Issues #2675
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 3)])
              (default-challenger [(qty "Counter Surveillance" 1) (qty "Obelus" 1) (qty "Sure Gamble" 3)]))
    (starting-hand state :challenger ["Counter Surveillance" "Obelus"])
    (take-credits state :contestant)
    (core/gain state :challenger :tag 2)
    (core/gain state :challenger :credit 2)
    (-> (get-challenger) :credit (= 7) (is "Challenger has 7 credits"))
    (play-from-hand state :challenger "Counter Surveillance")
    (play-from-hand state :challenger "Obelus")
    (-> (get-challenger) :credit (= 2) (is "Challenger has 2 credits")) ; Challenger has enough credits to pay for CS
    (let [cs (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger cs 0)
      (prompt-chocharacter :challenger "HQ")
      (run-successful state)
      (-> (get-challenger) :register :successful-run (= [:hq]) is)
      (-> (get-challenger) :hand count zero? (is "Challenger did not draw cards from Obelus yet"))
      (prompt-chocharacter :challenger "Card from hand")
      (-> (get-challenger) :prompt first :msg (= "You accessed Hedge Fund") is)
      (-> (get-challenger) :hand count zero? (is "Challenger did not draw cards from Obelus yet"))
      (prompt-chocharacter :challenger "OK")
      (prompt-chocharacter :challenger "Card from hand")
      (-> (get-challenger) :prompt first :msg (= "You accessed Hedge Fund") is)
      (prompt-chocharacter :challenger "OK")
      (-> (get-challenger) :hand count (= 2) (is "Challenger did draw cards from Obelus after all accesses are done"))
      (-> (get-challenger) :discard count (= 1) (is "Counter Surveillance trashed"))
      (-> (get-challenger) :credit (= 0) (is "Challenger has no credits")))))

(deftest-pending councilman-zone-change
  ;; Rezz no longer prevented when card changes zone (issues #1571)
  (do-game
    (new-game (default-contestant [(qty "Jackson Howard" 1)])
              (default-challenger [(qty "Councilman" 1)]))
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Councilman")
    (take-credits state :challenger)
    (let [jesus (get-content state :remote1 0)
          judas (get-resource state 0)]
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

(deftest daily-casts
  ;; Play and tick through all turns of daily casts
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Daily Casts" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Daily Casts")
    (let [dc (get-in @state [:challenger :rig :resource 0])]
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
      (is (nil? (get-in @state [:challenger :rig :resource 0]))))))

(deftest data-folding
  ;; Data Folding - Gain 1c at start of turn if 2+ unused MU
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Data Folding" 1) (qty "Hyperdriver" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Data Folding")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (is (= 4 (:memory (get-challenger))) "At least 2 unused MU")
    (is (= 6 (:credit (get-challenger))) "Gained 1c at turn start")
    (play-from-hand state :challenger "Hyperdriver")
    (take-credits state :challenger)
    (is (= 1 (:memory (get-challenger))) "Only 1 unused MU")
    (is (= 8 (:credit (get-challenger))))
    (take-credits state :contestant)
    (is (= 8 (:credit (get-challenger))) "No credits gained at turn start")))

(deftest ddos
  ;; Prevent rezzing of outermost character for the rest of the turn
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3)])
              (default-challenger [(qty "DDoS" 1)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "DDoS")
    (let [ddos (get-in @state [:challenger :rig :resource 0])
          iwall (get-character state :hq 1)]
      (card-ability state :challenger ddos 0)
      (is (= (:title ddos) (get-in @state [:challenger :discard 0 :title])))
      (run-on state "HQ")
      (core/rez state :contestant iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (run-jack-out state)
      (run-on state "HQ")
      (core/rez state :contestant iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (run-jack-out state)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-on state "HQ")
      (core/rez state :contestant iwall)
      (is (get-in (refresh iwall) [:rezzed])))))

(deftest decoy
  ;; Decoy - Trash to avoid 1 tag
  (do-game
    (new-game (default-contestant [(qty "SEA Source" 1)])
              (default-challenger [(qty "Decoy" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Decoy")
    (run-empty-server state :archives)
    (take-credits state :challenger)
    (play-from-hand state :contestant "SEA Source")
    (prompt-chocharacter :contestant 0)
    (prompt-chocharacter :challenger 0)
    (is (= 1 (count (:prompt (get-challenger)))) "Challenger prompted to avoid tag")
    (card-ability state :challenger (get-resource state 0) 0)
    (is (= 1 (count (:discard (get-challenger)))) "Decoy trashed")
    (is (= 0 (:tag (get-challenger))) "Tag avoided")))

(deftest donut-taganes
  ;; Donut Taganes - add 1 to play cost of Operations & Events when this is in play
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Donut Taganes" 1) (qty "Easy Mark" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Donut Taganes")
    (is (= 2 (:credit (get-challenger))) "Donut played for 3c")
    (play-from-hand state :challenger "Easy Mark")
    (is (= 4 (:credit (get-challenger))) "Easy Mark only gained 2c")
    (take-credits state :challenger)
    (is (= 8 (:credit (get-contestant))) "Contestant has 8c")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 11 (:credit (get-contestant))) "Contestant has 11c")))

(deftest eden-shard
  ;; Eden Shard - Install from Grip in lieu of accessing R&D; trash to make Contestant draw 2
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Eden Shard" 1)]))
    (starting-hand state :contestant ["Hedge Fund"])
    (take-credits state :contestant)
    (is (= 1 (count (:hand (get-contestant)))))
    (run-on state :rd)
    (core/no-action state :contestant nil)
    (play-from-hand state :challenger "Eden Shard")
    (is (= 5 (:credit (get-challenger))) "Eden Shard installed for 0c")
    (is (not (:run @state)) "Run is over")
    (card-ability state :challenger (get-resource state 0) 0)
    (is (= 3 (count (:hand (get-contestant)))) "Contestant drew 2 cards")
    (is (= 1 (count (:discard (get-challenger)))) "Eden Shard trashed")))

(deftest eden-shard-no-install-on-access
  ;; Eden Shard - Do not install when accessing cards
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Eden Shard" 1)]))
    (starting-hand state :contestant ["Hedge Fund"])
    (take-credits state :contestant)
    (is (= 1 (count (:hand (get-contestant)))))
    (run-empty-server state :rd)
    (play-from-hand state :challenger "Eden Shard")
    (is (not (get-resource state 0)) "Eden Shard not installed")
    (is (= 1 (count (:hand (get-challenger)))) "Eden Shard not installed")))

(deftest fan-site
  ;; Fan Site - Add to score area as 0 points when Contestant scores an agenda
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 1)])
              (default-challenger [(qty "Fan Site" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Fan Site")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (is (= 0 (:agenda-point (get-challenger))))
    (is (= 1 (count (:scored (get-challenger)))) "Fan Site added to Challenger score area")))

(deftest fan-site-eoi
  ;; Fan Site - Don't trigger after swap with Exchange of Information. Issue #1824
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 2) (qty "Exchange of Information" 1)])
              (default-challenger [(qty "Fan Site" 1)]))
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
    (is (= 0 (:agenda-point (get-contestant))))

    (is (find-card "Fan Site" (:scored (get-contestant))) "Fan Site swapped into Contestant score area")
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (score-agenda state :contestant (get-content state :remote2 0))
    (is (find-card "Fan Site" (:scored (get-contestant))) "Fan Site not removed from Contestant score area")))

(deftest fan-site-forfeit
  ;; Fan Site - Challenger can forfeit Fan Site
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 1)])
              (default-challenger [(qty "Fan Site" 1) (qty "Data Dealer" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Fan Site")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (is (= 0 (:agenda-point (get-challenger))))
    (is (= 1 (count (:scored (get-challenger)))) "Fan Site added to Challenger score area")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Data Dealer")
    (let [credits (:credit (get-challenger))]
      (card-ability state :challenger (get-resource state 0) 0)
      (prompt-select :challenger (get-scored state :challenger 0))
      (is (= 0 (count (:scored (get-challenger)))) "Fan Site successfully forfeit to Data Dealer")
      (is (= (+ credits 9) (:credit (get-challenger))) "Gained 9 credits from Data Dealer"))))

(deftest fester
  ;; Fester - Contestant loses 2c (if able) when purging viruses
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Fester" 1)]))
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

(deftest film-critic-discarded-executives
  ;; Film Critic - Prevent Contestant-trashed execs going to Challenger scored. Issues #1181/#1042
  (do-game
    (new-game (default-contestant [(qty "Director Haas" 3) (qty "Project Vitruvius" 3) (qty "Hedge Fund" 1)])
              (default-challenger [(qty "Film Critic" 1)]))
    (play-from-hand state :contestant "Project Vitruvius" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Film Critic")
    (let [fc (first (get-in @state [:challenger :rig :resource]))]
      (run-empty-server state "Server 1")
      (card-ability state :challenger fc 0)
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (take-credits state :challenger)
      (trash-from-hand state :contestant "Director Haas")
      (is (= 1 (count (:discard (get-contestant)))) "Director Haas stayed in Archives")
      (is (= 0 (:agenda-point (get-challenger))) "No points gained by Challenger")
      (is (empty? (:scored (get-challenger))) "Nothing in Challenger scored"))))

(deftest film-critic-fetal-ai
  ;; Film Critic - Fetal AI interaction
  (do-game
    (new-game (default-contestant [(qty "Fetal AI" 3)])
              (default-challenger [(qty "Film Critic" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Film Critic")
    (let [fc (first (get-in @state [:challenger :rig :resource]))]
      (run-empty-server state "HQ")
      ;; should not have taken damage yet
      (is (= 3 (count (:hand (get-challenger)))) "No damage dealt yet")
      (card-ability state :challenger fc 0)
      (is (= 3 (count (:hand (get-challenger)))) "No damage dealt")
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (card-ability state :challenger fc 1)
      (is (= 1 (count (:scored (get-challenger)))) "Agenda added to challenger scored")
      (is (= 3 (count (:hand (get-challenger)))) "No damage dealt"))))

(deftest film-critic-hostile-infrastructure
  ;; Do not take a net damage when a hosted agenda is trashed due to film critic trash #2382
  (do-game
    (new-game (default-contestant [(qty "Hostile Infrastructure" 3) (qty "Project Vitruvius" 1)])
              (default-challenger [(qty "Film Critic" 1) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
    (play-from-hand state :contestant "Project Vitruvius" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Film Critic")
    (let [fc (first (get-in @state [:challenger :rig :resource]))]
      (run-empty-server state :remote2)
      (card-ability state :challenger fc 0)
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (take-credits state :challenger)
      (core/gain state :contestant :credit 10)
      (core/trash-resource state :contestant nil)
      (prompt-select :contestant fc)
      (is (= 1 (count (:discard (get-challenger)))) "FC trashed")
      (is (= 1 (count (:discard (get-contestant)))) "Agenda trashed")
      (is (= 3 (count (:hand (get-challenger)))) "No damage dealt"))))

(deftest gang-sign
  ;; Gang Sign - accessing from HQ, not including root. Issue #2113.
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 3) (qty "Braintrust" 2) (qty "Crisium Grid" 1)])
              (default-challenger [(qty "Gang Sign" 2) (qty "HQ Interface" 1)]))
    (play-from-hand state :contestant "Crisium Grid" "HQ")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 100)
    (play-from-hand state :challenger "Gang Sign")
    (play-from-hand state :challenger "Gang Sign")
    (play-from-hand state :challenger "HQ Interface")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (prompt-chocharacter :challenger "Gang Sign") ; simultaneous effect resolution
    (let [gs1 (-> (get-challenger) :prompt first)]
      (is (= (:mutherfucker gs1) ["Card from hand"]) "Gang Sign does not let Challenger access upgrade in HQ root")
      (prompt-chocharacter :challenger "Card from hand")
      (prompt-chocharacter :challenger "Steal")
      (is (= (:card gs1) (-> (get-challenger) :prompt first :card)) "Second access from first Gang Sign triggered")
      (prompt-chocharacter :challenger "Card from hand")
      (prompt-chocharacter :challenger "Steal")
      (is (not= (:card gs1) (-> (get-challenger) :prompt first :card)) "First access from second Gang Sign triggered")
      (prompt-chocharacter :challenger "Card from hand")
      (prompt-chocharacter :challenger "Steal")
      (prompt-chocharacter :challenger "Card from hand")
      (prompt-chocharacter :challenger "Steal"))))

(deftest gene-conditioning-shoppe
  ;; Gene Conditioning Shoppe - set :genetics-trigger-twcharacter flag
  (do-game
   (new-game (default-contestant [(qty "Hedge Fund" 3)])
             (default-challenger [(qty "Gene Conditioning Shoppe" 1)
                              (qty "Adjusted Chronotype" 1)]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Adjusted Chronotype")
   (is (not (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter)))
   (play-from-hand state :challenger "Gene Conditioning Shoppe")
   (is (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter))
   (core/trash state :challenger (get-in @state [:challenger :rig :resource 1]))
   (is (not (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter)))))

(deftest gene-conditioning-shoppe-redundancy
  ;; Gene Conditioning Shoppe - set :genetics-trigger-twcharacter flag - ensure redundant copies work
  (do-game
   (new-game (default-contestant [(qty "Hedge Fund" 3)])
             (default-challenger [(qty "Gene Conditioning Shoppe" 2)
                              (qty "Adjusted Chronotype" 1)]))
   (take-credits state :contestant)
   (take-credits state :challenger)
   (take-credits state :contestant)
   (play-from-hand state :challenger "Adjusted Chronotype")
   (let [adjusted-chronotype (get-in @state [:challenger :rig :resource 0])]
     (is (not (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter)))
     (play-from-hand state :challenger "Gene Conditioning Shoppe")
     (play-from-hand state :challenger "Gene Conditioning Shoppe")
     (let [gcs1 (get-in @state [:challenger :rig :resource 1])
           gcs2 (get-in @state [:challenger :rig :resource 2])]
       (is (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter))
       (core/trash state :challenger gcs1)
       (is (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter))
       (core/trash state :challenger gcs2)
       (is (not (core/has-flag? state :challenger :persistent :genetics-trigger-twcharacter)))))))

(deftest globalsec-security-clearance
  ;; Globalsec Security Clearance - Ability, click lost on use
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Globalsec Security Clearance" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :link 2)
    (play-from-hand state :challenger "Globalsec Security Clearance")
    (take-credits state :challenger)
    (starting-hand state :contestant ["Hedge Fund"]) ; Hedge Fund on top
    (take-credits state :contestant)
    (is (:challenger-phase-12 @state) "Challenger in Step 1.2")
    (let [gsec (-> (get-challenger) :rig :resource first)]
      (card-ability state :challenger gsec 0)
      (is (pos? (.indexOf (-> (get-challenger) :prompt first :msg) "Hedge Fund")) "GSec revealed Hedge Fund")
      (core/end-phase-12 state :challenger nil)
      (is (= 3 (:click (get-challenger))) "Challenger lost 1 click from Globalsec Security Clearance"))))

(deftest grifter
  ;; Grifter - Gain 1c if you made a successful run this turn, otherwise trash it
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Grifter" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Grifter")
    (run-empty-server state :hq)
    (take-credits state :challenger)
    (is (= 6 (:credit (get-challenger))) "Gained 1c for a successful run during the turn")
    (take-credits state :contestant)
    (run-on state :hq)
    (run-jack-out state)
    (take-credits state :challenger)
    (is (= 1 (count (:discard (get-challenger)))) "No successful runs; Grifter is trashed")))

(deftest hard-at-work
  ;; Hard at Work - Gain 2c and lose 1 click when turn begins
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Hard at Work" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Hard at Work")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (is (= 5 (:credit (get-challenger))) "Gained 2c")
    (is (= 3 (:click (get-challenger))) "Lost 1 click")))

(deftest character-carver
  ;; Ice Carver - lower character strength on encounter
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 1)])
              (default-challenger [(qty "Ice Carver" 1)]))
    (play-from-hand state :contestant "Ice Wall" "Archives")
    (take-credits state :contestant 2)
    (let [iwall (get-character state :archives 0)]
      (core/rez state :contestant iwall)
      (play-from-hand state :challenger "Ice Carver")
      (run-on state "Archives")
      (is (= 0 (:current-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh iwall))) "Ice Wall strength at 1 after encounter"))))

(deftest investigative-journalism
  ;; Investigative Journalism - 4 clicks and trash to give the Contestant 1 bad pub
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Investigative Journalism" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Investigative Journalism")
    (is (empty? (get-in @state [:challenger :rig :resource])) "Contestant has no bad pub, couldn't install")
    (core/gain state :contestant :bad-publicity 1)
    (play-from-hand state :challenger "Investigative Journalism")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (card-ability state :challenger (get-resource state 0) 0)
    (is (= 0 (:click (get-challenger))) "Spent 4 clicks")
    (is (= 1 (count (:discard (get-challenger)))) "IJ is trashed")
    (is (= 2 (:bad-publicity (get-contestant))) "Contestant took 1 bad publicity")))

(deftest john-masanori
  ;; John Masanori - Draw 1 card on first successful run, take 1 tag on first unsuccessful run
  (do-game
    (new-game (default-contestant [(qty "Crisium Grid" 1)])
              (default-challenger [(qty "John Masanori" 3)
                               (qty "Sure Gamble" 3)
                               (qty "Fall Guy" 1)]))
    (play-from-hand state :contestant "Crisium Grid" "HQ")
    (core/rez state :contestant (get-content state :hq 0))
    (take-credits state :contestant)
    (core/gain state :challenger :click 2)
    (play-from-hand state :challenger "John Masanori")
    (is (= 4 (count (:hand (get-challenger)))))
    (run-empty-server state "HQ")
    (prompt-chocharacter :challenger "Yes") ; trash crisium #2433
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

(deftest joshua-b
  ;; Joshua B. - Take 1 tag at turn end if you choose to gain the extra click
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Joshua B." 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Joshua B.")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (is (= 0 (:click (get-challenger))) "Challenger has 0 clicks")
    (is (:challenger-phase-12 @state) "Challenger is in Step 1.2")
    (card-ability state :challenger (get-in @state [:challenger :rig :resource 0]) 0)
    (is (= 1 (:click (get-challenger))) "Gained extra click from Joshua")
    (core/end-phase-12 state :challenger nil)
    (is (= 5 (:click (get-challenger))) "Gained normal clicks as well")
    (take-credits state :challenger)
    (is (= 1 (:tag (get-challenger))) "Took 1 tag")))

(deftest kati-jones
  ;; Kati Jones - Click to store and take
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Kati Jones" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Kati Jones")
    (is (= 3 (:credit (get-challenger))))
    (let [kati (get-in @state [:challenger :rig :resource 0])]
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
  ;; Lewi Guilherme - lower contestant hand size by 1, pay 1 credit when turn begins or trash
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Lewi Guilherme" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Lewi Guilherme")
    (is (= -1 (:hand-size-modification (get-contestant))) "Contestant hand size reduced by 1")
    (take-credits state :challenger)
    (core/lose state :challenger :credit 6)
    (is (= 2 (:credit (get-challenger))) "Credits are 2")
    (take-credits state :contestant)
    (prompt-chocharacter :challenger "Yes")
    (is (= 1 (:credit (get-challenger))) "Lost a credit from Lewi")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (prompt-chocharacter :challenger "No")
    (is (= 1 (count (:discard (get-challenger)))) "First Lewi trashed")
    (is (= 0 (:hand-size-modification (get-contestant))) "Contestant hand size normal again")
    (play-from-hand state :challenger "Lewi Guilherme")
    (take-credits state :challenger)
    (core/lose state :challenger :credit 8)
    (is (= 0 (:credit (get-challenger))) "Credits are 0")
    (take-credits state :contestant)
    (prompt-chocharacter :challenger "Yes")
    (is (= 2 (count (:discard (get-challenger)))) "Second Lewi trashed due to no credits")))

(deftest london-library
  ;; Install non-virus programs on London library. Includes #325/409
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "London Library" 1) (qty "Darwin" 1) (qty "Study Guide" 1)
                                              (qty "Chameleon" 1) (qty "Femme Fatale" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 2)
    (play-from-hand state :challenger "London Library")
    (let [lib (get-in @state [:challenger :rig :resource 0])]
      (is (= 0 (count (:hosted (refresh lib)))) "0 programs hosted")
      (card-ability state :challenger lib 0) ; Install a non-virus program on London Library
      (prompt-select :challenger (find-card "Femme Fatale" (:hand (get-challenger))))
      (prompt-chocharacter :challenger "Done") ; Cancel out of Femme's bypass
      (is (= 1 (count (:hosted (refresh lib)))) "1 program hosted")
      (card-ability state :challenger lib 0)
      (prompt-select :challenger (find-card "Study Guide" (:hand (get-challenger))))
      (is (= 2 (count (:hosted (refresh lib)))) "2 programs hosted")
      (let [sg (second (:hosted (refresh lib)))]
        (is (= 0 (:current-strength (refresh sg))) "Study Guide at 0 strength")
        (card-ability state :challenger sg 1) ; Place 1 power counter
        (is (= 1 (:current-strength (refresh sg))) "Study Guide at 1 strength"))
      (card-ability state :challenger lib 0)
      (prompt-select :challenger (find-card "Chameleon" (:hand (get-challenger))))
      (prompt-chocharacter :challenger "Sentry")
      (is (= 3 (count (:hosted (refresh lib)))) "3 programs hosted")
      (is (= 2 (:click (get-challenger))) "At 2 clicks")
      (card-ability state :challenger lib 0)
      (prompt-select :challenger (find-card "Darwin" (:hand (get-challenger)))) ; Darwin is a virus
      (is (= 3 (count (:hosted (refresh lib)))) "Still 3 programs hosted")
      (is (= 2 (:click (get-challenger))) "Failed Darwin didn't use a click")
      (is (= 1 (count (:hand (get-challenger)))))
      (card-ability state :challenger lib 1) ; Add a program hosted on London Library to your Grip
      (prompt-card :challenger nil)
      (prompt-select :challenger (find-card "Study Guide" (:hosted (refresh lib))))
      (is (= 2 (count (:hand (get-challenger)))) "Return Study Guide to hand")
      (is (= 2 (count (:hosted (refresh lib)))) "2 programs hosted")
      (card-ability state :challenger lib 0)
      (prompt-select :challenger (find-card "Study Guide" (:hand (get-challenger))))
      (is (= 3 (count (:hosted (refresh lib)))) "3 programs hosted")
      (is (= 0 (count (:discard (get-challenger)))) "Nothing in archives yet")
      (take-credits state :challenger)
      (is (= 0 (count (:hosted (refresh lib)))) "All programs trashed when turn ends")
      (is (= 2 (count (:hand (get-challenger)))) "Darwin never got played, Chameleon returned to hand")
      (is (= 2 (count (:discard (get-challenger)))) "Femme Fatale and Study Guide trashed"))))

(deftest muertos-trashed
  ;; Muertos Gang Member - Install and Trash
  (do-game
    (new-game (default-contestant [(qty "Tollbooth" 1) (qty "Ice Wall" 1)])
              (default-challenger [(qty "Hedge Fund" 3) (qty "Muertos Gang Member" 1)]))
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
      (let [muer (get-in @state [:challenger :rig :resource 0])]
        (card-ability state :challenger muer 0)
        (is (= 3 (count (:hand (get-challenger)))) "Challenger drew a card from Muertos")
        (prompt-select :contestant toll)
        (is (:rezzed (refresh toll)) "Tollbooth was rezzed")))))

(deftest muertos-reina
  ;; Muertos Gang Member - Account for Reina interaction, #1098.
  (do-game
    (new-game (default-contestant [(qty "Tollbooth" 1) (qty "Ice Wall" 1)])
              (make-deck "Reina Roja: Freedom Fighter" [(qty "Hedge Fund" 3)
                                                        (qty "Muertos Gang Member" 1)]))
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
      (let [muer (get-in @state [:challenger :rig :resource 0])]
        (card-ability state :challenger muer 0)
        (is (= 3 (count (:hand (get-challenger)))) "Challenger drew a card from Muertos")
        (prompt-select :contestant toll)
        (is (:rezzed (refresh toll)) "Tollbooth was rezzed")
        (is (= 0 (:credit (get-contestant))) "Contestant has 0 credits")))))

(deftest net-mercur
  ;; Net Mercur - Gains 1 credit or draw 1 card when a stealth credit is used
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Net Mercur" 1) (qty "Silencer" 1) (qty "Ghost Challenger" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 4 :credit 10)
    (play-from-hand state :challenger "Silencer")
    (play-from-hand state :challenger "Net Mercur")
    (play-from-hand state :challenger "Ghost Challenger")
    (let [sil (get-hardware state 0)
          nm (get-resource state 0)
          gr (get-resource state 1)]
      (card-ability state :challenger gr 0)
      (is (empty? (:prompt (get-challenger))) "No Net Mercur prompt from stealth spent outside of run")
      (run-on state :hq)
      (card-ability state :challenger sil 0)
      (prompt-chocharacter :challenger "Place 1 [Credits]")
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
  (do-game
    (new-game (default-contestant [(qty "Paper Wall" 3)])
              (default-challenger [(qty "Network Exchange" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Network Exchange")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Paper Wall" "HQ")
    (is (= 8 (:credit (get-contestant))) "Paid 0 to install Paper Wall")
    (play-from-hand state :contestant "Paper Wall" "HQ")
    (is (= 6 (:credit (get-contestant))) "Paid 1 extra  to install Paper Wall")
    (play-from-hand state :contestant "Paper Wall" "HQ")
    (is (= 3 (:credit (get-contestant))) "Paid 1 extra  to install Paper Wall")))

(deftest network-exchange-architect
  ;; Architect 1st sub should ignore additional install cose
  (do-game
    (new-game (default-contestant [(qty "Architect" 3)])
              (default-challenger [(qty "Network Exchange" 1)]))
    (play-from-hand state :contestant "Architect" "HQ")
    (take-credits state :contestant) ; contestant has 7 credits
    (play-from-hand state :challenger "Network Exchange")
    (take-credits state :challenger)
    (let [architect (get-character state :hq 0)]
      (core/rez state :contestant architect)
      (is (= 3 (:credit (get-contestant))) "Contestant has 3 credits after rez")
      (core/move state :contestant (find-card "Architect" (:hand (get-contestant))) :deck)
      (card-subroutine state :contestant architect 0)
      (prompt-chocharacter :contestant (find-card "Architect" (:deck (get-contestant))))
      (prompt-chocharacter :contestant "HQ")
      (is (= 3 (:credit (get-contestant))) "Contestant has 7 credits"))))

(deftest neutralize-all-threats
  ;; Neutralize All Threats - Access 2 cards from HQ, force trash first accessed card with a trash cost
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 2) (qty "Breaker Bay Grid" 1) (qty "Elizabeth Mills" 1)])
              (default-challenger [(qty "Neutralize All Threats" 1)]))
    (play-from-hand state :contestant "Breaker Bay Grid" "New remote")
    (play-from-hand state :contestant "Elizabeth Mills" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Neutralize All Threats")
    (run-empty-server state "HQ")
    (prompt-chocharacter :challenger "Card from hand")
    (prompt-chocharacter :challenger "OK") ; access first Hedge Fund
    (prompt-chocharacter :challenger "Card from hand")
    (prompt-chocharacter :challenger "OK") ; access second Hedge Fund
    (run-empty-server state "Server 1")
    (is (= 3 (:credit (get-challenger))) "Forced to pay 2c to trash BBG")
    (is (= 1 (count (:discard (get-contestant)))) "Breaker Bay Grid trashed")
    (run-empty-server state "Server 2")
    (is (not (empty? (:prompt (get-challenger)))) "Challenger prompt to trash Elizabeth Mills")))

(deftest new-angeles-city-hall
  ;; New Angeles City Hall - Avoid tags; trash when agenda is stolen
  (do-game
    (new-game (default-contestant [(qty "SEA Source" 1) (qty "Breaking News" 1)])
              (default-challenger [(qty "New Angeles City Hall" 1)]))
    (play-from-hand state :contestant "Breaking News" "New remote")
    (take-credits state :contestant 2)
    (play-from-hand state :challenger "New Angeles City Hall")
    (let [nach (get-in @state [:challenger :rig :resource 0])]
      (run-empty-server state "Archives")
      (take-credits state :challenger)
      (is (= 6 (:credit (get-challenger))))
      (play-from-hand state :contestant "SEA Source")
      (prompt-chocharacter :contestant 0) ; default trace
      (prompt-chocharacter :challenger 0) ; Challenger won't match
      (card-ability state :challenger nach 0)
      (prompt-chocharacter :challenger "Done")
      (is (= 0 (:tag (get-challenger))) "Avoided SEA Source tag")
      (is (= 4 (:credit (get-challenger))) "Paid 2 credits")
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-chocharacter :challenger "Steal")
      (is (= 1 (:agenda-point (get-challenger))))
      (is (empty? (get-in @state [:challenger :rig :resource])) "NACH trashed by agenda steal"))))

(deftest new-angeles-city-hall-siphon
  ;; New Angeles City Hall - don't gain Siphon credits until opportunity to avoid tags has passed
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Account Siphon" 1) (qty "New Angeles City Hall" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "New Angeles City Hall")
    (play-run-event state (first (:hand (get-challenger))) :hq)
    (prompt-chocharacter :challenger "Run ability")
    (let [nach (get-in @state [:challenger :rig :resource 0])]
      (is (= 4 (:credit (get-challenger))) "Have not gained Account Siphon credits until tag avoidance window closes")
      (card-ability state :challenger nach 0)
      (card-ability state :challenger nach 0)
      (prompt-chocharacter :challenger "Done")
      (is (= 0 (:tag (get-challenger))) "Tags avoided")
      (is (= 10 (:credit (get-challenger))) "10 credits siphoned")
      (is (= 3 (:credit (get-contestant))) "Contestant lost 5 credits"))))

(deftest off-campus-apartment-simultaneous
  ;; Off-Campus Apartment - ability shows a simultaneous resolution prompt when appropriate
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Street Peddler" 1) (qty "Off-Campus Apartment" 1)
                               (qty "Underworld Contact" 1) (qty "Spy Camera" 6)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler" "Off-Campus Apartment" "Underworld Contact"])
    (play-from-hand state :challenger "Off-Campus Apartment")
    (let [oca (get-resource state 0)]
      (card-ability state :challenger oca 0)
      (prompt-select :challenger (find-card "Underworld Contact" (:hand (get-challenger))))
      (is (= 2 (count (:hand (get-challenger)))) "Drew a card from OCA")
      (card-ability state :challenger oca 0)
      (prompt-select :challenger (find-card "Street Peddler" (:hand (get-challenger))))
      ;; Make sure the simultaneous-resolution prompt is showing with 2 mutherfucker
      (is (= 2 (-> (get-challenger) :prompt first :mutherfucker count)) "Simultaneous-resolution prompt is showing")
      (prompt-chocharacter :challenger "Off-Campus Apartment")
      (is (= 2 (count (:hand (get-challenger)))) "Drew a card from OCA"))))

(deftest off-campus-peddler
  ;; Off-Campus Apartment - second ability does not break cards that are hosting others, e.g., Street Peddler
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Street Peddler" 2) (qty "Off-Campus Apartment" 1) (qty "Spy Camera" 6)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler" "Street Peddler" "Off-Campus Apartment"])
    (core/move state :challenger (find-card "Street Peddler" (:hand (get-challenger))) :deck {:front true})
    (play-from-hand state :challenger "Off-Campus Apartment")
    (let [oca (get-resource state 0)]
      (card-ability state :challenger oca 0)
      (prompt-select :challenger (find-card "Street Peddler" (:hand (get-challenger))))
      (prompt-chocharacter :challenger "Street Peddler")
      (let [ped1 (first (:hosted (refresh oca)))]
        (card-ability state :challenger ped1 0)
        (prompt-card :challenger (-> (get-challenger) :prompt first :mutherfucker second)) ; choose Street Peddler
        (card-ability state :challenger (refresh oca) 1)
        (prompt-select :challenger (get-resource state 1))
        (let [ped2 (first (:hosted (refresh oca)))]
          (card-ability state :challenger ped2 0)
          (prompt-card :challenger (-> (get-challenger) :prompt first :mutherfucker first)) ; choose Spy Camera
          ;; the fact that we got this far means the bug is fixed
          (is (= 1 (count (get-hardware state))) "Spy Camera installed"))))))

(deftest offcharacterr-frank
  ;; Offcharacterr Frank - meat damage to trash 2 from HQ
  (do-game
      (new-game (default-contestant [(qty "Swordsman" 1) (qty "Hedge Fund" 2)])
                (default-challenger [(qty "Offcharacterr Frank" 1) (qty "Skulljack" 1) (qty "Respirocytes" 4)]))
   (play-from-hand state :contestant "Swordsman" "Archives")
   (take-credits state :contestant)
   (starting-hand state :challenger ["Offcharacterr Frank" "Skulljack" "Respirocytes" "Respirocytes" "Respirocytes" "Respirocytes"])
   (play-from-hand state :challenger "Offcharacterr Frank")
   (card-ability state :challenger (get-resource state 0) 0)
   (is (= 0 (count (:discard (get-contestant)))) "Nothing discarded from HQ")
   (play-from-hand state :challenger "Skulljack")
   (is (= 3 (count (:hand (get-challenger)))) "Took 1 brain damage")
   (card-ability state :challenger (get-resource state 0) 0)
   (is (= 0 (count (:discard (get-contestant)))) "Nothing discarded from HQ")
   (let [sm (get-character state :archives 0)]
     (run-on state :archives)
     (core/rez state :contestant sm)
     (card-subroutine state :contestant sm 0)
     (run-jack-out state))
   (is (= 2 (count (:hand (get-challenger)))) "Took 1 net damage")
   (card-ability state :challenger (get-resource state 0) 0)
   (is (= 0 (count (:discard (get-contestant)))) "Nothing discarded from HQ")
   (play-from-hand state :challenger "Respirocytes")
   (is (= 0 (count (:hand (get-challenger)))) "Took 1 meat damage")
   (card-ability state :challenger (get-resource state 0) 0)
   (is (= 2 (count (:discard (get-contestant)))) "Two cards trashed from HQ")))

(deftest paige-piper-frantic-coding
  ;; Paige Piper - interaction with Frantic Coding. Issue #2190.
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Paige Piper" 1) (qty "Frantic Coding" 2) (qty "Sure Gamble" 3)
                               (qty "Gordian Blade" 2) (qty "Ninja" 1) (qty "Bank Job" 3) (qty "Indexing" 2)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Paige Piper" "Frantic Coding" "Frantic Coding"])
    (play-from-hand state :challenger "Paige Piper")
    (prompt-chocharacter :challenger "No")
    (take-credits state :challenger) ; now 8 credits
    (take-credits state :contestant)
    (play-from-hand state :challenger "Frantic Coding")
    (prompt-chocharacter :challenger "OK")
    (prompt-card :challenger (find-card "Gordian Blade" (:deck (get-challenger))))
    (is (= 1 (count (get-program state))) "Installed Gordian Blade")
    (prompt-chocharacter :challenger "Yes")
    (prompt-chocharacter :challenger "0")
    (is (= 1 (count (:discard (get-challenger)))) "Paige Piper intervention stopped Frantic Coding from trashing 9 cards")
    (is (= 5 (:credit (get-challenger))) "No charge to install Gordian")
    ;; a second Frantic Coding will not trigger Paige (once per turn)
    (play-from-hand state :challenger "Frantic Coding")
    (prompt-chocharacter :challenger "OK")
    (prompt-card :challenger (find-card "Ninja" (:deck (get-challenger))))
    (is (= 2 (count (get-program state))) "Installed Ninja")
    (is (= 11 (count (:discard (get-challenger)))) "11 cards in heap")
    (is (= 2 (:credit (get-challenger))) "No charge to install Ninja")))

(deftest patron
  ;; Patron - Ability
  (do-game
    (new-game (default-contestant [(qty "Jackson Howard" 1)])
              (default-challenger [(qty "Patron" 4) (qty "Easy Mark" 4)]))
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (take-credits state :contestant 2)
    (play-from-hand state :challenger "Patron")
    (let [p (get-in @state [:challenger :rig :resource 0])]
      (take-credits state :challenger 3)
      (take-credits state :contestant)
      (prompt-chocharacter :challenger "Server 1")
      (is (= 4 (count (:hand (get-challenger)))) "Starts with 4 cards")
      (run-empty-server state "Server 1")
      (is (= 6 (count (:hand (get-challenger)))) "Drew 2 cards")
      (run-empty-server state "Server 1")
      (prompt-chocharacter :challenger "No")
      (is (= 6 (count (:hand (get-challenger)))) "Drew no cards")
      (play-from-hand state :challenger "Easy Mark")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (prompt-chocharacter :challenger "Server 1")
      (run-empty-server state "Archives")
      (is (= 5 (count (:hand (get-challenger)))) "Did not draw cards when running other server"))))

(deftest patron-manual
  ;; Patron - Manually selecting during Step 1.2 does not show a second prompt at start of turn. Issue #1744.
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Patron" 3) (qty "Jak Sinclair" 3)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (starting-hand state :challenger ["Patron" "Jak Sinclair"])
    (play-from-hand state :challenger "Patron")
    (play-from-hand state :challenger "Jak Sinclair")
    (take-credits state :challenger)
    (let [p (get-resource state 0)
          j (get-resource state 1)]
      (take-credits state :contestant)
      (is (:challenger-phase-12 @state) "Challenger in Step 1.2")
      (card-ability state :challenger p 0)
      (prompt-chocharacter :challenger "Archives")
      (card-ability state :challenger j 0)
      (prompt-chocharacter :challenger "Archives")
      (run-successful state)
      (core/end-phase-12 state :challenger nil)
      (is (empty? (:prompt (get-challenger))) "No second prompt for Patron - used already"))))

(deftest professional-contacts
  ;; Professional Contacts - Click to gain 1 credit and draw 1 card
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Professional Contacts" 3)
                               (qty "Sure Gamble" 2)
                               (qty "Shiv" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Professional Contacts")
    (let [proco (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger proco 0)
      (is (= 2 (:click (get-challenger))) "Spent 1 click")
      (is (= 1 (:credit (get-challenger))) "Gained 1 credit")
      (is (= 5 (count (:hand (get-challenger)))) "Drew 1 card")
      (card-ability state :challenger proco 0)
      (is (= 1 (:click (get-challenger))) "Spent 1 click")
      (is (= 2 (:credit (get-challenger))) "Gained 1 credit")
      (is (= 6 (count (:hand (get-challenger)))) "Drew 1 card"))))

(deftest rolodex
  ;; Rolodex - Full test
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Rolodex" 1) (qty "Sure Gamble" 1) (qty "Desperado" 1)
                               (qty "Diesel" 1) (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :challenger ["Rolodex"])
    (is (= 1 (count (:hand (get-challenger)))))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Rolodex")
    (prompt-chocharacter :challenger (find-card "Sure Gamble" (:deck (get-challenger))))
    (prompt-chocharacter :challenger (find-card "Desperado" (:deck (get-challenger))))
    (prompt-chocharacter :challenger (find-card "Diesel" (:deck (get-challenger))))
    (prompt-chocharacter :challenger (find-card "Corroder" (:deck (get-challenger))))
    (prompt-chocharacter :challenger (find-card "Patron" (:deck (get-challenger))))
    ;; try starting over
    (prompt-chocharacter :challenger "Start over")
    (prompt-chocharacter :challenger (find-card "Patron" (:deck (get-challenger))))
    (prompt-chocharacter :challenger (find-card "Corroder" (:deck (get-challenger))))
    (prompt-chocharacter :challenger (find-card "Diesel" (:deck (get-challenger))))
    (prompt-chocharacter :challenger (find-card "Desperado" (:deck (get-challenger))))
    (prompt-chocharacter :challenger (find-card "Sure Gamble" (:deck (get-challenger)))) ;this is the top card on stack
    (prompt-chocharacter :challenger "Done")
    (is (= "Sure Gamble" (:title (first (:deck (get-challenger))))))
    (is (= "Desperado" (:title (second (:deck (get-challenger))))))
    (is (= "Diesel" (:title (second (rest (:deck (get-challenger)))))))
    (is (= "Corroder" (:title (second (rest (rest (:deck (get-challenger))))))))
    (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-challenger)))))))))
    (core/trash state :challenger (get-resource state 0))
    (is (last-log-contains? state "Sure Gamble, Desperado, Diesel")
        "Rolodex did log trashed card names")
    (is (= 4 (count (:discard (get-challenger)))) "Rolodex mills 3 cards when trashed")
    (is (= "Corroder" (:title (first (:deck (get-challenger))))))))

(deftest sacrificial-construct
  ;; Sacrificial Construct - Trash to prevent trash of installed program or hardware
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Sacrificial Construct" 2) (qty "Cache" 1)
                               (qty "Motivation" 1) (qty "Astrolabe" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 1)
    (play-from-hand state :challenger "Sacrificial Construct")
    (play-from-hand state :challenger "Sacrificial Construct")
    (play-from-hand state :challenger "Cache")
    (play-from-hand state :challenger "Motivation")
    (play-from-hand state :challenger "Astrolabe")
    (take-credits state :challenger)
    (core/trash state :challenger (get-resource state 2))
    (is (empty? (:prompt (get-challenger))) "Sac Con not prompting to prevent resource trash")
    (core/trash state :challenger (get-program state 0))
    (card-ability state :challenger (get-resource state 0) 0)
    (is (= 2 (count (:discard (get-challenger)))) "Sac Con trashed")
    (is (= 1 (count (get-in @state [:challenger :rig :program]))) "Cache still installed")
    (core/trash state :challenger (get-hardware state 0))
    (card-ability state :challenger (get-resource state 0) 0)
    (is (= 3 (count (:discard (get-challenger)))) "Sac Con trashed")
    (is (= 1 (count (get-in @state [:challenger :rig :hardware]))) "Astrolabe still installed")))

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

(deftest salsette-slums
  ;; Salsette Slums - Once per turn, when the trash cost of a card is paid, optionally remove from the game
  (do-game
    (new-game (default-contestant [(qty "Hostile Infrastructure" 1) (qty "Tech Startup" 1) (qty "Thomas Haas" 1)
                             (qty "Hedge Fund" 3)])
              (default-challenger [(qty "Salsette Slums" 2) (qty "Sure Gamble" 3)]))
    ;; Use Hostile Infrastructure to ensure on-trash effects don't fire.
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
          salsette1 (get-resource state 0)
          salsette2 (get-resource state 1)]
      (is (= 3 (count (:hand (get-challenger)))) "Challenger started this part with three cards in hand")
      (core/rez state :contestant hostile2)
      (run-empty-server state "Server 1")
      (is (not (empty? (:prompt (get-challenger)))) "Prompting to trash.")
      (card-ability state :challenger salsette1 0)
      (is (empty? (:prompt (get-challenger))) "All prompts done")
      (is (= 3 (count (:hand (get-challenger)))) "On-trash ability of other Hostile didn't fire")
      (is (= (:cid ts1) (:cid (last (:rfg (get-contestant))))) "Tech Startup was removed from game")
      (is (= 2 (:credit (get-challenger))) "Challenger paid the trash cost.")
      (is (not (:run @state)) "Run is over")
      (run-empty-server state :remote2)
      (is (not (empty? (:prompt (get-challenger)))) "Prompting to trash")
      ;; Only able to use the ability once per turn
      (card-ability state :challenger salsette1 0)
      (is (not (empty? (:prompt (get-challenger)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      ;; Can't use the ability if you can't afford to trash
      (card-ability state :challenger salsette2 0)
      (is (not (empty? (:prompt (get-challenger)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      (prompt-chocharacter :challenger "No")
      ;; Test the "oops I forgot" ability (challenger feels bad that they forgot to use Slums when a Hostile is out)
      (run-empty-server state :remote3)
      (prompt-chocharacter :challenger "Yes")
      ;; Can only use that first Slums once
      (card-ability state :challenger salsette1 1)
      (is (empty? (:prompt (get-challenger))) "Not prompting the challenger")
      (is (not (= (:cid th3) (:cid (last (:rfg (get-contestant)))))) "Card was not removed from the game")
      (card-ability state :challenger salsette2 1)
      (is (not (empty? (:prompt (get-challenger)))) "Prompting the challenger to choose a card")
      (prompt-select :challenger (find-card "Thomas Haas" (:discard (get-contestant))))
      (is (= (:cid th3) (:cid (last (:rfg (get-contestant))))) "Card was removed from the game"))
    ;; Set things up so we can trash the Hostile and then make sure we can't "oops I forgot on a later turn"
    (core/gain state :challenger :credit 5)
    (run-empty-server state :remote2)
    (prompt-chocharacter :challenger "Yes")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (let [salsette1 (get-resource state 0)
          hostile2 (get-content state :remote2 0)]
      (card-ability state :challenger salsette1 1)
      (prompt-select :challenger (find-card "Hostile Infrastructure" (:discard (get-contestant))))
      (is (not (= (:cid hostile2) (:cid (last (:rfg (get-contestant)))))) "Did not remove card from game"))))

(deftest security-testing
  ;; Security Testing - Ability
  (do-game
    (new-game (default-contestant [(qty "Jackson Howard" 1)])
              (default-challenger [(qty "Security Testing" 1)]))
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (take-credits state :contestant 2)
    (play-from-hand state :challenger "Security Testing")
    (let [st (get-in @state [:challenger :rig :resource 0])]
      (take-credits state :challenger 3)
      (take-credits state :contestant)
      (prompt-chocharacter :challenger "Server 1")
      (run-empty-server state "Server 1")
      (is (= 10 (:credit (get-challenger))) "Gained 2 credits from Security Testing")
      (run-empty-server state "Server 1")
      (prompt-chocharacter :challenger "No")
      (is (= 10 (:credit (get-challenger))) "Did not gain credits on second run")
      (take-credits state :challenger 2)
      (take-credits state :contestant)
      (prompt-chocharacter :challenger "Server 1")
      (run-empty-server state "Archives")
      (is (= 12 (:credit (get-challenger))) "Did not gain credits when running other server"))))

(deftest security-testing-multiple
  ;; Security Testing - multiple copies
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Security Testing" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Security Testing")
    (play-from-hand state :challenger "Security Testing")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (prompt-chocharacter :challenger "Archives")
    (prompt-chocharacter :challenger "R&D")
    (run-empty-server state "Archives")
    (is (= 9 (:credit (get-challenger))) "Gained 2 credits")
    (run-empty-server state "R&D")
    (is (= 11 (:credit (get-challenger))))))

(deftest spoilers
  ;; Spoilers - Mill the Contestant when it scores an agenda
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 1) (qty "Hedge Fund" 1)])
              (default-challenger [(qty "Spoilers" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Spoilers")
    (take-credits state :challenger)
    (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
    (is (= 1 (count (:deck (get-contestant)))))
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (score-agenda state :contestant ht)
      (is (= 1 (count (:discard (get-contestant)))))
      (is (= 0 (count (:deck (get-contestant)))) "Last card from R&D milled"))))

(deftest stim-dealer
  ;; Stim Dealer - Take 1 brain damage when it accumulates 2 power counters
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Stim Dealer" 1) (qty "Sure Gamble" 1) (qty "Feedback Filter" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Feedback Filter")
    (play-from-hand state :challenger "Stim Dealer")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (let [sd (get-resource state 0)]
      (is (= 1 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-challenger))) "Gained 1 click")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 2 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-challenger))) "Gained 1 click")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 0 (get-counters (refresh sd) :power)) "Lost all counters")
      (is (empty? (:prompt (get-challenger))) "No Feedback Filter brain dmg prevention possible")
      (is (= 1 (:brain-damage (get-challenger))) "Took 1 brain damage")
      (is (= 4 (:click (get-challenger))) "Didn't gain extra click"))))

(deftest street-peddler-ability
  ;; Street Peddler - Ability
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Street Peddler" 1)
                               (qty "Gordian Blade" 1)
                               (qty "Torch" 1)
                               (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler" "Sure Gamble"])
    (play-from-hand state :challenger "Street Peddler")
    (let [sp (get-in @state [:challenger :rig :resource 0])]
      (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
      (card-ability state :challenger sp 0)
      (prompt-card :challenger (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:challenger :rig :program 0])))
          "Gordian Blade was installed")
      (is (= 3 (:memory (get-challenger))) "Gordian cost 1 mu"))))

(deftest street-peddler-cant-afford
  ;; Street Peddler - Can't afford install
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Street Peddler" 1) (qty "Gordian Blade" 3)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler"])
    (play-from-hand state :challenger "Street Peddler")
    (let [sp (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger sp 0)
      (core/lose state :challenger :credit 3)
      (is (= 2 (count (:mutherfucker (first (:prompt (get-challenger))))))
          "1 card and 1 cancel option on Street Peddler")
      (prompt-card :challenger (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (zero? (count (get-in @state [:challenger :rig :program])))
          "Gordian Blade was not installed")
      (is (and (:installed (refresh sp)) (= 3 (count (:hosted (refresh sp))))
               "Street Peddler still installed with 3 hosted cards")))))

(deftest street-peddler-kate-discount
  ;; Street Peddler - Interaction with Kate discount
  (do-game
    (new-game (default-contestant)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Street Peddler" 1)
                                                                   (qty "Gordian Blade" 1)
                                                                   (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler"])
    (play-from-hand state :challenger "Street Peddler")
    (let [sp (get-in @state [:challenger :rig :resource 0])]
      ;; should still be able to afford Gordian w/ Kate discount
      (core/lose state :challenger :credit 3)
      (card-ability state :challenger sp 0)
      (is (= 2 (count (:mutherfucker (first (:prompt (get-challenger))))))
          "Only 1 chocharacter (plus Cancel) to install off Peddler")
      (prompt-card :challenger (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:challenger :rig :program 0])))
          "Gordian Blade was installed")
      (is (= 3 (:memory (get-challenger))) "Gordian cost 1 mu"))))

(deftest street-peddler-memory-units
  ;; Street Peddler - Programs Should Cost Memory. Issue #708
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Street Peddler" 1) (qty "Corroder" 3)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler"])
    (play-from-hand state :challenger "Street Peddler")
    (is (= 4 (:memory (get-challenger))) "No memory cost for hosting on Street Peddler")
    (let [sp (get-in @state [:challenger :rig :resource 0])]
      (is (= "Corroder" (:title (first (:hosted sp)))) "Street Peddler is hosting Corroder")
      (card-ability state :challenger sp 0)
      (prompt-card :challenger (first (:hosted sp))) ; choose to install Gordian
      (is (= "Corroder" (:title (get-in @state [:challenger :rig :program 0])))
          "Corroder was installed")
      (is (= 3 (:memory (get-challenger))) "Corroder cost 1 mu"))))

(deftest street-peddler-muertos-brain-chip
  ;; Muertos/Brain Chip uninstall effect not fired when removed off peddler/hosting Issue #2294+#2358
  (do-game
    (new-game (default-contestant [(qty "Jackson Howard" 1)])
              (default-challenger [(qty "Street Peddler" 2)(qty "Muertos Gang Member" 1) (qty "Brain Chip" 1)]))
    (core/move state :challenger (find-card "Muertos Gang Member" (:hand (get-challenger))) :deck {:front true})
    (core/move state :challenger (find-card "Brain Chip" (:hand (get-challenger))) :deck {:front true})
    (core/move state :challenger (find-card "Street Peddler" (:hand (get-challenger))) :deck {:front true})
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Street Peddler")
    (core/gain state :challenger :agenda-point 1)
    (let [jh (get-content state :remote1 0)
          sp (get-in @state [:challenger :rig :resource 0])]
      (core/rez state :contestant jh)
      (card-ability state :challenger sp 0)
      (prompt-card :challenger (find-card "Street Peddler" (:hosted sp))) ; choose to another Peddler
      (is (empty? (:prompt (get-contestant))) "Contestant not prompted to rez Jackson")
      (is (= 4 (:memory (get-challenger))) "Challenger has 4 MU"))))

(deftest street-peddler-in-play-effects
  ;; Street Peddler - Trashing hardware should not reduce :in-play values
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Street Peddler" 1) (qty "HQ Interface" 3)]))
   (take-credits state :contestant)
   (starting-hand state :challenger ["Street Peddler"])
   (play-from-hand state :challenger "Street Peddler")
   (let [sp (get-in @state [:challenger :rig :resource 0])]
     (card-ability state :challenger sp 0)
     (prompt-card :challenger (first (:hosted sp))) ; choose to install HQ Interface
     (is (= 2 (:hq-access (get-challenger)))
         "HQ Access increased by 1 from installed HQI and not reduced by the 2 trashed ones"))))

(deftest street-peddler-parasite-1cr
  ;; Street Peddler - Installing Parasite with only 1cr. Issue #491.
  (do-game
    (new-game (default-contestant [(qty "Pop-up Window" 3)])
              (default-challenger [(qty "Street Peddler" 1) (qty "Parasite" 3)]))
    (play-from-hand state :contestant "Pop-up Window" "HQ")
    (take-credits state :contestant 2)
    (starting-hand state :challenger ["Street Peddler"])
    (core/lose state :challenger :credit 4) ; go down to 1 credit
    (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
    (play-from-hand state :challenger "Street Peddler")
    (let [sp (get-in @state [:challenger :rig :resource 0])
          pu (get-character state :hq 0)]
      (core/rez state :contestant pu)
      (card-ability state :challenger sp 0)
      (prompt-card :challenger (first (:hosted sp))) ; choose to install Parasite
      (is (= "Parasite" (:title (:card (first (get-in @state [:challenger :prompt])))))
          "Parasite target prompt")
      (prompt-select :challenger pu)
      (is (= 4 (count (:discard (get-challenger)))) "3 Parasite, 1 Street Peddler in heap")
      (is (= 1 (count (:discard (get-contestant)))) "Pop-up Window in archives"))))

(deftest street-peddler-tech-trader
  ;; Street Peddler - Tech Trader install
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Street Peddler" 1)
                               (qty "Tech Trader" 1)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler"])
    (play-from-hand state :challenger "Street Peddler")
    (let [sp (get-in @state [:challenger :rig :resource 0])]
      (is (= 1 (count (:hosted sp))) "Street Peddler is hosting 1 card")
      (card-ability state :challenger sp 0)
      (prompt-card :challenger (find-card "Tech Trader" (:hosted sp))) ; choose to install Tech Trader
      (is (= "Tech Trader" (:title (get-in @state [:challenger :rig :resource 0])))
          "Tech Trader was installed")
      (is (= 5 (:credit (get-challenger))) "Did not gain 1cr from Tech Trader ability"))))

(deftest-pending street-peddler-trash-while-choosing-card
  ;; Street Peddler - trashing Street Peddler while choosing which card to
  ;; discard should dismiss the chocharacter prompt. Issue #587.
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Street Peddler" 1)
                               (qty "Gordian Blade" 1)
                               (qty "Torch" 1)
                               (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler" "Sure Gamble"])
    (play-from-hand state :challenger "Street Peddler")
    (let [street-peddler (get-in @state [:challenger :rig :resource 0])]
      (is (= 3 (count (:hosted street-peddler))) "Street Peddler is hosting 3 cards")
      (card-ability state :challenger street-peddler 0)
      (trash-resource state "Street Peddler")
      (is (zero? (count (get-in @state [:challenger :prompt])))))))

(deftest symmetrical-visage
  ;; Symmetrical Visage - Gain 1 credit the first time you click to draw each turn
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Symmetrical Visage" 3)
                               (qty "Sure Gamble" 3)
                               (qty "Fall Guy" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Symmetrical Visage")
    (is (= 3 (:credit (get-challenger))))
    (core/click-draw state :challenger nil)
    (is (= 4 (:credit (get-challenger))) "Gained 1 credit from first click spent to draw")
    (core/click-draw state :challenger nil)
    (is (= 4 (:credit (get-challenger))) "No credit gained from second click spent to draw")))

(deftest symmetrical-visage-gcs
  ;; Symmetrical Visage - Gain 1 credit the first and second time you click to draw each turn when GCS is installed
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Symmetrical Visage" 3)
                              (qty "Gene Conditioning Shoppe" 3)
                              (qty "Fall Guy" 1)]))
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
       "No credit gained from third click spent to draw with Gene Conditioning Shoppe")))

(deftest synthetic-blood
  ;; Synthetic Blood - The first time you take damage each turn, draw one card
  (do-game
   (new-game (default-contestant [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
             (default-challenger [(qty "Synthetic Blood" 3)
                              (qty "Sure Gamble" 3)
                              (qty "Fall Guy" 1)]))
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

(deftest synthetic-blood-gcs
  ;; Synthetic Blood - The first and second time you take damage each turn (with GCS installed), draw one card
  (do-game
   (new-game (default-contestant [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
             (default-challenger [(qty "Synthetic Blood" 3)
                              (qty "Sure Gamble" 1)
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
     (is (= 3 (count (:hand (get-challenger)))) "1 card drawn when receiving damage (2nd time)"))))

(deftest technical-writer
  ;; Technical Writer - Gain 1c per program/hardware install; click/trash to take all credits
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Technical Writer" 1) (qty "Faerie" 2)
                               (qty "Vigil" 1) (qty "Same Old Thing" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 2)
    (play-from-hand state :challenger "Technical Writer")
    (let [tw (get-resource state 0)]
      (play-from-hand state :challenger "Faerie")
      (is (= 1 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :challenger "Faerie")
      (is (= 2 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :challenger "Vigil")
      (is (= 3 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :challenger "Same Old Thing")
      (is (= 3 (get-counters (refresh tw) :credit)) "No credit gained for resource install")
      (card-ability state :challenger tw 0)
      (is (= 6 (:credit (get-challenger))) "Gained 3 credits")
      (is (= 0 (:click (get-challenger))) "Spent 1 click")
      (is (= 1 (count (:discard (get-challenger)))) "Technical Writer trashed"))))

(deftest the-helpful-ai
  ;; The Helpful AI - +1 link; trash to give an icebreaker +2 str until end of turn
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "The Helpful AI" 1) (qty "Corroder" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "The Helpful AI")
    (is (= 1 (:link (get-challenger))) "Gained 1 link")
    (play-from-hand state :challenger "Corroder")
    (let [corr (get-program state 0)]
      (card-ability state :challenger (get-resource state 0) 0)
      (prompt-select :challenger corr)
      (is (= 4 (:current-strength (refresh corr))) "Corroder has +2 strength")
      (is (= 1 (count (:discard (get-challenger)))) "Helpful AI trashed")
      (is (= 0 (:link (get-challenger))))
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
    (prompt-chocharacter :challenger "Yes") ; pay 3c extra to steal
    (is (= 4 (:credit (get-challenger))) "Paid 3c to steal")
    (is (= 2 (count (:discard (get-challenger)))) "The Source is trashed")
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
      (is (= 3 (count (:discard (get-challenger)))) "The Source is trashed"))))

(deftest the-supplier-ability
  ;; The Supplier - Ability
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "The Supplier" 1)
                               (qty "Plascrete Carapace" 1)
                               (qty "Utopia Shard" 1)
                               (qty "Hedge Fund" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "The Supplier")
    (let [ts (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger ts 0)
      (prompt-select :challenger (find-card "Plascrete Carapace" (:hand (get-challenger))))
      (card-ability state :challenger ts 0)
      (is (= 1 (count (-> @state :challenger :prompt first :mutherfucker))))
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

(deftest the-supplier-kate-discount
  ;; The Supplier - Interaction with Kate discount. Issue #578.
  (do-game
    (new-game (default-contestant)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                         [(qty "The Supplier" 1)
                          (qty "Plascrete Carapace" 1)
                          (qty "Kati Jones" 1)
                          (qty "Hedge Fund" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "The Supplier")
    (let [ts (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger ts 0)
      (prompt-select :challenger (find-card "Plascrete Carapace" (:hand (get-challenger))))
      (core/lose state :challenger :credit (:credit (get-challenger)))
      (core/end-turn state :challenger nil)
      (take-credits state :contestant)
      (prompt-select :challenger (find-card "Plascrete Carapace" (:hosted (refresh ts))))
      (is (= 0 (:credit (get-challenger))) "Kate discount applied")
      (is (= 1 (count (get-in @state [:challenger :rig :resource]))) "Plascrete installed"))))

(deftest the-supplier-trashed
  ;; Issue #2358 Brain chip mem is deducted when it is hosted and Supplier is trashed
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 2)])
              (default-challenger [(qty "The Supplier" 1)
                               (qty "Brain Chip" 1)]))
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (take-credits state :contestant)
    (is (= 4 (:memory (get-challenger))) "Challenger has 4 MU")
    (play-from-hand state :challenger "The Supplier")
    (let [ts (get-resource state 0)]
      (card-ability state :challenger ts 0)
      (prompt-select :challenger (find-card "Brain Chip" (:hand (get-challenger))))
      (is (= 4 (:memory (get-challenger))) "Challenger has 4 MU")
      (run-empty-server state "Server 1")
      (prompt-chocharacter :challenger "Steal")
      (take-credits state :challenger)
      (core/gain state :challenger :tag 1)
      (core/trash-resource state :contestant nil)
      (prompt-select :contestant (get-resource state 0))
      (is (= 2 (count (:discard (get-challenger)))))
      (is (= 4 (:memory (get-challenger))) "Challenger has 4 MU"))))

(deftest tech-trader
  ;; Basic test
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Tech Trader" 1) (qty "Fall Guy" 1)]))

    (take-credits state :contestant)
    (play-from-hand state :challenger "Tech Trader")
    (play-from-hand state :challenger "Fall Guy")
    (is (= 4 (:credit (get-challenger))))

    (let [fall (get-in @state [:challenger :rig :resource 1])]
      (card-ability state :challenger fall 1)
      (is (= 7 (:credit (get-challenger)))))))

(deftest the-black-file
  ;; The Black File - Prevent Contestant from winning by agenda points
  (do-game
    (new-game (default-contestant [(qty "Vanity Project" 3) (qty "Sure Gamble" 3)])
              (default-challenger [(qty "The Black File" 1)]))
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
    (let [bf (get-resource state 0)]
      (is (= 1 (get-in (refresh bf) [:counter :power])) "1 power counter on The Black File")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 2 (get-in (refresh bf) [:counter :power])) "2 power counters on The Black File")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 1 (count (:rfg (get-challenger)))) "The Black File removed from the game")
      (is (= :contestant (:winner @state)) "Contestant wins")
      (is (= "Agenda" (:reason @state)) "Win condition reports agendas"))))

(deftest the-black-file-flatline
  ;; The Black File - Contestant can still win by flatlining Challenger
  (do-game
    (new-game (default-contestant [(qty "Vanity Project" 3) (qty "Scorched Earth" 3)])
              (default-challenger [(qty "The Black File" 1)]))
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
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest temujin-contract
  ;; Temüjin Contract - Multiple times in one turn. Issue #1952.
  (do-game
    (new-game (default-contestant)
              (make-deck "Silhouette: Stealth Operative" [(qty "Temüjin Contract" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Temüjin Contract")
    (prompt-chocharacter :challenger "Archives")
    (run-empty-server state "Archives")
    (is (= 5 (:credit (get-challenger))) "Gained 4cr")
    (run-empty-server state "Archives")
    (is (= 9 (:credit (get-challenger))) "Gained 4cr")
    (is (= 12 (get-counters (get-resource state 0) :credit)) "Temjin has 12 credits remaining")))

(deftest tri-maf-contact
  ;; Tri-maf Contact - Click for 2c once per turn; take 3 meat dmg when trashed
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Tri-maf Contact" 1) (qty "Cache" 3) (qty "Shiv" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Tri-maf Contact")
    (let [tmc (get-resource state 0)]
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
      (core/trash-resource state :contestant nil)
      (prompt-select :contestant (get-resource state 0))
      (is (= 4 (count (:discard (get-challenger)))) "Took 3 meat damage"))))

(deftest virus-breeding-ground-gain
  ;; Virus Breeding Ground - Gain counters
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Virus Breeding Ground" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Virus Breeding Ground")
    (let [vbg (get-in @state [:challenger :rig :resource 0])]
      (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :challenger 3)
      (take-credits state :contestant)
      (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
      (take-credits state :challenger 3)
      (take-credits state :contestant)
      (is (= 2 (get-counters (refresh vbg) :virus))
          "Virus Breeding Ground gains 1 counter per turn"))))

(deftest virus-breeding-ground-gain
  ;; Virus Breeding Ground - Move counters
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Virus Breeding Ground" 1) (qty "Hivemind" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Virus Breeding Ground")
    (play-from-hand state :challenger "Hivemind")
    (let [hive (get-in @state [:challenger :rig :program 0])
          vbg (get-in @state [:challenger :rig :resource 0])]
      (is (= 1 (get-counters hive :virus)) "Hivemind starts with 1 counter")
      (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :challenger 3)
      (take-credits state :contestant)
      (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
      (card-ability state :challenger vbg 0)
      (prompt-select :challenger hive)
      (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
      (is (= 0 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))

(deftest wasteland
  ;; Wasteland - Gain 1c the first time you trash an installed card of yours each turn
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 1)])
              (default-challenger [(qty "Wasteland" 1) (qty "Faust" 1) (qty "Fall Guy" 4)]))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :click 2)
    (core/gain state :challenger :credit 4)
    (core/draw state :challenger)
    (play-from-hand state :challenger "Faust")
    (play-from-hand state :challenger "Wasteland")
    (is (= 4 (:credit (get-challenger))) "Challenger has 4 credits")
    (run-empty-server state "Server 1")
    (prompt-chocharacter :challenger "Yes") ; Trash PAD campaign
    (is (= 0 (:credit (get-challenger))) "Gained nothing from Wasteland on contestant trash")
    ; trash from hand first which should not trigger #2291
    (let [faust (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger faust 1)
      (prompt-card :challenger (first (:hand (get-challenger)))))
    (is (= 0 (:credit (get-challenger))) "Gained nothing from Wasteland")
    (play-from-hand state :challenger "Fall Guy")
    (play-from-hand state :challenger "Fall Guy")
    (play-from-hand state :challenger "Fall Guy")
    (card-ability state :challenger (get-resource state 1) 1)
    (is (= 1 (count (:discard (get-challenger)))) "Fall Guy trashed")
    (is (= 3 (:credit (get-challenger))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (take-credits state :challenger)
    (card-ability state :challenger (get-resource state 1) 1)
    (is (= 2 (count (:discard (get-challenger)))) "Fall Guy trashed")
    (is (= 6 (:credit (get-challenger))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (card-ability state :challenger (get-resource state 1) 1)
    (is (= 3 (count (:discard (get-challenger)))) "Fall Guy trashed")
    (is (= 8 (:credit (get-challenger))) "Gained 2c from Fall Guy but no credits from Wasteland")))

(deftest xanadu
  ;; Xanadu - Increase all Character rez cost by 1 credit
  (do-game
    (new-game (default-contestant [(qty "Paper Wall" 2) (qty "Launch Campaign" 1)])
              (default-challenger [(qty "Xanadu" 1)]))
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
  ;; Zona Sul Shipping - Gain 1c per turn, click to take all credits. Trash when tagged
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Zona Sul Shipping" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Zona Sul Shipping")
    (let [zss (get-resource state 0)]
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
      (is (= 1 (count (:discard (get-challenger)))) "Zona Sul trashed when tag taken"))))
