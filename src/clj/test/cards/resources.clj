(ns test.cards.resources
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest activist-support
  ;; Activist Support - Take tag if you have none; Corp gains bad pub if they have none
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Activist Support" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Activist Support")
    (is (= 0 (:tag (get-hero))))
    (take-credits state :hero)
    (is (= 1 (:tag (get-hero))) "Runner took 1 tag; had none")
    (is (= 0 (:bad-publicity (get-minion))))
    (take-credits state :minion)
    (is (= 1 (:bad-publicity (get-minion))) "Corp took 1 bad pub; had none")
    (take-credits state :hero)
    (is (= 1 (:tag (get-hero))) "Runner had 1 tag; didn't take another")
    (take-credits state :minion)
    (is (= 1 (:bad-publicity (get-minion))) "Corp had 1 bad pub; didn't take another")))

(deftest adjusted-chronotype
  ;; Ensure adjusted chronotype gains only 1 click when 2 clicks are lost
  (do-game
   (new-game (default-minion)
             (default-hero [(qty "Adjusted Chronotype" 1) (qty "Beach Party" 2)]))
   (take-credits state :minion)
   (play-from-hand state :hero "Adjusted Chronotype")
   (play-from-hand state :hero "Beach Party")
   (take-credits state :hero)
   (take-credits state :minion)
   (is (= 4 (:click (get-hero))) "Should have lost 1 click and gained 1 click")
   (play-from-hand state :hero "Beach Party")
   (take-credits state :hero)
   (take-credits state :minion)
   (is (= 3 (:click (get-hero))) "Should have lost 2 clicks and gained 1 click")))

(deftest adjusted-chronotype-mca
  ;; Chronotype to cancel out MCA click loss
  (do-game
    (new-game
      (default-minion [(qty "MCA Austerity Policy" 1)])
      (default-hero [(qty "Adjusted Chronotype" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Adjusted Chronotype")
    (take-credits state :hero)
    (play-from-hand state :minion "MCA Austerity Policy" "New remote")
    (let [mca (get-content state :remote1 0)]
      (core/rez state :minion mca)
      (card-ability state :minion mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      (take-credits state :minion)
      ; hero does not lose a click
      (is (= 4 (:click (get-hero)))))))

(deftest adjusted-chronotype-gcs
  ;; Ensure adjusted chronotype gains 2 clicks when 2 clicks are lost and GCS is installed
  (do-game
   (new-game (default-minion)
             (default-hero [(qty "Adjusted Chronotype" 1)
                              (qty "Beach Party" 3)
                              (qty "Gene Conditioning Shoppe" 1)]))
   (take-credits state :minion)
   (play-from-hand state :hero "Adjusted Chronotype")
   (play-from-hand state :hero "Beach Party")
   (take-credits state :hero)
   (take-credits state :minion)
   (is (= 4 (:click (get-hero))) "Should have lost 1 click and gained 1 click")
   (play-from-hand state :hero "Beach Party")
   (play-from-hand state :hero "Gene Conditioning Shoppe")
   (take-credits state :hero)
   (take-credits state :minion)
   (is (= 4 (:click (get-hero))) "Should have lost 2 clicks and gained 2 clicks")
   (play-from-hand state :hero "Beach Party")
   (take-credits state :hero)
   (take-credits state :minion)
   (is (= 3 (:click (get-hero))) "Should have lost 3 clicks and gained 2 clicks")))

(deftest aesops-pawnshop
  ;; Tests use cases for Aesop's Pawnshop
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Aesop's Pawnshop" 1) (qty "Cache" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Aesop's Pawnshop")
    (play-from-hand state :hero "Cache")
    (let [orig-credits (:credit (get-hero))
          ap (get-in @state [:hero :rig :resource 0])
          cache (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero ap 0)
      (prompt-select :hero cache)
      (card-ability state :hero ap 0)
      (prompt-select :hero ap)
      (let [ap (get-in @state [:hero :rig :resource 0])
            cache (get-in @state [:hero :discard 0])]
        (is (= (+ 3 orig-credits) (:credit (get-hero))) "Should have only gained 3 credits")
        (is (not= cache nil) "Cache should be in Heap")
        (is (not= ap nil) "Aesops should still be installed")))))

(deftest all-nighter
  ;; All-nighter - Click/trash to gain 2 clicks
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "All-nighter" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "All-nighter")
    (is (= 3 (:click (get-hero))))
    (card-ability state :hero (get-resource state 0) 0)
    (is (= 4 (:click (get-hero))) "Spent 1 click; gained 2 clicks")
    (is (= 1 (count (:discard (get-hero)))) "All-nighter is trashed")))

(deftest bank-job-manhunt
  ;; Bank Job - Manhunt trace happens first
  (do-game
    (new-game (default-minion [(qty "Manhunt" 1) (qty "PAD Campaign" 1)])
              (default-hero [(qty "Bank Job" 1)]))
    (play-from-hand state :minion "Manhunt")
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Bank Job")
    (run-empty-server state "Server 1")
    (prompt-choice :minion 2) ; Manhunt trace active
    (prompt-choice :hero 0)
    (prompt-choice :hero "Run ability")
    (is (= "Bank Job" (:title (:card (first (get-in @state [:hero :prompt])))))
        "Bank Job prompt active")
    (prompt-choice :hero 8)
    (is (empty? (get-in @state [:hero :rig :resource])) "Bank Job trashed after all credits taken")
    (is (= 1 (count (:discard (get-hero)))))))

(deftest bank-job-multiple-copies
  ;; Bank Job - Choose which to use when 2+ copies are installed
  (do-game
    (new-game (default-minion [(qty "PAD Campaign" 1)])
              (default-hero [(qty "Bank Job" 2)]))
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Bank Job")
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Run ability")
    (prompt-choice :hero 4)
    (play-from-hand state :hero "Bank Job")
    (let [bj1 (get-resource state 0)
          bj2 (get-resource state 1)]
      (is (= 4 (get-counters (refresh bj1) :credit)) "4 credits remaining on 1st copy")
      (run-empty-server state "Server 1")
      (prompt-choice :hero "Run ability")
      (prompt-select :hero bj2)
      (prompt-choice :hero 6)
      (is (= 13 (:credit (get-hero))))
      (is (= 2 (get-counters (refresh bj2) :credit)) "2 credits remaining on 2nd copy"))))

(deftest bank-job-sectesting
  ;; Bank Job - Security Testing takes priority
  (do-game
    (new-game (default-minion [(qty "PAD Campaign" 1)])
              (default-hero [(qty "Bank Job" 1) (qty "Security Testing" 1)]))
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Security Testing")
    (play-from-hand state :hero "Bank Job")
    (take-credits state :hero)
    (take-credits state :minion)
    (prompt-choice :hero "Server 1")
    (is (= 6 (:credit (get-hero))))
    (run-empty-server state "Server 1")
    (is (empty? (:prompt (get-hero))) "No Bank Job replacement choice")
    (is (= 8 (:credit (get-hero))) "Security Testing paid 2c")))

(deftest bazaar-grip-only
  ;; Bazaar - Only triggers when installing from Grip
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Street Peddler" 1)
                               (qty "Bazaar" 1)
                               (qty "Spy Camera" 6)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Street Peddler" "Bazaar" "Spy Camera" "Spy Camera" "Spy Camera"])
    (play-from-hand state :hero "Bazaar")
    (play-from-hand state :hero "Street Peddler")
    (let [peddler (get-resource state 1)]
      (card-ability state :hero peddler 0)
      (prompt-card :hero (first (:hosted peddler)))
      (is (empty? (:prompt (get-hero))) "No Bazaar prompt from install off Peddler"))))

(deftest beach-party
  ;; Beach Party - Lose 1 click when turn begins; hand size increased by 5
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Beach Party" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Beach Party")
    (is (= 10 (core/hand-size state :hero)) "Max hand size increased by 5")
    (take-credits state :hero)
    (take-credits state :minion)
    (is (= 3 (:click (get-hero))) "Lost 1 click at turn start")))

(deftest bhagat
  ;; Bhagat - only trigger on first run
  (do-game
    (new-game (default-minion [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3) (qty "Architect" 3)])
              (default-hero [(qty "Bhagat" 1)]))
    (starting-hand state :minion [])
    (take-credits state :minion)
    (run-empty-server state :hq)
    (play-from-hand state :hero "Bhagat")
    (run-empty-server state :hq)
    (is (empty? (:discard (get-minion))) "Bhagat did not trigger on second successful run")
    (take-credits state :hero)
    (take-credits state :minion)
    (run-empty-server state :hq)
    (is (= 1 (count (:discard (get-minion)))) "Bhagat milled one card")))

(deftest chrome-parlor
  ;; Chrome Parlor - Prevent all meat/brain dmg when installing cybernetics
  (do-game
    (new-game (default-minion [(qty "Traffic Accident" 1)])
              (default-hero [(qty "Chrome Parlor" 1) (qty "Titanium Ribs" 1)
                               (qty "Brain Cage" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Chrome Parlor")
    (play-from-hand state :hero "Titanium Ribs")
    (is (empty? (:prompt (get-hero))) "Damage prevented, no Ribs prompt to choose cards")
    (is (= 3 (count (:hand (get-hero)))))
    (play-from-hand state :hero "Brain Cage")
    (is (= 2 (count (:hand (get-hero)))) "No cards lost")
    (is (= 0 (:brain-damage (get-hero))))
    (is (= 8 (core/hand-size state :hero)) "Runner hand size boosted by Brain Cage")
    (take-credits state :hero)
    (core/gain state :hero :tag 2)
    (core/trash state :hero (get-hardware state 0))
    (play-from-hand state :minion "Traffic Accident")
    (is (= 3 (count (:discard (get-hero)))) "Conventional meat damage not prevented by Parlor")))

(deftest compromised-employee
  ;; Compromised Employee - Gain 1c every time Corp rezzes ICE
  (do-game
    (new-game (default-minion [(qty "Pup" 2) (qty "Launch Campaign" 1)])
              (default-hero [(qty "Compromised Employee" 1)]))
    (play-from-hand state :minion "Pup" "HQ")
    (play-from-hand state :minion "Pup" "R&D")
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Compromised Employee")
    (let [ce (get-resource state 0)]
      (is (= 1 (:rec-counter (refresh ce))) "Has 1 recurring credit")
      (core/rez state :minion (get-ice state :hq 0))
      (is (= 4 (:credit (get-hero))) "Gained 1c from ICE rez")
      (core/rez state :minion (get-ice state :rd 0))
      (is (= 5 (:credit (get-hero))) "Gained 1c from ICE rez")
      (core/rez state :minion (get-content state :remote1 0))
      (is (= 5 (:credit (get-hero))) "Asset rezzed, no credit gained"))))

(deftest councilman
  ;; Councilman reverses the rezz and prevents re-rezz
  (do-game
    (new-game (default-minion [(qty "Jackson Howard" 1)])
              (default-hero [(qty "Councilman" 1)]))
    (play-from-hand state :minion "Jackson Howard" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Councilman")
    (let [jesus (get-content state :remote1 0)
          judas (get-resource state 0)]
      (core/rez state :minion jesus)
      ;; Runner triggers Councilman
      (card-ability state :hero judas 0)
      (prompt-select :hero jesus)
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard no longer rezzed")
      (core/rez state :minion (refresh jesus))
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard cannot be rezzed")
      (take-credits state :hero)
      ;; Next turn
      (core/rez state :minion (refresh jesus))
      (is (core/rezzed? (refresh jesus)) "Jackson Howard can be rezzed next turn"))))

(deftest counter-surveillance
  ;; Trash to run, on successful run access cards equal to Tags and pay that amount in credits
  (do-game
    (new-game (default-minion [(qty "Hedge Fund" 3)])
              (default-hero [(qty "Counter Surveillance" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :tag 2)
    (play-from-hand state :hero "Counter Surveillance")
    (-> @state :hero :credit (= 4) (is "Runner has 4 credits"))
    (let [cs (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero cs 0)
      (prompt-choice :hero "HQ")
      (run-successful state)
      (-> (get-hero) :register :successful-run (= [:hq]) is)
      (prompt-choice :hero "Card from hand")
      (-> (get-hero) :prompt first :msg (= "You accessed Hedge Fund") is)
      (prompt-choice :hero "OK")
      (prompt-choice :hero "Card from hand")
      (-> (get-hero) :prompt first :msg (= "You accessed Hedge Fund") is)
      (prompt-choice :hero "OK")
      (-> @state :hero :discard count (= 1) (is "Counter Surveillance trashed"))
      (-> @state :hero :credit (= 2) (is "Runner has 2 credits")))))

(deftest counter-surveillance-obelus
  ;; Test Obelus does not trigger before Counter Surveillance accesses are done. Issues #2675
  (do-game
    (new-game (default-minion [(qty "Hedge Fund" 3)])
              (default-hero [(qty "Counter Surveillance" 1) (qty "Obelus" 1) (qty "Sure Gamble" 3)]))
    (starting-hand state :hero ["Counter Surveillance" "Obelus"])
    (take-credits state :minion)
    (core/gain state :hero :tag 2)
    (core/gain state :hero :credit 2)
    (-> (get-hero) :credit (= 7) (is "Runner has 7 credits"))
    (play-from-hand state :hero "Counter Surveillance")
    (play-from-hand state :hero "Obelus")
    (-> (get-hero) :credit (= 2) (is "Runner has 2 credits")) ; Runner has enough credits to pay for CS
    (let [cs (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero cs 0)
      (prompt-choice :hero "HQ")
      (run-successful state)
      (-> (get-hero) :register :successful-run (= [:hq]) is)
      (-> (get-hero) :hand count zero? (is "Runner did not draw cards from Obelus yet"))
      (prompt-choice :hero "Card from hand")
      (-> (get-hero) :prompt first :msg (= "You accessed Hedge Fund") is)
      (-> (get-hero) :hand count zero? (is "Runner did not draw cards from Obelus yet"))
      (prompt-choice :hero "OK")
      (prompt-choice :hero "Card from hand")
      (-> (get-hero) :prompt first :msg (= "You accessed Hedge Fund") is)
      (prompt-choice :hero "OK")
      (-> (get-hero) :hand count (= 2) (is "Runner did draw cards from Obelus after all accesses are done"))
      (-> (get-hero) :discard count (= 1) (is "Counter Surveillance trashed"))
      (-> (get-hero) :credit (= 0) (is "Runner has no credits")))))

(deftest-pending councilman-zone-change
  ;; Rezz no longer prevented when card changes zone (issues #1571)
  (do-game
    (new-game (default-minion [(qty "Jackson Howard" 1)])
              (default-hero [(qty "Councilman" 1)]))
    (play-from-hand state :minion "Jackson Howard" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Councilman")
    (take-credits state :hero)
    (let [jesus (get-content state :remote1 0)
          judas (get-resource state 0)]
      (core/rez state :minion jesus)
      ;; Runner triggers Councilman
      (card-ability state :hero judas 0)
      (prompt-select :hero jesus)
      (is (not (core/rezzed? (refresh jesus))) "Jackson Howard no longer rezzed")
      (core/move state :minion (refresh jesus) :hand))
    (play-from-hand state :minion "Jackson Howard" "New remote")
    (let [jesus (get-content state :remote2 0)]
      (core/rez state :minion jesus)
      (is (core/rezzed? (refresh jesus)) "Jackson Howard can be rezzed after changing zone"))))

(deftest daily-casts
  ;; Play and tick through all turns of daily casts
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Daily Casts" 3)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Daily Casts")
    (let [dc (get-in @state [:hero :rig :resource 0])]
      ;; Number of credits
      (is (= 8 (get-counters dc :credit)))
      (is (= 2 (get-in @state [:hero :credit])))
      ;; End turn
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 6 (get-counters (refresh dc) :credit)))
      (is (= 7 (get-in @state [:hero :credit])))
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 4 (get-counters (refresh dc) :credit)))
      (is (= 13 (get-in @state [:hero :credit])))
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 2 (get-counters (refresh dc) :credit)))
      (is (= 19 (get-in @state [:hero :credit])))
      (take-credits state :hero)
      (take-credits state :minion)
      (is (nil? (get-in @state [:hero :rig :resource 0]))))))

(deftest data-folding
  ;; Data Folding - Gain 1c at start of turn if 2+ unused MU
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Data Folding" 1) (qty "Hyperdriver" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Data Folding")
    (take-credits state :hero)
    (take-credits state :minion)
    (is (= 4 (:memory (get-hero))) "At least 2 unused MU")
    (is (= 6 (:credit (get-hero))) "Gained 1c at turn start")
    (play-from-hand state :hero "Hyperdriver")
    (take-credits state :hero)
    (is (= 1 (:memory (get-hero))) "Only 1 unused MU")
    (is (= 8 (:credit (get-hero))))
    (take-credits state :minion)
    (is (= 8 (:credit (get-hero))) "No credits gained at turn start")))

(deftest ddos
  ;; Prevent rezzing of outermost ice for the rest of the turn
  (do-game
    (new-game (default-minion [(qty "Ice Wall" 3)])
              (default-hero [(qty "DDoS" 1)]))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (play-from-hand state :minion "Ice Wall" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "DDoS")
    (let [ddos (get-in @state [:hero :rig :resource 0])
          iwall (get-ice state :hq 1)]
      (card-ability state :hero ddos 0)
      (is (= (:title ddos) (get-in @state [:hero :discard 0 :title])))
      (run-on state "HQ")
      (core/rez state :minion iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (run-jack-out state)
      (run-on state "HQ")
      (core/rez state :minion iwall)
      (is (not (get-in (refresh iwall) [:rezzed])))
      (run-jack-out state)
      (take-credits state :hero)
      (take-credits state :minion)
      (run-on state "HQ")
      (core/rez state :minion iwall)
      (is (get-in (refresh iwall) [:rezzed])))))

(deftest decoy
  ;; Decoy - Trash to avoid 1 tag
  (do-game
    (new-game (default-minion [(qty "SEA Source" 1)])
              (default-hero [(qty "Decoy" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Decoy")
    (run-empty-server state :archives)
    (take-credits state :hero)
    (play-from-hand state :minion "SEA Source")
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (is (= 1 (count (:prompt (get-hero)))) "Runner prompted to avoid tag")
    (card-ability state :hero (get-resource state 0) 0)
    (is (= 1 (count (:discard (get-hero)))) "Decoy trashed")
    (is (= 0 (:tag (get-hero))) "Tag avoided")))

(deftest donut-taganes
  ;; Donut Taganes - add 1 to play cost of Operations & Events when this is in play
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Donut Taganes" 1) (qty "Easy Mark" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Donut Taganes")
    (is (= 2 (:credit (get-hero))) "Donut played for 3c")
    (play-from-hand state :hero "Easy Mark")
    (is (= 4 (:credit (get-hero))) "Easy Mark only gained 2c")
    (take-credits state :hero)
    (is (= 8 (:credit (get-minion))) "Corp has 8c")
    (play-from-hand state :minion "Hedge Fund")
    (is (= 11 (:credit (get-minion))) "Corp has 11c")))

(deftest eden-shard
  ;; Eden Shard - Install from Grip in lieu of accessing R&D; trash to make Corp draw 2
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Eden Shard" 1)]))
    (starting-hand state :minion ["Hedge Fund"])
    (take-credits state :minion)
    (is (= 1 (count (:hand (get-minion)))))
    (run-on state :rd)
    (core/no-action state :minion nil)
    (play-from-hand state :hero "Eden Shard")
    (is (= 5 (:credit (get-hero))) "Eden Shard installed for 0c")
    (is (not (:run @state)) "Run is over")
    (card-ability state :hero (get-resource state 0) 0)
    (is (= 3 (count (:hand (get-minion)))) "Corp drew 2 cards")
    (is (= 1 (count (:discard (get-hero)))) "Eden Shard trashed")))

(deftest eden-shard-no-install-on-access
  ;; Eden Shard - Do not install when accessing cards
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Eden Shard" 1)]))
    (starting-hand state :minion ["Hedge Fund"])
    (take-credits state :minion)
    (is (= 1 (count (:hand (get-minion)))))
    (run-empty-server state :rd)
    (play-from-hand state :hero "Eden Shard")
    (is (not (get-resource state 0)) "Eden Shard not installed")
    (is (= 1 (count (:hand (get-hero)))) "Eden Shard not installed")))

(deftest fan-site
  ;; Fan Site - Add to score area as 0 points when Corp scores an agenda
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 1)])
              (default-hero [(qty "Fan Site" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Fan Site")
    (take-credits state :hero)
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    (is (= 0 (:agenda-point (get-hero))))
    (is (= 1 (count (:scored (get-hero)))) "Fan Site added to Runner score area")))

(deftest fan-site-eoi
  ;; Fan Site - Don't trigger after swap with Exchange of Information. Issue #1824
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 2) (qty "Exchange of Information" 1)])
              (default-hero [(qty "Fan Site" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Fan Site")
    (take-credits state :hero)
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    (core/tag-hero state :hero 1)

    (play-from-hand state :minion "Exchange of Information")

    (prompt-select :minion (find-card "Fan Site" (:scored (get-hero))))
    (prompt-select :minion (find-card "Hostile Takeover" (:scored (get-minion))))

    (is (= 1 (:agenda-point (get-hero))))
    (is (= 0 (:agenda-point (get-minion))))

    (is (find-card "Fan Site" (:scored (get-minion))) "Fan Site swapped into Corp score area")
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (score-agenda state :minion (get-content state :remote2 0))
    (is (find-card "Fan Site" (:scored (get-minion))) "Fan Site not removed from Corp score area")))

(deftest fan-site-forfeit
  ;; Fan Site - Runner can forfeit Fan Site
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 1)])
              (default-hero [(qty "Fan Site" 1) (qty "Data Dealer" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Fan Site")
    (take-credits state :hero)
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    (is (= 0 (:agenda-point (get-hero))))
    (is (= 1 (count (:scored (get-hero)))) "Fan Site added to Runner score area")
    (take-credits state :minion)
    (play-from-hand state :hero "Data Dealer")
    (let [credits (:credit (get-hero))]
      (card-ability state :hero (get-resource state 0) 0)
      (prompt-select :hero (get-scored state :hero 0))
      (is (= 0 (count (:scored (get-hero)))) "Fan Site successfully forfeit to Data Dealer")
      (is (= (+ credits 9) (:credit (get-hero))) "Gained 9 credits from Data Dealer"))))

(deftest fester
  ;; Fester - Corp loses 2c (if able) when purging viruses
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Fester" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Fester")
    (take-credits state :hero)
    (core/lose state :minion :credit 5)
    (core/gain state :minion :click 3)
    (is (= 3 (:credit (get-minion))))
    (core/purge state :minion)
    (is (= 1 (:credit (get-minion))) "Lost 2c when purging")
    (core/purge state :minion)
    (is (= 1 (:credit (get-minion))) "Lost no credits when purging, only had 1c")))

(deftest film-critic-discarded-executives
  ;; Film Critic - Prevent Corp-trashed execs going to Runner scored. Issues #1181/#1042
  (do-game
    (new-game (default-minion [(qty "Director Haas" 3) (qty "Project Vitruvius" 3) (qty "Hedge Fund" 1)])
              (default-hero [(qty "Film Critic" 1)]))
    (play-from-hand state :minion "Project Vitruvius" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Film Critic")
    (let [fc (first (get-in @state [:hero :rig :resource]))]
      (run-empty-server state "Server 1")
      (card-ability state :hero fc 0)
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (take-credits state :hero)
      (trash-from-hand state :minion "Director Haas")
      (is (= 1 (count (:discard (get-minion)))) "Director Haas stayed in Archives")
      (is (= 0 (:agenda-point (get-hero))) "No points gained by Runner")
      (is (empty? (:scored (get-hero))) "Nothing in Runner scored"))))

(deftest film-critic-fetal-ai
  ;; Film Critic - Fetal AI interaction
  (do-game
    (new-game (default-minion [(qty "Fetal AI" 3)])
              (default-hero [(qty "Film Critic" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Film Critic")
    (let [fc (first (get-in @state [:hero :rig :resource]))]
      (run-empty-server state "HQ")
      ;; should not have taken damage yet
      (is (= 3 (count (:hand (get-hero)))) "No damage dealt yet")
      (card-ability state :hero fc 0)
      (is (= 3 (count (:hand (get-hero)))) "No damage dealt")
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (card-ability state :hero fc 1)
      (is (= 1 (count (:scored (get-hero)))) "Agenda added to hero scored")
      (is (= 3 (count (:hand (get-hero)))) "No damage dealt"))))

(deftest film-critic-hostile-infrastructure
  ;; Do not take a net damage when a hosted agenda is trashed due to film critic trash #2382
  (do-game
    (new-game (default-minion [(qty "Hostile Infrastructure" 3) (qty "Project Vitruvius" 1)])
              (default-hero [(qty "Film Critic" 1) (qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Hostile Infrastructure" "New remote")
    (play-from-hand state :minion "Project Vitruvius" "New remote")
    (core/rez state :minion (get-content state :remote1 0))
    (take-credits state :minion)
    (play-from-hand state :hero "Film Critic")
    (let [fc (first (get-in @state [:hero :rig :resource]))]
      (run-empty-server state :remote2)
      (card-ability state :hero fc 0)
      (is (= 1 (count (:hosted (refresh fc)))) "Agenda hosted on FC")
      (take-credits state :hero)
      (core/gain state :minion :credit 10)
      (core/trash-resource state :minion nil)
      (prompt-select :minion fc)
      (is (= 1 (count (:discard (get-hero)))) "FC trashed")
      (is (= 1 (count (:discard (get-minion)))) "Agenda trashed")
      (is (= 3 (count (:hand (get-hero)))) "No damage dealt"))))

(deftest gang-sign
  ;; Gang Sign - accessing from HQ, not including root. Issue #2113.
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 3) (qty "Braintrust" 2) (qty "Crisium Grid" 1)])
              (default-hero [(qty "Gang Sign" 2) (qty "HQ Interface" 1)]))
    (play-from-hand state :minion "Crisium Grid" "HQ")
    (take-credits state :minion)
    (core/gain state :hero :credit 100)
    (play-from-hand state :hero "Gang Sign")
    (play-from-hand state :hero "Gang Sign")
    (play-from-hand state :hero "HQ Interface")
    (take-credits state :hero)
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    (prompt-choice :hero "Gang Sign") ; simultaneous effect resolution
    (let [gs1 (-> (get-hero) :prompt first)]
      (is (= (:choices gs1) ["Card from hand"]) "Gang Sign does not let Runner access upgrade in HQ root")
      (prompt-choice :hero "Card from hand")
      (prompt-choice :hero "Steal")
      (is (= (:card gs1) (-> (get-hero) :prompt first :card)) "Second access from first Gang Sign triggered")
      (prompt-choice :hero "Card from hand")
      (prompt-choice :hero "Steal")
      (is (not= (:card gs1) (-> (get-hero) :prompt first :card)) "First access from second Gang Sign triggered")
      (prompt-choice :hero "Card from hand")
      (prompt-choice :hero "Steal")
      (prompt-choice :hero "Card from hand")
      (prompt-choice :hero "Steal"))))

(deftest gene-conditioning-shoppe
  ;; Gene Conditioning Shoppe - set :genetics-trigger-twice flag
  (do-game
   (new-game (default-minion [(qty "Hedge Fund" 3)])
             (default-hero [(qty "Gene Conditioning Shoppe" 1)
                              (qty "Adjusted Chronotype" 1)]))
   (take-credits state :minion)
   (play-from-hand state :hero "Adjusted Chronotype")
   (is (not (core/has-flag? state :hero :persistent :genetics-trigger-twice)))
   (play-from-hand state :hero "Gene Conditioning Shoppe")
   (is (core/has-flag? state :hero :persistent :genetics-trigger-twice))
   (core/trash state :hero (get-in @state [:hero :rig :resource 1]))
   (is (not (core/has-flag? state :hero :persistent :genetics-trigger-twice)))))

(deftest gene-conditioning-shoppe-redundancy
  ;; Gene Conditioning Shoppe - set :genetics-trigger-twice flag - ensure redundant copies work
  (do-game
   (new-game (default-minion [(qty "Hedge Fund" 3)])
             (default-hero [(qty "Gene Conditioning Shoppe" 2)
                              (qty "Adjusted Chronotype" 1)]))
   (take-credits state :minion)
   (take-credits state :hero)
   (take-credits state :minion)
   (play-from-hand state :hero "Adjusted Chronotype")
   (let [adjusted-chronotype (get-in @state [:hero :rig :resource 0])]
     (is (not (core/has-flag? state :hero :persistent :genetics-trigger-twice)))
     (play-from-hand state :hero "Gene Conditioning Shoppe")
     (play-from-hand state :hero "Gene Conditioning Shoppe")
     (let [gcs1 (get-in @state [:hero :rig :resource 1])
           gcs2 (get-in @state [:hero :rig :resource 2])]
       (is (core/has-flag? state :hero :persistent :genetics-trigger-twice))
       (core/trash state :hero gcs1)
       (is (core/has-flag? state :hero :persistent :genetics-trigger-twice))
       (core/trash state :hero gcs2)
       (is (not (core/has-flag? state :hero :persistent :genetics-trigger-twice)))))))

(deftest globalsec-security-clearance
  ;; Globalsec Security Clearance - Ability, click lost on use
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Globalsec Security Clearance" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :link 2)
    (play-from-hand state :hero "Globalsec Security Clearance")
    (take-credits state :hero)
    (starting-hand state :minion ["Hedge Fund"]) ; Hedge Fund on top
    (take-credits state :minion)
    (is (:hero-phase-12 @state) "Runner in Step 1.2")
    (let [gsec (-> (get-hero) :rig :resource first)]
      (card-ability state :hero gsec 0)
      (is (pos? (.indexOf (-> (get-hero) :prompt first :msg) "Hedge Fund")) "GSec revealed Hedge Fund")
      (core/end-phase-12 state :hero nil)
      (is (= 3 (:click (get-hero))) "Runner lost 1 click from Globalsec Security Clearance"))))

(deftest grifter
  ;; Grifter - Gain 1c if you made a successful run this turn, otherwise trash it
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Grifter" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Grifter")
    (run-empty-server state :hq)
    (take-credits state :hero)
    (is (= 6 (:credit (get-hero))) "Gained 1c for a successful run during the turn")
    (take-credits state :minion)
    (run-on state :hq)
    (run-jack-out state)
    (take-credits state :hero)
    (is (= 1 (count (:discard (get-hero)))) "No successful runs; Grifter is trashed")))

(deftest hard-at-work
  ;; Hard at Work - Gain 2c and lose 1 click when turn begins
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Hard at Work" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Hard at Work")
    (take-credits state :hero)
    (take-credits state :minion)
    (is (= 5 (:credit (get-hero))) "Gained 2c")
    (is (= 3 (:click (get-hero))) "Lost 1 click")))

(deftest ice-carver
  ;; Ice Carver - lower ice strength on encounter
  (do-game
    (new-game (default-minion [(qty "Ice Wall" 1)])
              (default-hero [(qty "Ice Carver" 1)]))
    (play-from-hand state :minion "Ice Wall" "Archives")
    (take-credits state :minion 2)
    (let [iwall (get-ice state :archives 0)]
      (core/rez state :minion iwall)
      (play-from-hand state :hero "Ice Carver")
      (run-on state "Archives")
      (is (= 0 (:current-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh iwall))) "Ice Wall strength at 1 after encounter"))))

(deftest investigative-journalism
  ;; Investigative Journalism - 4 clicks and trash to give the Corp 1 bad pub
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Investigative Journalism" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Investigative Journalism")
    (is (empty? (get-in @state [:hero :rig :resource])) "Corp has no bad pub, couldn't install")
    (core/gain state :minion :bad-publicity 1)
    (play-from-hand state :hero "Investigative Journalism")
    (take-credits state :hero)
    (take-credits state :minion)
    (card-ability state :hero (get-resource state 0) 0)
    (is (= 0 (:click (get-hero))) "Spent 4 clicks")
    (is (= 1 (count (:discard (get-hero)))) "IJ is trashed")
    (is (= 2 (:bad-publicity (get-minion))) "Corp took 1 bad publicity")))

(deftest john-masanori
  ;; John Masanori - Draw 1 card on first successful run, take 1 tag on first unsuccessful run
  (do-game
    (new-game (default-minion [(qty "Crisium Grid" 1)])
              (default-hero [(qty "John Masanori" 3)
                               (qty "Sure Gamble" 3)
                               (qty "Fall Guy" 1)]))
    (play-from-hand state :minion "Crisium Grid" "HQ")
    (core/rez state :minion (get-content state :hq 0))
    (take-credits state :minion)
    (core/gain state :hero :click 2)
    (play-from-hand state :hero "John Masanori")
    (is (= 4 (count (:hand (get-hero)))))
    (run-empty-server state "HQ")
    (prompt-choice :hero "Yes") ; trash crisium #2433
    (run-empty-server state "Archives")
    (is (= 5 (count (:hand (get-hero)))) "1 card drawn from first successful run")
    (run-empty-server state "Archives")
    (is (= 5 (count (:hand (get-hero)))) "No card drawn from second successful run")
    (run-on state "HQ")
    (run-jack-out state)
    (is (= 1 (:tag (get-hero))) "1 tag taken from first unsuccessful run")
    (run-on state "HQ")
    (run-jack-out state)
    (is (= 1 (:tag (get-hero))) "No tag taken from second unsuccessful run")))

(deftest joshua-b
  ;; Joshua B. - Take 1 tag at turn end if you choose to gain the extra click
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Joshua B." 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Joshua B.")
    (take-credits state :hero)
    (take-credits state :minion)
    (is (= 0 (:click (get-hero))) "Runner has 0 clicks")
    (is (:hero-phase-12 @state) "Runner is in Step 1.2")
    (card-ability state :hero (get-in @state [:hero :rig :resource 0]) 0)
    (is (= 1 (:click (get-hero))) "Gained extra click from Joshua")
    (core/end-phase-12 state :hero nil)
    (is (= 5 (:click (get-hero))) "Gained normal clicks as well")
    (take-credits state :hero)
    (is (= 1 (:tag (get-hero))) "Took 1 tag")))

(deftest kati-jones
  ;; Kati Jones - Click to store and take
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Kati Jones" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Kati Jones")
    (is (= 3 (:credit (get-hero))))
    (let [kati (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero kati 0)
      (is (= 2 (:click (get-hero))))
      (is (= 3 (get-counters (refresh kati) :credit)) "Store 3cr on Kati")
      (card-ability state :hero kati 0)
      (is (= 2 (:click (get-hero))) "Second use of Kati should not be allowed")
      (is (= 3 (get-counters (refresh kati) :credit)) "Second use of Kati should not be allowed")
      (take-credits state :hero 2)
      (is (= 5 (:credit (get-hero))) "Pass turn, take 2cr")
      (take-credits state :minion)
      (card-ability state :hero kati 0)
      (is (= 6 (get-counters (refresh kati) :credit)) "Store 3cr more on Kati")
      (take-credits state :hero 3)
      (is (= 8 (:credit (get-hero))) "Pass turn, take 3cr")
      (take-credits state :minion)
      (card-ability state :hero (refresh kati) 1)
      (is (= 14 (:credit (get-hero))) "Take 6cr from Kati")
      (is (zero? (get-counters (refresh kati) :credit)) "No counters left on Kati"))))

(deftest lewi-guilherme
  ;; Lewi Guilherme - lower minion hand size by 1, pay 1 credit when turn begins or trash
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Lewi Guilherme" 2)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Lewi Guilherme")
    (is (= -1 (:hand-size-modification (get-minion))) "Corp hand size reduced by 1")
    (take-credits state :hero)
    (core/lose state :hero :credit 6)
    (is (= 2 (:credit (get-hero))) "Credits are 2")
    (take-credits state :minion)
    (prompt-choice :hero "Yes")
    (is (= 1 (:credit (get-hero))) "Lost a credit from Lewi")
    (take-credits state :hero)
    (take-credits state :minion)
    (prompt-choice :hero "No")
    (is (= 1 (count (:discard (get-hero)))) "First Lewi trashed")
    (is (= 0 (:hand-size-modification (get-minion))) "Corp hand size normal again")
    (play-from-hand state :hero "Lewi Guilherme")
    (take-credits state :hero)
    (core/lose state :hero :credit 8)
    (is (= 0 (:credit (get-hero))) "Credits are 0")
    (take-credits state :minion)
    (prompt-choice :hero "Yes")
    (is (= 2 (count (:discard (get-hero)))) "Second Lewi trashed due to no credits")))

(deftest london-library
  ;; Install non-virus programs on London library. Includes #325/409
  (do-game
    (new-game (default-minion) (default-hero [(qty "London Library" 1) (qty "Darwin" 1) (qty "Study Guide" 1)
                                              (qty "Chameleon" 1) (qty "Femme Fatale" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :click 2)
    (play-from-hand state :hero "London Library")
    (let [lib (get-in @state [:hero :rig :resource 0])]
      (is (= 0 (count (:hosted (refresh lib)))) "0 programs hosted")
      (card-ability state :hero lib 0) ; Install a non-virus program on London Library
      (prompt-select :hero (find-card "Femme Fatale" (:hand (get-hero))))
      (prompt-choice :hero "Done") ; Cancel out of Femme's bypass
      (is (= 1 (count (:hosted (refresh lib)))) "1 program hosted")
      (card-ability state :hero lib 0)
      (prompt-select :hero (find-card "Study Guide" (:hand (get-hero))))
      (is (= 2 (count (:hosted (refresh lib)))) "2 programs hosted")
      (let [sg (second (:hosted (refresh lib)))]
        (is (= 0 (:current-strength (refresh sg))) "Study Guide at 0 strength")
        (card-ability state :hero sg 1) ; Place 1 power counter
        (is (= 1 (:current-strength (refresh sg))) "Study Guide at 1 strength"))
      (card-ability state :hero lib 0)
      (prompt-select :hero (find-card "Chameleon" (:hand (get-hero))))
      (prompt-choice :hero "Sentry")
      (is (= 3 (count (:hosted (refresh lib)))) "3 programs hosted")
      (is (= 2 (:click (get-hero))) "At 2 clicks")
      (card-ability state :hero lib 0)
      (prompt-select :hero (find-card "Darwin" (:hand (get-hero)))) ; Darwin is a virus
      (is (= 3 (count (:hosted (refresh lib)))) "Still 3 programs hosted")
      (is (= 2 (:click (get-hero))) "Failed Darwin didn't use a click")
      (is (= 1 (count (:hand (get-hero)))))
      (card-ability state :hero lib 1) ; Add a program hosted on London Library to your Grip
      (prompt-card :hero nil)
      (prompt-select :hero (find-card "Study Guide" (:hosted (refresh lib))))
      (is (= 2 (count (:hand (get-hero)))) "Return Study Guide to hand")
      (is (= 2 (count (:hosted (refresh lib)))) "2 programs hosted")
      (card-ability state :hero lib 0)
      (prompt-select :hero (find-card "Study Guide" (:hand (get-hero))))
      (is (= 3 (count (:hosted (refresh lib)))) "3 programs hosted")
      (is (= 0 (count (:discard (get-hero)))) "Nothing in archives yet")
      (take-credits state :hero)
      (is (= 0 (count (:hosted (refresh lib)))) "All programs trashed when turn ends")
      (is (= 2 (count (:hand (get-hero)))) "Darwin never got played, Chameleon returned to hand")
      (is (= 2 (count (:discard (get-hero)))) "Femme Fatale and Study Guide trashed"))))

(deftest muertos-trashed
  ;; Muertos Gang Member - Install and Trash
  (do-game
    (new-game (default-minion [(qty "Tollbooth" 1) (qty "Ice Wall" 1)])
              (default-hero [(qty "Hedge Fund" 3) (qty "Muertos Gang Member" 1)]))
    (play-from-hand state :minion "Tollbooth" "HQ")
    (play-from-hand state :minion "Ice Wall" "Archives")
    (take-credits state :minion)
    (let [toll (get-ice state :hq 0)
          iw (get-ice state :archives 0)]
      (core/rez state :minion iw)
      (core/move state :hero (find-card "Hedge Fund" (:hand (get-hero))) :deck)

      (play-from-hand state :hero "Muertos Gang Member")
      (prompt-select :minion (refresh iw))
      (is (not (:rezzed (refresh iw))) "Ice Wall derezzed")
      (is (= 2 (count (:hand (get-hero)))) "2 cards in Runner's hand")
      (let [muer (get-in @state [:hero :rig :resource 0])]
        (card-ability state :hero muer 0)
        (is (= 3 (count (:hand (get-hero)))) "Runner drew a card from Muertos")
        (prompt-select :minion toll)
        (is (:rezzed (refresh toll)) "Tollbooth was rezzed")))))

(deftest muertos-reina
  ;; Muertos Gang Member - Account for Reina interaction, #1098.
  (do-game
    (new-game (default-minion [(qty "Tollbooth" 1) (qty "Ice Wall" 1)])
              (make-deck "Reina Roja: Freedom Fighter" [(qty "Hedge Fund" 3)
                                                        (qty "Muertos Gang Member" 1)]))
    (play-from-hand state :minion "Tollbooth" "HQ")
    (play-from-hand state :minion "Ice Wall" "Archives")
    (let [toll (get-ice state :hq 0)
          iw (get-ice state :archives 0)]
      (core/rez state :minion iw)
      (take-credits state :minion)
      (core/lose state :minion :credit 100)
      (core/move state :hero (find-card "Hedge Fund" (:hand (get-hero))) :deck)

      (play-from-hand state :hero "Muertos Gang Member")
      (prompt-select :minion (refresh iw))
      (is (not (:rezzed (refresh iw))) "Ice Wall derezzed")
      (is (= 2 (count (:hand (get-hero)))) "2 cards in Runner's hand")
      (let [muer (get-in @state [:hero :rig :resource 0])]
        (card-ability state :hero muer 0)
        (is (= 3 (count (:hand (get-hero)))) "Runner drew a card from Muertos")
        (prompt-select :minion toll)
        (is (:rezzed (refresh toll)) "Tollbooth was rezzed")
        (is (= 0 (:credit (get-minion))) "Corp has 0 credits")))))

(deftest net-mercur
  ;; Net Mercur - Gains 1 credit or draw 1 card when a stealth credit is used
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Net Mercur" 1) (qty "Silencer" 1) (qty "Ghost Runner" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :click 4 :credit 10)
    (play-from-hand state :hero "Silencer")
    (play-from-hand state :hero "Net Mercur")
    (play-from-hand state :hero "Ghost Runner")
    (let [sil (get-hardware state 0)
          nm (get-resource state 0)
          gr (get-resource state 1)]
      (card-ability state :hero gr 0)
      (is (empty? (:prompt (get-hero))) "No Net Mercur prompt from stealth spent outside of run")
      (run-on state :hq)
      (card-ability state :hero sil 0)
      (prompt-choice :hero "Place 1 [Credits]")
      (is (= 1 (get-counters (refresh nm) :credit)) "1 credit placed on Net Mercur")
      (card-ability state :hero gr 0)
      (is (empty? (:prompt (get-hero))) "No Net Mercur prompt for 2nd stealth in run")
      (run-jack-out state)
      (take-credits state :hero)
      (take-credits state :minion)
      (run-on state :hq)
      (card-ability state :hero nm 0)
      (is (= "Net Mercur" (:title (:card (first (get-in @state [:hero :prompt]))))) "Net Mercur triggers itself"))))

(deftest network-exchange
  ;; ICE install costs 1 more except for inner most
  (do-game
    (new-game (default-minion [(qty "Paper Wall" 3)])
              (default-hero [(qty "Network Exchange" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Network Exchange")
    (take-credits state :hero)
    (play-from-hand state :minion "Paper Wall" "HQ")
    (is (= 8 (:credit (get-minion))) "Paid 0 to install Paper Wall")
    (play-from-hand state :minion "Paper Wall" "HQ")
    (is (= 6 (:credit (get-minion))) "Paid 1 extra  to install Paper Wall")
    (play-from-hand state :minion "Paper Wall" "HQ")
    (is (= 3 (:credit (get-minion))) "Paid 1 extra  to install Paper Wall")))

(deftest network-exchange-architect
  ;; Architect 1st sub should ignore additional install cose
  (do-game
    (new-game (default-minion [(qty "Architect" 3)])
              (default-hero [(qty "Network Exchange" 1)]))
    (play-from-hand state :minion "Architect" "HQ")
    (take-credits state :minion) ; minion has 7 credits
    (play-from-hand state :hero "Network Exchange")
    (take-credits state :hero)
    (let [architect (get-ice state :hq 0)]
      (core/rez state :minion architect)
      (is (= 3 (:credit (get-minion))) "Corp has 3 credits after rez")
      (core/move state :minion (find-card "Architect" (:hand (get-minion))) :deck)
      (card-subroutine state :minion architect 0)
      (prompt-choice :minion (find-card "Architect" (:deck (get-minion))))
      (prompt-choice :minion "HQ")
      (is (= 3 (:credit (get-minion))) "Corp has 7 credits"))))

(deftest neutralize-all-threats
  ;; Neutralize All Threats - Access 2 cards from HQ, force trash first accessed card with a trash cost
  (do-game
    (new-game (default-minion [(qty "Hedge Fund" 2) (qty "Breaker Bay Grid" 1) (qty "Elizabeth Mills" 1)])
              (default-hero [(qty "Neutralize All Threats" 1)]))
    (play-from-hand state :minion "Breaker Bay Grid" "New remote")
    (play-from-hand state :minion "Elizabeth Mills" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Neutralize All Threats")
    (run-empty-server state "HQ")
    (prompt-choice :hero "Card from hand")
    (prompt-choice :hero "OK") ; access first Hedge Fund
    (prompt-choice :hero "Card from hand")
    (prompt-choice :hero "OK") ; access second Hedge Fund
    (run-empty-server state "Server 1")
    (is (= 3 (:credit (get-hero))) "Forced to pay 2c to trash BBG")
    (is (= 1 (count (:discard (get-minion)))) "Breaker Bay Grid trashed")
    (run-empty-server state "Server 2")
    (is (not (empty? (:prompt (get-hero)))) "Runner prompt to trash Elizabeth Mills")))

(deftest new-angeles-city-hall
  ;; New Angeles City Hall - Avoid tags; trash when agenda is stolen
  (do-game
    (new-game (default-minion [(qty "SEA Source" 1) (qty "Breaking News" 1)])
              (default-hero [(qty "New Angeles City Hall" 1)]))
    (play-from-hand state :minion "Breaking News" "New remote")
    (take-credits state :minion 2)
    (play-from-hand state :hero "New Angeles City Hall")
    (let [nach (get-in @state [:hero :rig :resource 0])]
      (run-empty-server state "Archives")
      (take-credits state :hero)
      (is (= 6 (:credit (get-hero))))
      (play-from-hand state :minion "SEA Source")
      (prompt-choice :minion 0) ; default trace
      (prompt-choice :hero 0) ; Runner won't match
      (card-ability state :hero nach 0)
      (prompt-choice :hero "Done")
      (is (= 0 (:tag (get-hero))) "Avoided SEA Source tag")
      (is (= 4 (:credit (get-hero))) "Paid 2 credits")
      (take-credits state :minion)
      (run-empty-server state "Server 1")
      (prompt-choice :hero "Steal")
      (is (= 1 (:agenda-point (get-hero))))
      (is (empty? (get-in @state [:hero :rig :resource])) "NACH trashed by agenda steal"))))

(deftest new-angeles-city-hall-siphon
  ;; New Angeles City Hall - don't gain Siphon credits until opportunity to avoid tags has passed
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Account Siphon" 1) (qty "New Angeles City Hall" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "New Angeles City Hall")
    (play-run-event state (first (:hand (get-hero))) :hq)
    (prompt-choice :hero "Run ability")
    (let [nach (get-in @state [:hero :rig :resource 0])]
      (is (= 4 (:credit (get-hero))) "Have not gained Account Siphon credits until tag avoidance window closes")
      (card-ability state :hero nach 0)
      (card-ability state :hero nach 0)
      (prompt-choice :hero "Done")
      (is (= 0 (:tag (get-hero))) "Tags avoided")
      (is (= 10 (:credit (get-hero))) "10 credits siphoned")
      (is (= 3 (:credit (get-minion))) "Corp lost 5 credits"))))

(deftest off-campus-apartment-simultaneous
  ;; Off-Campus Apartment - ability shows a simultaneous resolution prompt when appropriate
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Street Peddler" 1) (qty "Off-Campus Apartment" 1)
                               (qty "Underworld Contact" 1) (qty "Spy Camera" 6)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Street Peddler" "Off-Campus Apartment" "Underworld Contact"])
    (play-from-hand state :hero "Off-Campus Apartment")
    (let [oca (get-resource state 0)]
      (card-ability state :hero oca 0)
      (prompt-select :hero (find-card "Underworld Contact" (:hand (get-hero))))
      (is (= 2 (count (:hand (get-hero)))) "Drew a card from OCA")
      (card-ability state :hero oca 0)
      (prompt-select :hero (find-card "Street Peddler" (:hand (get-hero))))
      ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
      (is (= 2 (-> (get-hero) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
      (prompt-choice :hero "Off-Campus Apartment")
      (is (= 2 (count (:hand (get-hero)))) "Drew a card from OCA"))))

(deftest off-campus-peddler
  ;; Off-Campus Apartment - second ability does not break cards that are hosting others, e.g., Street Peddler
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Street Peddler" 2) (qty "Off-Campus Apartment" 1) (qty "Spy Camera" 6)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Street Peddler" "Street Peddler" "Off-Campus Apartment"])
    (core/move state :hero (find-card "Street Peddler" (:hand (get-hero))) :deck {:front true})
    (play-from-hand state :hero "Off-Campus Apartment")
    (let [oca (get-resource state 0)]
      (card-ability state :hero oca 0)
      (prompt-select :hero (find-card "Street Peddler" (:hand (get-hero))))
      (prompt-choice :hero "Street Peddler")
      (let [ped1 (first (:hosted (refresh oca)))]
        (card-ability state :hero ped1 0)
        (prompt-card :hero (-> (get-hero) :prompt first :choices second)) ; choose Street Peddler
        (card-ability state :hero (refresh oca) 1)
        (prompt-select :hero (get-resource state 1))
        (let [ped2 (first (:hosted (refresh oca)))]
          (card-ability state :hero ped2 0)
          (prompt-card :hero (-> (get-hero) :prompt first :choices first)) ; choose Spy Camera
          ;; the fact that we got this far means the bug is fixed
          (is (= 1 (count (get-hardware state))) "Spy Camera installed"))))))

(deftest officer-frank
  ;; Officer Frank - meat damage to trash 2 from HQ
  (do-game
      (new-game (default-minion [(qty "Swordsman" 1) (qty "Hedge Fund" 2)])
                (default-hero [(qty "Officer Frank" 1) (qty "Skulljack" 1) (qty "Respirocytes" 4)]))
   (play-from-hand state :minion "Swordsman" "Archives")
   (take-credits state :minion)
   (starting-hand state :hero ["Officer Frank" "Skulljack" "Respirocytes" "Respirocytes" "Respirocytes" "Respirocytes"])
   (play-from-hand state :hero "Officer Frank")
   (card-ability state :hero (get-resource state 0) 0)
   (is (= 0 (count (:discard (get-minion)))) "Nothing discarded from HQ")
   (play-from-hand state :hero "Skulljack")
   (is (= 3 (count (:hand (get-hero)))) "Took 1 brain damage")
   (card-ability state :hero (get-resource state 0) 0)
   (is (= 0 (count (:discard (get-minion)))) "Nothing discarded from HQ")
   (let [sm (get-ice state :archives 0)]
     (run-on state :archives)
     (core/rez state :minion sm)
     (card-subroutine state :minion sm 0)
     (run-jack-out state))
   (is (= 2 (count (:hand (get-hero)))) "Took 1 net damage")
   (card-ability state :hero (get-resource state 0) 0)
   (is (= 0 (count (:discard (get-minion)))) "Nothing discarded from HQ")
   (play-from-hand state :hero "Respirocytes")
   (is (= 0 (count (:hand (get-hero)))) "Took 1 meat damage")
   (card-ability state :hero (get-resource state 0) 0)
   (is (= 2 (count (:discard (get-minion)))) "Two cards trashed from HQ")))

(deftest paige-piper-frantic-coding
  ;; Paige Piper - interaction with Frantic Coding. Issue #2190.
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Paige Piper" 1) (qty "Frantic Coding" 2) (qty "Sure Gamble" 3)
                               (qty "Gordian Blade" 2) (qty "Ninja" 1) (qty "Bank Job" 3) (qty "Indexing" 2)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Paige Piper" "Frantic Coding" "Frantic Coding"])
    (play-from-hand state :hero "Paige Piper")
    (prompt-choice :hero "No")
    (take-credits state :hero) ; now 8 credits
    (take-credits state :minion)
    (play-from-hand state :hero "Frantic Coding")
    (prompt-choice :hero "OK")
    (prompt-card :hero (find-card "Gordian Blade" (:deck (get-hero))))
    (is (= 1 (count (get-program state))) "Installed Gordian Blade")
    (prompt-choice :hero "Yes")
    (prompt-choice :hero "0")
    (is (= 1 (count (:discard (get-hero)))) "Paige Piper intervention stopped Frantic Coding from trashing 9 cards")
    (is (= 5 (:credit (get-hero))) "No charge to install Gordian")
    ;; a second Frantic Coding will not trigger Paige (once per turn)
    (play-from-hand state :hero "Frantic Coding")
    (prompt-choice :hero "OK")
    (prompt-card :hero (find-card "Ninja" (:deck (get-hero))))
    (is (= 2 (count (get-program state))) "Installed Ninja")
    (is (= 11 (count (:discard (get-hero)))) "11 cards in heap")
    (is (= 2 (:credit (get-hero))) "No charge to install Ninja")))

(deftest patron
  ;; Patron - Ability
  (do-game
    (new-game (default-minion [(qty "Jackson Howard" 1)])
              (default-hero [(qty "Patron" 4) (qty "Easy Mark" 4)]))
    (play-from-hand state :minion "Jackson Howard" "New remote")
    (take-credits state :minion 2)
    (play-from-hand state :hero "Patron")
    (let [p (get-in @state [:hero :rig :resource 0])]
      (take-credits state :hero 3)
      (take-credits state :minion)
      (prompt-choice :hero "Server 1")
      (is (= 4 (count (:hand (get-hero)))) "Starts with 4 cards")
      (run-empty-server state "Server 1")
      (is (= 6 (count (:hand (get-hero)))) "Drew 2 cards")
      (run-empty-server state "Server 1")
      (prompt-choice :hero "No")
      (is (= 6 (count (:hand (get-hero)))) "Drew no cards")
      (play-from-hand state :hero "Easy Mark")
      (take-credits state :hero)
      (take-credits state :minion)
      (prompt-choice :hero "Server 1")
      (run-empty-server state "Archives")
      (is (= 5 (count (:hand (get-hero)))) "Did not draw cards when running other server"))))

(deftest patron-manual
  ;; Patron - Manually selecting during Step 1.2 does not show a second prompt at start of turn. Issue #1744.
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Patron" 3) (qty "Jak Sinclair" 3)]))
    (take-credits state :minion)
    (core/gain state :hero :credit 10)
    (starting-hand state :hero ["Patron" "Jak Sinclair"])
    (play-from-hand state :hero "Patron")
    (play-from-hand state :hero "Jak Sinclair")
    (take-credits state :hero)
    (let [p (get-resource state 0)
          j (get-resource state 1)]
      (take-credits state :minion)
      (is (:hero-phase-12 @state) "Runner in Step 1.2")
      (card-ability state :hero p 0)
      (prompt-choice :hero "Archives")
      (card-ability state :hero j 0)
      (prompt-choice :hero "Archives")
      (run-successful state)
      (core/end-phase-12 state :hero nil)
      (is (empty? (:prompt (get-hero))) "No second prompt for Patron - used already"))))

(deftest professional-contacts
  ;; Professional Contacts - Click to gain 1 credit and draw 1 card
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Professional Contacts" 3)
                               (qty "Sure Gamble" 2)
                               (qty "Shiv" 2)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Professional Contacts")
    (let [proco (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero proco 0)
      (is (= 2 (:click (get-hero))) "Spent 1 click")
      (is (= 1 (:credit (get-hero))) "Gained 1 credit")
      (is (= 5 (count (:hand (get-hero)))) "Drew 1 card")
      (card-ability state :hero proco 0)
      (is (= 1 (:click (get-hero))) "Spent 1 click")
      (is (= 2 (:credit (get-hero))) "Gained 1 credit")
      (is (= 6 (count (:hand (get-hero)))) "Drew 1 card"))))

(deftest rolodex
  ;; Rolodex - Full test
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Rolodex" 1) (qty "Sure Gamble" 1) (qty "Desperado" 1)
                               (qty "Diesel" 1) (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :hero ["Rolodex"])
    (is (= 1 (count (:hand (get-hero)))))
    (take-credits state :minion)
    (play-from-hand state :hero "Rolodex")
    (prompt-choice :hero (find-card "Sure Gamble" (:deck (get-hero))))
    (prompt-choice :hero (find-card "Desperado" (:deck (get-hero))))
    (prompt-choice :hero (find-card "Diesel" (:deck (get-hero))))
    (prompt-choice :hero (find-card "Corroder" (:deck (get-hero))))
    (prompt-choice :hero (find-card "Patron" (:deck (get-hero))))
    ;; try starting over
    (prompt-choice :hero "Start over")
    (prompt-choice :hero (find-card "Patron" (:deck (get-hero))))
    (prompt-choice :hero (find-card "Corroder" (:deck (get-hero))))
    (prompt-choice :hero (find-card "Diesel" (:deck (get-hero))))
    (prompt-choice :hero (find-card "Desperado" (:deck (get-hero))))
    (prompt-choice :hero (find-card "Sure Gamble" (:deck (get-hero)))) ;this is the top card on stack
    (prompt-choice :hero "Done")
    (is (= "Sure Gamble" (:title (first (:deck (get-hero))))))
    (is (= "Desperado" (:title (second (:deck (get-hero))))))
    (is (= "Diesel" (:title (second (rest (:deck (get-hero)))))))
    (is (= "Corroder" (:title (second (rest (rest (:deck (get-hero))))))))
    (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-hero)))))))))
    (core/trash state :hero (get-resource state 0))
    (is (last-log-contains? state "Sure Gamble, Desperado, Diesel")
        "Rolodex did log trashed card names")
    (is (= 4 (count (:discard (get-hero)))) "Rolodex mills 3 cards when trashed")
    (is (= "Corroder" (:title (first (:deck (get-hero))))))))

(deftest sacrificial-construct
  ;; Sacrificial Construct - Trash to prevent trash of installed program or hardware
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Sacrificial Construct" 2) (qty "Cache" 1)
                               (qty "Motivation" 1) (qty "Astrolabe" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :click 1)
    (play-from-hand state :hero "Sacrificial Construct")
    (play-from-hand state :hero "Sacrificial Construct")
    (play-from-hand state :hero "Cache")
    (play-from-hand state :hero "Motivation")
    (play-from-hand state :hero "Astrolabe")
    (take-credits state :hero)
    (core/trash state :hero (get-resource state 2))
    (is (empty? (:prompt (get-hero))) "Sac Con not prompting to prevent resource trash")
    (core/trash state :hero (get-program state 0))
    (card-ability state :hero (get-resource state 0) 0)
    (is (= 2 (count (:discard (get-hero)))) "Sac Con trashed")
    (is (= 1 (count (get-in @state [:hero :rig :program]))) "Cache still installed")
    (core/trash state :hero (get-hardware state 0))
    (card-ability state :hero (get-resource state 0) 0)
    (is (= 3 (count (:discard (get-hero)))) "Sac Con trashed")
    (is (= 1 (count (get-in @state [:hero :rig :hardware]))) "Astrolabe still installed")))

(deftest safety-first
  ;; Safety First - Reduce hand size by 2, draw 1 at turn end if below maximum
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Safety First" 3) (qty "Cache" 3)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Safety First" "Safety First" "Cache"])
    (play-from-hand state :hero "Safety First")
    (is (= 3 (core/hand-size state :hero)) "Max hand size reduced by 2")
    (take-credits state :hero)
    (is (= 3 (count (:hand (get-hero)))) "Drew 1 card at end of turn")
    (take-credits state :minion)
    (take-credits state :hero)
    (is (= 3 (count (:hand (get-hero)))) "Drew no cards, at maximum")))

(deftest salsette-slums
  ;; Salsette Slums - Once per turn, when the trash cost of a card is paid, optionally remove from the game
  (do-game
    (new-game (default-minion [(qty "Hostile Infrastructure" 1) (qty "Tech Startup" 1) (qty "Thomas Haas" 1)
                             (qty "Hedge Fund" 3)])
              (default-hero [(qty "Salsette Slums" 2) (qty "Sure Gamble" 3)]))
    ;; Use Hostile Infrastructure to ensure on-trash effects don't fire.
    (core/move state :minion (find-card "Hostile Infrastructure" (:deck (get-minion))) :hand)
    (core/move state :minion (find-card "Tech Startup" (:deck (get-minion))) :hand)
    (core/move state :minion (find-card "Thomas Haas" (:deck (get-minion))) :hand)
    (play-from-hand state :minion "Tech Startup" "New remote")
    (play-from-hand state :minion "Hostile Infrastructure" "New remote")
    (play-from-hand state :minion "Thomas Haas" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Salsette Slums")
    (play-from-hand state :hero "Salsette Slums")
    (core/gain state :hero :credit 2)
    (core/gain state :hero :click 4)
    (let [ts1 (get-content state :remote1 0)
          hostile2 (get-content state :remote2 0)
          th3 (get-content state :remote3 0)
          salsette1 (get-resource state 0)
          salsette2 (get-resource state 1)]
      (is (= 3 (count (:hand (get-hero)))) "Runner started this part with three cards in hand")
      (core/rez state :minion hostile2)
      (run-empty-server state "Server 1")
      (is (not (empty? (:prompt (get-hero)))) "Prompting to trash.")
      (card-ability state :hero salsette1 0)
      (is (empty? (:prompt (get-hero))) "All prompts done")
      (is (= 3 (count (:hand (get-hero)))) "On-trash ability of other Hostile didn't fire")
      (is (= (:cid ts1) (:cid (last (:rfg (get-minion))))) "Tech Startup was removed from game")
      (is (= 2 (:credit (get-hero))) "Runner paid the trash cost.")
      (is (not (:run @state)) "Run is over")
      (run-empty-server state :remote2)
      (is (not (empty? (:prompt (get-hero)))) "Prompting to trash")
      ;; Only able to use the ability once per turn
      (card-ability state :hero salsette1 0)
      (is (not (empty? (:prompt (get-hero)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      ;; Can't use the ability if you can't afford to trash
      (card-ability state :hero salsette2 0)
      (is (not (empty? (:prompt (get-hero)))) "Still prompting to trash")
      (is (:run @state) "Run is still occurring")
      (prompt-choice :hero "No")
      ;; Test the "oops I forgot" ability (hero feels bad that they forgot to use Slums when a Hostile is out)
      (run-empty-server state :remote3)
      (prompt-choice :hero "Yes")
      ;; Can only use that first Slums once
      (card-ability state :hero salsette1 1)
      (is (empty? (:prompt (get-hero))) "Not prompting the hero")
      (is (not (= (:cid th3) (:cid (last (:rfg (get-minion)))))) "Card was not removed from the game")
      (card-ability state :hero salsette2 1)
      (is (not (empty? (:prompt (get-hero)))) "Prompting the hero to choose a card")
      (prompt-select :hero (find-card "Thomas Haas" (:discard (get-minion))))
      (is (= (:cid th3) (:cid (last (:rfg (get-minion))))) "Card was removed from the game"))
    ;; Set things up so we can trash the Hostile and then make sure we can't "oops I forgot on a later turn"
    (core/gain state :hero :credit 5)
    (run-empty-server state :remote2)
    (prompt-choice :hero "Yes")
    (take-credits state :hero)
    (take-credits state :minion)
    (let [salsette1 (get-resource state 0)
          hostile2 (get-content state :remote2 0)]
      (card-ability state :hero salsette1 1)
      (prompt-select :hero (find-card "Hostile Infrastructure" (:discard (get-minion))))
      (is (not (= (:cid hostile2) (:cid (last (:rfg (get-minion)))))) "Did not remove card from game"))))

(deftest security-testing
  ;; Security Testing - Ability
  (do-game
    (new-game (default-minion [(qty "Jackson Howard" 1)])
              (default-hero [(qty "Security Testing" 1)]))
    (play-from-hand state :minion "Jackson Howard" "New remote")
    (take-credits state :minion 2)
    (play-from-hand state :hero "Security Testing")
    (let [st (get-in @state [:hero :rig :resource 0])]
      (take-credits state :hero 3)
      (take-credits state :minion)
      (prompt-choice :hero "Server 1")
      (run-empty-server state "Server 1")
      (is (= 10 (:credit (get-hero))) "Gained 2 credits from Security Testing")
      (run-empty-server state "Server 1")
      (prompt-choice :hero "No")
      (is (= 10 (:credit (get-hero))) "Did not gain credits on second run")
      (take-credits state :hero 2)
      (take-credits state :minion)
      (prompt-choice :hero "Server 1")
      (run-empty-server state "Archives")
      (is (= 12 (:credit (get-hero))) "Did not gain credits when running other server"))))

(deftest security-testing-multiple
  ;; Security Testing - multiple copies
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Security Testing" 2)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Security Testing")
    (play-from-hand state :hero "Security Testing")
    (take-credits state :hero)
    (take-credits state :minion)
    (prompt-choice :hero "Archives")
    (prompt-choice :hero "R&D")
    (run-empty-server state "Archives")
    (is (= 9 (:credit (get-hero))) "Gained 2 credits")
    (run-empty-server state "R&D")
    (is (= 11 (:credit (get-hero))))))

(deftest spoilers
  ;; Spoilers - Mill the Corp when it scores an agenda
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 1) (qty "Hedge Fund" 1)])
              (default-hero [(qty "Spoilers" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Spoilers")
    (take-credits state :hero)
    (core/move state :minion (find-card "Hedge Fund" (:hand (get-minion))) :deck)
    (is (= 1 (count (:deck (get-minion)))))
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (score-agenda state :minion ht)
      (is (= 1 (count (:discard (get-minion)))))
      (is (= 0 (count (:deck (get-minion)))) "Last card from R&D milled"))))

(deftest stim-dealer
  ;; Stim Dealer - Take 1 brain damage when it accumulates 2 power counters
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Stim Dealer" 1) (qty "Sure Gamble" 1) (qty "Feedback Filter" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Sure Gamble")
    (play-from-hand state :hero "Feedback Filter")
    (play-from-hand state :hero "Stim Dealer")
    (take-credits state :hero)
    (take-credits state :minion)
    (let [sd (get-resource state 0)]
      (is (= 1 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-hero))) "Gained 1 click")
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 2 (get-counters (refresh sd) :power)) "Gained 1 counter")
      (is (= 5 (:click (get-hero))) "Gained 1 click")
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 0 (get-counters (refresh sd) :power)) "Lost all counters")
      (is (empty? (:prompt (get-hero))) "No Feedback Filter brain dmg prevention possible")
      (is (= 1 (:brain-damage (get-hero))) "Took 1 brain damage")
      (is (= 4 (:click (get-hero))) "Didn't gain extra click"))))

(deftest street-peddler-ability
  ;; Street Peddler - Ability
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Street Peddler" 1)
                               (qty "Gordian Blade" 1)
                               (qty "Torch" 1)
                               (qty "Sure Gamble" 2)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Street Peddler" "Sure Gamble"])
    (play-from-hand state :hero "Street Peddler")
    (let [sp (get-in @state [:hero :rig :resource 0])]
      (is (= 3 (count (:hosted sp))) "Street Peddler is hosting 3 cards")
      (card-ability state :hero sp 0)
      (prompt-card :hero (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:hero :rig :program 0])))
          "Gordian Blade was installed")
      (is (= 3 (:memory (get-hero))) "Gordian cost 1 mu"))))

(deftest street-peddler-cant-afford
  ;; Street Peddler - Can't afford install
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Street Peddler" 1) (qty "Gordian Blade" 3)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Street Peddler"])
    (play-from-hand state :hero "Street Peddler")
    (let [sp (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero sp 0)
      (core/lose state :hero :credit 3)
      (is (= 2 (count (:choices (first (:prompt (get-hero))))))
          "1 card and 1 cancel option on Street Peddler")
      (prompt-card :hero (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (zero? (count (get-in @state [:hero :rig :program])))
          "Gordian Blade was not installed")
      (is (and (:installed (refresh sp)) (= 3 (count (:hosted (refresh sp))))
               "Street Peddler still installed with 3 hosted cards")))))

(deftest street-peddler-kate-discount
  ;; Street Peddler - Interaction with Kate discount
  (do-game
    (new-game (default-minion)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Street Peddler" 1)
                                                                   (qty "Gordian Blade" 1)
                                                                   (qty "Sure Gamble" 2)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Street Peddler"])
    (play-from-hand state :hero "Street Peddler")
    (let [sp (get-in @state [:hero :rig :resource 0])]
      ;; should still be able to afford Gordian w/ Kate discount
      (core/lose state :hero :credit 3)
      (card-ability state :hero sp 0)
      (is (= 2 (count (:choices (first (:prompt (get-hero))))))
          "Only 1 choice (plus Cancel) to install off Peddler")
      (prompt-card :hero (find-card "Gordian Blade" (:hosted sp))) ; choose to install Gordian
      (is (= "Gordian Blade" (:title (get-in @state [:hero :rig :program 0])))
          "Gordian Blade was installed")
      (is (= 3 (:memory (get-hero))) "Gordian cost 1 mu"))))

(deftest street-peddler-memory-units
  ;; Street Peddler - Programs Should Cost Memory. Issue #708
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Street Peddler" 1) (qty "Corroder" 3)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Street Peddler"])
    (play-from-hand state :hero "Street Peddler")
    (is (= 4 (:memory (get-hero))) "No memory cost for hosting on Street Peddler")
    (let [sp (get-in @state [:hero :rig :resource 0])]
      (is (= "Corroder" (:title (first (:hosted sp)))) "Street Peddler is hosting Corroder")
      (card-ability state :hero sp 0)
      (prompt-card :hero (first (:hosted sp))) ; choose to install Gordian
      (is (= "Corroder" (:title (get-in @state [:hero :rig :program 0])))
          "Corroder was installed")
      (is (= 3 (:memory (get-hero))) "Corroder cost 1 mu"))))

(deftest street-peddler-muertos-brain-chip
  ;; Muertos/Brain Chip uninstall effect not fired when removed off peddler/hosting Issue #2294+#2358
  (do-game
    (new-game (default-minion [(qty "Jackson Howard" 1)])
              (default-hero [(qty "Street Peddler" 2)(qty "Muertos Gang Member" 1) (qty "Brain Chip" 1)]))
    (core/move state :hero (find-card "Muertos Gang Member" (:hand (get-hero))) :deck {:front true})
    (core/move state :hero (find-card "Brain Chip" (:hand (get-hero))) :deck {:front true})
    (core/move state :hero (find-card "Street Peddler" (:hand (get-hero))) :deck {:front true})
    (play-from-hand state :minion "Jackson Howard" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Street Peddler")
    (core/gain state :hero :agenda-point 1)
    (let [jh (get-content state :remote1 0)
          sp (get-in @state [:hero :rig :resource 0])]
      (core/rez state :minion jh)
      (card-ability state :hero sp 0)
      (prompt-card :hero (find-card "Street Peddler" (:hosted sp))) ; choose to another Peddler
      (is (empty? (:prompt (get-minion))) "Corp not prompted to rez Jackson")
      (is (= 4 (:memory (get-hero))) "Runner has 4 MU"))))

(deftest street-peddler-in-play-effects
  ;; Street Peddler - Trashing hardware should not reduce :in-play values
  (do-game
   (new-game (default-minion)
             (default-hero [(qty "Street Peddler" 1) (qty "HQ Interface" 3)]))
   (take-credits state :minion)
   (starting-hand state :hero ["Street Peddler"])
   (play-from-hand state :hero "Street Peddler")
   (let [sp (get-in @state [:hero :rig :resource 0])]
     (card-ability state :hero sp 0)
     (prompt-card :hero (first (:hosted sp))) ; choose to install HQ Interface
     (is (= 2 (:hq-access (get-hero)))
         "HQ Access increased by 1 from installed HQI and not reduced by the 2 trashed ones"))))

(deftest street-peddler-parasite-1cr
  ;; Street Peddler - Installing Parasite with only 1cr. Issue #491.
  (do-game
    (new-game (default-minion [(qty "Pop-up Window" 3)])
              (default-hero [(qty "Street Peddler" 1) (qty "Parasite" 3)]))
    (play-from-hand state :minion "Pop-up Window" "HQ")
    (take-credits state :minion 2)
    (starting-hand state :hero ["Street Peddler"])
    (core/lose state :hero :credit 4) ; go down to 1 credit
    (is (= 1 (:credit (get-hero))) "Runner has 1 credit")
    (play-from-hand state :hero "Street Peddler")
    (let [sp (get-in @state [:hero :rig :resource 0])
          pu (get-ice state :hq 0)]
      (core/rez state :minion pu)
      (card-ability state :hero sp 0)
      (prompt-card :hero (first (:hosted sp))) ; choose to install Parasite
      (is (= "Parasite" (:title (:card (first (get-in @state [:hero :prompt])))))
          "Parasite target prompt")
      (prompt-select :hero pu)
      (is (= 4 (count (:discard (get-hero)))) "3 Parasite, 1 Street Peddler in heap")
      (is (= 1 (count (:discard (get-minion)))) "Pop-up Window in archives"))))

(deftest street-peddler-tech-trader
  ;; Street Peddler - Tech Trader install
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Street Peddler" 1)
                               (qty "Tech Trader" 1)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Street Peddler"])
    (play-from-hand state :hero "Street Peddler")
    (let [sp (get-in @state [:hero :rig :resource 0])]
      (is (= 1 (count (:hosted sp))) "Street Peddler is hosting 1 card")
      (card-ability state :hero sp 0)
      (prompt-card :hero (find-card "Tech Trader" (:hosted sp))) ; choose to install Tech Trader
      (is (= "Tech Trader" (:title (get-in @state [:hero :rig :resource 0])))
          "Tech Trader was installed")
      (is (= 5 (:credit (get-hero))) "Did not gain 1cr from Tech Trader ability"))))

(deftest-pending street-peddler-trash-while-choosing-card
  ;; Street Peddler - trashing Street Peddler while choosing which card to
  ;; discard should dismiss the choice prompt. Issue #587.
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Street Peddler" 1)
                               (qty "Gordian Blade" 1)
                               (qty "Torch" 1)
                               (qty "Sure Gamble" 2)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Street Peddler" "Sure Gamble"])
    (play-from-hand state :hero "Street Peddler")
    (let [street-peddler (get-in @state [:hero :rig :resource 0])]
      (is (= 3 (count (:hosted street-peddler))) "Street Peddler is hosting 3 cards")
      (card-ability state :hero street-peddler 0)
      (trash-resource state "Street Peddler")
      (is (zero? (count (get-in @state [:hero :prompt])))))))

(deftest symmetrical-visage
  ;; Symmetrical Visage - Gain 1 credit the first time you click to draw each turn
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Symmetrical Visage" 3)
                               (qty "Sure Gamble" 3)
                               (qty "Fall Guy" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Symmetrical Visage")
    (is (= 3 (:credit (get-hero))))
    (core/click-draw state :hero nil)
    (is (= 4 (:credit (get-hero))) "Gained 1 credit from first click spent to draw")
    (core/click-draw state :hero nil)
    (is (= 4 (:credit (get-hero))) "No credit gained from second click spent to draw")))

(deftest symmetrical-visage-gcs
  ;; Symmetrical Visage - Gain 1 credit the first and second time you click to draw each turn when GCS is installed
  (do-game
   (new-game (default-minion)
             (default-hero [(qty "Symmetrical Visage" 3)
                              (qty "Gene Conditioning Shoppe" 3)
                              (qty "Fall Guy" 1)]))
   (take-credits state :minion)
   (core/gain state :hero :click 1)
   (play-from-hand state :hero "Symmetrical Visage")
   (is (= 3 (:credit (get-hero))))
   (play-from-hand state :hero "Gene Conditioning Shoppe")
   (is (= 1 (:credit (get-hero))))
   (core/click-draw state :hero nil)
   (is (= 2 (:credit (get-hero))) "Gained 1 credit from first click spent to draw")
   (core/click-draw state :hero nil)
   (is (= 3 (:credit (get-hero)))
       "Gained 1 credit from second click spent to draw with Gene Conditioning Shoppe")
   ;; Move Fall Guy back to deck
   (core/move state :hero (find-card "Fall Guy" (:hand (get-hero))) :deck)
   (core/click-draw state :hero nil)
   (is (= 3 (:credit (get-hero)))
       "No credit gained from third click spent to draw with Gene Conditioning Shoppe")))

(deftest synthetic-blood
  ;; Synthetic Blood - The first time you take damage each turn, draw one card
  (do-game
   (new-game (default-minion [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
             (default-hero [(qty "Synthetic Blood" 3)
                              (qty "Sure Gamble" 3)
                              (qty "Fall Guy" 1)]))
   (play-from-hand state :minion "Data Mine" "HQ")
   (play-from-hand state :minion "Data Mine" "HQ")
   (take-credits state :minion)
   (let [first-dm (get-ice state :hq 1)
         second-dm (get-ice state :hq 0)]
     (play-from-hand state :hero "Synthetic Blood")
     (run-on state "HQ")
     (core/rez state :minion first-dm)
     (card-subroutine state :minion first-dm 0)
     (is (= 4 (count (:hand (get-hero)))) "1 card drawn when receiving damage (1st time)")
     (run-continue state)
     (core/rez state :minion second-dm)
     (card-subroutine state :minion second-dm 0)
     (is (= 3 (count (:hand (get-hero)))) "no card drawn when receiving damage (2nd time)"))))

(deftest synthetic-blood-gcs
  ;; Synthetic Blood - The first and second time you take damage each turn (with GCS installed), draw one card
  (do-game
   (new-game (default-minion [(qty "Data Mine" 3) (qty "Hedge Fund" 3)])
             (default-hero [(qty "Synthetic Blood" 3)
                              (qty "Sure Gamble" 1)
                              (qty "Gene Conditioning Shoppe" 3)]))
   (play-from-hand state :minion "Data Mine" "HQ")
   (play-from-hand state :minion "Data Mine" "HQ")
   (take-credits state :minion)
   (let [first-dm (get-ice state :hq 1)
         second-dm (get-ice state :hq 0)]
     (play-from-hand state :hero "Synthetic Blood")
     (play-from-hand state :hero "Gene Conditioning Shoppe")
     (run-on state "HQ")
     (core/rez state :minion first-dm)
     (card-subroutine state :minion first-dm 0)
     (is (= 3 (count (:hand (get-hero)))) "1 card drawn when receiving damage (1st time)")
     (run-continue state)
     (core/rez state :minion second-dm)
     (card-subroutine state :minion second-dm 0)
     (is (= 3 (count (:hand (get-hero)))) "1 card drawn when receiving damage (2nd time)"))))

(deftest technical-writer
  ;; Technical Writer - Gain 1c per program/hardware install; click/trash to take all credits
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Technical Writer" 1) (qty "Faerie" 2)
                               (qty "Vigil" 1) (qty "Same Old Thing" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :click 2)
    (play-from-hand state :hero "Technical Writer")
    (let [tw (get-resource state 0)]
      (play-from-hand state :hero "Faerie")
      (is (= 1 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :hero "Faerie")
      (is (= 2 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :hero "Vigil")
      (is (= 3 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :hero "Same Old Thing")
      (is (= 3 (get-counters (refresh tw) :credit)) "No credit gained for resource install")
      (card-ability state :hero tw 0)
      (is (= 6 (:credit (get-hero))) "Gained 3 credits")
      (is (= 0 (:click (get-hero))) "Spent 1 click")
      (is (= 1 (count (:discard (get-hero)))) "Technical Writer trashed"))))

(deftest the-helpful-ai
  ;; The Helpful AI - +1 link; trash to give an icebreaker +2 str until end of turn
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "The Helpful AI" 1) (qty "Corroder" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "The Helpful AI")
    (is (= 1 (:link (get-hero))) "Gained 1 link")
    (play-from-hand state :hero "Corroder")
    (let [corr (get-program state 0)]
      (card-ability state :hero (get-resource state 0) 0)
      (prompt-select :hero corr)
      (is (= 4 (:current-strength (refresh corr))) "Corroder has +2 strength")
      (is (= 1 (count (:discard (get-hero)))) "Helpful AI trashed")
      (is (= 0 (:link (get-hero))))
      (take-credits state :hero)
      (is (= 2 (:current-strength (refresh corr))) "Corroder back to default strength"))))

(deftest the-source
  ;; The Source - Increase advancement requirement of agendas by 1; 3c additional cost to steal
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 2)])
              (default-hero [(qty "The Source" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Sure Gamble")
    (play-from-hand state :hero "The Source")
    (run-empty-server state :remote1)
    (prompt-choice :hero "Yes") ; pay 3c extra to steal
    (is (= 4 (:credit (get-hero))) "Paid 3c to steal")
    (is (= 2 (count (:discard (get-hero)))) "The Source is trashed")
    (play-from-hand state :hero "The Source")
    (take-credits state :hero)
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote2 0)]
      (core/advance state :minion {:card (refresh ht)})
      (core/advance state :minion {:card (refresh ht)})
      (core/score state :minion {:card (refresh ht)})
      (is (empty? (:scored (get-minion))) "Hostile Takeover can't be scored with 2 adv")
      (core/gain state :minion :click 1)
      (core/advance state :minion {:card (refresh ht)})
      (core/score state :minion {:card (refresh ht)})
      (is (= 1 (:agenda-point (get-minion))) "Hostile Takeover scored with 3 adv")
      (is (= 3 (count (:discard (get-hero)))) "The Source is trashed"))))

(deftest the-supplier-ability
  ;; The Supplier - Ability
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "The Supplier" 1)
                               (qty "Plascrete Carapace" 1)
                               (qty "Utopia Shard" 1)
                               (qty "Hedge Fund" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "The Supplier")
    (let [ts (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero ts 0)
      (prompt-select :hero (find-card "Plascrete Carapace" (:hand (get-hero))))
      (card-ability state :hero ts 0)
      (is (= 1 (count (-> @state :hero :prompt first :choices))))
      (prompt-select :hero (find-card "Utopia Shard" (:hand (get-hero))))
      (is (= 2 (count (:hosted (refresh ts)))) "The Supplier is hosting 2 cards")
      (take-credits state :hero)
      (take-credits state :minion)
      ;; Utopia Shard cannot be afforded and should not be in the prompt
      (prompt-select :hero (find-card "Plascrete Carapace" (:hosted (refresh ts))))
      (is (= 2 (:credit (get-hero)))
          "Runner charged 1 credit to install Plascrete off The Supplier")
      (take-credits state :hero)
      (is (= 6 (:credit (get-hero))) "Runner ends turn with 5 credits")
      (is (= 1 (count (:hosted (refresh ts)))) "One card still on The Supplier"))))

(deftest the-supplier-kate-discount
  ;; The Supplier - Interaction with Kate discount. Issue #578.
  (do-game
    (new-game (default-minion)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                         [(qty "The Supplier" 1)
                          (qty "Plascrete Carapace" 1)
                          (qty "Kati Jones" 1)
                          (qty "Hedge Fund" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "The Supplier")
    (let [ts (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero ts 0)
      (prompt-select :hero (find-card "Plascrete Carapace" (:hand (get-hero))))
      (core/lose state :hero :credit (:credit (get-hero)))
      (core/end-turn state :hero nil)
      (take-credits state :minion)
      (prompt-select :hero (find-card "Plascrete Carapace" (:hosted (refresh ts))))
      (is (= 0 (:credit (get-hero))) "Kate discount applied")
      (is (= 1 (count (get-in @state [:hero :rig :resource]))) "Plascrete installed"))))

(deftest the-supplier-trashed
  ;; Issue #2358 Brain chip mem is deducted when it is hosted and Supplier is trashed
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 2)])
              (default-hero [(qty "The Supplier" 1)
                               (qty "Brain Chip" 1)]))
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (take-credits state :minion)
    (is (= 4 (:memory (get-hero))) "Runner has 4 MU")
    (play-from-hand state :hero "The Supplier")
    (let [ts (get-resource state 0)]
      (card-ability state :hero ts 0)
      (prompt-select :hero (find-card "Brain Chip" (:hand (get-hero))))
      (is (= 4 (:memory (get-hero))) "Runner has 4 MU")
      (run-empty-server state "Server 1")
      (prompt-choice :hero "Steal")
      (take-credits state :hero)
      (core/gain state :hero :tag 1)
      (core/trash-resource state :minion nil)
      (prompt-select :minion (get-resource state 0))
      (is (= 2 (count (:discard (get-hero)))))
      (is (= 4 (:memory (get-hero))) "Runner has 4 MU"))))

(deftest tech-trader
  ;; Basic test
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Tech Trader" 1) (qty "Fall Guy" 1)]))

    (take-credits state :minion)
    (play-from-hand state :hero "Tech Trader")
    (play-from-hand state :hero "Fall Guy")
    (is (= 4 (:credit (get-hero))))

    (let [fall (get-in @state [:hero :rig :resource 1])]
      (card-ability state :hero fall 1)
      (is (= 7 (:credit (get-hero)))))))

(deftest the-black-file
  ;; The Black File - Prevent Corp from winning by agenda points
  (do-game
    (new-game (default-minion [(qty "Vanity Project" 3) (qty "Sure Gamble" 3)])
              (default-hero [(qty "The Black File" 1)]))
    (starting-hand state :minion ["Vanity Project"])
    (core/gain state :minion :agenda-point 3)
    (take-credits state :minion)
    (play-from-hand state :hero "The Black File")
    (take-credits state :hero)
    (play-from-hand state :minion "Vanity Project" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    (is (= 7 (:agenda-point (get-minion))))
    (is (not (:winner @state)) "No registered Corp win")
    (take-credits state :minion)
    (let [bf (get-resource state 0)]
      (is (= 1 (get-in (refresh bf) [:counter :power])) "1 power counter on The Black File")
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 2 (get-in (refresh bf) [:counter :power])) "2 power counters on The Black File")
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 1 (count (:rfg (get-hero)))) "The Black File removed from the game")
      (is (= :minion (:winner @state)) "Corp wins")
      (is (= "Agenda" (:reason @state)) "Win condition reports agendas"))))

(deftest the-black-file-flatline
  ;; The Black File - Corp can still win by flatlining Runner
  (do-game
    (new-game (default-minion [(qty "Vanity Project" 3) (qty "Scorched Earth" 3)])
              (default-hero [(qty "The Black File" 1)]))
    (starting-hand state :minion ["Vanity Project" "Scorched Earth"])
    (core/gain state :minion :agenda-point 3)
    (take-credits state :minion)
    (play-from-hand state :hero "The Black File")
    (take-credits state :hero)
    (play-from-hand state :minion "Vanity Project" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    (is (= 7 (:agenda-point (get-minion))))
    (is (not (:winner @state)) "No registered Corp win")
    (take-credits state :minion)
    (take-credits state :hero)
    (core/gain state :hero :tag 1)
    (play-from-hand state :minion "Scorched Earth")
    (is (= :minion (:winner @state)) "Corp wins")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest temujin-contract
  ;; Temüjin Contract - Multiple times in one turn. Issue #1952.
  (do-game
    (new-game (default-minion)
              (make-deck "Silhouette: Stealth Operative" [(qty "Temüjin Contract" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Temüjin Contract")
    (prompt-choice :hero "Archives")
    (run-empty-server state "Archives")
    (is (= 5 (:credit (get-hero))) "Gained 4cr")
    (run-empty-server state "Archives")
    (is (= 9 (:credit (get-hero))) "Gained 4cr")
    (is (= 12 (get-counters (get-resource state 0) :credit)) "Temjin has 12 credits remaining")))

(deftest tri-maf-contact
  ;; Tri-maf Contact - Click for 2c once per turn; take 3 meat dmg when trashed
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Tri-maf Contact" 1) (qty "Cache" 3) (qty "Shiv" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Tri-maf Contact")
    (let [tmc (get-resource state 0)]
      (card-ability state :hero tmc 0)
      (is (= 5 (:credit (get-hero))) "Gained 2c")
      (is (= 2 (:click (get-hero))) "Spent 1 click")
      (card-ability state :hero tmc 0)
      (is (= 5 (:credit (get-hero))) "No credits gained; already used this turn")
      (core/move state :hero tmc :hand)
      (is (= 5 (count (:hand (get-hero)))) "No meat damage")
      (play-from-hand state :hero "Tri-maf Contact")
      (core/gain state :hero :tag 1)
      (take-credits state :hero)
      (core/trash-resource state :minion nil)
      (prompt-select :minion (get-resource state 0))
      (is (= 4 (count (:discard (get-hero)))) "Took 3 meat damage"))))

(deftest virus-breeding-ground-gain
  ;; Virus Breeding Ground - Gain counters
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Virus Breeding Ground" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Virus Breeding Ground")
    (let [vbg (get-in @state [:hero :rig :resource 0])]
      (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :hero 3)
      (take-credits state :minion)
      (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
      (take-credits state :hero 3)
      (take-credits state :minion)
      (is (= 2 (get-counters (refresh vbg) :virus))
          "Virus Breeding Ground gains 1 counter per turn"))))

(deftest virus-breeding-ground-gain
  ;; Virus Breeding Ground - Move counters
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Virus Breeding Ground" 1) (qty "Hivemind" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Virus Breeding Ground")
    (play-from-hand state :hero "Hivemind")
    (let [hive (get-in @state [:hero :rig :program 0])
          vbg (get-in @state [:hero :rig :resource 0])]
      (is (= 1 (get-counters hive :virus)) "Hivemind starts with 1 counter")
      (is (zero? (get-counters vbg :virus)) "Virus Breeding Ground starts with 0 counters")
      (take-credits state :hero 3)
      (take-credits state :minion)
      (is (= 1 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground gains 1 counter per turn")
      (card-ability state :hero vbg 0)
      (prompt-select :hero hive)
      (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
      (is (= 0 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter"))))

(deftest wasteland
  ;; Wasteland - Gain 1c the first time you trash an installed card of yours each turn
  (do-game
    (new-game (default-minion [(qty "PAD Campaign" 1)])
              (default-hero [(qty "Wasteland" 1) (qty "Faust" 1) (qty "Fall Guy" 4)]))
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (take-credits state :minion)
    (core/gain state :hero :click 2)
    (core/gain state :hero :credit 4)
    (core/draw state :hero)
    (play-from-hand state :hero "Faust")
    (play-from-hand state :hero "Wasteland")
    (is (= 4 (:credit (get-hero))) "Runner has 4 credits")
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes") ; Trash PAD campaign
    (is (= 0 (:credit (get-hero))) "Gained nothing from Wasteland on minion trash")
    ; trash from hand first which should not trigger #2291
    (let [faust (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero faust 1)
      (prompt-card :hero (first (:hand (get-hero)))))
    (is (= 0 (:credit (get-hero))) "Gained nothing from Wasteland")
    (play-from-hand state :hero "Fall Guy")
    (play-from-hand state :hero "Fall Guy")
    (play-from-hand state :hero "Fall Guy")
    (card-ability state :hero (get-resource state 1) 1)
    (is (= 1 (count (:discard (get-hero)))) "Fall Guy trashed")
    (is (= 3 (:credit (get-hero))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (take-credits state :hero)
    (card-ability state :hero (get-resource state 1) 1)
    (is (= 2 (count (:discard (get-hero)))) "Fall Guy trashed")
    (is (= 6 (:credit (get-hero))) "Gained 2c from Fall Guy and 1c from Wasteland")
    (card-ability state :hero (get-resource state 1) 1)
    (is (= 3 (count (:discard (get-hero)))) "Fall Guy trashed")
    (is (= 8 (:credit (get-hero))) "Gained 2c from Fall Guy but no credits from Wasteland")))

(deftest xanadu
  ;; Xanadu - Increase all ICE rez cost by 1 credit
  (do-game
    (new-game (default-minion [(qty "Paper Wall" 2) (qty "Launch Campaign" 1)])
              (default-hero [(qty "Xanadu" 1)]))
    (play-from-hand state :minion "Paper Wall" "HQ")
    (play-from-hand state :minion "Paper Wall" "R&D")
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Xanadu")
    (let [pw1 (get-ice state :hq 0)
          pw2 (get-ice state :rd 0)
          lc (get-content state :remote1 0)]
      (core/rez state :minion pw1)
      (is (= 4 (:credit (get-minion))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :minion pw2)
      (is (= 3 (:credit (get-minion))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :minion lc)
      (is (= 2 (:credit (get-minion))) "Paid 1 to rez Launch Campaign; no effect on non-ICE"))))

(deftest zona-sul-shipping
  ;; Zona Sul Shipping - Gain 1c per turn, click to take all credits. Trash when tagged
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Zona Sul Shipping" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Zona Sul Shipping")
    (let [zss (get-resource state 0)]
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 1 (get-counters (refresh zss) :credit)) "Zona Sul holds 1c")
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 2 (get-counters (refresh zss) :credit)) "Zona Sul holds 2c")
      (card-ability state :hero zss 0)
      (is (= 12 (:credit (get-hero))) "Took 2c off Zona Sul")
      (is (= 3 (:click (get-hero))) "Spent 1 click")
      (core/gain state :hero :tag 1)
      (is (= 1 (count (:discard (get-hero)))) "Zona Sul trashed when tag taken"))))
