(ns game-test.cards.identities
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "identities"))

(deftest andromeda:-dispossessed-ristie
  ;; Andromeda - 9 card starting hand, 1 link
  (testing "Basic test"
    (do-game
      (new-game
        (default-contestant)
        (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                     (qty "Security Testing" 3) (qty "Bank Job" 3)]))
      (is (= 1 (:link (get-challenger))) "1 link")
      (is (= 9 (count (:hand (get-challenger)))) "9 cards in Andromeda starting hand")))
  (testing "9 card starting hand after mulligan"
    (do-game
      (new-game
        (default-contestant)
        (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                     (qty "Security Testing" 3) (qty "Bank Job" 3)])
        {:mulligan :challenger})
      (is (= 1 (:link (get-challenger))) "1 link")
      (is (= 9 (count (:hand (get-challenger)))) "9 cards in Andromeda starting hand")))
  (testing "should not grant Palana credits"
    (do-game
      (new-game
        (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
        (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                     (qty "Security Testing" 3) (qty "Bank Job" 3)]))
      (is (= 5 (:credit (get-contestant))) "Palana does not gain credit from Andromeda's starting hand"))))

(deftest asa-group:-security-through-vigilance
  (testing "Asa Group should not allow placing operations"
    (do-game
      (new-game
        (make-deck "Asa Group: Security Through Vigilance" ["Pup" "BOOM!" "Urban Renewal"])
        (default-challenger))
      (play-from-hand state :contestant "Pup" "New party")
      (prompt-select :contestant (find-card "BOOM!" (:hand (get-contestant))))
      (is (empty? (get-content state :party1)) "Asa Group placed an event in a locale")
      (prompt-select :contestant (find-card "Urban Renewal" (:hand (get-contestant))))
      (is (= "Urban Renewal" (:title (get-content state :party1 0))) "Asa Group can place an site in a party")))
  (testing "Asa Group should not allow placing agendas"
    (do-game
      (new-game
        (make-deck "Asa Group: Security Through Vigilance" ["Pup" "Project Vitruvius" "Urban Renewal"])
        (default-challenger))
      (play-from-hand state :contestant "Pup" "New party")
      (prompt-select :contestant (find-card "Project Vitruvius" (:hand (get-contestant))))
      (is (empty? (get-content state :party1)) "Asa Group did not place Agenda with its ability")
      (prompt-select :contestant (find-card "Urban Renewal" (:hand (get-contestant))))
      (is (= "Urban Renewal" (:title (get-content state :party1 0))) "Asa Group can place an site in a party")))
  (testing "Asa Group ordering correct when playing Mirrormontestanth"
    (do-game
      (new-game
        (make-deck "Asa Group: Security Through Vigilance" ["Shipment from MirrorMontestanth"
                                                            "Pup"
                                                            "Red Herrings"
                                                            "Marilyn Campaign"
                                                            "Project Vitruvius"])
        (default-challenger))
      (let  [marilyn (find-card "Marilyn Campaign" (:hand (get-contestant)))
             pup (find-card "Pup" (:hand (get-contestant)))
             herrings (find-card "Red Herrings" (:hand (get-contestant)))
             vitruvius (find-card "Project Vitruvius" (:hand (get-contestant)))]
        (play-from-hand state :contestant "Shipment from MirrorMontestanth")
        (prompt-select :contestant marilyn)
        (prompt-choice :contestant "New party")
        (is (= (:cid marilyn) (:cid (get-content state :party1 0))) "Marilyn is placed as first card")
        (prompt-select :contestant herrings) ;; This should be the Asa prompt, should be automatically placed in party1
        (is (= (:cid herrings) (:cid (get-content state :party1 1))) "Red Herrings is placed in Locale 1")
        (prompt-select :contestant vitruvius)
        (prompt-choice :contestant "New party")
        (prompt-select :contestant pup)
        (prompt-choice :contestant "New party")
        (is (empty? (:prompt (get-contestant))) "No more prompts")
        (is (= 6 (count (:locales (get-contestant)))) "There are six locales, including centrals"))))
  (testing "don't allow placeation of operations"
    (do-game
      (new-game
        (make-deck "Asa Group: Security Through Vigilance" ["Pup" "BOOM!" "Urban Renewal"])
        (default-challenger))
      (play-from-hand state :contestant "Pup" "New party")
      (prompt-select :contestant (find-card "BOOM!" (:hand (get-contestant))))
      (is (empty? (get-content state :party1)) "Asa Group placed an event in a locale")
      (prompt-select :contestant (find-card "Urban Renewal" (:hand (get-contestant))))
      (is (= "Urban Renewal" (:title (get-content state :party1 0))) "Asa Group can place an site in a party"))))

(deftest cerebral-imaging:-infinite-frontiers
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game
      (make-deck "Cerebral Imaging: Infinite Frontiers" [(qty "Hedge Fund" 3)])
      (default-challenger))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 13 (:credit (get-contestant))) "Has 13 credits")
    (is (= 13 (core/hand-size state :contestant)) "Max hand size is 13")))

(deftest chaos-theory:-wunderkind
  ;; Chaos Theory, start with +1 MU
  (do-game
    (new-game (default-contestant)
              (make-deck "Chaos Theory: Wünderkind" []))
    (is (= 5 (core/available-mu state)) "Chaos Theory starts the game with +1 MU")))

(deftest haas-bioroid:-stronger-together
  ;; Stronger Together - +1 strength for Bioroid character
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Stronger Together" ["Eli 1.0"])
      (default-challenger))
    (play-from-hand state :contestant "Eli 1.0" "Archives")
    (let [eli (get-character state :archives 0)]
      (core/reveal state :contestant eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest next-design:-guarding-the-net
  ;; Next Design.  Place up to 3 Character before game starts, one per locale max, and re-draw to 5
  (do-game
    (new-game
      (make-deck "NEXT Design: Guarding the Net" [(qty "Snowflake" 10)])
      (default-challenger)
      {:dont-start-turn true})
    (prompt-select :contestant (find-card "Snowflake" (:hand (get-contestant))))
    (prompt-choice :contestant "HQ")
    (prompt-select :contestant (find-card "Snowflake" (:hand (get-contestant))))
    (prompt-choice :contestant "R&D")
    (prompt-select :contestant (find-card "Snowflake" (:hand (get-contestant))))
    (prompt-choice :contestant "New party")
    (is (= 2 (count (:hand (get-contestant)))) "Contestant should have 2 cards in hand")
    (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
    (is (= 5 (count (:hand (get-contestant)))) "Contestant should start with 5 cards in hand")))

(deftest the-foundry:-refining-the-process
  ;; The Foundry
  (testing "interaction with Accelerated Beta Test"
    (do-game
      (new-game
        (make-deck "The Foundry: Refining the Process" [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)])
        (default-challenger))
      (starting-hand state :contestant ["Accelerated Beta Test"])
      (play-from-hand state :contestant "Accelerated Beta Test" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (prompt-choice :contestant "Yes")
      (prompt-select :contestant (find-card "Eli 1.0" (:play-area (get-contestant))))
      (prompt-choice :contestant "Archives")
      (prompt-choice :contestant "Yes")
      (is (empty? (:play-area (get-contestant))) "Play area shuffled into R&D"))))

(deftest the-outfit:-family-owned-and-operated
  ;; The Outfit - Gain 3 whenever you take at least 1 bad publicity
  (testing "basic test"
    (do-game
      (new-game
        (make-deck "The Outfit: Family Owned and Operated" ["Hostile Takeover" "Profiteering"])
        (default-challenger))
      (play-from-hand state :contestant "Hostile Takeover" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (is (= 1 (:bad-publicity (get-contestant))) "Take 1 bad publicity")
      (is (= 15 (:credit (get-contestant))) "Contestant should gain 10 credits")
      (play-from-hand state :contestant "Profiteering" "New party")
      (score-agenda state :contestant (get-content state :party2 0))
      (prompt-choice :contestant "3")  ;; Take 3 bad publicity from Profiteering, gain 15
      (is (= 4 (:bad-publicity (get-contestant))) "Contestant should gain 1 bad publicity")
      (is (= 33 (:credit (get-contestant))) "Contestant should gain 18 credits")))
  (testing "with Profiteering - Only gain 3 credits when taking more than 1 bad publicity in a single effect"
    (do-game
      (new-game
        (make-deck "The Outfit: Family Owned and Operated" ["Profiteering"])
        (default-challenger))
      (play-from-hand state :contestant "Profiteering" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (prompt-choice :contestant "3")
      (is (= 3 (:bad-publicity (get-contestant))) "Take 3 bad publicity")
      (is (= 23 (:credit (get-contestant))) "Gain 15 from Profiteering + 3 from The Outfit")))
  (testing "vs Valencia - 1 bad pub at start means 8 credits to start with"
    (do-game
      (new-game
        (make-deck "The Outfit: Family Owned and Operated" ["Hostile Takeover"])
        (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
      (is (= 1 (:bad-publicity (get-contestant))) "The Outfit starts with 1 bad publicity")
      (is (= 8 (:credit (get-contestant))) "The Outfit starts with 8 credits")
      (play-from-hand state :contestant "Hostile Takeover" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (is (= 2 (:bad-publicity (get-contestant))) "Take 1 bad publicity")
      (is (= 18 (:credit (get-contestant))) "Gain 7 from Hostile Takeover + 3 from The Outfit"))))

(deftest titan-transnational:-investing-in-your-future
  ;; Titan Transnational
  (testing "Add a counter to a scored agenda"
    (do-game
      (new-game
        (make-deck "Titan Transnational: Investing In Your Future" ["Project Atlas"])
        (default-challenger))
      (play-from-hand state :contestant "Project Atlas" "New party")
      (let [atl (get-content state :party1 0)]
        (core/gain state :contestant :click 1)
        (core/advance state :contestant {:card (refresh atl)})
        (core/advance state :contestant {:card (refresh atl)})
        (core/advance state :contestant {:card (refresh atl)})
        (core/score state :contestant {:card (refresh atl)})
        (let [scored (get-scored state :contestant 0)]
          (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))
  (testing "only use one counter of Contestantorate Sales Team"
    (do-game
      (new-game
        (make-deck "Titan Transnational: Investing In Your Future" ["Contestantorate Sales Team" "Mark Yale"])
        (default-challenger))
      (play-from-hand state :contestant "Contestantorate Sales Team" "New party")
      (play-from-hand state :contestant "Mark Yale" "New party")
      (let [cst (get-content state :party1 0)
            my (get-content state :party2 0)]
        (core/gain state :contestant :click 3)
        (core/advance state :contestant {:card (refresh cst)})
        (core/advance state :contestant {:card (refresh cst)})
        (core/advance state :contestant {:card (refresh cst)})
        (core/advance state :contestant {:card (refresh cst)})
        (core/score state :contestant {:card (refresh cst)})
        (let [scored (get-scored state :contestant 0)]
          (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
          (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
          (core/reveal state :contestant my)
          (card-ability state :contestant my 1)
          (prompt-select :contestant (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
          (card-ability state :contestant my 1)
          (prompt-select :contestant (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "No agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale"))))))

(deftest weyland-consortium:-builder-of-nations
  ;; Builder of Nations
  (testing "1 meat damage per turn at most"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations" [(qty "Hedge Fund" 3)])
        (default-challenger))
      (let [bon (get-in @state [:contestant :identity])]
        (card-ability state :contestant bon 0)
        (prompt-choice :contestant "Cancel")
        (is (zero? (count (:discard (get-challenger)))) "Challenger took no meat damage from BoN")
        (card-ability state :contestant bon 0)
        (prompt-choice :contestant "Yes")
        (is (= 1 (count (:discard (get-challenger)))) "Challenger took 1 meat damage from BoN")
        (card-ability state :contestant bon 0)
        (is (= 1 (count (:discard (get-challenger)))) "Challenger took only 1 meat damage from BoN total")
        (is (zero? (count (:prompt (get-contestant))))))))
  (testing "2 meat damage from ID ability when The Cleaners is scored"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations" [(qty "The Cleaners" 3) (qty "Character Wall" 3)])
        (default-challenger [(qty "Sure Gamble" 2)]))
      (play-from-hand state :contestant "The Cleaners" "New party")
      (let [clean (get-content state :party1 0)]
        (score-agenda state :contestant clean)
        (let [bon (get-in @state [:contestant :identity])]
          (card-ability state :contestant bon 0)
          (prompt-choice :contestant "Yes")
          (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 meat damage from BoN/Cleaners combo"))))))
