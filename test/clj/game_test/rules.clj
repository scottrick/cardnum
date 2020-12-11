(ns game-test.rules
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs nil))

(deftest undo-turn
  (do-game
    (new-game (default-contestant)
              (default-challenger))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 1 (:click (get-contestant))) "Contestant spent 2 clicks")
    (is (= 13 (:credit (get-contestant))) "Contestant has 13 credits")
    (is (= 1 (count (:hand (get-contestant)))) "Contestant has 1 card in HQ")
    (core/command-undo-turn state :challenger)
    (core/command-undo-turn state :contestant)
    (is (= 3 (count (:hand (get-contestant)))) "Contestant has 3 cards in HQ")
    (is (zero? (:click (get-contestant))) "Contestant has no clicks - turn not yet started")
    (is (= 5 (:credit (get-contestant))) "Contestant has 5 credits")))

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

(deftest agenda-forfeit-contestant
  ;; forfeit - Deactivate agenda to trigger leave play effects if Contestant forfeits a scored agenda
  (do-game
    (new-game (default-contestant ["Mandatory Regions" "Contestantorate Town"])
              (default-challenger))
    (play-from-hand state :contestant "Mandatory Regions" "New party")
    (score-agenda state :contestant (get-content state :party1 0))
    (is (= 4 (:click-per-turn (get-contestant))) "Up to 4 clicks per turn")
    (play-from-hand state :contestant "Contestantorate Town" "New party")
    (let [ctown (get-content state :party2 0)]
      (core/reveal state :contestant ctown)
      (prompt-select :contestant (get-scored state :contestant 0))
      (is (= 3 (:click-per-turn (get-contestant))) "Back down to 3 clicks per turn"))))

(deftest card-str-test-simple
  ;; ensure card-str names cards in simple situations properly
  (do-game
    (new-game (default-contestant [(qty "Character Wall" 3) (qty "Jackson Howard" 2)])
              (default-challenger ["Corroder"
                               "Clone Chip"
                               "Paparazzi"
                               "Parasite"]))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Character Wall" "HQ")
    (play-from-hand state :contestant "Character Wall" "R&D")
    (play-from-hand state :contestant "Jackson Howard" "New party")
    (play-from-hand state :contestant "Jackson Howard" "New party")
    (play-from-hand state :contestant "Character Wall" "HQ")
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
          corr (get-resource state 0)
          cchip (get-hazard state 0)
          pap (get-radicle state 0)]
      (core/reveal state :contestant hqiwall0)
      (core/reveal state :contestant jh1)
      (prompt-select :challenger (refresh hqiwall0))
      (is (= (core/card-str state (refresh hqiwall0)) "Character Wall protecting HQ at position 0"))
      (is (= (core/card-str state (refresh hqiwall1)) "Character protecting HQ at position 1"))
      (is (= (core/card-str state (refresh rdiwall)) "Character protecting R&D at position 0"))
      (is (= (core/card-str state (refresh rdiwall) {:visible true})
             "Character Wall protecting R&D at position 0"))
      (is (= (core/card-str state (refresh jh1)) "Jackson Howard in Locale 1"))
      (is (= (core/card-str state (refresh jh2)) "a card in Locale 2"))
      (is (= (core/card-str state (refresh corr)) "Corroder"))
      (is (= (core/card-str state (refresh cchip)) "Clone Chip"))
      (is (= (core/card-str state (refresh pap)) "Paparazzi"))
      (is (= (core/card-str state (first (:hosted (refresh hqiwall0))))
             "Parasite hosted on Character Wall protecting HQ at position 0")))))

(deftest invalid-score-attempt
  ;; Test scoring with an incorrect number of advancement tokens
  (do-game
    (new-game (default-contestant ["Ancestral Imager"])
              (default-challenger))
    (play-from-hand state :contestant "Ancestral Imager" "New party")
    (let [ai (get-content state :party1 0)]
      ;; Trying to score without any tokens does not do anything
      (is (not (find-card "Ancestral Imager" (:scored (get-contestant)))) "AI not scored")
      (is (not (nil? (get-content state :party1 0))))
      (core/advance state :contestant {:card (refresh ai)})
      (core/score state :contestant {:card (refresh ai)})
      (is (not (nil? (get-content state :party1 0)))))))

(deftest counter-manipulation-commands-smart
  ;; Test interactions of smart counter advancement command
  (do-game
    (new-game (default-contestant ["House of Knives"])
              (default-challenger))
    (play-from-hand state :contestant "House of Knives" "New party")
    (let [hok (get-content state :party1 0)]
      (core/command-counter state :contestant [3])
      (prompt-select :contestant (refresh hok))
      (is (= 3 (get-counters (refresh hok) :advancement)))
      (core/score state :contestant (refresh hok)))
    (let [hok-scored (get-scored state :contestant 0)]
      (is (= 3 (get-counters (refresh hok-scored) :agenda)) "House of Knives should start with 3 counters")
      (core/command-counter state :contestant ["virus" 2])
      (prompt-select :contestant (refresh hok-scored))
      (is (= 3 (get-counters (refresh hok-scored) :agenda)) "House of Knives should stay at 3 counters")
      (is (= 2 (get-counters (refresh hok-scored) :virus)) "House of Knives should have 2 virus counters")
      (core/command-counter state :contestant [4])
      (prompt-select :contestant (refresh hok-scored)) ;; doesn't crash with unknown counter type
      (is (empty? (:prompt (get-contestant))) "Counter prompt closed")
      (is (= 4 (get-counters (refresh hok-scored) :agenda)) "House of Knives should have 4 agenda counters")
      (is (= 2 (get-counters (refresh hok-scored) :virus)) "House of Knives should have 2 virus counters"))))
