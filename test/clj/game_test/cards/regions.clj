(ns game-test.cards.regions
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "regions"))

(deftest calibration-testing
  ;; Calibration Testing - advanceable / non-advanceable
  (do-game
    (new-game (default-contestant [(qty "Calibration Testing" 2) "Project Junebug" "PAD Campaign"])
              (default-challenger))
    (core/gain state :contestant :credit 10)
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Calibration Testing" "New party")
    (play-from-hand state :contestant "Project Junebug" "Locale 1")
    (let [ct (get-content state :party1 0)
          pj (get-content state :party1 1)]
      (core/reveal state :contestant ct)
      (card-ability state :contestant ct 0)
      (prompt-select :contestant pj)
      (is (= 1 (get-counters (refresh pj) :advancement)) "Project Junebug advanced")
      (is (= 1 (count (:discard (get-contestant)))) "Calibration Testing discarded"))
    (play-from-hand state :contestant "Calibration Testing" "New party")
    (play-from-hand state :contestant "PAD Campaign" "Locale 2")
    (let [ct (get-content state :party2 0)
          pad (get-content state :party2 1)]
      (core/reveal state :contestant ct)
      (card-ability state :contestant ct 0)
      (prompt-select :contestant pad)
      (is (= 1 (get-counters (refresh pad) :advancement)) "PAD Campaign advanced")
      (is (= 2 (count (:discard (get-contestant)))) "Calibration Testing discarded"))))

(deftest jinja-city-grid
  ;; Jinja City Grid - place drawn character, lowering place cost by 4
  (do-game
    (new-game (default-contestant ["Jinja City Grid" (qty "Vanilla" 3) (qty "Character Wall" 3)])
              (default-challenger))
    (starting-hand state :contestant ["Jinja City Grid"])
    (core/gain state :contestant :click 6)
    (play-from-hand state :contestant "Jinja City Grid" "New party")
    (core/reveal state :contestant (get-content state :party1 0))
    (dotimes [n 5]
      (core/click-draw state :contestant 1)
      (prompt-choice :contestant (-> (get-contestant) :prompt first :choices first))
      (is (= 4 (:credit (get-contestant))) "Not charged to place character")
      (is (= (inc n) (count (get-in @state [:contestant :locales :party1 :characters]))) (str n " Character protecting Party1")))
    (core/click-draw state :contestant 1)
    (prompt-choice :contestant (-> (get-contestant) :prompt first :choices first))
    (is (= 3 (:credit (get-contestant))) "Charged to place character")
    (is (= 6 (count (get-in @state [:contestant :locales :party1 :characters]))) "6 Character protecting Party1")))
