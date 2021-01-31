(ns game-test.cards.radicles
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "radicles"))

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
