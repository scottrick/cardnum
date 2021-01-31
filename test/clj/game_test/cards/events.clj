(ns game-test.cards.events
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "events"))

;; Rebirth
(let [choose-challenger
      (fn [id-name state prompt-map]
        (let [kate-choice (some #(when (= id-name (:title %)) %) (:choices (prompt-map :challenger)))]
          (core/resolve-prompt state :challenger {:card kate-choice})))
      akiko "Akiko Nisei: Head Case"
      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"]
  (deftest rebirth
    ;; Rebirth - Kate's discount applies after rebirth
    (testing "Kate"
      (do-game
        (new-game (default-contestant)
                  (default-challenger ["Magnum Opus" "Rebirth"])
                  {:start-as :challenger})
        (play-from-hand state :challenger "Rebirth")
        (is (= (first (prompt-titles :challenger)) akiko) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :challenger))
                    [kate kit]))
        (is (not-any? #(some #{%} (prompt-titles :challenger))
                      [professor whizzard jamie]))
        (choose-challenger kate state prompt-map)
        (is (= kate (-> (get-challenger) :identity :title)))
        (is (= 1 (:link (get-challenger))) "1 link")
        (is (empty? (:discard (get-challenger))))
        (is (= "Rebirth" (-> (get-challenger) :rfg first :title)))
        (is (changes-credits (get-challenger) -4
                             (play-from-hand state :challenger "Magnum Opus")))))
    (testing "Whizzard works after rebirth"
      (do-game
        (new-game (default-contestant ["Character Wall"]) (make-deck reina ["Rebirth"]))
        (play-from-hand state :contestant "Character Wall" "R&D")
        (play-from-hand state :challenger "Rebirth")
        (choose-challenger whizzard state prompt-map)
        (card-ability state :challenger (:identity (get-challenger)) 0)
        (is (= 6 (:credit (get-challenger))) "Took a Whizzard credit")
        (is (changes-credits (get-contestant) -1
                             (core/reveal state :contestant (get-character state :rd 0)))
            "Reina is no longer active")))
    (testing "Lose link from ID"
      (do-game
        (new-game (default-contestant)
                  (make-deck kate ["Rebirth" "Access to Globalsec"])
                  {:start-as :challenger})
        (play-from-hand state :challenger "Access to Globalsec")
        (is (= 2 (:link (get-challenger))) "2 link before rebirth")
        (play-from-hand state :challenger "Rebirth")
        (choose-challenger chaos state prompt-map)
        (is (= 1 (:link (get-challenger))) "1 link after rebirth")))
    (testing "Gain link from ID"
      (do-game
        (new-game (default-contestant)
                  (default-challenger ["Rebirth" "Access to Globalsec"])
                  {:start-as :challenger})
        (play-from-hand state :challenger "Access to Globalsec")
        (is (= 1 (:link (get-challenger))) "1 link before rebirth")
        (play-from-hand state :challenger "Rebirth")
        (choose-challenger kate state prompt-map)
        (is (= 2 (:link (get-challenger))) "2 link after rebirth"))))
  (deftest-pending rebirth-kate-twcharacter
    ;; Rebirth - Kate's discount does not after rebirth if something already placed
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Akamatsu Mem Chip" "Rebirth" "Clone Chip"])
                {:start-as :challenger})
      (play-from-hand state :challenger "Clone Chip")
      (play-from-hand state :challenger "Rebirth")
      (choose-challenger kate state prompt-map)
      (is (changes-credits (get-contestant) -1
                           (play-from-hand state :challenger "Akamatsu Mem Chip"))
          "Discount not applied for 2nd place"))))
