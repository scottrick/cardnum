(ns game-test.cards.character
  (:require [game.core :as core]
            [game.utils :refer [has?]]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "character"))

(deftest architect
  (testing "Architect is undiscardable while placed and revealed, but discardable if hidden or from HQ"
    (do-game
      (new-game (default-contestant [(qty "Architect" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Architect" "HQ")
      (let [architect (get-character state :hq 0)]
        (core/reveal state :contestant architect)
        (core/discard state :contestant (refresh architect))
        (is (not= nil (get-character state :hq 0)) "Architect was discarded, but should be undiscardable")
        (core/hide state :contestant (refresh architect))
        (core/discard state :contestant (refresh architect))
        (is (= nil (get-character state :hq 0)) "Architect was not discarded, but should be discardable")
        (core/discard state :contestant (get-in @state [:contestant :hand 0]))
        (is (= (get-in @state [:contestant :discard 0 :title]) "Architect"))
        (is (= (get-in @state [:contestant :discard 1 :title]) "Architect"))))))

(deftest curtain-wall
  ;; Curtain Wall - Strength boost when outermost Character
  (do-game
    (new-game (default-contestant ["Curtain Wall" "Paper Wall"])
              (default-challenger))
    (core/gain state :contestant :credit 10)
    (play-from-hand state :contestant "Curtain Wall" "HQ")
    (let [curt (get-character state :hq 0)]
      (core/reveal state :contestant curt)
      (is (= 10 (:current-strength (refresh curt)))
          "Curtain Wall has +4 strength as outermost Character")
      (play-from-hand state :contestant "Paper Wall" "HQ")
      (let [paper (get-character state :hq 1)]
        (core/reveal state :contestant paper)
        (is (= 6 (:current-strength (refresh curt))) "Curtain Wall back to default 6 strength")))))

(deftest free-lunch
  ;; Free Lunch - Spend 1 power counter to make Challenger lose 1c
  (do-game
    (new-game (default-contestant ["Free Lunch"])
              (default-challenger))
    (play-from-hand state :contestant "Free Lunch" "HQ")
    (let [fl (get-character state :hq 0)]
      (core/reveal state :contestant fl)
      (card-subroutine state :contestant fl 0)
      (is (= 1 (get-counters (refresh fl) :power)) "Free Lunch has 1 power counter")
      (card-subroutine state :contestant fl 0)
      (is (= 2 (get-counters (refresh fl) :power)) "Free Lunch has 2 power counters")
      (is (= 5 (:credit (get-challenger))))
      (card-ability state :contestant (refresh fl) 0)
      (is (= 1 (get-counters (refresh fl) :power)) "Free Lunch has 1 power counter")
      (is (= 4 (:credit (get-challenger))) "Challenger lost 1 credit"))))

(deftest iq
  ;; IQ - Reveal cost and strength equal to cards in HQ
  (do-game
    (new-game (default-contestant [(qty "IQ" 3) (qty "Hedge Fund" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "IQ" "R&D")
    (let [iq1 (get-character state :rd 0)]
      (core/reveal state :contestant iq1)
      (is (and (= 4 (count (:hand (get-contestant))))
               (= 4 (:current-strength (refresh iq1)))
               (= 5 (:credit (get-contestant)))) "4 cards in HQ: paid 4 to reveal, has 4 strength")
      (play-from-hand state :contestant "IQ" "HQ")
      (let [iq2 (get-character state :hq 0)]
        (core/reveal state :contestant iq2)
        (is (and (= 3 (count (:hand (get-contestant))))
                 (= 3 (:current-strength (refresh iq1)))
                 (= 3 (:current-strength (refresh iq2)))
                 (= 2 (:credit (get-contestant)))) "3 cards in HQ: paid 3 to reveal, both have 3 strength")))))

(deftest meru-mati
  (do-game
    (new-game (default-contestant [(qty "Meru Mati" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Meru Mati" "HQ")
    (play-from-hand state :contestant "Meru Mati" "R&D")
    (core/reveal state :contestant (get-character state :hq 0))
    (core/reveal state :contestant (get-character state :rd 0))
    (is (= 4 (:current-strength (get-character state :hq 0))) "HQ Meru Mati at 4 strength")
    (is (= 1 (:current-strength (get-character state :rd 0))) "R&D at 0 strength")))

(deftest mother-goddess
  ;; Mother Goddess - Gains other character subtypes
  (do-game
    (new-game (default-contestant ["Mother Goddess" "NEXT Bronze"])
              (default-challenger))
    (core/gain state :contestant :credit 1)
    (play-from-hand state :contestant "Mother Goddess" "HQ")
    (play-from-hand state :contestant "NEXT Bronze" "R&D")
    (let [mg (get-character state :hq 0)
          nb (get-character state :rd 0)]
      (core/reveal state :contestant mg)
      (is (core/has-subtype? (refresh mg) "Mythic") "Mother Goddess has Mythic")
      (is (not (core/has-subtype? (refresh mg) "Code Gate")) "Mother Goddess does not have Code Gate")
      (is (not (core/has-subtype? (refresh mg) "NEXT")) "Mother Goddess does not have NEXT")
      (core/reveal state :contestant nb)
      (is (core/has-subtype? (refresh mg) "Mythic") "Mother Goddess has Mythic")
      (is (core/has-subtype? (refresh mg) "Code Gate") "Mother Goddess has Code Gate")
      (is (core/has-subtype? (refresh mg) "NEXT") "Mother Goddess has NEXT"))))

(deftest next-bronze
  ;; NEXT Bronze - Add 1 strength for every revealed NEXT character
  (do-game
    (new-game (default-contestant [(qty "NEXT Bronze" 2) "NEXT Silver"])
              (default-challenger))
    (core/gain state :contestant :credit 2)
    (play-from-hand state :contestant "NEXT Bronze" "HQ")
    (play-from-hand state :contestant "NEXT Bronze" "R&D")
    (play-from-hand state :contestant "NEXT Silver" "Archives")
    (let [nb1 (get-character state :hq 0)
          nb2 (get-character state :rd 0)
          ns1 (get-character state :archives 0)]
      (core/reveal state :contestant nb1)
      (is (= 1 (:current-strength (refresh nb1)))
          "NEXT Bronze at 1 strength: 1 revealed NEXT character")
      (core/reveal state :contestant nb2)
      (is (= 2 (:current-strength (refresh nb1)))
          "NEXT Bronze at 2 strength: 2 revealed NEXT character")
      (is (= 2 (:current-strength (refresh nb2)))
          "NEXT Bronze at 2 strength: 2 revealed NEXT character")
      (core/reveal state :contestant ns1)
      (is (= 3 (:current-strength (refresh nb1)))
          "NEXT Bronze at 3 strength: 3 revealed NEXT character")
      (is (= 3 (:current-strength (refresh nb2)))
          "NEXT Bronze at 3 strength: 3 revealed NEXT character"))))

(deftest next-diamond
  ;; NEXT Diamond - Reveal cost is lowered by 1 for each revealed NEXT character
  (testing "Base reveal cost"
    (do-game
      (new-game (default-contestant ["NEXT Diamond"])
                (default-challenger))
      (core/gain state :contestant :credit 5)
      (is (= 10 (:credit (get-contestant))) "Contestant starts with 10 credits")
      (play-from-hand state :contestant "NEXT Diamond" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (is (zero? (:credit (get-contestant))) "Contestant spends 10 credits to reveal")))
  (testing "Lowered reveal cost"
    (do-game
      (new-game (default-contestant ["NEXT Diamond" "NEXT Opal" "NEXT Bronze" "Kakugo"])
                (default-challenger))
      (core/gain state :contestant :credit 13 :click 1)
      (play-from-hand state :contestant "NEXT Diamond" "HQ")
      (play-from-hand state :contestant "NEXT Opal" "HQ")
      (play-from-hand state :contestant "NEXT Bronze" "R&D")
      (play-from-hand state :contestant "Kakugo" "Archives")
      (core/reveal state :contestant (get-character state :hq 1))
      (core/reveal state :contestant (get-character state :archives 0))
      (is (= 9 (:credit (get-contestant))) "Contestant starts with 9 credits")
      (core/reveal state :contestant (get-character state :hq 0))
      (is (zero? (:credit (get-contestant))) "Contestant spends 9 credits to reveal"))))

(deftest seidr-adaptive-barrier
  ;; Seidr Adaptive Barrier - +1 strength for every character protecting its locale
  (do-game
    (new-game (default-contestant ["Seidr Adaptive Barrier" (qty "Character Wall" 2)])
              (default-challenger))
    (core/gain state :contestant :credit 10)
    (play-from-hand state :contestant "Seidr Adaptive Barrier" "HQ")
    (let [sab (get-character state :hq 0)]
      (core/reveal state :contestant sab)
      (is (= 3 (:current-strength (refresh sab))) "Seidr gained 1 strength for itself")
      (play-from-hand state :contestant "Character Wall" "HQ")
      (is (= 4 (:current-strength (refresh sab))) "+2 strength for 2 pieces of Character")
      (play-from-hand state :contestant "Character Wall" "HQ")
      (is (= 5 (:current-strength (refresh sab))) "+3 strength for 3 pieces of Character")
      (core/move-card state :contestant {:card (get-character state :hq 1) :locale "Archives"})
      (is (= 4 (:current-strength (refresh sab))) "+2 strength for 2 pieces of Character"))))

(deftest surveyor
  ;; Surveyor character strength
  (do-game
    (new-game (default-contestant [(qty "Surveyor" 1) (qty "Character Wall" 2)])
              (default-challenger))
    (core/gain state :contestant :credit 10)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :contestant "Surveyor" "HQ")
    (let [surv (get-character state :hq 0)]
      (core/reveal state :contestant surv)
      (is (= 2 (:current-strength (refresh surv))) "Surveyor has 2 strength for itself")
      (play-from-hand state :contestant "Character Wall" "HQ")
      (is (= 4 (:current-strength (refresh surv))) "Surveyor has 4 strength for 2 pieces of Character")
      (play-from-hand state :contestant "Character Wall" "HQ")
      (is (= 6 (:current-strength (refresh surv))) "Surveyor has 6 strength for 3 pieces of Character")
      (run-on state "HQ")
      (card-subroutine state :contestant surv 0)
      (is (= 6 (-> (get-contestant) :prompt first :base)) "Trace should be base 6")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 5)
      (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags from Surveyor Trace 6 with boost 5")
      (card-subroutine state :contestant surv 0)
      (is (= 6 (-> (get-contestant) :prompt first :base)) "Trace should be base 6")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 6)
      (is (= 2 (:tag (get-challenger))) "Challenger did not take tags from Surveyor Trace 6 with boost 6")
      (core/move-card state :contestant {:card (get-character state :hq 1) :locale "Archives"})
      (is (= 4 (:current-strength (refresh surv))) "Surveyor has 4 strength for 2 pieces of Character"))))

(deftest tmi
  ;; TMI
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["TMI"])
                (default-challenger))
      (play-from-hand state :contestant "TMI" "HQ")
      (let [tmi (get-character state :hq 0)]
        (core/reveal state :contestant tmi)
        (prompt-choice :contestant 0)
        (prompt-choice :challenger 0)
        (is (:revealed (refresh tmi))))))
  (testing "Losing trace hides TMI"
    (do-game
      (new-game (default-contestant ["TMI"])
                (make-deck "Sunny Lebeau: Security Specialist" [(qty "Blackmail" 3)]))
      (play-from-hand state :contestant "TMI" "HQ")
      (let [tmi (get-character state :hq 0)]
        (core/reveal state :contestant tmi)
        (prompt-choice :contestant 0)
        (prompt-choice :challenger 0)
        (is (not (:revealed (refresh tmi))))))))

(deftest turing
  ;; Turing - Strength boosted when protecting a party locale
  (do-game
    (new-game (default-contestant [(qty "Turing" 2) "Hedge Fund"])
              (default-challenger))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Turing" "HQ")
    (play-from-hand state :contestant "Turing" "New party")
    (let [t1 (get-character state :hq 0)
          t2 (get-character state :party1 0)]
      (core/reveal state :contestant t1)
      (is (= 2 (:current-strength (refresh t1)))
          "Turing default 2 strength over a central locale")
      (core/reveal state :contestant t2)
      (is (= 5 (:current-strength (refresh t2)))
          "Turing increased to 5 strength over a party locale"))))

(deftest waiver
  ;; Waiver - Discard Challenger cards in grip with play/place cost <= trace exceed
  (do-game
    (new-game (default-contestant ["Waiver"])
              (default-challenger ["Corroder" "Dean Lister" "Ubax" "Caldera"]))
    (play-from-hand state :contestant "Waiver" "HQ")
    (let [waiv (get-character state :hq 0)]
      (core/reveal state :contestant waiv)
      (card-subroutine state :contestant waiv 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 3)
      (is (empty? (filter #(= "Ubax" (:title %)) (:discard (get-challenger)))) "Ubax not discarded")
      (is (empty? (filter #(= "Caldera" (:title %)) (:discard (get-challenger)))) "Caldera not discarded")
      (is (= 2 (count (:discard (get-challenger)))) "2 cards discarded"))))

