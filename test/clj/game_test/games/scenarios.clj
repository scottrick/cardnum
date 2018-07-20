(ns game-test.games.scenarios
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs nil))

(deftest minigame-prevent-netdmg-radiclediscard
  (testing "Mini-game testing prevention of net damage and radicle discarding, with hosted Fall Guy"
    (do-game
      (new-game
        (default-contestant ["Neural EMP" (qty "Hedge Fund" 3) "SEA Source"])
        (default-challenger ["Fall Guy" "Off-Campus Apartment" "Net Shield"
                         "Wireless Net Pavilion" "Sure Gamble"]))
      (play-from-hand state :contestant "Hedge Fund")
      (play-from-hand state :contestant "Hedge Fund")
      (take-credits state :contestant 1)
      (is (= 14 (:credit (get-contestant))))
      (core/gain state :challenger :click 2)
      (run-empty-locale state "Archives") ; enable Contestant play of Neural and SEA next turn
      (play-from-hand state :challenger "Sure Gamble")
      (play-from-hand state :challenger "Off-Campus Apartment")
      (play-from-hand state :challenger "Wireless Net Pavilion")
      (play-from-hand state :challenger "Net Shield")
      (let [apt (get-radicle state 0)]
        (card-ability state :challenger apt 0)
        (click-card state :challenger (find-card "Fall Guy" (:hand (get-challenger))))
        (take-credits state :challenger)
        (is (= 6 (:credit (get-challenger))))
        (play-from-hand state :contestant "Neural EMP")
        (let [ns (get-resource state 0)
              fg (first (:hosted (refresh apt)))]
          (card-ability state :challenger ns 0)
          (is (= 5 (:credit (get-challenger))) "Challenger paid 1c to survive Neural EMP")
          (click-prompt state :challenger "Done")
          (play-from-hand state :contestant "SEA Source")
          (click-prompt state :contestant "3") ; boost trace to 6
          (click-prompt state :challenger "0")
          (is (= 1 (:tag (get-challenger))) "Challenger took tag from SEA Source")
          (is (= 7 (:credit (get-contestant))))
          (core/discard-radicle state :contestant nil)
          (click-card state :contestant "Off-Campus Apartment")
          (is (= 3 (:credit (get-contestant))) "WNP increased cost to discard a radicle by 2")
          (card-ability state :challenger fg 0) ; Discard Fall Guy to save the Apartment!
          (is (= (:title (get-radicle state 0)) "Off-Campus Apartment")
              "Apartment still standing")
          (is (= (:title (last (:discard (get-challenger)))) "Fall Guy") "Fall Guy discarded"))))))

(deftest hb-glacier
  (testing "HB Glacier econ and locale protection with regions - Ash, Caprcharacter, Breaker Bay Grid, positional character strength boost"
    (do-game
      (new-game (make-deck "Haas-Bioroid: Engineering the Future"
                           ["Adonis Campaign"
                            "Global Food Initiative"
                            "Breaker Bay Grid"
                            "Caprcharacter Nisei"
                            "Ash 2X3ZB9CY"
                            "Turing"
                            "Hedge Fund"])
                (default-challenger ["Desperado"
                                 "Dirty Laundry"
                                 "Emergency Shutdown"
                                 "Lamprey"
                                 "Data Folding"
                                 "Career Fair"]))
      (core/draw state :contestant 1)
      (core/gain state :contestant :click 1)
      (play-from-hand state :contestant "Hedge Fund")
      (play-from-hand state :contestant "Adonis Campaign" "New party")
      (is (= 10 (:credit (get-contestant))) "HB:EtF ability paid 1 credit")
      (play-from-hand state :contestant "Breaker Bay Grid" "Locale 1")
      (play-from-hand state :contestant "Ash 2X3ZB9CY" "HQ")
      (let [adon (get-content state :party1 0)
            bbg (get-content state :party1 1)
            ash (get-content state :hq 0)]
        (core/reveal state :contestant bbg)
        (core/reveal state :contestant adon)
        (is (= 10 (:credit (get-contestant))) "Breaker Bay Grid allowed reveal of Adonis for free")
        (take-credits state :contestant)
        (core/draw state :challenger 1)
        (play-from-hand state :challenger "Career Fair")
        (click-card state :challenger (find-card "Data Folding" (:hand (get-challenger))))
        (is (= 5 (:credit (get-challenger))) "Data Folding placed for free by Career Fair")
        (play-from-hand state :challenger "Lamprey")
        (play-from-hand state :challenger "Desperado")
        (is (= 1 (:credit (get-challenger))))
        (run-on state "HQ")
        (core/reveal state :contestant ash)
        (run-successful state)
        (click-prompt state :contestant "0")
        (click-prompt state :challenger "0")
        (is (and (= 2 (:credit (get-challenger))) (= 7 (:credit (get-contestant))))
            "Desperado paid 1 to Challenger, Lamprey took 1 from Contestant")
        (click-prompt state :challenger "No action") ; can't afford to discard Ash
        (take-credits state :challenger)
        (play-from-hand state :contestant "Caprcharacter Nisei" "Locale 1")
        (is (= 11 (:credit (get-contestant))) "Gained 3 from Adonis and 1 from HB:EtF")
        (play-from-hand state :contestant "Turing" "Locale 1")
        (take-credits state :contestant 1)
        (is (= 3 (:credit (get-challenger))) "Gained 1 from Data Folding")
        (core/gain state :challenger :click 2)
        (run-empty-locale state "HQ")
        (click-prompt state :contestant "0")
        (click-prompt state :challenger "0")
        (click-prompt state :challenger "Pay 3 [Credits] to discard") ; discard Ash
        (is (and (= 1 (:credit (get-challenger))) (= 11 (:credit (get-contestant)))))
        (core/gain state :challenger :credit 1)
        (play-from-hand state :challenger "Dirty Laundry")
        (click-prompt state :challenger "HQ")
        (run-successful state)
        (click-prompt state :challenger "Steal")
        (is (= 2 (:agenda-point (get-challenger))) "Stole Global Food Initiative")
        (is (and (= 6 (:credit (get-challenger))) (= 10 (:credit (get-contestant))))
            "Desperado plus Dirty Laundry, Lamprey took 1 from Contestant")
        (run-on state "Locale 1")
        (let [tur (get-character state :party1 0)
              cap (get-content state :party1 2)]
          (core/reveal state :contestant tur)
          (is (= 5 (:current-strength (refresh tur))) "Turing +3 strength protecting a party")
          (card-subroutine state :contestant tur 0) ; end the run
          (play-from-hand state :challenger "Emergency Shutdown")
          (click-card state :challenger tur)
          (is (not (:revealed (refresh tur))) "Turing hidden")
          (run-on state "Locale 1") ; letting Challenger in this time to use Caprcharacter
          (core/reveal state :contestant cap)
          (run-continue state)
          ;; Caprcharacter psi game started automatically
          (click-prompt state :contestant "1 [Credits]")
          (click-prompt state :challenger "2 [Credits]")
          (is (not (:run @state)) "Contestant won Caprcharacter psi game and ended the run"))))))
