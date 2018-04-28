(ns test.games.scenarios
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest minigame-prevent-netdmg-resourcetrash
  "Mini-game testing prevention of net damage and resource trashing, with hosted Fall Guy"
  (do-game
    (new-game
      (default-corp [(qty "Neural EMP" 1) (qty "Hedge Fund" 3) (qty "SEA Source" 1)])
      (default-runner [(qty "Fall Guy" 1) (qty "Off-Campus Apartment" 1) (qty "Net Shield" 1)
                       (qty "Wireless Net Pavilion" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :resPlayer 1)
    (is (= 14 (:credit (get-corp))))
    (core/gain state :hazPlayer :click 2)
    (run-empty-server state "Archives") ; enable Corp play of Neural and SEA next turn
    (play-from-hand state :hazPlayer "Sure Gamble")
    (play-from-hand state :hazPlayer "Off-Campus Apartment")
    (play-from-hand state :hazPlayer "Wireless Net Pavilion")
    (play-from-hand state :hazPlayer "Net Shield")
    (let [apt (get-resource state 0)]
      (card-ability state :hazPlayer apt 0)
      (prompt-select :hazPlayer (find-card "Fall Guy" (:hand (get-runner))))
      (take-credits state :hazPlayer)
      (is (= 6 (:credit (get-runner))))
      (play-from-hand state :resPlayer "Neural EMP")
      (let [ns (get-program state 0)
            fg (first (:hosted (refresh apt)))]
        (card-ability state :hazPlayer ns 0)
        (is (= 5 (:credit (get-runner))) "Runner paid 1c to survive Neural EMP")
        (prompt-choice :hazPlayer "Done")
        (play-from-hand state :resPlayer "SEA Source")
        (prompt-choice :resPlayer 3)                             ; boost trace to 6
        (prompt-choice :hazPlayer 0)
        (is (= 1 (:tag (get-runner))) "Runner took tag from SEA Source")
        (is (= 7 (:credit (get-corp))))
        (core/trash-resource state :resPlayer nil)
        (prompt-select :resPlayer (find-card "Off-Campus Apartment" (:rig (get-runner))))
        (is (= 3 (:credit (get-corp))) "WNP increased cost to trash a resource by 2")
        (card-ability state :hazPlayer fg 0)                   ; Trash Fall Guy to save the Apartment!
        (is (= (:title (get-resource state 0)) "Off-Campus Apartment")
            "Apartment still standing")
        (is (= (:title (last (:discard (get-runner)))) "Fall Guy") "Fall Guy trashed")))))

(deftest hb-glacier
  "HB Glacier econ and server protection with upgrades - Ash, Caprice, Breaker Bay Grid, positional ice strength boost"
  (do-game
    (new-game (make-deck "Haas-Bioroid: Engineering the Future"
                         [(qty "Adonis Campaign" 1)
                          (qty "Global Food Initiative" 1)
                          (qty "Breaker Bay Grid" 1)
                          (qty "Caprice Nisei" 1)
                          (qty "Ash 2X3ZB9CY" 1)
                          (qty "Turing" 1)
                          (qty "Hedge Fund" 1)])
              (default-runner [(qty "Desperado" 1)
                               (qty "Dirty Laundry" 1)
                               (qty "Emergency Shutdown" 1)
                               (qty "Lamprey" 1)
                               (qty "Data Folding" 1)
                               (qty "Career Fair" 1)]))
    (core/draw state :resPlayer 1)
    (core/gain state :resPlayer :click 1)
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "Adonis Campaign" "New remote")
    (is (= 10 (:credit (get-corp))) "HB:EtF ability paid 1 credit")
    (play-from-hand state :resPlayer "Breaker Bay Grid" "Server 1")
    (play-from-hand state :resPlayer "Ash 2X3ZB9CY" "HQ")
    (let [adon (get-content state :remote1 0)
          bbg (get-content state :remote1 1)
          ash (get-content state :hq 0)]
      (core/rez state :resPlayer bbg)
      (core/rez state :resPlayer adon)
      (is (= 10 (:credit (get-corp))) "Breaker Bay Grid allowed rez of Adonis for free")
      (take-credits state :resPlayer)
      (core/draw state :hazPlayer 1)
      (play-from-hand state :hazPlayer "Career Fair")
      (prompt-select :hazPlayer (find-card "Data Folding" (:hand (get-runner))))
      (is (= 5 (:credit (get-runner))) "Data Folding installed for free by Career Fair")
      (play-from-hand state :hazPlayer "Lamprey")
      (play-from-hand state :hazPlayer "Desperado")
      (is (= 1 (:credit (get-runner))))
      (run-on state "HQ")
      (core/rez state :resPlayer ash)
      (run-successful state)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (and (= 2 (:credit (get-runner))) (= 7 (:credit (get-corp))))
          "Desperado paid 1 to Runner, Lamprey took 1 from Corp")
      (prompt-choice :hazPlayer "No") ; can't afford to trash Ash
      (take-credits state :hazPlayer)
      (play-from-hand state :resPlayer "Caprice Nisei" "Server 1")
      (is (= 11 (:credit (get-corp))) "Gained 3 from Adonis and 1 from HB:EtF")
      (play-from-hand state :resPlayer "Turing" "Server 1")
      (take-credits state :resPlayer 1)
      (is (= 3 (:credit (get-runner))) "Gained 1 from Data Folding")
      (core/gain state :hazPlayer :click 2)
      (run-empty-server state "HQ")
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (prompt-choice :hazPlayer "Yes") ; trash Ash
      (is (and (= 1 (:credit (get-runner))) (= 11 (:credit (get-corp)))))
      (core/gain state :hazPlayer :credit 1)
      (play-from-hand state :hazPlayer "Dirty Laundry")
      (prompt-choice :hazPlayer "HQ")
      (run-successful state)
      (prompt-choice :hazPlayer "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Stole Global Food Initiative")
      (is (and (= 6 (:credit (get-runner))) (= 10 (:credit (get-corp))))
          "Desperado plus Dirty Laundry, Lamprey took 1 from Corp")
      (run-on state "Server 1")
      (let [tur (get-ice state :remote1 0)
            cap (get-content state :remote1 2)]
        (core/rez state :resPlayer tur)
        (is (= 5 (:current-strength (refresh tur))) "Turing +3 strength protecting a remote")
        (card-subroutine state :resPlayer tur 0) ; end the run
        (play-from-hand state :hazPlayer "Emergency Shutdown")
        (prompt-select :hazPlayer tur)
        (is (not (get-in (refresh tur) [:rezzed])) "Turing derezzed")
        (run-on state "Server 1") ; letting Runner in this time to use Caprice
        (core/rez state :resPlayer cap)
        (run-continue state)
        ;; Caprice psi game started automatically
        (prompt-choice :resPlayer "1 [Credits]")
        (prompt-choice :hazPlayer "2 [Credits]")
        (is (not (:run @state)) "Corp won Caprice psi game and ended the run")))))
