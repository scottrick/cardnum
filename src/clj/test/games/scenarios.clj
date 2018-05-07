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
      (default-contestant [(qty "Neural EMP" 1) (qty "Hedge Fund" 3) (qty "SEA Source" 1)])
      (default-hero [(qty "Fall Guy" 1) (qty "Off-Campus Apartment" 1) (qty "Net Shield" 1)
                       (qty "Wireless Net Pavilion" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant 1)
    (is (= 14 (:credit (get-contestant))))
    (core/gain state :hero :click 2)
    (run-empty-server state "Archives") ; enable Corp play of Neural and SEA next turn
    (play-from-hand state :hero "Sure Gamble")
    (play-from-hand state :hero "Off-Campus Apartment")
    (play-from-hand state :hero "Wireless Net Pavilion")
    (play-from-hand state :hero "Net Shield")
    (let [apt (get-resource state 0)]
      (card-ability state :hero apt 0)
      (prompt-select :hero (find-card "Fall Guy" (:hand (get-hero))))
      (take-credits state :hero)
      (is (= 6 (:credit (get-hero))))
      (play-from-hand state :contestant "Neural EMP")
      (let [ns (get-program state 0)
            fg (first (:hosted (refresh apt)))]
        (card-ability state :hero ns 0)
        (is (= 5 (:credit (get-hero))) "Runner paid 1c to survive Neural EMP")
        (prompt-choice :hero "Done")
        (play-from-hand state :contestant "SEA Source")
        (prompt-choice :contestant 3)                             ; boost trace to 6
        (prompt-choice :hero 0)
        (is (= 1 (:tag (get-hero))) "Runner took tag from SEA Source")
        (is (= 7 (:credit (get-contestant))))
        (core/trash-resource state :contestant nil)
        (prompt-select :contestant (find-card "Off-Campus Apartment" (:rig (get-hero))))
        (is (= 3 (:credit (get-contestant))) "WNP increased cost to trash a resource by 2")
        (card-ability state :hero fg 0)                   ; Trash Fall Guy to save the Apartment!
        (is (= (:title (get-resource state 0)) "Off-Campus Apartment")
            "Apartment still standing")
        (is (= (:title (last (:discard (get-hero)))) "Fall Guy") "Fall Guy trashed")))))

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
              (default-hero [(qty "Desperado" 1)
                               (qty "Dirty Laundry" 1)
                               (qty "Emergency Shutdown" 1)
                               (qty "Lamprey" 1)
                               (qty "Data Folding" 1)
                               (qty "Career Fair" 1)]))
    (core/draw state :contestant 1)
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Adonis Campaign" "New remote")
    (is (= 10 (:credit (get-contestant))) "HB:EtF ability paid 1 credit")
    (play-from-hand state :contestant "Breaker Bay Grid" "Server 1")
    (play-from-hand state :contestant "Ash 2X3ZB9CY" "HQ")
    (let [adon (get-content state :remote1 0)
          bbg (get-content state :remote1 1)
          ash (get-content state :hq 0)]
      (core/rez state :contestant bbg)
      (core/rez state :contestant adon)
      (is (= 10 (:credit (get-contestant))) "Breaker Bay Grid allowed rez of Adonis for free")
      (take-credits state :contestant)
      (core/draw state :hero 1)
      (play-from-hand state :hero "Career Fair")
      (prompt-select :hero (find-card "Data Folding" (:hand (get-hero))))
      (is (= 5 (:credit (get-hero))) "Data Folding installed for free by Career Fair")
      (play-from-hand state :hero "Lamprey")
      (play-from-hand state :hero "Desperado")
      (is (= 1 (:credit (get-hero))))
      (run-on state "HQ")
      (core/rez state :contestant ash)
      (run-successful state)
      (prompt-choice :contestant 0)
      (prompt-choice :hero 0)
      (is (and (= 2 (:credit (get-hero))) (= 7 (:credit (get-contestant))))
          "Desperado paid 1 to Runner, Lamprey took 1 from Corp")
      (prompt-choice :hero "No") ; can't afford to trash Ash
      (take-credits state :hero)
      (play-from-hand state :contestant "Caprice Nisei" "Server 1")
      (is (= 11 (:credit (get-contestant))) "Gained 3 from Adonis and 1 from HB:EtF")
      (play-from-hand state :contestant "Turing" "Server 1")
      (take-credits state :contestant 1)
      (is (= 3 (:credit (get-hero))) "Gained 1 from Data Folding")
      (core/gain state :hero :click 2)
      (run-empty-server state "HQ")
      (prompt-choice :contestant 0)
      (prompt-choice :hero 0)
      (prompt-choice :hero "Yes") ; trash Ash
      (is (and (= 1 (:credit (get-hero))) (= 11 (:credit (get-contestant)))))
      (core/gain state :hero :credit 1)
      (play-from-hand state :hero "Dirty Laundry")
      (prompt-choice :hero "HQ")
      (run-successful state)
      (prompt-choice :hero "Steal")
      (is (= 2 (:agenda-point (get-hero))) "Stole Global Food Initiative")
      (is (and (= 6 (:credit (get-hero))) (= 10 (:credit (get-contestant))))
          "Desperado plus Dirty Laundry, Lamprey took 1 from Corp")
      (run-on state "Server 1")
      (let [tur (get-ice state :remote1 0)
            cap (get-content state :remote1 2)]
        (core/rez state :contestant tur)
        (is (= 5 (:current-strength (refresh tur))) "Turing +3 strength protecting a remote")
        (card-subroutine state :contestant tur 0) ; end the run
        (play-from-hand state :hero "Emergency Shutdown")
        (prompt-select :hero tur)
        (is (not (get-in (refresh tur) [:rezzed])) "Turing derezzed")
        (run-on state "Server 1") ; letting Runner in this time to use Caprice
        (core/rez state :contestant cap)
        (run-continue state)
        ;; Caprice psi game started automatically
        (prompt-choice :contestant "1 [Credits]")
        (prompt-choice :hero "2 [Credits]")
        (is (not (:run @state)) "Corp won Caprice psi game and ended the run")))))
