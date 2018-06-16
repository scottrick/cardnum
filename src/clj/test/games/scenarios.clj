(ns test.games.scenarios
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest minigame-prevent-netdmg-mutherefftrash
  "Mini-game testing prevention of net damage and muthereff trashing, with hosted Fall Guy"
  (do-game
    (new-game
      (default-contestant [(qty "Neural EMP" 1) (qty "Hedge Fund" 3) (qty "SEA Source" 1)])
      (default-challenger [(qty "Fall Guy" 1) (qty "Off-Campus Apartment" 1) (qty "Net Shield" 1)
                       (qty "Wireless Net Pavilion" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant 1)
    (is (= 14 (:credit (get-contestant))))
    (core/gain state :challenger :click 2)
    (run-empty-server state "Archives") ; enable Contestant play of Neural and SEA next turn
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Off-Campus Apartment")
    (play-from-hand state :challenger "Wireless Net Pavilion")
    (play-from-hand state :challenger "Net Shield")
    (let [apt (get-muthereff state 0)]
      (card-ability state :challenger apt 0)
      (prompt-select :challenger (find-card "Fall Guy" (:hand (get-challenger))))
      (take-credits state :challenger)
      (is (= 6 (:credit (get-challenger))))
      (play-from-hand state :contestant "Neural EMP")
      (let [ns (get-resource state 0)
            fg (first (:hosted (refresh apt)))]
        (card-ability state :challenger ns 0)
        (is (= 5 (:credit (get-challenger))) "Challenger paid 1c to survive Neural EMP")
        (prompt-choice :challenger "Done")
        (play-from-hand state :contestant "SEA Source")
        (prompt-choice :contestant 3)                             ; boost trace to 6
        (prompt-choice :challenger 0)
        (is (= 1 (:tag (get-challenger))) "Challenger took tag from SEA Source")
        (is (= 7 (:credit (get-contestant))))
        (core/trash-muthereff state :contestant nil)
        (prompt-select :contestant (find-card "Off-Campus Apartment" (:rig (get-challenger))))
        (is (= 3 (:credit (get-contestant))) "WNP increased cost to trash a muthereff by 2")
        (card-ability state :challenger fg 0)                   ; Trash Fall Guy to save the Apartment!
        (is (= (:title (get-muthereff state 0)) "Off-Campus Apartment")
            "Apartment still standing")
        (is (= (:title (last (:discard (get-challenger)))) "Fall Guy") "Fall Guy trashed")))))

(deftest hb-glacier
  "HB Glacier econ and server protection with regions - Ash, Caprcharacter, Breaker Bay Grid, positional character strength boost"
  (do-game
    (new-game (make-deck "Haas-Bioroid: Engineering the Future"
                         [(qty "Adonis Campaign" 1)
                          (qty "Global Food Initiative" 1)
                          (qty "Breaker Bay Grid" 1)
                          (qty "Caprcharacter Nisei" 1)
                          (qty "Ash 2X3ZB9CY" 1)
                          (qty "Turing" 1)
                          (qty "Hedge Fund" 1)])
              (default-challenger [(qty "Desperado" 1)
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
      (core/draw state :challenger 1)
      (play-from-hand state :challenger "Career Fair")
      (prompt-select :challenger (find-card "Data Folding" (:hand (get-challenger))))
      (is (= 5 (:credit (get-challenger))) "Data Folding installed for free by Career Fair")
      (play-from-hand state :challenger "Lamprey")
      (play-from-hand state :challenger "Desperado")
      (is (= 1 (:credit (get-challenger))))
      (run-on state "HQ")
      (core/rez state :contestant ash)
      (run-successful state)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (and (= 2 (:credit (get-challenger))) (= 7 (:credit (get-contestant))))
          "Desperado paid 1 to Challenger, Lamprey took 1 from Contestant")
      (prompt-choice :challenger "No") ; can't afford to trash Ash
      (take-credits state :challenger)
      (play-from-hand state :contestant "Caprcharacter Nisei" "Server 1")
      (is (= 11 (:credit (get-contestant))) "Gained 3 from Adonis and 1 from HB:EtF")
      (play-from-hand state :contestant "Turing" "Server 1")
      (take-credits state :contestant 1)
      (is (= 3 (:credit (get-challenger))) "Gained 1 from Data Folding")
      (core/gain state :challenger :click 2)
      (run-empty-server state "HQ")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (prompt-choice :challenger "Yes") ; trash Ash
      (is (and (= 1 (:credit (get-challenger))) (= 11 (:credit (get-contestant)))))
      (core/gain state :challenger :credit 1)
      (play-from-hand state :challenger "Dirty Laundry")
      (prompt-choice :challenger "HQ")
      (run-successful state)
      (prompt-choice :challenger "Steal")
      (is (= 2 (:agenda-point (get-challenger))) "Stole Global Food Initiative")
      (is (and (= 6 (:credit (get-challenger))) (= 10 (:credit (get-contestant))))
          "Desperado plus Dirty Laundry, Lamprey took 1 from Contestant")
      (run-on state "Server 1")
      (let [tur (get-character state :remote1 0)
            cap (get-content state :remote1 2)]
        (core/rez state :contestant tur)
        (is (= 5 (:current-strength (refresh tur))) "Turing +3 strength protecting a remote")
        (card-subroutine state :contestant tur 0) ; end the run
        (play-from-hand state :challenger "Emergency Shutdown")
        (prompt-select :challenger tur)
        (is (not (get-in (refresh tur) [:rezzed])) "Turing derezzed")
        (run-on state "Server 1") ; letting Challenger in this time to use Caprcharacter
        (core/rez state :contestant cap)
        (run-continue state)
        ;; Caprcharacter psi game started automatically
        (prompt-choice :contestant "1 [Credits]")
        (prompt-choice :challenger "2 [Credits]")
        (is (not (:run @state)) "Contestant won Caprcharacter psi game and ended the run")))))
