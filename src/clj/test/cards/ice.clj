(ns test.cards.ice
  (:require [game.core :as core]
            [game.utils :refer :all]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest end-the-run
  ;; Since all ETR ice share a common ability, we only need one test
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3) (qty "Hedge Fund" 3) (qty "Restructure" 2)])
              (default-runner))
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (take-credits state :resPlayer 2)
    (run-on state "HQ")
    (is (= [:hq] (get-in @state [:run :server])))
    (let [iwall (get-ice state :hq 0)]
      (core/rez state :resPlayer iwall)
      (card-subroutine state :resPlayer iwall 0)
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:hazPlayer :register :unsuccessful-run]) "Run was unsuccessful"))))

(deftest aimor
  ;; Aimor - trash the top 3 cards of the stack, trash Aimor
  (do-game
    (new-game (default-corp [(qty "Aimor" 1)])
              (default-runner [(qty "Sure Gamble" 2) (qty "Desperado" 1)
                               (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :hazPlayer ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :resPlayer "Aimor" "HQ")
    (is (= 1 (count (get-in @state [:resPlayer :servers :hq :ices]))) "Aimor installed")
    (take-credits state :resPlayer)
    (let [aim (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :resPlayer aim)
      (card-subroutine state :resPlayer aim 0)
      (is (= 3 (count (:discard (get-runner)))) "Runner trashed 3 cards")
      (is (= 1 (count (:deck (get-runner)))) "Runner has 1 card in deck"))
    (is (= 0 (count (get-in @state [:resPlayer :servers :hq :ices]))) "Aimor trashed")))

(deftest archangel
  ;; Archangel - accessing from R&D does not cause run to hang.
  (do-game
    (new-game (default-corp [(qty "Archangel" 1) (qty "Hedge Fund" 1)])
              (default-runner [(qty "Bank Job" 1)]))
    (starting-hand state :resPlayer ["Hedge Fund"])
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Bank Job")
    (run-empty-server state :rd)
    (prompt-choice :resPlayer "Yes")
    (prompt-choice :hazPlayer "Yes")
    (prompt-choice :resPlayer 0)
    (prompt-choice :hazPlayer 0)
    (prompt-select :resPlayer (get-resource state 0))
    (prompt-choice :hazPlayer "OK")
    (is (not (:run @state)) "Run ended")))

(deftest architect-untrashable
  ;; Architect is untrashable while installed and rezzed, but trashable if derezzed or from HQ
  (do-game
    (new-game (default-corp [(qty "Architect" 3)])
              (default-runner))
    (play-from-hand state :resPlayer "Architect" "HQ")
    (let [architect (get-ice state :hq 0)]
      (core/rez state :resPlayer architect)
      (core/trash state :resPlayer (refresh architect))
      (is (not= nil (get-ice state :hq 0)) "Architect was trashed, but should be untrashable")
      (core/derez state :resPlayer (refresh architect))
      (core/trash state :resPlayer (refresh architect))
      (is (= nil (get-ice state :hq 0)) "Architect was not trashed, but should be trashable")
      (core/trash state :resPlayer (get-in @state [:resPlayer :hand 0]))
      (is (= (get-in @state [:resPlayer :discard 0 :title]) "Architect"))
      (is (= (get-in @state [:resPlayer :discard 1 :title]) "Architect")))))

(deftest asteroid-belt
  ;; Asteroid Belt - Space ICE rez cost reduced by 3 credits per advancement
  (do-game
    (new-game (default-corp [(qty "Asteroid Belt" 1)])
              (default-runner))
    (core/gain state :resPlayer :credit 5)
    (play-from-hand state :resPlayer "Asteroid Belt" "HQ")
    (let [ab (get-ice state :hq 0)]
      (core/advance state :resPlayer {:card (refresh ab)})
      (core/advance state :resPlayer {:card (refresh ab)})
      (is (= 8 (:credit (get-corp))))
      (is (= 2 (:advance-counter (refresh ab))))
      (core/rez state :resPlayer (refresh ab))
      (is (= 5 (:credit (get-corp))) "Paid 3 credits to rez; 2 advancments on Asteroid Belt"))))

(deftest bandwidth
  ;; Bandwidth - Give the Runner 1 tag; remove 1 tag if the run is successful
  (do-game
    (new-game (default-corp [(qty "Bandwidth" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Bandwidth" "Archives")
    (let [bw (get-ice state :archives 0)]
      (take-credits state :resPlayer)
      (run-on state "Archives")
      (core/rez state :resPlayer bw)
      (card-subroutine state :resPlayer bw 0)
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag")
      (run-successful state)
      (is (= 0 (:tag (get-runner))) "Run successful; Runner lost 1 tag")
      (run-on state "Archives")
      (card-subroutine state :resPlayer bw 0)
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag")
      (run-jack-out state)
      (is (= 1 (:tag (get-runner))) "Run unsuccessful; Runner kept 1 tag"))))

(deftest bullfrog
  ;; Bullfrog - Win psi to move to outermost position of another server and continue run there
  (do-game
    (new-game (default-corp [(qty "Bullfrog" 1) (qty "Pup" 2)])
              (default-runner))
    (play-from-hand state :resPlayer "Bullfrog" "HQ")
    (play-from-hand state :resPlayer "Pup" "R&D")
    (play-from-hand state :resPlayer "Pup" "R&D")
    (take-credits state :resPlayer)
    (run-on state :hq)
    (let [frog (get-ice state :hq 0)]
      (core/rez state :resPlayer frog)
      (is (= :hq (first (get-in @state [:run :server]))))
      (card-subroutine state :resPlayer frog 0)
      (prompt-choice :resPlayer "0 [Credits]")
      (prompt-choice :hazPlayer "1 [Credits]")
      (prompt-choice :resPlayer "R&D")
      (is (= :rd (first (get-in @state [:run :server]))) "Run redirected to R&D")
      (is (= 2 (get-in @state [:run :position])) "Passed Bullfrog")
      (is (= "Bullfrog" (:title (get-ice state :rd 2))) "Bullfrog at outermost position of R&D"))))

(deftest cell-portal
  ;; Cell Portal - Bounce Runner to outermost position and derez itself
  (do-game
    (new-game (default-corp [(qty "Cell Portal" 1) (qty "Paper Wall" 2)])
              (default-runner))
    (core/gain state :resPlayer :credit 5)
    (play-from-hand state :resPlayer "Cell Portal" "HQ")
    (play-from-hand state :resPlayer "Paper Wall" "HQ")
    (play-from-hand state :resPlayer "Paper Wall" "HQ")
    (take-credits state :resPlayer)
    (run-on state :hq)
    (run-continue state)
    (run-continue state)
    (is (= 1 (get-in @state [:run :position])))
    (let [cp (get-ice state :hq 0)]
      (core/rez state :resPlayer cp)
      (card-subroutine state :resPlayer cp 0)
      (is (= 3 (get-in @state [:run :position])) "Run back at outermost position")
      (is (not (get-in (refresh cp) [:rezzed])) "Cell Portal derezzed"))))

(deftest chimera
  ;; Chimera - Gains chosen subtype
  (do-game
    (new-game (default-corp [(qty "Chimera" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Chimera" "HQ")
    (let [ch (get-ice state :hq 0)]
      (core/rez state :resPlayer ch)
      (prompt-choice :resPlayer "Barrier")
      (is (core/has-subtype? (refresh ch) "Barrier") "Chimera has Barrier")
      (take-credits state :resPlayer)
      (is (not (core/has-subtype? (refresh ch) "Barrier")) "Chimera does not have Barrier"))))

(deftest cortex-lock
  ;; Cortex Lock - Do net damage equal to Runner's unused memory
  (do-game
    (new-game (default-corp [(qty "Cortex Lock" 1)])
              (default-runner [(qty "Corroder" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Cortex Lock" "HQ")
    (take-credits state :resPlayer)
    (let [cort (get-ice state :hq 0)]
      (play-from-hand state :hazPlayer "Corroder")
      (is (= 3 (:memory (get-runner))))
      (run-on state "HQ")
      (core/rez state :resPlayer cort)
      (card-subroutine state :resPlayer cort 0)
      (is (= 3 (count (:discard (get-runner)))) "Runner suffered 3 net damage"))))

(deftest crick
  ;; Crick - Strength boost when protecting Archives; installs a card from Archives
  (do-game
    (new-game (default-corp [(qty "Crick" 2) (qty "Ice Wall" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Crick" "HQ")
    (play-from-hand state :resPlayer "Crick" "Archives")
    (core/move state :resPlayer (find-card "Ice Wall" (:hand (get-corp))) :discard)
    (take-credits state :resPlayer)
    (let [cr1 (get-ice state :hq 0)
          cr2 (get-ice state :archives 0)]
      (core/rez state :resPlayer cr1)
      (core/rez state :resPlayer cr2)
      (is (= 3 (:current-strength (refresh cr1))) "Normal strength over HQ")
      (is (= 6 (:current-strength (refresh cr2))) "+3 strength over Archives")
      (card-subroutine state :resPlayer cr2 0)
      (prompt-select :resPlayer (find-card "Ice Wall" (:discard (get-corp))))
      (prompt-choice :resPlayer "HQ")
      (is (= 3 (:credit (get-corp))) "Paid 1 credit to install as 2nd ICE over HQ"))))

(deftest curtain-wall
  ;; Curtain Wall - Strength boost when outermost ICE
  (do-game
    (new-game (default-corp [(qty "Curtain Wall" 1) (qty "Paper Wall" 1)])
              (default-runner))
    (core/gain state :resPlayer :credit 10)
    (play-from-hand state :resPlayer "Curtain Wall" "HQ")
    (let [curt (get-ice state :hq 0)]
      (core/rez state :resPlayer curt)
      (is (= 10 (:current-strength (refresh curt)))
          "Curtain Wall has +4 strength as outermost ICE")
      (play-from-hand state :resPlayer "Paper Wall" "HQ")
      (let [paper (get-ice state :hq 1)]
        (core/rez state :resPlayer paper)
        (is (= 6 (:current-strength (refresh curt))) "Curtain Wall back to default 6 strength")))))

(deftest data-hound
  ;; Data Hound - Full test
  (do-game
    (new-game (default-corp [(qty "Data Hound" 1)])
              (default-runner [(qty "Sure Gamble" 2) (qty "Desperado" 1)
                               (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :hazPlayer ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :resPlayer "Data Hound" "HQ")
    (take-credits state :resPlayer)
    (let [dh (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :resPlayer dh)
      (card-subroutine state :resPlayer dh 0)
      (prompt-choice :resPlayer 2)
      (prompt-choice :hazPlayer 0)
      ;; trash 1 card and rearrange the other 3
      (prompt-choice :resPlayer (find-card "Desperado" (:deck (get-runner))))
      (is (= 1 (count (:discard (get-runner)))))
      (prompt-choice :resPlayer (find-card "Sure Gamble" (:deck (get-runner))))
      (prompt-choice :resPlayer (find-card "Corroder" (:deck (get-runner))))
      (prompt-choice :resPlayer (find-card "Patron" (:deck (get-runner))))
      ;; try starting over
      (prompt-choice :resPlayer "Start over")
      (prompt-choice :resPlayer (find-card "Patron" (:deck (get-runner))))
      (prompt-choice :resPlayer (find-card "Corroder" (:deck (get-runner))))
      (prompt-choice :resPlayer (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
      (prompt-choice :resPlayer "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
      (is (= "Corroder" (:title (second (:deck (get-runner))))))
      (is (= "Patron" (:title (second (rest (:deck (get-runner)))))))
      (run-jack-out state)
      (run-on state "HQ")
      (card-subroutine state :resPlayer dh 0)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 1)
      ;; trash the only card automatically
      (is (= 2 (count (:discard (get-runner)))))
      (is (= "Corroder" (:title (first (:deck (get-runner)))))))))

(deftest data-mine
  ;; Data Mine - do one net and trash
  (do-game
    (new-game (default-corp [(qty "Data Mine" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Data Mine" "Server 1")
    (take-credits state :resPlayer)
    (let [dm (get-ice state :remote1 0)]
      (run-on state "Server 1")
      (core/rez state :resPlayer dm)
      (card-subroutine state :resPlayer dm 0)
      (is (= 1 (count (:discard (get-runner)))) "Runner suffered 1 net damage"))))

(deftest draco
  ;; Dracō - Pay credits when rezzed to increase strength; trace to give 1 tag and end the run
  (do-game
    (new-game (default-corp [(qty "Dracō" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Dracō" "HQ")
    (take-credits state :resPlayer)
    (let [drac (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :resPlayer drac)
      (prompt-choice :resPlayer 4)
      (is (= 4 (get-counters (refresh drac) :power)) "Dracō has 4 power counters")
      (is (= 4 (:current-strength (refresh drac))) "Dracō is 4 strength")
      (card-subroutine state :resPlayer drac 0)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag")
      (is (nil? (get-in @state [:run])) "Run was ended"))))

(deftest enigma
  ;; Enigma - Force Runner to lose 1 click if able
  (do-game
    (new-game (default-corp [(qty "Enigma" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Enigma" "HQ")
    (take-credits state :resPlayer)
    (let [enig (get-ice state :hq 0)]
      (run-on state "HQ")
      (is (= 3 (:click (get-runner))))
      (core/rez state :resPlayer enig)
      (card-subroutine state :resPlayer enig 0)
      (is (= 2 (:click (get-runner))) "Runner lost 1 click"))))

(deftest excalibur
  ;; Excalibur - Prevent Runner from making another run this turn
  (do-game
    (new-game (default-corp [(qty "Excalibur" 1)])
              (default-runner [(qty "Stimhack" 1)]))
    (play-from-hand state :resPlayer "Excalibur" "HQ")
    (take-credits state :resPlayer)
    (let [excal (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :resPlayer excal)
      (card-subroutine state :resPlayer excal 0)
      (run-jack-out state)
      (run-on state "R&D")
      (is (not (:run @state)) "No run initiated")
      (is (= 3 (:click (get-runner))))
      (play-from-hand state :hazPlayer "Stimhack")
      (is (not (:run @state)) "No run initiated")
      (is (= 3 (:click (get-runner))))
      (is (empty? (:discard (get-runner))) "Card not played from Grip")
      ; Check cannot run flag is cleared on next turn #2474
      (take-credits state :hazPlayer)
      (is (= :resPlayer (:active-player @state)) "Corp turn")
      (core/gain state :hazPlayer :click 1)
      (run-on state "HQ")
      (is (:run @state) "Run initiated ok"))))

(deftest fenris
  ;; Fenris - Illicit ICE give Corp 1 bad publicity when rezzed
  (do-game
    (new-game (default-corp [(qty "Fenris" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Fenris" "HQ")
    (take-credits state :resPlayer)
    (let [fen (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :resPlayer fen)
      (is (= 1 (:bad-publicity (get-corp))) "Gained 1 bad pub")
      (card-subroutine state :resPlayer fen 0)
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 brain damage")
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 4 (core/hand-size state :hazPlayer))))))

(deftest flare
  ;; Flare - Trash 1 program, do 2 unpreventable meat damage, and end the run
  (do-game
    (new-game (default-corp [(qty "Flare" 1)])
              (default-runner [(qty "Plascrete Carapace" 1) (qty "Clone Chip" 1) (qty "Cache" 3)]))
    (play-from-hand state :resPlayer "Flare" "HQ")
    (core/gain state :resPlayer :credit 2)
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Plascrete Carapace")
    (play-from-hand state :hazPlayer "Clone Chip")
    (let [flare (get-ice state :hq 0)
          cc (get-hardware state 1)]
      (run-on state :hq)
      (core/rez state :resPlayer flare)
      (card-subroutine state :resPlayer flare 0)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (prompt-select :resPlayer cc)
      (is (= 1 (count (get-in @state [:hazPlayer :rig :hardware]))) "Clone Chip trashed")
      (is (empty? (:prompt (get-runner))) "Plascrete didn't try preventing meat damage")
      (is (= 1 (count (:hand (get-runner)))))
      (is (= 3 (count (:discard (get-runner)))) "Clone Chip plus 2 cards lost from damage in discard")
      (is (not (:run @state)) "Run ended"))))

(deftest gemini-kicker
  ;; Gemini - Successfully trace to do 1 net damage; do 1 net damage if trace strength is 5 or more regardless of success
  (do-game
    (new-game (default-corp [(qty "Gemini" 1) (qty "Hedge Fund" 2)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Dirty Laundry" 2)]))
    (play-from-hand state :resPlayer "Gemini" "HQ")
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :resPlayer)
    (let [gem (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :resPlayer gem)
      (card-subroutine state :resPlayer gem 0)
      (prompt-choice :resPlayer 3) ; boost to trace strength 5
      (prompt-choice :hazPlayer 0)
      (is (= 2 (count (:discard (get-runner)))) "Did 2 net damage")
      (card-subroutine state :resPlayer gem 0)
      (prompt-choice :resPlayer 3) ; boost to trace strength 5
      (prompt-choice :hazPlayer 5) ; match trace
      (is (= 3 (count (:discard (get-runner)))) "Did only 1 net damage for having trace strength 5 or more"))))

(deftest gemini-chronos-protocol
  ;; Gemini - Interaction with Chronos Protocol and kicker
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Gemini" 1) (qty "Hedge Fund" 2)])
              (default-runner [(qty "Sure Gamble" 1) (qty "Dirty Laundry" 2)]))
    (play-from-hand state :resPlayer "Gemini" "HQ")
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :resPlayer)
    (let [gem (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :resPlayer gem)
      (card-subroutine state :resPlayer gem 0)
      (prompt-choice :resPlayer 3) ; boost to trace strength 5
      (prompt-choice :hazPlayer 0)
      (prompt-choice :resPlayer "Yes")
      (prompt-choice :resPlayer (find-card "Sure Gamble" (:hand (get-runner))))
      (is (= 2 (count (:discard (get-runner)))) "Did 2 net damage"))))

(deftest iq
  ;; IQ - Rez cost and strength equal to cards in HQ
  (do-game
    (new-game (default-corp [(qty "IQ" 3) (qty "Hedge Fund" 3)])
              (default-runner))
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "IQ" "R&D")
    (let [iq1 (get-ice state :rd 0)]
      (core/rez state :resPlayer iq1)
      (is (and (= 4 (count (:hand (get-corp))))
               (= 4 (:current-strength (refresh iq1)))
               (= 5 (:credit (get-corp)))) "4 cards in HQ: paid 4 to rez, has 4 strength")
      (play-from-hand state :resPlayer "IQ" "HQ")
      (let [iq2 (get-ice state :hq 0)]
        (core/rez state :resPlayer iq2)
        (is (and (= 3 (count (:hand (get-corp))))
                 (= 3 (:current-strength (refresh iq1)))
                 (= 3 (:current-strength (refresh iq2)))
                 (= 2 (:credit (get-corp)))) "3 cards in HQ: paid 3 to rez, both have 3 strength")))))

(deftest jua-encounter
  ;; Jua (encounter effect) - Prevent Runner from installing cards for the rest of the turn
  (do-game
    (new-game (default-corp [(qty "Jua" 1)])
              (default-runner [(qty "Desperado" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :resPlayer "Jua" "HQ")
    (take-credits state :resPlayer)
    (let [jua (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :resPlayer jua)
      (card-ability state :resPlayer (refresh jua) 0)
      (run-successful state)
      (is (= 2 (count (:hand (get-runner)))) "Runner starts with 2 cards in hand")
      (play-from-hand state :hazPlayer "Desperado")
      (is (= 2 (count (:hand (get-runner)))) "No cards installed")
      (play-from-hand state :hazPlayer "Sure Gamble")
      (is (= 1 (count (:hand (get-runner)))) "Can play events")
      (take-credits state :hazPlayer)
      (take-credits state :resPlayer)
      (is (= 1 (count (:hand (get-runner)))) "Runner starts with 1 cards in hand")
      (play-from-hand state :hazPlayer "Desperado")
      (is (= 0 (count (:hand (get-runner)))) "Card installed"))))

(deftest jua-sub
  ;; Jua (subroutine effect) - Select 2 runner cards, runner moves one to the stack
  (do-game
    (new-game (default-corp [(qty "Jua" 1)])
              (default-runner [(qty "Desperado" 1) (qty "Gordian Blade" 1)]))
    (play-from-hand state :resPlayer "Jua" "HQ")
    (take-credits state :resPlayer)
    (let [jua (get-ice state :hq 0)]
      (core/gain state :hazPlayer :credit 10)
      (play-from-hand state :hazPlayer "Desperado")
      (run-on state "HQ")
      (core/rez state :resPlayer jua)
      (card-subroutine state :resPlayer (refresh jua) 0)
      (is (empty? (:prompt (get-corp))) "Can't fire for 1 installed card")
      (run-successful state)

      (play-from-hand state :hazPlayer "Gordian Blade")
      (run-on state "HQ")
      (card-subroutine state :resPlayer (refresh jua) 0)
      (prompt-select :resPlayer (get-program state 0))
      (prompt-select :resPlayer (get-hardware state 0))
      (prompt-choice :hazPlayer "Gordian Blade")
      (is (nil? (get-program state 0)) "Card is uninstalled")
      (is (= 1 (count (:deck (get-runner)))) "Runner puts card in deck"))))

(deftest lockdown
  ;; Lockdown - Prevent Runner from drawing cards for the rest of the turn
  (do-game
    (new-game (default-corp [(qty "Lockdown" 1)])
              (default-runner [(qty "Diesel" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Lockdown" "R&D")
    (take-credits state :resPlayer)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (let [lock (get-ice state :rd 0)]
      (run-on state "R&D")
      (core/rez state :resPlayer lock)
      (card-subroutine state :resPlayer lock 0)
      (run-successful state)
      (play-from-hand state :hazPlayer "Diesel")
      (is (= 1 (count (:hand (get-runner)))) "No cards drawn")
      (take-credits state :hazPlayer)
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Diesel")
      (is (= 3 (count (:hand (get-runner))))
          "New turn ends prevention; remaining 3 cards drawn from Stack"))))

(deftest lotus-field-unlowerable
  ;; Lotus Field strength cannot be lowered
  (do-game
    (new-game (default-corp [(qty "Lotus Field" 1) (qty "Lag Time" 1)])
              (default-runner [(qty "Ice Carver" 1) (qty "Parasite" 1)]))
    (play-from-hand state :resPlayer "Lotus Field" "Archives")
    (take-credits state :resPlayer 2)
    (let [lotus (get-ice state :archives 0)]
      (core/rez state :resPlayer lotus)
      (play-from-hand state :hazPlayer "Ice Carver")
      (run-on state "Archives")
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (run-jack-out state)
      (play-from-hand state :hazPlayer "Parasite")
      (prompt-select :hazPlayer lotus)
      (is (= 1 (count (:hosted (refresh lotus)))) "Parasite hosted on Lotus Field")
      (take-credits state :hazPlayer 1)
      (take-credits state :resPlayer)
      (is (= 1 (core/get-virus-counters state :hazPlayer (first (:hosted (refresh lotus)))))
          "Parasite has 1 virus counter")
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (take-credits state :hazPlayer)
      (play-from-hand state :resPlayer "Lag Time")
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased")
      (take-credits state :resPlayer 2)
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased"))))

(deftest mausolus
  ;; Mausolus - 3 adv tokens change the subroutines
  (do-game
    (new-game (default-corp [(qty "Mausolus" 1)])
              (default-runner [(qty "NetChip" 5)]))
    (play-from-hand state :resPlayer "Mausolus" "HQ")
    (let [mau (get-ice state :hq 0)]
      (core/rez state :resPlayer mau)
      (take-credits state :resPlayer)
      (run-on state :hq)
      (is (= 3 (:credit (get-corp))) "corp starts encounter with 3 crs")
      (is (= 0 (count (:discard (get-runner)))) "runner starts encounter with no cards in heap")
      (is (= 0 (:tag (get-runner))) "runner starts encounter with 0 tags")
      (card-subroutine state :resPlayer mau 0)
      (card-subroutine state :resPlayer mau 1)
      (card-subroutine state :resPlayer mau 2)
      (is (= 4 (:credit (get-corp))) "corp gains 1 cr from mausolus")
      (is (= 1 (count (:discard (get-runner)))) "corp does 1 net damage")
      (is (= 1 (:tag (get-runner))) "corp gives 1 tag")
      (run-jack-out state)
      (take-credits state :hazPlayer)
      (core/advance state :resPlayer {:card (refresh mau)})
      (core/advance state :resPlayer {:card (refresh mau)})
      (core/advance state :resPlayer {:card (refresh mau)})
      (run-on state :hq)
      (is (= 1 (:credit (get-corp))) "corp starts encounter with 1 crs")
      (is (= 1 (count (:discard (get-runner)))) "runner starts encounter with 1 card in heap")
      (is (= 1 (:tag (get-runner))) "runner starts encounter with 1 tags")
      (card-subroutine state :resPlayer mau 0)
      (card-subroutine state :resPlayer mau 1)
      (card-subroutine state :resPlayer mau 2)
      (is (= 4 (:credit (get-corp))) "corp gains 3 cr")
      (is (= 4 (count (:discard (get-runner)))) "corp does 3 net damage")
      (is (= 2 (:tag (get-runner))) "corp gives 1 tag")
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:hazPlayer :register :unsuccessful-run]) "Run was unsuccessful"))))

(deftest meru-mati
  (do-game
    (new-game (default-corp [(qty "Meru Mati" 2)])
              (default-runner))
    (play-from-hand state :resPlayer "Meru Mati" "HQ")
    (play-from-hand state :resPlayer "Meru Mati" "R&D")
    (core/rez state :resPlayer (get-ice state :hq 0))
    (core/rez state :resPlayer (get-ice state :rd 0))
    (is (= 4 (:current-strength (get-ice state :hq 0))) "HQ Meru Mati at 4 strength")
	(is (= 1 (:current-strength (get-ice state :rd 0))) "R&D at 0 strength")))

(deftest mind-game
  ;; Mind game - PSI redirect to different server
  (do-game
    (new-game (default-corp [(qty "Mind Game" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Mind Game" "HQ")
    (take-credits state :resPlayer)
    (run-on state :hq)
    (let [mindgame (get-ice state :hq 0)]
      (core/rez state :resPlayer mindgame)
      (card-subroutine state :resPlayer mindgame 0))
    (prompt-choice :resPlayer "1 [Credits]")
    (prompt-choice :hazPlayer "0 [Credits]")
    (is (= (set ["R&D" "Archives"]) (set (:choices (prompt-map :resPlayer)))) "Corp cannot choose server Runner is on")
    (prompt-choice :resPlayer "Archives")
    (is (= [:archives] (get-in @state [:run :server])) "Runner now running on Archives")))

(deftest minelayer
  ;; Minelayer - Install a piece of ICE in outermost position of Minelayer's server at no cost
  (do-game
    (new-game (default-corp [(qty "Minelayer" 1) (qty "Fire Wall" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Minelayer" "HQ")
    (take-credits state :resPlayer)
    (run-on state :hq)
    (core/rez state :resPlayer (get-ice state :hq 0))
    (is (= 6 (:credit (get-corp))))
    (card-subroutine state :resPlayer (get-ice state :hq 0) 0)
    (prompt-select :resPlayer (find-card "Fire Wall" (:hand (get-corp))))
    (is (= 2 (count (get-in @state [:resPlayer :servers :hq :ices]))) "2 ICE protecting HQ")
    (is (= 6 (:credit (get-corp))) "Didn't pay 1 credit to install as second ICE")))

(deftest morph-ice-subtype-changing
  ;; Morph ice gain and lose subtypes from normal advancements and placed advancements
  (do-game
    (new-game (default-corp [(qty "Wendigo" 1)
                             (qty "Shipment from SanSan" 1)
                             (qty "Superior Cyberwalls" 1)])
              (default-runner))
    (core/gain state :resPlayer :click 2)
    (play-from-hand state :resPlayer "Superior Cyberwalls" "New remote")
    (let [sc (get-content state :remote1 0)]
      (score-agenda state :resPlayer sc)
      (play-from-hand state :resPlayer "Wendigo" "HQ")
      (let [wend (get-ice state :hq 0)]
        (core/rez state :resPlayer wend)
        (is (= 4 (:current-strength (refresh wend))) "Wendigo at normal 4 strength")
        (core/advance state :resPlayer {:card (refresh wend)})
        (is (= true (has? (refresh wend) :subtype "Barrier")) "Wendigo gained Barrier")
        (is (= false (has? (refresh wend) :subtype "Code Gate")) "Wendigo lost Code Gate")
        (is (= 5 (:current-strength (refresh wend))) "Wendigo boosted to 5 strength by scored Superior Cyberwalls")
        (play-from-hand state :resPlayer "Shipment from SanSan")
        (prompt-choice :resPlayer "1")
        (prompt-select :resPlayer wend)
        (is (= false (has? (refresh wend) :subtype "Barrier")) "Wendigo lost Barrier")
        (is (= true (has? (refresh wend) :subtype "Code Gate")) "Wendigo gained Code Gate")
        (is (= 4 (:current-strength (refresh wend))) "Wendigo returned to normal 4 strength")))))

(deftest mother-goddess
  ;; Mother Goddess - Gains other ice subtypes
  (do-game
    (new-game (default-corp [(qty "Mother Goddess" 1) (qty "NEXT Bronze" 1)])
              (default-runner))
    (core/gain state :resPlayer :credit 1)
    (play-from-hand state :resPlayer "Mother Goddess" "HQ")
    (play-from-hand state :resPlayer "NEXT Bronze" "R&D")
    (let [mg (get-ice state :hq 0)
          nb (get-ice state :rd 0)]
      (core/rez state :resPlayer mg)
      (is (core/has-subtype? (refresh mg) "Mythic") "Mother Goddess has Mythic")
      (is (not (core/has-subtype? (refresh mg) "Code Gate")) "Mother Goddess does not have Code Gate")
      (is (not (core/has-subtype? (refresh mg) "NEXT")) "Mother Goddess does not have NEXT")
      (core/rez state :resPlayer nb)
      (is (core/has-subtype? (refresh mg) "Mythic") "Mother Goddess has Mythic")
      (is (core/has-subtype? (refresh mg) "Code Gate") "Mother Goddess has Code Gate")
      (is (core/has-subtype? (refresh mg) "NEXT") "Mother Goddess has NEXT"))))

(deftest next-bronze
  ;; NEXT Bronze - Add 1 strength for every rezzed NEXT ice
  (do-game
    (new-game (default-corp [(qty "NEXT Bronze" 2) (qty "NEXT Silver" 1)])
              (default-runner))
    (core/gain state :resPlayer :credit 2)
    (play-from-hand state :resPlayer "NEXT Bronze" "HQ")
    (play-from-hand state :resPlayer "NEXT Bronze" "R&D")
    (play-from-hand state :resPlayer "NEXT Silver" "Archives")
    (let [nb1 (get-ice state :hq 0)
          nb2 (get-ice state :rd 0)
          ns1 (get-ice state :archives 0)]
      (core/rez state :resPlayer nb1)
      (is (= 1 (:current-strength (refresh nb1)))
          "NEXT Bronze at 1 strength: 1 rezzed NEXT ice")
      (core/rez state :resPlayer nb2)
      (is (= 2 (:current-strength (refresh nb1)))
          "NEXT Bronze at 2 strength: 2 rezzed NEXT ice")
      (is (= 2 (:current-strength (refresh nb2)))
          "NEXT Bronze at 2 strength: 2 rezzed NEXT ice")
      (core/rez state :resPlayer ns1)
      (is (= 3 (:current-strength (refresh nb1)))
          "NEXT Bronze at 3 strength: 3 rezzed NEXT ice")
      (is (= 3 (:current-strength (refresh nb2)))
          "NEXT Bronze at 3 strength: 3 rezzed NEXT ice"))))

(deftest nightdancer
  ;; Nightdancer - Runner loses a click if able, corp gains a click on next turn
  (do-game
    (new-game (default-corp [(qty "Nightdancer" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Nightdancer" "HQ")
    (take-credits state :resPlayer)
    (let [nd (get-ice state :hq 0)]
      (core/rez state :resPlayer nd)
      (run-on state "HQ")
      (is (= 3 (:click (get-runner))) "Runner starts with 3 clicks")
      (card-subroutine state :resPlayer nd 0)
      (is (= 2 (:click (get-runner))) "Runner lost 1 click")
      (card-subroutine state :resPlayer nd 0)
      (is (= 1 (:click (get-runner))) "Runner lost 1 click")
      (run-jack-out state)
      (take-credits state :hazPlayer)
      (is (= 5 (:click (get-corp))) "Corp has 5 clicks"))))

(deftest resistor
  ;; Resistor - Strength equal to Runner tags, lose strength when Runner removes a tag
  (do-game
    (new-game (default-corp [(qty "Resistor" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Resistor" "HQ")
    (let [resistor (get-ice state :hq 0)]
      (core/rez state :resPlayer resistor)
      (is (= 0 (:current-strength (refresh resistor))) "No Runner tags; 0 strength")
      (core/tag-runner state :hazPlayer 2)
      (is (= 2 (:tag (get-runner))))
      (is (= 2 (:current-strength (refresh resistor))) "2 Runner tags; 2 strength")
      (take-credits state :resPlayer)
      (core/remove-tag state :hazPlayer 1)
      (is (= 1 (:current-strength (refresh resistor))) "Runner removed 1 tag; down to 1 strength"))))

(deftest self-adapting-code-wall-unlowerable
  ;; self-adapting code wall strength cannot be lowered
  (do-game
    (new-game (default-corp [(qty "Self-Adapting Code Wall" 1) (qty "Lag Time" 1)])
              (default-runner [(qty "Ice Carver" 1) (qty "Parasite" 1)]))
    (play-from-hand state :resPlayer "Self-Adapting Code Wall" "Archives")
    (take-credits state :resPlayer 2)
    (let [sacw (get-ice state :archives 0)]
      (core/rez state :resPlayer sacw)
      (play-from-hand state :hazPlayer "Ice Carver")
      (run-on state "Archives")
      (is (= 1 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength unchanged")
      (run-jack-out state)
      (play-from-hand state :hazPlayer "Parasite")
      (prompt-select :hazPlayer sacw)
      (is (= 1 (count (:hosted (refresh sacw)))) "Parasite hosted on Self-Adapting Code Wall")
      (take-credits state :hazPlayer 1)
      (take-credits state :resPlayer)
      (is (= 1 (core/get-virus-counters state :hazPlayer (first (:hosted (refresh sacw)))))
          "Parasite has 1 virus counter")
      (is (= 1 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength unchanged")
      (take-credits state :hazPlayer)
      (play-from-hand state :resPlayer "Lag Time")
      (is (= 2 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength increased")
      (take-credits state :resPlayer 2)
      (is (= 2 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength increased"))))

(deftest searchlight
  ;; Searchlight - Trace bace equal to advancement counters
  (do-game
    (new-game (default-corp [(qty "Searchlight" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Searchlight" "HQ")
    (let [searchlight (get-ice state :hq 0)]
      (core/rez state :resPlayer searchlight)
      (card-subroutine state :resPlayer (refresh searchlight) 0)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (= 0 (:tag (get-runner))) "Trace failed with 0 advancements")
      (core/advance state :resPlayer {:card (refresh searchlight)})
      (card-subroutine state :resPlayer (refresh searchlight) 0)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (= 1 (:tag (get-runner))) "Trace succeeds with 0 advancements"))))

(deftest sherlock
  ;; Sherlock 1.0 - Trace to add an installed program to the top of Runner's Stack
  (do-game
    (new-game (default-corp [(qty "Sherlock 1.0" 1)])
              (default-runner [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Sherlock 1.0" "HQ")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Gordian Blade")
    (run-on state :hq)
    (core/rez state :resPlayer (get-ice state :hq 0))
    (card-subroutine state :resPlayer (get-ice state :hq 0) 0)
    (prompt-choice :resPlayer 0)
    (prompt-choice :hazPlayer 0)
    (prompt-select :resPlayer (get-in @state [:hazPlayer :rig :program 0]))
    (is (empty? (get-in @state [:hazPlayer :rig :program])) "Gordian uninstalled")
    (is (= "Gordian Blade" (:title (first (:deck (get-runner))))) "Gordian on top of Stack")))

(deftest shiro
  ;; Shiro - Full test
  (do-game
    (new-game (default-corp [(qty "Shiro" 1) (qty "Caprice Nisei" 1)
                             (qty "Quandary" 1) (qty "Jackson Howard" 1)])
              (default-runner [(qty "R&D Interface" 1)]))
    (starting-hand state :resPlayer ["Shiro"])
    (play-from-hand state :resPlayer "Shiro" "HQ")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "R&D Interface")
    (let [shiro (get-ice state :hq 0)]
      (run-on state :hq)
      (core/rez state :resPlayer shiro)
      (card-subroutine state :resPlayer shiro 0)
      (prompt-choice :resPlayer (find-card "Caprice Nisei" (:deck (get-corp))))
      (prompt-choice :resPlayer (find-card "Quandary" (:deck (get-corp))))
      (prompt-choice :resPlayer (find-card "Jackson Howard" (:deck (get-corp))))
      ;; try starting over
      (prompt-choice :resPlayer "Start over")
      (prompt-choice :resPlayer (find-card "Jackson Howard" (:deck (get-corp))))
      (prompt-choice :resPlayer (find-card "Quandary" (:deck (get-corp))))
      (prompt-choice :resPlayer (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
      (prompt-choice :resPlayer "Done")
      (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
      (is (= "Quandary" (:title (second (:deck (get-corp))))))
      (is (= "Jackson Howard" (:title (second (rest (:deck (get-corp)))))))
      (card-subroutine state :resPlayer shiro 1)
      (is (= (:cid (first (:deck (get-corp))))
             (:cid (:card (first (:prompt (get-runner)))))) "Access the top card of R&D")
      (prompt-choice :hazPlayer "No")
      (is (= (:cid (second (:deck (get-corp))))
             (:cid (:card (first (:prompt (get-runner)))))) "Access another card due to R&D Interface"))))

(deftest snowflake
  ;; Snowflake - Win a psi game to end the run
  (do-game
    (new-game (default-corp [(qty "Snowflake" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Snowflake" "HQ")
    (take-credits state :resPlayer)
    (run-on state :hq)
    (let [sf (get-ice state :hq 0)]
      (core/rez state :resPlayer sf)
      (card-subroutine state :resPlayer sf 0)
      (prompt-choice :resPlayer "0 [Credits]")
      (prompt-choice :hazPlayer "0 [Credits]")
      (is (:run @state) "Runner won psi, run continues")
      (card-subroutine state :resPlayer sf 0)
      (prompt-choice :resPlayer "0 [Credits]")
      (prompt-choice :hazPlayer "1 [Credits]")
      (is (not (:run @state)) "Run ended"))))

(deftest special-offer-trash-ice-during-run
  ;; Special Offer trashes itself and updates the run position
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1) (qty "Special Offer" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (play-from-hand state :resPlayer "Special Offer" "HQ")
    (take-credits state :resPlayer 1)
    (run-on state "HQ")
    (is (= 2 (:position (get-in @state [:run]))) "Initial position approaching Special Offer")
    (let [special (get-ice state :hq 1)]
      (core/rez state :resPlayer special)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :resPlayer special 0)
      (is (= 9 (:credit (get-corp))) "Special Offer paid 5 credits")
      (is (= 1 (:position (get-in @state [:run])))
          "Run position updated; now approaching Ice Wall"))))

(deftest tithonium
  ;; Forfeit option as rez cost, can have hosted condition counters
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 1) (qty "Tithonium" 1) (qty "Patch" 1)])
              (default-runner [(qty "Pawn" 1) (qty "Wasteland" 1)]))
    (core/gain state :resPlayer :click 10)
    (play-from-hand state :resPlayer "Hostile Takeover" "New remote")
    (play-from-hand state :resPlayer "Tithonium" "HQ")
    (let [ht (get-content state :remote1 0)
          ti (get-ice state :hq 0)]
      (score-agenda state :resPlayer ht)
      (is (= 1 (count (:scored (get-corp)))) "Agenda scored")
      (is (= 12 (:credit (get-corp))) "Gained 7 credits")
      (core/rez state :resPlayer ti)
      (prompt-choice :resPlayer "No") ; don't use alternative cost
      (is (= 3 (:credit (get-corp))) "Spent 9 to Rez")
      (core/derez state :resPlayer (refresh ti))
      (core/rez state :resPlayer ti)
      (prompt-choice :resPlayer "Yes") ; use alternative cost
      (prompt-select :resPlayer (get-in (get-corp) [:scored 0]))
      (is (= 3 (:credit (get-corp))) "Still on 3c")
      (is (= 0 (count (:scored (get-corp)))) "Agenda forfeited")
      ;; Can Host Conditions Counters
      (play-from-hand state :resPlayer "Patch")
      (prompt-select :resPlayer (refresh ti))
      (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
      (take-credits state :resPlayer)
      (core/derez state :resPlayer (refresh ti))
      (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
      (play-from-hand state :hazPlayer "Pawn")
      (play-from-hand state :hazPlayer "Wasteland")
      (let [pawn (get-program state 0)
            wast (get-resource state 0)]
        (card-ability state :hazPlayer (refresh pawn) 0)
        (prompt-select :hazPlayer (refresh ti))
        (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
        (core/derez state :resPlayer (refresh ti))
        (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
        (run-on state "HQ")
        (card-subroutine state :resPlayer ti 2)
        (prompt-select :resPlayer (refresh wast))
        (is (= 1 (count (:discard (get-runner)))) "1 card trashed")
        (card-subroutine state :resPlayer ti 1)
        (is (not (:run @state)) "Run ended")))))

(deftest tithonium-oversight-ai
  ;; Do not prompt for alt cost #2734
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 1) (qty "Oversight AI" 1) (qty "Tithonium" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Hostile Takeover" "New remote")
    (play-from-hand state :resPlayer "Tithonium" "R&D")
    (let [ht (get-content state :remote1 0)
          ti (get-ice state :rd 0)]
      (score-agenda state :resPlayer ht)
      (play-from-hand state :resPlayer "Oversight AI")
      (prompt-select :resPlayer ti)
      (is (get-in (refresh ti) [:rezzed]))
      (is (= "Oversight AI" (:title (first (:hosted (refresh ti)))))
          "Tithonium hosting OAI as a condition"))))

(deftest tmi
  ;; TMI ICE test
  (do-game
    (new-game (default-corp [(qty "TMI" 3)])
              (default-runner))
    (play-from-hand state :resPlayer "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
      (core/rez state :resPlayer tmi)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (get-in (refresh tmi) [:rezzed])))))

(deftest tmi-derez
  ;; TMI ICE trace derez
  (do-game
    (new-game (default-corp [(qty "TMI" 3)])
              (make-deck "Sunny Lebeau: Security Specialist" [(qty "Blackmail" 3)]))
    (play-from-hand state :resPlayer "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
      (core/rez state :resPlayer tmi)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (not (get-in (refresh tmi) [:rezzed]))))))

(deftest turing-positional-strength
  ;; Turing - Strength boosted when protecting a remote server
  (do-game
    (new-game (default-corp [(qty "Turing" 2) (qty "Hedge Fund" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "Turing" "HQ")
    (play-from-hand state :resPlayer "Turing" "New remote")
    (let [t1 (get-ice state :hq 0)
          t2 (get-ice state :remote1 0)]
      (core/rez state :resPlayer t1)
      (is (= 2 (:current-strength (refresh t1)))
          "Turing default 2 strength over a central server")
      (core/rez state :resPlayer t2)
      (is (= 5 (:current-strength (refresh t2)))
          "Turing increased to 5 strength over a remote server"))))

(deftest wraparound
  ;; Wraparound - Strength boosted when no fracter is installed
  (do-game
    (new-game (default-corp [(qty "Wraparound" 1)])
              (default-runner [(qty "Corroder" 1)]))
    (play-from-hand state :resPlayer "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (core/rez state :resPlayer wrap)
      (is (= 7 (:current-strength (refresh wrap)))
          "Wraparound +7 strength with no fracter in play")
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Corroder")
      (is (= 0 (:current-strength (refresh wrap)))
          "Wraparound 0 strength after Corroder installed"))))
