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
    (new-game (default-minion [(qty "Ice Wall" 3) (qty "Hedge Fund" 3) (qty "Restructure" 2)])
              (default-hero))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (take-credits state :minion 2)
    (run-on state "HQ")
    (is (= [:hq] (get-in @state [:run :server])))
    (let [iwall (get-ice state :hq 0)]
      (core/rez state :minion iwall)
      (card-subroutine state :minion iwall 0)
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:hero :register :unsuccessful-run]) "Run was unsuccessful"))))

(deftest aimor
  ;; Aimor - trash the top 3 cards of the stack, trash Aimor
  (do-game
    (new-game (default-minion [(qty "Aimor" 1)])
              (default-hero [(qty "Sure Gamble" 2) (qty "Desperado" 1)
                               (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :hero ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :minion "Aimor" "HQ")
    (is (= 1 (count (get-in @state [:minion :servers :hq :ices]))) "Aimor installed")
    (take-credits state :minion)
    (let [aim (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :minion aim)
      (card-subroutine state :minion aim 0)
      (is (= 3 (count (:discard (get-hero)))) "Runner trashed 3 cards")
      (is (= 1 (count (:deck (get-hero)))) "Runner has 1 card in deck"))
    (is (= 0 (count (get-in @state [:minion :servers :hq :ices]))) "Aimor trashed")))

(deftest archangel
  ;; Archangel - accessing from R&D does not cause run to hang.
  (do-game
    (new-game (default-minion [(qty "Archangel" 1) (qty "Hedge Fund" 1)])
              (default-hero [(qty "Bank Job" 1)]))
    (starting-hand state :minion ["Hedge Fund"])
    (take-credits state :minion)
    (play-from-hand state :hero "Bank Job")
    (run-empty-server state :rd)
    (prompt-choice :minion "Yes")
    (prompt-choice :hero "Yes")
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (prompt-select :minion (get-resource state 0))
    (prompt-choice :hero "OK")
    (is (not (:run @state)) "Run ended")))

(deftest architect-untrashable
  ;; Architect is untrashable while installed and rezzed, but trashable if derezzed or from HQ
  (do-game
    (new-game (default-minion [(qty "Architect" 3)])
              (default-hero))
    (play-from-hand state :minion "Architect" "HQ")
    (let [architect (get-ice state :hq 0)]
      (core/rez state :minion architect)
      (core/trash state :minion (refresh architect))
      (is (not= nil (get-ice state :hq 0)) "Architect was trashed, but should be untrashable")
      (core/derez state :minion (refresh architect))
      (core/trash state :minion (refresh architect))
      (is (= nil (get-ice state :hq 0)) "Architect was not trashed, but should be trashable")
      (core/trash state :minion (get-in @state [:minion :hand 0]))
      (is (= (get-in @state [:minion :discard 0 :title]) "Architect"))
      (is (= (get-in @state [:minion :discard 1 :title]) "Architect")))))

(deftest asteroid-belt
  ;; Asteroid Belt - Space ICE rez cost reduced by 3 credits per advancement
  (do-game
    (new-game (default-minion [(qty "Asteroid Belt" 1)])
              (default-hero))
    (core/gain state :minion :credit 5)
    (play-from-hand state :minion "Asteroid Belt" "HQ")
    (let [ab (get-ice state :hq 0)]
      (core/advance state :minion {:card (refresh ab)})
      (core/advance state :minion {:card (refresh ab)})
      (is (= 8 (:credit (get-minion))))
      (is (= 2 (:advance-counter (refresh ab))))
      (core/rez state :minion (refresh ab))
      (is (= 5 (:credit (get-minion))) "Paid 3 credits to rez; 2 advancments on Asteroid Belt"))))

(deftest bandwidth
  ;; Bandwidth - Give the Runner 1 tag; remove 1 tag if the run is successful
  (do-game
    (new-game (default-minion [(qty "Bandwidth" 1)])
              (default-hero))
    (play-from-hand state :minion "Bandwidth" "Archives")
    (let [bw (get-ice state :archives 0)]
      (take-credits state :minion)
      (run-on state "Archives")
      (core/rez state :minion bw)
      (card-subroutine state :minion bw 0)
      (is (= 1 (:tag (get-hero))) "Runner took 1 tag")
      (run-successful state)
      (is (= 0 (:tag (get-hero))) "Run successful; Runner lost 1 tag")
      (run-on state "Archives")
      (card-subroutine state :minion bw 0)
      (is (= 1 (:tag (get-hero))) "Runner took 1 tag")
      (run-jack-out state)
      (is (= 1 (:tag (get-hero))) "Run unsuccessful; Runner kept 1 tag"))))

(deftest bullfrog
  ;; Bullfrog - Win psi to move to outermost position of another server and continue run there
  (do-game
    (new-game (default-minion [(qty "Bullfrog" 1) (qty "Pup" 2)])
              (default-hero))
    (play-from-hand state :minion "Bullfrog" "HQ")
    (play-from-hand state :minion "Pup" "R&D")
    (play-from-hand state :minion "Pup" "R&D")
    (take-credits state :minion)
    (run-on state :hq)
    (let [frog (get-ice state :hq 0)]
      (core/rez state :minion frog)
      (is (= :hq (first (get-in @state [:run :server]))))
      (card-subroutine state :minion frog 0)
      (prompt-choice :minion "0 [Credits]")
      (prompt-choice :hero "1 [Credits]")
      (prompt-choice :minion "R&D")
      (is (= :rd (first (get-in @state [:run :server]))) "Run redirected to R&D")
      (is (= 2 (get-in @state [:run :position])) "Passed Bullfrog")
      (is (= "Bullfrog" (:title (get-ice state :rd 2))) "Bullfrog at outermost position of R&D"))))

(deftest cell-portal
  ;; Cell Portal - Bounce Runner to outermost position and derez itself
  (do-game
    (new-game (default-minion [(qty "Cell Portal" 1) (qty "Paper Wall" 2)])
              (default-hero))
    (core/gain state :minion :credit 5)
    (play-from-hand state :minion "Cell Portal" "HQ")
    (play-from-hand state :minion "Paper Wall" "HQ")
    (play-from-hand state :minion "Paper Wall" "HQ")
    (take-credits state :minion)
    (run-on state :hq)
    (run-continue state)
    (run-continue state)
    (is (= 1 (get-in @state [:run :position])))
    (let [cp (get-ice state :hq 0)]
      (core/rez state :minion cp)
      (card-subroutine state :minion cp 0)
      (is (= 3 (get-in @state [:run :position])) "Run back at outermost position")
      (is (not (get-in (refresh cp) [:rezzed])) "Cell Portal derezzed"))))

(deftest chimera
  ;; Chimera - Gains chosen subtype
  (do-game
    (new-game (default-minion [(qty "Chimera" 1)])
              (default-hero))
    (play-from-hand state :minion "Chimera" "HQ")
    (let [ch (get-ice state :hq 0)]
      (core/rez state :minion ch)
      (prompt-choice :minion "Barrier")
      (is (core/has-subtype? (refresh ch) "Barrier") "Chimera has Barrier")
      (take-credits state :minion)
      (is (not (core/has-subtype? (refresh ch) "Barrier")) "Chimera does not have Barrier"))))

(deftest cortex-lock
  ;; Cortex Lock - Do net damage equal to Runner's unused memory
  (do-game
    (new-game (default-minion [(qty "Cortex Lock" 1)])
              (default-hero [(qty "Corroder" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Cortex Lock" "HQ")
    (take-credits state :minion)
    (let [cort (get-ice state :hq 0)]
      (play-from-hand state :hero "Corroder")
      (is (= 3 (:memory (get-hero))))
      (run-on state "HQ")
      (core/rez state :minion cort)
      (card-subroutine state :minion cort 0)
      (is (= 3 (count (:discard (get-hero)))) "Runner suffered 3 net damage"))))

(deftest crick
  ;; Crick - Strength boost when protecting Archives; installs a card from Archives
  (do-game
    (new-game (default-minion [(qty "Crick" 2) (qty "Ice Wall" 1)])
              (default-hero))
    (play-from-hand state :minion "Crick" "HQ")
    (play-from-hand state :minion "Crick" "Archives")
    (core/move state :minion (find-card "Ice Wall" (:hand (get-minion))) :discard)
    (take-credits state :minion)
    (let [cr1 (get-ice state :hq 0)
          cr2 (get-ice state :archives 0)]
      (core/rez state :minion cr1)
      (core/rez state :minion cr2)
      (is (= 3 (:current-strength (refresh cr1))) "Normal strength over HQ")
      (is (= 6 (:current-strength (refresh cr2))) "+3 strength over Archives")
      (card-subroutine state :minion cr2 0)
      (prompt-select :minion (find-card "Ice Wall" (:discard (get-minion))))
      (prompt-choice :minion "HQ")
      (is (= 3 (:credit (get-minion))) "Paid 1 credit to install as 2nd ICE over HQ"))))

(deftest curtain-wall
  ;; Curtain Wall - Strength boost when outermost ICE
  (do-game
    (new-game (default-minion [(qty "Curtain Wall" 1) (qty "Paper Wall" 1)])
              (default-hero))
    (core/gain state :minion :credit 10)
    (play-from-hand state :minion "Curtain Wall" "HQ")
    (let [curt (get-ice state :hq 0)]
      (core/rez state :minion curt)
      (is (= 10 (:current-strength (refresh curt)))
          "Curtain Wall has +4 strength as outermost ICE")
      (play-from-hand state :minion "Paper Wall" "HQ")
      (let [paper (get-ice state :hq 1)]
        (core/rez state :minion paper)
        (is (= 6 (:current-strength (refresh curt))) "Curtain Wall back to default 6 strength")))))

(deftest data-hound
  ;; Data Hound - Full test
  (do-game
    (new-game (default-minion [(qty "Data Hound" 1)])
              (default-hero [(qty "Sure Gamble" 2) (qty "Desperado" 1)
                               (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :hero ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :minion "Data Hound" "HQ")
    (take-credits state :minion)
    (let [dh (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :minion dh)
      (card-subroutine state :minion dh 0)
      (prompt-choice :minion 2)
      (prompt-choice :hero 0)
      ;; trash 1 card and rearrange the other 3
      (prompt-choice :minion (find-card "Desperado" (:deck (get-hero))))
      (is (= 1 (count (:discard (get-hero)))))
      (prompt-choice :minion (find-card "Sure Gamble" (:deck (get-hero))))
      (prompt-choice :minion (find-card "Corroder" (:deck (get-hero))))
      (prompt-choice :minion (find-card "Patron" (:deck (get-hero))))
      ;; try starting over
      (prompt-choice :minion "Start over")
      (prompt-choice :minion (find-card "Patron" (:deck (get-hero))))
      (prompt-choice :minion (find-card "Corroder" (:deck (get-hero))))
      (prompt-choice :minion (find-card "Sure Gamble" (:deck (get-hero)))) ;this is the top card on stack
      (prompt-choice :minion "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-hero))))))
      (is (= "Corroder" (:title (second (:deck (get-hero))))))
      (is (= "Patron" (:title (second (rest (:deck (get-hero)))))))
      (run-jack-out state)
      (run-on state "HQ")
      (card-subroutine state :minion dh 0)
      (prompt-choice :minion 0)
      (prompt-choice :hero 1)
      ;; trash the only card automatically
      (is (= 2 (count (:discard (get-hero)))))
      (is (= "Corroder" (:title (first (:deck (get-hero)))))))))

(deftest data-mine
  ;; Data Mine - do one net and trash
  (do-game
    (new-game (default-minion [(qty "Data Mine" 1)])
              (default-hero))
    (play-from-hand state :minion "Data Mine" "Server 1")
    (take-credits state :minion)
    (let [dm (get-ice state :remote1 0)]
      (run-on state "Server 1")
      (core/rez state :minion dm)
      (card-subroutine state :minion dm 0)
      (is (= 1 (count (:discard (get-hero)))) "Runner suffered 1 net damage"))))

(deftest draco
  ;; Dracō - Pay credits when rezzed to increase strength; trace to give 1 tag and end the run
  (do-game
    (new-game (default-minion [(qty "Dracō" 1)])
              (default-hero))
    (play-from-hand state :minion "Dracō" "HQ")
    (take-credits state :minion)
    (let [drac (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :minion drac)
      (prompt-choice :minion 4)
      (is (= 4 (get-counters (refresh drac) :power)) "Dracō has 4 power counters")
      (is (= 4 (:current-strength (refresh drac))) "Dracō is 4 strength")
      (card-subroutine state :minion drac 0)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (= 1 (:tag (get-hero))) "Runner took 1 tag")
      (is (nil? (get-in @state [:run])) "Run was ended"))))

(deftest enigma
  ;; Enigma - Force Runner to lose 1 click if able
  (do-game
    (new-game (default-minion [(qty "Enigma" 1)])
              (default-hero))
    (play-from-hand state :minion "Enigma" "HQ")
    (take-credits state :minion)
    (let [enig (get-ice state :hq 0)]
      (run-on state "HQ")
      (is (= 3 (:click (get-hero))))
      (core/rez state :minion enig)
      (card-subroutine state :minion enig 0)
      (is (= 2 (:click (get-hero))) "Runner lost 1 click"))))

(deftest excalibur
  ;; Excalibur - Prevent Runner from making another run this turn
  (do-game
    (new-game (default-minion [(qty "Excalibur" 1)])
              (default-hero [(qty "Stimhack" 1)]))
    (play-from-hand state :minion "Excalibur" "HQ")
    (take-credits state :minion)
    (let [excal (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :minion excal)
      (card-subroutine state :minion excal 0)
      (run-jack-out state)
      (run-on state "R&D")
      (is (not (:run @state)) "No run initiated")
      (is (= 3 (:click (get-hero))))
      (play-from-hand state :hero "Stimhack")
      (is (not (:run @state)) "No run initiated")
      (is (= 3 (:click (get-hero))))
      (is (empty? (:discard (get-hero))) "Card not played from Grip")
      ; Check cannot run flag is cleared on next turn #2474
      (take-credits state :hero)
      (is (= :minion (:active-player @state)) "Corp turn")
      (core/gain state :hero :click 1)
      (run-on state "HQ")
      (is (:run @state) "Run initiated ok"))))

(deftest fenris
  ;; Fenris - Illicit ICE give Corp 1 bad publicity when rezzed
  (do-game
    (new-game (default-minion [(qty "Fenris" 1)])
              (default-hero))
    (play-from-hand state :minion "Fenris" "HQ")
    (take-credits state :minion)
    (let [fen (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :minion fen)
      (is (= 1 (:bad-publicity (get-minion))) "Gained 1 bad pub")
      (card-subroutine state :minion fen 0)
      (is (= 1 (:brain-damage (get-hero))) "Runner took 1 brain damage")
      (is (= 1 (count (:discard (get-hero)))))
      (is (= 4 (core/hand-size state :hero))))))

(deftest flare
  ;; Flare - Trash 1 program, do 2 unpreventable meat damage, and end the run
  (do-game
    (new-game (default-minion [(qty "Flare" 1)])
              (default-hero [(qty "Plascrete Carapace" 1) (qty "Clone Chip" 1) (qty "Cache" 3)]))
    (play-from-hand state :minion "Flare" "HQ")
    (core/gain state :minion :credit 2)
    (take-credits state :minion)
    (play-from-hand state :hero "Plascrete Carapace")
    (play-from-hand state :hero "Clone Chip")
    (let [flare (get-ice state :hq 0)
          cc (get-hardware state 1)]
      (run-on state :hq)
      (core/rez state :minion flare)
      (card-subroutine state :minion flare 0)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (prompt-select :minion cc)
      (is (= 1 (count (get-in @state [:hero :rig :hardware]))) "Clone Chip trashed")
      (is (empty? (:prompt (get-hero))) "Plascrete didn't try preventing meat damage")
      (is (= 1 (count (:hand (get-hero)))))
      (is (= 3 (count (:discard (get-hero)))) "Clone Chip plus 2 cards lost from damage in discard")
      (is (not (:run @state)) "Run ended"))))

(deftest gemini-kicker
  ;; Gemini - Successfully trace to do 1 net damage; do 1 net damage if trace strength is 5 or more regardless of success
  (do-game
    (new-game (default-minion [(qty "Gemini" 1) (qty "Hedge Fund" 2)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Dirty Laundry" 2)]))
    (play-from-hand state :minion "Gemini" "HQ")
    (play-from-hand state :minion "Hedge Fund")
    (play-from-hand state :minion "Hedge Fund")
    (take-credits state :minion)
    (let [gem (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :minion gem)
      (card-subroutine state :minion gem 0)
      (prompt-choice :minion 3) ; boost to trace strength 5
      (prompt-choice :hero 0)
      (is (= 2 (count (:discard (get-hero)))) "Did 2 net damage")
      (card-subroutine state :minion gem 0)
      (prompt-choice :minion 3) ; boost to trace strength 5
      (prompt-choice :hero 5) ; match trace
      (is (= 3 (count (:discard (get-hero)))) "Did only 1 net damage for having trace strength 5 or more"))))

(deftest gemini-chronos-protocol
  ;; Gemini - Interaction with Chronos Protocol and kicker
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Gemini" 1) (qty "Hedge Fund" 2)])
              (default-hero [(qty "Sure Gamble" 1) (qty "Dirty Laundry" 2)]))
    (play-from-hand state :minion "Gemini" "HQ")
    (play-from-hand state :minion "Hedge Fund")
    (play-from-hand state :minion "Hedge Fund")
    (take-credits state :minion)
    (let [gem (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :minion gem)
      (card-subroutine state :minion gem 0)
      (prompt-choice :minion 3) ; boost to trace strength 5
      (prompt-choice :hero 0)
      (prompt-choice :minion "Yes")
      (prompt-choice :minion (find-card "Sure Gamble" (:hand (get-hero))))
      (is (= 2 (count (:discard (get-hero)))) "Did 2 net damage"))))

(deftest iq
  ;; IQ - Rez cost and strength equal to cards in HQ
  (do-game
    (new-game (default-minion [(qty "IQ" 3) (qty "Hedge Fund" 3)])
              (default-hero))
    (play-from-hand state :minion "Hedge Fund")
    (play-from-hand state :minion "IQ" "R&D")
    (let [iq1 (get-ice state :rd 0)]
      (core/rez state :minion iq1)
      (is (and (= 4 (count (:hand (get-minion))))
               (= 4 (:current-strength (refresh iq1)))
               (= 5 (:credit (get-minion)))) "4 cards in HQ: paid 4 to rez, has 4 strength")
      (play-from-hand state :minion "IQ" "HQ")
      (let [iq2 (get-ice state :hq 0)]
        (core/rez state :minion iq2)
        (is (and (= 3 (count (:hand (get-minion))))
                 (= 3 (:current-strength (refresh iq1)))
                 (= 3 (:current-strength (refresh iq2)))
                 (= 2 (:credit (get-minion)))) "3 cards in HQ: paid 3 to rez, both have 3 strength")))))

(deftest jua-encounter
  ;; Jua (encounter effect) - Prevent Runner from installing cards for the rest of the turn
  (do-game
    (new-game (default-minion [(qty "Jua" 1)])
              (default-hero [(qty "Desperado" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :minion "Jua" "HQ")
    (take-credits state :minion)
    (let [jua (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :minion jua)
      (card-ability state :minion (refresh jua) 0)
      (run-successful state)
      (is (= 2 (count (:hand (get-hero)))) "Runner starts with 2 cards in hand")
      (play-from-hand state :hero "Desperado")
      (is (= 2 (count (:hand (get-hero)))) "No cards installed")
      (play-from-hand state :hero "Sure Gamble")
      (is (= 1 (count (:hand (get-hero)))) "Can play events")
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 1 (count (:hand (get-hero)))) "Runner starts with 1 cards in hand")
      (play-from-hand state :hero "Desperado")
      (is (= 0 (count (:hand (get-hero)))) "Card installed"))))

(deftest jua-sub
  ;; Jua (subroutine effect) - Select 2 hero cards, hero moves one to the stack
  (do-game
    (new-game (default-minion [(qty "Jua" 1)])
              (default-hero [(qty "Desperado" 1) (qty "Gordian Blade" 1)]))
    (play-from-hand state :minion "Jua" "HQ")
    (take-credits state :minion)
    (let [jua (get-ice state :hq 0)]
      (core/gain state :hero :credit 10)
      (play-from-hand state :hero "Desperado")
      (run-on state "HQ")
      (core/rez state :minion jua)
      (card-subroutine state :minion (refresh jua) 0)
      (is (empty? (:prompt (get-minion))) "Can't fire for 1 installed card")
      (run-successful state)

      (play-from-hand state :hero "Gordian Blade")
      (run-on state "HQ")
      (card-subroutine state :minion (refresh jua) 0)
      (prompt-select :minion (get-program state 0))
      (prompt-select :minion (get-hardware state 0))
      (prompt-choice :hero "Gordian Blade")
      (is (nil? (get-program state 0)) "Card is uninstalled")
      (is (= 1 (count (:deck (get-hero)))) "Runner puts card in deck"))))

(deftest lockdown
  ;; Lockdown - Prevent Runner from drawing cards for the rest of the turn
  (do-game
    (new-game (default-minion [(qty "Lockdown" 1)])
              (default-hero [(qty "Diesel" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Lockdown" "R&D")
    (take-credits state :minion)
    (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
    (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
    (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
    (let [lock (get-ice state :rd 0)]
      (run-on state "R&D")
      (core/rez state :minion lock)
      (card-subroutine state :minion lock 0)
      (run-successful state)
      (play-from-hand state :hero "Diesel")
      (is (= 1 (count (:hand (get-hero)))) "No cards drawn")
      (take-credits state :hero)
      (take-credits state :minion)
      (play-from-hand state :hero "Diesel")
      (is (= 3 (count (:hand (get-hero))))
          "New turn ends prevention; remaining 3 cards drawn from Stack"))))

(deftest lotus-field-unlowerable
  ;; Lotus Field strength cannot be lowered
  (do-game
    (new-game (default-minion [(qty "Lotus Field" 1) (qty "Lag Time" 1)])
              (default-hero [(qty "Ice Carver" 1) (qty "Parasite" 1)]))
    (play-from-hand state :minion "Lotus Field" "Archives")
    (take-credits state :minion 2)
    (let [lotus (get-ice state :archives 0)]
      (core/rez state :minion lotus)
      (play-from-hand state :hero "Ice Carver")
      (run-on state "Archives")
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (run-jack-out state)
      (play-from-hand state :hero "Parasite")
      (prompt-select :hero lotus)
      (is (= 1 (count (:hosted (refresh lotus)))) "Parasite hosted on Lotus Field")
      (take-credits state :hero 1)
      (take-credits state :minion)
      (is (= 1 (core/get-virus-counters state :hero (first (:hosted (refresh lotus)))))
          "Parasite has 1 virus counter")
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (take-credits state :hero)
      (play-from-hand state :minion "Lag Time")
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased")
      (take-credits state :minion 2)
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased"))))

(deftest mausolus
  ;; Mausolus - 3 adv tokens change the subroutines
  (do-game
    (new-game (default-minion [(qty "Mausolus" 1)])
              (default-hero [(qty "NetChip" 5)]))
    (play-from-hand state :minion "Mausolus" "HQ")
    (let [mau (get-ice state :hq 0)]
      (core/rez state :minion mau)
      (take-credits state :minion)
      (run-on state :hq)
      (is (= 3 (:credit (get-minion))) "minion starts encounter with 3 crs")
      (is (= 0 (count (:discard (get-hero)))) "hero starts encounter with no cards in heap")
      (is (= 0 (:tag (get-hero))) "hero starts encounter with 0 tags")
      (card-subroutine state :minion mau 0)
      (card-subroutine state :minion mau 1)
      (card-subroutine state :minion mau 2)
      (is (= 4 (:credit (get-minion))) "minion gains 1 cr from mausolus")
      (is (= 1 (count (:discard (get-hero)))) "minion does 1 net damage")
      (is (= 1 (:tag (get-hero))) "minion gives 1 tag")
      (run-jack-out state)
      (take-credits state :hero)
      (core/advance state :minion {:card (refresh mau)})
      (core/advance state :minion {:card (refresh mau)})
      (core/advance state :minion {:card (refresh mau)})
      (run-on state :hq)
      (is (= 1 (:credit (get-minion))) "minion starts encounter with 1 crs")
      (is (= 1 (count (:discard (get-hero)))) "hero starts encounter with 1 card in heap")
      (is (= 1 (:tag (get-hero))) "hero starts encounter with 1 tags")
      (card-subroutine state :minion mau 0)
      (card-subroutine state :minion mau 1)
      (card-subroutine state :minion mau 2)
      (is (= 4 (:credit (get-minion))) "minion gains 3 cr")
      (is (= 4 (count (:discard (get-hero)))) "minion does 3 net damage")
      (is (= 2 (:tag (get-hero))) "minion gives 1 tag")
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:hero :register :unsuccessful-run]) "Run was unsuccessful"))))

(deftest meru-mati
  (do-game
    (new-game (default-minion [(qty "Meru Mati" 2)])
              (default-hero))
    (play-from-hand state :minion "Meru Mati" "HQ")
    (play-from-hand state :minion "Meru Mati" "R&D")
    (core/rez state :minion (get-ice state :hq 0))
    (core/rez state :minion (get-ice state :rd 0))
    (is (= 4 (:current-strength (get-ice state :hq 0))) "HQ Meru Mati at 4 strength")
	(is (= 1 (:current-strength (get-ice state :rd 0))) "R&D at 0 strength")))

(deftest mind-game
  ;; Mind game - PSI redirect to different server
  (do-game
    (new-game (default-minion [(qty "Mind Game" 1)])
              (default-hero))
    (play-from-hand state :minion "Mind Game" "HQ")
    (take-credits state :minion)
    (run-on state :hq)
    (let [mindgame (get-ice state :hq 0)]
      (core/rez state :minion mindgame)
      (card-subroutine state :minion mindgame 0))
    (prompt-choice :minion "1 [Credits]")
    (prompt-choice :hero "0 [Credits]")
    (is (= (set ["R&D" "Archives"]) (set (:choices (prompt-map :minion)))) "Corp cannot choose server Runner is on")
    (prompt-choice :minion "Archives")
    (is (= [:archives] (get-in @state [:run :server])) "Runner now running on Archives")))

(deftest minelayer
  ;; Minelayer - Install a piece of ICE in outermost position of Minelayer's server at no cost
  (do-game
    (new-game (default-minion [(qty "Minelayer" 1) (qty "Fire Wall" 1)])
              (default-hero))
    (play-from-hand state :minion "Minelayer" "HQ")
    (take-credits state :minion)
    (run-on state :hq)
    (core/rez state :minion (get-ice state :hq 0))
    (is (= 6 (:credit (get-minion))))
    (card-subroutine state :minion (get-ice state :hq 0) 0)
    (prompt-select :minion (find-card "Fire Wall" (:hand (get-minion))))
    (is (= 2 (count (get-in @state [:minion :servers :hq :ices]))) "2 ICE protecting HQ")
    (is (= 6 (:credit (get-minion))) "Didn't pay 1 credit to install as second ICE")))

(deftest morph-ice-subtype-changing
  ;; Morph ice gain and lose subtypes from normal advancements and placed advancements
  (do-game
    (new-game (default-minion [(qty "Wendigo" 1)
                             (qty "Shipment from SanSan" 1)
                             (qty "Superior Cyberwalls" 1)])
              (default-hero))
    (core/gain state :minion :click 2)
    (play-from-hand state :minion "Superior Cyberwalls" "New remote")
    (let [sc (get-content state :remote1 0)]
      (score-agenda state :minion sc)
      (play-from-hand state :minion "Wendigo" "HQ")
      (let [wend (get-ice state :hq 0)]
        (core/rez state :minion wend)
        (is (= 4 (:current-strength (refresh wend))) "Wendigo at normal 4 strength")
        (core/advance state :minion {:card (refresh wend)})
        (is (= true (has? (refresh wend) :subtype "Barrier")) "Wendigo gained Barrier")
        (is (= false (has? (refresh wend) :subtype "Code Gate")) "Wendigo lost Code Gate")
        (is (= 5 (:current-strength (refresh wend))) "Wendigo boosted to 5 strength by scored Superior Cyberwalls")
        (play-from-hand state :minion "Shipment from SanSan")
        (prompt-choice :minion "1")
        (prompt-select :minion wend)
        (is (= false (has? (refresh wend) :subtype "Barrier")) "Wendigo lost Barrier")
        (is (= true (has? (refresh wend) :subtype "Code Gate")) "Wendigo gained Code Gate")
        (is (= 4 (:current-strength (refresh wend))) "Wendigo returned to normal 4 strength")))))

(deftest mother-goddess
  ;; Mother Goddess - Gains other ice subtypes
  (do-game
    (new-game (default-minion [(qty "Mother Goddess" 1) (qty "NEXT Bronze" 1)])
              (default-hero))
    (core/gain state :minion :credit 1)
    (play-from-hand state :minion "Mother Goddess" "HQ")
    (play-from-hand state :minion "NEXT Bronze" "R&D")
    (let [mg (get-ice state :hq 0)
          nb (get-ice state :rd 0)]
      (core/rez state :minion mg)
      (is (core/has-subtype? (refresh mg) "Mythic") "Mother Goddess has Mythic")
      (is (not (core/has-subtype? (refresh mg) "Code Gate")) "Mother Goddess does not have Code Gate")
      (is (not (core/has-subtype? (refresh mg) "NEXT")) "Mother Goddess does not have NEXT")
      (core/rez state :minion nb)
      (is (core/has-subtype? (refresh mg) "Mythic") "Mother Goddess has Mythic")
      (is (core/has-subtype? (refresh mg) "Code Gate") "Mother Goddess has Code Gate")
      (is (core/has-subtype? (refresh mg) "NEXT") "Mother Goddess has NEXT"))))

(deftest next-bronze
  ;; NEXT Bronze - Add 1 strength for every rezzed NEXT ice
  (do-game
    (new-game (default-minion [(qty "NEXT Bronze" 2) (qty "NEXT Silver" 1)])
              (default-hero))
    (core/gain state :minion :credit 2)
    (play-from-hand state :minion "NEXT Bronze" "HQ")
    (play-from-hand state :minion "NEXT Bronze" "R&D")
    (play-from-hand state :minion "NEXT Silver" "Archives")
    (let [nb1 (get-ice state :hq 0)
          nb2 (get-ice state :rd 0)
          ns1 (get-ice state :archives 0)]
      (core/rez state :minion nb1)
      (is (= 1 (:current-strength (refresh nb1)))
          "NEXT Bronze at 1 strength: 1 rezzed NEXT ice")
      (core/rez state :minion nb2)
      (is (= 2 (:current-strength (refresh nb1)))
          "NEXT Bronze at 2 strength: 2 rezzed NEXT ice")
      (is (= 2 (:current-strength (refresh nb2)))
          "NEXT Bronze at 2 strength: 2 rezzed NEXT ice")
      (core/rez state :minion ns1)
      (is (= 3 (:current-strength (refresh nb1)))
          "NEXT Bronze at 3 strength: 3 rezzed NEXT ice")
      (is (= 3 (:current-strength (refresh nb2)))
          "NEXT Bronze at 3 strength: 3 rezzed NEXT ice"))))

(deftest nightdancer
  ;; Nightdancer - Runner loses a click if able, minion gains a click on next turn
  (do-game
    (new-game (default-minion [(qty "Nightdancer" 1)])
              (default-hero))
    (play-from-hand state :minion "Nightdancer" "HQ")
    (take-credits state :minion)
    (let [nd (get-ice state :hq 0)]
      (core/rez state :minion nd)
      (run-on state "HQ")
      (is (= 3 (:click (get-hero))) "Runner starts with 3 clicks")
      (card-subroutine state :minion nd 0)
      (is (= 2 (:click (get-hero))) "Runner lost 1 click")
      (card-subroutine state :minion nd 0)
      (is (= 1 (:click (get-hero))) "Runner lost 1 click")
      (run-jack-out state)
      (take-credits state :hero)
      (is (= 5 (:click (get-minion))) "Corp has 5 clicks"))))

(deftest resistor
  ;; Resistor - Strength equal to Runner tags, lose strength when Runner removes a tag
  (do-game
    (new-game (default-minion [(qty "Resistor" 1)])
              (default-hero))
    (play-from-hand state :minion "Resistor" "HQ")
    (let [resistor (get-ice state :hq 0)]
      (core/rez state :minion resistor)
      (is (= 0 (:current-strength (refresh resistor))) "No Runner tags; 0 strength")
      (core/tag-hero state :hero 2)
      (is (= 2 (:tag (get-hero))))
      (is (= 2 (:current-strength (refresh resistor))) "2 Runner tags; 2 strength")
      (take-credits state :minion)
      (core/remove-tag state :hero 1)
      (is (= 1 (:current-strength (refresh resistor))) "Runner removed 1 tag; down to 1 strength"))))

(deftest self-adapting-code-wall-unlowerable
  ;; self-adapting code wall strength cannot be lowered
  (do-game
    (new-game (default-minion [(qty "Self-Adapting Code Wall" 1) (qty "Lag Time" 1)])
              (default-hero [(qty "Ice Carver" 1) (qty "Parasite" 1)]))
    (play-from-hand state :minion "Self-Adapting Code Wall" "Archives")
    (take-credits state :minion 2)
    (let [sacw (get-ice state :archives 0)]
      (core/rez state :minion sacw)
      (play-from-hand state :hero "Ice Carver")
      (run-on state "Archives")
      (is (= 1 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength unchanged")
      (run-jack-out state)
      (play-from-hand state :hero "Parasite")
      (prompt-select :hero sacw)
      (is (= 1 (count (:hosted (refresh sacw)))) "Parasite hosted on Self-Adapting Code Wall")
      (take-credits state :hero 1)
      (take-credits state :minion)
      (is (= 1 (core/get-virus-counters state :hero (first (:hosted (refresh sacw)))))
          "Parasite has 1 virus counter")
      (is (= 1 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength unchanged")
      (take-credits state :hero)
      (play-from-hand state :minion "Lag Time")
      (is (= 2 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength increased")
      (take-credits state :minion 2)
      (is (= 2 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength increased"))))

(deftest searchlight
  ;; Searchlight - Trace bace equal to advancement counters
  (do-game
    (new-game (default-minion [(qty "Searchlight" 1)])
              (default-hero))
    (play-from-hand state :minion "Searchlight" "HQ")
    (let [searchlight (get-ice state :hq 0)]
      (core/rez state :minion searchlight)
      (card-subroutine state :minion (refresh searchlight) 0)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (= 0 (:tag (get-hero))) "Trace failed with 0 advancements")
      (core/advance state :minion {:card (refresh searchlight)})
      (card-subroutine state :minion (refresh searchlight) 0)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (= 1 (:tag (get-hero))) "Trace succeeds with 0 advancements"))))

(deftest sherlock
  ;; Sherlock 1.0 - Trace to add an installed program to the top of Runner's Stack
  (do-game
    (new-game (default-minion [(qty "Sherlock 1.0" 1)])
              (default-hero [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Sherlock 1.0" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Gordian Blade")
    (run-on state :hq)
    (core/rez state :minion (get-ice state :hq 0))
    (card-subroutine state :minion (get-ice state :hq 0) 0)
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (prompt-select :minion (get-in @state [:hero :rig :program 0]))
    (is (empty? (get-in @state [:hero :rig :program])) "Gordian uninstalled")
    (is (= "Gordian Blade" (:title (first (:deck (get-hero))))) "Gordian on top of Stack")))

(deftest shiro
  ;; Shiro - Full test
  (do-game
    (new-game (default-minion [(qty "Shiro" 1) (qty "Caprice Nisei" 1)
                             (qty "Quandary" 1) (qty "Jackson Howard" 1)])
              (default-hero [(qty "R&D Interface" 1)]))
    (starting-hand state :minion ["Shiro"])
    (play-from-hand state :minion "Shiro" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "R&D Interface")
    (let [shiro (get-ice state :hq 0)]
      (run-on state :hq)
      (core/rez state :minion shiro)
      (card-subroutine state :minion shiro 0)
      (prompt-choice :minion (find-card "Caprice Nisei" (:deck (get-minion))))
      (prompt-choice :minion (find-card "Quandary" (:deck (get-minion))))
      (prompt-choice :minion (find-card "Jackson Howard" (:deck (get-minion))))
      ;; try starting over
      (prompt-choice :minion "Start over")
      (prompt-choice :minion (find-card "Jackson Howard" (:deck (get-minion))))
      (prompt-choice :minion (find-card "Quandary" (:deck (get-minion))))
      (prompt-choice :minion (find-card "Caprice Nisei" (:deck (get-minion)))) ;this is the top card of R&D
      (prompt-choice :minion "Done")
      (is (= "Caprice Nisei" (:title (first (:deck (get-minion))))))
      (is (= "Quandary" (:title (second (:deck (get-minion))))))
      (is (= "Jackson Howard" (:title (second (rest (:deck (get-minion)))))))
      (card-subroutine state :minion shiro 1)
      (is (= (:cid (first (:deck (get-minion))))
             (:cid (:card (first (:prompt (get-hero)))))) "Access the top card of R&D")
      (prompt-choice :hero "No")
      (is (= (:cid (second (:deck (get-minion))))
             (:cid (:card (first (:prompt (get-hero)))))) "Access another card due to R&D Interface"))))

(deftest snowflake
  ;; Snowflake - Win a psi game to end the run
  (do-game
    (new-game (default-minion [(qty "Snowflake" 1)])
              (default-hero))
    (play-from-hand state :minion "Snowflake" "HQ")
    (take-credits state :minion)
    (run-on state :hq)
    (let [sf (get-ice state :hq 0)]
      (core/rez state :minion sf)
      (card-subroutine state :minion sf 0)
      (prompt-choice :minion "0 [Credits]")
      (prompt-choice :hero "0 [Credits]")
      (is (:run @state) "Runner won psi, run continues")
      (card-subroutine state :minion sf 0)
      (prompt-choice :minion "0 [Credits]")
      (prompt-choice :hero "1 [Credits]")
      (is (not (:run @state)) "Run ended"))))

(deftest special-offer-trash-ice-during-run
  ;; Special Offer trashes itself and updates the run position
  (do-game
    (new-game (default-minion [(qty "Ice Wall" 1) (qty "Special Offer" 1)])
              (default-hero))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (play-from-hand state :minion "Special Offer" "HQ")
    (take-credits state :minion 1)
    (run-on state "HQ")
    (is (= 2 (:position (get-in @state [:run]))) "Initial position approaching Special Offer")
    (let [special (get-ice state :hq 1)]
      (core/rez state :minion special)
      (is (= 4 (:credit (get-minion))))
      (card-subroutine state :minion special 0)
      (is (= 9 (:credit (get-minion))) "Special Offer paid 5 credits")
      (is (= 1 (:position (get-in @state [:run])))
          "Run position updated; now approaching Ice Wall"))))

(deftest tithonium
  ;; Forfeit option as rez cost, can have hosted condition counters
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 1) (qty "Tithonium" 1) (qty "Patch" 1)])
              (default-hero [(qty "Pawn" 1) (qty "Wasteland" 1)]))
    (core/gain state :minion :click 10)
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (play-from-hand state :minion "Tithonium" "HQ")
    (let [ht (get-content state :remote1 0)
          ti (get-ice state :hq 0)]
      (score-agenda state :minion ht)
      (is (= 1 (count (:scored (get-minion)))) "Agenda scored")
      (is (= 12 (:credit (get-minion))) "Gained 7 credits")
      (core/rez state :minion ti)
      (prompt-choice :minion "No") ; don't use alternative cost
      (is (= 3 (:credit (get-minion))) "Spent 9 to Rez")
      (core/derez state :minion (refresh ti))
      (core/rez state :minion ti)
      (prompt-choice :minion "Yes") ; use alternative cost
      (prompt-select :minion (get-in (get-minion) [:scored 0]))
      (is (= 3 (:credit (get-minion))) "Still on 3c")
      (is (= 0 (count (:scored (get-minion)))) "Agenda forfeited")
      ;; Can Host Conditions Counters
      (play-from-hand state :minion "Patch")
      (prompt-select :minion (refresh ti))
      (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
      (take-credits state :minion)
      (core/derez state :minion (refresh ti))
      (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
      (play-from-hand state :hero "Pawn")
      (play-from-hand state :hero "Wasteland")
      (let [pawn (get-program state 0)
            wast (get-resource state 0)]
        (card-ability state :hero (refresh pawn) 0)
        (prompt-select :hero (refresh ti))
        (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
        (core/derez state :minion (refresh ti))
        (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
        (run-on state "HQ")
        (card-subroutine state :minion ti 2)
        (prompt-select :minion (refresh wast))
        (is (= 1 (count (:discard (get-hero)))) "1 card trashed")
        (card-subroutine state :minion ti 1)
        (is (not (:run @state)) "Run ended")))))

(deftest tithonium-oversight-ai
  ;; Do not prompt for alt cost #2734
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 1) (qty "Oversight AI" 1) (qty "Tithonium" 1)])
              (default-hero))
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (play-from-hand state :minion "Tithonium" "R&D")
    (let [ht (get-content state :remote1 0)
          ti (get-ice state :rd 0)]
      (score-agenda state :minion ht)
      (play-from-hand state :minion "Oversight AI")
      (prompt-select :minion ti)
      (is (get-in (refresh ti) [:rezzed]))
      (is (= "Oversight AI" (:title (first (:hosted (refresh ti)))))
          "Tithonium hosting OAI as a condition"))))

(deftest tmi
  ;; TMI ICE test
  (do-game
    (new-game (default-minion [(qty "TMI" 3)])
              (default-hero))
    (play-from-hand state :minion "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
      (core/rez state :minion tmi)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (get-in (refresh tmi) [:rezzed])))))

(deftest tmi-derez
  ;; TMI ICE trace derez
  (do-game
    (new-game (default-minion [(qty "TMI" 3)])
              (make-deck "Sunny Lebeau: Security Specialist" [(qty "Blackmail" 3)]))
    (play-from-hand state :minion "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
      (core/rez state :minion tmi)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (not (get-in (refresh tmi) [:rezzed]))))))

(deftest turing-positional-strength
  ;; Turing - Strength boosted when protecting a remote server
  (do-game
    (new-game (default-minion [(qty "Turing" 2) (qty "Hedge Fund" 1)])
              (default-hero))
    (play-from-hand state :minion "Hedge Fund")
    (play-from-hand state :minion "Turing" "HQ")
    (play-from-hand state :minion "Turing" "New remote")
    (let [t1 (get-ice state :hq 0)
          t2 (get-ice state :remote1 0)]
      (core/rez state :minion t1)
      (is (= 2 (:current-strength (refresh t1)))
          "Turing default 2 strength over a central server")
      (core/rez state :minion t2)
      (is (= 5 (:current-strength (refresh t2)))
          "Turing increased to 5 strength over a remote server"))))

(deftest wraparound
  ;; Wraparound - Strength boosted when no fracter is installed
  (do-game
    (new-game (default-minion [(qty "Wraparound" 1)])
              (default-hero [(qty "Corroder" 1)]))
    (play-from-hand state :minion "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (core/rez state :minion wrap)
      (is (= 7 (:current-strength (refresh wrap)))
          "Wraparound +7 strength with no fracter in play")
      (take-credits state :minion)
      (play-from-hand state :hero "Corroder")
      (is (= 0 (:current-strength (refresh wrap)))
          "Wraparound 0 strength after Corroder installed"))))
