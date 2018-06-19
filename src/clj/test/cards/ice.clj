(ns test.cards.character
  (:require [game.core :as core]
            [game.utils :refer :all]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest end-the-run
  ;; Since all ETR character share a common ability, we only need one test
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3) (qty "Hedge Fund" 3) (qty "Restructure" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant 2)
    (run-on state "HQ")
    (is (= [:hq] (get-in @state [:run :server])))
    (let [iwall (get-character state :hq 0)]
      (core/reveal state :contestant iwall)
      (card-subroutine state :contestant iwall 0)
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:challenger :register :unsuccessful-run]) "Run was unsuccessful"))))

(deftest aimor
  ;; Aimor - trash the top 3 cards of the stack, trash Aimor
  (do-game
    (new-game (default-contestant [(qty "Aimor" 1)])
              (default-challenger [(qty "Sure Gamble" 2) (qty "Desperado" 1)
                               (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :challenger ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :contestant "Aimor" "HQ")
    (is (= 1 (count (get-in @state [:contestant :servers :hq :characters]))) "Aimor installed")
    (take-credits state :contestant)
    (let [aim (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant aim)
      (card-subroutine state :contestant aim 0)
      (is (= 3 (count (:discard (get-challenger)))) "Challenger trashed 3 cards")
      (is (= 1 (count (:deck (get-challenger)))) "Challenger has 1 card in deck"))
    (is (= 0 (count (get-in @state [:contestant :servers :hq :characters]))) "Aimor trashed")))

(deftest archangel
  ;; Archangel - accessing from R&D does not cause run to hang.
  (do-game
    (new-game (default-contestant [(qty "Archangel" 1) (qty "Hedge Fund" 1)])
              (default-challenger [(qty "Bank Job" 1)]))
    (starting-hand state :contestant ["Hedge Fund"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Bank Job")
    (run-empty-server state :rd)
    (prompt-choice :contestant "Yes")
    (prompt-choice :challenger "Yes")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (prompt-select :contestant (get-muthereff state 0))
    (prompt-choice :challenger "OK")
    (is (not (:run @state)) "Run ended")))

(deftest architect-untrashable
  ;; Architect is untrashable while installed and revealed, but trashable if hidden or from HQ
  (do-game
    (new-game (default-contestant [(qty "Architect" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Architect" "HQ")
    (let [architect (get-character state :hq 0)]
      (core/reveal state :contestant architect)
      (core/trash state :contestant (refresh architect))
      (is (not= nil (get-character state :hq 0)) "Architect was trashed, but should be untrashable")
      (core/hide state :contestant (refresh architect))
      (core/trash state :contestant (refresh architect))
      (is (= nil (get-character state :hq 0)) "Architect was not trashed, but should be trashable")
      (core/trash state :contestant (get-in @state [:contestant :hand 0]))
      (is (= (get-in @state [:contestant :discard 0 :title]) "Architect"))
      (is (= (get-in @state [:contestant :discard 1 :title]) "Architect")))))

(deftest asteroid-belt
  ;; Asteroid Belt - Space Character reveal cost reduced by 3 credits per advancement
  (do-game
    (new-game (default-contestant [(qty "Asteroid Belt" 1)])
              (default-challenger))
    (core/gain state :contestant :credit 5)
    (play-from-hand state :contestant "Asteroid Belt" "HQ")
    (let [ab (get-character state :hq 0)]
      (core/advance state :contestant {:card (refresh ab)})
      (core/advance state :contestant {:card (refresh ab)})
      (is (= 8 (:credit (get-contestant))))
      (is (= 2 (:advance-counter (refresh ab))))
      (core/reveal state :contestant (refresh ab))
      (is (= 5 (:credit (get-contestant))) "Paid 3 credits to reveal; 2 advancments on Asteroid Belt"))))

(deftest bandwidth
  ;; Bandwidth - Give the Challenger 1 tag; remove 1 tag if the run is successful
  (do-game
    (new-game (default-contestant [(qty "Bandwidth" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Bandwidth" "Archives")
    (let [bw (get-character state :archives 0)]
      (take-credits state :contestant)
      (run-on state "Archives")
      (core/reveal state :contestant bw)
      (card-subroutine state :contestant bw 0)
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag")
      (run-successful state)
      (is (= 0 (:tag (get-challenger))) "Run successful; Challenger lost 1 tag")
      (run-on state "Archives")
      (card-subroutine state :contestant bw 0)
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag")
      (run-jack-out state)
      (is (= 1 (:tag (get-challenger))) "Run unsuccessful; Challenger kept 1 tag"))))

(deftest bullfrog
  ;; Bullfrog - Win psi to move to outermost position of another server and continue run there
  (do-game
    (new-game (default-contestant [(qty "Bullfrog" 1) (qty "Pup" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Bullfrog" "HQ")
    (play-from-hand state :contestant "Pup" "R&D")
    (play-from-hand state :contestant "Pup" "R&D")
    (take-credits state :contestant)
    (run-on state :hq)
    (let [frog (get-character state :hq 0)]
      (core/reveal state :contestant frog)
      (is (= :hq (first (get-in @state [:run :server]))))
      (card-subroutine state :contestant frog 0)
      (prompt-choice :contestant "0 [Credits]")
      (prompt-choice :challenger "1 [Credits]")
      (prompt-choice :contestant "R&D")
      (is (= :rd (first (get-in @state [:run :server]))) "Run redirected to R&D")
      (is (= 2 (get-in @state [:run :position])) "Passed Bullfrog")
      (is (= "Bullfrog" (:title (get-character state :rd 2))) "Bullfrog at outermost position of R&D"))))

(deftest cell-portal
  ;; Cell Portal - Bounce Challenger to outermost position and hide itself
  (do-game
    (new-game (default-contestant [(qty "Cell Portal" 1) (qty "Paper Wall" 2)])
              (default-challenger))
    (core/gain state :contestant :credit 5)
    (play-from-hand state :contestant "Cell Portal" "HQ")
    (play-from-hand state :contestant "Paper Wall" "HQ")
    (play-from-hand state :contestant "Paper Wall" "HQ")
    (take-credits state :contestant)
    (run-on state :hq)
    (run-continue state)
    (run-continue state)
    (is (= 1 (get-in @state [:run :position])))
    (let [cp (get-character state :hq 0)]
      (core/reveal state :contestant cp)
      (card-subroutine state :contestant cp 0)
      (is (= 3 (get-in @state [:run :position])) "Run back at outermost position")
      (is (not (get-in (refresh cp) [:revealed])) "Cell Portal hidden"))))

(deftest chimera
  ;; Chimera - Gains chosen subtype
  (do-game
    (new-game (default-contestant [(qty "Chimera" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Chimera" "HQ")
    (let [ch (get-character state :hq 0)]
      (core/reveal state :contestant ch)
      (prompt-choice :contestant "Barrier")
      (is (core/has-subtype? (refresh ch) "Barrier") "Chimera has Barrier")
      (take-credits state :contestant)
      (is (not (core/has-subtype? (refresh ch) "Barrier")) "Chimera does not have Barrier"))))

(deftest cortex-lock
  ;; Cortex Lock - Do net damage equal to Challenger's unused memory
  (do-game
    (new-game (default-contestant [(qty "Cortex Lock" 1)])
              (default-challenger [(qty "Corroder" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Cortex Lock" "HQ")
    (take-credits state :contestant)
    (let [cort (get-character state :hq 0)]
      (play-from-hand state :challenger "Corroder")
      (is (= 3 (:memory (get-challenger))))
      (run-on state "HQ")
      (core/reveal state :contestant cort)
      (card-subroutine state :contestant cort 0)
      (is (= 3 (count (:discard (get-challenger)))) "Challenger suffered 3 net damage"))))

(deftest crick
  ;; Crick - Strength boost when protecting Archives; installs a card from Archives
  (do-game
    (new-game (default-contestant [(qty "Crick" 2) (qty "Ice Wall" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Crick" "HQ")
    (play-from-hand state :contestant "Crick" "Archives")
    (core/move state :contestant (find-card "Ice Wall" (:hand (get-contestant))) :discard)
    (take-credits state :contestant)
    (let [cr1 (get-character state :hq 0)
          cr2 (get-character state :archives 0)]
      (core/reveal state :contestant cr1)
      (core/reveal state :contestant cr2)
      (is (= 3 (:current-strength (refresh cr1))) "Normal strength over HQ")
      (is (= 6 (:current-strength (refresh cr2))) "+3 strength over Archives")
      (card-subroutine state :contestant cr2 0)
      (prompt-select :contestant (find-card "Ice Wall" (:discard (get-contestant))))
      (prompt-choice :contestant "HQ")
      (is (= 3 (:credit (get-contestant))) "Paid 1 credit to install as 2nd Character over HQ"))))

(deftest curtain-wall
  ;; Curtain Wall - Strength boost when outermost Character
  (do-game
    (new-game (default-contestant [(qty "Curtain Wall" 1) (qty "Paper Wall" 1)])
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

(deftest data-hound
  ;; Data Hound - Full test
  (do-game
    (new-game (default-contestant [(qty "Data Hound" 1)])
              (default-challenger [(qty "Sure Gamble" 2) (qty "Desperado" 1)
                               (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :challenger ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :contestant "Data Hound" "HQ")
    (take-credits state :contestant)
    (let [dh (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant dh)
      (card-subroutine state :contestant dh 0)
      (prompt-choice :contestant 2)
      (prompt-choice :challenger 0)
      ;; trash 1 card and rearrange the other 3
      (prompt-choice :contestant (find-card "Desperado" (:deck (get-challenger))))
      (is (= 1 (count (:discard (get-challenger)))))
      (prompt-choice :contestant (find-card "Sure Gamble" (:deck (get-challenger))))
      (prompt-choice :contestant (find-card "Corroder" (:deck (get-challenger))))
      (prompt-choice :contestant (find-card "Patron" (:deck (get-challenger))))
      ;; try starting over
      (prompt-choice :contestant "Start over")
      (prompt-choice :contestant (find-card "Patron" (:deck (get-challenger))))
      (prompt-choice :contestant (find-card "Corroder" (:deck (get-challenger))))
      (prompt-choice :contestant (find-card "Sure Gamble" (:deck (get-challenger)))) ;this is the top card on stack
      (prompt-choice :contestant "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-challenger))))))
      (is (= "Corroder" (:title (second (:deck (get-challenger))))))
      (is (= "Patron" (:title (second (rest (:deck (get-challenger)))))))
      (run-jack-out state)
      (run-on state "HQ")
      (card-subroutine state :contestant dh 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 1)
      ;; trash the only card automatically
      (is (= 2 (count (:discard (get-challenger)))))
      (is (= "Corroder" (:title (first (:deck (get-challenger)))))))))

(deftest data-mine
  ;; Data Mine - do one net and trash
  (do-game
    (new-game (default-contestant [(qty "Data Mine" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Data Mine" "Server 1")
    (take-credits state :contestant)
    (let [dm (get-character state :remote1 0)]
      (run-on state "Server 1")
      (core/reveal state :contestant dm)
      (card-subroutine state :contestant dm 0)
      (is (= 1 (count (:discard (get-challenger)))) "Challenger suffered 1 net damage"))))

(deftest draco
  ;; Dracō - Pay credits when revealed to increase strength; trace to give 1 tag and end the run
  (do-game
    (new-game (default-contestant [(qty "Dracō" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Dracō" "HQ")
    (take-credits state :contestant)
    (let [drac (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant drac)
      (prompt-choice :contestant 4)
      (is (= 4 (get-counters (refresh drac) :power)) "Dracō has 4 power counters")
      (is (= 4 (:current-strength (refresh drac))) "Dracō is 4 strength")
      (card-subroutine state :contestant drac 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag")
      (is (nil? (get-in @state [:run])) "Run was ended"))))

(deftest enigma
  ;; Enigma - Force Challenger to lose 1 click if able
  (do-game
    (new-game (default-contestant [(qty "Enigma" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Enigma" "HQ")
    (take-credits state :contestant)
    (let [enig (get-character state :hq 0)]
      (run-on state "HQ")
      (is (= 3 (:click (get-challenger))))
      (core/reveal state :contestant enig)
      (card-subroutine state :contestant enig 0)
      (is (= 2 (:click (get-challenger))) "Challenger lost 1 click"))))

(deftest excalibur
  ;; Excalibur - Prevent Challenger from making another run this turn
  (do-game
    (new-game (default-contestant [(qty "Excalibur" 1)])
              (default-challenger [(qty "Stimhack" 1)]))
    (play-from-hand state :contestant "Excalibur" "HQ")
    (take-credits state :contestant)
    (let [excal (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant excal)
      (card-subroutine state :contestant excal 0)
      (run-jack-out state)
      (run-on state "R&D")
      (is (not (:run @state)) "No run initiated")
      (is (= 3 (:click (get-challenger))))
      (play-from-hand state :challenger "Stimhack")
      (is (not (:run @state)) "No run initiated")
      (is (= 3 (:click (get-challenger))))
      (is (empty? (:discard (get-challenger))) "Card not played from Grip")
      ; Check cannot run flag is cleared on next turn #2474
      (take-credits state :challenger)
      (is (= :contestant (:active-player @state)) "Contestant turn")
      (core/gain state :challenger :click 1)
      (run-on state "HQ")
      (is (:run @state) "Run initiated ok"))))

(deftest fenris
  ;; Fenris - Illicit Character give Contestant 1 bad publicity when revealed
  (do-game
    (new-game (default-contestant [(qty "Fenris" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Fenris" "HQ")
    (take-credits state :contestant)
    (let [fen (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant fen)
      (is (= 1 (:bad-publicity (get-contestant))) "Gained 1 bad pub")
      (card-subroutine state :contestant fen 0)
      (is (= 1 (:brain-damage (get-challenger))) "Challenger took 1 brain damage")
      (is (= 1 (count (:discard (get-challenger)))))
      (is (= 4 (core/hand-size state :challenger))))))

(deftest flare
  ;; Flare - Trash 1 resource, do 2 unpreventable meat damage, and end the run
  (do-game
    (new-game (default-contestant [(qty "Flare" 1)])
              (default-challenger [(qty "Plascrete Carapace" 1) (qty "Clone Chip" 1) (qty "Cache" 3)]))
    (play-from-hand state :contestant "Flare" "HQ")
    (core/gain state :contestant :credit 2)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Plascrete Carapace")
    (play-from-hand state :challenger "Clone Chip")
    (let [flare (get-character state :hq 0)
          cc (get-hazard state 1)]
      (run-on state :hq)
      (core/reveal state :contestant flare)
      (card-subroutine state :contestant flare 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (prompt-select :contestant cc)
      (is (= 1 (count (get-in @state [:challenger :rig :hazard]))) "Clone Chip trashed")
      (is (empty? (:prompt (get-challenger))) "Plascrete didn't try preventing meat damage")
      (is (= 1 (count (:hand (get-challenger)))))
      (is (= 3 (count (:discard (get-challenger)))) "Clone Chip plus 2 cards lost from damage in discard")
      (is (not (:run @state)) "Run ended"))))

(deftest gemini-kicker
  ;; Gemini - Successfully trace to do 1 net damage; do 1 net damage if trace strength is 5 or more regardless of success
  (do-game
    (new-game (default-contestant [(qty "Gemini" 1) (qty "Hedge Fund" 2)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Dirty Laundry" 2)]))
    (play-from-hand state :contestant "Gemini" "HQ")
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (let [gem (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant gem)
      (card-subroutine state :contestant gem 0)
      (prompt-choice :contestant 3) ; boost to trace strength 5
      (prompt-choice :challenger 0)
      (is (= 2 (count (:discard (get-challenger)))) "Did 2 net damage")
      (card-subroutine state :contestant gem 0)
      (prompt-choice :contestant 3) ; boost to trace strength 5
      (prompt-choice :challenger 5) ; match trace
      (is (= 3 (count (:discard (get-challenger)))) "Did only 1 net damage for having trace strength 5 or more"))))

(deftest gemini-chronos-protocol
  ;; Gemini - Interaction with Chronos Protocol and kicker
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Gemini" 1) (qty "Hedge Fund" 2)])
              (default-challenger [(qty "Sure Gamble" 1) (qty "Dirty Laundry" 2)]))
    (play-from-hand state :contestant "Gemini" "HQ")
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (let [gem (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant gem)
      (card-subroutine state :contestant gem 0)
      (prompt-choice :contestant 3) ; boost to trace strength 5
      (prompt-choice :challenger 0)
      (prompt-choice :contestant "Yes")
      (prompt-choice :contestant (find-card "Sure Gamble" (:hand (get-challenger))))
      (is (= 2 (count (:discard (get-challenger)))) "Did 2 net damage"))))

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

(deftest jua-encounter
  ;; Jua (encounter effect) - Prevent Challenger from installing cards for the rest of the turn
  (do-game
    (new-game (default-contestant [(qty "Jua" 1)])
              (default-challenger [(qty "Desperado" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :contestant "Jua" "HQ")
    (take-credits state :contestant)
    (let [jua (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant jua)
      (card-ability state :contestant (refresh jua) 0)
      (run-successful state)
      (is (= 2 (count (:hand (get-challenger)))) "Challenger starts with 2 cards in hand")
      (play-from-hand state :challenger "Desperado")
      (is (= 2 (count (:hand (get-challenger)))) "No cards installed")
      (play-from-hand state :challenger "Sure Gamble")
      (is (= 1 (count (:hand (get-challenger)))) "Can play events")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 1 (count (:hand (get-challenger)))) "Challenger starts with 1 cards in hand")
      (play-from-hand state :challenger "Desperado")
      (is (= 0 (count (:hand (get-challenger)))) "Card installed"))))

(deftest jua-sub
  ;; Jua (subroutine effect) - Select 2 challenger cards, challenger moves one to the stack
  (do-game
    (new-game (default-contestant [(qty "Jua" 1)])
              (default-challenger [(qty "Desperado" 1) (qty "Gordian Blade" 1)]))
    (play-from-hand state :contestant "Jua" "HQ")
    (take-credits state :contestant)
    (let [jua (get-character state :hq 0)]
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Desperado")
      (run-on state "HQ")
      (core/reveal state :contestant jua)
      (card-subroutine state :contestant (refresh jua) 0)
      (is (empty? (:prompt (get-contestant))) "Can't fire for 1 installed card")
      (run-successful state)

      (play-from-hand state :challenger "Gordian Blade")
      (run-on state "HQ")
      (card-subroutine state :contestant (refresh jua) 0)
      (prompt-select :contestant (get-resource state 0))
      (prompt-select :contestant (get-hazard state 0))
      (prompt-choice :challenger "Gordian Blade")
      (is (nil? (get-resource state 0)) "Card is uninstalled")
      (is (= 1 (count (:deck (get-challenger)))) "Challenger puts card in deck"))))

(deftest lockdown
  ;; Lockdown - Prevent Challenger from drawing cards for the rest of the turn
  (do-game
    (new-game (default-contestant [(qty "Lockdown" 1)])
              (default-challenger [(qty "Diesel" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Lockdown" "R&D")
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (let [lock (get-character state :rd 0)]
      (run-on state "R&D")
      (core/reveal state :contestant lock)
      (card-subroutine state :contestant lock 0)
      (run-successful state)
      (play-from-hand state :challenger "Diesel")
      (is (= 1 (count (:hand (get-challenger)))) "No cards drawn")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Diesel")
      (is (= 3 (count (:hand (get-challenger))))
          "New turn ends prevention; remaining 3 cards drawn from Stack"))))

(deftest lotus-field-unlowerable
  ;; Lotus Field strength cannot be lowered
  (do-game
    (new-game (default-contestant [(qty "Lotus Field" 1) (qty "Lag Time" 1)])
              (default-challenger [(qty "Ice Carver" 1) (qty "Parasite" 1)]))
    (play-from-hand state :contestant "Lotus Field" "Archives")
    (take-credits state :contestant 2)
    (let [lotus (get-character state :archives 0)]
      (core/reveal state :contestant lotus)
      (play-from-hand state :challenger "Ice Carver")
      (run-on state "Archives")
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (run-jack-out state)
      (play-from-hand state :challenger "Parasite")
      (prompt-select :challenger lotus)
      (is (= 1 (count (:hosted (refresh lotus)))) "Parasite hosted on Lotus Field")
      (take-credits state :challenger 1)
      (take-credits state :contestant)
      (is (= 1 (core/get-virus-counters state :challenger (first (:hosted (refresh lotus)))))
          "Parasite has 1 virus counter")
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Lag Time")
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased")
      (take-credits state :contestant 2)
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased"))))

(deftest mausolus
  ;; Mausolus - 3 adv tokens change the subroutines
  (do-game
    (new-game (default-contestant [(qty "Mausolus" 1)])
              (default-challenger [(qty "NetChip" 5)]))
    (play-from-hand state :contestant "Mausolus" "HQ")
    (let [mau (get-character state :hq 0)]
      (core/reveal state :contestant mau)
      (take-credits state :contestant)
      (run-on state :hq)
      (is (= 3 (:credit (get-contestant))) "contestant starts encounter with 3 crs")
      (is (= 0 (count (:discard (get-challenger)))) "challenger starts encounter with no cards in heap")
      (is (= 0 (:tag (get-challenger))) "challenger starts encounter with 0 tags")
      (card-subroutine state :contestant mau 0)
      (card-subroutine state :contestant mau 1)
      (card-subroutine state :contestant mau 2)
      (is (= 4 (:credit (get-contestant))) "contestant gains 1 cr from mausolus")
      (is (= 1 (count (:discard (get-challenger)))) "contestant does 1 net damage")
      (is (= 1 (:tag (get-challenger))) "contestant gives 1 tag")
      (run-jack-out state)
      (take-credits state :challenger)
      (core/advance state :contestant {:card (refresh mau)})
      (core/advance state :contestant {:card (refresh mau)})
      (core/advance state :contestant {:card (refresh mau)})
      (run-on state :hq)
      (is (= 1 (:credit (get-contestant))) "contestant starts encounter with 1 crs")
      (is (= 1 (count (:discard (get-challenger)))) "challenger starts encounter with 1 card in heap")
      (is (= 1 (:tag (get-challenger))) "challenger starts encounter with 1 tags")
      (card-subroutine state :contestant mau 0)
      (card-subroutine state :contestant mau 1)
      (card-subroutine state :contestant mau 2)
      (is (= 4 (:credit (get-contestant))) "contestant gains 3 cr")
      (is (= 4 (count (:discard (get-challenger)))) "contestant does 3 net damage")
      (is (= 2 (:tag (get-challenger))) "contestant gives 1 tag")
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:challenger :register :unsuccessful-run]) "Run was unsuccessful"))))

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

(deftest mind-game
  ;; Mind game - PSI redirect to different server
  (do-game
    (new-game (default-contestant [(qty "Mind Game" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Mind Game" "HQ")
    (take-credits state :contestant)
    (run-on state :hq)
    (let [mindgame (get-character state :hq 0)]
      (core/reveal state :contestant mindgame)
      (card-subroutine state :contestant mindgame 0))
    (prompt-choice :contestant "1 [Credits]")
    (prompt-choice :challenger "0 [Credits]")
    (is (= (set ["R&D" "Archives"]) (set (:choices (prompt-map :contestant)))) "Contestant cannot choose server Challenger is on")
    (prompt-choice :contestant "Archives")
    (is (= [:archives] (get-in @state [:run :server])) "Challenger now running on Archives")))

(deftest minelayer
  ;; Minelayer - Install a piece of Character in outermost position of Minelayer's server at no cost
  (do-game
    (new-game (default-contestant [(qty "Minelayer" 1) (qty "Fire Wall" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Minelayer" "HQ")
    (take-credits state :contestant)
    (run-on state :hq)
    (core/reveal state :contestant (get-character state :hq 0))
    (is (= 6 (:credit (get-contestant))))
    (card-subroutine state :contestant (get-character state :hq 0) 0)
    (prompt-select :contestant (find-card "Fire Wall" (:hand (get-contestant))))
    (is (= 2 (count (get-in @state [:contestant :servers :hq :characters]))) "2 Character protecting HQ")
    (is (= 6 (:credit (get-contestant))) "Didn't pay 1 credit to install as second Character")))

(deftest morph-character-subtype-changing
  ;; Morph character gain and lose subtypes from normal advancements and placed advancements
  (do-game
    (new-game (default-contestant [(qty "Wendigo" 1)
                             (qty "Shipment from SanSan" 1)
                             (qty "Superior Cyberwalls" 1)])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Superior Cyberwalls" "New remote")
    (let [sc (get-content state :remote1 0)]
      (score-agenda state :contestant sc)
      (play-from-hand state :contestant "Wendigo" "HQ")
      (let [wend (get-character state :hq 0)]
        (core/reveal state :contestant wend)
        (is (= 4 (:current-strength (refresh wend))) "Wendigo at normal 4 strength")
        (core/advance state :contestant {:card (refresh wend)})
        (is (= true (has? (refresh wend) :subtype "Barrier")) "Wendigo gained Barrier")
        (is (= false (has? (refresh wend) :subtype "Code Gate")) "Wendigo lost Code Gate")
        (is (= 5 (:current-strength (refresh wend))) "Wendigo boosted to 5 strength by scored Superior Cyberwalls")
        (play-from-hand state :contestant "Shipment from SanSan")
        (prompt-choice :contestant "1")
        (prompt-select :contestant wend)
        (is (= false (has? (refresh wend) :subtype "Barrier")) "Wendigo lost Barrier")
        (is (= true (has? (refresh wend) :subtype "Code Gate")) "Wendigo gained Code Gate")
        (is (= 4 (:current-strength (refresh wend))) "Wendigo returned to normal 4 strength")))))

(deftest mother-goddess
  ;; Mother Goddess - Gains other character subtypes
  (do-game
    (new-game (default-contestant [(qty "Mother Goddess" 1) (qty "NEXT Bronze" 1)])
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
    (new-game (default-contestant [(qty "NEXT Bronze" 2) (qty "NEXT Silver" 1)])
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

(deftest nightdancer
  ;; Nightdancer - Challenger loses a click if able, contestant gains a click on next turn
  (do-game
    (new-game (default-contestant [(qty "Nightdancer" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Nightdancer" "HQ")
    (take-credits state :contestant)
    (let [nd (get-character state :hq 0)]
      (core/reveal state :contestant nd)
      (run-on state "HQ")
      (is (= 3 (:click (get-challenger))) "Challenger starts with 3 clicks")
      (card-subroutine state :contestant nd 0)
      (is (= 2 (:click (get-challenger))) "Challenger lost 1 click")
      (card-subroutine state :contestant nd 0)
      (is (= 1 (:click (get-challenger))) "Challenger lost 1 click")
      (run-jack-out state)
      (take-credits state :challenger)
      (is (= 5 (:click (get-contestant))) "Contestant has 5 clicks"))))

(deftest resistor
  ;; Resistor - Strength equal to Challenger tags, lose strength when Challenger removes a tag
  (do-game
    (new-game (default-contestant [(qty "Resistor" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Resistor" "HQ")
    (let [resistor (get-character state :hq 0)]
      (core/reveal state :contestant resistor)
      (is (= 0 (:current-strength (refresh resistor))) "No Challenger tags; 0 strength")
      (core/tag-challenger state :challenger 2)
      (is (= 2 (:tag (get-challenger))))
      (is (= 2 (:current-strength (refresh resistor))) "2 Challenger tags; 2 strength")
      (take-credits state :contestant)
      (core/remove-tag state :challenger 1)
      (is (= 1 (:current-strength (refresh resistor))) "Challenger removed 1 tag; down to 1 strength"))))

(deftest self-adapting-code-wall-unlowerable
  ;; self-adapting code wall strength cannot be lowered
  (do-game
    (new-game (default-contestant [(qty "Self-Adapting Code Wall" 1) (qty "Lag Time" 1)])
              (default-challenger [(qty "Ice Carver" 1) (qty "Parasite" 1)]))
    (play-from-hand state :contestant "Self-Adapting Code Wall" "Archives")
    (take-credits state :contestant 2)
    (let [sacw (get-character state :archives 0)]
      (core/reveal state :contestant sacw)
      (play-from-hand state :challenger "Ice Carver")
      (run-on state "Archives")
      (is (= 1 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength unchanged")
      (run-jack-out state)
      (play-from-hand state :challenger "Parasite")
      (prompt-select :challenger sacw)
      (is (= 1 (count (:hosted (refresh sacw)))) "Parasite hosted on Self-Adapting Code Wall")
      (take-credits state :challenger 1)
      (take-credits state :contestant)
      (is (= 1 (core/get-virus-counters state :challenger (first (:hosted (refresh sacw)))))
          "Parasite has 1 virus counter")
      (is (= 1 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength unchanged")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Lag Time")
      (is (= 2 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength increased")
      (take-credits state :contestant 2)
      (is (= 2 (:current-strength (refresh sacw))) "Self-Adapting Code Wall strength increased"))))

(deftest searchlight
  ;; Searchlight - Trace bace equal to advancement counters
  (do-game
    (new-game (default-contestant [(qty "Searchlight" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Searchlight" "HQ")
    (let [searchlight (get-character state :hq 0)]
      (core/reveal state :contestant searchlight)
      (card-subroutine state :contestant (refresh searchlight) 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 0 (:tag (get-challenger))) "Trace failed with 0 advancements")
      (core/advance state :contestant {:card (refresh searchlight)})
      (card-subroutine state :contestant (refresh searchlight) 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 1 (:tag (get-challenger))) "Trace succeeds with 0 advancements"))))

(deftest sherlock
  ;; Sherlock 1.0 - Trace to add an installed resource to the top of Challenger's Stack
  (do-game
    (new-game (default-contestant [(qty "Sherlock 1.0" 1)])
              (default-challenger [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Sherlock 1.0" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Gordian Blade")
    (run-on state :hq)
    (core/reveal state :contestant (get-character state :hq 0))
    (card-subroutine state :contestant (get-character state :hq 0) 0)
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (prompt-select :contestant (get-in @state [:challenger :rig :resource 0]))
    (is (empty? (get-in @state [:challenger :rig :resource])) "Gordian uninstalled")
    (is (= "Gordian Blade" (:title (first (:deck (get-challenger))))) "Gordian on top of Stack")))

(deftest shiro
  ;; Shiro - Full test
  (do-game
    (new-game (default-contestant [(qty "Shiro" 1) (qty "Caprcharacter Nisei" 1)
                             (qty "Quandary" 1) (qty "Jackson Howard" 1)])
              (default-challenger [(qty "R&D Interface" 1)]))
    (starting-hand state :contestant ["Shiro"])
    (play-from-hand state :contestant "Shiro" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "R&D Interface")
    (let [shiro (get-character state :hq 0)]
      (run-on state :hq)
      (core/reveal state :contestant shiro)
      (card-subroutine state :contestant shiro 0)
      (prompt-choice :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
      (prompt-choice :contestant (find-card "Quandary" (:deck (get-contestant))))
      (prompt-choice :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
      ;; try starting over
      (prompt-choice :contestant "Start over")
      (prompt-choice :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
      (prompt-choice :contestant (find-card "Quandary" (:deck (get-contestant))))
      (prompt-choice :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant)))) ;this is the top card of R&D
      (prompt-choice :contestant "Done")
      (is (= "Caprcharacter Nisei" (:title (first (:deck (get-contestant))))))
      (is (= "Quandary" (:title (second (:deck (get-contestant))))))
      (is (= "Jackson Howard" (:title (second (rest (:deck (get-contestant)))))))
      (card-subroutine state :contestant shiro 1)
      (is (= (:cid (first (:deck (get-contestant))))
             (:cid (:card (first (:prompt (get-challenger)))))) "Access the top card of R&D")
      (prompt-choice :challenger "No")
      (is (= (:cid (second (:deck (get-contestant))))
             (:cid (:card (first (:prompt (get-challenger)))))) "Access another card due to R&D Interface"))))

(deftest snowflake
  ;; Snowflake - Win a psi game to end the run
  (do-game
    (new-game (default-contestant [(qty "Snowflake" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Snowflake" "HQ")
    (take-credits state :contestant)
    (run-on state :hq)
    (let [sf (get-character state :hq 0)]
      (core/reveal state :contestant sf)
      (card-subroutine state :contestant sf 0)
      (prompt-choice :contestant "0 [Credits]")
      (prompt-choice :challenger "0 [Credits]")
      (is (:run @state) "Challenger won psi, run continues")
      (card-subroutine state :contestant sf 0)
      (prompt-choice :contestant "0 [Credits]")
      (prompt-choice :challenger "1 [Credits]")
      (is (not (:run @state)) "Run ended"))))

(deftest special-offer-trash-character-during-run
  ;; Special Offer trashes itself and updates the run position
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 1) (qty "Special Offer" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Special Offer" "HQ")
    (take-credits state :contestant 1)
    (run-on state "HQ")
    (is (= 2 (:position (get-in @state [:run]))) "Initial position approaching Special Offer")
    (let [special (get-character state :hq 1)]
      (core/reveal state :contestant special)
      (is (= 4 (:credit (get-contestant))))
      (card-subroutine state :contestant special 0)
      (is (= 9 (:credit (get-contestant))) "Special Offer paid 5 credits")
      (is (= 1 (:position (get-in @state [:run])))
          "Run position updated; now approaching Ice Wall"))))

(deftest tithonium
  ;; Forfeit option as reveal cost, can have hosted condition counters
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 1) (qty "Tithonium" 1) (qty "Patch" 1)])
              (default-challenger [(qty "Pawn" 1) (qty "Wasteland" 1)]))
    (core/gain state :contestant :click 10)
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (play-from-hand state :contestant "Tithonium" "HQ")
    (let [ht (get-content state :remote1 0)
          ti (get-character state :hq 0)]
      (score-agenda state :contestant ht)
      (is (= 1 (count (:scored (get-contestant)))) "Agenda scored")
      (is (= 12 (:credit (get-contestant))) "Gained 7 credits")
      (core/reveal state :contestant ti)
      (prompt-choice :contestant "No") ; don't use alternative cost
      (is (= 3 (:credit (get-contestant))) "Spent 9 to Reveal")
      (core/hide state :contestant (refresh ti))
      (core/reveal state :contestant ti)
      (prompt-choice :contestant "Yes") ; use alternative cost
      (prompt-select :contestant (get-in (get-contestant) [:scored 0]))
      (is (= 3 (:credit (get-contestant))) "Still on 3c")
      (is (= 0 (count (:scored (get-contestant)))) "Agenda forfeited")
      ;; Can Host Conditions Counters
      (play-from-hand state :contestant "Patch")
      (prompt-select :contestant (refresh ti))
      (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
      (take-credits state :contestant)
      (core/hide state :contestant (refresh ti))
      (is (= 1 (count (:hosted (refresh ti)))) "1 card on Tithonium")
      (play-from-hand state :challenger "Pawn")
      (play-from-hand state :challenger "Wasteland")
      (let [pawn (get-resource state 0)
            wast (get-muthereff state 0)]
        (card-ability state :challenger (refresh pawn) 0)
        (prompt-select :challenger (refresh ti))
        (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
        (core/hide state :contestant (refresh ti))
        (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
        (run-on state "HQ")
        (card-subroutine state :contestant ti 2)
        (prompt-select :contestant (refresh wast))
        (is (= 1 (count (:discard (get-challenger)))) "1 card trashed")
        (card-subroutine state :contestant ti 1)
        (is (not (:run @state)) "Run ended")))))

(deftest tithonium-oversight-ai
  ;; Do not prompt for alt cost #2734
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 1) (qty "Oversight AI" 1) (qty "Tithonium" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (play-from-hand state :contestant "Tithonium" "R&D")
    (let [ht (get-content state :remote1 0)
          ti (get-character state :rd 0)]
      (score-agenda state :contestant ht)
      (play-from-hand state :contestant "Oversight AI")
      (prompt-select :contestant ti)
      (is (get-in (refresh ti) [:revealed]))
      (is (= "Oversight AI" (:title (first (:hosted (refresh ti)))))
          "Tithonium hosting OAI as a condition"))))

(deftest tmi
  ;; TMI Character test
  (do-game
    (new-game (default-contestant [(qty "TMI" 3)])
              (default-challenger))
    (play-from-hand state :contestant "TMI" "HQ")
    (let [tmi (get-character state :hq 0)]
      (core/reveal state :contestant tmi)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (get-in (refresh tmi) [:revealed])))))

(deftest tmi-hide
  ;; TMI Character trace hide
  (do-game
    (new-game (default-contestant [(qty "TMI" 3)])
              (make-deck "Sunny Lebeau: Security Specialist" [(qty "Blackmail" 3)]))
    (play-from-hand state :contestant "TMI" "HQ")
    (let [tmi (get-character state :hq 0)]
      (core/reveal state :contestant tmi)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (not (get-in (refresh tmi) [:revealed]))))))

(deftest turing-positional-strength
  ;; Turing - Strength boosted when protecting a remote server
  (do-game
    (new-game (default-contestant [(qty "Turing" 2) (qty "Hedge Fund" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Turing" "HQ")
    (play-from-hand state :contestant "Turing" "New remote")
    (let [t1 (get-character state :hq 0)
          t2 (get-character state :remote1 0)]
      (core/reveal state :contestant t1)
      (is (= 2 (:current-strength (refresh t1)))
          "Turing default 2 strength over a central server")
      (core/reveal state :contestant t2)
      (is (= 5 (:current-strength (refresh t2)))
          "Turing increased to 5 strength over a remote server"))))

(deftest wraparound
  ;; Wraparound - Strength boosted when no fracter is installed
  (do-game
    (new-game (default-contestant [(qty "Wraparound" 1)])
              (default-challenger [(qty "Corroder" 1)]))
    (play-from-hand state :contestant "Wraparound" "HQ")
    (let [wrap (get-character state :hq 0)]
      (core/reveal state :contestant wrap)
      (is (= 7 (:current-strength (refresh wrap)))
          "Wraparound +7 strength with no fracter in play")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Corroder")
      (is (= 0 (:current-strength (refresh wrap)))
          "Wraparound 0 strength after Corroder installed"))))
