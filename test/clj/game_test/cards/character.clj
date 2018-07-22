(ns game-test.cards.character
  (:require [game.core :as core]
            [game.utils :refer [has?]]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "character"))

(deftest ^:skip-card-coverage
  end-the-run-test
  ;; Since all ETR character share a common ability, we only need one test
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3) (qty "Hedge Fund" 3) (qty "Restructure" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant 2)
    (run-on state "HQ")
    (is (= [:hq] (get-in @state [:run :locale])))
    (let [iwall (get-character state :hq 0)]
      (core/reveal state :contestant iwall)
      (card-subroutine state :contestant iwall 0)
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:challenger :register :unsuccessful-run]) "Run was unsuccessful"))))

(deftest aimor
  ;; Aimor - discard the top 3 cards of the stack, discard Aimor
  (do-game
    (new-game (default-contestant ["Aimor"])
              (default-challenger [(qty "Sure Gamble" 2) "Desperado" "Corroder" "Patron"]))
    (starting-hand state :challenger ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :contestant "Aimor" "HQ")
    (is (= 1 (count (get-in @state [:contestant :locales :hq :characters]))) "Aimor placed")
    (take-credits state :contestant)
    (let [aim (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant aim)
      (card-subroutine state :contestant aim 0)
      (is (= 3 (count (:discard (get-challenger)))) "Challenger discarded 3 cards")
      (is (= 1 (count (:deck (get-challenger)))) "Challenger has 1 card in deck"))
    (is (zero? (count (get-in @state [:contestant :locales :hq :characters]))) "Aimor discarded")))

(deftest archangel
  ;; Archangel - accessing from R&D does not cause run to hang.
  (do-game
    (new-game (default-contestant ["Archangel" "Hedge Fund"])
              (default-challenger ["Bank Job"]))
    (starting-hand state :contestant ["Hedge Fund"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Bank Job")
    (run-empty-locale state :rd)
    (prompt-choice :contestant "Yes")
    (prompt-choice :challenger "Yes")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (prompt-select :contestant (get-radicle state 0))
    (prompt-choice :challenger "No action")
    (is (not (:run @state)) "Run ended")))

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

(deftest asteroid-belt
  ;; Asteroid Belt - Space Character reveal cost reduced by 3 credits per advancement
  (do-game
    (new-game (default-contestant ["Asteroid Belt"])
              (default-challenger))
    (core/gain state :contestant :credit 5)
    (play-from-hand state :contestant "Asteroid Belt" "HQ")
    (let [ab (get-character state :hq 0)]
      (core/advance state :contestant {:card (refresh ab)})
      (core/advance state :contestant {:card (refresh ab)})
      (is (= 8 (:credit (get-contestant))))
      (is (= 2 (get-counters (refresh ab) :advancement)))
      (core/reveal state :contestant (refresh ab))
      (is (= 5 (:credit (get-contestant))) "Paid 3 credits to reveal; 2 advancments on Asteroid Belt"))))

(deftest bandwidth
  ;; Bandwidth - Give the Challenger 1 tag; remove 1 tag if the run is successful
  (do-game
    (new-game (default-contestant ["Bandwidth"])
              (default-challenger))
    (play-from-hand state :contestant "Bandwidth" "Archives")
    (let [bw (get-character state :archives 0)]
      (take-credits state :contestant)
      (run-on state "Archives")
      (core/reveal state :contestant bw)
      (card-subroutine state :contestant bw 0)
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag")
      (run-successful state)
      (is (zero? (:tag (get-challenger))) "Run successful; Challenger lost 1 tag")
      (run-on state "Archives")
      (card-subroutine state :contestant bw 0)
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag")
      (run-jack-out state)
      (is (= 1 (:tag (get-challenger))) "Run unsuccessful; Challenger kept 1 tag"))))

(deftest bullfrog
  ;; Bullfrog - Win psi to move to outermost position of another locale and continue run there
  (do-game
    (new-game (default-contestant ["Bullfrog" (qty "Pup" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Bullfrog" "HQ")
    (play-from-hand state :contestant "Pup" "R&D")
    (play-from-hand state :contestant "Pup" "R&D")
    (take-credits state :contestant)
    (run-on state :hq)
    (let [frog (get-character state :hq 0)]
      (core/reveal state :contestant frog)
      (is (= :hq (first (get-in @state [:run :locale]))))
      (card-subroutine state :contestant frog 0)
      (prompt-choice :contestant "0 [Credits]")
      (prompt-choice :challenger "1 [Credits]")
      (prompt-choice :contestant "R&D")
      (is (= :rd (first (get-in @state [:run :locale]))) "Run redirected to R&D")
      (is (= 2 (get-in @state [:run :position])) "Passed Bullfrog")
      (is (= "Bullfrog" (:title (get-character state :rd 2))) "Bullfrog at outermost position of R&D"))))

(deftest cell-portal
  ;; Cell Portal - Bounce Challenger to outermost position and hide itself
  (do-game
    (new-game (default-contestant ["Cell Portal" (qty "Paper Wall" 2)])
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
      (is (not (:revealed (refresh cp))) "Cell Portal hidden"))))

(deftest chimera
  ;; Chimera - Gains chosen subtype
  (do-game
    (new-game (default-contestant ["Chimera"])
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
    (new-game (default-contestant ["Cortex Lock"])
              (default-challenger [(qty "Corroder" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Cortex Lock" "HQ")
    (take-credits state :contestant)
    (let [cort (get-character state :hq 0)]
      (play-from-hand state :challenger "Corroder")
      (is (= 3 (core/available-mu state)))
      (run-on state "HQ")
      (core/reveal state :contestant cort)
      (card-subroutine state :contestant cort 0)
      (is (= 3 (count (:discard (get-challenger)))) "Challenger suffered 3 net damage"))))

(deftest crick
  ;; Crick - Strength boost when protecting Archives; places a card from Archives
  (do-game
    (new-game (default-contestant [(qty "Crick" 2) "Ice Wall"])
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
      (is (= 3 (:credit (get-contestant))) "Paid 1 credit to place as 2nd Character over HQ"))))

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

(deftest data-hound
  ;; Data Hound - Full test
  (do-game
    (new-game (default-contestant ["Data Hound"])
              (default-challenger [(qty "Sure Gamble" 2) "Desperado"
                               "Corroder" "Patron"]))
    (starting-hand state :challenger ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :contestant "Data Hound" "HQ")
    (take-credits state :contestant)
    (let [dh (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant dh)
      (card-subroutine state :contestant dh 0)
      (prompt-choice :contestant 2)
      (prompt-choice :challenger 0)
      ;; discard 1 card and rearrange the other 3
      (prompt-card :contestant (find-card "Desperado" (:deck (get-challenger))))
      (is (= 1 (count (:discard (get-challenger)))))
      (prompt-card :contestant (find-card "Sure Gamble" (:deck (get-challenger))))
      (prompt-card :contestant (find-card "Corroder" (:deck (get-challenger))))
      (prompt-card :contestant (find-card "Patron" (:deck (get-challenger))))
      ;; try starting over
      (prompt-choice :contestant "Start over")
      (prompt-card :contestant (find-card "Patron" (:deck (get-challenger))))
      (prompt-card :contestant (find-card "Corroder" (:deck (get-challenger))))
      (prompt-card :contestant (find-card "Sure Gamble" (:deck (get-challenger)))) ;this is the top card on stack
      (prompt-choice :contestant "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-challenger))))))
      (is (= "Corroder" (:title (second (:deck (get-challenger))))))
      (is (= "Patron" (:title (second (rest (:deck (get-challenger)))))))
      (run-jack-out state)
      (run-on state "HQ")
      (card-subroutine state :contestant dh 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 1)
      ;; discard the only card automatically
      (is (= 2 (count (:discard (get-challenger)))))
      (is (= "Corroder" (:title (first (:deck (get-challenger)))))))))

(deftest data-mine
  ;; Data Mine - do one net and discard
  (do-game
    (new-game (default-contestant ["Data Mine"])
              (default-challenger))
    (play-from-hand state :contestant "Data Mine" "Locale 1")
    (take-credits state :contestant)
    (let [dm (get-character state :party1 0)]
      (run-on state "Locale 1")
      (core/reveal state :contestant dm)
      (card-subroutine state :contestant dm 0)
      (is (= 1 (count (:discard (get-challenger)))) "Challenger suffered 1 net damage"))))

(deftest draco
  ;; Dracō - Pay credits when revealed to increase strength; trace to give 1 tag and end the run
  (do-game
    (new-game (default-contestant ["Dracō"])
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
    (new-game (default-contestant ["Enigma"])
              (default-challenger))
    (play-from-hand state :contestant "Enigma" "HQ")
    (take-credits state :contestant)
    (let [enig (get-character state :hq 0)]
      (run-on state "HQ")
      (is (= 3 (:click (get-challenger))))
      (core/reveal state :contestant enig)
      (card-subroutine state :contestant enig 0)
      (is (= 2 (:click (get-challenger))) "Challenger lost 1 click"))))

(deftest envelope
  ;; Envelope - do 1 net damage, end the run
  (do-game
    (new-game (default-contestant ["Envelope"])
              (default-challenger))
    (play-from-hand state :contestant "Envelope" "HQ")
    (take-credits state :contestant)
    (let [envl (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant envl)
      (is (zero? (count (:discard (get-challenger)))) "No discarded cards")
      (card-subroutine state :contestant envl 0)
      (is (= 1 (count (:discard (get-challenger)))) "1 card in discard pile")
      (is (:run @state) "Run still ongoing")
      (card-subroutine state :contestant envl 1)
      (is (not (:run @state)) "Run ended"))))

(deftest excalibur
  ;; Excalibur - Prevent Challenger from making another run this turn
  (do-game
    (new-game (default-contestant ["Excalibur"])
              (default-challenger ["Stimhack"]))
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
    (new-game (default-contestant ["Fenris"])
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
  ;; Flare - Discard 1 resource, do 2 unpreventable meat damage, and end the run
  (do-game
    (new-game (default-contestant ["Flare"])
              (default-challenger ["Plascrete Carapace" "Clone Chip" (qty "Cache" 3)]))
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
      (is (= 1 (count (get-hazard state))) "Clone Chip discarded")
      (is (empty? (:prompt (get-challenger))) "Plascrete didn't try preventing meat damage")
      (is (= 1 (count (:hand (get-challenger)))))
      (is (= 3 (count (:discard (get-challenger)))) "Clone Chip plus 2 cards lost from damage in discard")
      (is (not (:run @state)) "Run ended"))))

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

(deftest gemini
  ;; Gemini - Successfully trace to do 1 net damage; do 1 net damage if trace strength is 5 or more regardless of success
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Gemini" (qty "Hedge Fund" 2)])
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
  (testing "Interaction with Chronos Protocol and kicker"
    (do-game
      (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" ["Gemini" (qty "Hedge Fund" 2)])
                (default-challenger ["Sure Gamble" (qty "Dirty Laundry" 2)]))
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
        (prompt-card :contestant (find-card "Sure Gamble" (:hand (get-challenger))))
        (is (= 2 (count (:discard (get-challenger)))) "Did 2 net damage")))))

(deftest holmegaard
  ;; Holmegaard - Stop Challenger from accessing cards if win trace
  (do-game
    (new-game (default-contestant ["Holmegaard" "Hostile Takeover"])
              (default-challenger ["Cache" "Inti"]))
    (core/gain state :contestant :credit 10)
    (play-from-hand state :contestant "Holmegaard" "HQ")
    (let [holm (get-character state :hq 0)]
      (core/reveal state :contestant holm)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Inti")
      (play-from-hand state :challenger "Cache")
      (run-on state "HQ")
      (card-subroutine state :contestant holm 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (card-subroutine state :contestant holm 1)
      (prompt-select :contestant (get-resource state 1))
      (is (empty? (:discard (get-challenger))) "Can't target non-characterbreaker resource")
      (prompt-select :contestant (get-resource state 0))
      (is (= 1 (count (:discard (get-challenger)))) "Inti discarded")
      (run-continue state)
      (run-successful state)
      ;; Prompt for "you cannot access any card this run"
      (prompt-choice :challenger "No action")
      (is (not (accessing state "Hostile Takeover"))))))

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

(deftest ^{:card-title "it's-a-trap!"}
  its-a-trap
  ;; It's a Trap! - 2 net dmg on expose, self-discard and make Challenger discard placed card
  (do-game
    (new-game (default-contestant ["It's a Trap!"])
              (default-challenger [(qty "Cache" 3) (qty "Infiltration" 2)]))
    (play-from-hand state :contestant "It's a Trap!" "Archives")
    (let [iat (get-character state :archives 0)]
      (take-credits state :contestant)
      (play-from-hand state :challenger "Infiltration")
      (prompt-choice :challenger "Expose a card")
      (prompt-select :challenger iat)
      (is (= 3 (count (:discard (get-challenger)))) "Did 2 net damage on expose")
      (play-from-hand state :challenger "Cache")
      (run-on state "archives")
      (core/reveal state :contestant iat)
      (card-subroutine state :contestant (refresh iat) 0)
      (prompt-select :challenger (get-resource state 0))
      (is (= 4 (count (:discard (get-challenger)))) "Cache discarded")
      (is (= 1 (count (:discard (get-contestant)))) "It's a Trap discarded"))))

(deftest jua
  ;; Jua
  (testing "Encounter effect - Prevent Challenger from placing cards for the rest of the turn"
    (do-game
      (new-game (default-contestant ["Jua"])
                (default-challenger ["Desperado" "Sure Gamble"]))
      (play-from-hand state :contestant "Jua" "HQ")
      (take-credits state :contestant)
      (let [jua (get-character state :hq 0)]
        (run-on state "HQ")
        (core/reveal state :contestant jua)
        (card-ability state :contestant (refresh jua) 0)
        (run-successful state)
        (is (= 2 (count (:hand (get-challenger)))) "Challenger starts with 2 cards in hand")
        (play-from-hand state :challenger "Desperado")
        (is (= 2 (count (:hand (get-challenger)))) "No cards placed")
        (play-from-hand state :challenger "Sure Gamble")
        (is (= 1 (count (:hand (get-challenger)))) "Can play events")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (count (:hand (get-challenger)))) "Challenger starts with 1 cards in hand")
        (play-from-hand state :challenger "Desperado")
        (is (zero? (count (:hand (get-challenger)))) "Card placed"))))
  (testing "Subroutine effect - Select 2 challenger cards, challenger moves one to the stack"
    (do-game
      (new-game (default-contestant ["Jua"])
                (default-challenger ["Desperado" "Gordian Blade"]))
      (play-from-hand state :contestant "Jua" "HQ")
      (take-credits state :contestant)
      (let [jua (get-character state :hq 0)]
        (core/gain state :challenger :credit 10)
        (play-from-hand state :challenger "Desperado")
        (run-on state "HQ")
        (core/reveal state :contestant jua)
        (card-subroutine state :contestant (refresh jua) 0)
        (is (empty? (:prompt (get-contestant))) "Can't fire for 1 placed card")
        (run-successful state)
        (play-from-hand state :challenger "Gordian Blade")
        (run-on state "HQ")
        (card-subroutine state :contestant (refresh jua) 0)
        (prompt-select :contestant (get-resource state 0))
        (prompt-select :contestant (get-hazard state 0))
        (prompt-card :challenger (get-resource state 0))
        (is (nil? (get-resource state 0)) "Card is unplaced")
        (is (= 1 (count (:deck (get-challenger)))) "Challenger puts card in deck"))))
 (testing "Should only lock placing for Challenger, not for both sides"
    (do-game
      (new-game (make-deck "Mti Mwekundu: Life Improved" ["Jua" "Kakugo"])
                (default-challenger ["Paperclip"]))
      (play-from-hand state :contestant "Jua" "HQ")
      (let [mti (get-in @state [:contestant :identity])
            jua (get-character state :hq 0)]
        (core/reveal state :contestant jua)
        (take-credits state :contestant)
        (discard-from-hand state :challenger "Paperclip")
        (run-on state "HQ")
        (is (= 1 (get-in @state [:run :position])) "Now approaching Jua")
        (card-ability state :contestant jua 0)
        (run-continue state)
        (is (zero? (get-in @state [:run :position])) "Initial position approaching locale")
        (card-ability state :contestant mti 0)
        (prompt-select :contestant (find-card "Kakugo" (:hand (get-contestant))))
        (is (= 1 (get-in @state [:run :position])) "Now approaching Kakugo")
        (is (= "Kakugo" (:title (get-character state :hq 0))) "Kakugo was placed")
        (is (empty? (:hand (get-contestant))) "Kakugo removed from HQ")
        (core/reveal state :contestant (get-character state :hq 0))
        (is (empty? (:prompt (get-challenger))) "Challenger can't place Paperclip because of Jua encounter ability")
        (run-continue state)
        (is (= 1 (-> (get-challenger) :discard count)) "Challenger should take 1 net damage from Kakugo")))))

(deftest kakugo
  ;; Kakugo
  (testing "ability continues to work when character is swapped"
    (do-game
      (new-game (default-contestant ["Kakugo"
                               "Ice Wall"])
                (default-challenger))
      (play-from-hand state :contestant "Kakugo" "R&D")
      (play-from-hand state :contestant "Ice Wall" "Archives")
      (take-credits state :contestant)
      (let [kakugo   (get-character state :rd 0)
            character-wall (get-character state :archives 0)]
        (run-on state "R&D")
        (core/reveal state :contestant kakugo)
        (run-continue state)
        (run-jack-out state)
        (is (= 2 (count (:hand (get-challenger)))) "Challenger took damage before swap")
        (core/swap-character state :contestant (refresh kakugo) (refresh character-wall))
        (run-on state "Archives")
        (run-continue state)
        (run-jack-out state)
        (is (= 1 (count (:hand (get-challenger)))) "Challenger took damage after swap")))))

(deftest kamali-1.0
  ;; Kamali 1.0
  (do-game
    (new-game (default-contestant ["Kamali 1.0"])
              (default-challenger ["Astrolabe" "Decoy"
                               "Cache" "Hedge Fund"]))
    (play-from-hand state :contestant "Kamali 1.0" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Astrolabe")
    (play-from-hand state :challenger "Decoy")
    (play-from-hand state :challenger "Cache")
   (let [kamali (get-character state :hq 0)]
     (run-on state "HQ")
     (core/reveal state :contestant kamali)
     (card-subroutine state :contestant kamali 0)
     (is (zero? (:brain-damage (get-challenger))) "Challenger starts with 0 brain damage")
     (prompt-choice :challenger "Take 1 brain damage")
     (is (= 1 (:brain-damage (get-challenger))) "Challenger took 1 brain damage")
     (card-subroutine state :contestant kamali 1)
     (is (empty? (:discard (get-challenger))) "Challenger starts with no discarded cards")
     (prompt-choice :challenger "Discard an placed piece of hazard")
     (prompt-select :challenger (get-hazard state 0))
     (is (empty? (get-hazard state)) "Astrolabe discarded")
     (is (= 1 (count (:discard (get-challenger)))) "Challenger discarded 1 card")
     (card-subroutine state :contestant kamali 2)
     (is (= 1 (count (:discard (get-challenger)))) "Challenger starts with 1 discarded card")
     (prompt-choice :challenger "Discard an placed resource")
     (prompt-select :challenger (get-resource state 0))
     (is (empty? (get-resource state)) "Cache discarded")
     (is (= 2 (count (:discard (get-challenger)))) "Challenger discarded 1 card"))))

(deftest kitsune
  (testing "Kitsune - Contestant choices card for Challenger to access"
    (do-game
      (new-game (default-contestant ["Kitsune" "Snare!"])
                (default-challenger))
      (play-from-hand state :contestant "Kitsune" "R&D")
      (take-credits state :contestant)
      (run-on state "R&D")
      (let [kitsune (get-character state :rd 0)]
        (core/reveal state :contestant kitsune)
        (card-subroutine state :contestant kitsune 0)
        (prompt-select :contestant (find-card "Snare!" (:hand (get-contestant))))
        ;; Challenger access Snare! contestant has prompt
        (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
            "Challenger has prompt to wait for Contestant to use Snare!")
        (prompt-choice :contestant "Yes")
        (is (= "Kitsune" (-> (get-contestant) :discard first :title)) "Kitsune was discarded after use")))))

(deftest lockdown
  ;; Lockdown - Prevent Challenger from drawing cards for the rest of the turn
  (do-game
    (new-game (default-contestant ["Lockdown"])
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

(deftest lotus-field
  ;; Lotus Field strength cannot be lowered
  (do-game
    (new-game (default-contestant ["Lotus Field" "Lag Time"])
              (default-challenger ["Ice Carver" "Parasite"]))
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

(deftest magnet
  ;; Magnet - host resource when revealed
  (testing "Faceup character"
    (do-game
      (new-game (default-contestant ["Magnet" "Enigma"])
                (default-challenger ["Parasite"]))
      (play-from-hand state :contestant "Magnet" "HQ")
      (play-from-hand state :contestant "Enigma" "R&D")
      (core/reveal state :contestant (get-character state :rd 0))
      (take-credits state :contestant)
      (let [m (get-character state :hq 0)
            e (get-character state :rd 0)]
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger (refresh e))
        (is (= 1 (count (:hosted (refresh e)))) "Parasite hosted on Enigma")
        (run-on state "HQ")
        (core/reveal state :contestant (get-character state :hq 0))
        (prompt-select :contestant (first (:hosted (get-character state :rd 0))))
        (is (empty? (:hosted (refresh e))) "Enigma not hosting Parasite")
        (is (= 1 (count (:hosted (refresh m)))) "Parasite hosted on Magnet")
        (prompt-choice-partial :challenger "Jack")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (zero? (core/get-virus-counters state :challenger (first (:hosted (refresh m)))))
          "Parasite does not gain a virus counter"))))
  (testing "Facedown character"
    (do-game
      (new-game (default-contestant ["Magnet" "Enigma"])
                (default-challenger ["Trypano"]))
      (play-from-hand state :contestant "Magnet" "HQ")
      (play-from-hand state :contestant "Enigma" "R&D")
      (take-credits state :contestant)
      (let [m (get-character state :hq 0)
            e (get-character state :rd 0)]
        (play-from-hand state :challenger "Trypano")
        (prompt-select :challenger (refresh e))
        (is (= 1 (count (:hosted (refresh e)))) "Trypano hosted on Enigma")
        (run-on state "HQ")
        (core/reveal state :contestant (get-character state :hq 0))
        (prompt-select :contestant (first (:hosted (get-character state :rd 0))))
        (is (empty? (:hosted (refresh e))) "Enigma not hosting Trypano")
        (is (= 1 (count (:hosted (refresh m)))) "Trypano hosted on Magnet")
        (prompt-choice-partial :challenger "Jack")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (empty? (:prompt (get-challenger))) "No Trypano prompt")
        (is (zero? (core/get-virus-counters state :challenger (first (:hosted (refresh m)))))
          "Trypano does not gain a virus counter"))))
  (testing "Hidden character"
    (do-game
      (new-game (default-contestant ["Magnet" "Enigma"])
                (default-challenger [(qty "Parasite" 2)]))
      (play-from-hand state :contestant "Magnet" "HQ")
      (play-from-hand state :contestant "Enigma" "R&D")
      (core/reveal state :contestant (get-character state :rd 0))
      (take-credits state :contestant)
      (let [m (get-character state :hq 0)
            e (get-character state :rd 0)]
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger (refresh e))
        (is (= 1 (count (:hosted (refresh e)))) "Parasite hosted on Enigma")
        (run-on state "HQ")
        (core/reveal state :contestant (get-character state :hq 0))
        (prompt-select :contestant (first (:hosted (get-character state :rd 0))))
        (is (empty? (:hosted (refresh e))) "Enigma not hosting Parasite")
        (is (= 1 (count (:hosted (refresh m)))) "Parasite hosted on Magnet")
        (prompt-choice-partial :challenger "Jack")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (zero? (core/get-virus-counters state :challenger (first (:hosted (refresh m)))))
          "Parasite does not gain a virus counter")
        (take-credits state :challenger)
        (core/hide state :contestant (refresh m))
        (take-credits state :contestant)
        (is (= 1 (core/get-virus-counters state :challenger (first (:hosted (refresh m)))))
          "Parasite gains a virus counter on hidden Magnet")
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger (refresh e))
        (is (= 1 (count (:hosted (refresh e)))) "Parasite hosted on Enigma")
        (run-on state "HQ")
        (core/reveal state :contestant (get-character state :hq 0))
        (prompt-select :contestant (first (:hosted (get-character state :rd 0))))
        (is (empty? (:hosted (refresh e))) "Enigma not hosting Parasite")
        (is (= 2 (count (:hosted (refresh m)))) "Parasites hosted on Magnet")
        (prompt-choice-partial :challenger "Jack")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (core/get-virus-counters state :challenger (first (:hosted (refresh m)))))
          "First parasite stays at 1 virus counter on revealed Magnet")
        (is (zero? (core/get-virus-counters state :challenger (second (:hosted (refresh m)))))
          "Second parasite does not gain a virus counter on hidden Magnet")
        (take-credits state :challenger)
        (core/hide state :contestant (refresh m))
        (take-credits state :contestant)
        (is (= 2 (core/get-virus-counters state :challenger (first (:hosted (refresh m)))))
          "First parasite gains a virus counter on hidden Magnet")
        (is (= 1 (core/get-virus-counters state :challenger (second (:hosted (refresh m)))))
          "Second parasite gains a virus counter on revealed Magnet")))))

(deftest mausolus
  ;; Mausolus - 3 adv tokens change the subroutines
  (do-game
    (new-game (default-contestant ["Mausolus"])
              (default-challenger [(qty "NetChip" 5)]))
    (play-from-hand state :contestant "Mausolus" "HQ")
    (let [mau (get-character state :hq 0)]
      (core/reveal state :contestant mau)
      (take-credits state :contestant)
      (run-on state :hq)
      (is (= 3 (:credit (get-contestant))) "contestant starts encounter with 3 crs")
      (is (zero? (count (:discard (get-challenger)))) "challenger starts encounter with no cards in heap")
      (is (zero? (:tag (get-challenger))) "challenger starts encounter with 0 tags")
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

(deftest masvingo
  (do-game
    (new-game (default-contestant ["Masvingo"])
              (default-challenger))
    (play-from-hand state :contestant "Masvingo" "HQ")
    (let [mas (get-character state :hq 0)]
      (is (zero? (get-counters (refresh mas) :advancement)) "Should place with 0 counter")
      (core/reveal state :contestant (refresh mas))
      (is (= 1 (get-counters (refresh mas) :advancement)) "Should reveal with 1 counter")
      (take-credits state :contestant)
      (run-on state :hq)
      (card-subroutine state :contestant mas 0)
      (is (not (:run @state)) "Run is ended"))))

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
  ;; Mind game - PSI redirect to different locale
  (do-game
    (new-game (default-contestant ["Mind Game"])
              (default-challenger))
    (play-from-hand state :contestant "Mind Game" "HQ")
    (take-credits state :contestant)
    (run-on state :hq)
    (let [mindgame (get-character state :hq 0)]
      (core/reveal state :contestant mindgame)
      (card-subroutine state :contestant mindgame 0))
    (prompt-choice :contestant "1 [Credits]")
    (prompt-choice :challenger "0 [Credits]")
    (is (= (set ["R&D" "Archives"]) (set (:choices (prompt-map :contestant)))) "Contestant cannot choose locale Challenger is on")
    (prompt-choice :contestant "Archives")
    (is (= [:archives] (get-in @state [:run :locale])) "Challenger now running on Archives")))

(deftest minelayer
  ;; Minelayer - Place a piece of Character in outermost position of Minelayer's locale at no cost
  (do-game
    (new-game (default-contestant ["Minelayer" "Fire Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Minelayer" "HQ")
    (take-credits state :contestant)
    (run-on state :hq)
    (core/reveal state :contestant (get-character state :hq 0))
    (is (= 6 (:credit (get-contestant))))
    (card-subroutine state :contestant (get-character state :hq 0) 0)
    (prompt-select :contestant (find-card "Fire Wall" (:hand (get-contestant))))
    (is (= 2 (count (get-in @state [:contestant :locales :hq :characters]))) "2 Character protecting HQ")
    (is (= 6 (:credit (get-contestant))) "Didn't pay 1 credit to place as second Character")))

(deftest mlinzi
  ;; Mlinzi - take X net damage or discard the top X+1 cards from the Stack
  (do-game
    (new-game (default-contestant ["Mlinzi"])
              (default-challenger [(qty "Sure Gamble" 3)]))
    (starting-hand state :challenger ["Sure Gamble"])
    (play-from-hand state :contestant "Mlinzi" "HQ")
    (take-credits state :contestant)
    (let [ml (get-character state :hq 0)]
      (run-on state "HQ")
      (core/reveal state :contestant ml)
      (card-subroutine state :contestant (refresh ml) 0)
      (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")
      (prompt-choice-partial :challenger "Take")
      (is (= 1 (count (:discard (get-challenger)))) "Challenger discarded 1 card")
      (is (empty? (:hand (get-challenger))) "Challenger discarded card from hand")
      (card-subroutine state :contestant (refresh ml) 0)
      (is (= 2 (count (:deck (get-challenger)))) "Challenger has 2 cards in stack")
      (prompt-choice-partial :challenger "Discard")
      (is (= 3 (count (:discard (get-challenger)))) "Challenger discarded 2 cards")
      (is (empty? (:deck (get-challenger))) "Challenger discarded card from stack"))))

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

(deftest nightdancer
  ;; Nightdancer - Challenger loses a click if able, contestant gains a click on next turn
  (do-game
    (new-game (default-contestant ["Nightdancer"])
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

(deftest oduduwa
  ;; Oduduwa - Gain 1 advancement token when encountered.
  ;; May placed x advancement tokens on another character where x is the number of counters on Oduduwa already.
  (do-game
    (new-game (default-contestant ["Oduduwa" "Enigma"])
              (default-challenger))
    (play-from-hand state :contestant "Oduduwa" "HQ")
    (play-from-hand state :contestant "Enigma" "R&D")
    (let [odu (get-character state :hq 0)
          eni (get-character state :rd 0)]
      (core/reveal state :contestant odu)
      (core/reveal state :contestant eni)
      (take-credits state :contestant)
      (run-on state :hq)
      (card-ability state :contestant (refresh odu) 0)
      (card-ability state :contestant (refresh odu) 1)
      (prompt-select :contestant (refresh eni))
      (is (= 1 (get-counters (refresh odu) :advancement)))
      (is (= 1 (get-counters (refresh eni) :advancement)))
      (run-jack-out state)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-on state :hq)
      (card-ability state :contestant (refresh odu) 0)
      (card-ability state :contestant (refresh odu) 1)
      (prompt-select :contestant (refresh eni))
      (is (= 2 (get-counters (refresh odu) :advancement)))
      (is (= 3 (get-counters (refresh eni) :advancement)))
      (run-jack-out state)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-on state :hq)
      (card-ability state :contestant (refresh odu) 0)
      (card-ability state :contestant (refresh odu) 1)
      (prompt-select :contestant (refresh eni))
      (is (= 3 (get-counters (refresh odu) :advancement)))
      (is (= 6 (get-counters (refresh eni) :advancement))))))

(deftest resistor
  ;; Resistor - Strength equal to Challenger tags, lose strength when Challenger removes a tag
  (do-game
    (new-game (default-contestant ["Resistor"])
              (default-challenger))
    (play-from-hand state :contestant "Resistor" "HQ")
    (let [resistor (get-character state :hq 0)]
      (core/reveal state :contestant resistor)
      (is (zero? (:current-strength (refresh resistor))) "No Challenger tags; 0 strength")
      (core/tag-challenger state :challenger 2)
      (is (= 2 (:tag (get-challenger))))
      (is (= 2 (:current-strength (refresh resistor))) "2 Challenger tags; 2 strength")
      (take-credits state :contestant)
      (core/remove-tag state :challenger 1)
      (is (= 1 (:current-strength (refresh resistor))) "Challenger removed 1 tag; down to 1 strength"))))

(deftest sadaka
  ;; Sadaka
  (testing "Sub 1 - Look at the top 3 cards of R&D, arrange those or shuffle R&D. You may draw 1 card"
    (do-game
      (new-game (default-contestant ["Sadaka" (qty "Enigma" 3)])
                (default-challenger))
      (starting-hand state :contestant ["Sadaka"])
      (play-from-hand state :contestant "Sadaka" "Archives")
      (let [sadaka (get-character state :archives 0)]
        (take-credits state :contestant)
        (run-on state "archives")
        (core/reveal state :contestant sadaka)
        (is (zero? (count (:hand (get-contestant)))) "Contestant starts with empty hand")
        (card-subroutine state :contestant (refresh sadaka) 0)
        (prompt-choice :contestant "Shuffle R&D")
        (prompt-choice :contestant "Yes")
        (is (= 1 (count (:hand (get-contestant)))) "Contestant draws a card")
        (card-subroutine state :contestant (refresh sadaka) 0)
        (prompt-choice :contestant "Shuffle R&D")
        (prompt-choice :contestant "No")
        (is (= 1 (count (:hand (get-contestant)))) "Contestant doesn't draw a card"))))
  (testing "Sub 2 - You may discard 1 card in HQ. If you do, discard 1 radicle. Discard Sadaka."
    (do-game
      (new-game (default-contestant [(qty "Sadaka" 2) (qty "Enigma" 3)])
                (default-challenger ["Bank Job"]))
      (play-from-hand state :contestant "Sadaka" "Archives")
      (play-from-hand state :contestant "Sadaka" "HQ")
      (let [sadaka (get-character state :archives 0)
            sadakaHQ (get-character state :hq 0)]
        (take-credits state :contestant)
        (play-from-hand state :challenger "Bank Job")
        (run-on state "archives")
        (core/reveal state :contestant sadaka)
        (is (= 3 (count (:hand (get-contestant)))) "Contestant starts with 3 cards in hand")
        (is (zero? (count (:discard (get-contestant)))) "Contestants starts with 0 cards in archives")
        (card-subroutine state :contestant (refresh sadaka) 1)
        (prompt-card :contestant (find-card "Enigma" (:hand (get-contestant))))
        (is (= 2 (count (:hand (get-contestant)))) "Contestant discards 1 card")
        (is (= 1 (count (:discard (get-contestant)))) "1 card discarded")
        (prompt-choice :contestant "Done")
        (is (= 2 (count (:discard (get-contestant)))) "Sadaka discarded")
        (run-jack-out state)
        (run-on state "archives")
        (core/reveal state :contestant sadakaHQ)
        (is (= 2 (count (:hand (get-contestant)))) "Contestant starts with 2 cards in hand")
        (is (= 2 (count (:discard (get-contestant)))) "Contestants starts with 2 cards in archives")
        (is (zero? (count (:discard (get-challenger)))) "Challenger starts with 0 cards in discard")
        (card-subroutine state :contestant (refresh sadakaHQ) 1)
        (prompt-card :contestant (find-card "Enigma" (:hand (get-contestant))))
        (is (= 1 (count (:hand (get-contestant)))) "Contestant discards 1 card")
        (is (= 3 (count (:discard (get-contestant)))) "1 card discarded")
        (prompt-select :contestant (get-radicle state 0))
        (is (= 1 (count (:discard (get-challenger)))) "Challenger radicle discarded")
        (is (= 4 (count (:discard (get-contestant)))) "sadakaHQ discarded")))))

(deftest sandman
  ;; Sandman - add an placed challenger card to the grip
  (do-game
    (new-game (default-contestant ["Sandman"])
              (default-challenger ["Inti" "Scrubber"]))
    (play-from-hand state :contestant "Sandman" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Inti")
    (play-from-hand state :challenger "Scrubber")
    (is (zero? (count (:hand (get-challenger)))) "Challenger's hand is empty")
    (run-on state "HQ")
    (let [sand (get-character state :hq 0)]
      (core/reveal state :contestant (refresh sand))
      (card-subroutine state :contestant (refresh sand) 0)
      (prompt-select :contestant (find-card "Inti" (get-in (get-challenger) [:rig :resource])))
      (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")
      (card-subroutine state :contestant (refresh sand) 0)
      (prompt-select :contestant (find-card "Scrubber" (get-in (get-challenger) [:rig :radicle])))
      (is (= 2 (count (:hand (get-challenger)))) "Challenger has 2 cards in hand")
      (card-subroutine state :contestant (refresh sand) 0)
      (is (empty? (:prompt (get-contestant))) "Sandman doesn't fire if no placed cards"))))

(deftest searchlight
  ;; Searchlight - Trace bace equal to advancement counters
  (do-game
    (new-game (default-contestant ["Searchlight"])
              (default-challenger))
    (play-from-hand state :contestant "Searchlight" "HQ")
    (let [searchlight (get-character state :hq 0)]
      (core/reveal state :contestant searchlight)
      (card-subroutine state :contestant (refresh searchlight) 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (zero? (:tag (get-challenger))) "Trace failed with 0 advancements")
      (advance state searchlight 1)
      (card-subroutine state :contestant (refresh searchlight) 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 1 (:tag (get-challenger))) "Trace succeeds with 1 advancement"))))

(deftest seidr-adaptive-barrier
  ;; Seidr Adaptive Barrier - +1 strength for every character protecting its locale
  (do-game
    (new-game (default-contestant ["Seidr Adaptive Barrier" (qty "Ice Wall" 2)])
              (default-challenger))
    (core/gain state :contestant :credit 10)
    (play-from-hand state :contestant "Seidr Adaptive Barrier" "HQ")
    (let [sab (get-character state :hq 0)]
      (core/reveal state :contestant sab)
      (is (= 3 (:current-strength (refresh sab))) "Seidr gained 1 strength for itself")
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (is (= 4 (:current-strength (refresh sab))) "+2 strength for 2 pieces of Character")
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (is (= 5 (:current-strength (refresh sab))) "+3 strength for 3 pieces of Character")
      (core/move-card state :contestant {:card (get-character state :hq 1) :locale "Archives"})
      (is (= 4 (:current-strength (refresh sab))) "+2 strength for 2 pieces of Character"))))

(deftest self-adapting-code-wall
  ;; Self-Adapting Code Wall
  (do-game
    (new-game (default-contestant ["Self-Adapting Code Wall" "Lag Time"])
              (default-challenger ["Ice Carver" "Parasite"]))
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

(deftest sherlock-1.0
  ;; Sherlock 1.0 - Trace to add an placed resource to the top of Challenger's Stack
  (do-game
    (new-game (default-contestant ["Sherlock 1.0"])
              (default-challenger [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Sherlock 1.0" "HQ")
    (take-credits state :contestant)
    (let [sherlock (get-character state :hq 0)]
      (play-from-hand state :challenger "Gordian Blade")
      (run-on state :hq)
      (core/reveal state :contestant sherlock)
      (card-subroutine state :contestant sherlock 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (prompt-select :contestant (get-resource state 0))
      (is (empty? (get-resource state)) "Gordian unplaced")
      (is (= "Gordian Blade" (:title (first (:deck (get-challenger))))) "Gordian on top of Stack"))))

(deftest sherlock-2.0
  ;; Sherlock 2.0 - Trace to add an placed resource to the bottom of Challenger's Stack
  (do-game
    (new-game (default-contestant [(qty "Sherlock 2.0" 1)])
              (default-challenger [(qty "Gordian Blade" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Sherlock 2.0" "HQ")
    (take-credits state :contestant)
    (let [sherlock (get-character state :hq 0)]
      (play-from-hand state :challenger "Gordian Blade")
      (run-on state :hq)
      (core/reveal state :contestant sherlock)
      (card-subroutine state :contestant sherlock 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (prompt-select :contestant (get-resource state 0))
      (is (empty? (get-resource state)) "Gordian unplaced")
      (is (= "Gordian Blade" (:title (last (:deck (get-challenger))))) "Gordian on bottom of Stack"))))

(deftest shiro
  ;; Shiro
  (testing "Full test"
    (do-game
      (new-game (default-contestant ["Shiro" "Caprcharacter Nisei"
                               "Quandary" "Jackson Howard"])
                (default-challenger ["R&D Interface"]))
      (starting-hand state :contestant ["Shiro"])
      (play-from-hand state :contestant "Shiro" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "R&D Interface")
      (let [shiro (get-character state :hq 0)]
        (run-on state :hq)
        (core/reveal state :contestant shiro)
        (card-subroutine state :contestant shiro 0)
        (prompt-card :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
        (prompt-card :contestant (find-card "Quandary" (:deck (get-contestant))))
        (prompt-card :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
        ;; try starting over
        (prompt-choice :contestant "Start over")
        (prompt-card :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
        (prompt-card :contestant (find-card "Quandary" (:deck (get-contestant))))
        (prompt-card :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant)))) ;this is the top card of R&D
        (prompt-choice :contestant "Done")
        (is (= "Caprcharacter Nisei" (:title (first (:deck (get-contestant))))))
        (is (= "Quandary" (:title (second (:deck (get-contestant))))))
        (is (= "Jackson Howard" (:title (second (rest (:deck (get-contestant)))))))
        (card-subroutine state :contestant shiro 1)
        (is (= (:cid (first (:deck (get-contestant))))
               (:cid (:card (first (:prompt (get-challenger)))))) "Access the top card of R&D")
        (prompt-choice :challenger "No action")
        (is (= (:cid (second (:deck (get-contestant))))
               (:cid (:card (first (:prompt (get-challenger)))))) "Access another card due to R&D Interface"))))
  (testing "with Mwanza City Grid, should access additional 3 cards"
    (do-game
      (new-game (default-contestant ["Shiro" "Mwanza City Grid"
                               (qty "Ice Wall" 10)])
                (default-challenger ["R&D Interface"]))
      (starting-hand state :contestant ["Shiro" "Mwanza City Grid"])
      (play-from-hand state :contestant "Mwanza City Grid" "R&D")
      (play-from-hand state :contestant "Shiro" "R&D")
      (take-credits state :contestant)
      (core/gain state :contestant :credit 100)
      (play-from-hand state :challenger "R&D Interface")
      (let [shiro (get-character state :rd 0)
            mwanza (get-content state :rd 0)]
        (run-on state :rd)
        (core/reveal state :contestant shiro)
        (core/reveal state :contestant mwanza)
        (let [credits (:credit (get-contestant))]
          (card-subroutine state :contestant shiro 1)
          (is (= 3 (-> @state :run :access-bonus)) "Should access an additional 3 cards")
          (dotimes [_ 5]
            (prompt-choice :challenger "No action"))
          (run-jack-out state)
          (is (= (+ credits 10) (:credit (get-contestant))) "Contestant should gain 10 credits from accessing 5 cards total"))))))

(deftest snowflake
  ;; Snowflake - Win a psi game to end the run
  (do-game
    (new-game (default-contestant ["Snowflake"])
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

(deftest special-offer
  ;; Special Offer discards itself and updates the run position
  (do-game
    (new-game (default-contestant ["Ice Wall" "Special Offer"])
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

(deftest sand-storm
  ;; Sand Storm should not end the run if protecting an otherwise empty/naked locale
  (do-game
    (new-game (default-contestant ["Sand Storm" "PAD Campaign"])
              (default-challenger))
    (play-from-hand state :contestant "Sand Storm" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (take-credits state :contestant)
    (run-on state "Locale 1")
    (let [sand-storm (get-character state :party1 0)]
      (core/reveal state :contestant sand-storm)
      (card-subroutine state :contestant sand-storm 0)
      (prompt-choice :contestant "Locale 2")
      (is (=  (first (get-in @state [:run :locale])) :party2) "Is running on locale 2"))))

(deftest surveyor
  ;; Surveyor character strength
  (do-game
    (new-game (default-contestant [(qty "Surveyor" 1) (qty "Ice Wall" 2)])
              (default-challenger))
    (core/gain state :contestant :credit 10)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :contestant "Surveyor" "HQ")
    (let [surv (get-character state :hq 0)]
      (core/reveal state :contestant surv)
      (is (= 2 (:current-strength (refresh surv))) "Surveyor has 2 strength for itself")
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (is (= 4 (:current-strength (refresh surv))) "Surveyor has 4 strength for 2 pieces of Character")
      (play-from-hand state :contestant "Ice Wall" "HQ")
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

(deftest tithonium
  ;; Forfeit option as reveal cost, can have hosted condition counters
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Hostile Takeover" "Tithonium" "Patch"])
                (default-challenger ["Pawn" "Wasteland"]))
      (core/gain state :contestant :click 10)
      (play-from-hand state :contestant "Hostile Takeover" "New party")
      (play-from-hand state :contestant "Tithonium" "HQ")
      (let [ht (get-content state :party1 0)
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
        (is (zero? (count (:scored (get-contestant)))) "Agenda forfeited")
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
              wast (get-radicle state 0)]
          (card-ability state :challenger (refresh pawn) 0)
          (prompt-select :challenger (refresh ti))
          (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
          (core/hide state :contestant (refresh ti))
          (is (= 2 (count (:hosted (refresh ti)))) "2 cards on Tithonium")
          (run-on state "HQ")
          (card-subroutine state :contestant ti 2)
          (prompt-select :contestant (refresh wast))
          (is (= 1 (count (:discard (get-challenger)))) "1 card discarded")
          (card-subroutine state :contestant ti 1)
          (is (not (:run @state)) "Run ended")))))
  (testing "Do not prompt for alt cost #2734"
    (do-game
      (new-game (default-contestant ["Hostile Takeover" "Oversight AI" "Tithonium"])
                (default-challenger))
      (play-from-hand state :contestant "Hostile Takeover" "New party")
      (play-from-hand state :contestant "Tithonium" "R&D")
      (let [ht (get-content state :party1 0)
            ti (get-character state :rd 0)]
        (score-agenda state :contestant ht)
        (play-from-hand state :contestant "Oversight AI")
        (prompt-select :contestant ti)
        (is (:revealed (refresh ti)))
        (is (= "Oversight AI" (:title (first (:hosted (refresh ti)))))
            "Tithonium hosting OAI as a condition")))))

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

(deftest troll
  ;; Troll
  (testing "Giving the challenger a choice on successful trace shouldn't make challenger pay trace first. #5335"
    (do-game
      (new-game (default-contestant ["Troll"])
                (default-challenger))
      (play-from-hand state :contestant "Troll" "HQ")
      (take-credits state :contestant)
      (let [troll (get-character state :hq 0)]
        (core/reveal state :contestant troll)
        (run-on state "HQ")
        (card-ability state :contestant troll 0)
        (is (= :waiting (-> (get-challenger) :prompt first :prompt-type)) "Challenger waits for Contestant to boost first")
        (prompt-choice :contestant 0)
        (prompt-choice :challenger 0)
        (prompt-choice :challenger "End the run")
        (is (not (:run @state)) "Run is ended")))))

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

(deftest wendigo
  ;; Montestanth character gain and lose subtypes from normal advancements and placed advancements
  (do-game
    (new-game (default-contestant ["Wendigo" "Shipment from SanSan"
                             "Superior Cyberwalls"])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Superior Cyberwalls" "New party")
    (let [sc (get-content state :party1 0)]
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

(deftest wraparound
  ;; Wraparound - Strength boosted when no fracter is placed
  (do-game
    (new-game (default-contestant ["Wraparound"])
              (default-challenger ["Corroder"]))
    (play-from-hand state :contestant "Wraparound" "HQ")
    (let [wrap (get-character state :hq 0)]
      (core/reveal state :contestant wrap)
      (is (= 7 (:current-strength (refresh wrap)))
          "Wraparound +7 strength with no fracter in play")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Corroder")
      (is (zero? (:current-strength (refresh wrap)))
          "Wraparound 0 strength after Corroder placed"))))
