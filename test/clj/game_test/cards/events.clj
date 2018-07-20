(ns game-test.cards.events
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "events"))

(deftest account-siphon
  ;; Account Siphon
  (testing "Use ability"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Account Siphon" 3)]))
      (take-credits state :contestant)
      (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits")
      ;; play Account Siphon, use ability
      (play-run-event state "Account Siphon" :hq)
      (click-prompt state :challenger "Replacement effect")
      (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags")
      (is (= 15 (:credit (get-challenger))) "Challenger gained 10 credits")
      (is (= 3 (:credit (get-contestant))) "Contestant lost 5 credits")))
  (testing "Access"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Account Siphon" 3)]))
      (take-credits state :contestant) ; pass to challenger's turn by taking credits
      (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits")
      ;; play another Siphon, do not use ability
      (play-run-event state "Account Siphon" :hq)
      (click-prompt state :challenger "Access cards")
      (is (zero? (:tag (get-challenger))) "Challenger did not take any tags")
      (is (= 5 (:credit (get-challenger))) "Challenger did not gain any credits")
      (is (= 8 (:credit (get-contestant))) "Contestant did not lose any credits")))
  (testing "New Angeles City Hall interaction"
    ;; Account Siphon - Access
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Account Siphon"
                                 "New Angeles City Hall"]))
      (core/gain state :contestant :bad-publicity 1)
      (is (= 1 (:bad-publicity (get-contestant))) "Contestant has 1 bad publicity")
      (core/lose state :challenger :credit 1)
      (is (= 4 (:credit (get-challenger))) "Challenger has 4 credits")
      (take-credits state :contestant) ; pass to challenger's turn by taking credits
      (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits")
      (play-from-hand state :challenger "New Angeles City Hall")
      (is (= 3 (:credit (get-challenger))) "Challenger has 3 credits")
      (let [nach (get-radicle state 0)]
        (play-run-event state (first (get-in @state [:challenger :hand])) :hq)
        (click-prompt state :challenger "Replacement effect")
        (is (= 4 (:credit (get-challenger))) "Challenger still has 4 credits due to BP")
        (card-ability state :challenger nach 0)
        (is (= 2 (:credit (get-challenger))) "Challenger has 2 credits left")
        (card-ability state :challenger nach 0)
        (is (zero? (:credit (get-challenger))) "Challenger has no credits left")
        (click-prompt state :challenger "Done"))
      (is (zero? (:tag (get-challenger))) "Challenger did not take any tags")
      (is (= 10 (:credit (get-challenger))) "Challenger gained 10 credits")
      (is (= 3 (:credit (get-contestant))) "Contestant lost 5 credits"))))

(deftest amped-up
  ;; Amped Up - Gain 3 clicks and take 1 unpreventable brain damage
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Amped Up"
                               "Feedback Filter"
                               (qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Feedback Filter")
    (play-from-hand state :challenger "Amped Up")
    (is (empty? (:prompt (get-challenger)))
        "Feedback Filter brain damage prevention opportunity not given")
    (is (= 5 (:click (get-challenger))) "Challenger gained 2 clicks from Amped Up")
    (is (= 2 (count (:discard (get-challenger)))) "Challenger discarded 1 card from damage")
    (is (= 4 (core/hand-size state :challenger)) "Challenger handsize decreased by 1")
    (is (= 1 (:brain-damage (get-challenger))) "Took 1 brain damage")))

(deftest ^{:card-title "another-day,-another-paycheck"}
  another-day-another-paycheck
  ;; Another Day, Another Paycheck
  (do-game
    (new-game
      (default-contestant [(qty "Project Atlas" 3)])
      (default-challenger ["Street Peddler" (qty "Another Day, Another Paycheck" 2)]))
    (starting-hand state :challenger ["Street Peddler" "Another Day, Another Paycheck"])
    (play-from-hand state :contestant "Project Atlas" "New party")
    (score-agenda state :contestant (get-content state :party1 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Street Peddler")
    (run-empty-locale state :hq)
    (click-prompt state :challenger "Steal")
    (is (= 5 (:credit (get-challenger))) "No trace, no gain")
    (play-from-hand state :challenger "Another Day, Another Paycheck")
    (run-empty-locale state :hq)
    (click-prompt state :challenger "Steal")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "1")
    ;; 4 credits after trace, gain 6
    (is (= 10 (:credit (get-challenger))) "Challenger gained 6 credits")))

(deftest apocalypse
  ;; Apocalypse
  (testing "Ensure MU is correct and no duplicate cards in heap"
    (do-game
      (new-game (default-contestant [(qty "Launch Campaign" 2) "Ice Wall"])
                (default-challenger ["Scheherazade" "Corroder" "Hivemind" (qty "Apocalypse" 2)]))
      (play-from-hand state :contestant "Ice Wall" "New party")
      (play-from-hand state :contestant "Launch Campaign" "New party")
      (play-from-hand state :contestant "Launch Campaign" "New party")
      (take-credits state :contestant)
      (core/gain state :challenger :click 3)
      (core/gain state :challenger :credit 2)
      (play-from-hand state :challenger "Scheherazade")
      (let [scheherazade (get-resource state 0)]
        (card-ability state :challenger scheherazade 0)
        (click-card state :challenger (find-card "Corroder" (:hand (get-challenger))))
        (is (= 3 (core/available-mu state)) "Memory at 3 (-1 from Corroder)"))
      (play-from-hand state :challenger "Hivemind")
      (is (= 1 (core/available-mu state)) "Memory at 1 (-1 from Corroder, -2 from Hivemind)")
      (run-empty-locale state "Archives")
      (run-empty-locale state "R&D")
      (run-empty-locale state "HQ")
      (play-from-hand state :challenger "Apocalypse")
      (is (zero? (count (core/all-placed state :contestant))) "All placed Contestant cards discarded")
      (is (= 3 (count (:discard (get-contestant)))) "3 Contestant cards in Archives")
      (is (zero? (count (core/all-active-placed state :challenger))) "No active placed challenger cards")
      (let [facedowns (filter :facedown (core/all-placed state :challenger))
            scheherazade (find-card "Scheherazade" facedowns)
            corroder (find-card "Corroder" facedowns)
            hivemind (find-card "Hivemind" facedowns)]
        (is scheherazade "Scheherazade facedown")
        (is corroder "Corroder facedown")
        (is hivemind "Hivemind facedown")
        (is (= 3 (count facedowns)) "No other cards facedown")
        (is (= corroder (first (:hosted scheherazade))) "Corroder is still hosted on Scheherazade")
        (is (= 1 (get-counters hivemind :virus)) "Hivemind still has a virus counters"))
      (is (find-card "Apocalypse" (:discard (get-challenger))) "Apocalypse is in the heap")
      (is (= 1 (count (:discard (get-challenger)))) "Only Apocalypse is in the heap")
      (is (= 4 (core/available-mu state)) "Memory back to 4")))
  (testing "with Full Immersion - no duplicate cards in heap #2606"
    (do-game
      (new-game
        (default-contestant ["Full Immersion RecStudio" "Sandburg"
                       "Oaktown Renovation"])
        (default-challenger  ["Apocalypse"]))
      (play-from-hand state :contestant "Full Immersion RecStudio" "New party")
      (let [fir (get-content state :party1 0)]
        (core/reveal state :contestant fir)
        (card-ability state :contestant fir 0)
        (click-card state :contestant (find-card "Sandburg" (:hand (get-contestant))))
        (card-ability state :contestant fir 0)
        (click-card state :contestant (find-card "Oaktown Renovation" (:hand (get-contestant))))
        (take-credits state :contestant)
        (run-empty-locale state "Archives")
        (run-empty-locale state "R&D")
        (run-empty-locale state "HQ")
        (play-from-hand state :challenger "Apocalypse")
        (is (zero? (count (core/all-placed state :contestant))) "All placed Contestant cards discarded")
        (is (= 3 (count (:discard (get-contestant)))) "3 Contestant cards in Archives")
        (is (= 1 (count (:discard (get-challenger)))) "Only Apocalypse is in the heap"))))
  (testing "with Hostile Infrastructure - should take damage equal to 2x cards on the table"
    (do-game
      (new-game
        (default-contestant [(qty "Hostile Infrastructure" 2) (qty "Ice Wall" 2)])
        (default-challenger ["Apocalypse" (qty "Sure Gamble" 9)]))
      (core/gain state :contestant :click 1)
      (play-from-hand state :contestant "Hostile Infrastructure" "New party")
      (play-from-hand state :contestant "Ice Wall" "New party")
      (play-from-hand state :contestant "Ice Wall" "New party")
      (play-from-hand state :contestant "Hostile Infrastructure" "New party")
      (core/reveal state :contestant (get-content state :party1 0) {:ignore-cost true})
      (core/reveal state :contestant (get-content state :party4 0) {:ignore-cost true})
      (take-credits state :contestant)
      (core/draw state :challenger 5)
      (is (= 10 (count (:hand (get-challenger)))) "Challenger has 9 cards in hand")
      (run-empty-locale state "Archives")
      (run-empty-locale state "R&D")
      (run-empty-locale state "HQ")
      (play-from-hand state :challenger "Apocalypse")
      (is (zero? (count (core/all-placed state :contestant))) "All placed Contestant cards discarded")
      (is (= 4 (count (:discard (get-contestant)))) "4 Contestant cards in Archives")
      (is (= 1 (count (:hand (get-challenger)))) "Challenger has one card in hand")
      (is (= 9 (count (:discard (get-challenger)))) "There are 9 cards in heap")))
  (testing "Turn Challenger cards facedown and reduce memory and hand-size gains"
    (do-game
      (new-game (default-contestant [(qty "Launch Campaign" 2) "Ice Wall"])
                (default-challenger ["Logos" "Apocalypse" (qty "Origami" 2)]))
      (play-from-hand state :contestant "Ice Wall" "New party")
      (play-from-hand state :contestant "Launch Campaign" "New party")
      (play-from-hand state :contestant "Launch Campaign" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Logos")
      (is (= 1 (get-in (get-challenger) [:hand-size :mod])) "Hand-size increased from Logos")
      (is (= 5 (core/available-mu state)) "Memory increased from Logos")
      (play-from-hand state :challenger "Origami")
      (play-from-hand state :challenger "Origami")
      (is (= 5 (get-in (get-challenger) [:hand-size :mod])) "Hand-size increased from Logos and Origami")
      (is (= 3 (core/available-mu state)) "Memory decreased from Origamis")
      (core/gain state :challenger :click 3 :credit 2)
      (run-empty-locale state "Archives")
      (run-empty-locale state "R&D")
      (run-empty-locale state "HQ")
      (play-from-hand state :challenger "Apocalypse")
      (is (zero? (count (core/all-placed state :contestant))) "All placed Contestant cards discarded")
      (is (= 3 (count (:discard (get-contestant)))) "3 Contestant cards in Archives")
      (let [logos (find-card "Logos" (get-in (get-challenger) [:rig :facedown]))]
        (is (:facedown (refresh logos)) "Logos is facedown")
        (is (zero? (get-in (get-challenger) [:hand-size :mod])) "Hand-size reset with Logos and Origami facedown")
        (is (= 4 (core/available-mu state)) "Memory reset with Logos and Origami facedown"))))
(testing "Turn Challenger cards facedown without firing their discard effects"
  (do-game
    (new-game (default-contestant [(qty "Launch Campaign" 2) "Ice Wall"])
              (default-challenger [(qty "Tri-maf Contact" 3) (qty "Apocalypse" 3)]))
    (play-from-hand state :contestant "Ice Wall" "New party")
    (play-from-hand state :contestant "Launch Campaign" "New party")
    (play-from-hand state :contestant "Launch Campaign" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Tri-maf Contact")
    (core/gain state :challenger :click 2)
    (run-empty-locale state "Archives")
    (run-empty-locale state "R&D")
    (run-empty-locale state "HQ")
    (play-from-hand state :challenger "Apocalypse")
    (is (zero? (count (core/all-placed state :contestant))) "All placed Contestant cards discarded")
    (is (= 3 (count (:discard (get-contestant)))) "3 Contestant cards in Archives")
    (let [tmc (get-challenger-facedown state 0)]
      (is (:facedown (refresh tmc)) "Tri-maf Contact is facedown")
      (is (= 3 (count (:hand (get-challenger))))
          "No meat damage dealt by Tri-maf's leave play effect")
      (core/discard state :challenger tmc)
      (is (= 3 (count (:hand (get-challenger))))
          "No meat damage dealt by discarding facedown Tri-maf")))))

(deftest because-i-can
  ;; make a successful run on a party to shuffle its contents into R&D
  (do-game
    (new-game (default-contestant ["Sand Storm" "PAD Campaign" "Project Atlas" (qty "Shell Contestantoration" 2)])
              (default-challenger [(qty "Because I Can" 2)]))
    (play-from-hand state :contestant "Shell Contestantoration" "New party")
    (play-from-hand state :contestant "Shell Contestantoration" "Locale 1")
    (play-from-hand state :contestant "Project Atlas" "Locale 1")
    (take-credits state :contestant)
    (let [n (count (get-in @state [:contestant :deck]))]
      (play-from-hand state :challenger "Because I Can")
      (click-prompt state :challenger "Locale 1")
      (is (= 3 (count (get-in @state [:contestant :locales :party1 :content])))
          "3 cards in locale 1 before successful run")
      (run-successful state)
      (click-prompt state :challenger "Replacement effect")
      (is (= (+ n 3) (count (get-in @state [:contestant :deck]))) "3 cards were shuffled into R&D")
      (is (zero? (count (get-in @state [:contestant :locales :party1 :content]))) "No cards left in locale 1"))
    (take-credits state :challenger)
    (play-from-hand state :contestant "Sand Storm" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (take-credits state :contestant)
    (let [n (count (get-in @state [:contestant :deck]))
          sand-storm (get-character state :party2 0)]
      (play-from-hand state :challenger "Because I Can")
      (click-prompt state :challenger "Locale 2")
      (core/reveal state :contestant sand-storm)
      (is (= :party2 (first (get-in @state [:run :locale]))))
      (card-subroutine state :contestant sand-storm 0)
      (click-prompt state :contestant "Locale 3")
      (is (= :party3 (first (get-in @state [:run :locale]))))
      (is (= 1 (count (get-in @state [:contestant :locales :party3 :content]))) "1 cards in locale 3 before successful run")
      (run-successful state)
      (click-prompt state :challenger "Replacement effect")
      (is (= (+ n 1) (count (get-in @state [:contestant :deck]))) "1 card was shuffled into R&D")
      (is (zero? (count (get-in @state [:contestant :locales :party3 :content]))) "No cards left in locale 3"))))

(deftest black-hat
  ;; Black Hat
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Hedge Fund" 10)])
                (default-challenger [(qty "Black Hat" 3)]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Black Hat")
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "4")
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :challenger "Card from deck")
      (click-prompt state :challenger "No action")
      (click-prompt state :challenger "Card from deck")
      (click-prompt state :challenger "No action")
      (click-prompt state :challenger "Card from deck")))
  (testing "Kitsune interaction"
    (do-game
     (new-game (default-contestant [(qty "Kitsune" 10)])
               (default-challenger [(qty "Black Hat" 3)]))
      (starting-hand state :contestant ["Kitsune" "Kitsune" "Kitsune" "Kitsune" "Kitsune"])
      (play-from-hand state :contestant "Kitsune" "R&D")
      (let [kitsune (get-character state :rd 0)]
        (core/reveal state :contestant kitsune)
        (take-credits state :contestant)
        (core/gain state :challenger :credit 10)
        (play-from-hand state :challenger "Black Hat")
        (click-prompt state  :contestant "0")
        (click-prompt state :challenger "4")
        (run-on state :rd)
        (card-subroutine state :contestant kitsune 0)
        (click-card state :contestant (find-card "Kitsune" (:hand (get-contestant))))
        (click-prompt state :challenger "No action")
        (click-prompt state :challenger "Card from hand")
        (click-prompt state :challenger "No action")
        (click-prompt state :challenger "Card from hand")
        (click-prompt state :challenger "No action")))))

(deftest blackmail
  ;; Prevent revealing of character for one run
  (testing "Basic test"
    (do-game
      (new-game
        (default-contestant [(qty "Ice Wall" 3)])
        (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
      (is (= 1 (get-in @state [:contestant :bad-publicity])) "Contestant has 1 bad-publicity")
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Blackmail")
      (click-prompt state :challenger "HQ")
      (let [iwall1 (get-character state :hq 0)
            iwall2 (get-character state :hq 1)]
        (core/reveal state :contestant iwall1)
        (is (not (:revealed (refresh iwall1))) "First Ice Wall is not revealed")
        (run-continue state)
        (core/reveal state :contestant iwall2)
        (is (not (:revealed (refresh iwall2))) "Second Ice Wall is not revealed")
        (core/jack-out state :challenger nil)
        ;; Do another run, where the character should reveal
        (run-on state "HQ")
        (core/reveal state :contestant iwall1)
        (is (:revealed (refresh iwall1)) "First Ice Wall is revealed"))))
  (testing "Regression test for a revealed tmi breaking game state on a blackmail run"
    (do-game
      (new-game (default-contestant [(qty "TMI" 3)])
                (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
      (is (= 1 (get-in @state [:contestant :bad-publicity])) "Contestant has 1 bad-publicity")
      (play-from-hand state :contestant "TMI" "HQ")
      (let [tmi (get-character state :hq 0)]
        (core/reveal state :contestant tmi)
        (click-prompt state :contestant "0")
        (click-prompt state :challenger "0")
        (is (:revealed (refresh tmi)) "TMI is revealed")
        (take-credits state :contestant)
        (play-from-hand state :challenger "Blackmail")
        (click-prompt state :challenger "HQ")
        (run-continue state)
        (run-jack-out state)
        (run-on state "Archives")))))

(deftest by-any-means
  ;; By Any Means
  (testing "Full test"
    (do-game
     (new-game (default-contestant ["Hedge Fund" "Ice Wall" "Paper Trail" "PAD Campaign"
                              "Project Junebug"])
               (default-challenger ["By Any Means" (qty "Sure Gamble" 5)]))
     (take-credits state :contestant)
     (run-empty-locale state "Archives")
     ; (play-from-hand state :challenger "By Any Means")
     (is (= 3 (:click (get-challenger))) "Card not played, priority restriction")
     (take-credits state :challenger)
     (starting-hand state :contestant ["Paper Trail" "Hedge Fund" "PAD Campaign" "Project Junebug"])
     (play-from-hand state :contestant "Paper Trail" "New party")
     (play-from-hand state :contestant "PAD Campaign" "New party")
     (play-from-hand state :contestant "Project Junebug" "New party")
     (core/add-counter state :contestant (get-content state :party3 0) :advancement 2)
     (take-credits state :contestant)
     (core/gain state :challenger :click 2)
     (core/draw state :challenger)
     (play-from-hand state :challenger "By Any Means")
     (run-empty-locale state "HQ")
     (is (= 1 (count (:discard (get-contestant)))) "Operation was discarded")
     (is (= 4 (count (:hand (get-challenger)))) "Took 1 meat damage")
     (run-empty-locale state "R&D")
     (is (= 2 (count (:discard (get-contestant)))) "Character was discarded")
     (is (= 3 (count (:hand (get-challenger)))) "Took 1 meat damage")
     (run-empty-locale state "Locale 1")
     (is (= 3 (count (:discard (get-contestant)))) "Agenda was discarded")
     (is (= 2 (count (:hand (get-challenger)))) "Took 1 meat damage")
     (run-empty-locale state "Locale 2")
     (is (= 4 (count (:discard (get-contestant)))) "Discardable was discarded")
     (is (= 1 (count (:hand (get-challenger)))) "Took 1 meat damage")
     (run-empty-locale state "Locale 3")
     (is (= 5 (count (:discard (get-contestant)))) "Ambush was discarded")
     (is (zero? (count (:hand (get-challenger)))) "Took 1 meat damage")))
  (testing "vs Controlling the Message"
    (do-game
      (new-game (make-deck "NBN: Controlling the Message" ["Paper Trail"])
                (default-challenger [(qty "By Any Means" 2)]))
      (play-from-hand state :contestant "Paper Trail" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "By Any Means")
      (run-empty-locale state "Locale 1")
      (click-prompt state :contestant "No") ;; Don't trigger CTM trace
      (is (empty? (:prompt (get-challenger))) "No prompt to steal since agenda was discarded")
      (is (= 1 (count (:discard (get-contestant)))) "Agenda was discarded")
      (is (zero? (count (:hand (get-challenger)))) "Took 1 meat damage")))
  (testing "alongside Film Critic: should get the option to trigger either"
    (do-game
      (new-game (default-contestant [(qty "Hostile Takeover" 2)])
                (default-challenger ["By Any Means" "Film Critic" (qty "Sure Gamble" 2)]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "By Any Means")
      (play-from-hand state :challenger "Film Critic")
      (is (= 1 (count (:discard (get-challenger)))) "By Any Means has been played")
      (run-empty-locale state "HQ")
      (is (= #{"Film Critic" "By Any Means"}
             (->> (get-challenger) :prompt first :choices (into #{}))) "A choice of which to trigger first")
      (click-prompt state :challenger "Film Critic")
      (click-prompt state :challenger "No")
      (is (= 1 (count (:discard (get-contestant)))) "Agenda was discarded")
      (is (= 1 (count (:hand (get-challenger)))) "Took 1 meat damage")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (core/move state :challenger (find-card "By Any Means" (:discard (get-challenger))) :hand)
      (play-from-hand state :challenger "By Any Means")
      (run-empty-locale state "HQ")
      (is (= #{"Film Critic" "By Any Means"}
             (->> (get-challenger) :prompt first :choices (into #{}))) "A choice of which to trigger first")
      (click-prompt state :challenger "By Any Means")
      (is (nil? (->> (get-challenger) :prompt first :choices)) "By Any Means discards with no prompt")
      (is (= 2 (count (:discard (get-contestant)))) "Agenda was discarded")
      (is (zero? (count (:hand (get-challenger)))) "Took 1 meat damage"))))

(deftest careful-planning
  ;; Careful Planning - Prevent card in/protecting party locale from being revealed this turn
  (do-game
    (new-game (default-contestant ["PAD Campaign" (qty "Vanilla" 2)])
              (default-challenger [(qty "Careful Planning" 2)]))
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "Vanilla" "HQ")
    (play-from-hand state :contestant "Vanilla" "Locale 1")
    (take-credits state :contestant)
    (let [pad (get-content state :party1 0)
          v1 (get-character state :hq 0)
          v2 (get-character state :party1 0)]
      (play-from-hand state :challenger "Careful Planning")
      (click-card state :challenger v1)
      (is (:prompt (get-challenger)) "Can't target card in central locale")
      (click-card state :challenger v2)
      (core/reveal state :contestant v2)
      (is (not (:revealed (refresh v2))) "Prevented party Character from revealing")
      (take-credits state :challenger)
      (core/reveal state :contestant (refresh v2))
      (is (:revealed (refresh v2)) "Reveal prevention of Character ended")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Careful Planning")
      (click-card state :challenger pad)
      (core/reveal state :contestant pad)
      (is (not (:revealed (refresh pad))) "Prevented party locale contents from revealing")
      (take-credits state :challenger)
      (core/reveal state :contestant (refresh pad))
      (is (:revealed (refresh pad)) "Reveal prevention of site ended"))))

(deftest cbi-raid
  ;; CBI Raid - Full test
  (do-game
    (new-game (default-contestant ["Caprcharacter Nisei" "Adonis Campaign" "Quandary"
                             "Jackson Howard" "Global Food Initiative"])
              (default-challenger ["CBI Raid"]))
    (take-credits state :contestant)
    (is (= 5 (count (:hand (get-contestant)))))
    (play-from-hand state :challenger "CBI Raid")
    (is (= :hq (get-in @state [:run :locale 0])))
    (run-successful state)
    (click-prompt state :contestant (find-card "Caprcharacter Nisei" (:hand (get-contestant))))
    (click-prompt state :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
    (click-prompt state :contestant (find-card "Quandary" (:hand (get-contestant))))
    (click-prompt state :contestant (find-card "Jackson Howard" (:hand (get-contestant))))
    (click-prompt state :contestant (find-card "Global Food Initiative" (:hand (get-contestant))))
    ;; try starting over
    (click-prompt state :contestant "Start over")
    (click-prompt state :contestant (find-card "Global Food Initiative" (:hand (get-contestant))))
    (click-prompt state :contestant (find-card "Jackson Howard" (:hand (get-contestant))))
    (click-prompt state :contestant (find-card "Quandary" (:hand (get-contestant))))
    (click-prompt state :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
    (click-prompt state :contestant (find-card "Caprcharacter Nisei" (:hand (get-contestant)))) ;this is the top card of R&D
    (click-prompt state :contestant "Done")
    (is (zero? (count (:hand (get-contestant)))))
    (is (= 5 (count (:deck (get-contestant)))))
    (is (= "Caprcharacter Nisei" (:title (first (:deck (get-contestant))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-contestant))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-contestant)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-contestant))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-contestant)))))))))))

(deftest cold-read
  ;; Make a run, and place 4 on this card, which you may use only during this run.
  ;; When this run ends, discard 1 resource (cannot be prevented) used during this run.
  (do-game
    (new-game (default-contestant [(qty "Blacklist" 3)])
              (default-challenger ["Imp" (qty "Cold Read" 2)]))
    (play-from-hand state :contestant "Blacklist" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Imp")
    (let [bl (get-content state :party1 0)]
      (play-from-hand state :challenger "Cold Read")
      (click-prompt state :challenger "HQ")
      (is (= 4 (get-counters (find-card "Cold Read" (get-in @state [:challenger :play-area])) :recurring)) "Cold Read has 4 counters")
      (run-successful state)
      (click-prompt state :challenger "[Imp]: Discard card")
      (click-card state :challenger (get-resource state 0))
      (is (= 2 (count (:discard (get-challenger)))) "Imp and Cold Read in discard")
      ; Cold Read works when Blacklist revealed - #2378
      (core/reveal state :contestant bl)
      (play-from-hand state :challenger "Cold Read")
      (click-prompt state :challenger "HQ")
      (is (= 4 (get-counters (find-card "Cold Read" (get-in @state [:challenger :play-area])) :recurring)) "Cold Read has 4 counters")
      (run-successful state))))

(deftest ^{:card-title "compile"}
  compile-test
  ;; Compile - Make a run, and place a resource for free which is shuffled back into stack
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Compile" "Gordian Blade"]))
      (starting-hand state :challenger ["Compile"])
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Compile")
      (click-prompt state :challenger "Archives")
      (click-prompt state :challenger "OK")  ; notification that Compile must be clicked to place
      (let [compile-card (first (get-in @state [:challenger :play-area]))]
        (card-ability state :challenger compile-card 0)
        (click-prompt state :challenger "Stack")
        (click-prompt state :challenger (find-card "Gordian Blade" (:deck (get-challenger))))
        (is (:placed (get-resource state 0)) "Gordian Blade should be placed"))
      (let [deck (count (:deck (get-challenger)))]
        (run-jack-out state)
        (is (= (+ 1 deck) (count (:deck (get-challenger)))) "Gordian Blade should be back in stack")
        (is (nil? (get-resource state 0))))))
  (testing "with Self-modifying Code, neither SMC nor other card should be shuffled back in"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Compile" "Clone Chip"
                                 (qty "Self-modifying Code" 3)]))
      (starting-hand state :challenger ["Compile" "Clone Chip"])
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Clone Chip")
      (play-from-hand state :challenger "Compile")
      (click-prompt state :challenger "Archives")
      (click-prompt state :challenger "OK")  ; notification that Compile must be clicked to place
      (let [compile-card (first (get-in @state [:challenger :play-area]))
            clone-chip (get-hazard state 0)]
        (card-ability state :challenger compile-card 0)
        (click-prompt state :challenger "Stack")
        (click-prompt state :challenger (find-card "Self-modifying Code" (:deck (get-challenger))))
        (let [smc (get-resource state 0)]
          (card-ability state :challenger smc 0)
          (click-prompt state :challenger (find-card "Self-modifying Code" (:deck (get-challenger))))
          (card-ability state :challenger clone-chip 0)
          (click-card state :challenger (find-card "Self-modifying Code" (:discard (get-challenger))))))
      (let [deck (count (:deck (get-challenger)))]
        (run-jack-out state)
        (is (= deck (count (:deck (get-challenger)))) "No card was shuffled back into the stack"))))
  (testing "vs ending the run via contestant action. #3639"
      (do-game
        (new-game (default-contestant ["Ice Wall"])
                  (default-challenger ["Compile" "Gordian Blade"]))
        (starting-hand state :challenger ["Compile"])
        (play-from-hand state :contestant "Ice Wall" "Archives")
        (let [iw (get-character state :archives 0)]
          (core/reveal state :contestant iw)
          (take-credits state :contestant)
          (core/gain state :challenger :credit 10)
          (play-from-hand state :challenger "Compile")
          (click-prompt state :challenger "Archives")
          (click-prompt state :challenger "OK")  ; notification that Compile must be clicked to place
          (let [compile-card (first (get-in @state [:challenger :play-area]))]
            (card-ability state :challenger compile-card 0)
            (click-prompt state :challenger "Stack")
            (click-prompt state :challenger (find-card "Gordian Blade" (:deck (get-challenger))))
            (is (:placed (get-resource state 0)) "Gordian Blade should be placed"))
          (let [deck (count (:deck (get-challenger)))]
            (card-subroutine state :contestant iw 0)
            (is (= (+ 1 deck) (count (:deck (get-challenger)))) "Gordian Blade should be back in stack")
            (is (nil? (get-resource state 0))))))))

(deftest contaminate
  ;; Contaminate - add 3 virus counters to an placed challenger card with no virus counters
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Yusuf" "Chrome Parlor" (qty "Contaminate" 3)]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5 :click 2)
      (play-from-hand state :challenger "Yusuf")
      (play-from-hand state :challenger "Chrome Parlor")
      (let [yus (get-resource state 0)
            cp (get-radicle state 0)]
        (is (zero? (get-counters (refresh yus) :virus)) "Yusuf starts with 0 virus counters")
        (is (zero? (get-counters (refresh cp) :virus)) "Chrome Parlor starts with 0 virus counters")
        (play-from-hand state :challenger "Contaminate")
        (click-card state :challenger (refresh yus))
        (is (= 3 (get-counters (refresh yus) :virus)) "Yusuf has 3 counters after Contaminate")
        (play-from-hand state :challenger "Contaminate")
        (click-card state :challenger (refresh cp))
        (is (= 3 (get-counters (refresh cp) :virus)) "Chrome Parlor has 3 counters after Contaminate")
        (play-from-hand state :challenger "Contaminate")
        (click-card state :challenger (refresh yus))
        (click-prompt state :challenger "Done")
        (is (= 3 (get-counters (refresh cp) :virus)) "Yusuf isn't selectable by Contaminate"))))
  (testing "Hivemind makes virus resources act like they have a virus counter"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Aumakua" "Friday Chip" "Hivemind" "Contaminate"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5 :click 2)
      (play-from-hand state :challenger "Aumakua")
      (play-from-hand state :challenger "Hivemind")
      (play-from-hand state :challenger "Friday Chip")
      (let [aum (get-resource state 0)
            fc (get-hazard state 0)]
        (is (zero? (get-counters (refresh aum) :virus)) "Aumakua starts with 0 virus counters (not counting Hivemind)")
        (is (zero? (get-counters (refresh fc) :virus)) "Friday Chip starts with 0 virus counters")
        (play-from-hand state :challenger "Contaminate")
        (click-card state :challenger (refresh aum))
        (click-card state :challenger (refresh fc))
        (is (= 3 (get-counters (refresh fc) :virus)) "Friday Chip has 3 counters after Contaminate")
        (is (zero? (get-counters (refresh aum) :virus)) "Aumakua ends with 0 virus counters (not counting Hivemind)")))))

(deftest ^{:card-title "contestantorate-\"grant\""}
  contestantorate-grant
  ;; Contestantorate "Grant" - First time challenger places a card, the contestant loses 1 credit
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Contestantorate \"Grant\"" (qty "Daily Casts" 2)]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5)
      (play-from-hand state :challenger "Contestantorate \"Grant\"")
      (is (= 8 (:credit (get-contestant))) "Contestant starts with 8 credits")
      (play-from-hand state :challenger "Daily Casts")
      (is (= 7 (:credit (get-contestant))) "Contestant loses 1 credit")
      (play-from-hand state :challenger "Daily Casts")
      (is (empty? (:hand (get-challenger))) "Played all cards in hand")
      (is (= 7 (:credit (get-contestant))) "Contestant doesn't lose 1 credit")))
  (testing "with Hayley Kaplan. Issue #3162"
    (do-game
      (new-game (default-contestant)
                (make-deck "Hayley Kaplan: Universal Scholar" ["Contestantorate \"Grant\"" (qty "Clone Chip" 2)]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5)
      (play-from-hand state :challenger "Contestantorate \"Grant\"")
      (is (= 8 (:credit (get-contestant))) "Contestant starts with 8 credits")
      (play-from-hand state :challenger "Clone Chip")
      (click-prompt state :challenger "Yes")
      (click-card state :challenger (find-card "Clone Chip" (:hand (get-challenger))))
      (is (= 7 (:credit (get-contestant))) "Contestant only loses 1 credit"))))

(deftest contestantorate-scandal
  ;; Contestantorate Scandal - Contestant has 1 additional bad pub even with 0
  (do-game
    (new-game (default-contestant ["Elizabeth Mills"])
              (default-challenger ["Contestantorate Scandal" "Activist Support"
                               "Raymond Flint" "Investigative Journalism"]))
    (play-from-hand state :contestant "Elizabeth Mills" "New party")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 5 :click 1)
    (play-from-hand state :challenger "Raymond Flint")
    (play-from-hand state :challenger "Contestantorate Scandal")
    (is (empty? (:prompt (get-challenger))) "No BP taken, so no HQ access from Raymond")
    (play-from-hand state :challenger "Investigative Journalism")
    (is (= "Investigative Journalism" (:title (get-radicle state 1))) "IJ able to be placed")
    (run-on state "HQ")
    (is (= 1 (:run-credit (get-challenger))) "1 run credit from bad publicity")
    (run-jack-out state)
    (play-from-hand state :challenger "Activist Support")
    (take-credits state :challenger)
    (let [em (get-content state :party1 0)]
      (core/reveal state :contestant em)
      (is (= 1 (:has-bad-pub (get-contestant))) "Contestant still has BP")
      (take-credits state :contestant)
      (is (zero? (:bad-publicity (get-contestant))) "Contestant has BP, didn't take 1 from Activist Support"))))

(deftest credit-kiting
  ;; Credit Kiting - After successful central run lower place cost by 8 and gain a tag
  (do-game
    (new-game (default-contestant ["PAD Campaign" "Ice Wall"])
              (default-challenger ["Credit Kiting" "Femme Fatale"]))
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (take-credits state :contestant)
    (run-empty-locale state "Locale 1")
    (click-prompt state :challenger "No action")
    (play-from-hand state :challenger "Credit Kiting")
    (is (= 3 (:click (get-challenger))) "Card not played, successful run on central not made")
    (run-empty-locale state "HQ")
    (play-from-hand state :challenger "Credit Kiting")
    (click-card state :challenger (find-card "Femme Fatale" (:hand (get-challenger))))
    (is (= 4 (:credit (get-challenger))) "Femme Fatale only cost 1 credit")
    (testing "Femme Fatale can still target character when placed with Credit Kiting, issue #3715"
      (let [iw (get-character state :rd 0)]
        (click-card state :challenger iw)
        (is (:icon (refresh iw)) "Ice Wall has an icon")))
    (is (= 1 (:tag (get-challenger))) "Challenger gained a tag")))

(deftest data-breach
  ;; Data Breach
  (testing "Basic test"
    (do-game
      (new-game
        (default-contestant)
        (default-challenger [(qty "Data Breach" 3)]))
      (starting-hand state :contestant ["Hedge Fund"])
      (take-credits state :contestant)
      (play-from-hand state :challenger "Data Breach")
      (core/no-action state :contestant nil)
      (run-successful state)
      (click-prompt state :challenger "No action")
      (click-prompt state :challenger "Yes")
      (is (= [:rd] (get-in @state [:run :locale])) "Second run on R&D triggered")
      (core/no-action state :contestant nil)
      (run-successful state)
      (click-prompt state :challenger "No action")
      (is (empty? (:prompt (get-challenger))) "No prompt to run a third time")
      (is (not (:run @state)) "Run is over")
      (play-from-hand state :challenger "Data Breach")
      (run-jack-out state)
      (is (empty? (:prompt (get-challenger))) "No option to run again on unsuccessful run")))
  (testing "FAQ 4.1 - ensure challenger gets choice of activation order"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Doppelgänger" (qty "Data Breach" 3)]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Doppelgänger")
      (play-from-hand state :challenger "Data Breach")
      (core/no-action state :contestant nil)
      (run-successful state)
      ; (click-prompt state :challenger "No action")
      (click-prompt state :challenger "Doppelgänger")
      (click-prompt state :challenger "Yes")
      (click-prompt state :challenger "HQ")
      (is (:run @state) "New run started")
      (is (= [:hq] (:locale (:run @state))) "Running on HQ via Doppelgänger")
      (core/no-action state :contestant nil)
      (run-successful state)
      (click-prompt state :challenger "No action")
      (click-prompt state :challenger "Yes")
      (is (= [:rd] (get-in @state [:run :locale])) "Second Data Breach run on R&D triggered")
      (core/no-action state :contestant nil)
      (run-successful state))))

(deftest deja-vu
  ;; Deja Vu - recur one non-virus or two virus cards
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Déjà Vu" 2)
                               "Cache"
                               "Datasucker"
                               "Dirty Laundry"]))
    (take-credits state :contestant 3) ; pass to challenger's turn
    (discard-from-hand state :challenger "Cache")
    (discard-from-hand state :challenger "Datasucker")
    (discard-from-hand state :challenger "Dirty Laundry")
    (is (= 2 (count (:hand (get-challenger)))) "Two cards in hand prior to playing Déjà Vu")
    (play-from-hand state :challenger "Déjà Vu")
    (click-prompt state :challenger (find-card "Dirty Laundry" (:discard (get-challenger))))
    (is (empty? (:prompt (get-challenger))) "Recurring a non-virus card stops Déjà Vu prompting further")
    (is (= 2 (count (:hand (get-challenger)))) "Two cards in after playing Déjà Vu")
    (play-from-hand state :challenger "Déjà Vu")
    (click-prompt state :challenger (find-card "Cache" (:discard (get-challenger))))
    (is (not (empty? (:prompt (get-challenger)))) "Recurring a virus card causes Déjà Vu to prompt for second virus to recur")
    (click-prompt state :challenger (find-card "Datasucker" (:discard (get-challenger))))
    (is (= 3 (count (:hand (get-challenger)))) "Three cards in after playing second Déjà Vu")))

(deftest demolition-run
  ;; Demolition Run - Discard at no cost
  (do-game
    (new-game (default-contestant ["False Lead"
                             "Shell Contestantoration"
                             (qty "Hedge Fund" 3)])
              (default-challenger ["Demolition Run"]))
    (core/move state :contestant (find-card "False Lead" (:hand (get-contestant))) :deck) ; put False Lead back in R&D
    (play-from-hand state :contestant "Shell Contestantoration" "R&D") ; place region with a discard cost in root of R&D
    (take-credits state :contestant 2) ; pass to challenger's turn by taking credits
    (play-from-hand state :challenger "Demolition Run")
    (is (= 3 (:credit (get-challenger))) "Paid 2 credits for the event")
    (click-prompt state :challenger "R&D")
    (is (= [:rd] (get-in @state [:run :locale])) "Run initiated on R&D")
    (run-successful state)
    (click-prompt state :challenger "Unrevealed region in R&D")
    (click-prompt state :challenger "[Demolition Run]: Discard card")
    (is (= 3 (:credit (get-challenger))) "Discarded Shell Contestantoration at no cost")
    (click-prompt state :challenger "Card from deck")
    (click-prompt state :challenger "[Demolition Run]: Discard card")
    (is (zero? (:agenda-point (get-challenger))) "Didn't steal False Lead")
    (is (= 2 (count (:discard (get-contestant)))) "2 cards in Archives")
    (is (empty? (:prompt (get-challenger))) "Run concluded")))

(deftest deuces-wild
  ;; Deuces Wild
  (do-game
    (new-game (default-contestant ["Wraparound"
                             "The Future Perfect"])
              (default-challenger [(qty "Deuces Wild" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Wraparound" "New party")
    (take-credits state :contestant)
    (starting-hand state :challenger ["Deuces Wild" "Deuces Wild"])
    (play-from-hand state :challenger "Deuces Wild")
    (click-prompt state :challenger "Gain 3 [Credits]")
    (is (= 6 (:credit (get-challenger))) "Gained 1 net credit")
    (click-prompt state :challenger "Draw 2 cards")
    (is (= 3 (count (:hand (get-challenger)))) "Drew 2 cards")
    (is (empty? (:prompt (get-challenger))) "Deuces Wild not showing a third choice option")
    (play-from-hand state :challenger "Deuces Wild")
    (click-prompt state :challenger "Expose 1 character and make a run")
    (click-card state :challenger (get-character state :party1 0))
    (click-prompt state :challenger "HQ")
    (is (empty? (:prompt (get-challenger))) "Deuces prompt not queued")
    (run-continue state)
    (run-successful state)
    (is (= 2 (count (:prompt (get-challenger)))) "Deuces prompt not queued")
    (click-prompt state :contestant "0 [Credits]")
    (click-prompt state :challenger "0 [Credits]")
    (click-prompt state :challenger "Steal")
    (is (= 1 (count (:scored (get-challenger)))) "TFP stolen")
    (core/gain state :challenger :tag 1)
    (is (= 1 (:tag (get-challenger))) "Challenger has 1 tag")
    (click-prompt state :challenger "Remove 1 tag")
    (is (zero? (:tag (get-challenger))))))

(deftest dirty-laundry
  ;; Dirty Laundry - Gain 5 credits at the end of the run if it was successful
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Dirty Laundry" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Dirty Laundry")
    (click-prompt state :challenger "Archives")
    (run-successful state)
    (is (= 8 (:credit (get-challenger))) "Gained 5 credits")
    (play-from-hand state :challenger "Dirty Laundry")
    (click-prompt state :challenger "Archives")
    (run-jack-out state)
    (is (= 6 (:credit (get-challenger))) "Run unsuccessful; gained no credits")))

(deftest diversion-of-funds
  ;; Diversion of Funds
  (testing "Use ability"
    (do-game
      (new-game (default-contestant) (default-challenger [(qty "Diversion of Funds" 3)]))
      (take-credits state :contestant)
      (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits")
      ;; play Diversion of Funds, use ability
      (play-from-hand state :challenger "Diversion of Funds")
      (run-successful state)
      (click-prompt state :challenger "Replacement effect")
      (is (= 9 (:credit (get-challenger))) "Challenger netted 4 credits")
      (is (= 3 (:credit (get-contestant))) "Contestant lost 5 credits")
      (is (not (:run @state)) "Run finished")))
  (testing "Access"
    (do-game
      (new-game (default-contestant) (default-challenger [(qty "Diversion of Funds" 3)]))
      (take-credits state :contestant) ; pass to challenger's turn by taking credits
      (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits")
      ;; play Diversion, do not use ability
      (play-from-hand state :challenger "Diversion of Funds")
      (run-successful state)
      (click-prompt state :challenger "Access cards")
      (click-prompt state :challenger "No action")
      (is (empty? (:prompt (get-challenger))) "Prompt is closed")
      (is (= 4 (:credit (get-challenger))) "Challenger is down a credit")
      (is (= 8 (:credit (get-contestant))) "Contestant did not lose any credits")
      (is (not (:run @state)) "Run finished"))))

(deftest divide-and-conquer
  ;; Divide and Conquer
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Hostile Takeover" (qty "Ice Wall" 100)])
                (default-challenger ["Divide and Conquer"]))
      (starting-hand state :contestant ["Hostile Takeover" "Ice Wall" "Ice Wall"])
      (discard-from-hand state :contestant "Ice Wall")
      (discard-from-hand state :contestant "Ice Wall")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Divide and Conquer")
      (run-successful state)
      (click-prompt state :challenger "No action")
      (click-prompt state :challenger "Steal")
      (is (= 4 (-> (get-challenger) :register :last-run core/total-cards-accessed)) "Challenger should access 2 cards in Archives, 1 in R&D, and 1 in HQ")))
  (testing "with The Turning Wheel counters"
    (do-game
      (new-game (default-contestant ["Hostile Takeover" (qty "Ice Wall" 100)])
                (default-challenger ["Divide and Conquer" "The Turning Wheel"]))
      (starting-hand state :contestant (concat ["Hostile Takeover"]
                                         (repeat 4 "Ice Wall")))
      (discard-from-hand state :contestant "Ice Wall")
      (discard-from-hand state :contestant "Ice Wall")
      (take-credits state :contestant)
      (play-from-hand state :challenger "The Turning Wheel")
      (let [ttw (get-radicle state 0)]
        (core/add-counter state :challenger ttw :power 4)
        (play-from-hand state :challenger "Divide and Conquer")
        (card-ability state :challenger ttw 0)
        (card-ability state :challenger ttw 0)
        (run-successful state)
        ;; R&D
        (dotimes [_ 3]
          (click-prompt state :challenger "Card from deck")
          (click-prompt state :challenger "No action"))
        ;; HQ
        (dotimes [_ 3]
          (click-prompt state :challenger "Card from hand")
          (click-prompt state :challenger (-> (prompt-map :challenger) :choices first)))
        (is (empty? (:prompt (get-challenger))) "No prompts after all accesses are complete")
        (is (= 2 (-> (get-challenger) :register :last-run :access-bonus)) "The Turning Wheel should provide 2 additional accesses")
        (is (= 8 (-> (get-challenger) :register :last-run core/total-cards-accessed)) "Challenger should access 2 cards in Archives, 1 + 2 in R&D, and 1 + 2 in HQ")))))

(deftest drive-by
  ;; Drive By - Expose card in party locale and discard if site or region
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Eve Campaign" 2)
                               "Product Placement"
                               "Project Atlas"])
                (default-challenger [(qty "Drive By" 2)]))
      (core/gain state :contestant :click 1)
      (play-from-hand state :contestant "Eve Campaign" "New party")
      (play-from-hand state :contestant "Eve Campaign" "New party")
      (play-from-hand state :contestant "Project Atlas" "New party")
      (play-from-hand state :contestant "Product Placement" "HQ")
      (take-credits state :contestant)
      (let [eve1 (get-content state :party1 0)
            eve2 (get-content state :party2 0)
            atl (get-content state :party3 0)
            pp (get-content state :hq 0)]
        (core/reveal state :contestant eve1)
        (play-from-hand state :challenger "Drive By")
        (click-card state :challenger pp)
        (is (= 1 (count (get-in @state [:contestant :locales :hq :content])))
            "Regions in root of central locales can't be targeted")
        (click-card state :challenger (refresh eve1))
        (is (= 1 (count (get-in @state [:contestant :locales :party1 :content])))
            "Revealed cards can't be targeted")
        (click-card state :challenger eve2)
        (is (= 2 (:click (get-challenger))) "Spent 2 clicks")
        (is (and (= 1 (count (:discard (get-contestant))))
                 (= 5 (:credit (get-challenger))))
            "Eve discarded at no cost")
        (is (nil? (get-in @state [:contestant :locales :party2 :content])) "Locale 2 no longer exists")
        (play-from-hand state :challenger "Drive By")
        (click-card state :challenger atl)
        (is (zero? (:click (get-challenger))) "Challenger has 0 clicks left")
        (is (= 1 (count (get-in @state [:contestant :locales :party3 :content])))
            "Project Atlas not discarded from Locale 3"))))
  (testing "Psychic Field discarded after psi game. Issue #2127."
    (do-game
      (new-game (default-contestant ["Psychic Field"])
                (default-challenger [(qty "Drive By" 3)]))
      (play-from-hand state :contestant "Psychic Field" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Drive By")
      (click-card state :challenger (get-content state :party1 0))
      (click-prompt state :contestant "0 [Credits]")
      (click-prompt state :challenger "1 [Credits]")
      (is (empty? (get-content state :party1)) "Psychic Field discarded")))
  (testing "Turn on reprisal cards. Issue #3755."
    (do-game
      (new-game (default-contestant ["PAD Campaign"])
                (default-challenger ["Drive By"]))
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Drive By")
      (click-card state :challenger "PAD Campaign")
      (is (empty? (get-content state :party1)) "PAD Campaign discarded")
      (is (get-in (get-challenger) [:register :discarded-card]) "Registered as challenger discarded a card"))))

(deftest early-bird
  ;; Early Bird - Priority, make a run and gain a click
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Early Bird"]))
    (take-credits state :contestant)
    (run-empty-locale state "Archives")
    (play-from-hand state :challenger "Early Bird")
    (is (= 3 (:click (get-challenger))) "Card not played, Early Bird priority restriction")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Early Bird")
    (click-prompt state :challenger "Archives")
    (is (= 4 (:click (get-challenger))) "Early Bird gains click")))

(deftest embezzle
  ;; Embezzle
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Ice Wall" "Archer"])
                (default-challenger ["Embezzle"]))
      (take-credits state :contestant)
      (is (= 5 (:credit (get-challenger))))
      (play-run-event state (first (:hand (get-challenger))) :hq)
      (click-prompt state :challenger "Character")
      (is (= 2 (count (:discard (get-contestant)))) "HQ card discarded")
      (is (= 12 (:credit (get-challenger))))))
  (testing "Check that discarded cards are discarded face-up"
    (do-game
      (new-game (default-contestant ["Ice Wall"])
                (default-challenger ["Embezzle"]))
      (take-credits state :contestant)
      (is (= 5 (:credit (get-challenger))))
      (play-run-event state (first (:hand (get-challenger))) :hq)
      (click-prompt state :challenger "Character")
      (is (= 1 (count (:discard (get-contestant)))) "HQ card discarded")
      (is (:seen (first (:discard (get-contestant)))) "Discarded card is registered as seen")
      (is (= 8 (:credit (get-challenger)))))))

(deftest emergent-creativity
  ;; Emergent Creativty - Double, discard resources/hazard from grip, place from heap
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Emergent Creativity" "Paperclip"
                               "Heartbeat" "Gordian Blade" "Test Run"]))
    (starting-hand state :challenger ["Emergent Creativity" "Heartbeat" "Gordian Blade" "Test Run"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Emergent Creativity")
    (click-card state :challenger (find-card "Heartbeat" (:hand (get-challenger))))
    (click-card state :challenger (find-card "Gordian Blade" (:hand (get-challenger))))
    (click-prompt state :challenger "Done")
    (click-prompt state :challenger (find-card "Paperclip" (:deck (get-challenger))))
    (is (= 3 (:credit (get-challenger))) "Offset cost of placing Paperclip")
    (is (zero? (count (:deck (get-challenger)))) "Placed from heap")
    (is (= 3 (count (:discard (get-challenger)))) "Discard is 3 cards - EC, Heartbeat, GB")
    (is (= 2 (:click (get-challenger))) "Emergent Creativity is a Double event")))

(deftest employee-strike
  ;; Employee Strike
  (testing "vs Blue Sun, suppress Step 1.2"
    (do-game
      (new-game (make-deck "Blue Sun: Powering the Future" ["Ice Wall"])
                (default-challenger ["Employee Strike" "Scrubbed"]))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Employee Strike")
      (take-credits state :challenger)
      (is (not (:contestant-phase-12 @state)) "Employee Strike suppressed Blue Sun step 1.2")))
  (testing "vs PU/Philotic - test for #2688"
    (do-game
      (new-game (make-deck "Cardnum: Potential Unleashed" ["Philotic Entanglement" (qty "Braintrust" 2)])
                (default-challenger [(qty "Employee Strike" 10)]))
      (play-from-hand state :contestant "Braintrust" "New party")
      (play-from-hand state :contestant "Braintrust" "New party")
      (take-credits state :contestant)
      (run-empty-locale state "Locale 1")
      (click-prompt state :challenger "Steal")
      (run-empty-locale state "Locale 2")
      (click-prompt state :challenger "Steal")
      (play-from-hand state :challenger "Employee Strike")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Philotic Entanglement" "New party")
      (score-agenda state :contestant (get-content state :party3 0))
      (is (= 3 (count (:discard (get-challenger))))
          "Discard is 3 cards - 2 from Philotic, 1 EStrike.  Nothing from PU mill"))))

(deftest encore
  ;; Encore - Run all 3 central locales successfully to take another turn.  Remove Encore from game.
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Hedge Fund"])
                (default-challenger ["Encore"]))
      (play-from-hand state :contestant "Hedge Fund")
      (take-credits state :contestant)
      (run-empty-locale state "Archives")
      (run-empty-locale state "R&D")
      (run-empty-locale state "HQ")
      (play-from-hand state :challenger "Encore")
      (is (= 1 (count (:rfg (get-challenger)))) "Encore removed from game")
      (take-credits state :challenger)
      (take-credits state :challenger)
      ; only get one extra turn
      (take-credits state :challenger)
      (is (= 9 (:credit (get-challenger))))))
  (testing "2 encores in a 5 click turn results in 2 extra turns"
    (do-game
      (new-game (default-contestant ["Hedge Fund"])
                (default-challenger [(qty "Encore" 2)]))
      (play-from-hand state :contestant "Hedge Fund")
      (take-credits state :contestant)
      (core/gain state :challenger :click 1)
      (run-empty-locale state "Archives")
      (run-empty-locale state "R&D")
      (run-empty-locale state "HQ")
      (play-from-hand state :challenger "Encore")
      (play-from-hand state :challenger "Encore")
      (is (= 2 (count (:rfg (get-challenger)))) "2 Encores removed from game")
      (take-credits state :challenger)
      (take-credits state :challenger)
      ;; Two extra turns
      (take-credits state :challenger)
      (is (= 13 (:credit (get-challenger)))))))

(deftest eureka!
  ;; Eureka! - Place the resource but discard the event
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Eureka!" 2) "Torch" "Sure Gamble"]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 1)
    (core/move state :challenger (find-card "Torch" (:hand (get-challenger))) :deck)
    (play-from-hand state :challenger "Eureka!")
    (click-prompt state :challenger "Yes")
    (is (= 3 (:credit (get-challenger))))
    (is (= 1 (count (get-resource state))))
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (play-from-hand state :challenger "Eureka!")
    (is (zero? (:credit (get-challenger))))
    (is (= 3 (count (:discard (get-challenger)))))))

(deftest exploratory-romp
  ;; Exploratory Romp - Remove advancements from card instead of accessing
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["TGTBT"])
                (default-challenger ["Exploratory Romp"]))
      (play-from-hand state :contestant "TGTBT" "New party")
      (let [tg (get-content state :party1 0)]
        (advance state tg 2)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Exploratory Romp")
        (click-prompt state :challenger "Locale 1")
        (run-successful state)
        (click-prompt state :challenger "Replacement effect")
        (click-prompt state :challenger "2")
        (click-card state :challenger (refresh tg))
        (is (zero? (:tag (get-challenger))) "No tags, didn't access TGTBT")
        (is (zero? (get-counters (refresh tg) :advancement)) "Advancements removed"))))
  (testing "Don't remove more than the existing number of advancement tokens"
    (do-game
      (new-game (default-contestant ["TGTBT"])
                (default-challenger ["Exploratory Romp"]))
      (play-from-hand state :contestant "TGTBT" "New party")
      (let [tg (get-content state :party1 0)]
        (advance state tg 2)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Exploratory Romp")
        (click-prompt state :challenger "Locale 1")
        (run-successful state)
        (click-prompt state :challenger "Replacement effect")
        (click-prompt state :challenger "3")
        (click-card state :challenger (refresh tg))
        (is (zero? (:tag (get-challenger))) "No tags, didn't access TGTBT")
        (is (zero? (get-counters (refresh tg) :advancement)) "Advancements removed")))))

(deftest falsified-credentials
  ;; Falsified Credentials - Expose card in party
  ;; locale and correctly guess its type to gain 5 creds
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Eve Campaign" 2)
                               (qty "Product Placement" 2)
                               "Project Atlas"])
                (default-challenger [(qty "Falsified Credentials" 3)]))
      (core/gain state :contestant :click 2)
      (play-from-hand state :contestant "Eve Campaign" "New party")
      (play-from-hand state :contestant "Eve Campaign" "New party")
      (play-from-hand state :contestant "Project Atlas" "New party")
      (play-from-hand state :contestant "Product Placement" "HQ")
      (play-from-hand state :contestant "Product Placement" "Locale 3")
      (take-credits state :contestant)
      (let [eve1 (get-content state :party1 0)
            eve2 (get-content state :party2 0)
            atl (get-content state :party3 0)
            pp1 (get-content state :hq 0)
            pp2 (get-content state :party3 1)]
        (core/reveal state :contestant eve1)
        (play-from-hand state :challenger "Falsified Credentials")
        (click-prompt state :challenger "Site")
        (click-card state :challenger (refresh eve1))
        (is (= 4 (:credit (get-challenger)))
            "Revealed cards can't be targeted")
        (click-card state :challenger eve2)
        (is (= 3 (:click (get-challenger))) "Spent 1 click")
        (is (= 9 (:credit (get-challenger))) "Gained 5 creds for guessing site correctly")
        (play-from-hand state :challenger "Falsified Credentials")
        (click-prompt state :challenger "Region")
        (click-card state :challenger pp1)
        (is (= 8 (:credit (get-challenger))) "Can't target cards in centrals")
        (click-card state :challenger pp2)
        (is (= 13 (:credit (get-challenger)))
            "Gained 5 creds for guessing region correctly, even if locale contains non-region as well")
        (core/reveal state :contestant pp2)
        (play-from-hand state :challenger "Falsified Credentials")
        (click-prompt state :challenger "Agenda")
        (click-card state :challenger atl)
        (is (= 17 (:credit (get-challenger)))
            "Gained 5 credits for guessing agenda correctly, even with revealed card in locale"))))
  (testing "vs Zaibatsu Loyalty. If Falsified Credentials fails to expose, it grants no credits."
    (do-game
      (new-game (default-contestant ["Zaibatsu Loyalty" "Project Atlas"])
                (default-challenger [(qty "Falsified Credentials" 2)]))
      (play-from-hand state :contestant "Project Atlas" "New party")
      (play-from-hand state :contestant "Zaibatsu Loyalty" "New party")
      (take-credits state :contestant)
      (let [atl (get-content state :party1 0)
            zaibatsu (get-content state :party2 0)]
        (core/reveal state :contestant zaibatsu)
        (play-from-hand state :challenger "Falsified Credentials")
        (click-prompt state :challenger "Agenda")
        (click-card state :challenger atl)
        (click-prompt state :contestant "Done")
        (is (= 9 (:credit (get-challenger))) "An unprevented expose gets credits")
        (play-from-hand state :challenger "Falsified Credentials")
        (click-prompt state :challenger "Agenda")
        (click-card state :challenger atl)
        (card-ability state :contestant (refresh zaibatsu) 0) ; prevent the expose!
        (click-prompt state :contestant "Done")
        (is (= 8 (:credit (get-challenger))) "A prevented expose does not")))))

(deftest feint
  ;; Feint - bypass 2 pieces of character on HQ, but access no cards
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Feint"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Feint")
    (run-successful state)
    (click-prompt state :challenger "OK")
    (is (not (:run @state)) "Run is over")))

(deftest frantic-coding
  ;; Frantic Coding - Place 1 resource, other 9 cards are discarded
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Frantic Coding" "Torch" "Corroder"
                                 "Magnum Opus" (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                                 "John Masanori" "Amped Up" "Wanton Destruction"]))
      (starting-hand state :challenger ["Frantic Coding"])
      (take-credits state :contestant)
      (play-from-hand state :challenger "Frantic Coding")
      (click-prompt state :challenger "OK")
      (let [get-prompt (fn [] (first (#(get-in @state [:challenger :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
        (is (= 2 (:credit (get-challenger))))
        (is (= 1 (count (:discard (get-challenger)))))
        (click-prompt state :challenger (find-card "Magnum Opus" (:deck (get-challenger))))
        (is (= 1 (count (get-resource state))))
        (is (= 2 (:credit (get-challenger))) "Magnum Opus placed for free")
        (is (= 10 (count (:discard (get-challenger))))))))
  (testing "Don't place anything, all 10 cards are discarded"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Frantic Coding" "Torch" "Corroder"
                                 "Magnum Opus" (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                                 "John Masanori" "Amped Up" "Wanton Destruction"]))
      (starting-hand state :challenger ["Frantic Coding"])
      (take-credits state :contestant)
      (play-from-hand state :challenger "Frantic Coding")
      (click-prompt state :challenger "OK")
      (let [get-prompt (fn [] (first (#(get-in @state [:challenger :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
        (is (= 1 (count (:discard (get-challenger)))))
        (click-prompt state :challenger "No place")
        (is (zero? (count (get-resource state))))
        (is (= 11 (count (:discard (get-challenger)))))))))

(deftest ^{:card-title "\"freedom-through-equality\""}
  freedom-through-equality
  ;; Move Freedom Through Equality to challenger score on another steal
  ;; Check only one current used
  (do-game
    (new-game (default-contestant [(qty "Project Beale" 2)])
              (default-challenger ["Street Peddler" (qty "\"Freedom Through Equality\"" 3) "Sure Gamble"]))
    (starting-hand state :challenger ["Street Peddler"
                                  "\"Freedom Through Equality\""
                                  "\"Freedom Through Equality\""
                                  "Sure Gamble"])
    (play-from-hand state :contestant "Project Beale" "New party")
    (play-from-hand state :contestant "Project Beale" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Street Peddler")
    (run-empty-locale state "Locale 1")
    (click-prompt state :challenger "Steal")
    (is (= 1 (count (:scored (get-challenger)))) "Freedom Through Equality not moved from Peddler to score area")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (run-empty-locale state "Locale 2")
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "\"Freedom Through Equality\"")
    (play-from-hand state :challenger "\"Freedom Through Equality\"")
    (click-prompt state :challenger "Steal")
    (is (= 3 (count (:scored (get-challenger)))) "Freedom Through Equality moved to score area")
    (is (= 5 (:agenda-point (get-challenger))) "Freedom Through Equality for 1 agenda point")))

(deftest freelance-coding-contract
  ;; Freelance Coding Contract - Gain 2 credits per resource discarded from Grip
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Freelance Coding Contract"
                               "Paricia" "Cloak" "Inti"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Freelance Coding Contract")
    (click-card state :challenger (find-card "Cloak" (:hand (get-challenger))))
    (click-card state :challenger (find-card "Paricia" (:hand (get-challenger))))
    (click-card state :challenger (find-card "Inti" (:hand (get-challenger))))
    (click-prompt state :challenger "Done")
    (is (= 3 (count (filter #(= (:type %) "Resource") (:discard (get-challenger)))))
        "3 resources in Heap")
    (is (= 11 (:credit (get-challenger))) "Gained 6 credits from 3 discarded resources")))

(deftest game-day
  ;; Game Day - draw until at handsize
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Game Day" 3)
                              (qty "Public Sympathy" 3)
                              (qty "Sure Gamble" 3)
                              (qty "Easy Mark" 3)]))
   (take-credits state :contestant)
   ;; move needed cards to hand -- in case they were not drawn
   (core/move state :challenger (find-card "Game Day" (:deck (get-challenger))) :hand)
   (core/move state :challenger (find-card "Public Sympathy" (:deck (get-challenger))) :hand)
   (play-from-hand state :challenger "Public Sympathy")
   (is (= 7 (core/hand-size state :challenger)) "Challenger hand size is 7")
   (play-from-hand state :challenger "Game Day")
   (is (= 7 (count (:hand (get-challenger)))) "Drew up to 7 cards")))

(deftest glut-cipher
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3) (qty "Wraparound" 2) "Hedge Fund"])
              (default-challenger [(qty "Glut Cipher" 3)]))
    (take-credits state :contestant)
    (discard-from-hand state :contestant "Ice Wall")
    (discard-from-hand state :contestant "Ice Wall")
    (discard-from-hand state :contestant "Hedge Fund")
    (is (= 3 (count (:discard (get-contestant)))) "There are 3 cards in Archives")
    (play-from-hand state :challenger "Glut Cipher")
    (is (= 3 (count (:discard (get-contestant)))) "Glut Cipher did not fire when < 5 cards")
    (is (zero? (count (filter :seen (:discard (get-contestant))))) "There are no faceup cards in Archives")
    (run-on state :archives)
    (run-successful state)
    (is (= 3 (count (filter :seen (:discard (get-contestant))))) "There are 3 faceup cards in Archives")
    (discard-from-hand state :contestant "Wraparound")
    (discard-from-hand state :contestant "Wraparound")
    (discard-from-hand state :contestant "Ice Wall")
    (is (= 3 (count (filter :seen (:discard (get-contestant))))) "There are 3 faceup cards in Archives")
    (is (= 6 (count (:discard (get-contestant)))) "There are 6 cards in Archives")
    (play-run-event state "Glut Cipher" :archives)
    (click-card state :contestant (get-discarded state :contestant 0))
    (click-card state :contestant (get-discarded state :contestant 1))
    (click-card state :contestant (get-discarded state :contestant 3))
    (is (:prompt (get-contestant)) "There is still a prompt")
    (click-card state :contestant (get-discarded state :contestant 4))
    (click-card state :contestant (get-discarded state :contestant 5))
    (is (nil? (-> (get-contestant) :prompt first)) "Selecting 5 cards closed prompt")
    (let [discard (:discard (get-contestant))]
      (is (find-card "Hedge Fund" discard) "Hedge Fund is still in Archives")
      (is (= 6 (count discard)) "There are 6 cards in Archives")
      (is (= 1 (count (filter :seen discard))) "There is 1 seen card in Archives"))
    (is (zero? (count (:hand (get-contestant)))) "There are no cards in hand")))

(deftest guinea-pig
  ;; Guinea Pig
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Guinea Pig" (qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Guinea Pig")
    (is (= 11 (:credit (get-challenger))) "Gained +6 credits from playing Guinea Pig")
    (is (empty? (:hand (get-challenger))) "No cards left in grip, discarded all cards due to Guinea Pig")
    (is (= 4 (count (:discard (get-challenger)))) "3 cards discarded from Guinea Pig + Guinea Pig itself")))

(deftest hacktivist-meeting
  ;; Hacktivist Meeting
  ;; Discard a random card from contestant hand while active
  ;; Make sure it is not active when hosted on Peddler
  (do-game
    (new-game (default-contestant [(qty "Jeeves Model Bioroids" 2)
                             (qty "Jackson Howard" 2)])
              (default-challenger ["Street Peddler"
                               (qty "Hacktivist Meeting" 3)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler" "Hacktivist Meeting"])
    (play-from-hand state :challenger "Street Peddler")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Jeeves Model Bioroids" "New party")
    (play-from-hand state :contestant "Jackson Howard" "New party")
    (let [jeeves (get-content state :party1 0)
          jackson (get-content state :party2 0)]
      (core/reveal state :contestant jeeves)
      (is (zero? (count (:discard (get-contestant)))) "Nothing discarded to reveal Jeeves - Hacktivist not active")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Hacktivist Meeting")
      (core/reveal state :contestant jackson)
      (is (= 1 (count (:discard (get-contestant)))) "Card discarded to reveal Jackson - Hacktivist active"))))

(deftest high-stakes-job
  ;; High Stakes Job - run on locale with at least 1 piece of unrevealed character, gains 12 credits if successful
  (do-game
    (new-game (default-contestant ["Ice Wall"])
              (default-challenger ["High-Stakes Job"]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 1)
    (is (= 6 (:credit (get-challenger))) "Challenger starts with 6 credits")
    (play-from-hand state :challenger "High-Stakes Job")
    (click-prompt state :challenger "HQ")
    (run-successful state)
    (is (= 12 (:credit (get-challenger))) "Challenger gains 12 credits")))

(deftest hot-pursuit
  ;; Hot Pursuit
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Hot Pursuit"]))
    (take-credits state :contestant)
    (play-run-event state (first (:hand (get-challenger))) :hq)
    (is (= (+ 5 -2 9) (:credit (get-challenger))) "Gained 9 credits on successful run")
    (is (= 1 (:tag (get-challenger))) "Took 1 tag on successful run")
    (is (prompt-map :challenger) "Still have access prompt")))

(deftest independent-thinking
  ;; Independent Thinking - Discard 2 placed cards, including a facedown directive, and draw 2 cards
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Apex: Invasive Predator"
                 ["Neutralize All Threats" (qty "Independent Thinking" 2)
                  (qty "Fan Site" 3) (qty "Street Magic" 3)]))
    (starting-hand state :challenger ["Fan Site" "Fan Site" "Neutralize All Threats"
                                  "Independent Thinking" "Independent Thinking"])
    (take-credits state :contestant)
    (core/end-phase-12 state :challenger nil)
    (click-card state :challenger (find-card "Neutralize All Threats" (:hand (get-challenger))))
    (play-from-hand state :challenger "Fan Site")
    (let [fs (get-radicle state 0)
          nat (get-challenger-facedown state 0)]
      (play-from-hand state :challenger "Independent Thinking")
      (click-card state :challenger fs)
      (click-card state :challenger nat)
      (click-prompt state :challenger "Done")
      (is (= 4 (count (:hand (get-challenger)))) "Discarding 2 cards draws 2 card"))))

(deftest indexing
  ;; Indexing - Full test
  (do-game
    (new-game (default-contestant ["Caprcharacter Nisei" "Adonis Campaign" "Quandary"
                            "Jackson Howard" "Global Food Initiative"])
            (default-challenger ["Indexing"]))
    (dotimes [_ 5] (core/move state :contestant (first (:hand (get-contestant))) :deck))
    (take-credits state :contestant)
    (is (zero? (count (:hand (get-contestant)))))
    (is (= 5 (count (:deck (get-contestant)))))
    (play-from-hand state :challenger "Indexing")
    (is (= :rd (get-in @state [:run :locale 0])))
    (run-successful state)
    (click-prompt state :challenger "Replacement effect")
    (click-prompt state :challenger (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
    (click-prompt state :challenger (find-card "Adonis Campaign" (:deck (get-contestant))))
    (click-prompt state :challenger (find-card "Quandary" (:deck (get-contestant))))
    (click-prompt state :challenger (find-card "Jackson Howard" (:deck (get-contestant))))
    (click-prompt state :challenger (find-card "Global Food Initiative" (:deck (get-contestant))))
    ;; try starting over
    (click-prompt state :challenger "Start over")
    (click-prompt state :challenger (find-card "Global Food Initiative" (:deck (get-contestant))))
    (click-prompt state :challenger (find-card "Jackson Howard" (:deck (get-contestant))))
    (click-prompt state :challenger (find-card "Quandary" (:deck (get-contestant))))
    (click-prompt state :challenger (find-card "Adonis Campaign" (:deck (get-contestant))))
    (click-prompt state :challenger (find-card "Caprcharacter Nisei" (:deck (get-contestant)))) ;this is the top card of R&D
    (click-prompt state :challenger "Done")
    (is (= "Caprcharacter Nisei" (:title (first (:deck (get-contestant))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-contestant))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-contestant)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-contestant))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-contestant)))))))))))

(deftest information-sifting
  ;; Information Sifting - complicated interactions with damage prevention
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping"
                         ["Snare!" "PAD Campaign" "Hostile Infrastructure"
                          "Braintrust" "Hedge Fund" "Power Shutdown"])
              (default-challenger [(qty "Information Sifting" 2) (qty "Deus X" 2) "Sure Gamble"]))
    (play-from-hand state :contestant "Hostile Infrastructure" "New party")
    (core/gain state :contestant :credit 10)
    (core/reveal state :contestant (get-content state :party1 0))
    (core/gain state :challenger :credit 10)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Deus X")
    (play-from-hand state :challenger "Deus X")
    (play-run-event state (find-card "Information Sifting" (:hand (get-challenger))) :hq)
    (click-card state :contestant (find-card "Snare!" (:hand (get-contestant))))
    (click-card state :contestant (find-card "PAD Campaign" (:hand (get-contestant))))
    (click-prompt state :contestant "Done")
    (is (= :waiting (-> (get-contestant) :prompt first :prompt-type)) "Contestant is waiting for Challenger selection")
    (click-prompt state :challenger "Pile 1 (2 cards)")
    (click-prompt state :challenger "Card from pile 1")
    ;; the cards are selected randomly :(
    (letfn [(prevent-snare [existing-dmg]
              (click-prompt state :contestant "Yes")
              (card-ability state :challenger (get-resource state 0) 1)
              (click-prompt state :challenger "Done")
              (is (= (inc existing-dmg) (count (:discard (get-challenger)))) "Damage from Snare! prevented")
              (click-prompt state :challenger (-> (prompt-map :challenger) :choices first))
              (when (-> (prompt-map :challenger) :choices first)
                (click-prompt state :challenger "Done")) ; don't prevent Hostile dmg
              ;; chronos prompt
              (click-prompt state :contestant "Yes")
              (click-prompt state :contestant (find-card "Sure Gamble" (:hand (get-challenger))))
              (is (= (+ 2 existing-dmg) (count (:discard (get-challenger)))) "Damage from Hostile Inf not prevented"))
            (allow-pad [existing-dmg]
              (click-prompt state :challenger (-> (prompt-map :challenger) :choices first))
              (card-ability state :challenger (get-resource state 0) 1)
              (is (= (inc existing-dmg) (count (:discard (get-challenger)))) "Challenger prevented damage from Hostile Inf")
              (click-prompt state :challenger "Done"))]
      (if (= :waiting (-> (get-challenger) :prompt first :prompt-type)) ; hit the snare
        ;; prevent the damage
        (do (prevent-snare (count (:discard (get-challenger))))
            (click-prompt state :challenger "Card from pile 1")
            (allow-pad (count (:discard (get-challenger)))))
        (do (allow-pad (count (:discard (get-challenger))))
            (click-prompt state :challenger "Card from pile 1")
            (prevent-snare (count (:discard (get-challenger)))))))
    (play-run-event state (find-card "Information Sifting" (:hand (get-challenger))) :hq)
    (click-card state :contestant (find-card "Power Shutdown" (:hand (get-contestant))))
    (click-card state :contestant (find-card "Hedge Fund" (:hand (get-contestant))))
    (is (= :waiting (-> (get-contestant) :prompt first :prompt-type)) "Selecting max cards closed the selection prompt")
    (click-prompt state :challenger "Pile 2 (1 card)")
    (click-prompt state :challenger "Card from pile 2")
    (click-prompt state :challenger "Steal")
    (is (= 1 (count (:scored (get-challenger)))) "Challenger stole agenda")))

(deftest inject
  ;; Inject - Draw 4 cards from Stack and gain 1 credit per discarded resource
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Inject" (qty "Imp" 2) (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Imp" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Imp" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (is (= 4 (count (:deck (get-challenger)))))
    (play-from-hand state :challenger "Inject")
    (is (= 2 (count (:hand (get-challenger)))) "2 non-resources kept in Grip")
    (is (= 2 (count (filter #(= (:type %) "Resource") (:discard (get-challenger)))))
        "2 resources in Heap")
    (is (= 6 (:credit (get-challenger)))
        "Paid 1 credit to play Inject, gained 2 credits from discarded resources")))

(deftest injection-attack
  ;; Injection Attack
  (do-game
    (new-game (default-contestant ["Paper Wall"])
              (default-challenger ["Injection Attack" "Corroder"]))
    (play-from-hand state :contestant "Paper Wall" "Archives")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "Injection Attack")
    (click-prompt state :challenger "Archives")
    (is (= 2 (:current-strength (get-resource state 0))) "Corroder at 2 strength")
    (click-card state :challenger (get-resource state 0))
    (is (= 4 (:current-strength (get-resource state 0))) "Corroder at 4 strength")
    (run-continue state)
    (is (= 4 (:current-strength (get-resource state 0))) "Corroder at 4 strength")
    (run-continue state)
    (run-successful state)
    (is (= 2 (:current-strength (get-resource state 0))) "Corroder reset to 2 strength")))

(deftest insight
  ;; Insight
  (do-game
    (new-game (default-contestant ["Caprcharacter Nisei" "Elizabeth Mills"
                            "Jackson Howard" "Director Haas"])
            (default-challenger ["Insight"]))
    (dotimes [_ 4] (core/move state :contestant (first (:hand (get-contestant))) :deck))
    (take-credits state :contestant)
    (is (zero? (count (:hand (get-contestant)))))
    (is (= 4 (count (:deck (get-contestant)))))
    (play-from-hand state :challenger "Insight")
    (is (= :waiting (-> (get-challenger) :prompt first :prompt-type)) "Challenger is waiting for Contestant to reorder")
    (click-prompt state :contestant (find-card "Director Haas" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Elizabeth Mills" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
    (click-prompt state :contestant "Done")
    (is (not= :waiting (-> (get-challenger) :prompt first :prompt-type)) "Waiting prompt done")
    (is (= "Caprcharacter Nisei" (:title (nth (:deck (get-contestant)) 0))))
    (is (= "Jackson Howard" (:title (nth (:deck (get-contestant)) 1))))
    (is (= "Elizabeth Mills" (:title (nth (:deck (get-contestant)) 2))))
    (is (= "Director Haas" (:title (nth (:deck (get-contestant)) 3))))))

(deftest interdiction
  ;; Contestant cannot reveal non-character cards during challenger's turn
  (do-game
    (new-game (default-contestant ["Jeeves Model Bioroids" "Jackson Howard"])
              (default-challenger ["Street Peddler"
                               (qty "Interdiction" 3)]))
    (starting-hand state :challenger ["Street Peddler" "Interdiction"])
    (play-from-hand state :contestant "Jeeves Model Bioroids" "New party")
    (play-from-hand state :contestant "Jackson Howard" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Street Peddler")
    (let [jeeves (get-content state :party1 0)
          jackson (get-content state :party2 0)]
      (core/reveal state :contestant jeeves)
      (is (:revealed (refresh jeeves)) "Jeeves is revealed.  Interdiction not active when on Peddler")
      (play-from-hand state :challenger "Interdiction")
      (core/reveal state :contestant jackson)
      (is (not (:revealed (refresh jackson))) "Jackson is not revealed"))))

(deftest ^{:card-title "i've-had-worse"}
  ive-had-worse
  ;; I've Had Worse - Draw 3 cards when lost to net/meat damage; don't trigger if flatlined
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Scorched Earth" 3) (qty "Pup" 3)])
                (default-challenger [(qty "I've Had Worse" 2) (qty "Sure Gamble" 3) (qty "Imp" 2)]))
      (core/gain state :challenger :tag 1)
      (core/gain state :contestant :credit 5)
      (starting-hand state :challenger ["I've Had Worse"])
      (play-from-hand state :contestant "Pup" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (card-subroutine state :contestant (get-character state :hq 0) 0)
      (is (= 1 (count (:discard (get-challenger)))))
      (is (= 3 (count (:hand (get-challenger)))) "I've Had Worse triggered and drew 3 cards")
      (starting-hand state :challenger ["I've Had Worse" "Imp" "Imp"])
      (play-from-hand state :contestant "Scorched Earth")
      (is (zero? (count (:hand (get-challenger)))) "Challenger has 0 cards in hand")
      (is (= :contestant (:winner @state)) "Contestant wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline")
      (is (= 4 (count (:discard (get-challenger)))) "All 3 cards in Grip discarded by Scorched Earth")
      (is (= 3 (count (:deck (get-challenger)))) "No cards drawn from I've Had Worse")))
  (testing "Will save you if you apocalypse away a lot of cards vs Hostile Infrastructure"
    (do-game
      (new-game (default-contestant ["Hostile Infrastructure" (qty "Ice Wall" 2)])
                (default-challenger [(qty "I've Had Worse" 3) (qty "Sure Gamble" 3) (qty "Apocalypse" 2)]))
      (starting-hand state :challenger ["I've Had Worse" "Apocalypse"])
      (starting-hand state :contestant ["Hostile Infrastructure" "Ice Wall" "Ice Wall"])
      (play-from-hand state :contestant "Hostile Infrastructure" "New party")
      (play-from-hand state :contestant "Ice Wall" "New party")
      (play-from-hand state :contestant "Ice Wall" "New party")
      (core/reveal state :contestant (get-content state :party1 0))
      (take-credits state :contestant)
      (run-empty-locale state "HQ")
      (run-empty-locale state "Archives")
      (run-empty-locale state "R&D")
      (play-from-hand state :challenger "Apocalypse")
      (is (not (= "Flatline" (:reason @state))) "Win condition does not report flatline"))))

(deftest lawyer-up
  ;; Lawyer Up - Lose 2 tags and draw 3 cards
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Lawyer Up" (qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/gain state :challenger :tag 3)
    (play-from-hand state :challenger "Lawyer Up")
    (is (= 3 (count (:hand (get-challenger)))) "Drew 3 cards")
    (is (= 2 (:click (get-challenger))) "Spent 2 clicks")
    (is (= 1 (:tag (get-challenger))) "Lost 2 tags")))

(deftest leave-no-trace
  ;; Leave No Trace should hide Character that was revealed during the run
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Ice Wall" 2)])
                (default-challenger ["Leave No Trace"]))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/reveal state :contestant (get-character state :hq 1))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Leave No Trace")
      (click-prompt state :challenger "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (run-successful state)
      (is (not (:revealed (get-character state :hq 0))) "Inner Ice Wall should not be revealed")
      (is (:revealed (get-character state :hq 1)) "Outer Ice Wall should be revealed still")))
  (testing "should not hide Character that has changed during a run"
    (do-game
      (new-game (default-contestant ["Ice Wall"])
                (default-challenger ["Leave No Trace"]))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (take-credits state :contestant)
      (is (:revealed (get-character state :hq 0)) "Ice Wall should be revealed initially")
      (play-from-hand state :challenger "Leave No Trace")
      (click-prompt state :challenger "Archives")
      (core/add-prop state :contestant (get-character state :hq 0) :advance-counter 1)
      (run-successful state)
      (is (= 1 (get-counters (get-character state :hq 0) :advancement)))
      (is (:revealed (get-character state :hq 0)) "Ice Wall should still be revealed"))))

(deftest mad-dash
  ;; Mad Dash - Make a run. Move to score pile as 1 point if steal agenda.  Take 1 meat if not
  (do-game
    (new-game (default-contestant ["Project Atlas"])
              (default-challenger [(qty "Mad Dash" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Mad Dash")
    (click-prompt state :challenger "Archives")
    (run-successful state)
    (is (= 2 (count (:discard (get-challenger)))) "Took a meat damage")
    (play-from-hand state :challenger "Mad Dash")
    (click-prompt state :challenger "HQ")
    (run-successful state)
    (click-prompt state :challenger "Steal")
    (is (= 2 (count (:scored (get-challenger)))) "Mad Dash moved to score area")
    (is (= 3 (:agenda-point (get-challenger))) "Mad Dash scored for 1 agenda point")))

(deftest making-an-entrance
  ;; Making an Entrance - Full test
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Making an Entrance" 2) "Sure Gamble" "Desperado"
                               "Diesel" "Corroder" "Patron"]))
    (starting-hand state :challenger ["Making an Entrance"])
    (is (= 1 (count (:hand (get-challenger)))))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Making an Entrance")
    ;; discard cards
    (is (= 1 (count (:discard (get-challenger)))))
    (click-prompt state :challenger (find-card "Desperado" (:deck (get-challenger))))
    (click-prompt state :challenger (find-card "Diesel" (:deck (get-challenger))))
    (is (= 3 (count (:discard (get-challenger)))))
    (click-prompt state :challenger "None")
    ;; start arranging
    (click-prompt state :challenger (find-card "Making an Entrance" (:deck (get-challenger))))
    (click-prompt state :challenger (find-card "Sure Gamble" (:deck (get-challenger))))
    (click-prompt state :challenger (find-card "Corroder" (:deck (get-challenger))))
    (click-prompt state :challenger (find-card "Patron" (:deck (get-challenger))))
    ;; try starting over
    (click-prompt state :challenger "Start over")
    (click-prompt state :challenger (find-card "Patron" (:deck (get-challenger))))
    (click-prompt state :challenger (find-card "Corroder" (:deck (get-challenger))))
    (click-prompt state :challenger (find-card "Sure Gamble" (:deck (get-challenger))))
    (click-prompt state :challenger (find-card "Making an Entrance" (:deck (get-challenger)))) ;this is the top card on stack
    (click-prompt state :challenger "Done")
    (is (= "Making an Entrance" (:title (first (:deck (get-challenger))))))
    (is (= "Sure Gamble" (:title (second (:deck (get-challenger))))))
    (is (= "Corroder" (:title (second (rest (:deck (get-challenger)))))))
    (is (= "Patron" (:title (second (rest (rest (:deck (get-challenger))))))))
    (core/draw state :challenger)
    (is (= "Making an Entrance" (:title (first (:hand (get-challenger))))))
    (is (= 1 (count (:hand (get-challenger)))))
    (play-from-hand state :challenger "Making an Entrance")
    (is (= 1 (count (:hand (get-challenger)))) "Can only play on first click")))

(deftest mars-for-martians
  ;; Mars for Martians - Full test
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Mars for Martians" "Clan Vengeance" "Counter Surveillance"
                               "Jarogniew Mercs" (qty "Sure Gamble" 3)]))
    (starting-hand state :challenger ["Mars for Martians" "Clan Vengeance" "Counter Surveillance" "Jarogniew Mercs"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Clan Vengeance")
    (play-from-hand state :challenger "Counter Surveillance")
    (play-from-hand state :challenger "Jarogniew Mercs")
    (play-from-hand state :challenger "Mars for Martians")
    (is (= 1 (:click (get-challenger))) "Mars for Martians not played, priority event")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (core/gain state :challenger :tag 4)
    (is (= 5 (:tag (get-challenger))) "+1 tag from Jarogniew Mercs")
    (is (= 1 (count (:hand (get-challenger)))))
    (is (= 2 (:credit (get-challenger))))
    (play-from-hand state :challenger "Mars for Martians")
    (is (= 3 (count (:hand (get-challenger)))) "3 clan radicles, +3 cards but -1 for playing Mars for Martians")
    (is (= 7 (:credit (get-challenger))) "5 tags, +5 credits")))

(deftest mobius
  ;; Mobius
  (do-game
    (new-game
      (default-contestant)
      (default-challenger [(qty "Möbius" 3)]))
    (starting-hand state :contestant ["Hedge Fund"])
    (take-credits state :contestant)
    (is (= 5 (:credit (get-challenger))))
    (play-from-hand state :challenger "Möbius")
    (core/no-action state :contestant nil)
    (run-successful state)
    (is (= 5 (:credit (get-challenger))))
    (click-prompt state :challenger "No action")
    (click-prompt state :challenger "Yes")
    (is (= [:rd] (get-in @state [:run :locale])) "Second run on R&D triggered")
    (core/no-action state :contestant nil)
    (run-successful state)
    (click-prompt state :challenger "No action")
    (is (= 9 (:credit (get-challenger))))
    (is (empty? (:prompt (get-challenger))) "No prompt to run a third time")
    (is (not (:run @state)) "Run is over")
    (play-from-hand state :challenger "Möbius")
    (run-jack-out state)
    (is (empty? (:prompt (get-challenger))) "No option to run again on unsuccessful run")))

(deftest modded
  ;; Modded - Place a resource or piece of hazard at a 3 credit discount
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Modded" 2)
                               "HQ Interface"
                               "Nerve Agent"
                               "Earthrise Hotel"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Modded")
    (click-card state :challenger (find-card "Earthrise Hotel" (:hand (get-challenger))))
    (is (empty? (get-radicle state)) "Can't place radicles with Modded")
    (click-card state :challenger (find-card "HQ Interface" (:hand (get-challenger))))
    (is (= 1 (count (get-hazard state))) "Placed HQ Interface")
    (is (= 4 (:credit (get-challenger))) "Paid 1 credit instead of 4")
    (play-from-hand state :challenger "Modded")
    (click-card state :challenger (find-card "Nerve Agent" (:hand (get-challenger))))
    (is (= 1 (count (get-resource state))) "Placed Nerve Agent")
    (is (= 4 (:credit (get-challenger))) "Paid 0 credits")))

(deftest the-noble-path
  ;; The Noble Path - Prevents damage during run
  (do-game
    (new-game (default-contestant) (default-challenger ["The Noble Path" (qty "Sure Gamble" 2)]))
    (let [hand-count #(count (:hand (get-challenger)))]
      (starting-hand state :challenger ["The Noble Path" "Sure Gamble"])
      (take-credits state :contestant)
      ;; Play The Noble Path and confirm it discards remaining cards in hand
      (is (= 2 (hand-count)) "Start with 2 cards")
      (play-from-hand state :challenger "The Noble Path")
      (is (zero? (hand-count)) "Playing Noble Path discards the remaining cards in hand")
      ;; Put a card into hand so I can confirm it's not discarded by damage
      ;; Don't want to dealing with checking damage on a zero card hand
      (starting-hand state :challenger ["Sure Gamble"])
      (core/damage state :challenger :net 1)
      (is (= 1 (hand-count)) "Damage was prevented")
      ;; Finish the run and check that damage works again
      (click-prompt state :challenger "HQ")
      (run-successful state)
      (click-prompt state :challenger "No action")
      (core/damage state :challenger :net 1)
      (is (zero? (hand-count)) "Damage works again after run"))))

(deftest notoriety
  ;; Notoriety - Run all 3 central locales successfully and play to gain 1 agenda point
  (do-game
    (new-game (default-contestant ["Hedge Fund"])
              (default-challenger ["Notoriety"]))
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (run-empty-locale state "Archives")
    (run-empty-locale state "R&D")
    (run-empty-locale state "HQ")
    (play-from-hand state :challenger "Notoriety")
    (is (= 1 (count (:scored (get-challenger)))) "Notoriety moved to score area")
    (is (= 1 (:agenda-point (get-challenger))) "Notoriety scored for 1 agenda point")))

(deftest offcharacter-supplies
  ;; Offcharacter Supplies
  (letfn [(offcharacter-supplies-test [link]
            (do-game
              (new-game (default-contestant)
                        (default-challenger [(qty "Offcharacter Supplies" 2)
                                         (qty "Access to Globalsec" 100)]))
              (take-credits state :contestant)
              (core/gain state :challenger :credit 1000 :click link)
              (starting-hand state :challenger (concat (repeat 2 "Offcharacter Supplies")
                                                   (repeat 4 "Access to Globalsec")))
              (dotimes [_ link]
                (play-from-hand state :challenger "Access to Globalsec"))
              (let [credits (:credit (get-challenger))]
                (play-from-hand state :challenger "Offcharacter Supplies")
                (is (= (- credits (- 4 link)) (:credit (get-challenger)))))
              (let [credits (:credit (get-challenger))]
                (click-prompt state :challenger "Gain 4 [Credits]")
                (is (= (+ 4 credits) (:credit (get-challenger))) (str "Challenger should gain " (utils/quantify link "credit"))))
              (play-from-hand state :challenger "Offcharacter Supplies")
              (let [grip (-> (get-challenger) :hand count)]
                (click-prompt state :challenger "Draw 4 cards")
                (is (= (+ 4 grip) (-> (get-challenger) :hand count)) "Challenger should draw 4 cards"))))]
    (doall (map offcharacter-supplies-test (range 5)))))

(deftest on-the-lam
  ;; On the Lam
  (testing "vs tags"
    (do-game
      (new-game (default-contestant ["SEA Source"])
                (default-challenger ["Daily Casts" "On the Lam"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Daily Casts")
      (play-from-hand state :challenger "On the Lam")
      (click-card state :challenger (get-radicle state 0))
      (run-empty-locale state "Archives")
      (take-credits state :challenger)
      (play-from-hand state :contestant "SEA Source")
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (card-ability state :challenger (-> (get-radicle state 0) :hosted first) 0)
      (click-prompt state :challenger "Done")
      (is (zero? (:tag (get-challenger))) "Challenger should avoid tag")
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should have 1 card in Heap")))
  (testing "vs damage"
    (do-game
      (new-game (default-contestant ["Show of Force"])
                (default-challenger ["Daily Casts" "On the Lam"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Daily Casts")
      (play-from-hand state :challenger "On the Lam")
      (click-card state :challenger (get-radicle state 0))
      (take-credits state :challenger)
      (play-and-score state "Show of Force")
      (card-ability state :challenger (-> (get-radicle state 0) :hosted first) 1)
      (click-prompt state :challenger "Done")
      (is (zero? (:tag (get-challenger))) "Challenger should avoid all meat damage")
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should have 1 card in Heap"))))

(deftest out-of-the-ashes
  ;; Out of the Ashes - ensure card works when played/discarded/milled
  (do-game
    (new-game (default-contestant ["Kala Ghoda Real TV" "Underway Renovation"])
              (default-challenger [(qty "Out of the Ashes" 6)]))
    (play-from-hand state :contestant "Underway Renovation" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Out of the Ashes")
    (click-prompt state :challenger "Archives")
    (is (:run @state))
    (run-successful state)
    (discard-from-hand state :challenger "Out of the Ashes")
    (discard-from-hand state :challenger "Out of the Ashes")
    (discard-from-hand state :challenger "Out of the Ashes")
    (discard-from-hand state :challenger "Out of the Ashes")
    (is (zero? (count (:hand (get-challenger)))))
    (is (= 5 (count (:discard (get-challenger)))))
    (take-credits state :challenger)
    (let [underway (get-content state :party1 0)]
      (core/advance state :contestant {:card (refresh underway)}))
    (is (= 6 (count (:discard (get-challenger)))))
    (take-credits state :contestant)
    ;; remove 5 Out of the Ashes from the game
    (dotimes [_ 5]
      (is (not (empty? (get-in @state [:challenger :prompt]))))
      (click-prompt state :challenger "Yes")
      (click-prompt state :challenger "Archives")
      (is (:run @state))
      (run-successful state))
    (click-prompt state :challenger "No")
    (is (= 1 (count (:discard (get-challenger)))))
    (is (= 5 (count (:rfg (get-challenger)))))
    (take-credits state :challenger)
    (take-credits state :contestant)
    ;; ensure that if you decline the rfg, game will still ask the next turn
    (is (not (empty? (get-in @state [:challenger :prompt]))))
    (click-prompt state :challenger "Yes")
    (click-prompt state :challenger "Archives")
    (is (:run @state))
    (run-successful state)
    (is (zero? (count (:discard (get-challenger)))))
    (is (= 6 (count (:rfg (get-challenger)))))))

(deftest peace-in-our-time
  ;; Peace in Our Time - challenger gains 10, contestant gains 5. No runs allowed during turn.
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Peace in Our Time"]))
    (take-credits state :contestant)
    (is (= 8 (:credit (get-contestant))) "Contestant starts with 8 credits")
    (is (= 5 (:credit (get-challenger))) "Challenger starts with 5 credits")
    (play-from-hand state :challenger "Peace in Our Time")
    (is (= 13 (:credit (get-contestant))) "Contestant gains 5 credits")
    (is (= 14 (:credit (get-challenger))) "Challenger gains 10 credits")
    (run-on state "HQ")
    (is (not (:run @state)) "Not allowed to make a run")))

(deftest political-graffiti
  ;; Political Graffiti - swapping with Turntable works / purging viruses restores points
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Breaking News" "Chronos Project"])
                (default-challenger ["Turntable" "Political Graffiti"]))
      (play-from-hand state :contestant "Breaking News" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (is (= 1 (:agenda-point (get-contestant))))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Political Graffiti")
      (is (= [:archives] (get-in @state [:run :locale])) "Run initiated on Archives")
      (run-successful state)
      (click-prompt state :challenger "Replacement effect")
      (click-card state :challenger (find-card "Breaking News" (:scored (get-contestant))))
      (is (zero? (:agenda-point (get-contestant))) "Political Dealings lowered agenda points by 1")
      (play-from-hand state :challenger "Turntable")
      (run-empty-locale state "HQ")
      (click-prompt state :challenger "Steal")
      (let [tt (get-hazard state 0)]
        (click-prompt state :challenger "Yes")
        (click-card state :challenger (find-card "Breaking News" (:scored (get-contestant))))
        (is (= 1 (:agenda-point (get-contestant))))
        (is (zero? (:agenda-point (get-challenger))))
        (take-credits state :challenger)
        (core/purge state :contestant)
        (is (= 1 (:agenda-point (get-contestant))))
        (is (= 1 (:agenda-point (get-challenger)))))))
  (testing "forfeiting agenda with Political Graffiti does not refund double points. Issue #2765"
    (do-game
      (new-game (default-contestant ["Hostile Takeover" "Sacrifcharacter"])
                (default-challenger ["Political Graffiti"]))
      (play-from-hand state :contestant "Hostile Takeover" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (is (= 1 (:agenda-point (get-contestant))))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Political Graffiti")
      (is (= [:archives] (get-in @state [:run :locale])) "Run initiated on Archives")
      (run-successful state)
      (click-prompt state :challenger "Replacement effect")
      (click-card state :challenger (find-card "Hostile Takeover" (:scored (get-contestant))))
      (is (zero? (:agenda-point (get-contestant))) "Political Dealings lowered agenda points by 1")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Sacrifcharacter")
      (click-card state :contestant (get-scored state :contestant 0))
      (is (zero? (:agenda-point (get-contestant))) "Forfeiting agenda did not refund extra agenda points ")
      (is (= 1 (count (:discard (get-challenger)))) "Political Graffiti is in the Heap"))))

(deftest power-to-the-people
  ;; Power to the People - Gain 7c the first time you access an agenda
  (do-game
    (new-game (default-contestant ["NAPD Contract" "Hostile Takeover"])
              (default-challenger ["Power to the People"]))
    (play-from-hand state :contestant "NAPD Contract" "New party")
    (take-credits state :contestant)
    (core/lose state :challenger :credit 2)
    (let [napd (get-content state :party1 0)]
      (play-from-hand state :challenger "Power to the People")
      (is (= 3 (:credit (get-challenger))) "Can't afford to steal NAPD")
      (run-empty-locale state "Locale 1")
      (is (= 10 (:credit (get-challenger))) "Gained 7c on access, can steal NAPD")
      (click-prompt state :challenger "Pay 4 [Credits] to steal")
      (is (= 2 (:agenda-point (get-challenger))) "Stole agenda")
      (is (= 6 (:credit (get-challenger))))
      (run-empty-locale state "HQ")
      (click-prompt state :challenger "Steal")
      (is (= 6 (:credit (get-challenger))) "No credits gained from 2nd agenda access"))))

(deftest push-your-luck
  ;; Push Your Luck
  (testing "Contestant guesses correctly"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Push Your Luck"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Push Your Luck")
      (click-prompt state :contestant "Odd")
      (click-prompt state :challenger "3")
      (is (zero? (:credit (get-challenger))) "Contestant guessed correctly")))
  (testing "Contestant guesses incorrectly"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Push Your Luck"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Push Your Luck")
      (click-prompt state :contestant "Even")
      (click-prompt state :challenger "3")
      (is (= 6 (:credit (get-challenger))) "Contestant guessed incorrectly"))))

(deftest pushing-the-envelope
  ;; Run. Add 2 strength to each placeer breaker.
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Pushing the Envelope" 3) (qty "Corroder" 2) "Atman"]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 20)
    (core/gain state :challenger :click 10)
    (core/draw state :challenger)
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "Atman")
    (click-prompt state :challenger "0")
    (let [atman (get-resource state 1)
          corr (get-resource state 0)]
      (is (zero? (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (play-from-hand state :challenger "Pushing the Envelope")
      (click-prompt state :challenger "Archives")
      ; 3 cards in hand - no boost
      (is (zero? (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (run-successful state)
      (play-from-hand state :challenger "Pushing the Envelope")
      (click-prompt state :challenger "Archives")
      (run-continue state)
      ; 2 cards in hand - boost
      (is (= 2 (:current-strength (refresh atman))) "Atman 2 current strength")
      (is (= 4 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (run-successful state)
      (is (zero? (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength"))))

(deftest ^{:card-title "queen's-gambit"}
  queens-gambit
  ;; Check that Queen's Gambit prevents access of card #1542
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 2)])
              (default-challenger ["Queen's Gambit"]))
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Queen's Gambit")
    (let [pad (get-content state :party1 0)
          challenger-creds (:credit (get-challenger))]
      (click-prompt state :challenger "3")
      (click-card state :challenger pad)
      (is (= (+ challenger-creds 6) (:credit (get-challenger))) "Gained 6 credits from Queen's Gambit")
      (is (= 3 (get-counters (refresh pad) :advancement)) "3 advancement counters placed on PAD Campaign by Queen's Gambit")
      (is (not (core/can-access? state :challenger (refresh pad))) "Cannot access PAD Campgain")
      (run-empty-locale state "Locale 1")
      (is (not (:run @state)) "Run ended since no cards could be accessed"))
    (let [other-pad (get-content state :party2 0)]
      (is (core/can-access? state :challenger other-pad)) "Not prevented from accessing other cards")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (let [pad (get-content state :party1 0)
          challenger-creds (:credit (get-challenger))]
      (run-empty-locale state "Locale 1")
      (is (core/can-access? state :challenger (refresh pad)) "Can access PAD Campgain next turn")
      (click-prompt state :challenger "Pay 4 [Credits] to discard")
      (is (= (- challenger-creds 4) (:credit (get-challenger))) "Paid 4 credits to discard PAD Campaign"))))

;; Rebirth
(let [akiko "Akiko Nisei: Head Case"
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
        (click-prompt state :challenger kate)
        (is (= kate (-> (get-challenger) :identity :title)))
        (is (= 1 (:link (get-challenger))) "1 link")
        (is (empty? (:discard (get-challenger))))
        (is (= "Rebirth" (-> (get-challenger) :rfg first :title)))
        (is (changes-credits (get-challenger) -4
                             (play-from-hand state :challenger "Magnum Opus")))))
    (testing "Whizzard works after rebirth"
      (do-game
        (new-game (default-contestant ["Ice Wall"]) (make-deck reina ["Rebirth"]))
        (play-from-hand state :contestant "Ice Wall" "R&D")
        (take-credits state :contestant)
        (play-from-hand state :challenger "Rebirth")
        (click-prompt state :challenger whizzard)
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
        (click-prompt state :challenger chaos)
        (is (= 1 (:link (get-challenger))) "1 link after rebirth")))
    (testing "Gain link from ID"
      (do-game
        (new-game (default-contestant)
                  (default-challenger ["Rebirth" "Access to Globalsec"])
                  {:start-as :challenger})
        (play-from-hand state :challenger "Access to Globalsec")
        (is (= 1 (:link (get-challenger))) "1 link before rebirth")
        (play-from-hand state :challenger "Rebirth")
        (click-prompt state :challenger kate)
        (is (= 2 (:link (get-challenger))) "2 link after rebirth")))
    (testing "Implementation notes are kept, regression test for #3722"
      (do-game
        (new-game (default-contestant)
                  (default-challenger ["Rebirth"])
                  {:start-as :challenger})
        (play-from-hand state :challenger "Rebirth")
        (click-prompt state :challenger chaos)
        (is (= :full (get-in (get-challenger) [:identity :implementation])) "Implementation note kept as `:full`"))))
  (deftest rebirth-kate-twcharacter
    ;; Rebirth - Kate does not give discount after rebirth if Hazard or Resource already placed
    (testing "Placing Hazard before does prevent discount"
      (do-game
        (new-game (default-contestant)
                  (default-challenger ["Akamatsu Mem Chip" "Rebirth" "Clone Chip"])
                  {:start-as :challenger})
        (play-from-hand state :challenger "Clone Chip")
        (play-from-hand state :challenger "Rebirth")
        (click-prompt state :challenger kate)
        (is (= kate (get-in (get-challenger) [:identity :title])) "Rebirthed into Kate")
        (is (changes-credits (get-challenger) -1
                             (play-from-hand state :challenger "Akamatsu Mem Chip"))
            "Discount not applied for 2nd place")))
    (testing "Placing Radicle before does not prevent discount"
      (do-game
        (new-game (default-contestant)
                  (default-challenger ["Akamatsu Mem Chip" "Rebirth" "Same Old Thing"])
                  {:start-as :challenger})
        (play-from-hand state :challenger "Same Old Thing")
        (play-from-hand state :challenger "Rebirth")
        (click-prompt state :challenger kate)
        (is (= kate (get-in (get-challenger) [:identity :title])) "Rebirthed into Kate")
        (is (changes-credits (get-challenger) 0
                             (play-from-hand state :challenger "Akamatsu Mem Chip"))
            "Discount is applied for 2nd place (since it is the first Hazard / Resource)"))))
  (deftest rebirth-reina-twcharacter
    ;; Rebirth - Reina does not increase reveal cost after rebirth if Ice already revealed
    (testing "Revealing Ice before does prevent cost"
      (do-game
        (new-game (default-contestant [(qty "Ice Wall" 2)])
                  (make-deck whizzard ["Rebirth"]))
        (play-from-hand state :contestant "Ice Wall" "HQ")
        (play-from-hand state :contestant "Ice Wall" "R&D")
        (take-credits state :contestant)
        (is (changes-credits (get-contestant) -1
                             (core/reveal state :contestant (get-character state :hq 0)))
            "Only pay 1 to reveal character wall when against Whizzard")
        (play-from-hand state :challenger "Rebirth")
        (click-prompt state :challenger reina)
        (is (= reina (get-in (get-challenger) [:identity :title])) "Rebirthed into Reina")
        (is (changes-credits (get-contestant) -1
                             (core/reveal state :contestant (get-character state :rd 0)))
            "Additional cost from Reina not applied for 2nd character reveal")))
    (testing "Revealing Site before does not prevent additional cost"
      (do-game
        (new-game (default-contestant ["Ice Wall" "Mark Yale"])
                  (make-deck whizzard ["Rebirth"]))
        (println "Reina Rebirth twcharacter test")
        (play-from-hand state :contestant "Ice Wall" "HQ")
        (play-from-hand state :contestant "Mark Yale" "New party")
        (take-credits state :contestant)
        (is (changes-credits (get-contestant) -1
                             (core/reveal state :contestant (get-content state :party1 0)))
            "Only pay 1 to reveal Mark Yale")
        (play-from-hand state :challenger "Rebirth")
        (click-prompt state :challenger reina)
        (is (= reina (get-in (get-challenger) [:identity :title])) "Rebirthed into Reina")
        (is (changes-credits (get-contestant) -2
                             (core/reveal state :contestant (get-character state :hq 0)))
            "Additional cost from Reina applied for 1st character reveal")))))

(deftest reboot
  ;; Reboot - run on Archives, place 5 cards from head facedown
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Reboot" "Sure Gamble" "Paperclip" "Clot"]))
    (take-credits state :contestant)
    (discard-from-hand state :challenger "Sure Gamble")
    (discard-from-hand state :challenger "Paperclip")
    (discard-from-hand state :challenger "Clot")
    (is (empty? (core/all-placed state :challenger)) "Challenger starts with no placed cards")
    (is (= 3 (count (:discard (get-challenger)))) "Challenger starts with 3 cards in discard")
    (is (empty? (:rfg (get-challenger))) "Challenger starts with no discarded cards")
    (play-from-hand state :challenger "Reboot")
    (run-successful state)
    (click-card state :challenger (find-card "Sure Gamble" (:discard (get-challenger))))
    (click-card state :challenger (find-card "Paperclip" (:discard (get-challenger))))
    (click-card state :challenger (find-card "Clot" (:discard (get-challenger))))
    (click-prompt state :challenger "Done")
    (is (= 3 (count (filter :facedown (core/all-placed state :challenger)))) "Challenger has 3 facedown cards")
    (is (= 3 (count (core/all-placed state :challenger))) "Challenger has no other cards placed")
    (is (empty? (:discard (get-challenger))) "Challenger has empty discard")
    (is (= 1 (count (:rfg (get-challenger)))) "Challenger has 1 card in RFG")))

(deftest reshape
  ;; Reshape - Swap 2 pieces of unrevealed Character
  (do-game
    (new-game (default-contestant [(qty "Vanilla" 2) "Paper Wall"])
              (default-challenger ["Reshape"]))
    (play-from-hand state :contestant "Paper Wall" "R&D")
    (play-from-hand state :contestant "Vanilla" "HQ")
    (play-from-hand state :contestant "Vanilla" "HQ")
    (core/reveal state :contestant (get-character state :hq 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Reshape")
    (click-card state :challenger (get-character state :rd 0))
    (click-card state :challenger (get-character state :hq 0))
    (is (:prompt (get-challenger)) "Can't target revealed Vanilla, prompt still open")
    (click-card state :challenger (get-character state :hq 1))
    (is (empty? (:prompt (get-challenger))))
    (is (= "Vanilla" (:title (get-character state :rd 0))) "Vanilla swapped to R&D")
    (is (= "Paper Wall" (:title (get-character state :hq 1))) "Paper Wall swapped to HQ outer position")))

(deftest retrieval-run
  ;; Retrieval Run - Run Archives successfully and place a resource from Heap for free
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Retrieval Run" "Morning Star"]))
    (take-credits state :contestant)
    (discard-from-hand state :challenger "Morning Star")
    (play-from-hand state :challenger "Retrieval Run")
    (is (= [:archives] (get-in @state [:run :locale])) "Run initiated on Archives")
    (run-successful state)
    (click-prompt state :challenger "Replacement effect")
    (let [ms (first (:discard (get-challenger)))]
      (click-prompt state :challenger ms)
      (is (= "Morning Star" (:title (first (get-resource state))))
          "Morning Star placed")
      (is (= 2 (:credit (get-challenger))) "Morning Star placed at no cost")
      (is (= 2 (core/available-mu state)) "Morning Star uses 2 memory"))))

(deftest rigged-results
  ;; Rigged Results - success and failure
  (do-game
    (new-game (default-contestant ["Ice Wall"])
              (default-challenger [(qty "Rigged Results" 3)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Rigged Results")
    (click-prompt state :challenger "0")
    (click-prompt state :contestant "0")
    (is (empty? (:prompt (get-challenger))) "Rigged Results failed for challenger")
    (is (empty? (:prompt (get-contestant))) "Rigged Results failed for challenger")
    (play-from-hand state :challenger "Rigged Results")
    (click-prompt state :challenger "2")
    (click-prompt state :contestant "1")
    (click-card state :challenger (get-character state :hq 0))
    (is (= [:hq] (:locale (:run @state))) "Challenger is running on HQ")
    (is (= 3 (:credit (get-challenger))) "Rigged results spends credits")))

(deftest rip-deal
  ;; Rip Deal - replaces number of HQ accesses with heap retrieval
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Crisium Grid" 2)(qty "Vanilla" 2)])
                (default-challenger ["The Gauntlet" "Rip Deal" (qty "Easy Mark" 2)]))
      (discard-from-hand state :challenger "Easy Mark")
      (discard-from-hand state :challenger "Easy Mark")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Rip Deal")
      (run-successful state)
      (click-prompt state :challenger "Replacement effect")
      (is (= "Choose 1 card(s) to move from the Heap to your Grip" (-> (get-challenger) :prompt first :msg)))))
  (testing "with Gauntlet #2942"
    (do-game
      (new-game (default-contestant [(qty "Crisium Grid" 2)(qty "Vanilla" 2)])
                (default-challenger ["The Gauntlet" "Rip Deal" (qty "Easy Mark" 2)]))
      (discard-from-hand state :challenger "Easy Mark")
      (discard-from-hand state :challenger "Easy Mark")
      (play-from-hand state :contestant "Vanilla" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 4)
      (play-from-hand state :challenger "The Gauntlet")
      (play-from-hand state :challenger "Rip Deal")
      (run-successful state)
      (click-prompt state :challenger "1")
      (click-prompt state :challenger "Replacement effect")
      (is (= "Choose 2 card(s) to move from the Heap to your Grip" (-> (get-challenger) :prompt first :msg))))))

(deftest rumor-mill
  ;; Rumor Mill - interactions with reveal effects, additional costs, general event handlers, and discard-effects
  (testing "Full test"
    (do-game
      (new-game
        (default-contestant [(qty "Project Atlas" 2)
                       "Caprcharacter Nisei" "Chairman Hiro" "Cybernetics Court"
                       "Elizabeth Mills" "Ibrahim Salem"
                       "Housekeeping" "Director Haas" "Oberth Protocol"])
        (default-challenger ["Rumor Mill"]))
      (core/gain state :contestant :credit 100 :click 100 :bad-publicity 1)
      (core/draw state :contestant 100)
      (play-from-hand state :contestant "Caprcharacter Nisei" "New party")
      (play-from-hand state :contestant "Chairman Hiro" "New party")
      (play-from-hand state :contestant "Cybernetics Court" "New party")
      (play-from-hand state :contestant "Elizabeth Mills" "New party")
      (play-from-hand state :contestant "Project Atlas" "New party")
      (play-from-hand state :contestant "Ibrahim Salem" "New party")
      (play-from-hand state :contestant "Oberth Protocol" "New party")
      (core/move state :contestant (find-card "Director Haas" (:hand (get-contestant))) :deck)
      (core/reveal state :contestant (get-content state :party2 0))
      (core/reveal state :contestant (get-content state :party3 0))
      (score-agenda state :contestant (get-content state :party5 0))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 100 :click 100)
      (is (= 4 (get-in (get-contestant) [:hand-size :mod])) "Contestant has +4 hand size")
      (is (= -2 (get-in (get-challenger) [:hand-size :mod])) "Challenger has -2 hand size")
      (play-from-hand state :challenger "Rumor Mill")
      ;; Additional costs to reveal should NOT be applied
      (core/reveal state :contestant (get-content state :party6 0))
      (is (= 1 (count (:scored (get-contestant)))) "No agenda was auto-forfeit to reveal Ibrahim Salem")
      ;; In-play effects
      (is (zero? (get-in (get-contestant) [:hand-size :mod])) "Contestant has original hand size")
      (is (zero? (get-in (get-challenger) [:hand-size :mod])) "Challenger has original hand size")
      ;; "When you reveal" effects should not apply
      (core/reveal state :contestant (get-content state :party4 0))
      (is (= 1 (:bad-publicity (get-contestant))) "Contestant still has 1 bad publicity")
      ;; Run events (Caprcharacter)
      ;; Make sure Rumor Mill applies even if card is revealed after RM is put in play.
      (core/reveal state :contestant (get-content state :party1 0))
      (run-on state :party1)
      (run-continue state)
      (is (empty? (:prompt (get-contestant))) "Caprcharacter prompt is not showing")
      (run-jack-out state)
      ;; Discardable execs
      (run-empty-locale state :party2)
      (click-prompt state :challenger "Pay 6 [Credits] to discard")
      (is (empty? (:scored (get-challenger))) "Chairman Hiro not added to challenger's score area")
      (run-jack-out state)
      (run-on state "R&D")
      (run-successful state)
      (click-prompt state :challenger "Pay 5 [Credits] to discard")
      (is (empty? (:scored (get-challenger))) "Director Haas not added to challenger's score area")
      (take-credits state :challenger)
      ;; Discard RM, make sure everything works again
      (play-from-hand state :contestant "Housekeeping")
      (is (= 4 (get-in (get-contestant) [:hand-size :mod])) "Contestant has +4 hand size")
      (is (zero? (get-in (get-challenger) [:hand-size :mod])) "Challenger has +0 hand size")
      ;; Additional costs to reveal should now be applied again
      (core/reveal state :contestant (get-content state :party7 0))
      (click-card state :contestant (get-in (get-contestant) [:scored 0]))
      (is (zero? (count (:scored (get-contestant)))) "Agenda was auto-forfeit to reveal Oberth")
      (core/hide state :contestant (get-content state :party4 0))
      (core/reveal state :contestant (get-content state :party4 0))
      (is (zero? (:bad-publicity (get-contestant))) "Contestant has 0 bad publicity")
      (card-ability state :contestant (get-content state :party4 0) 0) ; Elizabeth Mills, should show a prompt
      (is (:prompt (get-contestant)) "Elizabeth Mills ability allowed")))
  (testing "Make sure Rumor Mill is not active when hosted on Peddler"
    (do-game
      (new-game (default-contestant ["Jeeves Model Bioroids"])
                (default-challenger ["Street Peddler" (qty "Rumor Mill" 3)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Street Peddler"])
      (play-from-hand state :challenger "Street Peddler")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Jeeves Model Bioroids" "New party")
      (let [jeeves (get-content state :party1 0)]
        (core/reveal state :contestant jeeves)
        (card-ability state :contestant jeeves 0)
        (is (= 3 (:click (get-contestant))) "Contestant has 3 clicks - Jeeves working ok")))))

(deftest scrubbed
  ;; First piece of character encountered each turn has -2 Strength for remainder of the run
  (do-game
    (new-game (default-contestant ["Turing"])
              (default-challenger ["Street Peddler"
                               (qty "Scrubbed" 3)]))
    (starting-hand state :challenger ["Street Peddler" "Scrubbed"])
    (play-from-hand state :contestant "Turing" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Street Peddler")
    (let [turing (get-character state :hq 0)]
      (core/reveal state :contestant turing)
      (is (= 2 (:current-strength (refresh turing))))
      (run-on state "HQ")
      (run-continue state)
      (is (= 2 (:current-strength (refresh turing))) "Scrubbed not active when on Peddler")
      (play-from-hand state :challenger "Scrubbed")
      (run-on state "HQ")
      (run-continue state)
      (is (zero? (:current-strength (refresh turing))) "Scrubbed reduces strength by 2")
      (run-successful state))))

(deftest singularity
  ;; Singularity - Run a party; if successful, discard all contents at no cost
  (do-game
    (new-game (default-contestant ["Caprcharacter Nisei"
                             "Breaker Bay Grid"
                             "Eve Campaign"])
              (default-challenger ["Singularity"]))
    (play-from-hand state :contestant "Breaker Bay Grid" "New party")
    (play-from-hand state :contestant "Caprcharacter Nisei" "Locale 1")
    (play-from-hand state :contestant "Eve Campaign" "Locale 1")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Singularity")
    (click-prompt state :challenger "Locale 1")
    (is (= 2 (:click (get-challenger))) "Challenger spends 2 clicks on double event")
    (is (= 1 (:credit (get-challenger))) "Challenger pays 4 credits for Singularity")
    (run-successful state)
    (is (= 3 (count (:discard (get-contestant)))) "All 3 cards discarded from Locale 1")
    (is (= 1 (:credit (get-challenger))) "No credits paid for discarding")
    (is (nil? (get-in @state [:contestant :locales :party1 :content])) "Locale 1 no longer exists")))

(deftest stimhack
  ;; Stimhack - Gain 9 temporary credits and take 1 brain damage after the run
  (do-game
    (new-game (default-contestant ["Eve Campaign"])
              (default-challenger ["Stimhack" "Sure Gamble"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Stimhack")
    (click-prompt state :challenger "HQ")
    (is (= [:hq] (get-in @state [:run :locale])) "Run initiated on HQ")
    (run-successful state)
    (is (= 14 (:credit (get-challenger))))
    (is (= 9 (:run-credit (get-challenger))) "Gained 9 credits for use during the run")
    (click-prompt state :challenger "Pay 5 [Credits] to discard") ; choose to discard Eve
    (is (and (zero? (count (:hand (get-contestant))))
             (= 1 (count (:discard (get-contestant)))))
        "Contestant hand empty and Eve in Archives")
    (is (= 5 (:credit (get-challenger))))
    (is (zero? (count (:hand (get-challenger)))) "Lost card from Grip to brain damage")
    (is (= 4 (core/hand-size state :challenger)))
    (is (= 1 (:brain-damage (get-challenger))))))

(deftest sure-gamble
  ;; Sure Gamble
  (do-game
    (new-game (default-contestant) (default-challenger ["Sure Gamble"]))
    (take-credits state :contestant)
    (is (= 5 (:credit (get-challenger))))
    (play-from-hand state :challenger "Sure Gamble")
    (is (= 9 (:credit (get-challenger))))))

(deftest surge
  ;; Surge - Add counters if target is a virus and had a counter added this turn
  (testing "Valid target"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Imp" "Surge"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Imp")
      (let [imp (get-resource state 0)]
        (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after place")
        (play-from-hand state :challenger "Surge")
        (click-card state :challenger imp)
        (is (= 4 (get-counters (refresh imp) :virus)) "Imp has 4 counters after surge"))))
  (testing "Don't fire surge if target is not a virus"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Security Testing" "Surge"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Security Testing")
      (let [st (get-radicle state 0)]
        (play-from-hand state :challenger "Surge")
        (click-card state :challenger st)
        (is (not (contains? st :counter)) "Surge does not fire on Security Testing"))))
  (testing "Don't fire surge if target does not have virus counter flag set"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Imp" "Surge"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Imp")
      (let [imp (get-resource state 0)]
        (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after place")
        (take-credits state :challenger 3)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Surge")
        (click-card state :challenger imp)
        (is (= 2 (get-counters (refresh imp) :virus)) "Surge does not fire on Imp turn after place"))))
  (testing "Don't allow surging Gorman Drip, since it happens on the contestant turn"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Gorman Drip v1" "Surge"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Gorman Drip v1")
      (let [gd (get-resource state 0)]
        (is (zero? (get-counters gd :virus)) "Gorman Drip starts without counters")
        (take-credits state :challenger 3)
        (take-credits state :contestant)
        (is (= 3 (get-counters (refresh gd) :virus))
            "Gorman Drip gains 3 counters after Contestant clicks 3 times for credits")
        (play-from-hand state :challenger "Surge")
        (click-card state :challenger gd)
        (is (= 3 (get-counters (refresh gd) :virus)) "Surge does not trigger on Gorman Drip")))))

(deftest system-outage
  ;; When Contestant draws 1+ cards, it loses 1 if it is not the first time he or she has drawn cards this turn
  (do-game
    (new-game (default-contestant [(qty "Turing" 10)])
              (default-challenger ["Street Peddler"
                               (qty "System Outage" 3)]))
    (starting-hand state :contestant [])
    (starting-hand state :challenger ["Street Peddler" "System Outage"])
    (take-credits state :contestant) ; contestant at 8cr
    (play-from-hand state :challenger "Street Peddler")
    (take-credits state :challenger)
    (core/click-draw state :contestant 1)
    (is (= 8 (:credit (get-contestant))) "1st card drawn for free - System Outage on Peddler")
    (core/click-draw state :contestant 1)
    (is (= 8 (:credit (get-contestant))) "2nd card drawn for free - System Outage on Peddler")
    (take-credits state :contestant) ; contestant at 9cr
    (is (= 9 (:credit (get-contestant))) "Contestant at 9")
    (play-from-hand state :challenger "System Outage")
    (take-credits state :challenger)
    (core/click-draw state :contestant 1)
    (is (= 8 (:credit (get-contestant))) "1st card drawn cost 1cr - System Outage active")
    (core/click-draw state :contestant 1)
    (is (= 7 (:credit (get-contestant))) "2nd card drawn cost 1cr - System Outage active")))

(deftest system-seizure
  ;; System Seizure - First characterbreaker boosted keeps strength for remainder of that run.
  (do-game
    (new-game (default-contestant ["Wraparound"])
              (default-challenger [(qty "Corroder" 2) "System Seizure"]))
    (play-from-hand state :contestant "Wraparound" "HQ")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 3)
    (core/gain state :challenger :click 2)
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "System Seizure")
    (let [c1 (get-resource state 0)
          c2  (get-resource state 1)]
      (run-empty-locale state "R&D") ;; Check that System Seizure triggers even if another run has been made
      (run-on state "HQ") ;; Check that System Seizure only keeps strength on one of the breakers
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c2))) "Corroder 2 has 2 strength")
      (card-ability state :challenger c1 1)
      (card-ability state :challenger c2 1)
      (is (= 3 (core/breaker-strength state :challenger (core/get-card state c1))) "Corroder 1 has 3 strength")
      (is (= 3 (core/breaker-strength state :challenger (core/get-card state c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 3 (core/breaker-strength state :challenger (core/get-card state c1))) "Corroder 1 has 3 strength")
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c2))) "Corroder 2 has 2 strength")
      (run-successful state)
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c2))) "Corroder 2 has 2 strength")
      (run-on state "HQ") ;; Check that System Seizure does not keep strength on 2nd run
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c2))) "Corroder 2 has 2 strength")
      (card-ability state :challenger c1 1)
      (card-ability state :challenger c2 1)
      (is (= 3 (core/breaker-strength state :challenger (core/get-card state c1))) "Corroder 1 has 3 strength")
      (is (= 3 (core/breaker-strength state :challenger (core/get-card state c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c2))) "Corroder 2 has 2 strength")
      (run-successful state)
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :challenger (core/get-card state c2))) "Corroder 2 has 2 strength"))))

(deftest test-run
  ;; Test Run - Resources hosted after place get returned to Stack. Issue #1081
  (do-game
    (new-game (default-contestant ["Wraparound"])
              (default-challenger [(qty "Test Run" 2) "Morning Star"
                               "Knight" "Leprechaun"]))
    (play-from-hand state :contestant "Wraparound" "HQ")
    (let [wrap (get-character state :hq 0)]
      (core/reveal state :contestant wrap)
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5)
      (core/move state :challenger (find-card "Morning Star" (:hand (get-challenger))) :discard)
      (core/move state :challenger (find-card "Knight" (:hand (get-challenger))) :discard)
      (let [ms (find-card "Morning Star" (:discard (get-challenger)))]
        (play-from-hand state :challenger "Leprechaun")
        (play-from-hand state :challenger "Test Run")
        (click-prompt state :challenger "Heap")
        (click-prompt state :challenger ms)
        (let [lep (get-resource state 0)
              ms (get-resource state 1)]
          (card-ability state :challenger lep 1)
          (click-card state :challenger ms)
          (is (= "Morning Star" (:title (first (:hosted (refresh lep))))) "Morning Star hosted on Lep")
          (take-credits state :challenger)
          (is (= "Morning Star" (:title (first (:deck (get-challenger))))) "Morning Star returned to Stack from host")
          (take-credits state :contestant)
          (let [kn (find-card "Knight" (:discard (get-challenger)))]
            (play-from-hand state :challenger "Test Run")
            (click-prompt state :challenger "Heap")
            (click-prompt state :challenger kn)
            (let [kn (get-resource state 1)]
              (card-ability state :challenger kn 0)
              (click-card state :challenger wrap)
              (is (= "Knight" (:title (first (:hosted (refresh wrap))))) "Knight hosted on Wraparound")
              (take-credits state :challenger)
              (is (= "Knight" (:title (first (:deck (get-challenger))))) "Knight returned to Stack from host Character"))))))))

(deftest test-run
  ;; Test Run
  (testing "Make sure resource remains placed if Scavenged"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Test Run" "Morning Star"
                                 "Scavenge" "Inti"]))
      (take-credits state :contestant)
      (core/move state :challenger (find-card "Morning Star" (:hand (get-challenger))) :discard)
      (play-from-hand state :challenger "Test Run")
      (let [ms (find-card "Morning Star" (:discard (get-challenger)))]
        (click-prompt state :challenger "Heap")
        (click-prompt state :challenger ms)
        (is (= 2 (:credit (get-challenger))) "Resource placed for free")
        (let [ms (get-resource state 0)]
          (play-from-hand state :challenger "Scavenge")
          (click-card state :challenger ms)
          (click-card state :challenger (find-card "Morning Star" (:discard (get-challenger))))
          (take-credits state :challenger)
          (is (empty? (:deck (get-challenger))) "Morning Star not returned to Stack")
          (is (= "Morning Star" (:title (get-resource state 0))) "Morning Star still placed"))))))

(deftest the-maker's-eye
  (do-game
    (new-game (default-contestant [(qty "Quandary" 5)])
              (default-challenger ["The Maker's Eye"]))
    (dotimes [_ 5] (core/move state :contestant (first (:hand (get-contestant))) :deck))
    (take-credits state :contestant)
    (play-from-hand state :challenger "The Maker's Eye")
    (is (= :rd (get-in @state [:run :locale 0])))
    (run-successful state)
    (click-prompt state :challenger "Card from deck")
    (is (= "You accessed Quandary." (-> (get-challenger) :prompt first :msg)) "1st quandary")
    (click-prompt state :challenger "No action")
    (click-prompt state :challenger "Card from deck")
    (is (= "You accessed Quandary." (-> (get-challenger) :prompt first :msg)) "2nd quandary")
    (click-prompt state :challenger "No action")
    (click-prompt state :challenger "Card from deck")
    (is (= "You accessed Quandary." (-> (get-challenger) :prompt first :msg)) "3rd quandary")
    (click-prompt state :challenger "No action")
    (is (not (:run @state)))))

(deftest the-prcharacter-of-freedom
  ;; The Prcharacter of Freedom - A connection must be discarded, the card is removed from game, then the contestant can't advance cards next turn
  (do-game
    (new-game (default-contestant ["NAPD Contract"])
              (default-challenger ["Kati Jones" "The Prcharacter of Freedom"]))
    (play-from-hand state :contestant "NAPD Contract" "New party")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))) "Contestant has 7 credits (play NAPD + 2 clicks for credit")
    (play-from-hand state :challenger "The Prcharacter of Freedom")
    (is (= 2 (count (get-in @state [:challenger :hand]))) "The Prcharacter of Freedom could not be played because no connection is placed")
    (is (zero? (count (get-in (get-challenger) [:rig :radicle]))) "Kati Jones is not placed")
    (play-from-hand state :challenger "Kati Jones")
    (is (= 1 (count (get-radicle state))) "Kati Jones was placed")
    (play-from-hand state :challenger "The Prcharacter of Freedom")
    (let [kj (get-radicle state 0)]
      (click-card state :challenger kj)
      (is (zero? (count (get-in @state [:challenger :hand]))) "The Prcharacter of Freedom can be played because a connection is in play")
      (is (zero? (count (get-in (get-challenger) [:rig :radicle]))) "Kati Jones was discarded wth The Prcharacter of Freedom")
      (is (= 1 (count (get-in (get-challenger) [:discard]))) "The Prcharacter of Freedom was removed from game, and only Kati Jones is in the discard"))
    (take-credits state :challenger)
    (let [napd (get-content state :party1 0)]
      (core/advance state :contestant {:card (refresh napd)})
      (is (= 7 (:credit (get-contestant))) "NAPD contract could not be advanced because of The Prcharacter of Freedom")
      (take-credits state :contestant)
      (is (= 10 (:credit (get-contestant))) "Contestant has 10 credits now (3 clicks for credit, no click charged for failed advancing)")
      (take-credits state :challenger)
      (core/advance state :contestant {:card (refresh napd)})
      (core/advance state :contestant {:card (refresh napd)})
      (core/advance state :contestant {:card (refresh napd)})
      (is (= 7 (:credit (get-contestant))) "NAPD could be advanced (3 credits charged for advancing)"))))

(deftest tinkering
  ;; Tinkering - Add subtypes to character
  (do-game
    (new-game
      (default-contestant ["Ice Wall"])
      (default-challenger ["Tinkering"]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Tinkering")
    (let [iwall (get-character state :hq 0)]
      (click-card state :challenger iwall)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (core/has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (core/reveal state :contestant (refresh iwall))
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (core/has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (take-credits state :challenger)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (not (core/has-subtype? (refresh iwall) "Code Gate")) "Ice Wall does not have Code Gate")
      (is (not (core/has-subtype? (refresh iwall) "Sentry")) "Ice Wall does not have Sentry"))))

(deftest trade-in
  ;; Trade-in - discard an placed Hazard, gain credits equal to half of place cost,
  ;;            search stack for Hazard and add to grip
  (do-game
    (new-game
      (default-contestant)
      (default-challenger [(qty "Trade-In" 3) (qty "Astrolabe" 2) (qty "Sports Hopper" 2)])
      {:start-as :challenger})
    (starting-hand state :challenger ["Trade-In" "Trade-In" "Astrolabe" "Sports Hopper"])
    (core/gain state :challenger :click 5 :credit 5)
    (play-from-hand state :challenger "Astrolabe")
    (play-from-hand state :challenger "Sports Hopper")

    (testing "Trade-in works with Hazard costing 0 or 1 credits (issue #3750)"
      (let [challenger-credits (:credit (get-challenger))]
        (play-from-hand state :challenger "Trade-In")
        (click-card state :challenger (get-hazard state 0))
        (is (= 2 (count (:discard (get-challenger)))) "Trade-In and Astrolabe in discard")
        (is (= (- challenger-credits 1) (:credit (get-challenger)))
            "Paid 1 credit to play Trade-In and gained 0 credits from discarding Astrolabe")))

    (testing "Trade-In lets challenger search for Hazard and add it to Grip"
      (is (= 1 (count (:hand (get-challenger)))) "Only 1 Trade-In in Grip")
      ;; Add sports hopper to hand
      (click-prompt state :challenger (-> (get-challenger) :prompt first :choices first))
      (is (= 2 (count (:hand (get-challenger)))) "Sports Hopper added to Grip"))

    (testing "Gain credits when place cost is greater than 1"
      (let [challenger-credits (:credit (get-challenger))]
        (play-from-hand state :challenger "Trade-In")
        (click-card state :challenger (get-hazard state 0))
        (is (= (+ challenger-credits -1 1) (:credit (get-challenger)))
            "Paid 1 credit to play Trade-In and gained 1 credits from discarding Sports Hopper")
        (is (= 4 (count (:discard (get-challenger)))) "2 Trade-In, 1 Astrolabe and 1 Sports Hopper in discard")))))

(deftest traffic-jam
  ;; Traffic Jam - Increase adv requirement based on previously scored copies
  (do-game
    (new-game
      (default-contestant [(qty "TGTBT" 3)])
      (default-challenger ["Traffic Jam"]))
    (play-from-hand state :contestant "TGTBT" "New party")
    (score-agenda state :contestant (get-content state :party1 0))
    (play-from-hand state :contestant "TGTBT" "New party")
    (score-agenda state :contestant (get-content state :party2 0))
    (play-from-hand state :contestant "TGTBT" "New party")
    (take-credits state :contestant)
    (let [tg (get-content state :party3 0)]
      (play-from-hand state :challenger "Traffic Jam")
      (take-credits state :challenger)
      (core/gain state :contestant :click 2)
      (advance state tg 3)
      (core/score state :contestant {:card (refresh tg)})
      (is (= 2 (:agenda-point (get-contestant))) "Last TGTBT not scored")
      (is (= 1 (count (get-content state :party3))))
      (advance state (refresh tg) 1)
      (is (= 4 (get-counters (refresh tg) :advancement)))
      (core/score state :contestant {:card (refresh tg)})
      (is (= 2 (:agenda-point (get-contestant))) "Not scored with 4 advancements")
      (advance state (refresh tg) 1)
      (is (= 5 (get-counters (refresh tg) :advancement)))
      (core/score state :contestant {:card (refresh tg)})
      (is (= 3 (:agenda-point (get-contestant))) "Took 5 advancements to score"))))

(deftest unscheduled-maintenance
  ;; Unscheduled Maintenance - prevent Contestant from placing more than 1 Character per turn
  (do-game
    (new-game
      (default-contestant [(qty "Vanilla" 2) "Breaking News"])
      (default-challenger ["Unscheduled Maintenance"]))
    (play-from-hand state :contestant "Breaking News" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Unscheduled Maintenance")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Vanilla" "HQ")
    (is (= 1 (count (get-in @state [:contestant :locales :hq :characters]))) "First Character place of turn allowed")
    (play-from-hand state :contestant "Vanilla" "R&D")
    (is (empty? (get-in @state [:contestant :locales :rd :characters])) "Second Character place of turn blocked")
    (score-agenda state :contestant (get-content state :party1 0))
    (play-from-hand state :contestant "Vanilla" "R&D")
    (is (= 1 (count (get-in @state [:contestant :locales :rd :characters]))) "Current discarded; second Character place of turn allowed")))

(deftest vamp
  ;; Vamp - Run HQ and use replace access to pay credits to drain equal amount from Contestant
  (do-game
    (new-game (default-contestant) (default-challenger ["Vamp" (qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (is (= 8 (:credit (get-contestant))))
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Sure Gamble")
    (is (= 13 (:credit (get-challenger))))
    (play-run-event state (find-card "Vamp" (:hand (get-challenger))) :hq)
    (click-prompt state :challenger "Replacement effect")
    (click-prompt state :challenger "8")
    (is (= 1 (:tag (get-challenger))) "Took 1 tag")
    (is (= 5 (:credit (get-challenger))) "Paid 8 credits")
    (is (zero? (:credit (get-contestant))) "Contestant lost all 8 credits")))

(deftest ^:skip-card-coverage
  virus-counter-flags
  (testing "Set counter flag when virus card enters play with counters"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Surge" "Imp" "Crypsis"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Imp")
      (let [imp (get-resource state 0)]
        (is (get-in imp [:added-virus-counter]) "Counter flag was set on Imp"))))
  (testing "Set counter flag when add-prop is called on a virus"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Crypsis"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Crypsis")
      (let [crypsis (get-resource state 0)]
        (card-ability state :challenger crypsis 2) ;click to add a virus counter
        (is (= 1 (get-counters (refresh crypsis) :virus)) "Crypsis added a virus token")
        (is (get-in (refresh crypsis) [:added-virus-counter])
            "Counter flag was set on Crypsis"))))
  (testing "Clear the virus counter flag at the end of each turn"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Crypsis"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Crypsis")
      (let [crypsis (get-resource state 0)]
        (card-ability state :challenger crypsis 2) ; click to add a virus counter
        (take-credits state :challenger 2)
        (take-credits state :contestant 1)
        (is (not (get-in (refresh crypsis) [:added-virus-counter]))
            "Counter flag was cleared on Crypsis")))))

(deftest white-hat
  ;; White Hat
  (do-game
    (new-game (default-contestant ["Ice Wall" "Fire Wall" "Enigma"])
              (default-challenger ["White Hat"]))
    (take-credits state :contestant)
    (run-empty-locale state :rd)
    (play-from-hand state :challenger "White Hat")
    (is (= :waiting (-> (get-challenger) :prompt first :prompt-type)) "Challenger is waiting for Contestant to boost")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "4")
    (click-prompt state :challenger (find-card "Ice Wall" (:hand (get-contestant))))
    (click-prompt state :challenger (find-card "Enigma" (:hand (get-contestant))))
    (is (= #{"Ice Wall" "Enigma"} (->> (get-contestant) :deck (map :title) (into #{}))))))
