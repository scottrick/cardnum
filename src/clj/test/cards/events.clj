(ns test.cards.events
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest account-siphon-ability
  ;; Account Siphon - Use ability
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Account Siphon" 3)]))
    (take-credits state :contestant)
    (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits")
    ;; play Account Siphon, use ability
    (play-run-event state (first (:hand (get-challenger))) :hq)
    (prompt-choice :challenger "Run ability")
    (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags")
    (is (= 15 (:credit (get-challenger))) "Challenger gained 10 credits")
    (is (= 3 (:credit (get-contestant))) "Contestant lost 5 credits")))

(deftest account-siphon-access
  ;; Account Siphon - Access
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Account Siphon" 3)]))
    (take-credits state :contestant) ; pass to challenger's turn by taking credits
    (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits")
    ;; play another Siphon, do not use ability
    (play-run-event state (first (get-in @state [:challenger :hand])) :hq)
    (prompt-choice :challenger "Access")
    (is (= 0 (:tag (get-challenger))) "Challenger did not take any tags")
    (is (= 5 (:credit (get-challenger))) "Challenger did not gain any credits")
    (is (= 8 (:credit (get-contestant))) "Contestant did not lose any credits")))

(deftest account-siphon-nach-interaction
  ;; Account Siphon - Access
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Account Siphon" 1)
                                              (qty "New Angeles City Hall" 1)]))
    (core/gain state :contestant :bad-publicity 1)
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant has 1 bad publicity")
    (core/lose state :challenger :credit 1)
    (is (= 4 (:credit (get-challenger))) "Challenger has 4 credits")
    (take-credits state :contestant) ; pass to challenger's turn by taking credits
    (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits")
    (play-from-hand state :challenger "New Angeles City Hall")
    (is (= 3 (:credit (get-challenger))) "Challenger has 3 credits")
    (let [nach (get-in @state [:challenger :rig :resource 0])]
      (play-run-event state (first (get-in @state [:challenger :hand])) :hq)
      (prompt-choice :challenger "Run ability")
      (is (= 4 (:credit (get-challenger))) "Challenger still has 4 credits due to BP")
      (card-ability state :challenger nach 0)
      (is (= 2 (:credit (get-challenger))) "Challenger has 2 credits left")
      (card-ability state :challenger nach 0)
      (is (= 0 (:credit (get-challenger))) "Challenger has no credits left")
      (prompt-choice :challenger "Done"))
    (is (= 0 (:tag (get-challenger))) "Challenger did not take any tags")
    (is (= 10 (:credit (get-challenger))) "Challenger gained 10 credits")
    (is (= 3 (:credit (get-contestant))) "Contestant lost 5 credits")))

(deftest amped-up
  ;; Amped Up - Gain 3 clicks and take 1 unpreventable brain damage
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Amped Up" 1)
                               (qty "Feedback Filter" 1)
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

(deftest another-day-another-paycheck
  ;; Another Day, Another Paycheck
  (do-game
    (new-game
      (default-contestant [(qty "Project Atlas" 3)])
      (default-challenger [(qty "Street Peddler" 1) (qty "Another Day, Another Paycheck" 2)]))
    (starting-hand state :challenger ["Street Peddler" "Another Day, Another Paycheck"])
    (play-from-hand state :contestant "Project Atlas" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Street Peddler")
    (run-empty-server state :hq)
    (prompt-choice :challenger "Steal")
    (is (= 5 (:credit (get-challenger))) "No trace, no gain")
    (play-from-hand state :challenger "Another Day, Another Paycheck")
    (run-empty-server state :hq)
    (prompt-choice :challenger "Steal")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 1)
    ;; 4 credits after trace, gain 6
    (is (= 10 (:credit (get-challenger))) "Challenger gained 6 credits")))

(deftest apocalypse-hosting
  ;; Apocalypse - Ensure MU is correct and no duplicate cards in heap
  (do-game
    (new-game (default-contestant [(qty "Launch Campaign" 2) (qty "Ice Wall" 1)])
              (default-challenger [(qty "Scheherazade" 1) (qty "Corroder" 1)
                               (qty "Hyperdriver" 1) (qty "Apocalypse" 2)]))
    (play-from-hand state :contestant "Ice Wall" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :click 3)
    (play-from-hand state :challenger "Scheherazade")
    (let [scheherazade (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger scheherazade 0)
      (prompt-select :challenger (find-card "Corroder" (:hand (get-challenger))))
      (is (= 3 (:memory (get-challenger))) "Memory at 3 (-1 from Corroder)"))
    (play-from-hand state :challenger "Hyperdriver")
    (is (= 0 (:memory (get-challenger))) "Memory at 0 (-1 from Corroder, -3 from Hyperdriver)")
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :challenger "Apocalypse")
    (is (= 0 (count (core/all-installed state :contestant))) "All installed Contestant cards trashed")
    (is (= 3 (count (:discard (get-contestant)))) "3 Contestant cards in Archives")
    (is (= 3 (count (get-in @state [:challenger :rig :facedown]))) "Scheherazade, Corroder, Hyperdriver facedown")
    (is (= 1 (count (:discard (get-challenger)))) "Only Apocalypse is in the heap")
    (is (= 4 (:memory (get-challenger))) "Memory back to 4")))

(deftest apocalype-full-immersion-recstudio
  ;; Apocalypse with Full Immersion - no duplicate cards in heap #2606
  (do-game
    (new-game
      (default-contestant [(qty "Full Immersion RecStudio" 1) (qty "Sandburg" 1)
                     (qty "Oaktown Renovation" 1)])
      (default-challenger  [(qty "Apocalypse" 1)]))
    (play-from-hand state :contestant "Full Immersion RecStudio" "New remote")
    (let [fir (get-content state :remote1 0)]
      (core/rez state :contestant fir)
      (card-ability state :contestant fir 0)
      (prompt-select :contestant (find-card "Sandburg" (:hand (get-contestant))))
      (card-ability state :contestant fir 0)
      (prompt-select :contestant (find-card "Oaktown Renovation" (:hand (get-contestant))))
      (take-credits state :contestant)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :challenger "Apocalypse")
      (is (= 0 (count (core/all-installed state :contestant))) "All installed Contestant cards trashed")
      (is (= 3 (count (:discard (get-contestant)))) "3 Contestant cards in Archives")
      (is (= 1 (count (:discard (get-challenger)))) "Only Apocalypse is in the heap"))))

(deftest apocalypse-in-play-ability
  ;; Apocalypse - Turn Challenger cards facedown and reduce memory and hand-size gains
  (do-game
    (new-game (default-contestant [(qty "Launch Campaign" 2) (qty "Ice Wall" 1)])
              (default-challenger [(qty "Logos" 3) (qty "Apocalypse" 3)]))
    (play-from-hand state :contestant "Ice Wall" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Logos")
    (is (= 1 (:hand-size-modification (get-challenger))) "Hand-size increased from Logos")
    (is (= 5 (:memory (get-challenger))) "Memory increased from Logos")
    (core/gain state :challenger :click 1 :credit 2)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :challenger "Apocalypse")
    (is (= 0 (count (core/all-installed state :contestant))) "All installed Contestant cards trashed")
    (is (= 3 (count (:discard (get-contestant)))) "3 Contestant cards in Archives")
    (let [logos (get-in @state [:challenger :rig :facedown 0])]
      (is (:facedown (refresh logos)) "Logos is facedown")
      (is (= 0 (:hand-size-modification (get-challenger))) "Hand-size reset with Logos facedown")
      (is (= 4 (:memory (get-challenger))) "Memory reset with Logos facedown"))))

(deftest apocalypse-turn-facedown
  ;; Apocalypse - Turn Challenger cards facedown without firing their leave play effects
  (do-game
    (new-game (default-contestant [(qty "Launch Campaign" 2) (qty "Ice Wall" 1)])
              (default-challenger [(qty "Tri-maf Contact" 3) (qty "Apocalypse" 3)]))
    (play-from-hand state :contestant "Ice Wall" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Tri-maf Contact")
    (core/gain state :challenger :click 2)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :challenger "Apocalypse")
    (is (= 0 (count (core/all-installed state :contestant))) "All installed Contestant cards trashed")
    (is (= 3 (count (:discard (get-contestant)))) "3 Contestant cards in Archives")
    (let [tmc (get-in @state [:challenger :rig :facedown 0])]
      (is (:facedown (refresh tmc)) "Tri-maf Contact is facedown")
      (is (= 3 (count (:hand (get-challenger))))
          "No meat damage dealt by Tri-maf's leave play effect"))))

(deftest blackmail
  ;; Prevent rezzing of ice for one run
  (do-game
    (new-game
      (default-contestant [(qty "Ice Wall" 3)])
      (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
    (is (= 1 (get-in @state [:contestant :bad-publicity])) "Contestant has 1 bad-publicity")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Blackmail")
    (prompt-choice :challenger "HQ")
    (let [iwall1 (get-ice state :hq 0)
          iwall2 (get-ice state :hq 1)]
      (core/rez state :contestant iwall1)
      (is (not (get-in (refresh iwall1) [:rezzed])) "First Ice Wall is not rezzed")
      (run-continue state)
      (core/rez state :contestant iwall2)
      (is (not (get-in (refresh iwall2) [:rezzed])) "Second Ice Wall is not rezzed")
      (core/jack-out state :challenger nil)
      ;; Do another run, where the ice should rez
      (run-on state "HQ")
      (core/rez state :contestant iwall1)
      (is (get-in (refresh iwall1) [:rezzed]) "First Ice Wall is rezzed"))))

(deftest blackmail-tmi-interaction
  ;; Regression test for a rezzed tmi breaking game state on a blackmail run
  (do-game
    (new-game (default-contestant [(qty "TMI" 3)])
              (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
    (is (= 1 (get-in @state [:contestant :bad-publicity])) "Contestant has 1 bad-publicity")
    (play-from-hand state :contestant "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
      (core/rez state :contestant tmi)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (get-in (refresh tmi) [:rezzed]) "TMI is rezzed")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Blackmail")
      (prompt-choice :challenger "HQ")
      (run-continue state)
      (run-jack-out state)
      (run-on state "Archives"))))

(deftest by-any-means
  ;; By Any Means - Full test
  (do-game
   (new-game (default-contestant [(qty "Hedge Fund" 1) (qty "Ice Wall" 1) (qty "Paper Trail" 1) (qty "PAD Campaign" 1)])
             (default-challenger [(qty "By Any Means" 1), (qty "Sure Gamble" 4)]))
   (take-credits state :contestant)
   (run-empty-server state "Archives")
   (play-from-hand state :challenger "By Any Means")
   (is (= 3 (:click (get-challenger))) "Card not played, priority restriction")
   (take-credits state :challenger)
   (starting-hand state :contestant ["Paper Trail", "Hedge Fund", "PAD Campaign"])
   (play-from-hand state :contestant "Paper Trail", "New remote")
   (play-from-hand state :contestant "PAD Campaign", "New remote")
   (take-credits state :contestant)
   (core/gain state :challenger :click 1)
   (play-from-hand state :challenger "By Any Means")
   (run-empty-server state "HQ")
   (is (= 1 (count (:discard (get-contestant)))) "Operation was trashed")
   (is (= 3 (count (:hand (get-challenger)))) "Took 1 meat damage")
   (run-empty-server state "R&D")
   (is (= 2 (count (:discard (get-contestant)))) "ICE was trashed")
   (is (= 2 (count (:hand (get-challenger)))) "Took 1 meat damage")
   (run-empty-server state "Server 1")
   (is (= 3 (count (:discard (get-contestant)))) "Agenda was trashed")
   (is (= 1 (count (:hand (get-challenger)))) "Took 1 meat damage")
   (run-empty-server state "Server 2")
   (is (= 4 (count (:discard (get-contestant)))) "Trashable was trashed")
   (is (= 0 (count (:hand (get-challenger)))) "Took 1 meat damage")))

(deftest by-any-means-ctm-crash
  (do-game
    (new-game (make-deck "NBN: Controlling the Message" [(qty "Paper Trail" 1)])
              (default-challenger [(qty "By Any Means" 2)]))
    (play-from-hand state :contestant "Paper Trail" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "By Any Means")
    (run-empty-server state "Server 1")
    (prompt-choice :contestant "No") ;; Don't trigger CTM trace
    (is (empty? (:prompt (get-challenger))) "No prompt to steal since agenda was trashed")
    (is (= 1 (count (:discard (get-contestant)))) "Agenda was trashed")
    (is (= 0 (count (:hand (get-challenger)))) "Took 1 meat damage")))

(deftest credit-kiting
  ;; After successful central run lower install cost by 8 and gain a tag
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 1)])
              (default-challenger [(qty "Credit Kiting" 1) (qty "Femme Fatale" 1)]))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (play-from-hand state :challenger "Credit Kiting")
    (is (= 3 (:click (get-challenger))) "Card not played, successful run on central not made")
    (run-empty-server state "HQ")
    (play-from-hand state :challenger "Credit Kiting")
    (prompt-select :challenger (find-card "Femme Fatale" (:hand (get-challenger))))
    (is (= 4 (:credit (get-challenger))) "Femme Fatale only cost 1 credit")
    (is (= 1 (:tag (get-challenger))) "Challenger gained a tag")))

(deftest cbi-raid
  ;; CBI Raid - Full test
  (do-game
    (new-game (default-contestant [(qty "Caprice Nisei" 1) (qty "Adonis Campaign" 1) (qty "Quandary" 1)
                             (qty "Jackson Howard" 1) (qty "Global Food Initiative" 1)])
              (default-challenger [(qty "CBI Raid" 1)]))
    (take-credits state :contestant)
    (is (= 5 (count (:hand (get-contestant)))))
    (play-from-hand state :challenger "CBI Raid")
    (is (= :hq (get-in @state [:run :server 0])))
    (run-successful state)
    (prompt-choice :challenger "Run ability")
    (prompt-choice :contestant (find-card "Caprice Nisei" (:hand (get-contestant))))
    (prompt-choice :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
    (prompt-choice :contestant (find-card "Quandary" (:hand (get-contestant))))
    (prompt-choice :contestant (find-card "Jackson Howard" (:hand (get-contestant))))
    (prompt-choice :contestant (find-card "Global Food Initiative" (:hand (get-contestant))))
    ;; try starting over
    (prompt-choice :contestant "Start over")
    (prompt-choice :contestant (find-card "Global Food Initiative" (:hand (get-contestant))))
    (prompt-choice :contestant (find-card "Jackson Howard" (:hand (get-contestant))))
    (prompt-choice :contestant (find-card "Quandary" (:hand (get-contestant))))
    (prompt-choice :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
    (prompt-choice :contestant (find-card "Caprice Nisei" (:hand (get-contestant)))) ;this is the top card of R&D
    (prompt-choice :contestant "Done")
    (is (= 0 (count (:hand (get-contestant)))))
    (is (= 5 (count (:deck (get-contestant)))))
    (is (= "Caprice Nisei" (:title (first (:deck (get-contestant))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-contestant))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-contestant)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-contestant))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-contestant)))))))))))

(deftest cold-read
  ;; Make a run, and place 4 on this card, which you may use only during this run.
  ;; When this run ends, trash 1 program (cannot be prevented) used during this run.
  (do-game
    (new-game (default-contestant [(qty "Blacklist" 3)])
              (default-challenger [(qty "Imp" 1) (qty "Cold Read" 2)]))
    (play-from-hand state :contestant "Blacklist" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Imp")
    (let [bl (get-content state :remote1 0)]
      (play-from-hand state :challenger "Cold Read")
      (prompt-choice :challenger "HQ")
      (is (= 4 (:rec-counter (find-card "Cold Read" (get-in @state [:challenger :play-area])))) "Cold Read has 4 counters")
      (run-successful state)
      (card-ability state :challenger (get-program state 0) 0)
      (prompt-select :challenger (get-program state 0))
      (is (= 2 (count (:discard (get-challenger)))) "Imp and Cold Read in discard")
      ; Cold Read works when Blacklist rezzed - #2378
      (core/rez state :contestant bl)
      (play-from-hand state :challenger "Cold Read")
      (prompt-choice :challenger "HQ")
      (is (= 4 (:rec-counter (find-card "Cold Read" (get-in @state [:challenger :play-area])))) "Cold Read has 4 counters")
      (run-successful state))))

(deftest contestantorate-scandal
  ;; Contestantorate Scandal - Contestant has 1 additional bad pub even with 0
  (do-game
    (new-game (default-contestant [(qty "Elizabeth Mills" 1)])
              (default-challenger [(qty "Contestantorate Scandal" 1) (qty "Activist Support" 1)
                               (qty "Raymond Flint" 1) (qty "Investigative Journalism" 1)]))
    (play-from-hand state :contestant "Elizabeth Mills" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 5 :click 1)
    (play-from-hand state :challenger "Raymond Flint")
    (play-from-hand state :challenger "Contestantorate Scandal")
    (is (empty? (:prompt (get-challenger))) "No BP taken, so no HQ access from Raymond")
    (play-from-hand state :challenger "Investigative Journalism")
    (is (= "Investigative Journalism" (:title (get-in @state [:challenger :rig :resource 1]))) "IJ able to be installed")
    (run-on state "HQ")
    (is (= 1 (:run-credit (get-challenger))) "1 run credit from bad publicity")
    (run-jack-out state)
    (play-from-hand state :challenger "Activist Support")
    (take-credits state :challenger)
    (let [em (get-content state :remote1 0)]
      (core/rez state :contestant em)
      (is (= 1 (:has-bad-pub (get-contestant))) "Contestant still has BP")
      (take-credits state :contestant)
      (is (= 0 (:bad-publicity (get-contestant))) "Contestant has BP, didn't take 1 from Activist Support"))))

(deftest data-breach
  ;; Data Breach
  (do-game
    (new-game
      (default-contestant)
      (default-challenger [(qty "Data Breach" 3)]))
    (starting-hand state :contestant ["Hedge Fund"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Data Breach")
    (core/no-action state :contestant nil)
    (run-successful state)
    (prompt-choice :challenger "OK")
    (prompt-choice :challenger "Yes")
    (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
    (core/no-action state :contestant nil)
    (run-successful state)
    (prompt-choice :challenger "OK")
    (is (empty? (:prompt (get-challenger))) "No prompt to run a third time")
    (is (not (:run @state)) "Run is over")
    (play-from-hand state :challenger "Data Breach")
    (run-jack-out state)
    (is (empty? (:prompt (get-challenger))) "No option to run again on unsuccessful run")))

(deftest data-breach-doppelganger
  ;; FAQ 4.1 - ensure challenger gets choice of activation order
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Doppelgänger" 1) (qty "Data Breach" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Doppelgänger")
    (play-from-hand state :challenger "Data Breach")
    (core/no-action state :contestant nil)
    (run-successful state)
    (prompt-choice :challenger "OK")
    (prompt-choice :challenger "Doppelgänger")
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger "HQ")
    (is (:run @state) "New run started")
    (is (= [:hq] (:server (:run @state))) "Running on HQ via Doppelgänger")
    (core/no-action state :contestant nil)
    (run-successful state)
    (prompt-choice :challenger "OK")
    (prompt-choice :challenger "Yes")
    (is (= [:rd] (get-in @state [:run :server])) "Second Data Breach run on R&D triggered")
    (core/no-action state :contestant nil)
    (run-successful state)))

(deftest deja-vu
  ;; Deja Vu - recur one non-virus or two virus cards
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Déjà Vu" 2)
                               (qty "Cache" 1)
                               (qty "Datasucker" 1)
                               (qty "Dirty Laundry" 1)]))
    (take-credits state :contestant 3) ; pass to challenger's turn
    (trash-from-hand state :challenger "Cache")
    (trash-from-hand state :challenger "Datasucker")
    (trash-from-hand state :challenger "Dirty Laundry")
    (is (= 2 (count (:hand (get-challenger)))) "Two cards in hand prior to playing Déjà Vu")
    (play-from-hand state :challenger "Déjà Vu")
    (prompt-choice :challenger (find-card "Dirty Laundry" (:discard (get-challenger))))
    (is (empty? (:prompt (get-challenger))) "Recurring a non-virus card stops Déjà Vu prompting further")
    (is (= 2 (count (:hand (get-challenger)))) "Two cards in after playing Déjà Vu")
    (play-from-hand state :challenger "Déjà Vu")
    (prompt-choice :challenger (find-card "Cache" (:discard (get-challenger))))
    (is (not (empty? (:prompt (get-challenger)))) "Recurring a virus card causes Déjà Vu to prompt for second virus to recur")
    (prompt-choice :challenger (find-card "Datasucker" (:discard (get-challenger))))
    (is (= 3 (count (:hand (get-challenger)))) "Three cards in after playing second Déjà Vu")))

(deftest demolition-run
  ;; Demolition Run - Trash at no cost
  (do-game
    (new-game (default-contestant [(qty "False Lead" 1)
                             (qty "Shell Contestantoration" 1)
                             (qty "Hedge Fund" 3)])
              (default-challenger [(qty "Demolition Run" 1)]))
    (core/move state :contestant (find-card "False Lead" (:hand (get-contestant))) :deck) ; put False Lead back in R&D
    (play-from-hand state :contestant "Shell Contestantoration" "R&D") ; install upgrade with a trash cost in root of R&D
    (take-credits state :contestant 2) ; pass to challenger's turn by taking credits
    (play-from-hand state :challenger "Demolition Run")
    (is (= 3 (:credit (get-challenger))) "Paid 2 credits for the event")
    (prompt-choice :challenger "R&D")
    (is (= [:rd] (get-in @state [:run :server])) "Run initiated on R&D")
    (prompt-choice :challenger "OK") ; dismiss instructional prompt for Demolition Run
    (run-successful state)
    (let [demo (get-in @state [:challenger :play-area 0])] ; Demolition Run "hack" is to put it out in the play area
      (prompt-choice :challenger "Unrezzed upgrade in R&D")
      (card-ability state :challenger demo 0)
      (is (= 3 (:credit (get-challenger))) "Trashed Shell Contestantoration at no cost")
      (prompt-choice :challenger "Card from deck")
      (card-ability state :challenger demo 0)  ; trash False Lead instead of stealing
      (is (= 0 (:agenda-point (get-challenger))) "Didn't steal False Lead")
      (is (= 2 (count (:discard (get-contestant)))) "2 cards in Archives")
      (is (empty? (:prompt (get-challenger))) "Run concluded"))))

(deftest deuces-wild
  ;; Deuces Wild
  (do-game
    (new-game (default-contestant [(qty "Wraparound" 1)
                             (qty "The Future Perfect" 1)])
              (default-challenger [(qty "Deuces Wild" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Wraparound" "New remote")
    (take-credits state :contestant)
    (starting-hand state :challenger ["Deuces Wild" "Deuces Wild"])
    (play-from-hand state :challenger "Deuces Wild")
    (prompt-choice :challenger "Gain 3 [Credits]")
    (is (= 6 (:credit (get-challenger))) "Gained 1 net credit")
    (prompt-choice :challenger "Draw 2 cards")
    (is (= 3 (count (:hand (get-challenger)))) "Drew 2 cards")
    (is (empty? (:prompt (get-challenger))) "Deuces Wild not showing a third choice option")

    (play-from-hand state :challenger "Deuces Wild")
    (prompt-choice :challenger "Expose 1 ice and make a run")
    (prompt-select :challenger (get-ice state :remote1 0))
    (prompt-choice :challenger "HQ")
    (is (empty? (:prompt (get-challenger))) "Deuces prompt not queued")
    (run-continue state)
    (run-successful state)
    (is (= 1 (count (:prompt (get-challenger)))) "Deuces prompt not queued")
    (prompt-choice :challenger "Access")
    (prompt-choice :contestant "0")
    (prompt-choice :challenger "0")
    (prompt-choice :challenger "Steal")
    (is (= 1 (count (:scored (get-challenger)))) "TFP stolen")
    (core/gain state :challenger :tag 1)
    (is (= 1 (:tag (get-challenger))) "Challenger has 1 tag")
    (prompt-choice :challenger "Remove 1 tag")
    (is (= 0 (:tag (get-challenger))))))

(deftest dirty-laundry
  ;; Dirty Laundry - Gain 5 credits at the end of the run if it was successful
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Dirty Laundry" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Dirty Laundry")
    (prompt-choice :challenger "Archives")
    (run-successful state)
    (is (= 8 (:credit (get-challenger))) "Gained 5 credits")
    (play-from-hand state :challenger "Dirty Laundry")
    (prompt-choice :challenger "Archives")
    (run-jack-out state)
    (is (= 6 (:credit (get-challenger))) "Run unsuccessful; gained no credits")))

(deftest drive-by
  ;; Drive By - Expose card in remote server and trash if asset or upgrade
  (do-game
    (new-game (default-contestant [(qty "Eve Campaign" 2)
                             (qty "Product Placement" 1)
                             (qty "Project Atlas" 1)])
              (default-challenger [(qty "Drive By" 2)]))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Eve Campaign" "New remote")
    (play-from-hand state :contestant "Eve Campaign" "New remote")
    (play-from-hand state :contestant "Project Atlas" "New remote")
    (play-from-hand state :contestant "Product Placement" "HQ")
    (take-credits state :contestant)
    (let [eve1 (get-content state :remote1 0)
          eve2 (get-content state :remote2 0)
          atl (get-content state :remote3 0)
          pp (get-content state :hq 0)]
      (core/rez state :contestant eve1)
      (play-from-hand state :challenger "Drive By")
      (prompt-select :challenger pp)
      (is (= 1 (count (get-in @state [:contestant :servers :hq :content])))
          "Upgrades in root of central servers can't be targeted")
      (prompt-select :challenger (refresh eve1))
      (is (= 1 (count (get-in @state [:contestant :servers :remote1 :content])))
          "Rezzed cards can't be targeted")
      (prompt-select :challenger eve2)
      (is (= 2 (:click (get-challenger))) "Spent 2 clicks")
      (is (and (= 1 (count (:discard (get-contestant))))
               (= 5 (:credit (get-challenger))))
          "Eve trashed at no cost")
      (is (nil? (get-in @state [:contestant :servers :remote2 :content])) "Server 2 no longer exists")
      (play-from-hand state :challenger "Drive By")
      (prompt-select :challenger atl)
      (is (= 0 (:click (get-challenger))) "Challenger has 0 clicks left")
      (is (= 1 (count (get-in @state [:contestant :servers :remote3 :content])))
          "Project Atlas not trashed from Server 3"))))

(deftest drive-by-psychic-field
  ;; Drive By - Psychic Field trashed after psi game. Issue #2127.
  (do-game
    (new-game (default-contestant [(qty "Psychic Field" 1)])
              (default-challenger [(qty "Drive By" 3)]))
    (play-from-hand state :contestant "Psychic Field" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Drive By")
    (prompt-select :challenger (get-content state :remote1 0))
    (prompt-choice :contestant "0 [Credits]")
    (prompt-choice :challenger "1 [Credits]")
    (is (empty? (get-content state :remote1)) "Psychic Field trashed")))

(deftest early-bird
  ;; Early Bird - Priority, make a run and gain a click
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Early Bird" 1)]))
    (take-credits state :contestant)
    (run-empty-server state "Archives")
    (play-from-hand state :challenger "Early Bird")
    (is (= 3 (:click (get-challenger))) "Card not played, Early Bird priority restriction")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Early Bird")
    (prompt-choice :challenger "Archives")
    (is (= 4 (:click (get-challenger))) "Early Bird gains click")))

(deftest emergent-creativity
  ;; Emergent Creativty - Double, discard programs/hardware from grip, install from heap
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Emergent Creativity" 1) (qty "Paperclip" 1)
                               (qty "Heartbeat" 1) (qty "Gordian Blade" 1) (qty "Test Run" 1)]))
    (starting-hand state :challenger ["Emergent Creativity" "Heartbeat" "Gordian Blade" "Test Run"])
    (take-credits state :contestant)

    (play-from-hand state :challenger "Emergent Creativity")
    (prompt-select :challenger (find-card "Heartbeat" (:hand (get-challenger))))
    (prompt-select :challenger (find-card "Gordian Blade" (:hand (get-challenger))))
    (prompt-choice :challenger "Done")
    (prompt-choice :challenger (find-card "Paperclip" (:deck (get-challenger))))
    (is (= 3 (:credit (get-challenger))) "Offset cost of installing Paperclip")
    (is (= 0 (count (:deck (get-challenger)))) "Installed from heap")
    (is (= 3 (count (:discard (get-challenger)))) "Discard is 3 cards - EC, Heartbeat, GB")
    (is (= 2 (:click (get-challenger))) "Emergent Creativity is a Double event")))

(deftest employee-strike-blue-sun
  ;; Employee Strike - vs Blue Sun, suppress Step 1.2
  (do-game
    (new-game (make-deck "Blue Sun: Powering the Future" [(qty "Ice Wall" 1)])
              (default-challenger [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (core/rez state :contestant (get-ice state :hq 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Employee Strike")
    (take-credits state :challenger)
    (is (not (:contestant-phase-12 @state)) "Employee Strike suppressed Blue Sun step 1.2")))

(deftest employee-strike-pu-philotic
  ;; Employee Strike - vs PU/Philotic - test for #2688
  (do-game
    (new-game (make-deck "Jinteki: Potential Unleashed" [(qty "Philotic Entanglement" 1) (qty "Braintrust" 2)])
              (default-challenger [(qty "Employee Strike" 10)]))
    (play-from-hand state :contestant "Braintrust" "New remote")
    (play-from-hand state :contestant "Braintrust" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Steal")
    (run-empty-server state "Server 2")
    (prompt-choice :challenger "Steal")
    (play-from-hand state :challenger "Employee Strike")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Philotic Entanglement" "New remote")
    (score-agenda state :contestant (get-content state :remote3 0))
    (is (= 3 (count (:discard (get-challenger)))) "Discard is 3 cards - 2 from Philotic, 1 EStrike.  Nothing from PU mill")))

(deftest encore
  ;; Encore - Run all 3 central servers successfully to take another turn.  Remove Encore from game.
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 1)])
              (default-challenger [(qty "Encore" 1)]))
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :challenger "Encore")
    (is (= 1 (count (:rfg (get-challenger)))) "Encore removed from game")
    (take-credits state :challenger)
    (take-credits state :challenger)
    ; only get one extra turn
    (take-credits state :challenger)
    (is (= 9 (:credit (get-challenger))))))

(deftest encore-stacking
  ;; Encore - 2 encores in a 5 click turn results in 2 extra turns
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 1)])
              (default-challenger [(qty "Encore" 2)]))
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (core/gain state :challenger :click 1)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :challenger "Encore")
    (play-from-hand state :challenger "Encore")
    (is (= 2 (count (:rfg (get-challenger)))) "2 Encores removed from game")
    (take-credits state :challenger)
    (take-credits state :challenger)
    ;; Two extra turns
    (take-credits state :challenger)
    (is (= 13 (:credit (get-challenger))))))

(deftest eureka!
  ;; Eureka! - Install the program but trash the event
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Eureka!" 2) (qty "Torch" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 1)
    (core/move state :challenger (find-card "Torch" (:hand (get-challenger))) :deck)
    (play-from-hand state :challenger "Eureka!")
    (prompt-choice :challenger "Yes")
    (is (= 3 (:credit (get-challenger))))
    (is (= 1 (count (get-in @state [:challenger :rig :program]))))
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (play-from-hand state :challenger "Eureka!")
    (is (= 0 (:credit (get-challenger))))
    (is (= 3 (count (:discard (get-challenger)))))))

(deftest falsified-credentials
  ;; Falsified Credentials - Expose card in remote
  ;; server and correctly guess its type to gain 5 creds
  (do-game
    (new-game (default-contestant [(qty "Eve Campaign" 2)
                             (qty "Product Placement" 2)
                             (qty "Project Atlas" 1)])
              (default-challenger [(qty "Falsified Credentials" 3)]))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Eve Campaign" "New remote")
    (play-from-hand state :contestant "Eve Campaign" "New remote")
    (play-from-hand state :contestant "Project Atlas" "New remote")
    (play-from-hand state :contestant "Product Placement" "HQ")
    (play-from-hand state :contestant "Product Placement" "Server 3")
    (take-credits state :contestant)
    (let [eve1 (get-content state :remote1 0)
          eve2 (get-content state :remote2 0)
          atl (get-content state :remote3 0)
          pp1 (get-content state :hq 0)
          pp2 (get-content state :remote3 1)]
      (core/rez state :contestant eve1)
      (play-from-hand state :challenger "Falsified Credentials")
      (prompt-choice :challenger "Asset")
      (prompt-select :challenger (refresh eve1))
      (is (= 4 (:credit (get-challenger)))
          "Rezzed cards can't be targeted")
      (prompt-select :challenger eve2)
      (is (= 3 (:click (get-challenger))) "Spent 1 click")
      (is (= 9 (:credit (get-challenger))) "Gained 5 creds for guessing asset correctly")
      (play-from-hand state :challenger "Falsified Credentials")
      (prompt-choice :challenger "Upgrade")
      (prompt-select :challenger pp1)
      (is (= 8 (:credit (get-challenger))) "Can't target cards in centrals")
      (prompt-select :challenger pp2)
      (is (= 13 (:credit (get-challenger)))
          "Gained 5 creds for guessing upgrade correctly, even if server contains non-upgrade as well")
      (core/rez state :contestant pp2)
      (play-from-hand state :challenger "Falsified Credentials")
      (prompt-choice :challenger "Agenda")
      (prompt-select :challenger atl)
      (is (= 17 (:credit (get-challenger)))
          "Gained 5 credits for guessing agenda correctly, even with rezzed card in server"))))

(deftest falsified-credentials-zaibatsu-loyalty
  ;; If Falsified Credentials fails to expose, it grants no credits.
  (do-game
   (new-game (default-contestant [(qty "Zaibatsu Loyalty" 1)
                            (qty "Project Atlas" 1)])
             (default-challenger [(qty "Falsified Credentials" 2)]))
   
    (play-from-hand state :contestant "Project Atlas" "New remote")
    (play-from-hand state :contestant "Zaibatsu Loyalty" "New remote")
    (take-credits state :contestant)
    (let [atl (get-content state :remote1 0)
          zaibatsu (get-content state :remote2 0)]
      (core/rez state :contestant zaibatsu)
      (play-from-hand state :challenger "Falsified Credentials")
      (prompt-choice :challenger "Agenda")
      (prompt-select :challenger atl)
      (prompt-choice :contestant "Done")
      (is (= 9 (:credit (get-challenger))) "An unprevented expose gets credits")

      (play-from-hand state :challenger "Falsified Credentials")
      (prompt-choice :challenger "Agenda")
      (prompt-select :challenger atl)
      (card-ability state :contestant (refresh zaibatsu) 0) ; prevent the expose!
      (prompt-choice :contestant "Done")
      (is (= 8 (:credit (get-challenger))) "A prevented expose does not"))))

(deftest frantic-coding-install
  ;; Frantic Coding - Install 1 program, other 9 cards are trashed
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Frantic Coding" 1) (qty "Torch" 1) (qty "Corroder" 1)
                               (qty "Magnum Opus" 1) (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                               (qty "John Masanori" 1) (qty "Amped Up" 1) (qty "Wanton Destruction" 1)]))
    (starting-hand state :challenger ["Frantic Coding"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Frantic Coding")
    (prompt-choice :challenger "OK")
    (let [get-prompt (fn [] (first (#(get-in @state [:challenger :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
      (is (= 2 (:credit (get-challenger))))
      (is (= 1 (count (:discard (get-challenger)))))
      (prompt-choice :challenger (find-card "Magnum Opus" (:deck (get-challenger))))
      (is (= 1 (count (get-in @state [:challenger :rig :program]))))
      (is (= 2 (:credit (get-challenger))) "Magnum Opus installed for free")
      (is (= 10 (count (:discard (get-challenger))))))))

(deftest frantic-coding-noinstall
  ;; Frantic Coding - Don't install anything, all 10 cards are trashed
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Frantic Coding" 1) (qty "Torch" 1) (qty "Corroder" 1)
                               (qty "Magnum Opus" 1) (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                               (qty "John Masanori" 1) (qty "Amped Up" 1) (qty "Wanton Destruction" 1)]))
    (starting-hand state :challenger ["Frantic Coding"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Frantic Coding")
    (prompt-choice :challenger "OK")
    (let [get-prompt (fn [] (first (#(get-in @state [:challenger :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
      (is (= 1 (count (:discard (get-challenger)))))
      (prompt-choice :challenger "No install")
      (is (= 0 (count (get-in @state [:challenger :rig :program]))))
      (is (= 11 (count (:discard (get-challenger))))))))

(deftest freedom-through-equality
  ;; Move Freedom Through Equality to challenger score on another steal
  ;; Check only one current used
  (do-game
    (new-game (default-contestant [(qty "Project Beale" 2)])
              (default-challenger [(qty "Street Peddler" 1) (qty "\"Freedom Through Equality\"" 3) (qty "Sure Gamble" 1)]))
    (starting-hand state :challenger ["Street Peddler"
                                  "\"Freedom Through Equality\""
                                  "\"Freedom Through Equality\""
                                  "Sure Gamble"])
    (play-from-hand state :contestant "Project Beale" "New remote")
    (play-from-hand state :contestant "Project Beale" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Street Peddler")
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Steal")
    (is (= 1 (count (:scored (get-challenger)))) "Freedom Through Equality not moved from Peddler to score area")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (run-empty-server state "Server 2")
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "\"Freedom Through Equality\"")
    (play-from-hand state :challenger "\"Freedom Through Equality\"")
    (prompt-choice :challenger "Steal")
    (is (= 3 (count (:scored (get-challenger)))) "Freedom Through Equality moved to score area")
    (is (= 5 (:agenda-point (get-challenger))) "Freedom Through Equality for 1 agenda point")))

(deftest freelance-coding-contract
  ;; Freelance Coding Contract - Gain 2 credits per program trashed from Grip
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Freelance Coding Contract" 1)
                               (qty "Paricia" 1)
                               (qty "Cloak" 1)
                               (qty "Inti" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Freelance Coding Contract")
    (prompt-select :challenger (find-card "Cloak" (:hand (get-challenger))))
    (prompt-select :challenger (find-card "Paricia" (:hand (get-challenger))))
    (prompt-select :challenger (find-card "Inti" (:hand (get-challenger))))
    (prompt-choice :challenger "Done")
    (is (= 3 (count (filter #(= (:type %) "Program") (:discard (get-challenger)))))
        "3 programs in Heap")
    (is (= 11 (:credit (get-challenger))) "Gained 6 credits from 3 trashed programs")))

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

(deftest hacktivist-meeting
  ;; Trash a random card from contestant hand while active
  ;; Make sure it is not active when hosted on Peddler
  (do-game
    (new-game (default-contestant [(qty "Jeeves Model Bioroids" 2)
                             (qty "Jackson Howard" 2)])
              (default-challenger [(qty "Street Peddler" 1)
                               (qty "Hacktivist Meeting" 3)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler" "Hacktivist Meeting"])
    (play-from-hand state :challenger "Street Peddler")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Jeeves Model Bioroids" "New remote")
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (let [jeeves (get-content state :remote1 0)
          jackson (get-content state :remote2 0)]
      (core/rez state :contestant jeeves)
      (is (= 0 (count (:discard (get-contestant)))) "Nothing discarded to rez Jeeves - Hacktivist not active")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Hacktivist Meeting")
      (core/rez state :contestant jackson)
      (is (= 1 (count (:discard (get-contestant)))) "Card discarded to rez Jackson - Hacktivist active"))))

(deftest independent-thinking
  ;; Independent Thinking - Trash 2 installed cards, including a facedown directive, and draw 2 cards
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Apex: Invasive Predator" [(qty "Neutralize All Threats" 1)
                                            (qty "Independent Thinking" 2)
                                            (qty "Fan Site" 3)
                                            (qty "Street Magic" 3)]))
    (starting-hand state :challenger ["Fan Site"
                                  "Fan Site"
                                  "Neutralize All Threats"
                                  "Independent Thinking"
                                  "Independent Thinking"])
    (take-credits state :contestant)
    (core/end-phase-12 state :challenger nil)
    (prompt-select :challenger (find-card "Neutralize All Threats" (:hand (get-challenger))))
    (play-from-hand state :challenger "Fan Site")
    (let [fs (get-in @state [:challenger :rig :resource 0])
          nat (get-in @state [:challenger :rig :facedown 0])]
      (play-from-hand state :challenger "Independent Thinking")
      (prompt-select :challenger fs)
      (prompt-select :challenger nat)
      (prompt-choice :challenger "Done")
      (is (= 4 (count (:hand (get-challenger)))) "Trashing 2 cards draws 2 card"))))

(deftest indexing
  ;; Indexing - Full test
  (do-game
    (new-game (default-contestant [(qty "Caprice Nisei" 1) (qty "Adonis Campaign" 1) (qty "Quandary" 1)
                            (qty "Jackson Howard" 1) (qty "Global Food Initiative" 1)])
            (default-challenger [(qty "Indexing" 1)]))
    (dotimes [_ 5] (core/move state :contestant (first (:hand (get-contestant))) :deck))
    (take-credits state :contestant)
    (is (= 0 (count (:hand (get-contestant)))))
    (is (= 5 (count (:deck (get-contestant)))))
    (play-from-hand state :challenger "Indexing")
    (is (= :rd (get-in @state [:run :server 0])))
    (run-successful state)
    (prompt-choice :challenger "Run ability")
    (prompt-choice :challenger (find-card "Caprice Nisei" (:deck (get-contestant))))
    (prompt-choice :challenger (find-card "Adonis Campaign" (:deck (get-contestant))))
    (prompt-choice :challenger (find-card "Quandary" (:deck (get-contestant))))
    (prompt-choice :challenger (find-card "Jackson Howard" (:deck (get-contestant))))
    (prompt-choice :challenger (find-card "Global Food Initiative" (:deck (get-contestant))))
    ;; try starting over
    (prompt-choice :challenger "Start over")
    (prompt-choice :challenger (find-card "Global Food Initiative" (:deck (get-contestant))))
    (prompt-choice :challenger (find-card "Jackson Howard" (:deck (get-contestant))))
    (prompt-choice :challenger (find-card "Quandary" (:deck (get-contestant))))
    (prompt-choice :challenger (find-card "Adonis Campaign" (:deck (get-contestant))))
    (prompt-choice :challenger (find-card "Caprice Nisei" (:deck (get-contestant)))) ;this is the top card of R&D
    (prompt-choice :challenger "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-contestant))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-contestant))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-contestant)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-contestant))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-contestant)))))))))))

(deftest information-sifting
  ;; Information Sifting - complicated interactions with damage prevention
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping"
                         [(qty "Snare!" 1) (qty "PAD Campaign" 1) (qty "Hostile Infrastructure" 1)
                          (qty "Braintrust" 1) (qty "Hedge Fund" 1) (qty "Power Shutdown" 1)])
              (default-challenger [(qty "Information Sifting" 2) (qty "Deus X" 2) (qty "Sure Gamble" 1)]))
    (play-from-hand state :contestant "Hostile Infrastructure" "New remote")

    (core/gain state :contestant :credit 10)
    (core/rez state :contestant (get-content state :remote1 0))
    (core/gain state :challenger :credit 10)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Deus X")
    (play-from-hand state :challenger "Deus X")
    (play-run-event state (find-card "Information Sifting" (:hand (get-challenger))) :hq)
    (prompt-select :contestant (find-card "Snare!" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "PAD Campaign" (:hand (get-contestant))))
    (prompt-choice :contestant "Done")
    (is (= :waiting (-> (get-contestant) :prompt first :prompt-type)) "Contestant is waiting for Challenger selection")
    (prompt-choice :challenger "Pile 1")
    (prompt-choice :challenger "Card from Pile 1")
    ;; the cards are selected randomly :(
    (letfn [(prevent-snare [existing-dmg]
              (prompt-choice :contestant "Yes")
              (card-ability state :challenger (get-program state 0) 1)
              (prompt-choice :challenger "Done")
              (is (= (inc existing-dmg) (count (:discard (get-challenger)))) "Damage from Snare! prevented")
              (prompt-choice :challenger "Yes")
              (prompt-choice :challenger "Done") ; don't prevent Hostile dmg
              ;; chronos prompt
              (prompt-choice :contestant "Yes")
              (prompt-choice :contestant (find-card "Sure Gamble" (:hand (get-challenger))))
              (is (= (+ 2 existing-dmg) (count (:discard (get-challenger)))) "Damage from Hostile Inf not prevented"))
            (allow-pad [existing-dmg]
              (prompt-choice :challenger "Yes")
              (card-ability state :challenger (get-program state 0) 1)
              (prompt-choice :challenger "Done")
              (is (= (inc existing-dmg) (count (:discard (get-challenger)))) "Challenger prevented damage from Hostile Inf"))]
      (if (= :waiting (-> (get-challenger) :prompt first :prompt-type)) ; hit the snare
        ;; prevent the damage
        (do (prevent-snare (count (:discard (get-challenger))))
            (prompt-choice :challenger "Card from Pile 1")
            (allow-pad (count (:discard (get-challenger)))))
        (do (allow-pad (count (:discard (get-challenger))))
            (prompt-choice :challenger "Card from Pile 1")
            (prevent-snare (count (:discard (get-challenger)))))))
    (play-run-event state (find-card "Information Sifting" (:hand (get-challenger))) :hq)
    (prompt-select :contestant (find-card "Power Shutdown" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "Hedge Fund" (:hand (get-contestant))))
    (is (= :waiting (-> (get-contestant) :prompt first :prompt-type)) "Selecting max cards closed the selection prompt")
    (prompt-choice :challenger "Pile 2")
    (prompt-choice :challenger "Card from Pile 2")
    (prompt-choice :challenger "Steal")
    (is (= 1 (count (:scored (get-challenger)))) "Challenger stole agenda")))

(deftest inject
  ;; Inject - Draw 4 cards from Stack and gain 1 credit per trashed program
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Inject" 1) (qty "Imp" 2) (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Imp" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Imp" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (is (= 4 (count (:deck (get-challenger)))))
    (play-from-hand state :challenger "Inject")
    (is (= 2 (count (:hand (get-challenger)))) "2 non-programs kept in Grip")
    (is (= 2 (count (filter #(= (:type %) "Program") (:discard (get-challenger)))))
        "2 programs in Heap")
    (is (= 6 (:credit (get-challenger)))
        "Paid 1 credit to play Inject, gained 2 credits from trashed programs")))

(deftest injection-attack
  ;; Injection Attack
  (do-game
    (new-game (default-contestant [(qty "Paper Wall" 1)])
              (default-challenger [(qty "Injection Attack" 1) (qty "Corroder" 1)]))
    (play-from-hand state :contestant "Paper Wall" "Archives")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "Injection Attack")
    (prompt-choice :challenger "Archives")
    (is (= 2 (:current-strength (get-program state 0))) "Corroder at 2 strength")
    (prompt-select :challenger (get-program state 0))
    (is (= 4 (:current-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (is (= 4 (:current-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (run-successful state)
    (is (= 2 (:current-strength (get-program state 0))) "Corroder reset to 2 strength")))

(deftest interdiction
  ;; Contestant cannot rez non-ice cards during challenger's turn
  (do-game
    (new-game (default-contestant [(qty "Jeeves Model Bioroids" 1) (qty "Jackson Howard" 1)])
              (default-challenger [(qty "Street Peddler" 1)
                               (qty "Interdiction" 3)]))
    (starting-hand state :challenger ["Street Peddler" "Interdiction"])
    (play-from-hand state :contestant "Jeeves Model Bioroids" "New remote")
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Street Peddler")
    (let [jeeves (get-content state :remote1 0)
          jackson (get-content state :remote2 0)]
      (core/rez state :contestant jeeves)
      (is (get-in (refresh jeeves) [:rezzed]) "Jeeves is rezzed.  Interdiction not active when on Peddler")
      (play-from-hand state :challenger "Interdiction")
      (core/rez state :contestant jackson)
      (is (not (get-in (refresh jackson) [:rezzed])) "Jackson is not rezzed"))))

(deftest ive-had-worse
  ;; I've Had Worse - Draw 3 cards when lost to net/meat damage; don't trigger if flatlined
  (do-game
    (new-game (default-contestant [(qty "Scorched Earth" 3) (qty "Pup" 3)])
              (default-challenger [(qty "I've Had Worse" 2) (qty "Sure Gamble" 3) (qty "Imp" 2)]))
    (core/gain state :challenger :tag 1)
    (core/gain state :contestant :credit 5)
    (starting-hand state :challenger ["I've Had Worse"])
    (play-from-hand state :contestant "Pup" "HQ")
    (core/rez state :contestant (get-ice state :hq 0))
    (card-subroutine state :contestant (get-ice state :hq 0) 0)
    (is (= 1 (count (:discard (get-challenger)))))
    (is (= 3 (count (:hand (get-challenger)))) "I've Had Worse triggered and drew 3 cards")
    (starting-hand state :challenger ["I've Had Worse" "Imp" "Imp"])
    (play-from-hand state :contestant "Scorched Earth")
    (is (= 0 (count (:hand (get-challenger)))) "Challenger has 0 cards in hand")
    (is (= :contestant (:winner @state)) "Contestant wins")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")
    (is (= 4 (count (:discard (get-challenger)))) "All 3 cards in Grip trashed by Scorched Earth")
    (is (= 3 (count (:deck (get-challenger)))) "No cards drawn from I've Had Worse")))

(deftest lawyer-up
  ;; Lawyer Up - Lose 2 tags and draw 3 cards
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Lawyer Up" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/gain state :challenger :tag 3)
    (play-from-hand state :challenger "Lawyer Up")
    (is (= 3 (count (:hand (get-challenger)))) "Drew 3 cards")
    (is (= 2 (:click (get-challenger))) "Spent 2 clicks")
    (is (= 1 (:tag (get-challenger))) "Lost 2 tags")))

(deftest mad-dash
  ;; Mad Dash - Make a run. Move to score pile as 1 point if steal agenda.  Take 1 meat if not
  (do-game
    (new-game (default-contestant [(qty "Project Atlas" 1)])
              (default-challenger [(qty "Mad Dash" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Mad Dash")
    (prompt-choice :challenger "Archives")
    (run-successful state)
    (is (= 2 (count (:discard (get-challenger)))) "Took a meat damage")
    (play-from-hand state :challenger "Mad Dash")
    (prompt-choice :challenger "HQ")
    (run-successful state)
    (prompt-choice :challenger "Steal")
    (is (= 2 (count (:scored (get-challenger)))) "Mad Dash moved to score area")
    (is (= 3 (:agenda-point (get-challenger))) "Mad Dash scored for 1 agenda point")))

(deftest making-an-entrance
  ;; Making an Entrance - Full test
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Making an Entrance" 2) (qty "Sure Gamble" 1) (qty "Desperado" 1)
                               (qty "Diesel" 1) (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :challenger ["Making an Entrance"])
    (is (= 1 (count (:hand (get-challenger)))))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Making an Entrance")
    ;; trash cards
    (is (= 1 (count (:discard (get-challenger)))))
    (prompt-choice :challenger (find-card "Desperado" (:deck (get-challenger))))
    (prompt-choice :challenger (find-card "Diesel" (:deck (get-challenger))))
    (is (= 3 (count (:discard (get-challenger)))))
    (prompt-choice :challenger "None")
    ;; start arranging
    (prompt-choice :challenger (find-card "Making an Entrance" (:deck (get-challenger))))
    (prompt-choice :challenger (find-card "Sure Gamble" (:deck (get-challenger))))
    (prompt-choice :challenger (find-card "Corroder" (:deck (get-challenger))))
    (prompt-choice :challenger (find-card "Patron" (:deck (get-challenger))))
    ;; try starting over
    (prompt-choice :challenger "Start over")
    (prompt-choice :challenger (find-card "Patron" (:deck (get-challenger))))
    (prompt-choice :challenger (find-card "Corroder" (:deck (get-challenger))))
    (prompt-choice :challenger (find-card "Sure Gamble" (:deck (get-challenger))))
    (prompt-choice :challenger (find-card "Making an Entrance" (:deck (get-challenger)))) ;this is the top card on stack
    (prompt-choice :challenger "Done")
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
              (default-challenger [(qty "Mars for Martians" 1) (qty "Clan Vengeance" 1) (qty "Counter Surveillance" 1)
                               (qty "Jarogniew Mercs" 1) (qty "Sure Gamble" 3)]))
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
    (is (= 3 (count (:hand (get-challenger)))) "3 clan resources, +3 cards but -1 for playing Mars for Martians")
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
    (prompt-choice :challenger "OK")
    (prompt-choice :challenger "Yes")
    (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
    (core/no-action state :contestant nil)
    (run-successful state)
    (prompt-choice :challenger "OK")
    (is (= 9 (:credit (get-challenger))))
    (is (empty? (:prompt (get-challenger))) "No prompt to run a third time")
    (is (not (:run @state)) "Run is over")
    (play-from-hand state :challenger "Möbius")
    (run-jack-out state)
    (is (empty? (:prompt (get-challenger))) "No option to run again on unsuccessful run")))

(deftest modded
  ;; Modded - Install a program or piece of hardware at a 3 credit discount
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Modded" 2)
                               (qty "HQ Interface" 1)
                               (qty "Nerve Agent" 1)
                               (qty "Earthrise Hotel" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Modded")
    (prompt-select :challenger (find-card "Earthrise Hotel" (:hand (get-challenger))))
    (is (empty? (get-in @state [:challenger :rig :resource])) "Can't install resources with Modded")
    (prompt-select :challenger (find-card "HQ Interface" (:hand (get-challenger))))
    (is (= 1 (count (get-in @state [:challenger :rig :hardware]))) "Installed HQ Interface")
    (is (= 4 (:credit (get-challenger))) "Paid 1 credit instead of 4")
    (play-from-hand state :challenger "Modded")
    (prompt-select :challenger (find-card "Nerve Agent" (:hand (get-challenger))))
    (is (= 1 (count (get-in @state [:challenger :rig :program]))) "Installed Nerve Agent")
    (is (= 4 (:credit (get-challenger))) "Paid 0 credits")))

(deftest noble-path
  ;; The Noble Path - Prevents damage during run
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "The Noble Path" 1) (qty "Hedge Fund" 2)]))
    (let [hand-count #(count (:hand (get-challenger)))]
      (starting-hand state :challenger ["The Noble Path" "Hedge Fund"])
      (take-credits state :contestant)

      ;; Play The Noble Path and confirm it trashes remaining cards in hand
      (is (= 2 (hand-count)) "Start with 2 cards")
      (play-from-hand state :challenger "The Noble Path")
      (is (= 0 (hand-count)) "Playing Noble Path trashes the remaining cards in hand")

      ;; Put a card into hand so I can confirm it's not discarded by damage
      ;; Don't want to dealing with checking damage on a zero card hand
      (starting-hand state :challenger ["Hedge Fund"])

      (core/damage state :challenger :net 1)
      (is (= 1 (hand-count)) "Damage was prevented")

      ;; Finish the run and check that damage works again
      (prompt-choice :challenger "HQ")
      (run-successful state)
      (prompt-choice :challenger "OK")
      (core/damage state :challenger :net 1)
      (is (= 0 (hand-count)) "Damage works again after run"))))

(deftest notoriety
  ;; Notoriety - Run all 3 central servers successfully and play to gain 1 agenda point
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 1)])
              (default-challenger [(qty "Notoriety" 1)]))
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :challenger "Notoriety")
    (is (= 1 (count (:scored (get-challenger)))) "Notoriety moved to score area")
    (is (= 1 (:agenda-point (get-challenger))) "Notoriety scored for 1 agenda point")))

(deftest out-of-the-ashes
  ;; Out of the Ashes - ensure card works when played/trashed/milled
  (do-game
    (new-game (default-contestant [(qty "Kala Ghoda Real TV" 1) (qty "Underway Renovation" 1)])
              (default-challenger [(qty "Out of the Ashes" 6)]))
    (play-from-hand state :contestant "Underway Renovation" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Out of the Ashes")
    (prompt-choice :challenger "Archives")
    (is (:run @state))
    (run-successful state)
    (trash-from-hand state :challenger "Out of the Ashes")
    (trash-from-hand state :challenger "Out of the Ashes")
    (trash-from-hand state :challenger "Out of the Ashes")
    (trash-from-hand state :challenger "Out of the Ashes")
    (is (= 0 (count (:hand (get-challenger)))))
    (is (= 5 (count (:discard (get-challenger)))))
    (take-credits state :challenger)
    (let [underway (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh underway)}))
    (is (= 6 (count (:discard (get-challenger)))))
    (take-credits state :contestant)
    ;; remove 5 Out of the Ashes from the game
    (dotimes [_ 5]
      (is (not (empty? (get-in @state [:challenger :prompt]))))
      (prompt-choice :challenger "Yes")
      (prompt-choice :challenger "Archives")
      (is (:run @state))
      (run-successful state))
    (prompt-choice :challenger "No")
    (is (= 1 (count (:discard (get-challenger)))))
    (is (= 5 (count (:rfg (get-challenger)))))
    (take-credits state :challenger)
    (take-credits state :contestant)
    ;; ensure that if you decline the rfg, game will still ask the next turn
    (is (not (empty? (get-in @state [:challenger :prompt]))))
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger "Archives")
    (is (:run @state))
    (run-successful state)
    (is (= 0 (count (:discard (get-challenger)))))
    (is (= 6 (count (:rfg (get-challenger)))))))

(deftest political-graffiti
  ;; Political Graffiti - swapping with Turntable works / purging viruses restores points
  (do-game
    (new-game (default-contestant [(qty "Breaking News" 1) (qty "Chronos Project" 1)])
              (default-challenger [(qty "Turntable" 1) (qty "Political Graffiti" 1)]))
    (play-from-hand state :contestant "Breaking News" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (is (= 1 (:agenda-point (get-contestant))))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Political Graffiti")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (prompt-choice :challenger "Run ability")
    (prompt-select :challenger (find-card "Breaking News" (:scored (get-contestant))))
    (is (= 0 (:agenda-point (get-contestant))) "Political Dealings lowered agenda points by 1")
    (play-from-hand state :challenger "Turntable")
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Steal")
    (let [tt (get-in @state [:challenger :rig :hardware 0])]
      (prompt-choice :challenger "Yes")
      (prompt-select :challenger (find-card "Breaking News" (:scored (get-contestant))))
      (is (= 1 (:agenda-point (get-contestant))))
      (is (= 0 (:agenda-point (get-challenger))))
      (take-credits state :challenger)
      (core/purge state :contestant)
      (is (= 1 (:agenda-point (get-contestant))))
      (is (= 1 (:agenda-point (get-challenger)))))))

(deftest political-graffiti-forfeit
  ;; Political Graffiti - forfeiting agenda with Political Graffiti does not refund double points
  ;; Regression test for issue #2765
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 1) (qty "Sacrifice" 1)])
              (default-challenger [(qty "Political Graffiti" 1)]))
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (is (= 1 (:agenda-point (get-contestant))))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Political Graffiti")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (prompt-choice :challenger "Run ability")
    (prompt-select :challenger (find-card "Hostile Takeover" (:scored (get-contestant))))
    (is (= 0 (:agenda-point (get-contestant))) "Political Dealings lowered agenda points by 1")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Sacrifice")
    (prompt-select :contestant (get-scored state :contestant 0))
    (is (= 0 (:agenda-point (get-contestant))) "Forfeiting agenda did not refund extra agenda points ")
    (is (= 1 (count (:discard (get-challenger)))) "Political Graffiti is in the Heap")))

(deftest push-your-luck-correct-guess
  ;; Push Your Luck - Contestant guesses correctly
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Push Your Luck" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Push Your Luck")
    (prompt-choice :contestant "Odd")
    (prompt-choice :challenger 3)
    (is (= 0 (:credit (get-challenger))) "Contestant guessed correctly")))

(deftest push-your-luck-incorrect-guess
  ;; Push Your Luck - Contestant guesses incorrectly
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Push Your Luck" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Push Your Luck")
    (prompt-choice :contestant "Even")
    (prompt-choice :challenger 3)
    (is (= 6 (:credit (get-challenger))) "Contestant guessed incorrectly")))

(deftest pushing-the-envelope
  ;; Run. Add 2 strength to each installer breaker.
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Pushing the Envelope" 3) (qty "Corroder" 2) (qty "Atman" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 20)
    (core/gain state :challenger :click 10)
    (core/draw state :challenger)
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "Atman")
    (prompt-choice :challenger 0)
    (let [atman (get-in @state [:challenger :rig :program 1])
          corr (get-in @state [:challenger :rig :program 0])]
      (is (= 0 (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (play-from-hand state :challenger "Pushing the Envelope")
      (prompt-choice :challenger "Archives")
      ; 3 cards in hand - no boost
      (is (= 0 (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (run-successful state)
      (play-from-hand state :challenger "Pushing the Envelope")
      (prompt-choice :challenger "Archives")
      (run-continue state)
      ; 2 cards in hand - boost
      (is (= 2 (:current-strength (refresh atman))) "Atman 2 current strength")
      (is (= 4 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (run-successful state)
      (is (= 0 (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength"))))

(deftest queens-gambit
  ;; Check that Queen's Gambit prevents access of card #1542
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 2)])
              (default-challenger [(qty "Queen's Gambit" 1)]))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Queen's Gambit")
    (let [pad (get-content state :remote1 0)
          challenger-creds (:credit (get-challenger))]
      (prompt-choice :challenger "3")
      (prompt-select :challenger pad)
      (is (= (+ challenger-creds 6) (:credit (get-challenger))) "Gained 6 credits from Queen's Gambit")
      (is (= 3 (:advance-counter (refresh pad))) "3 advancement counters placed on PAD Campaign by Queen's Gambit")
      (is (not (core/can-access? state :challenger (refresh pad))) "Cannot access PAD Campgain")
      (run-empty-server state "Server 1")
      (is (not (:run @state)) "Run ended since no cards could be accessed"))
    (let [other-pad (get-content state :remote2 0)]
      (is (core/can-access? state :challenger other-pad)) "Not prevented from accessing other cards")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (let [pad (get-content state :remote1 0)
          challenger-creds (:credit (get-challenger))]
      (run-empty-server state "Server 1")
      (is (core/can-access? state :challenger (refresh pad)) "Can access PAD Campgain next turn")
      (prompt-choice :challenger "Yes")
      (is (= (- challenger-creds 4) (:credit (get-challenger))) "Paid 4 credits to trash PAD Campaign"))))

;; Rebirth
(let [choose-challenger (fn [name state prompt-map]
                      (let [kate-choice (some #(when (= name (:title %)) %) (:choices (prompt-map :challenger)))]
                        (core/resolve-prompt state :challenger {:card kate-choice})))

      ayla "Ayla \"Bios\" Rahim: Simulant Specialist"
      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"]

  (deftest rebirth-kate
    ;; Rebirth - Kate's discount applies after rebirth
    (do-game
      (new-game (default-contestant) (default-challenger ["Magnum Opus" "Rebirth"]) {:start-as :challenger})

      (play-from-hand state :challenger "Rebirth")
      (is (= (first (prompt-titles :challenger)) ayla) "List is sorted")
      (is (every?   #(some #{%} (prompt-titles :challenger))
                    [kate kit]))
      (is (not-any? #(some #{%} (prompt-titles :challenger))
                    [professor whizzard jamie]))

      (choose-challenger kate state prompt-map)

      (is (= kate (-> (get-challenger) :identity :title)))
      (is (= 1 (:link (get-challenger))) "1 link")

      (is (empty? (:discard (get-challenger))))
      (is (= "Rebirth" (-> (get-challenger) :rfg first :title)))

      (is (changes-credits (get-challenger) -4
        (play-from-hand state :challenger "Magnum Opus")))))

  (deftest-pending rebirth-kate-twice
    ;; Rebirth - Kate's discount does not after rebirth if something already installed
    (do-game
      (new-game (default-contestant) (default-challenger ["Akamatsu Mem Chip" "Rebirth" "Clone Chip"]) {:start-as :challenger})

      (play-from-hand state :challenger "Clone Chip")
      (play-from-hand state :challenger "Rebirth")
      (choose-challenger kate state prompt-map)

      (is (changes-credits (get-contestant) -1
        (play-from-hand state :challenger "Akamatsu Mem Chip"))
        "Discount not applied for 2nd install")))

  (deftest rebirth-whizzard
    ;; Rebirth - Whizzard works after rebirth
    (do-game
      (new-game (default-contestant ["Ice Wall"]) (make-deck reina ["Rebirth"]))
      (play-from-hand state :contestant "Ice Wall" "R&D")
      (take-credits state :contestant)

      (play-from-hand state :challenger "Rebirth")
      (choose-challenger whizzard state prompt-map)

      (card-ability state :challenger (:identity (get-challenger)) 0)
      (is (= 6 (:credit (get-challenger))) "Took a Whizzard credit")

      (is (changes-credits (get-contestant) -1
        (core/rez state :contestant (get-ice state :rd 0)))
        "Reina is no longer active")))

  (deftest rebirth-lose-link
    ;; Rebirth - Lose link from ID
    (do-game
      (new-game (default-contestant)
                (make-deck kate ["Rebirth" "Access to Globalsec"])
                {:start-as :challenger})
      (play-from-hand state :challenger "Access to Globalsec")
      (is (= 2 (:link (get-challenger))) "2 link before rebirth")

      (play-from-hand state :challenger "Rebirth")
      (choose-challenger chaos state prompt-map)
      (is (= 1 (:link (get-challenger))) "1 link after rebirth")))

  (deftest rebirth-gain-link
    ;; Rebirth - Gain link from ID
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Rebirth" "Access to Globalsec"])
                {:start-as :challenger})
      (play-from-hand state :challenger "Access to Globalsec")
      (is (= 1 (:link (get-challenger))) "1 link before rebirth")

      (play-from-hand state :challenger "Rebirth")
      (choose-challenger kate state prompt-map)
      (is (= 2 (:link (get-challenger))) "2 link after rebirth"))))

(deftest rigged-results
  ;; Rigged Results - success and failure
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 1)])
              (default-challenger [(qty "Rigged Results" 3)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Rigged Results")
    (prompt-choice :challenger "0")
    (prompt-choice :contestant "0")
    (is (empty? (:prompt (get-challenger))) "Rigged Results failed for challenger")
    (is (empty? (:prompt (get-contestant))) "Rigged Results failed for challenger")
    (play-from-hand state :challenger "Rigged Results")
    (prompt-choice :challenger "2")
    (prompt-choice :contestant "1")
    (prompt-select :challenger (get-ice state :hq 0))
    (is (= [:hq] (:server (:run @state))) "Challenger is running on HQ")
    (is (= 3 (:credit (get-challenger))) "Rigged results spends credits")))

(deftest retrieval-run
  ;; Retrieval Run - Run Archives successfully and install a program from Heap for free
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Retrieval Run" 1) (qty "Morning Star" 1)]))
    (take-credits state :contestant)
    (trash-from-hand state :challenger "Morning Star")
    (play-from-hand state :challenger "Retrieval Run")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (prompt-choice :challenger "Run ability")
    (let [ms (first (:discard (get-challenger)))]
      (prompt-choice :challenger ms)
      (is (= "Morning Star" (:title (first (get-in @state [:challenger :rig :program]))))
          "Morning Star installed")
      (is (= 2 (:credit (get-challenger))) "Morning Star installed at no cost")
      (is (= 2 (:memory (get-challenger))) "Morning Star uses 2 memory"))))

(deftest rumor-mill
  ;; Rumor Mill - interactions with rez effects, additional costs, general event handlers, and trash-effects
  (do-game
    (new-game
      (default-contestant [(qty "Project Atlas" 2)
                     (qty "Caprice Nisei" 1) (qty "Chairman Hiro" 1) (qty "Cybernetics Court" 1)
                     (qty "Elizabeth Mills" 1)
                     (qty "Ibrahim Salem" 1)
                     (qty "Housekeeping" 1)
                     (qty "Director Haas" 1)
                     (qty "Oberth Protocol" 1)])
      (default-challenger [(qty "Rumor Mill" 1)]))
    (core/gain state :contestant :credit 100 :click 100 :bad-publicity 1)
    (core/draw state :contestant 100)
    (play-from-hand state :contestant "Caprice Nisei" "New remote")
    (play-from-hand state :contestant "Chairman Hiro" "New remote")
    (play-from-hand state :contestant "Cybernetics Court" "New remote")
    (play-from-hand state :contestant "Elizabeth Mills" "New remote")
    (play-from-hand state :contestant "Project Atlas" "New remote")
    (play-from-hand state :contestant "Ibrahim Salem" "New remote")
    (play-from-hand state :contestant "Oberth Protocol" "New remote")
    (core/move state :contestant (find-card "Director Haas" (:hand (get-contestant))) :deck)
    (core/rez state :contestant (get-content state :remote2 0))
    (core/rez state :contestant (get-content state :remote3 0))
    (score-agenda state :contestant (get-content state :remote5 0))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 100 :click 100)
    (is (= 4 (:hand-size-modification (get-contestant))) "Contestant has +4 hand size")
    (is (= -2 (:hand-size-modification (get-challenger))) "Challenger has -2 hand size")

    (play-from-hand state :challenger "Rumor Mill")

    ;; Additional costs to rez should NOT be applied
    (core/rez state :contestant (get-content state :remote6 0))
    (is (= 1 (count (:scored (get-contestant)))) "No agenda was auto-forfeit to rez Ibrahim Salem")

    ;; In-play effects
    (is (= 0 (:hand-size-modification (get-contestant))) "Contestant has original hand size")
    (is (= 0 (:hand-size-modification (get-challenger))) "Challenger has original hand size")

    ;; "When you rez" effects should not apply
    (core/rez state :contestant (get-content state :remote4 0))
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant still has 1 bad publicity")

    ;; Run events (Caprice)
    ;; Make sure Rumor Mill applies even if card is rezzed after RM is put in play.
    (core/rez state :contestant (get-content state :remote1 0))
    (run-on state :remote1)
    (run-continue state)
    (is (empty? (:prompt (get-contestant))) "Caprice prompt is not showing")
    (run-jack-out state)

    ;; Trashable execs
    (run-empty-server state :remote2)
    (prompt-choice :challenger "Yes")
    (is (empty? (:scored (get-challenger))) "Chairman Hiro not added to challenger's score area")
    (run-jack-out state)
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :challenger "Yes")
    (is (empty? (:scored (get-challenger))) "Director Haas not added to challenger's score area")
    (take-credits state :challenger)

    ;; Trash RM, make sure everything works again
    (play-from-hand state :contestant "Housekeeping")
    (is (= 4 (:hand-size-modification (get-contestant))) "Contestant has +4 hand size")
    (is (= 0 (:hand-size-modification (get-challenger))) "Challenger has +0 hand size")

    ;; Additional costs to rez should now be applied again
    (core/rez state :contestant (get-content state :remote7 0))
    (prompt-select :contestant (get-in (get-contestant) [:scored 0]))
    (is (zero? (count (:scored (get-contestant)))) "Agenda was auto-forfeit to rez Oberth")

    (core/derez state :contestant (get-content state :remote4 0))
    (core/rez state :contestant (get-content state :remote4 0))
    (is (= 0 (:bad-publicity (get-contestant))) "Contestant has 0 bad publicity")
    (card-ability state :contestant (get-content state :remote4 0) 0) ; Elizabeth Mills, should show a prompt
    (is (:prompt (get-contestant)) "Elizabeth Mills ability allowed")))

(deftest rumor-mill-street-peddler
  ;; Make sure Rumor Mill is not active when hosted on Peddler
  (do-game
    (new-game (default-contestant [(qty "Jeeves Model Bioroids" 1)])
              (default-challenger [(qty "Street Peddler" 1)
                               (qty "Rumor Mill" 3)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Street Peddler"])
    (play-from-hand state :challenger "Street Peddler")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Jeeves Model Bioroids" "New remote")
    (let [jeeves (get-content state :remote1 0)]
      (core/rez state :contestant jeeves)
      (card-ability state :contestant jeeves 0)
      (is (= 3 (:click (get-contestant))) "Contestant has 3 clicks - Jeeves working ok"))))

(deftest scrubbed
  ;; First piece of ice encountered each turn has -2 Strength for remainder of the run
  (do-game
    (new-game (default-contestant [(qty "Turing" 1)])
              (default-challenger [(qty "Street Peddler" 1)
                               (qty "Scrubbed" 3)]))
    (starting-hand state :challenger ["Street Peddler" "Scrubbed"])
    (play-from-hand state :contestant "Turing" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Street Peddler")
    (let [turing (get-ice state :hq 0)]
      (core/rez state :contestant turing)
      (is (= 2 (:current-strength (refresh turing))))
      (run-on state "HQ")
      (run-continue state)
      (is (= 2 (:current-strength (refresh turing))) "Scrubbed not active when on Peddler")
      (play-from-hand state :challenger "Scrubbed")
      (run-on state "HQ")
      (run-continue state)
      (is (= 0 (:current-strength (refresh turing))) "Scrubbed reduces strength by 2")
      (run-successful state))))

(deftest singularity
  ;; Singularity - Run a remote; if successful, trash all contents at no cost
  (do-game
    (new-game (default-contestant [(qty "Caprice Nisei" 1)
                             (qty "Breaker Bay Grid" 1)
                             (qty "Eve Campaign" 1)])
              (default-challenger [(qty "Singularity" 1)]))
    (play-from-hand state :contestant "Breaker Bay Grid" "New remote")
    (play-from-hand state :contestant "Caprice Nisei" "Server 1")
    (play-from-hand state :contestant "Eve Campaign" "Server 1")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Singularity")
    (prompt-choice :challenger "Server 1")
    (is (= 2 (:click (get-challenger))) "Challenger spends 2 clicks on double event")
    (is (= 1 (:credit (get-challenger))) "Challenger pays 4 credits for Singularity")
    (run-successful state)
    (is (= 3 (count (:discard (get-contestant)))) "All 3 cards trashed from Server 1")
    (is (= 1 (:credit (get-challenger))) "No credits paid for trashing")
    (is (nil? (get-in @state [:contestant :servers :remote1 :content])) "Server 1 no longer exists")))

(deftest stimhack
  ;; Stimhack - Gain 9 temporary credits and take 1 brain damage after the run
  (do-game
    (new-game (default-contestant [(qty "Eve Campaign" 1)])
              (default-challenger [(qty "Stimhack" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Stimhack")
    (prompt-choice :challenger "HQ")
    (is (= [:hq] (get-in @state [:run :server])) "Run initiated on HQ")
    (run-successful state)
    (is (= 14 (:credit (get-challenger))))
    (is (= 9 (:run-credit (get-challenger))) "Gained 9 credits for use during the run")
    (prompt-choice :challenger "Yes") ; choose to trash Eve
    (is (and (= 0 (count (:hand (get-contestant))))
             (= 1 (count (:discard (get-contestant)))))
        "Contestant hand empty and Eve in Archives")
    (is (= 5 (:credit (get-challenger))))
    (is (= 0 (count (:hand (get-challenger)))) "Lost card from Grip to brain damage")
    (is (= 4 (core/hand-size state :challenger)))
    (is (= 1 (:brain-damage (get-challenger))))))

(deftest sure-gamble
  ;; Sure Gamble
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Sure Gamble" 1)]))
    (take-credits state :contestant)
    (is (= 5 (:credit (get-challenger))))
    (play-from-hand state :challenger "Sure Gamble")
    (is (= 9 (:credit (get-challenger))))))

;; Surge and virus counter flag tests
(deftest surge-valid-target
  ;; Add counters if target is a virus and had a counter added this turn
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Imp" 1) (qty "Surge" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Imp")
    (let [imp (get-in @state [:challenger :rig :program 0])]
      (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
      (play-from-hand state :challenger "Surge")
      (prompt-select :challenger imp)
      (is (= 4 (get-counters (refresh imp) :virus)) "Imp has 4 counters after surge"))))

(deftest surge-target-not-virus
  ;; Don't fire surge if target is not a virus
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Security Testing" 1) (qty "Surge" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Security Testing")
    (let [st (get-in @state [:challenger :rig :resource 0])]
      (play-from-hand state :challenger "Surge")
      (prompt-select :challenger st)
      (is (not (contains? st :counter)) "Surge does not fire on Security Testing"))))

(deftest surge-target-no-token-this-turn
  ;; Don't fire surge if target does not have virus counter flag set
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Imp" 1) (qty "Surge" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Imp")
    (let [imp (get-in @state [:challenger :rig :program 0])]
      (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
      (take-credits state :challenger 3)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Surge")
      (prompt-select :challenger imp)
      (is (= 2 (get-counters (refresh imp) :virus))
          "Surge does not fire on Imp turn after install"))))

(deftest surge-target-gorman-drip
  ;; Don't allow surging Gorman Drip, since it happens on the contestant turn
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Gorman Drip v1" 1) (qty "Surge" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Gorman Drip v1")
    (let [gd (get-in @state [:challenger :rig :program 0])]
      (is (= 0 (get-counters gd :virus)) "Gorman Drip starts without counters")
      (take-credits state :challenger 3)
      (take-credits state :contestant)
      (is (= 3 (get-counters (refresh gd) :virus))
          "Gorman Drip gains 3 counters after Contestant clicks 3 times for credits")
      (play-from-hand state :challenger "Surge")
      (prompt-select :challenger gd)
      (is (= 3 (get-counters (refresh gd) :virus)) "Surge does not trigger on Gorman Drip"))))

(deftest system-outage
  ;; When Contestant draws 1+ cards, it loses 1 if it is not the first time he or she has drawn cards this turn
  (do-game
    (new-game (default-contestant [(qty "Turing" 10)])
              (default-challenger [(qty "Street Peddler" 1)
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

(deftest test-run
  ;; Test Run - Programs hosted after install get returned to Stack. Issue #1081
  (do-game
    (new-game (default-contestant [(qty "Wraparound" 1)])
              (default-challenger [(qty "Test Run" 2) (qty "Morning Star" 1)
                               (qty "Knight" 1) (qty "Leprechaun" 1)]))
    (play-from-hand state :contestant "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (core/rez state :contestant wrap)
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5)
      (core/move state :challenger (find-card "Morning Star" (:hand (get-challenger))) :discard)
      (core/move state :challenger (find-card "Knight" (:hand (get-challenger))) :discard)
      (let [ms (find-card "Morning Star" (:discard (get-challenger)))]
        (play-from-hand state :challenger "Leprechaun")
        (play-from-hand state :challenger "Test Run")
        (prompt-choice :challenger "Heap")
        (prompt-choice :challenger ms)
        (let [lep (get-in @state [:challenger :rig :program 0])
              ms (get-in @state [:challenger :rig :program 1])]
          (card-ability state :challenger lep 1)
          (prompt-select :challenger ms)
          (is (= "Morning Star" (:title (first (:hosted (refresh lep))))) "Morning Star hosted on Lep")
          (take-credits state :challenger)
          (is (= "Morning Star" (:title (first (:deck (get-challenger))))) "Morning Star returned to Stack from host")
          (take-credits state :contestant)
          (let [kn (find-card "Knight" (:discard (get-challenger)))]
            (play-from-hand state :challenger "Test Run")
            (prompt-choice :challenger "Heap")
            (prompt-choice :challenger kn)
            (let [kn (get-in @state [:challenger :rig :program 1])]
              (card-ability state :challenger kn 0)
              (prompt-select :challenger wrap)
              (is (= "Knight" (:title (first (:hosted (refresh wrap))))) "Knight hosted on Wraparound")
              (take-credits state :challenger)
              (is (= "Knight" (:title (first (:deck (get-challenger))))) "Knight returned to Stack from host ICE"))))))))

(deftest test-run-scavenge
  ;; Test Run - Make sure program remains installed if Scavenged
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Test Run" 1) (qty "Morning Star" 1)
                               (qty "Scavenge" 1) (qty "Inti" 1)]))
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Morning Star" (:hand (get-challenger))) :discard)
    (play-from-hand state :challenger "Test Run")
    (let [ms (find-card "Morning Star" (:discard (get-challenger)))]
      (prompt-choice :challenger "Heap")
      (prompt-choice :challenger ms)
      (is (= 2 (:credit (get-challenger))) "Program installed for free")
      (let [ms (get-in @state [:challenger :rig :program 0])]
        (play-from-hand state :challenger "Scavenge")
        (prompt-select :challenger ms)
        (prompt-select :challenger (find-card "Morning Star" (:discard (get-challenger))))
        (take-credits state :challenger)
        (is (empty? (:deck (get-challenger))) "Morning Star not returned to Stack")
        (is (= "Morning Star" (:title (get-in @state [:challenger :rig :program 0]))) "Morning Star still installed")))))

(deftest makers-eye
  (do-game
    (new-game (default-contestant [(qty "Quandary" 5)])
              (default-challenger [(qty "The Maker's Eye" 1)]))
    (dotimes [_ 5] (core/move state :contestant (first (:hand (get-contestant))) :deck))
    (take-credits state :contestant)
    (play-from-hand state :challenger "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    (run-successful state)
    (prompt-choice :challenger "Card from deck")
    (is (= "You accessed Quandary" (-> (get-challenger) :prompt first :msg)) "1st quandary")
    (prompt-choice :challenger "OK")
    (prompt-choice :challenger "Card from deck")
    (is (= "You accessed Quandary" (-> (get-challenger) :prompt first :msg)) "2nd quandary")
    (prompt-choice :challenger "OK")
    (prompt-choice :challenger "Card from deck")
    (is (= "You accessed Quandary" (-> (get-challenger) :prompt first :msg)) "3rd quandary")
    (prompt-choice :challenger "OK")
    (is (not (:run @state)))))

(deftest the-price-of-freedom
  ;; The Price of Freedom - A connection must be trashed, the card is removed from game, then the contestant can't advance cards next turn
  (do-game
    (new-game (default-contestant [(qty "NAPD Contract" 1)])
              (default-challenger [(qty "Kati Jones" 1) (qty "The Price of Freedom" 1)]))
    (play-from-hand state :contestant "NAPD Contract" "New remote")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))) "Contestant has 7 credits (play NAPD + 2 clicks for credit")
    (play-from-hand state :challenger "The Price of Freedom")
    (is (= 2 (count (get-in @state [:challenger :hand]))) "The Price of Freedom could not be played because no connection is installed")
    (is (= 0 (count (get-in (get-challenger) [:rig :resource]))) "Kati Jones is not installed")
    (play-from-hand state :challenger "Kati Jones")
    (is (= 1 (count (get-in @state [:challenger :rig :resource]))) "Kati Jones was installed")
    (play-from-hand state :challenger "The Price of Freedom")
    (is (= 0 (count (get-in @state [:challenger :hand]))) "The Price of Freedom can be played because a connection is in play")
    (let [kj (find-card "Kati Jones" (:resource (:rig (get-challenger))))]
      (prompt-choice :challenger kj)
      (is (= 0 (count (get-in (get-challenger) [:rig :resource]))) "Kati Jones was trashed wth The Price of Freedom")
      (is (= 1 (count (get-in (get-challenger) [:discard]))) "The Price of Freedom was removed from game, and only Kati Jones is in the discard"))
    (take-credits state :challenger)
    (let [napd (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh napd)})
      (is (= 7 (:credit (get-contestant))) "NAPD contract could not be advanced because of The Price of Freedom")
      (take-credits state :contestant)
      (is (= 10 (:credit (get-contestant))) "Contestant has 10 credits now (3 clicks for credit, no click charged for failed advancing)")
      (take-credits state :challenger)
      (core/advance state :contestant {:card (refresh napd)})
      (core/advance state :contestant {:card (refresh napd)})
      (core/advance state :contestant {:card (refresh napd)})
      (is (= 7 (:credit (get-contestant))) "NAPD could be advanced (3 credits charged for advancing)"))))

(deftest tinkering
  ;; Tinkering - Add subtypes to ice
  (do-game
    (new-game
      (default-contestant [(qty "Ice Wall" 1)])
      (default-challenger [(qty "Tinkering" 1)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Tinkering")
    (let [iwall (get-ice state :hq 0)]
      (prompt-select :challenger iwall)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (core/has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (core/rez state :contestant iwall)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (core/has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (take-credits state :challenger)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (not (core/has-subtype? (refresh iwall) "Code Gate")) "Ice Wall does not have Code Gate")
      (is (not (core/has-subtype? (refresh iwall) "Sentry")) "Ice Wall does not have Sentry"))))

(deftest unscheduled-maintenance
  ;; Unscheduled Maintenance - prevent Contestant from installing more than 1 ICE per turn
  (do-game
    (new-game
      (default-contestant [(qty "Vanilla" 2) (qty "Breaking News" 1)])
      (default-challenger [(qty "Unscheduled Maintenance" 1)]))
    (play-from-hand state :contestant "Breaking News" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Unscheduled Maintenance")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Vanilla" "HQ")
    (is (= 1 (count (get-in @state [:contestant :servers :hq :ices]))) "First ICE install of turn allowed")
    (play-from-hand state :contestant "Vanilla" "R&D")
    (is (empty? (get-in @state [:contestant :servers :rd :ices])) "Second ICE install of turn blocked")
    (score-agenda state :contestant (get-content state :remote1 0))
    (play-from-hand state :contestant "Vanilla" "R&D")
    (is (= 1 (count (get-in @state [:contestant :servers :rd :ices]))) "Current trashed; second ICE install of turn allowed")))

(deftest vamp
  ;; Vamp - Run HQ and use replace access to pay credits to drain equal amount from Contestant
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Vamp" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (is (= 8 (:credit (get-contestant))))
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Sure Gamble")
    (is (= 13 (:credit (get-challenger))))
    (play-run-event state (find-card "Vamp" (:hand (get-challenger))) :hq)
    (prompt-choice :challenger "Run ability")
    (prompt-choice :challenger 8)
    (is (= 1 (:tag (get-challenger))) "Took 1 tag")
    (is (= 5 (:credit (get-challenger))) "Paid 8 credits")
    (is (= 0 (:credit (get-contestant))) "Contestant lost all 8 credits")))

(deftest virus-counter-flag-on-enter
  ;; Set counter flag when virus card enters play with counters
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Surge" 1) (qty "Imp" 1) (qty "Crypsis" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Imp")
    (let [imp (get-in @state [:challenger :rig :program 0])]
      (is (get-in imp [:added-virus-counter]) "Counter flag was not set on Imp"))))

(deftest virus-counter-flag-on-add-prop
  ;; Set counter flag when add-prop is called on a virus
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Crypsis" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Crypsis")
    (let [crypsis (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger crypsis 2) ;click to add a virus counter
      (is (= 1 (get-counters (refresh crypsis) :virus)) "Crypsis added a virus token")
      (is (get-in (refresh crypsis) [:added-virus-counter])
          "Counter flag was set on Crypsis"))))

(deftest virus-counter-flag-clear-on-end-turn
  ;; Clear the virus counter flag at the end of each turn
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Crypsis" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Crypsis")
    (let [crypsis (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger crypsis 2) ; click to add a virus counter
      (take-credits state :challenger 2)
      (take-credits state :contestant 1)
      (is (not (get-in (refresh crypsis) [:added-virus-counter]))
          "Counter flag was cleared on Crypsis"))))
