(ns test.cards.events
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest account-siphon-ability
  ;; Account Siphon - Use ability
  (do-game
    (new-game (default-corp) (default-runner [(qty "Account Siphon" 3)]))
    (take-credits state :resPlayer)
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    ;; play Account Siphon, use ability
    (play-run-event state (first (:hand (get-runner))) :hq)
    (prompt-choice :hazPlayer "Run ability")
    (is (= 2 (:tag (get-runner))) "Runner took 2 tags")
    (is (= 15 (:credit (get-runner))) "Runner gained 10 credits")
    (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")))

(deftest account-siphon-access
  ;; Account Siphon - Access
  (do-game
    (new-game (default-corp) (default-runner [(qty "Account Siphon" 3)]))
    (take-credits state :resPlayer) ; pass to runner's turn by taking credits
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    ;; play another Siphon, do not use ability
    (play-run-event state (first (get-in @state [:hazPlayer :hand])) :hq)
    (prompt-choice :hazPlayer "Access")
    (is (= 0 (:tag (get-runner))) "Runner did not take any tags")
    (is (= 5 (:credit (get-runner))) "Runner did not gain any credits")
    (is (= 8 (:credit (get-corp))) "Corp did not lose any credits")))

(deftest account-siphon-nach-interaction
  ;; Account Siphon - Access
  (do-game
    (new-game (default-corp) (default-runner [(qty "Account Siphon" 1)
                                              (qty "New Angeles City Hall" 1)]))
    (core/gain state :resPlayer :bad-publicity 1)
    (is (= 1 (:bad-publicity (get-corp))) "Corp has 1 bad publicity")
    (core/lose state :hazPlayer :credit 1)
    (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
    (take-credits state :resPlayer) ; pass to runner's turn by taking credits
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    (play-from-hand state :hazPlayer "New Angeles City Hall")
    (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
    (let [nach (get-in @state [:hazPlayer :rig :resource 0])]
      (play-run-event state (first (get-in @state [:hazPlayer :hand])) :hq)
      (prompt-choice :hazPlayer "Run ability")
      (is (= 4 (:credit (get-runner))) "Runner still has 4 credits due to BP")
      (card-ability state :hazPlayer nach 0)
      (is (= 2 (:credit (get-runner))) "Runner has 2 credits left")
      (card-ability state :hazPlayer nach 0)
      (is (= 0 (:credit (get-runner))) "Runner has no credits left")
      (prompt-choice :hazPlayer "Done"))
    (is (= 0 (:tag (get-runner))) "Runner did not take any tags")
    (is (= 10 (:credit (get-runner))) "Runner gained 10 credits")
    (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")))

(deftest amped-up
  ;; Amped Up - Gain 3 clicks and take 1 unpreventable brain damage
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Amped Up" 1)
                               (qty "Feedback Filter" 1)
                               (qty "Sure Gamble" 3)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Feedback Filter")
    (play-from-hand state :hazPlayer "Amped Up")
    (is (empty? (:prompt (get-runner)))
        "Feedback Filter brain damage prevention opportunity not given")
    (is (= 5 (:click (get-runner))) "Runner gained 2 clicks from Amped Up")
    (is (= 2 (count (:discard (get-runner)))) "Runner discarded 1 card from damage")
    (is (= 4 (core/hand-size state :hazPlayer)) "Runner handsize decreased by 1")
    (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")))

(deftest another-day-another-paycheck
  ;; Another Day, Another Paycheck
  (do-game
    (new-game
      (default-corp [(qty "Project Atlas" 3)])
      (default-runner [(qty "Street Peddler" 1) (qty "Another Day, Another Paycheck" 2)]))
    (starting-hand state :hazPlayer ["Street Peddler" "Another Day, Another Paycheck"])
    (play-from-hand state :resPlayer "Project Atlas" "New remote")
    (score-agenda state :resPlayer (get-content state :remote1 0))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Street Peddler")
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "Steal")
    (is (= 5 (:credit (get-runner))) "No trace, no gain")
    (play-from-hand state :hazPlayer "Another Day, Another Paycheck")
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "Steal")
    (prompt-choice :resPlayer 0)
    (prompt-choice :hazPlayer 1)
    ;; 4 credits after trace, gain 6
    (is (= 10 (:credit (get-runner))) "Runner gained 6 credits")))

(deftest apocalypse-hosting
  ;; Apocalypse - Ensure MU is correct and no duplicate cards in heap
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 2) (qty "Ice Wall" 1)])
              (default-runner [(qty "Scheherazade" 1) (qty "Corroder" 1)
                               (qty "Hyperdriver" 1) (qty "Apocalypse" 2)]))
    (play-from-hand state :resPlayer "Ice Wall" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :click 3)
    (play-from-hand state :hazPlayer "Scheherazade")
    (let [scheherazade (get-in @state [:hazPlayer :rig :program 0])]
      (card-ability state :hazPlayer scheherazade 0)
      (prompt-select :hazPlayer (find-card "Corroder" (:hand (get-runner))))
      (is (= 3 (:memory (get-runner))) "Memory at 3 (-1 from Corroder)"))
    (play-from-hand state :hazPlayer "Hyperdriver")
    (is (= 0 (:memory (get-runner))) "Memory at 0 (-1 from Corroder, -3 from Hyperdriver)")
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :hazPlayer "Apocalypse")
    (is (= 0 (count (core/all-installed state :resPlayer))) "All installed Corp cards trashed")
    (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
    (is (= 3 (count (get-in @state [:hazPlayer :rig :facedown]))) "Scheherazade, Corroder, Hyperdriver facedown")
    (is (= 1 (count (:discard (get-runner)))) "Only Apocalypse is in the heap")
    (is (= 4 (:memory (get-runner))) "Memory back to 4")))

(deftest apocalype-full-immersion-recstudio
  ;; Apocalypse with Full Immersion - no duplicate cards in heap #2606
  (do-game
    (new-game
      (default-corp [(qty "Full Immersion RecStudio" 1) (qty "Sandburg" 1)
                     (qty "Oaktown Renovation" 1)])
      (default-runner  [(qty "Apocalypse" 1)]))
    (play-from-hand state :resPlayer "Full Immersion RecStudio" "New remote")
    (let [fir (get-content state :remote1 0)]
      (core/rez state :resPlayer fir)
      (card-ability state :resPlayer fir 0)
      (prompt-select :resPlayer (find-card "Sandburg" (:hand (get-corp))))
      (card-ability state :resPlayer fir 0)
      (prompt-select :resPlayer (find-card "Oaktown Renovation" (:hand (get-corp))))
      (take-credits state :resPlayer)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :hazPlayer "Apocalypse")
      (is (= 0 (count (core/all-installed state :resPlayer))) "All installed Corp cards trashed")
      (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
      (is (= 1 (count (:discard (get-runner)))) "Only Apocalypse is in the heap"))))

(deftest apocalypse-in-play-ability
  ;; Apocalypse - Turn Runner cards facedown and reduce memory and hand-size gains
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 2) (qty "Ice Wall" 1)])
              (default-runner [(qty "Logos" 3) (qty "Apocalypse" 3)]))
    (play-from-hand state :resPlayer "Ice Wall" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Logos")
    (is (= 1 (:hand-size-modification (get-runner))) "Hand-size increased from Logos")
    (is (= 5 (:memory (get-runner))) "Memory increased from Logos")
    (core/gain state :hazPlayer :click 1 :credit 2)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :hazPlayer "Apocalypse")
    (is (= 0 (count (core/all-installed state :resPlayer))) "All installed Corp cards trashed")
    (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
    (let [logos (get-in @state [:hazPlayer :rig :facedown 0])]
      (is (:facedown (refresh logos)) "Logos is facedown")
      (is (= 0 (:hand-size-modification (get-runner))) "Hand-size reset with Logos facedown")
      (is (= 4 (:memory (get-runner))) "Memory reset with Logos facedown"))))

(deftest apocalypse-turn-facedown
  ;; Apocalypse - Turn Runner cards facedown without firing their leave play effects
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 2) (qty "Ice Wall" 1)])
              (default-runner [(qty "Tri-maf Contact" 3) (qty "Apocalypse" 3)]))
    (play-from-hand state :resPlayer "Ice Wall" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Tri-maf Contact")
    (core/gain state :hazPlayer :click 2)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :hazPlayer "Apocalypse")
    (is (= 0 (count (core/all-installed state :resPlayer))) "All installed Corp cards trashed")
    (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
    (let [tmc (get-in @state [:hazPlayer :rig :facedown 0])]
      (is (:facedown (refresh tmc)) "Tri-maf Contact is facedown")
      (is (= 3 (count (:hand (get-runner))))
          "No meat damage dealt by Tri-maf's leave play effect"))))

(deftest blackmail
  ;; Prevent rezzing of ice for one run
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
    (is (= 1 (get-in @state [:resPlayer :bad-publicity])) "Corp has 1 bad-publicity")
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Blackmail")
    (prompt-choice :hazPlayer "HQ")
    (let [iwall1 (get-ice state :hq 0)
          iwall2 (get-ice state :hq 1)]
      (core/rez state :resPlayer iwall1)
      (is (not (get-in (refresh iwall1) [:rezzed])) "First Ice Wall is not rezzed")
      (run-continue state)
      (core/rez state :resPlayer iwall2)
      (is (not (get-in (refresh iwall2) [:rezzed])) "Second Ice Wall is not rezzed")
      (core/jack-out state :hazPlayer nil)
      ;; Do another run, where the ice should rez
      (run-on state "HQ")
      (core/rez state :resPlayer iwall1)
      (is (get-in (refresh iwall1) [:rezzed]) "First Ice Wall is rezzed"))))

(deftest blackmail-tmi-interaction
  ;; Regression test for a rezzed tmi breaking game state on a blackmail run
  (do-game
    (new-game (default-corp [(qty "TMI" 3)])
              (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
    (is (= 1 (get-in @state [:resPlayer :bad-publicity])) "Corp has 1 bad-publicity")
    (play-from-hand state :resPlayer "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
      (core/rez state :resPlayer tmi)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (get-in (refresh tmi) [:rezzed]) "TMI is rezzed")
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Blackmail")
      (prompt-choice :hazPlayer "HQ")
      (run-continue state)
      (run-jack-out state)
      (run-on state "Archives"))))

(deftest by-any-means
  ;; By Any Means - Full test
  (do-game
   (new-game (default-corp [(qty "Hedge Fund" 1) (qty "Ice Wall" 1) (qty "Paper Trail" 1) (qty "PAD Campaign" 1)])
             (default-runner [(qty "By Any Means" 1), (qty "Sure Gamble" 4)]))
   (take-credits state :resPlayer)
   (run-empty-server state "Archives")
   (play-from-hand state :hazPlayer "By Any Means")
   (is (= 3 (:click (get-runner))) "Card not played, priority restriction")
   (take-credits state :hazPlayer)
   (starting-hand state :resPlayer ["Paper Trail", "Hedge Fund", "PAD Campaign"])
   (play-from-hand state :resPlayer "Paper Trail", "New remote")
   (play-from-hand state :resPlayer "PAD Campaign", "New remote")
   (take-credits state :resPlayer)
   (core/gain state :hazPlayer :click 1)
   (play-from-hand state :hazPlayer "By Any Means")
   (run-empty-server state "HQ")
   (is (= 1 (count (:discard (get-corp)))) "Operation was trashed")
   (is (= 3 (count (:hand (get-runner)))) "Took 1 meat damage")
   (run-empty-server state "R&D")
   (is (= 2 (count (:discard (get-corp)))) "ICE was trashed")
   (is (= 2 (count (:hand (get-runner)))) "Took 1 meat damage")
   (run-empty-server state "Server 1")
   (is (= 3 (count (:discard (get-corp)))) "Agenda was trashed")
   (is (= 1 (count (:hand (get-runner)))) "Took 1 meat damage")
   (run-empty-server state "Server 2")
   (is (= 4 (count (:discard (get-corp)))) "Trashable was trashed")
   (is (= 0 (count (:hand (get-runner)))) "Took 1 meat damage")))

(deftest by-any-means-ctm-crash
  (do-game
    (new-game (make-deck "NBN: Controlling the Message" [(qty "Paper Trail" 1)])
              (default-runner [(qty "By Any Means" 2)]))
    (play-from-hand state :resPlayer "Paper Trail" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "By Any Means")
    (run-empty-server state "Server 1")
    (prompt-choice :resPlayer "No") ;; Don't trigger CTM trace
    (is (empty? (:prompt (get-runner))) "No prompt to steal since agenda was trashed")
    (is (= 1 (count (:discard (get-corp)))) "Agenda was trashed")
    (is (= 0 (count (:hand (get-runner)))) "Took 1 meat damage")))

(deftest credit-kiting
  ;; After successful central run lower install cost by 8 and gain a tag
  (do-game
    (new-game (default-corp [(qty "PAD Campaign" 1)])
              (default-runner [(qty "Credit Kiting" 1) (qty "Femme Fatale" 1)]))
    (play-from-hand state :resPlayer "PAD Campaign" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (play-from-hand state :hazPlayer "Credit Kiting")
    (is (= 3 (:click (get-runner))) "Card not played, successful run on central not made")
    (run-empty-server state "HQ")
    (play-from-hand state :hazPlayer "Credit Kiting")
    (prompt-select :hazPlayer (find-card "Femme Fatale" (:hand (get-runner))))
    (is (= 4 (:credit (get-runner))) "Femme Fatale only cost 1 credit")
    (is (= 1 (:tag (get-runner))) "Runner gained a tag")))

(deftest cbi-raid
  ;; CBI Raid - Full test
  (do-game
    (new-game (default-corp [(qty "Caprice Nisei" 1) (qty "Adonis Campaign" 1) (qty "Quandary" 1)
                             (qty "Jackson Howard" 1) (qty "Global Food Initiative" 1)])
              (default-runner [(qty "CBI Raid" 1)]))
    (take-credits state :resPlayer)
    (is (= 5 (count (:hand (get-corp)))))
    (play-from-hand state :hazPlayer "CBI Raid")
    (is (= :hq (get-in @state [:run :server 0])))
    (run-successful state)
    (prompt-choice :hazPlayer "Run ability")
    (prompt-choice :resPlayer (find-card "Caprice Nisei" (:hand (get-corp))))
    (prompt-choice :resPlayer (find-card "Adonis Campaign" (:hand (get-corp))))
    (prompt-choice :resPlayer (find-card "Quandary" (:hand (get-corp))))
    (prompt-choice :resPlayer (find-card "Jackson Howard" (:hand (get-corp))))
    (prompt-choice :resPlayer (find-card "Global Food Initiative" (:hand (get-corp))))
    ;; try starting over
    (prompt-choice :resPlayer "Start over")
    (prompt-choice :resPlayer (find-card "Global Food Initiative" (:hand (get-corp))))
    (prompt-choice :resPlayer (find-card "Jackson Howard" (:hand (get-corp))))
    (prompt-choice :resPlayer (find-card "Quandary" (:hand (get-corp))))
    (prompt-choice :resPlayer (find-card "Adonis Campaign" (:hand (get-corp))))
    (prompt-choice :resPlayer (find-card "Caprice Nisei" (:hand (get-corp)))) ;this is the top card of R&D
    (prompt-choice :resPlayer "Done")
    (is (= 0 (count (:hand (get-corp)))))
    (is (= 5 (count (:deck (get-corp)))))
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))

(deftest cold-read
  ;; Make a run, and place 4 on this card, which you may use only during this run.
  ;; When this run ends, trash 1 program (cannot be prevented) used during this run.
  (do-game
    (new-game (default-corp [(qty "Blacklist" 3)])
              (default-runner [(qty "Imp" 1) (qty "Cold Read" 2)]))
    (play-from-hand state :resPlayer "Blacklist" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Imp")
    (let [bl (get-content state :remote1 0)]
      (play-from-hand state :hazPlayer "Cold Read")
      (prompt-choice :hazPlayer "HQ")
      (is (= 4 (:rec-counter (find-card "Cold Read" (get-in @state [:hazPlayer :play-area])))) "Cold Read has 4 counters")
      (run-successful state)
      (card-ability state :hazPlayer (get-program state 0) 0)
      (prompt-select :hazPlayer (get-program state 0))
      (is (= 2 (count (:discard (get-runner)))) "Imp and Cold Read in discard")
      ; Cold Read works when Blacklist rezzed - #2378
      (core/rez state :resPlayer bl)
      (play-from-hand state :hazPlayer "Cold Read")
      (prompt-choice :hazPlayer "HQ")
      (is (= 4 (:rec-counter (find-card "Cold Read" (get-in @state [:hazPlayer :play-area])))) "Cold Read has 4 counters")
      (run-successful state))))

(deftest corporate-scandal
  ;; Corporate Scandal - Corp has 1 additional bad pub even with 0
  (do-game
    (new-game (default-corp [(qty "Elizabeth Mills" 1)])
              (default-runner [(qty "Corporate Scandal" 1) (qty "Activist Support" 1)
                               (qty "Raymond Flint" 1) (qty "Investigative Journalism" 1)]))
    (play-from-hand state :resPlayer "Elizabeth Mills" "New remote")
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 5 :click 1)
    (play-from-hand state :hazPlayer "Raymond Flint")
    (play-from-hand state :hazPlayer "Corporate Scandal")
    (is (empty? (:prompt (get-runner))) "No BP taken, so no HQ access from Raymond")
    (play-from-hand state :hazPlayer "Investigative Journalism")
    (is (= "Investigative Journalism" (:title (get-in @state [:hazPlayer :rig :resource 1]))) "IJ able to be installed")
    (run-on state "HQ")
    (is (= 1 (:run-credit (get-runner))) "1 run credit from bad publicity")
    (run-jack-out state)
    (play-from-hand state :hazPlayer "Activist Support")
    (take-credits state :hazPlayer)
    (let [em (get-content state :remote1 0)]
      (core/rez state :resPlayer em)
      (is (= 1 (:has-bad-pub (get-corp))) "Corp still has BP")
      (take-credits state :resPlayer)
      (is (= 0 (:bad-publicity (get-corp))) "Corp has BP, didn't take 1 from Activist Support"))))

(deftest data-breach
  ;; Data Breach
  (do-game
    (new-game
      (default-corp)
      (default-runner [(qty "Data Breach" 3)]))
    (starting-hand state :resPlayer ["Hedge Fund"])
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Data Breach")
    (core/no-action state :resPlayer nil)
    (run-successful state)
    (prompt-choice :hazPlayer "OK")
    (prompt-choice :hazPlayer "Yes")
    (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
    (core/no-action state :resPlayer nil)
    (run-successful state)
    (prompt-choice :hazPlayer "OK")
    (is (empty? (:prompt (get-runner))) "No prompt to run a third time")
    (is (not (:run @state)) "Run is over")
    (play-from-hand state :hazPlayer "Data Breach")
    (run-jack-out state)
    (is (empty? (:prompt (get-runner))) "No option to run again on unsuccessful run")))

(deftest data-breach-doppelganger
  ;; FAQ 4.1 - ensure runner gets choice of activation order
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Doppelgänger" 1) (qty "Data Breach" 3)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Doppelgänger")
    (play-from-hand state :hazPlayer "Data Breach")
    (core/no-action state :resPlayer nil)
    (run-successful state)
    (prompt-choice :hazPlayer "OK")
    (prompt-choice :hazPlayer "Doppelgänger")
    (prompt-choice :hazPlayer "Yes")
    (prompt-choice :hazPlayer "HQ")
    (is (:run @state) "New run started")
    (is (= [:hq] (:server (:run @state))) "Running on HQ via Doppelgänger")
    (core/no-action state :resPlayer nil)
    (run-successful state)
    (prompt-choice :hazPlayer "OK")
    (prompt-choice :hazPlayer "Yes")
    (is (= [:rd] (get-in @state [:run :server])) "Second Data Breach run on R&D triggered")
    (core/no-action state :resPlayer nil)
    (run-successful state)))

(deftest deja-vu
  ;; Deja Vu - recur one non-virus or two virus cards
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Déjà Vu" 2)
                               (qty "Cache" 1)
                               (qty "Datasucker" 1)
                               (qty "Dirty Laundry" 1)]))
    (take-credits state :resPlayer 3) ; pass to runner's turn
    (trash-from-hand state :hazPlayer "Cache")
    (trash-from-hand state :hazPlayer "Datasucker")
    (trash-from-hand state :hazPlayer "Dirty Laundry")
    (is (= 2 (count (:hand (get-runner)))) "Two cards in hand prior to playing Déjà Vu")
    (play-from-hand state :hazPlayer "Déjà Vu")
    (prompt-choice :hazPlayer (find-card "Dirty Laundry" (:discard (get-runner))))
    (is (empty? (:prompt (get-runner))) "Recurring a non-virus card stops Déjà Vu prompting further")
    (is (= 2 (count (:hand (get-runner)))) "Two cards in after playing Déjà Vu")
    (play-from-hand state :hazPlayer "Déjà Vu")
    (prompt-choice :hazPlayer (find-card "Cache" (:discard (get-runner))))
    (is (not (empty? (:prompt (get-runner)))) "Recurring a virus card causes Déjà Vu to prompt for second virus to recur")
    (prompt-choice :hazPlayer (find-card "Datasucker" (:discard (get-runner))))
    (is (= 3 (count (:hand (get-runner)))) "Three cards in after playing second Déjà Vu")))

(deftest demolition-run
  ;; Demolition Run - Trash at no cost
  (do-game
    (new-game (default-corp [(qty "False Lead" 1)
                             (qty "Shell Corporation" 1)
                             (qty "Hedge Fund" 3)])
              (default-runner [(qty "Demolition Run" 1)]))
    (core/move state :resPlayer (find-card "False Lead" (:hand (get-corp))) :deck) ; put False Lead back in R&D
    (play-from-hand state :resPlayer "Shell Corporation" "R&D") ; install upgrade with a trash cost in root of R&D
    (take-credits state :resPlayer 2) ; pass to runner's turn by taking credits
    (play-from-hand state :hazPlayer "Demolition Run")
    (is (= 3 (:credit (get-runner))) "Paid 2 credits for the event")
    (prompt-choice :hazPlayer "R&D")
    (is (= [:rd] (get-in @state [:run :server])) "Run initiated on R&D")
    (prompt-choice :hazPlayer "OK") ; dismiss instructional prompt for Demolition Run
    (run-successful state)
    (let [demo (get-in @state [:hazPlayer :play-area 0])] ; Demolition Run "hack" is to put it out in the play area
      (prompt-choice :hazPlayer "Unrezzed upgrade in R&D")
      (card-ability state :hazPlayer demo 0)
      (is (= 3 (:credit (get-runner))) "Trashed Shell Corporation at no cost")
      (prompt-choice :hazPlayer "Card from deck")
      (card-ability state :hazPlayer demo 0)  ; trash False Lead instead of stealing
      (is (= 0 (:agenda-point (get-runner))) "Didn't steal False Lead")
      (is (= 2 (count (:discard (get-corp)))) "2 cards in Archives")
      (is (empty? (:prompt (get-runner))) "Run concluded"))))

(deftest deuces-wild
  ;; Deuces Wild
  (do-game
    (new-game (default-corp [(qty "Wraparound" 1)
                             (qty "The Future Perfect" 1)])
              (default-runner [(qty "Deuces Wild" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Wraparound" "New remote")
    (take-credits state :resPlayer)
    (starting-hand state :hazPlayer ["Deuces Wild" "Deuces Wild"])
    (play-from-hand state :hazPlayer "Deuces Wild")
    (prompt-choice :hazPlayer "Gain 3 [Credits]")
    (is (= 6 (:credit (get-runner))) "Gained 1 net credit")
    (prompt-choice :hazPlayer "Draw 2 cards")
    (is (= 3 (count (:hand (get-runner)))) "Drew 2 cards")
    (is (empty? (:prompt (get-runner))) "Deuces Wild not showing a third choice option")

    (play-from-hand state :hazPlayer "Deuces Wild")
    (prompt-choice :hazPlayer "Expose 1 ice and make a run")
    (prompt-select :hazPlayer (get-ice state :remote1 0))
    (prompt-choice :hazPlayer "HQ")
    (is (empty? (:prompt (get-runner))) "Deuces prompt not queued")
    (run-continue state)
    (run-successful state)
    (is (= 1 (count (:prompt (get-runner)))) "Deuces prompt not queued")
    (prompt-choice :hazPlayer "Access")
    (prompt-choice :resPlayer "0")
    (prompt-choice :hazPlayer "0")
    (prompt-choice :hazPlayer "Steal")
    (is (= 1 (count (:scored (get-runner)))) "TFP stolen")
    (core/gain state :hazPlayer :tag 1)
    (is (= 1 (:tag (get-runner))) "Runner has 1 tag")
    (prompt-choice :hazPlayer "Remove 1 tag")
    (is (= 0 (:tag (get-runner))))))

(deftest dirty-laundry
  ;; Dirty Laundry - Gain 5 credits at the end of the run if it was successful
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Dirty Laundry" 2)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Dirty Laundry")
    (prompt-choice :hazPlayer "Archives")
    (run-successful state)
    (is (= 8 (:credit (get-runner))) "Gained 5 credits")
    (play-from-hand state :hazPlayer "Dirty Laundry")
    (prompt-choice :hazPlayer "Archives")
    (run-jack-out state)
    (is (= 6 (:credit (get-runner))) "Run unsuccessful; gained no credits")))

(deftest drive-by
  ;; Drive By - Expose card in remote server and trash if asset or upgrade
  (do-game
    (new-game (default-corp [(qty "Eve Campaign" 2)
                             (qty "Product Placement" 1)
                             (qty "Project Atlas" 1)])
              (default-runner [(qty "Drive By" 2)]))
    (core/gain state :resPlayer :click 1)
    (play-from-hand state :resPlayer "Eve Campaign" "New remote")
    (play-from-hand state :resPlayer "Eve Campaign" "New remote")
    (play-from-hand state :resPlayer "Project Atlas" "New remote")
    (play-from-hand state :resPlayer "Product Placement" "HQ")
    (take-credits state :resPlayer)
    (let [eve1 (get-content state :remote1 0)
          eve2 (get-content state :remote2 0)
          atl (get-content state :remote3 0)
          pp (get-content state :hq 0)]
      (core/rez state :resPlayer eve1)
      (play-from-hand state :hazPlayer "Drive By")
      (prompt-select :hazPlayer pp)
      (is (= 1 (count (get-in @state [:resPlayer :servers :hq :content])))
          "Upgrades in root of central servers can't be targeted")
      (prompt-select :hazPlayer (refresh eve1))
      (is (= 1 (count (get-in @state [:resPlayer :servers :remote1 :content])))
          "Rezzed cards can't be targeted")
      (prompt-select :hazPlayer eve2)
      (is (= 2 (:click (get-runner))) "Spent 2 clicks")
      (is (and (= 1 (count (:discard (get-corp))))
               (= 5 (:credit (get-runner))))
          "Eve trashed at no cost")
      (is (nil? (get-in @state [:resPlayer :servers :remote2 :content])) "Server 2 no longer exists")
      (play-from-hand state :hazPlayer "Drive By")
      (prompt-select :hazPlayer atl)
      (is (= 0 (:click (get-runner))) "Runner has 0 clicks left")
      (is (= 1 (count (get-in @state [:resPlayer :servers :remote3 :content])))
          "Project Atlas not trashed from Server 3"))))

(deftest drive-by-psychic-field
  ;; Drive By - Psychic Field trashed after psi game. Issue #2127.
  (do-game
    (new-game (default-corp [(qty "Psychic Field" 1)])
              (default-runner [(qty "Drive By" 3)]))
    (play-from-hand state :resPlayer "Psychic Field" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Drive By")
    (prompt-select :hazPlayer (get-content state :remote1 0))
    (prompt-choice :resPlayer "0 [Credits]")
    (prompt-choice :hazPlayer "1 [Credits]")
    (is (empty? (get-content state :remote1)) "Psychic Field trashed")))

(deftest early-bird
  ;; Early Bird - Priority, make a run and gain a click
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Early Bird" 1)]))
    (take-credits state :resPlayer)
    (run-empty-server state "Archives")
    (play-from-hand state :hazPlayer "Early Bird")
    (is (= 3 (:click (get-runner))) "Card not played, Early Bird priority restriction")
    (take-credits state :hazPlayer)
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Early Bird")
    (prompt-choice :hazPlayer "Archives")
    (is (= 4 (:click (get-runner))) "Early Bird gains click")))

(deftest emergent-creativity
  ;; Emergent Creativty - Double, discard programs/hardware from grip, install from heap
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Emergent Creativity" 1) (qty "Paperclip" 1)
                               (qty "Heartbeat" 1) (qty "Gordian Blade" 1) (qty "Test Run" 1)]))
    (starting-hand state :hazPlayer ["Emergent Creativity" "Heartbeat" "Gordian Blade" "Test Run"])
    (take-credits state :resPlayer)

    (play-from-hand state :hazPlayer "Emergent Creativity")
    (prompt-select :hazPlayer (find-card "Heartbeat" (:hand (get-runner))))
    (prompt-select :hazPlayer (find-card "Gordian Blade" (:hand (get-runner))))
    (prompt-choice :hazPlayer "Done")
    (prompt-choice :hazPlayer (find-card "Paperclip" (:deck (get-runner))))
    (is (= 3 (:credit (get-runner))) "Offset cost of installing Paperclip")
    (is (= 0 (count (:deck (get-runner)))) "Installed from heap")
    (is (= 3 (count (:discard (get-runner)))) "Discard is 3 cards - EC, Heartbeat, GB")
    (is (= 2 (:click (get-runner))) "Emergent Creativity is a Double event")))

(deftest employee-strike-blue-sun
  ;; Employee Strike - vs Blue Sun, suppress Step 1.2
  (do-game
    (new-game (make-deck "Blue Sun: Powering the Future" [(qty "Ice Wall" 1)])
              (default-runner [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (core/rez state :resPlayer (get-ice state :hq 0))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Employee Strike")
    (take-credits state :hazPlayer)
    (is (not (:resPlayer-phase-12 @state)) "Employee Strike suppressed Blue Sun step 1.2")))

(deftest employee-strike-pu-philotic
  ;; Employee Strike - vs PU/Philotic - test for #2688
  (do-game
    (new-game (make-deck "Jinteki: Potential Unleashed" [(qty "Philotic Entanglement" 1) (qty "Braintrust" 2)])
              (default-runner [(qty "Employee Strike" 10)]))
    (play-from-hand state :resPlayer "Braintrust" "New remote")
    (play-from-hand state :resPlayer "Braintrust" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Steal")
    (run-empty-server state "Server 2")
    (prompt-choice :hazPlayer "Steal")
    (play-from-hand state :hazPlayer "Employee Strike")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Philotic Entanglement" "New remote")
    (score-agenda state :resPlayer (get-content state :remote3 0))
    (is (= 3 (count (:discard (get-runner)))) "Discard is 3 cards - 2 from Philotic, 1 EStrike.  Nothing from PU mill")))

(deftest encore
  ;; Encore - Run all 3 central servers successfully to take another turn.  Remove Encore from game.
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 1)])
              (default-runner [(qty "Encore" 1)]))
    (play-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :resPlayer)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :hazPlayer "Encore")
    (is (= 1 (count (:rfg (get-runner)))) "Encore removed from game")
    (take-credits state :hazPlayer)
    (take-credits state :hazPlayer)
    ; only get one extra turn
    (take-credits state :hazPlayer)
    (is (= 9 (:credit (get-runner))))))

(deftest encore-stacking
  ;; Encore - 2 encores in a 5 click turn results in 2 extra turns
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 1)])
              (default-runner [(qty "Encore" 2)]))
    (play-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :click 1)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :hazPlayer "Encore")
    (play-from-hand state :hazPlayer "Encore")
    (is (= 2 (count (:rfg (get-runner)))) "2 Encores removed from game")
    (take-credits state :hazPlayer)
    (take-credits state :hazPlayer)
    ;; Two extra turns
    (take-credits state :hazPlayer)
    (is (= 13 (:credit (get-runner))))))

(deftest eureka!
  ;; Eureka! - Install the program but trash the event
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Eureka!" 2) (qty "Torch" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 1)
    (core/move state :hazPlayer (find-card "Torch" (:hand (get-runner))) :deck)
    (play-from-hand state :hazPlayer "Eureka!")
    (prompt-choice :hazPlayer "Yes")
    (is (= 3 (:credit (get-runner))))
    (is (= 1 (count (get-in @state [:hazPlayer :rig :program]))))
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :hazPlayer "Eureka!")
    (is (= 0 (:credit (get-runner))))
    (is (= 3 (count (:discard (get-runner)))))))

(deftest falsified-credentials
  ;; Falsified Credentials - Expose card in remote
  ;; server and correctly guess its type to gain 5 creds
  (do-game
    (new-game (default-corp [(qty "Eve Campaign" 2)
                             (qty "Product Placement" 2)
                             (qty "Project Atlas" 1)])
              (default-runner [(qty "Falsified Credentials" 3)]))
    (core/gain state :resPlayer :click 2)
    (play-from-hand state :resPlayer "Eve Campaign" "New remote")
    (play-from-hand state :resPlayer "Eve Campaign" "New remote")
    (play-from-hand state :resPlayer "Project Atlas" "New remote")
    (play-from-hand state :resPlayer "Product Placement" "HQ")
    (play-from-hand state :resPlayer "Product Placement" "Server 3")
    (take-credits state :resPlayer)
    (let [eve1 (get-content state :remote1 0)
          eve2 (get-content state :remote2 0)
          atl (get-content state :remote3 0)
          pp1 (get-content state :hq 0)
          pp2 (get-content state :remote3 1)]
      (core/rez state :resPlayer eve1)
      (play-from-hand state :hazPlayer "Falsified Credentials")
      (prompt-choice :hazPlayer "Asset")
      (prompt-select :hazPlayer (refresh eve1))
      (is (= 4 (:credit (get-runner)))
          "Rezzed cards can't be targeted")
      (prompt-select :hazPlayer eve2)
      (is (= 3 (:click (get-runner))) "Spent 1 click")
      (is (= 9 (:credit (get-runner))) "Gained 5 creds for guessing asset correctly")
      (play-from-hand state :hazPlayer "Falsified Credentials")
      (prompt-choice :hazPlayer "Upgrade")
      (prompt-select :hazPlayer pp1)
      (is (= 8 (:credit (get-runner))) "Can't target cards in centrals")
      (prompt-select :hazPlayer pp2)
      (is (= 13 (:credit (get-runner)))
          "Gained 5 creds for guessing upgrade correctly, even if server contains non-upgrade as well")
      (core/rez state :resPlayer pp2)
      (play-from-hand state :hazPlayer "Falsified Credentials")
      (prompt-choice :hazPlayer "Agenda")
      (prompt-select :hazPlayer atl)
      (is (= 17 (:credit (get-runner)))
          "Gained 5 credits for guessing agenda correctly, even with rezzed card in server"))))

(deftest falsified-credentials-zaibatsu-loyalty
  ;; If Falsified Credentials fails to expose, it grants no credits.
  (do-game
   (new-game (default-corp [(qty "Zaibatsu Loyalty" 1)
                            (qty "Project Atlas" 1)])
             (default-runner [(qty "Falsified Credentials" 2)]))
   
    (play-from-hand state :resPlayer "Project Atlas" "New remote")
    (play-from-hand state :resPlayer "Zaibatsu Loyalty" "New remote")
    (take-credits state :resPlayer)
    (let [atl (get-content state :remote1 0)
          zaibatsu (get-content state :remote2 0)]
      (core/rez state :resPlayer zaibatsu)
      (play-from-hand state :hazPlayer "Falsified Credentials")
      (prompt-choice :hazPlayer "Agenda")
      (prompt-select :hazPlayer atl)
      (prompt-choice :resPlayer "Done")
      (is (= 9 (:credit (get-runner))) "An unprevented expose gets credits")

      (play-from-hand state :hazPlayer "Falsified Credentials")
      (prompt-choice :hazPlayer "Agenda")
      (prompt-select :hazPlayer atl)
      (card-ability state :resPlayer (refresh zaibatsu) 0) ; prevent the expose!
      (prompt-choice :resPlayer "Done")
      (is (= 8 (:credit (get-runner))) "A prevented expose does not"))))

(deftest frantic-coding-install
  ;; Frantic Coding - Install 1 program, other 9 cards are trashed
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Frantic Coding" 1) (qty "Torch" 1) (qty "Corroder" 1)
                               (qty "Magnum Opus" 1) (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                               (qty "John Masanori" 1) (qty "Amped Up" 1) (qty "Wanton Destruction" 1)]))
    (starting-hand state :hazPlayer ["Frantic Coding"])
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Frantic Coding")
    (prompt-choice :hazPlayer "OK")
    (let [get-prompt (fn [] (first (#(get-in @state [:hazPlayer :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
      (is (= 2 (:credit (get-runner))))
      (is (= 1 (count (:discard (get-runner)))))
      (prompt-choice :hazPlayer (find-card "Magnum Opus" (:deck (get-runner))))
      (is (= 1 (count (get-in @state [:hazPlayer :rig :program]))))
      (is (= 2 (:credit (get-runner))) "Magnum Opus installed for free")
      (is (= 10 (count (:discard (get-runner))))))))

(deftest frantic-coding-noinstall
  ;; Frantic Coding - Don't install anything, all 10 cards are trashed
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Frantic Coding" 1) (qty "Torch" 1) (qty "Corroder" 1)
                               (qty "Magnum Opus" 1) (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                               (qty "John Masanori" 1) (qty "Amped Up" 1) (qty "Wanton Destruction" 1)]))
    (starting-hand state :hazPlayer ["Frantic Coding"])
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Frantic Coding")
    (prompt-choice :hazPlayer "OK")
    (let [get-prompt (fn [] (first (#(get-in @state [:hazPlayer :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
      (is (= 1 (count (:discard (get-runner)))))
      (prompt-choice :hazPlayer "No install")
      (is (= 0 (count (get-in @state [:hazPlayer :rig :program]))))
      (is (= 11 (count (:discard (get-runner))))))))

(deftest freedom-through-equality
  ;; Move Freedom Through Equality to runner score on another steal
  ;; Check only one current used
  (do-game
    (new-game (default-corp [(qty "Project Beale" 2)])
              (default-runner [(qty "Street Peddler" 1) (qty "\"Freedom Through Equality\"" 3) (qty "Sure Gamble" 1)]))
    (starting-hand state :hazPlayer ["Street Peddler"
                                  "\"Freedom Through Equality\""
                                  "\"Freedom Through Equality\""
                                  "Sure Gamble"])
    (play-from-hand state :resPlayer "Project Beale" "New remote")
    (play-from-hand state :resPlayer "Project Beale" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Street Peddler")
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Steal")
    (is (= 1 (count (:scored (get-runner)))) "Freedom Through Equality not moved from Peddler to score area")
    (take-credits state :hazPlayer)
    (take-credits state :resPlayer)
    (run-empty-server state "Server 2")
    (play-from-hand state :hazPlayer "Sure Gamble")
    (play-from-hand state :hazPlayer "\"Freedom Through Equality\"")
    (play-from-hand state :hazPlayer "\"Freedom Through Equality\"")
    (prompt-choice :hazPlayer "Steal")
    (is (= 3 (count (:scored (get-runner)))) "Freedom Through Equality moved to score area")
    (is (= 5 (:agenda-point (get-runner))) "Freedom Through Equality for 1 agenda point")))

(deftest freelance-coding-contract
  ;; Freelance Coding Contract - Gain 2 credits per program trashed from Grip
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Freelance Coding Contract" 1)
                               (qty "Paricia" 1)
                               (qty "Cloak" 1)
                               (qty "Inti" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Freelance Coding Contract")
    (prompt-select :hazPlayer (find-card "Cloak" (:hand (get-runner))))
    (prompt-select :hazPlayer (find-card "Paricia" (:hand (get-runner))))
    (prompt-select :hazPlayer (find-card "Inti" (:hand (get-runner))))
    (prompt-choice :hazPlayer "Done")
    (is (= 3 (count (filter #(= (:type %) "Program") (:discard (get-runner)))))
        "3 programs in Heap")
    (is (= 11 (:credit (get-runner))) "Gained 6 credits from 3 trashed programs")))

(deftest game-day
  ;; Game Day - draw until at handsize
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Game Day" 3)
                              (qty "Public Sympathy" 3)
                              (qty "Sure Gamble" 3)
                              (qty "Easy Mark" 3)]))
   (take-credits state :resPlayer)
   ;; move needed cards to hand -- in case they were not drawn
   (core/move state :hazPlayer (find-card "Game Day" (:deck (get-runner))) :hand)
   (core/move state :hazPlayer (find-card "Public Sympathy" (:deck (get-runner))) :hand)
   (play-from-hand state :hazPlayer "Public Sympathy")
   (is (= 7 (core/hand-size state :hazPlayer)) "Runner hand size is 7")
   (play-from-hand state :hazPlayer "Game Day")
   (is (= 7 (count (:hand (get-runner)))) "Drew up to 7 cards")))

(deftest hacktivist-meeting
  ;; Trash a random card from corp hand while active
  ;; Make sure it is not active when hosted on Peddler
  (do-game
    (new-game (default-corp [(qty "Jeeves Model Bioroids" 2)
                             (qty "Jackson Howard" 2)])
              (default-runner [(qty "Street Peddler" 1)
                               (qty "Hacktivist Meeting" 3)]))
    (take-credits state :resPlayer)
    (starting-hand state :hazPlayer ["Street Peddler" "Hacktivist Meeting"])
    (play-from-hand state :hazPlayer "Street Peddler")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Jeeves Model Bioroids" "New remote")
    (play-from-hand state :resPlayer "Jackson Howard" "New remote")
    (let [jeeves (get-content state :remote1 0)
          jackson (get-content state :remote2 0)]
      (core/rez state :resPlayer jeeves)
      (is (= 0 (count (:discard (get-corp)))) "Nothing discarded to rez Jeeves - Hacktivist not active")
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Hacktivist Meeting")
      (core/rez state :resPlayer jackson)
      (is (= 1 (count (:discard (get-corp)))) "Card discarded to rez Jackson - Hacktivist active"))))

(deftest independent-thinking
  ;; Independent Thinking - Trash 2 installed cards, including a facedown directive, and draw 2 cards
  (do-game
    (new-game
      (default-corp)
      (make-deck "Apex: Invasive Predator" [(qty "Neutralize All Threats" 1)
                                            (qty "Independent Thinking" 2)
                                            (qty "Fan Site" 3)
                                            (qty "Street Magic" 3)]))
    (starting-hand state :hazPlayer ["Fan Site"
                                  "Fan Site"
                                  "Neutralize All Threats"
                                  "Independent Thinking"
                                  "Independent Thinking"])
    (take-credits state :resPlayer)
    (core/end-phase-12 state :hazPlayer nil)
    (prompt-select :hazPlayer (find-card "Neutralize All Threats" (:hand (get-runner))))
    (play-from-hand state :hazPlayer "Fan Site")
    (let [fs (get-in @state [:hazPlayer :rig :resource 0])
          nat (get-in @state [:hazPlayer :rig :facedown 0])]
      (play-from-hand state :hazPlayer "Independent Thinking")
      (prompt-select :hazPlayer fs)
      (prompt-select :hazPlayer nat)
      (prompt-choice :hazPlayer "Done")
      (is (= 4 (count (:hand (get-runner)))) "Trashing 2 cards draws 2 card"))))

(deftest indexing
  ;; Indexing - Full test
  (do-game
    (new-game (default-corp [(qty "Caprice Nisei" 1) (qty "Adonis Campaign" 1) (qty "Quandary" 1)
                            (qty "Jackson Howard" 1) (qty "Global Food Initiative" 1)])
            (default-runner [(qty "Indexing" 1)]))
    (dotimes [_ 5] (core/move state :resPlayer (first (:hand (get-corp))) :deck))
    (take-credits state :resPlayer)
    (is (= 0 (count (:hand (get-corp)))))
    (is (= 5 (count (:deck (get-corp)))))
    (play-from-hand state :hazPlayer "Indexing")
    (is (= :rd (get-in @state [:run :server 0])))
    (run-successful state)
    (prompt-choice :hazPlayer "Run ability")
    (prompt-choice :hazPlayer (find-card "Caprice Nisei" (:deck (get-corp))))
    (prompt-choice :hazPlayer (find-card "Adonis Campaign" (:deck (get-corp))))
    (prompt-choice :hazPlayer (find-card "Quandary" (:deck (get-corp))))
    (prompt-choice :hazPlayer (find-card "Jackson Howard" (:deck (get-corp))))
    (prompt-choice :hazPlayer (find-card "Global Food Initiative" (:deck (get-corp))))
    ;; try starting over
    (prompt-choice :hazPlayer "Start over")
    (prompt-choice :hazPlayer (find-card "Global Food Initiative" (:deck (get-corp))))
    (prompt-choice :hazPlayer (find-card "Jackson Howard" (:deck (get-corp))))
    (prompt-choice :hazPlayer (find-card "Quandary" (:deck (get-corp))))
    (prompt-choice :hazPlayer (find-card "Adonis Campaign" (:deck (get-corp))))
    (prompt-choice :hazPlayer (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
    (prompt-choice :hazPlayer "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))

(deftest information-sifting
  ;; Information Sifting - complicated interactions with damage prevention
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping"
                         [(qty "Snare!" 1) (qty "PAD Campaign" 1) (qty "Hostile Infrastructure" 1)
                          (qty "Braintrust" 1) (qty "Hedge Fund" 1) (qty "Power Shutdown" 1)])
              (default-runner [(qty "Information Sifting" 2) (qty "Deus X" 2) (qty "Sure Gamble" 1)]))
    (play-from-hand state :resPlayer "Hostile Infrastructure" "New remote")

    (core/gain state :resPlayer :credit 10)
    (core/rez state :resPlayer (get-content state :remote1 0))
    (core/gain state :hazPlayer :credit 10)
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Deus X")
    (play-from-hand state :hazPlayer "Deus X")
    (play-run-event state (find-card "Information Sifting" (:hand (get-runner))) :hq)
    (prompt-select :resPlayer (find-card "Snare!" (:hand (get-corp))))
    (prompt-select :resPlayer (find-card "PAD Campaign" (:hand (get-corp))))
    (prompt-choice :resPlayer "Done")
    (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Corp is waiting for Runner selection")
    (prompt-choice :hazPlayer "Pile 1")
    (prompt-choice :hazPlayer "Card from Pile 1")
    ;; the cards are selected randomly :(
    (letfn [(prevent-snare [existing-dmg]
              (prompt-choice :resPlayer "Yes")
              (card-ability state :hazPlayer (get-program state 0) 1)
              (prompt-choice :hazPlayer "Done")
              (is (= (inc existing-dmg) (count (:discard (get-runner)))) "Damage from Snare! prevented")
              (prompt-choice :hazPlayer "Yes")
              (prompt-choice :hazPlayer "Done") ; don't prevent Hostile dmg
              ;; chronos prompt
              (prompt-choice :resPlayer "Yes")
              (prompt-choice :resPlayer (find-card "Sure Gamble" (:hand (get-runner))))
              (is (= (+ 2 existing-dmg) (count (:discard (get-runner)))) "Damage from Hostile Inf not prevented"))
            (allow-pad [existing-dmg]
              (prompt-choice :hazPlayer "Yes")
              (card-ability state :hazPlayer (get-program state 0) 1)
              (prompt-choice :hazPlayer "Done")
              (is (= (inc existing-dmg) (count (:discard (get-runner)))) "Runner prevented damage from Hostile Inf"))]
      (if (= :waiting (-> (get-runner) :prompt first :prompt-type)) ; hit the snare
        ;; prevent the damage
        (do (prevent-snare (count (:discard (get-runner))))
            (prompt-choice :hazPlayer "Card from Pile 1")
            (allow-pad (count (:discard (get-runner)))))
        (do (allow-pad (count (:discard (get-runner))))
            (prompt-choice :hazPlayer "Card from Pile 1")
            (prevent-snare (count (:discard (get-runner)))))))
    (play-run-event state (find-card "Information Sifting" (:hand (get-runner))) :hq)
    (prompt-select :resPlayer (find-card "Power Shutdown" (:hand (get-corp))))
    (prompt-select :resPlayer (find-card "Hedge Fund" (:hand (get-corp))))
    (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Selecting max cards closed the selection prompt")
    (prompt-choice :hazPlayer "Pile 2")
    (prompt-choice :hazPlayer "Card from Pile 2")
    (prompt-choice :hazPlayer "Steal")
    (is (= 1 (count (:scored (get-runner)))) "Runner stole agenda")))

(deftest inject
  ;; Inject - Draw 4 cards from Stack and gain 1 credit per trashed program
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Inject" 1) (qty "Imp" 2) (qty "Sure Gamble" 2)]))
    (take-credits state :resPlayer)
    (core/move state :hazPlayer (find-card "Imp" (:hand (get-runner))) :deck)
    (core/move state :hazPlayer (find-card "Imp" (:hand (get-runner))) :deck)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (is (= 4 (count (:deck (get-runner)))))
    (play-from-hand state :hazPlayer "Inject")
    (is (= 2 (count (:hand (get-runner)))) "2 non-programs kept in Grip")
    (is (= 2 (count (filter #(= (:type %) "Program") (:discard (get-runner)))))
        "2 programs in Heap")
    (is (= 6 (:credit (get-runner)))
        "Paid 1 credit to play Inject, gained 2 credits from trashed programs")))

(deftest injection-attack
  ;; Injection Attack
  (do-game
    (new-game (default-corp [(qty "Paper Wall" 1)])
              (default-runner [(qty "Injection Attack" 1) (qty "Corroder" 1)]))
    (play-from-hand state :resPlayer "Paper Wall" "Archives")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Corroder")
    (play-from-hand state :hazPlayer "Injection Attack")
    (prompt-choice :hazPlayer "Archives")
    (is (= 2 (:current-strength (get-program state 0))) "Corroder at 2 strength")
    (prompt-select :hazPlayer (get-program state 0))
    (is (= 4 (:current-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (is (= 4 (:current-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (run-successful state)
    (is (= 2 (:current-strength (get-program state 0))) "Corroder reset to 2 strength")))

(deftest interdiction
  ;; Corp cannot rez non-ice cards during runner's turn
  (do-game
    (new-game (default-corp [(qty "Jeeves Model Bioroids" 1) (qty "Jackson Howard" 1)])
              (default-runner [(qty "Street Peddler" 1)
                               (qty "Interdiction" 3)]))
    (starting-hand state :hazPlayer ["Street Peddler" "Interdiction"])
    (play-from-hand state :resPlayer "Jeeves Model Bioroids" "New remote")
    (play-from-hand state :resPlayer "Jackson Howard" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Street Peddler")
    (let [jeeves (get-content state :remote1 0)
          jackson (get-content state :remote2 0)]
      (core/rez state :resPlayer jeeves)
      (is (get-in (refresh jeeves) [:rezzed]) "Jeeves is rezzed.  Interdiction not active when on Peddler")
      (play-from-hand state :hazPlayer "Interdiction")
      (core/rez state :resPlayer jackson)
      (is (not (get-in (refresh jackson) [:rezzed])) "Jackson is not rezzed"))))

(deftest ive-had-worse
  ;; I've Had Worse - Draw 3 cards when lost to net/meat damage; don't trigger if flatlined
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 3) (qty "Pup" 3)])
              (default-runner [(qty "I've Had Worse" 2) (qty "Sure Gamble" 3) (qty "Imp" 2)]))
    (core/gain state :hazPlayer :tag 1)
    (core/gain state :resPlayer :credit 5)
    (starting-hand state :hazPlayer ["I've Had Worse"])
    (play-from-hand state :resPlayer "Pup" "HQ")
    (core/rez state :resPlayer (get-ice state :hq 0))
    (card-subroutine state :resPlayer (get-ice state :hq 0) 0)
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 3 (count (:hand (get-runner)))) "I've Had Worse triggered and drew 3 cards")
    (starting-hand state :hazPlayer ["I've Had Worse" "Imp" "Imp"])
    (play-from-hand state :resPlayer "Scorched Earth")
    (is (= 0 (count (:hand (get-runner)))) "Runner has 0 cards in hand")
    (is (= :resPlayer (:winner @state)) "Corp wins")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")
    (is (= 4 (count (:discard (get-runner)))) "All 3 cards in Grip trashed by Scorched Earth")
    (is (= 3 (count (:deck (get-runner)))) "No cards drawn from I've Had Worse")))

(deftest lawyer-up
  ;; Lawyer Up - Lose 2 tags and draw 3 cards
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Lawyer Up" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :resPlayer)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/gain state :hazPlayer :tag 3)
    (play-from-hand state :hazPlayer "Lawyer Up")
    (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards")
    (is (= 2 (:click (get-runner))) "Spent 2 clicks")
    (is (= 1 (:tag (get-runner))) "Lost 2 tags")))

(deftest mad-dash
  ;; Mad Dash - Make a run. Move to score pile as 1 point if steal agenda.  Take 1 meat if not
  (do-game
    (new-game (default-corp [(qty "Project Atlas" 1)])
              (default-runner [(qty "Mad Dash" 3)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Mad Dash")
    (prompt-choice :hazPlayer "Archives")
    (run-successful state)
    (is (= 2 (count (:discard (get-runner)))) "Took a meat damage")
    (play-from-hand state :hazPlayer "Mad Dash")
    (prompt-choice :hazPlayer "HQ")
    (run-successful state)
    (prompt-choice :hazPlayer "Steal")
    (is (= 2 (count (:scored (get-runner)))) "Mad Dash moved to score area")
    (is (= 3 (:agenda-point (get-runner))) "Mad Dash scored for 1 agenda point")))

(deftest making-an-entrance
  ;; Making an Entrance - Full test
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Making an Entrance" 2) (qty "Sure Gamble" 1) (qty "Desperado" 1)
                               (qty "Diesel" 1) (qty "Corroder" 1) (qty "Patron" 1)]))
    (starting-hand state :hazPlayer ["Making an Entrance"])
    (is (= 1 (count (:hand (get-runner)))))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Making an Entrance")
    ;; trash cards
    (is (= 1 (count (:discard (get-runner)))))
    (prompt-choice :hazPlayer (find-card "Desperado" (:deck (get-runner))))
    (prompt-choice :hazPlayer (find-card "Diesel" (:deck (get-runner))))
    (is (= 3 (count (:discard (get-runner)))))
    (prompt-choice :hazPlayer "None")
    ;; start arranging
    (prompt-choice :hazPlayer (find-card "Making an Entrance" (:deck (get-runner))))
    (prompt-choice :hazPlayer (find-card "Sure Gamble" (:deck (get-runner))))
    (prompt-choice :hazPlayer (find-card "Corroder" (:deck (get-runner))))
    (prompt-choice :hazPlayer (find-card "Patron" (:deck (get-runner))))
    ;; try starting over
    (prompt-choice :hazPlayer "Start over")
    (prompt-choice :hazPlayer (find-card "Patron" (:deck (get-runner))))
    (prompt-choice :hazPlayer (find-card "Corroder" (:deck (get-runner))))
    (prompt-choice :hazPlayer (find-card "Sure Gamble" (:deck (get-runner))))
    (prompt-choice :hazPlayer (find-card "Making an Entrance" (:deck (get-runner)))) ;this is the top card on stack
    (prompt-choice :hazPlayer "Done")
    (is (= "Making an Entrance" (:title (first (:deck (get-runner))))))
    (is (= "Sure Gamble" (:title (second (:deck (get-runner))))))
    (is (= "Corroder" (:title (second (rest (:deck (get-runner)))))))
    (is (= "Patron" (:title (second (rest (rest (:deck (get-runner))))))))
    (core/draw state :hazPlayer)
    (is (= "Making an Entrance" (:title (first (:hand (get-runner))))))
    (is (= 1 (count (:hand (get-runner)))))
    (play-from-hand state :hazPlayer "Making an Entrance")
    (is (= 1 (count (:hand (get-runner)))) "Can only play on first click")))

(deftest mars-for-martians
  ;; Mars for Martians - Full test
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Mars for Martians" 1) (qty "Clan Vengeance" 1) (qty "Counter Surveillance" 1)
                               (qty "Jarogniew Mercs" 1) (qty "Sure Gamble" 3)]))
    (starting-hand state :hazPlayer ["Mars for Martians" "Clan Vengeance" "Counter Surveillance" "Jarogniew Mercs"])
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Clan Vengeance")
    (play-from-hand state :hazPlayer "Counter Surveillance")
    (play-from-hand state :hazPlayer "Jarogniew Mercs")
    (play-from-hand state :hazPlayer "Mars for Martians")
    (is (= 1 (:click (get-runner))) "Mars for Martians not played, priority event")
    (take-credits state :hazPlayer)
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :tag 4)
    (is (= 5 (:tag (get-runner))) "+1 tag from Jarogniew Mercs")
    (is (= 1 (count (:hand (get-runner)))))
    (is (= 2 (:credit (get-runner))))
    (play-from-hand state :hazPlayer "Mars for Martians")
    (is (= 3 (count (:hand (get-runner)))) "3 clan resources, +3 cards but -1 for playing Mars for Martians")
    (is (= 7 (:credit (get-runner))) "5 tags, +5 credits")))

(deftest mobius
  ;; Mobius
  (do-game
    (new-game
      (default-corp)
      (default-runner [(qty "Möbius" 3)]))
    (starting-hand state :resPlayer ["Hedge Fund"])
    (take-credits state :resPlayer)
    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :hazPlayer "Möbius")
    (core/no-action state :resPlayer nil)
    (run-successful state)
    (is (= 5 (:credit (get-runner))))
    (prompt-choice :hazPlayer "OK")
    (prompt-choice :hazPlayer "Yes")
    (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
    (core/no-action state :resPlayer nil)
    (run-successful state)
    (prompt-choice :hazPlayer "OK")
    (is (= 9 (:credit (get-runner))))
    (is (empty? (:prompt (get-runner))) "No prompt to run a third time")
    (is (not (:run @state)) "Run is over")
    (play-from-hand state :hazPlayer "Möbius")
    (run-jack-out state)
    (is (empty? (:prompt (get-runner))) "No option to run again on unsuccessful run")))

(deftest modded
  ;; Modded - Install a program or piece of hardware at a 3 credit discount
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Modded" 2)
                               (qty "HQ Interface" 1)
                               (qty "Nerve Agent" 1)
                               (qty "Earthrise Hotel" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Modded")
    (prompt-select :hazPlayer (find-card "Earthrise Hotel" (:hand (get-runner))))
    (is (empty? (get-in @state [:hazPlayer :rig :resource])) "Can't install resources with Modded")
    (prompt-select :hazPlayer (find-card "HQ Interface" (:hand (get-runner))))
    (is (= 1 (count (get-in @state [:hazPlayer :rig :hardware]))) "Installed HQ Interface")
    (is (= 4 (:credit (get-runner))) "Paid 1 credit instead of 4")
    (play-from-hand state :hazPlayer "Modded")
    (prompt-select :hazPlayer (find-card "Nerve Agent" (:hand (get-runner))))
    (is (= 1 (count (get-in @state [:hazPlayer :rig :program]))) "Installed Nerve Agent")
    (is (= 4 (:credit (get-runner))) "Paid 0 credits")))

(deftest noble-path
  ;; The Noble Path - Prevents damage during run
  (do-game
    (new-game (default-corp) (default-runner [(qty "The Noble Path" 1) (qty "Hedge Fund" 2)]))
    (let [hand-count #(count (:hand (get-runner)))]
      (starting-hand state :hazPlayer ["The Noble Path" "Hedge Fund"])
      (take-credits state :resPlayer)

      ;; Play The Noble Path and confirm it trashes remaining cards in hand
      (is (= 2 (hand-count)) "Start with 2 cards")
      (play-from-hand state :hazPlayer "The Noble Path")
      (is (= 0 (hand-count)) "Playing Noble Path trashes the remaining cards in hand")

      ;; Put a card into hand so I can confirm it's not discarded by damage
      ;; Don't want to dealing with checking damage on a zero card hand
      (starting-hand state :hazPlayer ["Hedge Fund"])

      (core/damage state :hazPlayer :net 1)
      (is (= 1 (hand-count)) "Damage was prevented")

      ;; Finish the run and check that damage works again
      (prompt-choice :hazPlayer "HQ")
      (run-successful state)
      (prompt-choice :hazPlayer "OK")
      (core/damage state :hazPlayer :net 1)
      (is (= 0 (hand-count)) "Damage works again after run"))))

(deftest notoriety
  ;; Notoriety - Run all 3 central servers successfully and play to gain 1 agenda point
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 1)])
              (default-runner [(qty "Notoriety" 1)]))
    (play-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :resPlayer)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :hazPlayer "Notoriety")
    (is (= 1 (count (:scored (get-runner)))) "Notoriety moved to score area")
    (is (= 1 (:agenda-point (get-runner))) "Notoriety scored for 1 agenda point")))

(deftest out-of-the-ashes
  ;; Out of the Ashes - ensure card works when played/trashed/milled
  (do-game
    (new-game (default-corp [(qty "Kala Ghoda Real TV" 1) (qty "Underway Renovation" 1)])
              (default-runner [(qty "Out of the Ashes" 6)]))
    (play-from-hand state :resPlayer "Underway Renovation" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Out of the Ashes")
    (prompt-choice :hazPlayer "Archives")
    (is (:run @state))
    (run-successful state)
    (trash-from-hand state :hazPlayer "Out of the Ashes")
    (trash-from-hand state :hazPlayer "Out of the Ashes")
    (trash-from-hand state :hazPlayer "Out of the Ashes")
    (trash-from-hand state :hazPlayer "Out of the Ashes")
    (is (= 0 (count (:hand (get-runner)))))
    (is (= 5 (count (:discard (get-runner)))))
    (take-credits state :hazPlayer)
    (let [underway (get-content state :remote1 0)]
      (core/advance state :resPlayer {:card (refresh underway)}))
    (is (= 6 (count (:discard (get-runner)))))
    (take-credits state :resPlayer)
    ;; remove 5 Out of the Ashes from the game
    (dotimes [_ 5]
      (is (not (empty? (get-in @state [:hazPlayer :prompt]))))
      (prompt-choice :hazPlayer "Yes")
      (prompt-choice :hazPlayer "Archives")
      (is (:run @state))
      (run-successful state))
    (prompt-choice :hazPlayer "No")
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 5 (count (:rfg (get-runner)))))
    (take-credits state :hazPlayer)
    (take-credits state :resPlayer)
    ;; ensure that if you decline the rfg, game will still ask the next turn
    (is (not (empty? (get-in @state [:hazPlayer :prompt]))))
    (prompt-choice :hazPlayer "Yes")
    (prompt-choice :hazPlayer "Archives")
    (is (:run @state))
    (run-successful state)
    (is (= 0 (count (:discard (get-runner)))))
    (is (= 6 (count (:rfg (get-runner)))))))

(deftest political-graffiti
  ;; Political Graffiti - swapping with Turntable works / purging viruses restores points
  (do-game
    (new-game (default-corp [(qty "Breaking News" 1) (qty "Chronos Project" 1)])
              (default-runner [(qty "Turntable" 1) (qty "Political Graffiti" 1)]))
    (play-from-hand state :resPlayer "Breaking News" "New remote")
    (score-agenda state :resPlayer (get-content state :remote1 0))
    (is (= 1 (:agenda-point (get-corp))))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Political Graffiti")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (prompt-choice :hazPlayer "Run ability")
    (prompt-select :hazPlayer (find-card "Breaking News" (:scored (get-corp))))
    (is (= 0 (:agenda-point (get-corp))) "Political Dealings lowered agenda points by 1")
    (play-from-hand state :hazPlayer "Turntable")
    (run-empty-server state "HQ")
    (prompt-choice :hazPlayer "Steal")
    (let [tt (get-in @state [:hazPlayer :rig :hardware 0])]
      (prompt-choice :hazPlayer "Yes")
      (prompt-select :hazPlayer (find-card "Breaking News" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 0 (:agenda-point (get-runner))))
      (take-credits state :hazPlayer)
      (core/purge state :resPlayer)
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 1 (:agenda-point (get-runner)))))))

(deftest political-graffiti-forfeit
  ;; Political Graffiti - forfeiting agenda with Political Graffiti does not refund double points
  ;; Regression test for issue #2765
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 1) (qty "Sacrifice" 1)])
              (default-runner [(qty "Political Graffiti" 1)]))
    (play-from-hand state :resPlayer "Hostile Takeover" "New remote")
    (score-agenda state :resPlayer (get-content state :remote1 0))
    (is (= 1 (:agenda-point (get-corp))))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Political Graffiti")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (prompt-choice :hazPlayer "Run ability")
    (prompt-select :hazPlayer (find-card "Hostile Takeover" (:scored (get-corp))))
    (is (= 0 (:agenda-point (get-corp))) "Political Dealings lowered agenda points by 1")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Sacrifice")
    (prompt-select :resPlayer (get-scored state :resPlayer 0))
    (is (= 0 (:agenda-point (get-corp))) "Forfeiting agenda did not refund extra agenda points ")
    (is (= 1 (count (:discard (get-runner)))) "Political Graffiti is in the Heap")))

(deftest push-your-luck-correct-guess
  ;; Push Your Luck - Corp guesses correctly
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Push Your Luck" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Push Your Luck")
    (prompt-choice :resPlayer "Odd")
    (prompt-choice :hazPlayer 3)
    (is (= 0 (:credit (get-runner))) "Corp guessed correctly")))

(deftest push-your-luck-incorrect-guess
  ;; Push Your Luck - Corp guesses incorrectly
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Push Your Luck" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Push Your Luck")
    (prompt-choice :resPlayer "Even")
    (prompt-choice :hazPlayer 3)
    (is (= 6 (:credit (get-runner))) "Corp guessed incorrectly")))

(deftest pushing-the-envelope
  ;; Run. Add 2 strength to each installer breaker.
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Pushing the Envelope" 3) (qty "Corroder" 2) (qty "Atman" 1)]))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 20)
    (core/gain state :hazPlayer :click 10)
    (core/draw state :hazPlayer)
    (play-from-hand state :hazPlayer "Corroder")
    (play-from-hand state :hazPlayer "Atman")
    (prompt-choice :hazPlayer 0)
    (let [atman (get-in @state [:hazPlayer :rig :program 1])
          corr (get-in @state [:hazPlayer :rig :program 0])]
      (is (= 0 (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (play-from-hand state :hazPlayer "Pushing the Envelope")
      (prompt-choice :hazPlayer "Archives")
      ; 3 cards in hand - no boost
      (is (= 0 (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (run-successful state)
      (play-from-hand state :hazPlayer "Pushing the Envelope")
      (prompt-choice :hazPlayer "Archives")
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
    (new-game (default-corp [(qty "PAD Campaign" 2)])
              (default-runner [(qty "Queen's Gambit" 1)]))
    (play-from-hand state :resPlayer "PAD Campaign" "New remote")
    (play-from-hand state :resPlayer "PAD Campaign" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Queen's Gambit")
    (let [pad (get-content state :remote1 0)
          runner-creds (:credit (get-runner))]
      (prompt-choice :hazPlayer "3")
      (prompt-select :hazPlayer pad)
      (is (= (+ runner-creds 6) (:credit (get-runner))) "Gained 6 credits from Queen's Gambit")
      (is (= 3 (:advance-counter (refresh pad))) "3 advancement counters placed on PAD Campaign by Queen's Gambit")
      (is (not (core/can-access? state :hazPlayer (refresh pad))) "Cannot access PAD Campgain")
      (run-empty-server state "Server 1")
      (is (not (:run @state)) "Run ended since no cards could be accessed"))
    (let [other-pad (get-content state :remote2 0)]
      (is (core/can-access? state :hazPlayer other-pad)) "Not prevented from accessing other cards")
    (take-credits state :hazPlayer)
    (take-credits state :resPlayer)
    (let [pad (get-content state :remote1 0)
          runner-creds (:credit (get-runner))]
      (run-empty-server state "Server 1")
      (is (core/can-access? state :hazPlayer (refresh pad)) "Can access PAD Campgain next turn")
      (prompt-choice :hazPlayer "Yes")
      (is (= (- runner-creds 4) (:credit (get-runner))) "Paid 4 credits to trash PAD Campaign"))))

;; Rebirth
(let [choose-runner (fn [name state prompt-map]
                      (let [kate-choice (some #(when (= name (:title %)) %) (:choices (prompt-map :hazPlayer)))]
                        (core/resolve-prompt state :hazPlayer {:card kate-choice})))

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
      (new-game (default-corp) (default-runner ["Magnum Opus" "Rebirth"]) {:start-as :hazPlayer})

      (play-from-hand state :hazPlayer "Rebirth")
      (is (= (first (prompt-titles :hazPlayer)) ayla) "List is sorted")
      (is (every?   #(some #{%} (prompt-titles :hazPlayer))
                    [kate kit]))
      (is (not-any? #(some #{%} (prompt-titles :hazPlayer))
                    [professor whizzard jamie]))

      (choose-runner kate state prompt-map)

      (is (= kate (-> (get-runner) :identity :title)))
      (is (= 1 (:link (get-runner))) "1 link")

      (is (empty? (:discard (get-runner))))
      (is (= "Rebirth" (-> (get-runner) :rfg first :title)))

      (is (changes-credits (get-runner) -4
        (play-from-hand state :hazPlayer "Magnum Opus")))))

  (deftest-pending rebirth-kate-twice
    ;; Rebirth - Kate's discount does not after rebirth if something already installed
    (do-game
      (new-game (default-corp) (default-runner ["Akamatsu Mem Chip" "Rebirth" "Clone Chip"]) {:start-as :hazPlayer})

      (play-from-hand state :hazPlayer "Clone Chip")
      (play-from-hand state :hazPlayer "Rebirth")
      (choose-runner kate state prompt-map)

      (is (changes-credits (get-corp) -1
        (play-from-hand state :hazPlayer "Akamatsu Mem Chip"))
        "Discount not applied for 2nd install")))

  (deftest rebirth-whizzard
    ;; Rebirth - Whizzard works after rebirth
    (do-game
      (new-game (default-corp ["Ice Wall"]) (make-deck reina ["Rebirth"]))
      (play-from-hand state :resPlayer "Ice Wall" "R&D")
      (take-credits state :resPlayer)

      (play-from-hand state :hazPlayer "Rebirth")
      (choose-runner whizzard state prompt-map)

      (card-ability state :hazPlayer (:identity (get-runner)) 0)
      (is (= 6 (:credit (get-runner))) "Took a Whizzard credit")

      (is (changes-credits (get-corp) -1
        (core/rez state :resPlayer (get-ice state :rd 0)))
        "Reina is no longer active")))

  (deftest rebirth-lose-link
    ;; Rebirth - Lose link from ID
    (do-game
      (new-game (default-corp)
                (make-deck kate ["Rebirth" "Access to Globalsec"])
                {:start-as :hazPlayer})
      (play-from-hand state :hazPlayer "Access to Globalsec")
      (is (= 2 (:link (get-runner))) "2 link before rebirth")

      (play-from-hand state :hazPlayer "Rebirth")
      (choose-runner chaos state prompt-map)
      (is (= 1 (:link (get-runner))) "1 link after rebirth")))

  (deftest rebirth-gain-link
    ;; Rebirth - Gain link from ID
    (do-game
      (new-game (default-corp)
                (default-runner ["Rebirth" "Access to Globalsec"])
                {:start-as :hazPlayer})
      (play-from-hand state :hazPlayer "Access to Globalsec")
      (is (= 1 (:link (get-runner))) "1 link before rebirth")

      (play-from-hand state :hazPlayer "Rebirth")
      (choose-runner kate state prompt-map)
      (is (= 2 (:link (get-runner))) "2 link after rebirth"))))

(deftest rigged-results
  ;; Rigged Results - success and failure
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1)])
              (default-runner [(qty "Rigged Results" 3)]))
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Rigged Results")
    (prompt-choice :hazPlayer "0")
    (prompt-choice :resPlayer "0")
    (is (empty? (:prompt (get-runner))) "Rigged Results failed for runner")
    (is (empty? (:prompt (get-corp))) "Rigged Results failed for runner")
    (play-from-hand state :hazPlayer "Rigged Results")
    (prompt-choice :hazPlayer "2")
    (prompt-choice :resPlayer "1")
    (prompt-select :hazPlayer (get-ice state :hq 0))
    (is (= [:hq] (:server (:run @state))) "Runner is running on HQ")
    (is (= 3 (:credit (get-runner))) "Rigged results spends credits")))

(deftest retrieval-run
  ;; Retrieval Run - Run Archives successfully and install a program from Heap for free
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Retrieval Run" 1) (qty "Morning Star" 1)]))
    (take-credits state :resPlayer)
    (trash-from-hand state :hazPlayer "Morning Star")
    (play-from-hand state :hazPlayer "Retrieval Run")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (prompt-choice :hazPlayer "Run ability")
    (let [ms (first (:discard (get-runner)))]
      (prompt-choice :hazPlayer ms)
      (is (= "Morning Star" (:title (first (get-in @state [:hazPlayer :rig :program]))))
          "Morning Star installed")
      (is (= 2 (:credit (get-runner))) "Morning Star installed at no cost")
      (is (= 2 (:memory (get-runner))) "Morning Star uses 2 memory"))))

(deftest rumor-mill
  ;; Rumor Mill - interactions with rez effects, additional costs, general event handlers, and trash-effects
  (do-game
    (new-game
      (default-corp [(qty "Project Atlas" 2)
                     (qty "Caprice Nisei" 1) (qty "Chairman Hiro" 1) (qty "Cybernetics Court" 1)
                     (qty "Elizabeth Mills" 1)
                     (qty "Ibrahim Salem" 1)
                     (qty "Housekeeping" 1)
                     (qty "Director Haas" 1)
                     (qty "Oberth Protocol" 1)])
      (default-runner [(qty "Rumor Mill" 1)]))
    (core/gain state :resPlayer :credit 100 :click 100 :bad-publicity 1)
    (core/draw state :resPlayer 100)
    (play-from-hand state :resPlayer "Caprice Nisei" "New remote")
    (play-from-hand state :resPlayer "Chairman Hiro" "New remote")
    (play-from-hand state :resPlayer "Cybernetics Court" "New remote")
    (play-from-hand state :resPlayer "Elizabeth Mills" "New remote")
    (play-from-hand state :resPlayer "Project Atlas" "New remote")
    (play-from-hand state :resPlayer "Ibrahim Salem" "New remote")
    (play-from-hand state :resPlayer "Oberth Protocol" "New remote")
    (core/move state :resPlayer (find-card "Director Haas" (:hand (get-corp))) :deck)
    (core/rez state :resPlayer (get-content state :remote2 0))
    (core/rez state :resPlayer (get-content state :remote3 0))
    (score-agenda state :resPlayer (get-content state :remote5 0))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 100 :click 100)
    (is (= 4 (:hand-size-modification (get-corp))) "Corp has +4 hand size")
    (is (= -2 (:hand-size-modification (get-runner))) "Runner has -2 hand size")

    (play-from-hand state :hazPlayer "Rumor Mill")

    ;; Additional costs to rez should NOT be applied
    (core/rez state :resPlayer (get-content state :remote6 0))
    (is (= 1 (count (:scored (get-corp)))) "No agenda was auto-forfeit to rez Ibrahim Salem")

    ;; In-play effects
    (is (= 0 (:hand-size-modification (get-corp))) "Corp has original hand size")
    (is (= 0 (:hand-size-modification (get-runner))) "Runner has original hand size")

    ;; "When you rez" effects should not apply
    (core/rez state :resPlayer (get-content state :remote4 0))
    (is (= 1 (:bad-publicity (get-corp))) "Corp still has 1 bad publicity")

    ;; Run events (Caprice)
    ;; Make sure Rumor Mill applies even if card is rezzed after RM is put in play.
    (core/rez state :resPlayer (get-content state :remote1 0))
    (run-on state :remote1)
    (run-continue state)
    (is (empty? (:prompt (get-corp))) "Caprice prompt is not showing")
    (run-jack-out state)

    ;; Trashable execs
    (run-empty-server state :remote2)
    (prompt-choice :hazPlayer "Yes")
    (is (empty? (:scored (get-runner))) "Chairman Hiro not added to runner's score area")
    (run-jack-out state)
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :hazPlayer "Yes")
    (is (empty? (:scored (get-runner))) "Director Haas not added to runner's score area")
    (take-credits state :hazPlayer)

    ;; Trash RM, make sure everything works again
    (play-from-hand state :resPlayer "Housekeeping")
    (is (= 4 (:hand-size-modification (get-corp))) "Corp has +4 hand size")
    (is (= 0 (:hand-size-modification (get-runner))) "Runner has +0 hand size")

    ;; Additional costs to rez should now be applied again
    (core/rez state :resPlayer (get-content state :remote7 0))
    (prompt-select :resPlayer (get-in (get-corp) [:scored 0]))
    (is (zero? (count (:scored (get-corp)))) "Agenda was auto-forfeit to rez Oberth")

    (core/derez state :resPlayer (get-content state :remote4 0))
    (core/rez state :resPlayer (get-content state :remote4 0))
    (is (= 0 (:bad-publicity (get-corp))) "Corp has 0 bad publicity")
    (card-ability state :resPlayer (get-content state :remote4 0) 0) ; Elizabeth Mills, should show a prompt
    (is (:prompt (get-corp)) "Elizabeth Mills ability allowed")))

(deftest rumor-mill-street-peddler
  ;; Make sure Rumor Mill is not active when hosted on Peddler
  (do-game
    (new-game (default-corp [(qty "Jeeves Model Bioroids" 1)])
              (default-runner [(qty "Street Peddler" 1)
                               (qty "Rumor Mill" 3)]))
    (take-credits state :resPlayer)
    (starting-hand state :hazPlayer ["Street Peddler"])
    (play-from-hand state :hazPlayer "Street Peddler")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Jeeves Model Bioroids" "New remote")
    (let [jeeves (get-content state :remote1 0)]
      (core/rez state :resPlayer jeeves)
      (card-ability state :resPlayer jeeves 0)
      (is (= 3 (:click (get-corp))) "Corp has 3 clicks - Jeeves working ok"))))

(deftest scrubbed
  ;; First piece of ice encountered each turn has -2 Strength for remainder of the run
  (do-game
    (new-game (default-corp [(qty "Turing" 1)])
              (default-runner [(qty "Street Peddler" 1)
                               (qty "Scrubbed" 3)]))
    (starting-hand state :hazPlayer ["Street Peddler" "Scrubbed"])
    (play-from-hand state :resPlayer "Turing" "HQ")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Street Peddler")
    (let [turing (get-ice state :hq 0)]
      (core/rez state :resPlayer turing)
      (is (= 2 (:current-strength (refresh turing))))
      (run-on state "HQ")
      (run-continue state)
      (is (= 2 (:current-strength (refresh turing))) "Scrubbed not active when on Peddler")
      (play-from-hand state :hazPlayer "Scrubbed")
      (run-on state "HQ")
      (run-continue state)
      (is (= 0 (:current-strength (refresh turing))) "Scrubbed reduces strength by 2")
      (run-successful state))))

(deftest singularity
  ;; Singularity - Run a remote; if successful, trash all contents at no cost
  (do-game
    (new-game (default-corp [(qty "Caprice Nisei" 1)
                             (qty "Breaker Bay Grid" 1)
                             (qty "Eve Campaign" 1)])
              (default-runner [(qty "Singularity" 1)]))
    (play-from-hand state :resPlayer "Breaker Bay Grid" "New remote")
    (play-from-hand state :resPlayer "Caprice Nisei" "Server 1")
    (play-from-hand state :resPlayer "Eve Campaign" "Server 1")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Singularity")
    (prompt-choice :hazPlayer "Server 1")
    (is (= 2 (:click (get-runner))) "Runner spends 2 clicks on double event")
    (is (= 1 (:credit (get-runner))) "Runner pays 4 credits for Singularity")
    (run-successful state)
    (is (= 3 (count (:discard (get-corp)))) "All 3 cards trashed from Server 1")
    (is (= 1 (:credit (get-runner))) "No credits paid for trashing")
    (is (nil? (get-in @state [:resPlayer :servers :remote1 :content])) "Server 1 no longer exists")))

(deftest stimhack
  ;; Stimhack - Gain 9 temporary credits and take 1 brain damage after the run
  (do-game
    (new-game (default-corp [(qty "Eve Campaign" 1)])
              (default-runner [(qty "Stimhack" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Stimhack")
    (prompt-choice :hazPlayer "HQ")
    (is (= [:hq] (get-in @state [:run :server])) "Run initiated on HQ")
    (run-successful state)
    (is (= 14 (:credit (get-runner))))
    (is (= 9 (:run-credit (get-runner))) "Gained 9 credits for use during the run")
    (prompt-choice :hazPlayer "Yes") ; choose to trash Eve
    (is (and (= 0 (count (:hand (get-corp))))
             (= 1 (count (:discard (get-corp)))))
        "Corp hand empty and Eve in Archives")
    (is (= 5 (:credit (get-runner))))
    (is (= 0 (count (:hand (get-runner)))) "Lost card from Grip to brain damage")
    (is (= 4 (core/hand-size state :hazPlayer)))
    (is (= 1 (:brain-damage (get-runner))))))

(deftest sure-gamble
  ;; Sure Gamble
  (do-game
    (new-game (default-corp) (default-runner [(qty "Sure Gamble" 1)]))
    (take-credits state :resPlayer)
    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :hazPlayer "Sure Gamble")
    (is (= 9 (:credit (get-runner))))))

;; Surge and virus counter flag tests
(deftest surge-valid-target
  ;; Add counters if target is a virus and had a counter added this turn
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Imp" 1) (qty "Surge" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Imp")
    (let [imp (get-in @state [:hazPlayer :rig :program 0])]
      (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
      (play-from-hand state :hazPlayer "Surge")
      (prompt-select :hazPlayer imp)
      (is (= 4 (get-counters (refresh imp) :virus)) "Imp has 4 counters after surge"))))

(deftest surge-target-not-virus
  ;; Don't fire surge if target is not a virus
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Security Testing" 1) (qty "Surge" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Security Testing")
    (let [st (get-in @state [:hazPlayer :rig :resource 0])]
      (play-from-hand state :hazPlayer "Surge")
      (prompt-select :hazPlayer st)
      (is (not (contains? st :counter)) "Surge does not fire on Security Testing"))))

(deftest surge-target-no-token-this-turn
  ;; Don't fire surge if target does not have virus counter flag set
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Imp" 1) (qty "Surge" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Imp")
    (let [imp (get-in @state [:hazPlayer :rig :program 0])]
      (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
      (take-credits state :hazPlayer 3)
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Surge")
      (prompt-select :hazPlayer imp)
      (is (= 2 (get-counters (refresh imp) :virus))
          "Surge does not fire on Imp turn after install"))))

(deftest surge-target-gorman-drip
  ;; Don't allow surging Gorman Drip, since it happens on the corp turn
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Gorman Drip v1" 1) (qty "Surge" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Gorman Drip v1")
    (let [gd (get-in @state [:hazPlayer :rig :program 0])]
      (is (= 0 (get-counters gd :virus)) "Gorman Drip starts without counters")
      (take-credits state :hazPlayer 3)
      (take-credits state :resPlayer)
      (is (= 3 (get-counters (refresh gd) :virus))
          "Gorman Drip gains 3 counters after Corp clicks 3 times for credits")
      (play-from-hand state :hazPlayer "Surge")
      (prompt-select :hazPlayer gd)
      (is (= 3 (get-counters (refresh gd) :virus)) "Surge does not trigger on Gorman Drip"))))

(deftest system-outage
  ;; When Corp draws 1+ cards, it loses 1 if it is not the first time he or she has drawn cards this turn
  (do-game
    (new-game (default-corp [(qty "Turing" 10)])
              (default-runner [(qty "Street Peddler" 1)
                               (qty "System Outage" 3)]))
    (starting-hand state :resPlayer [])
    (starting-hand state :hazPlayer ["Street Peddler" "System Outage"])
    (take-credits state :resPlayer) ; corp at 8cr
    (play-from-hand state :hazPlayer "Street Peddler")
    (take-credits state :hazPlayer)
    (core/click-draw state :resPlayer 1)
    (is (= 8 (:credit (get-corp))) "1st card drawn for free - System Outage on Peddler")
    (core/click-draw state :resPlayer 1)
    (is (= 8 (:credit (get-corp))) "2nd card drawn for free - System Outage on Peddler")
    (take-credits state :resPlayer) ; corp at 9cr
    (is (= 9 (:credit (get-corp))) "Corp at 9")
    (play-from-hand state :hazPlayer "System Outage")
    (take-credits state :hazPlayer)
    (core/click-draw state :resPlayer 1)
    (is (= 8 (:credit (get-corp))) "1st card drawn cost 1cr - System Outage active")
    (core/click-draw state :resPlayer 1)
    (is (= 7 (:credit (get-corp))) "2nd card drawn cost 1cr - System Outage active")))

(deftest test-run
  ;; Test Run - Programs hosted after install get returned to Stack. Issue #1081
  (do-game
    (new-game (default-corp [(qty "Wraparound" 1)])
              (default-runner [(qty "Test Run" 2) (qty "Morning Star" 1)
                               (qty "Knight" 1) (qty "Leprechaun" 1)]))
    (play-from-hand state :resPlayer "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (core/rez state :resPlayer wrap)
      (take-credits state :resPlayer)
      (core/gain state :hazPlayer :credit 5)
      (core/move state :hazPlayer (find-card "Morning Star" (:hand (get-runner))) :discard)
      (core/move state :hazPlayer (find-card "Knight" (:hand (get-runner))) :discard)
      (let [ms (find-card "Morning Star" (:discard (get-runner)))]
        (play-from-hand state :hazPlayer "Leprechaun")
        (play-from-hand state :hazPlayer "Test Run")
        (prompt-choice :hazPlayer "Heap")
        (prompt-choice :hazPlayer ms)
        (let [lep (get-in @state [:hazPlayer :rig :program 0])
              ms (get-in @state [:hazPlayer :rig :program 1])]
          (card-ability state :hazPlayer lep 1)
          (prompt-select :hazPlayer ms)
          (is (= "Morning Star" (:title (first (:hosted (refresh lep))))) "Morning Star hosted on Lep")
          (take-credits state :hazPlayer)
          (is (= "Morning Star" (:title (first (:deck (get-runner))))) "Morning Star returned to Stack from host")
          (take-credits state :resPlayer)
          (let [kn (find-card "Knight" (:discard (get-runner)))]
            (play-from-hand state :hazPlayer "Test Run")
            (prompt-choice :hazPlayer "Heap")
            (prompt-choice :hazPlayer kn)
            (let [kn (get-in @state [:hazPlayer :rig :program 1])]
              (card-ability state :hazPlayer kn 0)
              (prompt-select :hazPlayer wrap)
              (is (= "Knight" (:title (first (:hosted (refresh wrap))))) "Knight hosted on Wraparound")
              (take-credits state :hazPlayer)
              (is (= "Knight" (:title (first (:deck (get-runner))))) "Knight returned to Stack from host ICE"))))))))

(deftest test-run-scavenge
  ;; Test Run - Make sure program remains installed if Scavenged
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Test Run" 1) (qty "Morning Star" 1)
                               (qty "Scavenge" 1) (qty "Inti" 1)]))
    (take-credits state :resPlayer)
    (core/move state :hazPlayer (find-card "Morning Star" (:hand (get-runner))) :discard)
    (play-from-hand state :hazPlayer "Test Run")
    (let [ms (find-card "Morning Star" (:discard (get-runner)))]
      (prompt-choice :hazPlayer "Heap")
      (prompt-choice :hazPlayer ms)
      (is (= 2 (:credit (get-runner))) "Program installed for free")
      (let [ms (get-in @state [:hazPlayer :rig :program 0])]
        (play-from-hand state :hazPlayer "Scavenge")
        (prompt-select :hazPlayer ms)
        (prompt-select :hazPlayer (find-card "Morning Star" (:discard (get-runner))))
        (take-credits state :hazPlayer)
        (is (empty? (:deck (get-runner))) "Morning Star not returned to Stack")
        (is (= "Morning Star" (:title (get-in @state [:hazPlayer :rig :program 0]))) "Morning Star still installed")))))

(deftest makers-eye
  (do-game
    (new-game (default-corp [(qty "Quandary" 5)])
              (default-runner [(qty "The Maker's Eye" 1)]))
    (dotimes [_ 5] (core/move state :resPlayer (first (:hand (get-corp))) :deck))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    (run-successful state)
    (prompt-choice :hazPlayer "Card from deck")
    (is (= "You accessed Quandary" (-> (get-runner) :prompt first :msg)) "1st quandary")
    (prompt-choice :hazPlayer "OK")
    (prompt-choice :hazPlayer "Card from deck")
    (is (= "You accessed Quandary" (-> (get-runner) :prompt first :msg)) "2nd quandary")
    (prompt-choice :hazPlayer "OK")
    (prompt-choice :hazPlayer "Card from deck")
    (is (= "You accessed Quandary" (-> (get-runner) :prompt first :msg)) "3rd quandary")
    (prompt-choice :hazPlayer "OK")
    (is (not (:run @state)))))

(deftest the-price-of-freedom
  ;; The Price of Freedom - A connection must be trashed, the card is removed from game, then the corp can't advance cards next turn
  (do-game
    (new-game (default-corp [(qty "NAPD Contract" 1)])
              (default-runner [(qty "Kati Jones" 1) (qty "The Price of Freedom" 1)]))
    (play-from-hand state :resPlayer "NAPD Contract" "New remote")
    (take-credits state :resPlayer)
    (is (= 7 (:credit (get-corp))) "Corp has 7 credits (play NAPD + 2 clicks for credit")
    (play-from-hand state :hazPlayer "The Price of Freedom")
    (is (= 2 (count (get-in @state [:hazPlayer :hand]))) "The Price of Freedom could not be played because no connection is installed")
    (is (= 0 (count (get-in (get-runner) [:rig :resource]))) "Kati Jones is not installed")
    (play-from-hand state :hazPlayer "Kati Jones")
    (is (= 1 (count (get-in @state [:hazPlayer :rig :resource]))) "Kati Jones was installed")
    (play-from-hand state :hazPlayer "The Price of Freedom")
    (is (= 0 (count (get-in @state [:hazPlayer :hand]))) "The Price of Freedom can be played because a connection is in play")
    (let [kj (find-card "Kati Jones" (:resource (:rig (get-runner))))]
      (prompt-choice :hazPlayer kj)
      (is (= 0 (count (get-in (get-runner) [:rig :resource]))) "Kati Jones was trashed wth The Price of Freedom")
      (is (= 1 (count (get-in (get-runner) [:discard]))) "The Price of Freedom was removed from game, and only Kati Jones is in the discard"))
    (take-credits state :hazPlayer)
    (let [napd (get-content state :remote1 0)]
      (core/advance state :resPlayer {:card (refresh napd)})
      (is (= 7 (:credit (get-corp))) "NAPD contract could not be advanced because of The Price of Freedom")
      (take-credits state :resPlayer)
      (is (= 10 (:credit (get-corp))) "Corp has 10 credits now (3 clicks for credit, no click charged for failed advancing)")
      (take-credits state :hazPlayer)
      (core/advance state :resPlayer {:card (refresh napd)})
      (core/advance state :resPlayer {:card (refresh napd)})
      (core/advance state :resPlayer {:card (refresh napd)})
      (is (= 7 (:credit (get-corp))) "NAPD could be advanced (3 credits charged for advancing)"))))

(deftest tinkering
  ;; Tinkering - Add subtypes to ice
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 1)])
      (default-runner [(qty "Tinkering" 1)]))
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Tinkering")
    (let [iwall (get-ice state :hq 0)]
      (prompt-select :hazPlayer iwall)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (core/has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (core/rez state :resPlayer iwall)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (core/has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (take-credits state :hazPlayer)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (not (core/has-subtype? (refresh iwall) "Code Gate")) "Ice Wall does not have Code Gate")
      (is (not (core/has-subtype? (refresh iwall) "Sentry")) "Ice Wall does not have Sentry"))))

(deftest unscheduled-maintenance
  ;; Unscheduled Maintenance - prevent Corp from installing more than 1 ICE per turn
  (do-game
    (new-game
      (default-corp [(qty "Vanilla" 2) (qty "Breaking News" 1)])
      (default-runner [(qty "Unscheduled Maintenance" 1)]))
    (play-from-hand state :resPlayer "Breaking News" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Unscheduled Maintenance")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Vanilla" "HQ")
    (is (= 1 (count (get-in @state [:resPlayer :servers :hq :ices]))) "First ICE install of turn allowed")
    (play-from-hand state :resPlayer "Vanilla" "R&D")
    (is (empty? (get-in @state [:resPlayer :servers :rd :ices])) "Second ICE install of turn blocked")
    (score-agenda state :resPlayer (get-content state :remote1 0))
    (play-from-hand state :resPlayer "Vanilla" "R&D")
    (is (= 1 (count (get-in @state [:resPlayer :servers :rd :ices]))) "Current trashed; second ICE install of turn allowed")))

(deftest vamp
  ;; Vamp - Run HQ and use replace access to pay credits to drain equal amount from Corp
  (do-game
    (new-game (default-corp) (default-runner [(qty "Vamp" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :resPlayer)
    (is (= 8 (:credit (get-corp))))
    (play-from-hand state :hazPlayer "Sure Gamble")
    (play-from-hand state :hazPlayer "Sure Gamble")
    (is (= 13 (:credit (get-runner))))
    (play-run-event state (find-card "Vamp" (:hand (get-runner))) :hq)
    (prompt-choice :hazPlayer "Run ability")
    (prompt-choice :hazPlayer 8)
    (is (= 1 (:tag (get-runner))) "Took 1 tag")
    (is (= 5 (:credit (get-runner))) "Paid 8 credits")
    (is (= 0 (:credit (get-corp))) "Corp lost all 8 credits")))

(deftest virus-counter-flag-on-enter
  ;; Set counter flag when virus card enters play with counters
  (do-game
    (new-game (default-corp) (default-runner [(qty "Surge" 1) (qty "Imp" 1) (qty "Crypsis" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Imp")
    (let [imp (get-in @state [:hazPlayer :rig :program 0])]
      (is (get-in imp [:added-virus-counter]) "Counter flag was not set on Imp"))))

(deftest virus-counter-flag-on-add-prop
  ;; Set counter flag when add-prop is called on a virus
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Crypsis" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Crypsis")
    (let [crypsis (get-in @state [:hazPlayer :rig :program 0])]
      (card-ability state :hazPlayer crypsis 2) ;click to add a virus counter
      (is (= 1 (get-counters (refresh crypsis) :virus)) "Crypsis added a virus token")
      (is (get-in (refresh crypsis) [:added-virus-counter])
          "Counter flag was set on Crypsis"))))

(deftest virus-counter-flag-clear-on-end-turn
  ;; Clear the virus counter flag at the end of each turn
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Crypsis" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Crypsis")
    (let [crypsis (get-in @state [:hazPlayer :rig :program 0])]
      (card-ability state :hazPlayer crypsis 2) ; click to add a virus counter
      (take-credits state :hazPlayer 2)
      (take-credits state :resPlayer 1)
      (is (not (get-in (refresh crypsis) [:added-virus-counter]))
          "Counter flag was cleared on Crypsis"))))
