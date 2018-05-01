(ns test.cards.operations
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest twenty-four-seven-news-cycle-breaking-news
  ;; 24/7 News Cycle - Breaking News interaction
  (do-game
    (new-game (default-minion [(qty "Breaking News" 2) (qty "24/7 News Cycle" 3)])
              (default-hero))
    (play-from-hand state :minion "Breaking News" "New remote")
    (play-from-hand state :minion "Breaking News" "New remote")
    (let [ag1 (get-content state :remote1 0)
          ag2 (get-content state :remote2 0)]
      (score-agenda state :minion ag1)
      (score-agenda state :minion ag2)
      (take-credits state :minion)
      (is (= 0 (:tag (get-hero)))) ; tags cleared
      (take-credits state :hero)
      (play-from-hand state :minion "24/7 News Cycle")
      (prompt-select :minion (find-card "Breaking News" (:scored (get-minion))))
      (is (= 1 (:agenda-point (get-minion))) "Forfeited Breaking News")
      (prompt-select :minion (find-card "Breaking News" (:scored (get-minion))))
      (is (= 2 (:tag (get-hero))) "Runner given 2 tags")
      (take-credits state :minion 2)
      (is (= 2 (:tag (get-hero))) "Tags remained after Corp ended turn"))))

(deftest twenty-four-seven-news-cycle-posted-bounty
  ;; 24/7 News Cycle and Posted Bounty interaction -- Issue #1043
  (do-game
    (new-game (default-minion [(qty "Posted Bounty" 2) (qty "24/7 News Cycle" 3)])
              (default-hero))
    (play-from-hand state :minion "Posted Bounty" "New remote")
    (play-from-hand state :minion "Posted Bounty" "New remote")
    (let [ag1 (get-content state :remote1 0)
          ag2 (get-content state :remote2 0)]
      (score-agenda state :minion ag1)
      (prompt-choice :minion "No")
      (score-agenda state :minion ag2)
      (prompt-choice :minion "No")
      (play-from-hand state :minion "24/7 News Cycle")
      (prompt-select :minion (find-card "Posted Bounty" (:scored (get-minion))))
      (is (= 1 (:agenda-point (get-minion))) "Forfeited Posted Bounty")
      (prompt-select :minion (find-card "Posted Bounty" (:scored (get-minion))))
      (prompt-choice :minion "Yes") ; "Forfeit Posted Bounty to give 1 tag?"
      (is (= 1 (:tag (get-hero))) "Runner given 1 tag")
      (is (= 1 (:bad-publicity (get-minion))) "Corp has 1 bad publicity")
      (is (= 0 (:agenda-point (get-minion))) "Forfeited Posted Bounty to 24/7 News Cycle"))))

(deftest twenty-four-seven-news-cycle-swaps
  ;; 24/7 News Cycle - Swapped agendas are able to be used. #1555
  (do-game
    (new-game (default-minion [(qty "24/7 News Cycle" 1) (qty "Chronos Project" 1)
                             (qty "Philotic Entanglement" 1) (qty "Profiteering" 1)])
              (default-hero [(qty "Turntable" 3)]))
    (score-agenda state :minion (find-card "Chronos Project" (:hand (get-minion))))
    (score-agenda state :minion (find-card "Philotic Entanglement" (:hand (get-minion))))
    (take-credits state :minion)
    (play-from-hand state :hero "Turntable")
    (core/steal state :hero (find-card "Profiteering" (:hand (get-minion))))
    (prompt-choice :hero "Yes")
    (prompt-select :hero (find-card "Philotic Entanglement" (:scored (get-minion))))
    (is (= 2 (:agenda-point (get-minion))))
    (is (= 2 (:agenda-point (get-hero))))
    (take-credits state :hero)
    (play-from-hand state :minion "24/7 News Cycle")
    (prompt-select :minion (find-card "Chronos Project" (:scored (get-minion))))
    (is (= "Chronos Project" (:title (first (:rfg (get-minion))))))
    ;; shouldn't work on an agenda in the Runner's scored area
    (is (= 2 (count (:hand (get-hero)))))
    (prompt-select :minion (find-card "Philotic Entanglement" (:scored (get-hero))))
    (is (= 2 (count (:hand (get-hero)))))
    ;; resolve 'when scored' ability on swapped Profiteering
    (is (= 8 (:credit (get-minion))))
    (prompt-select :minion (find-card "Profiteering" (:scored (get-minion))))
    (prompt-choice :minion "3")
    (is (= 1 (:agenda-point (get-minion))))
    (is (= 3 (:bad-publicity (get-minion))))
    (is (= 23 (:credit (get-minion))) "Gained 15 credits")))

(deftest accelerated-diagnostics
  ;; Accelerated Diagnostics - Interaction with prompt effects, like Shipment from SanSan
  (do-game
    (new-game (default-minion [(qty "Accelerated Diagnostics" 1) (qty "Cerebral Overwriter" 1) (qty "Shipment from SanSan" 1)
                             (qty "Hedge Fund" 1) (qty "Back Channels" 1)])
              (default-hero))
    (starting-hand state :minion ["Accelerated Diagnostics" "Cerebral Overwriter"])
    (play-from-hand state :minion "Cerebral Overwriter" "New remote")
    (core/gain state :minion :credit 1)
    (play-from-hand state :minion "Accelerated Diagnostics")

    (let [playarea (get-in @state [:minion :play-area])
          hf (find-card "Hedge Fund" playarea)
          ss (find-card "Shipment from SanSan" playarea)
          bc (find-card "Back Channels" playarea)
          co (get-content state :remote1 0)]
      (is (= 3 (count playarea)) "3 cards in play area")
      (prompt-select :minion ss)
      (prompt-choice :minion "2")
      (prompt-select :minion co)
      (is (= 2 (:advance-counter (refresh co))) "Cerebral Overwriter gained 2 advancements")
      (prompt-select :minion hf)
      (is (= 9 (:credit (get-minion))) "Corp gained credits from Hedge Fund")
      (prompt-select :minion bc)
      (prompt-select :minion (refresh co))
      (is (= 15 (:credit (get-minion))) "Corp gained 6 credits for Back Channels"))))

(deftest accelerated-diagnostics-with-current
  ;; Accelerated Diagnostics - Interaction with Current
  (do-game
    (new-game (default-minion [(qty "Accelerated Diagnostics" 1) (qty "Cerebral Overwriter" 1)
                             (qty "Enhanced Login Protocol" 1) (qty "Shipment from SanSan" 1)
                             (qty "Hedge Fund" 1)])
              (default-hero))
    (starting-hand state :minion ["Accelerated Diagnostics" "Cerebral Overwriter"])
    (play-from-hand state :minion "Cerebral Overwriter" "New remote")
    (core/gain state :minion :credit 3)
    (play-from-hand state :minion "Accelerated Diagnostics")

    (let [playarea (get-in @state [:minion :play-area])
          hf (find-card "Hedge Fund" playarea)
          ss (find-card "Shipment from SanSan" playarea)
          elp (find-card "Enhanced Login Protocol" playarea)
          co (get-content state :remote1 0)]
      (is (= 3 (count playarea)) "3 cards in play area")
      (prompt-select :minion elp)
      (is (= "Enhanced Login Protocol" (:title (first (get-in @state [:minion :current]))))
        "Enhanced Login Protocol active in Current area")
      (prompt-select :minion ss)
      (prompt-choice :minion "2")
      (prompt-select :minion co)
      (is (= 2 (:advance-counter (refresh co))) "Cerebral Overwriter gained 2 advancements")
      (prompt-select :minion hf)
      (is (= 9 (:credit (get-minion))) "Corp gained credits from Hedge Fund"))))

(deftest an-offer-you-cant-refuse
  ;; An Offer You Can't Refuse - exact card added to score area, not the last discarded one
  (do-game
    (new-game (default-minion [(qty "Celebrity Gift" 1) (qty "An Offer You Can't Refuse" 1)])
              (default-hero))
    (play-from-hand state :minion "An Offer You Can't Refuse")
    (prompt-choice :minion "R&D")
    (core/move state :minion (find-card "Celebrity Gift" (:hand (get-minion))) :discard)
    (is (= 2 (count (:discard (get-minion)))))
    (prompt-choice :hero "No")
    (is (= 1 (:agenda-point (get-minion))) "An Offer the Runner refused")
    (is (= 1 (count (:scored (get-minion)))))
    (is (find-card "An Offer You Can't Refuse" (:scored (get-minion))))
    (is (= 1 (count (:discard (get-minion)))))
    (is (find-card "Celebrity Gift" (:discard (get-minion))))))

(deftest big-brother
  ;; Big Brother - Give the Runner 2 tags if already tagged
  (do-game
    (new-game (default-minion [(qty "Big Brother" 1)])
              (default-hero))
    (play-from-hand state :minion "Big Brother")
    (is (= 1 (count (:hand (get-minion)))) "Card not played because Runner has no tags")
    (core/gain state :hero :tag 1)
    (play-from-hand state :minion "Big Brother")
    (is (= 3 (:tag (get-hero))) "Runner gained 2 tags")))

(deftest biotic-labor
  ;; Biotic Labor - Gain 2 clicks
  (do-game
    (new-game (default-minion [(qty "Biotic Labor" 1)])
              (default-hero))
    (play-from-hand state :minion "Biotic Labor")
    (is (= 1 (:credit (get-minion))))
    (is (= 4 (:click (get-minion))) "Spent 1 click to gain 2 additional clicks")))

(deftest blue-level-clearance
  ;; Blue Level Clearance - Gain 5 credits and draw 2 cards
  (do-game
    (new-game (default-minion [(qty "Blue Level Clearance" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Sweeps Week" 2)])
              (default-hero))
    (play-from-hand state :minion "Blue Level Clearance")
    (is (= 8 (:credit (get-minion))) "Gained 5 credits")
    (is (= 1 (:click (get-minion))))
    (is (= 7 (count (:hand (get-minion)))) "Drew 2 cards")))

(deftest casting-call
  ;; Casting Call - Only do card-init on the Public agendas.  Issue #1128
  (do-game
    (new-game (default-minion [(qty "Casting Call" 2) (qty "Oaktown Renovation" 1)
                             (qty "Improved Tracers" 1) (qty "Hunter" 1)])
              (default-hero))
    (core/gain state :minion :click 1)
    (play-from-hand state :minion "Hunter" "HQ")
    (let [hunter (get-ice state :hq 0)]
      (core/rez state :minion hunter)
      (is (= 4 (:current-strength (refresh hunter))))
      (play-from-hand state :minion "Casting Call")
      (prompt-select :minion (find-card "Improved Tracers" (:hand (get-minion))))
      (prompt-choice :minion "New remote")
      (let [imptrac (get-content state :remote1 0)]
        (is (get-in (refresh imptrac) [:rezzed]) "Improved Tracers is faceup")
        (is (= 4 (:current-strength (refresh hunter))) "Hunter hasn't gained strength")
        (play-from-hand state :minion "Casting Call")
        (prompt-select :minion (find-card "Oaktown Renovation" (:hand (get-minion))))
        (prompt-choice :minion "New remote")
        (let [oak (get-content state :remote2 0)]
          (core/advance state :minion {:card (refresh oak)})
          (is (= 5 (:credit (get-minion))) "Events on Public agenda work; gained 2 credits from advancing")
          (take-credits state :minion)
          (run-empty-server state "Server 2")
          (prompt-select :hero oak)
          (prompt-choice :hero "Steal")
          (is (= 2 (:tag (get-hero))) "Runner took 2 tags from accessing agenda with Casting Call hosted on it"))))))

(deftest cerebral-cast-hero-wins
  ;; Cerebral Cast: if the hero succefully ran last turn, psi game to give hero choice of tag or BD
  (do-game
    (new-game (default-minion [(qty "Cerebral Cast" 1)])
              (default-hero))
	    (play-from-hand state :minion "Cerebral Cast")
	    (is (= 3 (:click (get-minion))) "Cerebral Cast precondition not met; card not played")
	    (take-credits state :minion)
	    (run-empty-server state "Archives")
	    (take-credits state :hero)
	    (play-from-hand state :minion "Cerebral Cast")
        (prompt-choice :minion "0 [Credits]")
        (prompt-choice :hero "0 [Credits]")
	    (is (= 0 (count (:discard (get-hero)))) "Runner took no damage")
		(is (= 0 (:tag (get-hero))) "Runner took no tags")))

(deftest cerebral-cast-minion-wins
  ;; Cerebral Cast: if the hero succefully ran last turn, psi game to give hero choice of tag or BD
  (do-game
    (new-game (default-minion [(qty "Cerebral Cast" 2)])
              (default-hero))
	    (take-credits state :minion)
	    (run-empty-server state "Archives")
	    (take-credits state :hero)
	    (play-from-hand state :minion "Cerebral Cast")
        (prompt-choice :minion "0 [Credits]")
        (prompt-choice :hero "1 [Credits]")
		(prompt-choice :hero "1 brain damage")
	    (is (= 1 (count (:discard (get-hero)))) "Runner took a brain damage")
		(is (= 0 (:tag (get-hero))) "Runner took no tags from brain damage choice")
	    (play-from-hand state :minion "Cerebral Cast")
        (prompt-choice :minion "0 [Credits]")
        (prompt-choice :hero "1 [Credits]")
		(prompt-choice :hero "1 tag")
	    (is (= 1 (count (:discard (get-hero)))) "Runner took no additional damage")
		(is (= 1 (:tag (get-hero))) "Runner took a tag from Cerebral Cast choice")))


(deftest cerebral-static-chaos-theory
  ;; Cerebral Static - vs Chaos Theory
  (do-game
    (new-game (default-minion [(qty "Cerebral Static" 1) (qty "Lag Time" 1)])
              (make-deck "Chaos Theory: Wünderkind" [(qty "Sure Gamble" 3)]))
    (is (= 5 (:memory (get-hero))) "CT starts with 5 memory")
    (play-from-hand state :minion "Cerebral Static")
    (is (= 4 (:memory (get-hero))) "Cerebral Static causes CT to have 4 memory")
    (play-from-hand state :minion "Lag Time")
    (is (= 5 (:memory (get-hero))) "CT 5 memory restored")))

(deftest closed-accounts
  ;; Closed Accounts - Play if Runner is tagged to make Runner lose all credits
  (do-game
    (new-game (default-minion [(qty "Closed Accounts" 1)])
              (default-hero))
    (play-from-hand state :minion "Closed Accounts")
    (is (and (= 3 (:click (get-minion)))
             (= 5 (:credit (get-hero))))
        "Closed Accounts precondition not met; card not played")
    (core/gain state :hero :tag 1)
    (play-from-hand state :minion "Closed Accounts")
    (is (= 0 (:credit (get-hero))) "Runner lost all credits")))

(deftest commercialization-single-advancement
  ;; Commercialization - Single advancement token
  (do-game
    (new-game (default-minion [(qty "Commercialization" 1)
                             (qty "Ice Wall" 1)])
              (default-hero))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (core/add-counter state :minion (refresh (get-ice state :hq 0)) :advancement 1)
    (play-from-hand state :minion "Commercialization")
    (prompt-select :minion (refresh (get-ice state :hq 0)))
    (is (= 6 (:credit (get-minion))) "Gained 1 for single advanced ice from Commercialization")))

(deftest commercialization-double-advancement
  ;; Commercialization - Two advancement tokens
  (do-game
    (new-game (default-minion [(qty "Commercialization" 1)
                             (qty "Ice Wall" 1)])
              (default-hero))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (core/add-counter state :minion (refresh (get-ice state :hq 0)) :advancement 2)
    (play-from-hand state :minion "Commercialization")
    (prompt-select :minion (refresh (get-ice state :hq 0)))
    (is (= 7 (:credit (get-minion))) "Gained 2 for double advanced ice from Commercialization")))

(deftest consulting-visit
  ;; Consulting Visit - Only show single copies of operations minion can afford as choices. Play chosen operation
  (do-game
    (new-game (default-minion [(qty "Consulting Visit" 1)
                             (qty "Beanstalk Royalties" 2)
                             (qty "Green Level Clearance" 1)
                             (qty "Breaking News" 1)
                             (qty "Hedge Fund" 1)])
              (default-hero))
    (is (= 5 (:credit (get-minion))))
    (starting-hand state :minion ["Consulting Visit"])
    (play-from-hand state :minion "Consulting Visit")

    (let [get-prompt (fn [] (first (#(get-in @state [:minion :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]

      (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
      (prompt-choice :minion (find-card "Beanstalk Royalties" (:deck (get-minion))))
      (is (= 6 (:credit (get-minion)))))))

(deftest consulting-visit-mumbad
  ;; Consulting Visit - Works properly when played with Mumbad City Hall
  (do-game
    (new-game (default-minion [(qty "Mumbad City Hall" 1)
                             (qty "Beanstalk Royalties" 1)
                             (qty "Green Level Clearance" 1)
                             (qty "Breaking News" 1)
                             (qty "Hedge Fund" 1)
                             (qty "Consulting Visit" 1)
                             (qty "Mumba Temple" 1)])
              (default-hero))
    (is (= 5 (:credit (get-minion))))
    (starting-hand state :minion ["Mumbad City Hall"])
    (play-from-hand state :minion "Mumbad City Hall" "New remote")

    (let [hall (get-content state :remote1 0)
          get-prompt (fn [] (first (#(get-in @state [:minion :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]

      (card-ability state :minion hall 0)
      (is (= (list "Consulting Visit" "Mumba Temple" nil) (prompt-names)))

      (prompt-choice :minion (find-card "Consulting Visit" (:deck (get-minion))))
      (is (= 3 (:credit (get-minion))))
      (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))

      (prompt-choice :minion (find-card "Green Level Clearance" (:deck (get-minion))))
      (is (= 5 (:credit (get-minion)))))))

(deftest defective-brainchips
  ;; Defective Brainchips - Do 1 add'l brain damage the first time Runner takes some each turn
  (do-game
    (new-game (default-minion [(qty "Defective Brainchips" 1) (qty "Viktor 1.0" 1)])
              (default-hero [(qty "Sure Gamble" 2) (qty "Shiv" 2)]))
    (play-from-hand state :minion "Defective Brainchips")
    (play-from-hand state :minion "Viktor 1.0" "HQ")
    (take-credits state :minion)
    (run-on state :hq)
    (let [vik (get-ice state :hq 0)]
      (core/rez state :minion vik)
      (card-subroutine state :minion vik 0)
      (is (= 2 (count (:discard (get-hero)))) "2 cards lost to brain damage")
      (is (= 2 (:brain-damage (get-hero))) "Brainchips dealt 1 additional brain dmg")
      (card-subroutine state :minion vik 0)
      (is (= 3 (count (:discard (get-hero)))) "2 cards lost to brain damage")
      (is (= 3 (:brain-damage (get-hero))) "Brainchips didn't do additional brain dmg"))))

(deftest distract-the-masses
  (do-game
    (new-game (default-minion [(qty "Distract the Masses" 2) (qty "Hedge Fund" 3)])
              (default-hero))
    (starting-hand state :minion ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Distract the Masses" "Distract the Masses"])
    (play-from-hand state :minion "Distract the Masses")
    (prompt-select :minion (first (:hand (get-minion))))
    (prompt-select :minion (first (next (:hand (get-minion)))))
    (prompt-select :minion (first (:discard (get-minion))))
    (prompt-choice :minion "Done")
    (is (= 1 (count (:discard (get-minion)))) "1 card still discarded")
    (is (= 1 (count (:deck (get-minion)))) "1 card shuffled into R&D")
    (is (= 1 (count (:rfg (get-minion)))) "Distract the Masses removed from game")
    (is (= 7 (:credit (get-hero))) "Runner gained 2 credits")
    (play-from-hand state :minion "Distract the Masses")
    (prompt-select :minion (first (:hand (get-minion))))
    (prompt-choice :minion "Done")
    (prompt-select :minion (first (:discard (get-minion))))
    (prompt-select :minion (first (next (:discard (get-minion)))))
    (is (= 0 (count (:discard (get-minion)))) "No cards left in archives")
    (is (= 3 (count (:deck (get-minion)))) "2 more cards shuffled into R&D")
    (is (= 2 (count (:rfg (get-minion)))) "Distract the Masses removed from game")
    (is (= 9 (:credit (get-hero))) "Runner gained 2 credits")))

(deftest diversified-portfolio
  (do-game
    (new-game (default-minion [(qty "Diversified Portfolio" 1)
                             (qty "Paper Wall" 1)
                             (qty "PAD Campaign" 3)])
              (default-hero))
    (core/gain state :minion :click 2)
    (play-from-hand state :minion "Paper Wall" "New remote")
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (play-from-hand state :minion "Diversified Portfolio")
    (is (= 7 (:credit (get-minion))) "Ignored remote with ICE but no server contents")))

(deftest economic-warfare
  ;; Economic Warfare - If successful run last turn, make the hero lose 4 credits if able
  (do-game
    (new-game (default-minion [(qty "Economic Warfare" 3)])
              (default-hero))
    (play-from-hand state :minion "Economic Warfare")
    (is (= 5 (:credit (get-hero))) "Runner has 5 credits")
    (is (= 3 (count (:hand (get-minion)))) "Corp still has 3 cards")
    (take-credits state :minion)

    (run-on state :archives)
    (run-successful state)
    (take-credits state :hero)

    (play-from-hand state :minion "Economic Warfare")
    (is (= 4 (:credit (get-hero))) "Runner has 4 credits")
    (play-from-hand state :minion "Economic Warfare")
    (is (= 0 (:credit (get-hero))) "Runner has 0 credits")
    (take-credits state :minion)

    (run-on state :archives)
    (take-credits state :hero)

    (play-from-hand state :minion "Economic Warfare")
    (is (= 3 (:credit (get-hero))) "Runner has 3 credits")))

(deftest enhanced-login-protocol
  ;; Enhanced Login Protocol - First click run each turn costs an additional click
  (do-game
    (new-game (default-minion [(qty "Enhanced Login Protocol" 1)])
              (default-hero [(qty "Employee Strike" 1)]))
    (play-from-hand state :minion "Enhanced Login Protocol")
    (take-credits state :minion)

    (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
    (run-on state :archives)
    (is (= 2 (:click (get-hero)))
        "Runner spends 1 additional click to make the first run")
    (run-successful state)

    (run-on state :archives)
    (is (= 1 (:click (get-hero)))
        "Runner doesn't spend 1 additional click to make the second run")
    (run-successful state)

    (take-credits state :hero)
    (take-credits state :minion)
    (take-credits state :hero 3)

    (is (= 1 (:click (get-hero))) "Runner has 1 click")
    (run-on state :archives)
    (is (not (:run @state)) "No run was initiated")
    (is (= 1 (:click (get-hero))) "Runner has 1 click")

    (take-credits state :hero)
    (take-credits state :minion)

    (play-from-hand state :hero "Employee Strike")

    (is (= 3 (:click (get-hero))) "Runner has 3 clicks")
    (run-on state :archives)
    (is (= 2 (:click (get-hero)))
        "Runner doesn't spend 1 additional click to make a run")))

(deftest enhanced-login-protocol-card-ability
  ;; Enhanced Login Protocol - Card ability runs don't cost additional clicks
  (do-game
    (new-game (default-minion [(qty "Enhanced Login Protocol" 1)])
              (default-hero [(qty "Sneakdoor Beta" 1)]))
    (play-from-hand state :minion "Enhanced Login Protocol")
    (take-credits state :minion)
    (play-from-hand state :hero "Sneakdoor Beta")
    (take-credits state :hero)
    (take-credits state :minion)

    (is (= 4 (:click (get-hero))) "Runner has 2 clicks")
    (let [sneakdoor (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero sneakdoor 0)
      (is (= 3 (:click (get-hero)))
          "Runner doesn't spend 1 additional click to run with a card ability")
      (run-successful state)

      (run-on state :archives)
      (is (= 1 (:click (get-hero)))
          "Runner spends 1 additional click to make a run")
      (run-successful state)

      (take-credits state :hero)
      (take-credits state :minion)

      (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-hero)))
          "Runner spends 1 additional click to make a run"))))

(deftest enhanced-login-protocol-new-angeles-sol
  ;; Enhanced Login Protocol trashed and reinstalled on steal doesn't double remove penalty
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Enhanced Login Protocol" 1) (qty "Breaking News" 1)])
      (default-hero))
    (play-from-hand state :minion "Breaking News" "New remote")
    (play-from-hand state :minion "Enhanced Login Protocol")
    (take-credits state :minion)

    (run-on state :remote1)
    (run-successful state)
    (prompt-choice :hero "Steal")

    (prompt-choice :minion "Yes")
    (prompt-select :minion (find-card "Enhanced Login Protocol"
                                    (:discard (get-minion))))

    (run-on state :archives)
    (is (= 1 (:click (get-hero))) "Runner has 1 click")))

(deftest enhanced-login-protocol-run-events
  ;; Enhanced Login Protocol - Run event don't cost additional clicks
  (do-game
    (new-game (default-minion [(qty "Enhanced Login Protocol" 1)])
              (default-hero [(qty "Out of the Ashes" 1)]))
    (play-from-hand state :minion "Enhanced Login Protocol")
    (take-credits state :minion)

    (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
    (play-from-hand state :hero "Out of the Ashes")
    (prompt-choice :hero "Archives")
    (is (= 3 (:click (get-hero)))
        "Runner doesn't spend 1 additional click to run with a run event")
    (run-successful state)

    (run-on state :archives)
    (is (= 1 (:click (get-hero)))
        "Runner spends 1 additional click to make a run")
    (run-successful state)

    (take-credits state :hero)
    (take-credits state :minion)
    (prompt-choice :hero "No") ; Out of the Ashes prompt

    (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
    (run-on state :archives)
    (is (= 2 (:click (get-hero)))
        "Runner spends 1 additional click to make a run")))

(deftest enhanced-login-protocol-hero-turn-first-run
  ;; Enhanced Login Protocol - Works when played on the hero's turn
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News"
                         [(qty "Enhanced Login Protocol" 1)
                          (qty "Breaking News" 1)])
              (default-hero [(qty "Hades Shard" 1)]))
    (trash-from-hand state :minion "Breaking News")
    (take-credits state :minion)

    (core/gain state :hero :credit 2)
    (play-from-hand state :hero "Hades Shard")
    (card-ability state :hero (get-in @state [:hero :rig :resource 0]) 0)
    (prompt-choice :hero "Steal")
    (prompt-choice :minion "Yes")
    (prompt-select :minion (find-card "Enhanced Login Protocol"
                                    (:hand (get-minion))))
    (is (find-card "Enhanced Login Protocol" (:current (get-minion)))
        "Enhanced Login Protocol is in play")

    (is (= 3 (:click (get-hero))) "Runner has 3 clicks")
    (run-on state :archives)
    (is (= 1 (:click (get-hero)))
        "Runner spends 1 additional click to make a run")))

(deftest enhanced-login-protocol-hero-turn-second-run
  ;; Enhanced Login Protocol - Doesn't fire if already run when played on the hero's turn
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News"
                         [(qty "Enhanced Login Protocol" 1)
                          (qty "Breaking News" 1)])
              (default-hero [(qty "Hades Shard" 1)]))
    (trash-from-hand state :minion "Breaking News")
    (take-credits state :minion)

    (run-on state :hq)
    (run-successful state)
    (prompt-choice :hero "OK")

    (core/gain state :hero :credit 2)
    (play-from-hand state :hero "Hades Shard")
    (card-ability state :hero (get-in @state [:hero :rig :resource 0]) 0)
    (prompt-choice :hero "Steal")
    (prompt-choice :minion "Yes")
    (prompt-select :minion (find-card "Enhanced Login Protocol"
                                    (:hand (get-minion))))
    (is (find-card "Enhanced Login Protocol" (:current (get-minion)))
        "Enhanced Login Protocol is in play")

    (is (= 2 (:click (get-hero))) "Runner has 2 clicks")
    (run-on state :archives)
    (is (= 1 (:click (get-hero)))
        "Runner doesn't spend 1 additional click to make a run")))

(deftest exchange-of-information
  ;; Exchange of Information - Swapping agendas works correctly
  (do-game
    (new-game (default-minion [(qty "Exchange of Information" 1)
                             (qty "Market Research" 1)
                             (qty "Breaking News" 1)
                             (qty "Project Beale" 1)
                             (qty "Explode-a-palooza" 1)])
              (default-hero))

      (score-agenda state :minion (find-card "Market Research" (:hand (get-minion))))
      (score-agenda state :minion (find-card "Breaking News" (:hand (get-minion))))
      (is (= 2 (:tag (get-hero))) "Runner gained 2 tags")
      (take-credits state :minion)
      (is (= 0 (:tag (get-hero))) "Runner lost 2 tags")

      (core/steal state :hero (find-card "Project Beale" (:hand (get-minion))))
      (core/steal state :hero (find-card "Explode-a-palooza" (:hand (get-minion))))
      (take-credits state :hero)

      (is (= 4 (:agenda-point (get-hero))))
      (is (= 3 (:agenda-point (get-minion))))

      (core/gain state :hero :tag 1)
      (play-from-hand state :minion "Exchange of Information")

      (prompt-select :minion (find-card "Project Beale" (:scored (get-hero))))
      (prompt-select :minion (find-card "Breaking News" (:scored (get-minion))))

      (is (= 3 (:agenda-point (get-hero))))
      (is (= 4 (:agenda-point (get-minion))))))

(deftest exchange-of-information-breaking-news
  ;; Exchange of Information - Swapping a just scored Breaking News keeps the tags
  (do-game
    (new-game (default-minion [(qty "Exchange of Information" 1)
                             (qty "Market Research" 1)
                             (qty "Breaking News" 1)
                             (qty "Project Beale" 1)
                             (qty "Explode-a-palooza" 1)])
              (default-hero))

      (take-credits state :minion)

      (core/steal state :hero (find-card "Project Beale" (:hand (get-minion))))
      (core/steal state :hero (find-card "Explode-a-palooza" (:hand (get-minion))))
      (take-credits state :hero)

      (score-agenda state :minion (find-card "Breaking News" (:hand (get-minion))))
      (is (= 2 (:tag (get-hero))) "Runner gained 2 tags")
      (play-from-hand state :minion "Exchange of Information")

      (prompt-select :minion (find-card "Project Beale" (:scored (get-hero))))
      (prompt-select :minion (find-card "Breaking News" (:scored (get-minion))))
      (is (= 2 (:tag (get-hero))) "Still has tags after swap and before end of turn")

      (take-credits state :minion)
      (is (= 3 (:agenda-point (get-hero))))
      (is (= 2 (:agenda-point (get-minion))))
      (is (= 2 (:tag (get-hero))) "Runner does not lose tags at end of turn")))

(deftest exchange-of-information-fifteen-minutes
  ;; Exchange of Information - Swapping a 15 Minutes still keeps the ability. #1783
  (do-game
    (new-game (default-minion [(qty "Exchange of Information" 2) (qty "15 Minutes" 1)
                             (qty "Project Beale" 1)])
              (default-hero))
    (score-agenda state :minion (find-card "15 Minutes" (:hand (get-minion))))
    (take-credits state :minion)
    (core/gain state :hero :tag 1)
    (core/steal state :hero (find-card "Project Beale" (:hand (get-minion))))
    (take-credits state :hero)
    (is (= 1 (:agenda-point (get-minion))))
    (is (= 2 (:agenda-point (get-hero))))
    (play-from-hand state :minion "Exchange of Information")
    (prompt-select :minion (find-card "Project Beale" (:scored (get-hero))))
    (prompt-select :minion (find-card "15 Minutes" (:scored (get-minion))))
    (is (= 2 (:agenda-point (get-minion))))
    (is (= 1 (:agenda-point (get-hero))))
    (is (= 0 (count (:deck (get-minion)))))
    ;; shuffle back into R&D from hero's scored area
    (let [fifm (get-in @state [:hero :scored 0])]
      (card-ability state :minion fifm 0))
    (is (= 2 (:agenda-point (get-minion))))
    (is (= 0 (:agenda-point (get-hero))))
    (is (= "15 Minutes" (:title (first (:deck (get-minion))))))
    (take-credits state :minion)
    (core/steal state :hero (find-card "15 Minutes" (:deck (get-minion))))
    (take-credits state :hero)
    (is (= 2 (:agenda-point (get-minion))))
    (is (= 1 (:agenda-point (get-hero))))
    (play-from-hand state :minion "Exchange of Information")
    (prompt-select :minion (find-card "15 Minutes" (:scored (get-hero))))
    (prompt-select :minion (find-card "Project Beale" (:scored (get-minion))))
    (is (= 1 (:agenda-point (get-minion))))
    (is (= 2 (:agenda-point (get-hero))))
    ;; shuffle back into R&D from minion's scored area
    (let [fifm (get-in @state [:minion :scored 0])]
      (card-ability state :minion fifm 0))
    (is (= "15 Minutes" (:title (first (:deck (get-minion))))))))

(deftest exchange-of-information-mandatory-upgrades
  ;; Exchange of Information - Swapping a Mandatory Upgrades gives the Corp an additional click per turn. #1687
  (do-game
    (new-game (default-minion [(qty "Exchange of Information" 2) (qty "Mandatory Upgrades" 1)
                             (qty "Global Food Initiative" 1)])
              (default-hero))
    (score-agenda state :minion (find-card "Global Food Initiative" (:hand (get-minion))))
    (take-credits state :minion)
    (core/gain state :hero :tag 1)
    (core/steal state :hero (find-card "Mandatory Upgrades" (:hand (get-minion))))
    (take-credits state :hero)
    (is (= 3 (:agenda-point (get-minion))))
    (is (= 2 (:agenda-point (get-hero))))
    (is (= 3 (:click (get-minion))))
    (is (= 3 (:click-per-turn (get-minion))))
    (play-from-hand state :minion "Exchange of Information")
    (prompt-select :minion (find-card "Mandatory Upgrades" (:scored (get-hero))))
    (prompt-select :minion (find-card "Global Food Initiative" (:scored (get-minion))))
    (is (= 2 (:agenda-point (get-minion))))
    (is (= 2 (:agenda-point (get-hero))))
    (is (= 3 (:click (get-minion))))
    (is (= 4 (:click-per-turn (get-minion))))
    (take-credits state :minion)
    (take-credits state :hero)
    (is (= 4 (:click (get-minion))))
    (is (= 4 (:click-per-turn (get-minion))))
    (play-from-hand state :minion "Exchange of Information")
    (prompt-select :minion (find-card "Global Food Initiative" (:scored (get-hero))))
    (prompt-select :minion (find-card "Mandatory Upgrades" (:scored (get-minion))))
    (is (= 3 (:agenda-point (get-minion))))
    (is (= 2 (:agenda-point (get-hero))))
    (is (= 2 (:click (get-minion))))
    (is (= 3 (:click-per-turn (get-minion))))
    (take-credits state :minion)
    (take-credits state :hero)
    (is (= 3 (:click (get-minion))))
    (is (= 3 (:click-per-turn (get-minion))))))

(deftest election-day
  (do-game
    (new-game (default-minion [(qty "Election Day" 7)])
                (default-hero))
      (is (= 6 (count (:hand (get-minion)))) "Corp starts with 5 + 1 cards")
      (core/move state :minion (find-card "Election Day" (:hand (get-minion))) :deck)
      (core/move state :minion (find-card "Election Day" (:hand (get-minion))) :deck)
      (core/move state :minion (find-card "Election Day" (:hand (get-minion))) :deck)
      (core/move state :minion (find-card "Election Day" (:hand (get-minion))) :deck)
      (core/move state :minion (find-card "Election Day" (:hand (get-minion))) :deck)
      (play-from-hand state :minion "Election Day")
      (is (= 1 (count (:hand (get-minion)))) "Could not play Election Day")
      (take-credits state :minion)
      (take-credits state :hero)
      (is (= 2 (count (:hand (get-minion)))) "Corp has now 1 + 1 cards before Election Day")
      (play-from-hand state :minion "Election Day")
      (is (= 5 (count (:hand (get-minion)))) "Corp has now 5 cards due to Election Day")))

(deftest hedge-fund
  (do-game
    (new-game (default-minion) (default-hero))
    (is (= 5 (:credit (get-minion))))
    (play-from-hand state :minion "Hedge Fund")
    (is (= 9 (:credit (get-minion))))))

(deftest housekeeping
  ;; Housekeeping - Runner must trash a card from Grip on first install of a turn
  (do-game
    (new-game (default-minion [(qty "Housekeeping" 1)])
              (default-hero [(qty "Cache" 2) (qty "Fall Guy" 1) (qty "Mr. Li" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Fall Guy")
    (take-credits state :hero)
    (play-from-hand state :minion "Housekeeping")
    (take-credits state :minion)
    (play-from-hand state :hero "Cache")
    (prompt-select :hero (find-card "Mr. Li" (:hand (get-hero))))
    (is (empty? (:prompt (get-hero))) "Fall Guy prevention didn't trigger")
    (is (= 1 (count (:discard (get-hero)))) "Card trashed")
    (play-from-hand state :hero "Cache")
    (is (empty? (:prompt (get-hero))) "Housekeeping didn't trigger on 2nd install")))

(deftest invasion-of-privacy
  ;; Invasion of Privacy - Full test
  (do-game
    (new-game (default-minion [(qty "Invasion of Privacy" 3)])
              (default-hero [(qty "Sure Gamble" 2) (qty "Fall Guy" 1) (qty "Cache" 2)]))
    (core/gain state :minion :click 3 :credit 6)
    ;; trash 2 cards
    (play-from-hand state :minion "Invasion of Privacy")
    (prompt-choice :minion 0) ; default trace
    (prompt-choice :hero 0) ; Runner won't match
    (is (= 5 (count (:hand (get-hero)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:minion :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" "Sure Gamble" nil) (prompt-names)))
      (prompt-choice :minion (find-card "Sure Gamble" (:hand (get-hero))))
      (prompt-choice :minion (find-card "Sure Gamble" (:hand (get-hero)))))
    (is (= 3 (count (:hand (get-hero)))))
    ;; able to trash 2 cards but only 1 available target in Runner's hand
    (play-from-hand state :minion "Invasion of Privacy")
    (prompt-choice :minion 0) ; default trace
    (prompt-choice :hero 0) ; Runner won't match
    (is (= 3 (count (:hand (get-hero)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:minion :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" nil) (prompt-names)))
      (prompt-choice :minion (find-card "Fall Guy" (:hand (get-hero))))
      (is (empty? (get-in @state [:minion :prompt])) "No prompt for second card"))
    (is (= 2 (count (:hand (get-hero)))))
    ;; failed trace - take the bad publicity
    (play-from-hand state :minion "Invasion of Privacy")
    (prompt-choice :minion 0) ; default trace
    (prompt-choice :hero 2) ; Runner matches
    (is (= 1 (:bad-publicity (get-minion))))))

(deftest ipo-terminal
  ;; IPO - credits with Terminal operations
  (do-game
    (new-game
      (default-minion [(qty "IPO" 1)])
      (default-hero))
    (take-credits state :minion)
    (take-credits state :hero)
    (play-from-hand state :minion "IPO")
	(is (= 13 (:credit (get-minion))))
	(is (= 0 (:click (get-minion))) "Terminal ends turns")))

(deftest lag-time
  (do-game
    (new-game (default-minion [(qty "Lag Time" 1) (qty "Vanilla" 1) (qty "Lotus Field" 1)])
              (default-hero))
    (take-credits state :minion)
    (take-credits state :hero)
    (play-from-hand state :minion "Vanilla" "HQ")
    (play-from-hand state :minion "Lotus Field" "R&D")
    (play-from-hand state :minion "Lag Time")
    (core/rez state :minion (get-ice state :hq 0))
    (core/rez state :minion (get-ice state :rd 0))
    (is (= 1 (:current-strength (get-ice state :hq 0))) "Vanilla at 1 strength")
	(is (= 5 (:current-strength (get-ice state :rd 0))) "Lotus Field at 5 strength")))

(deftest lateral-growth
  (do-game
    (new-game (default-minion [(qty "Lateral Growth" 1) (qty "Breaking News" 1)])
              (default-hero))
    (is (= 5 (:credit (get-minion))))
    (play-from-hand state :minion "Lateral Growth")
    (prompt-select :minion (find-card "Breaking News" (:hand (get-minion))))
    (prompt-choice :minion "New remote")
    (is (= "Breaking News" (:title (get-content state :remote1 0)))
      "Breaking News installed by Lateral Growth")
    (is (= 7 (:credit (get-minion))))))

(deftest mass-commercialization
  ;; Mass Commercialization
  (do-game
    (new-game (default-minion [(qty "Mass Commercialization" 1)
                             (qty "Ice Wall" 3)])
              (default-hero))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (play-from-hand state :minion "Ice Wall" "R&D")
    (play-from-hand state :minion "Ice Wall" "Archives")
    (take-credits state :hero)
    (core/advance state :minion {:card (refresh (get-ice state :hq 0))})
    (core/advance state :minion {:card (refresh (get-ice state :archives 0))})
    (core/advance state :minion {:card (refresh (get-ice state :rd 0))})
    (take-credits state :hero)
    (play-from-hand state :minion "Mass Commercialization")
    (is (= 8 (:credit (get-minion))) "Gained 6 for 3 advanced ice from Mass Commercialization")))

(deftest manhunt-every-run
  ;; Manhunt - only fires once per turn. Unreported issue.
  (do-game
    (new-game (default-minion [(qty "Manhunt" 1) (qty "Hedge Fund" 3)])
              (default-hero))
    (play-from-hand state :minion "Manhunt")
    (take-credits state :minion)
    (run-empty-server state "HQ")
    (is (:prompt (get-minion)) "Manhunt trace initiated")
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (is (= 1 (:tag (get-hero))) "Runner took 1 tag")
    (prompt-choice :hero "OK")
    (is (not (:run @state)) "Run ended")
    (run-empty-server state "HQ")
    (is (empty? (:prompt (get-minion))) "No Manhunt trace on second run")
    (prompt-choice :hero "OK")
    (is (not (:run @state)) "Run ended")))

(deftest midseason-replacements
  ;; Midseason Replacements - Trace to give Runner tags after they steal an agenda
  (do-game
    (new-game (default-minion [(qty "Midseason Replacements" 1) (qty "Breaking News" 1)])
              (default-hero))
    (play-from-hand state :minion "Midseason Replacements")
    (is (= 3 (:click (get-minion))) "Midseason precondition not met; Corp not charged a click")
    (play-from-hand state :minion "Breaking News" "New remote")
    (take-credits state :minion)
    (is (= 7 (:credit (get-minion))))
    (let [bn (get-content state :remote1 0)]
      (run-empty-server state "Server 1")
      (prompt-choice :hero "Steal")
      (is (= 1 (:agenda-point (get-hero))) "Stole Breaking News")
      (take-credits state :hero)
      (play-from-hand state :minion "Midseason Replacements")
      (prompt-choice :minion 0) ; default trace
      (prompt-choice :hero 0) ; Runner won't match
      (is (= 6 (:tag (get-hero))) "Runner took 6 tags"))))

(deftest mushin-no-shin
  ;; Mushin No Shin - Add 3 advancements to a card; prevent rez/score of that card the rest of the turn
  (do-game
    (new-game (default-minion [(qty "Mushin No Shin" 2) (qty "Ronin" 1) (qty "Profiteering" 1)])
              (default-hero))
    (play-from-hand state :minion "Mushin No Shin")
    (prompt-select :minion (find-card "Ronin" (:hand (get-minion))))
    (let [ronin (get-content state :remote1 0)]
      (is (= 3 (:advance-counter (refresh ronin))) "3 advancements placed on Ronin")
      (core/rez state :minion (refresh ronin))
      (is (not (get-in (refresh ronin) [:rezzed])) "Ronin did not rez")
      (take-credits state :minion)
      (take-credits state :hero)
      (core/rez state :minion (refresh ronin))
      (is (get-in (refresh ronin) [:rezzed]) "Ronin now rezzed")
      (play-from-hand state :minion "Mushin No Shin")
      (prompt-select :minion (find-card "Profiteering" (:hand (get-minion))))
      (let [prof (get-content state :remote2 0)]
        (core/score state :minion (refresh prof))
        (is (empty? (:scored (get-minion))) "Profiteering not scored")
        (is (= 0 (:agenda-point (get-minion))))
        (take-credits state :minion)
        (take-credits state :hero)
        (core/score state :minion (refresh prof))
        (prompt-choice :minion "0")
        (is (= 1 (:agenda-point (get-minion))) "Profiteering was able to be scored")))))

(deftest neural-emp
  ;; Neural EMP - Play if Runner made a run the previous turn to do 1 net damage
  (do-game
    (new-game (default-minion [(qty "Neural EMP" 1)])
              (default-hero))
    (play-from-hand state :minion "Neural EMP")
    (is (= 3 (:click (get-minion))) "Neural precondition not met; card not played")
    (take-credits state :minion)
    (run-empty-server state "Archives")
    (take-credits state :hero)
    (play-from-hand state :minion "Neural EMP")
    (is (= 1 (count (:discard (get-hero)))) "Runner took 1 net damage")))

(deftest oversight-ai
  ;; Oversight AI - Rez a piece of ICE ignoring all costs
  (do-game
    (new-game (default-minion [(qty "Oversight AI" 1) (qty "Archer" 1)])
              (default-hero))
    (play-from-hand state :minion "Archer" "R&D")
    (let [archer (get-ice state :rd 0)]
      (play-from-hand state :minion "Oversight AI")
      (prompt-select :minion archer)
      (is (get-in (refresh archer) [:rezzed]))
      (is (= 4 (:credit (get-minion))) "Archer rezzed at no credit cost")
      (is (= "Oversight AI" (:title (first (:hosted (refresh archer)))))
          "Archer hosting OAI as a condition"))))

(deftest patch
  ;; Patch - +2 current strength
  (do-game
    (new-game (default-minion [(qty "Patch" 1) (qty "Vanilla" 1)])
              (default-hero))
    (play-from-hand state :minion "Vanilla" "HQ")
    (core/rez state :minion (get-ice state :hq 0))
    (play-from-hand state :minion "Patch")
    (prompt-select :minion (get-ice state :hq 0))
    (is (= 2 (:current-strength (get-ice state :hq 0))) "Vanilla at 2 strength")))

(deftest paywall-implementation
  ;; Paywall Implementation - Gain 1 credit for every successful run
  (do-game
    (new-game (default-minion [(qty "Paywall Implementation" 1)])
              (default-hero))
    (play-from-hand state :minion "Paywall Implementation")
    (is (= "Paywall Implementation" (:title (first (get-in @state [:minion :current]))))
        "Paywall active in Current area")
    (take-credits state :minion)
    (is (= 7 (:credit (get-minion))))
    (run-empty-server state "Archives")
    (is (= 8 (:credit (get-minion))) "Gained 1 credit from successful run")
    (run-empty-server state "Archives")
    (is (= 9 (:credit (get-minion))) "Gained 1 credit from successful run")))

(deftest peak-efficiency
  ;; Peak Efficiency - Gain 1 credit for each rezzed ICE
  (do-game
    (new-game (default-minion [(qty "Peak Efficiency" 1) (qty "Paper Wall" 3) (qty "Wraparound" 1)])
              (default-hero))
    (core/gain state :minion :click 3)
    (play-from-hand state :minion "Paper Wall" "HQ")
    (play-from-hand state :minion "Paper Wall" "R&D")
    (play-from-hand state :minion "Paper Wall" "New remote")
    (play-from-hand state :minion "Wraparound" "New remote")
    (core/rez state :minion (get-ice state :hq 0))
    (core/rez state :minion (get-ice state :rd 0))
    (core/rez state :minion (get-ice state :remote1 0))
    (play-from-hand state :minion "Peak Efficiency")
    (is (= 7 (:credit (get-minion))) "Gained 3 credits for 3 rezzed ICE; unrezzed ICE ignored")))

(deftest power-shutdown
  ;; Power Shutdown - Trash cards from R&D to force Runner to trash a program or hardware
  (do-game
    (new-game (default-minion [(qty "Power Shutdown" 3) (qty "Hive" 3)])
              (default-hero [(qty "Grimoire" 1) (qty "Cache" 1)]))
    (play-from-hand state :minion "Power Shutdown")
    (is (empty? (:discard (get-minion))) "Not played, no run last turn")
    (take-credits state :minion)
    (play-from-hand state :hero "Cache")
    (play-from-hand state :hero "Grimoire")
    (run-empty-server state :archives)
    (take-credits state :hero)
    (core/move state :minion (find-card "Hive" (:hand (get-minion))) :deck)
    (core/move state :minion (find-card "Hive" (:hand (get-minion))) :deck)
    (core/move state :minion (find-card "Hive" (:hand (get-minion))) :deck)
    (play-from-hand state :minion "Power Shutdown")
    (prompt-choice :minion 2)
    (is (= 3 (count (:discard (get-minion)))) "2 cards trashed from R&D")
    (is (= 1 (count (:deck (get-minion)))) "1 card remaining in R&D")
    (prompt-select :hero (get-in @state [:hero :rig :hardware 0])) ; try targeting Grimoire
    (is (empty? (:discard (get-hero))) "Grimoire too expensive to be targeted")
    (prompt-select :hero (get-in @state [:hero :rig :program 0]))
    (is (= 1 (count (:discard (get-hero)))) "Cache trashed")))

(deftest precognition
  ;; Precognition - Full test
  (do-game
    (new-game (default-minion [(qty "Precognition" 1) (qty "Caprice Nisei" 1) (qty "Adonis Campaign" 1)
                             (qty "Quandary" 1) (qty "Jackson Howard" 1) (qty "Global Food Initiative" 1)])
              (default-hero))
    (starting-hand state :minion ["Precognition"])
    (play-from-hand state :minion "Precognition")
    (prompt-choice :minion (find-card "Caprice Nisei" (:deck (get-minion))))
    (prompt-choice :minion (find-card "Adonis Campaign" (:deck (get-minion))))
    (prompt-choice :minion (find-card "Quandary" (:deck (get-minion))))
    (prompt-choice :minion (find-card "Jackson Howard" (:deck (get-minion))))
    (prompt-choice :minion (find-card "Global Food Initiative" (:deck (get-minion))))
    ;; try starting over
    (prompt-choice :minion "Start over")
    (prompt-choice :minion (find-card "Global Food Initiative" (:deck (get-minion))))
    (prompt-choice :minion (find-card "Jackson Howard" (:deck (get-minion))))
    (prompt-choice :minion (find-card "Quandary" (:deck (get-minion))))
    (prompt-choice :minion (find-card "Adonis Campaign" (:deck (get-minion))))
    (prompt-choice :minion (find-card "Caprice Nisei" (:deck (get-minion)))) ;this is the top card of R&D
    (prompt-choice :minion "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-minion))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-minion))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-minion)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-minion))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-minion)))))))))))

(deftest preemptive-action
  ;; Preemptive Action - Shuffles cards into R&D and removes itself from game
  (do-game
    (new-game (default-minion [(qty "Subliminal Messaging" 3)
                             (qty "Preemptive Action" 1)])
              (default-hero))
    (play-from-hand state :minion "Subliminal Messaging")
    (play-from-hand state :minion "Subliminal Messaging")
    (play-from-hand state :minion "Subliminal Messaging")
    (play-from-hand state :minion "Preemptive Action")
    (prompt-select :minion (first (:discard (get-minion))))
    (prompt-select :minion (second (:discard (get-minion))))
    (prompt-select :minion (last (:discard (get-minion))))
    (is (= 0 (count (:discard (get-minion)))))
    (is (= 1 (count (:rfg (get-minion)))))))

(deftest psychographics
  ;; Psychographics - Place advancements up to the number of Runner tags on a card
  (do-game
    (new-game (default-minion [(qty "Psychographics" 1) (qty "Project Junebug" 1)])
              (default-hero))
    (core/gain state :hero :tag 4)
    (play-from-hand state :minion "Project Junebug" "New remote")
    (let [pj (get-content state :remote1 0)]
      (play-from-hand state :minion "Psychographics")
      (prompt-choice :minion 4)
      (prompt-select :minion pj)
      (is (= 1 (:credit (get-minion))) "Spent 4 credits")
      (is (= 4 (:advance-counter (refresh pj))) "Junebug has 4 advancements"))))

(deftest psychokinesis
  ;; Pyschokinesis - Terminal Event (end the turn); Look at R&D, install an Asset, Agenda, or Upgrade in a Remote Server
  (do-game
    (new-game (default-minion [(qty "Psychokinesis" 3) (qty "Caprice Nisei" 1) (qty "Adonis Campaign" 1)
                              (qty "Global Food Initiative" 1)])
              (default-hero))
    (starting-hand state :minion ["Psychokinesis","Psychokinesis","Psychokinesis"])
    ;; Test installing an Upgrade
    (play-from-hand state :minion "Psychokinesis")
    (prompt-choice :minion (find-card "Caprice Nisei" (:deck (get-minion))))
    (prompt-choice :minion "New remote")
    (is (= "Caprice Nisei" (:title (get-content state :remote1 0)))
      "Caprice Nisei installed by Psychokinesis")
    ;; Test installing an Asset
    (core/gain state :minion :click 1)
    (play-from-hand state :minion "Psychokinesis")
    (prompt-choice :minion (find-card "Adonis Campaign" (:deck (get-minion))))
    (prompt-choice :minion "New remote")
    (is (= "Adonis Campaign" (:title (get-content state :remote2 0)))
      "Adonis Campaign installed by Psychokinesis")
    ;; Test installing an Agenda
    (core/gain state :minion :click 1)
    (play-from-hand state :minion "Psychokinesis")
    (prompt-choice :minion (find-card "Global Food Initiative" (:deck (get-minion))))
    (prompt-choice :minion "New remote")
    (is (= "Global Food Initiative" (:title (get-content state :remote3 0)))
      "Global Food Initiative installed by Psychokinesis")
    ;; Test selecting "None"
    (core/gain state :minion :click 1)
    (core/move state :minion (find-card "Psychokinesis" (:discard (get-minion))) :hand)
    (play-from-hand state :minion "Psychokinesis")
    (prompt-choice :minion "None")
    (is (= nil (:title (get-content state :remote4 0)))
      "Nothing is installed by Psychokinesis")))

(deftest punitive-counterstrike
  ;; Punitive Counterstrike - deal meat damage equal to printed agenda points
  (do-game
    (new-game (default-minion [(qty "Global Food Initiative" 1) (qty "Punitive Counterstrike" 1)])
              (default-hero))
    (play-from-hand state :minion "Global Food Initiative" "New remote")
    (take-credits state :minion)
    (run-empty-server state :remote1)
    (prompt-choice :hero "Steal")
    (is (= 2 (:agenda-point (get-hero))) "Runner scored 2 points")
    (take-credits state :hero)
    (play-from-hand state :minion "Punitive Counterstrike")
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (is (empty? (:hand (get-hero))) "Runner took 3 meat damage")))

(deftest reuse
  ;; Reuse - Gain 2 credits for each card trashed from HQ
  (do-game
    (new-game (default-minion [(qty "Reuse" 2) (qty "Hive" 1) (qty "IQ" 1)
                             (qty "Ice Wall" 1)])
              (default-hero))
    (play-from-hand state :minion "Reuse")
    (prompt-select :minion (find-card "Ice Wall" (:hand (get-minion))))
    (prompt-select :minion (find-card "Hive" (:hand (get-minion))))
    (prompt-select :minion (find-card "IQ" (:hand (get-minion))))
    (prompt-choice :minion "Done")
    (is (= 4 (count (:discard (get-minion)))) "3 cards trashed plus operation played")
    (is (= 11 (:credit (get-minion))) "Gained 6 credits")
    (is (= 1 (:click (get-minion))) "Spent 2 clicks")))

(deftest salems-hospitality
  ;; Salem's Hospitality - Full test
  (do-game
    (new-game (default-minion [(qty "Salem's Hospitality" 3)])
              (default-hero [(qty "I've Had Worse" 3) (qty "Faust" 1)
                               (qty "Levy AR Lab Access" 1)]))
    (play-from-hand state :minion "Salem's Hospitality")
    (is (= 5 (count (:hand (get-hero)))))
    (prompt-choice :minion "I've Had Worse")
    (is (= 2 (count (:hand (get-hero)))))
    (play-from-hand state :minion "Salem's Hospitality")
    (prompt-choice :minion "Plascrete Carapace")
    (is (= 2 (count (:hand (get-hero)))))))

(deftest scorched-earth
  ;; Scorched Earth - burn 'em
  (do-game
    (new-game (default-minion [(qty "Scorched Earth" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
    (core/gain state :hero :tag 1)
    (play-from-hand state :minion "Scorched Earth")
    (is (= 1 (count (:hand (get-hero)))) "Runner has 1 card in hand")))

(deftest scorched-earth-no-tag
  ;; Scorched Earth - not tagged
  (do-game
    (new-game (default-minion [(qty "Scorched Earth" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
    (play-from-hand state :minion "Scorched Earth")
    (is (= 3 (:click (get-minion))) "Corp not charged a click")
    (is (= 5 (count (:hand (get-hero)))) "Runner did not take damage")))

(deftest scorched-earth-flatline
  ;; Scorched Earth - murderize 'em
  (do-game
    (new-game (default-minion [(qty "Scorched Earth" 10)])
              (default-hero))
    (core/gain state :hero :tag 1)
    (play-from-hand state :minion "Scorched Earth")
    (is (= 0 (count (:hand (get-hero)))) "Runner has 0 cards in hand")
    (is (= :minion (:winner @state)) "Corp wins")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest subcontract-scorched
  ;; Subcontract - Don't allow second operation until damage prevention completes
  (do-game
    (new-game (default-minion [(qty "Scorched Earth" 2) (qty "Subcontract" 1)])
              (default-hero [(qty "Plascrete Carapace" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :tag 1)
    (play-from-hand state :hero "Plascrete Carapace")
    (take-credits state :hero)
    (play-from-hand state :minion "Subcontract")
    (prompt-select :minion (find-card "Scorched Earth" (:hand (get-minion))))
    (is (and (= 1 (count (:prompt (get-minion)))) (= :waiting (-> (get-minion) :prompt first :prompt-type)))
        "Corp does not have Subcontract prompt until damage prevention completes")
    (prompt-choice :hero "Done")
    (is (not-empty (:prompt (get-minion))) "Corp can now play second Subcontract operation")))

(deftest subcontract-terminal
  ;; Subcontract - interaction with Terminal operations
  (do-game
    (new-game
      (default-minion [(qty "Hard-Hitting News" 2) (qty "Subcontract" 1)])
      (default-hero))
    (core/gain state :hero :tag 1)
    (take-credits state :minion)
    (run-empty-server state :archives)
    (take-credits state :hero)
    (play-from-hand state :minion "Subcontract")
    (prompt-select :minion (find-card "Hard-Hitting News" (:hand (get-minion))))
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (is (= 5 (:tag (get-hero))) "Runner has 5 tags")
    (is (empty? (:prompt (get-minion))) "Corp does not have a second Subcontract selection prompt")))

(deftest self-growth-program
  ;; Self-Growth Program - Add 2 installed cards to grip if hero is tagged
  (do-game
    (new-game (default-minion [(qty "Self-Growth Program" 1)])
              (default-hero [(qty "Clone Chip" 1) (qty "Inti" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Clone Chip")
    (play-from-hand state :hero "Inti")
    (take-credits state :hero)
    (play-from-hand state :minion "Self-Growth Program")
    (is (= 3 (:click (get-minion))) "Self-Growth Program precondition not met; card not played")
    (core/gain state :hero :tag 1)
    (is (= 0 (count (:hand (get-hero)))) "Runner hand is empty")
    (let [inti (get-in @state [:hero :rig :program 0])
          cc (get-in @state [:hero :rig :hardware 0])]
      (play-from-hand state :minion "Self-Growth Program")
      (prompt-select :minion inti)
      (prompt-select :minion cc))
    (is (= 2 (count (:hand (get-hero)))) "2 cards returned to hand")
    (is (= 0 (count (get-in @state [:hero :rig :program]))) "No programs installed")
    (is (= 0 (count (get-in @state [:hero :rig :hardware]))) "No hardware installed")))

(deftest service-outage
  ;; Service Outage - First click run each turn costs a credit
  (do-game
    (new-game (default-minion [(qty "Service Outage" 1)])
              (default-hero [(qty "Employee Strike" 1)]))
    (play-from-hand state :minion "Service Outage")
    (take-credits state :minion)

    (is (= 5 (:credit (get-hero))) "Runner has 5 credits")
    (run-on state :archives)
    (is (= 4 (:credit (get-hero)))
        "Runner spends 1 credit to make the first run")
    (run-successful state)

    (run-on state :archives)
    (is (= 4 (:credit (get-hero)))
        "Runner doesn't spend 1 credit to make the second run")
    (run-successful state)

    (take-credits state :hero)
    (take-credits state :minion)

    (core/lose state :hero :credit 6)
    (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
    (is (= 0 (:credit (get-hero))) "Runner has 0 credits")
    (run-on state :archives)
    (is (not (:run @state)) "No run was initiated")
    (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
    (is (= 0 (:credit (get-hero))) "Runner has 0 credits")

    (take-credits state :hero)
    (take-credits state :minion)

    (core/lose state :hero :credit 2)
    (play-from-hand state :hero "Employee Strike")
    (is (= 1 (:credit (get-hero))) "Runner has 1 credit")

    (run-on state :archives)
    (is (= 1 (:credit (get-hero)))
        "Runner doesn't spend 1 credit to make a run")))

(deftest service-outage-card-ability
  ;; Service Outage - First card ability run each turn costs an additional credit
  (do-game
    (new-game (default-minion [(qty "Service Outage" 1)])
              (default-hero [(qty "Sneakdoor Beta" 1)]))
    (play-from-hand state :minion "Service Outage")
    (take-credits state :minion)
    (play-from-hand state :hero "Sneakdoor Beta")
    (take-credits state :hero 1)

    (is (= 2 (:credit (get-hero))) "Runner has 2 credits")
    (let [sneakdoor (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero sneakdoor 0)
      (is (= 1 (:credit (get-hero)))
          "Runner spends 1 additional credit to run with a card ability")
      (run-successful state)

      (run-on state :archives)
      (is (= 1 (:credit (get-hero)))
          "Runner doesn't spend 1 credit to make a run")
      (run-successful state)

      (take-credits state :hero)
      (take-credits state :minion)

      (core/lose state :hero :credit 1)
      (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
      (is (= 0 (:credit (get-hero))) "Runner has 0 credits")
      (card-ability state :hero sneakdoor 0)
      (is (not (:run @state)) "No run was initiated")
      (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
      (is (= 0 (:credit (get-hero))) "Runner has 0 credits"))))

(deftest service-outage-run-events
  ;; Service Outage - First run event each turn costs an additional credit
  (do-game
    (new-game (default-minion [(qty "Service Outage" 1)])
              (default-hero [(qty "Out of the Ashes" 2)]))
    (play-from-hand state :minion "Service Outage")
    (take-credits state :minion)

    (is (= 5 (:credit (get-hero))) "Runner has 5 credits")
    (play-from-hand state :hero "Out of the Ashes")
    (is (= 3 (:credit (get-hero)))
        "Runner spends 1 additional credit to run with a run event")
    (prompt-choice :hero "Archives")
    (run-successful state)

    (run-on state :archives)
    (is (= 3 (:credit (get-hero)))
        "Runner doesn't spend 1 credit to make a run")
    (run-successful state)

    (take-credits state :hero)
    (take-credits state :minion)
    (prompt-choice :hero "No") ; Out of the Ashes prompt

    (core/lose state :hero :credit 4)
    (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
    (is (= 1 (:credit (get-hero))) "Runner has 1 credit")
    (play-from-hand state :hero "Out of the Ashes")
    (is (empty? (get-in @state [:hero :prompt]))
        "Out of the Ashes was not played")
    (is (= 4 (:click (get-hero))) "Runner has 4 clicks")
    (is (= 1 (:credit (get-hero))) "Runner has 1 credit")))

(deftest service-outage-hero-turn-first-run
  ;; Service Outage - Works when played on the hero's turn
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News" [(qty "Service Outage" 1)
                                                       (qty "Breaking News" 1)])
              (default-hero [(qty "Hades Shard" 1)]))
    (trash-from-hand state :minion "Breaking News")
    (take-credits state :minion)

    (core/gain state :hero :credit 3)
    (play-from-hand state :hero "Hades Shard")
    (card-ability state :hero (get-in @state [:hero :rig :resource 0]) 0)
    (prompt-choice :hero "Steal")
    (prompt-choice :minion "Yes")
    (prompt-select :minion (find-card "Service Outage" (:hand (get-minion))))
    (is (find-card "Service Outage" (:current (get-minion)))
        "Service Outage is in play")

    (is (= 1 (:credit (get-hero))) "Runner has 1 credit")
    (run-on state :archives)
    (is (= 0 (:credit (get-hero)))
        "Runner spends 1 additional credit to make a run")))

(deftest service-outage-hero-turn-second-run
  ;; Service Outage - Doesn't fire if already run when played on the hero's turn
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News" [(qty "Service Outage" 1)
                                                       (qty "Breaking News" 1)])
              (default-hero [(qty "Hades Shard" 1)]))
    (trash-from-hand state :minion "Breaking News")
    (take-credits state :minion)

    (run-on state :hq)
    (run-successful state)
    (prompt-choice :hero "OK")

    (core/gain state :hero :credit 3)
    (play-from-hand state :hero "Hades Shard")
    (card-ability state :hero (get-in @state [:hero :rig :resource 0]) 0)
    (prompt-choice :hero "Steal")
    (prompt-choice :minion "Yes")
    (prompt-select :minion (find-card "Service Outage" (:hand (get-minion))))
    (is (find-card "Service Outage" (:current (get-minion)))
        "Service Outage is in play")

    (is (= 1 (:credit (get-hero))) "Runner has 1 credit")
    (run-on state :archives)
    (is (= 1 (:credit (get-hero)))
        "Runner doesn't spend 1 additional credit to make a run")))

(deftest service-outage-new-angeles-sol
  ;; Service Outage trashed and reinstalled on steal doesn't double remove penalty
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Service Outage" 1)
                                               (qty "Breaking News" 1)])
      (default-hero))
    (play-from-hand state :minion "Breaking News" "New remote")
    (play-from-hand state :minion "Service Outage")
    (take-credits state :minion)

    (run-on state :remote1)
    (run-successful state)
    (prompt-choice :hero "Steal")

    (prompt-choice :minion "Yes")
    (prompt-select :minion (find-card "Service Outage"
                                    (:discard (get-minion))))

    (take-credits state :hero)

    (take-credits state :minion)

    (is (= 7 (:credit (get-hero))) "Runner has 7 credits")
    (run-on state :archives)
    (is (= 6 (:credit (get-hero)))
        "Runner spends 1 credit to make a run")))

(deftest shipment-from-sansan
  ;; Shipment from SanSan - placing advancements
  (do-game
    (new-game (default-minion [(qty "Shipment from SanSan" 3) (qty "Ice Wall" 3)])
              (default-hero))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (let [iwall (get-ice state :hq 0)]
      (play-from-hand state :minion "Shipment from SanSan")
      (prompt-choice :minion "2")
      (prompt-select :minion iwall)
      (is (= 5 (:credit (get-minion))))
      (is (= 2 (:advance-counter (refresh iwall)))))))

(deftest stock-buy-back
  ;; Stock Buy-Back - Gain 3c for every agenda in Runner's area
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 2) (qty "Stock Buy-Back" 3)])
              (default-hero))
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (take-credits state :minion)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Steal")
    (run-empty-server state "Server 2")
    (prompt-choice :hero "Steal")
    (take-credits state :hero)
    (is (= 2 (count (:scored (get-hero)))))
    (play-from-hand state :minion "Stock Buy-Back")
    (is (= 11 (:credit (get-minion))))))

(deftest sub-boost
  ;; Sub Boost - Give ICE Barrier
  (do-game
    (new-game (default-minion [(qty "Sub Boost" 1) (qty "Quandary" 1)])
              (default-hero))
    (play-from-hand state :minion "Quandary" "HQ")
    (play-from-hand state :minion "Sub Boost")
    (let [qu (get-ice state :hq 0)]
      (core/rez state :minion qu)
      (prompt-select :minion qu)
      (is (core/has-subtype? (refresh qu) "Code Gate") "Quandary has Code Gate")
      (is (core/has-subtype? (refresh qu) "Barrier") "Quandary ICE Barrier"))))

(deftest subliminal-messaging
  ;; Subliminal Messaging - Playing/trashing/milling will all prompt returning to hand
  (do-game
    (new-game (default-minion [(qty "Subliminal Messaging" 3)])
              (make-deck "Noise: Hacker Extraordinaire" [(qty "Cache" 3) (qty "Utopia Shard" 1)]))
    (play-from-hand state :minion "Subliminal Messaging")
    (is (= 6 (:credit (get-minion))))
    (is (= 3 (:click (get-minion))) "First Subliminal Messaging gains 1 click")
    (play-from-hand state :minion "Subliminal Messaging")
    (is (= 7 (:credit (get-minion))))
    (is (= 2 (:click (get-minion))) "Second Subliminal Messaging does not gain 1 click")
    (trash-from-hand state :minion "Subliminal Messaging")
    (is (= 0 (count (:hand (get-minion)))))
    (take-credits state :minion)
    (take-credits state :hero)
    (prompt-choice :minion "Yes")
    (prompt-choice :minion "Yes")
    (prompt-choice :minion "Yes")
    (is (= 3 (count (:hand (get-minion)))) "All 3 Subliminals returned to HQ")
    (core/move state :minion (find-card "Subliminal Messaging" (:hand (get-minion))) :deck)
    (take-credits state :minion)
    (play-from-hand state :hero "Cache")
    (play-from-hand state :hero "Utopia Shard")
    (let [utopia (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero utopia 0))
    (take-credits state :hero)
    (prompt-choice :minion "Yes")
    (prompt-choice :minion "Yes")
    (prompt-choice :minion "Yes")
    (is (= 3 (count (:hand (get-minion)))) "All 3 Subliminals returned to HQ")
    (play-from-hand state :minion "Subliminal Messaging")
    (take-credits state :minion)
    (run-on state "R&D")
    (run-jack-out state)
    (take-credits state :hero)
    (is (empty? (get-in @state [:minion :prompt])) "No prompt here because hero made a run last turn")
    (take-credits state :minion)
    (is (= 2 (count (:hand (get-minion)))))
    (is (= 1 (count (:discard (get-minion)))) "1 Subliminal not returned because hero made a run last turn")))

(deftest subliminal-messaging-archived
  ;; Subliminal Messaging - Scenario involving Subliminal being added to HQ with Archived Memories
  (do-game
    (new-game (default-minion [(qty "Subliminal Messaging" 2) (qty "Archived Memories" 1)])
              (default-hero))
    (play-from-hand state :minion "Subliminal Messaging")
    (play-from-hand state :minion "Subliminal Messaging")
    (play-from-hand state :minion "Archived Memories")
    (prompt-select :minion (find-card "Subliminal Messaging" (:discard (get-minion))))
    (is (= 2 (count (:discard (get-minion)))))
    (is (= 1 (count (:hand (get-minion)))))
    (take-credits state :minion)
    (take-credits state :hero)
    (prompt-choice :minion "No")
    (is (empty? (get-in @state [:minion :prompt])) "Only 1 Subliminal prompt")
    (play-from-hand state :minion "Subliminal Messaging")
    (take-credits state :minion)
    (take-credits state :hero)
    (prompt-choice :minion "Yes")
    (prompt-choice :minion "Yes")
    (is (empty? (get-in @state [:minion :prompt]))
        "Only 2 Subliminal prompts - there will be a third if flag not cleared")))

(deftest subliminal-messaging-jackson
  ;; Subliminal Messaging - Scenario involving Subliminal being reshuffled into R&D with Jackson
  (do-game
    (new-game (default-minion [(qty "Subliminal Messaging" 1) (qty "Jackson Howard" 1)])
              (default-hero))
    (play-from-hand state :minion "Subliminal Messaging")
    (play-from-hand state :minion "Jackson Howard" "New remote")
    (take-credits state :minion)
    (let [jhow (get-content state :remote1 0)]
      (core/rez state :minion jhow)
      (card-ability state :minion jhow 1)
      (prompt-select :minion (find-card "Subliminal Messaging" (:discard (get-minion))))
      (prompt-choice :minion "Done")
      (is (= 0 (count (:discard (get-minion)))))
      (is (= 1 (count (:rfg (get-minion))))))
    (take-credits state :hero)
    (play-from-hand state :minion "Subliminal Messaging")
    (take-credits state :minion)
    (take-credits state :hero)
    (prompt-choice :minion "Yes")
    (is (= 1 (count (:hand (get-minion)))) "Subliminal returned to HQ")
    (is (empty? (get-in @state [:minion :prompt]))
        "Subliminal prompt cleared - there will be a second prompt if flag not cleared")))

(deftest subliminal-messaging-made-run
  ;; Subliminal Messaging - Runner made run, ensure game asks again next turn
  (do-game
    (new-game (default-minion [(qty "Subliminal Messaging" 2)])
              (default-hero))
  (play-from-hand state :minion "Subliminal Messaging")
  (trash-from-hand state :minion "Subliminal Messaging")
  (take-credits state :minion)
  (run-on state "R&D")
  (run-jack-out state)
  (take-credits state :hero)
  (is (empty? (get-in @state [:minion :prompt])) "No prompt here because hero made a run last turn")
  (take-credits state :minion)
  (take-credits state :hero)
  (prompt-choice :minion "Yes")
  (prompt-choice :minion "Yes")
  (is (= 2 (count (:hand (get-minion)))) "Both Subliminals returned to HQ")
  (is (= 0 (count (:discard (get-minion)))) "No Subliminals in Archives")))

(deftest subliminal-messaging-no
  ;; Subliminal Messaging - User declines to return to hand, ensure game asks again next turn
  (do-game
    (new-game (default-minion [(qty "Subliminal Messaging" 2)])
              (default-hero))
    (play-from-hand state :minion "Subliminal Messaging")
    (trash-from-hand state :minion "Subliminal Messaging")
    (take-credits state :minion)
    (take-credits state :hero)
    (prompt-choice :minion "No")
    (prompt-choice :minion "No")
    (is (= 0 (count (:hand (get-minion)))) "Neither Subliminal returned to HQ")
    (is (= 2 (count (:discard (get-minion)))) "Both Subliminals in Archives")
    (take-credits state :minion)
    (take-credits state :hero)
    (prompt-choice :minion "Yes")
    (prompt-choice :minion "Yes")
    (is (= 2 (count (:hand (get-minion)))) "Both Subliminals returned to HQ")
    (is (= 0 (count (:discard (get-minion)))) "No Subliminals in Archives")))

(deftest success-bad-publicity
  ;; Success - Works with bad publicity
  (do-game
    (new-game (default-minion [(qty "NAPD Contract" 1) (qty "Project Beale" 1) (qty "Success" 1)])
              (default-hero))
    (play-from-hand state :minion "NAPD Contract" "New remote")
    (play-from-hand state :minion "Project Beale" "New remote")
    (core/gain state :minion :bad-publicity 9)
    (core/gain state :minion :credit 8)
    (core/gain state :minion :click 15)
    (let [napd (get-content state :remote1 0)
          beale (get-content state :remote2 0)]
      (dotimes [_ 13] (core/advance state :minion {:card (refresh napd)}))
      (is (= 13 (:advance-counter (refresh napd))))
      (core/score state :minion {:card (refresh napd)})
      (is (= 2 (:agenda-point (get-minion))))
      (play-from-hand state :minion "Success")
      (prompt-select :minion (get-scored state :minion 0))
      (is (= "NAPD Contract" (:title (first (:rfg (get-minion))))))
      (prompt-select :minion (refresh beale))
      (is (= 13 (:advance-counter (refresh beale))))
      (core/score state :minion {:card (refresh beale)})
      (is (= 7 (:agenda-point (get-minion)))))))

(deftest success-public-agenda
  ;; Success - Works with public agendas
  (do-game
    (new-game (default-minion [(qty "Oaktown Renovation" 1) (qty "Vanity Project" 1) (qty "Success" 1)])
              (default-hero))
    (core/gain state :minion :click 1)
    (score-agenda state :minion (find-card "Vanity Project" (:hand (get-minion))))
    (is (= 4 (:agenda-point (get-minion))))
    (play-from-hand state :minion "Oaktown Renovation" "New remote")
    (is (= 5 (:credit (get-minion))))
    (play-from-hand state :minion "Success")
    (prompt-select :minion (get-scored state :minion))
    (is (= "Vanity Project" (:title (first (:rfg (get-minion))))))
    (let [oaktown (get-content state :remote1 0)]
      (prompt-select :minion (refresh oaktown))
      (is (= 6 (:advance-counter (refresh oaktown))))
      (is (= 19 (:credit (get-minion))) "Gain 2 + 2 + 2 + 2 + 3 + 3 = 14 credits for advancing Oaktown")
      (core/score state :minion {:card (refresh oaktown)})
      (is (= 2 (:agenda-point (get-minion)))))))

(deftest success-jemison
  ;; Success interaction with Jemison, regression test for issue #2704
  (do-game
    (new-game (make-deck "Jemison Astronautics: Sacrifice. Audacity. Success."
                         [(qty "Success" 1)
                          (qty "High-Risk Investment" 1)
                          (qty "Government Takeover" 1)])
              (default-hero))
    (core/gain state :minion :click 1)
    (score-agenda state :minion (find-card "High-Risk Investment" (:hand (get-minion))))
    (play-from-hand state :minion "Government Takeover" "New remote")
    (play-from-hand state :minion "Success")
    (prompt-select :minion (get-in (get-minion) [:scored 0]))
    (let [gto (get-content state :remote1 0)]
      ;; Prompt for Success
      (prompt-select :minion (refresh gto))
      (is (= 5 (:advance-counter (refresh gto))) "Advance 5 times from Success")
      ;; Prompt for Jemison
      (prompt-select :minion (refresh gto))
      (is (= 9 (:advance-counter (refresh gto))) "Added 4 counters from Jemison trigger"))))

(deftest successful-demonstration
  ;; Successful Demonstration - Play if only Runner made unsuccessful run last turn; gain 7 credits
  (do-game
    (new-game (default-minion [(qty "Successful Demonstration" 1)])
              (default-hero))
    (play-from-hand state :minion "Successful Demonstration")
    (is (and (= 3 (:click (get-minion)))
             (= 5 (:credit (get-hero))))
        "Successful Demonstration precondition not met; card not played")
    (take-credits state :minion)
    (run-on state "R&D")
    (run-jack-out state)
    (take-credits state :hero)
    (play-from-hand state :minion "Successful Demonstration")
    (is (= 13 (:credit (get-minion))) "Paid 2 to play event; gained 7 credits")))

(deftest threat-assessment
  ;; Threat Assessment - play only if hero trashed a card last turn, move a card to the stack or take 2 tags
  (do-game
    (new-game (default-minion [(qty "Threat Assessment" 3) (qty "Adonis Campaign" 1)])
              (default-hero [(qty "Desperado" 1) (qty "Corroder" 1)]))
    (play-from-hand state :minion "Adonis Campaign" "New remote")
    (take-credits state :minion)

    (run-on state :remote1)
    (run-successful state)
    (prompt-choice :hero "Yes") ;trash
    (core/gain state :hero :credit 5)
    (play-from-hand state :hero "Desperado")
    (play-from-hand state :hero "Corroder")
    (take-credits state :hero)

    (is (= 0 (:tag (get-hero))) "Runner starts with 0 tags")
    (play-from-hand state :minion "Threat Assessment")
    (prompt-select :minion (find-card "Desperado" (-> (get-hero) :rig :hardware)))
    (prompt-choice :hero "2 tags")
    (is (= 2 (:tag (get-hero))) "Runner took 2 tags")
    (is (= 1 (count (-> (get-hero) :rig :hardware))) "Didn't trash Desperado")
    (is (= "Threat Assessment" (:title (first (:rfg (get-minion))))) "Threat Assessment removed from game")

    (play-from-hand state :minion "Threat Assessment")
    (prompt-select :minion (find-card "Corroder" (-> (get-hero) :rig :program)))
    (prompt-choice :hero "Move Corroder")
    (is (= 2 (:tag (get-hero))) "Runner didn't take tags")
    (is (= "Corroder" (:title (first (:deck (get-hero))))) "Moved Corroder to the deck")
    (is (= 2 (count (:rfg (get-minion)))))
    (take-credits state :hero)

    (take-credits state :minion)
    (take-credits state :hero)

    (play-from-hand state :minion "Threat Assessment")
    (is (empty? (:prompt (get-minion))) "Threat Assessment triggered with no trash")))

(deftest transparency-initiative
  ;; Transparency Initiative - Full test
  (do-game
    (new-game (default-minion [(qty "Transparency Initiative" 1) (qty "Oaktown Renovation" 1)
                             (qty "Project Atlas" 1) (qty "Hostile Takeover" 1) (qty "Casting Call" 1)])
              (default-hero))
    (core/gain state :minion :click 5)
    (play-from-hand state :minion "Oaktown Renovation" "New remote")
    (play-from-hand state :minion "Casting Call")
    (prompt-select :minion (find-card "Project Atlas" (:hand (get-minion))))
    (prompt-choice :minion "New remote")
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (let [oaktown (get-content state :remote1 0)
          atlas (get-content state :remote2 0)
          hostile (get-content state :remote3 0)]
      (play-from-hand state :minion "Transparency Initiative")
      (prompt-select :minion (refresh oaktown))
      ;; doesn't work on face-up agendas
      (is (= 0 (count (:hosted (refresh oaktown)))))
      (prompt-select :minion (refresh atlas))
      (is (= 1 (count (:hosted (refresh atlas)))) "Casting Call")
      ;; works on facedown agenda
      (prompt-select :minion (refresh hostile))
      (is (= 1 (count (:hosted (refresh hostile)))))
      ;; gains Public subtype
      (is (core/has-subtype? (refresh hostile) "Public"))
      ;; gain 1 credit when advancing
      (is (= 5 (:credit (get-minion))))
      (core/advance state :minion {:card (refresh hostile)})
      (is (= 5 (:credit (get-minion))))
      ;; make sure advancing other agendas doesn't gain 1
      (core/advance state :minion {:card (refresh oaktown)})
      (is (= 6 (:credit (get-minion))) "Transparency initiative didn't fire")
      (core/advance state :minion {:card (refresh atlas)})
      (is (= 5 (:credit (get-minion))) "Transparency initiative didn't fire"))))

(deftest wetwork-refit
  ;; Wetwork Refit - Only works on Bioroid ICE and adds a subroutine
  (do-game
    (new-game (default-minion [(qty "Eli 1.0" 1)
                             (qty "Vanilla" 1)
                             (qty "Wetwork Refit" 3)])
              (default-hero))
    (core/gain state :minion :credit 20)
    (core/gain state :minion :click 10)
    (play-from-hand state :minion "Eli 1.0" "R&D")
    (play-from-hand state :minion "Vanilla" "HQ")
    (let [eli (get-ice state :rd 0)
          vanilla (get-ice state :hq 0)]
      (play-from-hand state :minion "Wetwork Refit")
      (is (not-any? #{"Eli 1.0"} (get-in @state [:minion :prompt :choices]))
          "Unrezzed Eli 1.0 is not a choice to host Wetwork Refit")
      (prompt-choice :minion "Done")

      (take-credits state :minion)
      (take-credits state :hero)
      (core/rez state :minion (refresh eli))
      (core/rez state :minion (refresh vanilla))

      (play-from-hand state :minion "Wetwork Refit")
      (prompt-select :minion (refresh eli))
      (is (= "Wetwork Refit" (:title (first (:hosted (refresh eli)))))
          "Wetwork Refit is hosted on Eli 1.0")
      (is (= 2 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 2 different subroutines")
      (is (= "Do 1 brain damage" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has a brain damage subroutine as his first subroutine")

      (core/move state :minion (first (:hosted (refresh eli))) :hand)
      (is (empty? (:hosted (refresh eli))) "No cards are hosted on Eli 1.0")
      (is (= 1 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 1 different subroutine")
      (is (= "End the run" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has an end the run subroutine as his first subroutine")

      (play-from-hand state :minion "Wetwork Refit")
      (prompt-select :minion (refresh vanilla))
      (is (not= "Wetwork Refit" (:title (first (:hosted (refresh vanilla)))))
          "Wetwork Refit is not hosted on Vanilla"))))
