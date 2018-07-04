(ns test.cards.operations
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest twenty-four-seven-news-cycle-breaking-news
  ;; 24/7 News Cycle - Breaking News interaction
  (do-game
    (new-game (default-contestant [(qty "Breaking News" 2) (qty "24/7 News Cycle" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Breaking News" "New party")
    (play-from-hand state :contestant "Breaking News" "New party")
    (let [ag1 (get-content state :party1 0)
          ag2 (get-content state :party2 0)]
      (score-agenda state :contestant ag1)
      (score-agenda state :contestant ag2)
      (take-credits state :contestant)
      (is (= 0 (:tag (get-challenger)))) ; tags cleared
      (take-credits state :challenger)
      (play-from-hand state :contestant "24/7 News Cycle")
      (prompt-select :contestant (find-card "Breaking News" (:scored (get-contestant))))
      (is (= 1 (:agenda-point (get-contestant))) "Forfeited Breaking News")
      (prompt-select :contestant (find-card "Breaking News" (:scored (get-contestant))))
      (is (= 2 (:tag (get-challenger))) "Challenger given 2 tags")
      (take-credits state :contestant 2)
      (is (= 2 (:tag (get-challenger))) "Tags remained after Contestant ended turn"))))

(deftest twenty-four-seven-news-cycle-posted-bounty
  ;; 24/7 News Cycle and Posted Bounty interaction -- Issue #1043
  (do-game
    (new-game (default-contestant [(qty "Posted Bounty" 2) (qty "24/7 News Cycle" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Posted Bounty" "New party")
    (play-from-hand state :contestant "Posted Bounty" "New party")
    (let [ag1 (get-content state :party1 0)
          ag2 (get-content state :party2 0)]
      (score-agenda state :contestant ag1)
      (prompt-choice :contestant "No")
      (score-agenda state :contestant ag2)
      (prompt-choice :contestant "No")
      (play-from-hand state :contestant "24/7 News Cycle")
      (prompt-select :contestant (find-card "Posted Bounty" (:scored (get-contestant))))
      (is (= 1 (:agenda-point (get-contestant))) "Forfeited Posted Bounty")
      (prompt-select :contestant (find-card "Posted Bounty" (:scored (get-contestant))))
      (prompt-choice :contestant "Yes") ; "Forfeit Posted Bounty to give 1 tag?"
      (is (= 1 (:tag (get-challenger))) "Challenger given 1 tag")
      (is (= 1 (:bad-publicity (get-contestant))) "Contestant has 1 bad publicity")
      (is (= 0 (:agenda-point (get-contestant))) "Forfeited Posted Bounty to 24/7 News Cycle"))))

(deftest twenty-four-seven-news-cycle-swaps
  ;; 24/7 News Cycle - Swapped agendas are able to be used. #1555
  (do-game
    (new-game (default-contestant [(qty "24/7 News Cycle" 1) (qty "Chronos Project" 1)
                             (qty "Philotic Entanglement" 1) (qty "Profiteering" 1)])
              (default-challenger [(qty "Turntable" 3)]))
    (score-agenda state :contestant (find-card "Chronos Project" (:hand (get-contestant))))
    (score-agenda state :contestant (find-card "Philotic Entanglement" (:hand (get-contestant))))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Turntable")
    (core/steal state :challenger (find-card "Profiteering" (:hand (get-contestant))))
    (prompt-choice :challenger "Yes")
    (prompt-select :challenger (find-card "Philotic Entanglement" (:scored (get-contestant))))
    (is (= 2 (:agenda-point (get-contestant))))
    (is (= 2 (:agenda-point (get-challenger))))
    (take-credits state :challenger)
    (play-from-hand state :contestant "24/7 News Cycle")
    (prompt-select :contestant (find-card "Chronos Project" (:scored (get-contestant))))
    (is (= "Chronos Project" (:title (first (:rfg (get-contestant))))))
    ;; shouldn't work on an agenda in the Challenger's scored area
    (is (= 2 (count (:hand (get-challenger)))))
    (prompt-select :contestant (find-card "Philotic Entanglement" (:scored (get-challenger))))
    (is (= 2 (count (:hand (get-challenger)))))
    ;; resolve 'when scored' ability on swapped Profiteering
    (is (= 8 (:credit (get-contestant))))
    (prompt-select :contestant (find-card "Profiteering" (:scored (get-contestant))))
    (prompt-choice :contestant "3")
    (is (= 1 (:agenda-point (get-contestant))))
    (is (= 3 (:bad-publicity (get-contestant))))
    (is (= 23 (:credit (get-contestant))) "Gained 15 credits")))

(deftest accelerated-diagnostics
  ;; Accelerated Diagnostics - Interaction with prompt effects, like Shipment from SanSan
  (do-game
    (new-game (default-contestant [(qty "Accelerated Diagnostics" 1) (qty "Cerebral Overwriter" 1) (qty "Shipment from SanSan" 1)
                             (qty "Hedge Fund" 1) (qty "Back Channels" 1)])
              (default-challenger))
    (starting-hand state :contestant ["Accelerated Diagnostics" "Cerebral Overwriter"])
    (play-from-hand state :contestant "Cerebral Overwriter" "New party")
    (core/gain state :contestant :credit 1)
    (play-from-hand state :contestant "Accelerated Diagnostics")

    (let [playarea (get-in @state [:contestant :play-area])
          hf (find-card "Hedge Fund" playarea)
          ss (find-card "Shipment from SanSan" playarea)
          bc (find-card "Back Channels" playarea)
          co (get-content state :party1 0)]
      (is (= 3 (count playarea)) "3 cards in play area")
      (prompt-select :contestant ss)
      (prompt-choice :contestant "2")
      (prompt-select :contestant co)
      (is (= 2 (:advance-counter (refresh co))) "Cerebral Overwriter gained 2 advancements")
      (prompt-select :contestant hf)
      (is (= 9 (:credit (get-contestant))) "Contestant gained credits from Hedge Fund")
      (prompt-select :contestant bc)
      (prompt-select :contestant (refresh co))
      (is (= 15 (:credit (get-contestant))) "Contestant gained 6 credits for Back Channels"))))

(deftest accelerated-diagnostics-with-current
  ;; Accelerated Diagnostics - Interaction with Current
  (do-game
    (new-game (default-contestant [(qty "Accelerated Diagnostics" 1) (qty "Cerebral Overwriter" 1)
                             (qty "Enhanced Login Protocol" 1) (qty "Shipment from SanSan" 1)
                             (qty "Hedge Fund" 1)])
              (default-challenger))
    (starting-hand state :contestant ["Accelerated Diagnostics" "Cerebral Overwriter"])
    (play-from-hand state :contestant "Cerebral Overwriter" "New party")
    (core/gain state :contestant :credit 3)
    (play-from-hand state :contestant "Accelerated Diagnostics")

    (let [playarea (get-in @state [:contestant :play-area])
          hf (find-card "Hedge Fund" playarea)
          ss (find-card "Shipment from SanSan" playarea)
          elp (find-card "Enhanced Login Protocol" playarea)
          co (get-content state :party1 0)]
      (is (= 3 (count playarea)) "3 cards in play area")
      (prompt-select :contestant elp)
      (is (= "Enhanced Login Protocol" (:title (first (get-in @state [:contestant :current]))))
        "Enhanced Login Protocol active in Current area")
      (prompt-select :contestant ss)
      (prompt-choice :contestant "2")
      (prompt-select :contestant co)
      (is (= 2 (:advance-counter (refresh co))) "Cerebral Overwriter gained 2 advancements")
      (prompt-select :contestant hf)
      (is (= 9 (:credit (get-contestant))) "Contestant gained credits from Hedge Fund"))))

(deftest an-offer-you-cant-refuse
  ;; An Offer You Can't Refuse - exact card added to score area, not the last discarded one
  (do-game
    (new-game (default-contestant [(qty "Celebrity Gift" 1) (qty "An Offer You Can't Refuse" 1)])
              (default-challenger))
    (play-from-hand state :contestant "An Offer You Can't Refuse")
    (prompt-choice :contestant "R&D")
    (core/move state :contestant (find-card "Celebrity Gift" (:hand (get-contestant))) :discard)
    (is (= 2 (count (:discard (get-contestant)))))
    (prompt-choice :challenger "No")
    (is (= 1 (:agenda-point (get-contestant))) "An Offer the Challenger refused")
    (is (= 1 (count (:scored (get-contestant)))))
    (is (find-card "An Offer You Can't Refuse" (:scored (get-contestant))))
    (is (= 1 (count (:discard (get-contestant)))))
    (is (find-card "Celebrity Gift" (:discard (get-contestant))))))

(deftest big-brother
  ;; Big Brother - Give the Challenger 2 tags if already tagged
  (do-game
    (new-game (default-contestant [(qty "Big Brother" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Big Brother")
    (is (= 1 (count (:hand (get-contestant)))) "Card not played because Challenger has no tags")
    (core/gain state :challenger :tag 1)
    (play-from-hand state :contestant "Big Brother")
    (is (= 3 (:tag (get-challenger))) "Challenger gained 2 tags")))

(deftest biotic-labor
  ;; Biotic Labor - Gain 2 clicks
  (do-game
    (new-game (default-contestant [(qty "Biotic Labor" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Biotic Labor")
    (is (= 1 (:credit (get-contestant))))
    (is (= 4 (:click (get-contestant))) "Spent 1 click to gain 2 additional clicks")))

(deftest blue-level-clearance
  ;; Blue Level Clearance - Gain 5 credits and draw 2 cards
  (do-game
    (new-game (default-contestant [(qty "Blue Level Clearance" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Sweeps Week" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Blue Level Clearance")
    (is (= 8 (:credit (get-contestant))) "Gained 5 credits")
    (is (= 1 (:click (get-contestant))))
    (is (= 7 (count (:hand (get-contestant)))) "Drew 2 cards")))

(deftest casting-call
  ;; Casting Call - Only do card-init on the Public agendas.  Issue #1128
  (do-game
    (new-game (default-contestant [(qty "Casting Call" 2) (qty "Oaktown Renovation" 1)
                             (qty "Improved Tracers" 1) (qty "Hunter" 1)])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Hunter" "HQ")
    (let [hunter (get-character state :hq 0)]
      (core/reveal state :contestant hunter)
      (is (= 4 (:current-strength (refresh hunter))))
      (play-from-hand state :contestant "Casting Call")
      (prompt-select :contestant (find-card "Improved Tracers" (:hand (get-contestant))))
      (prompt-choice :contestant "New party")
      (let [imptrac (get-content state :party1 0)]
        (is (get-in (refresh imptrac) [:revealed]) "Improved Tracers is faceup")
        (is (= 4 (:current-strength (refresh hunter))) "Hunter hasn't gained strength")
        (play-from-hand state :contestant "Casting Call")
        (prompt-select :contestant (find-card "Oaktown Renovation" (:hand (get-contestant))))
        (prompt-choice :contestant "New party")
        (let [oak (get-content state :party2 0)]
          (core/advance state :contestant {:card (refresh oak)})
          (is (= 5 (:credit (get-contestant))) "Events on Public agenda work; gained 2 credits from advancing")
          (take-credits state :contestant)
          (run-empty-locale state "Locale 2")
          (prompt-select :challenger oak)
          (prompt-choice :challenger "Steal")
          (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags from accessing agenda with Casting Call hosted on it"))))))

(deftest cerebral-cast-challenger-wins
  ;; Cerebral Cast: if the challenger succefully ran last turn, psi game to give challenger choice of tag or BD
  (do-game
    (new-game (default-contestant [(qty "Cerebral Cast" 1)])
              (default-challenger))
	    (play-from-hand state :contestant "Cerebral Cast")
	    (is (= 3 (:click (get-contestant))) "Cerebral Cast precondition not met; card not played")
	    (take-credits state :contestant)
	    (run-empty-locale state "Archives")
	    (take-credits state :challenger)
	    (play-from-hand state :contestant "Cerebral Cast")
        (prompt-choice :contestant "0 [Credits]")
        (prompt-choice :challenger "0 [Credits]")
	    (is (= 0 (count (:discard (get-challenger)))) "Challenger took no damage")
		(is (= 0 (:tag (get-challenger))) "Challenger took no tags")))

(deftest cerebral-cast-contestant-wins
  ;; Cerebral Cast: if the challenger succefully ran last turn, psi game to give challenger choice of tag or BD
  (do-game
    (new-game (default-contestant [(qty "Cerebral Cast" 2)])
              (default-challenger))
	    (take-credits state :contestant)
	    (run-empty-locale state "Archives")
	    (take-credits state :challenger)
	    (play-from-hand state :contestant "Cerebral Cast")
        (prompt-choice :contestant "0 [Credits]")
        (prompt-choice :challenger "1 [Credits]")
		(prompt-choice :challenger "1 brain damage")
	    (is (= 1 (count (:discard (get-challenger)))) "Challenger took a brain damage")
		(is (= 0 (:tag (get-challenger))) "Challenger took no tags from brain damage choice")
	    (play-from-hand state :contestant "Cerebral Cast")
        (prompt-choice :contestant "0 [Credits]")
        (prompt-choice :challenger "1 [Credits]")
		(prompt-choice :challenger "1 tag")
	    (is (= 1 (count (:discard (get-challenger)))) "Challenger took no additional damage")
		(is (= 1 (:tag (get-challenger))) "Challenger took a tag from Cerebral Cast choice")))


(deftest cerebral-static-chaos-theory
  ;; Cerebral Static - vs Chaos Theory
  (do-game
    (new-game (default-contestant [(qty "Cerebral Static" 1) (qty "Lag Time" 1)])
              (make-deck "Chaos Theory: Wünderkind" [(qty "Sure Gamble" 3)]))
    (is (= 5 (:memory (get-challenger))) "CT starts with 5 memory")
    (play-from-hand state :contestant "Cerebral Static")
    (is (= 4 (:memory (get-challenger))) "Cerebral Static causes CT to have 4 memory")
    (play-from-hand state :contestant "Lag Time")
    (is (= 5 (:memory (get-challenger))) "CT 5 memory restored")))

(deftest closed-accounts
  ;; Closed Accounts - Play if Challenger is tagged to make Challenger lose all credits
  (do-game
    (new-game (default-contestant [(qty "Closed Accounts" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Closed Accounts")
    (is (and (= 3 (:click (get-contestant)))
             (= 5 (:credit (get-challenger))))
        "Closed Accounts precondition not met; card not played")
    (core/gain state :challenger :tag 1)
    (play-from-hand state :contestant "Closed Accounts")
    (is (= 0 (:credit (get-challenger))) "Challenger lost all credits")))

(deftest commercialization-single-advancement
  ;; Commercialization - Single advancement token
  (do-game
    (new-game (default-contestant [(qty "Commercialization" 1)
                             (qty "Ice Wall" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (core/add-counter state :contestant (refresh (get-character state :hq 0)) :advancement 1)
    (play-from-hand state :contestant "Commercialization")
    (prompt-select :contestant (refresh (get-character state :hq 0)))
    (is (= 6 (:credit (get-contestant))) "Gained 1 for single advanced character from Commercialization")))

(deftest commercialization-double-advancement
  ;; Commercialization - Two advancement tokens
  (do-game
    (new-game (default-contestant [(qty "Commercialization" 1)
                             (qty "Ice Wall" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (core/add-counter state :contestant (refresh (get-character state :hq 0)) :advancement 2)
    (play-from-hand state :contestant "Commercialization")
    (prompt-select :contestant (refresh (get-character state :hq 0)))
    (is (= 7 (:credit (get-contestant))) "Gained 2 for double advanced character from Commercialization")))

(deftest consulting-visit
  ;; Consulting Visit - Only show single copies of operations contestant can afford as choices. Play chosen operation
  (do-game
    (new-game (default-contestant [(qty "Consulting Visit" 1)
                             (qty "Beanstalk Royalties" 2)
                             (qty "Green Level Clearance" 1)
                             (qty "Breaking News" 1)
                             (qty "Hedge Fund" 1)])
              (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (starting-hand state :contestant ["Consulting Visit"])
    (play-from-hand state :contestant "Consulting Visit")

    (let [get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]

      (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
      (prompt-choice :contestant (find-card "Beanstalk Royalties" (:deck (get-contestant))))
      (is (= 6 (:credit (get-contestant)))))))

(deftest consulting-visit-mumbad
  ;; Consulting Visit - Works properly when played with Mumbad City Hall
  (do-game
    (new-game (default-contestant [(qty "Mumbad City Hall" 1)
                             (qty "Beanstalk Royalties" 1)
                             (qty "Green Level Clearance" 1)
                             (qty "Breaking News" 1)
                             (qty "Hedge Fund" 1)
                             (qty "Consulting Visit" 1)
                             (qty "Mumba Temple" 1)])
              (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (starting-hand state :contestant ["Mumbad City Hall"])
    (play-from-hand state :contestant "Mumbad City Hall" "New party")

    (let [hall (get-content state :party1 0)
          get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]

      (card-ability state :contestant hall 0)
      (is (= (list "Consulting Visit" "Mumba Temple" nil) (prompt-names)))

      (prompt-choice :contestant (find-card "Consulting Visit" (:deck (get-contestant))))
      (is (= 3 (:credit (get-contestant))))
      (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))

      (prompt-choice :contestant (find-card "Green Level Clearance" (:deck (get-contestant))))
      (is (= 5 (:credit (get-contestant)))))))

(deftest defective-brainchips
  ;; Defective Brainchips - Do 1 add'l brain damage the first time Challenger takes some each turn
  (do-game
    (new-game (default-contestant [(qty "Defective Brainchips" 1) (qty "Viktor 1.0" 1)])
              (default-challenger [(qty "Sure Gamble" 2) (qty "Shiv" 2)]))
    (play-from-hand state :contestant "Defective Brainchips")
    (play-from-hand state :contestant "Viktor 1.0" "HQ")
    (take-credits state :contestant)
    (run-on state :hq)
    (let [vik (get-character state :hq 0)]
      (core/reveal state :contestant vik)
      (card-subroutine state :contestant vik 0)
      (is (= 2 (count (:discard (get-challenger)))) "2 cards lost to brain damage")
      (is (= 2 (:brain-damage (get-challenger))) "Brainchips dealt 1 additional brain dmg")
      (card-subroutine state :contestant vik 0)
      (is (= 3 (count (:discard (get-challenger)))) "2 cards lost to brain damage")
      (is (= 3 (:brain-damage (get-challenger))) "Brainchips didn't do additional brain dmg"))))

(deftest distract-the-masses
  (do-game
    (new-game (default-contestant [(qty "Distract the Masses" 2) (qty "Hedge Fund" 3)])
              (default-challenger))
    (starting-hand state :contestant ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Distract the Masses" "Distract the Masses"])
    (play-from-hand state :contestant "Distract the Masses")
    (prompt-select :contestant (first (:hand (get-contestant))))
    (prompt-select :contestant (first (next (:hand (get-contestant)))))
    (prompt-select :contestant (first (:discard (get-contestant))))
    (prompt-choice :contestant "Done")
    (is (= 1 (count (:discard (get-contestant)))) "1 card still discarded")
    (is (= 1 (count (:deck (get-contestant)))) "1 card shuffled into R&D")
    (is (= 1 (count (:rfg (get-contestant)))) "Distract the Masses removed from game")
    (is (= 7 (:credit (get-challenger))) "Challenger gained 2 credits")
    (play-from-hand state :contestant "Distract the Masses")
    (prompt-select :contestant (first (:hand (get-contestant))))
    (prompt-choice :contestant "Done")
    (prompt-select :contestant (first (:discard (get-contestant))))
    (prompt-select :contestant (first (next (:discard (get-contestant)))))
    (is (= 0 (count (:discard (get-contestant)))) "No cards left in archives")
    (is (= 3 (count (:deck (get-contestant)))) "2 more cards shuffled into R&D")
    (is (= 2 (count (:rfg (get-contestant)))) "Distract the Masses removed from game")
    (is (= 9 (:credit (get-challenger))) "Challenger gained 2 credits")))

(deftest diversified-portfolio
  (do-game
    (new-game (default-contestant [(qty "Diversified Portfolio" 1)
                             (qty "Paper Wall" 1)
                             (qty "PAD Campaign" 3)])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Paper Wall" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "Diversified Portfolio")
    (is (= 7 (:credit (get-contestant))) "Ignored party with Character but no locale contents")))

(deftest economic-warfare
  ;; Economic Warfare - If successful run last turn, make the challenger lose 4 credits if able
  (do-game
    (new-game (default-contestant [(qty "Economic Warfare" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Economic Warfare")
    (is (= 5 (:credit (get-challenger))) "Challenger has 5 credits")
    (is (= 3 (count (:hand (get-contestant)))) "Contestant still has 3 cards")
    (take-credits state :contestant)

    (run-on state :archives)
    (run-successful state)
    (take-credits state :challenger)

    (play-from-hand state :contestant "Economic Warfare")
    (is (= 4 (:credit (get-challenger))) "Challenger has 4 credits")
    (play-from-hand state :contestant "Economic Warfare")
    (is (= 0 (:credit (get-challenger))) "Challenger has 0 credits")
    (take-credits state :contestant)

    (run-on state :archives)
    (take-credits state :challenger)

    (play-from-hand state :contestant "Economic Warfare")
    (is (= 3 (:credit (get-challenger))) "Challenger has 3 credits")))

(deftest enhanced-login-protocol
  ;; Enhanced Login Protocol - First click run each turn costs an additional click
  (do-game
    (new-game (default-contestant [(qty "Enhanced Login Protocol" 1)])
              (default-challenger [(qty "Employee Strike" 1)]))
    (play-from-hand state :contestant "Enhanced Login Protocol")
    (take-credits state :contestant)

    (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
    (run-on state :archives)
    (is (= 2 (:click (get-challenger)))
        "Challenger spends 1 additional click to make the first run")
    (run-successful state)

    (run-on state :archives)
    (is (= 1 (:click (get-challenger)))
        "Challenger doesn't spend 1 additional click to make the second run")
    (run-successful state)

    (take-credits state :challenger)
    (take-credits state :contestant)
    (take-credits state :challenger 3)

    (is (= 1 (:click (get-challenger))) "Challenger has 1 click")
    (run-on state :archives)
    (is (not (:run @state)) "No run was initiated")
    (is (= 1 (:click (get-challenger))) "Challenger has 1 click")

    (take-credits state :challenger)
    (take-credits state :contestant)

    (play-from-hand state :challenger "Employee Strike")

    (is (= 3 (:click (get-challenger))) "Challenger has 3 clicks")
    (run-on state :archives)
    (is (= 2 (:click (get-challenger)))
        "Challenger doesn't spend 1 additional click to make a run")))

(deftest enhanced-login-protocol-card-ability
  ;; Enhanced Login Protocol - Card ability runs don't cost additional clicks
  (do-game
    (new-game (default-contestant [(qty "Enhanced Login Protocol" 1)])
              (default-challenger [(qty "Sneakdoor Beta" 1)]))
    (play-from-hand state :contestant "Enhanced Login Protocol")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sneakdoor Beta")
    (take-credits state :challenger)
    (take-credits state :contestant)

    (is (= 4 (:click (get-challenger))) "Challenger has 2 clicks")
    (let [sneakdoor (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger sneakdoor 0)
      (is (= 3 (:click (get-challenger)))
          "Challenger doesn't spend 1 additional click to run with a card ability")
      (run-successful state)

      (run-on state :archives)
      (is (= 1 (:click (get-challenger)))
          "Challenger spends 1 additional click to make a run")
      (run-successful state)

      (take-credits state :challenger)
      (take-credits state :contestant)

      (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-challenger)))
          "Challenger spends 1 additional click to make a run"))))

(deftest enhanced-login-protocol-new-angeles-sol
  ;; Enhanced Login Protocol discarded and replaced on steal doesn't double remove penalty
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Enhanced Login Protocol" 1) (qty "Breaking News" 1)])
      (default-challenger))
    (play-from-hand state :contestant "Breaking News" "New party")
    (play-from-hand state :contestant "Enhanced Login Protocol")
    (take-credits state :contestant)

    (run-on state :party1)
    (run-successful state)
    (prompt-choice :challenger "Steal")

    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (find-card "Enhanced Login Protocol"
                                    (:discard (get-contestant))))

    (run-on state :archives)
    (is (= 1 (:click (get-challenger))) "Challenger has 1 click")))

(deftest enhanced-login-protocol-run-events
  ;; Enhanced Login Protocol - Run event don't cost additional clicks
  (do-game
    (new-game (default-contestant [(qty "Enhanced Login Protocol" 1)])
              (default-challenger [(qty "Out of the Ashes" 1)]))
    (play-from-hand state :contestant "Enhanced Login Protocol")
    (take-credits state :contestant)

    (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
    (play-from-hand state :challenger "Out of the Ashes")
    (prompt-choice :challenger "Archives")
    (is (= 3 (:click (get-challenger)))
        "Challenger doesn't spend 1 additional click to run with a run event")
    (run-successful state)

    (run-on state :archives)
    (is (= 1 (:click (get-challenger)))
        "Challenger spends 1 additional click to make a run")
    (run-successful state)

    (take-credits state :challenger)
    (take-credits state :contestant)
    (prompt-choice :challenger "No") ; Out of the Ashes prompt

    (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
    (run-on state :archives)
    (is (= 2 (:click (get-challenger)))
        "Challenger spends 1 additional click to make a run")))

(deftest enhanced-login-protocol-challenger-turn-first-run
  ;; Enhanced Login Protocol - Works when played on the challenger's turn
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News"
                         [(qty "Enhanced Login Protocol" 1)
                          (qty "Breaking News" 1)])
              (default-challenger [(qty "Hades Shard" 1)]))
    (discard-from-hand state :contestant "Breaking News")
    (take-credits state :contestant)

    (core/gain state :challenger :credit 2)
    (play-from-hand state :challenger "Hades Shard")
    (card-ability state :challenger (get-in @state [:challenger :rig :muthereff 0]) 0)
    (prompt-choice :challenger "Steal")
    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (find-card "Enhanced Login Protocol"
                                    (:hand (get-contestant))))
    (is (find-card "Enhanced Login Protocol" (:current (get-contestant)))
        "Enhanced Login Protocol is in play")

    (is (= 3 (:click (get-challenger))) "Challenger has 3 clicks")
    (run-on state :archives)
    (is (= 1 (:click (get-challenger)))
        "Challenger spends 1 additional click to make a run")))

(deftest enhanced-login-protocol-challenger-turn-second-run
  ;; Enhanced Login Protocol - Doesn't fire if already run when played on the challenger's turn
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News"
                         [(qty "Enhanced Login Protocol" 1)
                          (qty "Breaking News" 1)])
              (default-challenger [(qty "Hades Shard" 1)]))
    (discard-from-hand state :contestant "Breaking News")
    (take-credits state :contestant)

    (run-on state :hq)
    (run-successful state)
    (prompt-choice :challenger "OK")

    (core/gain state :challenger :credit 2)
    (play-from-hand state :challenger "Hades Shard")
    (card-ability state :challenger (get-in @state [:challenger :rig :muthereff 0]) 0)
    (prompt-choice :challenger "Steal")
    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (find-card "Enhanced Login Protocol"
                                    (:hand (get-contestant))))
    (is (find-card "Enhanced Login Protocol" (:current (get-contestant)))
        "Enhanced Login Protocol is in play")

    (is (= 2 (:click (get-challenger))) "Challenger has 2 clicks")
    (run-on state :archives)
    (is (= 1 (:click (get-challenger)))
        "Challenger doesn't spend 1 additional click to make a run")))

(deftest exchange-of-information
  ;; Exchange of Information - Swapping agendas works correctly
  (do-game
    (new-game (default-contestant [(qty "Exchange of Information" 1)
                             (qty "Market Research" 1)
                             (qty "Breaking News" 1)
                             (qty "Project Beale" 1)
                             (qty "Explode-a-palooza" 1)])
              (default-challenger))

      (score-agenda state :contestant (find-card "Market Research" (:hand (get-contestant))))
      (score-agenda state :contestant (find-card "Breaking News" (:hand (get-contestant))))
      (is (= 2 (:tag (get-challenger))) "Challenger gained 2 tags")
      (take-credits state :contestant)
      (is (= 0 (:tag (get-challenger))) "Challenger lost 2 tags")

      (core/steal state :challenger (find-card "Project Beale" (:hand (get-contestant))))
      (core/steal state :challenger (find-card "Explode-a-palooza" (:hand (get-contestant))))
      (take-credits state :challenger)

      (is (= 4 (:agenda-point (get-challenger))))
      (is (= 3 (:agenda-point (get-contestant))))

      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "Exchange of Information")

      (prompt-select :contestant (find-card "Project Beale" (:scored (get-challenger))))
      (prompt-select :contestant (find-card "Breaking News" (:scored (get-contestant))))

      (is (= 3 (:agenda-point (get-challenger))))
      (is (= 4 (:agenda-point (get-contestant))))))

(deftest exchange-of-information-breaking-news
  ;; Exchange of Information - Swapping a just scored Breaking News keeps the tags
  (do-game
    (new-game (default-contestant [(qty "Exchange of Information" 1)
                             (qty "Market Research" 1)
                             (qty "Breaking News" 1)
                             (qty "Project Beale" 1)
                             (qty "Explode-a-palooza" 1)])
              (default-challenger))

      (take-credits state :contestant)

      (core/steal state :challenger (find-card "Project Beale" (:hand (get-contestant))))
      (core/steal state :challenger (find-card "Explode-a-palooza" (:hand (get-contestant))))
      (take-credits state :challenger)

      (score-agenda state :contestant (find-card "Breaking News" (:hand (get-contestant))))
      (is (= 2 (:tag (get-challenger))) "Challenger gained 2 tags")
      (play-from-hand state :contestant "Exchange of Information")

      (prompt-select :contestant (find-card "Project Beale" (:scored (get-challenger))))
      (prompt-select :contestant (find-card "Breaking News" (:scored (get-contestant))))
      (is (= 2 (:tag (get-challenger))) "Still has tags after swap and before end of turn")

      (take-credits state :contestant)
      (is (= 3 (:agenda-point (get-challenger))))
      (is (= 2 (:agenda-point (get-contestant))))
      (is (= 2 (:tag (get-challenger))) "Challenger does not lose tags at end of turn")))

(deftest exchange-of-information-fifteen-minutes
  ;; Exchange of Information - Swapping a 15 Minutes still keeps the ability. #1783
  (do-game
    (new-game (default-contestant [(qty "Exchange of Information" 2) (qty "15 Minutes" 1)
                             (qty "Project Beale" 1)])
              (default-challenger))
    (score-agenda state :contestant (find-card "15 Minutes" (:hand (get-contestant))))
    (take-credits state :contestant)
    (core/gain state :challenger :tag 1)
    (core/steal state :challenger (find-card "Project Beale" (:hand (get-contestant))))
    (take-credits state :challenger)
    (is (= 1 (:agenda-point (get-contestant))))
    (is (= 2 (:agenda-point (get-challenger))))
    (play-from-hand state :contestant "Exchange of Information")
    (prompt-select :contestant (find-card "Project Beale" (:scored (get-challenger))))
    (prompt-select :contestant (find-card "15 Minutes" (:scored (get-contestant))))
    (is (= 2 (:agenda-point (get-contestant))))
    (is (= 1 (:agenda-point (get-challenger))))
    (is (= 0 (count (:deck (get-contestant)))))
    ;; shuffle back into R&D from challenger's scored area
    (let [fifm (get-in @state [:challenger :scored 0])]
      (card-ability state :contestant fifm 0))
    (is (= 2 (:agenda-point (get-contestant))))
    (is (= 0 (:agenda-point (get-challenger))))
    (is (= "15 Minutes" (:title (first (:deck (get-contestant))))))
    (take-credits state :contestant)
    (core/steal state :challenger (find-card "15 Minutes" (:deck (get-contestant))))
    (take-credits state :challenger)
    (is (= 2 (:agenda-point (get-contestant))))
    (is (= 1 (:agenda-point (get-challenger))))
    (play-from-hand state :contestant "Exchange of Information")
    (prompt-select :contestant (find-card "15 Minutes" (:scored (get-challenger))))
    (prompt-select :contestant (find-card "Project Beale" (:scored (get-contestant))))
    (is (= 1 (:agenda-point (get-contestant))))
    (is (= 2 (:agenda-point (get-challenger))))
    ;; shuffle back into R&D from contestant's scored area
    (let [fifm (get-in @state [:contestant :scored 0])]
      (card-ability state :contestant fifm 0))
    (is (= "15 Minutes" (:title (first (:deck (get-contestant))))))))

(deftest exchange-of-information-mandatory-regions
  ;; Exchange of Information - Swapping a Mandatory Regions gives the Contestant an additional click per turn. #1687
  (do-game
    (new-game (default-contestant [(qty "Exchange of Information" 2) (qty "Mandatory Regions" 1)
                             (qty "Global Food Initiative" 1)])
              (default-challenger))
    (score-agenda state :contestant (find-card "Global Food Initiative" (:hand (get-contestant))))
    (take-credits state :contestant)
    (core/gain state :challenger :tag 1)
    (core/steal state :challenger (find-card "Mandatory Regions" (:hand (get-contestant))))
    (take-credits state :challenger)
    (is (= 3 (:agenda-point (get-contestant))))
    (is (= 2 (:agenda-point (get-challenger))))
    (is (= 3 (:click (get-contestant))))
    (is (= 3 (:click-per-turn (get-contestant))))
    (play-from-hand state :contestant "Exchange of Information")
    (prompt-select :contestant (find-card "Mandatory Regions" (:scored (get-challenger))))
    (prompt-select :contestant (find-card "Global Food Initiative" (:scored (get-contestant))))
    (is (= 2 (:agenda-point (get-contestant))))
    (is (= 2 (:agenda-point (get-challenger))))
    (is (= 3 (:click (get-contestant))))
    (is (= 4 (:click-per-turn (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 4 (:click (get-contestant))))
    (is (= 4 (:click-per-turn (get-contestant))))
    (play-from-hand state :contestant "Exchange of Information")
    (prompt-select :contestant (find-card "Global Food Initiative" (:scored (get-challenger))))
    (prompt-select :contestant (find-card "Mandatory Regions" (:scored (get-contestant))))
    (is (= 3 (:agenda-point (get-contestant))))
    (is (= 2 (:agenda-point (get-challenger))))
    (is (= 2 (:click (get-contestant))))
    (is (= 3 (:click-per-turn (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 3 (:click (get-contestant))))
    (is (= 3 (:click-per-turn (get-contestant))))))

(deftest election-day
  (do-game
    (new-game (default-contestant [(qty "Election Day" 7)])
                (default-challenger))
      (is (= 6 (count (:hand (get-contestant)))) "Contestant starts with 5 + 1 cards")
      (core/move state :contestant (find-card "Election Day" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Election Day" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Election Day" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Election Day" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Election Day" (:hand (get-contestant))) :deck)
      (play-from-hand state :contestant "Election Day")
      (is (= 1 (count (:hand (get-contestant)))) "Could not play Election Day")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 2 (count (:hand (get-contestant)))) "Contestant has now 1 + 1 cards before Election Day")
      (play-from-hand state :contestant "Election Day")
      (is (= 5 (count (:hand (get-contestant)))) "Contestant has now 5 cards due to Election Day")))

(deftest hedge-fund
  (do-game
    (new-game (default-contestant) (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 9 (:credit (get-contestant))))))

(deftest housekeeping
  ;; Housekeeping - Challenger must discard a card from Grip on first place of a turn
  (do-game
    (new-game (default-contestant [(qty "Housekeeping" 1)])
              (default-challenger [(qty "Cache" 2) (qty "Fall Guy" 1) (qty "Mr. Li" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Fall Guy")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Housekeeping")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Cache")
    (prompt-select :challenger (find-card "Mr. Li" (:hand (get-challenger))))
    (is (empty? (:prompt (get-challenger))) "Fall Guy prevention didn't trigger")
    (is (= 1 (count (:discard (get-challenger)))) "Card discarded")
    (play-from-hand state :challenger "Cache")
    (is (empty? (:prompt (get-challenger))) "Housekeeping didn't trigger on 2nd place")))

(deftest invasion-of-privacy
  ;; Invasion of Privacy - Full test
  (do-game
    (new-game (default-contestant [(qty "Invasion of Privacy" 3)])
              (default-challenger [(qty "Sure Gamble" 2) (qty "Fall Guy" 1) (qty "Cache" 2)]))
    (core/gain state :contestant :click 3 :credit 6)
    ;; discard 2 cards
    (play-from-hand state :contestant "Invasion of Privacy")
    (prompt-choice :contestant 0) ; default trace
    (prompt-choice :challenger 0) ; Challenger won't match
    (is (= 5 (count (:hand (get-challenger)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" "Sure Gamble" nil) (prompt-names)))
      (prompt-choice :contestant (find-card "Sure Gamble" (:hand (get-challenger))))
      (prompt-choice :contestant (find-card "Sure Gamble" (:hand (get-challenger)))))
    (is (= 3 (count (:hand (get-challenger)))))
    ;; able to discard 2 cards but only 1 available target in Challenger's hand
    (play-from-hand state :contestant "Invasion of Privacy")
    (prompt-choice :contestant 0) ; default trace
    (prompt-choice :challenger 0) ; Challenger won't match
    (is (= 3 (count (:hand (get-challenger)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" nil) (prompt-names)))
      (prompt-choice :contestant (find-card "Fall Guy" (:hand (get-challenger))))
      (is (empty? (get-in @state [:contestant :prompt])) "No prompt for second card"))
    (is (= 2 (count (:hand (get-challenger)))))
    ;; failed trace - take the bad publicity
    (play-from-hand state :contestant "Invasion of Privacy")
    (prompt-choice :contestant 0) ; default trace
    (prompt-choice :challenger 2) ; Challenger matches
    (is (= 1 (:bad-publicity (get-contestant))))))

(deftest ipo-terminal
  ;; IPO - credits with Terminal operations
  (do-game
    (new-game
      (default-contestant [(qty "IPO" 1)])
      (default-challenger))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "IPO")
	(is (= 13 (:credit (get-contestant))))
	(is (= 0 (:click (get-contestant))) "Terminal ends turns")))

(deftest lag-time
  (do-game
    (new-game (default-contestant [(qty "Lag Time" 1) (qty "Vanilla" 1) (qty "Lotus Field" 1)])
              (default-challenger))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Vanilla" "HQ")
    (play-from-hand state :contestant "Lotus Field" "R&D")
    (play-from-hand state :contestant "Lag Time")
    (core/reveal state :contestant (get-character state :hq 0))
    (core/reveal state :contestant (get-character state :rd 0))
    (is (= 1 (:current-strength (get-character state :hq 0))) "Vanilla at 1 strength")
	(is (= 5 (:current-strength (get-character state :rd 0))) "Lotus Field at 5 strength")))

(deftest lateral-growth
  (do-game
    (new-game (default-contestant [(qty "Lateral Growth" 1) (qty "Breaking News" 1)])
              (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (play-from-hand state :contestant "Lateral Growth")
    (prompt-select :contestant (find-card "Breaking News" (:hand (get-contestant))))
    (prompt-choice :contestant "New party")
    (is (= "Breaking News" (:title (get-content state :party1 0)))
      "Breaking News placed by Lateral Growth")
    (is (= 7 (:credit (get-contestant))))))

(deftest mass-commercialization
  ;; Mass Commercialization
  (do-game
    (new-game (default-contestant [(qty "Mass Commercialization" 1)
                             (qty "Ice Wall" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (play-from-hand state :contestant "Ice Wall" "Archives")
    (take-credits state :challenger)
    (core/advance state :contestant {:card (refresh (get-character state :hq 0))})
    (core/advance state :contestant {:card (refresh (get-character state :archives 0))})
    (core/advance state :contestant {:card (refresh (get-character state :rd 0))})
    (take-credits state :challenger)
    (play-from-hand state :contestant "Mass Commercialization")
    (is (= 8 (:credit (get-contestant))) "Gained 6 for 3 advanced character from Mass Commercialization")))

(deftest manhunt-every-run
  ;; Manhunt - only fires once per turn. Unreported issue.
  (do-game
    (new-game (default-contestant [(qty "Manhunt" 1) (qty "Hedge Fund" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Manhunt")
    (take-credits state :contestant)
    (run-empty-locale state "HQ")
    (is (:prompt (get-contestant)) "Manhunt trace initiated")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag")
    (prompt-choice :challenger "OK")
    (is (not (:run @state)) "Run ended")
    (run-empty-locale state "HQ")
    (is (empty? (:prompt (get-contestant))) "No Manhunt trace on second run")
    (prompt-choice :challenger "OK")
    (is (not (:run @state)) "Run ended")))

(deftest midseason-replacements
  ;; Midseason Replacements - Trace to give Challenger tags after they steal an agenda
  (do-game
    (new-game (default-contestant [(qty "Midseason Replacements" 1) (qty "Breaking News" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Midseason Replacements")
    (is (= 3 (:click (get-contestant))) "Midseason precondition not met; Contestant not charged a click")
    (play-from-hand state :contestant "Breaking News" "New party")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))))
    (let [bn (get-content state :party1 0)]
      (run-empty-locale state "Locale 1")
      (prompt-choice :challenger "Steal")
      (is (= 1 (:agenda-point (get-challenger))) "Stole Breaking News")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Midseason Replacements")
      (prompt-choice :contestant 0) ; default trace
      (prompt-choice :challenger 0) ; Challenger won't match
      (is (= 6 (:tag (get-challenger))) "Challenger took 6 tags"))))

(deftest mushin-no-shin
  ;; Mushin No Shin - Add 3 advancements to a card; prevent reveal/score of that card the rest of the turn
  (do-game
    (new-game (default-contestant [(qty "Mushin No Shin" 2) (qty "Ronin" 1) (qty "Profiteering" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Mushin No Shin")
    (prompt-select :contestant (find-card "Ronin" (:hand (get-contestant))))
    (let [ronin (get-content state :party1 0)]
      (is (= 3 (:advance-counter (refresh ronin))) "3 advancements placed on Ronin")
      (core/reveal state :contestant (refresh ronin))
      (is (not (get-in (refresh ronin) [:revealed])) "Ronin did not reveal")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/reveal state :contestant (refresh ronin))
      (is (get-in (refresh ronin) [:revealed]) "Ronin now revealed")
      (play-from-hand state :contestant "Mushin No Shin")
      (prompt-select :contestant (find-card "Profiteering" (:hand (get-contestant))))
      (let [prof (get-content state :party2 0)]
        (core/score state :contestant (refresh prof))
        (is (empty? (:scored (get-contestant))) "Profiteering not scored")
        (is (= 0 (:agenda-point (get-contestant))))
        (take-credits state :contestant)
        (take-credits state :challenger)
        (core/score state :contestant (refresh prof))
        (prompt-choice :contestant "0")
        (is (= 1 (:agenda-point (get-contestant))) "Profiteering was able to be scored")))))

(deftest neural-emp
  ;; Neural EMP - Play if Challenger made a run the previous turn to do 1 net damage
  (do-game
    (new-game (default-contestant [(qty "Neural EMP" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Neural EMP")
    (is (= 3 (:click (get-contestant))) "Neural precondition not met; card not played")
    (take-credits state :contestant)
    (run-empty-locale state "Archives")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Neural EMP")
    (is (= 1 (count (:discard (get-challenger)))) "Challenger took 1 net damage")))

(deftest oversight-ai
  ;; Oversight AI - Reveal a piece of Character ignoring all costs
  (do-game
    (new-game (default-contestant [(qty "Oversight AI" 1) (qty "Archer" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Archer" "R&D")
    (let [archer (get-character state :rd 0)]
      (play-from-hand state :contestant "Oversight AI")
      (prompt-select :contestant archer)
      (is (get-in (refresh archer) [:revealed]))
      (is (= 4 (:credit (get-contestant))) "Archer revealed at no credit cost")
      (is (= "Oversight AI" (:title (first (:hosted (refresh archer)))))
          "Archer hosting OAI as a condition"))))

(deftest patch
  ;; Patch - +2 current strength
  (do-game
    (new-game (default-contestant [(qty "Patch" 1) (qty "Vanilla" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Vanilla" "HQ")
    (core/reveal state :contestant (get-character state :hq 0))
    (play-from-hand state :contestant "Patch")
    (prompt-select :contestant (get-character state :hq 0))
    (is (= 2 (:current-strength (get-character state :hq 0))) "Vanilla at 2 strength")))

(deftest paywall-implementation
  ;; Paywall Implementation - Gain 1 credit for every successful run
  (do-game
    (new-game (default-contestant [(qty "Paywall Implementation" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Paywall Implementation")
    (is (= "Paywall Implementation" (:title (first (get-in @state [:contestant :current]))))
        "Paywall active in Current area")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))))
    (run-empty-locale state "Archives")
    (is (= 8 (:credit (get-contestant))) "Gained 1 credit from successful run")
    (run-empty-locale state "Archives")
    (is (= 9 (:credit (get-contestant))) "Gained 1 credit from successful run")))

(deftest peak-efficiency
  ;; Peak Efficiency - Gain 1 credit for each revealed Character
  (do-game
    (new-game (default-contestant [(qty "Peak Efficiency" 1) (qty "Paper Wall" 3) (qty "Wraparound" 1)])
              (default-challenger))
    (core/gain state :contestant :click 3)
    (play-from-hand state :contestant "Paper Wall" "HQ")
    (play-from-hand state :contestant "Paper Wall" "R&D")
    (play-from-hand state :contestant "Paper Wall" "New party")
    (play-from-hand state :contestant "Wraparound" "New party")
    (core/reveal state :contestant (get-character state :hq 0))
    (core/reveal state :contestant (get-character state :rd 0))
    (core/reveal state :contestant (get-character state :party1 0))
    (play-from-hand state :contestant "Peak Efficiency")
    (is (= 7 (:credit (get-contestant))) "Gained 3 credits for 3 revealed Character; unrevealed Character ignored")))

(deftest power-shutdown
  ;; Power Shutdown - Discard cards from R&D to force Challenger to discard a resource or hazard
  (do-game
    (new-game (default-contestant [(qty "Power Shutdown" 3) (qty "Hive" 3)])
              (default-challenger [(qty "Grimoire" 1) (qty "Cache" 1)]))
    (play-from-hand state :contestant "Power Shutdown")
    (is (empty? (:discard (get-contestant))) "Not played, no run last turn")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Cache")
    (play-from-hand state :challenger "Grimoire")
    (run-empty-locale state :archives)
    (take-credits state :challenger)
    (core/move state :contestant (find-card "Hive" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Hive" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Hive" (:hand (get-contestant))) :deck)
    (play-from-hand state :contestant "Power Shutdown")
    (prompt-choice :contestant 2)
    (is (= 3 (count (:discard (get-contestant)))) "2 cards discarded from R&D")
    (is (= 1 (count (:deck (get-contestant)))) "1 card remaining in R&D")
    (prompt-select :challenger (get-in @state [:challenger :rig :hazard 0])) ; try targeting Grimoire
    (is (empty? (:discard (get-challenger))) "Grimoire too expensive to be targeted")
    (prompt-select :challenger (get-in @state [:challenger :rig :resource 0]))
    (is (= 1 (count (:discard (get-challenger)))) "Cache discarded")))

(deftest precognition
  ;; Precognition - Full test
  (do-game
    (new-game (default-contestant [(qty "Precognition" 1) (qty "Caprcharacter Nisei" 1) (qty "Adonis Campaign" 1)
                             (qty "Quandary" 1) (qty "Jackson Howard" 1) (qty "Global Food Initiative" 1)])
              (default-challenger))
    (starting-hand state :contestant ["Precognition"])
    (play-from-hand state :contestant "Precognition")
    (prompt-choice :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
    (prompt-choice :contestant (find-card "Adonis Campaign" (:deck (get-contestant))))
    (prompt-choice :contestant (find-card "Quandary" (:deck (get-contestant))))
    (prompt-choice :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
    (prompt-choice :contestant (find-card "Global Food Initiative" (:deck (get-contestant))))
    ;; try starting over
    (prompt-choice :contestant "Start over")
    (prompt-choice :contestant (find-card "Global Food Initiative" (:deck (get-contestant))))
    (prompt-choice :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
    (prompt-choice :contestant (find-card "Quandary" (:deck (get-contestant))))
    (prompt-choice :contestant (find-card "Adonis Campaign" (:deck (get-contestant))))
    (prompt-choice :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant)))) ;this is the top card of R&D
    (prompt-choice :contestant "Done")
    (is (= "Caprcharacter Nisei" (:title (first (:deck (get-contestant))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-contestant))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-contestant)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-contestant))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-contestant)))))))))))

(deftest preemptive-action
  ;; Preemptive Action - Shuffles cards into R&D and removes itself from game
  (do-game
    (new-game (default-contestant [(qty "Subliminal Messaging" 3)
                             (qty "Preemptive Action" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Subliminal Messaging")
    (play-from-hand state :contestant "Subliminal Messaging")
    (play-from-hand state :contestant "Subliminal Messaging")
    (play-from-hand state :contestant "Preemptive Action")
    (prompt-select :contestant (first (:discard (get-contestant))))
    (prompt-select :contestant (second (:discard (get-contestant))))
    (prompt-select :contestant (last (:discard (get-contestant))))
    (is (= 0 (count (:discard (get-contestant)))))
    (is (= 1 (count (:rfg (get-contestant)))))))

(deftest psychographics
  ;; Psychographics - Place advancements up to the number of Challenger tags on a card
  (do-game
    (new-game (default-contestant [(qty "Psychographics" 1) (qty "Project Junebug" 1)])
              (default-challenger))
    (core/gain state :challenger :tag 4)
    (play-from-hand state :contestant "Project Junebug" "New party")
    (let [pj (get-content state :party1 0)]
      (play-from-hand state :contestant "Psychographics")
      (prompt-choice :contestant 4)
      (prompt-select :contestant pj)
      (is (= 1 (:credit (get-contestant))) "Spent 4 credits")
      (is (= 4 (:advance-counter (refresh pj))) "Junebug has 4 advancements"))))

(deftest psychokinesis
  ;; Pyschokinesis - Terminal Event (end the turn); Look at R&D, place an Site, Agenda, or Region in a Party Locale
  (do-game
    (new-game (default-contestant [(qty "Psychokinesis" 3) (qty "Caprcharacter Nisei" 1) (qty "Adonis Campaign" 1)
                              (qty "Global Food Initiative" 1)])
              (default-challenger))
    (starting-hand state :contestant ["Psychokinesis","Psychokinesis","Psychokinesis"])
    ;; Test placing an Region
    (play-from-hand state :contestant "Psychokinesis")
    (prompt-choice :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
    (prompt-choice :contestant "New party")
    (is (= "Caprcharacter Nisei" (:title (get-content state :party1 0)))
      "Caprcharacter Nisei placed by Psychokinesis")
    ;; Test placing an Site
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Psychokinesis")
    (prompt-choice :contestant (find-card "Adonis Campaign" (:deck (get-contestant))))
    (prompt-choice :contestant "New party")
    (is (= "Adonis Campaign" (:title (get-content state :party2 0)))
      "Adonis Campaign placed by Psychokinesis")
    ;; Test placing an Agenda
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Psychokinesis")
    (prompt-choice :contestant (find-card "Global Food Initiative" (:deck (get-contestant))))
    (prompt-choice :contestant "New party")
    (is (= "Global Food Initiative" (:title (get-content state :party3 0)))
      "Global Food Initiative placed by Psychokinesis")
    ;; Test selecting "None"
    (core/gain state :contestant :click 1)
    (core/move state :contestant (find-card "Psychokinesis" (:discard (get-contestant))) :hand)
    (play-from-hand state :contestant "Psychokinesis")
    (prompt-choice :contestant "None")
    (is (= nil (:title (get-content state :party4 0)))
      "Nothing is placed by Psychokinesis")))

(deftest punitive-counterstrike
  ;; Punitive Counterstrike - deal meat damage equal to printed agenda points
  (do-game
    (new-game (default-contestant [(qty "Global Food Initiative" 1) (qty "Punitive Counterstrike" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Global Food Initiative" "New party")
    (take-credits state :contestant)
    (run-empty-locale state :party1)
    (prompt-choice :challenger "Steal")
    (is (= 2 (:agenda-point (get-challenger))) "Challenger scored 2 points")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Punitive Counterstrike")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (empty? (:hand (get-challenger))) "Challenger took 3 meat damage")))

(deftest reuse
  ;; Reuse - Gain 2 credits for each card discarded from HQ
  (do-game
    (new-game (default-contestant [(qty "Reuse" 2) (qty "Hive" 1) (qty "IQ" 1)
                             (qty "Ice Wall" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Reuse")
    (prompt-select :contestant (find-card "Ice Wall" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "Hive" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "IQ" (:hand (get-contestant))))
    (prompt-choice :contestant "Done")
    (is (= 4 (count (:discard (get-contestant)))) "3 cards discarded plus operation played")
    (is (= 11 (:credit (get-contestant))) "Gained 6 credits")
    (is (= 1 (:click (get-contestant))) "Spent 2 clicks")))

(deftest salems-hospitality
  ;; Salem's Hospitality - Full test
  (do-game
    (new-game (default-contestant [(qty "Salem's Hospitality" 3)])
              (default-challenger [(qty "I've Had Worse" 3) (qty "Faust" 1)
                               (qty "Levy AR Lab Access" 1)]))
    (play-from-hand state :contestant "Salem's Hospitality")
    (is (= 5 (count (:hand (get-challenger)))))
    (prompt-choice :contestant "I've Had Worse")
    (is (= 2 (count (:hand (get-challenger)))))
    (play-from-hand state :contestant "Salem's Hospitality")
    (prompt-choice :contestant "Plascrete Carapace")
    (is (= 2 (count (:hand (get-challenger)))))))

(deftest scorched-earth
  ;; Scorched Earth - burn 'em
  (do-game
    (new-game (default-contestant [(qty "Scorched Earth" 1)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
    (core/gain state :challenger :tag 1)
    (play-from-hand state :contestant "Scorched Earth")
    (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")))

(deftest scorched-earth-no-tag
  ;; Scorched Earth - not tagged
  (do-game
    (new-game (default-contestant [(qty "Scorched Earth" 1)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
    (play-from-hand state :contestant "Scorched Earth")
    (is (= 3 (:click (get-contestant))) "Contestant not charged a click")
    (is (= 5 (count (:hand (get-challenger)))) "Challenger did not take damage")))

(deftest scorched-earth-flatline
  ;; Scorched Earth - murderize 'em
  (do-game
    (new-game (default-contestant [(qty "Scorched Earth" 10)])
              (default-challenger))
    (core/gain state :challenger :tag 1)
    (play-from-hand state :contestant "Scorched Earth")
    (is (= 0 (count (:hand (get-challenger)))) "Challenger has 0 cards in hand")
    (is (= :contestant (:winner @state)) "Contestant wins")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest subcontract-scorched
  ;; Subcontract - Don't allow second operation until damage prevention completes
  (do-game
    (new-game (default-contestant [(qty "Scorched Earth" 2) (qty "Subcontract" 1)])
              (default-challenger [(qty "Plascrete Carapace" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :tag 1)
    (play-from-hand state :challenger "Plascrete Carapace")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Subcontract")
    (prompt-select :contestant (find-card "Scorched Earth" (:hand (get-contestant))))
    (is (and (= 1 (count (:prompt (get-contestant)))) (= :waiting (-> (get-contestant) :prompt first :prompt-type)))
        "Contestant does not have Subcontract prompt until damage prevention completes")
    (prompt-choice :challenger "Done")
    (is (not-empty (:prompt (get-contestant))) "Contestant can now play second Subcontract operation")))

(deftest subcontract-terminal
  ;; Subcontract - interaction with Terminal operations
  (do-game
    (new-game
      (default-contestant [(qty "Hard-Hitting News" 2) (qty "Subcontract" 1)])
      (default-challenger))
    (core/gain state :challenger :tag 1)
    (take-credits state :contestant)
    (run-empty-locale state :archives)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Subcontract")
    (prompt-select :contestant (find-card "Hard-Hitting News" (:hand (get-contestant))))
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (= 5 (:tag (get-challenger))) "Challenger has 5 tags")
    (is (empty? (:prompt (get-contestant))) "Contestant does not have a second Subcontract selection prompt")))

(deftest self-growth-resource
  ;; Self-Growth Resource - Add 2 placed cards to grip if challenger is tagged
  (do-game
    (new-game (default-contestant [(qty "Self-Growth Resource" 1)])
              (default-challenger [(qty "Clone Chip" 1) (qty "Inti" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Clone Chip")
    (play-from-hand state :challenger "Inti")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Self-Growth Resource")
    (is (= 3 (:click (get-contestant))) "Self-Growth Resource precondition not met; card not played")
    (core/gain state :challenger :tag 1)
    (is (= 0 (count (:hand (get-challenger)))) "Challenger hand is empty")
    (let [inti (get-in @state [:challenger :rig :resource 0])
          cc (get-in @state [:challenger :rig :hazard 0])]
      (play-from-hand state :contestant "Self-Growth Resource")
      (prompt-select :contestant inti)
      (prompt-select :contestant cc))
    (is (= 2 (count (:hand (get-challenger)))) "2 cards returned to hand")
    (is (= 0 (count (get-in @state [:challenger :rig :resource]))) "No resources placed")
    (is (= 0 (count (get-in @state [:challenger :rig :hazard]))) "No hazard placed")))

(deftest servcharacter-outage
  ;; Servcharacter Outage - First click run each turn costs a credit
  (do-game
    (new-game (default-contestant [(qty "Servcharacter Outage" 1)])
              (default-challenger [(qty "Employee Strike" 1)]))
    (play-from-hand state :contestant "Servcharacter Outage")
    (take-credits state :contestant)

    (is (= 5 (:credit (get-challenger))) "Challenger has 5 credits")
    (run-on state :archives)
    (is (= 4 (:credit (get-challenger)))
        "Challenger spends 1 credit to make the first run")
    (run-successful state)

    (run-on state :archives)
    (is (= 4 (:credit (get-challenger)))
        "Challenger doesn't spend 1 credit to make the second run")
    (run-successful state)

    (take-credits state :challenger)
    (take-credits state :contestant)

    (core/lose state :challenger :credit 6)
    (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
    (is (= 0 (:credit (get-challenger))) "Challenger has 0 credits")
    (run-on state :archives)
    (is (not (:run @state)) "No run was initiated")
    (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
    (is (= 0 (:credit (get-challenger))) "Challenger has 0 credits")

    (take-credits state :challenger)
    (take-credits state :contestant)

    (core/lose state :challenger :credit 2)
    (play-from-hand state :challenger "Employee Strike")
    (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")

    (run-on state :archives)
    (is (= 1 (:credit (get-challenger)))
        "Challenger doesn't spend 1 credit to make a run")))

(deftest servcharacter-outage-card-ability
  ;; Servcharacter Outage - First card ability run each turn costs an additional credit
  (do-game
    (new-game (default-contestant [(qty "Servcharacter Outage" 1)])
              (default-challenger [(qty "Sneakdoor Beta" 1)]))
    (play-from-hand state :contestant "Servcharacter Outage")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sneakdoor Beta")
    (take-credits state :challenger 1)

    (is (= 2 (:credit (get-challenger))) "Challenger has 2 credits")
    (let [sneakdoor (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger sneakdoor 0)
      (is (= 1 (:credit (get-challenger)))
          "Challenger spends 1 additional credit to run with a card ability")
      (run-successful state)

      (run-on state :archives)
      (is (= 1 (:credit (get-challenger)))
          "Challenger doesn't spend 1 credit to make a run")
      (run-successful state)

      (take-credits state :challenger)
      (take-credits state :contestant)

      (core/lose state :challenger :credit 1)
      (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
      (is (= 0 (:credit (get-challenger))) "Challenger has 0 credits")
      (card-ability state :challenger sneakdoor 0)
      (is (not (:run @state)) "No run was initiated")
      (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
      (is (= 0 (:credit (get-challenger))) "Challenger has 0 credits"))))

(deftest servcharacter-outage-run-events
  ;; Servcharacter Outage - First run event each turn costs an additional credit
  (do-game
    (new-game (default-contestant [(qty "Servcharacter Outage" 1)])
              (default-challenger [(qty "Out of the Ashes" 2)]))
    (play-from-hand state :contestant "Servcharacter Outage")
    (take-credits state :contestant)

    (is (= 5 (:credit (get-challenger))) "Challenger has 5 credits")
    (play-from-hand state :challenger "Out of the Ashes")
    (is (= 3 (:credit (get-challenger)))
        "Challenger spends 1 additional credit to run with a run event")
    (prompt-choice :challenger "Archives")
    (run-successful state)

    (run-on state :archives)
    (is (= 3 (:credit (get-challenger)))
        "Challenger doesn't spend 1 credit to make a run")
    (run-successful state)

    (take-credits state :challenger)
    (take-credits state :contestant)
    (prompt-choice :challenger "No") ; Out of the Ashes prompt

    (core/lose state :challenger :credit 4)
    (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
    (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
    (play-from-hand state :challenger "Out of the Ashes")
    (is (empty? (get-in @state [:challenger :prompt]))
        "Out of the Ashes was not played")
    (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
    (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")))

(deftest servcharacter-outage-challenger-turn-first-run
  ;; Servcharacter Outage - Works when played on the challenger's turn
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News" [(qty "Servcharacter Outage" 1)
                                                       (qty "Breaking News" 1)])
              (default-challenger [(qty "Hades Shard" 1)]))
    (discard-from-hand state :contestant "Breaking News")
    (take-credits state :contestant)

    (core/gain state :challenger :credit 3)
    (play-from-hand state :challenger "Hades Shard")
    (card-ability state :challenger (get-in @state [:challenger :rig :muthereff 0]) 0)
    (prompt-choice :challenger "Steal")
    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (find-card "Servcharacter Outage" (:hand (get-contestant))))
    (is (find-card "Servcharacter Outage" (:current (get-contestant)))
        "Servcharacter Outage is in play")

    (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
    (run-on state :archives)
    (is (= 0 (:credit (get-challenger)))
        "Challenger spends 1 additional credit to make a run")))

(deftest servcharacter-outage-challenger-turn-second-run
  ;; Servcharacter Outage - Doesn't fire if already run when played on the challenger's turn
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News" [(qty "Servcharacter Outage" 1)
                                                       (qty "Breaking News" 1)])
              (default-challenger [(qty "Hades Shard" 1)]))
    (discard-from-hand state :contestant "Breaking News")
    (take-credits state :contestant)

    (run-on state :hq)
    (run-successful state)
    (prompt-choice :challenger "OK")

    (core/gain state :challenger :credit 3)
    (play-from-hand state :challenger "Hades Shard")
    (card-ability state :challenger (get-in @state [:challenger :rig :muthereff 0]) 0)
    (prompt-choice :challenger "Steal")
    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (find-card "Servcharacter Outage" (:hand (get-contestant))))
    (is (find-card "Servcharacter Outage" (:current (get-contestant)))
        "Servcharacter Outage is in play")

    (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
    (run-on state :archives)
    (is (= 1 (:credit (get-challenger)))
        "Challenger doesn't spend 1 additional credit to make a run")))

(deftest servcharacter-outage-new-angeles-sol
  ;; Servcharacter Outage discarded and replaced on steal doesn't double remove penalty
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Servcharacter Outage" 1)
                                               (qty "Breaking News" 1)])
      (default-challenger))
    (play-from-hand state :contestant "Breaking News" "New party")
    (play-from-hand state :contestant "Servcharacter Outage")
    (take-credits state :contestant)

    (run-on state :party1)
    (run-successful state)
    (prompt-choice :challenger "Steal")

    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (find-card "Servcharacter Outage"
                                    (:discard (get-contestant))))

    (take-credits state :challenger)

    (take-credits state :contestant)

    (is (= 7 (:credit (get-challenger))) "Challenger has 7 credits")
    (run-on state :archives)
    (is (= 6 (:credit (get-challenger)))
        "Challenger spends 1 credit to make a run")))

(deftest shipment-from-sansan
  ;; Shipment from SanSan - placing advancements
  (do-game
    (new-game (default-contestant [(qty "Shipment from SanSan" 3) (qty "Ice Wall" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [iwall (get-character state :hq 0)]
      (play-from-hand state :contestant "Shipment from SanSan")
      (prompt-choice :contestant "2")
      (prompt-select :contestant iwall)
      (is (= 5 (:credit (get-contestant))))
      (is (= 2 (:advance-counter (refresh iwall)))))))

(deftest stock-buy-back
  ;; Stock Buy-Back - Gain 3c for every agenda in Challenger's area
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 2) (qty "Stock Buy-Back" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (take-credits state :contestant)
    (run-empty-locale state "Locale 1")
    (prompt-choice :challenger "Steal")
    (run-empty-locale state "Locale 2")
    (prompt-choice :challenger "Steal")
    (take-credits state :challenger)
    (is (= 2 (count (:scored (get-challenger)))))
    (play-from-hand state :contestant "Stock Buy-Back")
    (is (= 11 (:credit (get-contestant))))))

(deftest sub-boost
  ;; Sub Boost - Give Character Barrier
  (do-game
    (new-game (default-contestant [(qty "Sub Boost" 1) (qty "Quandary" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Quandary" "HQ")
    (play-from-hand state :contestant "Sub Boost")
    (let [qu (get-character state :hq 0)]
      (core/reveal state :contestant qu)
      (prompt-select :contestant qu)
      (is (core/has-subtype? (refresh qu) "Code Gate") "Quandary has Code Gate")
      (is (core/has-subtype? (refresh qu) "Barrier") "Quandary Character Barrier"))))

(deftest subliminal-messaging
  ;; Subliminal Messaging - Playing/discarding/milling will all prompt returning to hand
  (do-game
    (new-game (default-contestant [(qty "Subliminal Messaging" 3)])
              (make-deck "Noise: Hacker Extraordinaire" [(qty "Cache" 3) (qty "Utopia Shard" 1)]))
    (play-from-hand state :contestant "Subliminal Messaging")
    (is (= 6 (:credit (get-contestant))))
    (is (= 3 (:click (get-contestant))) "First Subliminal Messaging gains 1 click")
    (play-from-hand state :contestant "Subliminal Messaging")
    (is (= 7 (:credit (get-contestant))))
    (is (= 2 (:click (get-contestant))) "Second Subliminal Messaging does not gain 1 click")
    (discard-from-hand state :contestant "Subliminal Messaging")
    (is (= 0 (count (:hand (get-contestant)))))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant "Yes")
    (is (= 3 (count (:hand (get-contestant)))) "All 3 Subliminals returned to HQ")
    (core/move state :contestant (find-card "Subliminal Messaging" (:hand (get-contestant))) :deck)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Cache")
    (play-from-hand state :challenger "Utopia Shard")
    (let [utopia (get-in @state [:challenger :rig :muthereff 0])]
      (card-ability state :challenger utopia 0))
    (take-credits state :challenger)
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant "Yes")
    (is (= 3 (count (:hand (get-contestant)))) "All 3 Subliminals returned to HQ")
    (play-from-hand state :contestant "Subliminal Messaging")
    (take-credits state :contestant)
    (run-on state "R&D")
    (run-jack-out state)
    (take-credits state :challenger)
    (is (empty? (get-in @state [:contestant :prompt])) "No prompt here because challenger made a run last turn")
    (take-credits state :contestant)
    (is (= 2 (count (:hand (get-contestant)))))
    (is (= 1 (count (:discard (get-contestant)))) "1 Subliminal not returned because challenger made a run last turn")))

(deftest subliminal-messaging-archived
  ;; Subliminal Messaging - Scenario involving Subliminal being added to HQ with Archived Memories
  (do-game
    (new-game (default-contestant [(qty "Subliminal Messaging" 2) (qty "Archived Memories" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Subliminal Messaging")
    (play-from-hand state :contestant "Subliminal Messaging")
    (play-from-hand state :contestant "Archived Memories")
    (prompt-select :contestant (find-card "Subliminal Messaging" (:discard (get-contestant))))
    (is (= 2 (count (:discard (get-contestant)))))
    (is (= 1 (count (:hand (get-contestant)))))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (prompt-choice :contestant "No")
    (is (empty? (get-in @state [:contestant :prompt])) "Only 1 Subliminal prompt")
    (play-from-hand state :contestant "Subliminal Messaging")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant "Yes")
    (is (empty? (get-in @state [:contestant :prompt]))
        "Only 2 Subliminal prompts - there will be a third if flag not cleared")))

(deftest subliminal-messaging-jackson
  ;; Subliminal Messaging - Scenario involving Subliminal being reshuffled into R&D with Jackson
  (do-game
    (new-game (default-contestant [(qty "Subliminal Messaging" 1) (qty "Jackson Howard" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Subliminal Messaging")
    (play-from-hand state :contestant "Jackson Howard" "New party")
    (take-credits state :contestant)
    (let [jhow (get-content state :party1 0)]
      (core/reveal state :contestant jhow)
      (card-ability state :contestant jhow 1)
      (prompt-select :contestant (find-card "Subliminal Messaging" (:discard (get-contestant))))
      (prompt-choice :contestant "Done")
      (is (= 0 (count (:discard (get-contestant)))))
      (is (= 1 (count (:rfg (get-contestant))))))
    (take-credits state :challenger)
    (play-from-hand state :contestant "Subliminal Messaging")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (prompt-choice :contestant "Yes")
    (is (= 1 (count (:hand (get-contestant)))) "Subliminal returned to HQ")
    (is (empty? (get-in @state [:contestant :prompt]))
        "Subliminal prompt cleared - there will be a second prompt if flag not cleared")))

(deftest subliminal-messaging-made-run
  ;; Subliminal Messaging - Challenger made run, ensure game asks again next turn
  (do-game
    (new-game (default-contestant [(qty "Subliminal Messaging" 2)])
              (default-challenger))
  (play-from-hand state :contestant "Subliminal Messaging")
  (discard-from-hand state :contestant "Subliminal Messaging")
  (take-credits state :contestant)
  (run-on state "R&D")
  (run-jack-out state)
  (take-credits state :challenger)
  (is (empty? (get-in @state [:contestant :prompt])) "No prompt here because challenger made a run last turn")
  (take-credits state :contestant)
  (take-credits state :challenger)
  (prompt-choice :contestant "Yes")
  (prompt-choice :contestant "Yes")
  (is (= 2 (count (:hand (get-contestant)))) "Both Subliminals returned to HQ")
  (is (= 0 (count (:discard (get-contestant)))) "No Subliminals in Archives")))

(deftest subliminal-messaging-no
  ;; Subliminal Messaging - User declines to return to hand, ensure game asks again next turn
  (do-game
    (new-game (default-contestant [(qty "Subliminal Messaging" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Subliminal Messaging")
    (discard-from-hand state :contestant "Subliminal Messaging")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (prompt-choice :contestant "No")
    (prompt-choice :contestant "No")
    (is (= 0 (count (:hand (get-contestant)))) "Neither Subliminal returned to HQ")
    (is (= 2 (count (:discard (get-contestant)))) "Both Subliminals in Archives")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant "Yes")
    (is (= 2 (count (:hand (get-contestant)))) "Both Subliminals returned to HQ")
    (is (= 0 (count (:discard (get-contestant)))) "No Subliminals in Archives")))

(deftest success-bad-publicity
  ;; Success - Works with bad publicity
  (do-game
    (new-game (default-contestant [(qty "NAPD Contract" 1) (qty "Project Beale" 1) (qty "Success" 1)])
              (default-challenger))
    (play-from-hand state :contestant "NAPD Contract" "New party")
    (play-from-hand state :contestant "Project Beale" "New party")
    (core/gain state :contestant :bad-publicity 9)
    (core/gain state :contestant :credit 8)
    (core/gain state :contestant :click 15)
    (let [napd (get-content state :party1 0)
          beale (get-content state :party2 0)]
      (dotimes [_ 13] (core/advance state :contestant {:card (refresh napd)}))
      (is (= 13 (:advance-counter (refresh napd))))
      (core/score state :contestant {:card (refresh napd)})
      (is (= 2 (:agenda-point (get-contestant))))
      (play-from-hand state :contestant "Success")
      (prompt-select :contestant (get-scored state :contestant 0))
      (is (= "NAPD Contract" (:title (first (:rfg (get-contestant))))))
      (prompt-select :contestant (refresh beale))
      (is (= 13 (:advance-counter (refresh beale))))
      (core/score state :contestant {:card (refresh beale)})
      (is (= 7 (:agenda-point (get-contestant)))))))

(deftest success-public-agenda
  ;; Success - Works with public agendas
  (do-game
    (new-game (default-contestant [(qty "Oaktown Renovation" 1) (qty "Vanity Project" 1) (qty "Success" 1)])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (score-agenda state :contestant (find-card "Vanity Project" (:hand (get-contestant))))
    (is (= 4 (:agenda-point (get-contestant))))
    (play-from-hand state :contestant "Oaktown Renovation" "New party")
    (is (= 5 (:credit (get-contestant))))
    (play-from-hand state :contestant "Success")
    (prompt-select :contestant (get-scored state :contestant))
    (is (= "Vanity Project" (:title (first (:rfg (get-contestant))))))
    (let [oaktown (get-content state :party1 0)]
      (prompt-select :contestant (refresh oaktown))
      (is (= 6 (:advance-counter (refresh oaktown))))
      (is (= 19 (:credit (get-contestant))) "Gain 2 + 2 + 2 + 2 + 3 + 3 = 14 credits for advancing Oaktown")
      (core/score state :contestant {:card (refresh oaktown)})
      (is (= 2 (:agenda-point (get-contestant)))))))

(deftest success-jemison
  ;; Success interaction with Jemison, regression test for issue #2704
  (do-game
    (new-game (make-deck "Jemison Astronautics: Sacrifcharacter. Audacity. Success."
                         [(qty "Success" 1)
                          (qty "High-Risk Investment" 1)
                          (qty "Government Takeover" 1)])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (score-agenda state :contestant (find-card "High-Risk Investment" (:hand (get-contestant))))
    (play-from-hand state :contestant "Government Takeover" "New party")
    (play-from-hand state :contestant "Success")
    (prompt-select :contestant (get-in (get-contestant) [:scored 0]))
    (let [gto (get-content state :party1 0)]
      ;; Prompt for Success
      (prompt-select :contestant (refresh gto))
      (is (= 5 (:advance-counter (refresh gto))) "Advance 5 times from Success")
      ;; Prompt for Jemison
      (prompt-select :contestant (refresh gto))
      (is (= 9 (:advance-counter (refresh gto))) "Added 4 counters from Jemison trigger"))))

(deftest successful-demonstration
  ;; Successful Demonstration - Play if only Challenger made unsuccessful run last turn; gain 7 credits
  (do-game
    (new-game (default-contestant [(qty "Successful Demonstration" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Successful Demonstration")
    (is (and (= 3 (:click (get-contestant)))
             (= 5 (:credit (get-challenger))))
        "Successful Demonstration precondition not met; card not played")
    (take-credits state :contestant)
    (run-on state "R&D")
    (run-jack-out state)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Successful Demonstration")
    (is (= 13 (:credit (get-contestant))) "Paid 2 to play event; gained 7 credits")))

(deftest threat-assessment
  ;; Threat Assessment - play only if challenger discarded a card last turn, move a card to the stack or take 2 tags
  (do-game
    (new-game (default-contestant [(qty "Threat Assessment" 3) (qty "Adonis Campaign" 1)])
              (default-challenger [(qty "Desperado" 1) (qty "Corroder" 1)]))
    (play-from-hand state :contestant "Adonis Campaign" "New party")
    (take-credits state :contestant)

    (run-on state :party1)
    (run-successful state)
    (prompt-choice :challenger "Yes") ;discard
    (core/gain state :challenger :credit 5)
    (play-from-hand state :challenger "Desperado")
    (play-from-hand state :challenger "Corroder")
    (take-credits state :challenger)

    (is (= 0 (:tag (get-challenger))) "Challenger starts with 0 tags")
    (play-from-hand state :contestant "Threat Assessment")
    (prompt-select :contestant (find-card "Desperado" (-> (get-challenger) :rig :hazard)))
    (prompt-choice :challenger "2 tags")
    (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags")
    (is (= 1 (count (-> (get-challenger) :rig :hazard))) "Didn't discard Desperado")
    (is (= "Threat Assessment" (:title (first (:rfg (get-contestant))))) "Threat Assessment removed from game")

    (play-from-hand state :contestant "Threat Assessment")
    (prompt-select :contestant (find-card "Corroder" (-> (get-challenger) :rig :resource)))
    (prompt-choice :challenger "Move Corroder")
    (is (= 2 (:tag (get-challenger))) "Challenger didn't take tags")
    (is (= "Corroder" (:title (first (:deck (get-challenger))))) "Moved Corroder to the deck")
    (is (= 2 (count (:rfg (get-contestant)))))
    (take-credits state :challenger)

    (take-credits state :contestant)
    (take-credits state :challenger)

    (play-from-hand state :contestant "Threat Assessment")
    (is (empty? (:prompt (get-contestant))) "Threat Assessment triggered with no discard")))

(deftest transparency-initiative
  ;; Transparency Initiative - Full test
  (do-game
    (new-game (default-contestant [(qty "Transparency Initiative" 1) (qty "Oaktown Renovation" 1)
                             (qty "Project Atlas" 1) (qty "Hostile Takeover" 1) (qty "Casting Call" 1)])
              (default-challenger))
    (core/gain state :contestant :click 5)
    (play-from-hand state :contestant "Oaktown Renovation" "New party")
    (play-from-hand state :contestant "Casting Call")
    (prompt-select :contestant (find-card "Project Atlas" (:hand (get-contestant))))
    (prompt-choice :contestant "New party")
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (let [oaktown (get-content state :party1 0)
          atlas (get-content state :party2 0)
          hostile (get-content state :party3 0)]
      (play-from-hand state :contestant "Transparency Initiative")
      (prompt-select :contestant (refresh oaktown))
      ;; doesn't work on face-up agendas
      (is (= 0 (count (:hosted (refresh oaktown)))))
      (prompt-select :contestant (refresh atlas))
      (is (= 1 (count (:hosted (refresh atlas)))) "Casting Call")
      ;; works on facedown agenda
      (prompt-select :contestant (refresh hostile))
      (is (= 1 (count (:hosted (refresh hostile)))))
      ;; gains Public subtype
      (is (core/has-subtype? (refresh hostile) "Public"))
      ;; gain 1 credit when advancing
      (is (= 5 (:credit (get-contestant))))
      (core/advance state :contestant {:card (refresh hostile)})
      (is (= 5 (:credit (get-contestant))))
      ;; make sure advancing other agendas doesn't gain 1
      (core/advance state :contestant {:card (refresh oaktown)})
      (is (= 6 (:credit (get-contestant))) "Transparency initiative didn't fire")
      (core/advance state :contestant {:card (refresh atlas)})
      (is (= 5 (:credit (get-contestant))) "Transparency initiative didn't fire"))))

(deftest wetwork-refit
  ;; Wetwork Refit - Only works on Bioroid Character and adds a subroutine
  (do-game
    (new-game (default-contestant [(qty "Eli 1.0" 1)
                             (qty "Vanilla" 1)
                             (qty "Wetwork Refit" 3)])
              (default-challenger))
    (core/gain state :contestant :credit 20)
    (core/gain state :contestant :click 10)
    (play-from-hand state :contestant "Eli 1.0" "R&D")
    (play-from-hand state :contestant "Vanilla" "HQ")
    (let [eli (get-character state :rd 0)
          vanilla (get-character state :hq 0)]
      (play-from-hand state :contestant "Wetwork Refit")
      (is (not-any? #{"Eli 1.0"} (get-in @state [:contestant :prompt :choices]))
          "Unrevealed Eli 1.0 is not a choice to host Wetwork Refit")
      (prompt-choice :contestant "Done")

      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/reveal state :contestant (refresh eli))
      (core/reveal state :contestant (refresh vanilla))

      (play-from-hand state :contestant "Wetwork Refit")
      (prompt-select :contestant (refresh eli))
      (is (= "Wetwork Refit" (:title (first (:hosted (refresh eli)))))
          "Wetwork Refit is hosted on Eli 1.0")
      (is (= 2 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 2 different subroutines")
      (is (= "Do 1 brain damage" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has a brain damage subroutine as his first subroutine")

      (core/move state :contestant (first (:hosted (refresh eli))) :hand)
      (is (empty? (:hosted (refresh eli))) "No cards are hosted on Eli 1.0")
      (is (= 1 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 1 different subroutine")
      (is (= "End the run" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has an end the run subroutine as his first subroutine")

      (play-from-hand state :contestant "Wetwork Refit")
      (prompt-select :contestant (refresh vanilla))
      (is (not= "Wetwork Refit" (:title (first (:hosted (refresh vanilla)))))
          "Wetwork Refit is not hosted on Vanilla"))))
