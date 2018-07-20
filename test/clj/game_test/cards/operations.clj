(ns game-test.cards.operations
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "operations"))

(deftest ^{:card-title "24/7-news-cycle"}
  twenty-four-seven-news
  ;; 24/7 News Cycle
  (testing "Breaking News interaction"
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
        (is (zero? (:tag (get-challenger)))) ; tags cleared
        (take-credits state :challenger)
        (play-from-hand state :contestant "24/7 News Cycle")
        (click-card state :contestant (find-card "Breaking News" (:scored (get-contestant))))
        (is (= 1 (:agenda-point (get-contestant))) "Forfeited Breaking News")
        (click-card state :contestant (find-card "Breaking News" (:scored (get-contestant))))
        (is (= 2 (:tag (get-challenger))) "Challenger given 2 tags")
        (take-credits state :contestant 2)
        (is (= 2 (:tag (get-challenger))) "Tags remained after Contestant ended turn"))))
  (testing "Posted Bounty interaction -- Issue #1043"
    (do-game
      (new-game (default-contestant [(qty "Posted Bounty" 2) (qty "24/7 News Cycle" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Posted Bounty" "New party")
      (play-from-hand state :contestant "Posted Bounty" "New party")
      (let [ag1 (get-content state :party1 0)
            ag2 (get-content state :party2 0)]
        (score-agenda state :contestant ag1)
        (click-prompt state :contestant "No")
        (score-agenda state :contestant ag2)
        (click-prompt state :contestant "No")
        (play-from-hand state :contestant "24/7 News Cycle")
        (click-card state :contestant (find-card "Posted Bounty" (:scored (get-contestant))))
        (is (= 1 (:agenda-point (get-contestant))) "Forfeited Posted Bounty")
        (click-card state :contestant (find-card "Posted Bounty" (:scored (get-contestant))))
        (click-prompt state :contestant "Yes") ; "Forfeit Posted Bounty to give 1 tag?"
        (is (= 1 (:tag (get-challenger))) "Challenger given 1 tag")
        (is (= 1 (:bad-publicity (get-contestant))) "Contestant has 1 bad publicity")
        (is (zero? (:agenda-point (get-contestant))) "Forfeited Posted Bounty to 24/7 News Cycle"))))
  (testing "Swapped agendas are able to be used. #1555"
    (do-game
      (new-game (default-contestant ["24/7 News Cycle" "Chronos Project"
                               "Philotic Entanglement" "Profiteering"])
                (default-challenger [(qty "Turntable" 3)]))
      (score-agenda state :contestant (find-card "Chronos Project" (:hand (get-contestant))))
      (score-agenda state :contestant (find-card "Philotic Entanglement" (:hand (get-contestant))))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Turntable")
      (core/steal state :challenger (find-card "Profiteering" (:hand (get-contestant))))
      (click-prompt state :challenger "Yes")
      (click-card state :challenger (find-card "Philotic Entanglement" (:scored (get-contestant))))
      (is (= 2 (:agenda-point (get-contestant))))
      (is (= 2 (:agenda-point (get-challenger))))
      (take-credits state :challenger)
      (play-from-hand state :contestant "24/7 News Cycle")
      (click-card state :contestant (find-card "Chronos Project" (:scored (get-contestant))))
      (is (= "Chronos Project" (:title (first (:rfg (get-contestant))))))
      ;; shouldn't work on an agenda in the Challenger's scored area
      (is (= 2 (count (:hand (get-challenger)))))
      (click-card state :contestant (find-card "Philotic Entanglement" (:scored (get-challenger))))
      (is (= 2 (count (:hand (get-challenger)))))
      ;; resolve 'when scored' ability on swapped Profiteering
      (is (= 8 (:credit (get-contestant))))
      (click-card state :contestant (find-card "Profiteering" (:scored (get-contestant))))
      (click-prompt state :contestant "3")
      (is (= 1 (:agenda-point (get-contestant))))
      (is (= 3 (:bad-publicity (get-contestant))))
      (is (= 23 (:credit (get-contestant))) "Gained 15 credits"))))

(deftest accelerated-diagnostics
  ;; Accelerated Diagnostics - Interaction with prompt effects, like Shipment from SanSan
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Accelerated Diagnostics" "Cerebral Overwriter" "Shipment from SanSan"
                               "Hedge Fund" "Back Channels"])
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
        (click-card state :contestant ss)
        (click-prompt state :contestant "2")
        (click-card state :contestant co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (click-card state :contestant hf)
        (is (= 9 (:credit (get-contestant))) "Contestant gained credits from Hedge Fund")
        (click-card state :contestant bc)
        (click-card state :contestant (refresh co))
        (is (= 15 (:credit (get-contestant))) "Contestant gained 6 credits for Back Channels"))))
  (testing "Interaction with Current"
    (do-game
      (new-game (default-contestant ["Accelerated Diagnostics" "Cerebral Overwriter"
                               "Enhanced Login Protocol" "Shipment from SanSan"
                               "Hedge Fund"])
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
        (click-card state :contestant elp)
        (is (= "Enhanced Login Protocol" (:title (first (get-in @state [:contestant :current]))))
            "Enhanced Login Protocol active in Current area")
        (click-card state :contestant ss)
        (click-prompt state :contestant "2")
        (click-card state :contestant co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (click-card state :contestant hf)
        (is (= 9 (:credit (get-contestant))) "Contestant gained credits from Hedge Fund")))))

(deftest an-offer-you-can't-refuse
  ;; An Offer You Can't Refuse - exact card added to score area, not the last discarded one
  (do-game
    (new-game (default-contestant ["Celebrity Gift" "An Offer You Can't Refuse"])
              (default-challenger))
    (play-from-hand state :contestant "An Offer You Can't Refuse")
    (click-prompt state :contestant "R&D")
    (core/move state :contestant (find-card "Celebrity Gift" (:hand (get-contestant))) :discard)
    (is (= 2 (count (:discard (get-contestant)))))
    (click-prompt state :challenger "No")
    (is (= 1 (:agenda-point (get-contestant))) "An Offer the Challenger refused")
    (is (= 1 (count (:scored (get-contestant)))))
    (is (find-card "An Offer You Can't Refuse" (:scored (get-contestant))))
    (is (= 1 (count (:discard (get-contestant)))))
    (is (find-card "Celebrity Gift" (:discard (get-contestant))))))

(deftest attitude-adjustment
  ;; Attitude Adjustment
  (do-game
    (new-game (default-contestant ["Attitude Adjustment"
                             (qty "Hostile Takeover" 2)
                             (qty "Ice Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant ["Attitude Adjustment" "Hostile Takeover" "Hostile Takeover"])
    (discard-from-hand state :contestant "Hostile Takeover")
    (let [hand (-> (get-contestant) :hand count dec)] ;; cuz we're playing Attitude Adjustment
      (play-from-hand state :contestant "Attitude Adjustment")
      (is (= (+ 2 hand) (-> (get-contestant) :hand count)) "Contestant should draw 2 cards"))
    (let [credits (-> (get-contestant) :credit)
          hand (-> (get-contestant) :hand count)
          discard (-> (get-contestant) :discard count)
          deck (-> (get-contestant) :deck count)]
      (click-card state :contestant (find-card "Hostile Takeover" (:hand (get-contestant))))
      (click-card state :contestant (find-card "Hostile Takeover" (:discard (get-contestant))))
      (is (= (+ 4 credits) (:credit (get-contestant))) "Contestant should gain 4 [Credits] for two revealed agendas")
      (is (= (dec hand) (-> (get-contestant) :hand count)) "One card from HQ is shuffled into R&D")
      (is (= (dec discard) (-> (get-contestant) :discard count)) "One card from Archives should be shuffled into R&D")
      (is (= (+ 2 deck) (-> (get-contestant) :deck count)) "Contestant should draw two cards and shuffle two cards into R&D"))))

(deftest biased-reporting
  ;; Biased Reporting
  (do-game
    (new-game (default-contestant ["Biased Reporting"])
              (default-challenger [(qty "Fan Site" 5)]))
    (take-credits state :contestant)
    (starting-hand state :challenger (repeat 5 "Fan Site"))
    (core/gain state :challenger :click 10)
    (dotimes [_ 5]
      (play-from-hand state :challenger "Fan Site"))
    (take-credits state :challenger)
    (play-from-hand state :contestant "Biased Reporting")
    (let [cc (:credit (get-contestant))
          rc (:credit (get-challenger))]
      (click-prompt state :contestant "Radicle")
      (click-card state :challenger (get-radicle state 0))
      (click-prompt state :challenger "Done")
      (is (= (inc rc) (:credit (get-challenger))) "Challenger should gain 1 credit for discarding a Fan Site")
      (is (= (+ (* 4 2) cc) (:credit (get-contestant))) "Contestant should gain 8 credits for remaining 4 Fan Sites"))))

(deftest big-brother
  ;; Big Brother - Give the Challenger 2 tags if already tagged
  (do-game
    (new-game (default-contestant ["Big Brother"])
              (default-challenger))
    (play-from-hand state :contestant "Big Brother")
    (is (= 1 (count (:hand (get-contestant)))) "Card not played because Challenger has no tags")
    (core/gain state :challenger :tag 1)
    (play-from-hand state :contestant "Big Brother")
    (is (= 3 (:tag (get-challenger))) "Challenger gained 2 tags")))

(deftest biotic-labor
  ;; Biotic Labor - Gain 2 clicks
  (do-game
    (new-game (default-contestant ["Biotic Labor"])
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

(deftest building-blocks
  ;; Building Blocks - place and reveal a barrier from HQ at no cost
  (testing "Basic behavior"
    (do-game
      (new-game (default-contestant ["Building Blocks" "Ice Wall"])
                (default-challenger))
      (core/gain state :contestant :credit 1)
      (is (= 6 (:credit (get-contestant))) "Contestant starts with 6 credits")
      (play-from-hand state :contestant "Building Blocks")
      (is (= 1 (:credit (get-contestant))) "Spent 5 credits on Building Blocks")
      (click-card state :contestant (find-card "Ice Wall" (:hand (get-contestant))))
      (click-prompt state :contestant "New party")
      (let [iw (get-character state :party1 0)]
        (is (= 1 (:credit (get-contestant))) "Contestant spent no credits placing character")
        (is (:revealed (refresh iw)) "Ice Wall is placed and revealed"))))
  (testing "Select invalid card"
    (do-game
      (new-game (default-contestant ["Building Blocks" "Hedge Fund" "Cortex Lock"])
                (default-challenger))
      (core/gain state :contestant :credit 1)
      (play-from-hand state :contestant "Building Blocks")
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-contestant))))) "Starting prompt is correct")
      (click-card state :contestant (find-card "Hedge Fund" (:hand (get-contestant))))
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-contestant))))) "Cannot select non-Character")
      (click-card state :contestant (find-card "Cortex Lock" (:hand (get-contestant))))
      (is (= "Select a target for Building Blocks" (:msg (first (:prompt (get-contestant))))) "Cannot select non-barrier Character"))))

(deftest casting-call
  ;; Casting Call - Only do card-init on the Public agendas.  Issue #1128
  (do-game
    (new-game (default-contestant [(qty "Casting Call" 2) "Oaktown Renovation"
                             "Improved Tracers" "Hunter"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Hunter" "HQ")
    (let [hunter (get-character state :hq 0)]
      (core/reveal state :contestant hunter)
      (is (= 4 (:current-strength (refresh hunter))))
      (play-from-hand state :contestant "Casting Call")
      (click-card state :contestant (find-card "Improved Tracers" (:hand (get-contestant))))
      (click-prompt state :contestant "New party")
      (let [imptrac (get-content state :party1 0)]
        (is (:revealed (refresh imptrac)) "Improved Tracers is faceup")
        (is (= 4 (:current-strength (refresh hunter))) "Hunter hasn't gained strength")
        (play-from-hand state :contestant "Casting Call")
        (click-card state :contestant (find-card "Oaktown Renovation" (:hand (get-contestant))))
        (click-prompt state :contestant "New party")
        (let [oak (get-content state :party2 0)]
          (core/advance state :contestant {:card (refresh oak)})
          (is (= 5 (:credit (get-contestant))) "Events on Public agenda work; gained 2 credits from advancing")
          (take-credits state :contestant)
          (run-empty-locale state "Locale 2")
          (click-card state :challenger oak)
          (click-prompt state :challenger "Steal")
          (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags from accessing agenda with Casting Call hosted on it"))))))

(deftest cerebral-cast
  ;; Cerebral Cast
  (testing "Challenger wins"
    (do-game
      (new-game (default-contestant ["Cerebral Cast"])
                (default-challenger))
      (play-from-hand state :contestant "Cerebral Cast")
      (is (= 3 (:click (get-contestant))) "Cerebral Cast precondition not met; card not played")
      (take-credits state :contestant)
      (run-empty-locale state "Archives")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Cerebral Cast")
      (click-prompt state :contestant "0 [Credits]")
      (click-prompt state :challenger "0 [Credits]")
      (is (zero? (count (:discard (get-challenger)))) "Challenger took no damage")
      (is (zero? (:tag (get-challenger))) "Challenger took no tags")))
  (testing "Contestant wins"
    (do-game
      (new-game (default-contestant [(qty "Cerebral Cast" 2)])
                (default-challenger))
      (take-credits state :contestant)
      (run-empty-locale state "Archives")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Cerebral Cast")
      (click-prompt state :contestant "0 [Credits]")
      (click-prompt state :challenger "1 [Credits]")
      (click-prompt state :challenger "1 brain damage")
      (is (= 1 (count (:discard (get-challenger)))) "Challenger took a brain damage")
      (is (zero? (:tag (get-challenger))) "Challenger took no tags from brain damage choice")
      (play-from-hand state :contestant "Cerebral Cast")
      (click-prompt state :contestant "0 [Credits]")
      (click-prompt state :challenger "1 [Credits]")
      (click-prompt state :challenger "1 tag")
      (is (= 1 (count (:discard (get-challenger)))) "Challenger took no additional damage")
      (is (= 1 (:tag (get-challenger))) "Challenger took a tag from Cerebral Cast choice"))))

(deftest cerebral-static
  ;; Cerebral Static
  (testing "vs Chaos Theory"
    (do-game
      (new-game (default-contestant ["Cerebral Static" "Lag Time"])
                (make-deck "Chaos Theory: Wünderkind" [(qty "Sure Gamble" 3)]))
      (is (= 5 (core/available-mu state)) "CT starts with 5 memory")
      (play-from-hand state :contestant "Cerebral Static")
      (is (= 4 (core/available-mu state)) "Cerebral Static causes CT to have 4 memory")
      (play-from-hand state :contestant "Lag Time")
      (is (= 5 (core/available-mu state)) "CT 5 memory restored"))))

(deftest closed-accounts
  ;; Closed Accounts - Play if Challenger is tagged to make Challenger lose all credits
  (do-game
    (new-game (default-contestant ["Closed Accounts"])
              (default-challenger))
    (play-from-hand state :contestant "Closed Accounts")
    (is (and (= 3 (:click (get-contestant)))
             (= 5 (:credit (get-challenger))))
        "Closed Accounts precondition not met; card not played")
    (core/gain state :challenger :tag 1)
    (play-from-hand state :contestant "Closed Accounts")
    (is (zero? (:credit (get-challenger))) "Challenger lost all credits")))

(deftest commercialization
  ;; Commercialization
  (testing "Single advancement token"
    (do-game
      (new-game (default-contestant ["Commercialization"
                               "Ice Wall"])
                (default-challenger))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/add-counter state :contestant (refresh (get-character state :hq 0)) :advancement 1)
      (play-from-hand state :contestant "Commercialization")
      (click-card state :contestant (refresh (get-character state :hq 0)))
      (is (= 6 (:credit (get-contestant))) "Gained 1 for single advanced character from Commercialization")))
  (testing "Two advancement tokens"
    (do-game
      (new-game (default-contestant ["Commercialization"
                               "Ice Wall"])
                (default-challenger))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/add-counter state :contestant (refresh (get-character state :hq 0)) :advancement 2)
      (play-from-hand state :contestant "Commercialization")
      (click-card state :contestant (refresh (get-character state :hq 0)))
      (is (= 7 (:credit (get-contestant))) "Gained 2 for double advanced character from Commercialization"))))

(deftest consulting-visit
  ;; Consulting Visit - Only show single copies of operations contestant can afford as choices. Play chosen operation
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Consulting Visit"
                               (qty "Beanstalk Royalties" 2)
                               "Green Level Clearance"
                               "Breaking News"
                               "Hedge Fund"])
                (default-challenger))
      (is (= 5 (:credit (get-contestant))))
      (starting-hand state :contestant ["Consulting Visit"])
      (play-from-hand state :contestant "Consulting Visit")
      (let [get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
        (click-prompt state :contestant (find-card "Beanstalk Royalties" (:deck (get-contestant))))
        (is (= 6 (:credit (get-contestant)))))))
  (testing "Works properly when played with Mumbad City Hall"
    (do-game
      (new-game (default-contestant ["Mumbad City Hall"
                               "Beanstalk Royalties"
                               "Green Level Clearance"
                               "Breaking News"
                               "Hedge Fund"
                               "Consulting Visit"
                               "Mumba Temple"])
                (default-challenger))
      (is (= 5 (:credit (get-contestant))))
      (starting-hand state :contestant ["Mumbad City Hall"])
      (play-from-hand state :contestant "Mumbad City Hall" "New party")
      (let [hall (get-content state :party1 0)
            get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
            prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
        (card-ability state :contestant hall 0)
        (is (= (list "Consulting Visit" "Mumba Temple" nil) (prompt-names)))
        (click-prompt state :contestant (find-card "Consulting Visit" (:deck (get-contestant))))
        (is (= 3 (:credit (get-contestant))))
        (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
        (click-prompt state :contestant (find-card "Green Level Clearance" (:deck (get-contestant))))
        (is (= 5 (:credit (get-contestant))))))))

(deftest death-and-taxes
  ;; Death and Taxes gain credit on challenger place, challenger discard placed card
  ;; Also regression test for #3160
  (do-game
    (new-game (default-contestant ["Death and Taxes" "PAD Campaign"])
              (default-challenger ["Aumakua" "DaVinci" "Fall Guy"]))
    (play-from-hand state :contestant "Death and Taxes")
    (is (= (- 5 2) (:credit (get-contestant))) "Contestant paid 2 to play Death and Taxes")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (take-credits state :contestant)
    (let [contestant-creds (:credit (get-contestant))]
      (discard-from-hand state :challenger "DaVinci")
      (is (= contestant-creds (:credit (get-contestant))) "Contestant did not gain credit when challenger discards / discards from hand")
      (play-from-hand state :challenger "Aumakua")
      (is (= (+ 1 contestant-creds) (:credit (get-contestant))) "Contestant gained 1 when challenger placed Aumakua")
      (play-from-hand state :challenger "Fall Guy")
      (is (= (+ 2 contestant-creds) (:credit (get-contestant))) "Contestant gained 1 when challenger placed Fall Guy")
      (card-ability state :challenger (get-radicle state 0) 1)
      (is (= (+ 3 contestant-creds) (:credit (get-contestant))) "Contestant gained 1 when challenger discarded Fall Guy")
      (run-empty-locale state :party1)
      (click-prompt state :challenger "Pay 4 [Credits] to discard")
      (is (= (+ 4 contestant-creds) (:credit (get-contestant))) "Contestant gained 1 when challenger discarded PAD Campaign"))))

(deftest defective-brainchips
  ;; Defective Brainchips - Do 1 add'l brain damage the first time Challenger takes some each turn
  (do-game
    (new-game (default-contestant ["Defective Brainchips" "Viktor 1.0"])
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
    (click-card state :contestant (first (:hand (get-contestant))))
    (click-card state :contestant (first (next (:hand (get-contestant)))))
    (click-card state :contestant (first (:discard (get-contestant))))
    (click-prompt state :contestant "Done")
    (is (= 1 (count (:discard (get-contestant)))) "1 card still discarded")
    (is (= 1 (count (:deck (get-contestant)))) "1 card shuffled into R&D")
    (is (= 1 (count (:rfg (get-contestant)))) "Distract the Masses removed from game")
    (is (= 7 (:credit (get-challenger))) "Challenger gained 2 credits")
    (play-from-hand state :contestant "Distract the Masses")
    (click-card state :contestant (first (:hand (get-contestant))))
    (click-prompt state :contestant "Done")
    (click-card state :contestant (first (:discard (get-contestant))))
    (click-card state :contestant (first (next (:discard (get-contestant)))))
    (is (zero? (count (:discard (get-contestant)))) "No cards left in archives")
    (is (= 3 (count (:deck (get-contestant)))) "2 more cards shuffled into R&D")
    (is (= 2 (count (:rfg (get-contestant)))) "Distract the Masses removed from game")
    (is (= 9 (:credit (get-challenger))) "Challenger gained 2 credits")))

(deftest diversified-portfolio
  (do-game
    (new-game (default-contestant ["Diversified Portfolio"
                             "Paper Wall"
                             (qty "PAD Campaign" 3)])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Paper Wall" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (play-from-hand state :contestant "Diversified Portfolio")
    (is (= 7 (:credit (get-contestant))) "Ignored party with Character but no locale contents")))

(deftest divert-power
  (do-game
    (new-game (default-contestant [(qty "Divert Power" 2) "Paper Wall" (qty "Eve Campaign" 3) ])
              (default-challenger))
    (core/gain state :contestant :click 3 :credit 11)
    (play-from-hand state :contestant "Paper Wall" "HQ")
    (play-from-hand state :contestant "Eve Campaign" "New party")
    (play-from-hand state :contestant "Eve Campaign" "New party")
    (play-from-hand state :contestant "Eve Campaign" "New party")
    (let [pw (get-character state :hq 0)
          ec1 (get-content state :party1 0)
          ec2 (get-content state :party2 0)
          ec3 (get-content state :party3 0)]
      (core/reveal state :contestant pw)
      (core/reveal state :contestant ec1)
      (core/reveal state :contestant ec2)
      (play-from-hand state :contestant "Divert Power")
      (is (= 4 (:credit (get-contestant))) "Contestant has 4 credits after reveals and playing Divert Power")
      (testing "Choose 2 targets to hide"
        (click-card state :contestant (refresh pw))
        (click-card state :contestant (refresh ec1))
        (click-prompt state :contestant "Done"))

      (testing "Choose a target to reveal for -6 cost"
        (click-card state :contestant (refresh ec3)))

      (is (core/revealed? (refresh ec3)) "Eve Campaign was revealed")
      (is (= 4 (:credit (get-contestant))) "Revealed Eve Campaign for 0 credits")
      (is (not (core/revealed? (refresh pw))) "Paper Wall was hidden")
      (is (not (core/revealed? (refresh ec1))) "First Eve Campaign was hidden")
      (is (= 16 (get-counters (refresh ec3) :credit)) "Eve gained 16 credits on reveal")

      (play-from-hand state :contestant "Divert Power")
      (testing "Choose 1 target to hide"
        (click-card state :contestant (refresh ec2))
        (click-prompt state :contestant "Done"))

      (testing "Choose a target to reveal for -3 cost"
        (click-card state :contestant (refresh ec1)))

      (is (core/revealed? (refresh ec1)) "First Eve Campaign was revealed")
      (is (= 0 (:credit (get-contestant))) "Revealed Eve Campaign for 2 credits")
      (is (not (core/revealed? (refresh ec2))) "Second Eve Campaign was hidden")
      (is (= 32 (get-counters (refresh ec1) :credit)) "First Eve gained 16  more credits on reveal"))))

(deftest door-to-door
  ;; Door to Door
  (do-game
    (new-game (default-contestant ["Door to Door"])
              (default-challenger))
    (play-from-hand state :contestant "Door to Door")
    (take-credits state :contestant)
    (is (zero? (:tag (get-challenger))) "Challenger should start with 0 tags")
    (is (= 3 (-> (get-challenger) :hand count)) "Challenger should start with 3 cards in hand")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 1 (:tag (get-challenger))) "Challenger should gain 1 tag from Door to Door")
    (is (= 3 (-> (get-challenger) :hand count)) "Challenger should start with 3 cards in hand")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 1 (:tag (get-challenger))) "Challenger should still have 1 tag")
    (is (= 2 (-> (get-challenger) :hand count)) "Challenger should take 1 meat damage from Door to Door")))

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
    (is (zero? (:credit (get-challenger))) "Challenger has 0 credits")
    (take-credits state :contestant)
    (run-on state :archives)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Economic Warfare")
    (is (= 3 (:credit (get-challenger))) "Challenger has 3 credits")))

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

(deftest enforcing-loyalty
  ;; Enforcing Loyalty - Win trace to discard placed card not of Challenger's faction
  (do-game
    (new-game (default-contestant [(qty "Enforcing Loyalty" 2)])
              (make-deck "Chaos Theory: Wünderkind" ["Inti" "Caldera"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Inti")
    (play-from-hand state :challenger "Caldera")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Enforcing Loyalty")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (click-card state :contestant (get-resource state 0))
    (is (empty? (:discard (get-challenger))) "Can't target Inti; matches Challenger faction")
    (click-card state :contestant (get-radicle state 0))
    (is (= 1 (count (:discard (get-challenger)))) "Caldera discarded")))

(deftest enhanced-login-protocol
  ;; Enhanced Login Protocol
  (testing "First click run each turn costs an additional click"
    (do-game
      (new-game (default-contestant ["Enhanced Login Protocol"])
                (default-challenger ["Employee Strike"]))
      (play-from-hand state :contestant "Enhanced Login Protocol")
      (take-credits state :contestant)
      (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-challenger))) "Challenger spends 1 additional click to make the first run")
      (run-successful state)
      (run-on state :archives)
      (is (= 1 (:click (get-challenger))) "Challenger doesn't spend 1 additional click to make the second run")
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
      (is (= 2 (:click (get-challenger))) "Challenger doesn't spend 1 additional click to make a run")))
  (testing "Card ability runs don't cost additional clicks"
    (do-game
      (new-game (default-contestant ["Enhanced Login Protocol"])
                (default-challenger ["Sneakdoor Beta"]))
      (play-from-hand state :contestant "Enhanced Login Protocol")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sneakdoor Beta")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 4 (:click (get-challenger))) "Challenger has 2 clicks")
      (let [sneakdoor (get-resource state 0)]
        (card-ability state :challenger sneakdoor 0)
        (is (= 3 (:click (get-challenger))) "Challenger doesn't spend 1 additional click to run with a card ability")
        (run-successful state)
        (run-on state :archives)
        (is (= 1 (:click (get-challenger))) "Challenger spends 1 additional click to make a run")
        (run-successful state)
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
        (run-on state :archives)
        (is (= 2 (:click (get-challenger))) "Challenger spends 1 additional click to make a run"))))
  (testing "with New Angeles Sol, Enhanced Login Protocol discarded and replaced on steal doesn't double remove penalty"
    (do-game
      (new-game
        (make-deck "New Angeles Sol: Your News" ["Enhanced Login Protocol" "Breaking News"])
        (default-challenger))
      (play-from-hand state :contestant "Breaking News" "New party")
      (play-from-hand state :contestant "Enhanced Login Protocol")
      (take-credits state :contestant)
      (run-on state :party1)
      (run-successful state)
      (click-prompt state :challenger "Steal")
      (click-prompt state :contestant "Yes")
      (click-card state :contestant (find-card "Enhanced Login Protocol" (:discard (get-contestant))))
      (run-on state :archives)
      (is (= 1 (:click (get-challenger))) "Challenger has 1 click")))
  (testing "Run event don't cost additional clicks"
    (do-game
      (new-game (default-contestant ["Enhanced Login Protocol"])
                (default-challenger ["Out of the Ashes"]))
      (play-from-hand state :contestant "Enhanced Login Protocol")
      (take-credits state :contestant)
      (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
      (play-from-hand state :challenger "Out of the Ashes")
      (click-prompt state :challenger "Archives")
      (is (= 3 (:click (get-challenger))) "Challenger doesn't spend 1 additional click to run with a run event")
      (run-successful state)
      (run-on state :archives)
      (is (= 1 (:click (get-challenger))) "Challenger spends 1 additional click to make a run")
      (run-successful state)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (click-prompt state :challenger "No") ; Out of the Ashes prompt
      (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
      (run-on state :archives)
      (is (= 2 (:click (get-challenger))) "Challenger spends 1 additional click to make a run")))
  (testing "Works when played on the challenger's turn"
    (do-game
      (new-game (make-deck "New Angeles Sol: Your News"
                           ["Enhanced Login Protocol"
                            "Breaking News"])
                (default-challenger ["Hades Shard"]))
      (discard-from-hand state :contestant "Breaking News")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 2)
      (play-from-hand state :challenger "Hades Shard")
      (card-ability state :challenger (get-radicle state 0) 0)
      (click-prompt state :challenger "Steal")
      (click-prompt state :contestant "Yes")
      (click-card state :contestant (find-card "Enhanced Login Protocol" (:hand (get-contestant))))
      (is (find-card "Enhanced Login Protocol" (:current (get-contestant))) "Enhanced Login Protocol is in play")
      (is (= 3 (:click (get-challenger))) "Challenger has 3 clicks")
      (run-on state :archives)
      (is (= 1 (:click (get-challenger))) "Challenger spends 1 additional click to make a run")))
(testing "Doesn't fire if already run when played on the challenger's turn"
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News"
                         ["Enhanced Login Protocol"
                          "Breaking News"])
              (default-challenger ["Hades Shard"]))
    (discard-from-hand state :contestant "Breaking News")
    (take-credits state :contestant)
    (run-on state :hq)
    (run-successful state)
    (click-prompt state :challenger "No action")
    (core/gain state :challenger :credit 2)
    (play-from-hand state :challenger "Hades Shard")
    (card-ability state :challenger (get-radicle state 0) 0)
    (click-prompt state :challenger "Steal")
    (click-prompt state :contestant "Yes")
    (click-card state :contestant (find-card "Enhanced Login Protocol" (:hand (get-contestant))))
    (is (find-card "Enhanced Login Protocol" (:current (get-contestant))) "Enhanced Login Protocol is in play")
    (is (= 2 (:click (get-challenger))) "Challenger has 2 clicks")
    (run-on state :archives)
    (is (= 1 (:click (get-challenger))) "Challenger doesn't spend 1 additional click to make a run"))))

(deftest exchange-of-information
  ;; Exchange of Information
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Exchange of Information"
                               "Market Research"
                               "Breaking News"
                               "Project Beale"
                               "Explode-a-palooza"])
                (default-challenger))
      (score-agenda state :contestant (find-card "Market Research" (:hand (get-contestant))))
      (score-agenda state :contestant (find-card "Breaking News" (:hand (get-contestant))))
      (is (= 2 (:tag (get-challenger))) "Challenger gained 2 tags")
      (take-credits state :contestant)
      (is (zero? (:tag (get-challenger))) "Challenger lost 2 tags")
      (core/steal state :challenger (find-card "Project Beale" (:hand (get-contestant))))
      (core/steal state :challenger (find-card "Explode-a-palooza" (:hand (get-contestant))))
      (take-credits state :challenger)
      (is (= 4 (:agenda-point (get-challenger))))
      (is (= 3 (:agenda-point (get-contestant))))
      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "Exchange of Information")
      (click-card state :contestant (find-card "Project Beale" (:scored (get-challenger))))
      (click-card state :contestant (find-card "Breaking News" (:scored (get-contestant))))
      (is (= 3 (:agenda-point (get-challenger))))
      (is (= 4 (:agenda-point (get-contestant))))))
  (testing "Swapping a just scored Breaking News keeps the tags"
    (do-game
      (new-game (default-contestant ["Exchange of Information"
                               "Market Research"
                               "Breaking News"
                               "Project Beale"
                               "Explode-a-palooza"])
                (default-challenger))
      (take-credits state :contestant)
      (core/steal state :challenger (find-card "Project Beale" (:hand (get-contestant))))
      (core/steal state :challenger (find-card "Explode-a-palooza" (:hand (get-contestant))))
      (take-credits state :challenger)
      (score-agenda state :contestant (find-card "Breaking News" (:hand (get-contestant))))
      (is (= 2 (:tag (get-challenger))) "Challenger gained 2 tags")
      (play-from-hand state :contestant "Exchange of Information")
      (click-card state :contestant (find-card "Project Beale" (:scored (get-challenger))))
      (click-card state :contestant (find-card "Breaking News" (:scored (get-contestant))))
      (is (= 2 (:tag (get-challenger))) "Still has tags after swap and before end of turn")
      (take-credits state :contestant)
      (is (= 3 (:agenda-point (get-challenger))))
      (is (= 2 (:agenda-point (get-contestant))))
      (is (= 2 (:tag (get-challenger))) "Challenger does not lose tags at end of turn")))
  (testing "Swapping a 15 Minutes still keeps the ability. #1783"
    (do-game
      (new-game (default-contestant [(qty "Exchange of Information" 2) "15 Minutes"
                               "Project Beale"])
                (default-challenger))
      (score-agenda state :contestant (find-card "15 Minutes" (:hand (get-contestant))))
      (take-credits state :contestant)
      (core/gain state :challenger :tag 1)
      (core/steal state :challenger (find-card "Project Beale" (:hand (get-contestant))))
      (take-credits state :challenger)
      (is (= 1 (:agenda-point (get-contestant))))
      (is (= 2 (:agenda-point (get-challenger))))
      (play-from-hand state :contestant "Exchange of Information")
      (click-card state :contestant (find-card "Project Beale" (:scored (get-challenger))))
      (click-card state :contestant (find-card "15 Minutes" (:scored (get-contestant))))
      (is (= 2 (:agenda-point (get-contestant))))
      (is (= 1 (:agenda-point (get-challenger))))
      (is (zero? (count (:deck (get-contestant)))))
      ;; shuffle back into R&D from challenger's scored area
      (let [fifm (get-scored state :challenger 0)]
        (card-ability state :contestant fifm 0))
      (is (= 2 (:agenda-point (get-contestant))))
      (is (zero? (:agenda-point (get-challenger))))
      (is (= "15 Minutes" (:title (first (:deck (get-contestant))))))
      (take-credits state :contestant)
      (core/steal state :challenger (find-card "15 Minutes" (:deck (get-contestant))))
      (take-credits state :challenger)
      (is (= 2 (:agenda-point (get-contestant))))
      (is (= 1 (:agenda-point (get-challenger))))
      (play-from-hand state :contestant "Exchange of Information")
      (click-card state :contestant (find-card "15 Minutes" (:scored (get-challenger))))
      (click-card state :contestant (find-card "Project Beale" (:scored (get-contestant))))
      (is (= 1 (:agenda-point (get-contestant))))
      (is (= 2 (:agenda-point (get-challenger))))
      ;; shuffle back into R&D from contestant's scored area
      (let [fifm (get-scored state :contestant 0)]
        (card-ability state :contestant fifm 0))
      (is (= "15 Minutes" (:title (first (:deck (get-contestant))))))))
  (testing "Swapping a Mandatory Regions gives the Contestant an additional click per turn. #1687"
    (do-game
      (new-game (default-contestant [(qty "Exchange of Information" 2) "Mandatory Regions"
                               "Global Food Initiative"])
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
      (click-card state :contestant (find-card "Mandatory Regions" (:scored (get-challenger))))
      (click-card state :contestant (find-card "Global Food Initiative" (:scored (get-contestant))))
      (is (= 2 (:agenda-point (get-contestant))))
      (is (= 2 (:agenda-point (get-challenger))))
      (is (= 3 (:click (get-contestant))))
      (is (= 4 (:click-per-turn (get-contestant))))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 4 (:click (get-contestant))))
      (is (= 4 (:click-per-turn (get-contestant))))
      (play-from-hand state :contestant "Exchange of Information")
      (click-card state :contestant (find-card "Global Food Initiative" (:scored (get-challenger))))
      (click-card state :contestant (find-card "Mandatory Regions" (:scored (get-contestant))))
      (is (= 3 (:agenda-point (get-contestant))))
      (is (= 2 (:agenda-point (get-challenger))))
      (is (= 2 (:click (get-contestant))))
      (is (= 3 (:click-per-turn (get-contestant))))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 3 (:click (get-contestant))))
      (is (= 3 (:click-per-turn (get-contestant)))))))

(deftest fast-break
  ;; Fast Break
  (do-game
    (new-game (default-contestant ["Fast Break" "Hostile Takeover" "Keegan Lane" "Haas Arcology AI"
                             "Research Station" (qty "Ice Wall" 10)])
              (default-challenger [(qty "Fan Site" 3)]))
    (starting-hand state :contestant ["Fast Break" "Hostile Takeover" "Keegan Lane"
                                "Haas Arcology AI" "Research Station"])
    (take-credits state :contestant)
    (dotimes [_ 3]
      (play-from-hand state :challenger "Fan Site"))
    (take-credits state :challenger)
    (play-and-score state "Hostile Takeover")
    (is (= 3 (count (get-scored state :challenger))) "Challenger should have 3 agendas in score area")
    (play-from-hand state :contestant "Fast Break")
    (let [hand (-> (get-contestant) :hand count)
          credits (:credit (get-contestant))]
      (click-prompt state :contestant "3")
      (is (= (+ hand 3) (-> (get-contestant) :hand count)) "Contestant should draw 3 cards from Fast Break")
      (click-prompt state :contestant "New party")
      (click-card state :contestant (find-card "Keegan Lane" (:hand (get-contestant))))
      (click-card state :contestant (find-card "Ice Wall" (:hand (get-contestant))))
      (click-card state :contestant (find-card "Ice Wall" (:hand (get-contestant))))
      (is (= (dec credits) (:credit (get-contestant))) "Contestant should pay 1 credit to place second Ice Wall"))
    (core/move state :contestant (find-card "Fast Break" (:discard (get-contestant))) :hand)
    (play-from-hand state :contestant "Fast Break")
    (let [hand (-> (get-contestant) :hand count)
          credits (:credit (get-contestant))]
      (click-prompt state :contestant "0")
      (is (= hand (-> (get-contestant) :hand count)) "Contestant should draw no cards as they're allowed to draw no cards")
      (is (some #{"Locale 2"} (:choices (prompt-map :contestant))) "Contestant should be able to choose existing parties")
      (click-prompt state :contestant "Locale 2")
      (click-card state :contestant (find-card "Haas Arcology AI" (:hand (get-contestant))))
      (click-card state :contestant (find-card "Research Station" (:hand (get-contestant))))
      (is (= 2 (count (get-content state :party2))) "Contestant can't choose Research Station to place in a party")
      (click-card state :contestant (find-card "Ice Wall" (:hand (get-contestant))))
      (click-prompt state :contestant "Done")
      (is (= (- credits 2) (:credit (get-contestant))) "Contestant should pay 2 credits to place third Ice Wall")
      (is (empty? (:prompt (get-contestant))) "Contestant should be able to stop placing early"))))

(deftest foxfire
  ;; Foxfire
  (do-game
    (new-game (default-contestant [(qty "Foxfire" 2)])
              (default-challenger ["Dyson Mem Chip" "Ice Carver"]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 100)
    (play-from-hand state :challenger "Dyson Mem Chip")
    (play-from-hand state :challenger "Ice Carver")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Foxfire")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (click-card state :contestant (get-hazard state 0))
    (is (= 1 (-> (get-challenger) :discard count)) "Contestant should discard Dyson Mem Chip from winning Foxfire trace")
    (play-from-hand state :contestant "Foxfire")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (click-card state :contestant (get-radicle state 0))
    (is (= 2 (-> (get-challenger) :discard count)) "Contestant should discard Ice Carver from winning Foxfire trace")))

(deftest game-changer
  (letfn [(game-changer-test [num-agenda]
            (do-game
              (new-game (default-contestant ["Game Changer" "Hostile Takeover"])
                        (default-contestant [(qty "Fan Site" num-agenda)]))
              (take-credits state :contestant)
              (core/gain state :challenger :click num-agenda)
              (dotimes [_ num-agenda]
                (play-from-hand state :challenger "Fan Site"))
              (take-credits state :challenger)
              (play-and-score state "Hostile Takeover")
              (is (= num-agenda (count (get-scored state :challenger))) (str "Challenger should have " (utils/quantify num-agenda "Fan Site") " in play"))
              (let [clicks (:click (get-contestant))
                    n (dec num-agenda)]
                (play-from-hand state :contestant "Game Changer")
                (is (= (+ n clicks) (:click (get-contestant))) (str "Contestant should gain " (utils/quantify n "click")))
                (is (= 1 (-> (get-contestant) :rfg count)) "Game Changer should be in rfg zone now"))))]
    (doall (map game-changer-test (range 5)))))

(deftest hangeki
  ;; Hangeki
  (doseq [choice ["Yes" "No"]]
    (testing (str "choosing to " (when (= choice "No") "not ") "access card")
      (do-game
        (new-game (default-contestant ["Hostile Takeover" "Dedicated Response Team" "Hangeki"])
                  (default-challenger))
        (play-from-hand state :contestant "Hostile Takeover" "New party")
        (play-from-hand state :contestant "Dedicated Response Team" "New party")
        (take-credits state :contestant)
        (run-on state :party2)
        (run-successful state)
        (click-prompt state :challenger "Pay 3 [Credits] to discard")
        (take-credits state :challenger)
        (play-from-hand state :contestant "Hangeki")
        (click-card state :contestant (get-content state :party1 0))
        (click-prompt state :challenger choice)
        (if (= "Yes" choice)
          (do (click-prompt state :challenger "Steal")
              (is (= 1 (:agenda-point (get-challenger))) "Challenger should steal Hostile Takeover")
              (is (= 1 (-> (get-contestant) :rfg count)) "Hangeki should be removed from the game"))
          (do (is (empty? (:prompt (get-challenger))) "Challenger should have no more prompts as access ended")
              (is (= -1 (:agenda-point (get-challenger))) "Challenger should add Hangeki to their score area worth -1 agenda point")
              (is (zero? (-> (get-contestant) :rfg count)) "Hangeki shouldn't be removed from the game")))))))

(deftest hard-hitting-news
  ;; Hard-Hitting News
  (do-game
    (new-game (default-contestant ["Hard-Hitting News"])
              (default-challenger))
    (take-credits state :contestant)
    (run-empty-locale state :rd)
    (take-credits state :challenger)
    (is (= 3 (:click (get-contestant))) "Contestant should start with 3 clicks")
    (play-from-hand state :contestant "Hard-Hitting News")
    (is (zero? (:click (get-contestant))) "Playing Hard-Hitting News should lose all remaining clicks")
    (is (zero? (:tag (get-challenger))) "Challenger should start with 0 tags")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 4 (:tag (get-challenger))) "Challenger should gain 4 tags from losing Hard-Hitting News trace")))

(deftest hatchet-job
  ;; Hatchet Job - Win trace to add placed non-virtual to grip
  (do-game
    (new-game (default-contestant ["Hatchet Job"])
              (default-challenger ["Upya" "Ghost Challenger"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Ghost Challenger")
    (play-from-hand state :challenger "Upya")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hatchet Job")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (click-card state :contestant (get-radicle state 0))
    (is (empty? (:hand (get-challenger))) "Can't choose virtual card")
    (is (not (empty? (:prompt (get-contestant)))))
    (click-card state :contestant (get-resource state 0))
    (is (= 1 (count (:hand (get-challenger)))) "Upya returned to grip")))

(deftest hedge-fund
  (do-game
    (new-game (default-contestant) (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 9 (:credit (get-contestant))))))

(deftest hellion-alpha-test
  ;; Hellion Alpha Test
  (do-game
    (new-game (default-contestant [(qty "Hellion Alpha Test" 2)])
              (default-challenger ["Daily Casts"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Daily Casts")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hellion Alpha Test")
    (is (zero? (-> (get-challenger) :deck count)) "Challenger should have no cards in Stack")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (click-card state :contestant (get-radicle state 0))
    (is (= 1 (-> (get-challenger) :deck count)) "Challenger should have 1 card in Stack from losing Hellion Alpha Test trace")
    (is (= "Daily Casts" (-> (get-challenger) :deck first :title))
        "Challenger should have Daily Casts on top of Stack from losing Hellion Alpha Test trace")
    (take-credits state :contestant)
    (core/draw state :challenger)
    (play-from-hand state :challenger "Daily Casts")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hellion Alpha Test")
    (is (zero? (:bad-publicity (get-contestant))) "Contestant should start with 0 bad publicity")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "2")
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant should gain 1 bad publicity from losing Hellion Alpha Test trace")))

(deftest hellion-beta-test
  ;; Hellion Beta Test
  (testing "Winning Trace - Discarding 2 cards"
    (do-game
      (new-game (default-contestant ["Dedicated Response Team" "Hellion Beta Test"])
                (default-challenger ["Daily Casts" "Dyson Mem Chip"]))
      (play-from-hand state :contestant "Dedicated Response Team" "New party")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 100)
      (play-from-hand state :challenger "Daily Casts")
      (play-from-hand state :challenger "Dyson Mem Chip")
      (run-empty-locale state :party1)
      (click-prompt state :challenger "Pay 3 [Credits] to discard")
      (take-credits state :challenger)
      (is (zero? (-> (get-challenger) :discard count)) "Challenger's heap should be empty")
      (play-from-hand state :contestant "Hellion Beta Test")
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (click-card state :contestant (get-radicle state 0))
      (click-card state :contestant (get-hazard state 0))
      (is (= 2 (-> (get-challenger) :discard count)) "Challenger should have 2 cards in heap after losing Hellion Beta Test trace")))
  (testing "Losing trace - Gaining bad publicity"
    (do-game
      (new-game (default-contestant ["Dedicated Response Team" "Hellion Beta Test"])
                (default-challenger ["Daily Casts" "Dyson Mem Chip"]))
      (play-from-hand state :contestant "Dedicated Response Team" "New party")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 100)
      (play-from-hand state :challenger "Daily Casts")
      (play-from-hand state :challenger "Dyson Mem Chip")
      (run-empty-locale state :party1)
      (click-prompt state :challenger "Pay 3 [Credits] to discard")
      (take-credits state :challenger)
      (is (zero? (:bad-publicity (get-contestant))) "Contestant should start with 0 bad publicity")
      (play-from-hand state :contestant "Hellion Beta Test")
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "2")
      (is (= 1 (:bad-publicity (get-contestant))) "Contestant should gain 1 bad publicity from losing Hellion Beta Test trace"))))

(deftest high-profile-target
  (testing "when the challenger has no tags"
    (do-game
      (new-game (default-contestant [(qty "High-Profile Target" 6)])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (play-from-hand state :contestant "High-Profile Target")
      (is (= 3 (:click (get-contestant))) "Contestant not charged a click")
      (is (= 5 (count (:hand (get-challenger)))) "Challenger did not take damage")))
  (testing "when the challenger has one tag"
    (do-game
      (new-game (default-contestant [(qty "High-Profile Target" 6)])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "High-Profile Target")
      (is (= 3 (count (:hand (get-challenger)))) "Challenger has 3 cards in hand")))
  (testing "when the challenger has two tags"
    (do-game
      (new-game (default-contestant [(qty "High-Profile Target" 6)])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (core/gain state :challenger :tag 2)
      (play-from-hand state :contestant "High-Profile Target")
      (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")))
  (testing "When the challenger has three tags, gg"
    (do-game
      (new-game (default-contestant [(qty "High-Profile Target" 10)])
                (default-challenger))
      (core/gain state :challenger :tag 3)
      (play-from-hand state :contestant "High-Profile Target")
      (is (zero? (count (:hand (get-challenger)))) "Challenger has 0 cards in hand")
      (is (= :contestant (:winner @state)) "Contestant wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest housekeeping
  ;; Housekeeping - Challenger must discard a card from Grip on first place of a turn
  (do-game
    (new-game (default-contestant ["Housekeeping"])
              (default-challenger [(qty "Cache" 2) "Fall Guy" "Mr. Li"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Fall Guy")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Housekeeping")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Cache")
    (click-card state :challenger (find-card "Mr. Li" (:hand (get-challenger))))
    (is (empty? (:prompt (get-challenger))) "Fall Guy prevention didn't trigger")
    (is (= 1 (count (:discard (get-challenger)))) "Card discarded")
    (play-from-hand state :challenger "Cache")
    (is (empty? (:prompt (get-challenger))) "Housekeeping didn't trigger on 2nd place")))

(deftest invasion-of-privacy
  ;; Invasion of Privacy - Full test
  (do-game
    (new-game (default-contestant [(qty "Invasion of Privacy" 3)])
              (default-challenger [(qty "Sure Gamble" 2) "Fall Guy" (qty "Cache" 2)]))
    (core/gain state :contestant :click 3 :credit 6)
    ;; discard 2 cards
    (play-from-hand state :contestant "Invasion of Privacy")
    (click-prompt state :contestant "0") ; default trace
    (click-prompt state :challenger "0") ; Challenger won't match
    (is (= 5 (count (:hand (get-challenger)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" "Sure Gamble" nil) (prompt-names)))
      (click-prompt state :contestant (find-card "Sure Gamble" (:hand (get-challenger))))
      (click-prompt state :contestant (find-card "Sure Gamble" (:hand (get-challenger)))))
    (is (= 3 (count (:hand (get-challenger)))))
    ;; able to discard 2 cards but only 1 available target in Challenger's hand
    (play-from-hand state :contestant "Invasion of Privacy")
    (click-prompt state :contestant "0") ; default trace
    (click-prompt state :challenger "0") ; Challenger won't match
    (is (= 3 (count (:hand (get-challenger)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" nil) (prompt-names)))
      (click-prompt state :contestant (find-card "Fall Guy" (:hand (get-challenger))))
      (is (empty? (get-in @state [:contestant :prompt])) "No prompt for second card"))
    (is (= 2 (count (:hand (get-challenger)))))
    ;; failed trace - take the bad publicity
    (play-from-hand state :contestant "Invasion of Privacy")
    (click-prompt state :contestant "0") ; default trace
    (click-prompt state :challenger "2") ; Challenger matches
    (is (= 1 (:bad-publicity (get-contestant))))))

(deftest ipo
  ;; IPO - credits with Terminal operations
  (do-game
    (new-game
      (default-contestant ["IPO"])
      (default-challenger))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "IPO")
    (is (= 13 (:credit (get-contestant))))
    (is (zero? (:click (get-contestant))) "Terminal ends turns")))

(deftest kill-switch
  ;; Kill Switch
  (do-game
    (new-game (default-contestant ["Kill Switch" (qty "Hostile Takeover" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Kill Switch")
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (is (zero? (:brain-damage (get-challenger))) "Challenger should start with 0 brain damage")
    (play-and-score state "Hostile Takeover")
    (click-prompt state :contestant "Hostile Takeover")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 1 (:brain-damage (get-challenger))) "Challenger should get 1 brain damage from Kill Switch after Contestant scores an agenda")
    (take-credits state :contestant)
    (run-empty-locale state :party1)
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 2 (:brain-damage (get-challenger))) "Challenger should get 1 brain damage from Kill Switch after accecssing an agenda")))

(deftest lag-time
  (do-game
    (new-game (default-contestant ["Lag Time" "Vanilla" "Lotus Field"])
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
    (new-game (default-contestant ["Lateral Growth" "Breaking News"])
              (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (play-from-hand state :contestant "Lateral Growth")
    (click-card state :contestant (find-card "Breaking News" (:hand (get-contestant))))
    (click-prompt state :contestant "New party")
    (is (= "Breaking News" (:title (get-content state :party1 0)))
      "Breaking News placed by Lateral Growth")
    (is (= 7 (:credit (get-contestant))))))

(deftest manhunt
  ;; Manhunt - only fires once per turn. Unreported issue.
  (do-game
    (new-game (default-contestant ["Manhunt" (qty "Hedge Fund" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Manhunt")
    (take-credits state :contestant)
    (run-empty-locale state "HQ")
    (is (:prompt (get-contestant)) "Manhunt trace initiated")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag")
    (click-prompt state :challenger "No action")
    (is (not (:run @state)) "Run ended")
    (run-empty-locale state "HQ")
    (is (empty? (:prompt (get-contestant))) "No Manhunt trace on second run")
    (click-prompt state :challenger "No action")
    (is (not (:run @state)) "Run ended")))

(deftest market-forces
  (testing "Full test"
    (letfn [(market-forces-credit-test
              [{:keys [tag-count challenger-creds expected-credit-diff]}]
              (testing (str "when the challenger has " tag-count " tags and " challenger-creds " credits")
                (do-game
                  (new-game (default-contestant [(qty "Market Forces" 6)])
                            (default-challenger))
                  (swap! state assoc-in [:contestant :credit] 0)
                  (swap! state assoc-in [:challenger :credit] challenger-creds)
                  (core/gain state :challenger :tag tag-count)
                  (play-from-hand state :contestant "Market Forces")
                  (is (= expected-credit-diff (:credit (get-contestant)))
                      (str "the contestant gains " expected-credit-diff " credits"))
                  (is (= expected-credit-diff (- challenger-creds (:credit (get-challenger))))
                      (str "the challenger loses " expected-credit-diff " credits")))))]
      (doall (map market-forces-credit-test
                  [{:tag-count            1
                    :challenger-creds         10
                    :expected-credit-diff 3}
                   {:tag-count            2
                    :challenger-creds         10
                    :expected-credit-diff 6}
                   {:tag-count            3
                    :challenger-creds         10
                    :expected-credit-diff 9}
                   {:tag-count            3
                    :challenger-creds         0
                    :expected-credit-diff 0}
                   {:tag-count            3
                    :challenger-creds         5
                    :expected-credit-diff 5}]))))
  (testing "when the challenger is not tagged"
    (do-game
      (new-game (default-contestant [(qty "Market Forces" 6)])
                (default-challenger))
      (play-from-hand state :contestant "Market Forces")
      (is (= 6 (count (:hand (get-contestant))))
          "Market Forces is not played")
      (is (= 3 (:click (get-contestant)))
          "the contestant does not spend a click")
      (is (= 5 (:credit (get-contestant)) (:credit (get-challenger)))
          "credits are unaffected"))))

(deftest mass-commercialization
  ;; Mass Commercialization
  (do-game
    (new-game (default-contestant ["Mass Commercialization"
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

(deftest medical-research-fundraiser
  ;; Medical Research Fundraiser - challenger gains 8creds, challenger gains 3creds
  (do-game
    (new-game (default-contestant ["Medical Research Fundraiser"])
              (default-challenger))
    (is (= 5 (:credit (get-contestant))) "Contestant starts with 5 credits")
    (is (= 5 (:credit (get-challenger))) "Challenger starts with 5 credits")
    (play-from-hand state :contestant "Medical Research Fundraiser")
    (is (= 10 (:credit (get-contestant))) "Contestant gains 8 credits")
    (is (= 8 (:credit (get-challenger))) "Challenger gains 3 credits")))

(deftest midseason-replacements
  ;; Midseason Replacements - Trace to give Challenger tags after they steal an agenda
  (do-game
    (new-game (default-contestant ["Midseason Replacements" "Breaking News"])
              (default-challenger))
    (play-from-hand state :contestant "Midseason Replacements")
    (is (= 3 (:click (get-contestant))) "Midseason precondition not met; Contestant not charged a click")
    (play-from-hand state :contestant "Breaking News" "New party")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))))
    (let [bn (get-content state :party1 0)]
      (run-empty-locale state "Locale 1")
      (click-prompt state :challenger "Steal")
      (is (= 1 (:agenda-point (get-challenger))) "Stole Breaking News")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Midseason Replacements")
      (click-prompt state :contestant "0") ; default trace
      (click-prompt state :challenger "0") ; Challenger won't match
      (is (= 6 (:tag (get-challenger))) "Challenger took 6 tags"))))

(deftest mushin-no-shin
  ;; Mushin No Shin - Add 3 advancements to a card; prevent reveal/score of that card the rest of the turn
  (do-game
    (new-game (default-contestant [(qty "Mushin No Shin" 2) "Ronin" "Profiteering"])
              (default-challenger))
    (play-from-hand state :contestant "Mushin No Shin")
    (click-card state :contestant (find-card "Ronin" (:hand (get-contestant))))
    (let [ronin (get-content state :party1 0)]
      (is (= 3 (get-counters (refresh ronin) :advancement)) "3 advancements placed on Ronin")
      (core/reveal state :contestant (refresh ronin))
      (is (not (:revealed (refresh ronin))) "Ronin did not reveal")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/reveal state :contestant (refresh ronin))
      (is (:revealed (refresh ronin)) "Ronin now revealed")
      (play-from-hand state :contestant "Mushin No Shin")
      (click-card state :contestant (find-card "Profiteering" (:hand (get-contestant))))
      (let [prof (get-content state :party2 0)]
        (core/score state :contestant (refresh prof))
        (is (empty? (:scored (get-contestant))) "Profiteering not scored")
        (is (zero? (:agenda-point (get-contestant))))
        (take-credits state :contestant)
        (take-credits state :challenger)
        (core/score state :contestant (refresh prof))
        (click-prompt state :contestant "0")
        (is (= 1 (:agenda-point (get-contestant))) "Profiteering was able to be scored")))))

(deftest mutate
  ;; Mutate - discard a revealed piece of character, place and reveal one from R&D
  (testing "Basic operation"
    (do-game
      (new-game (default-contestant ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"])
                (default-challenger))
      (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Enigma" (:hand (get-contestant))) :deck)
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (is (= 1 (count (get-character state :hq))) "1 character placed")
      (is (= "Ice Wall" (:title (get-character state :hq 0))) "Ice Wall is placed")
      (play-from-hand state :contestant "Mutate")
      (click-card state :contestant (get-character state :hq 0))
      (is (= 1 (count (get-character state :hq))) "1 character placed")
      (is (= "Enigma" (:title (get-character state :hq 0))) "Enigma is placed")
      (is (:revealed (get-character state :hq 0)) "Enigma is revealed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")
      (is (second-last-log-contains? state "Enigma") "Placed card name was logged")))
  (testing "No character in R&D"
    (do-game
      (new-game (default-contestant ["Mutate" "Ice Wall" "Enigma" "Hedge Fund"])
                (default-challenger))
      (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (is (= 1 (count (get-character state :hq))) "1 character placed")
      (is (= "Ice Wall" (:title (get-character state :hq 0))) "Ice Wall is placed")
      (play-from-hand state :contestant "Mutate")
      (click-card state :contestant (get-character state :hq 0))
      (is (empty? (get-character state :hq)) "No character placed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged"))))

(deftest neural-emp
  ;; Neural EMP - Play if Challenger made a run the previous turn to do 1 net damage
  (do-game
    (new-game (default-contestant ["Neural EMP"])
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
    (new-game (default-contestant ["Oversight AI" "Archer"])
              (default-challenger))
    (play-from-hand state :contestant "Archer" "R&D")
    (let [archer (get-character state :rd 0)]
      (play-from-hand state :contestant "Oversight AI")
      (click-card state :contestant archer)
      (is (:revealed (refresh archer)))
      (is (= 4 (:credit (get-contestant))) "Archer revealed at no credit cost")
      (is (= "Oversight AI" (:title (first (:hosted (refresh archer)))))
          "Archer hosting OAI as a condition"))))

(deftest patch
  ;; Patch - +2 current strength
  (do-game
    (new-game (default-contestant ["Patch" "Vanilla"])
              (default-challenger))
    (play-from-hand state :contestant "Vanilla" "HQ")
    (core/reveal state :contestant (get-character state :hq 0))
    (play-from-hand state :contestant "Patch")
    (click-card state :contestant (get-character state :hq 0))
    (is (= 2 (:current-strength (get-character state :hq 0))) "Vanilla at 2 strength")))

(deftest paywall-implementation
  ;; Paywall Implementation - Gain 1 credit for every successful run
  (do-game
    (new-game (default-contestant ["Paywall Implementation"])
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
    (new-game (default-contestant ["Peak Efficiency" (qty "Paper Wall" 3) "Wraparound"])
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
              (default-challenger ["Grimoire" "Cache"]))
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
    (click-prompt state :contestant "2")
    (is (= 3 (count (:discard (get-contestant)))) "2 cards discarded from R&D")
    (is (= 1 (count (:deck (get-contestant)))) "1 card remaining in R&D")
    (click-card state :challenger (get-hazard state 0)) ; try targeting Grimoire
    (is (empty? (:discard (get-challenger))) "Grimoire too expensive to be targeted")
    (click-card state :challenger (get-resource state 0))
    (is (= 1 (count (:discard (get-challenger)))) "Cache discarded")))

(deftest power-grid-overload
  ;; Power Grid Overload
  (do-game
    (new-game (default-contestant ["Power Grid Overload"])
              (default-challenger ["Dyson Mem Chip"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Dyson Mem Chip")
    (run-empty-locale state :rd)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Power Grid Overload")
    (click-prompt state :contestant "3")
    (click-prompt state :challenger "0")
    (click-card state :contestant (get-hazard state 0))
    (is (= 1 (-> (get-challenger) :discard count)) "Dyson Mem Chip should be in heap after Challenger loses Power Grid Overload trace")))

(deftest precognition
  ;; Precognition - Full test
  (do-game
    (new-game (default-contestant ["Precognition" "Caprcharacter Nisei" "Adonis Campaign"
                             "Quandary" "Jackson Howard" "Global Food Initiative"])
              (default-challenger))
    (starting-hand state :contestant ["Precognition"])
    (play-from-hand state :contestant "Precognition")
    (click-prompt state :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Adonis Campaign" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Quandary" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Global Food Initiative" (:deck (get-contestant))))
    ;; try starting over
    (click-prompt state :contestant "Start over")
    (click-prompt state :contestant (find-card "Global Food Initiative" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Quandary" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Adonis Campaign" (:deck (get-contestant))))
    (click-prompt state :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant)))) ;this is the top card of R&D
    (click-prompt state :contestant "Done")
    (is (= "Caprcharacter Nisei" (:title (first (:deck (get-contestant))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-contestant))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-contestant)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-contestant))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-contestant)))))))))))

(deftest preemptive-action
  ;; Preemptive Action - Shuffles cards into R&D and removes itself from game
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Subliminal Messaging" 3)
                               "Preemptive Action"])
                (default-challenger))
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Preemptive Action")
      (click-card state :contestant (first (:discard (get-contestant))))
      (click-card state :contestant (second (:discard (get-contestant))))
      (click-card state :contestant (last (:discard (get-contestant))))
      (is (zero? (count (:discard (get-contestant)))))
      (is (= 1 (count (:rfg (get-contestant)))))))
  (testing "forces you to take 3 if there are three, and removes itself from game"
    (do-game
      (new-game (default-contestant [(qty "Subliminal Messaging" 3)
                               (qty "Preemptive Action" 1)])
                (default-challenger))
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Preemptive Action")
      (click-card state :contestant (first (:discard (get-contestant))))
      (click-card state :contestant (last (:discard (get-contestant))))
      (is (= 3 (count (:discard (get-contestant)))))
      (is (= 1 (count (:rfg (get-contestant)))))))
  (testing "Shuffles all archives cards into R&D if Archives has less than 3 cards, and removes itself from game"
    (do-game
      (new-game (default-contestant [(qty "Subliminal Messaging" 2)
                               (qty "Preemptive Action" 1)])
                (default-challenger))
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Preemptive Action")
      (click-card state :contestant (first (:discard (get-contestant))))
      (click-card state :contestant (last (:discard (get-contestant))))
      (is (zero? (count (:discard (get-contestant)))))
      (is (= 1 (count (:rfg (get-contestant))))))))

(deftest psychographics
  ;; Psychographics - Place advancements up to the number of Challenger tags on a card
  (do-game
    (new-game (default-contestant ["Psychographics" "Project Junebug"])
              (default-challenger))
    (core/gain state :challenger :tag 4)
    (play-from-hand state :contestant "Project Junebug" "New party")
    (let [pj (get-content state :party1 0)]
      (play-from-hand state :contestant "Psychographics")
      (click-prompt state :contestant "4")
      (click-card state :contestant pj)
      (is (= 1 (:credit (get-contestant))) "Spent 4 credits")
      (is (= 4 (get-counters (refresh pj) :advancement)) "Junebug has 4 advancements"))))

(deftest psychokinesis
  ;; Pyschokinesis - Terminal Event (end the turn); Look at R&D, place an Site, Agenda, or Region in a Party Locale
  (do-game
    (new-game (default-contestant [(qty "Psychokinesis" 3) "Caprcharacter Nisei" "Adonis Campaign"
                              "Global Food Initiative" "Mwanza City Grid"])
              (default-challenger))
    (starting-hand state :contestant ["Psychokinesis" "Psychokinesis" "Psychokinesis"])
    ;; Test placing an Region
    (play-from-hand state :contestant "Psychokinesis")
    (is (not-any? #{"Mwanza City Grid"} (map :title (-> (get-contestant) :prompt first :choices)))
        "Mwanza City Grid is not on the list of placeable cards")
    (click-prompt state :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
    (click-prompt state :contestant "New party")
    (is (= "Caprcharacter Nisei" (:title (get-content state :party1 0)))
      "Caprcharacter Nisei placed by Psychokinesis")
    ;; Test placing an Site
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Psychokinesis")
    (click-prompt state :contestant (find-card "Adonis Campaign" (:deck (get-contestant))))
    (click-prompt state :contestant "New party")
    (is (= "Adonis Campaign" (:title (get-content state :party2 0)))
      "Adonis Campaign placed by Psychokinesis")
    ;; Test placing an Agenda
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Psychokinesis")
    (click-prompt state :contestant (find-card "Global Food Initiative" (:deck (get-contestant))))
    (click-prompt state :contestant "New party")
    (is (= "Global Food Initiative" (:title (get-content state :party3 0)))
      "Global Food Initiative placed by Psychokinesis")
    ;; Test selecting "None"
    (core/gain state :contestant :click 1)
    (core/move state :contestant (find-card "Psychokinesis" (:discard (get-contestant))) :hand)
    (play-from-hand state :contestant "Psychokinesis")
    (click-prompt state :contestant "None")
    (is (= nil (:title (get-content state :party4 0)))
      "Nothing is placed by Psychokinesis")))

(deftest punitive-counterstrike
  ;; Punitive Counterstrike - deal meat damage equal to printed agenda points
  (do-game
    (new-game (default-contestant ["Global Food Initiative" "Punitive Counterstrike"])
              (default-challenger))
    (play-from-hand state :contestant "Global Food Initiative" "New party")
    (take-credits state :contestant)
    (run-empty-locale state :party1)
    (click-prompt state :challenger "Steal")
    (is (= 2 (:agenda-point (get-challenger))) "Challenger scored 2 points")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Punitive Counterstrike")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (empty? (:hand (get-challenger))) "Challenger took 3 meat damage")))

(deftest red-planet-couriers
  ;; Red Planet Couriers - Move all advancements on cards to 1 advanceable card
  (do-game
    (new-game (default-contestant ["Red Planet Couriers" (qty "Ice Wall" 2)
                             "GRNDL Refinery" "Government Takeover"])
              (default-challenger))
    (core/gain state :contestant :click 4)
    (play-from-hand state :contestant "Government Takeover" "New party")
    (play-from-hand state :contestant "GRNDL Refinery" "New party")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (let [gt (get-content state :party1 0)
          gr (get-content state :party2 0)
          iw1 (get-character state :hq 0)
          iw2 (get-character state :rd 0)]
      (core/add-prop state :contestant gr :advance-counter 3)
      (core/add-prop state :contestant iw1 :advance-counter 2)
      (core/add-prop state :contestant iw2 :advance-counter 1)
      (play-from-hand state :contestant "Red Planet Couriers")
      (click-card state :contestant gt)
      (is (zero? (get-counters (refresh gr) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw1) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw2) :advancement)) "Advancements removed")
      (is (= 6 (get-counters (refresh gt) :advancement)) "Gained 6 advancements"))))

(deftest reuse
  ;; Reuse - Gain 2 credits for each card discarded from HQ
  (do-game
    (new-game (default-contestant [(qty "Reuse" 2) "Hive" "IQ"
                             "Ice Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Reuse")
    (click-card state :contestant (find-card "Ice Wall" (:hand (get-contestant))))
    (click-card state :contestant (find-card "Hive" (:hand (get-contestant))))
    (click-card state :contestant (find-card "IQ" (:hand (get-contestant))))
    (click-prompt state :contestant "Done")
    (is (= 4 (count (:discard (get-contestant)))) "3 cards discarded plus operation played")
    (is (= 11 (:credit (get-contestant))) "Gained 6 credits")
    (is (= 1 (:click (get-contestant))) "Spent 2 clicks")))

(deftest reverse-infection
  ;; Reverse Infection - purge and discard 1 card from stack for every 3 counters purged - or gain 2 credits
  (do-game
    (new-game (default-contestant [(qty "Reverse Infection" 2)])
              (default-challenger ["Virus Breeding Ground" "Datasucker" (qty "Sure Gamble" 3)]))
    (starting-hand state :challenger ["Virus Breeding Ground" "Datasucker"])
    (play-from-hand state :contestant "Reverse Infection")
    (click-prompt state :contestant "Gain 2 [Credits]")
    (is (= 7 (:credit (get-contestant))) "Contestant gained 2 credits")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Virus Breeding Ground")
    (play-from-hand state :challenger "Datasucker")
    (take-credits state :challenger)
    (core/add-counter state :challenger (get-radicle state 0) :virus 4)
    (core/add-counter state :challenger (get-resource state 0) :virus 3)
    (play-from-hand state :contestant "Reverse Infection")
    (click-prompt state :contestant "Purge virus counters.")
    (is (= 9 (:credit (get-contestant))) "Contestant did not gain credits")
    (is (zero? (get-counters (get-radicle state 0) :virus)) "Viruses purged from VBG")
    (is (zero? (get-counters (get-resource state 0) :virus)) "Viruses purged from Datasucker")
    (is (= 2 (count (:discard (get-challenger)))) "Two cards discarded from stack")))

(deftest riot-suppression
  ;; Riot Suppression - lose 3 clicks or take 1 brain damage
  (testing "Take 1 brain damage"
    (do-game
      (new-game (default-contestant ["Riot Suppression" "Adonis Campaign"])
                (default-challenger))
      (play-from-hand state :contestant "Adonis Campaign" "New party")
      (take-credits state :contestant)
      (run-empty-locale state "Locale 1")
      (click-prompt state :challenger "Pay 3 [Credits] to discard")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Riot Suppression")
      (is (empty? (:discard (get-challenger))) "Challenger discard is empty")
      (is (zero? (:brain-damage (get-challenger))) "Challenger starts with no brain damage")
      (click-prompt state :challenger "Yes")
      (is (= 1 (count (:discard (get-challenger)))) "1 card lost to brain damage")
      (is (= 1 (:brain-damage (get-challenger))) "Challenger took 1 brain damage")
      (is (= 1 (count (:discard (get-contestant)))) "No contestant cards discarded")
      (is (= 1 (count (:rfg (get-contestant)))) "Riot Suppestion removed from game")
      (take-credits state :contestant)
      (is (= 4 (:click (get-challenger))) "Challenger has all clicks the following turn")))
  (testing "Lose 3 clicks"
    (do-game
      (new-game (default-contestant ["Riot Suppression" "Adonis Campaign"])
                (default-challenger))
      (play-from-hand state :contestant "Adonis Campaign" "New party")
      (take-credits state :contestant)
      (run-empty-locale state "Locale 1")
      (click-prompt state :challenger "Pay 3 [Credits] to discard")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Riot Suppression")
      (is (empty? (:discard (get-challenger))) "Challenger discard is empty")
      (is (zero? (:brain-damage (get-challenger))) "Challenger starts with no brain damage")
      (click-prompt state :challenger "No")
      (is (empty? (:discard (get-challenger))) "Challenger discard statys empty")
      (is (zero? (:brain-damage (get-challenger))) "Challenger takes no brain damage")
      (is (= 1 (count (:discard (get-contestant)))) "No contestant cards discarded")
      (is (= 1 (count (:rfg (get-contestant)))) "Riot Suppestion removed from game")
      (take-credits state :contestant)
      (is (= 1 (:click (get-challenger))) "Challenger has 3 fewer clicks following turn"))))

(deftest rolling-brownout
  ;; Rolling Brownout - Increase cost of events/operations by 1, gain 1c on first Challenger event of turn
  (do-game
    (new-game (default-contestant ["Rolling Brownout" "Beanstalk Royalties"
                             "Domestic Sleepers"])
              (default-challenger [(qty "Easy Mark" 3)]))
    (play-from-hand state :contestant "Rolling Brownout")
    (play-from-hand state :contestant "Beanstalk Royalties")
    (is (= 5 (:credit (get-contestant))) "Beanstalk netted only 2c")
    (play-from-hand state :contestant "Domestic Sleepers" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Easy Mark")
    (is (= 7 (:credit (get-challenger))) "Easy Mark netted only 2c")
    (is (= 6 (:credit (get-contestant))) "Contestant gained 1c from Brownout")
    (play-from-hand state :challenger "Easy Mark")
    (is (= 6 (:credit (get-contestant))) "No Contestant credit gain from 2nd event")
    (run-empty-locale state "Locale 1")
    (click-prompt state :challenger "Steal")
    (play-from-hand state :challenger "Easy Mark")
    (is (= 12 (:credit (get-challenger))) "Easy Mark netted 3c after Brownout discarded")))

(deftest salem's-hospitality
  ;; Salem's Hospitality - Full test
  (do-game
    (new-game (default-contestant [(qty "Salem's Hospitality" 3)])
              (default-challenger [(qty "I've Had Worse" 3) "Faust"
                               "Levy AR Lab Access"]))
    (play-from-hand state :contestant "Salem's Hospitality")
    (is (= 5 (count (:hand (get-challenger)))))
    (click-prompt state :contestant "I've Had Worse")
    (is (= 2 (count (:hand (get-challenger)))))
    (play-from-hand state :contestant "Salem's Hospitality")
    (click-prompt state :contestant "Plascrete Carapace")
    (is (= 2 (count (:hand (get-challenger)))))))

(deftest scorched-earth
  ;; Scorched Earth
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Scorched Earth"])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "Scorched Earth")
      (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")))
  (testing "not tagged"
    (do-game
      (new-game (default-contestant ["Scorched Earth"])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Lucky Find" 3)]))
      (play-from-hand state :contestant "Scorched Earth")
      (is (= 3 (:click (get-contestant))) "Contestant not charged a click")
      (is (= 5 (count (:hand (get-challenger)))) "Challenger did not take damage")))
  (testing "flatline"
    (do-game
      (new-game (default-contestant [(qty "Scorched Earth" 10)])
                (default-challenger))
      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "Scorched Earth")
      (is (zero? (count (:hand (get-challenger)))) "Challenger has 0 cards in hand")
      (is (= :contestant (:winner @state)) "Contestant wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest sea-source
  ;; SEA Source
  (do-game
    (new-game (default-contestant ["SEA Source"])
              (default-challenger))
    (take-credits state :contestant)
    (run-empty-locale state :rd)
    (take-credits state :challenger)
    (is (zero? (:tag (get-challenger))) "Challenger should start with 0 tags")
    (play-from-hand state :contestant "SEA Source")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 1 (:tag (get-challenger))) "Challenger should get 1 tag from losing SEA Source trace")))

(deftest self-growth-resource
  ;; Self-Growth Resource - Add 2 placed cards to grip if challenger is tagged
  (do-game
    (new-game (default-contestant ["Self-Growth Resource"])
              (default-challenger ["Clone Chip" "Inti"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Clone Chip")
    (play-from-hand state :challenger "Inti")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Self-Growth Resource")
    (is (= 3 (:click (get-contestant))) "Self-Growth Resource precondition not met; card not played")
    (core/gain state :challenger :tag 1)
    (is (zero? (count (:hand (get-challenger)))) "Challenger hand is empty")
    (let [inti (get-resource state 0)
          cc (get-hazard state 0)]
      (play-from-hand state :contestant "Self-Growth Resource")
      (click-card state :contestant inti)
      (click-card state :contestant cc))
    (is (= 2 (count (:hand (get-challenger)))) "2 cards returned to hand")
    (is (zero? (count (get-resource state))) "No resources placed")
    (is (zero? (count (get-hazard state))) "No hazard placed")))

(deftest servcharacter-outage
  ;; Servcharacter Outage
  (testing "First click run each turn costs a credit"
    (do-game
      (new-game (default-contestant ["Servcharacter Outage"])
                (default-challenger ["Employee Strike"]))
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
      (is (zero? (:credit (get-challenger))) "Challenger has 0 credits")
      (run-on state :archives)
      (is (not (:run @state)) "No run was initiated")
      (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
      (is (zero? (:credit (get-challenger))) "Challenger has 0 credits")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (core/lose state :challenger :credit 2)
      (play-from-hand state :challenger "Employee Strike")
      (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
      (run-on state :archives)
      (is (= 1 (:credit (get-challenger)))
          "Challenger doesn't spend 1 credit to make a run")))
  (testing "First card ability run each turn costs an additional credit"
    (do-game
      (new-game (default-contestant ["Servcharacter Outage"])
                (default-challenger ["Sneakdoor Beta"]))
      (play-from-hand state :contestant "Servcharacter Outage")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sneakdoor Beta")
      (take-credits state :challenger 1)
      (is (= 2 (:credit (get-challenger))) "Challenger has 2 credits")
      (let [sneakdoor (get-resource state 0)]
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
        (is (zero? (:credit (get-challenger))) "Challenger has 0 credits")
        (card-ability state :challenger sneakdoor 0)
        (is (not (:run @state)) "No run was initiated")
        (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
        (is (zero? (:credit (get-challenger))) "Challenger has 0 credits"))))
  (testing "First run event each turn costs an additional credit"
    (do-game
      (new-game (default-contestant ["Servcharacter Outage"])
                (default-challenger [(qty "Out of the Ashes" 2)]))
      (play-from-hand state :contestant "Servcharacter Outage")
      (take-credits state :contestant)
      (is (= 5 (:credit (get-challenger))) "Challenger has 5 credits")
      (play-from-hand state :challenger "Out of the Ashes")
      (is (= 3 (:credit (get-challenger)))
          "Challenger spends 1 additional credit to run with a run event")
      (click-prompt state :challenger "Archives")
      (run-successful state)
      (run-on state :archives)
      (is (= 3 (:credit (get-challenger)))
          "Challenger doesn't spend 1 credit to make a run")
      (run-successful state)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (click-prompt state :challenger "No") ; Out of the Ashes prompt
      (core/lose state :challenger :credit 4)
      (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
      (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
      (play-from-hand state :challenger "Out of the Ashes")
      (is (empty? (get-in @state [:challenger :prompt]))
          "Out of the Ashes was not played")
      (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks")
      (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")))
  (testing "Works when played on the challenger's turn"
    (do-game
      (new-game (make-deck "New Angeles Sol: Your News" ["Servcharacter Outage"
                                                         "Breaking News"])
                (default-challenger ["Hades Shard"]))
      (discard-from-hand state :contestant "Breaking News")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 3)
      (play-from-hand state :challenger "Hades Shard")
      (card-ability state :challenger (get-radicle state 0) 0)
      (click-prompt state :challenger "Steal")
      (click-prompt state :contestant "Yes")
      (click-card state :contestant (find-card "Servcharacter Outage" (:hand (get-contestant))))
      (is (find-card "Servcharacter Outage" (:current (get-contestant)))
          "Servcharacter Outage is in play")
      (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
      (run-on state :archives)
      (is (zero? (:credit (get-challenger)))
          "Challenger spends 1 additional credit to make a run")))
(testing "Doesn't fire if already run when played on the challenger's turn"
  (do-game
    (new-game (make-deck "New Angeles Sol: Your News" ["Servcharacter Outage"
                                                       "Breaking News"])
              (default-challenger ["Hades Shard"]))
    (discard-from-hand state :contestant "Breaking News")
    (take-credits state :contestant)
    (run-on state :hq)
    (run-successful state)
    (click-prompt state :challenger "No action")
    (core/gain state :challenger :credit 3)
    (play-from-hand state :challenger "Hades Shard")
    (card-ability state :challenger (get-radicle state 0) 0)
    (click-prompt state :challenger "Steal")
    (click-prompt state :contestant "Yes")
    (click-card state :contestant (find-card "Servcharacter Outage" (:hand (get-contestant))))
    (is (find-card "Servcharacter Outage" (:current (get-contestant)))
        "Servcharacter Outage is in play")
    (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
    (run-on state :archives)
    (is (= 1 (:credit (get-challenger)))
        "Challenger doesn't spend 1 additional credit to make a run")))
(testing "discarded and replaced on steal doesn't double remove penalty"
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" ["Servcharacter Outage"
                                               "Breaking News"])
      (default-challenger))
    (play-from-hand state :contestant "Breaking News" "New party")
    (play-from-hand state :contestant "Servcharacter Outage")
    (take-credits state :contestant)
    (run-on state :party1)
    (run-successful state)
    (click-prompt state :challenger "Steal")
    (click-prompt state :contestant "Yes")
    (click-card state :contestant (find-card "Servcharacter Outage" (:discard (get-contestant))))
    (take-credits state :challenger)
    (take-credits state :contestant)
    (is (= 7 (:credit (get-challenger))) "Challenger has 7 credits")
    (run-on state :archives)
    (is (= 6 (:credit (get-challenger))) "Challenger spends 1 credit to make a run"))))

(deftest shipment-from-sansan
  ;; Shipment from SanSan - placing advancements
  (do-game
    (new-game (default-contestant [(qty "Shipment from SanSan" 3) (qty "Ice Wall" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [iwall (get-character state :hq 0)]
      (play-from-hand state :contestant "Shipment from SanSan")
      (click-prompt state :contestant "2")
      (click-card state :contestant iwall)
      (is (= 5 (:credit (get-contestant))))
      (is (= 2 (get-counters (refresh iwall) :advancement))))))

(deftest snatch-and-grab
  ;; Snatch and Grab
  (do-game
    (new-game (default-contestant [(qty "Snatch and Grab" 2)])
              (default-challenger ["Scrubber"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Scrubber")
    (take-credits state :challenger)
    (is (zero? (:tag (get-challenger))) "Challenger should start with 0 tags")
    (play-from-hand state :contestant "Snatch and Grab")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (click-card state :contestant (get-radicle state 0))
    (click-prompt state :challenger "Yes")
    (is (= 1 (:tag (get-challenger))) "Challenger should get 1 tag from losing Snatch and Grab trace and opting to take the tag")
    (is (zero? (-> (get-challenger) :discard count)) "Challenger should start with 0 cards in heap")
    (play-from-hand state :contestant "Snatch and Grab")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (click-card state :contestant (get-radicle state 0))
    (click-prompt state :challenger "No")
    (is (= 1 (-> (get-challenger) :discard count)) "Scrubber should be in Challenger's heap after losing Snatch and Grab trace")))

(deftest stock-buy-back
  ;; Stock Buy-Back - Gain 3c for every agenda in Challenger's area
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 2) (qty "Stock Buy-Back" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (take-credits state :contestant)
    (run-empty-locale state "Locale 1")
    (click-prompt state :challenger "Steal")
    (run-empty-locale state "Locale 2")
    (click-prompt state :challenger "Steal")
    (take-credits state :challenger)
    (is (= 2 (count (:scored (get-challenger)))))
    (play-from-hand state :contestant "Stock Buy-Back")
    (is (= 11 (:credit (get-contestant))))))

(deftest sub-boost
  ;; Sub Boost - Give Character Barrier
  (do-game
    (new-game (default-contestant ["Sub Boost" "Quandary"])
              (default-challenger))
    (play-from-hand state :contestant "Quandary" "HQ")
    (let [qu (get-character state :hq 0)]
      (core/reveal state :contestant qu)
      (is (not (core/has-subtype? (refresh qu) "Barrier")) "Quandry starts without Barrier")
      (is (= 1 (count (:subroutines (refresh qu)))) "Quandry has 1 subroutine")
      (play-from-hand state :contestant "Sub Boost")
      (click-card state :contestant (refresh qu))
      (is (core/has-subtype? (refresh qu) "Code Gate") "Quandary has Code Gate")
      (is (core/has-subtype? (refresh qu) "Barrier") "Quandary Character Barrier")
      (is (= 2 (count (:subroutines (refresh qu)))) "Quandry gains a subroutine"))))

(deftest subcontract
  ;; Subcontract
  (testing "Don't allow second operation until damage prevention completes"
    (do-game
      (new-game (default-contestant [(qty "Scorched Earth" 2) "Subcontract"])
                (default-challenger ["Plascrete Carapace"]))
      (take-credits state :contestant)
      (core/gain state :challenger :tag 1)
      (play-from-hand state :challenger "Plascrete Carapace")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Subcontract")
      (click-card state :contestant (find-card "Scorched Earth" (:hand (get-contestant))))
      (is (and (= 1 (count (:prompt (get-contestant)))) (= :waiting (-> (get-contestant) :prompt first :prompt-type)))
          "Contestant does not have Subcontract prompt until damage prevention completes")
      (click-prompt state :challenger "Done")
      (is (not-empty (:prompt (get-contestant))) "Contestant can now play second Subcontract operation")))
  (testing "interaction with Terminal operations"
    (do-game
      (new-game
        (default-contestant [(qty "Hard-Hitting News" 2) "Subcontract"])
        (default-challenger))
      (core/gain state :challenger :tag 1)
      (take-credits state :contestant)
      (run-empty-locale state :archives)
      (take-credits state :challenger)
      (play-from-hand state :contestant "Subcontract")
      (click-card state :contestant (find-card "Hard-Hitting News" (:hand (get-contestant))))
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (is (= 5 (:tag (get-challenger))) "Challenger has 5 tags")
      (is (empty? (:prompt (get-contestant))) "Contestant does not have a second Subcontract selection prompt"))))

(deftest subliminal-messaging
  ;; Subliminal Messaging - Playing/discarding/milling will all prompt returning to hand
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Subliminal Messaging" 3)])
                (make-deck "Noise: Hacker Extraordinaire" [(qty "Cache" 3) "Utopia Shard"]))
      (play-from-hand state :contestant "Subliminal Messaging")
      (is (= 6 (:credit (get-contestant))))
      (is (= 3 (:click (get-contestant))) "First Subliminal Messaging gains 1 click")
      (play-from-hand state :contestant "Subliminal Messaging")
      (is (= 7 (:credit (get-contestant))))
      (is (= 2 (:click (get-contestant))) "Second Subliminal Messaging does not gain 1 click")
      (discard-from-hand state :contestant "Subliminal Messaging")
      (is (zero? (count (:hand (get-contestant)))))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (click-prompt state :contestant "Yes")
      (click-prompt state :contestant "Yes")
      (click-prompt state :contestant "Yes")
      (is (= 3 (count (:hand (get-contestant)))) "All 3 Subliminals returned to HQ")
      (core/move state :contestant (find-card "Subliminal Messaging" (:hand (get-contestant))) :deck)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Cache")
      (play-from-hand state :challenger "Utopia Shard")
      (let [utopia (get-radicle state 0)]
        (card-ability state :challenger utopia 0))
      (take-credits state :challenger)
      (click-prompt state :contestant "Yes")
      (click-prompt state :contestant "Yes")
      (click-prompt state :contestant "Yes")
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
  (testing "Scenario involving Subliminal being added to HQ with Archived Memories"
    (do-game
      (new-game (default-contestant [(qty "Subliminal Messaging" 2) "Archived Memories"])
                (default-challenger))
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Archived Memories")
      (click-card state :contestant (find-card "Subliminal Messaging" (:discard (get-contestant))))
      (is (= 2 (count (:discard (get-contestant)))))
      (is (= 1 (count (:hand (get-contestant)))))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (click-prompt state :contestant "No")
      (is (empty? (get-in @state [:contestant :prompt])) "Only 1 Subliminal prompt")
      (play-from-hand state :contestant "Subliminal Messaging")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (click-prompt state :contestant "Yes")
      (click-prompt state :contestant "Yes")
      (is (empty? (get-in @state [:contestant :prompt]))
          "Only 2 Subliminal prompts - there will be a third if flag not cleared")))
  (testing "Scenario involving Subliminal being reshuffled into R&D with Jackson"
    (do-game
      (new-game (default-contestant ["Subliminal Messaging" "Jackson Howard"])
                (default-challenger))
      (play-from-hand state :contestant "Subliminal Messaging")
      (play-from-hand state :contestant "Jackson Howard" "New party")
      (take-credits state :contestant)
      (let [jhow (get-content state :party1 0)]
        (core/reveal state :contestant jhow)
        (card-ability state :contestant jhow 1)
        (click-card state :contestant (find-card "Subliminal Messaging" (:discard (get-contestant))))
        (click-prompt state :contestant "Done")
        (is (zero? (count (:discard (get-contestant)))))
        (is (= 1 (count (:rfg (get-contestant))))))
      (take-credits state :challenger)
      (play-from-hand state :contestant "Subliminal Messaging")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (click-prompt state :contestant "Yes")
      (is (= 1 (count (:hand (get-contestant)))) "Subliminal returned to HQ")
      (is (empty? (get-in @state [:contestant :prompt]))
          "Subliminal prompt cleared - there will be a second prompt if flag not cleared")))
  (testing "Challenger made run, ensure game asks again next turn"
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
      (click-prompt state :contestant "Yes")
      (click-prompt state :contestant "Yes")
      (is (= 2 (count (:hand (get-contestant)))) "Both Subliminals returned to HQ")
      (is (zero? (count (:discard (get-contestant)))) "No Subliminals in Archives")))
  (testing "User declines to return to hand, ensure game asks again next turn"
    (do-game
      (new-game (default-contestant [(qty "Subliminal Messaging" 2)])
                (default-challenger))
      (play-from-hand state :contestant "Subliminal Messaging")
      (discard-from-hand state :contestant "Subliminal Messaging")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (click-prompt state :contestant "No")
      (click-prompt state :contestant "No")
      (is (zero? (count (:hand (get-contestant)))) "Neither Subliminal returned to HQ")
      (is (= 2 (count (:discard (get-contestant)))) "Both Subliminals in Archives")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (click-prompt state :contestant "Yes")
      (click-prompt state :contestant "Yes")
      (is (= 2 (count (:hand (get-contestant)))) "Both Subliminals returned to HQ")
      (is (zero? (count (:discard (get-contestant)))) "No Subliminals in Archives"))))

(deftest success
  ;; Success
  (testing "Works with bad publicity"
    (do-game
      (new-game (default-contestant ["NAPD Contract" "Project Beale" "Success"])
                (default-challenger))
      (play-from-hand state :contestant "NAPD Contract" "New party")
      (play-from-hand state :contestant "Project Beale" "New party")
      (core/gain state :contestant :bad-publicity 9)
      (core/gain state :contestant :credit 8)
      (core/gain state :contestant :click 15)
      (let [napd (get-content state :party1 0)
            beale (get-content state :party2 0)]
        (dotimes [_ 13] (core/advance state :contestant {:card (refresh napd)}))
        (is (= 13 (get-counters (refresh napd) :advancement)))
        (core/score state :contestant {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-contestant))))
        (play-from-hand state :contestant "Success")
        (click-card state :contestant (get-scored state :contestant 0))
        (is (= "NAPD Contract" (:title (first (:rfg (get-contestant))))))
        (click-card state :contestant (refresh beale))
        (is (= 13 (get-counters (refresh beale) :advancement)))
        (core/score state :contestant {:card (refresh beale)})
        (is (= 7 (:agenda-point (get-contestant)))))))
  (testing "Works with public agendas"
    (do-game
      (new-game (default-contestant ["Oaktown Renovation" "Vanity Project" "Success"])
                (default-challenger))
      (core/gain state :contestant :click 1)
      (score-agenda state :contestant (find-card "Vanity Project" (:hand (get-contestant))))
      (is (= 4 (:agenda-point (get-contestant))))
      (play-from-hand state :contestant "Oaktown Renovation" "New party")
      (is (= 5 (:credit (get-contestant))))
      (play-from-hand state :contestant "Success")
      (click-card state :contestant (get-scored state :contestant 0))
      (is (= "Vanity Project" (:title (first (:rfg (get-contestant))))))
      (let [oaktown (get-content state :party1 0)]
        (click-card state :contestant (refresh oaktown))
        (is (= 6 (get-counters (refresh oaktown) :advancement)))
        (is (= 19 (:credit (get-contestant))) "Gain 2 + 2 + 2 + 2 + 3 + 3 = 14 credits for advancing Oaktown")
        (core/score state :contestant {:card (refresh oaktown)})
        (is (= 2 (:agenda-point (get-contestant)))))))
  (testing "interaction with Jemison, regression test for issue #2704"
    (do-game
      (new-game (make-deck "Jemison Astronautics: Sacrifcharacter. Audacity. Success."
                           ["Success"
                            "High-Risk Investment"
                            "Government Takeover"])
                (default-challenger))
      (core/gain state :contestant :click 1)
      (score-agenda state :contestant (find-card "High-Risk Investment" (:hand (get-contestant))))
      (play-from-hand state :contestant "Government Takeover" "New party")
      (play-from-hand state :contestant "Success")
      (click-card state :contestant (get-in (get-contestant) [:scored 0]))
      (let [gto (get-content state :party1 0)]
        ;; Prompt for Jemison
        (click-card state :contestant (refresh gto))
        (is (= 4 (get-counters (refresh gto) :advancement)) "Added 4 counters from Jemison trigger")
        ;; Prompt for Success
        (click-card state :contestant (refresh gto))
        (is (= (+ 4 5) (get-counters (refresh gto) :advancement)) "Advance 5 times from Success")))))

(deftest successful-demonstration
  ;; Successful Demonstration - Play if only Challenger made unsuccessful run last turn; gain 7 credits
  (do-game
    (new-game (default-contestant ["Successful Demonstration"])
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

(deftest surveillance-sweep
  ;; Surveillance Sweep
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Restructured Datapool" "Surveillance Sweep" "Data Raven"])
                (default-challenger ["Scrubbed"]))
      (is (zero? (:tag (get-challenger))) "Challenger should start with no tags")
      (play-from-hand state :contestant "Surveillance Sweep")
      (play-and-score state "Restructured Datapool")
      (let [rd-scored (get-scored state :contestant 0)]
        (card-ability state :contestant rd-scored 0)
        (is (not= :waiting (-> (get-contestant) :prompt first :prompt-type)) "Surveillance Sweep only works during a run")
        (click-prompt state :contestant "0")
        (click-prompt state :challenger "0")
        (is (= 1 (:tag (get-challenger))) "Challenger should gain a tag from Restructured Datapool ability"))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (play-from-hand state :contestant "Data Raven" "HQ")
      (take-credits state :contestant)
      (let [dr (get-character state :hq 0)]
        (core/reveal state :contestant (refresh dr))
        (run-on state :hq)
        (card-subroutine state :contestant dr 0)
        (is (prompt-is-type? state :contestant :waiting) "During a run, Contestant should wait on Challenger first")
        (click-prompt state :challenger "0")
        (click-prompt state :contestant "0")
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state)
        (play-from-hand state :challenger "Scrubbed")
        (run-on state :hq)
        (card-subroutine state :contestant dr 0)
        (is (prompt-is-type? state :challenger :waiting) "Challenger should now be waiting on Contestant")
        (click-prompt state :contestant "0")
        (click-prompt state :challenger "0")
        (is (= 2 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state))))
  (testing "trace during run after stealing an agenda"
    (do-game
      (new-game (default-contestant ["Surveillance Sweep" "Breaking News" "Forced Connection" "Data Raven"])
                (default-challenger))
      (core/gain state :contestant :click 4)
      (core/gain state :contestant :credit 20)
      (play-from-hand state :contestant "Surveillance Sweep")
      (play-from-hand state :contestant "Breaking News" "New party")
      (play-from-hand state :contestant "Forced Connection" "Locale 1")
      (play-from-hand state :contestant "Data Raven" "Locale 1")
      (take-credits state :contestant)
      (let [dr (get-character state :party1 0)
            bn (get-content state :party1 0)
            fc (get-content state :party1 1)]
        (core/reveal state :contestant (refresh dr))
        (run-on state :party1)
        (card-subroutine state :contestant dr 0)
        (is (prompt-is-type? state :contestant :waiting) "During a run, Contestant should wait on Challenger first")
        (click-prompt state :challenger "0")
        (click-prompt state :contestant "0")
        (is (= 1 (get-counters (refresh dr) :power)) "Data Raven should gain a power counter from trace")
        (run-successful state)
        (click-card state :challenger bn)
        (click-prompt state :challenger "Steal")
        (click-card state :challenger fc)
        (is (prompt-is-type? state :challenger :waiting) "After steal, Surveillance Sweep leaves play and Challenger waits on Contestant")))))

(deftest targeted-marketing
  ;; Targeted Marketing
  (do-game
    (new-game (default-contestant ["Targeted Marketing"])
              (default-challenger))
    (play-from-hand state :contestant "Targeted Marketing")
    (click-prompt state :contestant "Sure Gamble")
    (take-credits state :contestant)
    (let [credits (:credit (get-contestant))]
      (play-from-hand state :challenger "Sure Gamble")
      (is (= (+ 10 credits) (:credit (get-contestant))) "Contestant gains 10 credits from Challenger playing named card"))))

(deftest the-all-seeing-i
  (testing "Counts number of cards if one card is prevented discarded with fall guy"
    (do-game
      (new-game (default-contestant ["The All-Seeing I"])
                (default-challenger ["Fall Guy" (qty "Same Old Thing" 2)]))
      (letfn [(res [] (count (get-in (get-challenger) [:rig :radicle])))]
        (take-credits state :contestant)
        (play-from-hand state :challenger "Same Old Thing")
        (play-from-hand state :challenger "Fall Guy")
        (play-from-hand state :challenger "Same Old Thing")
        (take-credits state :challenger)
        (play-from-hand state :contestant "The All-Seeing I")
        (is (= 1 (count (:hand (get-contestant)))) "Contestant could not play All Seeing I when challenger was not tagged")
        (core/gain state :challenger :tag 1)
        (play-from-hand state :contestant "The All-Seeing I")
        (let [fall-guy (get-radicle state 1)]
          (card-ability state :challenger fall-guy 0))
        (click-prompt state :challenger "Done")
        (is (= 1 (res)) "One placed radicle saved by Fall Guy")
        (is (= 2 (count (:discard (get-challenger)))) "Two cards in heap"))))
  (testing "Checks that All-seeing I does not double-discard hosted cards, discards hosted cards"
    (do-game
      (new-game (default-contestant ["The All-Seeing I"])
                (default-challenger [(qty "Fall Guy" 2) "Off-Campus Apartment"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Off-Campus Apartment")
      (let [oca (get-radicle state 0)
            fg1 (get-in (get-challenger) [:hand 0])
            fg2 (get-in (get-challenger) [:hand 1])]
        (card-ability state :challenger oca 0)
        (click-card state :challenger fg1)
        (card-ability state :challenger oca 0)
        (click-card state :challenger fg2))
      (core/gain state :challenger :tag 1)
      (take-credits state :challenger)
      (play-from-hand state :contestant "The All-Seeing I")
      (click-prompt state :challenger "Done")
      (click-prompt state :challenger "Done")
      (let  [fall-guy (find-card "Fall Guy" (core/all-active-placed state :challenger))]
        (card-ability state :challenger fall-guy 0))
      (click-prompt state :challenger "Done") ;; This assumes hosted cards get put in discard-list before host
      (is (= 1 (count (core/all-active-placed state :challenger))) "One placed card (Off-Campus)")
      (is  (= 2 (count (:discard (get-challenger)))) "Two cards in heap")))
  (testing "should not discard Jarogniew Mercs if there are other placed radicles"
    (do-game
      (new-game (default-contestant [(qty "The All-Seeing I" 4)])
                (default-challenger [(qty "Jarogniew Mercs" 2) (qty "Same Old Thing" 2)]))
      (letfn [(res [] (count (get-in (get-challenger) [:rig :radicle])))]
        (take-credits state :contestant)
        (play-from-hand state :challenger "Same Old Thing")
        (play-from-hand state :challenger "Jarogniew Mercs")
        (take-credits state :challenger)
        (is (= 2 (res)) "There are two placed radicles")
        (play-from-hand state :contestant "The All-Seeing I")
        (is (= 1 (res)) "Jarogniew Mercs still placed")
        (play-from-hand state :contestant "The All-Seeing I")
        (is (zero? (res)) "There are no placed radicles")
        (take-credits state :contestant)
        (play-from-hand state :challenger "Jarogniew Mercs") ;; Testing if order matters
        (play-from-hand state :challenger "Same Old Thing")
        (take-credits state :challenger)
        (is (= 2 (res)) "There are two placed radicles")
        (play-from-hand state :contestant "The All-Seeing I")
        (is (= 1 (res)) "Jarogniew Mercs still placed")
        (play-from-hand state :contestant "The All-Seeing I")
        (is (zero? (res)) "There are no placed radicles")))))

(deftest threat-assessment
  ;; Threat Assessment - play only if challenger discarded a card last turn, move a card to the stack or take 2 tags
  (do-game
    (new-game (default-contestant [(qty "Threat Assessment" 3) "Adonis Campaign"])
              (default-challenger ["Desperado" "Corroder"]))
    (play-from-hand state :contestant "Adonis Campaign" "New party")
    (take-credits state :contestant)
    (run-on state :party1)
    (run-successful state)
    (click-prompt state :challenger "Pay 3 [Credits] to discard")
    (core/gain state :challenger :credit 5)
    (play-from-hand state :challenger "Desperado")
    (play-from-hand state :challenger "Corroder")
    (take-credits state :challenger)
    (is (zero? (:tag (get-challenger))) "Challenger starts with 0 tags")
    (play-from-hand state :contestant "Threat Assessment")
    (click-card state :contestant (find-card "Desperado" (-> (get-challenger) :rig :hazard)))
    (click-prompt state :challenger "2 tags")
    (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags")
    (is (= 1 (count (-> (get-challenger) :rig :hazard))) "Didn't discard Desperado")
    (is (= "Threat Assessment" (:title (first (:rfg (get-contestant))))) "Threat Assessment removed from game")
    (play-from-hand state :contestant "Threat Assessment")
    (click-card state :contestant (find-card "Corroder" (-> (get-challenger) :rig :resource)))
    (click-prompt state :challenger "Move Corroder")
    (is (= 2 (:tag (get-challenger))) "Challenger didn't take tags")
    (is (= "Corroder" (:title (first (:deck (get-challenger))))) "Moved Corroder to the deck")
    (is (= 2 (count (:rfg (get-contestant)))))
    (take-credits state :challenger)
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Threat Assessment")
    (is (empty? (:prompt (get-contestant))) "Threat Assessment triggered with no discard")))

(deftest threat-level-alpha
  ;; Threat Level Alpha - Win trace to give tags = Challenger tags; or 1 tag if 0
  (do-game
    (new-game (default-contestant [(qty "Threat Level Alpha" 2)])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (core/gain state :contestant :credit 2)
    (is (zero? (:tag (get-challenger))))
    (play-from-hand state :contestant "Threat Level Alpha")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag because they had 0")
    (core/gain state :challenger :tag 2)
    (play-from-hand state :contestant "Threat Level Alpha")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 6 (:tag (get-challenger))) "Challenger took 3 tag because they had 3")))

(deftest transparency-initiative
  ;; Transparency Initiative - Full test
  (do-game
    (new-game (default-contestant ["Transparency Initiative" "Oaktown Renovation"
                             "Project Atlas" "Hostile Takeover" "Casting Call"])
              (default-challenger))
    (core/gain state :contestant :click 5)
    (play-from-hand state :contestant "Oaktown Renovation" "New party")
    (play-from-hand state :contestant "Casting Call")
    (click-card state :contestant (find-card "Project Atlas" (:hand (get-contestant))))
    (click-prompt state :contestant "New party")
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (let [oaktown (get-content state :party1 0)
          atlas (get-content state :party2 0)
          hostile (get-content state :party3 0)]
      (play-from-hand state :contestant "Transparency Initiative")
      (click-card state :contestant (refresh oaktown))
      ;; doesn't work on face-up agendas
      (is (zero? (count (:hosted (refresh oaktown)))))
      (click-card state :contestant (refresh atlas))
      (is (= 1 (count (:hosted (refresh atlas)))) "Casting Call")
      ;; works on facedown agenda
      (click-card state :contestant (refresh hostile))
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

(deftest trojan-horse
  ;; Trojan Horse
  (do-game
    (new-game (default-contestant ["Trojan Horse" "Dedicated Response Team"])
              (default-challenger ["Wyrm"]))
    (play-from-hand state :contestant "Dedicated Response Team" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Wyrm")
    (run-empty-locale state :party1)
    (take-credits state :challenger)
    (is (zero? (-> (get-challenger) :discard count)) "Challenger should start with 0 cards in heap")
    (play-from-hand state :contestant "Trojan Horse")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (click-card state :contestant (get-resource state 0))
    (is (= 1 (-> (get-challenger) :discard count)) "Wyrm should be in heap after Challenger loses Trojan Horse trace")))

(deftest under-the-bus
  ;; Under the Bus
  (do-game
    (new-game (default-contestant ["Under the Bus"])
              (default-challenger ["Film Critic"]))
    (take-credits state :contestant)
    (run-on state :hq)
    (run-successful state)
    (click-prompt state :challenger "No action")
    (play-from-hand state :challenger "Film Critic")
    (take-credits state :challenger)
    (is (= 1 (count (get-radicle state))) "Challenger has 1 radicle placed")
    (is (zero? (:bad-publicity (get-contestant))) "Contestant has no bad pub")
    (play-from-hand state :contestant "Under the Bus")
    (click-card state :contestant (get-radicle state 0))
    (is (empty? (get-radicle state)) "Challenger has no radicle placed")
    (is (= 1 (count (:discard (get-challenger)))) "Challenger has 1 discarded card")
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant takes 1 bad pub")))

(deftest wake-up-call
  ;; Wake Up Call
  (testing "should fire after using En Passant to discard character"
    (do-game
      (new-game (default-contestant ["Enigma" "Wake Up Call"])
                (default-challenger ["En Passant" "Maya"]))
      (play-from-hand state :contestant "Enigma" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Maya")
      (run-on state :hq)
      (run-successful state)
      (click-prompt state :challenger "No action")
      (is (zero? (count (:discard (get-contestant)))) "Contestant starts with no discards")
      (play-from-hand state :challenger "En Passant")
      (click-card state :challenger (get-character state :hq 0))
      (is (= 1 (count (:discard (get-contestant)))) "Contestant discards placed character")
      (take-credits state :challenger)
      (is (= 1 (count (:discard (get-challenger)))) "Challenger starts with 1 discarded card (En Passant)")
      (play-from-hand state :contestant "Wake Up Call")
      (click-card state :contestant (get-hazard state 0))
      (click-prompt state :challenger "Discard Maya")
      (is (= 2 (count (:discard (get-challenger)))) "Maya is discarded")
      (is (= 1 (count (:rfg (get-contestant)))) "Wake Up Call is removed from the game"))))

(deftest wetwork-refit
  ;; Wetwork Refit - Only works on Bioroid Character and adds a subroutine
  (do-game
    (new-game (default-contestant ["Eli 1.0"
                             "Vanilla"
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
      (click-prompt state :contestant "Done")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/reveal state :contestant (refresh eli))
      (core/reveal state :contestant (refresh vanilla))
      (play-from-hand state :contestant "Wetwork Refit")
      (click-card state :contestant (refresh eli))
      (is (= "Wetwork Refit" (:title (first (:hosted (refresh eli)))))
          "Wetwork Refit is hosted on Eli 1.0")
      (is (= 2 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 2 different subroutines")
      (is (= "[Wetwork Refit] Do 1 brain damage" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has a brain damage subroutine as his first subroutine")
      (core/move state :contestant (first (:hosted (refresh eli))) :hand)
      (is (empty? (:hosted (refresh eli))) "No cards are hosted on Eli 1.0")
      (is (= 1 (count (:subroutines (refresh eli))))
          "Eli 1.0 has 1 different subroutine")
      (is (= "End the run" (:label (first (:subroutines (refresh eli)))))
          "Eli 1.0 has an end the run subroutine as his first subroutine")
      (play-from-hand state :contestant "Wetwork Refit")
      (click-card state :contestant (refresh vanilla))
      (is (not= "Wetwork Refit" (:title (first (:hosted (refresh vanilla)))))
          "Wetwork Refit is not hosted on Vanilla"))))
