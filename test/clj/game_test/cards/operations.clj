(ns game-test.cards.operations
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "operations"))

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
        (prompt-select :contestant ss)
        (prompt-choice :contestant "2")
        (prompt-select :contestant co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (prompt-select :contestant hf)
        (is (= 9 (:credit (get-contestant))) "Contestant gained credits from Hedge Fund")
        (prompt-select :contestant bc)
        (prompt-select :contestant (refresh co))
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
        (prompt-select :contestant elp)
        (is (= "Enhanced Login Protocol" (:title (first (get-in @state [:contestant :current]))))
            "Enhanced Login Protocol active in Current area")
        (prompt-select :contestant ss)
        (prompt-choice :contestant "2")
        (prompt-select :contestant co)
        (is (= 2 (get-counters (refresh co) :advancement)) "Cerebral Overwriter gained 2 advancements")
        (prompt-select :contestant hf)
        (is (= 9 (:credit (get-contestant))) "Contestant gained credits from Hedge Fund")))))

(deftest an-offer-you-can't-refuse
  ;; An Offer You Can't Refuse - exact card added to score area, not the last discarded one
  (do-game
    (new-game (default-contestant ["Celebrity Gift" "An Offer You Can't Refuse"])
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
                               "Character Wall"])
                (default-challenger))
      (play-from-hand state :contestant "Character Wall" "HQ")
      (core/add-counter state :contestant (refresh (get-character state :hq 0)) :advancement 1)
      (play-from-hand state :contestant "Commercialization")
      (prompt-select :contestant (refresh (get-character state :hq 0)))
      (is (= 6 (:credit (get-contestant))) "Gained 1 for single advanced character from Commercialization")))
  (testing "Two advancement tokens"
    (do-game
      (new-game (default-contestant ["Commercialization"
                               "Character Wall"])
                (default-challenger))
      (play-from-hand state :contestant "Character Wall" "HQ")
      (core/add-counter state :contestant (refresh (get-character state :hq 0)) :advancement 2)
      (play-from-hand state :contestant "Commercialization")
      (prompt-select :contestant (refresh (get-character state :hq 0)))
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
        (prompt-card :contestant (find-card "Beanstalk Royalties" (:deck (get-contestant))))
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
        (prompt-card :contestant (find-card "Consulting Visit" (:deck (get-contestant))))
        (is (= 3 (:credit (get-contestant))))
        (is (= (list "Beanstalk Royalties" "Green Level Clearance" nil) (prompt-names)))
        (prompt-card :contestant (find-card "Green Level Clearance" (:deck (get-contestant))))
        (is (= 5 (:credit (get-contestant))))))))

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

(deftest hedge-fund
  (do-game
    (new-game (default-contestant) (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 9 (:credit (get-contestant))))))

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

(deftest invasion-of-privacy
  ;; Invasion of Privacy - Full test
  (do-game
    (new-game (default-contestant [(qty "Invasion of Privacy" 3)])
              (default-challenger [(qty "Sure Gamble" 2) "Fall Guy" (qty "Cache" 2)]))
    (core/gain state :contestant :click 3 :credit 6)
    ;; discard 2 cards
    (play-from-hand state :contestant "Invasion of Privacy")
    (prompt-choice :contestant 0) ; default trace
    (prompt-choice :challenger 0) ; Challenger won't match
    (is (= 5 (count (:hand (get-challenger)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" "Sure Gamble" nil) (prompt-names)))
      (prompt-card :contestant (find-card "Sure Gamble" (:hand (get-challenger))))
      (prompt-card :contestant (find-card "Sure Gamble" (:hand (get-challenger)))))
    (is (= 3 (count (:hand (get-challenger)))))
    ;; able to discard 2 cards but only 1 available target in Challenger's hand
    (play-from-hand state :contestant "Invasion of Privacy")
    (prompt-choice :contestant 0) ; default trace
    (prompt-choice :challenger 0) ; Challenger won't match
    (is (= 3 (count (:hand (get-challenger)))))
    (let [get-prompt (fn [] (first (#(get-in @state [:contestant :prompt]))))
          prompt-names (fn [] (map #(:title %) (:choices (get-prompt))))]
      (is (= (list "Fall Guy" nil) (prompt-names)))
      (prompt-card :contestant (find-card "Fall Guy" (:hand (get-challenger))))
      (is (empty? (get-in @state [:contestant :prompt])) "No prompt for second card"))
    (is (= 2 (count (:hand (get-challenger)))))
    ;; failed trace - take the bad publicity
    (play-from-hand state :contestant "Invasion of Privacy")
    (prompt-choice :contestant 0) ; default trace
    (prompt-choice :challenger 2) ; Challenger matches
    (is (= 1 (:bad-publicity (get-contestant))))))

(deftest lateral-growth
  (do-game
    (new-game (default-contestant ["Lateral Growth" "Breaking News"])
              (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (play-from-hand state :contestant "Lateral Growth")
    (prompt-select :contestant (find-card "Breaking News" (:hand (get-contestant))))
    (prompt-choice :contestant "New party")
    (is (= "Breaking News" (:title (get-content state :party1 0)))
      "Breaking News placed by Lateral Growth")
    (is (= 7 (:credit (get-contestant))))))

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

(deftest mutate
  ;; Mutate - discard a revealed piece of character, place and reveal one from R&D
  (testing "Basic operation"
    (do-game
      (new-game (default-contestant ["Mutate" "Character Wall" "Enigma" "Hedge Fund"])
                (default-challenger))
      (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Enigma" (:hand (get-contestant))) :deck)
      (play-from-hand state :contestant "Character Wall" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (is (= 1 (count (get-character state :hq))) "1 character placed")
      (is (= "Character Wall" (:title (get-character state :hq 0))) "Character Wall is placed")
      (play-from-hand state :contestant "Mutate")
      (prompt-select :contestant (get-character state :hq 0))
      (is (= 1 (count (get-character state :hq))) "1 character placed")
      (is (= "Enigma" (:title (get-character state :hq 0))) "Enigma is placed")
      (is (:revealed (get-character state :hq 0)) "Enigma is revealed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged")
      (is (second-last-log-contains? state "Enigma") "Placed card name was logged")))
  (testing "No character in R&D"
    (do-game
      (new-game (default-contestant ["Mutate" "Character Wall" "Enigma" "Hedge Fund"])
                (default-challenger))
      (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
      (play-from-hand state :contestant "Character Wall" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (is (= 1 (count (get-character state :hq))) "1 character placed")
      (is (= "Character Wall" (:title (get-character state :hq 0))) "Character Wall is placed")
      (play-from-hand state :contestant "Mutate")
      (prompt-select :contestant (get-character state :hq 0))
      (is (empty? (get-character state :hq)) "No character placed")
      (is (second-last-log-contains? state "Hedge Fund") "Skipped card name was logged"))))

(deftest oversight-ai
  ;; Oversight AI - Reveal a piece of Character ignoring all costs
  (do-game
    (new-game (default-contestant ["Oversight AI" "Archer"])
              (default-challenger))
    (play-from-hand state :contestant "Archer" "R&D")
    (let [archer (get-character state :rd 0)]
      (play-from-hand state :contestant "Oversight AI")
      (prompt-select :contestant archer)
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
    (prompt-select :contestant (get-character state :hq 0))
    (is (= 2 (:current-strength (get-character state :hq 0))) "Vanilla at 2 strength")))

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

(deftest precognition
  ;; Precognition - Full test
  (do-game
    (new-game (default-contestant ["Precognition" "Caprcharacter Nisei" "Adonis Campaign"
                             "Quandary" "Jackson Howard" "Global Food Initiative"])
              (default-challenger))
    (starting-hand state :contestant ["Precognition"])
    (play-from-hand state :contestant "Precognition")
    (prompt-card :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
    (prompt-card :contestant (find-card "Adonis Campaign" (:deck (get-contestant))))
    (prompt-card :contestant (find-card "Quandary" (:deck (get-contestant))))
    (prompt-card :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
    (prompt-card :contestant (find-card "Global Food Initiative" (:deck (get-contestant))))
    ;; try starting over
    (prompt-choice :contestant "Start over")
    (prompt-card :contestant (find-card "Global Food Initiative" (:deck (get-contestant))))
    (prompt-card :contestant (find-card "Jackson Howard" (:deck (get-contestant))))
    (prompt-card :contestant (find-card "Quandary" (:deck (get-contestant))))
    (prompt-card :contestant (find-card "Adonis Campaign" (:deck (get-contestant))))
    (prompt-card :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant)))) ;this is the top card of R&D
    (prompt-choice :contestant "Done")
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
      (prompt-select :contestant (first (:discard (get-contestant))))
      (prompt-select :contestant (second (:discard (get-contestant))))
      (prompt-select :contestant (last (:discard (get-contestant))))
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
      (prompt-select :contestant (first (:discard (get-contestant))))
      (prompt-select :contestant (last (:discard (get-contestant))))
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
      (prompt-select :contestant (first (:discard (get-contestant))))
      (prompt-select :contestant (last (:discard (get-contestant))))
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
      (prompt-choice :contestant 4)
      (prompt-select :contestant pj)
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
    (prompt-card :contestant (find-card "Caprcharacter Nisei" (:deck (get-contestant))))
    (prompt-choice :contestant "New party")
    (is (= "Caprcharacter Nisei" (:title (get-content state :party1 0)))
      "Caprcharacter Nisei placed by Psychokinesis")
    ;; Test placing an Site
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Psychokinesis")
    (prompt-card :contestant (find-card "Adonis Campaign" (:deck (get-contestant))))
    (prompt-choice :contestant "New party")
    (is (= "Adonis Campaign" (:title (get-content state :party2 0)))
      "Adonis Campaign placed by Psychokinesis")
    ;; Test placing an Agenda
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Psychokinesis")
    (prompt-card :contestant (find-card "Global Food Initiative" (:deck (get-contestant))))
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

(deftest red-planet-couriers
  ;; Red Planet Couriers - Move all advancements on cards to 1 advanceable card
  (do-game
    (new-game (default-contestant ["Red Planet Couriers" (qty "Character Wall" 2)
                             "GRNDL Refinery" "Government Takeover"])
              (default-challenger))
    (core/gain state :contestant :click 4)
    (play-from-hand state :contestant "Government Takeover" "New party")
    (play-from-hand state :contestant "GRNDL Refinery" "New party")
    (play-from-hand state :contestant "Character Wall" "HQ")
    (play-from-hand state :contestant "Character Wall" "R&D")
    (let [gt (get-content state :party1 0)
          gr (get-content state :party2 0)
          iw1 (get-character state :hq 0)
          iw2 (get-character state :rd 0)]
      (core/add-prop state :contestant gr :advance-counter 3)
      (core/add-prop state :contestant iw1 :advance-counter 2)
      (core/add-prop state :contestant iw2 :advance-counter 1)
      (play-from-hand state :contestant "Red Planet Couriers")
      (prompt-select :contestant gt)
      (is (zero? (get-counters (refresh gr) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw1) :advancement)) "Advancements removed")
      (is (zero? (get-counters (refresh iw2) :advancement)) "Advancements removed")
      (is (= 6 (get-counters (refresh gt) :advancement)) "Gained 6 advancements"))))

(deftest reuse
  ;; Reuse - Gain 2 credits for each card discarded from HQ
  (do-game
    (new-game (default-contestant [(qty "Reuse" 2) "Hive" "IQ"
                             "Character Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Reuse")
    (prompt-select :contestant (find-card "Character Wall" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "Hive" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "IQ" (:hand (get-contestant))))
    (prompt-choice :contestant "Done")
    (is (= 4 (count (:discard (get-contestant)))) "3 cards discarded plus operation played")
    (is (= 11 (:credit (get-contestant))) "Gained 6 credits")
    (is (= 1 (:click (get-contestant))) "Spent 2 clicks")))

(deftest salem's-hospitality
  ;; Salem's Hospitality - Full test
  (do-game
    (new-game (default-contestant [(qty "Salem's Hospitality" 3)])
              (default-challenger [(qty "I've Had Worse" 3) "Faust"
                               "Levy AR Lab Access"]))
    (play-from-hand state :contestant "Salem's Hospitality")
    (is (= 5 (count (:hand (get-challenger)))))
    (prompt-choice :contestant "I've Had Worse")
    (is (= 2 (count (:hand (get-challenger)))))
    (play-from-hand state :contestant "Salem's Hospitality")
    (prompt-choice :contestant "Plascrete Carapace")
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

(deftest shipment-from-sansan
  ;; Shipment from SanSan - placing advancements
  (do-game
    (new-game (default-contestant [(qty "Shipment from SanSan" 3) (qty "Character Wall" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Character Wall" "HQ")
    (let [iwall (get-character state :hq 0)]
      (play-from-hand state :contestant "Shipment from SanSan")
      (prompt-choice :contestant "2")
      (prompt-select :contestant iwall)
      (is (= 5 (:credit (get-contestant))))
      (is (= 2 (get-counters (refresh iwall) :advancement))))))

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
      (prompt-select :contestant (refresh qu))
      (is (core/has-subtype? (refresh qu) "Code Gate") "Quandary has Code Gate")
      (is (core/has-subtype? (refresh qu) "Barrier") "Quandary Character Barrier")
      (is (= 2 (count (:subroutines (refresh qu)))) "Quandry gains a subroutine"))))

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
        (prompt-select :contestant (get-scored state :contestant 0))
        (is (= "NAPD Contract" (:title (first (:rfg (get-contestant))))))
        (prompt-select :contestant (refresh beale))
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
      (prompt-select :contestant (get-scored state :contestant 0))
      (is (= "Vanity Project" (:title (first (:rfg (get-contestant))))))
      (let [oaktown (get-content state :party1 0)]
        (prompt-select :contestant (refresh oaktown))
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
      (prompt-select :contestant (get-in (get-contestant) [:scored 0]))
      (let [gto (get-content state :party1 0)]
        ;; Prompt for Jemison
        (prompt-select :contestant (refresh gto))
        (is (= 4 (get-counters (refresh gto) :advancement)) "Added 4 counters from Jemison trigger")
        ;; Prompt for Success
        (prompt-select :contestant (refresh gto))
        (is (= (+ 4 5) (get-counters (refresh gto) :advancement)) "Advance 5 times from Success")))))

(deftest threat-level-alpha
  ;; Threat Level Alpha - Win trace to give tags = Challenger tags; or 1 tag if 0
  (do-game
    (new-game (default-contestant [(qty "Threat Level Alpha" 2)])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (core/gain state :contestant :credit 2)
    (is (zero? (:tag (get-challenger))))
    (play-from-hand state :contestant "Threat Level Alpha")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag because they had 0")
    (core/gain state :challenger :tag 2)
    (play-from-hand state :contestant "Threat Level Alpha")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
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
    (prompt-select :contestant (find-card "Project Atlas" (:hand (get-contestant))))
    (prompt-choice :contestant "New party")
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (let [oaktown (get-content state :party1 0)
          atlas (get-content state :party2 0)
          hostile (get-content state :party3 0)]
      (play-from-hand state :contestant "Transparency Initiative")
      (prompt-select :contestant (refresh oaktown))
      ;; doesn't work on face-up agendas
      (is (zero? (count (:hosted (refresh oaktown)))))
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
