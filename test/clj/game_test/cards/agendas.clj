(ns game-test.cards.agendas
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "agendas"))

(deftest ^{:card-title "15-minutes"}
  fifteen-minutes
  ;; 15 Minutes - check if it works correctly from both sides
  (do-game
    (new-game (default-contestant ["15 Minutes"])
              (default-challenger))
    (play-from-hand state :contestant "15 Minutes" "New party")
    (take-credits state :contestant)
    ;; use 15 minutes to take it away from challenger
    (run-empty-locale state "Locale 1")
    (click-prompt state :challenger "Steal")
    (take-credits state :challenger)
    (is (= 1 (:agenda-point (get-challenger))))
    (is (= 1 (count (:scored (get-challenger)))))
    (let [fifm (first (:scored (get-challenger)))]
      (is (= 3 (:click (get-contestant))))
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :contestant (refresh fifm) 0)
      (is (zero? (:agenda-point (get-challenger))))
      (is (zero? (count (:scored (get-challenger))))))
    (is (= "15 Minutes" (:title (first (:deck (get-contestant))))))
    ;; TODO: could also check for deck shuffle
    (is (= 2 (:click (get-contestant))))
    ;; use 15 minutes to take it away from contestant (hey, maybe some obscure case happens where contestant would want that)
    (core/click-draw state :contestant 1)
    (play-from-hand state :contestant "15 Minutes" "New party")
    (take-credits state :challenger)
    (score-agenda state :contestant (get-content state :party2 0))
    (is (= 1 (:agenda-point (get-contestant))))
    (is (= 1 (count (:scored (get-contestant)))))
    (let [fifm (first (:scored (get-contestant)))]
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :contestant (refresh fifm) 0)
      (is (zero? (:agenda-point (get-contestant))))
      (is (zero? (count (:scored (get-contestant))))))
    (is (= "15 Minutes" (:title (first (:deck (get-contestant))))))))

(deftest accelerated-beta-test
  ;; Accelerated Beta Test
  (do-game
    (new-game (default-contestant ["Accelerated Beta Test" "Enigma" (qty "Hedge Fund" 2)])
              (default-challenger))
    ;; Set up
    (starting-hand state :contestant ["Accelerated Beta Test"])
    (play-and-score state "Accelerated Beta Test")
    (click-prompt state :contestant "Yes")
    (click-card state :contestant (find-card "Enigma" (get-in @state [:contestant :play-area])))
    (click-prompt state :contestant "HQ")
    (is (some? (get-character state :hq 0)))
    (is (= 2 (count (:discard (get-contestant)))))
    (core/move state :contestant (find-card "Accelerated Beta Test" (:scored (get-contestant))) :hand)
    (core/move state :contestant (find-card "Hedge Fund" (:discard (get-contestant))) :deck)
    (core/move state :contestant (find-card "Hedge Fund" (:discard (get-contestant))) :deck)
    (play-and-score state "Accelerated Beta Test")
    (click-prompt state :contestant "Yes")
    (click-prompt state :contestant "I have no regrets")
    (is (= 2 (count (:discard (get-contestant)))))))

(deftest advanced-concept-hopper
  ;; Advanced Concept Hopper
  (do-game
    (new-game (default-contestant ["Advanced Concept Hopper" (qty "Hedge Fund" 4)])
              (default-challenger))
    (starting-hand state :contestant ["Advanced Concept Hopper"])
    (play-and-score state "Advanced Concept Hopper")
    (take-credits state :contestant)
    (testing "Contestant draws 1 card, only once per turn"
      (let [cards (count (:hand (get-contestant)))]
        (is (= cards (count (:hand (get-contestant)))) (str "Contestant should have " cards " cards in hand"))
        (run-on state :archives)
        (click-prompt state :contestant "Draw 1 card")
        (is (= (inc cards) (count (:hand (get-contestant)))) (str "Contestant should have " (inc cards) " card in hand"))
        (run-successful state)
        (run-on state :archives)
        (is (empty (:prompt (get-contestant))) "No prompt as it's once per turn")))
    (take-credits state :challenger)
    (take-credits state :contestant)
    (testing "Contestant gains 1 credit, only once per turn"
      (let [credits (:credit (get-contestant))]
        (is (= credits (:credit (get-contestant))) (str "Contestant should have " credits " credits"))
        (run-on state :archives)
        (click-prompt state :contestant "Gain 1 [Credits]")
        (is (= (inc credits) (:credit (get-contestant))) (str "Contestant should have " (inc credits) " credits"))
        (run-successful state)
        (run-on state :archives)
        (is (empty (:prompt (get-contestant))) "No prompt as it's once per turn")))))

(deftest ancestral-imager
  ;; Ancestral Imager
  (do-game
    (new-game (default-contestant [(qty "Ancestral Imager" 3)])
              (default-challenger))
    (play-and-score state "Ancestral Imager")
    (take-credits state :contestant)
    (let [grip (count (:hand (get-challenger)))]
      (is (= grip (count (:hand (get-challenger)))) (str "Challenger has " grip " cards in hand"))
      (run-on state :hq)
      (run-jack-out state)
      (is (= (dec grip) (count (:hand (get-challenger)))) "Challenger took 1 net damage"))))

(deftest ar-enhanced-security
  ;; AR-Enhanced Security
  (do-game
    (new-game (default-contestant ["AR-Enhanced Security" (qty "NGO Front" 3)])
              (default-challenger))
    (testing "set up"
      (core/gain state :contestant :click 10 :credit 10)
      (core/gain state :challenger :credit 10)
      (dotimes [_ 3]
        (play-from-hand state :contestant "NGO Front" "New party"))
      (take-credits state :contestant))
    (testing "don't take a tag from discarding normally"
      (run-on state :party1)
      (run-successful state)
      (click-prompt state :challenger "Pay 1 [Credits] to discard")
      (is (= 1 (count (:discard (get-contestant)))) "discarded")
      (is (zero? (:tag (get-challenger))) "Challenger took 0 tags")
      (take-credits state :challenger)
      (play-and-score state "AR-Enhanced Security")
      (take-credits state :contestant))
    (testing "gain a tag from first discard"
      (run-on state :party2)
      (run-successful state)
      (click-prompt state :challenger "Pay 1 [Credits] to discard")
      (is (= 2 (count (:discard (get-contestant)))) "discarded")
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag"))
    (testing "don't gain a tag from second discard"
      (run-on state :party3)
      (run-successful state)
      (click-prompt state :challenger "Pay 1 [Credits] to discard")
      (is (= 3 (count (:discard (get-contestant)))) "discarded")
      (is (= 1 (:tag (get-challenger))) "Challenger took 0 tags"))))

(deftest armed-intimidation
  ;; Armed Intimidation
  (do-game
    (new-game (default-contestant [(qty "Armed Intimidation" 2)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 2)]))
    (play-and-score state "Armed Intimidation")
    (click-prompt state :challenger "Take 2 tags")
    (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags from Armed Intimidation tag choice")
    (play-and-score state "Armed Intimidation")
    (is (= 5 (count (:hand (get-challenger)))) "Challenger has 5 cards before Armed Intimidation meat damage")
    (click-prompt state :challenger "Suffer 5 meat damage")
    (is (zero? (count (:hand (get-challenger)))) "Challenger has 0 cards after Armed Intimidation meat damage")))

(deftest armored-locales
  ;; Armored Locales
  (do-game
    (new-game (default-contestant ["Armored Locales"])
              (default-challenger))
    (play-and-score state "Armored Locales")
    (let [as-scored (get-scored state :contestant 0)]
      (is (= 1 (get-counters (refresh as-scored) :agenda)) "Should start with 1 agenda counters")
      (take-credits state :contestant)
      (run-on state "HQ")
      (card-ability state :contestant as-scored 0)
      (is (last-log-contains? state "make the Challenger discard") "Should only write to log"))))

(deftest astroscript-pilot-resource
  ;; AstroScript token placement
  (do-game
    (new-game (default-contestant [(qty "AstroScript Pilot Resource" 3) (qty "Ice Wall" 2)])
              (default-challenger))
    (core/gain state :contestant :click 3)
    (letfn [(try-place [from to]
              (card-ability state :contestant (refresh from) 0)
              (click-card state :contestant (refresh to)))
            (should-not-place [from to msg]
              (try-place from to)
              (click-prompt state :contestant "Done")
              (is (= 1 (get-counters (refresh from) :agenda))
                  (str (:title from)" token was not used on " (:title to) msg))
              (is (zero? (get-counters (refresh to) :advancement))
                  (str "Advancement token not placed on " (:title to) msg)))
            (should-place [from to msg]
              (try-place from to)
              (is (zero? (get-counters (refresh from) :agenda))
                  (str (:title from) " token was used on " (:title to) msg))
              (is (= 1 (get-counters (refresh to) :advancement))
                  (str "Advancement token placed on " (:title to) msg)))]
      (play-and-score state "AstroScript Pilot Resource")
      (play-from-hand state :contestant "AstroScript Pilot Resource" "New party")
      (let [scored-astro (get-scored state :contestant 0)
            placed-astro (get-content state :party2 0)
            hand-astro (find-card "AstroScript Pilot Resource" (:hand (get-contestant)))]
        (should-not-place scored-astro hand-astro " in hand")
        (should-place scored-astro placed-astro " that is placed")
        (advance state placed-astro 2)
        (core/score state :contestant {:card (refresh placed-astro)}))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (let [no-token-astro (get-scored state :contestant 0)
            token-astro (get-scored state :contestant 1)
            hand-character-wall (find-card "Ice Wall" (:hand (get-contestant)))
            placed-character-wall (get-character state :hq 0)]
        (should-not-place token-astro no-token-astro " that is scored")
        (should-not-place token-astro hand-character-wall " in hand")
        (should-place token-astro placed-character-wall " that is placed")))))

(deftest award-bait
  ;; Award Bait
  (do-game
    (new-game (default-contestant [(qty "Award Bait" 2) "Ice Wall"])
              (default-challenger))
    (core/move state :contestant (find-card "Award Bait" (:hand (get-contestant))) :deck)
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [iw (get-character state :hq 0)]
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (play-from-hand state :contestant "Award Bait" "New party")
      (take-credits state :contestant)
      (run-on state :party1)
      (run-successful state)
      (click-prompt state :contestant "2")
      (click-card state :contestant "Ice Wall")
      (click-prompt state :challenger "Steal")
      (is (= 2 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement tokens")
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :contestant "2")
      (click-card state :contestant (refresh iw))
      (click-prompt state :challenger "Steal")
      (is (= 4 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement tokens"))))

(deftest bacterial-resourceming
  ;; Bacterial Resourceming
  (testing "Scoring should not cause a run to exist for challenger."
    (do-game
      (new-game (default-contestant ["Bacterial Resourceming" "Hedge Fund"])
                (default-challenger))
      (starting-hand state :contestant ["Bacterial Resourceming"])
      (play-and-score state "Bacterial Resourceming")
      (click-prompt state :contestant "Yes")
      (click-prompt state :contestant "Done")
      (click-prompt state :contestant "Done")
      (click-prompt state :contestant (first (:deck (get-contestant))))
      (click-prompt state :contestant "Done")
      (is (empty (:prompt (get-contestant))) "Bacterial Resourceming prompts finished")
      (is (not (:run @state)) "No run is active")))
  (testing "Removing all cards from R&D should not freeze for challenger, nor give an extra access."
    (do-game
      (new-game (default-contestant [(qty "Bacterial Resourceming" 8)])
                (default-challenger)
                {:start-as :challenger})
      (starting-hand state :contestant [])
      (run-empty-locale state :rd)
      (click-prompt state :challenger "Steal")
      (click-prompt state :contestant "Yes")
      ;; Move all 7 cards to discard
      (doseq [_ (range 7)
              ;; Get the first card listed in the prompt choice
              ;; TODO make this function
              :let [card (-> @state
                             (get-in [:contestant :prompt])
                             first
                             (get-in [:choices 0]))]]
        (click-prompt state :contestant card))
      (click-prompt state :contestant "Done")                          ; Finished with discarding
      (click-prompt state :contestant "Done")                          ; Finished with move-to-hq (no cards to move)
      ;; Run and prompts should be over now
      (is (empty (:prompt (get-contestant))) "Bacterial Resourceming prompts finished")
      (is (empty (:prompt (get-challenger))) "Bacterial Resourceming prompts finished")
      (is (not (:run @state))))))

(deftest better-citizen-resource
  ;; Better Citizen Resource
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Better Citizen Resource"])
                (default-challenger [(qty "The Maker's Eye" 2)
                                 (qty "Wyrm" 2)]))
      (play-and-score state "Better Citizen Resource")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (is (zero? (:tag (get-challenger))) "Challenger starts with 0 tags")
      (play-from-hand state :challenger "The Maker's Eye")
      (click-prompt state :contestant "Yes")
      (is (= 1 (:tag (get-challenger))) "Challenger takes 1 tag for playing a Run event")
      (run-successful state)
      (play-from-hand state :challenger "Wyrm")
      (is (empty? (-> (get-contestant) :prompt)) "Contestant shouldn't get a prompt to use Better Citizen Resource")
      (is (= 1 (:tag (get-challenger))) "Challenger doesn't gain a tag from placing an characterbreaker after playing a Run event")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Wyrm")
      (click-prompt state :contestant "Yes")
      (is (= 2 (:tag (get-challenger))) "Challenger gains 1 tag for placing an Icebreaker")
      (play-from-hand state :challenger "The Maker's Eye")
      (is (empty? (-> (get-contestant) :prompt)) "Contestant shouldn't get a prompt to use Better Citizen Resource")
      (is (= 2 (:tag (get-challenger))) "Challenger doesn't gain a tag from playing a Run event after placing an Icebreaker")
      (run-successful state)))
  (testing "Should only trigger on Run events. #3619"
    (do-game
      (new-game (default-contestant ["Better Citizen Resource"])
                (default-challenger ["Mining Accident"]))
      (play-and-score state "Better Citizen Resource")
      (take-credits state :contestant)
      (run-empty-locale state "HQ")
      (play-from-hand state :challenger "Mining Accident")
      (click-prompt state :contestant "Pay 5 [Credits]")
      (is (empty? (-> (get-contestant) :prompt)) "Contestant shouldn't get a prompt to use Better Citizen Resource")
      (is (zero? (:tag (get-challenger))) "Challenger should not gain a tag from playing a non-Run event"))))

(deftest bifrost-array
  ;; Bifrost Array
  (do-game
    (new-game (default-contestant ["Bifrost Array" "Hostile Takeover"])
              (default-challenger))
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-contestant))) "Should gain 7 credits from 5 to 12")
    (is (= 1 (:bad-publicity (get-contestant))) "Should gain 1 bad publicity")
    (let [ht-scored (get-scored state :contestant 0)]
      (play-and-score state "Bifrost Array")
      (click-prompt state :contestant "Yes")
      (click-card state :contestant "Hostile Takeover")
      (is (= 19 (:credit (get-contestant))) "Should gain 7 credits from 12 to 19")
      (is (= 2 (:bad-publicity (get-contestant))) "Should gain 1 bad publicity"))))

(deftest brain-rewiring
  ;; Brain Rewiring
  (do-game
    (new-game (default-contestant ["Brain Rewiring"])
              (default-challenger))
    (starting-hand state :challenger ["Sure Gamble" "Sure Gamble"])
    (play-and-score state "Brain Rewiring")
    (click-prompt state :contestant "Yes")
    (click-prompt state :contestant "2")
    (is (= 1 (count (:hand (get-challenger)))))))

(deftest braintrust
  ;; Braintrust
  (do-game
    (new-game (default-contestant ["Braintrust" "Ichi 1.0"])
              (default-challenger))
    (play-from-hand state :contestant "Braintrust" "New party")
    (let [bt (get-content state :party1 0)]
      (core/add-prop state :contestant bt :advance-counter 7)
      (core/score state :contestant {:card (refresh bt)})
      (let [scored-bt (get-scored state :contestant 0)]
        (is (= 2 (get-counters (refresh scored-bt) :agenda))
            "Scored w/ 4 over-advancements; 2 agenda counters")
        (play-from-hand state :contestant "Ichi 1.0" "HQ")
        (core/reveal state :contestant (get-character state :hq 0))
        (is (= 2 (:credit (get-contestant))) "2c discount to reveal Ichi")))))

(deftest breaking-news
  ;; Breaking News
  (do-game
    (new-game (default-contestant [(qty "Breaking News" 3)])
              (default-challenger))
    (play-and-score state "Breaking News")
    (is (= 2 (get-in @state [:challenger :tag])) "Challenger receives 2 tags from Breaking News")
    (take-credits state :contestant)
    (is (zero? (get-in @state [:challenger :tag]))) "Two tags removed at the end of the turn"))

(deftest broad-daylight
  ;; Broad Daylight
  (testing "take bad pub"
    (do-game
      (new-game (default-contestant [(qty "Broad Daylight" 3)])
                (default-challenger))
      (is (zero? (:bad-publicity (get-contestant))) "Contestant start with no bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :contestant "Yes")
      (is (= 1 (:bad-publicity (get-contestant))) "Contestant gains 1 bad pub")
      (is (= 1 (get-counters (get-scored state :contestant 0) :agenda)) "Should gain 1 agenda counter")
      (play-and-score state "Broad Daylight")
      (click-prompt state :contestant "No")
      (is (= 1 (:bad-publicity (get-contestant))) "Contestant doesn't gain bad pub")
      (is (= 1 (get-counters (get-scored state :contestant 1) :agenda)) "Should gain 1 agenda counter")
      (play-and-score state "Broad Daylight")
      (click-prompt state :contestant "Yes")
      (is (= 2 (:bad-publicity (get-contestant))) "Contestant gains 1 bad pub")
      (is (= 2 (get-counters (get-scored state :contestant 2) :agenda)) "Should gain 2 agenda counters")))
  (testing "deal damage"
    (do-game
      (new-game (default-contestant ["Broad Daylight"])
                (default-challenger))
      (core/gain state :contestant :bad-publicity 3)
      (play-and-score state "Broad Daylight")
      (click-prompt state :contestant "Yes")
      (is (= 4 (:bad-publicity (get-contestant))) "Contestant gains 1 bad pub")
      (is (= 4 (get-counters (get-scored state :contestant 0) :agenda)) "Should gain 1 agenda counter")
      (is (empty? (:discard (get-challenger))) "Challenger has no discarded cards")
      (card-ability state :contestant (get-scored state :contestant 0) 0)
      (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 damage")
      (card-ability state :contestant (get-scored state :contestant 0) 0)
      (is (= 2 (count (:discard (get-challenger)))) "Challenger didn't take additional damage")))
  (testing "bad pub triggers"
    (do-game
      (new-game (default-contestant ["Broad Daylight" "Broadcast Square"])
                (default-challenger))
      (core/gain state :contestant :bad-publicity 1)
      (play-from-hand state :contestant "Broadcast Square" "New party")
      (core/reveal state :contestant (get-content state :party1 0))
      (is (= 1 (:bad-publicity (get-contestant))) "Contestant start with one bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :contestant "Yes")
      (is (= 1 (:bad-publicity (get-contestant))) "Doesn't gain additional bad pub yet")
      (click-prompt state :contestant "0")  ;; Contestant doesn't pump trace, base 3
      (click-prompt state :challenger "0")  ;; Challenger doesn't pump trace; loses trace
      (is (= 1 (:bad-publicity (get-contestant))) "Blocks gaining additional bad pub")
      (is (= 1 (get-counters (get-scored state :contestant 0) :agenda)) "Should gain 1 agenda counter")))
  (testing "bad pub triggers - more cases"
    (do-game
      (new-game (default-contestant ["Broad Daylight" "Broadcast Square"])
                (default-challenger))
      (core/gain state :contestant :bad-publicity 1)
      (play-from-hand state :contestant "Broadcast Square" "New party")
      (core/reveal state :contestant (get-content state :party1 0))
      (is (= 1 (:bad-publicity (get-contestant))) "Contestant start with one bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :contestant "Yes")
      (is (= 1 (:bad-publicity (get-contestant))) "Doesn't gain additional bad pub yet")
      (click-prompt state :contestant "0")  ;; Contestant doesn't pump trace, base 3
      (click-prompt state :challenger "5")  ;; Challenger pumps trace; wins trace
      (is (= 2 (:bad-publicity (get-contestant))) "Gains additional bad pub")
      (is (= 2 (get-counters (get-scored state :contestant 0) :agenda)) "Should gain 2 agenda counter"))))

(deftest cfc-excavation-contract
  ;; CFC Excavation Contract
  (dotimes [n 5]
    (do-game
      (new-game (default-contestant ["CFC Excavation Contract" (qty "Eli 1.0" n)])
                (default-challenger))
      (core/gain state :contestant :click 10 :credit 10)
      (is (= 15 (:credit (get-contestant))) "Should start with 5 credits")
      (dotimes [_ n]
        (play-from-hand state :contestant "Eli 1.0" "New party")
        (core/reveal state :contestant (get-character state (keyword (str "party" (:rid @state))) 0)))
      (let [credit (:credit (get-contestant))]
        (play-and-score state "CFC Excavation Contract")
        (is (= (+ credit (* 2 n)) (:credit (get-contestant)))
            (str "Should now have with " (+ credit (* 2 n)) " credits"))))))

(deftest character-assassination
  ;; Character Assassination
  (do-game
    (new-game (default-contestant ["Character Assassination"])
              (default-challenger ["Fall Guy" "Kati Jones"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Kati Jones")
    (play-from-hand state :challenger "Fall Guy")
    (take-credits state :challenger)
    (play-and-score state "Character Assassination")
    (let [kati (get-radicle state 0)]
      (click-card state :contestant kati)
      (is (empty? (:prompt (get-challenger))) "Fall Guy prevention didn't occur")
      (is (= 1 (count (:discard (get-challenger)))) "Kati Jones discarded"))))

(deftest chronos-project
  ;; Chronos Project
  (do-game
    (new-game (default-contestant ["Chronos Project"])
              (default-challenger))
    (dotimes [_ 3]
      (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :discard))
    (is (= 3 (count (:discard (get-challenger)))) "Challenger should have 3 cards in heap")
    (play-and-score state "Chronos Project")
    (is (zero? (count (:discard (get-challenger)))) "Challenger should have 0 cards in heap")))

(deftest city-works-project
  ;; City Works Project
  (do-game
    (new-game (default-contestant ["City Works Project"])
              (default-challenger [(qty "Sure Gamble" 4)]))
    (play-from-hand state :contestant "City Works Project" "New party")
    (let [cwp (get-content state :party1 0)]
      (core/advance state :contestant {:card (refresh cwp)})
      (core/advance state :contestant {:card (refresh cwp)}))
    (take-credits state :contestant)
    (run-empty-locale state "Locale 1")
    (click-prompt state :challenger "Steal")
    (is (= 4 (count (:discard (get-challenger)))) "Challenger paid 4 meat damage")))

(deftest clone-retirement
  ;; Clone Retirement
  (do-game
    (new-game (default-contestant [(qty "Clone Retirement" 2) "Hostile Takeover"])
              (default-challenger))
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-contestant))))
    (is (= 1 (:bad-publicity (get-contestant))))
    (play-and-score state "Clone Retirement")
    (is (zero? (:bad-publicity (get-contestant))))
    (play-from-hand state :contestant "Clone Retirement" "New party")
    (take-credits state :contestant)
    (run-on state "Locale 3")
    (run-successful state)
    (click-prompt state :challenger "Steal")
    (is (= 1 (:bad-publicity (get-contestant))))))

(deftest contestantorate-sales-team
  ;; Contestantorate Sales Team
  (do-game
    (new-game (default-contestant [(qty "Contestantorate Sales Team" 2)])
              (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (play-and-score state "Contestantorate Sales Team")
    (let [scored-cst (get-scored state :contestant 0)]
      (core/end-turn state :contestant nil)
      (core/start-turn state :challenger nil)
      (is (= 6 (:credit (get-contestant))) "Increments at challenger's start of turn")
      (is (= 9 (get-counters (refresh scored-cst) :credit)))
      (core/end-turn state :challenger nil)
      (core/start-turn state :contestant nil)
      (is (= 7 (:credit (get-contestant))) "Increments at contestant's start of turn")
      (is (= 8 (get-counters (refresh scored-cst) :credit))))))

(deftest contestantorate-war
  ;; Contestantorate War
  (do-game
    (new-game (default-contestant [(qty "Contestantorate War" 2)])
              (default-challenger))
    (is (= 5 (:credit (get-contestant))))
    (play-and-score state "Contestantorate War")
    (is (zero? (:credit (get-contestant))) "Lost all credits")
    (core/gain state :contestant :credit 7)
    (play-and-score state "Contestantorate War")
    (is (= 14 (:credit (get-contestant))) "Had 7 credits when scoring, gained another 7")))

(deftest crisis-management
  ;; Crisis Management
  (do-game
    (new-game (default-contestant ["Crisis Management"])
              (default-challenger))
    (play-and-score state "Crisis Management")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 3 (count (:hand (get-challenger)))) "No damage done, Challenger not tagged")
    (take-credits state :contestant)
    (core/gain state :challenger :tag 1)
    (take-credits state :challenger)
    (is (= 2 (count (:hand (get-challenger)))) "Crisis Management dealt 1 meat damage")))

(deftest dedicated-neural-net
  ;; Dedicated Neural Net
  (do-game
    (new-game (default-contestant ["Dedicated Neural Net" (qty "Scorched Earth" 2)
                             "Hedge Fund" "Caprcharacter Nisei"])
              (default-challenger ["HQ Interface"]))
    (play-from-hand state :contestant "Caprcharacter Nisei" "HQ")
    (play-and-score state "Dedicated Neural Net")
    (take-credits state :contestant)
    (run-empty-locale state :hq)
    (click-prompt state :challenger "0 [Credits]")
    (click-prompt state :contestant "1 [Credits]")
    (is (-> @state :run :run-effect :replace-access) "Replace-access tiggered")
    (click-card state :contestant (find-card "Hedge Fund" (:hand (get-contestant))))
    (click-prompt state :challenger "Card from hand")
    (is (accessing state "Hedge Fund") "Challenger accessing Hedge Fund")
    (click-prompt state :challenger "No action")
    ;; test for #2376
    (click-prompt state :challenger "Unrevealed region in HQ")
    (is (accessing state "Caprcharacter Nisei") "Challenger accessing Caprcharacter")
    (click-prompt state :challenger "No action")
    (is (not (:run @state)) "Run completed")
    (run-empty-locale state :hq)
    (click-prompt state :challenger "Card from hand")
    (click-prompt state :challenger "No action")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (play-from-hand state :challenger "HQ Interface")
    (run-empty-locale state :hq)
    (click-prompt state :challenger "0 [Credits]")
    (click-prompt state :contestant "1 [Credits]")
    (is (= 2 (-> (get-contestant) :selected first :max)) "Contestant chooses 2 cards for Challenger to access")))

(deftest degree-mill
  ;; Degree Mill
  (testing "Basic behavior"
    (do-game
      (new-game (default-contestant [(qty "Degree Mill" 2)])
                (default-challenger ["Ice Analyzer" "All-nighter" "Hunting Grounds"]))
      (play-from-hand state :contestant "Degree Mill" "New party")
      (take-credits state :contestant)
      (is (= 0 (count (:deck (get-challenger)))) "Challenger starts with empty deck")
      (run-on state "Locale 1")
      (run-successful state)
      (click-prompt state :challenger "No action")
      (is (= 0 (:agenda-point (get-challenger))) "Challenger stole Degree Mill with no placed cards")
      (play-from-hand state :challenger "Ice Analyzer")
      (play-from-hand state :challenger "All-nighter")
      (let [ia (get-radicle state 0)
            an (get-radicle state 1)]
        (run-on state "Locale 1")
        (run-successful state)
        (click-prompt state :challenger "Pay shuffling 2 placed cards into the stack to steal")
        (click-card state :challenger ia)
        (click-card state :challenger an)
        (is (= 3 (:agenda-point (get-challenger))) "Challenger failed to steal Degree Mill")
        (is (empty? (get-in @state [:challenger :rig :radicle])) "Degree Mill didn't remove placed cards")
        (is (= 2 (count (:deck (get-challenger)))) "Degree Mill didn't put cards back in deck"))
      (take-credits state :challenger)
      ;; Checking if facedowns work as well
      (play-from-hand state :contestant "Degree Mill" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Hunting Grounds")
      (let [hg (get-radicle state 0)]
        (run-on state "Locale 2")
        (run-successful state)
        (click-prompt state :challenger "No action")
        (is (= 3 (:agenda-point (get-challenger))) "Challenger stole Degree Mill with single card")
        (card-ability state :challenger hg 1)
        (is (= 2 (count (get-in (get-challenger) [:rig :facedown]))) "Hunting Ground did not place cards facedown")
        (is (empty? (:deck (get-challenger))) "Hunting Grounds did not remove cards from deck")
        (let [fd1 (get-challenger-facedown state 0)
              fd2 (get-challenger-facedown state 1)]
          (run-on state "Locale 2")
          (run-successful state)
          (click-prompt state :challenger "Pay shuffling 2 placed cards into the stack to steal")
          (click-card state :challenger fd1)
          (click-card state :challenger fd2)
          (is (= 6 (:agenda-point (get-challenger))) "Challenger failed to steal Degree Mill with facedown cards")
          (is (empty? (get-in (get-challenger)  [:rig :facedown])) "Degree Mill didn't remove facedown cards")
          (is (= 2 (count (:deck (get-challenger)))) "Degree Mill didn't put cards back in deck")))))
  (testing "Multiple steal costs"
    (do-game
      (new-game (default-contestant [(qty "Degree Mill" 1) (qty "Strongbox" 1)])
                (default-challenger [(qty "Ice Analyzer" 3) (qty "All-nighter" 3)]))
      (play-from-hand state :contestant "Degree Mill" "New party")
      (play-from-hand state :contestant "Strongbox" "Locale 1")
      (let [dm (get-content state :party1 0)
            sb (get-content state :party1 1)]
        (core/reveal state :contestant sb)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Ice Analyzer")
        (play-from-hand state :challenger "All-nighter")
        (run-empty-locale state :party1)
        (click-card state :challenger (refresh dm))
        (click-prompt state :challenger "Pay to steal")
        (is (= 1 (:click (get-challenger))) "Challenger should start with 1 remaining click")
        (click-prompt state :challenger "[Click]")
        (is (zero? (:click (get-challenger))) "Challenger should have spent a click")
        (is (= 2 (count (get-in @state [:challenger :rig :radicle]))) "Challenger starts with 2 radicles")
        (click-prompt state :challenger "shuffling 2 placed cards into the stack")
        (click-card state :challenger (get-radicle state 1))
        (click-card state :challenger (get-radicle state 0))
        (is (empty? (get-radicle state)) "Degree Mill removed placed cards")
        (is (not-empty (get-scored state :challenger)) "Challenger stole an agenda")))))

(deftest director-haas'-pet-project
  ;; Director Haas' Pet Project
  (do-game
    (new-game (default-contestant ["Director Haas' Pet Project"
                             "Adonis Campaign"
                             "Strongbox"
                             "Eli 1.0"
                             (qty "Hedge Fund" 5)])
              (default-challenger))
    (starting-hand state :contestant ["Director Haas' Pet Project" "Adonis Campaign" "Strongbox"])
    (core/move state :contestant (find-card "Eli 1.0" (:deck (get-contestant))) :discard)
    (play-and-score state "Director Haas' Pet Project")
    (click-prompt state :contestant "Yes")
    (click-card state :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
    (click-card state :contestant (find-card "Strongbox" (:hand (get-contestant))))
    (click-card state :contestant (find-card "Eli 1.0" (:discard (get-contestant))))))

(deftest domestic-sleepers
  ;; Domestic Sleepers
  (do-game
    (new-game (default-contestant ["Domestic Sleepers"])
              (default-challenger))
    (play-and-score state "Domestic Sleepers")
    (core/gain state :contestant :click 3)
    (let [ds_scored (get-scored state :contestant 0)]
      (is (zero? (get-counters (refresh ds_scored) :agenda)) "Should start with 0 agenda counters")
      (is (zero? (:agenda-point (get-contestant))) "Should provide 0 agenda points initially")
      (card-ability state :contestant ds_scored 0)
      (is (= 1 (get-counters (refresh ds_scored) :agenda)) "Should gain 1 agenda counter")
      (is (= 1 (:agenda-point (get-contestant))) "Should provide 1 agenda point after ability use"))))

(deftest eden-fragment
  ;; Test that Eden Fragment ignores the place cost of the first character
  (do-game
    (new-game (default-contestant [(qty "Eden Fragment" 3) (qty "Ice Wall" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-and-score state "Eden Fragment")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (is (some? (get-character state :hq 1)) "Contestant has two character placed on HQ")
    (is (= 6 (:credit (get-contestant))) "Contestant does not pay for placing the first Character of the turn")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (is (some? (get-character state :hq 2)) "Contestant has three character placed on HQ")
    (is (= 4 (:credit (get-contestant))) "Contestant pays for placing the second Character of the turn")))

(deftest efficiency-committee
  ;; Efficiency Committee
  (do-game
    (new-game (default-contestant [(qty "Efficiency Committee" 3) (qty "Shipment from SanSan" 2)
                             "Ice Wall"])
              (default-challenger))
    (core/gain state :contestant :click 4)
    (play-from-hand state :contestant "Efficiency Committee" "New party")
    (play-from-hand state :contestant "Efficiency Committee" "New party")
    (play-from-hand state :contestant "Efficiency Committee" "New party")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [ec1 (get-content state :party1 0)
          ec2 (get-content state :party2 0)
          ec3 (get-content state :party3 0)
          iw (get-character state :hq 0)]
      (score-agenda state :contestant ec1)
      (let [ec1_scored (get-scored state :contestant 0)]
        (is (= 3 (get-counters (refresh ec1_scored) :agenda)))
        (is (= 2 (:agenda-point (get-contestant))))
        ;; use token
        (is (= 3 (:click (get-contestant))))
        (card-ability state :contestant ec1_scored 0)
        (is (= 4 (:click (get-contestant))))
        ;; try to advance Ice Wall
        (advance state iw)
        (is (= 4 (:click (get-contestant))))
        (is (zero? (get-counters (refresh iw) :advancement)))
        ;; try to advance Efficiency Committee
        (advance state ec2)
        (is (= 4 (:click (get-contestant))))
        (is (zero? (get-counters (refresh ec2) :advancement)))
        ;; advance with Shipment from SanSan
        (play-from-hand state :contestant "Shipment from SanSan")
        (click-prompt state :contestant "2")
        (click-card state :contestant ec2)
        (is (= 2 (get-counters (refresh ec2) :advancement)))
        (play-from-hand state :contestant "Shipment from SanSan")
        (click-prompt state :contestant "2")
        (click-card state :contestant ec2)
        (is (= 4 (get-counters (refresh ec2) :advancement)))
        (core/score state :contestant {:card (refresh ec2)})
        (is (= 4 (:agenda-point (get-contestant))))
        (take-credits state :contestant)
        (take-credits state :challenger)
        ;; can advance again
        (advance state iw)
        (is (= 1 (get-counters (refresh iw) :advancement)))
        (advance state ec3)
        (is (= 1 (get-counters (refresh ec3) :advancement)))))))

(deftest elective-region
  ;; Elective Region
  (do-game
    (new-game (default-contestant ["Elective Region"])
              (default-challenger))
    (play-and-score state "Elective Region")
    (let [eu-scored (get-scored state :contestant 0)]
      (is (= 2 (get-counters (refresh eu-scored) :agenda)) "Should start with 2 agenda counters")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 3 (:click (get-contestant))) "Should start with 4 clicks")
      (card-ability state :contestant eu-scored 0)
      (card-ability state :contestant eu-scored 0)
      (is (= 4 (:click (get-contestant))) "Should gain 2 clicks, not 3")
      (is (= 1 (get-counters (refresh eu-scored) :agenda)) "Should still have 1 agenda counter"))))

(deftest encrypted-portals
  ;; Encrypted Portals
  (do-game
    (new-game (default-contestant ["Encrypted Portals" "Lotus Field"])
              (default-challenger))
    (play-from-hand state :contestant "Lotus Field" "HQ")
    (let [lf (get-character state :hq 0)]
      (core/reveal state :contestant lf)
      (is (= 4 (:current-strength (refresh lf))) "Should start with base strength of 4")
      (is (zero? (:credit (get-contestant))) "Should have 0 credits after reveal")
      (play-and-score state "Encrypted Portals")
      (is (= 5 (:current-strength (refresh lf))) "Should gain 1 strength from 4 to 5")
      (is (= 1 (:credit (get-contestant))) "Should gain 1 credit for revealed code gate"))))

(deftest escalate-vitriol
  ;; Escalate Vitriol
  (do-game
    (new-game (default-contestant ["Escalate Vitriol"])
              (default-challenger))
    (core/lose state :contestant :credit 5)
    (play-and-score state "Escalate Vitriol")
    (let [ev-scored (get-scored state :contestant 0)]
      (dotimes [tag 10]
        (is (zero? (:tag (get-challenger))) "Should start with 0 tags")
        (is (zero? (:credit (get-contestant))) "Should start with 0 credits")
        (core/gain state :challenger :tag tag)
        (card-ability state :contestant ev-scored 0)
        (is (= tag (:credit (get-contestant))) (str "Should gain " tag " credits"))
        (take-credits state :contestant)
        (take-credits state :challenger)
        (core/lose state :contestant :credit (:credit (get-contestant)))
        (core/lose state :challenger :tag tag)))))

(deftest executive-retreat
  ;; Executive Retreat
  (do-game
    (new-game (default-contestant ["Executive Retreat" (qty "Hedge Fund" 5)])
              (default-challenger))
    (starting-hand state :contestant ["Executive Retreat" "Hedge Fund"])
    (is (= 2 (count (:hand (get-contestant)))) "Contestant should start with 1 card in HQ")
    (play-and-score state "Executive Retreat")
    (is (zero? (count (:hand (get-contestant)))) "Contestant should have 0 cards in HQ after shuffling HQ back into R&D")
    (let [er-scored (get-scored state :contestant 0)]
      (card-ability state :contestant er-scored 0)
      (is (= 5 (count (:hand (get-contestant)))) "Contestant should have 5 cards in hand")
      (is (zero? (get-counters (refresh er-scored) :agenda)) "Executive Retreat should have 0 agenda counters")))
  (testing "Overdraw"
    (do-game
      (new-game (default-contestant ["Executive Retreat" (qty "Hedge Fund" 4)])
                (default-challenger))
      (starting-hand state :contestant ["Executive Retreat" "Hedge Fund"])
      (is (= 2 (count (:hand (get-contestant)))) "Contestant should start with 1 card in HQ")
      (play-and-score state "Executive Retreat")
      (is (zero? (count (:hand (get-contestant)))) "Contestant should have 0 cards in HQ after shuffling HQ back into R&D")
      (let [er-scored (get-scored state :contestant 0)]
        (card-ability state :contestant er-scored 0)
        (is (= 4 (count (:hand (get-contestant)))) "Contestant should have 5 cards in hand")
        (is (zero? (get-counters (refresh er-scored) :agenda)) "Executive Retreat should have 0 agenda counters")
        (is (= :challenger (:winner @state)) "Challenger wins")
        (is (= "Decked" (:reason @state)) "Win condition reports decked")))))

(deftest explode-a-palooza
  ;; Explode-a-palooza
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Explode-a-palooza"])
                (default-challenger))
      (play-from-hand state :contestant "Explode-a-palooza" "New party")
      (take-credits state :contestant)
      (run-empty-locale state :party1)
      (click-prompt state :contestant "Yes")
      (click-prompt state :challenger "Steal")
      (is (= 12 (:credit (get-contestant))) "Gained 5 credits")))
  (testing "Interaction with The Turning Wheel. Issue #1717."
    (do-game
      (new-game (default-contestant [(qty "Explode-a-palooza" 3)])
                (default-challenger ["The Turning Wheel"]))
      (starting-hand state :contestant ["Explode-a-palooza" "Explode-a-palooza"])
      (play-from-hand state :contestant "Explode-a-palooza" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "The Turning Wheel")
      (run-empty-locale state :party1)
      (click-prompt state :contestant "Yes")
      (click-prompt state :challenger "Steal")
      (let [ttw (get-radicle state 0)]
        (is (zero? (get-counters (refresh ttw) :power)) "TTW did not gain counters")
        (is (= 1 (count (:scored (get-challenger)))) "Challenger stole Explodapalooza")
        (is (= 12 (:credit (get-contestant))) "Gained 5 credits")
        (run-empty-locale state :rd)
        (click-prompt state :contestant "Yes")
        (click-prompt state :challenger "Steal")
        (is (zero? (get-counters (refresh ttw) :power)) "TTW did not gain counters")
        (is (= 2 (count (:scored (get-challenger)))) "Challenger stole Explodapalooza")
        (is (= 17 (:credit (get-contestant))) "Gained 5 credits")))))

(deftest false-lead
  ;; False Lead
  (do-game
    (new-game (default-contestant ["False Lead"])
              (default-challenger))
    (play-and-score state "False Lead")
    (is (= 1 (count (:scored (get-contestant)))) "Contestant should have 1 agenda point")
    (take-credits state :contestant)
    (is (= 4 (:click (get-challenger))) "Challenger should start turn with 4 clicks")
    (card-ability state :contestant (get-scored state :contestant 0) 0)
    (is (= 2 (:click (get-challenger))) "Challenger should lose 2 clicks from False Lead")))

(deftest fetal-ai
  ;; Fetal AI
  (testing "basic test"
    (do-game
      (new-game (default-contestant [(qty "Fetal AI" 3)])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
      (play-from-hand state :contestant "Fetal AI" "New party")
      (take-credits state :contestant 2)
      (run-empty-locale state "Locale 1")
      (click-prompt state :challenger "Pay 2 [Credits] to steal")
      (is (= 3 (count (:hand (get-challenger)))) "Challenger took 2 net damage from Fetal AI")
      (is (= 3 (:credit (get-challenger))) "Challenger paid 2cr to steal Fetal AI")
      (is (= 1 (count (:scored (get-challenger)))) "Challenger stole Fetal AI"))
    (testing "can't afford to steal"
      (do-game
        (new-game (default-contestant [(qty "Fetal AI" 3)])
                  (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
        (play-from-hand state :contestant "Fetal AI" "New party")
        (take-credits state :contestant 2)
        (core/lose state :challenger :credit 5)
        (run-empty-locale state "Locale 1")
        (click-prompt state :challenger "No action")
        (is (= 3 (count (:hand (get-challenger)))) "Challenger took 2 net damage from Fetal AI")
        (is (zero? (count (:scored (get-challenger)))) "Challenger could not steal Fetal AI")))))

(deftest fly-on-the-wall
  ;; Fly on the Wall - give the challenger 1 tag
  (do-game
    (new-game (default-contestant ["Fly on the Wall"])
              (default-challenger))
    (is (zero? (:tag (get-challenger))) "Challenger starts with no tags")
    (play-and-score state "Fly on the Wall")
    (is (= 1 (:tag (get-challenger))) "Challenger is tagged")))

(deftest firmware-updates
  ;; Firmware Updates
  (do-game
    (new-game (default-contestant ["Firmware Updates"
                             "Ice Wall"])
              (default-challenger))
    (play-and-score state "Firmware Updates")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [fu (get-scored state :contestant 0)
          iw (get-character state :hq 0)]
      (is (= 3 (get-counters (refresh fu) :agenda)) "Firmware Updates should start with 3 agenda counters")
      (core/reveal state :contestant iw)
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (card-ability state :contestant fu 0)
      (click-card state :contestant (refresh iw))
      (is (= 2 (get-counters (refresh fu) :agenda)) "Firmware Updates should now have 2 agenda counters")
      (is (= 1 (get-counters (refresh iw) :advancement)) "Ice Wall should have 1 advancement token"))))

(deftest geothermal-fracking
  ;; Geothermal Fracking
  (testing "basic test"
    (do-game
      (new-game (default-contestant ["Geothermal Fracking"])
                (default-challenger))
      (play-and-score state "Geothermal Fracking")
      (is (= 2 (:click (get-contestant))) "Should have 2 clicks left")
      (is (= 5 (:credit (get-contestant))) "Should start with 5 credits")
      (is (zero? (:bad-publicity (get-contestant))) "Should start with 0 bad publicity")
      (let [gf-scored (get-scored state :contestant 0)]
        (is (= 2 (get-counters (refresh gf-scored) :agenda)) "Should start with 2 agenda counters")
        (card-ability state :contestant gf-scored 0)
        (is (= 1 (:click (get-contestant))) "Should have 1 click left")
        (is (= 12 (:credit (get-contestant))) "Should gain 7 credits from 5 to 12")
        (is (= 1 (:bad-publicity (get-contestant))) "Should gain 1 bad publicity"))))
  (testing "prevented bad publicity shouldn't block credit gain"
    (do-game
      (new-game (default-contestant ["Geothermal Fracking" "Broadcast Square"])
                (default-challenger))
      (play-and-score state "Geothermal Fracking")
      (is (= 2 (:click (get-contestant))) "Should have 2 clicks left")
      (is (= 5 (:credit (get-contestant))) "Should start with 5 credits")
      (is (zero? (:bad-publicity (get-contestant))) "Should start with 0 bad publicity")
      (play-from-hand state :contestant "Broadcast Square" "New party")
      (let [gf-scored (get-scored state :contestant 0)
            bs (get-content state :party2 0)]
        (core/reveal state :contestant bs)
        (is (= 2 (get-counters (refresh gf-scored) :agenda)) "Should start with 2 agenda counters")
        (card-ability state :contestant gf-scored 0)
        (click-prompt state :contestant "0")
        (click-prompt state :challenger "0")
        (is (zero? (:click (get-contestant))) "Should have 0 click left")
        (is (= 10 (:credit (get-contestant))) "Should gain 7 credits from 3 to 10")
        (is (zero? (:bad-publicity (get-contestant))) "Should gain 0 bad publicity from prevention")))))

(deftest genetic-resequencing
  ;; Genetic Resequencing
  (do-game
    (new-game (default-contestant ["Genetic Resequencing" (qty "Braintrust" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Braintrust" "New party")
    (play-from-hand state :contestant "Braintrust" "New party")
    (play-from-hand state :contestant "Genetic Resequencing" "New party")
    (let [bt1 (get-content state :party1 0)
          bt2 (get-content state :party2 0)
          gr (get-content state :party3 0)]
      (score-agenda state :contestant bt1)
      (let [btscored (get-scored state :contestant 0)]
        (is (zero? (get-counters (refresh btscored) :agenda)) "No agenda counters on scored Braintrust")
        (score-agenda state :contestant gr)
        (click-card state :contestant bt2)
        (is (zero? (get-counters (refresh bt2) :agenda))
            "No agenda counters on placed Braintrust; not a valid target")
        (click-card state :contestant btscored)
        (is (= 1 (get-counters (refresh btscored) :agenda))
            "1 agenda counter placed on scored Braintrust")))))

(deftest gila-hands-arcology
  ;; Gila Hands Arcology
  (do-game
    (new-game (default-contestant ["Gila Hands Arcology"])
              (default-challenger))
    (play-and-score state "Gila Hands Arcology")
    (is (= 2 (:click (get-contestant))) "Should have 2 clicks left")
    (is (= 5 (:credit (get-contestant))) "Should start with 5 credits")
    (core/gain state :contestant :click 2)
    (let [gha-scored (get-scored state :contestant 0)]
      (card-ability state :contestant gha-scored 0)
      (is (= 2 (:click (get-contestant))) "Should spend 2 clicks on Gila Hands")
      (is (= 8 (:credit (get-contestant))) "Should gain 3 credits from 5 to 8")
      (card-ability state :contestant gha-scored 0)
      (is (zero? (:click (get-contestant))) "Should spend 2 clicks on Gila Hands")
      (is (= 11 (:credit (get-contestant))) "Should gain 3 credits from 8 to 11"))))

(deftest glenn-station
  ;; Glenn Station
  (do-game
    (new-game (default-contestant ["Glenn Station" "Ice Wall"])
              (default-challenger))
    (play-and-score state "Glenn Station")
    (let [gs-scored (get-scored state :contestant 0)]
      (card-ability state :contestant gs-scored 0)
      (click-prompt state :contestant (find-card "Ice Wall" (:hand (get-contestant))))
      (is (= 1 (count (:hosted (refresh gs-scored)))))
      (card-ability state :contestant gs-scored 1)
      (click-prompt state :contestant (find-card "Ice Wall" (:hosted (refresh gs-scored))))
      (is (zero? (count (:hosted (refresh gs-scored))))))))

(deftest global-food-initiative
  ;; Global Food Initiative
  (do-game
    (new-game (default-contestant [(qty "Global Food Initiative" 2)])
              (default-challenger))
    (testing "Contestant scores"
      (is (zero? (:agenda-point (get-challenger))) "Challenger should start with 0 agenda points")
      (is (zero? (:agenda-point (get-contestant))) "Contestant should start with 0 agenda points")
      (play-and-score state "Global Food Initiative")
      (is (= 3 (:agenda-point (get-contestant))) "Contestant should gain 3 agenda points"))
    (testing "Challenger steals"
      (play-from-hand state :contestant "Global Food Initiative" "New party")
      (take-credits state :contestant)
      (run-on state :party2)
      (run-successful state)
      (click-prompt state :challenger "Steal")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger should gain 2 agenda points, not 3"))))

(deftest government-contracts
  ;; Government Contracts
  (do-game
    (new-game (default-contestant ["Government Contracts"])
              (default-challenger))
    (play-and-score state "Government Contracts")
    (is (= 2 (:click (get-contestant))))
    (card-ability state :contestant (get-scored state :contestant 0) 0)
    (is (zero? (:click (get-contestant))) "Spent 2 clicks")
    (is (= 9 (:credit (get-contestant))) "Gained 4 credits")))

(deftest government-takeover
  ;; Government Takeover
  (do-game
    (new-game (default-contestant ["Government Takeover"])
              (default-challenger))
    (play-and-score state "Government Takeover")
    (is (= 5 (:credit (get-contestant))) "Should start with 5 credits")
    (let [gt-scored (get-scored state :contestant 0)]
      (card-ability state :contestant gt-scored 0)
      (is (= 8 (:credit (get-contestant))) "Should gain 3 credits from 5 to 8"))))

(deftest graft
  ;; Graft
  (letfn [(graft-test [[number-of-picks deck-size]]
            (let [cards ["Ice Wall" "Fire Wall" "Orion"]]
              (do-game
                (new-game (default-contestant ["Graft" "Ice Wall"
                                         "Fire Wall" "Orion"])
                          (default-challenger))
                (starting-hand state :contestant ["Graft"])
                (play-and-score state "Graft")
                (dotimes [current-pick number-of-picks]
                  (click-prompt state :contestant (find-card (nth cards current-pick) (:deck (get-contestant)))))
                (is (= number-of-picks (count (:hand (get-contestant)))))
                (is (= deck-size (count (:deck (get-contestant))))))))]
    (doall (map graft-test
                [[0 3]
                 [1 2]
                 [2 1]
                 [3 0]]))))

(deftest hades-fragment
  ;; Hades Fragment
  (do-game
    (new-game (default-contestant ["Hades Fragment" (qty "Hedge Fund" 2)])
              (default-challenger))
    (starting-hand state :contestant ["Hades Fragment"])
    (play-and-score state "Hades Fragment")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 1 (count (:hand (get-contestant)))) "Contestant should have no opportunity to use Hades Shard")
    (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :discard)
    (take-credits state :contestant)
    (take-credits state :challenger)
    (let [hf-scored (get-scored state :contestant 0)]
      (card-ability state :contestant hf-scored 0)
      (click-card state :contestant (find-card "Hedge Fund" (:discard (get-contestant))))
      (is (= 2 (count (:deck (get-contestant)))) "R&D should have 2 cards in it after Hades Fragment use"))))

(deftest helium-3-deposit
  ;; Helium-3 Deposit
  (do-game
    (new-game (default-contestant ["Helium-3 Deposit"
                             "Chief Slee"
                             "Ice Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Chief Slee" "New party")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (let [cs (get-content state :party1 0)
          iw (get-character state :hq 0)]
      (is (zero? (get-counters (refresh cs) :power)) "Chief Slee should start with 0 power counters")
      (core/reveal state :contestant iw)
      (run-on state "HQ")
      (card-ability state :contestant cs 0)
      (is (= 1 (get-counters (refresh cs) :power)) "Chief Slee should gain 1 power counter")
      (take-credits state :challenger)
      (play-and-score state "Helium-3 Deposit")
      (click-prompt state :contestant "2")
      (click-card state :contestant cs)
      (is (= 3 (get-counters (refresh cs) :power)) "Chief Slee should gain 2 power counters from 1 to 3"))))

(deftest high-risk-investment
  ;; High-Risk Investment
  (do-game
    (new-game (default-contestant ["High-Risk Investment"])
              (default-challenger))
    (play-and-score state "High-Risk Investment")
    (let [hri-scored (get-scored state :contestant 0)]
      (is (= 1 (get-counters (refresh hri-scored) :agenda)) "Has 1 agenda counter")
      (take-credits state :contestant)
      (is (= 7 (:credit (get-contestant))))
      (take-credits state :challenger)
      (is (= 9 (:credit (get-challenger))))
      (card-ability state :contestant hri-scored 0)
      (is (= 16 (:credit (get-contestant))) "Gained 9 credits")
      (is (= 2 (:click (get-contestant))) "Spent 1 click")
      (is (zero? (get-counters (refresh hri-scored) :agenda)) "Spent agenda counter"))))

(deftest hollywood-renovation
  ;; Hollywood Renovation
  (do-game
    (new-game (default-contestant ["Hollywood Renovation" "Ice Wall"])
              (default-challenger))
    (core/gain state :contestant :click 10 :credit 10)
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Hollywood Renovation" "New party")
    (let [hr (get-content state :party1 0)
          iw (get-character state :hq 0)]
      (is (zero? (get-counters (refresh hr) :advancement)) "Hollywood Renovation should start with 0 advancement tokens")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement tokens")
      (dotimes [n 5]
        (advance state (refresh hr))
        (click-card state :contestant (refresh iw)))
      (is (= 5 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 5 advancement tokens")
      (is (= 5 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 5 advancement tokens")
      (advance state (refresh hr))
      (click-card state :contestant (refresh iw))
      (is (= 6 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 1 from 5 to 6 advancement tokens")
      (is (= 7 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 from 5 to 7 advancement tokens"))))

(deftest hostile-takeover
  ;; Hostile Takeover
  (do-game
    (new-game (default-contestant ["Hostile Takeover"])
              (default-challenger))
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-contestant))) "Gain 7 credits")
    (is (= 1 (:bad-publicity (get-contestant))) "Take 1 bad publicity")))

(deftest house-of-knives
  ;; House of Knives
  (do-game
    (new-game (default-contestant ["House of Knives"])
              (default-challenger))
    (play-and-score state "House of Knives")
    (let [hok-scored (get-scored state :contestant 0)]
      (is (= 3 (get-counters (refresh hok-scored) :agenda)) "House of Knives should start with 3 counters")
      (take-credits state :contestant)
      (run-empty-locale state "R&D")
      (run-phase-43 state)
      (card-ability state :contestant hok-scored 0)
      (is (= 1 (count (:discard (get-challenger)))) "Challenger should pay 1 net damage")
      (run-empty-locale state "R&D")
      (run-phase-43 state)
      (card-ability state :contestant hok-scored 0)
      (card-ability state :contestant hok-scored 0)
      (is (= 2 (count (:discard (get-challenger)))) "Challenger should pay 1 net damage"))))

(deftest hyperloop-extension
  ;; Hyperloop Extension
  (testing "Score"
    (do-game
      (new-game (default-contestant ["Hyperloop Extension"])
                (default-challenger))
      (play-from-hand state :contestant "Hyperloop Extension" "New party")
      (is (= 5 (:credit (get-contestant))) "Contestant starts with 5 credits")
      (score-agenda state :contestant (get-content state :party1 0))
      (is (= 8 (:credit (get-contestant))) "Contestant gains 3 credits")))
  (testing "Steal"
    (do-game
      (new-game (default-contestant ["Hyperloop Extension"])
                (default-challenger))
      (play-from-hand state :contestant "Hyperloop Extension" "New party")
      (take-credits state :contestant)
      (run-empty-locale state "Locale 1")
      (is (= 7 (:credit (get-contestant))) "Contestant starts with 5 credits")
      (click-prompt state :challenger "Steal")
      (is (= 10 (:credit (get-contestant))) "Contestant gains 3 credits"))))

(deftest ikawah-project
  ;; Ikawah Project
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Ikawah Project"])
                (default-challenger))
      (play-from-hand state :contestant "Ikawah Project" "New party")
      (testing "No credits"
        (take-credits state :contestant)
        (core/lose state :challenger :credit (:credit (get-challenger)) :click 3)
        (run-empty-locale state :party1)
        (click-prompt state :challenger "No action")
        (is (zero? (:credit (get-challenger))) "Challenger couldn't afford to steal, so no credits spent")
        (is (zero? (count (:scored (get-challenger)))) "Challenger could not steal Ikawah Project"))
      (testing "No clicks"
        (take-credits state :challenger)
        (take-credits state :contestant)
        (core/lose state :challenger :credit (:credit (get-challenger)) :click 3)
        (run-empty-locale state :party1)
        (click-prompt state :challenger "No action")
        (is (zero? (:click (get-challenger))) "Challenger couldn't afford to steal, so no clicks spent")
        (is (zero? (count (:scored (get-challenger)))) "Challenger could not steal Ikawah Project"))
      (testing "Enough of both"
        (take-credits state :challenger)
        (take-credits state :contestant)
        (core/lose state :challenger :credit (:credit (get-challenger)) :click (:click (get-challenger)))
        (core/gain state :challenger :credit 5 :click 4)
        (is (= 5 (:credit (get-challenger))) "Challenger should be reset to 5 credits")
        (is (= 4 (:click (get-challenger))) "Challenger should be reset to 4 clicks")
        (run-empty-locale state :party1)
        (click-prompt state :challenger "Pay to steal")
        (click-prompt state :challenger "[Click]")
        (click-prompt state :challenger "2 [Credits]")
        (is (= 2 (:click (get-challenger))) "Challenger should lose 1 click to steal")
        (is (= 3 (:credit (get-challenger))) "Challenger should lose 2 credits to steal")
        (is (= 3 (:agenda-point (get-challenger))))
        (is (= 1 (count (:scored (get-challenger)))) "Challenger should steal Ikawah Project"))))
  (testing "Not stealing"
    ;; do not reveal when the Challenger does not steal from R&D
    (do-game
      (new-game (default-contestant [(qty "Ikawah Project" 2)])
                (default-challenger))
      (take-credits state :contestant)
      (starting-hand state :contestant ["Ikawah Project"])
      (run-empty-locale state "R&D")
      (click-prompt state :challenger "No action")
      (is (not (last-log-contains? state "Ikawah Project")) "Ikawah Project should not be mentioned")
      (run-empty-locale state "HQ")
      (click-prompt state :challenger "No action")
      (is (last-log-contains? state "Ikawah Project") "Ikawah Project should be mentioned"))))

(deftest illicit-sales
  ;; Illicit Sales
  (letfn [(illicit-sales-test [[starting-bp answer credits-gained]]
            (testing (str "starting with " starting-bp " and answering " answer " and gaining " credits-gained)
              (do-game
                (new-game (default-contestant ["Illicit Sales"])
                          (default-challenger))
                (let [credits (:credit (get-contestant))]
                  (core/gain state :contestant :bad-publicity starting-bp)
                  (play-and-score state "Illicit Sales")
                  (click-prompt state :contestant answer)
                  (is (= (:credit (get-contestant)) (+ credits credits-gained)))))))]
    (doall (map illicit-sales-test
                [[0 "No" 0]
                 [0 "Yes" 3]
                 [1 "No" 3]
                 [1 "Yes" 6]
                 [2 "No" 6]
                 [2 "Yes" 9]
                 [3 "No" 9]
                 [3 "Yes" 12]]))))

(deftest improved-protein-source
  ;; Improved Protein Source
  (do-game
    (new-game (default-contestant [(qty "Improved Protein Source" 2)])
              (default-challenger))
    (is (= 5 (:credit (get-challenger))) "Challenger starts with 5 credits")
    (play-and-score state "Improved Protein Source")
    (is (= 9 (:credit (get-challenger))) "Challenger should gain 4 credits from Contestant scoring")
    (play-from-hand state :contestant "Improved Protein Source" "New party")
    (take-credits state :contestant)
    (run-empty-locale state :party2)
    (click-prompt state :challenger "Steal")
    (is (= 13 (:credit (get-challenger))) "Challenger should gain 4 credits from Contestant scoring")))

(deftest improved-tracers
  ;; Improved Tracers
  (do-game
    (new-game (default-contestant ["Improved Tracers" "News Hound" "Information Overload"])
              (default-challenger))
    (core/gain state :contestant :credit 10)
    (play-from-hand state :contestant "News Hound" "HQ")
    (play-from-hand state :contestant "Information Overload" "R&D")
    (let [nh (get-character state :hq 0)
          io (get-character state :rd 0)]
      (core/reveal state :contestant nh)
      (core/reveal state :contestant io)
      (is (= 4 (:current-strength (refresh nh))) "Should start with base strength of 4")
      (is (= 7 (:credit (get-contestant))) "Should have 7 credits after reveal")
      (play-and-score state "Improved Tracers")
      (is (= 5 (:current-strength (refresh nh))) "Should gain 1 strength from 4 to 5")
      (take-credits state :contestant)
      (run-on state "HQ")
      (card-subroutine state :contestant nh 0)
      (is (= 1 (-> (get-contestant) :prompt first :bonus)) "Should gain 1 bonus trace strength")
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (is (= 1 (:tag (get-challenger))))
      (run-jack-out state)
      (run-on state "HQ")
      (card-subroutine state :contestant nh 0)
      (is (= 1 (-> (get-contestant) :prompt first :bonus))
          "Should gain only 1 bonus trace strength regardless of number of runs in a turn")
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (is (= 2 (:tag (get-challenger))))
      (run-on state "R&D")
      (card-ability state :contestant io 1)
      (is (zero? (-> (get-contestant) :prompt first :bonus)) "Should gain 0 bonus trace strength, as it's an encounter ability"))))

(deftest jumon
  ;; Jumon
  (do-game
    (new-game (default-contestant ["Jumon" "Ice Wall" "Crisium Grid" "Project Atlas"])
              (default-challenger))
    (play-and-score state "Jumon")
    (play-from-hand state :contestant "Ice Wall" "New party")
    (play-from-hand state :contestant "Project Atlas" "Locale 2")
    (core/end-turn state :contestant nil)
    (let [pa (get-content state :party2 0)
          iw (get-character state :party2 0)]
      (is (zero? (get-counters (refresh pa) :advancement)) "Project Atlas starts with no counters")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall starts with no counters")
      (click-card state :contestant iw)
      (click-card state :contestant pa)
      (is (= 2 (get-counters (refresh pa) :advancement)) "Project Atlas gains 2 counters")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall doesn't gain any counters")
      (core/start-turn state :challenger nil)
      (take-credits state :challenger)
      (play-from-hand state :contestant "Crisium Grid" "Locale 2")
      (let [cg (get-content state :party2 1)]
        (is (zero? (get-counters (refresh cg) :advancement)) "Crisium Grid starts with no counters")
        (core/end-turn state :contestant nil)
        (click-card state :contestant cg)
        (is (= 2 (get-counters (refresh cg) :advancement)) "Crisium Grid gains 2 counters")))))

(deftest labyrinthine-locales
  ;; Labyrinthine Locales
  (do-game
    (new-game (default-contestant [(qty "Labyrinthine Locales" 2)])
              (default-challenger))
    (play-and-score state "Labyrinthine Locales")
    (play-and-score state "Labyrinthine Locales")
    (take-credits state :contestant)
    (let [ls1 (get-scored state :contestant 0)
          ls2 (get-scored state :contestant 1)]
      (is (= 2 (get-counters (refresh ls1) :power)))
      (is (= 2 (get-counters (refresh ls2) :power)))
      (testing "Don't use token"
        (run-on state "HQ")
        (run-jack-out state)
        (is (:run @state) "Jack out prevent prompt")
        (click-prompt state :contestant "Done")
        (is (not (:run @state)) "Contestant does not prevent the jack out, run ends"))
      (testing "Use token"
        (run-on state "HQ")
        (run-jack-out state)
        (card-ability state :contestant ls1 0)
        (card-ability state :contestant ls2 0)
        (card-ability state :contestant ls1 0)
        (click-prompt state :contestant "Done")
        (is (:run @state) "Jack out prevented, run is still ongoing")
        (is (true? (get-in @state [:run :cannot-jack-out])) "Cannot jack out flag is in effect")
        (run-successful state)
        (is (not (:run @state))))
      (testing "one Labyrinthine is empty but the other still has one token, ensure prompt still occurs"
        (is (zero? (get-counters (refresh ls1) :power)))
        (is (= 1 (get-counters (refresh ls2) :power)))
        (run-on state "HQ")
        (run-jack-out state)
        (is (:run @state))
        (card-ability state :contestant ls2 0)
        (click-prompt state :contestant "Done")
        (is (true? (get-in @state [:run :cannot-jack-out])))
        (run-successful state)
        (is (not (:run @state))))
      (testing "No more tokens"
        (run-on state "HQ")
        (run-jack-out state)
        (is (not (:run @state)) "No jack out prevent prompt")))))

(deftest lcharacternse-acquisition
  ;; Lcharacternse Acquisition
  (do-game
    (new-game (default-contestant [(qty "Lcharacternse Acquisition" 4)
                             "Adonis Campaign" "Eve Campaign"
                             "Strongbox" "Contestantorate Troubleshooter"])
              (default-challenger))
    (testing "Set up"
      (starting-hand state :contestant ["Lcharacternse Acquisition" "Lcharacternse Acquisition" "Lcharacternse Acquisition" "Lcharacternse Acquisition"
                                  "Adonis Campaign" "Strongbox"])
      (core/move state :contestant (find-card "Eve Campaign" (:deck (get-contestant))) :discard)
      (core/move state :contestant (find-card "Contestantorate Troubleshooter" (:deck (get-contestant))) :discard)
      (core/gain state :contestant :click 4))
    (testing "Site & HQ"
      (play-and-score state "Lcharacternse Acquisition")
      (click-card state :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
      (click-prompt state :contestant "New party")
      (is (some? (get-content state :party2 0))))
    (testing "Region & HQ"
      (play-and-score state "Lcharacternse Acquisition")
      (click-card state :contestant (find-card "Strongbox" (:hand (get-contestant))))
      (click-prompt state :contestant "New party")
      (is (some? (get-content state :party4 0))))
    (testing "Site & Archives"
      (play-and-score state "Lcharacternse Acquisition")
      (click-card state :contestant (find-card "Eve Campaign" (:discard (get-contestant))))
      (click-prompt state :contestant "New party")
      (is (some? (get-content state :party6 0))))
    (testing "Region & Archives"
      (play-and-score state "Lcharacternse Acquisition")
      (click-card state :contestant (find-card "Contestantorate Troubleshooter" (:discard (get-contestant))))
      (click-prompt state :contestant "New party")
      (is (some? (get-content state :party8 0))))))

(deftest mandatory-seed-replacement
  ;; Mandatory Seed Replacement
  (do-game
    (new-game (default-contestant ["Mandatory Seed Replacement"
                             "Ice Wall" "Fire Wall"
                             "Kakugo" "Chum"
                             "RSVP" "Sensei"])
              (default-challenger))
    (core/click-draw state :contestant 2)
    (core/gain state :contestant :click 10 :credit 10)
    (play-from-hand state :contestant "Ice Wall" "Archives")
    (play-from-hand state :contestant "Fire Wall" "R&D")
    (play-from-hand state :contestant "Kakugo" "HQ")
    (play-from-hand state :contestant "Chum" "Archives")
    (play-from-hand state :contestant "RSVP" "R&D")
    (play-from-hand state :contestant "Sensei" "HQ")
    (let [iw (get-character state :archives 0)
          fw (get-character state :rd 0)
          kk (get-character state :hq 0)
          ch (get-character state :archives 1)
          rs (get-character state :rd 1)
          sn (get-character state :hq 1)]
      (core/reveal state :contestant iw)
      (core/reveal state :contestant fw)
      (core/reveal state :contestant kk)
      (core/reveal state :contestant ch)
      (core/reveal state :contestant rs)
      (core/reveal state :contestant sn)
      (play-and-score state "Mandatory Seed Replacement")
      (click-card state :contestant (refresh iw))
      (click-card state :contestant (refresh fw))
      (click-card state :contestant (refresh kk))
      (click-card state :contestant (refresh ch))
      (click-card state :contestant (refresh rs))
      (click-card state :contestant (refresh sn)))))

(deftest mandatory-regions
  ;; Mandatory Regions
  (testing "Gain an additional click"
    (do-game
      (new-game (default-contestant ["Mandatory Regions"
                               "Melange Mining Contestant."])
                (default-challenger))
      (play-and-score state "Mandatory Regions")
      (is (= 2 (:agenda-point (get-contestant))))
      (play-from-hand state :contestant "Melange Mining Contestant." "New party")
      (let [mmc (get-content state :party2 0)]
        (core/reveal state :contestant mmc)
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (= 4 (:click (get-contestant))))
        (card-ability state :contestant mmc 0)
        (is (= 1 (:click (get-contestant)))))))
  (testing "Lose additional click if sacrifcharacterd"
    (do-game
      (new-game (default-contestant ["Mandatory Regions"
                               "Archer"])
                (default-challenger))
      (play-and-score state "Mandatory Regions")
      (is (= 2 (:agenda-point (get-contestant))))
      (play-from-hand state :contestant "Archer" "HQ")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (let [arc (get-character state :hq 0)
            mu (get-scored state :contestant 0)]
        (is (= 4 (:click (get-contestant))) "Contestant should start turn with 4 clicks")
        (core/reveal state :contestant arc)
        (click-card state :contestant (refresh mu))
        (is (= 3 (:click (get-contestant))) "Contestant should lose 1 click on agenda sacrifcharacter")))))

(deftest market-research
  ;; Market Research
  (do-game
    (new-game (default-contestant [(qty "Market Research" 2)])
              (default-challenger))
    (testing "Challenger is not tagged"
      (play-and-score state "Market Research")
      (is (= 2 (:agenda-point (get-contestant))) "Only 4 advancements: scored for standard 2 points"))
    (testing "Challenger is tagged"
      (core/gain state :challenger :tag 1)
      (play-and-score state "Market Research")
      (is (= 5 (:agenda-point (get-contestant))) "5 advancements: scored for 3 points"))))

(deftest medical-breakthrough
  ;; Medical Breakthrough
  (do-game
    (new-game (default-contestant [(qty "Medical Breakthrough" 3) (qty "Hedge Fund" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Medical Breakthrough" "New party")
    (play-from-hand state :contestant "Medical Breakthrough" "New party")
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (run-empty-locale state :party1)
    (click-prompt state :challenger "Steal")
    (take-credits state :challenger)
    (let [mb2 (get-content state :party2 0)]
      (advance state mb2 3)
      (core/score state :contestant {:card (refresh mb2)})
      (is (= 2 (:agenda-point (get-contestant))) "Only needed 3 advancements to score"))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Medical Breakthrough" "New party")
    (let [mb3 (get-content state :party3 0)]
      (advance state mb3 2)
      (core/score state :contestant {:card (refresh mb3)})
      (is (= 4 (:agenda-point (get-contestant))) "Only needed 2 advancements to score"))))

(deftest merger
  ;; Merger
  (do-game
    (new-game (default-contestant [(qty "Merger" 2)])
              (default-challenger))
    (play-and-score state "Merger")
    (is (= 2 (:agenda-point (get-contestant))) "Contestant should score 2 points")
    (play-from-hand state :contestant "Merger" "New party")
    (take-credits state :contestant)
    (run-empty-locale state :party2)
    (click-prompt state :challenger "Steal")
    (is (= 3 (:agenda-point (get-challenger))) "Challenger should score 3 points")))

(deftest meteor-mining
  ;; Meteor Mining
  (testing "when Meteor Mining is stolen"
    (do-game
      (new-game (default-contestant ["Meteor Mining"])
                (default-challenger))
      (play-from-hand state :contestant "Meteor Mining" "New party")
      (take-credits state :contestant)
      (run-empty-locale state :party1)
      (click-prompt state :challenger "Steal")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger should score 2 points")))
  (testing "when Meteor Mining is scored"
    (letfn [(meteor-mining-test [[tags num-choices pick creds dmg]]
              (do-game
                (new-game (default-contestant ["Meteor Mining"])
                          (default-challenger [(qty "Sure Gamble" 7)]))
                (starting-hand state :challenger (repeat 7 "Sure Gamble"))
                (let [credits (:credit (get-contestant))
                      grip (count (:hand (get-challenger)))]
                  (core/gain state :challenger :tag tags)
                  (play-and-score state "Meteor Mining")
                  (is (= num-choices (count (:choices (first (get-in @state [:contestant :prompt]))))))
                  (click-prompt state :contestant pick)
                  (is (= (+ credits creds) (:credit (get-contestant)))
                      (str "Contestant should have " (+ credits creds) " credits"))
                  (is (= (- grip dmg) (count (:hand (get-challenger))))
                      (str "Challenger should have " (- grip dmg) " cards in hand")))))]
      (doall (map meteor-mining-test
                  [[0 2 "No action" 0 0]
                   [0 2 "Gain 7 [Credits]" 7 0]
                   [1 2 "No action" 0 0]
                   [1 2 "Gain 7 [Credits]" 7 0]
                   [2 3 "No action" 0 0]
                   [2 3 "Gain 7 [Credits]" 7 0]
                   [2 3 "Do 7 meat damage" 0 7]
                   [3 3 "No action" 0 0]
                   [3 3 "Gain 7 [Credits]" 7 0]
                   [3 3 "Do 7 meat damage" 0 7]])))))

(deftest napd-contract
  ;; NAPD Contract
  (testing "basic test"
    (do-game
      (new-game (default-contestant ["NAPD Contract"])
                (default-challenger))
      (play-from-hand state :contestant "NAPD Contract" "New party")
      (let [napd (get-content state :party1 0)]
        (advance state napd 2)
        (take-credits state :contestant)
        (core/lose state :challenger :credit 2)
        (run-empty-locale state "Locale 1")
        (click-prompt state :challenger "No action")
        (is (zero? (count (:scored (get-challenger)))) "Challenger could not steal NAPD Contract")
        (is (= 3 (:credit (get-challenger))) "Challenger couldn't afford to steal, so no credits spent")
        (take-credits state :challenger)
        (core/gain state :contestant :bad-publicity 1)
        (advance state napd 2)
        (core/score state :contestant {:card (refresh napd)})
        (is (some? (get-content state :party1 0))
            "Contestant can't score with 4 advancements because of BP")
        (advance state napd)
        (core/score state :contestant {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-contestant))) "Scored NAPD for 2 points after 5 advancements"))))
  (testing "scoring requirement increases with bad publicity from Contestantorate Scandal"
    (do-game
      (new-game (default-contestant ["NAPD Contract"])
                (default-challenger ["Contestantorate Scandal"]))
      (play-from-hand state :contestant "NAPD Contract" "New party")
      (let [napd (get-content state :party1 0)]
        (advance state napd 2)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Contestantorate Scandal")
        (take-credits state :challenger)
        (advance state napd 2)
        (core/score state :contestant {:card (refresh napd)})
        (is (some? (get-content state :party1 0))
            "Contestant can't score with 4 advancements because of BP")
        (advance state napd)
        (core/score state :contestant {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-contestant))) "Scored NAPD for 2 points after 5 advancements")))))

(deftest net-quarantine
  ;; Net Quarantine
  (do-game
    (new-game (default-contestant ["Net Quarantine"])
              (default-challenger))
    (core/gain state :challenger :link 1)
    (core/gain state :contestant :click 3)
    (play-and-score state "Net Quarantine")
    (let [credits (:credit (get-contestant))]
      (is (= credits (:credit (get-contestant))) (str "Contestant has " credits " credits"))
      (is (= 1 (:link (get-challenger))) "Challenger has 1 link")
      (core/init-trace state :contestant {:title "/trace command" :side :contestant} {:base 1})
      (click-prompt state :contestant "0")
      (is (zero? (-> (get-challenger) :prompt first :link)) "Challenger has 0 link during first trace")
      (click-prompt state :challenger "3")
      (is (= (+ credits 1) (:credit (get-contestant))) "Contestant gained a credit from NQ")
      ; second trace of turn - no link reduction
      (core/init-trace state :contestant {:title "/trace command" :side :contestant} {:base 1})
      (click-prompt state :contestant "0")
      (is (= 1 (-> (get-challenger) :prompt first :link)) "Challenger has 1 link during later traces")
      (click-prompt state :challenger "2")
      (is (= (+ credits 2) (:credit (get-contestant))) "Contestant gained a credit from NQ"))))

(deftest new-construction
  ;; New Construction
  (do-game
    (new-game (default-contestant ["New Construction" (qty "Commercial Bankers Group" 10)])
              (default-challenger))
    (starting-hand state :contestant (vec (cons "New Construction" (repeat 10 "Commercial Bankers Group"))))
    (core/gain state :contestant :click 10 :credit 10)
    (play-from-hand state :contestant "New Construction" "New party")
    (let [nc (get-content state :party1 0)]
      (is (zero? (get-counters (refresh nc) :advancement)))
      (dotimes [n 4]
        (advance state (refresh nc))
        (click-prompt state :contestant "Yes")
        (click-card state :contestant (find-card "Commercial Bankers Group" (:hand (get-contestant)))))
      (is (= 4 (get-counters (refresh nc) :advancement)))
      (is (not= :this-turn (:revealed (get-content state :party5 0))))
      (let [credits (:credit (get-contestant))]
        (advance state (refresh nc))
        (click-prompt state :contestant "Yes")
        (click-card state :contestant (find-card "Commercial Bankers Group" (:hand (get-contestant))))
        (is (= 5 (get-counters (refresh nc) :advancement)))
        (is (= :this-turn (:revealed (get-content state :party6 0))))
        (is (= (dec credits) (:credit (get-contestant))))))))

(deftest next-wave-2
  ;; NEXT Wave 2
  (do-game
    (new-game (default-contestant [(qty "NEXT Wave 2" 2) "NEXT Bronze"])
              (default-challenger))
    (is (zero? (:brain-damage (get-challenger))) "Challenger should start with 0 brain damage")
    (play-from-hand state :contestant "NEXT Bronze" "HQ")
    (let [nxbr (get-character state :hq 0)]
      (core/reveal state :contestant nxbr))
    (play-and-score state "NEXT Wave 2")
    (click-prompt state :contestant "No")
    (is (zero? (:brain-damage (get-challenger))) "Challenger should stay at 0 brain damage")
    (play-and-score state "NEXT Wave 2")
    (click-prompt state :contestant "Yes")
    (is (= 1 (:brain-damage (get-challenger))) "Challenger should gain 1 brain damage")))

(deftest nisei-mk-ii
  ;; Nisei MK II - Remove hosted counter to ETR, check this works in 4.3
  (do-game
    (new-game (default-contestant ["Nisei MK II"])
              (default-challenger))
    (play-and-score state "Nisei MK II")
    (let [scored-nisei (get-scored state :contestant 0)]
      (is (= 1 (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has one counter")
      (take-credits state :contestant)
      (run-on state "HQ")
      (run-phase-43 state)
      (card-ability state :contestant (refresh scored-nisei) 0)
      (click-prompt state :contestant "Done") ; close 4.3 contestant
      (is (not (:run @state)) "Run ended by using Nisei counter")
      (is (zero? (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has no counters"))))

(deftest oaktown-renovation
  ;; Oaktown Renovation
  (do-game
    (new-game (default-contestant ["Oaktown Renovation" "Shipment from SanSan"])
              (default-challenger))
    (core/gain state :contestant :click 3)
    (play-from-hand state :contestant "Oaktown Renovation" "New party")
    (let [oak (get-content state :party1 0)]
      (is (:revealed (refresh oak)) "Oaktown placed face up")
      (advance state oak)
      (is (= 6 (:credit (get-contestant))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (play-from-hand state :contestant "Shipment from SanSan")
      (click-prompt state :contestant "2")
      (click-card state :contestant oak)
      (is (= 3 (get-counters (refresh oak) :advancement)))
      (is (= 6 (:credit (get-contestant))) "No credits gained due to advancements being placed")
      (advance state oak)
      (is (= 7 (:credit (get-contestant))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (advance state oak)
      (is (= 5 (get-counters (refresh oak) :advancement)))
      (is (= 9 (:credit (get-contestant)))
          "Spent 1 credit to advance, gained 3 credits from Oaktown"))))

(deftest obokata-protocol
  ;; Obotaka Protocol
  (do-game
    (new-game (make-deck "Cardnum: Personal Evolution" [(qty "Obokata Protocol" 10)])
              (default-challenger [(qty "Sure Gamble" 4)]))
    (play-from-hand state :contestant "Obokata Protocol" "New party")
    (take-credits state :contestant)
    (core/gain state :challenger :agenda-point 6)
    (run-empty-locale state "Locale 1")
    (click-prompt state :challenger "Pay 4 net damage to steal")
    (is (= 4 (count (:discard (get-challenger)))) "Challenger paid 4 net damage")
    (is (= :challenger (:winner @state)) "Challenger wins")
    (is (= "Agenda" (:reason @state)) "Win condition reports agenda points")
    (is (last-log-contains? state "wins the game") "PE did not fire")))

(deftest paper-trail
  ;; Paper Trail
  (do-game
    (new-game (default-contestant ["Paper Trail"])
              (default-challenger ["Aeneas Informant" "Bank Job"
                               "Rosetta 2.0" "Magnum Opus"
                               "Astrolabe"]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 10 :credit 10)
    (play-from-hand state :challenger "Aeneas Informant")
    (play-from-hand state :challenger "Bank Job")
    (play-from-hand state :challenger "Rosetta 2.0")
    (play-from-hand state :challenger "Magnum Opus")
    (play-from-hand state :challenger "Astrolabe")
    (take-credits state :challenger)
    (play-and-score state "Paper Trail")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (is (= 2 (count (:discard (get-challenger)))))
    (is (some? (get-radicle state 0)))
    (is (= 1 (count (get-radicle state))))
    (is (some? (get-resource state 0)))
    (is (some? (get-hazard state 0)))))

(deftest personality-profiles
  ;; Personality Profiles
  (testing "basic test"
    (do-game
      (new-game (default-contestant ["Personality Profiles"])
                (default-challenger ["Self-modifying Code" "Clone Chip"
                                 "Corroder" (qty "Patron" 2)]))
      (starting-hand state :challenger ["Self-modifying Code" "Clone Chip" "Patron" "Patron"])
      (play-and-score state "Personality Profiles")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Self-modifying Code")
      (play-from-hand state :challenger "Clone Chip")
      (let [smc (get-resource state 0)]
        (card-ability state :challenger smc 0)
        (click-prompt state :challenger (find-card "Corroder" (:deck (get-challenger))))
        (is (= 2 (count (:discard (get-challenger))))))
      (let [chip (get-hazard state 0)]
        (card-ability state :challenger chip 0)
        (click-card state :challenger (find-card "Self-modifying Code" (:discard (get-challenger))))
        (is (second-last-log-contains? state "Patron")
            "Personality Profiles discarded card name is in log")
        (is (= 3 (count (:discard (get-challenger))))))))
  (testing "Ensure effects still fire with an empty hand, #1840"
    (do-game
      (new-game (default-contestant ["Personality Profiles"])
                (default-challenger ["Self-modifying Code" "Clone Chip"
                                 "Corroder"]))
      (starting-hand state :challenger ["Self-modifying Code" "Clone Chip"])
      (play-and-score state "Personality Profiles")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Self-modifying Code")
      (play-from-hand state :challenger "Clone Chip")
      (let [smc (get-resource state 0)]
        (card-ability state :challenger smc 0)
        (click-prompt state :challenger (find-card "Corroder" (:deck (get-challenger)))))
      (let [cor (get-resource state 0)]
        (is (some? cor))
        (is (= (:title cor) "Corroder"))
        (is (= "Self-modifying Code" (:title (first (:discard (get-challenger)))))))
      (let [chip (get-hazard state 0)]
        (card-ability state :challenger chip 0)
        (click-card state :challenger (find-card "Self-modifying Code" (:discard (get-challenger)))))
      (let [smc (get-resource state 1)]
        (is (some? smc))
        (is (= (:title smc) "Self-modifying Code"))
        (is (= "Clone Chip" (:title (first (:discard (get-challenger))))))))))

(deftest philotic-entanglement
  ;; Philotic Entanglement
  (do-game
    (new-game (default-contestant ["Philotic Entanglement" (qty "House of Knives" 3)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Cache" 2)]))
    (play-from-hand state :contestant "House of Knives" "New party")
    (play-from-hand state :contestant "House of Knives" "New party")
    (play-from-hand state :contestant "House of Knives" "New party")
    (take-credits state :contestant)
    (run-empty-locale state :party1)
    (click-prompt state :challenger "Steal")
    (run-empty-locale state :party2)
    (click-prompt state :challenger "Steal")
    (run-empty-locale state :party3)
    (click-prompt state :challenger "Steal")
    (is (= 3 (count (:scored (get-challenger)))))
    (take-credits state :challenger)
    (play-and-score state "Philotic Entanglement")
    (is (= 2 (:agenda-point (get-contestant))))
    (is (= 3 (count (:discard (get-challenger)))) "Dealt 3 net damage upon scoring")))

(deftest posted-bounty
  ;; Posted Bounty
  (testing "Forfeiting takes 1 bad publicity"
    (do-game
      (new-game (default-contestant ["Posted Bounty"])
                (default-challenger))
      (play-and-score state "Posted Bounty")
      (click-prompt state :contestant "Yes")
      (is (zero? (:agenda-point (get-contestant))) "Forfeiting Posted Bounty nullifies agenda points")
      (is (= 1 (:bad-publicity (get-contestant))) "Forfeiting takes 1 bad publicity")
      (is (= 1 (:tag (get-challenger))) "Challenger receives 1 tag forfeiting Posted Bounty")))
  (testing "Choosing not to forfeit scores normally"
    (do-game
      (new-game (default-contestant ["Posted Bounty"])
                (default-challenger))
      (play-and-score state "Posted Bounty")
      (click-prompt state :contestant "No")
      (is (= 1 (:agenda-point (get-contestant))))
      (is (zero? (:bad-publicity (get-contestant))))
      (is (zero? (:tag (get-challenger)))))))

(deftest priority-requisition
  ;; Priority Requisition
  (do-game
    (new-game (default-contestant ["Priority Requisition" "Archer"])
              (default-challenger))
    (play-from-hand state :contestant "Archer" "HQ")
    (let [arc (get-character state :hq 0)]
      (play-and-score state "Priority Requisition")
      (click-card state :contestant arc)
      (is (:revealed (refresh arc))))))

(deftest private-security-force
  ;; Private Security Force
  (do-game
    (new-game (default-contestant [(qty "Private Security Force" 10)])
              (default-challenger))
    (core/gain state :challenger :tag 1)
    (play-and-score state "Private Security Force")
    (let [psf-scored (get-scored state :contestant 0)]
      (card-ability state :contestant psf-scored 0)
      (is (= 1 (count (:discard (get-challenger)))))
      (take-credits state :challenger)
      (dotimes [n 3]
        (card-ability state :contestant psf-scored 0))
      (is (= 3 (count (:discard (get-challenger)))))
      (is (= :contestant (:winner @state)) "Contestant wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest profiteering
  ;; Profiteering
  (do-game
    (new-game (default-contestant ["Profiteering"])
              (default-challenger))
    (play-and-score state "Profiteering")
    (click-prompt state :contestant "3")
    (is (= 1 (:agenda-point (get-contestant))))
    (is (= 3 (:bad-publicity (get-contestant))) "Took 3 bad publicity")
    (is (= 20 (:credit (get-contestant))) "Gained 15 credits")))

(deftest project-ares
  ;; Project Ares
  (do-game
    (new-game (default-contestant [(qty "Project Ares" 2)])
              (default-challenger ["Clone Chip"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Clone Chip")
    (take-credits state :challenger)
    (play-and-score state "Project Ares")
    (is (empty? (get-in @state [:challenger :prompt])) "No prompt for Challenger if scored with 4 advancement tokens")
    (core/gain state :contestant :click 5)
    (play-from-hand state :contestant "Project Ares" "New party")
    (let [ares (get-content state :party2 0)]
      (advance state ares 6)
      (is (= 6 (get-counters (refresh ares) :advancement)))
      (core/score state :contestant {:card (refresh ares)})
      (is (prompt-is-card? state :challenger ares) "Challenger has Ares prompt to discard placed cards"))
    (click-card state :challenger (find-card "Clone Chip" (:hazard (:rig (get-challenger)))))
    (is (empty? (get-in @state [:challenger :prompt])) "Challenger must discard 2 cards but only has 1 card in rig, prompt ended")
    (is (= 1 (count (:discard (get-challenger)))))
    (is (= 1 (:bad-publicity (get-contestant))))))

(deftest project-atlas
  ;; Project Atlas
  (testing "basic test"
    (do-game
      (new-game (default-challenger ["Project Atlas"
                                 "Beanstalk Royalties"])
                (default-challenger))
      ;; Set up
      (starting-hand state :contestant ["Project Atlas"])
      (is (= 1 (count (:hand (get-contestant)))) "Contestant should have 1 cards in hand")
      (core/gain state :contestant :click 10 :credit 10)
      ;; Should gain 1 counter
      (play-from-hand state :contestant "Project Atlas" "New party")
      (let [atlas (get-content state :party1 0)]
        (advance state atlas 4)
        (is (= 4 (get-counters (refresh atlas) :advancement)) "Atlas should have 4 advancement tokens")
        (core/score state :contestant {:card (refresh atlas)}))
      (let [atlas-scored (get-scored state :contestant 0)]
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counter")
        (card-ability state :contestant atlas-scored 0)
        (click-prompt state :contestant (find-card "Beanstalk Royalties" (:deck (get-contestant))))
        (is (zero? (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 0 agenda counters")
        (is (= 1 (count (:hand (get-contestant)))) "Contestant should have 1 cards in hand"))))
  (testing "test with Titan"
    (do-game
      (new-game (make-deck "Titan Transnational: Investing In Your Future"
                           [(qty "Project Atlas" 2) "Beanstalk Royalties" "Hedge Fund"])
                (default-challenger))
      ;; Set up
      (starting-hand state :contestant ["Project Atlas" "Project Atlas"])
      (is (= 2 (count (:hand (get-contestant)))) "Contestant should have 2 cards in hand")
      (core/gain state :contestant :click 10 :credit 10)
      ;; Should gain 1 counter
      (play-from-hand state :contestant "Project Atlas" "New party")
      (let [atlas (get-content state :party1 0)]
        (advance state atlas 3)
        (is (= 3 (get-counters (refresh atlas) :advancement)) "Atlas should have 3 advancement tokens")
        (core/score state :contestant {:card (refresh atlas)}))
      (let [atlas-scored (get-scored state :contestant 0)]
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counter")
        (card-ability state :contestant atlas-scored 0)
        (click-prompt state :contestant (find-card "Beanstalk Royalties" (:deck (get-contestant))))
        (is (zero? (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 0 agenda counters")
        (is (= 2 (count (:hand (get-contestant)))) "Contestant should have 2 card in hand"))
      ;; Should gain 2 counters
      (play-from-hand state :contestant "Project Atlas" "New party")
      (let [atlas (get-content state :party2 0)]
        (advance state atlas 4)
        (is (= 4 (get-counters (refresh atlas) :advancement)) "Atlas should have 4 advancement tokens")
        (core/score state :contestant {:card (refresh atlas)}))
      (let [atlas-scored (get-scored state :contestant 1)]
        (is (= 2 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 2 agenda counter")
        (card-ability state :contestant atlas-scored 0)
        (click-prompt state :contestant (find-card "Hedge Fund" (:deck (get-contestant))))
        (is (= 1 (get-counters (refresh atlas-scored) :agenda)) "Atlas should have 1 agenda counters")
        (is (= 2 (count (:hand (get-contestant)))) "Contestant should have 2 cards in hand")))))

(deftest project-beale
  ;; Project Beale
  (do-game
    (new-game (default-contestant [(qty "Project Beale" 2)])
              (default-challenger))
    (core/gain state :contestant :click 8 :credit 8)
    (play-from-hand state :contestant "Project Beale" "New party")
    (let [pb1 (get-content state :party1 0)]
      (advance state pb1 4)
      (core/score state :contestant {:card (refresh pb1)})
      (is (= 2 (:agenda-point (get-contestant))) "Only 4 advancements: scored for standard 2 points")
      (play-from-hand state :contestant "Project Beale" "New party"))
    (let [pb2 (get-content state :party2 0)]
      (advance state pb2 5)
      (core/score state :contestant {:card (refresh pb2)})
      (is (= 5 (:agenda-point (get-contestant))) "5 advancements: scored for 3 points"))))

(deftest project-kusanagi
  ;; Project Kusanagi
  (do-game
    (new-game (default-contestant [(qty "Project Kusanagi" 2) "Ice Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (core/gain state :contestant :click 10 :credit 10)
    (testing "Should gain 0 counters"
      (play-and-score state "Project Kusanagi")
      (let [pk-scored (get-scored state :contestant 0)]
        (is (zero? (get-counters (refresh pk-scored) :agenda)) "Kusanagi should start with 0 agenda counters")))
    (testing "Should gain 1 counter"
      (play-from-hand state :contestant "Project Kusanagi" "New party")
      (let [pk (get-content state :party2 0)]
        (advance state pk 3)
        (is (= 3 (get-counters (refresh pk) :advancement)) "Kusanagi should have 3 advancement tokens")
        (core/score state :contestant {:card (refresh pk)}))
      (let [pk-scored (get-scored state :contestant 1)]
        (is (= 1 (get-counters (refresh pk-scored) :agenda)) "Kusanagi should have 1 agenda counter")
        (run-empty-locale state :hq)
        (card-ability state :contestant pk-scored 0)
        (is (last-log-contains? state "Do 1 net damage"))
        (is (zero? (get-counters (refresh pk-scored) :agenda)) "Kusanagi should have 0 agenda counters")))))

(deftest project-vitruvius
  ;; Project Vitruvius
  (do-game
    (new-game (default-contestant ["Project Vitruvius"
                             "Hedge Fund"])
              (default-challenger))
    ;; Set up
    (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :discard)
    (is (= 1 (count (:discard (get-contestant)))) "Contestant should have 1 cards in hand")
    (is (= 1 (count (:hand (get-contestant)))) "Contestant should have 1 cards in hand")
    (core/gain state :contestant :click 10 :credit 10)
    ;; Should gain 1 counter
    (play-from-hand state :contestant "Project Vitruvius" "New party")
    (let [vit (get-content state :party1 0)]
      (advance state vit 4)
      (is (= 4 (get-counters (refresh vit) :advancement)) "Vitruvius should have 4 advancement tokens")
      (core/score state :contestant {:card (refresh vit)}))
    (let [vit-scored (get-scored state :contestant 0)]
      (is (= 1 (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 1 agenda counter")
      (card-ability state :contestant vit-scored 0)
      (click-card state :contestant (find-card "Hedge Fund" (:discard (get-contestant))))
      (is (zero? (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 0 agenda counters")
      (is (= 1 (count (:hand (get-contestant)))) "Contestant should have 1 cards in hand"))))

(deftest project-wotan
  ;; Project Wotan - Only checks if agenda counter is spent
  (do-game
    (new-game (default-contestant ["Project Wotan"
                             "Eli 1.0"
                             (qty "Hedge Fund" 3)])
              (default-challenger))
    (starting-hand state :contestant ["Project Wotan" "Eli 1.0"])
    (play-from-hand state :contestant "Eli 1.0" "HQ")
    (let [eli (get-character state :hq 0)]
      (core/reveal state :contestant eli))
    (play-and-score state "Project Wotan")
    (take-credits state :contestant)
    (let [wot-scored (get-scored state :contestant 0)]
      (is (= 3 (get-counters (refresh wot-scored) :agenda)) "Wotan should start with 3 agenda counters")
      (run-on state "HQ")
      (card-ability state :contestant wot-scored 0)
      (is (= 2 (get-counters (refresh wot-scored) :agenda))) "Wotan should only have 2 agenda counters")))

(deftest puppet-master
  ;; Puppet Master - game progresses if no valid targets. Issue #1661.
  (do-game
    (new-game (default-contestant ["Puppet Master"])
              (default-challenger))
    (play-and-score state "Puppet Master")
    (take-credits state :contestant)
    (run-empty-locale state :archives)
    (click-prompt state :contestant "Done")
    (is (empty? (:prompt (get-challenger))) "Challenger's waiting prompt resolved")))

(deftest quantum-predictive-model
  ;; Quantum Predictive Model
  (do-game
    (new-game (default-contestant [(qty "Quantum Predictive Model" 4)])
              (default-challenger))
    (testing "Set up"
      (starting-hand state :contestant ["Quantum Predictive Model" "Quantum Predictive Model"])
      (play-from-hand state :contestant "Quantum Predictive Model" "New party")
      (play-from-hand state :contestant "Quantum Predictive Model" "New party")
      (take-credits state :contestant))
    (testing "Access placed with no tag"
      (run-on state :party1)
      (run-successful state)
      (click-prompt state :challenger "Steal")
      (is (= 1 (:agenda-point (get-challenger))) "Challenger should steal"))
    (testing "Access R&D with no tag"
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :challenger "Steal")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger should steal"))
    (core/gain state :challenger :tag 1)
    (testing "Access intalled with tag"
      (run-on state :party2)
      (run-successful state)
      (click-prompt state :challenger "OK") ;; this is now a prompt that QPM was added to Contestant score area
      (is (= 2 (:agenda-point (get-challenger))) "Challenger should not steal")
      (is (= 1 (:agenda-point (get-contestant))) "Contestant should score"))
    (testing "Access R&D with tag"
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :challenger "OK")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger should not steal")
      (is (= 2 (:agenda-point (get-contestant))) "Contestant should score"))
    (is (zero? (count (:deck (get-contestant)))))))

(deftest rebranding-team
  ;; Rebranding Team
  (do-game
    (new-game (default-contestant ["Rebranding Team" "Launch Campaign" "City Surveillance"
                             "Jackson Howard" "Museum of History" "Advanced Assembly Lines"])
              (default-challenger))
    (play-and-score state "Rebranding Team")
    (core/click-draw state :challenger 1)
    (is (core/has-subtype? (find-card "Advanced Assembly Lines" (:hand (get-contestant))) "Advertisement"))
    ; #2608 part 2 - retain Advertisement always
    (discard-from-hand state :contestant "Advanced Assembly Lines")
    (is (core/has-subtype? (find-card "Advanced Assembly Lines" (:discard (get-contestant))) "Advertisement"))
    (is (core/has-subtype? (find-card "Launch Campaign" (:hand (get-contestant))) "Advertisement"))
    (is (core/has-subtype? (find-card "City Surveillance" (:hand (get-contestant))) "Advertisement"))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-contestant))) "Advertisement"))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-contestant))) "Executive"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Advertisement"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Alliance"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Ritzy"))
    (core/move state :contestant (find-card "Rebranding Team" (:scored (get-contestant))) :deck)
    (is (core/has-subtype? (find-card "Launch Campaign" (:hand (get-contestant))) "Advertisement"))
    (is (not (core/has-subtype? (find-card "Advanced Assembly Lines" (:discard (get-contestant))) "Advertisement")))
    (is (not (core/has-subtype? (find-card "City Surveillance" (:hand (get-contestant))) "Advertisement")))
    (is (not (core/has-subtype? (find-card "Jackson Howard" (:hand (get-contestant))) "Advertisement")))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-contestant))) "Executive"))
    (is (not (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Advertisement")))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Alliance"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Ritzy"))))

(deftest reeducation
  ;; Reeducation
  (testing "Simple test"
    (do-game
      (new-game (default-contestant ["Reeducation" "Sweeps Week" "Hedge Fund"
                               "Jackson Howard" "Gutenberg"])
                (default-challenger ["Self-modifying Code" "Clone Chip"
                                 "Corroder" "Sure Gamble" "Desperado"]))
      (starting-hand state :contestant ["Reeducation" "Sweeps Week"])
      (starting-hand state :challenger ["Self-modifying Code"])
      (play-and-score state "Reeducation")
      (is (prompt-is-type? state :challenger :waiting) "Challenger has wait prompt")
      (is (= 1 (count (get-in @state [:contestant :hand]))))
      (is (= 1 (count (get-in @state [:challenger :hand]))))
      (click-prompt state :contestant (find-card "Sweeps Week" (:hand (get-contestant)))) ; put Sweeps Week at bottom of R&D
      (click-prompt state :contestant "Done") ; finished selecting cards
      (click-prompt state :contestant "Done") ; contestant prompt for Done/Start Over
      (is (= "Sweeps Week" (:title (last (:deck (get-contestant))))))
      (is (= "Self-modifying Code" (:title (last (:deck (get-challenger))))))
      (is (= 1 (count (get-in @state [:contestant :hand]))))
      (is (zero? (count (get-in @state [:challenger :hand]))))))
  (testing "Extra cards"
    ;; If Contestant is adding more cards in HQ than Challenger has in their Grip, Challenger
    ;; is not 'able' to resolve the effect and doesn't have to add to bottom of Stack
    (do-game
      (new-game (default-contestant ["Reeducation" "Sweeps Week" "Hedge Fund"
                               "Jackson Howard" "Gutenberg"])
                (default-challenger ["Self-modifying Code" "Clone Chip"
                                 "Corroder" "Sure Gamble" "Desperado"]))
      (starting-hand state :contestant ["Reeducation" "Sweeps Week" "Hedge Fund"])
      (starting-hand state :challenger ["Self-modifying Code"])
      (play-and-score state "Reeducation")
      (is (prompt-is-type? state :challenger :waiting) "Challenger has wait prompt")
      (is (= 2 (count (:hand (get-contestant)))))
      (is (= 1 (count (:hand (get-challenger)))))
      (click-prompt state :contestant (find-card "Sweeps Week" (:hand (get-contestant))))
      (click-prompt state :contestant (find-card "Hedge Fund" (:hand (get-contestant)))) ; this is the bottom card of R&D
      (click-prompt state :contestant "Done") ; finished selecting cards
      (click-prompt state :contestant "Done") ; contestant prompt for Done/Start Over
      (is (= "Hedge Fund" (:title (last (:deck (get-contestant))))))
      (is (= "Sweeps Week" (:title (last (butlast (:deck (get-contestant)))))))
      (is (= "Self-modifying Code" (:title (first (:hand (get-challenger))))))
      (is (= 2 (count (:hand (get-contestant)))))
      (is (= 1 (count (:hand (get-challenger))))))))

(deftest party-data-farm
  ;; Party Data Farm
  (do-game
    (new-game (default-contestant ["Party Data Farm"])
              (default-challenger))
    (is (= 5 (get-hand-size :contestant)))
    (play-and-score state "Party Data Farm")
    (is (= 7 (get-hand-size :contestant)))))

(deftest party-enforcement
  ;; Party Enforcement - Search R&D for a piece of character and place it on a party at no reveal cost
  (do-game
   (new-game (default-contestant [(qty "Party Enforcement" 2)
                            "Archer"
                            "Chiyashi"])
             (make-deck "Reina Roja: Freedom Fighter" []))
   (starting-hand state :contestant ["Party Enforcement" "Party Enforcement"])
   (is (= 2 (count (:deck (get-contestant)))))
   (play-and-score state "Party Enforcement")
   (let [N (:credit (get-contestant))]
     (click-prompt state :contestant "Yes")
     (click-prompt state :contestant (find-card "Chiyashi" (:deck (get-contestant))))
     (click-prompt state :contestant "New party")
     (is (core/revealed? (get-character state :party2 0)) "Chiyashi was placed revealed")
     (is (= N (:credit (get-contestant))) "Revealing Chiyashi was free"))
   (play-and-score state "Party Enforcement")
   (let [N (:credit (get-contestant))]
     (click-prompt state :contestant "Yes")
     (click-prompt state :contestant (find-card "Archer" (:deck (get-contestant))))
     (click-prompt state :contestant "Locale 2")
     (is (= (dec N) (:credit (get-contestant))) "Placing Archer cost a credit")
     (is (not-empty (:prompt (get-contestant))) "Contestant prompted to forfeit an agenda for Archer")
     (is (= (dec N) (:credit (get-contestant))) "Revealing Archer didn't cost any credits"))))

(deftest research-grant
  ;; Research Grant
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Research Grant" 2)])
                (default-challenger))
      (play-from-hand state :contestant "Research Grant" "New party")
      (play-and-score state "Research Grant")
      (click-card state :contestant (get-content state :party1 0))
      (is (= 2 (count (:scored (get-contestant)))) "2 copies of Research Grant scored")))
  (testing "vs Leela"
    ;; Issue #3069
    (do-game
      (new-game (default-contestant [(qty "Research Grant" 2) (qty "Ice Wall" 2)])
                (make-deck "Leela Patel: Trained Pragmatist" ["Sure Gamble"]))
      (core/gain state :contestant :click 1)
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (play-from-hand state :contestant "Ice Wall" "R&D")
      (play-from-hand state :contestant "Research Grant" "New party")
      (play-and-score state "Research Grant")
      (click-card state :contestant (get-content state :party1 0))
      (is (= 2 (count (:scored (get-contestant)))) "2 copies of Research Grant scored")
      (click-card state :challenger (get-character state :hq 0))
      (click-card state :challenger (get-character state :rd 0))
      (is (empty? (:effect-completed @state)) "All score and Leela effects resolved"))))

(deftest restructured-datapool
  ;; Restructured Datapool
  (do-game
    (new-game (default-contestant ["Restructured Datapool"])
              (default-challenger))
    (is (zero? (:tag (get-challenger))) "Challenger should start with no tags")
    (play-and-score state "Restructured Datapool")
    (let [rd-scored (get-scored state :contestant 0)]
      (card-ability state :contestant rd-scored 0)
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (is (= 1 (:tag (get-challenger))) "Challenger should gain a tag from Restructured Datapool ability"))))

(deftest self-destruct-chips
  ;; Self-Destruct Chips
  (do-game
    (new-game (default-contestant ["Self-Destruct Chips"])
              (default-challenger))
    (is (= 5 (get-hand-size :challenger)) "Challenger's hand size starts at 5")
    (play-and-score state "Self-Destruct Chips")
    (is (= 4 (get-hand-size :challenger)) "By scoring Self-Destruct Chips, Challenger's hand size is reduced by 1")))

(deftest sensor-net-activation
  ;; Sensor Net Activation
  (do-game
    (new-game (default-contestant [(qty "Sensor Net Activation" 2) "Enforcer 1.0" "Ash 2X3ZB9CY"])
              (default-challenger))
    (play-from-hand state :contestant "Enforcer 1.0" "HQ")
    (play-and-score state "Sensor Net Activation")
    (let [sna-scored (get-scored state :contestant 0)
          enf (get-character state :hq 0)]
      (is (= 1 (get-counters (refresh sna-scored) :agenda)) "Should start with 1 agenda counter")
      (is (not (:revealed (refresh enf))) "Enforcer 1.0 should start hidden")
      (card-ability state :contestant (refresh sna-scored) 0)
      (click-card state :contestant enf)
      (is (:revealed (refresh enf)) "Enforcer 1.0 should be revealed")
      (is (= 1 (count (:scored (get-contestant)))) "Enforcer 1.0 should be revealed without forfeiting agenda")
      (take-credits state :contestant)
      (is (not (:revealed (refresh enf))) "Enforcer 1.0 should be hidden"))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Ash 2X3ZB9CY" "New party")
    (play-and-score state "Sensor Net Activation")
    (let [sna-scored (get-scored state :contestant 1)
          ash (get-content state :party2 0)]
      (is (= 1 (get-counters (refresh sna-scored) :agenda)) "Should start with 1 agenda counter")
      (is (not (:revealed (refresh ash))) "Ash should start hidden")
      (card-ability state :contestant (refresh sna-scored) 0)
      (click-card state :contestant ash)
      (is (:revealed (refresh ash)) "Ash should be revealed")
      (take-credits state :contestant)
      (is (not (:revealed (refresh ash))) "Ash should be hidden"))))

(deftest sentinel-defense-resource
  ;; Sentinel Defense Resource - Doesn't fire if brain damage is prevented
  (do-game
    (new-game (default-contestant ["Sentinel Defense Resource" "Viktor 1.0"])
              (default-challenger ["Feedback Filter" (qty "Sure Gamble" 3)]))
    (play-and-score state "Sentinel Defense Resource")
    (play-from-hand state :contestant "Viktor 1.0" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Feedback Filter")
    (let [viktor (get-character state :hq 0)
          ff (get-hazard state 0)]
      (run-on state "HQ")
      (core/reveal state :contestant viktor)
      (card-subroutine state :contestant viktor 0)
      (click-prompt state :challenger "Done")  ;; Don't prevent the brain damage
      (is (= 1 (count (:discard (get-challenger)))))
      (is (= 1 (:brain-damage (get-challenger))))
      (click-prompt state :challenger "Done")  ;; So we take the net, but don't prevent it either
      (is (= 2 (count (:discard (get-challenger)))))
      (card-subroutine state :contestant viktor 0)
      (card-ability state :challenger ff 1)  ;; Prevent the brain damage this time
      (click-prompt state :challenger "Done")
      (is (= 3 (count (:discard (get-challenger)))) "Feedback filter discarded, didn't take another net damage")
      (is (= 1 (:brain-damage (get-challenger)))))))

(deftest show-of-force
  ;; Show of Force
  (do-game
    (new-game (default-contestant ["Show of Force"])
              (default-challenger))
    (is (= 3 (count (:hand (get-challenger)))) "Challenger should start with 3 cards in hand")
    (play-and-score state "Show of Force")
    (is (= 1 (count (:hand (get-challenger)))) "Challenger should have 1 card in hand")
    (is (= 2 (count (:discard (get-challenger)))) "Challenger should have discarded 2 cards")))

(deftest ssl-endorsement
  ;; SSL Endorsement
  (testing "gain credits when in contestant score area before turn begins"
    (do-game
      (new-game (default-contestant ["SSL Endorsement"])
                (default-challenger))
      (play-and-score state "SSL Endorsement")
      (take-credits state :challenger)
      (is (not-empty (:prompt (get-contestant))) "Contestant prompted to take credits")
      (is (= 5 (:credit (get-contestant))) "Contestant starts with 5 credits")
      (click-prompt state :contestant "Yes")
      (is (= 8 (:credit (get-contestant))) "Contestant gains 3 credits")
      (take-credits state :challenger)
      (is (= 8 (:credit (get-contestant))) "Contestant starts with 8 credits")
      (click-prompt state :contestant "No")
      (is (= 8 (:credit (get-contestant))) "Contestant doesn't gain 3 credits")
      (take-credits state :challenger)
      (is (= 8 (:credit (get-contestant))) "Contestant starts with 8 credits")
      (click-prompt state :contestant "Yes")
      (is (= 11 (:credit (get-contestant))) "Contestant gains 3 credits")
      (take-credits state :challenger)
      (is (= 11 (:credit (get-contestant))) "Contestant starts with 11 credits")
      (click-prompt state :contestant "Yes")
      (is (= 14 (:credit (get-contestant))) "Contestant gains 3 credits")
      (take-credits state :challenger)
      (is (empty? (:prompt (get-contestant))) "Not prompted when out of money")))
  (testing "gain credits when in challenger score area before turn begins"
    (do-game
      (new-game (default-contestant ["SSL Endorsement"])
                (default-challenger))
      (play-from-hand state :contestant "SSL Endorsement" "New party")
      (take-credits state :contestant)
      (run-on state "Locale 1")
      (run-successful state)
      (click-prompt state :challenger "Steal")
      (take-credits state :challenger)
      (is (not-empty (:prompt (get-contestant))) "Contestant prompted to take credits")
      (is (= 7 (:credit (get-contestant))) "Contestant starts with 7 credits")
      (click-prompt state :contestant "Yes")
      (is (= 10 (:credit (get-contestant))) "Contestant gains 3 credits")
      (take-credits state :challenger)
      (is (= 10 (:credit (get-contestant))) "Contestant starts with 10 credits")
      (click-prompt state :contestant "No")
      (is (= 10 (:credit (get-contestant))) "Contestant doesn't gain 3 credits")
      (take-credits state :challenger)
      (is (= 10 (:credit (get-contestant))) "Contestant starts with 10 credits")
      (click-prompt state :contestant "Yes")
      (is (= 13 (:credit (get-contestant))) "Contestant gains 3 credits")
      (take-credits state :challenger)
      (is (= 13 (:credit (get-contestant))) "Contestant starts with 13 credits")
      (click-prompt state :contestant "Yes")
      (is (= 16 (:credit (get-contestant))) "Contestant gains 3 credits")
      (take-credits state :challenger)
      (is (empty? (:prompt (get-contestant))) "Not prompted when out of money")))
  (testing "register event when agenda swapped with Turntable"
    ;; Regression test for #3114
    (do-game
      (new-game (default-contestant ["SSL Endorsement" "Breaking News"])
                (default-challenger ["Turntable"]))
      (play-from-hand state :contestant "Breaking News" "New party")
      (play-and-score state "SSL Endorsement")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Turntable")
      (run-on state "Locale 1")
      (run-successful state)
      (click-prompt state :challenger "Steal")
      (click-prompt state :challenger "Yes")
      (click-card state :challenger (find-card "SSL Endorsement" (:scored (get-contestant))))  ;; Swap BN with SSL
      (take-credits state :challenger)
      (is (not-empty (:prompt (get-contestant))) "Contestant prompted to take credits")
      (is (= 6 (:credit (get-contestant))) "Contestant starts with 7 credits")
      (click-prompt state :contestant "Yes")
      (is (= 9 (:credit (get-contestant))) "Contestant gains 3 credits from Turntable'd SSL Endorsement")))
  (testing "don't double register event when agenda is swapped"
    (do-game
      (new-game (default-contestant ["SSL Endorsement" "Breaking News"
                               "Exchange of Information"])
                (default-challenger))
      (play-from-hand state :contestant "SSL Endorsement" "New party")
      (play-and-score state "Breaking News")
      (take-credits state :contestant)
      (run-on state "Locale 1")
      (run-successful state)
      (click-prompt state :challenger "Steal")
      (take-credits state :challenger)
      (is (not-empty (:prompt (get-contestant))) "Contestant prompted to take credits")
      (is (= 6 (:credit (get-contestant))) "Contestant starts with 6 credits")
      (click-prompt state :contestant "Yes")
      (is (= 9 (:credit (get-contestant))) "Contestant gains 3 credits")
      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "Exchange of Information")
      (click-card state :contestant (find-card "SSL Endorsement" (:scored (get-challenger))))
      (click-card state :contestant (find-card "Breaking News" (:scored (get-contestant))))
      (take-credits state :challenger)
      (is (= 9 (:credit (get-contestant))) "Contestant starts with 9 credits")
      (click-prompt state :contestant "No")
      (is (empty? (:prompt (get-contestant))) "Not double prompted for credits")
      (is (= 9 (:credit (get-contestant))) "Contestant doesn't gain 3 credits")
      (take-credits state :challenger)
      (is (= 9 (:credit (get-contestant))) "Contestant starts with 9 credits")
      (click-prompt state :contestant "Yes")
      (is (= 12 (:credit (get-contestant))) "Contestant gains 3 credits")
      (take-credits state :challenger)
      (is (= 12 (:credit (get-contestant))) "Contestant starts with 12 credits")
      (click-prompt state :contestant "Yes")
      (is (= 15 (:credit (get-contestant))) "Contestant gains 3 credits")
      (take-credits state :challenger)
      (is (empty? (:prompt (get-contestant))) "Not prompted when out of money"))))

(deftest standoff
  ;; Standoff
  (testing "Challenger declines first"
    (do-game
      (new-game (default-contestant ["Standoff" "Ice Wall" "News Team"])
                (default-challenger ["Cache"]))
      (starting-hand state :contestant ["Standoff" "Ice Wall"])
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Cache")
      (take-credits state :challenger)
      (play-and-score state "Standoff")
      (starting-hand state :contestant [])
      (is (zero? (-> (get-challenger) :discard count)) "Challenger should have no cards in Heap")
      (click-card state :challenger (get-resource state 0))
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should now have 1 card in Heap")
      (is (zero? (-> (get-contestant) :discard count)) "Contestant should have no cards in Archives")
      (click-card state :contestant (get-character state :hq 0))
      (is (= 1 (-> (get-contestant) :discard count)) "Contestant should now have 1 card in Archives")
      (is (zero? (-> (get-contestant) :hand count)) "Contestant should have no cards in hand")
      (let [credits (:credit (get-contestant))]
        (click-prompt state :challenger "Done")
        (is (= (+ credits 5) (:credit (get-contestant))) "Contestant should gain 5 credits from Challenger declining to discard an placed card")
        (is (= 1 (-> (get-contestant) :hand count)) "Contestant should draw a card from Challenger declining to discard an placed card"))))
  (testing "Contestant declines first"
    (do-game
      (new-game (default-contestant ["Standoff" "Ice Wall" "News Team"])
                (default-challenger ["Cache" "Cache"]))
      (starting-hand state :contestant ["Standoff" "Ice Wall"])
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Cache")
      (play-from-hand state :challenger "Cache")
      (take-credits state :challenger)
      (play-and-score state "Standoff")
      (starting-hand state :contestant [])
      (is (zero? (-> (get-challenger) :discard count)) "Challenger should have no cards in Heap")
      (click-card state :challenger (get-resource state 0))
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should now have 1 card in Heap")
      (is (zero? (-> (get-contestant) :discard count)) "Contestant should have no cards in Archives")
      (click-card state :contestant (get-character state :hq 0))
      (is (= 1 (-> (get-contestant) :discard count)) "Contestant should now have 1 card in Archives")
      (is (zero? (-> (get-contestant) :hand count)) "Contestant should have no cards in hand")
      (click-card state :challenger (get-resource state 0))
      (is (= 2 (-> (get-challenger) :discard count)) "Challenger should now have 2 cards in Heap")
      (let [credits (:credit (get-contestant))]
        (click-prompt state :contestant "Done")
        (is (= credits (:credit (get-contestant))) "Contestant should gain no credits from declining to discard an placed card")
        (is (zero? (-> (get-contestant) :hand count)) "Contestant should draw no cards from declining to discard an placed card")))))

(deftest successful-field-test
  ;; Successful Field Test
  (do-game
    (new-game (default-contestant ["Successful Field Test" (qty "Ice Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant (vec (cons "Successful Field Test" (repeat 10 "Ice Wall"))))
    (is (= 5 (:credit (get-contestant))) "Should start with 5 credits")
    (play-and-score state "Successful Field Test")
    (dotimes [n 10]
      (click-card state :contestant (find-card "Ice Wall" (:hand (get-contestant))))
      (click-prompt state :contestant "HQ"))
    (is (= 5 (:credit (get-contestant))) "Should still have 5 credits")
    (is (some? (get-character state :hq 9)))))

(deftest superior-cyberwalls
  ;; Superior Cyberwalls
  (do-game
    (new-game (default-contestant ["Superior Cyberwalls" "Ice Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [iw (get-character state :hq 0)]
      (core/reveal state :contestant iw)
      (is (= 1 (:current-strength (refresh iw))) "Should start with base strength of 1")
      (is (= 4 (:credit (get-contestant))) "Should have 4 credits after reveal")
      (play-and-score state "Superior Cyberwalls")
      (is (= 2 (:current-strength (refresh iw))) "Should gain 1 strength from 1 to 2")
      (is (= 5 (:credit (get-contestant))) "Should gain 1 credit for revealed barrier"))))

(deftest tgtbt
  ;; TGTBT - Give the Challenger 1 tag when they access
  ;; OHG still not working...
  (do-game
    (new-game (default-contestant [(qty "TGTBT" 2) "Old Hollywood Grid"])
              (default-challenger))
    (play-from-hand state :contestant "TGTBT" "New party")
    (play-from-hand state :contestant "Old Hollywood Grid" "Locale 1")
    (play-from-hand state :contestant "TGTBT" "New party")
    (take-credits state :contestant)
    (let [tg1 (get-content state :party1 0)
          ohg (get-content state :party1 1)]
      (run-on state "Locale 1")
      (core/reveal state :contestant ohg)
      (run-successful state)
      (click-card state :challenger tg1)
      ;; Accesses TGTBT but can't steal
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag from accessing without stealing")
      (click-prompt state :challenger "No action")
      (click-card state :challenger ohg))
    (click-prompt state :challenger "Pay 4 [Credits] to discard") ;; Discards OHG
    (run-empty-locale state "Locale 2")
    ;; Accesses TGTBT and can steal
    (click-prompt state :challenger "Steal")
    (is (= 2 (:tag (get-challenger))) "Challenger took 1 tag from accessing and stealing")))

(deftest the-cleaners
  ;; The Cleaners
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["The Cleaners" "Scorched Earth"])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
      (play-and-score state "The Cleaners")
      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "Scorched Earth")
      (is (zero? (count (:hand (get-challenger)))) "5 damage dealt to Challenger")))
  (testing "No bonus damage when challenger 'suffers' damage, ie Cybernetics"
    (do-game
      (new-game (default-contestant ["The Cleaners"])
                (default-challenger [(qty "Respirocytes" 3)]))
      (play-and-score state "The Cleaners")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Respirocytes")
      (is (= 1 (count (:hand (get-challenger)))) "Only 1 damage dealt to Challenger from Cybernetics"))))

(deftest the-future-is-now
  ;; The Future is Now
  (testing "With at least one card in deck"
    (do-game
      (new-game (default-contestant ["The Future is Now" "Ice Wall"])
                (default-challenger))
      (starting-hand state :contestant ["The Future is Now"])
      (is (= 1 (count (:hand (get-contestant)))))
      (is (= 1 (count (:deck (get-contestant)))))
      (play-and-score state "The Future is Now")
      (click-prompt state :contestant (find-card "Ice Wall" (:deck (get-contestant))))
      (is (= 1 (count (:hand (get-contestant)))))
      (is (zero? (count (:deck (get-contestant)))))))
  (testing "With an empty deck"
    (do-game
      (new-game (default-contestant ["The Future is Now"])
                (default-challenger))
      (is (= 1 (count (:hand (get-contestant)))))
      (is (zero? (count (:deck (get-contestant)))))
      (play-and-score state "The Future is Now")
      (is (empty? (:prompt (get-contestant))) "Ability shouldn't fire if deck is empty")
      (is (zero? (count (:hand (get-contestant)))))
      (is (zero? (count (:deck (get-contestant))))))))

(deftest the-future-perfect
  ;; The Future Perfect
  (do-game
    (new-game (default-contestant [(qty "The Future Perfect" 2)])
              (default-challenger))
    (play-from-hand state :contestant "The Future Perfect" "New party")
    (take-credits state :contestant)
    (testing "No steal on not-equal Psi game"
      (run-empty-locale state "HQ")
      (click-prompt state :contestant "1 [Credits]")
      (click-prompt state :challenger "0 [Credits]")
      ;; Cannot steal prompt
      (click-prompt state :challenger "No action")
      (is (zero? (:agenda-point (get-challenger))) "Challenger did not steal TFP"))
    (testing "Successful steal on equal Psi game"
      (run-empty-locale state "HQ")
      (click-prompt state :contestant "1 [Credits]")
      (click-prompt state :challenger "1 [Credits]")
      (click-prompt state :challenger "Steal")
      (is (= 3 (:agenda-point (get-challenger))) "Challenger stole TFP"))
    (testing "No Psi game and successful steal when placed"
      (run-empty-locale state "Locale 1")
      (click-prompt state :challenger "Steal")
      (is (= 6 (:agenda-point (get-challenger))) "Challenger stole TFP - no Psi game on placed TFP"))))

(deftest underway-renovation
  ;; Underway Renovation
  (do-game
    (new-game (default-contestant ["Underway Renovation" "Shipment from SanSan"])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (starting-hand state :challenger [])
    (play-from-hand state :contestant "Underway Renovation" "New party")
    (let [ur (get-content state :party1 0)]
      (advance state ur)
      (is (last-log-contains? state "Sure Gamble")
          "Underway Renovation discarded card name is in log")
      ; check for #2370
      (is (not (last-log-contains? state "Sure Gamble, Sure Gamble"))
          "Underway Renovation discarded card name is in log")
      (is (= 1 (count (:discard (get-challenger)))) "1 card milled from Challenger Stack")
      (play-from-hand state :contestant "Shipment from SanSan")
      (click-prompt state :contestant "2")
      (click-card state :contestant ur)
      (is (= 3 (get-counters (refresh ur) :advancement)))
      (is (= 1 (count (:discard (get-challenger)))) "No Challenger mills; advancements were placed")
      (advance state ur)
      (is (= 4 (get-counters (refresh ur) :advancement)))
      (is (last-log-contains? state "Sure Gamble, Sure Gamble")
          "Underway Renovation discarded card name is in log")
      (is (= 3 (count (:discard (get-challenger)))) "2 cards milled from Challenger Stack; 4+ advancements"))))

(deftest unorthodox-predictions
  ;; Unorthodox Predictions
  (do-game
    (new-game (default-contestant ["Unorthodox Predictions"])
              (default-challenger))
    (play-and-score state "Unorthodox Predictions")
    (click-prompt state :contestant "Barrier")
    (is (last-log-contains? state "Barrier"))))

(deftest utopia-fragment
  ;; Utopia Fragment
  (do-game
    (new-game (default-contestant ["Utopia Fragment"
                             "Hostile Takeover"])
              (default-challenger))
    (play-and-score state "Utopia Fragment")
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (advance state (get-content state :party2 0))
    (take-credits state :contestant)
    (run-on state :party2)
    (run-successful state)
    (is (= ["Pay 2 [Credits] to steal" "No action"] (:choices (prompt-map :challenger))))
    (click-prompt state :challenger "Pay 2 [Credits] to steal")
    (is (= 1 (:agenda-point (get-challenger))))
    (is (= 3 (:credit (get-challenger))))))

(deftest vanity-project
  ;; Vanity Project
  (do-game
    (new-game (default-contestant ["Vanity Project"])
              (default-challenger))
    (play-and-score state "Vanity Project")
    (is (= 4 (:agenda-point (get-contestant))))))

(deftest veterans-resource
  ;; Veterans Resource
  (testing "Veterans Resource basic test"
    (do-game
      (new-game (default-contestant [(qty "Hostile Takeover" 2) "Veterans Resource"])
                (default-challenger))
      (play-and-score state "Hostile Takeover")
      (play-and-score state "Hostile Takeover")
      (is (= 19 (:credit (get-contestant))) "Should gain 14 credits from 5 to 19")
      (is (= 2 (:bad-publicity (get-contestant))) "Should gain 2 bad publicity")
      (play-and-score state "Veterans Resource")
      (is (zero? (:bad-publicity (get-contestant))) "Should lose 2 bad publicity")))
  (testing "Removes _up to 2_ bad publicity"
    (do-game
      (new-game (default-contestant ["Hostile Takeover" "Veterans Resource"])
                (default-challenger))
      (play-and-score state "Hostile Takeover")
      (is (= 12 (:credit (get-contestant))) "Should gain 7 credits from 5 to 12")
      (is (= 1 (:bad-publicity (get-contestant))) "Should gain 1 bad publicity")
      (play-and-score state "Veterans Resource")
      (is (zero? (:bad-publicity (get-contestant))) "Should lose 1 bad publicity"))))

(deftest viral-weaponization
  ;; Viral Weaponization - at the end of turn scored, do 1 net damage for each card in grip
  (testing "Score on contestant turn"
    (do-game
      (new-game (default-contestant [(qty "Viral Weaponization" 2)])
                (default-challenger [(qty "Sure Gamble" 3)]))
      (starting-hand state :challenger ["Sure Gamble" "Sure Gamble"])
      (play-and-score state "Viral Weaponization")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger doesn't take damage when scored")
      (take-credits state :contestant)
      (is (zero? (count (:hand (get-challenger)))) "Challenger takes damage at end of turn")
      (core/click-draw state :challenger 1)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 1 (count (:hand (get-challenger)))) "Challenger doesn't take damage in future turns")
      (play-from-hand state :challenger "Sure Gamble")
      (take-credits state :challenger)
      (is (zero? (count (:hand (get-challenger)))) "Challenger's hand is empty")
      (play-and-score state "Viral Weaponization")
      (take-credits state :contestant)
      (is (zero? (count (:hand (get-challenger)))) "Challenger's hand is empty")))
  (testing "Score on challengers turn"
    (do-game
      (new-game (default-contestant ["Viral Weaponization" "Plan B"])
                (default-challenger [(qty "Sure Gamble" 3)]))
      (starting-hand state :challenger ["Sure Gamble" "Sure Gamble"])
      (play-from-hand state :contestant "Plan B" "New party")
      (core/add-prop state :contestant (get-content state :party1 0) :advance-counter 4)
      (take-credits state :contestant)
      (run-on state "Locale 1")
      (run-successful state)
      (click-prompt state :contestant "Yes")
      (click-card state :contestant (find-card "Viral Weaponization" (:hand (get-contestant))))
      (is (= ["Pay 1 [Credits] to discard" "No action"] (:choices (prompt-map :challenger))))
      (click-prompt state :challenger "No action")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger doesn't take damage when scored")
      (take-credits state :challenger)
      (is (zero? (count (:hand (get-challenger)))) "Challenger takes damage at end of turn"))))

(deftest voting-machine-initiative
  ;; Voting Machine Initiative
  (testing "Voting Machine Initiative"
    (do-game
      (new-game (default-contestant ["Voting Machine Initiative"])
                (default-challenger))
      (letfn [(vmi-test [vmi choice counter]
                (let [diff (if (= "Yes" choice) 1 0)]
                  (is (= counter (get-counters (refresh vmi) :agenda)))
                  (is (= 4 (:click (get-challenger))))
                  (click-prompt state :contestant choice)
                  (is (= (- 4 diff) (:click (get-challenger))))
                  (is (= (- counter diff) (get-counters (refresh vmi) :agenda)))
                  (take-credits state :challenger)
                  (take-credits state :contestant)))]
        (play-and-score state "Voting Machine Initiative")
        (take-credits state :contestant)
        (let [vmi-scored (get-scored state :contestant 0)]
          (vmi-test vmi-scored "Yes" 3)
          (vmi-test vmi-scored "No" 2)
          (vmi-test vmi-scored "Yes" 2)
          (vmi-test vmi-scored "Yes" 1)
          (is (empty (:prompt (get-contestant))) "No prompt as there are no agenda counters left"))))))

(deftest vulcan-coverup
  ;; Vulcan Coverup
  (do-game
    (new-game (default-contestant [(qty "Vulcan Coverup" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Vulcan Coverup" "New party")
    (take-credits state :contestant)
    (run-empty-locale state :party1)
    (click-prompt state :challenger "Steal")
    (is (= 1 (:bad-publicity (get-contestant))) "Took 1 bad pub from stolen agenda")
    (take-credits state :challenger)
    (play-and-score state "Vulcan Coverup")
    (is (= 2 (count (:discard (get-challenger)))) "Did 2 meat damage upon scoring")))

(deftest water-monopoly
  ;; Water Monopoly
  (do-game
    (new-game (default-contestant ["Water Monopoly"])
              (default-challenger ["Fan Site" "Levy Advanced Research Lab"]))
    (play-and-score state "Water Monopoly")
    (take-credits state :contestant)
    (is (= 5 (:credit (get-challenger))) "Challenger should start with 5 credits")
    (play-from-hand state :challenger "Fan Site")
    (is (= 5 (:credit (get-challenger))) "Shouldn't lose any credits")
    (play-from-hand state :challenger "Levy Advanced Research Lab")
    (is (zero? (:credit (get-challenger))) "Should cost an extra credit to play")))
