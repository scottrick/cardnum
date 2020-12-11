(ns game-test.cards.agendas
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "agendas"))

(deftest accelerated-beta-test
  ;; Accelerated Beta Test
  (do-game
    (new-game (default-contestant ["Accelerated Beta Test" "Enigma" (qty "Hedge Fund" 2)])
              (default-challenger))
    ;; Set up
    (starting-hand state :contestant ["Accelerated Beta Test"])
    (play-and-score state "Accelerated Beta Test")
    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (find-card "Enigma" (get-in @state [:contestant :play-area])))
    (prompt-choice :contestant "HQ")
    (is (some? (get-character state :hq 0)))
    (is (= 2 (count (:discard (get-contestant)))))
    (core/move state :contestant (find-card "Accelerated Beta Test" (:scored (get-contestant))) :hand)
    (core/move state :contestant (find-card "Hedge Fund" (:discard (get-contestant))) :deck)
    (core/move state :contestant (find-card "Hedge Fund" (:discard (get-contestant))) :deck)
    (play-and-score state "Accelerated Beta Test")
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant "I have no regrets")
    (is (= 2 (count (:discard (get-contestant)))))))

(deftest armed-intimidation
  ;; Armed Intimidation
  (do-game
    (new-game (default-contestant [(qty "Armed Intimidation" 2)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 2)]))
    (play-and-score state "Armed Intimidation")
    (prompt-choice :challenger "Take 2 tags")
    (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags from Armed Intimidation tag choice")
    (play-and-score state "Armed Intimidation")
    (is (= 5 (count (:hand (get-challenger)))) "Challenger has 5 cards before Armed Intimidation meat damage")
    (prompt-choice :challenger "Suffer 5 meat damage")
    (is (zero? (count (:hand (get-challenger)))) "Challenger has 0 cards after Armed Intimidation meat damage")))

(deftest astroscript-pilot-resource
  ;; AstroScript token placement
  (do-game
    (new-game (default-contestant [(qty "AstroScript Pilot Resource" 3) (qty "Character Wall" 2)])
              (default-challenger))
    (core/gain state :contestant :click 3)
    (letfn [(try-place [from to]
              (card-ability state :contestant (refresh from) 0)
              (prompt-select :contestant (refresh to)))
            (should-not-place [from to msg]
              (try-place from to)
              (prompt-choice :contestant "Done")
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
            hand-astro (find-card "AstroScript Pilot Resource" (:hand get-contestant))]
        (should-not-place scored-astro hand-astro " in hand")
        (should-place scored-astro placed-astro " that is placed")
        (advance state placed-astro 2)
        (core/score state :contestant {:card (refresh placed-astro)}))
      (play-from-hand state :contestant "Character Wall" "HQ")
      (let [no-token-astro (get-scored state :contestant 0)
            token-astro (get-scored state :contestant 1)
            hand-character-wall (find-card "Character Wall" (:hand get-contestant))
            placed-character-wall (get-character state :hq 0)]
        (should-not-place token-astro no-token-astro " that is scored")
        (should-not-place token-astro hand-character-wall " in hand")
        (should-place token-astro placed-character-wall " that is placed")))))

(deftest bacterial-resourceming
  ;; Bacterial Resourceming
  (testing "Scoring should not cause a run to exist for challenger."
    (do-game
      (new-game (default-contestant ["Bacterial Resourceming" "Hedge Fund"])
                (default-challenger))
      (starting-hand state :contestant ["Bacterial Resourceming"])
      (play-and-score state "Bacterial Resourceming")
      (prompt-choice :contestant "Yes")
      (prompt-choice :contestant "Done")
      (prompt-choice :contestant "Done")
      (prompt-card :contestant (first (:deck (get-contestant))))
      (prompt-choice :contestant "Done")
      (is (empty (:prompt (get-contestant))) "Bacterial Resourceming prompts finished")
      (is (not (:run @state)) "No run is active")))
  (testing "Removing all cards from R&D should not freeze for challenger, nor give an extra access."
    (do-game
      (new-game (default-contestant [(qty "Bacterial Resourceming" 8)])
                (default-challenger)
                {:start-as :challenger})
      (starting-hand state :contestant [])
      (run-empty-locale state :rd)
      (prompt-choice :challenger "Steal")
      (prompt-choice :contestant "Yes")
      ;; Move all 7 cards to discard
      (doseq [_ (range 7)
              ;; Get the first card listed in the prompt choice
              ;; TODO make this function
              :let [card (-> @state
                             (get-in [:contestant :prompt])
                             first
                             (get-in [:choices 0]))]]
        (prompt-card :contestant card))
      (prompt-choice :contestant "Done")                          ; Finished with discarding
      (prompt-choice :contestant "Done")                          ; Finished with move-to-hq (no cards to move)
      ;; Run and prompts should be over now
      (is (empty (:prompt (get-contestant))) "Bacterial Resourceming prompts finished")
      (is (empty (:prompt (get-challenger))) "Bacterial Resourceming prompts finished")
      (is (not (:run @state))))))

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
      (prompt-choice :contestant "Yes")
      (prompt-select :contestant (refresh ht-scored))
      (is (= 19 (:credit (get-contestant))) "Should gain 7 credits from 12 to 19")
      (is (= 2 (:bad-publicity (get-contestant))) "Should gain 1 bad publicity"))))

(deftest brain-rewiring
  ;; Brain Rewiring
  (do-game
    (new-game (default-contestant ["Brain Rewiring"])
              (default-challenger))
    (starting-hand state :challenger ["Sure Gamble" "Sure Gamble"])
    (play-and-score state "Brain Rewiring")
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant 2)
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
    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "Strongbox" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "Eli 1.0" (:discard (get-contestant))))))

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

(deftest firmware-updates
  ;; Firmware Updates
  (do-game
    (new-game (default-contestant ["Firmware Updates"
                             "Character Wall"])
              (default-challenger))
    (play-and-score state "Firmware Updates")
    (play-from-hand state :contestant "Character Wall" "HQ")
    (let [fu (get-scored state :contestant 0)
          iw (get-character state :hq 0)]
      (is (= 3 (get-counters (refresh fu) :agenda)) "Firmware Updates should start with 3 agenda counters")
      (core/reveal state :contestant iw)
      (is (zero? (get-counters (refresh iw) :advancement)) "Character Wall should start with 0 advancement tokens")
      (card-ability state :contestant fu 0)
      (prompt-select :contestant (refresh iw))
      (is (= 2 (get-counters (refresh fu) :agenda)) "Firmware Updates should now have 2 agenda counters")
      (is (= 1 (get-counters (refresh iw) :advancement)) "Character Wall should have 1 advancement token"))))

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
        (prompt-choice :contestant 0)
        (prompt-choice :challenger 0)
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
        (prompt-select :contestant bt2)
        (is (zero? (get-counters (refresh bt2) :agenda))
            "No agenda counters on placed Braintrust; not a valid target")
        (prompt-select :contestant btscored)
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
    (new-game (default-contestant ["Glenn Station" "Character Wall"])
              (default-challenger))
    (play-and-score state "Glenn Station")
    (let [gs-scored (get-scored state :contestant 0)]
      (card-ability state :contestant gs-scored 0)
      (prompt-card :contestant (find-card "Character Wall" (:hand (get-contestant))))
      (is (= 1 (count (:hosted (refresh gs-scored)))))
      (card-ability state :contestant gs-scored 1)
      (prompt-card :contestant (find-card "Character Wall" (:hosted (refresh gs-scored))))
      (is (zero? (count (:hosted (refresh gs-scored))))))))

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
            (let [cards ["Character Wall" "Fire Wall" "Orion"]]
              (do-game
                (new-game (default-contestant ["Graft" "Character Wall"
                                         "Fire Wall" "Orion"])
                          (default-challenger))
                (starting-hand state :contestant ["Graft"])
                (play-and-score state "Graft")
                (dotimes [current-pick number-of-picks]
                  (prompt-card :contestant (find-card (nth cards current-pick) (:deck (get-contestant)))))
                (is (= number-of-picks (count (:hand (get-contestant)))))
                (is (= deck-size (count (:deck (get-contestant))))))))]
    (doall (map graft-test
                [[0 3]
                 [1 2]
                 [2 1]
                 [3 0]]))))

(deftest hollywood-renovation
  ;; Hollywood Renovation
  (do-game
    (new-game (default-contestant ["Hollywood Renovation" "Character Wall"])
              (default-challenger))
    (core/gain state :contestant :click 10 :credit 10)
    (play-from-hand state :contestant "Character Wall" "HQ")
    (play-from-hand state :contestant "Hollywood Renovation" "New party")
    (let [hr (get-content state :party1 0)
          iw (get-character state :hq 0)]
      (is (zero? (get-counters (refresh hr) :advancement)) "Hollywood Renovation should start with 0 advancement tokens")
      (is (zero? (get-counters (refresh iw) :advancement)) "Character Wall should start with 0 advancement tokens")
      (dotimes [n 5]
        (advance state (refresh hr))
        (prompt-select :contestant (refresh iw)))
      (is (= 5 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 5 advancement tokens")
      (is (= 5 (get-counters (refresh iw) :advancement)) "Character Wall should gain 5 advancement tokens")
      (advance state (refresh hr))
      (prompt-select :contestant (refresh iw))
      (is (= 6 (get-counters (refresh hr) :advancement)) "Hollywood Renovation should gain 1 from 5 to 6 advancement tokens")
      (is (= 7 (get-counters (refresh iw) :advancement)) "Character Wall should gain 2 from 5 to 7 advancement tokens"))))

(deftest hostile-takeover
  ;; Hostile Takeover
  (do-game
    (new-game (default-contestant ["Hostile Takeover"])
              (default-challenger))
    (play-and-score state "Hostile Takeover")
    (is (= 12 (:credit (get-contestant))) "Gain 7 credits")
    (is (= 1 (:bad-publicity (get-contestant))) "Take 1 bad publicity")))

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
                  (prompt-choice :contestant answer)
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
      (prompt-select :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
      (prompt-choice :contestant "New party")
      (is (some? (get-content state :party2 0))))
    (testing "Region & HQ"
      (play-and-score state "Lcharacternse Acquisition")
      (prompt-select :contestant (find-card "Strongbox" (:hand (get-contestant))))
      (prompt-choice :contestant "New party")
      (is (some? (get-content state :party4 0))))
    (testing "Site & Archives"
      (play-and-score state "Lcharacternse Acquisition")
      (prompt-select :contestant (find-card "Eve Campaign" (:discard (get-contestant))))
      (prompt-choice :contestant "New party")
      (is (some? (get-content state :party6 0))))
    (testing "Region & Archives"
      (play-and-score state "Lcharacternse Acquisition")
      (prompt-select :contestant (find-card "Contestantorate Troubleshooter" (:discard (get-contestant))))
      (prompt-choice :contestant "New party")
      (is (some? (get-content state :party8 0))))))

(deftest mandatory-seed-replacement
  ;; Mandatory Seed Replacement
  (do-game
    (new-game (default-contestant ["Mandatory Seed Replacement"
                             "Character Wall" "Fire Wall"
                             "Kakugo" "Chum"
                             "RSVP" "Sensei"])
              (default-challenger))
    (core/click-draw state :contestant 2)
    (core/gain state :contestant :click 10 :credit 10)
    (play-from-hand state :contestant "Character Wall" "Archives")
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
      (prompt-select :contestant (refresh iw))
      (prompt-select :contestant (refresh fw))
      (prompt-select :contestant (refresh kk))
      (prompt-select :contestant (refresh ch))
      (prompt-select :contestant (refresh rs))
      (prompt-select :contestant (refresh sn)))))

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

(deftest net-quarantine
  ;; Net Quarantine
  (do-game
    (new-game (default-contestant ["Net Quarantine"])
              (default-challenger))
    (core/gain state :challenger :link 1)
    (core/gain state :contestant :click 3)
    (play-and-score state "Net Quarantine")
    (is (= 5 (:credit (get-contestant))) "Contestant has 5 credits")
    (is (= 1 (:link (get-challenger))) "Challenger has 1 link")
    (core/init-trace state :contestant {:title "/trace command" :side :contestant} {:base 1})
    (prompt-choice :contestant 0)
    (is (zero? (:link (get-challenger))) "Challenger has 0 link")
    (prompt-choice :challenger 3)
    (is (= 1 (:link (get-challenger))) "Challenger has 1 link again")
    (is (= 6 (:credit (get-contestant))) "Contestant gained a credit from NQ")
    ; second trace of turn - no link reduction
    (core/init-trace state :contestant {:title "/trace command" :side :contestant} {:base 1})
    (prompt-choice :contestant 0)
    (is (= 1 (:link (get-challenger))) "Challenger has 1 link")
    (prompt-choice :challenger 2)
    (is (= 7 (:credit (get-contestant))) "Contestant gained a credit from NQ")))

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
        (prompt-choice :contestant "Yes")
        (prompt-select :contestant (find-card "Commercial Bankers Group" (:hand (get-contestant)))))
      (is (= 4 (get-counters (refresh nc) :advancement)))
      (is (not= :this-turn (:revealed (get-content state :party5 0))))
      (let [credits (:credit (get-contestant))]
        (advance state (refresh nc))
        (prompt-choice :contestant "Yes")
        (prompt-select :contestant (find-card "Commercial Bankers Group" (:hand (get-contestant))))
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
    (prompt-choice :contestant "No")
    (is (zero? (:brain-damage (get-challenger))) "Challenger should stay at 0 brain damage")
    (play-and-score state "NEXT Wave 2")
    (prompt-choice :contestant "Yes")
    (is (= 1 (:brain-damage (get-challenger))) "Challenger should gain 1 brain damage")))

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
      (prompt-choice :contestant "2")
      (prompt-select :contestant oak)
      (is (= 3 (get-counters (refresh oak) :advancement)))
      (is (= 6 (:credit (get-contestant))) "No credits gained due to advancements being placed")
      (advance state oak)
      (is (= 7 (:credit (get-contestant))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (advance state oak)
      (is (= 5 (get-counters (refresh oak) :advancement)))
      (is (= 9 (:credit (get-contestant)))
          "Spent 1 credit to advance, gained 3 credits from Oaktown"))))

(deftest posted-bounty
  ;; Posted Bounty
  (testing "Forfeiting takes 1 bad publicity"
    (do-game
      (new-game (default-contestant ["Posted Bounty"])
                (default-challenger))
      (play-and-score state "Posted Bounty")
      (prompt-choice :contestant "Yes")
      (is (zero? (:agenda-point (get-contestant))) "Forfeiting Posted Bounty nullifies agenda points")
      (is (= 1 (:bad-publicity (get-contestant))) "Forfeiting takes 1 bad publicity")
      (is (= 1 (:tag (get-challenger))) "Challenger receives 1 tag forfeiting Posted Bounty")))
  (testing "Choosing not to forfeit scores normally"
    (do-game
      (new-game (default-contestant ["Posted Bounty"])
                (default-challenger))
      (play-and-score state "Posted Bounty")
      (prompt-choice :contestant "No")
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
      (prompt-select :contestant arc)
      (is (:revealed (refresh arc))))))

(deftest profiteering
  ;; Profiteering
  (do-game
    (new-game (default-contestant ["Profiteering"])
              (default-challenger))
    (play-and-score state "Profiteering")
    (prompt-choice :contestant "3")
    (is (= 1 (:agenda-point (get-contestant))))
    (is (= 3 (:bad-publicity (get-contestant))) "Took 3 bad publicity")
    (is (= 20 (:credit (get-contestant))) "Gained 15 credits")))

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
        (prompt-card :contestant (find-card "Beanstalk Royalties" (:deck (get-contestant))))
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
        (prompt-card :contestant (find-card "Beanstalk Royalties" (:deck (get-contestant))))
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
        (prompt-card :contestant (find-card "Hedge Fund" (:deck (get-contestant))))
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
    (new-game (default-contestant [(qty "Project Kusanagi" 2) "Character Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Character Wall" "HQ")
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
      (prompt-select :contestant (find-card "Hedge Fund" (:discard (get-contestant))))
      (is (zero? (get-counters (refresh vit-scored) :agenda)) "Vitruvius should have 0 agenda counters")
      (is (= 1 (count (:hand (get-contestant)))) "Contestant should have 1 cards in hand"))))

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
      (is (prompt-is-type? :challenger :waiting) "Challenger has wait prompt")
      (is (= 1 (count (get-in @state [:contestant :hand]))))
      (is (= 1 (count (get-in @state [:challenger :hand]))))
      (prompt-card :contestant (find-card "Sweeps Week" (:hand (get-contestant)))) ; put Sweeps Week at bottom of R&D
      (prompt-choice :contestant "Done") ; finished selecting cards
      (prompt-choice :contestant "Done") ; contestant prompt for Done/Start Over
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
      (is (prompt-is-type? :challenger :waiting) "Challenger has wait prompt")
      (is (= 2 (count (:hand (get-contestant)))))
      (is (= 1 (count (:hand (get-challenger)))))
      (prompt-card :contestant (find-card "Sweeps Week" (:hand (get-contestant))))
      (prompt-card :contestant (find-card "Hedge Fund" (:hand (get-contestant)))) ; this is the bottom card of R&D
      (prompt-choice :contestant "Done") ; finished selecting cards
      (prompt-choice :contestant "Done") ; contestant prompt for Done/Start Over
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
     (prompt-choice :contestant "Yes")
     (prompt-choice :contestant (find-card "Chiyashi" (:deck (get-contestant))))
     (prompt-choice :contestant "New party")
     (is (core/revealed? (get-character state :party2 0)) "Chiyashi was placed revealed")
     (is (= N (:credit (get-contestant))) "Revealing Chiyashi was free"))
   (play-and-score state "Party Enforcement")
   (let [N (:credit (get-contestant))]
     (prompt-choice :contestant "Yes")
     (prompt-card :contestant (find-card "Archer" (:deck (get-contestant))))
     (prompt-choice :contestant "Locale 2")
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
      (prompt-select :contestant (get-content state :party1 0))
      (is (= 2 (count (:scored (get-contestant)))) "2 copies of Research Grant scored")))
  (testing "vs Leela"
    ;; Issue #3069
    (do-game
      (new-game (default-contestant [(qty "Research Grant" 2) (qty "Character Wall" 2)])
                (make-deck "Leela Patel: Trained Pragmatist" ["Sure Gamble"]))
      (core/gain state :contestant :click 1)
      (play-from-hand state :contestant "Character Wall" "HQ")
      (play-from-hand state :contestant "Character Wall" "R&D")
      (play-from-hand state :contestant "Research Grant" "New party")
      (play-and-score state "Research Grant")
      (prompt-select :contestant (get-content state :party1 0))
      (is (= 2 (count (:scored (get-contestant)))) "2 copies of Research Grant scored")
      (prompt-select :challenger (get-character state :hq 0))
      (prompt-select :challenger (get-character state :rd 0))
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
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 1 (:tag (get-challenger))) "Challenger should gain a tag from Restructured Datapool ability"))))

(deftest self-destruct-chips
  ;; Self-Destruct Chips
  (do-game
    (new-game (default-contestant ["Self-Destruct Chips"])
              (default-challenger))
    (is (= 5 (get-hand-size :challenger)) "Challenger's hand size starts at 5")
    (play-and-score state "Self-Destruct Chips")
    (is (= 4 (get-hand-size :challenger)) "By scoring Self-Destruct Chips, Challenger's hand size is reduced by 1")))

(deftest show-of-force
  ;; Show of Force
  (do-game
    (new-game (default-contestant ["Show of Force"])
              (default-challenger))
    (is (= 3 (count (:hand (get-challenger)))) "Challenger should start with 3 cards in hand")
    (play-and-score state "Show of Force")
    (is (= 1 (count (:hand (get-challenger)))) "Challenger should have 1 card in hand")
    (is (= 2 (count (:discard (get-challenger)))) "Challenger should have discarded 2 cards")))

(deftest successful-field-test
  ;; Successful Field Test
  (do-game
    (new-game (default-contestant ["Successful Field Test" (qty "Character Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant (vec (cons "Successful Field Test" (repeat 10 "Character Wall"))))
    (is (= 5 (:credit (get-contestant))) "Should start with 5 credits")
    (play-and-score state "Successful Field Test")
    (dotimes [n 10]
      (prompt-select :contestant (find-card "Character Wall" (:hand (get-contestant))))
      (prompt-choice :contestant "HQ"))
    (is (= 5 (:credit (get-contestant))) "Should still have 5 credits")
    (is (some? (get-character state :hq 9)))))

(deftest superior-cyberwalls
  ;; Superior Cyberwalls
  (do-game
    (new-game (default-contestant ["Superior Cyberwalls" "Character Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Character Wall" "HQ")
    (let [iw (get-character state :hq 0)]
      (core/reveal state :contestant iw)
      (is (= 1 (:current-strength (refresh iw))) "Should start with base strength of 1")
      (is (= 4 (:credit (get-contestant))) "Should have 4 credits after reveal")
      (play-and-score state "Superior Cyberwalls")
      (is (= 2 (:current-strength (refresh iw))) "Should gain 1 strength from 1 to 2")
      (is (= 5 (:credit (get-contestant))) "Should gain 1 credit for revealed barrier"))))

(deftest the-future-is-now
  ;; The Future is Now
  (testing "With at least one card in deck"
    (do-game
      (new-game (default-contestant ["The Future is Now" "Character Wall"])
                (default-challenger))
      (starting-hand state :contestant ["The Future is Now"])
      (is (= 1 (count (:hand (get-contestant)))))
      (is (= 1 (count (:deck (get-contestant)))))
      (play-and-score state "The Future is Now")
      (prompt-card :contestant (find-card "Character Wall" (:deck (get-contestant))))
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
      (prompt-choice :contestant "2")
      (prompt-select :contestant ur)
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
    (prompt-choice :contestant "Barrier")
    (is (last-log-contains? state "Barrier"))))

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
