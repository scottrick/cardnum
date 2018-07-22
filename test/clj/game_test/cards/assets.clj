(ns game-test.cards.hardware
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "sites"))

(deftest adonis-campaign
  ;; Adonis Campaign
  (do-game
    (new-game (default-contestant ["Adonis Campaign"])
              (default-challenger))
    (play-from-hand state :contestant "Adonis Campaign" "New remote")
    (let [ac (get-content state :remote1 0)]
      (core/rez state :contestant ac)
      (is (= 1 (:credit (get-contestant))))
      (is (= 12 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
      (take-credits state :contestant)
      (let [credits (:credit (get-contestant))
            counters (get-counters (refresh ac) :credit)]
        (take-credits state :challenger)
        (is (= (:credit (get-contestant)) (+ credits 3)) "Gain 3 from Adonis")
        (is (= (get-counters (refresh ac) :credit) (- counters 3)) "9 counter remaining on Adonis")))))

(deftest advanced-assembly-lines
  ;; Advanced Assembly Lines
  (do-game
    (new-game (default-contestant ["Advanced Assembly Lines"
                             "PAD Campaign"])
              (default-challenger))
    (play-from-hand state :contestant "Advanced Assembly Lines" "New remote")
    (let [aal (get-content state :remote1 0)
          credits (:credit (get-contestant))
          hq (count (:hand (get-contestant)))]
      (core/rez state :contestant aal)
      (is (= (+ credits 2) (:credit (get-contestant))) "Spend 1 gain 3")
      (card-ability state :contestant aal 0)
      (prompt-select :contestant (find-card "PAD Campaign" (:hand (get-contestant))))
      (prompt-choice :contestant "New remote")
      (is (= (- hq 1) (count (:hand (get-contestant)))) "Installed 1 card, hq is empty"))))

(deftest aggressive-secretary
  ;; Aggressive Secretary
  (do-game
    (new-game
      (default-contestant ["Aggressive Secretary"])
      (default-challenger [(qty "Cache" 3)]))
    (play-from-hand state :contestant "Aggressive Secretary" "New remote")
    (let [as (get-content state :remote1 0)]
      ;; Single advance AggSec
      (core/advance state :contestant {:card (refresh as)})
      (take-credits state :contestant)
      ;; Run on AggSec with 3 resources
      (play-from-hand state :challenger "Cache")
      (play-from-hand state :challenger "Cache")
      (play-from-hand state :challenger "Cache")
      (run-empty-server state "Server 1")
      (prompt-choice :contestant "Yes")
      (is (= 3 (:credit (get-contestant))))
      ;; Contestant can discard one resource
      (prompt-select :contestant (get-resource state 1))
      ;; There should be two Caches left
      (is (= 3 (:credit (get-contestant))))
      (is (= 2 (count (get-resource state)))))))

(deftest alexa-belsky
  ;; Alexa Belsky
  (do-game
    (new-game
      (default-contestant ["Alexa Belsky" "Hedge Fund" "Breaking News"
                     "Gutenberg" "Product Placement" "Jackson Howard"])
      (default-challenger))
    (play-from-hand state :contestant "Alexa Belsky" "New remote")
    (let [alexa (get-content state :remote1 0)]
      (core/rez state :contestant alexa)
      (card-ability state :contestant alexa 0)
      (is (= 1 (count (:discard (get-contestant)))) "Alexa Belsky discarded")
      (is (= 5 (count (:hand (get-contestant)))))
      (is (zero? (count (:deck (get-contestant)))))
      (prompt-choice :challenger 5) ;Challenger chooses to pay 5 credits so 2 cards are prevented from being shuffled
      (is (= 2 (count (:hand (get-contestant)))))
      (is (= 3 (count (:deck (get-contestant)))))
      (is (zero? (:credit (get-challenger)))))))

(deftest alix-t4lb07
  ;; Alix T4LB07
  (do-game
    (new-game
      (default-contestant ["Alix T4LB07" (qty "PAD Campaign" 3)])
      (default-challenger))
    (play-from-hand state :contestant "Alix T4LB07" "New remote")
    (let [alix (get-content state :remote1 0)]
      (core/rez state :contestant alix)
      (play-from-hand state :contestant "PAD Campaign" "New remote")
      (play-from-hand state :contestant "PAD Campaign" "New remote")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 2 (get-counters (refresh alix) :power)) "Two counters on Alix")
      (is (= 4 (:credit (get-contestant))))
      (card-ability state :contestant alix 0)
      (is (= 8 (:credit (get-contestant))) "Gain 4 credits from Alix"))))

(deftest allele-repression
  ;; Allele Repression
  (do-game
    (new-game (default-contestant ["Allele Repression"])
              (default-challenger))
    (play-from-hand state :contestant "Allele Repression" "New remote")
    (let [ar (get-content state :remote1 0)]
      (core/advance state :contestant (refresh ar))
      (core/advance state :contestant (refresh ar))
      (card-ability state :contestant ar 0)
      (is (= 1 (count (:discard (get-contestant)))) "Allele Repression is discarded"))))

(deftest amani-senai
  ;; Amani Senai - trace on score/steal to bounce, with base strength = advancement req of the agenda
  (do-game
    (new-game (default-contestant ["Amani Senai"
                             (qty "Medical Breakthrough" 2)])
              (default-challenger ["Analog Dreamers"]))
    (play-from-hand state :contestant "Amani Senai" "New remote")
    (play-from-hand state :contestant "Medical Breakthrough" "New remote")
    (play-from-hand state :contestant "Medical Breakthrough" "New remote")
    (take-credits state :contestant)
    (let [senai (get-content state :remote1 0)
          breakthrough (get-content state :remote3 0)]
      (core/rez state :contestant senai)
      (play-from-hand state :challenger "Analog Dreamers")
      (run-empty-server state "Server 2")
      (prompt-choice :challenger "Steal")
      (is (zero? (count (get-content state :remote2))) "Agenda was stolen")
      (prompt-choice :contestant "Medical Breakthrough") ; simult. effect resolution
      (prompt-choice :contestant "Yes")
      (prompt-choice :contestant 0)
      (is (= 3 (-> (get-challenger) :prompt first :strength)) "Trace base strength is 3 after stealing first Breakthrough")
      (prompt-choice :challenger 0)
      (let [grip (-> (get-challenger) :hand count)]
        (is (= 1 (count (get-resource state))) "There is an Analog Dreamers installed")
        (prompt-select :contestant (get-resource state 0))
        (is (zero? (count (get-resource state))) "Analog Dreamers was uninstalled")
        (is (= (+ grip 1) (-> (get-challenger) :hand count)) "Analog Dreamers was added to hand"))
      (take-credits state :challenger)
      (score-agenda state :contestant breakthrough)
      ;; (prompt-choice :contestant "Medical Breakthrough") ; there is no simult. effect resolution on score for some reason
      (prompt-choice :contestant "Yes") ; contestant should get to trigger trace even when no challenger cards are installed
      (prompt-choice :contestant 0)
      (is (= 2 (-> (get-challenger) :prompt first :strength)) "Trace base strength is 2 after scoring second Breakthrough"))))

(deftest anson-rose
  ;; Anson Rose
  (do-game
    (new-game
      (default-contestant ["Anson Rose" "Ice Wall"])
      (default-challenger))
    (play-from-hand state :contestant "Anson Rose" "New remote")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [ar (get-content state :remote1 0)
          iw (get-character state :hq 0)]
      (core/rez state :contestant (refresh ar))
      (is (zero? (get-counters (refresh ar) :advancement)) "Anson Rose should start with 0 advancement counters")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should start with 0 advancement counters")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/end-phase-12 state :contestant nil)
      (is (= 1 (get-counters (refresh ar) :advancement)) "Anson Rose should gain 1 advancement counter at start of turn")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should still have 0 counters so far")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/end-phase-12 state :contestant nil)
      (is (= 2 (get-counters (refresh ar) :advancement)) "Anson Rose should gain 1 advancement counter at start of turn")
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall should still have 0 counters so far")
      (core/rez state :contestant (refresh iw))
      (prompt-choice :contestant "Yes")
      (prompt-choice :contestant 2)
      (is (zero? (get-counters (refresh ar) :advancement)) "Anson Rose should lose all advancement counters")
      (is (= 2 (get-counters (refresh iw) :advancement)) "Ice Wall should gain 2 advancement counter"))))

(deftest aryabhata-tech
  ;; Aryabhata Tech
  (do-game
    (new-game
      (default-contestant ["Aryabhata Tech"
                     "Hunter"])
      (default-challenger))
    (play-from-hand state :contestant "Aryabhata Tech" "New remote")
    (play-from-hand state :contestant "Hunter" "HQ")
    (let [at (get-content state :remote1 0)
          h (get-character state :hq 0)]
      (core/rez state :contestant (refresh at))
      (core/rez state :contestant (refresh h))
      (take-credits state :contestant)
      (run-on state :hq)
      (let [c-credits (:credit (get-contestant))
            r-credits (:credit (get-challenger))]
        (card-subroutine state :contestant h 0)
        (prompt-choice :contestant 0)
        (prompt-choice :challenger 0)
        (is (= 1 (- (:credit (get-contestant)) c-credits)))
        (is (= -1 (- (:credit (get-challenger)) r-credits)))))))

(deftest bio-ethics-association
  ;; Bio-Ethics Association
  (testing "Basic test"
    (do-game
      (new-game
        (default-contestant ["Bio-Ethics Association"])
        (default-challenger))
      (play-from-hand state :contestant "Bio-Ethics Association" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 1 (count (:discard (get-challenger)))))))
  (testing "should be able to prevent damage from multiple copies"
    (do-game
      (new-game
        (default-contestant [(qty "Bio-Ethics Association" 2)])
        (default-challenger ["Feedback Filter" (qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Bio-Ethics Association" "New remote")
      (play-from-hand state :contestant "Bio-Ethics Association" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (core/rez state :contestant (get-content state :remote2 0))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Feedback Filter")
      (take-credits state :challenger)
      (let [filter (get-hazard state 0)]
        (is (= 1 (count (:prompt (get-challenger)))) "Challenger has a single damage prevention prompt")
        (card-ability state :challenger filter 0)
        (prompt-choice :challenger "Done")
        (is (zero? (count (:discard (get-challenger)))) "Challenger prevented damage")
        (is (= 1 (count (:prompt (get-challenger)))) "Challenger has a next damage prevention prompt")
        (prompt-choice :challenger "Done")
        (is (= 1 (count (:discard (get-challenger)))) "Challenger took 1 net damage")))))

(deftest bioroid-work-crew
  ;; Bioroid Work Crew
  (letfn [(bwc-test [card]
            (do-game
              (new-game
                (default-contestant ["Bioroid Work Crew" card])
                (default-challenger))
              (play-from-hand state :contestant "Bioroid Work Crew" "New remote")
              (let [bwc (get-content state :remote1 0)]
                (core/rez state :contestant bwc)
                (card-ability state :contestant bwc 0)
                (prompt-select :contestant (find-card card (:hand (get-contestant))))
                (prompt-choice :contestant "New remote")
                (is (zero? (count (:hand (get-contestant)))))
                (is (= 1 (count (:discard (get-contestant)))) "Card should be discarded now"))))]
    (doall (map bwc-test
                ["Hostile Takeover"
                 "Dedicated Response Team"
                 "Builder"
                 "Research Station"]))))

(deftest blacklist
  ;; Blacklist
  (testing "#2426.  Need to allow steal."
    (do-game
      (new-game (default-contestant [(qty "Fetal AI" 3) "Blacklist"])
                (default-challenger))
      (discard-from-hand state :contestant "Fetal AI")
      (play-from-hand state :contestant "Blacklist" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (is (= 1 (count (:discard (get-contestant)))))
      (take-credits state :contestant)
      (run-empty-server state :archives)
      (prompt-choice-partial :challenger "Pay")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger has 2 agenda points")
      (is (= 1 (count (:scored (get-challenger))))))))

(deftest brain-taping-warehouse
  ;; Brain-Taping Warehouse - Lower rez cost of Bioroid Character by 1 for each unspent Challenger click
  (do-game
    (new-game (default-contestant ["Brain-Taping Warehouse" "Ichi 1.0"
                             "Eli 1.0"])
              (default-challenger))
    (play-from-hand state :contestant "Brain-Taping Warehouse" "New remote")
    (play-from-hand state :contestant "Ichi 1.0" "Server 1")
    (play-from-hand state :contestant "Eli 1.0" "HQ")
    (let [ichi (get-character state :remote1 0)
          eli (get-character state :hq 0)]
      (take-credits state :contestant)
      (run-on state :remote1)
      (core/rez state :contestant (get-content state :remote1 0))
      (is (= 3 (:click (get-challenger))))
      (core/rez state :contestant ichi)
      (is (= 2 (:credit (get-contestant))) "Paid only 2c to rez Ichi; reduction of 3c")
      (run-jack-out state)
      (run-on state :hq)
      (is (= 2 (:click (get-challenger))))
      (core/rez state :contestant eli)
      (is (= 1 (:credit (get-contestant))) "Paid only 1c to rez Eli; reduction of 2c"))))

(deftest breached-dome
  ;; Breached Dome
  (do-game
    (new-game (default-contestant [(qty "Breached Dome" 10)])
              (default-challenger [(qty "Sure Gamble" 10)]))
    (discard-from-hand state :contestant "Breached Dome")
    (play-from-hand state :contestant "Breached Dome" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "R&D")
    (prompt-choice :challenger "No action")
    (is (= 4 (count (:hand (get-challenger)))) "Challenger took 1 meat damage")
    (is (= 4 (count (:deck (get-challenger)))) "Challenger milled 1 card")
    (is (= 2 (count (:discard (get-challenger)))) "Challenger's discard grew by 2")
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "No action")
    (is (= 3 (count (:hand (get-challenger)))) "Challenger took 1 meat damage")
    (is (= 3 (count (:deck (get-challenger)))) "Challenger milled 1 card")
    (is (= 4 (count (:discard (get-challenger)))) "Challenger's discard grew by 2")
    (run-empty-server state "Archives")
    (is (= 2 (count (:hand (get-challenger)))) "Challenger took 1 meat damage")
    (is (= 2 (count (:deck (get-challenger)))) "Challenger milled 1 card")
    (is (= 6 (count (:discard (get-challenger)))) "Challenger's discard grew by 2")))

(deftest broadcast-square
  ;; Broadcast Square - Trace 3: Prevent all bad publicity
  (do-game
    (new-game (default-contestant ["Profiteering" "Hostile Takeover" "Broadcast Square"])
              (default-challenger))
    (play-from-hand state :contestant "Broadcast Square" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (is (= 3 (:credit (get-contestant))) "Contestant should have spent 2 credits")
    (play-from-hand state :contestant "Profiteering" "New remote")
    (score-agenda state :contestant (get-content state :remote2 0))
    (prompt-choice :contestant "3")  ;; Take 3 bad publicity from Profiteering, gain 15 (if bad publicity actually taken)
    (prompt-choice :contestant 0)  ;; Contestant doesn't pump trace, base 3
    (prompt-choice :challenger 0)  ;; Challenger doesn't pump trace; loses trace
    (is (= 1 (:agenda-point (get-contestant))) "Contestant should score a 1-point agenda")
    (is (zero? (:bad-publicity (get-contestant))) "Contestant should gain 0 bad publicity")
    (is (= 3 (:credit (get-contestant))) "Contestant should gain 0 credits")
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (score-agenda state :contestant (get-content state :remote3 0))
    (prompt-choice :contestant 0)  ;; Contestant doesn't pump trace, base 3
    (prompt-choice :challenger 3)  ;; Challenger pumps trace; wins trace
    (is (= 2 (:agenda-point (get-contestant))) "Contestant should score a 1-point agenda")
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant should gain 1 bad publicity from failed trace")
    (is (= 10 (:credit (get-contestant))) "Contestant should gain 7 credits")))

(deftest capital-investors
  ;; Capital Investors - Click for 2 credits
  (do-game
    (new-game (default-contestant ["Capital Investors"])
              (default-challenger))
    (play-from-hand state :contestant "Capital Investors" "New remote")
    (let [cap (get-content state :remote1 0)]
      (core/rez state :contestant cap)
      (card-ability state :contestant cap 0)
      (card-ability state :contestant cap 0)
      (is (zero? (:click (get-contestant))) "Used twcharacter, spent 2 clicks")
      (is (= 7 (:credit (get-contestant))) "Used twcharacter, gained 4 credits"))))

(deftest cerebral-overwriter
  ;; Cerebral Overwriter
  (do-game
    (new-game (default-contestant ["Cerebral Overwriter"])
              (default-challenger))
    (play-from-hand state :contestant "Cerebral Overwriter" "New remote")
    (let [co (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh co)})
      (core/advance state :contestant {:card (refresh co)})
      (is (= 2 (get-counters (refresh co) :advancement)))
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-choice :contestant "Yes") ; choose to do the optional ability
      (is (= 2 (:brain-damage (get-challenger))) "Challenger takes 2 brain damage"))))

(deftest chairman-hiro
  ;; Chairman Hiro - Reduce Challenger max hand size; add as 2 agenda points if Challenger discards him
  (do-game
    (new-game (default-contestant [(qty "Chairman Hiro" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Chairman Hiro" "New remote")
    (play-from-hand state :contestant "Chairman Hiro" "Server 1")
    (prompt-choice :contestant "OK")
    (is (= 1 (count (:discard (get-contestant)))) "First Hiro discarded")
    (is (zero? (:agenda-point (get-challenger))) "No points for Challenger if discarded by Contestant")
    (let [hiro (get-content state :remote1 0)]
      (core/rez state :contestant hiro)
      (is (= 3 (core/hand-size state :challenger)) "Challenger max hand size reduced by 2")
      (take-credits state :contestant)
      (take-credits state :challenger 3)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :challenger "Pay") ; discard Hiro
      (is (= 2 (:credit (get-challenger))) "Challenger paid 6 credits to discard")
      (is (= 5 (core/hand-size state :challenger)) "Challenger max hand size restored to 5")
      (is (= 1 (count (get-scored state :challenger)))
          "Chairman Hiro added to Challenger score area")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger gained 2 agenda points"))))

(deftest chief-slee
  ;; Chief Slee
  (do-game
    (new-game (default-contestant ["Chief Slee" "Hive" "Hedge Fund"])
              (default-challenger [(qty "Sure Gamble" 5)]))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hive" "HQ")
    (play-from-hand state :contestant "Chief Slee" "New remote")
    (run-on state :hq)
    (let [slee (get-content state :remote1 0)
          hive (get-character state :hq 0)]
      (core/rez state :contestant hive)
      (card-subroutine state :contestant hive 0)
      (dotimes [_ 5]
        (card-ability state :contestant slee 0))
      (take-credits state :challenger)
      (card-ability state :contestant slee 1)
      (is (= 5 (count (:discard (get-challenger)))) "Chief Slee should do 5 meat damage"))))

(deftest c.i.-fund
  ;; C.I. Fund
  (do-game
    (new-game (default-contestant ["C.I. Fund" "Hedge Fund"])
              (default-challenger))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "C.I. Fund" "New remote")
    (take-credits state :contestant)
    (let [ci (get-content state :remote1 0)]
      (core/rez state :contestant ci)
      (take-credits state :challenger)
      (card-ability state :contestant ci 0)
      (prompt-choice :contestant 3)
      (is (= 3 (get-counters (refresh ci) :credit)))
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (card-ability state :contestant ci 0)
      (prompt-choice :contestant 3)
      (is (= 6 (get-counters (refresh ci) :credit)))
      (core/end-phase-12 state :contestant nil)
      (is (= 8 (get-counters (refresh ci) :credit)))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/end-phase-12 state :contestant nil)
      (is (= 10 (get-counters (refresh ci) :credit)))
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant ci 1)
        (is (= 8 (- (:credit (get-contestant)) credits)))
        (is (zero? (get-counters (refresh ci) :credit)))))))

(deftest city-surveillance
  ;; City Surveillance - Challenger chooses to pay 1 credit or take 1 tag at start of their turn
  (do-game
    (new-game (default-contestant ["City Surveillance"])
              (default-challenger))
    (play-from-hand state :contestant "City Surveillance" "New remote")
    (let [surv (get-content state :remote1 0)]
      (core/rez state :contestant surv)
      (take-credits state :contestant)
      (is (some #{"Pay 1[Credits]" "Take 1 tag"} (-> (get-challenger) :prompt first :choices)))
      (prompt-choice :challenger "Pay 1[Credits]")
      (is (= 4 (:credit (get-challenger))) "Challenger paid 1 credit")
      (is (zero? (:tag (get-challenger))) "Challenger didn't take a tag")
      (is (empty? (:prompt (get-challenger))) "City Surveillance only fired once")
      (take-credits state :challenger)
      (core/lose state :challenger :credit (:credit (get-challenger))) ;; Set Challenger's credits to 0 so they can't choose to pay
      (take-credits state :contestant)
      (is (some #{"Take 1 tag"} (-> (get-challenger) :prompt first :choices)))
      (prompt-choice :challenger "Take 1 tag")
      (is (zero? (:credit (get-challenger))) "Challenger paid no credits")
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag"))
      (is (empty? (:prompt (get-challenger))) "City Surveillance only fired once")))

(deftest clone-suffrage-movement
  ;; Clone Suffrage Movement
  (do-game
    (new-game (default-contestant ["Clone Suffrage Movement" (qty "Hedge Fund" 2) "Ice Wall"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Clone Suffrage Movement" "New remote")
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (let [csm (get-content state :remote1 0)]
      (core/rez state :contestant (refresh csm))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 2 (-> (get-contestant) :discard count)) "Clone Suffrage Movement should activate")
      (is (:contestant-phase-12 @state) "Contestant should get option to fire Clone Suffrage Movement")
      ;; Challenger has 1+ credit and chooses to pay 1 credit
      (card-ability state :contestant csm 0)
      (prompt-select :contestant (find-card "Hedge Fund" (:discard (get-contestant))))
      (core/end-phase-12 state :contestant nil)
      (play-from-hand state :contestant "Ice Wall" "Server 1")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (not (:contestant-phase-12 @state)) "Clone Suffrage Movement didn't activate cuz of the character"))))

(deftest clyde-van-rite
  ;; Clyde Van Rite - Multiple scenarios involving Challenger not having credits/cards to discard
  (do-game
    (new-game (default-contestant ["Clyde Van Rite"])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Restructure" 2) (qty "John Masanori" 2)]))
    (play-from-hand state :contestant "Clyde Van Rite" "New remote")
    (let [clyde (get-content state :remote1 0)]
      (core/rez state :contestant clyde)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (:contestant-phase-12 @state) "Contestant in Step 1.2")
      ;; Challenger has 1+ credit and chooses to pay 1 credit
      (card-ability state :contestant clyde 0)
      (is (= 9 (:credit (get-challenger))))
      (is (= 2 (count (:deck (get-challenger)))))
      (is (some #{"Pay 1[Credits]" "Discard top card"} (-> (get-challenger) :prompt first :choices)))
      (prompt-choice-partial :challenger "Pay")
      (is (= 8 (:credit (get-challenger))))
      (is (= 2 (count (:deck (get-challenger)))))
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :challenger)
      ;; Challenger can't pay 1 credit so must discard top card
      (core/lose state :challenger :credit (:credit (get-challenger)))
      (card-ability state :contestant clyde 0)
      (is (zero? (:credit (get-challenger))))
      (is (= 2 (count (:deck (get-challenger)))))
      (is (some #{"Discard top card"} (-> (get-challenger) :prompt first :choices)))
      (prompt-choice-partial :challenger "Discard")
      (is (zero? (:credit (get-challenger))))
      (is (= 1 (count (:deck (get-challenger)))))
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :challenger)
      ;; Challenger has 1+ card in Stack and chooses to discard 1 card
      (card-ability state :contestant clyde 0)
      (is (= 4 (:credit (get-challenger))))
      (is (= 1 (count (:deck (get-challenger)))))
      (is (some #{"Pay 1[Credits]" "Discard top card"} (-> (get-challenger) :prompt first :choices)))
      (prompt-choice :challenger "Discard top card")
      (is (= 4 (:credit (get-challenger))))
      (is (zero? (count (:deck (get-challenger)))))
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :challenger)
      ;; Challenger has no cards in Stack so must pay 1 credit
      (card-ability state :contestant clyde 0)
      (is (= 8 (:credit (get-challenger))))
      (is (zero? (count (:deck (get-challenger)))))
      (is (some #{"Pay 1[Credits]"} (-> (get-challenger) :prompt first :choices)))
      (prompt-choice-partial :challenger "Pay")
      (is (= 7 (:credit (get-challenger))))
      (is (zero? (count (:deck (get-challenger)))))
      (take-credits state :contestant)
      (dotimes [_ 4]
        (core/click-credit state :challenger nil))
      (core/lose state :challenger :credit (:credit (get-challenger)))
      (core/end-turn state :challenger nil)
      ;; Challenger has no credits and no cards so nothing happens
      (card-ability state :contestant clyde 0)
      (is (zero? (:credit (get-challenger))))
      (is (zero? (count (:deck (get-challenger)))))
      (is (empty? (-> @state :contestant :prompt))))))

(deftest commercial-bankers-group
  ;; Commercial Bankers Group - Gain 3 credits at turn start if unprotected by character
  (do-game
    (new-game (default-contestant ["Commercial Bankers Group" "Ice Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Commercial Bankers Group" "New remote")
    (let [cbg (get-content state :remote1 0)]
      (core/rez state :contestant cbg)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 9 (:credit (get-contestant))) "Bankers Group paid 3 credits")
      (play-from-hand state :contestant "Ice Wall" "Server 1")
      (take-credits state :contestant)
      (is (= 11 (:credit (get-contestant))))
      (take-credits state :challenger)
      (is (= 11 (:credit (get-contestant))) "Bankers Group didn't pay credits"))))

(deftest constellation-protocol
  ;; Constellation Protocol
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Constellation Protocol" "Ice Wall" "Fire Wall"])
                (default-challenger))
      (core/gain state :contestant :credit 100 :click 10)
      (play-from-hand state :contestant "Constellation Protocol" "New remote")
      (play-from-hand state :contestant "Ice Wall" "New remote")
      (play-from-hand state :contestant "Fire Wall" "New remote")
      (let [cp (get-content state :remote1 0)
            iw (get-character state :remote2 0)
            fw (get-character state :remote3 0)]
        (core/rez state :contestant cp)
        (core/rez state :contestant iw)
        (core/rez state :contestant fw)
        (advance state iw 1)
        (advance state fw 1)
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (:contestant-phase-12 @state) "Should be waiting for Constellation Protocol to be fired")
        (card-ability state :contestant cp 0)
        (is (= 1 (get-counters (refresh iw) :advancement)))
        (is (= 1 (get-counters (refresh fw) :advancement)))
        (prompt-select :contestant (refresh iw))
        (prompt-select :contestant (refresh fw))
        (is (zero? (get-counters (refresh iw) :advancement)))
        (is (= 2 (get-counters (refresh fw) :advancement)))
        (core/end-phase-12 state :contestant nil))))
  (testing "Variable number of advanceable cards"
    (do-game
      (new-game (default-contestant ["Constellation Protocol" "Ice Wall" "Hive"])
                (default-challenger))
      (core/gain state :contestant :credit 100 :click 10)
      (play-from-hand state :contestant "Constellation Protocol" "New remote")
      (let [cp (get-content state :remote1 0)]
        (core/rez state :contestant cp))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (not (:contestant-phase-12 @state)) "Constellation Protocol shouldn't fire with no advanceable character")
      (play-from-hand state :contestant "Ice Wall" "New remote")
      (let [iw (get-character state :remote2 0)]
        (core/rez state :contestant iw)
        (advance state iw 1)
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (not (:contestant-phase-12 @state)) "Constellation Protocol shouldn't fire with only a single character"))
      (play-from-hand state :contestant "Hive" "New remote")
      (let [hive (get-character state :remote3 0)]
        (core/rez state :contestant hive)
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (not (:contestant-phase-12 @state)) "Constellation Protocol shouldn't fire when the target character can't be advanced"))))
  (testing "Can't advance sites"
    (do-game
      (new-game (default-contestant ["Constellation Protocol" "Ice Wall" "Contract Killer"])
                (default-challenger))
      (core/gain state :contestant :credit 100 :click 10)
      (play-from-hand state :contestant "Constellation Protocol" "New remote")
      (play-from-hand state :contestant "Ice Wall" "New remote")
      (play-from-hand state :contestant "Contract Killer" "New remote")
      (let [cp (get-content state :remote1 0)
            iw (get-character state :remote2 0)
            ck (get-content state :remote3 0)]
        (core/rez state :contestant cp)
        (core/rez state :contestant iw)
        (core/rez state :contestant ck)
        (advance state iw 1))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (not (:contestant-phase-12 @state)) "Constellation Protocol shouldn't fire when only target is site"))))

(deftest contract-killer
  ;; Contract Killer
  (do-game
    (new-game (default-contestant ["Contract Killer"])
              (default-challenger [(qty "Sure Gamble" 2) "Data Dealer"]))
    (core/gain state :contestant :credit 10)
    (play-from-hand state :contestant "Contract Killer" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Data Dealer")
    (take-credits state :challenger)
    (let [ck (get-content state :remote1 0)]
      (advance state ck 2)
      (card-ability state :contestant ck 0)
      (prompt-select :contestant (get-radicle state 0))
      (is (= 1 (-> (get-contestant) :discard count)) "Contract Killer should be discarded as an ability cost")
      (is (= 1 (-> (get-challenger) :discard count)) "Contract Killer should discard Data Dealer"))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (core/gain state :contestant :click 1)
    (core/move state :contestant (find-card "Contract Killer" (:discard (get-contestant))) :hand)
    (play-from-hand state :contestant "Contract Killer" "New remote")
    (let [ck (get-content state :remote2 0)]
      (advance state ck 2)
      (card-ability state :contestant ck 1)
      (is (= 1 (-> (get-contestant) :discard count)) "Contract Killer should be discarded as an ability cost")
      (is (= 3 (-> (get-challenger) :discard count)) "Contract Killer should do 2 meat damage"))))

(deftest contestantorate-town
  ;; Contestantorate Town
  (do-game
    (new-game (default-contestant ["Contestantorate Town" "Hostile Takeover"])
              (default-challenger ["Data Dealer"]))
    (core/gain state :contestant :click 1)
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :contestant "Contestantorate Town" "New remote")
    (let [ct (get-content state :remote2 0)
          ht (get-scored state :contestant 0)]
      (core/rez state :contestant ct)
      (prompt-select :contestant ht)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Data Dealer")
      (take-credits state :challenger)
      (card-ability state :contestant ct 0)
      (prompt-select :contestant (get-radicle state 0))
      (is (= 1 (-> (get-challenger) :discard count)) "Contestantorate Town should discard Data Dealer")
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (not (:contestant-phase-12 @state)) "Contestantorate Town shouldn't activate if there are no radicles"))))

(deftest cpc-generator
  ;; CPC Generator
  (do-game
    (new-game (default-contestant ["CPC Generator"])
              (default-challenger))
    (play-from-hand state :contestant "CPC Generator" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (let [credits (:credit (get-contestant))]
      (core/click-credit state :challenger nil)
      (is (= 1 (- (:credit (get-contestant)) credits)) "Should gain one from CPC Generator"))
    (let [credits (:credit (get-contestant))]
      (core/click-credit state :challenger nil)
      (is (zero? (- (:credit (get-contestant)) credits)) "Shouldn't gain another credit from CPC Generator"))))

(deftest cybernetics-court
  ;; Cybernetics Court
  (do-game
    (new-game (default-contestant ["Cybernetics Court"])
              (default-challenger))
    (play-from-hand state :contestant "Cybernetics Court" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (is (= 9 (get-hand-size :contestant)) "Contestant should have hand size of 9")))

(deftest daily-business-show
  ;; Daily Business Show
  (testing "Full test"
    (do-game
      (new-game (default-contestant [(qty "Daily Business Show" 3) "Hedge Fund" "Jackson Howard"
                               "Resistor" "Product Placement" "Breaking News"])
                (default-challenger))
      (starting-hand state :contestant ["Daily Business Show" "Daily Business Show" "Daily Business Show" "Hedge Fund"])
      (core/gain state :contestant :credit 1)
      (play-from-hand state :contestant "Daily Business Show" "New remote")
      (play-from-hand state :contestant "Daily Business Show" "New remote")
      (play-from-hand state :contestant "Daily Business Show" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (core/rez state :contestant (get-content state :remote2 0))
      (core/rez state :contestant (get-content state :remote3 0))
      (take-credits state :contestant)
      (is (= 1 (count (:hand (get-contestant)))))
      (take-credits state :challenger)
      (is (= 5 (count (:hand (get-contestant)))) "Drew an additional 3 cards with 3 DBS")
      (is (not-empty (:prompt (get-challenger))) "Challenger is waiting for Contestant to use DBS")
      (prompt-select :contestant (find-card "Hedge Fund" (:hand (get-contestant)))) ;invalid target
      (prompt-select :contestant (find-card "Resistor" (:hand (get-contestant))))
      (prompt-select :contestant (find-card "Product Placement" (:hand (get-contestant))))
      (prompt-select :contestant (find-card "Breaking News" (:hand (get-contestant))))
      (is (empty? (:prompt (get-challenger))) "Challenger prompt cleared")
      (is (= 2 (count (:hand (get-contestant)))))
      (is (= "Hedge Fund" (:title (first (:hand (get-contestant))))))
      (is (= "Jackson Howard" (:title (second (:hand (get-contestant))))))
      (is (= "Resistor" (:title (last (:deck (get-contestant))))) "Resistor last card in deck")
      (is (= "Product Placement" (:title (last (butlast (:deck (get-contestant))))))
          "Product Placement second last card in deck")
      (is (= "Breaking News" (:title (last (butlast (butlast (:deck (get-contestant)))))))
          "Breaking News third last card in deck")))
  (testing "Sensie Actors Union interaction"
    (do-game
      (new-game (default-contestant ["Daily Business Show" (qty "Sensie Actors Union" 2)
                               "Hedge Fund" "Jackson Howard"
                               "Resistor" "Product Placement" "Breaking News"])
                (default-challenger))
      (starting-hand state :contestant ["Daily Business Show" "Sensie Actors Union" "Sensie Actors Union" "Hedge Fund"])
      (play-from-hand state :contestant "Daily Business Show" "New remote")
      (play-from-hand state :contestant "Sensie Actors Union" "New remote")
      (play-from-hand state :contestant "Sensie Actors Union" "New remote")
      (let [sensie1 (get-content state :remote2 0)
            sensie2 (get-content state :remote3 0)]
        (core/rez state :contestant (get-content state :remote1 0))
        (core/rez state :contestant sensie1)
        (core/rez state :contestant sensie2)
        (take-credits state :contestant)
        (take-credits state :challenger)
        ;; Use first Sensie
        (is (= 1 (count (:hand (get-contestant)))))
        (card-ability state :contestant sensie1 0)
        (is (= 5 (count (:hand (get-contestant)))) "Drew 3 cards with Sensie, +1 with DBS")
        (prompt-select :contestant (find-card "Resistor" (:hand (get-contestant)))) ; DBS target
        (prompt-select :contestant (find-card "Hedge Fund" (:hand (get-contestant)))) ; Sensie target
        (is (= 3 (count (:hand (get-contestant)))))
        (is (= "Hedge Fund" (:title (last (:deck (get-contestant))))) "Hedge Fund last card in deck")
        (is (= "Resistor" (:title (last (butlast (:deck (get-contestant))))))
            "Resistor second last card in deck")
        ;; Try to use first Sensie again
        (card-ability state :contestant sensie1 0)
        (is (empty? (-> @state :contestant :prompt)) "Sensie didn't activate")
        (is (= 3 (count (:hand (get-contestant)))))
        ;; Use second Sensie
        (starting-hand state :contestant ["Hedge Fund" "Jackson Howard"])
        (is (= 2 (count (:hand (get-contestant)))))
        (card-ability state :contestant sensie2 0)
        (is (= 5 (count (:hand (get-contestant)))) "Drew 3 cards with Sensie, DBS didn't activate")
        (prompt-select :contestant (find-card "Breaking News" (:hand (get-contestant)))) ; Sensie target
        (is (= "Breaking News" (:title (last (:deck (get-contestant))))) "Breaking News last card in deck"))))
  (testing "Should not trigger if rezzed after mandatory draw"
    (do-game
      (new-game (default-contestant [(qty "Daily Business Show" 3) "Hedge Fund" "Jackson Howard"
                               "Resistor" "Product Placement" "Breaking News"])
                (default-challenger))
      (starting-hand state :contestant ["Daily Business Show"])
      (play-from-hand state :contestant "Daily Business Show" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (core/draw state :contestant)
      (is (= 1 (count (:hand (get-contestant)))) "DBS did not fire on manual draw")
      (is (empty? (:prompt (get-contestant))) "Contestant is not being asked to bury a card with DBS")))
  (testing "Fire on Challenger turn"
    (do-game
      (new-game (default-contestant ["Daily Business Show" "Hedge Fund"
                               "Resistor" "Product Placement" "Breaking News"])
                (default-challenger ["Fisk Investment Seminar"]))
      (starting-hand state :contestant ["Daily Business Show"])
      (play-from-hand state :contestant "Daily Business Show" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (take-credits state :contestant)
      (is (empty? (:hand (get-contestant))) "Contestant hand is empty")
      (play-from-hand state :challenger "Fisk Investment Seminar")
      (is (= 4 (count (:hand (get-contestant)))) "Drew an additional card from FIS")
      (is (not-empty (:prompt (get-challenger))) "Challenger is waiting for Contestant to use DBS")
      (prompt-select :contestant (find-card "Resistor" (:hand (get-contestant))))
      (is (empty? (:prompt (get-challenger))) "Challenger prompt cleared")
      (is (= 3 (count (:hand (get-contestant))))))))


(deftest dedicated-response-team
  ;; Dedicated Response Team - Do 2 meat damage when successful run ends if Challenger is tagged
  (do-game
    (new-game (default-contestant ["Dedicated Response Team"])
              (default-challenger))
    (play-from-hand state :contestant "Dedicated Response Team" "New remote")
    (let [drt (get-content state :remote1 0)]
      (core/rez state :contestant drt)
      (take-credits state :contestant)
      (run-empty-server state :rd)
      (is (empty? (:discard (get-challenger))) "Not tagged, no damage done")
      (core/gain state :challenger :tag 1)
      (run-on state :rd)
      (run-jack-out state)
      (is (empty? (:discard (get-challenger))) "Tagged but run unsuccessful, no damage done")
      (run-empty-server state :rd)
      (is (= 2 (count (:discard (get-challenger)))) "Suffered 2 damage for successful run w/ tag"))))

(deftest dedicated-server
  ;; Dedicated Servers
  (do-game
    (new-game (default-contestant ["Dedicated Server"])
              (default-challenger))
    (play-from-hand state :contestant "Dedicated Server" "New remote")
    (let [servers (get-content state :remote1 0)]
      (core/rez state :contestant servers)
      (is (= 2 (get-counters (refresh servers) :recurring)) "Should have 2 recurring credits"))))

(deftest director-haas
  ;; Director Haas
  (do-game
    (new-game (default-contestant [(qty "Director Haas" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Director Haas" "New remote")
    (play-from-hand state :contestant "Director Haas" "Server 1")
    (prompt-choice :contestant "OK")
    (is (= 1 (count (:discard (get-contestant)))) "First Haas discarded")
    (is (zero? (:agenda-point (get-challenger))) "No points for Challenger if discarded by Contestant")
    (let [dh (get-content state :remote1 0)]
      (core/rez state :contestant dh))
    (is (= 2 (:click (get-contestant))) "Contestant should immediately gain a click")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 4 (:click (get-contestant))) "Contestant should have an extra click each turn")
    (take-credits state :contestant)
    (take-credits state :challenger 3)
    (run-empty-server state "Server 1")
    (prompt-choice-partial :challenger "Pay") ; discard Haas
    (take-credits state :challenger)
    (is (= 3 (:click (get-contestant))) "Contestant should be back to 3 clicks")
    (is (= 1 (count (get-scored state :challenger))) "Director Haas added to Challenger score area")
    (is (= 2 (:agenda-point (get-challenger))) "Challenger gained 2 agenda points")))

(deftest docklands-crackdown
  ;; Docklands Crackdown
  (letfn [(dlcd-test [number]
            (do-game
              (new-game (default-contestant ["Docklands Crackdown"])
                        (default-challenger ["Cache"]))
              (play-from-hand state :contestant "Docklands Crackdown" "New remote")
              (let [dlcd (get-content state :remote1 0)]
                (core/rez state :contestant dlcd)
                (core/add-counter state :contestant dlcd :power number)
                (take-credits state :contestant)
                (play-from-hand state :challenger "Cache")
                (is (= (- 4 number) (:credit (get-challenger)))))))]
    (doall (map dlcd-test [0 1 2 3 4]))))

(deftest early-premiere
  ;; Early Premiere - Pay 1c at start of turn to place an advancement on a card in a server
  (do-game
    (new-game (default-contestant ["Early Premiere" "Ice Wall"
                             "Ghost Branch" "Blacklist"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Early Premiere" "New remote")
    (play-from-hand state :contestant "Blacklist" "New remote")
    (play-from-hand state :contestant "Ghost Branch" "New remote")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [ep (get-content state :remote1 0)
          bl (get-content state :remote2 0)
          gb (get-content state :remote3 0)
          iw (get-character state :hq 0)]
      (core/rez state :contestant ep)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (card-ability state :contestant ep 0)
      (prompt-select :contestant iw)
      (is (zero? (get-counters (refresh iw) :advancement)) "Ice Wall can't targeted, not in server")
      (prompt-select :contestant bl)
      (is (zero? (get-counters (refresh bl) :advancement)) "Blacklist can't targeted, can't be advanced")
      (prompt-select :contestant gb)
      (is (= 1 (get-counters (refresh gb) :advancement)) "1 advancement on Ghost Branch")
      (is (= 4 (:credit (get-contestant)))))))

(deftest echo-chamber
  ;; Echo Chamber - 3 clicks to become 1 point agenda
  (do-game
    (new-game (default-contestant ["Echo Chamber"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Echo Chamber" "New remote")
    (let [ec (get-content state :remote1 0)]
      (core/rez state :contestant ec)
      (card-ability state :contestant ec 0))
    (is (= 1 (:agendapoints (get-scored state :contestant 0))) "Echo Chamber added to Contestant score area")))

(deftest edge-of-world
  ;; Edge of World
  (do-game
    (new-game (default-contestant [(qty "Edge of World" 3) (qty "Ice Wall" 3)])
              (default-challenger))
    (core/gain state :contestant :credit 6 :click 1)
    (play-from-hand state :contestant "Edge of World" "New remote")
    (play-from-hand state :contestant "Edge of World" "New remote")
    (play-from-hand state :contestant "Ice Wall" "Server 1")
    (play-from-hand state :contestant "Ice Wall" "Server 1")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
        "Challenger waiting for Contestant to act")
    (prompt-choice :contestant "Yes")
    (prompt-choice :challenger "Yes")
    (is (= 2 (:brain-damage (get-challenger))) "Challenger took 2 brain damage")
    (run-empty-server state "Server 2")
    (prompt-choice :contestant "Yes")
    (prompt-choice :challenger "Yes")
    (is (= 2 (:brain-damage (get-challenger))) "Challenger did not take brain damage when no Character protected Edge of World")))

(deftest eliza's-toybox
  ;; Eliza's Toybox - Rez a card ignoring all costs
  (do-game
    (new-game (default-contestant ["Eliza's Toybox" "Wotan" "Archer"])
              (default-challenger))
    (play-from-hand state :contestant "Wotan" "R&D")
    (play-from-hand state :contestant "Archer" "HQ")
    (play-from-hand state :contestant "Eliza's Toybox" "New remote")
    (let [wotan (get-character state :rd 0)
          archer (get-character state :hq 0)
          eliza (get-content state :remote1 0)]
      (core/rez state :contestant eliza)
      (is (= 1 (:credit (get-contestant))))
      (is (zero? (:click (get-contestant))) "3 clicks spent")
      (core/gain state :contestant :click 6)
      (card-ability state :contestant eliza 0)
      (prompt-select :contestant wotan)
      (is (:rezzed (refresh wotan)))
      (is (= 3 (:click (get-contestant))) "3 clicks spent")
      (is (= 1 (:credit (get-contestant))) "No credits spent")
      (card-ability state :contestant eliza 0)
      (prompt-select :contestant archer)
      (is (:rezzed (refresh archer)))
      (is (zero? (:click (get-contestant))) "3 clicks spent")
      (is (= 1 (:credit (get-contestant))) "No credits or agendas spent"))))

(deftest elizabeth-mills
  ;; Elizabeth Mills - Remove 1 bad publicity when rezzed; click-discard to discard a location
  (do-game
    (new-game (default-contestant ["Elizabeth Mills"])
              (default-challenger ["Earthrise Hotel"]))
    (core/gain state :contestant :bad-publicity 1)
    (play-from-hand state :contestant "Elizabeth Mills" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Earthrise Hotel")
    (take-credits state :challenger)
    (let [liz (get-content state :remote1 0)
          hotel (get-radicle state 0)]
      (core/rez state :contestant liz)
      (is (zero? (:bad-publicity (get-contestant))) "1 bad publicity removed")
      (card-ability state :contestant liz 0)
      (prompt-select :contestant hotel)
      (is (= 1 (count (:discard (get-challenger)))) "Earthrise discarded")
      (is (= 1 (count (:discard (get-contestant)))) "Elizabeth Mills discarded")
      (is (= 1 (:bad-publicity (get-contestant))) "1 bad publicity taken from discarding a location"))))

(deftest encryption-protocol
  ;; Encryption Protocol - Discard cost of installed cards increased by 1
  (do-game
    (new-game (default-contestant [(qty "Encryption Protocol" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Encryption Protocol" "New remote")
    (play-from-hand state :contestant "Encryption Protocol" "New remote")
    (let [ep1 (get-content state :remote1 0)
          ep2 (get-content state :remote2 0)]
      (core/rez state :contestant ep1)
      (core/rez state :contestant ep2)
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (is (= 4 (core/discard-cost state :challenger (refresh ep1)))
          "Discard cost increased to 4 by two active Encryption Protocols")
      (prompt-choice-partial :challenger "Pay") ; discard first EP
      (run-empty-server state "Server 2")
      (is (= 3 (core/discard-cost state :challenger (refresh ep2)))
          "Discard cost increased to 3 by one active Encryption Protocol"))))

(deftest estelle-moon
  ;; Estelle Moon
  (letfn [(estelle-test [number]
            (do-game
              (new-game (default-contestant ["Estelle Moon" (qty "Encryption Protocol" 20)])
                        (default-challenger))
              (starting-hand state :contestant (repeat 9 "Encryption Protocol"))
              (core/move state :contestant (find-card "Estelle Moon" (:deck (get-contestant))) :hand)
              (play-from-hand state :contestant "Estelle Moon" "New remote")
              (let [em (get-content state :remote1 0)]
                (core/rez state :contestant (refresh em))
                (core/gain state :contestant :click 10)
                (dotimes [_ number]
                  (play-from-hand state :contestant "Encryption Protocol" "New remote"))
                (let [credits (:credit (get-contestant))
                      hand (count (:hand (get-contestant)))]
                  (card-ability state :contestant (refresh em) 0)
                  (is (= (* 2 number) (- (:credit (get-contestant)) credits)) (str "Should gain " (* 2 number) " credits"))
                  (is (= number (- (count (:hand (get-contestant))) hand)) (str "Should draw " number " cards"))
                  (is (= 1 (-> (get-contestant) :discard count)) "Estelle Moon should be discarded")))))]
    (doall (map estelle-test (range 10)))))

(deftest eve-campaign
  ;; Eve Campaign
  (do-game
    (new-game (default-contestant ["Eve Campaign"])
              (default-challenger))
    (play-from-hand state :contestant "Eve Campaign" "New remote")
    (let [eve (get-content state :remote1 0)]
      (core/rez state :contestant eve)
      (is (zero? (:credit (get-contestant))))
      (is (= 16 (get-counters (refresh eve) :credit)))
      (take-credits state :contestant 2)
      (take-credits state :challenger)
      (is (= 4 (:credit (get-contestant))))
      (is (= 14 (get-counters (refresh eve) :credit))))))

(deftest executive-boot-camp
  ;; Executive Boot Camp
  (testing "suppress the start-of-turn event on a rezzed card. Issue #1346"
    (do-game
      (new-game (default-contestant ["Eve Campaign" "Executive Boot Camp"])
                (default-challenger))
      (play-from-hand state :contestant "Eve Campaign" "New remote")
      (play-from-hand state :contestant "Executive Boot Camp" "New remote")
      (take-credits state :contestant)
      (is (= 6 (:credit (get-contestant))) "Contestant ends turn with 6 credits")
      (let [eve (get-content state :remote1 0)
            ebc (get-content state :remote2 0)]
        (core/rez state :contestant ebc)
        (take-credits state :challenger)
        (is (:contestant-phase-12 @state) "Contestant in Step 1.2")
        (card-ability state :contestant ebc 0)
        (prompt-select :contestant eve)
        (is (= 2 (:credit (get-contestant))) "EBC saved 1 credit on the rez of Eve")
        (is (= 16 (get-counters (refresh eve) :credit)))
        (core/end-phase-12 state :contestant nil)
        (is (= 2 (:credit (get-contestant))) "Contestant did not gain credits from Eve")
        (is (= 16 (get-counters (refresh eve) :credit)) "Did not take counters from Eve")
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (not (:contestant-phase-12 @state)) "With nothing to rez, EBC does not trigger Step 1.2")
        (is (= 14 (get-counters (refresh eve) :credit)) "Took counters from Eve"))))
  (testing "works with Ice that has alternate rez costs"
    (do-game
      (new-game (default-contestant ["15 Minutes" "Executive Boot Camp"
                               "Tithonium"])
                (default-challenger))
      (core/gain state :contestant :credit 3)
      (score-agenda state :contestant (find-card "15 Minutes" (:hand (get-contestant))))
      (play-from-hand state :contestant "Tithonium" "HQ")
      (play-from-hand state :contestant "Executive Boot Camp" "New remote")
      (let [ebc (get-content state :remote1 0)
            tith (get-character state :hq 0)]
        (core/rez state :contestant ebc)
        (take-credits state :contestant)
        (is (= 9 (:credit (get-contestant))) "Contestant ends turn with 9 credits")
        (take-credits state :challenger)
        (is (not (:rezzed (refresh tith))) "Tithonium not rezzed")
        (is (:contestant-phase-12 @state) "Contestant in Step 1.2")
        (card-ability state :contestant ebc 0)
        (prompt-select :contestant tith)
        (prompt-choice :contestant "No")
        (is (and (:installed (refresh tith)) (:rezzed (refresh tith))) "Rezzed Tithonium")
        (is (= 1 (:credit (get-contestant))) "EBC saved 1 credit on the rez of Tithonium")))))

(deftest executive-search-firm
  ;; Executive Search Firm
  (do-game
    (new-game (default-contestant ["Executive Search Firm" "Elizabeth Mills"
                             "Midori" "Shannon Claire"])
              (default-challenger))
    (starting-hand state :contestant ["Executive Search Firm"])
    (core/gain state :contestant :click 4)
    (play-from-hand state :contestant "Executive Search Firm" "New remote")
    (doseq [card ["Elizabeth Mills" "Midori" "Shannon Claire"]]
      (let [esf (get-content state :remote1 0)
            number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))]
        (card-ability state :contestant esf 0)
        (prompt-choice :contestant (find-card card (:deck (get-contestant))))
        (is (= card (-> (get-contestant) :hand first :title)) (str card " should be in hand"))
        (core/move state :contestant (find-card card (:hand (get-contestant))) :deck)
        (is (< number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))) "Should be shuffled")))))

(deftest expose
  ;; Exposé
  (do-game
    (new-game (default-contestant ["Exposé"])
              (default-challenger))
    (core/gain state :contestant :click 100 :credit 100)
    (dotimes [i 5]
      (play-from-hand state :contestant "Exposé" "New remote")
      (let [expose (get-content state (keyword (str "remote" (inc i))) 0)]
        (core/rez state :contestant (refresh expose))
        (is (zero? (:bad-publicity (get-contestant))) "Contestant should have 0 bad publicity to start with")
        (when (pos? i)
          (core/gain-bad-publicity state :contestant i)
          (is (= i (:bad-publicity (get-contestant))) (str "Contestant should gain " i " bad publicity"))
          (advance state (refresh expose) i))
        (card-ability state :contestant (refresh expose) 0)
        (is (zero? (:bad-publicity (get-contestant))) "Contestant should have 0 bad publicity after using Exposé's ability")
        (is (= 1 (-> (get-contestant) :discard count)) "Archives should have 1 card in it")
        (is (= "Exposé" (-> (get-contestant) :discard first :title)) "Only card in Archives should be Exposé")
        (core/move state :contestant (find-card "Exposé" (:discard (get-contestant))) :hand)))))

(deftest false-flag
  ;; False Flag
  (testing "when the contestant attempts to score False Flag"
    (testing "and False Flag has 7 advancements"
      (do-game
       (new-game (default-contestant ["False Flag"])
                 (default-challenger))
       (play-from-hand state :contestant "False Flag" "New remote")
       (let [ff (get-content state :remote1 0)]
         (core/add-counter state :contestant ff :advancement 7)
         (core/rez state :contestant (refresh ff))
         (card-ability state :contestant (refresh ff) 0)
         (is (nil? (get-content state :remote1 0))
             "False Flag is no longer in remote")
         (is (= 3 (:agendapoints (get-scored state :contestant 0)))
             "the contestant can score False Flag")
         (is (= 1 (:click (get-contestant)))
             "scoring False Flag costs one click"))))
    (testing "and False Flag has less than 7 advancements"
      (do-game
       (new-game (default-contestant ["False Flag"])
                 (default-challenger))
       (play-from-hand state :contestant "False Flag" "New remote")
       (let [ff (get-content state :remote1 0)]
         (core/add-counter state :contestant ff :advancement 6)
         (core/rez state :contestant (refresh ff))
         (card-ability state :contestant (refresh ff) 0)
         (is (not (nil? (get-content state :remote1 0)))
             "False Flag remains in the remote")
         (is (nil? (:agendapoints (get-scored state :contestant 0)))
             "the contestant cannot score false flag")
         (is (= 2 (:click (get-contestant)))
             "the contestant does not lose a click")))))
  (testing "when the challenger accesses False Flag"
    (letfn [(false-flag-tags-test
              [[advancements expected-tags]]
              (testing (str "and False Flag has " advancements " advancements")
                (do-game
                 (new-game (default-contestant ["False Flag"])
                           (default-challenger))
                 (play-from-hand state :contestant "False Flag" "New remote")
                 (core/add-prop state :contestant
                                (get-content state :remote1 0)
                                :advance-counter advancements)
                 (take-credits state :contestant)
                 (run-empty-server state "Server 1")
                 (prompt-choice :challenger "No")
                 (let [tags (:tag (get-challenger))]
                   (is (= expected-tags tags)
                       (str "the challenger recieves " tags " tags"))))))]
      (doall (map false-flag-tags-test
                  [[0 0]
                   [2 1]
                   [5 2]
                   [10 5]])))))

(deftest franchise-city
  ;; Franchise City
  (do-game
    (new-game (default-contestant ["Franchise City" "Accelerated Beta Test"])
              (default-challenger))
    (play-from-hand state :contestant "Franchise City" "New remote")
    (play-from-hand state :contestant "Accelerated Beta Test" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant 1)
    (run-empty-server state "Server 2")
    (prompt-choice :challenger "Steal")
    (is (zero? (count (get-content state :remote2))) "Agenda was stolen")
    (is (= 2 (:agenda-point (get-challenger))) "Challenger stole 2 points")
    (is (zero? (count (get-content state :remote2)))
        "Franchise City no longer installed")
    (is (find-card "Franchise City" (:scored (get-contestant))) "Franchise City in contestant scored area")
    (is (= 1 (:agenda-point (get-contestant))) "Contestant has 1 point")))

(deftest full-immersion-recstudio
  ;; Full Immmersion RecStudio - install directly, and via Interns
  (testing "Full test"
    (do-game
      (new-game
        (default-contestant ["Full Immersion RecStudio"
                       (qty "Interns" 2)
                       (qty "Launch Campaign" 3)])
        (default-challenger))
      (play-from-hand state :contestant "Full Immersion RecStudio" "New remote")
      (let [fir (get-content state :remote1 0)]
        (core/rez state :contestant fir)
        (card-ability state :contestant fir 0)
        (prompt-select :contestant (find-card "Launch Campaign" (:hand (get-contestant))))
        (let [lc (first (:hosted (refresh fir)))]
          (is lc "Launch Campaign hosted on Full Immersion RecStudio")
          (core/rez state :contestant lc)
          (is (and (:installed (refresh lc)) (:rezzed (refresh lc))) "Rezzed Launch Campaign")
          (take-credits state :contestant)
          (take-credits state :challenger)
          (is (= 5 (:credit (get-contestant))) "Gained 2cr from Launch Campaign")
          (is (= 4 (get-counters (refresh lc) :credit)) "4cr left on Launch Campaign")
          (play-from-hand state :contestant "Interns")
          (prompt-select :contestant (find-card "Launch Campaign" (:hand (get-contestant))))
          (prompt-choice :contestant (refresh fir))
          (is (= 2 (count (:hosted (refresh fir)))) "Interns installed onto FIR")))))
  (testing "hosting an site with events does not double-register events. Issue #1827"
    (do-game
      (new-game
        (default-contestant ["Full Immersion RecStudio" "Sandburg" "Vanilla"
                       "Oaktown Renovation"])
        (default-challenger))
      (play-from-hand state :contestant "Full Immersion RecStudio" "New remote")
      (play-from-hand state :contestant "Vanilla" "HQ")
      (let [fir (get-content state :remote1 0)
            van (get-character state :hq 0)]
        (core/rez state :contestant fir)
        (core/rez state :contestant van)
        (card-ability state :contestant fir 0)
        (prompt-select :contestant (find-card "Sandburg" (:hand (get-contestant))))
        (core/gain state :contestant :credit 7 :click 3)
        (core/rez state :contestant (first (:hosted (refresh fir))))
        (is (= 2 (:current-strength (refresh van))) "Vanilla at 2 strength")
        (card-ability state :contestant fir 0)
        (prompt-select :contestant (find-card "Oaktown Renovation" (:hand (get-contestant))))
        (core/advance state :contestant {:card (last (:hosted (refresh fir)))})
        (is (= 11 (:credit (get-contestant))) "Gained 1cr from advancing Oaktown")))))

(deftest fumiko-yamamori
  ;; Fumiko Yamamori
  (do-game
    (new-game
      (default-contestant ["Fumiko Yamamori"])
      (default-challenger))
    (core/gain state :contestant :credit 10)
    (play-from-hand state :contestant "Fumiko Yamamori" "New remote")
    (let [fumiko (get-content state :remote1 0)]
      (core/rez state :contestant (refresh fumiko))
      (core/psi-game state :contestant (refresh fumiko)
                     {:equal  {:msg "resolve equal bets effect"}
                      :not-equal {:msg "resolve unequal bets effect"}})
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :challenger "0 [Credits]")
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should discard a card to meat damage"))))

(deftest gene-splcharacterr
  ;; Gene Splcharacterr
  (testing "Challenger accesses an unadvanced Gene Splcharacterr and doesn't discard
           ;; No net damage is dealt and Gene Splcharacterr remains installed"
    (do-game
      (new-game
        (default-contestant ["Gene Splcharacterr"])
        (default-challenger [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Gene Splcharacterr" "New remote")
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-choice :challenger "No action")
      (is (zero? (count (:discard (get-challenger)))) "Challenger took no net damage")
      (is (= "Gene Splcharacterr" (:title (get-content state :remote1 0))) "Gene Splcharacterr was not discarded")
      (is (= 5 (:credit (get-challenger))) "Challenger spent no credits")))
  (testing "Challenger accesses an unadvanced Gene Splcharacterr and discards it.
           No net damage is dealt and Gene Splcharacterr is discarded"
    (do-game
      (new-game
        (default-contestant ["Gene Splcharacterr"])
        (default-challenger [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Gene Splcharacterr" "New remote")
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :challenger "Pay")
      (is (zero? (count (:discard (get-challenger)))) "Challenger took no net damage")
      (is (= nil (get-content state :remote1 0)) "Gene Splcharacterr is no longer in remote")
      (is (= (:title (last (:discard (get-contestant)))) "Gene Splcharacterr") "Gene Splcharacterr discarded")
      (is (= 4 (:credit (get-challenger))) "Challenger spent 1 credit to discard Gene Splcharacterr")))
  (testing "Challenger accesses a single-advanced Gene Splcharacterr and doesn't discard.
           1 net damage is dealt and Gene Splcharacterr remains installed"
    (do-game
      (new-game
        (default-contestant ["Gene Splcharacterr"])
        (default-challenger [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Gene Splcharacterr" "New remote")
      (core/add-counter state :contestant (get-content state :remote1 0) :advancement 1)
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-choice :challenger "No action")
      (is (= 1 (count (:discard (get-challenger)))) "Challenger took 1 net damage")
      (is (= "Gene Splcharacterr" (:title (get-content state :remote1 0))) "Gene Splcharacterr was not discarded")
      (is (= 5 (:credit (get-challenger))) "Challenger spent no credits")))
  (testing "Challenger accesses a single-advanced Gene Splcharacterr and discards it.
           1 net damage is dealt and Gene Splcharacterr is discarded"
    (do-game
      (new-game
        (default-contestant ["Gene Splcharacterr"])
        (default-challenger [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Gene Splcharacterr" "New remote")
      (core/add-counter state :contestant (get-content state :remote1 0) :advancement 1)
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :challenger "Pay")
      (is (= 1 (count (:discard (get-challenger)))) "Challenger took 1 net damage")
      (is (= nil (get-content state :remote1 0)) "Gene Splcharacterr is no longer in remote")
      (is (= (:title (last (:discard (get-contestant)))) "Gene Splcharacterr") "Gene Splcharacterr discarded")
      (is (= 4 (:credit (get-challenger))) "Challenger spent 1 credit to discard Gene Splcharacterr")))
  (testing "Challenger accesses a double-advanced Gene Splcharacterr and doesn't discard
           2 net damage is dealt and Gene Splcharacterr remains installed"
    (do-game
      (new-game
        (default-contestant ["Gene Splcharacterr"])
        (default-challenger [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Gene Splcharacterr" "New remote")
      (core/add-counter state :contestant (get-content state :remote1 0) :advancement 2)
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-choice :challenger "No")
      (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 net damage")
      (is (= "Gene Splcharacterr" (:title (get-content state :remote1 0))) "Gene Splcharacterr was not discarded")
      (is (= 5 (:credit (get-challenger))) "Challenger spent no credits")))
  (testing "Challenger accesses a double-advanced Gene Splcharacterr and discards it.
           2 net damage is dealt and Gene Splcharacterr is discarded"
    (do-game
      (new-game
        (default-contestant ["Gene Splcharacterr"])
        (default-challenger [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Gene Splcharacterr" "New remote")
      (core/add-counter state :contestant (get-content state :remote1 0) :advancement 2)
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :challenger "Pay")
      (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 net damage")
      (is (= nil (get-content state :remote1 0)) "Gene Splcharacterr is no longer in remote")
      (is (= (:title (last (:discard (get-contestant)))) "Gene Splcharacterr") "Gene Splcharacterr discarded")
      (is (= 4 (:credit (get-challenger))) "Challenger spent 1 credit to discard Gene Splcharacterr")))
  (testing "Contestant triple-advances a Gene Splcharacterr and uses its ability to add to their score area as a 1 point agenda"
    (do-game
      (new-game
        (default-contestant [(qty "Gene Splcharacterr" 2) (qty "Ice Wall" 3) (qty "Vanilla" 2)])
        (default-challenger [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Gene Splcharacterr" "New remote")
      (let [gs (get-content state :remote1 0)]
        (core/add-counter state :contestant gs :advancement 2)
        (take-credits state :challenger)
        (core/add-counter state :contestant (refresh gs) :advancement 1)
        (core/rez state :contestant (refresh gs))
        (card-ability state :contestant (refresh gs) 0)
        (is (= nil (get-content state :remote1 0)) "Gene Splcharacterr is no longer in remote")
        (is (= 1 (:agendapoints (get-scored state :contestant 0))) "Gene Splcharacterr added to Contestant score area")))))

(deftest genetics-pavilion
  ;; Genetics Pavilion - Limit Challenger to 2 draws per turn, but only during Challenger's turn
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Genetics Pavilion"])
                (default-challenger ["Diesel" (qty "Sure Gamble" 3) "Sports Hopper"]))
      (play-from-hand state :contestant "Genetics Pavilion" "New remote")
      (let [gp (get-content state :remote1 0)]
        (take-credits state :contestant)
        (core/rez state :contestant gp)
        (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
        (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
        (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
        (play-from-hand state :challenger "Sports Hopper")
        (play-from-hand state :challenger "Diesel")
        (is (= 2 (count (:hand (get-challenger)))) "Drew only 2 cards because of Genetics Pavilion")
        (take-credits state :challenger)
        (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
        (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
        (let [hopper (get-hazard state 0)]
          (card-ability state :challenger hopper 0)
          (is (= 3 (count (:hand (get-challenger)))) "Able to draw 3 cards during Contestant's turn")
          (core/derez state :contestant (refresh gp))
          (take-credits state :contestant)
          (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
          (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
          (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
          (core/move state :challenger (find-card "Diesel" (:discard (get-challenger))) :hand)
          (is (= 1 (count (:hand (get-challenger)))))
          (play-from-hand state :challenger "Diesel")
          (is (= 3 (count (:hand (get-challenger)))) "Drew 3 cards with Diesel")
          (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
          (core/rez state :contestant (refresh gp))
          (core/draw state :challenger)
          (is (= 2 (count (:hand (get-challenger)))) "No card drawn; GP counts cards drawn prior to rez")))))
  (testing "vs Fisk Investment Seminar"
    (do-game
      (new-game (default-contestant ["Genetics Pavilion" (qty "Hedge Fund" 3)])
                (default-challenger ["Fisk Investment Seminar" (qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Genetics Pavilion" "New remote")
      (let [gp (get-content state :remote1 0)]
        (take-credits state :contestant)
        (core/rez state :contestant gp)
        (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
        (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
        (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
        (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
        (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
        (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
        (is (= 1 (count (:hand (get-challenger)))))
        (is (zero? (count (:hand (get-contestant)))))
        (play-from-hand state :challenger "Fisk Investment Seminar")
        (is (= 2 (count (:hand (get-challenger)))) "Drew only 2 cards because of Genetics Pavilion")
        (is (= 3 (count (:hand (get-contestant)))) "Drew all 3 cards"))))
  (testing "Mr. Li interaction. #1594"
    (do-game
      (new-game (default-contestant ["Genetics Pavilion"])
                (default-challenger ["Mr. Li" "Account Siphon" "Faerie"
                                 "Sure Gamble" "John Masanori" "Desperado"]))
      (starting-hand state :challenger ["Mr. Li"])
      (play-from-hand state :contestant "Genetics Pavilion" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Mr. Li")
      (let [mrli (get-radicle state 0)]
        (is (zero? (count (:hand (get-challenger)))))
        ; use Mr. Li with 2 draws allowed
        (card-ability state :challenger mrli 0)
        (is (= 2 (count (:hand (get-challenger)))))
        (prompt-select :challenger (first (:hand (get-challenger))))
        (is (= 1 (count (:hand (get-challenger)))))
        ; use Mr. Li with 0 draws allowed
        (card-ability state :challenger mrli 0)
        (is (= 1 (count (:hand (get-challenger)))))
        (prompt-select :challenger (first (:hand (get-challenger)))) ; will fail because not a valid target
        (prompt-choice :challenger "Done") ; cancel out
        (take-credits state :challenger)
        (take-credits state :contestant)
        (core/draw state :challenger)
        (is (= 2 (count (:hand (get-challenger)))))
        ; use Mr. Li with 1 draw allowed
        (card-ability state :challenger mrli 0)
        (is (= 3 (count (:hand (get-challenger)))))
        (prompt-select :challenger (first (:hand (get-challenger)))) ; will fail
        (prompt-select :challenger (second (:hand (get-challenger)))) ; will fail
        (prompt-select :challenger (second (rest (:hand (get-challenger)))))
        (is (= 2 (count (:hand (get-challenger)))))))))

(deftest ghost-branch
  ;; Ghost Branch - Advanceable; give the Challenger tags equal to advancements when accessed
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Ghost Branch"])
                (default-challenger))
      (play-from-hand state :contestant "Ghost Branch" "New remote")
      (let [gb (get-content state :remote1 0)]
        (core/advance state :contestant {:card (refresh gb)})
        (core/advance state :contestant {:card (refresh gb)})
        (is (= 2 (get-counters (refresh gb) :advancement)))
        (take-credits state :contestant)
        (run-empty-server state "Server 1")
        (prompt-choice :contestant "Yes") ; choose to do the optional ability
        (is (= 2 (:tag (get-challenger))) "Challenger given 2 tags"))))
  (testing "with Dedicated Response Team"
    (do-game
      (new-game (default-contestant ["Ghost Branch" "Dedicated Response Team"])
                (default-challenger))
      (play-from-hand state :contestant "Ghost Branch" "New remote")
      (play-from-hand state :contestant "Dedicated Response Team" "New remote")
      (core/gain state :contestant :click 1)
      (let [gb (get-content state :remote1 0)
            drt (get-content state :remote2 0)]
        (core/advance state :contestant {:card gb})
        (core/advance state :contestant {:card (refresh gb)})
        (is (= 2 (get-counters (refresh gb) :advancement)) "Ghost Branch advanced twcharacter")
        (take-credits state :contestant)
        (run-on state "Server 1")
        (core/rez state :contestant drt)
        (run-successful state)
        (is (prompt-is-type? :challenger :waiting) "Challenger has prompt to wait for Ghost Branch")
        (prompt-choice :contestant "Yes")
        (is (= 2 (:tag (get-challenger))) "Challenger has 2 tags")
        (prompt-choice-partial :challenger "Pay")
        (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 meat damage")))))

(deftest grndl-refinery
  ;; GRNDL Refinery
  (do-game
    (new-game (default-contestant ["GRNDL Refinery"])
              (default-challenger))
    (core/gain state :contestant :click 100 :credit 100)
    (dotimes [i 5]
      (play-from-hand state :contestant "GRNDL Refinery" "New remote")
      (let [grndl (get-content state (keyword (str "remote" (inc i))) 0)
            credits (- (:credit (get-contestant)) i)]
        (when (pos? i)
          (advance state (refresh grndl) i)
          (is (= i (get-counters (refresh grndl) :advancement)) (str "GRNDL Refinery should have " i " advancement counters on it")))
        (card-ability state :contestant (refresh grndl) 0)
        (is (= (+ credits (* i 4)) (:credit (get-contestant))) (str "Contestant should gain " (* i 4) " credits"))
        (is (= 1 (-> (get-contestant) :discard count)) "Archives should have 1 card in it")
        (is (= "GRNDL Refinery" (-> (get-contestant) :discard first :title)) "Only card in Archives should be GRNDL Refinery")
        (core/move state :contestant (find-card "GRNDL Refinery" (:discard (get-contestant))) :hand)))))

(deftest haas-arcology-ai
  ;; Haas Arcology AI - Click and advancement to gain 2 clicks, once per turn
  (do-game
    (new-game (default-contestant ["Haas Arcology AI"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Haas Arcology AI" "New remote")
    (let [haa (get-content state :remote1 0)]
      (advance state haa 2)
      (core/rez state :contestant (refresh haa))
      (is (= 1 (:click (get-contestant))))
      (is (= 2 (get-counters (refresh haa) :advancement)))
      (card-ability state :contestant (refresh haa) 0)
      (is (= 1 (get-counters (refresh haa) :advancement)) "Spent 1 advancement")
      (is (= 2 (:click (get-contestant))) "Spent last click to gain 2 clicks")
      (card-ability state :contestant (refresh haa) 0)
      (is (= 1 (get-counters (refresh haa) :advancement)) "Can't use twcharacter in a turn")
      (is (= 2 (:click (get-contestant))) "Didn't spend a click"))))

(deftest honeyfarm
  ;; Honeyfarm - lose one credit on access
  (do-game
    (new-game (default-contestant [(qty "Honeyfarm" 3)])
              (default-challenger))
    (discard-from-hand state :contestant "Honeyfarm")
    (play-from-hand state :contestant "Honeyfarm" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (is (= 4 (:credit (get-challenger))))
    (run-empty-server state "Archives")
    (is (= 3 (:credit (get-challenger))))
    (run-empty-server state "HQ")
    (is (= 2 (:credit (get-challenger))))))

(deftest hostile-infrastructure
  ;; Hostile Infrastructure - do 1 net damage when challenger discards a contestant card
  (do-game
    (new-game (default-contestant [(qty "Hostile Infrastructure" 3)])
              (default-challenger))
    (core/gain state :challenger :credit 50)
    (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (run-empty-server state :hq)
    (prompt-choice-partial :challenger "Pay")
    (is (= 1 (count (:discard (get-challenger)))) "Took 1 net damage")
    (run-empty-server state :remote1)
    (prompt-choice-partial :challenger "Pay")
    (is (= 2 (count (:discard (get-challenger)))) "Took 1 net damage")))

(deftest hyoubu-research-facility
  ;; Hyoubu Research Facility
  (do-game
    (new-game (default-contestant ["Hyoubu Research Facility" "Snowflake"])
              (default-challenger))
    (play-from-hand state :contestant "Hyoubu Research Facility" "New remote")
    (play-from-hand state :contestant "Snowflake" "HQ")
    (let [hrf (get-content state :remote1 0)
          sf (get-character state :hq 0)]
      (take-credits state :contestant)
      (run-on state "HQ")
      (core/rez state :contestant hrf)
      (core/rez state :contestant sf)
      (card-subroutine state :contestant sf 0)
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :challenger "0 [Credits]")
      (is (= 5 (:credit (get-contestant))) "Gained 2c from Hyoubu")
      (run-on state "HQ")
      (card-subroutine state :contestant sf 0)
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :challenger "0 [Credits]")
      (is (= 3 (:credit (get-contestant))) "No credits gained from Hyoubu"))))

(deftest ibrahim-salem
  ;; Ibrahim Salem
  (do-game
    (new-game (default-contestant ["Hostile Takeover" "Ibrahim Salem"])
              (default-challenger ["Sure Gamble" "Astrolabe" "Paperclip" "Daily Casts"]))
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :contestant "Ibrahim Salem" "New remote")
    (let [is (get-content state :remote2 0)]
      (core/rez state :contestant (refresh is))
      (prompt-select :contestant (-> (get-contestant) :scored first))
      (doseq [[i [card-type card-name]]
              (map-indexed vector ['("Event" "Sure Gamble")
                                   '("Hazard" "Astrolabe")
                                   '("Resource" "Paperclip")
                                   '("Radicle" "Daily Casts")])]
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (:contestant-phase-12 @state) "Contestant is in Step 1.2")
        (card-ability state :contestant is 0)
        (prompt-choice :contestant card-type)
        (prompt-choice :contestant (find-card card-name (:hand (get-challenger))))
        (core/end-phase-12 state :contestant nil)
        (is (= (inc i) (-> (get-challenger) :discard count)))))))

(deftest illegal-arms-factory
  ;; Illegal Arms Factory; draw a card, gain a credit, bad pub when discarded while rezzed
  (do-game
    (new-game (default-contestant ["Hedge Fund"
                             "Beanstalk Royalties"
                             "IPO"
                             (qty "Illegal Arms Factory" 3)])
              (default-challenger))
    (core/gain state :challenger :credit 20)
    (core/move state :contestant (find-card "IPO" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Beanstalk Royalties" (:hand (get-contestant))) :deck)
    (play-from-hand state :contestant "Illegal Arms Factory" "New remote")
    (play-from-hand state :contestant "Illegal Arms Factory" "New remote")
    (let [iaf (get-content state :remote2 0)]
      (core/rez state :contestant iaf)
      (take-credits state :contestant)
      (run-empty-server state :remote1)
      (prompt-choice-partial :challenger "Pay")
      (is (zero? (:bad-publicity (get-contestant))) "Took no bad pub on unrezzed discard")
      (take-credits state :challenger)
      (is (= 3 (count (:hand (get-contestant)))) "Drew a card from IAF + mandatory")
      (is (= 4 (:credit (get-contestant))) "Gained 1 credit from IAF")
      (take-credits state :contestant)
      (run-empty-server state :remote2)
      (prompt-choice-partial :challenger "Pay")
      (is (= 1 (:bad-publicity (get-contestant))) "Took a bad pub on rezzed discard"))))

(deftest indian-union-stock-exchange
  ;; Indian Union Stock Exchange
  (do-game
    (new-game (make-deck "Argus Security: Protection Guaranteed"
                         ["Indian Union Stock Exchange" "Beanstalk Royalties"
                          "Kill Switch" "Net Police"])
              (default-challenger))
    (core/gain state :contestant :click 3)
    (play-from-hand state :contestant "Indian Union Stock Exchange" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (let [credits (:credit (get-contestant))]
      (play-from-hand state :contestant "Beanstalk Royalties")
      (is (= (+ 3 credits) (:credit (get-contestant))) "Contestant should only gain 3 credits"))
    (let [credits (:credit (get-contestant))]
      (play-from-hand state :contestant "Kill Switch")
      (is (= credits (:credit (get-contestant))) "Contestant should neither gain nor lose any credits"))
    (let [credits (:credit (get-contestant))]
      (play-from-hand state :contestant "Net Police" "New remote")
      (core/rez state :contestant (get-content state :remote2 0))
      (is (= credits (:credit (get-contestant))) "Contestant should neither gain nor lose any credits"))))

(deftest isabel-mcguire
  ;; Isabel McGuire
  (do-game
    (new-game (default-contestant ["Ice Wall" "Isabel McGuire"])
              (default-challenger))
    (play-from-hand state :contestant "Isabel McGuire" "New remote")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (is (zero? (-> (get-contestant) :hand count)))
    (let [isabel (get-content state :remote1 0)
          iw (get-character state :hq 0)]
      (core/rez state :contestant isabel)
      (card-ability state :contestant isabel 0)
      (prompt-select :contestant (refresh iw))
      (is (= 1 (-> (get-contestant) :hand count))))))

(deftest it-department
  ;; IT Department - Add strength to rezzed Character until end of turn
  (do-game
    (new-game (default-contestant ["IT Department" "Wall of Static"])
              (default-challenger))
    (play-from-hand state :contestant "IT Department" "New remote")
    (play-from-hand state :contestant "Wall of Static" "Server 1")
    (let [itd (get-content state :remote1 0)
          wos (get-character state :remote1 0)]
      (core/rez state :contestant itd)
      (core/rez state :contestant wos)
      (card-ability state :contestant itd 1)
      (is (zero? (:click (get-contestant))) "Spent 1 click")
      (is (= 1 (get-counters (refresh itd) :power)) "IT Dept has 1 counter")
      (core/add-counter state :contestant (refresh itd) :power 4)
      (is (= 5 (get-counters (refresh itd) :power)) "IT Dept has 5 counters")
      (card-ability state :contestant itd 0)
      (prompt-select :contestant wos)
      ;; refer to online guides for summary of how this ludicrous formula is calculated
      (is (= 8 (:current-strength (refresh wos))) "Gained 5 strength")
      (is (= 4 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :contestant itd 0)
      (prompt-select :contestant wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 3 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :contestant itd 0)
      (prompt-select :contestant wos)
      (is (= 12 (:current-strength (refresh wos))) "Gained total of 9 strength")
      (is (= 2 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :contestant itd 0)
      (prompt-select :contestant wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 1 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (take-credits state :contestant)
      (is (= 3 (:current-strength (refresh wos))) "Back to default strength"))))

(deftest jackson-howard
  ;; Jackson Howard - Draw 2 cards
  (do-game
    (new-game (default-contestant [(qty "Jackson Howard" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Restructure" 2)])
              (default-challenger))
    ;; guaranteed to be at least 1 jhow in hand after draw, and 2 cards in R&D
    (play-from-hand state :contestant "Jackson Howard" "New remote")
    (let [jhow (get-content state :remote1 0)]
      (core/rez state :contestant jhow)
      (is (= 5 (count (:hand (get-contestant)))))
      (is (= 2 (:click (get-contestant))))
      (card-ability state :contestant jhow 0)
      (is (= 7 (count (:hand (get-contestant)))) "Drew 2 cards")
      (is (= 1 (:click (get-contestant)))))))

(deftest jeeves-model-bioroids
  ;; Jeeves Model Bioroids
  (do-game
    (new-game (default-contestant ["Jeeves Model Bioroids" "TGTBT"
                             (qty "Melange Mining Contestant." 2)])
              (default-challenger [(qty "Ghost Challenger" 3)]))
    (play-from-hand state :contestant "Jeeves Model Bioroids" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Ghost Challenger")
    (play-from-hand state :challenger "Ghost Challenger")
    (play-from-hand state :challenger "Ghost Challenger")
    (take-credits state :challenger)
    ; install 3 things
    (play-from-hand state :contestant "TGTBT" "New remote")
    (play-from-hand state :contestant "Melange Mining Contestant." "New remote")
    (play-from-hand state :contestant "Melange Mining Contestant." "New remote")
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :challenger)
    ;;click for credits
    (take-credits state :contestant 3)
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :challenger)
    ;;click to purge
    (core/do-purge state :contestant 3)
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :challenger)
    ;;click to advance
    (core/advance state :contestant (get-content state :remote2 0))
    (core/advance state :contestant (get-content state :remote2 0))
    (core/advance state :contestant (get-content state :remote2 0))
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :challenger)
    ;; use 3 clicks on card ability - Melange
    (core/rez state :contestant (get-content state :remote3 0))
    (card-ability state :contestant (get-content state :remote3 0) 0)
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :challenger)
    ;; discard 3 radicles
    (core/gain state :challenger :tag 1)
    (core/discard-radicle state :contestant nil)
    (prompt-select :contestant (get-radicle state 0))
    (is (= 1 (count (:discard (get-challenger)))))
    (core/discard-radicle state :contestant nil)
    (prompt-select :contestant (get-radicle state 0))
    (is (= 2 (count (:discard (get-challenger)))))
    (core/discard-radicle state :contestant nil)
    (prompt-select :contestant (get-radicle state 0))
    (is (= 3 (count (:discard (get-challenger)))))
    (is (= 1 (:click (get-contestant))))))

(deftest kala-ghoda-real-tv
  ;; Kala Ghoda Real TV
  (do-game
    (new-game (default-contestant ["Kala Ghoda Real TV"])
              (default-challenger) [(qty "Sure Gamble" 3)])
    (starting-hand state :challenger ["Sure Gamble"])
    (play-from-hand state :contestant "Kala Ghoda Real TV" "New remote")
    (let [tv (get-content state :remote1 0)]
      (core/rez state :contestant tv)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (:contestant-phase-12 @state) "Contestant is in Step 1.2")
      (card-ability state :contestant tv 0)
      (prompt-choice :contestant "Done")
      (card-ability state :contestant tv 1)
      (is (= 1 (count (:discard (get-contestant)))))
      (is (= 1 (count (:discard (get-challenger)))))
      (is (last-log-contains? state "Sure Gamble")
          "Kala Ghoda did log discarded card names"))))

(deftest kuwinda-k4h1u3
  ;; Kuwinda K4H1U3
  (do-game
    (new-game (default-contestant ["Kuwinda K4H1U3"])
              (default-challenger))
    (core/gain state :contestant :credit 100)
    (core/gain state :challenger :credit 100)
    (play-from-hand state :contestant "Kuwinda K4H1U3" "New remote")
    (let [kuwinda (get-content state :remote1 0)]
      (core/rez state :contestant kuwinda)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (:contestant-phase-12 @state) "Contestant is in Step 1.2")
      (card-ability state :contestant (refresh kuwinda) 0)
      (is (zero? (-> (get-contestant) :prompt first :base)) "Base Trace should start at 0")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (zero? (-> (get-challenger) :discard count)) "Challenger shouldn't take any damage")
      (is (= 1 (get-counters (refresh kuwinda) :power)) "Kuwinda should gain 1 power counter")
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (:contestant-phase-12 @state) "Contestant is in Step 1.2")
      (card-ability state :contestant (refresh kuwinda) 0)
      (is (= 1 (-> (get-contestant) :prompt first :base)) "Base Trace should now start at 1")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 1)
      (is (zero? (-> (get-challenger) :discard count)) "Challenger shouldn't take any damage")
      (is (= 2 (get-counters (refresh kuwinda) :power)) "Kuwinda should gain another power counter")
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (:contestant-phase-12 @state) "Contestant is in Step 1.2")
      (card-ability state :contestant (refresh kuwinda) 0)
      (is (= 2 (-> (get-contestant) :prompt first :base)) "Base Trace should be up to 2")
      (prompt-choice :contestant 1)
      (prompt-choice :challenger 0)
      (is (= 1 (-> (get-challenger) :brain-damage)) "Trace succeeded so challenger should take 1 brain damage")
      (is (= 1 (-> (get-challenger) :discard count)) "Trace succeeded so challenger should discard card from damage")
      (is (= 1 (-> (get-contestant) :discard count)) "Kuwinda should be in Archives")
      (is (= "Kuwinda K4H1U3" (-> (get-contestant) :discard first :title)) "Kuwinda should be in Archives")
      (core/end-phase-12 state :contestant nil))))

(deftest lakshmi-smartfabrics
  ;; Lakshmi Smartfabrics - Gain power counter when rezzing a card; use counters to protect agenda in HQ
  (do-game
    (new-game (default-contestant ["Lakshmi Smartfabrics" "Vanilla"
                             "Marked Accounts" "Elective Region"])
              (default-challenger))
    (play-from-hand state :contestant "Lakshmi Smartfabrics" "New remote")
    (let [lak (get-content state :remote1 0)]
      (core/rez state :contestant lak)
      (is (= 1 (get-counters (refresh lak) :power)) "Smartfabrics gained 1 power counter for itself")
      (play-from-hand state :contestant "Vanilla" "R&D")
      (play-from-hand state :contestant "Marked Accounts" "New remote")
      (core/rez state :contestant (get-character state :rd 0))
      (is (= 2 (get-counters (refresh lak) :power)) "Smartfabrics gained 1 power counter")
      (core/rez state :contestant (get-content state :remote2 0))
      (is (= 3 (get-counters (refresh lak) :power)) "Smartfabrics gained 1 power counter")
      (take-credits state :contestant)
      (card-ability state :contestant (refresh lak) 0)
      (prompt-select :contestant (find-card "Elective Region" (:hand (get-contestant))))
      (is (last-log-contains? state "Elective Region") "Revealed agenda")
      (is (zero? (get-counters (refresh lak) :power)) "Spent 3 power counters")
      (run-empty-server state "HQ")
      (prompt-choice :challenger "No action")
      (is (empty? (:scored (get-challenger))) "Steal prevented by Smartfabrics")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-empty-server state "HQ")
      (prompt-choice :challenger "Steal")
      (is (= 3 (:agenda-point (get-challenger))) "Challenger could steal on later turn"))))

(deftest launch-campaign
  ;; Launch Campaign
  (do-game
    (new-game (default-contestant ["Launch Campaign"])
              (default-challenger))
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (let [launch (get-content state :remote1 0)]
      (core/rez state :contestant launch)
      (is (= 4 (:credit (get-contestant))))
      (is (= 6 (get-counters (refresh launch) :credit)))
      (take-credits state :contestant 2)
      (take-credits state :challenger)
      (is (= 8 (:credit (get-contestant))))
      (is (= 4 (get-counters (refresh launch) :credit))))))

(deftest levy-university
  ;; Levy University
  (do-game
    (new-game (default-contestant ["Levy University" "Ice Wall" (qty "Fire Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant ["Levy University"])
    (play-from-hand state :contestant "Levy University" "New remote")
    (let [levy (get-content state :remote1 0)
          number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))]
      (core/rez state :contestant levy)
      (is (zero? (-> (get-contestant) :hand count)) "HQ should be empty")
      (let [clicks (:click (get-contestant))
            credits (:credit (get-contestant))]
        (card-ability state :contestant (refresh levy) 0)
        (prompt-card :contestant (find-card "Ice Wall" (:deck (get-contestant))))
        (is (= (- credits 1) (:credit (get-contestant))) "Levy University ability should cost 1 credit")
        (is (= (- clicks 1) (:click (get-contestant))) "Levy University ability should cost 1 click"))
      (is (= 1 (-> (get-contestant) :hand count)) "HQ should have 1 card")
      (is (= "Ice Wall" (-> (get-contestant) :hand first :title)) "HQ should contain Ice Wall")
      (is (< number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))) "Contestant should shuffle deck"))))

(deftest lily-lockwell
  ;; Lily Lockwell
  (do-game
    (new-game (default-contestant ["Lily Lockwell" "Beanstalk Royalties" (qty "Fire Wall" 10)])
              (default-challenger))
    (core/gain state :contestant :click 10)
    (starting-hand state :contestant ["Lily Lockwell" "Beanstalk Royalties"])
    (play-from-hand state :contestant "Lily Lockwell" "New remote")
    (core/gain state :challenger :tag 2)
    (let [lily (get-content state :remote1 0)
          clicks (:click (get-contestant))
          number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))
          hand (-> (get-contestant) :hand count)]
      (core/rez state :contestant lily)
      (is (= (+ 3 hand) (-> (get-contestant) :hand count)) "Rezzing Lily Lockwell should draw 3 cards")
      (core/move state :contestant (find-card "Beanstalk Royalties" (:hand (get-contestant))) :deck)
      (card-ability state :contestant (refresh lily) 0)
      (prompt-card :contestant (find-card "Beanstalk Royalties" (-> (get-contestant) :prompt first :choices)))
      (is (= "Beanstalk Royalties" (-> (get-contestant) :deck first :title)) "Beanstalk Royalties should be moved to top of R&D")
      (is (= 1 (:tag (get-challenger))) "Challenger should have 1 tag from Lily Lockwell ability")
      (is (= (- clicks 1) (:click (get-contestant))) "Lily Lockwell ability should cost 1 click")
      (is (< number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))) "Contestant should shuffle deck")
      (core/draw state :contestant)
      (card-ability state :contestant (refresh lily) 0)
      (prompt-choice :contestant "Cancel")
      (is (last-log-contains? state "did not find") "Lily Lockwell's ability didn't find an operation")
      (is (zero? (:tag (get-challenger))) "Challenger should have 0 tags from Lily Lockwell ability even when no operation found"))))

(deftest long-term-investment
  ;; Long-Term Investment
  (do-game
    (new-game (default-contestant ["Long-Term Investment"])
              (default-challenger))
    (play-from-hand state :contestant "Long-Term Investment" "New remote")
    (let [lti (get-content state :remote1 0)]
      (core/rez state :contestant lti)
      (dotimes [i 4]
        (is (= (* i 2) (get-counters (refresh lti) :credit)) "Long-Term Investement should gain 2 credits at start of turn")
        (take-credits state :contestant)
        (take-credits state :challenger))
      (is (= 8 (get-counters (refresh lti) :credit)) "Long-Term Investment should have 8 credit after 4 turns")
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant (refresh lti) 0)
        (prompt-choice :contestant 8)
        (is (= (+ credits 8) (:credit (get-contestant))) "Contestant should gain 8 credits from Long-Term Investment ability")))))

(deftest malia-z0l0k4
  ;; Malia Z0L0K4 - blank an installed non-virtual challenger radicle
  (do-game
   (new-game (default-contestant [(qty "Malia Z0L0K4" 2)
                            "Mausolus"])
             (default-challenger ["Rachel Beckman"
                              "Daily Casts"
                              "Rumor Mill"]))
   (play-from-hand state :contestant "Malia Z0L0K4" "New remote")
   (play-from-hand state :contestant "Malia Z0L0K4" "New remote")
   (play-from-hand state :contestant "Mausolus" "HQ")
   (take-credits state :contestant)
   (let [malia1 (get-content state :remote1 0)
         malia2 (get-content state :remote2 0)
         mausolus (get-character state :hq 0)]
     (play-from-hand state :challenger "Daily Casts")
     (take-credits state :challenger)
     (let [N (:credit (get-challenger))]
       (core/rez state :contestant malia1)
       (prompt-select :contestant (get-radicle state 0))
       (take-credits state :contestant)
       (is (= N (:credit (get-challenger))) "Daily casts did not trigger when blanked"))
     (take-credits state :challenger)
     (core/derez state :contestant malia1)
     (let [N (:credit (get-challenger))]
       (take-credits state :contestant)
       (is (= (+ N 2) (:credit (get-challenger))) "Daily casts triggers again when unblanked"))
     (play-from-hand state :challenger "Rachel Beckman")
     (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks after playing Beckman")
     (core/rez state :contestant malia1)
     (prompt-select :contestant (get-radicle state 1))
     (is (= 3 (:click (get-challenger))) "Challenger has 3 clicks after Beckman is blank")
     (core/derez state :contestant malia1)
     (is (= 4 (:click (get-challenger))) "Challenger has 4 clicks after Beckman is unblanked")
     (core/rez state :contestant malia1)
     (prompt-select :contestant (get-radicle state 1))
     (core/rez state :contestant mausolus)
     (card-subroutine state :contestant mausolus 2)
     (is (and (= 1 (:tag (get-challenger)))
              (zero? (count (:discard (get-challenger))))) "Challenger has 1 tag, but Rachel Beckman not discarded")
     (take-credits state :challenger)
     (is (zero? (count (:hand (get-contestant)))) "Malia is not in hand")
     (core/move-card state :contestant {:card malia1 :server "HQ"})
     (is (= 1 (count (:hand (get-contestant)))) "Malia is in hand")
     (is (= 1 (count (:discard (get-challenger)))) "Rachel Beckman got discarded on unblanking")
     (core/rez state :contestant malia2)
     (prompt-select :contestant (get-radicle state 0))
     (let [N (:credit (get-challenger))]
       (take-credits state :contestant)
       (is (= N (:credit (get-challenger))) "Daily casts is blank, so no drip")))
   (play-from-hand state :challenger "Rumor Mill")
   (take-credits state :challenger)
   (let [N (:credit (get-challenger))]
     (take-credits state :contestant)
     (is (= (+ N 2) (:credit (get-challenger)))))))

(deftest marilyn-campaign
  ;; Marilyn Campaign
  (do-game
    (new-game
      (default-contestant ["Marilyn Campaign"])
      (default-challenger))
    (play-from-hand state :contestant "Marilyn Campaign" "New remote")
    (let [marilyn (get-content state :remote1 0)]
      (core/rez state :contestant marilyn)
      (is (= 8 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should start with 8 credits")
      (is (zero? (-> (get-contestant) :deck count)) "R&D should be empty")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 6 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 4 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 2 (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (zero? (get-counters (refresh marilyn) :credit)) "Marilyn Campaign should lose 2 credits start of turn")
      (prompt-choice :contestant "Yes")
      (is (= 1 (-> (get-contestant) :hand count)) "HQ should have 1 card in it, after mandatory draw")
      (is (= "Marilyn Campaign" (-> (get-contestant) :hand first :title)) "Marilyn Campaign should be in HQ, after mandatory draw"))))

(deftest mark-yale
  ;; Mark Yale
  (do-game
    (new-game
      (default-contestant ["Mark Yale" "Project Atlas" (qty "Ice Wall" 10)])
      (default-challenger))
    (starting-hand state :contestant ["Mark Yale" "Project Atlas"])
    (core/gain state :contestant :credit 100 :click 100)
    (play-from-hand state :contestant "Mark Yale" "New remote")
    (play-from-hand state :contestant "Project Atlas" "New remote")
    (let [mark (get-content state :remote1 0)
          atlas (get-content state :remote2 0)]
      (core/rez state :contestant mark)
      (advance state atlas 5)
      (core/score state :contestant {:card (refresh atlas)}))
    (let [mark (get-content state :remote1 0)
          scored-atlas (get-scored state :contestant 0)
          credits (:credit (get-contestant))]
      (card-ability state :contestant mark 1)
      (prompt-select :contestant scored-atlas)
      (is (= (+ credits 3) (:credit (get-contestant))) "Mark Yale spending an agenda counter should gain 3 credits")
      (card-ability state :contestant scored-atlas 0)
      (prompt-card :contestant (find-card "Ice Wall" (:deck (get-contestant))))
      (is (= (+ credits 4) (:credit (get-contestant))) "Spending an agenda counter for another reason should gain 1 credit")
      (card-ability state :contestant mark 0)
      (is (= (+ credits 6) (:credit (get-contestant))) "Mark Yale discarding itself should gain 2 credits"))))

(deftest marked-accounts
  ;; Marked Accounts
  (do-game
    (new-game
      (default-contestant ["Marked Accounts"])
      (default-challenger))
    (play-from-hand state :contestant "Marked Accounts" "New remote")
    (let [ma (get-content state :remote1 0)]
      (core/rez state :contestant ma)
      (is (zero? (get-counters (refresh ma) :credit)) "Marked Accounts should start with 0 credits on it")
      (card-ability state :contestant ma 1)
      (is (= 3 (get-counters (refresh ma) :credit)) "Marked Accounts should gain 3 credits when ability is used")
      (take-credits state :contestant)
      (let [credits (:credit (get-contestant))]
        (take-credits state :challenger)
        (is (= (+ credits 1) (:credit (get-contestant))) "Should gain 1 credit at beginning of turn from Marked Accounts")))))

(deftest mca-austerity-policy
  (do-game
    (new-game
      (default-contestant ["MCA Austerity Policy"])
      (default-challenger))
    (play-from-hand state :contestant "MCA Austerity Policy" "New remote")
    (let [mca (get-content state :remote1 0)]
      (core/rez state :contestant mca)
      (card-ability state :contestant mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      ; once per turn only
      (card-ability state :contestant mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      (take-credits state :contestant)
      ; challenger loses a click
      (is (= 3 (:click (get-challenger))))
      (take-credits state :challenger)
      (card-ability state :contestant mca 0)
      (is (= 2 (get-counters (refresh mca) :power)))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (card-ability state :contestant mca 0)
      (is (= 3 (get-counters (refresh mca) :power)))
      ; Fire MCA
      (is (= 2 (:click (get-contestant))))
      (card-ability state :contestant (refresh mca) 1)
      (is (= 5 (:click (get-contestant)))))))

(deftest melange-mining-contestant.
  ;; Melange Mining Contestant.
  (do-game
    (new-game (default-contestant ["Melange Mining Contestant."])
              (default-challenger))
    (play-from-hand state :contestant "Melange Mining Contestant." "New remote")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (core/rez state :contestant (get-content state :remote1 0))
    (let [mmc (get-content state :remote1 0)
          credits (:credit (get-contestant))]
      (is (= 3 (:click (get-contestant))) "Contestant should have 3 clicks")
      (card-ability state :contestant mmc 0)
      (is (zero? (:click (get-contestant))) "Contestant should have 0 clicks after using Melange Mining Contestant ability")
      (is (= (+ credits 7) (:credit (get-contestant))) "Contestant should gain 7 credits from Melange Mining Contestant ability"))))

(deftest mental-health-clinic
  ;; Mental Health Clinic - Gain 1 credit when turn begins; Challenger max hand size increased by 1
  (do-game
    (new-game (default-contestant ["Mental Health Clinic"])
              (default-challenger))
    (play-from-hand state :contestant "Mental Health Clinic" "New remote")
    (let [mhc (get-content state :remote1 0)]
      (core/rez state :contestant mhc)
      (is (= 6 (core/hand-size state :challenger)) "Challenger max hand size increased by 1")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 8 (:credit (get-contestant))) "Gained 1 credit at start of turn"))))

(deftest mr.-stone
  ;; Mr Stone
  (do-game
    (new-game (default-contestant ["Mr. Stone"])
              (default-challenger))
    (play-from-hand state :contestant "Mr. Stone" "New remote")
    (let [stone (get-content state :remote1 0)]
      (core/rez state :contestant stone)
      (core/tag-challenger state :challenger 1)
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should take 1 meat damage from gaining 1 tag")
      (core/tag-challenger state :challenger 5)
      (is (= 2 (-> (get-challenger) :discard count)) "Challenger should take 1 meat damage from gaining 5 tags"))))

(deftest mumba-temple
  ;; Mumba Temple
  (do-game
    (new-game (default-contestant ["Mumba Temple"])
              (default-challenger))
    (play-from-hand state :contestant "Mumba Temple" "New remote")
    (let [mumba (get-content state :remote1 0)]
      (core/rez state :contestant mumba)
      (is (= 2 (get-counters (refresh mumba) :recurring)) "Should have 2 recurring credits"))))

(deftest mumbad-city-hall
  ;; Mumbad City Hall
  (do-game
    (new-game (default-contestant ["Mumbad City Hall"
                             "PAD Factory"
                             "Salem's Hospitality"])
              (default-challenger))
    (core/gain state :contestant :click 3 :credit 100)
    (starting-hand state :contestant ["Mumbad City Hall"])
    (play-from-hand state :contestant "Mumbad City Hall" "New remote")
    (let [mumbad (get-content state :remote1 0)]
      (core/rez state :contestant mumbad)
      (card-ability state :contestant mumbad 0)
      (prompt-card :contestant (find-card "PAD Factory" (:deck (get-contestant))))
      (prompt-choice :contestant "New remote")
      (is (= "PAD Factory" (:title (get-content state :remote2 0))))
      (card-ability state :contestant mumbad 0)
      (prompt-card :contestant (find-card "Salem's Hospitality" (:deck (get-contestant))))
      (prompt-choice :contestant "Sure Gamble")
      (is (= 3 (-> (get-challenger) :discard count)) "Challenger should have discarded all cards from Salem's Hospitality"))))

(deftest mumbad-construction-co.
  ;; Mumbad Construction Co.
  (do-game
    (new-game (default-contestant ["Mumbad Construction Co."
                             "Oaktown Renovation"])
              (default-challenger))
    (play-from-hand state :contestant "Mumbad Construction Co." "New remote")
    (play-from-hand state :contestant "Oaktown Renovation" "New remote")
    (let [mcc (get-content state :remote1 0)
          oak (get-content state :remote2 0)]
      (core/rez state :contestant mcc)
      (is (zero? (get-counters (refresh mcc) :advancement)) "Mumbad Construction Co should start with 0 counters")
      (is (zero? (get-counters (refresh oak) :advancement)) "Oaktown Renovation should start with 0 counters")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 1 (get-counters (refresh mcc) :advancement)) "Mumbad Construction Co should gain 1 counter at start of turn")
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant mcc 0)
        (prompt-select :contestant (refresh oak))
        (is (zero? (get-counters (refresh mcc) :advancement)) "Mumbad Construction Co should lose 1 counter when using ability")
        (is (= 1 (get-counters (refresh oak) :advancement)) "Oaktown Renovation should gain 1 counter from MCC ability")
        (is (= (- credits 2) (:credit (get-contestant))) "Mumbad Construction Co ability should cost 2[Credits]")))))

(deftest museum-of-history
  ;; Museum of History
  (do-game
    (new-game (default-contestant ["Museum of History" "Beanstalk Royalties"
                             (qty "Ice Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant ["Beanstalk Royalties" "Museum of History"])
    (play-from-hand state :contestant "Beanstalk Royalties")
    (play-from-hand state :contestant "Museum of History" "New remote")
    (let [museum (get-content state :remote1 0)
          number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))]
      (core/rez state :contestant museum)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (card-ability state :contestant museum 0)
      (prompt-select :contestant (find-card "Beanstalk Royalties" (:discard (get-contestant))))
      (is (< number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))) "Contestant should shuffle deck")
      (is (zero? (-> (get-contestant) :discard count)) "Archives should be empty after shuffling Beanstalk into R&D"))))

(deftest nasx
  ;; NASX
  (do-game
    (new-game (default-contestant ["NASX"])
              (default-challenger))
    (play-from-hand state :contestant "NASX" "New remote")
    (let [nasx (get-content state :remote1 0)]
      (core/rez state :contestant nasx)
      (take-credits state :contestant)
      (let [credits (:credit (get-contestant))]
        (take-credits state :challenger)
        (is (= (+ credits 1) (:credit (get-contestant))) "Contestant should gain 1 credit at start of turn from NASX"))
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant nasx 1)
        (is (= (- credits 1) (:credit (get-contestant))) "Contestant should spend 1 credit on NASX ability")
        (is (= 1 (get-counters (refresh nasx) :power)) "NASX should gain 1 power counter from spent credits"))
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant nasx 2)
        (is (= (- credits 2) (:credit (get-contestant))) "Contestant should spend 2 credit on NASX ability")
        (is (= 3 (get-counters (refresh nasx) :power)) "NASX should gain 2 power counter from spent credits"))
      (let [credits (:credit (get-contestant))
            counters (get-counters (refresh nasx) :power)]
        (card-ability state :contestant nasx 3)
        (is (= (+ credits (* 2 counters)) (:credit (get-contestant))) (str "Contestant should gain " (* 2 counters) " from NASX discard ability"))
        (is (= 1 (-> (get-contestant) :discard count)) "Contestant should discard NASX for ability")
        (is (= "NASX" (-> (get-contestant) :discard first :title)) "NASX should be in archives")))))

(deftest net-analytics
  ;; Draw a card when challenger avoids or removes 1 or more tags
  (do-game
    (new-game (default-contestant [(qty "Ghost Branch" 3) (qty "Net Analytics" 3)])
              (default-challenger [(qty "New Angeles City Hall" 3)]))
    (starting-hand state :contestant ["Net Analytics" "Ghost Branch"])
    (play-from-hand state :contestant "Ghost Branch" "New remote")
    (play-from-hand state :contestant "Net Analytics" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "New Angeles City Hall")
    (take-credits state :challenger)
    (let [gb (get-content state :remote1 0)
          net (get-content state :remote2 0)
          nach (get-radicle state 0)]
      (core/rez state :contestant (refresh net))
      (core/advance state :contestant {:card (refresh gb)})
      (is (= 1 (get-counters (refresh gb) :advancement)))
      (take-credits state :contestant)
      (is (= 1 (count (:hand (get-contestant)))) "Contestant hand size is 1 before run")
      (run-empty-server state "Server 1")
      (prompt-choice :contestant "Yes") ; Ghost Branch ability
      (card-ability state :challenger nach 0)
      (prompt-choice :challenger "Done")
      (prompt-choice :contestant "Yes") ; Draw from Net Analytics
      (prompt-choice :challenger "No action")
      (is (empty? (:prompt (get-challenger))) "Challenger waiting prompt is cleared")
      (is (zero? (:tag (get-challenger))) "Avoided 1 Ghost Branch tag")
      (is (= 2 (count (:hand (get-contestant)))) "Contestant draw from NA")
      ; tag removal
      (core/tag-challenger state :challenger 1)
      (prompt-choice :challenger "Done") ; Don't prevent the tag
      (core/remove-tag state :challenger 1)
      (prompt-choice :contestant "Yes") ; Draw from Net Analytics
      (is (= 3 (count (:hand (get-contestant)))) "Contestant draw from NA"))))

(deftest net-polcharacter
  ;; Net Polcharacter - Recurring credits equal to Challenger's link
  (do-game
    (new-game
      (default-contestant ["Net Police"])
      (make-deck "Sunny Lebeau: Security Specialist" ["Dyson Mem Chip"
                                                      "Access to Globalsec"]))
    (play-from-hand state :contestant "Net Police" "New remote")
    (is (= 2 (:link (get-challenger))))
    (let [netpol (get-content state :remote1 0)]
      (core/rez state :contestant netpol)
      (is (= 2 (get-counters (refresh netpol) :recurring)) "2 recurring for Challenger's 2 link")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Dyson Mem Chip")
      (take-credits state :challenger)
      (is (= 3 (get-counters (refresh netpol) :recurring)) "3 recurring for Challenger's 3 link")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Access to Globalsec")
      (take-credits state :challenger)
      (is (= 4 (get-counters (refresh netpol) :recurring)) "4 recurring for Challenger's 4 link"))))

(deftest news-team
  ;; News Team - on access take 2 tags or take as agenda worth -1
  (do-game
    (new-game (default-contestant [(qty "News Team" 3) "Blacklist"])
              (default-challenger))
    (discard-from-hand state :contestant "News Team")
    (play-from-hand state :contestant "Blacklist" "New remote")
    (take-credits state :contestant)
    (run-empty-server state :archives)
    (prompt-choice :challenger "Take 2 tags")
    (is (= 2 (:tag (get-challenger))) "Challenger has 2 tags")
    (run-empty-server state :archives)
    (prompt-choice :challenger "Add News Team to score area")
    (is (= 1 (count (:scored (get-challenger)))) "News Team added to Challenger score area")
    (discard-from-hand state :contestant "News Team")
    (core/rez state :contestant (get-content state :remote1 0))
    (run-empty-server state :archives)
    (prompt-choice :challenger "Add News Team to score area")
    (is (= 2 (count (:scored (get-challenger)))) "News Team added to Challenger score area with Blacklist rez")))

(deftest ngo-front
  ;; NGO Front - full test
  (do-game
    (new-game (default-contestant [(qty "NGO Front" 3)])
              (default-challenger))
    (core/gain state :contestant :click 3)
    (play-from-hand state :contestant "NGO Front" "New remote")
    (play-from-hand state :contestant "NGO Front" "New remote")
    (play-from-hand state :contestant "NGO Front" "New remote")
    (let [ngo1 (get-content state :remote1 0)
          ngo2 (get-content state :remote2 0)
          ngo3 (get-content state :remote3 0)]
      (core/advance state :contestant {:card ngo2})
      (core/advance state :contestant {:card (refresh ngo3)})
      (core/advance state :contestant {:card (refresh ngo3)})
      (core/rez state :contestant (refresh ngo1))
      (core/rez state :contestant (refresh ngo2))
      (core/rez state :contestant (refresh ngo3))
      (is (= 2 (:credit (get-contestant))) "Contestant at 2 credits")
      (card-ability state :contestant ngo1 1)
      (card-ability state :contestant ngo1 0)
      (is (= 2 (:credit (get-contestant))) "Contestant still 2 credits")
      (is (zero? (count (:discard (get-contestant)))) "Nothing discarded")
      (card-ability state :contestant ngo2 1)
      (is (= 2 (:credit (get-contestant))) "Contestant still 2 credits")
      (is (zero? (count (:discard (get-contestant)))) "Nothing discarded")
      (card-ability state :contestant ngo2 0)
      (is (= 7 (:credit (get-contestant))) "Contestant gained 5 credits")
      (is (= 1 (count (:discard (get-contestant)))) "1 NGO Front Discarded")
      (card-ability state :contestant ngo3 1)
      (is (= 15 (:credit (get-contestant))) "Contestant gained 8 credits")
      (is (= 2 (count (:discard (get-contestant)))) "2 NGO Front Discarded"))))

(deftest open-forum
  ;; Open Forum
  (do-game
    (new-game (default-contestant ["Open Forum" "Ice Wall" "Fire Wall" "Enigma"])
              (default-challenger))
    (play-from-hand state :contestant "Open Forum" "New remote")
    (core/move state :contestant (find-card "Ice Wall" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Fire Wall" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Enigma" (:hand (get-contestant))) :deck)
    (is (-> @state :contestant :hand count zero?))
    (let [forum (get-content state :remote1 0)]
      (core/rez state :contestant forum)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (last-log-contains? state "Fire Wall") "Mandatory Draw was Ice Wall, Open Forum should reveal Fire Wall")
      (prompt-select :contestant (find-card "Ice Wall" (:hand (get-contestant))))
      (is (= 2 (-> @state :contestant :deck count)) "Two cards should remain in R&D")
      (is (= "Ice Wall" (-> @state :contestant :deck first :title)) "Top card in R&D should be Ice Wall"))))

(deftest pad-campaign
  ;; PAD Campaign
  (do-game
    (new-game (default-contestant ["PAD Campaign"])
              (default-challenger))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (let [pad (get-content state :remote1 0)]
      (core/rez state :contestant pad)
      (take-credits state :contestant)
      (let [credits (:credit (get-contestant))]
        (take-credits state :challenger)
        (is (= (+ 1 credits) (:credit (get-contestant))) "Should gain 1 credit at start of turn from PAD Campaign")))))

(deftest pad-factory
  ;; PAD Factory - Click to place an advancement, cannot score target until next turn
  (do-game
    (new-game (default-contestant ["PAD Factory" "15 Minutes"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "PAD Factory" "New remote")
    (play-from-hand state :contestant "15 Minutes" "New remote")
    (let [pf (get-content state :remote1 0)
          fif (get-content state :remote2 0)]
      (core/rez state :contestant pf)
      (card-ability state :contestant (refresh pf) 0)
      (prompt-select :contestant fif)
      (card-ability state :contestant (refresh pf) 0)
      (prompt-select :contestant (refresh fif))
      (is (zero? (:click (get-contestant))) "Spent 2 clicks using PAD Factory twcharacter")
      (is (= 2 (get-counters (refresh fif) :advancement)) "Agenda has 2 advancements")
      (core/score state :contestant {:card (refresh fif)})
      (is (empty? (:scored (get-contestant))) "Prevented from scoring this turn")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/score state :contestant {:card (refresh fif)})
      (is (= 1 (count (:scored (get-contestant)))) "Scored agenda"))))

(deftest palana-agroplex
  ;; Pālanā Agroplex - Both players draw 1 at start of Contestant turn
  (do-game
    (new-game (default-contestant ["Pālanā Agroplex" (qty "Hedge Fund" 3)])
              (default-challenger))
    (starting-hand state :contestant ["Pālanā Agroplex"])
    (starting-hand state :challenger ["Sure Gamble"])
    (play-from-hand state :contestant "Pālanā Agroplex" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (core/end-phase-12 state :contestant nil)
    (is (= 2 (count (:hand (get-contestant)))) "Contestant drew 1 from Agroplex")
    (is (= 2 (count (:hand (get-challenger)))) "Challenger drew 1 from Agroplex")))

(deftest personalized-portal
  ;; Personalized Portal - on contestant turn start, force the challenger to draw 1 card
  ;; and then gain 1 credit for every 2 cards in the challengers hand
  (do-game
    (new-game (default-contestant ["Personalized Portal"])
              (default-challenger [(qty "Daily Casts" 3) (qty "Dyson Mem Chip" 3)]))
    (play-from-hand state :contestant "Personalized Portal" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (starting-hand state :challenger [])
    (is (empty? (:hand (get-challenger))) "Challenger's grip is empty to start")
    (is (= 4 (:credit (get-contestant))) "Contestant starts with 4 credits")
    (take-credits state :challenger)
    (is (= 1 (count (:hand (get-challenger)))) "Challenger drew 1 card")
    (is (= 4 (:credit (get-contestant))) "Contestant gained 0 credits")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 2 (count (:hand (get-challenger)))) "Challenger drew 1 card")
    (is (= 8 (:credit (get-contestant))) "Contestant gained 1 credit")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 3 (count (:hand (get-challenger)))) "Challenger drew 1 card")
    (is (= 12 (:credit (get-contestant))) "Contestant gained 1 credit")))

(deftest plan-b
  ;; Plan B - score agenda with adv cost <= # of adv counters
  (do-game
    (new-game (default-contestant ["Plan B"
                             "Braintrust"
                             "The Future Perfect"
                             "Mushin No Shin"])
              (default-challenger))
    (play-from-hand state :contestant "Mushin No Shin")
    (prompt-select :contestant (find-card "Plan B" (:hand (get-contestant))))
    (take-credits state :contestant)
    (run-empty-server state :remote1)
    ;; prompt for contestant to use Plan B
    (prompt-choice :contestant "Yes")
    ;; Pick TFP, does not score
    (prompt-select :contestant (find-card "The Future Perfect" (:hand (get-contestant))))
    (is (find-card "The Future Perfect" (:hand (get-contestant))) "TFP is not scored")
    ;; Pick Brain Trust, scores
    (prompt-select :contestant (find-card "Braintrust" (:hand (get-contestant))))
    (is (find-card "Braintrust" (:scored (get-contestant))) "Braintrust is scored")))

(deftest political-dealings
  ;; Political Dealings
  (testing "Full test"
    (do-game
      (new-game (default-contestant ["Political Dealings" "Medical Breakthrough" "Oaktown Renovation"])
                (default-challenger))
      (core/move state :contestant (find-card "Medical Breakthrough" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Oaktown Renovation" (:hand (get-contestant))) :deck)
      (play-from-hand state :contestant "Political Dealings" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      ;; Install Medical Breakthrough
      (core/draw state :contestant)
      (prompt-choice :contestant "Yes")
      (prompt-choice :contestant "New remote")
      (is (= "Medical Breakthrough" (:title (get-content state :remote2 0)))
          "Medical Breakthrough installed by Political Dealings")
      ;; Install Oaktown Renovation
      (core/draw state :contestant)
      (prompt-choice :contestant "Yes")
      (prompt-choice :contestant "New remote")
      (is (= "Oaktown Renovation" (:title (get-content state :remote3 0)))
          "Oaktown Renovation installed by Political Dealings")
      (is (:rezzed (get-content state :remote3 0))
          "Oaktown Renovation installed face up")))
  (testing "Daily Business Show interaction - Draw 2 agendas, install both of them but return 1 to bottom of R&D"
    (do-game
      (new-game (default-contestant ["Political Dealings" "Daily Business Show" "Turtlebacks"
                               "Breaking News" "Project Beale"])
                (default-challenger))
      (starting-hand state :contestant ["Political Dealings" "Daily Business Show" "Turtlebacks"])
      (core/gain state :contestant :credit 3)
      (play-from-hand state :contestant "Political Dealings" "New remote")
      (play-from-hand state :contestant "Daily Business Show" "New remote")
      (play-from-hand state :contestant "Turtlebacks" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (core/rez state :contestant (get-content state :remote2 0))
      (core/rez state :contestant (get-content state :remote3 0))
      (take-credits state :contestant)
      (is (zero? (count (:hand (get-contestant)))))
      (let [agenda1 (first (:deck (get-contestant)))
            agenda2 (second (:deck (get-contestant)))]
        (take-credits state :challenger)
        ;; Install first agenda
        (is (= 2 (count (:hand (get-contestant)))))
        (is (zero? (:credit (get-contestant))))
        (prompt-choice :contestant "Yes")
        (prompt-choice :contestant "New remote")
        (is (= (:cid agenda1) (:cid (get-content state :remote4 0))))
        (is (= 1 (:credit (get-contestant))) "Turtlebacks triggered")
        ;; Install second agenda
        (prompt-choice :contestant "Yes")
        (prompt-choice :contestant "New remote")
        (is (= (:cid agenda2) (:cid (get-content state :remote5 0))))
        (is (= 2 (:credit (get-contestant))) "Turtlebacks triggered")
        ;; DBS - put first agenda at bottom of R&D
        (prompt-select :contestant (get-content state :remote4 0))
        (is (zero? (count (:hand (get-contestant)))))
        (is (= (:cid agenda1) (:cid (last (:deck (get-contestant))))))))))

(deftest primary-transmission-dish
  ;; Primary Transmission Dish
  (do-game
    (new-game (default-contestant ["Primary Transmission Dish"])
              (default-challenger))
    (play-from-hand state :contestant "Primary Transmission Dish" "New remote")
    (let [dish (get-content state :remote1 0)]
      (core/rez state :contestant dish)
      (is (= 3 (get-counters (refresh dish) :recurring)) "Should have 3 recurring credits"))))

(deftest private-contracts
  ;; Private Contracts
  (do-game
    (new-game (default-contestant ["Private Contracts"])
              (default-challenger))
    (play-from-hand state :contestant "Private Contracts" "New remote")
    (let [pri (get-content state :remote1 0)]
      (core/rez state :contestant pri)
      (is (= 14 (get-counters (refresh pri) :credit)) "Should start with 14 credits")
      (is (zero? (-> (get-contestant) :discard count)) "Contestant should have 0 cards in Archives")
      (core/gain state :contestant :click 7)
      (core/lose state :contestant :credit 2)
      (dotimes [_ 7]
        (card-ability state :contestant pri 0))
      (is (= 1 (-> (get-contestant) :discard count)) "Private Contracts should be in discard")
      (is (= 14 (:credit (get-contestant))) "Contestant should now have 14 credits"))))

(deftest project-junebug
  ;; Project Junebug
  (do-game
    (new-game (default-contestant ["Project Junebug"])
              (default-challenger [(qty "Sure Gamble" 100)]))
    (play-from-hand state :contestant "Project Junebug" "New remote")
    (advance state (get-content state :remote1 0) 2)
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (let [credits (:credit (get-contestant))]
      (prompt-choice :contestant "Yes")
      (is (= (- credits 1) (:credit (get-contestant))) "Contestant should pay 1 for Project Junebug ability")
      (is (= 4 (-> (get-challenger) :discard count)) "Project Junebug should do 4 net damage"))))

(deftest psychic-field
  (testing "Basic test"
    ;; Psychic Field - Do 1 net damage for every card in Challenger's hand when accessed/exposed
    (do-game
      (new-game (default-contestant [(qty "Psychic Field" 2)])
                (default-challenger [(qty "Infiltration" 3) (qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Psychic Field" "New remote")
      (play-from-hand state :contestant "Psychic Field" "New remote")
      (let [psyf1 (get-content state :remote1 0)
            psyf2 (get-content state :remote2 0)]
        (take-credits state :contestant)
        (starting-hand state :challenger ["Infiltration" "Sure Gamble" "Sure Gamble"])
        (play-from-hand state :challenger "Infiltration")
        (prompt-choice :challenger "Expose a card")
        (prompt-select :challenger psyf1)
        (is (= 2 (count (:hand (get-challenger)))))
        (prompt-choice :contestant "2 [Credits]")
        (prompt-choice :challenger "0 [Credits]")
        (is (= 3 (count (:discard (get-challenger)))) "Suffered 2 net damage on expose and psi loss")
        (core/gain state :challenger :click 3)
        (core/draw state :challenger 3)
        (is (= 3 (count (:hand (get-challenger)))))
        (run-empty-server state :remote2)
        (prompt-choice :contestant "1 [Credits]")
        (prompt-choice :challenger "0 [Credits]")
        (is (= 6 (count (:discard (get-challenger)))) "Suffered 3 net damage on access and psi loss"))))
  (testing "when in Archives. #1965"
    (do-game
      (new-game (default-contestant [(qty "Psychic Field" 2) (qty "Shock!" 2) (qty "Clone Retirement" 2)])
                (default-challenger))
      (discard-from-hand state :contestant "Psychic Field")
      (discard-from-hand state :contestant "Shock!")
      (discard-from-hand state :contestant "Clone Retirement")
      (take-credits state :contestant)
      ;; Challenger run on archives to trigger access choice
      (run-empty-server state :archives)
      (is (not-any? #{"Psychic Field"} (-> @state :challenger :prompt first :choices))
          "Psychic Field is not a choice to access in Archives")))
  (testing "Interaction with Neutralize All Threats and Hostile Infrastructure, #1208"
    (do-game
      (new-game (default-contestant [(qty "Psychic Field" 3) (qty "Hostile Infrastructure" 3)])
                (default-challenger ["Neutralize All Threats" (qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Psychic Field" "New remote")
      (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
      (core/rez state :contestant (get-content state :remote2 0))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Neutralize All Threats")
      (run-empty-server state :remote1)
      (prompt-choice :contestant "0 [Credits]")
      (prompt-choice :challenger "1 [Credits]")
      (prompt-choice-partial :challenger "Pay")
      (is (not (get-content state :remote1)) "Psychic Field discarded by Neutralize All Threats")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest public-support
  ;; Public support scoring and discarding
  ;; TODO could also test for NOT triggering "when scored" events
  (do-game
    (new-game (default-contestant [(qty "Public Support" 2)])
              (default-challenger))
    ;; Contestant turn 1, install and rez public supports
    (play-from-hand state :contestant "Public Support" "New remote")
    (play-from-hand state :contestant "Public Support" "New remote")
    (let [publics1 (get-content state :remote1 0)
          publics2 (get-content state :remote2 0)]
      (core/rez state :contestant (refresh publics1))
      (core/rez state :contestant (refresh publics2))
      (take-credits state :contestant)
      ;; Challenger turn 1, creds
      (is (= 2 (:credit (get-contestant))))
      (is (= 3 (get-counters (refresh publics1) :power)))
      (take-credits state :challenger)
      ;; Contestant turn 2, creds, check if supports are ticking
      (is (= 2 (get-counters (refresh publics1) :power)))
      (is (zero? (:agenda-point (get-contestant))))
      (is (nil? (:agendapoints (refresh publics1))))
      (take-credits state :contestant)
      ;; Challenger turn 2, run and discard publics2
      (run-empty-server state "Server 2")
      (prompt-choice-partial :challenger "Pay") ; pay to discard
      (is (= 5 (:credit (get-challenger))))
      (take-credits state :challenger)
      ;; Contestant turn 3, check how publics1 is doing
      (is (= 1 (get-counters (refresh publics1) :power)))
      (is (zero? (:agenda-point (get-contestant))))
      (take-credits state :contestant)
      ;; Challenger turn 3, boring
      (take-credits state :challenger)
      ;; Contestant turn 4, check the delicious agenda points
      (let [scored-pub (get-scored state :contestant 0)]
        (is (= 1 (:agenda-point (get-contestant))) "Gained 1 agenda point")
        (is (= "Public Support" (:title scored-pub)))
        (is (= 1 (:agendapoints scored-pub)))))))

(deftest quarantine-system
  ;; Forfeit agenda to rez up to 3 Character with 2 credit discount per agenda point
  (do-game
    (new-game
      (default-contestant [(qty "Chiyashi" 3) "Quarantine System" "Project Beale"])
      (default-challenger))
    (core/gain state :contestant :credit 100)
    (core/gain state :contestant :click 100)
    (play-from-hand state :contestant "Chiyashi" "HQ")
    (play-from-hand state :contestant "Chiyashi" "HQ")
    (play-from-hand state :contestant "Chiyashi" "HQ")
    (play-from-hand state :contestant "Quarantine System" "New remote")
    (play-from-hand state :contestant "Project Beale" "New remote")
    (is (= 102 (:credit (get-contestant))) "Contestant has 102 creds")
    (let [ch1 (get-character state :hq 0)
          ch2 (get-character state :hq 1)
          ch3 (get-character state :hq 2)
          qs (get-content state :remote1 0)
          beale (get-content state :remote2 0)]
      (core/rez state :contestant qs)
      (card-ability state :contestant qs 0)
      (is (empty? (:prompt (get-contestant))) "No prompt to rez Character")
      (score-agenda state :contestant beale)
      ; 1 on rez
      (is (= 101 (:credit (get-contestant))) "Contestant has 101 creds")
      (card-ability state :contestant qs 0)
      (prompt-select :contestant (get-scored state :contestant 0))
      (prompt-select :contestant ch1)
      (prompt-select :contestant ch2)
      (prompt-select :contestant ch3)
      ; pay 8 per Chiyashi - 24 total
      (is (= 77 (:credit (get-contestant))) "Contestant has 77 creds")
      (is (empty? (:prompt (get-contestant))) "No prompt to rez Character"))))

(deftest raman-rai
  ;; Raman Rai
  (do-game
    (new-game (default-contestant ["Raman Rai" "Ice Wall" "Fire Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Raman Rai" "New remote")
    (let [raman (get-content state :remote1 0)]
      (core/move state :contestant (find-card "Ice Wall" (:hand (get-contestant))) :deck)
      (discard-from-hand state :contestant "Fire Wall")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (card-ability state :contestant raman 0)
      (prompt-select :contestant (find-card "Ice Wall" (:hand (get-contestant))))
      (prompt-select :contestant (find-card "Fire Wall" (:discard (get-contestant))))
      (is (= "Fire Wall" (-> (get-contestant) :hand first :title)))
      (is (= "Ice Wall" (-> (get-contestant) :discard first :title))))))

(deftest rashida-jaheem
  ;; Rashida Jaheem
  (testing "when there are enough cards in R&D"
    (do-game
      (new-game (default-contestant ["Rashida Jaheem" (qty "Hedge Fund" 3)])
                (default-challenger))
      (starting-hand state :contestant ["Rashida Jaheem"])
      (play-from-hand state :contestant "Rashida Jaheem" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (let [credits (:credit (get-contestant))
            cards (count (:hand (get-contestant)))
            rj (get-content state :remote1 0)]
        (card-ability state :contestant rj 0)
        (prompt-choice :contestant "Yes")
        (is (= (+ 3 credits) (:credit (get-contestant))))
        (is (= (+ 3 cards) (count (:hand (get-contestant))))))))
  (testing "when there aren't enough cards in R&D"
    (do-game
      (new-game (default-contestant ["Rashida Jaheem" (qty "Hedge Fund" 4)])
                (default-challenger))
      (starting-hand state :contestant ["Rashida Jaheem"])
      (play-from-hand state :contestant "Rashida Jaheem" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (core/draw state :contestant)
      (core/draw state :contestant)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (let [credits (:credit (get-contestant))
            cards (count (:hand (get-contestant)))
            rj (get-content state :remote1 0)]
        (card-ability state :contestant rj 0)
        (prompt-choice :contestant "Yes")
        (is (= (+ 3 credits) (:credit (get-contestant))))
        (is (= (+ 2 cards) (count (:hand (get-contestant)))))
        (is (= :challenger (:winner @state)) "Challenger wins")))))

(deftest reality-threedee
  ;; Reality Threedee - Take 1 bad pub on rez; gain 1c at turn start (2c if Challenger tagged)
  (do-game
    (new-game (default-contestant ["Reality Threedee"])
              (default-challenger))
    (play-from-hand state :contestant "Reality Threedee" "New remote")
    (let [r3d (get-content state :remote1 0)]
      (core/rez state :contestant r3d)
      (is (= 1 (:bad-publicity (get-contestant))) "Took 1 bad pub on rez")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 8 (:credit (get-contestant))) "Gained 1 credit")
      (take-credits state :contestant)
      (core/gain state :challenger :tag 1)
      (take-credits state :challenger)
      (is (= 13 (:credit (get-contestant))) "Gained 2 credits because Challenger is tagged"))))

(deftest reconstruction-contract
  ;; Reconstruction Contract - place advancement token when challenger takes meat damage
  (do-game
    (new-game (default-contestant ["Reconstruction Contract" "Scorched Earth" "Pup"])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Imp" 3)]))
    (core/gain state :challenger :tag 1)
    (core/gain state :contestant :credit 5)
    (starting-hand state :challenger ["Sure Gamble" "Sure Gamble" "Sure Gamble" "Imp" "Imp"])
    (play-from-hand state :contestant "Reconstruction Contract" "New remote")
    (let [rc (get-content state :remote1 0)]
      (core/rez state :contestant (refresh rc))
      (play-from-hand state :contestant "Scorched Earth")
      (is (= 4 (count (:discard (get-challenger)))))
      (is (= 1 (get-counters (refresh rc) :advancement)) "Reconstruction Contract has 1 advancement token")
      (starting-hand state :challenger ["Imp" "Imp"])
      (play-from-hand state :contestant "Pup" "HQ")
      (core/rez state :contestant (get-character state :hq 0))
      (card-subroutine state :contestant (get-character state :hq 0) 0)
      (is (= 5 (count (:discard (get-challenger)))))
      (is (= 1 (get-counters (refresh rc) :advancement)) "Reconstruction Contract doesn't get advancement token for net damage"))))

(deftest reversed-accounts
  ;; Reversed Accounts - Discard to make Challenger lose 4 credits per advancement
  (do-game
    (new-game (default-contestant ["Reversed Accounts"])
              (default-challenger))
    (play-from-hand state :contestant "Reversed Accounts" "New remote")
    (let [rev (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh rev)})
      (core/advance state :contestant {:card (refresh rev)})
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sure Gamble")
      (play-from-hand state :challenger "Sure Gamble")
      (play-from-hand state :challenger "Sure Gamble")
      (take-credits state :challenger)
      (is (= 18 (:credit (get-challenger))))
      (core/advance state :contestant {:card (refresh rev)})
      (core/advance state :contestant {:card (refresh rev)})
      (is (= 4 (get-counters (refresh rev) :advancement)))
      (core/rez state :contestant (refresh rev))
      (card-ability state :contestant rev 0)
      (is (= 1 (count (:discard (get-contestant)))) "Reversed Accounts discarded")
      (is (= 2 (:credit (get-challenger))) "Challenger lost 16 credits"))))

(deftest rex-campaign
  ;; Rex Campaign
  (testing "Gain 5 credits"
    (do-game
      (new-game (default-contestant ["Rex Campaign"])
                (default-challenger))
      (play-from-hand state :contestant "Rex Campaign" "New remote")
      (let [rex (get-content state :remote1 0)]
        (core/rez state :contestant rex)
        (is (= 3 (get-counters (refresh rex) :power)))
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (= 2 (get-counters (refresh rex) :power)))
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (= 1 (get-counters (refresh rex) :power)))
        (take-credits state :contestant)
        (take-credits state :challenger)
        (let [credits (:credit (get-contestant))]
          (is (zero? (get-counters (refresh rex) :power)))
          (prompt-choice-partial :contestant "Gain")
          (is (= (+ 5 credits) (:credit (get-contestant))))
          (is (= "Rex Campaign" (-> (get-contestant) :discard first :title)))))))
  (testing "Lose 1 bad publicity"
    (do-game
      (new-game (default-contestant ["Rex Campaign"])
                (default-challenger))
      (core/gain-bad-publicity state :contestant 1)
      (play-from-hand state :contestant "Rex Campaign" "New remote")
      (let [rex (get-content state :remote1 0)]
        (core/rez state :contestant rex)
        (is (= 3 (get-counters (refresh rex) :power)))
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (= 2 (get-counters (refresh rex) :power)))
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (= 1 (get-counters (refresh rex) :power)))
        (take-credits state :contestant)
        (take-credits state :challenger)
        (is (zero? (get-counters (refresh rex) :power)))
        (prompt-choice-partial :contestant "Remove")
        (is (zero? (:bad-publicity (get-contestant))) "Should not have the same amount of bad publicity")
        (is (= "Rex Campaign" (-> (get-contestant) :discard first :title)))))))

(deftest ronald-five
  ;; Ronald Five - Challenger loses a click every time they discard a Contestant card
  (do-game
    (new-game (default-contestant ["Ronald Five" "Melange Mining Contestant."])
              (default-challenger))
    (play-from-hand state :contestant "Ronald Five" "New remote")
    (play-from-hand state :contestant "Melange Mining Contestant." "New remote")
    (take-credits state :contestant)
    (core/rez state :contestant (get-content state :remote1 0))
    (run-empty-server state :remote2)
    (prompt-choice-partial :challenger "Pay") ; discard MMC
    (is (= 2 (:click (get-challenger))) "Lost 1 click")
    (run-empty-server state :remote1)
    (prompt-choice-partial :challenger "Pay") ; discard Ronald Five
    (is (zero? (:click (get-challenger))) "Lost 1 click")))

(deftest ronin
  ;; Ronin - Click-discard to do 3 net damage when it has 4 or more advancements
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Ronin" "Mushin No Shin"])
                (default-challenger))
      (play-from-hand state :contestant "Mushin No Shin")
      (prompt-select :contestant (find-card "Ronin" (:hand (get-contestant))))
      (let [ron (get-content state :remote1 0)]
        (is (= 3 (get-counters (refresh ron) :advancement)))
        (core/rez state :contestant (refresh ron))
        (card-ability state :contestant ron 0)
        (is (= 3 (count (:hand (get-challenger)))) "Ronin ability didn't fire with only 3 advancements")
        (take-credits state :contestant)
        (take-credits state :challenger)
        (core/advance state :contestant {:card (refresh ron)})
        (is (= 4 (get-counters (refresh ron) :advancement)))
        (card-ability state :contestant ron 0)
        (is (= 3 (count (:discard (get-challenger)))) "Ronin did 3 net damage")
        (is (= 2 (count (:discard (get-contestant)))) "Ronin discarded"))))
  (testing "doesn't fire (or crash) if no advance counters"
    (do-game
      (new-game (default-contestant ["Ronin"])
                (default-challenger))
      (play-from-hand state :contestant "Ronin" "New remote")
      (let [ron (get-content state :remote1 0)]
        (is (zero? (get-counters (refresh ron) :advancement)) "Ronin starts with no counters")
        (core/rez state :contestant (refresh ron))
        (card-ability state :contestant (refresh ron) 0)
        (is (zero? (get-counters (refresh ron) :advancement)) "Ronin didn't gain counters")
        (is (= 3 (count (:hand (get-challenger)))) "Ronin ability didn't fire with 0 advancements")))))

(deftest sandburg
  ;; Sandburg - +1 strength to all Character for every 5c when Contestant has over 10c
  (do-game
    (new-game (default-contestant ["Sandburg" (qty "Ice Wall" 2) (qty "Hedge Fund" 3)])
              (default-challenger))
    (core/gain state :contestant :click 3 :credit 3)
    (play-from-hand state :contestant "Sandburg" "New remote")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (let [sb (get-content state :remote1 0)
          iwall1 (get-character state :hq 0)
          iwall2 (get-character state :rd 0)]
      (core/rez state :contestant iwall1)
      (core/rez state :contestant iwall2)
      (core/rez state :contestant sb)
      (is (= 6 (:credit (get-contestant))))
      (play-from-hand state :contestant "Hedge Fund")
      (is (= 10 (:credit (get-contestant))))
      (is (= 3 (:current-strength (refresh iwall1))) "Strength boosted by 2")
      (is (= 3 (:current-strength (refresh iwall2))) "Strength boosted by 2")
      (play-from-hand state :contestant "Hedge Fund")
      (play-from-hand state :contestant "Hedge Fund")
      (is (= 18 (:credit (get-contestant))))
      (is (= 4 (:current-strength (refresh iwall1))) "Strength boosted by 3")
      (is (= 4 (:current-strength (refresh iwall2))) "Strength boosted by 3")
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-choice-partial :challenger "Pay")
      (is (= 1 (:current-strength (refresh iwall1))) "Strength back to default")
      (is (= 1 (:current-strength (refresh iwall2))) "Strength back to default"))))

(deftest sealed-vault
  ;; Sealed Vault - Store credits for 1c, retrieve credits by discarding or spending click
  (do-game
    (new-game (default-contestant ["Sealed Vault" "Hedge Fund"])
              (default-challenger))
    (play-from-hand state :contestant "Sealed Vault" "New remote")
    (play-from-hand state :contestant "Hedge Fund")
    (let [sv (get-content state :remote1 0)]
      (core/rez state :contestant sv)
      (card-ability state :contestant sv 0)
      (prompt-choice :contestant 8)
      (is (= 8 (get-counters (refresh sv) :credit)) "8 credits stored on Sealed Vault")
      (is (zero? (:credit (get-contestant))))
      (card-ability state :contestant sv 1)
      (prompt-choice :contestant 8)
      (is (zero? (get-counters (refresh sv) :credit)) "Credits removed from Sealed Vault")
      (is (= 8 (:credit (get-contestant))))
      (is (zero? (:click (get-contestant))) "Spent a click")
      (card-ability state :contestant sv 0)
      (prompt-choice :contestant 7)
      (is (= 7 (get-counters (refresh sv) :credit)) "7 credits stored on Sealed Vault")
      (is (zero? (:credit (get-contestant))))
      (card-ability state :contestant sv 2)
      (prompt-choice :contestant 7)
      (is (= 7 (:credit (get-contestant))))
      (is (= 2 (count (:discard (get-contestant)))) "Sealed Vault discarded"))))

(deftest security-subcontract
  ;; Security Subcontract
  (do-game
    (new-game (default-contestant ["Security Subcontract" "Ice Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Security Subcontract" "New remote")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [ss (get-content state :remote1 0)
          iw (get-character state :hq 0)]
      (core/rez state :contestant ss)
      (core/rez state :contestant iw)
      (card-ability state :contestant ss 0)
      (let [credits (:credit (get-contestant))
            clicks (:click (get-contestant))]
        (prompt-select :contestant iw)
        (is (= (+ credits 4) (:credit (get-contestant))) "Contestant should gain 4 from Security Subcontract ability")
        (is (= "Ice Wall" (-> (get-contestant) :discard first :title)) "Ice Wall should be in Archives from Security Subcontract ability")
        (is (= (- clicks 1) (:click (get-contestant))) "Contestant should lose 1 click from Security Subcontract ability")))))

(deftest sensie-actors-union
  ;; Sensie Actors Union
  (do-game
    (new-game (default-contestant ["Sensie Actors Union" "Ronin" (qty "Ice Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant ["Sensie Actors Union" "Ronin"])
    (core/move state :contestant (find-card "Ronin" (:hand (get-contestant))) :deck {:front true})
    (play-from-hand state :contestant "Sensie Actors Union" "New remote")
    (let [sau (get-content state :remote1 0)]
      (core/rez state :contestant sau)
      (take-credits state :contestant)
      (is (zero? (count (:hand (get-contestant)))) "Contestant should have no cards in hand before starting turn")
      (take-credits state :challenger)
      (is (:contestant-phase-12 @state) "Contestant is in Step 1.2")
      (card-ability state :contestant sau 0)
      (is (= 3 (count (:hand (get-contestant)))) "Contestant should draw 3 cards from Sensie Actors Union's ability")
      (prompt-select :contestant (find-card "Ronin" (:hand (get-contestant))))
      (is (= "Ronin" (-> (get-contestant) :deck last :title)) "Ronin should be on bottom of deck")
      (core/end-phase-12 state :contestant nil)
      (is (= 3 (count (:hand (get-contestant)))) "Contestant should have 3 cards in hand after putting one on bottom of R&D and mandatory draw")
      (play-from-hand state :contestant "Ice Wall" "Server 1")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (nil? (:contestant-phase-12 @state)) "Sensie Actors Union doesn't trigger if protected by character"))))

(deftest server-diagnostics
  ;; Server Diagnostics - Gain 2c when turn begins; discarded when Character is installed
  (do-game
    (new-game (default-contestant ["Server Diagnostics" "Pup"
                             "Launch Campaign"])
              (default-challenger))
    (play-from-hand state :contestant "Server Diagnostics" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (is (= 1 (count (get-content state :remote1))) "Non-Character install didn't discard Serv Diag")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 5 (:credit (get-contestant))) "Gained 2c at start of turn")
    (play-from-hand state :contestant "Pup" "HQ")
    (is (= 1 (count (:discard (get-contestant)))) "Server Diagnostics discarded by Character install")))

(deftest shannon-claire
  ;; Shannon Claire
  (do-game
    (new-game (default-contestant ["Shannon Claire" "Hostile Takeover" "Ronin" (qty "Ice Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant ["Shannon Claire" "Ronin"])
    (core/move state :contestant (find-card "Ronin" (:hand (get-contestant))) :deck)
    (play-from-hand state :contestant "Shannon Claire" "New remote")
    (let [shannon (get-content state :remote1 0)]
      (core/rez state :contestant shannon)
      (is (zero? (count (:hand (get-contestant)))) "Contestant should have 0 cards in hand to start")
      (card-ability state :contestant shannon 0)
      (is (= "Ronin" (-> (get-contestant) :hand first :title)) "Contestant should draw Ronin with Shannon's click ability")
      (let [number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))]
        (card-ability state :contestant shannon 1)
        (prompt-card :contestant (find-card "Hostile Takeover" (:deck (get-contestant))))
        (is (= "Hostile Takeover" (-> (get-contestant) :deck last :title))
            "Agenda selected with Shannon's R&D ability should be on bottom of deck")
        (is (< number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))) "Searching R&D should shuffle deck")))
    (core/move state :contestant (find-card "Hostile Takeover" (:deck (get-contestant))) :discard)
    (core/move state :contestant (find-card "Shannon Claire" (:discard (get-contestant))) :hand)
    (play-from-hand state :contestant "Shannon Claire" "New remote")
    (let [shannon (get-content state :remote2 0)
          number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))]
      (core/rez state :contestant shannon)
      (card-ability state :contestant shannon 2)
      (prompt-card :contestant (find-card "Hostile Takeover" (:discard (get-contestant))))
      (is (= "Hostile Takeover" (-> (get-contestant) :deck last :title))
          "Agenda selected with Shannon's Archives ability should be on bottom of deck")
      (is (= number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck)))
          "Searching Archives shouldn't shuffle deck"))))

(deftest shattered-remains
  ;; Shattered Remains
  (do-game
    (new-game (default-contestant [(qty "Shattered Remains" 2)])
              (default-challenger ["Cyberfeeder" "Lemuria Codecracker"]))
    (play-from-hand state :contestant "Shattered Remains" "New remote")
    (play-from-hand state :contestant "Shattered Remains" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Cyberfeeder")
    (play-from-hand state :challenger "Lemuria Codecracker")
    (take-credits state :challenger)
    (let [remains1 (get-content state :remote1 0)
          remains2 (get-content state :remote2 0)
          cyber (get-hazard state 0)
          lemuria (get-hazard state 1)]
      (core/rez state :contestant remains1)
      (advance state remains2 1)
      (take-credits state :contestant)
      (run-empty-server state :remote1)
      (prompt-choice :contestant "Yes")
      (is (empty? (-> (get-contestant) :prompt)) "Contestant shouldn't get Shattered Remains ability prompt when no counters")
      (prompt-choice :challenger "No action")
      (run-empty-server state :remote2)
      (let [credits (:credit (get-contestant))]
        (prompt-choice :contestant "Yes")
        (prompt-select :contestant cyber)
        (is (= (- credits 1) (:credit (get-contestant))) "Shattered Remains ability should cost 1")
        (is (count (:discard (get-challenger))) "Cyberfeeder should be in discard from Shattered Remains")))))

(deftest shi.kyu
  ;; Shi.Kyū
  (do-game
    (new-game (default-contestant ["Shi.Kyū"])
              (default-challenger [(qty "Sure Gamble" 5)]))
    (play-from-hand state :contestant "Shi.Kyū" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant 5)
    (is (= "Take 5 net damage" (-> (get-challenger) :prompt first :choices first)))
    (prompt-choice-partial :challenger "net damage")
    (prompt-choice :challenger "No action")
    (is (zero? (count (:hand (get-challenger)))) "Challenger took 5 net damage from Shi.Kyū")
    (run-empty-server state "Server 1")
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant 2)
    (is (= "Take 2 net damage" (-> (get-challenger) :prompt first :choices first)))
    (prompt-choice-partial :challenger "Add")
    (is (empty? (-> (get-challenger) :prompt)) "Challenger shouldn't get the option to discard Shi.Kyū as it was added to agenda area")
    (is (= -1 (:agenda-point (get-challenger))) "Challenger should be at -1 agenda points after adding Shi.Kyū to agenda area")))

(deftest shock!
  ;; Shock! - do 1 net damage on access
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Shock!" 3)])
                (default-challenger))
      (discard-from-hand state :contestant "Shock!")
      (play-from-hand state :contestant "Shock!" "New remote")
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger took 1 net damage")
      (run-empty-server state "Archives")
      (is (= 1 (count (:hand (get-challenger)))) "Challenger took 1 net damage")))
  (testing "ensure :access flag is cleared on run end. Issue #2319"
    (do-game
      (new-game (default-contestant [(qty "Shock!" 3) "Chairman Hiro"])
                (default-challenger))
      (discard-from-hand state :contestant "Shock!")
      (play-from-hand state :contestant "Shock!" "New remote")
      (take-credits state :contestant)
      (run-empty-server state "Archives")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger took 1 net damage")
      (is (not (:run @state)) "Run is complete")
      (discard-from-hand state :contestant "Chairman Hiro")
      (is (= 2 (count (:discard (get-contestant)))) "Hiro and Shock still in archives")
      (is (zero? (count (:scored (get-challenger)))) "Hiro not scored by Challenger"))))

(deftest snare!
  (testing "Basic test"
    ;; pay 4 on access, and do 3 net damage and give 1 tag
    (do-game
      (new-game (default-contestant [(qty "Snare!" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Snare!" "New remote")
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
          "Challenger has prompt to wait for Snare!")
      (prompt-choice :contestant "Yes")
      (is (= 3 (:credit (get-contestant))) "Contestant had 7 and paid 4 for Snare! 1 left")
      (is (= 1 (:tag (get-challenger))) "Challenger has 1 tag")
      (is (zero? (count (:hand (get-challenger)))) "Challenger took 3 net damage")))
  (testing "Can't afford"
    (do-game
      (new-game (default-contestant ["Snare!"])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
      (play-from-hand state :contestant "Snare!" "New remote")
      (take-credits state :contestant)
      (core/lose state :contestant :credit 7)
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
          "Challenger has prompt to wait for Snare!")
      (prompt-choice :contestant "Yes")
      (is (zero? (:tag (get-challenger))) "Challenger has 0 tags")
      (prompt-choice-partial :challenger "Pay")
      (is (empty? (:prompt (get-challenger))) "Challenger waiting prompt is cleared")
      (is (zero? (count (:discard (get-challenger)))) "Challenger took no damage")))
  (testing "with Dedicated Response Team"
    (do-game
      (new-game (default-contestant ["Snare!" "Dedicated Response Team"])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
      (play-from-hand state :contestant "Snare!" "New remote")
      (play-from-hand state :contestant "Dedicated Response Team" "New remote")
      (core/gain state :contestant :click 1 :credit 4)
      (let [drt (get-content state :remote2 0)]
        (take-credits state :contestant)
        (run-on state "Server 1")
        (core/rez state :contestant drt)
        (run-successful state)
        (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
            "Challenger has prompt to wait for Snare!")
        (prompt-choice :contestant "Yes")
        (is (= 1 (:tag (get-challenger))) "Challenger has 1 tag")
        (prompt-choice-partial :challenger "Pay")
        (is (= 5 (count (:discard (get-challenger)))) "Challenger took 5 damage")))))

(deftest space-camp
  (testing "when in Archives. #1929"
    (do-game
      (new-game (default-contestant ["Space Camp" "News Team" "Breaking News"])
                (default-challenger))
      (discard-from-hand state :contestant "Space Camp")
      (discard-from-hand state :contestant "News Team")
      (play-from-hand state :contestant "Breaking News" "New remote")
      (take-credits state :contestant)
      (run-empty-server state :archives)
      (prompt-choice :challenger "News Team")
      (prompt-choice :challenger "Take 2 tags")
      (prompt-choice :challenger "Space Camp")
      (prompt-choice :contestant "Yes")
      (prompt-select :contestant (get-content state :remote1 0))
      (is (= 1 (get-counters (get-content state :remote1 0) :advancement)) "Agenda advanced once from Space Camp")
      (is (= 2 (:tag (get-challenger))) "Challenger has 2 tags")
      (is (not (:run @state)) "Run completed"))))

(deftest student-loans
  ;; Student Loans - costs Challenger 2c extra to play event if already same one in discard
  (do-game
    (new-game (default-contestant ["Student Loans" (qty "Hedge Fund" 2)])
              (default-challenger))
    (core/gain state :contestant :credit 2)
    (play-from-hand state :contestant "Student Loans" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (is (= 5 (:credit (get-contestant))) "Contestant has 5c")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 9 (:credit (get-contestant))) "Contestant has 9c - no penalty from Student Loans")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 13 (:credit (get-contestant))) "Contestant has 13c - no penalty from Student Loans")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sure Gamble")
    (is (= 9 (:credit (get-challenger))) "1st Gamble played for 4c")
    (play-from-hand state :challenger "Sure Gamble")
    (is (= 11 (:credit (get-challenger))) "2nd Gamble played for 2c")
    (play-from-hand state :challenger "Sure Gamble")
    (is (= 13 (:credit (get-challenger))) "3rd Gamble played for 2c")))

(deftest sundew
  ;; Sundew
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Sundew"])
                (default-challenger))
      (play-from-hand state :contestant "Sundew" "New remote")
      (let [sund (get-content state :remote1 0)]
        (core/rez state :contestant sund)
        (take-credits state :contestant 2)
        (is (= 5 (:credit (get-contestant))) "Cost 2cr to rez")
        ;; spend a click not on a run
        (take-credits state :challenger)
        (is (= 7 (:credit (get-contestant))) "Contestant gained 2cr from Sundew")
        (take-credits state :contestant)
        (run-on state "Server 1")
        (is (= 10 (:credit (get-contestant))) "Contestant did not gain 2cr from run on Sundew")
        (is (= 3 (:click (get-challenger))) "Challenger spent 1 click to start run"))))
  ; (testing "Sundew - Dirty Laundry"
  ;   (do-game
  ;     (new-game (default-contestant ["Sundew"])
  ;               (default-challenger ["Dirty Laundry"]))
  ;     (play-from-hand state :contestant "Sundew" "New remote")
  ;     (let [sund (get-content state :remote1 0)]
  ;       (core/rez state :contestant (refresh sund))
  ;       (is (= 3 (:credit (get-contestant))) "Cost 2cr to rez")
  ;       (take-credits state :contestant)
  ;       (play-from-hand state :challenger "Dirty Laundry")
  ;       (prompt-choice :challenger "Server 1")
  ;       ;; spend a click on a run through a card, not through click-run
  ;       (is (= 5 (:credit (get-contestant))) "Contestant did not gain 2cr from run on Sundew"))))
  )

(deftest synth-dna-modification
  ;; Synth DNA Modification
  (do-game
    (new-game (default-contestant ["Synth DNA Modification" "Data Mine"])
              (default-challenger))
    (play-from-hand state :contestant "Synth DNA Modification" "New remote")
    (play-from-hand state :contestant "Data Mine" "HQ")
    (let [dna (get-content state :remote1 0)
          data (get-character state :hq 0)]
      (core/rez state :contestant dna)
      (core/rez state :contestant data)
      (take-credits state :contestant)
      (run-on state "HQ")
      (card-subroutine state :contestant data 0)
      (is (= 1 (count (:discard (get-challenger)))) "Challenger should take 1 net damage from Data Mine")
      (is (= 1 (count (:discard (get-contestant)))) "Data Mine should discard self after subroutine fires")
      (card-ability state :contestant dna 0)
      (is (= 2 (count (:discard (get-challenger))))
          "Challenger should take 1 net damage from Synth DNA Modification after Data Mine subroutine"))))

(deftest team-sponsorship
  ;; Team Sponsorship
  (testing "Install from HQ"
    (do-game
      (new-game (default-contestant ["Domestic Sleepers"
                               "Team Sponsorship"
                               "Adonis Campaign"])
                (default-challenger))
      (play-from-hand state :contestant "Team Sponsorship" "New remote")
      (play-from-hand state :contestant "Domestic Sleepers" "New remote")
      (let [ag1 (get-content state :remote2 0)
            tsp (get-content state :remote1 0)]
        (core/rez state :contestant tsp)
        (score-agenda state :contestant ag1)
        (prompt-select :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
        (prompt-choice :contestant "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
            "Adonis installed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:hand (get-contestant)))) "No Adonis in hand"))))
  (testing "Install from Archives"
    (do-game
      (new-game (default-contestant ["Domestic Sleepers"
                               "Team Sponsorship"
                               "Adonis Campaign"])
                (default-challenger))
      (play-from-hand state :contestant "Team Sponsorship" "New remote")
      (play-from-hand state :contestant "Domestic Sleepers" "New remote")
      (discard-from-hand state :contestant "Adonis Campaign")
      (let [ag1 (get-content state :remote2 0)
            tsp (get-content state :remote1 0)]
        (core/rez state :contestant tsp)
        (score-agenda state :contestant ag1)
        (prompt-select :contestant (find-card "Adonis Campaign" (:discard (get-contestant))))
        (prompt-choice :contestant "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
            "Adonis installed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:discard (get-contestant)))) "No Adonis in discard"))))
  (testing "Multiple installs"
    (do-game
      (new-game (default-contestant ["Domestic Sleepers"
                               (qty "Team Sponsorship" 2)
                               (qty "Adonis Campaign" 2)])
                (default-challenger))
      (play-from-hand state :contestant "Team Sponsorship" "New remote")
      (play-from-hand state :contestant "Team Sponsorship" "New remote")
      (play-from-hand state :contestant "Domestic Sleepers" "New remote")
      (discard-from-hand state :contestant "Adonis Campaign")
      (let [ag1 (get-content state :remote3 0)
            tsp2 (get-content state :remote2 0)
            tsp1 (get-content state :remote1 0)]
        (core/rez state :contestant tsp1)
        (core/rez state :contestant tsp2)
        (score-agenda state :contestant ag1)
        (prompt-choice :contestant "Team Sponsorship")
        (prompt-select :contestant (find-card "Adonis Campaign" (:discard (get-contestant))))
        (prompt-choice :contestant "New remote")
        (prompt-select :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
        (prompt-choice :contestant "New remote")
        (is (= "Adonis Campaign" (:title (get-content state :remote4 0)))
            "Adonis installed by Team Sponsorship")
        (is (= "Adonis Campaign" (:title (get-content state :remote5 0)))
            "Adonis installed by Team Sponsorship"))))
  (testing "Score 5 points in one window"
    (do-game
      (new-game (default-contestant [(qty "AstroScript Pilot Resource" 3)
                               "Team Sponsorship"
                               "Breaking News"
                               "SanSan City Grid"])
                (default-challenger))
      (play-from-hand state :contestant "SanSan City Grid" "New remote")
      (core/gain state :contestant :credit 100 :click 5)
      (core/rez state :contestant (get-content state :remote1 0))
      (play-from-hand state :contestant "AstroScript Pilot Resource" "New remote")
      (score-agenda state :contestant (get-content state :remote2 0))
      (play-from-hand state :contestant "AstroScript Pilot Resource" "Server 1")
      (play-from-hand state :contestant "Team Sponsorship" "New remote")
      (core/rez state :contestant (get-content state :remote3 0))
      (score-agenda state :contestant (get-content state :remote1 1))
      (prompt-select :contestant (find-card "AstroScript Pilot Resource" (:hand (get-contestant))))
      (is (zero? (get-counters (second (:scored (get-contestant))) :agenda)) "AstroScript not resolved yet")
      (prompt-choice :contestant "Server 1")
      (is (= 1 (get-counters (second (:scored (get-contestant))) :agenda)) "AstroScript resolved")
      (card-ability state :contestant (first (:scored (get-contestant))) 0)
      (prompt-select :contestant (get-content state :remote1 1))
      (card-ability state :contestant (second (:scored (get-contestant))) 0)
      (prompt-select :contestant (get-content state :remote1 1))
      (core/score state :contestant {:card (get-content state :remote1 1)})
      (prompt-select :contestant (find-card "Breaking News" (:hand (get-contestant))))
      (prompt-choice :contestant "Server 1")
      (card-ability state :contestant (second (next (:scored (get-contestant)))) 0)
      (prompt-select :contestant (get-content state :remote1 1))
      (core/score state :contestant {:card (get-content state :remote1 1)})
      (prompt-choice :contestant "Done")
      (is (= 7 (:agenda-point (get-contestant))) "Scored 5 points in one turn"))))

(deftest tech-startup
  ;; Tech Startup
  (do-game
    (new-game (default-contestant ["Tech Startup" "TechnoCo" (qty "Ice Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant ["Tech Startup"])
    (play-from-hand state :contestant "Tech Startup" "New remote")
    (let [tech (get-content state :remote1 0)]
      (core/rez state :contestant (refresh tech))
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (zero? (-> (get-contestant) :discard count)) "Contestant should start with 0 cards in Archives")
      (card-ability state :contestant tech 0)
      (prompt-card :contestant (find-card "TechnoCo" (:deck (get-contestant))))
      (prompt-choice :contestant "New remote")
      (is (= "TechnoCo" (:title (get-content state :remote2 0)))
          "TechnoCo should be installed in a new remote from Tech Startup's ability")
      (is (= 1 (-> (get-contestant) :discard count)) "Tech Startup should now be in discard"))))

(deftest technoco
  ;; TechnoCo - Increase resource / hazard / virtual cost by 1 and gain 1 when they are installed
  (do-game
    (new-game (default-contestant ["TechnoCo"])
              (default-challenger ["Misdirection"       ;; 0 cost resource
                               "Bookmark"           ;; 0 cost hazard
                               "Ice Analyzer"       ;; 0 cost virtual radicle
                               "Fall Guy"]))        ;; 0 cost non-virtual radicle
    (play-from-hand state :contestant "TechnoCo" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (is (= 5 (:credit (get-contestant))) "Contestant at 5 credits")
    (is (= 5 (:credit (get-challenger))) "Challenger at 5 credits")
    (play-from-hand state :challenger "Misdirection")
    (is (= 6 (:credit (get-contestant))) "Contestant gained a credit")
    (is (= 4 (:credit (get-challenger))) "Challenger spent an extra credit")
    (play-from-hand state :challenger "Bookmark")
    (is (= 7 (:credit (get-contestant))) "Contestant gained a credit")
    (is (= 3 (:credit (get-challenger))) "Challenger spent an extra credit")
    (play-from-hand state :challenger "Ice Analyzer")
    (is (= 8 (:credit (get-contestant))) "Contestant gained a credit")
    (is (= 2 (:credit (get-challenger))) "Challenger spent an extra credit")
    (play-from-hand state :challenger "Fall Guy")
    (is (= 8 (:credit (get-contestant))) "Contestant did not gain a credit")
    (is (= 2 (:credit (get-challenger))) "Challenger did not spend an extra credit")))

(deftest tenma-line
  ;; Tenma Line - Swap 2 pieces of installed Character
  (do-game
    (new-game (default-contestant ["Tenma Line" "Harvester"
                             "Aimor" "Lockdown"])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Tenma Line" "New remote")
    (play-from-hand state :contestant "Harvester" "HQ")
    (play-from-hand state :contestant "Aimor" "HQ")
    (play-from-hand state :contestant "Lockdown" "R&D")
    (core/rez state :contestant (get-content state :rd 0))
    (core/rez state :contestant (get-content state :remote1 0))
    (is (= 1 (:click (get-contestant))))
    (card-ability state :contestant (get-content state :remote1 0) 0)
    (prompt-select :contestant (get-character state :rd 0))
    (prompt-select :contestant (get-character state :hq 1))
    (is (empty? (:prompt (get-contestant))))
    (is (zero? (:click (get-contestant))) "Spent 1 click")
    (is (= "Aimor" (:title (get-character state :rd 0))) "Aimor swapped to R&D")
    (is (= "Lockdown" (:title (get-character state :hq 1))) "Lockdown swapped to HQ outer position")))

(deftest test-ground
  ;; Test Ground
  (do-game
    (new-game (default-contestant ["Test Ground" "Ice Wall" "News Team"])
              (default-challenger))
    (core/gain state :contestant :credit 100 :click 100)
    (play-from-hand state :contestant "Test Ground" "New remote")
    (play-from-hand state :contestant "Ice Wall" "New remote")
    (play-from-hand state :contestant "News Team" "New remote")
    (let [ground (get-content state :remote1 0)
          iw (get-character state :remote2 0)
          news (get-content state :remote3 0)]
      (core/rez state :contestant ground)
      (core/rez state :contestant iw)
      (core/rez state :contestant news)
      (advance state ground 2)
      (is (:rezzed (refresh iw)) "Ice Wall should be rezzed")
      (is (:rezzed (refresh news)) "News Team should be rezzed")
      (is (zero? (-> (get-contestant) :discard count)) "Contestant should start with 0 cards in Archives")
      (card-ability state :contestant ground 0)
      (prompt-select :contestant iw)
      (prompt-select :contestant news)
      (is (not (:rezzed (refresh iw))) "Ice Wall should be rezzed")
      (is (not (:rezzed (refresh news))) "News Team should be rezzed")
      (is (= 1 (-> (get-contestant) :discard count)) "Contestant should now have 1 card in discard"))))

(deftest the-board
  ;; The Board
  (testing "Modify everything in the score area (regression test for #1938)"
    (do-game
      (new-game (default-contestant ["The Board" "News Team" (qty "Firmware Updates" 2)])
                (default-challenger [(qty "Artist Colony" 3) (qty "Fan Site" 3)]))
      (play-from-hand state :contestant "The Board" "New remote")
      (play-from-hand state :contestant "News Team" "New remote")
      (play-from-hand state :contestant "Firmware Updates" "New remote")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Artist Colony")
      (play-from-hand state :challenger "Fan Site")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Firmware Updates" "New remote")
      (score-agenda state :contestant (get-content state :remote4 0))
      (is (= 1 (count (:scored (get-challenger)))) "Fan Site added to Challenger score area")
      (is (zero? (:agenda-point (get-challenger))) "Challenger has 0 agenda points")
      (take-credits state :contestant)
      (run-empty-server state :remote3)
      (prompt-choice :challenger "Steal")
      (is (= 2 (count (:scored (get-challenger)))) "Firmware Updates stolen")
      (is (= 1 (:agenda-point (get-challenger))) "Challenger has 1 agenda point")
      (core/rez state :contestant (get-content state :remote1 0))
      (is (= -1 (:agenda-point (get-challenger))) "Challenger has -1 agenda points")
      (run-empty-server state :remote2)
      (prompt-choice :challenger "Add News Team to score area")
      (is (= 3 (count (:scored (get-challenger)))) "News Team added to Challenger score area")
      (is (= -3 (:agenda-point (get-challenger))) "Challenger has -3 agenda points")
      (card-ability state :challenger (get-radicle state 0) 0)
      (prompt-choice :challenger (-> @state :challenger :prompt first :choices first))
      (prompt-select :challenger (first (:scored (get-challenger))))
      (is (= 2 (count (:scored (get-challenger)))) "Fan Site removed from Challenger score area")
      (is (= -2 (:agenda-point (get-challenger))) "Challenger has -2 agenda points")
      (run-empty-server state :remote1)
      (prompt-choice-partial :challenger "Pay")
      (is (= 3 (count (:scored (get-challenger)))) "The Board added to Challenger score area")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger has 2 agenda points")))
  (testing "handle Fifteen Minutes clicked out of Challenger's score area"
    (do-game
      (new-game (default-contestant ["The Board" "15 Minutes"])
                (default-challenger))
      (play-from-hand state :contestant "The Board" "New remote")
      (play-from-hand state :contestant "15 Minutes" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (take-credits state :contestant)
      (is (zero? (:agenda-point (get-challenger))) "Challenger has 0 agenda points")
      (run-empty-server state :remote2)
      (prompt-choice-partial :challenger "Steal")
      (is (zero? (:agenda-point (get-challenger))) "Challenger stays at 1 agenda point")
      (is (= 1 (count (:scored (get-challenger)))) "Challenger has 1 agenda in scored area")
      (take-credits state :challenger)
      (let [fifm (first (:scored (get-challenger)))]
        (card-ability state :contestant (refresh fifm) 0)
        (is (zero? (:agenda-point (get-challenger))) "Challenger drops to 0 agenda points")
        (is (empty? (:scored (get-challenger))) "Challenger has no agendas in scored area"))))
  (testing "Contestant scoring agenda shouldn't trigger The Board to lower Challenger points"
    (do-game
      (new-game (default-contestant ["The Board" (qty "Hostile Takeover" 2)])
                (default-challenger))
      (core/gain state :contestant :credit 6)
      (play-from-hand state :contestant "The Board" "New remote")
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (take-credits state :contestant)
      (is (zero? (:agenda-point (get-challenger))) "Challenger has 0 agenda points")
      (run-empty-server state :remote3)
      (prompt-choice-partial :challenger "Steal")
      (is (= 1 (:agenda-point (get-challenger))) "Challenger has 1 agenda point")
      (is (= 1 (count (:scored (get-challenger)))) "Challenger has 1 agenda in scored area")
      (take-credits state :challenger)
      (core/rez state :contestant (get-content state :remote1 0))
      (is (zero? (:agenda-point (get-challenger))) "Challenger loses 1 agenda point")
      (is (= 1 (count (:scored (get-challenger)))) "Challenger still has 1 agenda in scored area")
      (score-agenda state :contestant (get-content state :remote2 0))
      (is (zero? (:agenda-point (get-challenger))) "Challenger still has 0 agenda points")
      (is (= 1 (count (:scored (get-challenger)))) "Challenger still has 1 agenda in scored area")))
  (testing "Scoring two copies should be 4 agenda points"
    (do-game
      (new-game (default-contestant [(qty "The Board" 2)])
                (default-challenger))
      (core/gain state :contestant :credit 6)
      (play-from-hand state :contestant "The Board" "New remote")
      (play-from-hand state :contestant "The Board" "New remote")
      (core/rez state :contestant (get-content state :remote1 0))
      (core/rez state :contestant (get-content state :remote2 0))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 14)
      (is (zero? (:agenda-point (get-challenger))) "Challenger has 0 agenda points")
      (is (empty? (:scored (get-challenger))) "Challenger has no agendas")
      (run-empty-server state :remote2)
      (prompt-choice-partial :challenger "Pay")
      (is (= 1 (:agenda-point (get-challenger))) "Challenger has 1 agenda point")
      (is (= 1 (count (:scored (get-challenger)))) "Challenger has 1 agenda in scored area")
      (run-empty-server state :remote1)
      (prompt-choice-partial :challenger "Pay")
      (is (= 4 (:agenda-point (get-challenger))) "Challenger has 4 agenda points")
      (is (= 2 (count (:scored (get-challenger)))) "Challenger has 2 agendas in scored area"))))

(deftest the-news-now-hour
  ;; The News Now Hour
  (do-game
    (new-game (default-contestant ["The News Now Hour"])
              (default-challenger ["Rumor Mill"]))
    (play-from-hand state :contestant "The News Now Hour" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Rumor Mill")
    (is (= 1 (-> (get-challenger) :hand count)) "Rumor Mill should still be in hand after trying to play it")))

(deftest the-root
  ;; The Root - recurring credits refill at Step 1.2
  (do-game
    (new-game (make-deck "Blue Sun: Powering the Future" ["The Root"])
              (default-challenger))
    (play-from-hand state :contestant "The Root" "New remote")
    (core/gain state :contestant :credit 6)
    (let [root (get-content state :remote1 0)]
      (core/rez state :contestant root)
      (card-ability state :contestant (refresh root) 0)
      (is (= 2 (get-counters (refresh root) :recurring)) "Took 1 credit from The Root")
       (is (= 6 (:credit (get-contestant))) "Contestant took Root credit into credit pool")
      (take-credits state :contestant)
      (take-credits state :challenger)
      ;; we expect Step 1.2 to have triggered because of Blue Sun
      (is (:contestant-phase-12 @state) "Contestant is in Step 1.2")
      (is (= 3 (get-counters (refresh root) :recurring)) "Recurring credits were refilled before Step 1.2 window"))))

(deftest thomas-haas
  ;; Thomas Haas
  (letfn [(haas-test [number]
            (do-game
              (new-game (default-contestant ["Thomas Haas"])
                        (default-challenger))
              (core/gain state :contestant :credit 10 :click 10)
              (play-from-hand state :contestant "Thomas Haas" "New remote")
              (let [haas (get-content state :remote1 0)]
                (core/rez state :contestant haas)
                (advance state (refresh haas) number)
                (core/lose state :contestant :credit (:credit (get-contestant)))
                (is (zero? (-> (get-contestant) :discard count)) "Contestant should start with 0 cards in Archives")
                (is (zero? (:credit (get-contestant))) "Contestant should fire ability with 0 credits")
                (is (= number (get-counters (refresh haas) :advancement))
                    (str "Thomas Haas should have " number " advancement tokens"))
                (card-ability state :contestant (refresh haas) 0)
                (is (= (* 2 number) (:credit (get-contestant)))
                    (str "Contestant should gain " (* 2 number) " credits from Thomas Haas' ability"))
                (is (= 1 (-> (get-contestant) :discard count)) "Thomas Haas should be in Archives after ability"))))]
    (doall (map haas-test [1 2 3 4 5]))))

(deftest toshiyuki-sakai
  ;; Toshiyuki Sakai - Swap with an site/agenda from HQ; Challenger can choose to access new card or not
  (do-game
    (new-game (default-contestant ["Toshiyuki Sakai" "Project Junebug" "Hedge Fund"])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
    (play-from-hand state :contestant "Toshiyuki Sakai" "New remote")
    (let [toshi (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh toshi)})
      (core/advance state :contestant {:card (refresh toshi)})
      (take-credits state :contestant)
      (is (= 2 (get-counters (refresh toshi) :advancement)) "Toshiyuki has 2 advancements")
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
          "Challenger has prompt to wait for Toshiyuki")
      (prompt-choice :contestant "Yes") ; choose to do a swap
      (prompt-select :contestant (find-card "Hedge Fund" (:hand (get-contestant))))
      (is (= (refresh toshi) (get-content state :remote1 0)) "Toshiyuki still in remote; can't target an operation in hand")
      (prompt-select :contestant (find-card "Project Junebug" (:hand (get-contestant))))
      (let [june (get-content state :remote1 0)]
        (is (= "Project Junebug" (:title (refresh june))) "Project Junebug swapped into Server 1")
        (is (= 2 (get-counters (refresh june) :advancement)) "Project Junebug has 2 advancements")
        (prompt-choice :challenger "Yes") ; choose to access new card
        (prompt-choice :contestant "Yes") ; pay 1c to fire Junebug
        (is (= 4 (count (:discard (get-challenger)))) "Challenger took 4 net damage")))))

(deftest turtlebacks
  ;; Turtlebacks - Gain 1 credit for every new server created
  (do-game
    (new-game (default-contestant ["Turtlebacks" (qty "PAD Campaign" 2) "Wraparound"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Turtlebacks" "New remote")
    (let [tb (get-content state :remote1 0)]
      (core/rez state :contestant tb)
      (play-from-hand state :contestant "PAD Campaign" "New remote")
      (is (= 4 (:credit (get-contestant))) "Gained 1 credit for new server created")
      (play-from-hand state :contestant "Wraparound" "Server 1")
      (is (= 4 (:credit (get-contestant))) "No credit gained for install into existing server")
      (play-from-hand state :contestant "PAD Campaign" "New remote")
      (is (= 5 (:credit (get-contestant))) "Gained 1 credit for new server created"))))

(deftest urban-renewal
  ;; Urban renewal meat damage
  (do-game
    (new-game (default-contestant ["Urban Renewal"])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
    ;; Contestant turn 1, install and rez urban renewal
    (play-from-hand state :contestant "Urban Renewal" "New remote")
    (let [ur (get-content state :remote1 0)]
      (core/rez state :contestant (refresh ur))
      (take-credits state :contestant)
      ;; Challenger turn 1, creds
      (is (= 3 (get-counters (refresh ur) :power)))
      (take-credits state :challenger)
      ;; Contestant turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :contestant)
      ;; Challenger turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :challenger)
      ;; Contestant turn 3
      (is (= 1 (get-counters (refresh ur) :power)))
      (take-credits state :contestant)
      ;; Challenger turn 3
      (is (zero? (count (:discard (get-contestant)))) "Nothing in Contestant discard")
      (is (zero? (count (:discard (get-challenger)))) "Nothing in Challenger discard")
      (take-credits state :challenger)
      ;; Contestant turn 4 - damage fires
      (is (= 1 (count (:discard (get-contestant)))) "Urban Renewal got discarded")
      (is (= 4 (count (:discard (get-challenger)))) "Urban Renewal did 4 meat damage"))))

(deftest victoria-jenkins
  ;; Victoria Jenkins
  (do-game
    (new-game (default-contestant ["Victoria Jenkins"])
              (default-challenger))
    (play-from-hand state :contestant "Victoria Jenkins" "New remote")
    (take-credits state :contestant)
    (is (= 4 (:click (get-challenger))) "Challenger should have 4 clicks by default")
    (let [victoria (get-content state :remote1 0)]
      (core/rez state :contestant victoria)
      (is (= 3 (:click (get-challenger))) "Challenger should have 3 clicks when Victoria Jenkins is rezzed")
      (run-empty-server state "Server 1")
      (prompt-choice-partial :challenger "Pay")
      (is (= 3 (:click (get-challenger))) "Challenger should have 3 clicks again after discarding Victoria Jenkins")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger should gain 2 agenda points from discarding Victoria Jenkins")
      (is (= 1 (count (get-scored state :challenger))) "Challenger should have 1 card in score area")
      (is (zero? (-> (get-contestant) :discard count)) "Victoria Jenkins shouldn't go to Archives when discarded"))))

(deftest warden-fatuma
  ;; Warden Fatuma - rezzed bioroid character gains an additional sub
  (do-game
    (new-game (default-contestant ["Warden Fatuma" "Kakugo"
                             "Eli 2.0" "Ichi 2.0"])
              (default-challenger))
    (core/gain state :contestant :credit 20 :click 5)
    (play-from-hand state :contestant "Kakugo" "Archives")
    (play-from-hand state :contestant "Eli 2.0" "HQ")
    (play-from-hand state :contestant "Ichi 2.0" "R&D")
    (play-from-hand state :contestant "Warden Fatuma" "New remote")
    (let [wf (get-content state :remote1 0)
          kak (get-character state :archives 0)
          eli (get-character state :hq 0)
          ichi (get-character state :rd 0)]
      (core/rez state :contestant kak)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo starts with 1 sub")
      (core/rez state :contestant eli)
      (is (= 2 (count (:subroutines (refresh eli)))) "Eli 2.0 starts with 2 subs")
      (is (zero? (count (:subroutines (refresh ichi)))) "Unrezzed Ichi 2.0 starts with 0 subs")
      (core/rez state :contestant wf)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 2.0 gains 1 sub")
      (is (zero? (count (:subroutines (refresh ichi)))) "Unrezzed Ichi 2.0 stays at 0 subs")
      (core/rez state :contestant ichi)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 2.0 stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh ichi)))) "Ichi 2.0 rezzes with 3 subs")
      (core/derez state :contestant (refresh wf))
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 2 (count (:subroutines (refresh eli)))) "Eli 2.0 reverts")
      (is (= 2 (count (:subroutines (refresh ichi)))) "Ichi 2.0 reverts"))))

(deftest watchdog
  ;; Watchdog - Reduce rez cost of first Character per turn by number of Challenger tags
  (do-game
    (new-game (default-contestant ["Watchdog" "Architect" "Wraparound"])
              (default-challenger))
    (play-from-hand state :contestant "Watchdog" "New remote")
    (play-from-hand state :contestant "Wraparound" "HQ")
    (play-from-hand state :contestant "Architect" "HQ")
    (let [wd (get-content state :remote1 0)
          arch (get-character state :hq 1)
          wrap (get-character state :hq 0)]
      (take-credits state :contestant)
      (is (= 4 (:credit (get-contestant))))
      (core/gain state :challenger :tag 2)
      (run-on state "HQ")
      (core/rez state :contestant wd)
      (core/rez state :contestant arch)
      (is (= 2 (:credit (get-contestant))) "Only 2 credits to rez Architect")
      (core/rez state :contestant wrap)
      (is (zero? (:credit (get-contestant))) "No rez discount on Wraparound"))))

(deftest whampoa-reclamation
  ;; Whampoa Reclamation: Enable discarding a card from HQ to place a card in Archives on the bottom of R+D
  (do-game
    (new-game (default-contestant [(qty "Whampoa Reclamation" 3)
                             (qty "PAD Campaign" 2)
                             (qty "Global Food Initiative" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Whampoa Reclamation" "New remote")
    (let [wr (get-content state :remote1 0)]
      (core/draw state :contestant)
      (take-credits state :contestant)
      (core/rez state :contestant wr)
      (let [gfi (find-card "Global Food Initiative" (:hand (get-contestant)))]
        (core/discard state :challenger gfi)
        (card-ability state :contestant wr 0)
        (prompt-choice :contestant "Global Food Initiative") ;; into archives
        (prompt-select :contestant (first (:discard (get-contestant)))) ;; into R&D
        (is (zero? (count (:discard (get-contestant)))) "Only card in discard placed in bottom of R&D")
        (is (= "Global Food Initiative" (-> (get-contestant) :deck last :title)) "GFI last card in deck")))))

(deftest worlds-plaza
  ;; Worlds Plaza
  (do-game
    (new-game (default-contestant ["Worlds Plaza"
                             "Personalized Portal"
                             "Dedicated Response Team"
                             "Honeyfarm"])
              (default-challenger))
    (core/gain state :contestant :credit 10 :click 10)
    (play-from-hand state :contestant "Worlds Plaza" "New remote")
    (let [plaza (get-content state :remote1 0)]
      (core/rez state :contestant plaza)
      (card-ability state :contestant plaza 0)
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant plaza 0)
        (prompt-select :contestant (find-card "Personalized Portal" (:hand (get-contestant))))
        (is (= (- credits 1) (:credit (get-contestant))) "Contestant should only spend 1 credit to rez Personalized Portal"))
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant plaza 0)
        (prompt-select :contestant (find-card "Dedicated Response Team" (:hand (get-contestant))))
        (is (= credits (:credit (get-contestant))) "Contestant should spend 0 credit to rez Dedicated Response Team"))
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant plaza 0)
        (prompt-select :contestant (find-card "Honeyfarm" (:hand (get-contestant))))
        (is (= credits (:credit (get-contestant))) "Contestant should spend 0 credit to rez Honeyfarm")))))

(deftest zaibatsu-loyalty
  ;; Zaibatsu Loyalty
  (do-game
    (new-game (default-contestant ["Zaibatsu Loyalty" "Ice Wall"])
              (default-challenger ["Lemuria Codecracker"]))
    (core/gain state :contestant :click 10 :click 10)
    (play-from-hand state :contestant "Zaibatsu Loyalty" "New remote")
    (play-from-hand state :contestant "Ice Wall" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :click 10 :click 10)
    (play-from-hand state :challenger "Lemuria Codecracker")
    (let [code (get-hazard state 0)
          iw (get-character state :remote2 0)
          zai (get-content state :remote1 0)]
      (run-empty-server state "HQ")
      (card-ability state :challenger code 0)
      (prompt-select :challenger (refresh iw))
      (is (some? (-> (get-contestant) :prompt first)) "Contestant should get the option to rez Zaibatsu Loyalty before expose")
      (prompt-choice :contestant "Yes")
      (is (:rezzed (refresh zai)) "Zaibatsu Loyalty should be rezzed")
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant zai 0)
        (is (= (- credits 1) (:credit (get-contestant))) "Contestant should lose 1 credit for stopping the expose"))
      (card-ability state :challenger code 0)
      (prompt-select :challenger (refresh iw))
      (is (some? (-> (get-contestant) :prompt first)) "Contestant should get the option to rez Zaibatsu Loyalty before expose")
      (card-ability state :challenger zai 1)
      (is (= 1 (-> (get-contestant) :discard count)) "Zaibatsu Loyalty should be in discard after using ability"))))

(deftest zealous-judge
  ;; Zealous Judge
  (do-game
    (new-game (default-contestant ["Zealous Judge"])
              (default-challenger))
    (play-from-hand state :contestant "Zealous Judge" "New remote")
    (let [judge (get-content state :remote1 0)]
      (core/rez state :contestant judge)
      (is (not (:rezzed (refresh judge))) "Zealous Judge can't be rezzed until Challenger is tagged")
      (core/gain state :challenger :tag 1)
      (core/rez state :contestant judge)
      (is (:rezzed (refresh judge)) "Zealous Judge can be rezzed while the Challenger is tagged")
      (card-ability state :contestant judge 0)
      (is (= 2 (:tag (get-challenger))) "Challenger should gain a tag from Zealous Judge's ability"))))
