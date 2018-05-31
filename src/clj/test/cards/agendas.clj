(ns test.cards.agendas
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest fifteen-minutes
  ;; 15 Minutes - check if it works correctly from both sides
  (do-game
    (new-game (default-contestant [(qty "15 Minutes" 1)]) (default-challenger))
    (play-from-hand state :contestant "15 Minutes" "New remote")
    (take-credits state :contestant)
    ;; use 15 minutes to take it away from challenger
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Steal")
    (take-credits state :challenger)
    (is (= 1 (:agenda-point (get-challenger))))
    (is (= 1 (count (:scored (get-challenger)))))
    (let [fifm (first (:scored (get-challenger)))]
      (is (= 3 (:click (get-contestant))))
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :contestant (refresh fifm) 0)
      (is (= 0 (:agenda-point (get-challenger))))
      (is (= 0 (count (:scored (get-challenger))))))
    (is (= "15 Minutes" (:title (first (:deck (get-contestant))))))
    ;; TODO: could also check for deck shuffle
    (is (= 2 (:click (get-contestant))))
    ;; use 15 minutes to take it away from contestant (hey, maybe some obscure case happens where contestant would want that)
    (core/click-draw state :contestant 1)
    (play-from-hand state :contestant "15 Minutes" "New remote")
    (take-credits state :challenger)
    (score-agenda state :contestant (get-content state :remote2 0))
    (is (= 1 (:agenda-point (get-contestant))))
    (is (= 1 (count (:scored (get-contestant)))))
    (let [fifm (first (:scored (get-contestant)))]
      (is (= 1 (count (:abilities (refresh fifm)))))
      (card-ability state :contestant (refresh fifm) 0)
      (is (= 0 (:agenda-point (get-contestant))))
      (is (= 0 (count (:scored (get-contestant))))))
    (is (= "15 Minutes" (:title (first (:deck (get-contestant))))))))

(deftest ancestral-imager
  ;; Ancestral Imager - damage on jack out
  (do-game
    (new-game (default-contestant [(qty "Ancestral Imager" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Ancestral Imager" "New remote")
    (let [ai (get-content state :remote1 0)]
      (score-agenda state :contestant ai)
      (take-credits state :contestant)
      (is (= 3 (count(get-in @state [:challenger :hand]))) "Challenger has 3 cards in hand")
      (run-on state :hq)
      (run-jack-out state)
      (is (= 2 (count(get-in @state [:challenger :hand]))) "Challenger took 1 net damage"))))

(deftest astro-script-token
  ;; AstroScript token placement
  (do-game
    (new-game (default-contestant [(qty "AstroScript Pilot Resource" 3) (qty "Ice Wall" 2)])
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
              (is (or (= nil (:advance-counter (refresh to)))
                      (= 0 (:advance-counter (refresh to))))
                  (str "Advancement token not placed on " (:title to) msg)))
            (should-place [from to msg]
              (try-place from to)
              (is (= 0 (get-counters (refresh from) :agenda))
                  (str (:title from) " token was used on " (:title to) msg))
              (is (= 1 (:advance-counter (refresh to)))
                  (str "Advancement token placed on " (:title to) msg)))]
      (play-from-hand state :contestant "AstroScript Pilot Resource" "New remote")
      (score-agenda state :contestant (get-content state :remote1 0))
      (play-from-hand state :contestant "AstroScript Pilot Resource" "New remote")
      (let [scored-astro (get-in @state [:contestant :scored 0])
            installed-astro (get-content state :remote2 0)
            hand-astro (find-card "AstroScript Pilot Resource" (:hand get-contestant))]
        (should-not-place scored-astro hand-astro " in hand")
        (should-place scored-astro installed-astro " that is installed")
        (core/advance state :contestant {:card (refresh installed-astro)})
        (core/advance state :contestant {:card (refresh installed-astro)})
        (core/score   state :contestant {:card (refresh installed-astro)}))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (let [no-token-astro (get-in @state [:contestant :scored 0])
            token-astro (get-in @state [:contestant :scored 1])
            hand-character-wall (find-card "Ice Wall" (:hand get-contestant))
            installed-character-wall (get-character state :hq 0)]
        (should-not-place token-astro no-token-astro " that is scored")
        (should-not-place token-astro hand-character-wall " in hand")
        (should-place token-astro installed-character-wall " that is installed")))))

(deftest braintrust
  ;; Braintrust - Discount Character rez by 1 for every 2 over-advancements when scored
  (do-game
    (new-game (default-contestant [(qty "Braintrust" 1) (qty "Ichi 1.0" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Braintrust" "New remote")
    (let [bt (get-content state :remote1 0)]
      (core/add-prop state :contestant bt :advance-counter 7)
      (core/score state :contestant {:card (refresh bt)})
      (let [scored-bt (get-in @state [:contestant :scored 0])]
        (is (= 2 (get-counters (refresh scored-bt) :agenda))
            "Scored w/ 4 over-advancements; 2 agenda counters")
        (play-from-hand state :contestant "Ichi 1.0" "HQ")
        (core/rez state :contestant (get-character state :hq 0))
        (is (= 2 (:credit (get-contestant))) "2c discount to rez Ichi")))))

(deftest breaking-news
  ;; Test scoring breaking news
  (do-game
    (new-game (default-contestant [(qty "Breaking News" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Breaking News" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (is (= 2 (get-in @state [:challenger :tag])) "Challenger receives 2 tags from Breaking News")
    (take-credits state :contestant)
    (is (= 0 (get-in @state [:challenger :tag]))) "Two tags removed at the end of the turn"))

(deftest character-assassination
  ;; Character Assassination - Unpreventable trash of 1 muthereff when scored
  (do-game
    (new-game (default-contestant [(qty "Character Assassination" 1)])
              (default-challenger [(qty "Fall Guy" 1) (qty "Kati Jones" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Kati Jones")
    (play-from-hand state :challenger "Fall Guy")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Character Assassination" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (let [kati (get-in @state [:challenger :rig :muthereff 0])]
      (prompt-select :contestant kati)
      (is (empty? (:prompt (get-challenger))) "Fall Guy prevention didn't occur")
      (is (= 1 (count (:discard (get-challenger)))) "Kati Jones trashed"))))
	  
(deftest contestantorate-sales-team
  ;; Contestantorate Sales Team - Places 10c on card, contestant takes 1c on each turn start
  (do-game
    (new-game (default-contestant [(qty "Contestantorate Sales Team" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Contestantorate Sales Team" "New remote")
    (is (= 5 (:credit (get-contestant))))
    (score-agenda state :contestant (get-content state :remote1 0))
	(let [scored-cst (get-in @state [:contestant :scored 0])]
	  (core/end-turn state :contestant nil)
	  (core/start-turn state :challenger nil)
	  (is (= 6 (:credit (get-contestant))) "Increments at challenger's start of turn")
	  (is (= 9 (get-counters (refresh scored-cst) :credit)))
	  (core/end-turn state :challenger nil)
	  (core/start-turn state :contestant nil)
	  (is (= 7 (:credit (get-contestant))) "Increments at contestant's start of turn")
	  (is (= 8 (get-counters (refresh scored-cst) :credit)))
	)))

(deftest contestantorate-war
  ;; Contestantorate War - Gain 7c if you have 7c or more when scoring, otherwise lose all credits
  (do-game
    (new-game (default-contestant [(qty "Contestantorate War" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Contestantorate War" "New remote")
    (is (= 5 (:credit (get-contestant))))
    (score-agenda state :contestant (get-content state :remote1 0))
    (is (= 0 (:credit (get-contestant))) "Lost all credits")
    (core/gain state :contestant :credit 7)
    (play-from-hand state :contestant "Contestantorate War" "New remote")
    (score-agenda state :contestant (get-content state :remote2 0))
    (is (= 14 (:credit (get-contestant))) "Had 7 credits when scoring, gained another 7")))

(deftest crisis-management
  ;; Crisis Management - Do 1 meat damage at turn start if Challenger is tagged
  (do-game
    (new-game (default-contestant [(qty "Crisis Management" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Crisis Management" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
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
    (new-game (default-contestant [(qty "Dedicated Neural Net" 1) (qty "Scorched Earth" 2)
                             (qty "Hedge Fund" 1) "Caprcharacter Nisei"])
              (default-challenger [(qty "HQ Interface" 1)]))
    (play-from-hand state :contestant "Dedicated Neural Net" "New remote")
    (play-from-hand state :contestant "Caprcharacter Nisei" "HQ")
    (score-agenda state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (run-empty-server state :hq)
    (prompt-choice :challenger "0")
    (prompt-choice :contestant "1")
    (is (-> @state :run :run-effect :replace-access) "Replace-access tiggered")
    (prompt-select :contestant (find-card "Hedge Fund" (:hand (get-contestant))))
    (prompt-choice :challenger "Card from hand")
    (is (accessing state "Hedge Fund") "Challenger accessing Hedge Fund")
    (prompt-choice :challenger "OK")
    ;; test for #2376
    (prompt-choice :challenger "Unrezzed upgrade in HQ")
    (is (accessing state "Caprcharacter Nisei") "Challenger accessing Caprcharacter")
    (prompt-choice :challenger "No")
    (is (not (:run @state)) "Run completed")
    (run-empty-server state :hq)
    (prompt-choice :challenger "OK")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (play-from-hand state :challenger "HQ Interface")
    (run-empty-server state :hq)
    (prompt-choice :challenger "0")
    (prompt-choice :contestant "1")
    (is (= 2 (-> (get-contestant) :selected first :max)) "Contestant chooses 2 cards for Challenger to access")))

(deftest eden-fragment
  ;; Test that Eden Fragment ignores the install cost of the first character
  (do-game
    (new-game (default-contestant [(qty "Eden Fragment" 3) (qty "Ice Wall" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Eden Fragment" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (take-credits state :challenger)
    (take-credits state :challenger)
    (take-credits state :challenger)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (is (not (nil? (get-character state :hq 1))) "Contestant has two character installed on HQ")
    (is (= 6 (get-in @state [:contestant :credit])) "Contestant does not pay for installing the first Character of the turn")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (is (not (nil? (get-character state :hq 2))) "Contestant has three character installed on HQ")
    (is (= 4 (get-in @state [:contestant :credit])) "Contestant pays for installing the second Character of the turn")))

(deftest efficiency-committee
  ;; Efficiency Committee - Cannot advance cards if agenda counter is used
  (do-game
    (new-game (default-contestant [(qty "Efficiency Committee" 3) (qty "Shipment from SanSan" 2)
                             (qty "Ice Wall" 1)])
              (default-challenger))
    (core/gain state :contestant :click 4)
    (play-from-hand state :contestant "Efficiency Committee" "New remote")
    (play-from-hand state :contestant "Efficiency Committee" "New remote")
    (play-from-hand state :contestant "Efficiency Committee" "New remote")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [ec1 (get-content state :remote1 0)
          ec2 (get-content state :remote2 0)
          ec3 (get-content state :remote3 0)
          iw (get-character state :hq 0)]
      (score-agenda state :contestant ec1)
      (let [ec1_scored (get-in @state [:contestant :scored 0])]
        (is (= 3 (get-counters (refresh ec1_scored) :agenda)))
        (is (= 2 (:agenda-point (get-contestant))))
        ;; use token
        (is (= 3 (:click (get-contestant))))
        (card-ability state :contestant ec1_scored 0)
        (is (= 4 (:click (get-contestant))))
        ;; try to advance Ice Wall
        (core/advance state :contestant {:card (refresh iw)})
        (is (= 4 (:click (get-contestant))))
        (is (= nil (:advance-counter (refresh iw))))
        ;; try to advance Efficiency Committee
        (core/advance state :contestant {:card (refresh ec2)})
        (is (= 4 (:click (get-contestant))))
        (is (= nil (:advance-counter (refresh ec2))))
        ;; advance with Shipment from SanSan
        (play-from-hand state :contestant "Shipment from SanSan")
        (prompt-choice :contestant "2")
        (prompt-select :contestant ec2)
        (is (= 2 (:advance-counter (refresh ec2))))
        (play-from-hand state :contestant "Shipment from SanSan")
        (prompt-choice :contestant "2")
        (prompt-select :contestant ec2)
        (is (= 4 (:advance-counter (refresh ec2))))
        (core/score state :contestant {:card (refresh ec2)})
        (is (= 4 (:agenda-point (get-contestant))))
        (take-credits state :contestant)
        (take-credits state :challenger)
        ;; can advance again
        (core/advance state :contestant {:card (refresh iw)})
        (is (= 1 (:advance-counter (refresh iw))))
        (core/advance state :contestant {:card (refresh ec3)})
        (is (= 1 (:advance-counter (refresh ec3))))))))

(deftest explode-a-palooza
  ;; Explode-a-palooza - Gain 5 credits when Challenger accesses it
  (do-game
    (new-game (default-contestant [(qty "Explode-a-palooza" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Explode-a-palooza" "New remote")
    (take-credits state :contestant)
    (run-empty-server state :remote1)
    (prompt-choice :challenger "Access")
    (prompt-choice :challenger "Steal")
    (prompt-choice :contestant "Yes")
    (is (= 12 (:credit (get-contestant))) "Gained 5 credits")))

(deftest explode-ttw
  ;; Explode-a-palooza - Interaction with The Turning Wheel. Issue #1717.
  (do-game
    (new-game (default-contestant [(qty "Explode-a-palooza" 3)])
              (default-challenger [(qty "The Turning Wheel" 1)]))
    (starting-hand state :contestant ["Explode-a-palooza" "Explode-a-palooza"])
    (play-from-hand state :contestant "Explode-a-palooza" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "The Turning Wheel")
    (run-empty-server state :remote1)
    (prompt-choice :challenger "Access")
    (prompt-choice :contestant "Yes")
    (prompt-choice :challenger "Steal")
    (let [ttw (get-muthereff state 0)]
      (is (= 0 (get-counters (refresh ttw) :power)) "TTW did not gain counters")
      (is (= 1 (count (:scored (get-challenger)))) "Challenger stole Explodapalooza")
      (is (= 12 (:credit (get-contestant))) "Gained 5 credits")
      (run-empty-server state :rd)
      (prompt-choice :challenger "Access")
      (prompt-choice :contestant "Yes")
      (prompt-choice :challenger "Steal")
      (is (= 0 (get-counters (refresh ttw) :power)) "TTW did not gain counters")
      (is (= 2 (count (:scored (get-challenger)))) "Challenger stole Explodapalooza")
      (is (= 17 (:credit (get-contestant))) "Gained 5 credits"))))

(deftest fetal-ai-damage
  ;; Fetal AI - damage on access
  (do-game
    (new-game (default-contestant [(qty "Fetal AI" 3)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
    (play-from-hand state :contestant "Fetal AI" "New remote")
    (take-credits state :contestant 2)
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Access")
    (prompt-choice :challenger "Yes")
    (is (= 3 (count (:hand (get-challenger)))) "Challenger took 2 net damage from Fetal AI")
    (is (= 3 (:credit (get-challenger))) "Challenger paid 2cr to steal Fetal AI")
    (is (= 1 (count (:scored (get-challenger)))) "Challenger stole Fetal AI")))

(deftest fetal-ai-cant-afford
  ;; Fetal AI - can't afford to steal
  (do-game
    (new-game (default-contestant [(qty "Fetal AI" 3)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]))
    (play-from-hand state :contestant "Fetal AI" "New remote")
    (take-credits state :contestant 2)
    (core/lose state :challenger :credit 5)
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Yes")
    (is (= 3 (count (:hand (get-challenger)))) "Challenger took 2 net damage from Fetal AI")
    (is (= 0 (count (:scored (get-challenger)))) "Challenger could not steal Fetal AI")))

(deftest genetic-resequencing
  ;; Genetic Resequencing - Place 1 agenda counter on a scored agenda
  (do-game
    (new-game (default-contestant [(qty "Genetic Resequencing" 1) (qty "Braintrust" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Braintrust" "New remote")
    (play-from-hand state :contestant "Braintrust" "New remote")
    (play-from-hand state :contestant "Genetic Resequencing" "New remote")
    (let [bt1 (get-content state :remote1 0)
          bt2 (get-content state :remote2 0)
          gr (get-content state :remote3 0)]
      (score-agenda state :contestant bt1)
      (let [btscored (get-in @state [:contestant :scored 0])]
        (is (= 0 (get-counters (refresh btscored) :agenda)) "No agenda counters on scored Braintrust")
        (score-agenda state :contestant gr)
        (prompt-select :contestant bt2)
        (is (zero? (get-counters (refresh bt2) :agenda))
            "No agenda counters on installed Braintrust; not a valid target")
        (prompt-select :contestant btscored)
        (is (= 1 (get-counters (refresh btscored) :agenda))
            "1 agenda counter placed on scored Braintrust")))))

(deftest government-contracts
  ;; Government Contracts - Spend 2 clicks for 4 credits
  (do-game
    (new-game (default-contestant [(qty "Government Contracts" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Government Contracts" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (is (= 2 (:click (get-contestant))))
    (card-ability state :contestant (get-in @state [:contestant :scored 0]) 0)
    (is (= 0 (:click (get-contestant))) "Spent 2 clicks")
    (is (= 9 (:credit (get-contestant))) "Gained 4 credits")))

(deftest high-risk-investment
  ;; High-Risk Investment - Gain 1 agenda counter when scored; spend it to gain credits equal to Challenger's credits
  (do-game
    (new-game (default-contestant [(qty "High-Risk Investment" 1)])
              (default-challenger))
    (play-from-hand state :contestant "High-Risk Investment" "New remote")
    (let [hri (get-content state :remote1 0)]
      (score-agenda state :contestant hri)
      (let [hriscored (get-in @state [:contestant :scored 0])]
        (is (= 1 (get-counters (refresh hriscored) :agenda)) "Has 1 agenda counter")
        (take-credits state :contestant)
        (is (= 7 (:credit (get-contestant))))
        (take-credits state :challenger)
        (is (= 9 (:credit (get-challenger))))
        (card-ability state :contestant hriscored 0)
        (is (= 16 (:credit (get-contestant))) "Gained 9 credits")
        (is (= 2 (:click (get-contestant))) "Spent 1 click")
        (is (= 0 (get-counters (refresh hriscored) :agenda)) "Spent agenda counter")))))

(deftest hostile-takeover
  ;; Hostile Takeover - Gain 7 credits and take 1 bad publicity
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (let [ht (get-content state :remote1 0)]
      (score-agenda state :contestant ht)
      (is (= 12 (:credit (get-contestant))) "Gain 7 credits")
      (is (= 1 (:bad-publicity (get-contestant))) "Take 1 bad publicity"))))

(deftest labyrinthine-servers
  ;; Labyrinthine Servers - Prevent the Challenger from jacking out as long as there is still a power counter
  (do-game
    (new-game (default-contestant [(qty "Labyrinthine Servers" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Labyrinthine Servers" "New remote")
    (play-from-hand state :contestant "Labyrinthine Servers" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (score-agenda state :contestant (get-content state :remote2 0))
    (take-credits state :contestant)
    (let [ls1 (get-in @state [:contestant :scored 0])
          ls2 (get-in @state [:contestant :scored 1])]
      (is (= 2 (get-counters (refresh ls1) :power)))
      (is (= 2 (get-counters (refresh ls2) :power)))
      ;; don't use token
      (run-on state "HQ")
      (run-jack-out state)
      (is (:run @state) "Jack out prevent prompt")
      (prompt-choice :contestant "Done")
      (is (not (:run @state)) "Contestant does not prevent the jack out, run ends")
      ;; use token
      (run-on state "HQ")
      (run-jack-out state)
      (card-ability state :contestant ls1 0)
      (card-ability state :contestant ls2 0)
      (card-ability state :contestant ls1 0)
      (prompt-choice :contestant "Done")
      (is (:run @state) "Jack out prevented, run is still ongoing")
      (is (true? (get-in @state [:run :cannot-jack-out])) "Cannot jack out flag is in effect")
      (run-successful state)
      (is (not (:run @state)))
      ;; one Labyrinthine is empty but the other still has one token, ensure prompt still occurs
      (is (= 0 (get-counters (refresh ls1) :power)))
      (is (= 1 (get-counters (refresh ls2) :power)))
      (run-on state "HQ")
      (run-jack-out state)
      (is (:run @state))
      (card-ability state :contestant ls2 0)
      (prompt-choice :contestant "Done")
      (is (true? (get-in @state [:run :cannot-jack-out])))
      (run-successful state)
      (is (not (:run @state)))
      ;; no more tokens
      (run-on state "HQ")
      (run-jack-out state)
      (is (not (:run @state)) "No jack out prevent prompt"))))

(deftest medical-breakthrough
  ;; Medical Breakthrough - Lower advancement requirement by 1 for each scored/stolen copy
  (do-game
    (new-game (default-contestant [(qty "Medical Breakthrough" 3) (qty "Hedge Fund" 3)])
              (default-challenger))
    (play-from-hand state :contestant "Medical Breakthrough" "New remote")
    (play-from-hand state :contestant "Medical Breakthrough" "New remote")
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (run-empty-server state :remote1)
    (prompt-choice :challenger "Steal")
    (take-credits state :challenger)
    (let [mb2 (get-content state :remote2 0)]
      (core/advance state :contestant {:card (refresh mb2)})
      (core/advance state :contestant {:card (refresh mb2)})
      (core/advance state :contestant {:card (refresh mb2)})
      (core/score state :contestant {:card (refresh mb2)})
      (is (= 2 (:agenda-point (get-contestant))) "Only needed 3 advancements to score")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (play-from-hand state :contestant "Medical Breakthrough" "New remote")
      (let [mb3 (get-content state :remote3 0)]
        (core/advance state :contestant {:card (refresh mb3)})
        (core/advance state :contestant {:card (refresh mb3)})
        (core/score state :contestant {:card (refresh mb3)})
        (is (= 4 (:agenda-point (get-contestant))) "Only needed 2 advancements to score")))))

(deftest napd-contract
  ;; NAPD Contract - Requires 4 credits to steal; scoring requirement increases with bad publicity
  (do-game
    (new-game (default-contestant [(qty "NAPD Contract" 1)])
              (default-challenger))
    (play-from-hand state :contestant "NAPD Contract" "New remote")
      (let [napd (get-content state :remote1 0)]
        (core/advance state :contestant {:card (refresh napd)})
        (core/advance state :contestant {:card (refresh napd)})
        (take-credits state :contestant)
        (core/lose state :challenger :credit 2)
        (run-empty-server state "Server 1")
        (prompt-choice :challenger "Yes")
        (is (= 0 (count (:scored (get-challenger)))) "Challenger could not steal NAPD Contract")
        (is (= 3 (:credit (get-challenger))) "Challenger couldn't afford to steal, so no credits spent")
        (take-credits state :challenger)
        (core/gain state :contestant :bad-publicity 1)
        (core/advance state :contestant {:card (refresh napd)})
        (core/advance state :contestant {:card (refresh napd)})
        (core/score state :contestant {:card (refresh napd)})
        (is (not (nil? (get-content state :remote1 0)))
            "Contestant can't score with 4 advancements because of BP")
        (core/advance state :contestant {:card (refresh napd)})
        (core/score state :contestant {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-contestant))) "Scored NAPD for 2 points after 5 advancements"))))

(deftest napd-contract-contestantorate-scandal
  ;; NAPD Contract - scoring requirement increases with bad publicity from Contestantorate Scandal
  (do-game
    (new-game (default-contestant [(qty "NAPD Contract" 1)])
              (default-challenger [(qty "Contestantorate Scandal" 1)]))
    (play-from-hand state :contestant "NAPD Contract" "New remote")
      (let [napd (get-content state :remote1 0)]
        (core/advance state :contestant {:card (refresh napd)})
        (core/advance state :contestant {:card (refresh napd)})
        (take-credits state :contestant)
        (play-from-hand state :challenger "Contestantorate Scandal")
        (take-credits state :challenger)
        (core/advance state :contestant {:card (refresh napd)})
        (core/advance state :contestant {:card (refresh napd)})
        (core/score state :contestant {:card (refresh napd)})
        (is (not (nil? (get-content state :remote1 0)))
            "Contestant can't score with 4 advancements because of BP")
        (core/advance state :contestant {:card (refresh napd)})
        (core/score state :contestant {:card (refresh napd)})
        (is (= 2 (:agenda-point (get-contestant))) "Scored NAPD for 2 points after 5 advancements"))))

(deftest net-quarantine
  ;; The Challenger's base link strength is reduced to 0 during the first trace each turn.
  ;; Whenever the Challenger increases his or her link strength by spending credits, gain 1 for every 2 spent.
  (do-game
    (new-game (default-contestant [(qty "Net Quarantine" 1)])
              (default-challenger))
    (core/gain state :challenger :link 1)
    (core/gain state :contestant :click 3)
    (play-from-hand state :contestant "Net Quarantine" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (is (= 5 (:credit (get-contestant))) "Contestant has 5 credits")
    (is (= 1 (:link (get-challenger))) "Challenger has 1 link")
    (core/contestant-trace-prompt state {:title "/trace command" :side :contestant} {:base 1})
    (prompt-choice :contestant 0)
    (is (= 0 (:link (get-challenger))) "Challenger has 0 link")
    (prompt-choice :challenger 3)
    (is (= 1 (:link (get-challenger))) "Challenger has 1 link again")
    (is (= 6 (:credit (get-contestant))) "Contestant gained a credit from NQ")
    ; second trace of turn - no link reduction
    (core/contestant-trace-prompt state {:title "/trace command" :side :contestant} {:base 1})
    (prompt-choice :contestant 0)
    (is (= 1 (:link (get-challenger))) "Challenger has 1 link")
    (prompt-choice :challenger 2)
    (is (= 7 (:credit (get-contestant))) "Contestant gained a credit from NQ")))

(deftest nisei-mk-ii-step-43
  ;; Nisei MK II - Remove hosted counter to ETR, check this works in 4.3
  (do-game
   (new-game (default-contestant [(qty "Nisei MK II" 1)])
             (default-challenger))
   (play-from-hand state :contestant "Nisei MK II" "New remote")
   (score-agenda state :contestant (get-content state :remote1 0))
   (let [scored-nisei (get-in @state [:contestant :scored 0])]
     (is (= 1 (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has one counter")
     (take-credits state :contestant)

     (run-on state "HQ")
     (run-phase-43 state)
     (card-ability state :contestant (refresh scored-nisei) 0)
     (prompt-choice :contestant "Done") ; close 4.3 contestant
     (is (not (:run @state)) "Run ended by using Nisei counter")
     (is (= 0 (get-counters (refresh scored-nisei) :agenda)) "Scored Nisei has no counters"))))

(deftest oaktown-renovation
  ;; Oaktown Renovation - Installed face up, gain credits with each conventional advancement
  (do-game
    (new-game (default-contestant [(qty "Oaktown Renovation" 1) (qty "Shipment from SanSan" 1)])
              (default-challenger))
    (core/gain state :contestant :click 3)
    (play-from-hand state :contestant "Oaktown Renovation" "New remote")
    (let [oak (get-content state :remote1 0)]
      (is (get-in (refresh oak) [:rezzed]) "Oaktown installed face up")
      (core/advance state :contestant {:card (refresh oak)})
      (is (= 6 (:credit (get-contestant))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (play-from-hand state :contestant "Shipment from SanSan")
      (prompt-choice :contestant "2")
      (prompt-select :contestant oak)
      (is (= 3 (:advance-counter (refresh oak))))
      (is (= 6 (:credit (get-contestant))) "No credits gained due to advancements being placed")
      (core/advance state :contestant {:card (refresh oak)})
      (is (= 7 (:credit (get-contestant))) "Spent 1 credit to advance, gained 2 credits from Oaktown")
      (core/advance state :contestant {:card (refresh oak)})
      (is (= 5 (:advance-counter (refresh oak))))
      (is (= 9 (:credit (get-contestant)))
          "Spent 1 credit to advance, gained 3 credits from Oaktown"))))

(deftest obokata-protocol
  ;; Pay 4 net damage to steal.  Challenger win retained on flatline
  (do-game
    (new-game (make-deck "Jinteki: Personal Evolution" [(qty "Obokata Protocol" 10)])
              (default-challenger [(qty "Sure Gamble" 4)]))
    (play-from-hand state :contestant "Obokata Protocol" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :agenda-point 6)
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Yes")
    (is (= 4 (count (:discard (get-challenger)))) "Challenger paid 4 net damage")
    (is (= :challenger (:winner @state)) "Challenger wins")
    (is (= "Agenda" (:reason @state)) "Win condition reports agenda points")
    (is (last-log-contains? state "wins the game") "PE did not fire")))

(deftest personality-profiles
  ;; Personality Profiles - Full test
  (do-game
    (new-game (default-contestant [(qty "Personality Profiles" 1)])
              (default-challenger [(qty "Self-modifying Code" 1) (qty "Clone Chip" 1)
                               (qty "Corroder" 1) (qty "Patron" 2)]))
    (starting-hand state :challenger ["Self-modifying Code" "Clone Chip" "Patron" "Patron"])
    (score-agenda state :contestant (find-card "Personality Profiles" (:hand (get-contestant))))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Self-modifying Code")
    (play-from-hand state :challenger "Clone Chip")
    (let [smc (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger smc 0)
      (prompt-choice :challenger (find-card "Corroder" (:deck (get-challenger))))
      (is (= 2 (count (:discard (get-challenger))))))
    (let [chip (get-in @state [:challenger :rig :hazard 0])]
      (card-ability state :challenger chip 0)
      (prompt-select :challenger (find-card "Self-modifying Code" (:discard (get-challenger))))
      (is (second-last-log-contains? state "Patron")
          "Personality Profiles trashed card name is in log")
      (is (= 3 (count (:discard (get-challenger))))))))

(deftest personality-profiles-empty-hand
  ;; Personality Profiles - Ensure effects still fire with an empty hand, #1840
  (do-game
    (new-game (default-contestant [(qty "Personality Profiles" 1)])
              (default-challenger [(qty "Self-modifying Code" 1) (qty "Clone Chip" 1)
                               (qty "Corroder" 1)]))
    (starting-hand state :challenger ["Self-modifying Code" "Clone Chip"])
    (score-agenda state :contestant (find-card "Personality Profiles" (:hand (get-contestant))))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Self-modifying Code")
    (play-from-hand state :challenger "Clone Chip")
    (let [smc (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger smc 0)
      (prompt-choice :challenger (find-card "Corroder" (:deck (get-challenger))))
      (let [cor (get-in @state [:challenger :rig :resource 0])]
        (is (not (nil? cor)))
        (is (= (:title cor) "Corroder"))
        (is (= "Self-modifying Code" (:title (first (:discard (get-challenger))))))))
    (let [chip (get-in @state [:challenger :rig :hazard 0])]
      (card-ability state :challenger chip 0)
      (prompt-select :challenger (find-card "Self-modifying Code" (:discard (get-challenger))))
      (let [smc (get-in @state [:challenger :rig :resource 1])]
        (is (not (nil? smc)))
        (is (= (:title smc) "Self-modifying Code"))
        (is (= "Clone Chip" (:title (first (:discard (get-challenger))))))))))

(deftest philotic-entanglement
  ;; Philotic Entanglement - When scored, do 1 net damage for each agenda in the Challenger's score area
  (do-game
    (new-game (default-contestant [(qty "Philotic Entanglement" 1) (qty "House of Knives" 3)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Cache" 2)]))
    (play-from-hand state :contestant "House of Knives" "New remote")
    (play-from-hand state :contestant "House of Knives" "New remote")
    (play-from-hand state :contestant "House of Knives" "New remote")
    (take-credits state :contestant)
    (run-empty-server state :remote1)
    (prompt-choice :challenger "Steal")
    (run-empty-server state :remote2)
    (prompt-choice :challenger "Steal")
    (run-empty-server state :remote3)
    (prompt-choice :challenger "Steal")
    (is (= 3 (count (:scored (get-challenger)))))
    (take-credits state :challenger)
    (play-from-hand state :contestant "Philotic Entanglement" "New remote")
    (score-agenda state :contestant (get-content state :remote4 0))
    (is (= 2 (:agenda-point (get-contestant))))
    (is (= 3 (count (:discard (get-challenger)))) "Dealt 3 net damage upon scoring")))
	  
(deftest posted-bounty-yes
  ;; Posted Bounty - Forfeiting takes 1 bad publicity
  (do-game
    (new-game (default-contestant [(qty "Posted Bounty" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Posted Bounty" "New remote")
    (let [pb (get-content state :remote1 0)]
      (score-agenda state :contestant pb)
	  (prompt-choice :contestant "Yes")
	  (is (= 0 (:agenda-point (get-contestant))) "Forfeiting Posted Bounty nullifies agenda points")
      (is (= 1 (:bad-publicity (get-contestant))) "Forfeiting takes 1 bad publicity"))
	  (is (= 1 (get-in @state [:challenger :tag])) "Challenger receives 1 tag forfeiting Posted Bounty")))
	  
(deftest posted-bounty-no
  ;; Posted Bounty - Choosing not to forfeit scores normally
  (do-game
    (new-game (default-contestant [(qty "Posted Bounty" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Posted Bounty" "New remote")
    (let [pb (get-content state :remote1 0)]
      (score-agenda state :contestant pb)
	  (prompt-choice :contestant "No")
	  (is (= 1 (:agenda-point (get-contestant))))
      (is (= 0 (:bad-publicity (get-contestant)))))
	  (is (= 0 (get-in @state [:challenger :tag])))))

(deftest profiteering
  ;; Profiteering - Gain 5 credits per bad publicity taken
  (do-game
    (new-game (default-contestant [(qty "Profiteering" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Profiteering" "New remote")
    (let [prof (get-content state :remote1 0)]
      (score-agenda state :contestant prof)
      (prompt-choice :contestant "3")
      (is (= 1 (:agenda-point (get-contestant))))
      (is (= 3 (:bad-publicity (get-contestant))) "Took 3 bad publicity")
      (is (= 20 (:credit (get-contestant))) "Gained 15 credits"))))

(deftest project-ares
  ;; Project Ares - Full test
  (do-game
    (new-game (default-contestant [(qty "Project Ares" 2)])
              (default-challenger [(qty "Clone Chip" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Clone Chip")
    (take-credits state :challenger)
    (score-agenda state :contestant (find-card "Project Ares" (:hand (get-contestant))))
    (is (empty? (get-in @state [:challenger :prompt])) "No prompt for Challenger if scored with 4 advancement tokens")
    (core/gain state :contestant :click 4)
    (play-from-hand state :contestant "Project Ares" "New remote")
    (let [ares (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh ares)})
      (core/advance state :contestant {:card (refresh ares)})
      (core/advance state :contestant {:card (refresh ares)})
      (core/advance state :contestant {:card (refresh ares)})
      (core/advance state :contestant {:card (refresh ares)})
      (core/advance state :contestant {:card (refresh ares)})
      (is (= 6 (:advance-counter (refresh ares)))
      (core/score state :contestant {:card (refresh ares)}))
      (is (prompt-is-card? :challenger ares) "Challenger has Ares prompt to trash installed cards"))
    (prompt-select :challenger (find-card "Clone Chip" (:hazard (:rig (get-challenger)))))
    (is (empty? (get-in @state [:challenger :prompt])) "Challenger must trash 2 cards but only has 1 card in rig, prompt ended")
    (is (= 1 (count (:discard (get-challenger)))))
    (is (= 1 (:bad-publicity (get-contestant))))))

(deftest project-beale
  ;; Project Beale - Extra agenda points for over-advancing
  (do-game
    (new-game (default-contestant [(qty "Project Beale" 2)])
              (default-challenger))
    (core/gain state :contestant :click 8 :credit 8)
    (play-from-hand state :contestant "Project Beale" "New remote")
    (let [pb1 (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh pb1)})
      (core/advance state :contestant {:card (refresh pb1)})
      (core/advance state :contestant {:card (refresh pb1)})
      (core/advance state :contestant {:card (refresh pb1)})
      (core/score state :contestant {:card (refresh pb1)})
      (is (= 2 (:agenda-point (get-contestant))) "Only 4 advancements: scored for standard 2 points")
      (play-from-hand state :contestant "Project Beale" "New remote")
        (let [pb2 (get-content state :remote2 0)]
          (core/advance state :contestant {:card (refresh pb2)})
          (core/advance state :contestant {:card (refresh pb2)})
          (core/advance state :contestant {:card (refresh pb2)})
          (core/advance state :contestant {:card (refresh pb2)})
          (core/advance state :contestant {:card (refresh pb2)})
          (core/score state :contestant {:card (refresh pb2)})
          (is (= 5 (:agenda-point (get-contestant))) "5 advancements: scored for 3 points")))))

(deftest puppet-master
  ;; Puppet Master - game progresses if no valid targets. Issue #1661.
  (do-game
    (new-game (default-contestant [(qty "Puppet Master" 1)])
              (default-challenger))
    (play-from-hand state :contestant "Puppet Master" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (run-empty-server state :archives)
    (prompt-choice :contestant "Done")
    (is (empty? (:prompt (get-challenger))) "Challenger's waiting prompt resolved")))

(deftest rebranding-team
  ;; Rebranding Team - Full test
  (do-game
    (new-game (default-contestant [(qty "Rebranding Team" 1) (qty "Launch Campaign" 1) (qty "City Surveillance" 1)
                             (qty "Jackson Howard" 1) (qty "Museum of History" 1)])
              (default-challenger))
    (score-agenda state :contestant (find-card "Rebranding Team" (:hand (get-contestant))))
    (is (core/has-subtype? (find-card "Launch Campaign" (:hand (get-contestant))) "Advertisement"))
    (is (core/has-subtype? (find-card "City Surveillance" (:hand (get-contestant))) "Advertisement"))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-contestant))) "Advertisement"))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-contestant))) "Executive"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Advertisement"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Alliance"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Ritzy"))
    (core/move state :contestant (find-card "Rebranding Team" (:scored (get-contestant))) :deck)
    (is (core/has-subtype? (find-card "Launch Campaign" (:hand (get-contestant))) "Advertisement"))
    (is (not (core/has-subtype? (find-card "City Surveillance" (:hand (get-contestant))) "Advertisement")))
    (is (not (core/has-subtype? (find-card "Jackson Howard" (:hand (get-contestant))) "Advertisement")))
    (is (core/has-subtype? (find-card "Jackson Howard" (:hand (get-contestant))) "Executive"))
    (is (not (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Advertisement")))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Alliance"))
    (is (core/has-subtype? (find-card "Museum of History" (:hand (get-contestant))) "Ritzy"))))

(deftest reeducation
  ;; Reeducation - Simple test
  (do-game
    (new-game (default-contestant [(qty "Reeducation" 1) (qty "Sweeps Week" 1) (qty "Hedge Fund" 1)
                             (qty "Jackson Howard" 1) (qty "Gutenberg" 1)])
              (default-challenger [(qty "Self-modifying Code" 1) (qty "Clone Chip" 1)
                               (qty "Corroder" 1) (qty "Sure Gamble" 1) (qty "Desperado" 1)]))
    (starting-hand state :contestant ["Reeducation" "Sweeps Week"])
    (starting-hand state :challenger ["Self-modifying Code"])
    (score-agenda state :contestant (find-card "Reeducation" (:hand (get-contestant))))
    (is (prompt-is-type? :challenger :waiting) "Challenger has wait prompt")
    (is (= 1 (count (get-in @state [:contestant :hand]))))
    (is (= 1 (count (get-in @state [:challenger :hand]))))
    (prompt-choice :contestant (find-card "Sweeps Week" (:hand (get-contestant)))) ; put Sweeps Week at bottom of R&D
    (prompt-choice :contestant "Done") ; finished selecting cards
    (prompt-choice :contestant "Done") ; contestant prompt for Done/Start Over
    (is (= "Sweeps Week" (:title (last (:deck (get-contestant))))))
    (is (= "Self-modifying Code" (:title (last (:deck (get-challenger))))))
    (is (= 1 (count (get-in @state [:contestant :hand]))))
    (is (= 0 (count (get-in @state [:challenger :hand]))))))

(deftest reeducation-extra-cards
  ;; Reeducation - If Contestant is adding more cards in HQ than Challenger has in their Grip, Challenger
  ;; is not 'able' to resolve the effect and doesn't have to add to bottom of Stack
  (do-game
    (new-game (default-contestant [(qty "Reeducation" 1) (qty "Sweeps Week" 1) (qty "Hedge Fund" 1)
                             (qty "Jackson Howard" 1) (qty "Gutenberg" 1)])
              (default-challenger [(qty "Self-modifying Code" 1) (qty "Clone Chip" 1)
                               (qty "Corroder" 1) (qty "Sure Gamble" 1) (qty "Desperado" 1)]))
    (starting-hand state :contestant ["Reeducation" "Sweeps Week" "Hedge Fund"])
    (starting-hand state :challenger ["Self-modifying Code"])
    (score-agenda state :contestant (find-card "Reeducation" (:hand (get-contestant))))
    (is (prompt-is-type? :challenger :waiting) "Challenger has wait prompt")
    (is (= 2 (count (get-in @state [:contestant :hand]))))
    (is (= 1 (count (get-in @state [:challenger :hand]))))
    (prompt-choice :contestant (find-card "Sweeps Week" (:hand (get-contestant))))
    (prompt-choice :contestant (find-card "Hedge Fund" (:hand (get-contestant)))) ; this is the bottom card of R&D
    (prompt-choice :contestant "Done") ; finished selecting cards
    (prompt-choice :contestant "Done") ; contestant prompt for Done/Start Over
    (is (= "Hedge Fund" (:title (last (:deck (get-contestant))))))
    (is (= "Sweeps Week" (:title (last (butlast (:deck (get-contestant)))))))
    (is (= "Self-modifying Code" (:title (first (:hand (get-challenger))))))
    (is (= 2 (count (get-in @state [:contestant :hand]))))
    (is (= 1 (count (get-in @state [:challenger :hand]))))))

(deftest ssl-endorsement-scored
  ;; SSL Endorsement - gain credits when in contestant score area before turn begins
  (do-game
    (new-game (default-contestant [(qty "SSL Endorsement" 1)])
              (default-challenger))
    (score-agenda state :contestant (find-card "SSL Endorsement" (:hand (get-contestant))))
    (take-credits state :challenger)

    (is (not-empty (:prompt (get-contestant))) "Contestant prompted to take credits")
    (is (= 5 (:credit (get-contestant))) "Contestant starts with 5 credits")
    (prompt-choice :contestant "Yes")
    (is (= 8 (:credit (get-contestant))) "Contestant gains 3 credits")
    (take-credits state :challenger)

    (is (= 8 (:credit (get-contestant))) "Contestant starts with 8 credits")
    (prompt-choice :contestant "No")
    (is (= 8 (:credit (get-contestant))) "Contestant doesn't gain 3 credits")
    (take-credits state :challenger)

    (is (= 8 (:credit (get-contestant))) "Contestant starts with 8 credits")
    (prompt-choice :contestant "Yes")
    (is (= 11 (:credit (get-contestant))) "Contestant gains 3 credits")
    (take-credits state :challenger)

    (is (= 11 (:credit (get-contestant))) "Contestant starts with 11 credits")
    (prompt-choice :contestant "Yes")
    (is (= 14 (:credit (get-contestant))) "Contestant gains 3 credits")
    (take-credits state :challenger)

    (is (empty? (:prompt (get-contestant))) "Not prompted when out of money")))

(deftest ssl-endorsement-stolen
  ;; SSL Endorsement - gain credits when in challenger score area before turn begins
  (do-game
    (new-game (default-contestant [(qty "SSL Endorsement" 1)])
              (default-challenger))
    (play-from-hand state :contestant "SSL Endorsement" "New remote")
    (take-credits state :contestant)

    (run-on state "Server 1")
    (run-successful state)
    (prompt-choice :challenger "Steal")
    (take-credits state :challenger)

    (is (not-empty (:prompt (get-contestant))) "Contestant prompted to take credits")
    (is (= 7 (:credit (get-contestant))) "Contestant starts with 7 credits")
    (prompt-choice :contestant "Yes")
    (is (= 10 (:credit (get-contestant))) "Contestant gains 3 credits")
    (take-credits state :challenger)

    (is (= 10 (:credit (get-contestant))) "Contestant starts with 10 credits")
    (prompt-choice :contestant "No")
    (is (= 10 (:credit (get-contestant))) "Contestant doesn't gain 3 credits")
    (take-credits state :challenger)

    (is (= 10 (:credit (get-contestant))) "Contestant starts with 10 credits")
    (prompt-choice :contestant "Yes")
    (is (= 13 (:credit (get-contestant))) "Contestant gains 3 credits")
    (take-credits state :challenger)

    (is (= 13 (:credit (get-contestant))) "Contestant starts with 13 credits")
    (prompt-choice :contestant "Yes")
    (is (= 16 (:credit (get-contestant))) "Contestant gains 3 credits")
    (take-credits state :challenger)

    (is (empty? (:prompt (get-contestant))) "Not prompted when out of money")))

(deftest ssl-endorsement-stolen-swapped
  ;; SSL Endorsement - don't double register event when agenda is swapped
  (do-game
    (new-game (default-contestant [(qty "SSL Endorsement" 1) (qty "Breaking News" 1)
                             (qty "Exchange of Information" 1)])
              (default-challenger))
    (play-from-hand state :contestant "SSL Endorsement" "New remote")
    (score-agenda state :contestant (find-card "Breaking News" (:hand (get-contestant))))
    (take-credits state :contestant)

    (run-on state "Server 1")
    (run-successful state)
    (prompt-choice :challenger "Steal")
    (take-credits state :challenger)

    (is (not-empty (:prompt (get-contestant))) "Contestant prompted to take credits")
    (is (= 7 (:credit (get-contestant))) "Contestant starts with 7 credits")
    (prompt-choice :contestant "Yes")
    (is (= 10 (:credit (get-contestant))) "Contestant gains 3 credits")
    (core/gain state :challenger :tag 1)
    (play-from-hand state :contestant "Exchange of Information")
    (prompt-select :contestant (find-card "SSL Endorsement" (:scored (get-challenger))))
    (prompt-select :contestant (find-card "Breaking News" (:scored (get-contestant))))
    (take-credits state :challenger)

    (is (= 10 (:credit (get-contestant))) "Contestant starts with 10 credits")
    (prompt-choice :contestant "No")
    (is (empty? (:prompt (get-contestant))) "Not double prompted for credits")
    (is (= 10 (:credit (get-contestant))) "Contestant doesn't gain 3 credits")
    (take-credits state :challenger)

    (is (= 10 (:credit (get-contestant))) "Contestant starts with 10 credits")
    (prompt-choice :contestant "Yes")
    (is (= 13 (:credit (get-contestant))) "Contestant gains 3 credits")
    (take-credits state :challenger)

    (is (= 13 (:credit (get-contestant))) "Contestant starts with 13 credits")
    (prompt-choice :contestant "Yes")
    (is (= 16 (:credit (get-contestant))) "Contestant gains 3 credits")
    (take-credits state :challenger)

    (is (empty? (:prompt (get-contestant))) "Not prompted when out of money")))

(deftest sentinel-defense-resource
  ;; Sentinel Defense Resource - Doesn't fire if brain damage is prevented
  (do-game
    (new-game (default-contestant [(qty "Sentinel Defense Resource" 1) (qty "Viktor 1.0" 1)])
              (default-challenger [(qty "Feedback Filter" 1) (qty "Sure Gamble" 3)]))
    (score-agenda state :contestant (find-card "Sentinel Defense Resource" (:hand (get-contestant))))
    (play-from-hand state :contestant "Viktor 1.0" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Feedback Filter")
    (let [viktor (get-character state :hq 0)
          ff (get-in @state [:challenger :rig :hazard 0])]
      (run-on state "HQ")
      (core/rez state :contestant viktor)
      (card-subroutine state :contestant viktor 0)
      (prompt-choice :challenger "Done") ;don't prevent the brain damage
      (is (= 1 (count (:discard (get-challenger)))))
      (is (= 1 (:brain-damage (get-challenger))))
      (prompt-choice :challenger "Done") ;so we take the net, but don't prevent it either
      (is (= 2 (count (:discard (get-challenger)))))
      (card-subroutine state :contestant viktor 0)
      (card-ability state :challenger ff 1) ;prevent the brain damage this time
      (prompt-choice :challenger "Done")
      (is (= 3 (count (:discard (get-challenger)))) "Feedback filter trashed, didn't take another net damage")
      (is (= 1 (:brain-damage (get-challenger)))))))

;; OHG still not working...
(deftest tgtbt
  ;; TGTBT - Give the Challenger 1 tag when they access
  (do-game
    (new-game (default-contestant [(qty "TGTBT" 2) (qty "Old Hollywood Grid" 1)])
              (default-challenger))
    (play-from-hand state :contestant "TGTBT" "New remote")
    (play-from-hand state :contestant "Old Hollywood Grid" "Server 1")
    (play-from-hand state :contestant "TGTBT" "New remote")
    (take-credits state :contestant)
    (let [tg1 (get-content state :remote1 0)
          ohg (get-content state :remote1 1)]
      (run-on state "Server 1")
      (core/rez state :contestant ohg)
      (run-successful state)
      (prompt-select :challenger tg1)
      ;; Accesses TGTBT but can't steal
      (prompt-choice :challenger "Access")
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 tag from accessing without stealing")
      (prompt-select :challenger ohg))
    (prompt-choice :challenger "Yes") ; Trashes OHG
    (run-empty-server state "Server 2")
    ;; Accesses TGTBT and can steal
    (prompt-choice :challenger "Access")
    (prompt-choice :challenger "Steal")

    (is (= 2 (:tag (get-challenger))) "Challenger took 1 tag from accessing and stealing")))

(deftest the-cleaners
  ;; The Cleaners - Bonus damage
  (do-game
    (new-game (default-contestant [(qty "The Cleaners" 1) (qty "Scorched Earth" 1)])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
    (play-from-hand state :contestant "The Cleaners" "New remote")
    (let [clean (get-content state :remote1 0)]
      (score-agenda state :contestant clean)
      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "Scorched Earth")
      (is (= 0 (count (:hand (get-challenger)))) "5 damage dealt to Challenger"))))

(deftest the-cleaners-cybernetics
  ;; The Cleaners - No bonus damage when challenger "suffers" damage
  (do-game
    (new-game (default-contestant [(qty "The Cleaners" 1)])
              (default-challenger [(qty "Respirocytes" 3)]))
    (play-from-hand state :contestant "The Cleaners" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Respirocytes")
    (is (= 1 (count (:hand (get-challenger)))) "Only 1 damage dealt to Challenger from Cybernetics")))

(deftest the-future-perfect
  ;; The Future Perfect - cannot steal on failed psi game (if not installed)
  (do-game
    (new-game (default-contestant [(qty "The Future Perfect" 2)])
              (default-challenger))
    (play-from-hand state :contestant "The Future Perfect" "New remote")
    (take-credits state :contestant)

    (testing "No steal on not-equal Psi game"
      (run-empty-server state "HQ")
      (prompt-choice :challenger "Access")
      (prompt-choice :contestant "1 [Credits]")
      (prompt-choice :challenger "0 [Credits]")
      ;; Cannot steal prompt
      (prompt-choice :challenger "OK")
      (is (= 0 (:agenda-point (get-challenger))) "Challenger did not steal TFP"))

    (testing "Successful steal on equal Psi game"
      (run-empty-server state "HQ")
      (prompt-choice :challenger "Access")
      (prompt-choice :contestant "1 [Credits]")
      (prompt-choice :challenger "1 [Credits]")
      (prompt-choice :challenger "Steal")
      (is (= 3 (:agenda-point (get-challenger))) "Challenger stole TFP"))

    (testing "No Psi game and successful steal when installed"
      (run-empty-server state "Server 1")
      (prompt-choice :challenger "Steal")
      (is (= 6 (:agenda-point (get-challenger))) "Challenger stole TFP - no Psi game on installed TFP"))))

(deftest underway-renovation
  ;; Underway Renovation - Mill the Challenger when advanced
  (do-game
    (new-game (default-contestant [(qty "Underway Renovation" 1) (qty "Shipment from SanSan" 1)])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (play-from-hand state :contestant "Underway Renovation" "New remote")
    (let [ur (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh ur)})
      (is (last-log-contains? state "Sure Gamble")
          "Underway Renovation trashed card name is in log")
      ; check for #2370
      (is (not (last-log-contains? state "Sure Gamble, Sure Gamble"))
          "Underway Renovation trashed card name is in log")
      (is (= 1 (count (:discard (get-challenger)))) "1 card milled from Challenger Stack")
      (play-from-hand state :contestant "Shipment from SanSan")
      (prompt-choice :contestant "2")
      (prompt-select :contestant ur)
      (is (= 3 (:advance-counter (refresh ur))))
      (is (= 1 (count (:discard (get-challenger)))) "No Challenger mills; advancements were placed")
      (core/advance state :contestant {:card (refresh ur)})
      (is (= 4 (:advance-counter (refresh ur))))
      (is (last-log-contains? state "Sure Gamble, Sure Gamble")
          "Underway Renovation trashed card name is in log")
      (is (= 3 (count (:discard (get-challenger)))) "2 cards milled from Challenger Stack; 4+ advancements"))))

(deftest vulcan-coverup
  ;; Vulcan Coverup - Do 2 meat damage when scored; take 1 bad pub when stolen
  (do-game
    (new-game (default-contestant [(qty "Vulcan Coverup" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Vulcan Coverup" "New remote")
    (take-credits state :contestant)
    (run-empty-server state :remote1)
    (prompt-choice :challenger "Steal")
    (is (= 1 (:bad-publicity (get-contestant))) "Took 1 bad pub from stolen agenda")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Vulcan Coverup" "New remote")
    (let [vc (get-content state :remote2 0)]
      (score-agenda state :contestant vc)
      (is (= 2 (count (:discard (get-challenger)))) "Did 2 meat damage upon scoring"))))
