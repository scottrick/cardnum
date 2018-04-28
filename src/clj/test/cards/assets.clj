(ns test.cards.assets
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adonis-campaign
  (do-game
    (new-game (default-corp [(qty "Adonis Campaign" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Adonis Campaign" "New remote")
    (let [ac (get-content state :remote1 0)]
      (core/rez state :resPlayer ac)
      (is (= 1 (get-in @state [:resPlayer :credit])))
      (is (= 12 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
      (take-credits state :resPlayer 2)
      (take-credits state :hazPlayer)
      (is (= 6 (get-in @state [:resPlayer :credit])) "Gain 3 from Adonis")
      (is (= 9 (get-counters (refresh ac) :credit))) "9 counter remaining on Adonis")))

(deftest aggressive-secretary
  (do-game
    (new-game
      (default-corp [(qty "Aggressive Secretary" 1)])
      (default-runner [(qty "Cache" 3)]))
    (play-from-hand state :resPlayer "Aggressive Secretary" "New remote")
    (let [as (get-content state :remote1 0)]
      ;; Single advance AggSec
      (core/advance state :resPlayer {:card (refresh as)})
      (take-credits state :resPlayer)
      ;; Run on AggSec with 3 programs
      (play-from-hand state :hazPlayer "Cache")
      (play-from-hand state :hazPlayer "Cache")
      (play-from-hand state :hazPlayer "Cache")
      (run-empty-server state "Server 1")
      (prompt-choice :resPlayer "Yes")
      (is (= 3 (get-in @state [:resPlayer :credit])))
      ;; Corp can trash one program
      (prompt-select :resPlayer (get-in @state [:hazPlayer :rig :program 1]))
      ;; There should be two Caches left
      (is (= 3 (get-in @state [:resPlayer :credit])))
      (is (= 2 (count (get-in @state [:hazPlayer :rig :program])))))))

(deftest alexa-belsky
  (do-game
    (new-game
      (default-corp [(qty "Alexa Belsky" 1) (qty "Hedge Fund" 1) (qty "Breaking News" 1)
                     (qty "Gutenberg" 1) (qty "Product Placement" 1) (qty "Jackson Howard" 1)])
      (default-runner))
    (play-from-hand state :resPlayer "Alexa Belsky" "New remote")
    (let [alexa (get-content state :remote1 0)]
      (core/rez state :resPlayer alexa)
      (card-ability state :resPlayer alexa 0)
      (is (= 1 (count (:discard (get-corp)))) "Alexa Belsky trashed")
      (is (= 5 (count (:hand (get-corp)))))
      (is (= 0 (count (:deck (get-corp)))))
      (prompt-choice :hazPlayer 5) ;Runner chooses to pay 5 credits so 2 cards are prevented from being shuffled
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 3 (count (:deck (get-corp)))))
      (is (= 0 (:credit (get-runner)))))))

(deftest alix-t4lb07
  (do-game
    (new-game
      (default-corp [(qty "Alix T4LB07" 1) (qty "PAD Campaign" 3)])
      (default-runner))
    (play-from-hand state :resPlayer "Alix T4LB07" "New remote")
    (let [alix (get-content state :remote1 0)]
      (core/rez state :resPlayer alix)
      (play-from-hand state :resPlayer "PAD Campaign" "New remote")
      (play-from-hand state :resPlayer "PAD Campaign" "New remote")
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (is (= 2 (get-counters (refresh alix) :power)) "Two counters on Alix")
      (is (= 4 (get-in @state [:resPlayer :credit])))
      (card-ability state :resPlayer alix 0)
      (is (= 8 (get-in @state [:resPlayer :credit]))))) "Gain 4 credits from Alix")

(deftest blacklist-steal
  ;; Blacklist - #2426.  Need to allow steal.
  (do-game
    (new-game (default-corp [(qty "Fetal AI" 3) (qty "Blacklist" 1)])
              (default-runner))
    (trash-from-hand state :resPlayer "Fetal AI")
    (play-from-hand state :resPlayer "Blacklist" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (= 1 (count (get-in @state [:resPlayer :discard])))
    (take-credits state :resPlayer)
    (run-empty-server state :archives)
    (prompt-choice :hazPlayer "Yes")
    (is (= 2 (:agenda-point (get-runner))) "Runner has 2 agenda points")
    (= 1 (count (get-in @state [:hazPlayer :scored])))))

(deftest bio-ethics-multiple
  ;; Bio-Ethics Association: preventing damage from multiple copies
  (do-game
    (new-game
      (default-corp [(qty "Bio-Ethics Association" 2)])
      (default-runner [(qty "Feedback Filter" 1) (qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Bio-Ethics Association" "New remote")
    (play-from-hand state :resPlayer "Bio-Ethics Association" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (core/rez state :resPlayer (get-content state :remote2 0))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Feedback Filter")
    (take-credits state :hazPlayer)
    (let [filter (get-hardware state 0)]
      (is (= 1 (count (:prompt (get-runner)))) "Runner has a single damage prevention prompt")
      (card-ability state :hazPlayer filter 0)
      (prompt-choice :hazPlayer "Done")
      (is (= 0 (count (:discard (get-runner)))) "Runner prevented damage")
      (is (= 1 (count (:prompt (get-runner)))) "Runner has a next damage prevention prompt")
      (prompt-choice :hazPlayer "Done")
      (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage"))))

(deftest brain-taping-warehouse
  ;; Brain-Taping Warehouse - Lower rez cost of Bioroid ICE by 1 for each unspent Runner click
  (do-game
    (new-game (default-corp [(qty "Brain-Taping Warehouse" 1) (qty "Ichi 1.0" 1)
                             (qty "Eli 1.0" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Brain-Taping Warehouse" "New remote")
    (play-from-hand state :resPlayer "Ichi 1.0" "Server 1")
    (play-from-hand state :resPlayer "Eli 1.0" "HQ")
    (let [ichi (get-ice state :remote1 0)
          eli (get-ice state :hq 0)]
      (take-credits state :resPlayer)
      (run-on state :remote1)
      (core/rez state :resPlayer (get-content state :remote1 0))
      (is (= 3 (:click (get-runner))))
      (core/rez state :resPlayer ichi)
      (is (= 2 (:credit (get-corp))) "Paid only 2c to rez Ichi; reduction of 3c")
      (run-jack-out state)
      (run-on state :hq)
      (is (= 2 (:click (get-runner))))
      (core/rez state :resPlayer eli)
      (is (= 1 (:credit (get-corp))) "Paid only 1c to rez Eli; reduction of 2c"))))

(deftest capital-investors
  ;; Capital Investors - Click for 2 credits
  (do-game
    (new-game (default-corp [(qty "Capital Investors" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Capital Investors" "New remote")
    (let [cap (get-content state :remote1 0)]
      (core/rez state :resPlayer cap)
      (card-ability state :resPlayer cap 0)
      (card-ability state :resPlayer cap 0)
      (is (= 0 (:click (get-corp))) "Used twice, spent 2 clicks")
      (is (= 7 (:credit (get-corp))) "Used twice, gained 4 credits"))))

(deftest chairman-hiro
  ;; Chairman Hiro - Reduce Runner max hand size; add as 2 agenda points if Runner trashes him
  (do-game
    (new-game (default-corp [(qty "Chairman Hiro" 2)])
              (default-runner))
    (play-from-hand state :resPlayer "Chairman Hiro" "New remote")
    (play-from-hand state :resPlayer "Chairman Hiro" "Server 1")
    (prompt-choice :resPlayer "OK")
    (is (= 1 (count (:discard (get-corp)))) "First Hiro trashed")
    (is (= 0 (:agenda-point (get-runner))) "No points for Runner if trashed by Corp")
    (let [hiro (get-content state :remote1 0)]
      (core/rez state :resPlayer hiro)
      (is (= 3 (core/hand-size state :hazPlayer)) "Runner max hand size reduced by 2")
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer 3)
      (run-empty-server state "Server 1")
      (prompt-choice :hazPlayer "Yes") ; trash Hiro
      (is (= 2 (:credit (get-runner))) "Runner paid 6 credits to trash")
      (is (= 5 (core/hand-size state :hazPlayer)) "Runner max hand size restored to 5")
      (is (= 1 (count (get-in @state [:hazPlayer :scored])))
          "Chairman Hiro added to Runner score area")
      (is (= 2 (:agenda-point (get-runner))) "Runner gained 2 agenda points"))))

(deftest city-surveillance
  ;; City Surveillance - Runner chooses to pay 1 credit or take 1 tag at start of their turn
  (do-game
    (new-game (default-corp [(qty "City Surveillance" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "City Surveillance" "New remote")
    (let [surv (get-content state :remote1 0)]
      (core/rez state :resPlayer surv)
      (take-credits state :resPlayer)
      (prompt-choice :hazPlayer "Pay 1 [Credits]")
      (is (= 4 (:credit (get-runner))) "Runner paid 1 credit")
      (is (= 0 (:tag (get-runner))) "Runner didn't take a tag")
      (is (empty? (:prompt (get-runner))) "City Surveillance only fired once")
      (take-credits state :hazPlayer)
      (take-credits state :resPlayer)
      (prompt-choice :hazPlayer "Take 1 tag")
      (is (= 8 (:credit (get-runner))) "Runner paid no credits")
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag"))
      (is (empty? (:prompt (get-runner))) "City Surveillance only fired once")))

(deftest clyde-van-rite
  ;; Clyde Van Rite - Multiple scenarios involving Runner not having credits/cards to trash
  (do-game
    (new-game (default-corp [(qty "Clyde Van Rite" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Restructure" 2) (qty "John Masanori" 2)]))
    (play-from-hand state :resPlayer "Clyde Van Rite" "New remote")
    (let [clyde (get-content state :remote1 0)]
      (core/rez state :resPlayer clyde)
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (is (:resPlayer-phase-12 @state) "Corp in Step 1.2")
      ;; Runner chooses to pay - has 1+ credit so pays 1 credit
      (card-ability state :resPlayer clyde 0)
      (is (= 9 (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (prompt-choice :hazPlayer "Pay 1 [Credits]")
      (is (= 8 (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (core/end-phase-12 state :resPlayer nil)
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      ;; Runner chooses to pay - can't pay 1 credit so trash top card
      (core/lose state :hazPlayer :credit 12)
      (card-ability state :resPlayer clyde 0)
      (is (= 0 (:credit (get-runner))))
      (is (= 2 (count (:deck (get-runner)))))
      (prompt-choice :hazPlayer "Pay 1 [Credits]")
      (is (= 0 (:credit (get-runner))))
      (is (= 1 (count (:deck (get-runner)))))
      (core/end-phase-12 state :resPlayer nil)
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      ;; Runner chooses to trash - has 1+ card in Stack so trash 1 card
      (card-ability state :resPlayer clyde 0)
      (is (= 4 (:credit (get-runner))))
      (is (= 1 (count (:deck (get-runner)))))
      (prompt-choice :hazPlayer "Trash top card")
      (is (= 4 (:credit (get-runner))))
      (is (= 0 (count (:deck (get-runner)))))
      (core/end-phase-12 state :resPlayer nil)
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      ;; Runner chooses to trash - no cards in Stack so pays 1 credit
      (card-ability state :resPlayer clyde 0)
      (is (= 8 (:credit (get-runner))))
      (is (= 0 (count (:deck (get-runner)))))
      (prompt-choice :hazPlayer "Trash top card")
      (is (= 7 (:credit (get-runner))))
      (is (= 0 (count (:deck (get-runner))))))))

(deftest daily-business-show
  ;; Daily Business Show - Full test
  (do-game
    (new-game (default-corp [(qty "Daily Business Show" 3) (qty "Hedge Fund" 1) (qty "Jackson Howard" 1)
                             (qty "Resistor" 1) (qty "Product Placement" 1) (qty "Breaking News" 1)])
              (default-runner))
    (starting-hand state :resPlayer ["Daily Business Show" "Daily Business Show" "Daily Business Show" "Hedge Fund"])
    (core/gain state :resPlayer :credit 1)
    (play-from-hand state :resPlayer "Daily Business Show" "New remote")
    (play-from-hand state :resPlayer "Daily Business Show" "New remote")
    (play-from-hand state :resPlayer "Daily Business Show" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (core/rez state :resPlayer (get-content state :remote2 0))
    (core/rez state :resPlayer (get-content state :remote3 0))
    (take-credits state :resPlayer)
    (is (= 1 (count (:hand (get-corp)))))
    (take-credits state :hazPlayer)
    (is (= 5 (count (:hand (get-corp)))) "Drew an additional 3 cards with 3 DBS")
    (is (not-empty (:prompt (get-runner))) "Runner is waiting for Corp to use DBS")
    (prompt-select :resPlayer (find-card "Hedge Fund" (:hand (get-corp)))) ;invalid target
    (prompt-select :resPlayer (find-card "Resistor" (:hand (get-corp))))
    (prompt-select :resPlayer (find-card "Product Placement" (:hand (get-corp))))
    (prompt-select :resPlayer (find-card "Breaking News" (:hand (get-corp))))
    (is (= 2 (count (:hand (get-corp)))))
    (is (= "Hedge Fund" (:title (first (:hand (get-corp))))))
    (is (= "Jackson Howard" (:title (second (:hand (get-corp))))))
    (is (= "Resistor" (:title (last (:deck (get-corp))))) "Resistor last card in deck")
    (is (= "Product Placement" (:title (last (butlast (:deck (get-corp))))))
        "Product Placement second last card in deck")
    (is (= "Breaking News" (:title (last (butlast (butlast (:deck (get-corp)))))))
        "Breaking News third last card in deck")))

(deftest daily-business-show-sensie-actors-union
  ;; Daily Business Show - Sensie Actors Union interaction
  (do-game
    (new-game (default-corp [(qty "Daily Business Show" 1) (qty "Sensie Actors Union" 2)
                             (qty "Hedge Fund" 1) (qty "Jackson Howard" 1)
                             (qty "Resistor" 1) (qty "Product Placement" 1) (qty "Breaking News" 1)])
              (default-runner))
    (starting-hand state :resPlayer ["Daily Business Show" "Sensie Actors Union" "Sensie Actors Union" "Hedge Fund"])
    (play-from-hand state :resPlayer "Daily Business Show" "New remote")
    (play-from-hand state :resPlayer "Sensie Actors Union" "New remote")
    (play-from-hand state :resPlayer "Sensie Actors Union" "New remote")
    (let [sensie1 (get-content state :remote2 0)
          sensie2 (get-content state :remote3 0)]
      (core/rez state :resPlayer (get-content state :remote1 0))
      (core/rez state :resPlayer sensie1)
      (core/rez state :resPlayer sensie2)
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      ;; Use first Sensie
      (is (= 1 (count (:hand (get-corp)))))
      (card-ability state :resPlayer sensie1 0)
      (is (= 5 (count (:hand (get-corp)))) "Drew 3 cards with Sensie, +1 with DBS")
      (prompt-select :resPlayer (find-card "Resistor" (:hand (get-corp)))) ; DBS target
      (prompt-select :resPlayer (find-card "Hedge Fund" (:hand (get-corp)))) ; Sensie target
      (is (= 3 (count (:hand (get-corp)))))
      (is (= "Hedge Fund" (:title (last (:deck (get-corp))))) "Hedge Fund last card in deck")
      (is (= "Resistor" (:title (last (butlast (:deck (get-corp))))))
          "Resistor second last card in deck")
      ;; Try to use first Sensie again
      (card-ability state :resPlayer sensie1 0)
      (is (empty? (get-in @state [:resPlayer :prompt])) "Sensie didn't activate")
      (is (= 3 (count (:hand (get-corp)))))
      ;; Use second Sensie
      (starting-hand state :resPlayer ["Hedge Fund" "Jackson Howard"])
      (is (= 2 (count (:hand (get-corp)))))
      (card-ability state :resPlayer sensie2 0)
      (is (= 5 (count (:hand (get-corp)))) "Drew 3 cards with Sensie, DBS didn't activate")
      (prompt-select :resPlayer (find-card "Breaking News" (:hand (get-corp)))) ; Sensie target
      (is (= "Breaking News" (:title (last (:deck (get-corp))))) "Breaking News last card in deck"))))

(deftest daily-business-show-manual-draw
  ;; Daily Business Show - Should not trigger if rezzed after mandatory draw
  (do-game
    (new-game (default-corp [(qty "Daily Business Show" 3) (qty "Hedge Fund" 1) (qty "Jackson Howard" 1)
                             (qty "Resistor" 1) (qty "Product Placement" 1) (qty "Breaking News" 1)])
              (default-runner))
    (starting-hand state :resPlayer ["Daily Business Show"])
    (play-from-hand state :resPlayer "Daily Business Show" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (core/draw state :resPlayer)
    (is (= 1 (count (:hand (get-corp)))) "DBS did not fire on manual draw")
    (is (empty? (:prompt (get-corp))) "Corp is not being asked to bury a card with DBS")    ))

(deftest dedicated-response-team
  ;; Dedicated Response Team - Do 2 meat damage when successful run ends if Runner is tagged
  (do-game
    (new-game (default-corp [(qty "Dedicated Response Team" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Dedicated Response Team" "New remote")
    (let [drt (get-content state :remote1 0)]
      (core/rez state :resPlayer drt)
      (take-credits state :resPlayer)
      (run-empty-server state :rd)
      (is (empty? (:discard (get-runner))) "Not tagged, no damage done")
      (core/gain state :hazPlayer :tag 1)
      (run-on state :rd)
      (run-jack-out state)
      (is (empty? (:discard (get-runner))) "Tagged but run unsuccessful, no damage done")
      (run-empty-server state :rd)
      (is (= 2 (count (:discard (get-runner)))) "Suffered 2 damage for successful run w/ tag"))))

(deftest early-premiere
  ;; Early Premiere - Pay 1c at start of turn to place an advancement on a card in a server
  (do-game
    (new-game (default-corp [(qty "Early Premiere" 1) (qty "Ice Wall" 1)
                             (qty "Ghost Branch" 1) (qty "Blacklist" 1)])
              (default-runner))
    (core/gain state :resPlayer :click 1)
    (play-from-hand state :resPlayer "Early Premiere" "New remote")
    (play-from-hand state :resPlayer "Blacklist" "New remote")
    (play-from-hand state :resPlayer "Ghost Branch" "New remote")
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (let [ep (get-content state :remote1 0)
          bl (get-content state :remote2 0)
          gb (get-content state :remote3 0)
          iw (get-ice state :hq 0)]
      (core/rez state :resPlayer ep)
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (card-ability state :resPlayer ep 0)
      (prompt-select :resPlayer iw)
      (is (nil? (:advance-counter (refresh iw))) "Ice Wall can't targeted, not in server")
      (prompt-select :resPlayer bl)
      (is (nil? (:advance-counter (refresh bl))) "Blacklist can't targeted, can't be advanced")
      (prompt-select :resPlayer gb)
      (is (= 1 (:advance-counter (refresh gb))) "1 advancement on Ghost Branch")
      (is (= 4 (:credit (get-corp)))))))

(deftest echochamber
  ;; Echo Chamber - 3 clicks to become 1 point agenda
  (do-game
    (new-game (default-corp [(qty "Echo Chamber" 1)])
              (default-runner))
    (core/gain state :resPlayer :click 1)
    (play-from-hand state :resPlayer "Echo Chamber" "New remote")
    (let [ec (get-content state :remote1 0)]
      (core/rez state :resPlayer ec)
      (card-ability state :resPlayer ec 0))
    (is (= 1 (:agendapoints (get-in @state [:resPlayer :scored 0]))) "Echo Chamber added to Corp score area")))

(deftest edge-of-world
  ;; Edge of World - ability
  (do-game
    (new-game (default-corp [(qty "Edge of World" 3) (qty "Ice Wall" 3)])
              (default-runner))
    (core/gain state :resPlayer :credit 6 :click 1)
    (play-from-hand state :resPlayer "Edge of World" "New remote")
    (play-from-hand state :resPlayer "Edge of World" "New remote")
    (play-from-hand state :resPlayer "Ice Wall" "Server 1")
    (play-from-hand state :resPlayer "Ice Wall" "Server 1")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
        "Runner waiting for Corp to act")
    (prompt-choice :resPlayer "Yes")
    (prompt-choice :hazPlayer "Yes")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
    (run-empty-server state "Server 2")
    (prompt-choice :resPlayer "Yes")
    (prompt-choice :hazPlayer "Yes")
    (is (= 2 (:brain-damage (get-runner))) "Runner did not take brain damage when no ICE protected Edge of World")))

(deftest elizabeth-mills
  ;; Elizabeth Mills - Remove 1 bad publicity when rezzed; click-trash to trash a location
  (do-game
    (new-game (default-corp [(qty "Elizabeth Mills" 1)])
              (default-runner [(qty "Earthrise Hotel" 1)]))
    (core/gain state :resPlayer :bad-publicity 1)
    (play-from-hand state :resPlayer "Elizabeth Mills" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Earthrise Hotel")
    (take-credits state :hazPlayer)
    (let [liz (get-content state :remote1 0)
          hotel (get-in @state [:hazPlayer :rig :resource 0])]
      (core/rez state :resPlayer liz)
      (is (= 0 (:bad-publicity (get-corp))) "1 bad publicity removed")
      (card-ability state :resPlayer liz 0)
      (prompt-select :resPlayer hotel)
      (is (= 1 (count (:discard (get-runner)))) "Earthrise trashed")
      (is (= 1 (count (:discard (get-corp)))) "Elizabeth Mills trashed")
      (is (= 1 (:bad-publicity (get-corp))) "1 bad publicity taken from trashing a location"))))

(deftest elizas-toybox
  ;; Eliza's Toybox - Rez a card ignoring all costs
  (do-game
    (new-game (default-corp [(qty "Eliza's Toybox" 1) (qty "Wotan" 1)])
              (default-runner))
    (core/gain state :resPlayer :click 2)
    (play-from-hand state :resPlayer "Wotan" "R&D")
    (play-from-hand state :resPlayer "Eliza's Toybox" "New remote")
    (let [wotan (get-ice state :rd 0)
          eliza (get-content state :remote1 0)]
      (core/rez state :resPlayer eliza)
      (is (= 1 (:credit (get-corp))))
      (card-ability state :resPlayer eliza 0)
      (prompt-select :resPlayer wotan)
      (is (get-in (refresh wotan) [:rezzed]))
      (is (= 0 (:click (get-corp))) "3 clicks spent")
      (is (= 1 (:credit (get-corp))) "No credits spent"))))

(deftest encryption-protocol
  ;; Encryption Protocol - Trash cost of installed cards increased by 1
  (do-game
    (new-game (default-corp [(qty "Encryption Protocol" 2)])
              (default-runner))
    (play-from-hand state :resPlayer "Encryption Protocol" "New remote")
    (play-from-hand state :resPlayer "Encryption Protocol" "New remote")
    (let [ep1 (get-content state :remote1 0)
          ep2 (get-content state :remote2 0)]
      (core/rez state :resPlayer ep1)
      (core/rez state :resPlayer ep2)
      (take-credits state :resPlayer)
      (run-empty-server state "Server 1")
      (is (= 4 (core/trash-cost state :hazPlayer (refresh ep1)))
          "Trash cost increased to 4 by two active Encryption Protocols")
      (prompt-choice :hazPlayer "Yes") ; trash first EP
      (run-empty-server state "Server 2")
      (is (= 3 (core/trash-cost state :hazPlayer (refresh ep2)))
          "Trash cost increased to 3 by one active Encryption Protocol"))))

(deftest eve-campaign
  (do-game
    (new-game (default-corp [(qty "Eve Campaign" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Eve Campaign" "New remote")
    (let [eve (get-content state :remote1 0)]
      (core/rez state :resPlayer eve)
      (is (= 0 (get-in @state [:resPlayer :credit])))
      (is (= 16 (get-counters (refresh eve) :credit)))
      (take-credits state :resPlayer 2)
      (take-credits state :hazPlayer)
      (is (= 4 (get-in @state [:resPlayer :credit])))
      (is (= 14 (get-counters (refresh eve) :credit))))))

(deftest executive-boot-camp-suppress-start-of-turn
  ;; Executive Boot Camp - suppress the start-of-turn event on a rezzed card. Issue #1346.
  (do-game
    (new-game (default-corp [(qty "Eve Campaign" 1) (qty "Executive Boot Camp" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Eve Campaign" "New remote")
    (play-from-hand state :resPlayer "Executive Boot Camp" "New remote")
    (take-credits state :resPlayer)
    (is (= 6 (:credit (get-corp))) "Corp ends turn with 6 credits")
    (let [eve (get-content state :remote1 0)
          ebc (get-content state :remote2 0)]
      (core/rez state :resPlayer ebc)
      (take-credits state :hazPlayer)
      (is (:resPlayer-phase-12 @state) "Corp in Step 1.2")
      (card-ability state :resPlayer ebc 0)
      (prompt-select :resPlayer eve)
      (is (= 2 (:credit (get-corp))) "EBC saved 1 credit on the rez of Eve")
      (is (= 16 (get-counters (refresh eve) :credit)))
      (core/end-phase-12 state :resPlayer nil)
      (is (= 2 (:credit (get-corp))) "Corp did not gain credits from Eve")
      (is (= 16 (get-counters (refresh eve) :credit)) "Did not take counters from Eve")
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (is (not (:resPlayer-phase-12 @state)) "With nothing to rez, EBC does not trigger Step 1.2")
      (is (= 14 (get-counters (refresh eve) :credit)) "Took counters from Eve"))))

(deftest franchise-city
  (do-game
    (new-game (default-corp [(qty "Franchise City" 1) (qty "Accelerated Beta Test" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Franchise City" "New remote")
    (play-from-hand state :resPlayer "Accelerated Beta Test" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (take-credits state :resPlayer 1)
    (run-empty-server state "Server 2")
    (prompt-choice :hazPlayer "Steal")
    (is (= 0 (count (get-in @state [:resPlayer :servers :server2 :content]))) "Agenda was stolen")
    (is (= 2 (:agenda-point (get-runner))) "Runner stole 2 points")
    (is (= 0 (count (get-in @state [:resPlayer :servers :server1 :content])))
        "Franchise City no longer installed")
    (is (find-card "Franchise City" (:scored (get-corp))) "Franchise City in corp scored area")
    (is (= 1 (:agenda-point (get-corp))) "Corp has 1 point")))

(deftest full-immersion-recstudio
  ;; Full Immmersion RecStudio - install directly, and via Interns
  (do-game
    (new-game
      (default-corp [(qty "Full Immersion RecStudio" 1)
                     (qty "Interns" 2)
                     (qty "Launch Campaign" 3)])
      (default-runner))
    (play-from-hand state :resPlayer "Full Immersion RecStudio" "New remote")
    (let [fir (get-content state :remote1 0)]
      (core/rez state :resPlayer fir)
      (card-ability state :resPlayer fir 0)
      (prompt-select :resPlayer (find-card "Launch Campaign" (:hand (get-corp))))
      (let [lc (first (:hosted (refresh fir)))]
        (is lc "Launch Campaign hosted on Full Immersion RecStudio")
        (core/rez state :resPlayer lc)
        (is (and (:installed (refresh lc)) (:rezzed (refresh lc))) "Rezzed Launch Campaign")
        (take-credits state :resPlayer)
        (take-credits state :hazPlayer)
        (is (= 5 (:credit (get-corp))) "Gained 2cr from Launch Campaign")
        (is (= 4 (get-counters (refresh lc) :credit)) "4cr left on Launch Campaign")
        (play-from-hand state :resPlayer "Interns")
        (prompt-select :resPlayer (find-card "Launch Campaign" (:hand (get-corp))))
        (prompt-choice :resPlayer (refresh fir))
        (is (= 2 (count (:hosted (refresh fir)))) "Interns installed onto FIR")))))

(deftest full-immersion-recstudio-sandburg
  ;; Full Immmersion RecStudio - hosting an asset with events does not double-register events. Issue #1827.
  (do-game
    (new-game
      (default-corp [(qty "Full Immersion RecStudio" 1) (qty "Sandburg" 1) (qty "Vanilla" 1)
                     (qty "Oaktown Renovation" 1)])
      (default-runner))
    (play-from-hand state :resPlayer "Full Immersion RecStudio" "New remote")
    (play-from-hand state :resPlayer "Vanilla" "HQ")
    (let [fir (get-content state :remote1 0)
          van (get-ice state :hq 0)]
      (core/rez state :resPlayer fir)
      (core/rez state :resPlayer van)
      (card-ability state :resPlayer fir 0)
      (prompt-select :resPlayer (find-card "Sandburg" (:hand (get-corp))))
      (core/gain state :resPlayer :credit 7 :click 3)
      (core/rez state :resPlayer (first (:hosted (refresh fir))))
      (is (= 2 (:current-strength (refresh van))) "Vanilla at 2 strength")
      (card-ability state :resPlayer fir 0)
      (prompt-select :resPlayer (find-card "Oaktown Renovation" (:hand (get-corp))))
      (core/advance state :resPlayer {:card (last (:hosted (refresh fir)))})
      (is (= 11 (:credit (get-corp))) "Gained 1cr from advancing Oaktown"))))

(deftest gene-splicer-access-unadvanced-no-trash
  ;; Runner accesses an unadvanced Gene Splicer and doesn't trash
  ;; No net damage is dealt and Gene Splicer remains installed
  (do-game
    (new-game
      (default-corp [(qty "Gene Splicer" 1)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Gene Splicer" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "No")
    (is (= 0 (count (:discard (get-runner)))) "Runner took no net damage")
    (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
    (is (= 5 (:credit (get-runner))) "Runner spent no credits")))

(deftest gene-splicer-access-unadvanced-trash
  ;; Runner accesses an unadvanced Gene Splicer and trashes it - no net damage is dealt and Gene Splicer is trashed
  (do-game
    (new-game
      (default-corp [(qty "Gene Splicer" 1)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Gene Splicer" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Yes")
    (is (= 0 (count (:discard (get-runner)))) "Runner took no net damage")
    (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
    (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
    (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))

(deftest gene-splicer-access-single-advanced-no-trash
  ;; Runner accesses a single-advanced Gene Splicer and doesn't trash
  ;; 1 net damage is dealt and Gene Splicer remains installed
  (do-game
    (new-game
      (default-corp [(qty "Gene Splicer" 1)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Gene Splicer" "New remote")
    (core/add-counter state :resPlayer (get-content state :remote1 0) :advancement 1)
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "No")
    (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
    (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
    (is (= 5 (:credit (get-runner))) "Runner spent no credits")))

(deftest gene-splicer-access-single-advanced-trash
  ;; Runner accesses a single-advanced Gene Splicer and trashes it
  ;; 1 net damage is dealt and Gene Splicer is trashed
  (do-game
    (new-game
      (default-corp [(qty "Gene Splicer" 1)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Gene Splicer" "New remote")
    (core/add-counter state :resPlayer (get-content state :remote1 0) :advancement 1)
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Yes")
    (is (= 1 (count (:discard (get-runner)))) "Runner took 1 net damage")
    (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
    (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
    (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))

(deftest gene-splicer-access-double-advanced-no-trash
  ;; Runner accesses a double-advanced Gene Splicer and doesn't trash
  ;; 2 net damage is dealt and Gene Splicer remains installed
  (do-game
    (new-game
      (default-corp [(qty "Gene Splicer" 1)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Gene Splicer" "New remote")
    (core/add-counter state :resPlayer (get-content state :remote1 0) :advancement 2)
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "No")
    (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
    (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
    (is (= 5 (:credit (get-runner))) "Runner spent no credits")))

(deftest gene-splicer-access-double-advanced-trash
  ;; Runner accesses a double-advanced Gene Splicer and trashes it
  ;; 2 net damage is dealt and Gene Splicer is trashed
  (do-game
    (new-game
      (default-corp [(qty "Gene Splicer" 1)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Gene Splicer" "New remote")
    (core/add-counter state :resPlayer (get-content state :remote1 0) :advancement 2)
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Yes")
    (is (= 2 (count (:discard (get-runner)))) "Runner took 2 net damage")
    (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
    (is (= (:title (last (:discard (get-corp)))) "Gene Splicer") "Gene Splicer trashed")
    (is (= 4 (:credit (get-runner))) "Runner spent 1 credit to trash Gene Splicer")))

(deftest gene-splicer-agenda-ability
  ;; Corp triple-advances a Gene Splicer and uses its ability to add to their score area as a 1 point agenda
  (do-game
    (new-game
      (default-corp [(qty "Gene Splicer" 2) (qty "Ice Wall" 3) (qty "Vanilla" 2)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Gene Splicer" "New remote")
    (let [gs (get-content state :remote1 0)]
      (core/add-counter state :resPlayer gs :advancement 2)
      (take-credits state :hazPlayer)
      (core/add-counter state :resPlayer (refresh gs) :advancement 1)
      (core/rez state :resPlayer (refresh gs))
      (card-ability state :resPlayer (refresh gs) 0)
      (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= 1 (:agendapoints (get-in @state [:resPlayer :scored 0]))) "Gene Splicer added to Corp score area"))))

(deftest genetics-pavilion
  ;; Genetics Pavilion - Limit Runner to 2 draws per turn, but only during Runner's turn
  (do-game
    (new-game (default-corp [(qty "Genetics Pavilion" 1)])
              (default-runner [(qty "Diesel" 1) (qty "Sure Gamble" 3) (qty "Sports Hopper" 1)]))
    (play-from-hand state :resPlayer "Genetics Pavilion" "New remote")
    (let [gp (get-content state :remote1 0)]
      (take-credits state :resPlayer)
      (core/rez state :resPlayer gp)
      (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (play-from-hand state :hazPlayer "Sports Hopper")
      (play-from-hand state :hazPlayer "Diesel")
      (is (= 2 (count (:hand (get-runner)))) "Drew only 2 cards because of Genetics Pavilion")
      (take-credits state :hazPlayer)
      (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (let [hopper (get-in @state [:hazPlayer :rig :hardware 0])]
        (card-ability state :hazPlayer hopper 0)
        (is (= 3 (count (:hand (get-runner)))) "Able to draw 3 cards during Corp's turn")
        (core/derez state :resPlayer (refresh gp))
        (take-credits state :resPlayer)
        (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/move state :hazPlayer (find-card "Diesel" (:discard (get-runner))) :hand)
        (is (= 1 (count (:hand (get-runner)))))
        (play-from-hand state :hazPlayer "Diesel")
        (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards with Diesel")
        (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
        (core/rez state :resPlayer (refresh gp))
        (core/draw state :hazPlayer)
        (is (= 2 (count (:hand (get-runner)))) "No card drawn; GP counts cards drawn prior to rez")))))

(deftest genetics-pavilion-fisk-investment
  (do-game
    (new-game (default-corp [(qty "Genetics Pavilion" 1) (qty "Hedge Fund" 3)])
              (default-runner [(qty "Fisk Investment Seminar" 1) (qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Genetics Pavilion" "New remote")
    (let [gp (get-content state :remote1 0)]
      (take-credits state :resPlayer)
      (core/rez state :resPlayer gp)
      (core/move state :resPlayer (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :resPlayer (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :resPlayer (find-card "Hedge Fund" (:hand (get-corp))) :deck)
      (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (is (= 1 (count (:hand (get-runner)))))
      (is (= 0 (count (:hand (get-corp)))))
      (play-from-hand state :hazPlayer "Fisk Investment Seminar")
      (is (= 2 (count (:hand (get-runner)))) "Drew only 2 cards because of Genetics Pavilion")
      (is (= 3 (count (:hand (get-corp)))) "Drew all 3 cards"))))

(deftest genetics-pavilion-mr-li
  ;; Genetics Pavilion - Mr. Li interaction. #1594
  (do-game
    (new-game (default-corp [(qty "Genetics Pavilion" 1)])
              (default-runner [(qty "Mr. Li" 1) (qty "Account Siphon" 1) (qty "Faerie" 1)
                               (qty "Sure Gamble" 1) (qty "John Masanori" 1) (qty "Desperado" 1)]))
    (starting-hand state :hazPlayer ["Mr. Li"])
    (play-from-hand state :resPlayer "Genetics Pavilion" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Mr. Li")
    (let [mrli (get-in @state [:hazPlayer :rig :resource 0])]
      (is (= 0 (count (:hand (get-runner)))))
      ;use Mr. Li with 2 draws allowed
      (card-ability state :hazPlayer mrli 0)
      (is (= 2 (count (:hand (get-runner)))))
      (prompt-select :hazPlayer (first (:hand (get-runner))))
      (is (= 1 (count (:hand (get-runner)))))
      ;use Mr. Li with 0 draws allowed
      (card-ability state :hazPlayer mrli 0)
      (is (= 1 (count (:hand (get-runner)))))
      (prompt-select :hazPlayer (first (:hand (get-runner)))) ;will fail because not a valid target
      (prompt-choice :hazPlayer "Done") ;cancel out
      (take-credits state :hazPlayer)
      (take-credits state :resPlayer)
      (core/draw state :hazPlayer)
      (is (= 2 (count (:hand (get-runner)))))
      ;use Mr. Li with 1 draw allowed
      (card-ability state :hazPlayer mrli 0)
      (is (= 3 (count (:hand (get-runner)))))
      (prompt-select :hazPlayer (first (:hand (get-runner)))) ;will fail
      (prompt-select :hazPlayer (second (:hand (get-runner)))) ;will fail
      (prompt-select :hazPlayer (second (rest (:hand (get-runner)))))
      (is (= 2 (count (:hand (get-runner))))))))

(deftest ghost-branch
  ;; Ghost Branch - Advanceable; give the Runner tags equal to advancements when accessed
  (do-game
    (new-game (default-corp [(qty "Ghost Branch" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Ghost Branch" "New remote")
    (let [gb (get-content state :remote1 0)]
      (core/advance state :resPlayer {:card (refresh gb)})
      (core/advance state :resPlayer {:card (refresh gb)})
      (is (= 2 (get-in (refresh gb) [:advance-counter])))
      (take-credits state :resPlayer)
      (run-empty-server state "Server 1")
      (prompt-choice :resPlayer "Yes") ; choose to do the optional ability
      (is (= 2 (:tag (get-runner))) "Runner given 2 tags"))))

(deftest honeyfarm
  ;; lose one credit on access
  (do-game
    (new-game (default-corp [(qty "Honeyfarm" 3)])
              (default-runner))
    (trash-from-hand state :resPlayer "Honeyfarm")
    (play-from-hand state :resPlayer "Honeyfarm" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (is (= 4 (:credit (get-runner))))
    (run-empty-server state "Archives")
    (is (= 3 (:credit (get-runner))))
	(run-empty-server state "HQ")
    (is (= 2 (:credit (get-runner))))))

(deftest hostile-infrastructure
  ;; Hostile Infrastructure - do 1 net damage when runner trashes a corp card
  (do-game
    (new-game (default-corp [(qty "Hostile Infrastructure" 3)])
              (default-runner))
    (core/gain state :hazPlayer :credit 50)
    (play-from-hand state :resPlayer "Hostile Infrastructure" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (take-credits state :resPlayer)
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "Yes")
    (is (= 1 (count (:discard (get-runner)))) "Took 1 net damage")
    (run-empty-server state :remote1)
    (prompt-choice :hazPlayer "Yes")
    (is (= 2 (count (:discard (get-runner)))) "Took 1 net damage")))

(deftest hyoubu-research-facility
  (do-game
    (new-game (default-corp [(qty "Hyoubu Research Facility" 1) (qty "Snowflake" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Hyoubu Research Facility" "New remote")
    (play-from-hand state :resPlayer "Snowflake" "HQ")
    (let [hrf (get-content state :remote1 0)
          sf (get-ice state :hq 0)]
      (take-credits state :resPlayer)
      (run-on state "HQ")
      (core/rez state :resPlayer hrf)
      (core/rez state :resPlayer sf)
      (card-subroutine state :resPlayer sf 0)
      (prompt-choice :resPlayer "2 [Credits]")
      (prompt-choice :hazPlayer "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 2c from Hyoubu")
      (run-on state "HQ")
      (card-subroutine state :resPlayer sf 0)
      (prompt-choice :resPlayer "2 [Credits]")
      (prompt-choice :hazPlayer "0 [Credits]")
      (is (= 3 (:credit (get-corp))) "No credits gained from Hyoubu"))))

(deftest illegal-arms-factory
  ;; Illegal Arms Factory; draw a card, gain a credit, bad pub when trashed while rezzed
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 1)
	                         (qty "Beanstalk Royalties" 1)
	                         (qty "IPO" 1)
							 (qty "Illegal Arms Factory" 3)])
              (default-runner))
    (core/gain state :hazPlayer :credit 20)
	(core/move state :resPlayer (find-card "IPO" (:hand (get-corp))) :deck)
	(core/move state :resPlayer (find-card "Hedge Fund" (:hand (get-corp))) :deck)
	(core/move state :resPlayer (find-card "Beanstalk Royalties" (:hand (get-corp))) :deck)
    (play-from-hand state :resPlayer "Illegal Arms Factory" "New remote")
    (play-from-hand state :resPlayer "Illegal Arms Factory" "New remote")
    (let [iaf (get-content state :remote2 0)]
      (core/rez state :resPlayer iaf)
      (take-credits state :resPlayer)
	  (run-empty-server state :remote1)
      (prompt-choice :hazPlayer "Yes")
      (is (= 0 (:bad-publicity (get-corp))) "Took no bad pub on unrezzed trash")
      (take-credits state :hazPlayer)
	  (is (= 3 (count (:hand (get-corp)))) "Drew a card from IAF + mandatory")
      (is (= 4 (:credit (get-corp))) "Gained 1 credit from IAF")
      (take-credits state :resPlayer)
	  (run-empty-server state :remote2)
      (prompt-choice :hazPlayer "Yes")
      (is (= 1 (:bad-publicity (get-corp))) "Took a bad pub on rezzed trash"))))

(deftest it-department
  ;; IT Department - Add strength to rezzed ICE until end of turn
  (do-game
    (new-game (default-corp [(qty "IT Department" 1) (qty "Wall of Static" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "IT Department" "New remote")
    (play-from-hand state :resPlayer "Wall of Static" "Server 1")
    (let [itd (get-content state :remote1 0)
          wos (get-ice state :remote1 0)]
      (core/rez state :resPlayer itd)
      (core/rez state :resPlayer wos)
      (card-ability state :resPlayer itd 1)
      (is (= 0 (:click (get-corp))) "Spent 1 click")
      (is (= 1 (get-counters (refresh itd) :power)) "IT Dept has 1 counter")
      (core/add-counter state :resPlayer (refresh itd) :power 4)
      (is (= 5 (get-counters (refresh itd) :power)) "IT Dept has 5 counters")
      (card-ability state :resPlayer itd 0)
      (prompt-select :resPlayer wos)
      ;; refer to online guides for summary of how this ludicrous formula is calculated
      (is (= 8 (:current-strength (refresh wos))) "Gained 5 strength")
      (is (= 4 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :resPlayer itd 0)
      (prompt-select :resPlayer wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 3 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :resPlayer itd 0)
      (prompt-select :resPlayer wos)
      (is (= 12 (:current-strength (refresh wos))) "Gained total of 9 strength")
      (is (= 2 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (card-ability state :resPlayer itd 0)
      (prompt-select :resPlayer wos)
      (is (= 11 (:current-strength (refresh wos))) "Gained total of 8 strength")
      (is (= 1 (get-counters (refresh itd) :power)) "Spent 1 counter")
      (take-credits state :resPlayer)
      (is (= 3 (:current-strength (refresh wos))) "Back to default strength"))))

(deftest jackson-howard-draw
  ;; Jackson Howard - Draw 2 cards
  (do-game
    (new-game (default-corp [(qty "Jackson Howard" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Restructure" 2)])
              (default-runner))
    ;; guaranteed to be at least 1 jhow in hand after draw, and 2 cards in R&D
    (play-from-hand state :resPlayer "Jackson Howard" "New remote")
    (let [jhow (get-content state :remote1 0)]
      (core/rez state :resPlayer jhow)
      (is (= 5 (count (:hand (get-corp)))))
      (is (= 2 (:click (get-corp))))
      (card-ability state :resPlayer jhow 0)
      (is (= 7 (count (:hand (get-corp)))) "Drew 2 cards")
      (is (= 1 (:click (get-corp)))))))

(deftest jeeves-model-bioroids
  (do-game
    (new-game (default-corp [(qty "Jeeves Model Bioroids" 1) (qty "TGTBT" 1)
                             (qty "Melange Mining Corp." 2)])
              (default-runner [(qty "Ghost Runner" 3)]))
    (play-from-hand state :resPlayer "Jeeves Model Bioroids" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Ghost Runner")
    (play-from-hand state :hazPlayer "Ghost Runner")
    (play-from-hand state :hazPlayer "Ghost Runner")
    (take-credits state :hazPlayer)
    ; install 3 things
    (play-from-hand state :resPlayer "TGTBT" "New remote")
    (play-from-hand state :resPlayer "Melange Mining Corp." "New remote")
    (play-from-hand state :resPlayer "Melange Mining Corp." "New remote")
    (is (= 1 (:click (get-corp))))
    (take-credits state :resPlayer)
    (take-credits state :hazPlayer)
    ;;click for credits
    (take-credits state :resPlayer 3)
    (is (= 1 (:click (get-corp))))
    (take-credits state :resPlayer)
    (take-credits state :hazPlayer)
    ;;click to purge
    (core/do-purge state :resPlayer 3)
    (is (= 1 (:click (get-corp))))
    (take-credits state :resPlayer)
    (take-credits state :hazPlayer)
    ;;click to advance
    (core/advance state :resPlayer (get-content state :remote2 0))
    (core/advance state :resPlayer (get-content state :remote2 0))
    (core/advance state :resPlayer (get-content state :remote2 0))
    (is (= 1 (:click (get-corp))))
    (take-credits state :resPlayer)
    (take-credits state :hazPlayer)
    ;; use 3 clicks on card ability - Melange
    (core/rez state :resPlayer (get-content state :remote3 0))
    (card-ability state :resPlayer (get-content state :remote3 0) 0)
    (is (= 1 (:click (get-corp))))
    (take-credits state :resPlayer)
    (take-credits state :hazPlayer)
    ;; trash 3 resources
    (core/gain state :hazPlayer :tag 1)
    (core/trash-resource state :resPlayer nil)
    (prompt-select :resPlayer (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))))
    (core/trash-resource state :resPlayer nil)
    (prompt-select :resPlayer (get-resource state 0))
    (is (= 2 (count (:discard (get-runner)))))
    (core/trash-resource state :resPlayer nil)
    (prompt-select :resPlayer (get-resource state 0))
    (is (= 3 (count (:discard (get-runner)))))
    (is (= 1 (:click (get-corp))))))

(deftest kala-ghoda
  ; Kala Ghoda Real TV
  (do-game
    (new-game (default-corp [(qty "Kala Ghoda Real TV" 1)])
              (default-runner) [(qty "Sure Gamble" 3)])
    (starting-hand state :hazPlayer ["Sure Gamble"])
    (play-from-hand state :resPlayer "Kala Ghoda Real TV" "New remote")
    (let [tv (get-content state :remote1 0)]
      (core/rez state :resPlayer tv)
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (is (:resPlayer-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :resPlayer tv 0)
      (prompt-choice :resPlayer "Done")
      (card-ability state :resPlayer tv 1)
      (is (= 1 (count (:discard (get-corp)))))
      (is (= 1 (count (:discard (get-runner)))))
      (is (last-log-contains? state "Sure Gamble")
          "Kala Ghoda did log trashed card names"))))

(deftest launch-campaign
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (let [launch (get-content state :remote1 0)]
      (core/rez state :resPlayer launch)
      (is (= 4 (get-in @state [:resPlayer :credit])))
      (is (= 6 (get-counters (refresh launch) :credit)))
      (take-credits state :resPlayer 2)
      (take-credits state :hazPlayer)
      (is (= 8 (get-in @state [:resPlayer :credit])))
      (is (= 4 (get-counters (refresh launch) :credit))))))

(deftest mark-yale
  ;; Mark Yale - Spend agenda counters or trash himself to gain credits
  (do-game
    (new-game (default-corp [(qty "Firmware Updates" 1) (qty "Mark Yale" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Firmware Updates" "New remote")
    (play-from-hand state :resPlayer "Mark Yale" "New remote")
    (let [firm (get-content state :remote1 0)
          yale (get-content state :remote2 0)]
      (score-agenda state :resPlayer firm)
      (core/rez state :resPlayer yale)
      (let [firmscored (get-in @state [:resPlayer :scored 0])]
        (is (= 3 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :resPlayer yale 1)
        (prompt-select :resPlayer firmscored)
        (is (= 7 (:credit (get-corp))) "Gained 3 credits")
        (is (= 2 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :resPlayer yale 1)
        (prompt-select :resPlayer firmscored)
        (is (= 10 (:credit (get-corp))) "Gained 3 credits")
        (is (= 1 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :resPlayer yale 1)
        (prompt-select :resPlayer firmscored)
        (is (= 13 (:credit (get-corp))) "Gained 3 credits")
        (is (= 0 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :resPlayer yale 1)
        (prompt-select :resPlayer firmscored)
        (is (= 13 (:credit (get-corp))) "Gained 0 credits because agenda needs a counter")
        (is (= 0 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :resPlayer yale 0)
        (is (= 15 (:credit (get-corp))) "Gained 2 credits")
        (is (= 1 (count (:discard (get-corp)))) "Mark Yale trashed")))))

(deftest mca-austerity-policy
  (do-game
    (new-game
      (default-corp [(qty "MCA Austerity Policy" 1)])
      (default-runner))
    (play-from-hand state :resPlayer "MCA Austerity Policy" "New remote")
    (let [mca (get-content state :remote1 0)]
      (core/rez state :resPlayer mca)
      (card-ability state :resPlayer mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      ; once per turn only
      (card-ability state :resPlayer mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      (take-credits state :resPlayer)
      ; runner loses a click
      (is (= 3 (:click (get-runner))))
      (take-credits state :hazPlayer)
      (card-ability state :resPlayer mca 0)
      (is (= 2 (get-counters (refresh mca) :power)))
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (card-ability state :resPlayer mca 0)
      (is (= 3 (get-counters (refresh mca) :power)))
      ; Fire MCA
      (is (= 2 (:click (get-corp))))
      (card-ability state :resPlayer (refresh mca) 1)
      (is (= 5 (:click (get-corp)))))))

(deftest mental-health-clinic
  ;; Mental Health Clinic - Gain 1 credit when turn begins; Runner max hand size increased by 1
  (do-game
    (new-game (default-corp [(qty "Mental Health Clinic" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Mental Health Clinic" "New remote")
    (let [mhc (get-content state :remote1 0)]
      (core/rez state :resPlayer mhc)
      (is (= 6 (core/hand-size state :hazPlayer)) "Runner max hand size increased by 1")
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (is (= 8 (:credit (get-corp))) "Gained 1 credit at start of turn"))))

(deftest news-team
  ;; News Team - on access take 2 tags or take as agenda worth -1
  (do-game
    (new-game (default-corp [(qty "News Team" 3) (qty "Blacklist" 1)])
              (default-runner))
    (trash-from-hand state :resPlayer "News Team")
    (play-from-hand state :resPlayer "Blacklist" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state :archives)
    (prompt-choice :hazPlayer "Take 2 tags")
    (is (= 2 (:tag (get-runner))) "Runner has 2 tags")
    (run-empty-server state :archives)
    (prompt-choice :hazPlayer "Add News Team to score area")
    (is (= 1 (count (:scored (get-runner)))) "News Team added to Runner score area")
    (trash-from-hand state :resPlayer "News Team")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (run-empty-server state :archives)
    (prompt-choice :hazPlayer "Add News Team to score area")
    (is (= 2 (count (:scored (get-runner)))) "News Team added to Runner score area with Blacklist rez")))

(deftest net-analytics
  ;; Draw a card when runner avoids or removes 1 or more tags
  (do-game
    (new-game (default-corp [(qty "Ghost Branch" 3) (qty "Net Analytics" 3)])
              (default-runner [(qty "New Angeles City Hall" 3)]))
    (starting-hand state :resPlayer ["Net Analytics" "Ghost Branch"])
    (play-from-hand state :resPlayer "Ghost Branch" "New remote")
    (play-from-hand state :resPlayer "Net Analytics" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "New Angeles City Hall")
    (take-credits state :hazPlayer)
    (let [gb (get-content state :remote1 0)
          net (get-content state :remote2 0)
          nach (get-in @state [:hazPlayer :rig :resource 0])]
      (core/rez state :resPlayer (refresh net))
      (core/advance state :resPlayer {:card (refresh gb)})
      (is (= 1 (get-in (refresh gb) [:advance-counter])))
      (take-credits state :resPlayer)
      (is (= 1 (count (:hand (get-corp)))) "Corp hand size is 1 before run")
      (run-empty-server state "Server 1")
      (prompt-choice :resPlayer "Yes") ; choose to do the optional ability
      (card-ability state :hazPlayer nach 0)
      (prompt-choice :hazPlayer "Done")
      (prompt-choice :resPlayer "Yes") ; Draw from Net Analytics
      (prompt-choice :hazPlayer "No")
      (is (empty? (:prompt (get-runner))) "Runner waiting prompt is cleared")
      (is (= 0 (:tag (get-runner))) "Avoided 1 Ghost Branch tag")
      (is (= 2 (count (:hand (get-corp)))) "Corp draw from NA")
      ; tag removal
      (core/tag-runner state :hazPlayer 1)
      (prompt-choice :hazPlayer "No") ; Don't prevent the tag
      (core/remove-tag state :hazPlayer 1)
      (prompt-choice :resPlayer "Yes") ; Draw from Net Analytics
      (is (= 3 (count (:hand (get-corp)))) "Corp draw from NA"))))

(deftest net-police
  ;; Net Police - Recurring credits equal to Runner's link
  (do-game
    (new-game
      (default-corp [(qty "Net Police" 1)])
      (make-deck "Sunny Lebeau: Security Specialist" [(qty "Dyson Mem Chip" 1)
                                                      (qty "Access to Globalsec" 1)]))
    (play-from-hand state :resPlayer "Net Police" "New remote")
    (is (= 2 (:link (get-runner))))
    (let [netpol (get-content state :remote1 0)]
      (core/rez state :resPlayer netpol)
      (is (= 2 (:rec-counter (refresh netpol))) "2 recurring for Runner's 2 link")
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Dyson Mem Chip")
      (take-credits state :hazPlayer)
      (is (= 3 (:rec-counter (refresh netpol))) "3 recurring for Runner's 3 link")
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Access to Globalsec")
      (take-credits state :hazPlayer)
      (is (= 4 (:rec-counter (refresh netpol))) "4 recurring for Runner's 4 link"))))

(deftest ngo-front
  ;; NGO Front - full test
  (do-game
    (new-game (default-corp [(qty "NGO Front" 3)])
              (default-runner))
    (core/gain state :resPlayer :click 3)
    (play-from-hand state :resPlayer "NGO Front" "New remote")
    (play-from-hand state :resPlayer "NGO Front" "New remote")
    (play-from-hand state :resPlayer "NGO Front" "New remote")
    (let [ngo1 (get-content state :remote1 0)
          ngo2 (get-content state :remote2 0)
          ngo3 (get-content state :remote3 0)]
      (core/advance state :resPlayer {:card ngo2})
      (core/advance state :resPlayer {:card (refresh ngo3)})
      (core/advance state :resPlayer {:card (refresh ngo3)})
      (core/rez state :resPlayer (refresh ngo1))
      (core/rez state :resPlayer (refresh ngo2))
      (core/rez state :resPlayer (refresh ngo3))
      (is (= 2 (:credit (get-corp))) "Corp at 2 credits")
      (card-ability state :resPlayer ngo1 1)
      (card-ability state :resPlayer ngo1 0)
      (is (= 2 (:credit (get-corp))) "Corp still 2 credits")
      (is (= 0 (count (:discard (get-corp)))) "Nothing trashed")
      (card-ability state :resPlayer ngo2 1)
      (is (= 2 (:credit (get-corp))) "Corp still 2 credits")
      (is (= 0 (count (:discard (get-corp)))) "Nothing trashed")
      (card-ability state :resPlayer ngo2 0)
      (is (= 7 (:credit (get-corp))) "Corp gained 5 credits")
      (is (= 1 (count (:discard (get-corp)))) "1 NGO Front Trashed")
      (card-ability state :resPlayer ngo3 1)
      (is (= 15 (:credit (get-corp))) "Corp gained 8 credits")
      (is (= 2 (count (:discard (get-corp)))) "2 NGO Front Trashed")
      )))

(deftest plan-b
  ;; Plan B - score agenda with adv cost <= # of adv counters
  (do-game
    (new-game (default-corp [(qty "Plan B" 1)
                             (qty "Braintrust" 1)
                             (qty "The Future Perfect" 1)
                             (qty "Mushin No Shin" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Mushin No Shin")
    (prompt-select :resPlayer (find-card "Plan B" (:hand (get-corp))))
    (take-credits state :resPlayer)
    (run-empty-server state :remote1)
    ;; prompt for corp to use Plan B
    (prompt-choice :resPlayer "Yes")
    ;; Pick TFP, does not score
    (prompt-select :resPlayer (find-card "The Future Perfect" (:hand (get-corp))))
    (is (find-card "The Future Perfect" (:hand (get-corp))) "TFP is not scored")
    ;; Pick Brain Trust, scores
    (prompt-select :resPlayer (find-card "Braintrust" (:hand (get-corp))))
    (is (find-card "Braintrust" (:scored (get-corp))) "Braintrust is scored")))

(deftest political-dealings
  ;; Political Dealings - Full test
  (do-game
    (new-game (default-corp [(qty "Political Dealings" 1) (qty "Medical Breakthrough" 1) (qty "Oaktown Renovation" 1)])
              (default-runner))
    (core/move state :resPlayer (find-card "Medical Breakthrough" (:hand (get-corp))) :deck)
    (core/move state :resPlayer (find-card "Oaktown Renovation" (:hand (get-corp))) :deck)
    (play-from-hand state :resPlayer "Political Dealings" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    ;; Install Medical Breakthrough
    (core/draw state :resPlayer)
    (prompt-choice :resPlayer "Yes")
    (prompt-choice :resPlayer "New remote")
    (is (= "Medical Breakthrough" (:title (get-content state :remote2 0)))
        "Medical Breakthrough installed by Political Dealings")
    ;; Install Oaktown Renovation
    (core/draw state :resPlayer)
    (prompt-choice :resPlayer "Yes")
    (prompt-choice :resPlayer "New remote")
    (is (= "Oaktown Renovation" (:title (get-content state :remote3 0)))
        "Oaktown Renovation installed by Political Dealings")
    (is (= true (:rezzed (get-content state :remote3 0)))
        "Oaktown Renovation installed face up")))

(deftest political-dealings-daily-business-show
  ;; Political Dealings - Daily Business Show interaction.
  ;; Draw 2 agendas, install both of them but return 1 to bottom of R&D"
  (do-game
    (new-game (default-corp [(qty "Political Dealings" 1) (qty "Daily Business Show" 1) (qty "Turtlebacks" 1)
                             (qty "Breaking News" 1) (qty "Project Beale" 1)])
              (default-runner))
    (starting-hand state :resPlayer ["Political Dealings" "Daily Business Show" "Turtlebacks"])
    (core/gain state :resPlayer :credit 3)
    (play-from-hand state :resPlayer "Political Dealings" "New remote")
    (play-from-hand state :resPlayer "Daily Business Show" "New remote")
    (play-from-hand state :resPlayer "Turtlebacks" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (core/rez state :resPlayer (get-content state :remote2 0))
    (core/rez state :resPlayer (get-content state :remote3 0))
    (take-credits state :resPlayer)
    (is (= 0 (count (:hand (get-corp)))))
    (let [agenda1 (first (:deck (get-corp)))
          agenda2 (second (:deck (get-corp)))]
      (take-credits state :hazPlayer)
      ;; Install first agenda
      (is (= 2 (count (:hand (get-corp)))))
      (is (= 0 (:credit (get-corp))))
      (prompt-choice :resPlayer "Yes")
      (prompt-choice :resPlayer "New remote")
      (is (= (:cid agenda1) (:cid (get-content state :remote4 0))))
      (is (= 1 (:credit (get-corp))) "Turtlebacks triggered")
      ;; Install second agenda
      (prompt-choice :resPlayer "Yes")
      (prompt-choice :resPlayer "New remote")
      (is (= (:cid agenda2) (:cid (get-content state :remote5 0))))
      (is (= 2 (:credit (get-corp))) "Turtlebacks triggered")
      ;; DBS - put first agenda at bottom of R&D
      (prompt-select :resPlayer (get-content state :remote4 0))
      (is (= 0 (count (:hand (get-corp)))))
      (is (= (:cid agenda1) (:cid (last (:deck (get-corp)))))))))

(deftest psychic-field
  ;; Psychic Field - Do 1 net damage for every card in Runner's hand when accessed/exposed
  (do-game
    (new-game (default-corp [(qty "Psychic Field" 2)])
              (default-runner [(qty "Infiltration" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Psychic Field" "New remote")
    (play-from-hand state :resPlayer "Psychic Field" "New remote")
    (let [psyf1 (get-content state :remote1 0)
          psyf2 (get-content state :remote2 0)]
      (take-credits state :resPlayer)
      (starting-hand state :hazPlayer ["Infiltration" "Sure Gamble" "Sure Gamble"])
      (play-from-hand state :hazPlayer "Infiltration")
      (prompt-choice :hazPlayer "Expose a card")
      (prompt-select :hazPlayer psyf1)
      (is (= 2 (count (:hand (get-runner)))))
      (prompt-choice :resPlayer "2 [Credits]")
      (prompt-choice :hazPlayer "0 [Credits]")
      (is (= 3 (count (:discard (get-runner)))) "Suffered 2 net damage on expose and psi loss")
      (core/gain state :hazPlayer :click 3)
      (core/draw state :hazPlayer 3)
      (is (= 3 (count (:hand (get-runner)))))
      (run-empty-server state :remote2)
      (prompt-choice :resPlayer "1 [Credits]")
      (prompt-choice :hazPlayer "0 [Credits]")
      (is (= 6 (count (:discard (get-runner)))) "Suffered 3 net damage on access and psi loss"))))

(deftest psychic-field-no-access-choice-in-archives
  ;; Regression test for issue #1965 (Psychic Field showing up as an option to access / trigger in archives
  (do-game
    (new-game (default-corp [(qty "Psychic Field" 2) (qty "Shock!" 2) (qty "Clone Retirement" 2)])
              (default-runner))
    (trash-from-hand state :resPlayer "Psychic Field")
    (trash-from-hand state :resPlayer "Shock!")
    (trash-from-hand state :resPlayer "Clone Retirement")
    (take-credits state :resPlayer)
    ;; Runner run on archives to trigger access choice
    (run-empty-server state :archives)
    (is (not-any? #{"Psychic Field"} (get-in @state [:hazPlayer :prompt :choices]))
        "Psychic Field is not a choice to access in Archives")))

(deftest psychic-field-neutralize-all-threats
  ;; Psychic Field - Interaction with Neutralize All Threats and Hostile Infrastructure, #1208
  (do-game
    (new-game (default-corp [(qty "Psychic Field" 3) (qty "Hostile Infrastructure" 3)])
              (default-runner [(qty "Neutralize All Threats" 1) (qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Psychic Field" "New remote")
    (play-from-hand state :resPlayer "Hostile Infrastructure" "New remote")
    (core/rez state :resPlayer (get-content state :remote2 0))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Neutralize All Threats")
    (run-empty-server state :remote1)
    (prompt-choice :resPlayer "0 [Credits]")
    (prompt-choice :hazPlayer "1 [Credits]")
    (is (not (get-content state :remote1)) "Psychic Field trashed by Neutralize All Threats")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest public-support
  ;; Public support scoring and trashing
  ;; TODO could also test for NOT triggering "when scored" events
  (do-game
    (new-game (default-corp [(qty "Public Support" 2)])
              (default-runner))
    ;; Corp turn 1, install and rez public supports
    (play-from-hand state :resPlayer "Public Support" "New remote")
    (play-from-hand state :resPlayer "Public Support" "New remote")
    (let [publics1 (get-content state :remote1 0)
          publics2 (get-content state :remote2 0)]
      (core/rez state :resPlayer (refresh publics1))
      (core/rez state :resPlayer (refresh publics2))
      (take-credits state :resPlayer)

      ;; Runner turn 1, creds
      (is (= 2 (:credit (get-corp))))
      (is (= 3 (get-counters (refresh publics1) :power)))
      (take-credits state :hazPlayer)

      ;; Corp turn 2, creds, check if supports are ticking
      (is (= 2 (get-counters (refresh publics1) :power)))
      (is (= 0 (:agenda-point (get-corp))))
      (is (nil? (:agendapoints (refresh publics1))))
      (take-credits state :resPlayer)

      ;; Runner turn 2, run and trash publics2
      (run-empty-server state "Server 2")
      (prompt-choice :hazPlayer "Yes") ; pay to trash
      (is (= 5 (:credit (get-runner))))
      (take-credits state :hazPlayer)

      ;; Corp turn 3, check how publics1 is doing
      (is (= 1 (get-counters (refresh publics1) :power)))
      (is (= 0 (:agenda-point (get-corp))))
      (take-credits state :resPlayer)

      ;; Runner turn 3, boring
      (take-credits state :hazPlayer)

      ;; Corp turn 4, check the delicious agenda points
      (let [scored-pub (get-in @state [:resPlayer :scored 0])]
        (is (= 1 (:agenda-point (get-corp))) "Gained 1 agenda point")
        (is (= "Public Support" (:title scored-pub)))
        (is (= 1 (:agendapoints scored-pub)))))))

(deftest quarantine-system
  ;; Forfeit agenda to rez up to 3 ICE with 2 credit discount per agenda point
  (do-game
    (new-game
      (default-corp [(qty "Chiyashi" 3) (qty "Quarantine System" 1) (qty "Project Beale" 1)])
      (default-runner))
    (core/gain state :resPlayer :credit 100)
    (core/gain state :resPlayer :click 100)
    (play-from-hand state :resPlayer "Chiyashi" "HQ")
    (play-from-hand state :resPlayer "Chiyashi" "HQ")
    (play-from-hand state :resPlayer "Chiyashi" "HQ")
    (play-from-hand state :resPlayer "Quarantine System" "New remote")
    (play-from-hand state :resPlayer "Project Beale" "New remote")
    (is (= 102 (:credit (get-corp))) "Corp has 102 creds")
    (let [ch1 (get-ice state :hq 0)
          ch2 (get-ice state :hq 1)
          ch3 (get-ice state :hq 2)
          qs (get-content state :remote1 0)
          beale (get-content state :remote2 0)]
      (core/rez state :resPlayer qs)
      (card-ability state :resPlayer qs 0)
      (is (empty? (:prompt (get-corp))) "No prompt to rez ICE")
      (score-agenda state :resPlayer beale)
      ; 1 on rez
      (is (= 101 (:credit (get-corp))) "Corp has 101 creds")
      (card-ability state :resPlayer qs 0)
      (prompt-select :resPlayer (get-in (get-corp) [:scored 0]))
      (prompt-select :resPlayer ch1)
      (prompt-select :resPlayer ch2)
      (prompt-select :resPlayer ch3)
      ; pay 8 per Chiyashi - 24 total
      (is (= 77 (:credit (get-corp))) "Corp has 77 creds")
      (is (empty? (:prompt (get-corp))) "No prompt to rez ICE"))))

(deftest reality-threedee
  ;; Reality Threedee - Take 1 bad pub on rez; gain 1c at turn start (2c if Runner tagged)
  (do-game
    (new-game (default-corp [(qty "Reality Threedee" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Reality Threedee" "New remote")
    (let [r3d (get-content state :remote1 0)]
      (core/rez state :resPlayer r3d)
      (is (= 1 (:bad-publicity (get-corp))) "Took 1 bad pub on rez")
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (is (= 8 (:credit (get-corp))) "Gained 1 credit")
      (take-credits state :resPlayer)
      (core/gain state :hazPlayer :tag 1)
      (take-credits state :hazPlayer)
      (is (= 13 (:credit (get-corp))) "Gained 2 credits because Runner is tagged"))))

(deftest reconstruction-contract
  ;; Reconstruction Contract - place advancement token when runner takes meat damage
  (do-game
    (new-game (default-corp [(qty "Reconstruction Contract" 1) (qty "Scorched Earth" 1) (qty "Pup" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Imp" 3)]))
    (core/gain state :hazPlayer :tag 1)
    (core/gain state :resPlayer :credit 5)
    (starting-hand state :hazPlayer ["Sure Gamble" "Sure Gamble" "Sure Gamble" "Imp" "Imp"])
    (play-from-hand state :resPlayer "Reconstruction Contract" "New remote")
    (let [rc (get-content state :remote1 0)]
      (core/rez state :resPlayer (refresh rc))
      (play-from-hand state :resPlayer "Scorched Earth")
      (is (= 4 (count (:discard (get-runner)))))
      (is (= 1 (:advance-counter (refresh rc))) "Reconstruction Contract has 1 advancement token")
      (starting-hand state :hazPlayer ["Imp" "Imp"])
      (play-from-hand state :resPlayer "Pup" "HQ")
      (core/rez state :resPlayer (get-ice state :hq 0))
      (card-subroutine state :resPlayer (get-ice state :hq 0) 0)
      (is (= 5 (count (:discard (get-runner)))))
      (is (= 1 (:advance-counter (refresh rc))) "Reconstruction Contract doesn't get advancement token for net damage"))))

(deftest reversed-accounts
  ;; Reversed Accounts - Trash to make Runner lose 4 credits per advancement
  (do-game
    (new-game (default-corp [(qty "Reversed Accounts" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Reversed Accounts" "New remote")
    (let [rev (get-content state :remote1 0)]
      (core/advance state :resPlayer {:card (refresh rev)})
      (core/advance state :resPlayer {:card (refresh rev)})
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Sure Gamble")
      (play-from-hand state :hazPlayer "Sure Gamble")
      (play-from-hand state :hazPlayer "Sure Gamble")
      (take-credits state :hazPlayer)
      (is (= 18 (:credit (get-runner))))
      (core/advance state :resPlayer {:card (refresh rev)})
      (core/advance state :resPlayer {:card (refresh rev)})
      (is (= 4 (:advance-counter (refresh rev))))
      (core/rez state :resPlayer (refresh rev))
      (card-ability state :resPlayer rev 0)
      (is (= 1 (count (:discard (get-corp)))) "Reversed Accounts trashed")
      (is (= 2 (:credit (get-runner))) "Runner lost 16 credits"))))

(deftest ronald-five
  ;; Ronald Five - Runner loses a click every time they trash a Corp card
  (do-game
    (new-game (default-corp [(qty "Ronald Five" 1) (qty "Melange Mining Corp." 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Ronald Five" "New remote")
    (play-from-hand state :resPlayer "Melange Mining Corp." "New remote")
    (take-credits state :resPlayer)
    (core/rez state :resPlayer (get-content state :remote1 0))
    (run-empty-server state :remote2)
    (prompt-choice :hazPlayer "Yes") ; trash MMC
    (is (= 2 (:click (get-runner))) "Lost 1 click")
    (run-empty-server state :remote1)
    (prompt-choice :hazPlayer "Yes") ; trash Ronald Five
    (is (= 0 (:click (get-runner))) "Lost 1 click")))

(deftest ronin
  ;; Ronin - Click-trash to do 3 net damage when it has 4 or more advancements
  (do-game
    (new-game (default-corp [(qty "Ronin" 1) (qty "Mushin No Shin" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Mushin No Shin")
    (prompt-select :resPlayer (find-card "Ronin" (:hand (get-corp))))
    (let [ron (get-content state :remote1 0)]
      (is (= 3 (:advance-counter (refresh ron))))
      (core/rez state :resPlayer (refresh ron))
      (card-ability state :resPlayer ron 0)
      (is (= 3 (count (:hand (get-runner))))
          "Ronin ability didn't fire with only 3 advancements")
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (core/advance state :resPlayer {:card (refresh ron)})
      (is (= 4 (:advance-counter (refresh ron))))
      (card-ability state :resPlayer ron 0)
      (is (= 3 (count (:discard (get-runner)))) "Ronin did 3 net damage")
      (is (= 2 (count (:discard (get-corp)))) "Ronin trashed"))))

(deftest sandburg
  ;; Sandburg - +1 strength to all ICE for every 5c when Corp has over 10c
  (do-game
    (new-game (default-corp [(qty "Sandburg" 1) (qty "Ice Wall" 2) (qty "Hedge Fund" 3)])
              (default-runner))
    (core/gain state :resPlayer :click 3 :credit 3)
    (play-from-hand state :resPlayer "Sandburg" "New remote")
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (play-from-hand state :resPlayer "Ice Wall" "R&D")
    (let [sb (get-content state :remote1 0)
          iwall1 (get-ice state :hq 0)
          iwall2 (get-ice state :rd 0)]
      (core/rez state :resPlayer iwall1)
      (core/rez state :resPlayer iwall2)
      (core/rez state :resPlayer sb)
      (is (= 6 (:credit (get-corp))))
      (play-from-hand state :resPlayer "Hedge Fund")
      (is (= 10 (:credit (get-corp))))
      (is (= 3 (:current-strength (refresh iwall1))) "Strength boosted by 2")
      (is (= 3 (:current-strength (refresh iwall2))) "Strength boosted by 2")
      (play-from-hand state :resPlayer "Hedge Fund")
      (play-from-hand state :resPlayer "Hedge Fund")
      (is (= 18 (:credit (get-corp))))
      (is (= 4 (:current-strength (refresh iwall1))) "Strength boosted by 3")
      (is (= 4 (:current-strength (refresh iwall2))) "Strength boosted by 3")
      (take-credits state :resPlayer)
      (run-empty-server state "Server 1")
      (prompt-choice :hazPlayer "Yes")
      (is (= 1 (:current-strength (refresh iwall1))) "Strength back to default")
      (is (= 1 (:current-strength (refresh iwall2))) "Strength back to default"))))

(deftest sealed-vault
  ;; Sealed Vault - Store credits for 1c, retrieve credits by trashing or spending click
  (do-game
    (new-game (default-corp [(qty "Sealed Vault" 1) (qty "Hedge Fund" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Sealed Vault" "New remote")
    (play-from-hand state :resPlayer "Hedge Fund")
    (let [sv (get-content state :remote1 0)]
      (core/rez state :resPlayer sv)
      (card-ability state :resPlayer sv 0)
      (prompt-choice :resPlayer 8)
      (is (= 8 (get-counters (refresh sv) :credit)) "8 credits stored on Sealed Vault")
      (is (= 0 (:credit (get-corp))))
      (card-ability state :resPlayer sv 1)
      (prompt-choice :resPlayer 8)
      (is (= 0 (get-counters (refresh sv) :credit)) "Credits removed from Sealed Vault")
      (is (= 8 (:credit (get-corp))))
      (is (= 0 (:click (get-corp))) "Spent a click")
      (card-ability state :resPlayer sv 0)
      (prompt-choice :resPlayer 7)
      (is (= 7 (get-counters (refresh sv) :credit)) "7 credits stored on Sealed Vault")
      (is (= 0 (:credit (get-corp))))
      (card-ability state :resPlayer sv 2)
      (prompt-choice :resPlayer 7)
      (is (= 7 (:credit (get-corp))))
      (is (= 2 (count (:discard (get-corp)))) "Sealed Vault trashed"))))

(deftest server-diagnostics
  ;; Server Diagnostics - Gain 2c when turn begins; trashed when ICE is installed
  (do-game
    (new-game (default-corp [(qty "Server Diagnostics" 1) (qty "Pup" 1)
                             (qty "Launch Campaign" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Server Diagnostics" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (is (= 1 (count (get-content state :remote1))) "Non-ICE install didn't trash Serv Diag")
    (take-credits state :resPlayer)
    (take-credits state :hazPlayer)
    (is (= 5 (:credit (get-corp))) "Gained 2c at start of turn")
    (play-from-hand state :resPlayer "Pup" "HQ")
    (is (= 1 (count (:discard (get-corp)))) "Server Diagnostics trashed by ICE install")))

(deftest shock
  ;; do 1 net damage on access
  (do-game
    (new-game (default-corp [(qty "Shock!" 3)])
              (default-runner))
    (trash-from-hand state :resPlayer "Shock!")
    (play-from-hand state :resPlayer "Shock!" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage")
    (run-empty-server state "Archives")
    (is (= 1 (count (:hand (get-runner)))) "Runner took 1 net damage")))

(deftest shock-chairman-hiro
  ;; issue #2319 - ensure :access flag is cleared on run end
  (do-game
    (new-game (default-corp [(qty "Shock!" 3) (qty "Chairman Hiro" 1)])
              (default-runner))
    (trash-from-hand state :resPlayer "Shock!")
    (play-from-hand state :resPlayer "Shock!" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Archives")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage")
    (is (not (:run @state)) "Run is complete")
    (trash-from-hand state :resPlayer "Chairman Hiro")
    (is (= 2 (count (:discard (get-corp)))) "Hiro and Shock still in archives")
    (is (= 0 (count (:scored (get-runner)))) "Hiro not scored by Runner")))

(deftest snare
  ;; pay 4 on access, and do 3 net damage and give 1 tag
  (do-game
    (new-game (default-corp [(qty "Snare!" 3)])
              (default-runner))
    (play-from-hand state :resPlayer "Snare!" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
        "Runner has prompt to wait for Snare!")
    (prompt-choice :resPlayer "Yes")
    (is (= 3 (:credit (get-corp))) "Corp had 7 and paid 4 for Snare! 1 left")
    (is (= 1 (:tag (get-runner))) "Runner has 1 tag")
    (is (= 0 (count (:hand (get-runner)))) "Runner took 3 net damage")
    ))

(deftest snare-cant-afford
  ;; Snare! - Can't afford
  (do-game
    (new-game (default-corp [(qty "Snare!" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
    (play-from-hand state :resPlayer "Snare!" "New remote")
    (take-credits state :resPlayer)
    (core/lose state :resPlayer :credit 7)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
        "Runner has prompt to wait for Snare!")
    (prompt-choice :resPlayer "Yes")
    (is (= 0 (:tag (get-runner))) "Runner has 0 tags")
    (prompt-choice :hazPlayer "Yes")
    (is (empty? (:prompt (get-runner))) "Runner waiting prompt is cleared")
    (is (= 0 (count (:discard (get-runner)))) "Runner took no damage")))

(deftest snare-dedicated-response-team
  ;; Snare! - with Dedicated Response Team
  (do-game
    (new-game (default-corp [(qty "Snare!" 1) (qty "Dedicated Response Team" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
    (play-from-hand state :resPlayer "Snare!" "New remote")
    (play-from-hand state :resPlayer "Dedicated Response Team" "New remote")
    (core/gain state :resPlayer :click 1 :credit 4)
    (let [drt (get-content state :remote2 0)]
      (take-credits state :resPlayer)
      (run-on state "Server 1")
      (core/rez state :resPlayer drt)
      (run-successful state)
      (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :resPlayer "Yes")
      (is (= 1 (:tag (get-runner))) "Runner has 1 tag")
      (prompt-choice :hazPlayer "Yes")
      (is (= 5 (count (:discard (get-runner)))) "Runner took 5 damage"))))

(deftest space-camp-archives
  ;; Space Camp - bugged interaction from Archives. Issue #1929.
  (do-game
    (new-game (default-corp [(qty "Space Camp" 1) (qty "News Team" 1) (qty "Breaking News" 1)])
              (default-runner))
    (trash-from-hand state :resPlayer "Space Camp")
    (trash-from-hand state :resPlayer "News Team")
    (play-from-hand state :resPlayer "Breaking News" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state :archives)
    (prompt-choice :hazPlayer "News Team")
    (prompt-choice :hazPlayer "Take 2 tags")
    (prompt-choice :hazPlayer "Space Camp")
    (prompt-choice :resPlayer "Yes")
    (prompt-select :resPlayer (get-content state :remote1 0))
    (is (= 1 (:advance-counter (get-content state :remote1 0))) "Agenda advanced once from Space Camp")
    (is (= 2 (:tag (get-runner))) "Runner has 2 tags")
    (is (not (:run @state)) "Run completed")))

(deftest student-loans
  ;; Student Loans - costs Runner 2c extra to play event if already same one in discard
  (do-game
    (new-game (default-corp [(qty "Student Loans" 1) (qty "Hedge Fund" 2)])
              (default-runner))
    (core/gain state :resPlayer :credit 2)
    (play-from-hand state :resPlayer "Student Loans" "New remote")
    (core/rez state :resPlayer (get-content state :remote1 0))
    (is (= 5 (:credit (get-corp))) "Corp has 5c")
    (play-from-hand state :resPlayer "Hedge Fund")
    (is (= 9 (:credit (get-corp))) "Corp has 9c - no penalty from Student Loans")
    (play-from-hand state :resPlayer "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Corp has 13c - no penalty from Student Loans")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Sure Gamble")
    (is (= 9 (:credit (get-runner))) "1st Gamble played for 4c")
    (play-from-hand state :hazPlayer "Sure Gamble")
    (is (= 11 (:credit (get-runner))) "2nd Gamble played for 2c")
    (play-from-hand state :hazPlayer "Sure Gamble")
    (is (= 13 (:credit (get-runner))) "3rd Gamble played for 2c")))

(deftest sundew
  ;; Sundew
  (do-game
    (new-game (default-corp [(qty "Sundew" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Sundew" "New remote")
    (let [sund (get-content state :remote1 0)]
      (core/rez state :resPlayer sund)
      (take-credits state :resPlayer 2)
      (is (= 5 (:credit (get-corp))) "Cost 2cr to rez")
      ;; spend a click not on a run
      (take-credits state :hazPlayer)
      (is (= 7 (:credit (get-corp))) "Corp gained 2cr from Sundew")
      (take-credits state :resPlayer)
      (run-on state "Server 1")
      (is (= 10 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew")
      (is (= 3 (:click (get-runner))) "Runner spent 1 click to start run"))))

;(deftest sundew-dirty-laundry
;  "Sundew - Dirty Laundry"
;  (do-game
;    (new-game (default-corp [(qty "Sundew" 1)])
;              (default-runner [(qty "Dirty Laundry" 1)]))
;    (play-from-hand state :resPlayer "Sundew" "New remote")
;    (let [sund (first (get-in @state [:resPlayer :servers :remote1 :content]))]
;      (core/rez state :resPlayer sund)
;      (take-credits state :resPlayer 2)
;      (is (= 5 (:credit (get-corp))) "Cost 2cr to rez")
;      ; spend a click on a run through a card, not through click-run.
;      (play-run-event state (find-card "Dirty Laundry" (:hand (get-runner))) :remote1)
;      (is (= 5 (:credit (get-corp))) "Corp did not gain 2cr from run on Sundew"))))

(deftest team-sponsorship-hq
  ;; Team Sponsorship - Install from HQ
  (do-game
    (new-game (default-corp [(qty "Domestic Sleepers" 1)
                             (qty "Team Sponsorship" 1)
                             (qty "Adonis Campaign" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Team Sponsorship" "New remote")
    (play-from-hand state :resPlayer "Domestic Sleepers" "New remote")
    (let [ag1 (get-content state :remote2 0)
          tsp (get-content state :remote1 0)]
      (core/rez state :resPlayer tsp)
      (score-agenda state :resPlayer ag1)
      (prompt-select :resPlayer (find-card "Adonis Campaign" (:hand (get-corp))))
      (prompt-choice :resPlayer "New remote")
      (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
          "Adonis installed by Team Sponsorship")
      (is (nil? (find-card "Adonis Campaign" (:hand (get-corp)))) "No Adonis in hand"))))

(deftest team-sponsorship-archives
  ;; Team Sponsorship - Install from Archives
  (do-game
    (new-game (default-corp [(qty "Domestic Sleepers" 1)
                             (qty "Team Sponsorship" 1)
                             (qty "Adonis Campaign" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Team Sponsorship" "New remote")
    (play-from-hand state :resPlayer "Domestic Sleepers" "New remote")
    (trash-from-hand state :resPlayer "Adonis Campaign")
    (let [ag1 (get-content state :remote2 0)
          tsp (get-content state :remote1 0)]
      (core/rez state :resPlayer tsp)
      (score-agenda state :resPlayer ag1)
      (prompt-select :resPlayer (find-card "Adonis Campaign" (:discard (get-corp))))
      (prompt-choice :resPlayer "New remote")
      (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
          "Adonis installed by Team Sponsorship")
      (is (nil? (find-card "Adonis Campaign" (:discard (get-corp)))) "No Adonis in discard"))))

(deftest team-sponsorship-multiple
  ;; Team Sponsorship - Multiple installed
  (do-game
    (new-game (default-corp [(qty "Domestic Sleepers" 1)
                             (qty "Team Sponsorship" 2)
                             (qty "Adonis Campaign" 2)])
              (default-runner))
    (play-from-hand state :resPlayer "Team Sponsorship" "New remote")
    (play-from-hand state :resPlayer "Team Sponsorship" "New remote")
    (play-from-hand state :resPlayer "Domestic Sleepers" "New remote")
    (trash-from-hand state :resPlayer "Adonis Campaign")
    (let [ag1 (get-content state :remote3 0)
          tsp2 (get-content state :remote2 0)
          tsp1 (get-content state :remote1 0)]
      (core/rez state :resPlayer tsp1)
      (core/rez state :resPlayer tsp2)
      (score-agenda state :resPlayer ag1)
      (prompt-choice :resPlayer "Team Sponsorship")
      (prompt-select :resPlayer (find-card "Adonis Campaign" (:discard (get-corp))))
      (prompt-choice :resPlayer "New remote")
      (prompt-select :resPlayer (find-card "Adonis Campaign" (:hand (get-corp))))
      (prompt-choice :resPlayer "New remote")
      (is (= "Adonis Campaign" (:title (get-content state :remote4 0)))
          "Adonis installed by Team Sponsorship")
      (is (= "Adonis Campaign" (:title (get-content state :remote5 0)))
          "Adonis installed by Team Sponsorship"))))

(deftest team-sponsorship-one-window
  ;; Team Sponsorship - Score 5 points in one window
  (do-game
    (new-game (default-corp [(qty "AstroScript Pilot Program" 3)
                             (qty "Team Sponsorship" 1)
                             (qty "Breaking News" 1)
                             (qty "SanSan City Grid" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "SanSan City Grid" "New remote")
    (core/gain state :resPlayer :credit 100 :click 5)
    (core/rez state :resPlayer (get-content state :remote1 0))
    (play-from-hand state :resPlayer "AstroScript Pilot Program" "New remote")
    (score-agenda state :resPlayer (get-content state :remote2 0))
    (play-from-hand state :resPlayer "AstroScript Pilot Program" "Server 1")
    (play-from-hand state :resPlayer "Team Sponsorship" "New remote")
    (core/rez state :resPlayer (get-content state :remote3 0))
    (score-agenda state :resPlayer (get-content state :remote1 1))
    (prompt-select :resPlayer (find-card "AstroScript Pilot Program" (:hand (get-corp))))
    (is (= 0 (get-counters (second (:scored (get-corp))) :agenda)) "AstroScript not resolved yet")
    (prompt-choice :resPlayer "Server 1")
    (is (= 1 (get-counters (second (:scored (get-corp))) :agenda)) "AstroScript resolved")
    (card-ability state :resPlayer (first (:scored (get-corp))) 0)
    (prompt-select :resPlayer (get-content state :remote1 1))
    (card-ability state :resPlayer (second (:scored (get-corp))) 0)
    (prompt-select :resPlayer (get-content state :remote1 1))
    (core/score state :resPlayer {:card (get-content state :remote1 1)})
    (prompt-select :resPlayer (find-card "Breaking News" (:hand (get-corp))))
    (prompt-choice :resPlayer "Server 1")
    (card-ability state :resPlayer (second (next (:scored (get-corp)))) 0)
    (prompt-select :resPlayer (get-content state :remote1 1))
    (core/score state :resPlayer {:card (get-content state :remote1 1)})
    (prompt-choice :resPlayer "Done")
    (is (= 7 (:agenda-point (get-corp))) "Scored 5 points in one turn")))

(deftest the-board
  ;; The Board - Modify everything in the score area (regression test for #1938)
  (do-game
    (new-game (default-corp [(qty "The Board" 1)
                             (qty "News Team" 1)
                             (qty "Firmware Updates" 2)])
              (default-runner [(qty "Artist Colony" 3)
                               (qty "Fan Site" 3)]))
    (play-from-hand state :resPlayer "The Board" "New remote")
    (play-from-hand state :resPlayer "News Team" "New remote")
    (play-from-hand state :resPlayer "Firmware Updates" "New remote")
    (take-credits state :resPlayer)

    (play-from-hand state :hazPlayer "Artist Colony")
    (play-from-hand state :hazPlayer "Fan Site")
    (take-credits state :hazPlayer)

    (play-from-hand state :resPlayer "Firmware Updates" "New remote")
    (score-agenda state :resPlayer (get-content state :remote4 0))
    (is (= 1 (count (:scored (get-runner)))) "Fan Site added to Runner score area")
    (is (= 0 (:agenda-point (get-runner))) "Runner has 0 agenda points")

    (take-credits state :resPlayer)

    (run-empty-server state :remote3)
    (prompt-choice :hazPlayer "Steal")
    (is (= 2 (count (:scored (get-runner)))) "Firmware Updates stolen")
    (is (= 1 (:agenda-point (get-runner))) "Runner has 1 agenda point")

    (core/rez state :resPlayer (get-content state :remote1 0))
    (is (= -1 (:agenda-point (get-runner))) "Runner has -1 agenda points")

    (run-empty-server state :remote2)
    (prompt-choice :hazPlayer "Add News Team to score area")
    (is (= 3 (count (:scored (get-runner)))) "News Team added to Runner score area")
    (is (= -3 (:agenda-point (get-runner))) "Runner has -3 agenda points")

    (card-ability state :hazPlayer (get-resource state 0) 0)
    (prompt-choice :hazPlayer (->> @state :hazPlayer :prompt first :choices first))
    (prompt-select :hazPlayer (first (:scored (get-runner))))
    (is (= 2 (count (:scored (get-runner)))) "Fan Site removed from Runner score area")
    (is (= -2 (:agenda-point (get-runner))) "Runner has -2 agenda points")

    (run-empty-server state :remote1)
    (prompt-choice :hazPlayer "Yes")
    (is (= 3 (count (:scored (get-runner)))) "The Board added to Runner score area")
    (is (= 2 (:agenda-point (get-runner))) "Runner has 2 agenda points")))

(deftest the-root
  ;; The Root - recurring credits refill at Step 1.2
  (do-game
    (new-game (make-deck "Blue Sun: Powering the Future" [(qty "The Root" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "The Root" "New remote")
    (core/gain state :resPlayer :credit 6)
    (let [root (get-content state :remote1 0)]
      (core/rez state :resPlayer root)
      (card-ability state :resPlayer (refresh root) 0)
      (is (= 2 (:rec-counter (refresh root))) "Took 1 credit from The Root")
       (is (= 6 (:credit (get-corp))) "Corp took Root credit into credit pool")
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      ; we expect Step 1.2 to have triggered because of Blue Sun
      (is (:resPlayer-phase-12 @state) "Corp is in Step 1.2")
      (is (= 3 (:rec-counter (refresh root))) "Recurring credits were refilled before Step 1.2 window"))))

(deftest toshiyuki-sakai
  ;; Toshiyuki Sakai - Swap with an asset/agenda from HQ; Runner can choose to access new card or not
  (do-game
    (new-game (default-corp [(qty "Toshiyuki Sakai" 1) (qty "Project Junebug" 1) (qty "Hedge Fund" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
    (play-from-hand state :resPlayer "Toshiyuki Sakai" "New remote")
    (let [toshi (get-content state :remote1 0)]
      (core/advance state :resPlayer {:card (refresh toshi)})
      (core/advance state :resPlayer {:card (refresh toshi)})
      (take-credits state :resPlayer)
      (is (= 2 (:advance-counter (refresh toshi))) "Toshiyuki has 2 advancements")
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
          "Runner has prompt to wait for Toshiyuki")
      (prompt-choice :resPlayer "Yes") ; choose to do a swap
      (prompt-select :resPlayer (find-card "Hedge Fund" (:hand (get-corp))))
      (is (= (refresh toshi) (get-content state :remote1 0)) "Toshiyuki still in remote; can't target an operation in hand")
      (prompt-select :resPlayer (find-card "Project Junebug" (:hand (get-corp))))
      (let [june (get-content state :remote1 0)]
        (is (= "Project Junebug" (:title (refresh june))) "Project Junebug swapped into Server 1")
        (is (= 2 (:advance-counter (refresh june))) "Project Junebug has 2 advancements")
        (prompt-choice :hazPlayer "Yes") ; choose to access new card
        (prompt-choice :resPlayer "Yes") ; pay 1c to fire Junebug
        (is (= 4 (count (:discard (get-runner)))) "Runner took 4 net damage")))))

(deftest turtlebacks
  ;; Turtlebacks - Gain 1 credit for every new server created
  (do-game
    (new-game (default-corp [(qty "Turtlebacks" 1) (qty "PAD Campaign" 2) (qty "Wraparound" 1)])
              (default-runner))
    (core/gain state :resPlayer :click 1)
    (play-from-hand state :resPlayer "Turtlebacks" "New remote")
    (let [tb (get-content state :remote1 0)]
      (core/rez state :resPlayer tb)
      (play-from-hand state :resPlayer "PAD Campaign" "New remote")
      (is (= 4 (:credit (get-corp))) "Gained 1 credit for new server created")
      (play-from-hand state :resPlayer "Wraparound" "Server 1")
      (is (= 4 (:credit (get-corp))) "No credit gained for install into existing server")
      (play-from-hand state :resPlayer "PAD Campaign" "New remote")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit for new server created"))))

(deftest urban-renewal
  ;; Urban renewal meat damage
  (do-game
    (new-game (default-corp [(qty "Urban Renewal" 1)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
    ;; Corp turn 1, install and rez urban renewal
    (play-from-hand state :resPlayer "Urban Renewal" "New remote")
    (let [ur (get-content state :remote1 0)]
      (core/rez state :resPlayer (refresh ur))
      (take-credits state :resPlayer)
      ;; Runner turn 1, creds
      (is (= 3 (get-counters (refresh ur) :power)))
      (take-credits state :hazPlayer)

      ;; Corp turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :resPlayer)

      ;; Runner turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :hazPlayer)

      ;; Corp turn 3
      (is (= 1 (get-counters (refresh ur) :power)))
      (take-credits state :resPlayer)

      ;; Runner turn 3
      (is (= 0 (count (:discard (get-corp)))) "Nothing in Corp trash")
      (is (= 0 (count (:discard (get-runner)))) "Nothing in Runner trash")
      (take-credits state :hazPlayer)

      ;; Corp turn 4 - damage fires
      (is (= 1 (count (:discard (get-corp)))) "Urban Renewal got trashed")
      (is (= 4 (count (:discard (get-runner)))) "Urban Renewal did 4 meat damage"))))

(deftest watchdog
  ;; Watchdog - Reduce rez cost of first ICE per turn by number of Runner tags
  (do-game
    (new-game (default-corp [(qty "Watchdog" 1) (qty "Architect" 1) (qty "Wraparound" 1)])
              (default-runner))
    (play-from-hand state :resPlayer "Watchdog" "New remote")
    (play-from-hand state :resPlayer "Wraparound" "HQ")
    (play-from-hand state :resPlayer "Architect" "HQ")
    (let [wd (get-content state :remote1 0)
          arch (get-ice state :hq 1)
          wrap (get-ice state :hq 0)]
      (take-credits state :resPlayer)
      (is (= 4 (:credit (get-corp))))
      (core/gain state :hazPlayer :tag 2)
      (run-on state "HQ")
      (core/rez state :resPlayer wd)
      (core/rez state :resPlayer arch)
      (is (= 2 (:credit (get-corp))) "Only 2 credits to rez Architect")
      (core/rez state :resPlayer wrap)
      (is (= 0 (:credit (get-corp))) "No rez discount on Wraparound"))))

(deftest whampoa-reclamation
  ;; Whampoa Reclamation: Enable trashing a card from HQ to place a card in Archives on the bottom of R+D
  (do-game
    (new-game (default-corp [(qty "Whampoa Reclamation" 3) (qty "PAD Campaign" 2) (qty "Global Food Initiative" 3)])
              (default-runner))
    (play-from-hand state :resPlayer "Whampoa Reclamation" "New remote")
    (let [wr (get-content state :remote1 0)]
      (core/draw state :resPlayer)
      (take-credits state :resPlayer)
      (core/rez state :resPlayer wr)
      (let [gfi (find-card "Global Food Initiative" (:hand (get-corp)))]
        (core/trash state :hazPlayer gfi)
        (card-ability state :resPlayer wr 0)
        (prompt-choice :resPlayer "Global Food Initiative") ;; into archives
        (prompt-select :resPlayer (first (:discard (get-corp)))) ;; into R&D
        (is (= 0 (count (:discard (get-corp)))) "Only card in discard placed in bottom of R&D")
        (is (= "Global Food Initiative" (:title (last (:deck (get-corp))))) "GFI last card in deck")))))
