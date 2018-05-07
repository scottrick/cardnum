(ns test.cards.assets
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adonis-campaign
  (do-game
    (new-game (default-contestant [(qty "Adonis Campaign" 1)])
              (default-hero))
    (play-from-hand state :contestant "Adonis Campaign" "New remote")
    (let [ac (get-content state :remote1 0)]
      (core/rez state :contestant ac)
      (is (= 1 (get-in @state [:contestant :credit])))
      (is (= 12 (get-counters (refresh ac) :credit)) "12 counters on Adonis")
      (take-credits state :contestant 2)
      (take-credits state :hero)
      (is (= 6 (get-in @state [:contestant :credit])) "Gain 3 from Adonis")
      (is (= 9 (get-counters (refresh ac) :credit))) "9 counter remaining on Adonis")))

(deftest aggressive-secretary
  (do-game
    (new-game
      (default-contestant [(qty "Aggressive Secretary" 1)])
      (default-hero [(qty "Cache" 3)]))
    (play-from-hand state :contestant "Aggressive Secretary" "New remote")
    (let [as (get-content state :remote1 0)]
      ;; Single advance AggSec
      (core/advance state :contestant {:card (refresh as)})
      (take-credits state :contestant)
      ;; Run on AggSec with 3 programs
      (play-from-hand state :hero "Cache")
      (play-from-hand state :hero "Cache")
      (play-from-hand state :hero "Cache")
      (run-empty-server state "Server 1")
      (prompt-choice :contestant "Yes")
      (is (= 3 (get-in @state [:contestant :credit])))
      ;; Corp can trash one program
      (prompt-select :contestant (get-in @state [:hero :rig :program 1]))
      ;; There should be two Caches left
      (is (= 3 (get-in @state [:contestant :credit])))
      (is (= 2 (count (get-in @state [:hero :rig :program])))))))

(deftest alexa-belsky
  (do-game
    (new-game
      (default-contestant [(qty "Alexa Belsky" 1) (qty "Hedge Fund" 1) (qty "Breaking News" 1)
                     (qty "Gutenberg" 1) (qty "Product Placement" 1) (qty "Jackson Howard" 1)])
      (default-hero))
    (play-from-hand state :contestant "Alexa Belsky" "New remote")
    (let [alexa (get-content state :remote1 0)]
      (core/rez state :contestant alexa)
      (card-ability state :contestant alexa 0)
      (is (= 1 (count (:discard (get-contestant)))) "Alexa Belsky trashed")
      (is (= 5 (count (:hand (get-contestant)))))
      (is (= 0 (count (:deck (get-contestant)))))
      (prompt-choice :hero 5) ;Runner chooses to pay 5 credits so 2 cards are prevented from being shuffled
      (is (= 2 (count (:hand (get-contestant)))))
      (is (= 3 (count (:deck (get-contestant)))))
      (is (= 0 (:credit (get-hero)))))))

(deftest alix-t4lb07
  (do-game
    (new-game
      (default-contestant [(qty "Alix T4LB07" 1) (qty "PAD Campaign" 3)])
      (default-hero))
    (play-from-hand state :contestant "Alix T4LB07" "New remote")
    (let [alix (get-content state :remote1 0)]
      (core/rez state :contestant alix)
      (play-from-hand state :contestant "PAD Campaign" "New remote")
      (play-from-hand state :contestant "PAD Campaign" "New remote")
      (take-credits state :contestant)
      (take-credits state :hero)
      (is (= 2 (get-counters (refresh alix) :power)) "Two counters on Alix")
      (is (= 4 (get-in @state [:contestant :credit])))
      (card-ability state :contestant alix 0)
      (is (= 8 (get-in @state [:contestant :credit]))))) "Gain 4 credits from Alix")

(deftest blacklist-steal
  ;; Blacklist - #2426.  Need to allow steal.
  (do-game
    (new-game (default-contestant [(qty "Fetal AI" 3) (qty "Blacklist" 1)])
              (default-hero))
    (trash-from-hand state :contestant "Fetal AI")
    (play-from-hand state :contestant "Blacklist" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (= 1 (count (get-in @state [:contestant :discard])))
    (take-credits state :contestant)
    (run-empty-server state :archives)
    (prompt-choice :hero "Yes")
    (is (= 2 (:agenda-point (get-hero))) "Runner has 2 agenda points")
    (= 1 (count (get-in @state [:hero :scored])))))

(deftest bio-ethics-multiple
  ;; Bio-Ethics Association: preventing damage from multiple copies
  (do-game
    (new-game
      (default-contestant [(qty "Bio-Ethics Association" 2)])
      (default-hero [(qty "Feedback Filter" 1) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Bio-Ethics Association" "New remote")
    (play-from-hand state :contestant "Bio-Ethics Association" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (core/rez state :contestant (get-content state :remote2 0))
    (take-credits state :contestant)
    (play-from-hand state :hero "Feedback Filter")
    (take-credits state :hero)
    (let [filter (get-hardware state 0)]
      (is (= 1 (count (:prompt (get-hero)))) "Runner has a single damage prevention prompt")
      (card-ability state :hero filter 0)
      (prompt-choice :hero "Done")
      (is (= 0 (count (:discard (get-hero)))) "Runner prevented damage")
      (is (= 1 (count (:prompt (get-hero)))) "Runner has a next damage prevention prompt")
      (prompt-choice :hero "Done")
      (is (= 1 (count (:discard (get-hero)))) "Runner took 1 net damage"))))

(deftest brain-taping-warehouse
  ;; Brain-Taping Warehouse - Lower rez cost of Bioroid ICE by 1 for each unspent Runner click
  (do-game
    (new-game (default-contestant [(qty "Brain-Taping Warehouse" 1) (qty "Ichi 1.0" 1)
                             (qty "Eli 1.0" 1)])
              (default-hero))
    (play-from-hand state :contestant "Brain-Taping Warehouse" "New remote")
    (play-from-hand state :contestant "Ichi 1.0" "Server 1")
    (play-from-hand state :contestant "Eli 1.0" "HQ")
    (let [ichi (get-ice state :remote1 0)
          eli (get-ice state :hq 0)]
      (take-credits state :contestant)
      (run-on state :remote1)
      (core/rez state :contestant (get-content state :remote1 0))
      (is (= 3 (:click (get-hero))))
      (core/rez state :contestant ichi)
      (is (= 2 (:credit (get-contestant))) "Paid only 2c to rez Ichi; reduction of 3c")
      (run-jack-out state)
      (run-on state :hq)
      (is (= 2 (:click (get-hero))))
      (core/rez state :contestant eli)
      (is (= 1 (:credit (get-contestant))) "Paid only 1c to rez Eli; reduction of 2c"))))

(deftest capital-investors
  ;; Capital Investors - Click for 2 credits
  (do-game
    (new-game (default-contestant [(qty "Capital Investors" 1)])
              (default-hero))
    (play-from-hand state :contestant "Capital Investors" "New remote")
    (let [cap (get-content state :remote1 0)]
      (core/rez state :contestant cap)
      (card-ability state :contestant cap 0)
      (card-ability state :contestant cap 0)
      (is (= 0 (:click (get-contestant))) "Used twice, spent 2 clicks")
      (is (= 7 (:credit (get-contestant))) "Used twice, gained 4 credits"))))

(deftest chairman-hiro
  ;; Chairman Hiro - Reduce Runner max hand size; add as 2 agenda points if Runner trashes him
  (do-game
    (new-game (default-contestant [(qty "Chairman Hiro" 2)])
              (default-hero))
    (play-from-hand state :contestant "Chairman Hiro" "New remote")
    (play-from-hand state :contestant "Chairman Hiro" "Server 1")
    (prompt-choice :contestant "OK")
    (is (= 1 (count (:discard (get-contestant)))) "First Hiro trashed")
    (is (= 0 (:agenda-point (get-hero))) "No points for Runner if trashed by Corp")
    (let [hiro (get-content state :remote1 0)]
      (core/rez state :contestant hiro)
      (is (= 3 (core/hand-size state :hero)) "Runner max hand size reduced by 2")
      (take-credits state :contestant)
      (take-credits state :hero 3)
      (run-empty-server state "Server 1")
      (prompt-choice :hero "Yes") ; trash Hiro
      (is (= 2 (:credit (get-hero))) "Runner paid 6 credits to trash")
      (is (= 5 (core/hand-size state :hero)) "Runner max hand size restored to 5")
      (is (= 1 (count (get-in @state [:hero :scored])))
          "Chairman Hiro added to Runner score area")
      (is (= 2 (:agenda-point (get-hero))) "Runner gained 2 agenda points"))))

(deftest city-surveillance
  ;; City Surveillance - Runner chooses to pay 1 credit or take 1 tag at start of their turn
  (do-game
    (new-game (default-contestant [(qty "City Surveillance" 1)])
              (default-hero))
    (play-from-hand state :contestant "City Surveillance" "New remote")
    (let [surv (get-content state :remote1 0)]
      (core/rez state :contestant surv)
      (take-credits state :contestant)
      (prompt-choice :hero "Pay 1 [Credits]")
      (is (= 4 (:credit (get-hero))) "Runner paid 1 credit")
      (is (= 0 (:tag (get-hero))) "Runner didn't take a tag")
      (is (empty? (:prompt (get-hero))) "City Surveillance only fired once")
      (take-credits state :hero)
      (take-credits state :contestant)
      (prompt-choice :hero "Take 1 tag")
      (is (= 8 (:credit (get-hero))) "Runner paid no credits")
      (is (= 1 (:tag (get-hero))) "Runner took 1 tag"))
      (is (empty? (:prompt (get-hero))) "City Surveillance only fired once")))

(deftest clyde-van-rite
  ;; Clyde Van Rite - Multiple scenarios involving Runner not having credits/cards to trash
  (do-game
    (new-game (default-contestant [(qty "Clyde Van Rite" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Restructure" 2) (qty "John Masanori" 2)]))
    (play-from-hand state :contestant "Clyde Van Rite" "New remote")
    (let [clyde (get-content state :remote1 0)]
      (core/rez state :contestant clyde)
      (take-credits state :contestant)
      (take-credits state :hero)
      (is (:contestant-phase-12 @state) "Corp in Step 1.2")
      ;; Runner chooses to pay - has 1+ credit so pays 1 credit
      (card-ability state :contestant clyde 0)
      (is (= 9 (:credit (get-hero))))
      (is (= 2 (count (:deck (get-hero)))))
      (prompt-choice :hero "Pay 1 [Credits]")
      (is (= 8 (:credit (get-hero))))
      (is (= 2 (count (:deck (get-hero)))))
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :hero)
      ;; Runner chooses to pay - can't pay 1 credit so trash top card
      (core/lose state :hero :credit 12)
      (card-ability state :contestant clyde 0)
      (is (= 0 (:credit (get-hero))))
      (is (= 2 (count (:deck (get-hero)))))
      (prompt-choice :hero "Pay 1 [Credits]")
      (is (= 0 (:credit (get-hero))))
      (is (= 1 (count (:deck (get-hero)))))
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :hero)
      ;; Runner chooses to trash - has 1+ card in Stack so trash 1 card
      (card-ability state :contestant clyde 0)
      (is (= 4 (:credit (get-hero))))
      (is (= 1 (count (:deck (get-hero)))))
      (prompt-choice :hero "Trash top card")
      (is (= 4 (:credit (get-hero))))
      (is (= 0 (count (:deck (get-hero)))))
      (core/end-phase-12 state :contestant nil)
      (take-credits state :contestant)
      (take-credits state :hero)
      ;; Runner chooses to trash - no cards in Stack so pays 1 credit
      (card-ability state :contestant clyde 0)
      (is (= 8 (:credit (get-hero))))
      (is (= 0 (count (:deck (get-hero)))))
      (prompt-choice :hero "Trash top card")
      (is (= 7 (:credit (get-hero))))
      (is (= 0 (count (:deck (get-hero))))))))

(deftest daily-business-show
  ;; Daily Business Show - Full test
  (do-game
    (new-game (default-contestant [(qty "Daily Business Show" 3) (qty "Hedge Fund" 1) (qty "Jackson Howard" 1)
                             (qty "Resistor" 1) (qty "Product Placement" 1) (qty "Breaking News" 1)])
              (default-hero))
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
    (take-credits state :hero)
    (is (= 5 (count (:hand (get-contestant)))) "Drew an additional 3 cards with 3 DBS")
    (is (not-empty (:prompt (get-hero))) "Runner is waiting for Corp to use DBS")
    (prompt-select :contestant (find-card "Hedge Fund" (:hand (get-contestant)))) ;invalid target
    (prompt-select :contestant (find-card "Resistor" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "Product Placement" (:hand (get-contestant))))
    (prompt-select :contestant (find-card "Breaking News" (:hand (get-contestant))))
    (is (= 2 (count (:hand (get-contestant)))))
    (is (= "Hedge Fund" (:title (first (:hand (get-contestant))))))
    (is (= "Jackson Howard" (:title (second (:hand (get-contestant))))))
    (is (= "Resistor" (:title (last (:deck (get-contestant))))) "Resistor last card in deck")
    (is (= "Product Placement" (:title (last (butlast (:deck (get-contestant))))))
        "Product Placement second last card in deck")
    (is (= "Breaking News" (:title (last (butlast (butlast (:deck (get-contestant)))))))
        "Breaking News third last card in deck")))

(deftest daily-business-show-sensie-actors-union
  ;; Daily Business Show - Sensie Actors Union interaction
  (do-game
    (new-game (default-contestant [(qty "Daily Business Show" 1) (qty "Sensie Actors Union" 2)
                             (qty "Hedge Fund" 1) (qty "Jackson Howard" 1)
                             (qty "Resistor" 1) (qty "Product Placement" 1) (qty "Breaking News" 1)])
              (default-hero))
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
      (take-credits state :hero)
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
      (is (empty? (get-in @state [:contestant :prompt])) "Sensie didn't activate")
      (is (= 3 (count (:hand (get-contestant)))))
      ;; Use second Sensie
      (starting-hand state :contestant ["Hedge Fund" "Jackson Howard"])
      (is (= 2 (count (:hand (get-contestant)))))
      (card-ability state :contestant sensie2 0)
      (is (= 5 (count (:hand (get-contestant)))) "Drew 3 cards with Sensie, DBS didn't activate")
      (prompt-select :contestant (find-card "Breaking News" (:hand (get-contestant)))) ; Sensie target
      (is (= "Breaking News" (:title (last (:deck (get-contestant))))) "Breaking News last card in deck"))))

(deftest daily-business-show-manual-draw
  ;; Daily Business Show - Should not trigger if rezzed after mandatory draw
  (do-game
    (new-game (default-contestant [(qty "Daily Business Show" 3) (qty "Hedge Fund" 1) (qty "Jackson Howard" 1)
                             (qty "Resistor" 1) (qty "Product Placement" 1) (qty "Breaking News" 1)])
              (default-hero))
    (starting-hand state :contestant ["Daily Business Show"])
    (play-from-hand state :contestant "Daily Business Show" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (core/draw state :contestant)
    (is (= 1 (count (:hand (get-contestant)))) "DBS did not fire on manual draw")
    (is (empty? (:prompt (get-contestant))) "Corp is not being asked to bury a card with DBS")    ))

(deftest dedicated-response-team
  ;; Dedicated Response Team - Do 2 meat damage when successful run ends if Runner is tagged
  (do-game
    (new-game (default-contestant [(qty "Dedicated Response Team" 1)])
              (default-hero))
    (play-from-hand state :contestant "Dedicated Response Team" "New remote")
    (let [drt (get-content state :remote1 0)]
      (core/rez state :contestant drt)
      (take-credits state :contestant)
      (run-empty-server state :rd)
      (is (empty? (:discard (get-hero))) "Not tagged, no damage done")
      (core/gain state :hero :tag 1)
      (run-on state :rd)
      (run-jack-out state)
      (is (empty? (:discard (get-hero))) "Tagged but run unsuccessful, no damage done")
      (run-empty-server state :rd)
      (is (= 2 (count (:discard (get-hero)))) "Suffered 2 damage for successful run w/ tag"))))

(deftest early-premiere
  ;; Early Premiere - Pay 1c at start of turn to place an advancement on a card in a server
  (do-game
    (new-game (default-contestant [(qty "Early Premiere" 1) (qty "Ice Wall" 1)
                             (qty "Ghost Branch" 1) (qty "Blacklist" 1)])
              (default-hero))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Early Premiere" "New remote")
    (play-from-hand state :contestant "Blacklist" "New remote")
    (play-from-hand state :contestant "Ghost Branch" "New remote")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (let [ep (get-content state :remote1 0)
          bl (get-content state :remote2 0)
          gb (get-content state :remote3 0)
          iw (get-ice state :hq 0)]
      (core/rez state :contestant ep)
      (take-credits state :contestant)
      (take-credits state :hero)
      (card-ability state :contestant ep 0)
      (prompt-select :contestant iw)
      (is (nil? (:advance-counter (refresh iw))) "Ice Wall can't targeted, not in server")
      (prompt-select :contestant bl)
      (is (nil? (:advance-counter (refresh bl))) "Blacklist can't targeted, can't be advanced")
      (prompt-select :contestant gb)
      (is (= 1 (:advance-counter (refresh gb))) "1 advancement on Ghost Branch")
      (is (= 4 (:credit (get-contestant)))))))

(deftest echochamber
  ;; Echo Chamber - 3 clicks to become 1 point agenda
  (do-game
    (new-game (default-contestant [(qty "Echo Chamber" 1)])
              (default-hero))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Echo Chamber" "New remote")
    (let [ec (get-content state :remote1 0)]
      (core/rez state :contestant ec)
      (card-ability state :contestant ec 0))
    (is (= 1 (:agendapoints (get-in @state [:contestant :scored 0]))) "Echo Chamber added to Corp score area")))

(deftest edge-of-world
  ;; Edge of World - ability
  (do-game
    (new-game (default-contestant [(qty "Edge of World" 3) (qty "Ice Wall" 3)])
              (default-hero))
    (core/gain state :contestant :credit 6 :click 1)
    (play-from-hand state :contestant "Edge of World" "New remote")
    (play-from-hand state :contestant "Edge of World" "New remote")
    (play-from-hand state :contestant "Ice Wall" "Server 1")
    (play-from-hand state :contestant "Ice Wall" "Server 1")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :hero :prompt first :prompt-type))
        "Runner waiting for Corp to act")
    (prompt-choice :contestant "Yes")
    (prompt-choice :hero "Yes")
    (is (= 2 (:brain-damage (get-hero))) "Runner took 2 brain damage")
    (run-empty-server state "Server 2")
    (prompt-choice :contestant "Yes")
    (prompt-choice :hero "Yes")
    (is (= 2 (:brain-damage (get-hero))) "Runner did not take brain damage when no ICE protected Edge of World")))

(deftest elizabeth-mills
  ;; Elizabeth Mills - Remove 1 bad publicity when rezzed; click-trash to trash a location
  (do-game
    (new-game (default-contestant [(qty "Elizabeth Mills" 1)])
              (default-hero [(qty "Earthrise Hotel" 1)]))
    (core/gain state :contestant :bad-publicity 1)
    (play-from-hand state :contestant "Elizabeth Mills" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :hero "Earthrise Hotel")
    (take-credits state :hero)
    (let [liz (get-content state :remote1 0)
          hotel (get-in @state [:hero :rig :resource 0])]
      (core/rez state :contestant liz)
      (is (= 0 (:bad-publicity (get-contestant))) "1 bad publicity removed")
      (card-ability state :contestant liz 0)
      (prompt-select :contestant hotel)
      (is (= 1 (count (:discard (get-hero)))) "Earthrise trashed")
      (is (= 1 (count (:discard (get-contestant)))) "Elizabeth Mills trashed")
      (is (= 1 (:bad-publicity (get-contestant))) "1 bad publicity taken from trashing a location"))))

(deftest elizas-toybox
  ;; Eliza's Toybox - Rez a card ignoring all costs
  (do-game
    (new-game (default-contestant [(qty "Eliza's Toybox" 1) (qty "Wotan" 1)])
              (default-hero))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Wotan" "R&D")
    (play-from-hand state :contestant "Eliza's Toybox" "New remote")
    (let [wotan (get-ice state :rd 0)
          eliza (get-content state :remote1 0)]
      (core/rez state :contestant eliza)
      (is (= 1 (:credit (get-contestant))))
      (card-ability state :contestant eliza 0)
      (prompt-select :contestant wotan)
      (is (get-in (refresh wotan) [:rezzed]))
      (is (= 0 (:click (get-contestant))) "3 clicks spent")
      (is (= 1 (:credit (get-contestant))) "No credits spent"))))

(deftest encryption-protocol
  ;; Encryption Protocol - Trash cost of installed cards increased by 1
  (do-game
    (new-game (default-contestant [(qty "Encryption Protocol" 2)])
              (default-hero))
    (play-from-hand state :contestant "Encryption Protocol" "New remote")
    (play-from-hand state :contestant "Encryption Protocol" "New remote")
    (let [ep1 (get-content state :remote1 0)
          ep2 (get-content state :remote2 0)]
      (core/rez state :contestant ep1)
      (core/rez state :contestant ep2)
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (is (= 4 (core/trash-cost state :hero (refresh ep1)))
          "Trash cost increased to 4 by two active Encryption Protocols")
      (prompt-choice :hero "Yes") ; trash first EP
      (run-empty-server state "Server 2")
      (is (= 3 (core/trash-cost state :hero (refresh ep2)))
          "Trash cost increased to 3 by one active Encryption Protocol"))))

(deftest eve-campaign
  (do-game
    (new-game (default-contestant [(qty "Eve Campaign" 1)])
              (default-hero))
    (play-from-hand state :contestant "Eve Campaign" "New remote")
    (let [eve (get-content state :remote1 0)]
      (core/rez state :contestant eve)
      (is (= 0 (get-in @state [:contestant :credit])))
      (is (= 16 (get-counters (refresh eve) :credit)))
      (take-credits state :contestant 2)
      (take-credits state :hero)
      (is (= 4 (get-in @state [:contestant :credit])))
      (is (= 14 (get-counters (refresh eve) :credit))))))

(deftest executive-boot-camp-suppress-start-of-turn
  ;; Executive Boot Camp - suppress the start-of-turn event on a rezzed card. Issue #1346.
  (do-game
    (new-game (default-contestant [(qty "Eve Campaign" 1) (qty "Executive Boot Camp" 1)])
              (default-hero))
    (play-from-hand state :contestant "Eve Campaign" "New remote")
    (play-from-hand state :contestant "Executive Boot Camp" "New remote")
    (take-credits state :contestant)
    (is (= 6 (:credit (get-contestant))) "Corp ends turn with 6 credits")
    (let [eve (get-content state :remote1 0)
          ebc (get-content state :remote2 0)]
      (core/rez state :contestant ebc)
      (take-credits state :hero)
      (is (:contestant-phase-12 @state) "Corp in Step 1.2")
      (card-ability state :contestant ebc 0)
      (prompt-select :contestant eve)
      (is (= 2 (:credit (get-contestant))) "EBC saved 1 credit on the rez of Eve")
      (is (= 16 (get-counters (refresh eve) :credit)))
      (core/end-phase-12 state :contestant nil)
      (is (= 2 (:credit (get-contestant))) "Corp did not gain credits from Eve")
      (is (= 16 (get-counters (refresh eve) :credit)) "Did not take counters from Eve")
      (take-credits state :contestant)
      (take-credits state :hero)
      (is (not (:contestant-phase-12 @state)) "With nothing to rez, EBC does not trigger Step 1.2")
      (is (= 14 (get-counters (refresh eve) :credit)) "Took counters from Eve"))))

(deftest franchise-city
  (do-game
    (new-game (default-contestant [(qty "Franchise City" 1) (qty "Accelerated Beta Test" 1)])
              (default-hero))
    (play-from-hand state :contestant "Franchise City" "New remote")
    (play-from-hand state :contestant "Accelerated Beta Test" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant 1)
    (run-empty-server state "Server 2")
    (prompt-choice :hero "Steal")
    (is (= 0 (count (get-in @state [:contestant :servers :server2 :content]))) "Agenda was stolen")
    (is (= 2 (:agenda-point (get-hero))) "Runner stole 2 points")
    (is (= 0 (count (get-in @state [:contestant :servers :server1 :content])))
        "Franchise City no longer installed")
    (is (find-card "Franchise City" (:scored (get-contestant))) "Franchise City in contestant scored area")
    (is (= 1 (:agenda-point (get-contestant))) "Corp has 1 point")))

(deftest full-immersion-recstudio
  ;; Full Immmersion RecStudio - install directly, and via Interns
  (do-game
    (new-game
      (default-contestant [(qty "Full Immersion RecStudio" 1)
                     (qty "Interns" 2)
                     (qty "Launch Campaign" 3)])
      (default-hero))
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
        (take-credits state :hero)
        (is (= 5 (:credit (get-contestant))) "Gained 2cr from Launch Campaign")
        (is (= 4 (get-counters (refresh lc) :credit)) "4cr left on Launch Campaign")
        (play-from-hand state :contestant "Interns")
        (prompt-select :contestant (find-card "Launch Campaign" (:hand (get-contestant))))
        (prompt-choice :contestant (refresh fir))
        (is (= 2 (count (:hosted (refresh fir)))) "Interns installed onto FIR")))))

(deftest full-immersion-recstudio-sandburg
  ;; Full Immmersion RecStudio - hosting an asset with events does not double-register events. Issue #1827.
  (do-game
    (new-game
      (default-contestant [(qty "Full Immersion RecStudio" 1) (qty "Sandburg" 1) (qty "Vanilla" 1)
                     (qty "Oaktown Renovation" 1)])
      (default-hero))
    (play-from-hand state :contestant "Full Immersion RecStudio" "New remote")
    (play-from-hand state :contestant "Vanilla" "HQ")
    (let [fir (get-content state :remote1 0)
          van (get-ice state :hq 0)]
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
      (is (= 11 (:credit (get-contestant))) "Gained 1cr from advancing Oaktown"))))

(deftest gene-splicer-access-unadvanced-no-trash
  ;; Runner accesses an unadvanced Gene Splicer and doesn't trash
  ;; No net damage is dealt and Gene Splicer remains installed
  (do-game
    (new-game
      (default-contestant [(qty "Gene Splicer" 1)])
      (default-hero [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Gene Splicer" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "No")
    (is (= 0 (count (:discard (get-hero)))) "Runner took no net damage")
    (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
    (is (= 5 (:credit (get-hero))) "Runner spent no credits")))

(deftest gene-splicer-access-unadvanced-trash
  ;; Runner accesses an unadvanced Gene Splicer and trashes it - no net damage is dealt and Gene Splicer is trashed
  (do-game
    (new-game
      (default-contestant [(qty "Gene Splicer" 1)])
      (default-hero [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Gene Splicer" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes")
    (is (= 0 (count (:discard (get-hero)))) "Runner took no net damage")
    (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
    (is (= (:title (last (:discard (get-contestant)))) "Gene Splicer") "Gene Splicer trashed")
    (is (= 4 (:credit (get-hero))) "Runner spent 1 credit to trash Gene Splicer")))

(deftest gene-splicer-access-single-advanced-no-trash
  ;; Runner accesses a single-advanced Gene Splicer and doesn't trash
  ;; 1 net damage is dealt and Gene Splicer remains installed
  (do-game
    (new-game
      (default-contestant [(qty "Gene Splicer" 1)])
      (default-hero [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Gene Splicer" "New remote")
    (core/add-counter state :contestant (get-content state :remote1 0) :advancement 1)
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "No")
    (is (= 1 (count (:discard (get-hero)))) "Runner took 1 net damage")
    (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
    (is (= 5 (:credit (get-hero))) "Runner spent no credits")))

(deftest gene-splicer-access-single-advanced-trash
  ;; Runner accesses a single-advanced Gene Splicer and trashes it
  ;; 1 net damage is dealt and Gene Splicer is trashed
  (do-game
    (new-game
      (default-contestant [(qty "Gene Splicer" 1)])
      (default-hero [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Gene Splicer" "New remote")
    (core/add-counter state :contestant (get-content state :remote1 0) :advancement 1)
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes")
    (is (= 1 (count (:discard (get-hero)))) "Runner took 1 net damage")
    (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
    (is (= (:title (last (:discard (get-contestant)))) "Gene Splicer") "Gene Splicer trashed")
    (is (= 4 (:credit (get-hero))) "Runner spent 1 credit to trash Gene Splicer")))

(deftest gene-splicer-access-double-advanced-no-trash
  ;; Runner accesses a double-advanced Gene Splicer and doesn't trash
  ;; 2 net damage is dealt and Gene Splicer remains installed
  (do-game
    (new-game
      (default-contestant [(qty "Gene Splicer" 1)])
      (default-hero [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Gene Splicer" "New remote")
    (core/add-counter state :contestant (get-content state :remote1 0) :advancement 2)
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "No")
    (is (= 2 (count (:discard (get-hero)))) "Runner took 2 net damage")
    (is (= "Gene Splicer" (:title (get-content state :remote1 0))) "Gene Splicer was not trashed")
    (is (= 5 (:credit (get-hero))) "Runner spent no credits")))

(deftest gene-splicer-access-double-advanced-trash
  ;; Runner accesses a double-advanced Gene Splicer and trashes it
  ;; 2 net damage is dealt and Gene Splicer is trashed
  (do-game
    (new-game
      (default-contestant [(qty "Gene Splicer" 1)])
      (default-hero [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Gene Splicer" "New remote")
    (core/add-counter state :contestant (get-content state :remote1 0) :advancement 2)
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes")
    (is (= 2 (count (:discard (get-hero)))) "Runner took 2 net damage")
    (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
    (is (= (:title (last (:discard (get-contestant)))) "Gene Splicer") "Gene Splicer trashed")
    (is (= 4 (:credit (get-hero))) "Runner spent 1 credit to trash Gene Splicer")))

(deftest gene-splicer-agenda-ability
  ;; Corp triple-advances a Gene Splicer and uses its ability to add to their score area as a 1 point agenda
  (do-game
    (new-game
      (default-contestant [(qty "Gene Splicer" 2) (qty "Ice Wall" 3) (qty "Vanilla" 2)])
      (default-hero [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Gene Splicer" "New remote")
    (let [gs (get-content state :remote1 0)]
      (core/add-counter state :contestant gs :advancement 2)
      (take-credits state :hero)
      (core/add-counter state :contestant (refresh gs) :advancement 1)
      (core/rez state :contestant (refresh gs))
      (card-ability state :contestant (refresh gs) 0)
      (is (= nil (get-content state :remote1 0)) "Gene Splicer is no longer in remote")
      (is (= 1 (:agendapoints (get-in @state [:contestant :scored 0]))) "Gene Splicer added to Corp score area"))))

(deftest genetics-pavilion
  ;; Genetics Pavilion - Limit Runner to 2 draws per turn, but only during Runner's turn
  (do-game
    (new-game (default-contestant [(qty "Genetics Pavilion" 1)])
              (default-hero [(qty "Diesel" 1) (qty "Sure Gamble" 3) (qty "Sports Hopper" 1)]))
    (play-from-hand state :contestant "Genetics Pavilion" "New remote")
    (let [gp (get-content state :remote1 0)]
      (take-credits state :contestant)
      (core/rez state :contestant gp)
      (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
      (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
      (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
      (play-from-hand state :hero "Sports Hopper")
      (play-from-hand state :hero "Diesel")
      (is (= 2 (count (:hand (get-hero)))) "Drew only 2 cards because of Genetics Pavilion")
      (take-credits state :hero)
      (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
      (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
      (let [hopper (get-in @state [:hero :rig :hardware 0])]
        (card-ability state :hero hopper 0)
        (is (= 3 (count (:hand (get-hero)))) "Able to draw 3 cards during Corp's turn")
        (core/derez state :contestant (refresh gp))
        (take-credits state :contestant)
        (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
        (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
        (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
        (core/move state :hero (find-card "Diesel" (:discard (get-hero))) :hand)
        (is (= 1 (count (:hand (get-hero)))))
        (play-from-hand state :hero "Diesel")
        (is (= 3 (count (:hand (get-hero)))) "Drew 3 cards with Diesel")
        (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
        (core/rez state :contestant (refresh gp))
        (core/draw state :hero)
        (is (= 2 (count (:hand (get-hero)))) "No card drawn; GP counts cards drawn prior to rez")))))

(deftest genetics-pavilion-fisk-investment
  (do-game
    (new-game (default-contestant [(qty "Genetics Pavilion" 1) (qty "Hedge Fund" 3)])
              (default-hero [(qty "Fisk Investment Seminar" 1) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Genetics Pavilion" "New remote")
    (let [gp (get-content state :remote1 0)]
      (take-credits state :contestant)
      (core/rez state :contestant gp)
      (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
      (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
      (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
      (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
      (is (= 1 (count (:hand (get-hero)))))
      (is (= 0 (count (:hand (get-contestant)))))
      (play-from-hand state :hero "Fisk Investment Seminar")
      (is (= 2 (count (:hand (get-hero)))) "Drew only 2 cards because of Genetics Pavilion")
      (is (= 3 (count (:hand (get-contestant)))) "Drew all 3 cards"))))

(deftest genetics-pavilion-mr-li
  ;; Genetics Pavilion - Mr. Li interaction. #1594
  (do-game
    (new-game (default-contestant [(qty "Genetics Pavilion" 1)])
              (default-hero [(qty "Mr. Li" 1) (qty "Account Siphon" 1) (qty "Faerie" 1)
                               (qty "Sure Gamble" 1) (qty "John Masanori" 1) (qty "Desperado" 1)]))
    (starting-hand state :hero ["Mr. Li"])
    (play-from-hand state :contestant "Genetics Pavilion" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (play-from-hand state :hero "Mr. Li")
    (let [mrli (get-in @state [:hero :rig :resource 0])]
      (is (= 0 (count (:hand (get-hero)))))
      ;use Mr. Li with 2 draws allowed
      (card-ability state :hero mrli 0)
      (is (= 2 (count (:hand (get-hero)))))
      (prompt-select :hero (first (:hand (get-hero))))
      (is (= 1 (count (:hand (get-hero)))))
      ;use Mr. Li with 0 draws allowed
      (card-ability state :hero mrli 0)
      (is (= 1 (count (:hand (get-hero)))))
      (prompt-select :hero (first (:hand (get-hero)))) ;will fail because not a valid target
      (prompt-choice :hero "Done") ;cancel out
      (take-credits state :hero)
      (take-credits state :contestant)
      (core/draw state :hero)
      (is (= 2 (count (:hand (get-hero)))))
      ;use Mr. Li with 1 draw allowed
      (card-ability state :hero mrli 0)
      (is (= 3 (count (:hand (get-hero)))))
      (prompt-select :hero (first (:hand (get-hero)))) ;will fail
      (prompt-select :hero (second (:hand (get-hero)))) ;will fail
      (prompt-select :hero (second (rest (:hand (get-hero)))))
      (is (= 2 (count (:hand (get-hero))))))))

(deftest ghost-branch
  ;; Ghost Branch - Advanceable; give the Runner tags equal to advancements when accessed
  (do-game
    (new-game (default-contestant [(qty "Ghost Branch" 1)])
              (default-hero))
    (play-from-hand state :contestant "Ghost Branch" "New remote")
    (let [gb (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh gb)})
      (core/advance state :contestant {:card (refresh gb)})
      (is (= 2 (get-in (refresh gb) [:advance-counter])))
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (prompt-choice :contestant "Yes") ; choose to do the optional ability
      (is (= 2 (:tag (get-hero))) "Runner given 2 tags"))))

(deftest honeyfarm
  ;; lose one credit on access
  (do-game
    (new-game (default-contestant [(qty "Honeyfarm" 3)])
              (default-hero))
    (trash-from-hand state :contestant "Honeyfarm")
    (play-from-hand state :contestant "Honeyfarm" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (is (= 4 (:credit (get-hero))))
    (run-empty-server state "Archives")
    (is (= 3 (:credit (get-hero))))
	(run-empty-server state "HQ")
    (is (= 2 (:credit (get-hero))))))

(deftest hostile-infrastructure
  ;; Hostile Infrastructure - do 1 net damage when hero trashes a contestant card
  (do-game
    (new-game (default-contestant [(qty "Hostile Infrastructure" 3)])
              (default-hero))
    (core/gain state :hero :credit 50)
    (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (run-empty-server state :hq)
    (prompt-choice :hero "Yes")
    (is (= 1 (count (:discard (get-hero)))) "Took 1 net damage")
    (run-empty-server state :remote1)
    (prompt-choice :hero "Yes")
    (is (= 2 (count (:discard (get-hero)))) "Took 1 net damage")))

(deftest hyoubu-research-facility
  (do-game
    (new-game (default-contestant [(qty "Hyoubu Research Facility" 1) (qty "Snowflake" 1)])
              (default-hero))
    (play-from-hand state :contestant "Hyoubu Research Facility" "New remote")
    (play-from-hand state :contestant "Snowflake" "HQ")
    (let [hrf (get-content state :remote1 0)
          sf (get-ice state :hq 0)]
      (take-credits state :contestant)
      (run-on state "HQ")
      (core/rez state :contestant hrf)
      (core/rez state :contestant sf)
      (card-subroutine state :contestant sf 0)
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :hero "0 [Credits]")
      (is (= 5 (:credit (get-contestant))) "Gained 2c from Hyoubu")
      (run-on state "HQ")
      (card-subroutine state :contestant sf 0)
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :hero "0 [Credits]")
      (is (= 3 (:credit (get-contestant))) "No credits gained from Hyoubu"))))

(deftest illegal-arms-factory
  ;; Illegal Arms Factory; draw a card, gain a credit, bad pub when trashed while rezzed
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 1)
	                         (qty "Beanstalk Royalties" 1)
	                         (qty "IPO" 1)
							 (qty "Illegal Arms Factory" 3)])
              (default-hero))
    (core/gain state :hero :credit 20)
	(core/move state :contestant (find-card "IPO" (:hand (get-contestant))) :deck)
	(core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
	(core/move state :contestant (find-card "Beanstalk Royalties" (:hand (get-contestant))) :deck)
    (play-from-hand state :contestant "Illegal Arms Factory" "New remote")
    (play-from-hand state :contestant "Illegal Arms Factory" "New remote")
    (let [iaf (get-content state :remote2 0)]
      (core/rez state :contestant iaf)
      (take-credits state :contestant)
	  (run-empty-server state :remote1)
      (prompt-choice :hero "Yes")
      (is (= 0 (:bad-publicity (get-contestant))) "Took no bad pub on unrezzed trash")
      (take-credits state :hero)
	  (is (= 3 (count (:hand (get-contestant)))) "Drew a card from IAF + mandatory")
      (is (= 4 (:credit (get-contestant))) "Gained 1 credit from IAF")
      (take-credits state :contestant)
	  (run-empty-server state :remote2)
      (prompt-choice :hero "Yes")
      (is (= 1 (:bad-publicity (get-contestant))) "Took a bad pub on rezzed trash"))))

(deftest it-department
  ;; IT Department - Add strength to rezzed ICE until end of turn
  (do-game
    (new-game (default-contestant [(qty "IT Department" 1) (qty "Wall of Static" 1)])
              (default-hero))
    (play-from-hand state :contestant "IT Department" "New remote")
    (play-from-hand state :contestant "Wall of Static" "Server 1")
    (let [itd (get-content state :remote1 0)
          wos (get-ice state :remote1 0)]
      (core/rez state :contestant itd)
      (core/rez state :contestant wos)
      (card-ability state :contestant itd 1)
      (is (= 0 (:click (get-contestant))) "Spent 1 click")
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

(deftest jackson-howard-draw
  ;; Jackson Howard - Draw 2 cards
  (do-game
    (new-game (default-contestant [(qty "Jackson Howard" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Restructure" 2)])
              (default-hero))
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
  (do-game
    (new-game (default-contestant [(qty "Jeeves Model Bioroids" 1) (qty "TGTBT" 1)
                             (qty "Melange Mining Corp." 2)])
              (default-hero [(qty "Ghost Runner" 3)]))
    (play-from-hand state :contestant "Jeeves Model Bioroids" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (take-credits state :contestant)
    (play-from-hand state :hero "Ghost Runner")
    (play-from-hand state :hero "Ghost Runner")
    (play-from-hand state :hero "Ghost Runner")
    (take-credits state :hero)
    ; install 3 things
    (play-from-hand state :contestant "TGTBT" "New remote")
    (play-from-hand state :contestant "Melange Mining Corp." "New remote")
    (play-from-hand state :contestant "Melange Mining Corp." "New remote")
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :hero)
    ;;click for credits
    (take-credits state :contestant 3)
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :hero)
    ;;click to purge
    (core/do-purge state :contestant 3)
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :hero)
    ;;click to advance
    (core/advance state :contestant (get-content state :remote2 0))
    (core/advance state :contestant (get-content state :remote2 0))
    (core/advance state :contestant (get-content state :remote2 0))
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :hero)
    ;; use 3 clicks on card ability - Melange
    (core/rez state :contestant (get-content state :remote3 0))
    (card-ability state :contestant (get-content state :remote3 0) 0)
    (is (= 1 (:click (get-contestant))))
    (take-credits state :contestant)
    (take-credits state :hero)
    ;; trash 3 resources
    (core/gain state :hero :tag 1)
    (core/trash-resource state :contestant nil)
    (prompt-select :contestant (get-resource state 0))
    (is (= 1 (count (:discard (get-hero)))))
    (core/trash-resource state :contestant nil)
    (prompt-select :contestant (get-resource state 0))
    (is (= 2 (count (:discard (get-hero)))))
    (core/trash-resource state :contestant nil)
    (prompt-select :contestant (get-resource state 0))
    (is (= 3 (count (:discard (get-hero)))))
    (is (= 1 (:click (get-contestant))))))

(deftest kala-ghoda
  ; Kala Ghoda Real TV
  (do-game
    (new-game (default-contestant [(qty "Kala Ghoda Real TV" 1)])
              (default-hero) [(qty "Sure Gamble" 3)])
    (starting-hand state :hero ["Sure Gamble"])
    (play-from-hand state :contestant "Kala Ghoda Real TV" "New remote")
    (let [tv (get-content state :remote1 0)]
      (core/rez state :contestant tv)
      (take-credits state :contestant)
      (take-credits state :hero)
      (is (:contestant-phase-12 @state) "Corp is in Step 1.2")
      (card-ability state :contestant tv 0)
      (prompt-choice :contestant "Done")
      (card-ability state :contestant tv 1)
      (is (= 1 (count (:discard (get-contestant)))))
      (is (= 1 (count (:discard (get-hero)))))
      (is (last-log-contains? state "Sure Gamble")
          "Kala Ghoda did log trashed card names"))))

(deftest launch-campaign
  (do-game
    (new-game (default-contestant [(qty "Launch Campaign" 1)])
              (default-hero))
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (let [launch (get-content state :remote1 0)]
      (core/rez state :contestant launch)
      (is (= 4 (get-in @state [:contestant :credit])))
      (is (= 6 (get-counters (refresh launch) :credit)))
      (take-credits state :contestant 2)
      (take-credits state :hero)
      (is (= 8 (get-in @state [:contestant :credit])))
      (is (= 4 (get-counters (refresh launch) :credit))))))

(deftest mark-yale
  ;; Mark Yale - Spend agenda counters or trash himself to gain credits
  (do-game
    (new-game (default-contestant [(qty "Firmware Updates" 1) (qty "Mark Yale" 1)])
              (default-hero))
    (play-from-hand state :contestant "Firmware Updates" "New remote")
    (play-from-hand state :contestant "Mark Yale" "New remote")
    (let [firm (get-content state :remote1 0)
          yale (get-content state :remote2 0)]
      (score-agenda state :contestant firm)
      (core/rez state :contestant yale)
      (let [firmscored (get-in @state [:contestant :scored 0])]
        (is (= 3 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :contestant yale 1)
        (prompt-select :contestant firmscored)
        (is (= 7 (:credit (get-contestant))) "Gained 3 credits")
        (is (= 2 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :contestant yale 1)
        (prompt-select :contestant firmscored)
        (is (= 10 (:credit (get-contestant))) "Gained 3 credits")
        (is (= 1 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :contestant yale 1)
        (prompt-select :contestant firmscored)
        (is (= 13 (:credit (get-contestant))) "Gained 3 credits")
        (is (= 0 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :contestant yale 1)
        (prompt-select :contestant firmscored)
        (is (= 13 (:credit (get-contestant))) "Gained 0 credits because agenda needs a counter")
        (is (= 0 (get-counters (refresh firmscored) :agenda)))
        (card-ability state :contestant yale 0)
        (is (= 15 (:credit (get-contestant))) "Gained 2 credits")
        (is (= 1 (count (:discard (get-contestant)))) "Mark Yale trashed")))))

(deftest mca-austerity-policy
  (do-game
    (new-game
      (default-contestant [(qty "MCA Austerity Policy" 1)])
      (default-hero))
    (play-from-hand state :contestant "MCA Austerity Policy" "New remote")
    (let [mca (get-content state :remote1 0)]
      (core/rez state :contestant mca)
      (card-ability state :contestant mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      ; once per turn only
      (card-ability state :contestant mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      (take-credits state :contestant)
      ; hero loses a click
      (is (= 3 (:click (get-hero))))
      (take-credits state :hero)
      (card-ability state :contestant mca 0)
      (is (= 2 (get-counters (refresh mca) :power)))
      (take-credits state :contestant)
      (take-credits state :hero)
      (card-ability state :contestant mca 0)
      (is (= 3 (get-counters (refresh mca) :power)))
      ; Fire MCA
      (is (= 2 (:click (get-contestant))))
      (card-ability state :contestant (refresh mca) 1)
      (is (= 5 (:click (get-contestant)))))))

(deftest mental-health-clinic
  ;; Mental Health Clinic - Gain 1 credit when turn begins; Runner max hand size increased by 1
  (do-game
    (new-game (default-contestant [(qty "Mental Health Clinic" 1)])
              (default-hero))
    (play-from-hand state :contestant "Mental Health Clinic" "New remote")
    (let [mhc (get-content state :remote1 0)]
      (core/rez state :contestant mhc)
      (is (= 6 (core/hand-size state :hero)) "Runner max hand size increased by 1")
      (take-credits state :contestant)
      (take-credits state :hero)
      (is (= 8 (:credit (get-contestant))) "Gained 1 credit at start of turn"))))

(deftest news-team
  ;; News Team - on access take 2 tags or take as agenda worth -1
  (do-game
    (new-game (default-contestant [(qty "News Team" 3) (qty "Blacklist" 1)])
              (default-hero))
    (trash-from-hand state :contestant "News Team")
    (play-from-hand state :contestant "Blacklist" "New remote")
    (take-credits state :contestant)
    (run-empty-server state :archives)
    (prompt-choice :hero "Take 2 tags")
    (is (= 2 (:tag (get-hero))) "Runner has 2 tags")
    (run-empty-server state :archives)
    (prompt-choice :hero "Add News Team to score area")
    (is (= 1 (count (:scored (get-hero)))) "News Team added to Runner score area")
    (trash-from-hand state :contestant "News Team")
    (core/rez state :contestant (get-content state :remote1 0))
    (run-empty-server state :archives)
    (prompt-choice :hero "Add News Team to score area")
    (is (= 2 (count (:scored (get-hero)))) "News Team added to Runner score area with Blacklist rez")))

(deftest net-analytics
  ;; Draw a card when hero avoids or removes 1 or more tags
  (do-game
    (new-game (default-contestant [(qty "Ghost Branch" 3) (qty "Net Analytics" 3)])
              (default-hero [(qty "New Angeles City Hall" 3)]))
    (starting-hand state :contestant ["Net Analytics" "Ghost Branch"])
    (play-from-hand state :contestant "Ghost Branch" "New remote")
    (play-from-hand state :contestant "Net Analytics" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :hero "New Angeles City Hall")
    (take-credits state :hero)
    (let [gb (get-content state :remote1 0)
          net (get-content state :remote2 0)
          nach (get-in @state [:hero :rig :resource 0])]
      (core/rez state :contestant (refresh net))
      (core/advance state :contestant {:card (refresh gb)})
      (is (= 1 (get-in (refresh gb) [:advance-counter])))
      (take-credits state :contestant)
      (is (= 1 (count (:hand (get-contestant)))) "Corp hand size is 1 before run")
      (run-empty-server state "Server 1")
      (prompt-choice :contestant "Yes") ; choose to do the optional ability
      (card-ability state :hero nach 0)
      (prompt-choice :hero "Done")
      (prompt-choice :contestant "Yes") ; Draw from Net Analytics
      (prompt-choice :hero "No")
      (is (empty? (:prompt (get-hero))) "Runner waiting prompt is cleared")
      (is (= 0 (:tag (get-hero))) "Avoided 1 Ghost Branch tag")
      (is (= 2 (count (:hand (get-contestant)))) "Corp draw from NA")
      ; tag removal
      (core/tag-hero state :hero 1)
      (prompt-choice :hero "No") ; Don't prevent the tag
      (core/remove-tag state :hero 1)
      (prompt-choice :contestant "Yes") ; Draw from Net Analytics
      (is (= 3 (count (:hand (get-contestant)))) "Corp draw from NA"))))

(deftest net-police
  ;; Net Police - Recurring credits equal to Runner's link
  (do-game
    (new-game
      (default-contestant [(qty "Net Police" 1)])
      (make-deck "Sunny Lebeau: Security Specialist" [(qty "Dyson Mem Chip" 1)
                                                      (qty "Access to Globalsec" 1)]))
    (play-from-hand state :contestant "Net Police" "New remote")
    (is (= 2 (:link (get-hero))))
    (let [netpol (get-content state :remote1 0)]
      (core/rez state :contestant netpol)
      (is (= 2 (:rec-counter (refresh netpol))) "2 recurring for Runner's 2 link")
      (take-credits state :contestant)
      (play-from-hand state :hero "Dyson Mem Chip")
      (take-credits state :hero)
      (is (= 3 (:rec-counter (refresh netpol))) "3 recurring for Runner's 3 link")
      (take-credits state :contestant)
      (play-from-hand state :hero "Access to Globalsec")
      (take-credits state :hero)
      (is (= 4 (:rec-counter (refresh netpol))) "4 recurring for Runner's 4 link"))))

(deftest ngo-front
  ;; NGO Front - full test
  (do-game
    (new-game (default-contestant [(qty "NGO Front" 3)])
              (default-hero))
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
      (is (= 2 (:credit (get-contestant))) "Corp at 2 credits")
      (card-ability state :contestant ngo1 1)
      (card-ability state :contestant ngo1 0)
      (is (= 2 (:credit (get-contestant))) "Corp still 2 credits")
      (is (= 0 (count (:discard (get-contestant)))) "Nothing trashed")
      (card-ability state :contestant ngo2 1)
      (is (= 2 (:credit (get-contestant))) "Corp still 2 credits")
      (is (= 0 (count (:discard (get-contestant)))) "Nothing trashed")
      (card-ability state :contestant ngo2 0)
      (is (= 7 (:credit (get-contestant))) "Corp gained 5 credits")
      (is (= 1 (count (:discard (get-contestant)))) "1 NGO Front Trashed")
      (card-ability state :contestant ngo3 1)
      (is (= 15 (:credit (get-contestant))) "Corp gained 8 credits")
      (is (= 2 (count (:discard (get-contestant)))) "2 NGO Front Trashed")
      )))

(deftest plan-b
  ;; Plan B - score agenda with adv cost <= # of adv counters
  (do-game
    (new-game (default-contestant [(qty "Plan B" 1)
                             (qty "Braintrust" 1)
                             (qty "The Future Perfect" 1)
                             (qty "Mushin No Shin" 1)])
              (default-hero))
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
  ;; Political Dealings - Full test
  (do-game
    (new-game (default-contestant [(qty "Political Dealings" 1) (qty "Medical Breakthrough" 1) (qty "Oaktown Renovation" 1)])
              (default-hero))
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
    (is (= true (:rezzed (get-content state :remote3 0)))
        "Oaktown Renovation installed face up")))

(deftest political-dealings-daily-business-show
  ;; Political Dealings - Daily Business Show interaction.
  ;; Draw 2 agendas, install both of them but return 1 to bottom of R&D"
  (do-game
    (new-game (default-contestant [(qty "Political Dealings" 1) (qty "Daily Business Show" 1) (qty "Turtlebacks" 1)
                             (qty "Breaking News" 1) (qty "Project Beale" 1)])
              (default-hero))
    (starting-hand state :contestant ["Political Dealings" "Daily Business Show" "Turtlebacks"])
    (core/gain state :contestant :credit 3)
    (play-from-hand state :contestant "Political Dealings" "New remote")
    (play-from-hand state :contestant "Daily Business Show" "New remote")
    (play-from-hand state :contestant "Turtlebacks" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (core/rez state :contestant (get-content state :remote2 0))
    (core/rez state :contestant (get-content state :remote3 0))
    (take-credits state :contestant)
    (is (= 0 (count (:hand (get-contestant)))))
    (let [agenda1 (first (:deck (get-contestant)))
          agenda2 (second (:deck (get-contestant)))]
      (take-credits state :hero)
      ;; Install first agenda
      (is (= 2 (count (:hand (get-contestant)))))
      (is (= 0 (:credit (get-contestant))))
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
      (is (= 0 (count (:hand (get-contestant)))))
      (is (= (:cid agenda1) (:cid (last (:deck (get-contestant)))))))))

(deftest psychic-field
  ;; Psychic Field - Do 1 net damage for every card in Runner's hand when accessed/exposed
  (do-game
    (new-game (default-contestant [(qty "Psychic Field" 2)])
              (default-hero [(qty "Infiltration" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Psychic Field" "New remote")
    (play-from-hand state :contestant "Psychic Field" "New remote")
    (let [psyf1 (get-content state :remote1 0)
          psyf2 (get-content state :remote2 0)]
      (take-credits state :contestant)
      (starting-hand state :hero ["Infiltration" "Sure Gamble" "Sure Gamble"])
      (play-from-hand state :hero "Infiltration")
      (prompt-choice :hero "Expose a card")
      (prompt-select :hero psyf1)
      (is (= 2 (count (:hand (get-hero)))))
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :hero "0 [Credits]")
      (is (= 3 (count (:discard (get-hero)))) "Suffered 2 net damage on expose and psi loss")
      (core/gain state :hero :click 3)
      (core/draw state :hero 3)
      (is (= 3 (count (:hand (get-hero)))))
      (run-empty-server state :remote2)
      (prompt-choice :contestant "1 [Credits]")
      (prompt-choice :hero "0 [Credits]")
      (is (= 6 (count (:discard (get-hero)))) "Suffered 3 net damage on access and psi loss"))))

(deftest psychic-field-no-access-choice-in-archives
  ;; Regression test for issue #1965 (Psychic Field showing up as an option to access / trigger in archives
  (do-game
    (new-game (default-contestant [(qty "Psychic Field" 2) (qty "Shock!" 2) (qty "Clone Retirement" 2)])
              (default-hero))
    (trash-from-hand state :contestant "Psychic Field")
    (trash-from-hand state :contestant "Shock!")
    (trash-from-hand state :contestant "Clone Retirement")
    (take-credits state :contestant)
    ;; Runner run on archives to trigger access choice
    (run-empty-server state :archives)
    (is (not-any? #{"Psychic Field"} (get-in @state [:hero :prompt :choices]))
        "Psychic Field is not a choice to access in Archives")))

(deftest psychic-field-neutralize-all-threats
  ;; Psychic Field - Interaction with Neutralize All Threats and Hostile Infrastructure, #1208
  (do-game
    (new-game (default-contestant [(qty "Psychic Field" 3) (qty "Hostile Infrastructure" 3)])
              (default-hero [(qty "Neutralize All Threats" 1) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Psychic Field" "New remote")
    (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
    (core/rez state :contestant (get-content state :remote2 0))
    (take-credits state :contestant)
    (play-from-hand state :hero "Neutralize All Threats")
    (run-empty-server state :remote1)
    (prompt-choice :contestant "0 [Credits]")
    (prompt-choice :hero "1 [Credits]")
    (is (not (get-content state :remote1)) "Psychic Field trashed by Neutralize All Threats")
    (is (= "Flatline" (:reason @state)) "Win condition reports flatline")))

(deftest public-support
  ;; Public support scoring and trashing
  ;; TODO could also test for NOT triggering "when scored" events
  (do-game
    (new-game (default-contestant [(qty "Public Support" 2)])
              (default-hero))
    ;; Corp turn 1, install and rez public supports
    (play-from-hand state :contestant "Public Support" "New remote")
    (play-from-hand state :contestant "Public Support" "New remote")
    (let [publics1 (get-content state :remote1 0)
          publics2 (get-content state :remote2 0)]
      (core/rez state :contestant (refresh publics1))
      (core/rez state :contestant (refresh publics2))
      (take-credits state :contestant)

      ;; Runner turn 1, creds
      (is (= 2 (:credit (get-contestant))))
      (is (= 3 (get-counters (refresh publics1) :power)))
      (take-credits state :hero)

      ;; Corp turn 2, creds, check if supports are ticking
      (is (= 2 (get-counters (refresh publics1) :power)))
      (is (= 0 (:agenda-point (get-contestant))))
      (is (nil? (:agendapoints (refresh publics1))))
      (take-credits state :contestant)

      ;; Runner turn 2, run and trash publics2
      (run-empty-server state "Server 2")
      (prompt-choice :hero "Yes") ; pay to trash
      (is (= 5 (:credit (get-hero))))
      (take-credits state :hero)

      ;; Corp turn 3, check how publics1 is doing
      (is (= 1 (get-counters (refresh publics1) :power)))
      (is (= 0 (:agenda-point (get-contestant))))
      (take-credits state :contestant)

      ;; Runner turn 3, boring
      (take-credits state :hero)

      ;; Corp turn 4, check the delicious agenda points
      (let [scored-pub (get-in @state [:contestant :scored 0])]
        (is (= 1 (:agenda-point (get-contestant))) "Gained 1 agenda point")
        (is (= "Public Support" (:title scored-pub)))
        (is (= 1 (:agendapoints scored-pub)))))))

(deftest quarantine-system
  ;; Forfeit agenda to rez up to 3 ICE with 2 credit discount per agenda point
  (do-game
    (new-game
      (default-contestant [(qty "Chiyashi" 3) (qty "Quarantine System" 1) (qty "Project Beale" 1)])
      (default-hero))
    (core/gain state :contestant :credit 100)
    (core/gain state :contestant :click 100)
    (play-from-hand state :contestant "Chiyashi" "HQ")
    (play-from-hand state :contestant "Chiyashi" "HQ")
    (play-from-hand state :contestant "Chiyashi" "HQ")
    (play-from-hand state :contestant "Quarantine System" "New remote")
    (play-from-hand state :contestant "Project Beale" "New remote")
    (is (= 102 (:credit (get-contestant))) "Corp has 102 creds")
    (let [ch1 (get-ice state :hq 0)
          ch2 (get-ice state :hq 1)
          ch3 (get-ice state :hq 2)
          qs (get-content state :remote1 0)
          beale (get-content state :remote2 0)]
      (core/rez state :contestant qs)
      (card-ability state :contestant qs 0)
      (is (empty? (:prompt (get-contestant))) "No prompt to rez ICE")
      (score-agenda state :contestant beale)
      ; 1 on rez
      (is (= 101 (:credit (get-contestant))) "Corp has 101 creds")
      (card-ability state :contestant qs 0)
      (prompt-select :contestant (get-in (get-contestant) [:scored 0]))
      (prompt-select :contestant ch1)
      (prompt-select :contestant ch2)
      (prompt-select :contestant ch3)
      ; pay 8 per Chiyashi - 24 total
      (is (= 77 (:credit (get-contestant))) "Corp has 77 creds")
      (is (empty? (:prompt (get-contestant))) "No prompt to rez ICE"))))

(deftest reality-threedee
  ;; Reality Threedee - Take 1 bad pub on rez; gain 1c at turn start (2c if Runner tagged)
  (do-game
    (new-game (default-contestant [(qty "Reality Threedee" 1)])
              (default-hero))
    (play-from-hand state :contestant "Reality Threedee" "New remote")
    (let [r3d (get-content state :remote1 0)]
      (core/rez state :contestant r3d)
      (is (= 1 (:bad-publicity (get-contestant))) "Took 1 bad pub on rez")
      (take-credits state :contestant)
      (take-credits state :hero)
      (is (= 8 (:credit (get-contestant))) "Gained 1 credit")
      (take-credits state :contestant)
      (core/gain state :hero :tag 1)
      (take-credits state :hero)
      (is (= 13 (:credit (get-contestant))) "Gained 2 credits because Runner is tagged"))))

(deftest reconstruction-contract
  ;; Reconstruction Contract - place advancement token when hero takes meat damage
  (do-game
    (new-game (default-contestant [(qty "Reconstruction Contract" 1) (qty "Scorched Earth" 1) (qty "Pup" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Imp" 3)]))
    (core/gain state :hero :tag 1)
    (core/gain state :contestant :credit 5)
    (starting-hand state :hero ["Sure Gamble" "Sure Gamble" "Sure Gamble" "Imp" "Imp"])
    (play-from-hand state :contestant "Reconstruction Contract" "New remote")
    (let [rc (get-content state :remote1 0)]
      (core/rez state :contestant (refresh rc))
      (play-from-hand state :contestant "Scorched Earth")
      (is (= 4 (count (:discard (get-hero)))))
      (is (= 1 (:advance-counter (refresh rc))) "Reconstruction Contract has 1 advancement token")
      (starting-hand state :hero ["Imp" "Imp"])
      (play-from-hand state :contestant "Pup" "HQ")
      (core/rez state :contestant (get-ice state :hq 0))
      (card-subroutine state :contestant (get-ice state :hq 0) 0)
      (is (= 5 (count (:discard (get-hero)))))
      (is (= 1 (:advance-counter (refresh rc))) "Reconstruction Contract doesn't get advancement token for net damage"))))

(deftest reversed-accounts
  ;; Reversed Accounts - Trash to make Runner lose 4 credits per advancement
  (do-game
    (new-game (default-contestant [(qty "Reversed Accounts" 1)])
              (default-hero))
    (play-from-hand state :contestant "Reversed Accounts" "New remote")
    (let [rev (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh rev)})
      (core/advance state :contestant {:card (refresh rev)})
      (take-credits state :contestant)
      (play-from-hand state :hero "Sure Gamble")
      (play-from-hand state :hero "Sure Gamble")
      (play-from-hand state :hero "Sure Gamble")
      (take-credits state :hero)
      (is (= 18 (:credit (get-hero))))
      (core/advance state :contestant {:card (refresh rev)})
      (core/advance state :contestant {:card (refresh rev)})
      (is (= 4 (:advance-counter (refresh rev))))
      (core/rez state :contestant (refresh rev))
      (card-ability state :contestant rev 0)
      (is (= 1 (count (:discard (get-contestant)))) "Reversed Accounts trashed")
      (is (= 2 (:credit (get-hero))) "Runner lost 16 credits"))))

(deftest ronald-five
  ;; Ronald Five - Runner loses a click every time they trash a Corp card
  (do-game
    (new-game (default-contestant [(qty "Ronald Five" 1) (qty "Melange Mining Corp." 1)])
              (default-hero))
    (play-from-hand state :contestant "Ronald Five" "New remote")
    (play-from-hand state :contestant "Melange Mining Corp." "New remote")
    (take-credits state :contestant)
    (core/rez state :contestant (get-content state :remote1 0))
    (run-empty-server state :remote2)
    (prompt-choice :hero "Yes") ; trash MMC
    (is (= 2 (:click (get-hero))) "Lost 1 click")
    (run-empty-server state :remote1)
    (prompt-choice :hero "Yes") ; trash Ronald Five
    (is (= 0 (:click (get-hero))) "Lost 1 click")))

(deftest ronin
  ;; Ronin - Click-trash to do 3 net damage when it has 4 or more advancements
  (do-game
    (new-game (default-contestant [(qty "Ronin" 1) (qty "Mushin No Shin" 1)])
              (default-hero))
    (play-from-hand state :contestant "Mushin No Shin")
    (prompt-select :contestant (find-card "Ronin" (:hand (get-contestant))))
    (let [ron (get-content state :remote1 0)]
      (is (= 3 (:advance-counter (refresh ron))))
      (core/rez state :contestant (refresh ron))
      (card-ability state :contestant ron 0)
      (is (= 3 (count (:hand (get-hero))))
          "Ronin ability didn't fire with only 3 advancements")
      (take-credits state :contestant)
      (take-credits state :hero)
      (core/advance state :contestant {:card (refresh ron)})
      (is (= 4 (:advance-counter (refresh ron))))
      (card-ability state :contestant ron 0)
      (is (= 3 (count (:discard (get-hero)))) "Ronin did 3 net damage")
      (is (= 2 (count (:discard (get-contestant)))) "Ronin trashed"))))

(deftest sandburg
  ;; Sandburg - +1 strength to all ICE for every 5c when Corp has over 10c
  (do-game
    (new-game (default-contestant [(qty "Sandburg" 1) (qty "Ice Wall" 2) (qty "Hedge Fund" 3)])
              (default-hero))
    (core/gain state :contestant :click 3 :credit 3)
    (play-from-hand state :contestant "Sandburg" "New remote")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (let [sb (get-content state :remote1 0)
          iwall1 (get-ice state :hq 0)
          iwall2 (get-ice state :rd 0)]
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
      (prompt-choice :hero "Yes")
      (is (= 1 (:current-strength (refresh iwall1))) "Strength back to default")
      (is (= 1 (:current-strength (refresh iwall2))) "Strength back to default"))))

(deftest sealed-vault
  ;; Sealed Vault - Store credits for 1c, retrieve credits by trashing or spending click
  (do-game
    (new-game (default-contestant [(qty "Sealed Vault" 1) (qty "Hedge Fund" 1)])
              (default-hero))
    (play-from-hand state :contestant "Sealed Vault" "New remote")
    (play-from-hand state :contestant "Hedge Fund")
    (let [sv (get-content state :remote1 0)]
      (core/rez state :contestant sv)
      (card-ability state :contestant sv 0)
      (prompt-choice :contestant 8)
      (is (= 8 (get-counters (refresh sv) :credit)) "8 credits stored on Sealed Vault")
      (is (= 0 (:credit (get-contestant))))
      (card-ability state :contestant sv 1)
      (prompt-choice :contestant 8)
      (is (= 0 (get-counters (refresh sv) :credit)) "Credits removed from Sealed Vault")
      (is (= 8 (:credit (get-contestant))))
      (is (= 0 (:click (get-contestant))) "Spent a click")
      (card-ability state :contestant sv 0)
      (prompt-choice :contestant 7)
      (is (= 7 (get-counters (refresh sv) :credit)) "7 credits stored on Sealed Vault")
      (is (= 0 (:credit (get-contestant))))
      (card-ability state :contestant sv 2)
      (prompt-choice :contestant 7)
      (is (= 7 (:credit (get-contestant))))
      (is (= 2 (count (:discard (get-contestant)))) "Sealed Vault trashed"))))

(deftest server-diagnostics
  ;; Server Diagnostics - Gain 2c when turn begins; trashed when ICE is installed
  (do-game
    (new-game (default-contestant [(qty "Server Diagnostics" 1) (qty "Pup" 1)
                             (qty "Launch Campaign" 1)])
              (default-hero))
    (play-from-hand state :contestant "Server Diagnostics" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (is (= 1 (count (get-content state :remote1))) "Non-ICE install didn't trash Serv Diag")
    (take-credits state :contestant)
    (take-credits state :hero)
    (is (= 5 (:credit (get-contestant))) "Gained 2c at start of turn")
    (play-from-hand state :contestant "Pup" "HQ")
    (is (= 1 (count (:discard (get-contestant)))) "Server Diagnostics trashed by ICE install")))

(deftest shock
  ;; do 1 net damage on access
  (do-game
    (new-game (default-contestant [(qty "Shock!" 3)])
              (default-hero))
    (trash-from-hand state :contestant "Shock!")
    (play-from-hand state :contestant "Shock!" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (is (= 2 (count (:hand (get-hero)))) "Runner took 1 net damage")
    (run-empty-server state "Archives")
    (is (= 1 (count (:hand (get-hero)))) "Runner took 1 net damage")))

(deftest shock-chairman-hiro
  ;; issue #2319 - ensure :access flag is cleared on run end
  (do-game
    (new-game (default-contestant [(qty "Shock!" 3) (qty "Chairman Hiro" 1)])
              (default-hero))
    (trash-from-hand state :contestant "Shock!")
    (play-from-hand state :contestant "Shock!" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Archives")
    (is (= 2 (count (:hand (get-hero)))) "Runner took 1 net damage")
    (is (not (:run @state)) "Run is complete")
    (trash-from-hand state :contestant "Chairman Hiro")
    (is (= 2 (count (:discard (get-contestant)))) "Hiro and Shock still in archives")
    (is (= 0 (count (:scored (get-hero)))) "Hiro not scored by Runner")))

(deftest snare
  ;; pay 4 on access, and do 3 net damage and give 1 tag
  (do-game
    (new-game (default-contestant [(qty "Snare!" 3)])
              (default-hero))
    (play-from-hand state :contestant "Snare!" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :hero :prompt first :prompt-type))
        "Runner has prompt to wait for Snare!")
    (prompt-choice :contestant "Yes")
    (is (= 3 (:credit (get-contestant))) "Corp had 7 and paid 4 for Snare! 1 left")
    (is (= 1 (:tag (get-hero))) "Runner has 1 tag")
    (is (= 0 (count (:hand (get-hero)))) "Runner took 3 net damage")
    ))

(deftest snare-cant-afford
  ;; Snare! - Can't afford
  (do-game
    (new-game (default-contestant [(qty "Snare!" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
    (play-from-hand state :contestant "Snare!" "New remote")
    (take-credits state :contestant)
    (core/lose state :contestant :credit 7)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :hero :prompt first :prompt-type))
        "Runner has prompt to wait for Snare!")
    (prompt-choice :contestant "Yes")
    (is (= 0 (:tag (get-hero))) "Runner has 0 tags")
    (prompt-choice :hero "Yes")
    (is (empty? (:prompt (get-hero))) "Runner waiting prompt is cleared")
    (is (= 0 (count (:discard (get-hero)))) "Runner took no damage")))

(deftest snare-dedicated-response-team
  ;; Snare! - with Dedicated Response Team
  (do-game
    (new-game (default-contestant [(qty "Snare!" 1) (qty "Dedicated Response Team" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
    (play-from-hand state :contestant "Snare!" "New remote")
    (play-from-hand state :contestant "Dedicated Response Team" "New remote")
    (core/gain state :contestant :click 1 :credit 4)
    (let [drt (get-content state :remote2 0)]
      (take-credits state :contestant)
      (run-on state "Server 1")
      (core/rez state :contestant drt)
      (run-successful state)
      (is (= :waiting (-> @state :hero :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :contestant "Yes")
      (is (= 1 (:tag (get-hero))) "Runner has 1 tag")
      (prompt-choice :hero "Yes")
      (is (= 5 (count (:discard (get-hero)))) "Runner took 5 damage"))))

(deftest space-camp-archives
  ;; Space Camp - bugged interaction from Archives. Issue #1929.
  (do-game
    (new-game (default-contestant [(qty "Space Camp" 1) (qty "News Team" 1) (qty "Breaking News" 1)])
              (default-hero))
    (trash-from-hand state :contestant "Space Camp")
    (trash-from-hand state :contestant "News Team")
    (play-from-hand state :contestant "Breaking News" "New remote")
    (take-credits state :contestant)
    (run-empty-server state :archives)
    (prompt-choice :hero "News Team")
    (prompt-choice :hero "Take 2 tags")
    (prompt-choice :hero "Space Camp")
    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (get-content state :remote1 0))
    (is (= 1 (:advance-counter (get-content state :remote1 0))) "Agenda advanced once from Space Camp")
    (is (= 2 (:tag (get-hero))) "Runner has 2 tags")
    (is (not (:run @state)) "Run completed")))

(deftest student-loans
  ;; Student Loans - costs Runner 2c extra to play event if already same one in discard
  (do-game
    (new-game (default-contestant [(qty "Student Loans" 1) (qty "Hedge Fund" 2)])
              (default-hero))
    (core/gain state :contestant :credit 2)
    (play-from-hand state :contestant "Student Loans" "New remote")
    (core/rez state :contestant (get-content state :remote1 0))
    (is (= 5 (:credit (get-contestant))) "Corp has 5c")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 9 (:credit (get-contestant))) "Corp has 9c - no penalty from Student Loans")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 13 (:credit (get-contestant))) "Corp has 13c - no penalty from Student Loans")
    (take-credits state :contestant)
    (play-from-hand state :hero "Sure Gamble")
    (is (= 9 (:credit (get-hero))) "1st Gamble played for 4c")
    (play-from-hand state :hero "Sure Gamble")
    (is (= 11 (:credit (get-hero))) "2nd Gamble played for 2c")
    (play-from-hand state :hero "Sure Gamble")
    (is (= 13 (:credit (get-hero))) "3rd Gamble played for 2c")))

(deftest sundew
  ;; Sundew
  (do-game
    (new-game (default-contestant [(qty "Sundew" 1)])
              (default-hero))
    (play-from-hand state :contestant "Sundew" "New remote")
    (let [sund (get-content state :remote1 0)]
      (core/rez state :contestant sund)
      (take-credits state :contestant 2)
      (is (= 5 (:credit (get-contestant))) "Cost 2cr to rez")
      ;; spend a click not on a run
      (take-credits state :hero)
      (is (= 7 (:credit (get-contestant))) "Corp gained 2cr from Sundew")
      (take-credits state :contestant)
      (run-on state "Server 1")
      (is (= 10 (:credit (get-contestant))) "Corp did not gain 2cr from run on Sundew")
      (is (= 3 (:click (get-hero))) "Runner spent 1 click to start run"))))

;(deftest sundew-dirty-laundry
;  "Sundew - Dirty Laundry"
;  (do-game
;    (new-game (default-contestant [(qty "Sundew" 1)])
;              (default-hero [(qty "Dirty Laundry" 1)]))
;    (play-from-hand state :contestant "Sundew" "New remote")
;    (let [sund (first (get-in @state [:contestant :servers :remote1 :content]))]
;      (core/rez state :contestant sund)
;      (take-credits state :contestant 2)
;      (is (= 5 (:credit (get-contestant))) "Cost 2cr to rez")
;      ; spend a click on a run through a card, not through click-run.
;      (play-run-event state (find-card "Dirty Laundry" (:hand (get-hero))) :remote1)
;      (is (= 5 (:credit (get-contestant))) "Corp did not gain 2cr from run on Sundew"))))

(deftest team-sponsorship-hq
  ;; Team Sponsorship - Install from HQ
  (do-game
    (new-game (default-contestant [(qty "Domestic Sleepers" 1)
                             (qty "Team Sponsorship" 1)
                             (qty "Adonis Campaign" 1)])
              (default-hero))
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

(deftest team-sponsorship-archives
  ;; Team Sponsorship - Install from Archives
  (do-game
    (new-game (default-contestant [(qty "Domestic Sleepers" 1)
                             (qty "Team Sponsorship" 1)
                             (qty "Adonis Campaign" 1)])
              (default-hero))
    (play-from-hand state :contestant "Team Sponsorship" "New remote")
    (play-from-hand state :contestant "Domestic Sleepers" "New remote")
    (trash-from-hand state :contestant "Adonis Campaign")
    (let [ag1 (get-content state :remote2 0)
          tsp (get-content state :remote1 0)]
      (core/rez state :contestant tsp)
      (score-agenda state :contestant ag1)
      (prompt-select :contestant (find-card "Adonis Campaign" (:discard (get-contestant))))
      (prompt-choice :contestant "New remote")
      (is (= "Adonis Campaign" (:title (get-content state :remote3 0)))
          "Adonis installed by Team Sponsorship")
      (is (nil? (find-card "Adonis Campaign" (:discard (get-contestant)))) "No Adonis in discard"))))

(deftest team-sponsorship-multiple
  ;; Team Sponsorship - Multiple installed
  (do-game
    (new-game (default-contestant [(qty "Domestic Sleepers" 1)
                             (qty "Team Sponsorship" 2)
                             (qty "Adonis Campaign" 2)])
              (default-hero))
    (play-from-hand state :contestant "Team Sponsorship" "New remote")
    (play-from-hand state :contestant "Team Sponsorship" "New remote")
    (play-from-hand state :contestant "Domestic Sleepers" "New remote")
    (trash-from-hand state :contestant "Adonis Campaign")
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

(deftest team-sponsorship-one-window
  ;; Team Sponsorship - Score 5 points in one window
  (do-game
    (new-game (default-contestant [(qty "AstroScript Pilot Program" 3)
                             (qty "Team Sponsorship" 1)
                             (qty "Breaking News" 1)
                             (qty "SanSan City Grid" 1)])
              (default-hero))
    (play-from-hand state :contestant "SanSan City Grid" "New remote")
    (core/gain state :contestant :credit 100 :click 5)
    (core/rez state :contestant (get-content state :remote1 0))
    (play-from-hand state :contestant "AstroScript Pilot Program" "New remote")
    (score-agenda state :contestant (get-content state :remote2 0))
    (play-from-hand state :contestant "AstroScript Pilot Program" "Server 1")
    (play-from-hand state :contestant "Team Sponsorship" "New remote")
    (core/rez state :contestant (get-content state :remote3 0))
    (score-agenda state :contestant (get-content state :remote1 1))
    (prompt-select :contestant (find-card "AstroScript Pilot Program" (:hand (get-contestant))))
    (is (= 0 (get-counters (second (:scored (get-contestant))) :agenda)) "AstroScript not resolved yet")
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
    (is (= 7 (:agenda-point (get-contestant))) "Scored 5 points in one turn")))

(deftest the-board
  ;; The Board - Modify everything in the score area (regression test for #1938)
  (do-game
    (new-game (default-contestant [(qty "The Board" 1)
                             (qty "News Team" 1)
                             (qty "Firmware Updates" 2)])
              (default-hero [(qty "Artist Colony" 3)
                               (qty "Fan Site" 3)]))
    (play-from-hand state :contestant "The Board" "New remote")
    (play-from-hand state :contestant "News Team" "New remote")
    (play-from-hand state :contestant "Firmware Updates" "New remote")
    (take-credits state :contestant)

    (play-from-hand state :hero "Artist Colony")
    (play-from-hand state :hero "Fan Site")
    (take-credits state :hero)

    (play-from-hand state :contestant "Firmware Updates" "New remote")
    (score-agenda state :contestant (get-content state :remote4 0))
    (is (= 1 (count (:scored (get-hero)))) "Fan Site added to Runner score area")
    (is (= 0 (:agenda-point (get-hero))) "Runner has 0 agenda points")

    (take-credits state :contestant)

    (run-empty-server state :remote3)
    (prompt-choice :hero "Steal")
    (is (= 2 (count (:scored (get-hero)))) "Firmware Updates stolen")
    (is (= 1 (:agenda-point (get-hero))) "Runner has 1 agenda point")

    (core/rez state :contestant (get-content state :remote1 0))
    (is (= -1 (:agenda-point (get-hero))) "Runner has -1 agenda points")

    (run-empty-server state :remote2)
    (prompt-choice :hero "Add News Team to score area")
    (is (= 3 (count (:scored (get-hero)))) "News Team added to Runner score area")
    (is (= -3 (:agenda-point (get-hero))) "Runner has -3 agenda points")

    (card-ability state :hero (get-resource state 0) 0)
    (prompt-choice :hero (->> @state :hero :prompt first :choices first))
    (prompt-select :hero (first (:scored (get-hero))))
    (is (= 2 (count (:scored (get-hero)))) "Fan Site removed from Runner score area")
    (is (= -2 (:agenda-point (get-hero))) "Runner has -2 agenda points")

    (run-empty-server state :remote1)
    (prompt-choice :hero "Yes")
    (is (= 3 (count (:scored (get-hero)))) "The Board added to Runner score area")
    (is (= 2 (:agenda-point (get-hero))) "Runner has 2 agenda points")))

(deftest the-root
  ;; The Root - recurring credits refill at Step 1.2
  (do-game
    (new-game (make-deck "Blue Sun: Powering the Future" [(qty "The Root" 1)])
              (default-hero))
    (play-from-hand state :contestant "The Root" "New remote")
    (core/gain state :contestant :credit 6)
    (let [root (get-content state :remote1 0)]
      (core/rez state :contestant root)
      (card-ability state :contestant (refresh root) 0)
      (is (= 2 (:rec-counter (refresh root))) "Took 1 credit from The Root")
       (is (= 6 (:credit (get-contestant))) "Corp took Root credit into credit pool")
      (take-credits state :contestant)
      (take-credits state :hero)
      ; we expect Step 1.2 to have triggered because of Blue Sun
      (is (:contestant-phase-12 @state) "Corp is in Step 1.2")
      (is (= 3 (:rec-counter (refresh root))) "Recurring credits were refilled before Step 1.2 window"))))

(deftest toshiyuki-sakai
  ;; Toshiyuki Sakai - Swap with an asset/agenda from HQ; Runner can choose to access new card or not
  (do-game
    (new-game (default-contestant [(qty "Toshiyuki Sakai" 1) (qty "Project Junebug" 1) (qty "Hedge Fund" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
    (play-from-hand state :contestant "Toshiyuki Sakai" "New remote")
    (let [toshi (get-content state :remote1 0)]
      (core/advance state :contestant {:card (refresh toshi)})
      (core/advance state :contestant {:card (refresh toshi)})
      (take-credits state :contestant)
      (is (= 2 (:advance-counter (refresh toshi))) "Toshiyuki has 2 advancements")
      (run-empty-server state "Server 1")
      (is (= :waiting (-> @state :hero :prompt first :prompt-type))
          "Runner has prompt to wait for Toshiyuki")
      (prompt-choice :contestant "Yes") ; choose to do a swap
      (prompt-select :contestant (find-card "Hedge Fund" (:hand (get-contestant))))
      (is (= (refresh toshi) (get-content state :remote1 0)) "Toshiyuki still in remote; can't target an operation in hand")
      (prompt-select :contestant (find-card "Project Junebug" (:hand (get-contestant))))
      (let [june (get-content state :remote1 0)]
        (is (= "Project Junebug" (:title (refresh june))) "Project Junebug swapped into Server 1")
        (is (= 2 (:advance-counter (refresh june))) "Project Junebug has 2 advancements")
        (prompt-choice :hero "Yes") ; choose to access new card
        (prompt-choice :contestant "Yes") ; pay 1c to fire Junebug
        (is (= 4 (count (:discard (get-hero)))) "Runner took 4 net damage")))))

(deftest turtlebacks
  ;; Turtlebacks - Gain 1 credit for every new server created
  (do-game
    (new-game (default-contestant [(qty "Turtlebacks" 1) (qty "PAD Campaign" 2) (qty "Wraparound" 1)])
              (default-hero))
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
    (new-game (default-contestant [(qty "Urban Renewal" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
    ;; Corp turn 1, install and rez urban renewal
    (play-from-hand state :contestant "Urban Renewal" "New remote")
    (let [ur (get-content state :remote1 0)]
      (core/rez state :contestant (refresh ur))
      (take-credits state :contestant)
      ;; Runner turn 1, creds
      (is (= 3 (get-counters (refresh ur) :power)))
      (take-credits state :hero)

      ;; Corp turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :contestant)

      ;; Runner turn 2
      (is (= 2 (get-counters (refresh ur) :power)))
      (take-credits state :hero)

      ;; Corp turn 3
      (is (= 1 (get-counters (refresh ur) :power)))
      (take-credits state :contestant)

      ;; Runner turn 3
      (is (= 0 (count (:discard (get-contestant)))) "Nothing in Corp trash")
      (is (= 0 (count (:discard (get-hero)))) "Nothing in Runner trash")
      (take-credits state :hero)

      ;; Corp turn 4 - damage fires
      (is (= 1 (count (:discard (get-contestant)))) "Urban Renewal got trashed")
      (is (= 4 (count (:discard (get-hero)))) "Urban Renewal did 4 meat damage"))))

(deftest watchdog
  ;; Watchdog - Reduce rez cost of first ICE per turn by number of Runner tags
  (do-game
    (new-game (default-contestant [(qty "Watchdog" 1) (qty "Architect" 1) (qty "Wraparound" 1)])
              (default-hero))
    (play-from-hand state :contestant "Watchdog" "New remote")
    (play-from-hand state :contestant "Wraparound" "HQ")
    (play-from-hand state :contestant "Architect" "HQ")
    (let [wd (get-content state :remote1 0)
          arch (get-ice state :hq 1)
          wrap (get-ice state :hq 0)]
      (take-credits state :contestant)
      (is (= 4 (:credit (get-contestant))))
      (core/gain state :hero :tag 2)
      (run-on state "HQ")
      (core/rez state :contestant wd)
      (core/rez state :contestant arch)
      (is (= 2 (:credit (get-contestant))) "Only 2 credits to rez Architect")
      (core/rez state :contestant wrap)
      (is (= 0 (:credit (get-contestant))) "No rez discount on Wraparound"))))

(deftest whampoa-reclamation
  ;; Whampoa Reclamation: Enable trashing a card from HQ to place a card in Archives on the bottom of R+D
  (do-game
    (new-game (default-contestant [(qty "Whampoa Reclamation" 3) (qty "PAD Campaign" 2) (qty "Global Food Initiative" 3)])
              (default-hero))
    (play-from-hand state :contestant "Whampoa Reclamation" "New remote")
    (let [wr (get-content state :remote1 0)]
      (core/draw state :contestant)
      (take-credits state :contestant)
      (core/rez state :contestant wr)
      (let [gfi (find-card "Global Food Initiative" (:hand (get-contestant)))]
        (core/trash state :hero gfi)
        (card-ability state :contestant wr 0)
        (prompt-choice :contestant "Global Food Initiative") ;; into archives
        (prompt-select :contestant (first (:discard (get-contestant)))) ;; into R&D
        (is (= 0 (count (:discard (get-contestant)))) "Only card in discard placed in bottom of R&D")
        (is (= "Global Food Initiative" (:title (last (:deck (get-contestant))))) "GFI last card in deck")))))
