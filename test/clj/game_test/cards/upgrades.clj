(ns game-test.cards.regions
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "regions"))

(deftest amazon-industrial-zone
  ;; Amazon Industrial Zone - Immediately reveal Character placed over its locale at 3 credit discount
  (do-game
    (new-game (default-contestant ["Spiderweb" "Amazon Industrial Zone"])
              (default-challenger))
    (take-credits state :contestant 1)
    (play-from-hand state :contestant "Amazon Industrial Zone" "New party")
    (let [aiz (get-content state :party1 0)]
      (core/reveal state :contestant aiz)
      (is (= 2 (:credit (get-contestant))))
      (play-from-hand state :contestant "Spiderweb" "Locale 1")
      (click-prompt state :contestant "Yes") ; optional ability
      (let [spid (get-character state :party1 0)]
        (is (:revealed (refresh spid)) "Spiderweb revealed")
        (is (= 1 (:credit (get-contestant))) "Paid only 1 credit to reveal")))))

(deftest arella-salvatore
  ;; Arella Salvatore - when an agenda is scored from this locale, place a card from hq w/ advancement token
  (testing "Place to locale"
    (do-game
      (new-game (default-contestant ["Arella Salvatore" "Bryan Stinson" (qty "TGTBT" 2)])
                (default-challenger))
      (play-from-hand state :contestant "Arella Salvatore" "New party")
      (play-from-hand state :contestant "TGTBT" "Locale 1")
      (play-from-hand state :contestant "TGTBT" "New party")
      (let [arella (get-content state :party1 0)
            same-tg (get-content state :party1 1)
            diff-tg (get-content state :party2 0)]
        (core/reveal state :contestant arella)
        (score-agenda state :contestant (refresh diff-tg))
        (is (empty? (get-in @state [:contestant :prompt])) "Arella not triggered for different party score")
        (is (= 1 (count (get-scored state :contestant))) "1 Agenda scored")
        (score-agenda state :contestant (refresh same-tg))
        (click-card state :contestant (find-card "Bryan Stinson" (:hand (get-contestant))))
        (click-prompt state :contestant "New party")
        (is (= 2 (count (get-scored state :contestant))) "2 Agendas scored")
        (is (= 1 (count (get-content state :party3))) "Bryan placed in new party")
        (is (= 1 (get-counters (get-content state :party3 0) :advancement)) "Bryan has 1 advancement counter"))))
  (testing "Interaction w/ other on-scored triggers"
    (do-game
      (new-game (make-deck "Sportsmetal: Go Big or Go Home" ["Arella Salvatore" "Domestic Sleepers" "Project Vitruvius" "Hedge Fund"])
                (default-challenger))
      (starting-hand state :contestant ["Arella Salvatore" "Domestic Sleepers"])
      (play-from-hand state :contestant "Arella Salvatore" "New party")
      (play-from-hand state :contestant "Domestic Sleepers" "Locale 1")
      (let [arella (get-content state :party1 0)
            domest (get-content state :party1 1)]
        (core/reveal state :contestant arella)
        (score-agenda state :contestant (refresh domest))
        ;; Simultaneous prompt: Sportsmetal automatically triggers, as Arella is silent because there are no placeable cards in HQ
        (click-prompt state :contestant "2 cards")
        ;; Arella is no longer silent and now triggers
        (click-card state :contestant (find-card "Project Vitruvius" (:hand (get-contestant))))
        (click-prompt state :contestant "Locale 1")
        (is (= 2 (count (get-content state :party1))) "Agenda placed in locale 1")
        (is (= 1 (get-counters (get-content state :party1 1) :advancement)) "Agenda has 1 advancement counter"))))
  (testing "No cost"
    (do-game
      (new-game (default-contestant ["Arella Salvatore" "TGTBT" (qty "Ice Wall" 2)])
                (default-challenger))
      (core/gain state :contestant :click 5)
      (play-from-hand state :contestant "Arella Salvatore" "New party")
      (play-from-hand state :contestant "TGTBT" "Locale 1")
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (is (= 1 (count (get-character state :hq))) "One character on hq")
      (let [arella (get-content state :party1 0)
            tg (get-content state :party1 1)]
        (core/reveal state :contestant arella)
        (score-agenda state :contestant (refresh tg))
        (click-card state :contestant (find-card "Ice Wall" (:hand (get-contestant))))
        (click-prompt state :contestant "HQ")
        (is (= 2 (count (get-character state :hq))) "Two character on hq")
        (is (= 1 (get-counters (get-character state :hq 1) :advancement)) "Ice Wall has 1 counter")))))

(deftest ash-2x3zb9cy
  ;; Ash 2X3ZB9CY
  (do-game
    (new-game (default-contestant ["Ash 2X3ZB9CY" (qty "Ice Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant ["Ash 2X3ZB9CY" "Ice Wall"])
    (play-from-hand state :contestant "Ash 2X3ZB9CY" "HQ")
    (take-credits state :contestant)
    (let [ash (get-content state :hq 0)]
      (core/reveal state :contestant ash)
      (run-empty-locale state "HQ")
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (is (= "Ash 2X3ZB9CY" (-> (get-challenger) :prompt first :card :title)) "Should access Ash")
      (click-prompt state :challenger "Pay 3 [Credits] to discard")
      (is (not (:run @state)) "Accessing Ash then ends the run"))))

(deftest ben-musashi
  ;; Ben Musashi
  (testing "Basic test - pay 2 net damage to steal from this locale"
    (do-game
      (new-game (default-contestant ["Ben Musashi" "House of Knives"])
                (default-challenger))
      (play-from-hand state :contestant "Ben Musashi" "New party")
      (play-from-hand state :contestant "House of Knives" "Locale 1")
      (take-credits state :contestant 1)
      (let [bm (get-content state :party1 0)
            hok (get-content state :party1 1)]
        (core/reveal state :contestant bm)
        (run-empty-locale state "Locale 1")
        ;; challenger now chooses which to access.
        (click-card state :challenger hok)
        ;; prompt should be asking for the 2 net damage cost
        (is (= "House of Knives" (:title (:card (first (:prompt (get-challenger))))))
            "Prompt to pay 2 net damage")
        (click-prompt state :challenger "No action")
        (is (= 5 (:credit (get-challenger))) "Challenger did not pay 2 net damage")
        (is (zero? (count (:scored (get-challenger)))) "No scored agendas")
        (click-card state :challenger bm)
        (click-prompt state :challenger "No action")
        (run-empty-locale state "Locale 1")
        (click-card state :challenger hok)
        (click-prompt state :challenger "Pay 2 net damage to steal")
        (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 net")
        (is (= 1 (count (:scored (get-challenger)))) "1 scored agenda"))))
  (testing "on R&D access"
    (do-game
      (new-game (default-contestant ["Ben Musashi" "House of Knives"])
                (default-challenger))
      (starting-hand state :contestant ["Ben Musashi"])
      (play-from-hand state :contestant "Ben Musashi" "R&D")
      (take-credits state :contestant)
      (let [bm (get-content state :rd 0)]
        (core/reveal state :contestant bm)
        (run-empty-locale state "R&D")
        ;; challenger now chooses which to access.
        (click-prompt state :challenger "Card from deck")
        ;; prompt should be asking for the 2 net damage cost
        (is (= "House of Knives" (:title (:card (first (:prompt (get-challenger))))))
            "Prompt to pay 2 net damage")
        (click-prompt state :challenger "No action")
        (is (= 5 (:credit (get-challenger))) "Challenger did not pay 2 net damage")
        (is (zero? (count (:scored (get-challenger)))) "No scored agendas")
        (click-prompt state :challenger "Ben Musashi")
        (click-prompt state :challenger "No action")
        (run-empty-locale state "R&D")
        (click-prompt state :challenger "Card from deck")
        (click-prompt state :challenger "Pay 2 net damage to steal")
        (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 net")
        (is (= 1 (count (:scored (get-challenger)))) "1 scored agenda"))))
  (testing "pay even when discarded"
    (do-game
      (new-game (default-contestant [(qty "Ben Musashi" 3) (qty "House of Knives" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Ben Musashi" "New party")
      (play-from-hand state :contestant "House of Knives" "Locale 1")
      (take-credits state :contestant 1)
      (core/gain state :challenger :credit 1)
      (let [bm (get-content state :party1 0)
            hok (get-content state :party1 1)]
        (core/reveal state :contestant bm)
        (run-empty-locale state "Locale 1")
        ;; challenger now chooses which to access.
        (click-card state :challenger bm)
        (click-prompt state :challenger "Pay 3 [Credits] to discard") ; pay to discard
        (click-card state :challenger hok)
        ;; should now have prompt to pay 2 net for HoK
        (click-prompt state :challenger "Pay 2 net damage to steal")
        (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 net")
        (is (= 1 (count (:scored (get-challenger)))) "1 scored agenda"))))
  (testing "Check challenger chooses order of payment"
    (do-game
      (new-game (default-contestant ["Ben Musashi" "Obokata Protocol"])
                (default-challenger [(qty "Sure Gamble" 6)]))
      (play-from-hand state :contestant "Ben Musashi" "New party")
      (play-from-hand state :contestant "Obokata Protocol" "Locale 1")
      (take-credits state :contestant)
      (let [bm (get-content state :party1 0)
            op (get-content state :party1 1)]
        (core/reveal state :contestant bm)
        (run-empty-locale state "Locale 1")
        ;; challenger now chooses which to access.
        (click-card state :challenger op)
        ;; prompt should be asking for the net damage costs
        (is (= "Obokata Protocol" (:title (:card (first (:prompt (get-challenger))))))
            "Prompt to pay steal costs")
        (click-prompt state :challenger "Pay to steal")
        (click-prompt state :challenger "2 net damage")
        (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 net damage")
        (is (zero? (count (:scored (get-challenger)))) "No scored agendas")
        (click-prompt state :challenger "4 net damage")
        (is (= 5 (count (:discard (get-challenger)))) "Challenger took 4 net damage")
        (is (= 1 (count (:scored (get-challenger)))) "Scored agenda"))))
  (testing "Check Fetal AI can be stolen, #2586"
    (do-game
      (new-game (default-contestant ["Ben Musashi" "Fetal AI"])
                (default-challenger [(qty "Sure Gamble" 5)]))
      (play-from-hand state :contestant "Ben Musashi" "New party")
      (play-from-hand state :contestant "Fetal AI" "Locale 1")
      (take-credits state :contestant)
      (let [bm (get-content state :party1 0)
            fai (get-content state :party1 1)]
        (core/reveal state :contestant bm)
        (run-empty-locale state "Locale 1")
        ;; challenger now chooses which to access.
        (click-card state :challenger fai)
        ;; prompt should be asking for the net damage costs
        (is (= "Fetal AI" (:title (:card (first (:prompt (get-challenger))))))
            "Prompt to pay steal costs")
        (click-prompt state :challenger "Pay to steal")
        (click-prompt state :challenger "2 [Credits]")
        (is (= 3 (:credit (get-challenger))) "Challenger paid 2 credits")
        (is (zero? (count (:scored (get-challenger)))) "No scored agendas")
        (click-prompt state :challenger "2 net damage")
        (is (= 4 (count (:discard (get-challenger)))) "Challenger took 4 net damage - 2 from Fetal, 2 from Ben")
        (is (= 1 (count (:scored (get-challenger)))) "Scored agenda")))))

(deftest berncharacter-mai
  ;; Berncharacter Mai
  (testing "Basic test - successful and unsuccessful"
    (do-game
      (new-game (default-contestant [(qty "Berncharacter Mai" 3) (qty "Hedge Fund" 3) (qty "Wall of Static" 3)])
                (default-challenger))
      (starting-hand state :contestant ["Berncharacter Mai" "Berncharacter Mai" "Berncharacter Mai"])
      (play-from-hand state :contestant "Berncharacter Mai" "New party")
      (play-from-hand state :contestant "Berncharacter Mai" "New party")
      (play-from-hand state :contestant "Berncharacter Mai" "R&D")
      (core/reveal state :contestant (get-content state :party1 0))
      (take-credits state :contestant)
      (run-empty-locale state :party1)
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (click-prompt state :challenger "Pay 3 [Credits] to discard")
      (is (= 1 (:tag (get-challenger))))
      (is (= 2 (:credit (get-challenger))) "Challenger paid 3cr to discard Berncharacter")
      (core/reveal state :contestant (get-content state :party2 0))
      (core/gain state :challenger :credit 20)
      (run-empty-locale state :party2)
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "10")
      (is (not (get-content state :party2 0)) "Berncharacter auto-discarded from unsuccessful trace")
      (is (not (:run @state)) "Run ended when Berncharacter was discarded from locale")
      (core/reveal state :contestant (get-content state :rd 0))
      (run-empty-locale state :rd)
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "10")
      (is (:card (first (:prompt (get-challenger)))) "Accessing a card from R&D; not showing Berncharacter Mai as possible access")))
  (testing "interaction with Dedicated Response Team"
    (do-game
      (new-game (default-contestant [(qty "Berncharacter Mai" 3) "Dedicated Response Team"])
                (default-challenger))
      (play-from-hand state :contestant "Berncharacter Mai" "New party")
      (play-from-hand state :contestant "Dedicated Response Team" "New party")
      (core/reveal state :contestant (get-content state :party1 0))
      (core/reveal state :contestant (get-content state :party2 0))
      (take-credits state :contestant)
      (run-empty-locale state :party1)
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (click-prompt state :challenger "Pay 3 [Credits] to discard")
      (is (= 1 (:tag (get-challenger))))
      (is (= 2 (:credit (get-challenger))) "Challenger paid 3cr to discard Berncharacter")
      (is (= 2 (count (:discard (get-challenger)))) "Challenger took 1 meat damage"))))

(deftest bio-vault
  ;; Bio Vault - 2 advancement tokens + discard to end the run
  (do-game
    (new-game (default-contestant ["Bio Vault"])
              (default-challenger))
    (play-from-hand state :contestant "Bio Vault" "New party")
    (take-credits state :contestant)
    (let [bv (get-content state :party1 0)]
      (run-on state "Locale 1")
      (core/reveal state :contestant (refresh bv))
      (card-ability state :contestant (refresh bv) 0)
      (is (:run @state) "Bio Vault doesn't fire if less than 2 advancements")
      (run-successful state)
      (click-prompt state :challenger "No action")
      (take-credits state :challenger)
      (advance state (refresh bv) 2)
      (take-credits state :contestant)
      (run-on state "Locale 1")
      (card-ability state :contestant (refresh bv) 0)
      (is (not (:run @state)) "Bio Vault fires with 2 advancement tokens")
      (is (= 1 (count (:discard (get-contestant)))) "Bio Vault discarded"))))

(deftest breaker-bay-grid
  ;; Breaker Bay Grid - Reduce reveal cost of other cards in this locale by 5 credits
  (do-game
   (new-game (default-contestant [(qty "Breaker Bay Grid" 2) "The Root" "Strongbox"])
             (default-challenger))
   (core/gain state :contestant :click 1)
   (play-from-hand state :contestant "Breaker Bay Grid" "New party")
   (play-from-hand state :contestant "The Root" "Locale 1")
   (let [bbg1 (get-content state :party1 0)
         root (get-content state :party1 1)]
     (core/reveal state :contestant bbg1)
     (core/reveal state :contestant root)
     (is (= 4 (:credit (get-contestant))) "Paid only 1 to reveal The Root")
     (play-from-hand state :contestant "Breaker Bay Grid" "R&D")
     (play-from-hand state :contestant "Strongbox" "R&D")
     (let [bbg2 (get-content state :rd 0)
           sbox (get-content state :rd 1)]
       (core/reveal state :contestant bbg2)
       (core/reveal state :contestant sbox)
       (is (= 1 (:credit (get-contestant))) "Paid full 3 credits to reveal Strongbox")))))

(deftest bryan-stinson
  ;; Bryan Stinson - play a transaction from archives and remove from game. Ensure Currents are RFG and not discarded.
  (do-game
   (new-game (default-contestant ["Bryan Stinson" "Death and Taxes"
                            "Paywall Implementation" "Global Food Initiative"
                            "IPO"])
             (default-challenger ["Interdiction"]))
    (discard-from-hand state :contestant "Death and Taxes")
    (play-from-hand state :contestant "Bryan Stinson" "New party")
    (let [bs (get-content state :party1 0)]
      (core/reveal state :contestant (refresh bs))
      (card-ability state :contestant (refresh bs) 0)
      (click-prompt state :contestant (find-card "Death and Taxes" (:discard (get-contestant))))
      (is (find-card "Death and Taxes" (:current (get-contestant))) "Death and Taxes is active Current")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Interdiction")
      (is (find-card "Interdiction" (:current (get-challenger))) "Interdiction is active Current")
      (is (find-card "Death and Taxes" (:rfg (get-contestant))) "Death and Taxes removed from game")
      (is (not= "Death and Taxes" (:title (first (:discard (get-contestant))))) "Death and Taxes not moved to discard")
      (take-credits state :challenger)
      (core/lose state :challenger :credit 3)
      (discard-from-hand state :contestant "Paywall Implementation")
      (card-ability state :contestant (refresh bs) 0)
      (click-prompt state :contestant (find-card "Paywall Implementation" (:discard (get-contestant))))
      (is (find-card "Paywall Implementation" (:current (get-contestant))) "Paywall Implementation is active Current")
      (is (find-card "Interdiction" (:discard (get-challenger))) "Interdiction is discarded")
      (discard-from-hand state :contestant "IPO")
      (take-credits state :contestant)
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :challenger "Steal")
      (is (find-card "Paywall Implementation" (:rfg (get-contestant))) "Paywall Implementation removed from game")
      (is (not= "Paywall Implementation" (:title (first (:discard (get-contestant))))) "Paywall Implementation not moved to discard")
      (take-credits state :challenger)
      (core/lose state :challenger :credit 3)
      (card-ability state :contestant (refresh bs) 0)
      (click-prompt state :contestant (find-card "IPO" (:discard (get-contestant))))
      (is (find-card "IPO" (:rfg (get-contestant))) "IPO is removed from game"))))

(deftest calibration-testing
  ;; Calibration Testing - advanceable / non-advanceable
  (do-game
    (new-game (default-contestant [(qty "Calibration Testing" 2) "Project Junebug" "PAD Campaign"])
              (default-challenger))
    (core/gain state :contestant :credit 10)
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Calibration Testing" "New party")
    (play-from-hand state :contestant "Project Junebug" "Locale 1")
    (let [ct (get-content state :party1 0)
          pj (get-content state :party1 1)]
      (core/reveal state :contestant ct)
      (card-ability state :contestant ct 0)
      (click-card state :contestant pj)
      (is (= 1 (get-counters (refresh pj) :advancement)) "Project Junebug advanced")
      (is (= 1 (count (:discard (get-contestant)))) "Calibration Testing discarded"))
    (play-from-hand state :contestant "Calibration Testing" "New party")
    (play-from-hand state :contestant "PAD Campaign" "Locale 2")
    (let [ct (get-content state :party2 0)
          pad (get-content state :party2 1)]
      (core/reveal state :contestant ct)
      (card-ability state :contestant ct 0)
      (click-card state :contestant pad)
      (is (= 1 (get-counters (refresh pad) :advancement)) "PAD Campaign advanced")
      (is (= 2 (count (:discard (get-contestant)))) "Calibration Testing discarded"))))

(deftest caprcharacter-nisei
  ;; Caprcharacter Nisei - Psi game for ETR after challenger passes last character
  (do-game
   (new-game (default-contestant [(qty "Caprcharacter Nisei" 3) (qty "Quandary" 3)])
             (default-challenger))
   (play-from-hand state :contestant "Caprcharacter Nisei" "New party")
   (take-credits state :contestant)
   (let [caprcharacter (get-content state :party1 0)]
     ;; Check Caprcharacter triggers properly on no character (and revealed)
     (core/reveal state :contestant caprcharacter)
     (run-on state "Locale 1")
     (is (prompt-is-card? state :contestant caprcharacter)
         "Caprcharacter prompt even with no character, once challenger makes run")
     (is (prompt-is-card? state :challenger caprcharacter) "Challenger has Caprcharacter prompt")
     (click-prompt state :contestant "0 [Credits]")
     (click-prompt state :challenger "1 [Credits]")
     (take-credits state :challenger)
     (play-from-hand state :contestant "Quandary" "Locale 1")
     (play-from-hand state :contestant "Quandary" "Locale 1")
     (take-credits state :contestant)
     ;; Check Caprcharacter triggers properly on multiple character
     (run-on state "Locale 1")
     (run-continue state)
     (is (empty? (get-in @state [:contestant :prompt])) "Caprcharacter not trigger on first character")
     (run-continue state) ; Caprcharacter prompt after this
     (is (prompt-is-card? state :contestant caprcharacter)
         "Contestant has Caprcharacter prompt (triggered automatically as challenger passed last character)")
     (is (prompt-is-card? state :challenger caprcharacter) "Challenger has Caprcharacter prompt")
     (click-prompt state :contestant "0 [Credits]")
     (click-prompt state :challenger "1 [Credits]")
     (is (not (:run @state)) "Run ended by Caprcharacter")
     (is (empty? (get-in @state [:contestant :prompt])) "Caprcharacter prompted cleared")
     ;; Check Caprcharacter does not trigger on other locales
     (run-on state "HQ")
     (is (empty? (get-in @state [:contestant :prompt])) "Caprcharacter does not trigger on other locales"))))

(deftest chilo-city-grid
  ;; ChiLo City Grid - Give 1 tag for successful traces during runs on its locale
  (do-game
    (new-game (default-contestant [(qty "Caduceus" 2) "ChiLo City Grid"])
              (default-challenger))
    (play-from-hand state :contestant "ChiLo City Grid" "New party")
    (play-from-hand state :contestant "Caduceus" "Locale 1")
    (take-credits state :contestant)
    (let [chilo (get-content state :party1 0)
          cad (get-character state :party1 0)]
      (run-on state "R&D")
      (core/reveal state :contestant cad)
      (core/reveal state :contestant chilo)
      (card-subroutine state :contestant cad 0)
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (is (= 3 (:credit (get-contestant))) "Trace was successful")
      (is (zero? (:tag (get-challenger))) "No tags given for run on different locale")
      (run-successful state)
      (run-on state "Locale 1")
      (card-subroutine state :contestant cad 0)
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (is (= 6 (:credit (get-contestant))) "Trace was successful")
      (is (= 1 (:tag (get-challenger)))
          "Challenger took 1 tag given from successful trace during run on ChiLo locale"))))

(deftest code-replicator
  ;; Code Replicator - discard to make challenger approach passed (revealed) character again
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3) "Code Replicator"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (core/gain state :contestant :credit 5)
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Code Replicator" "HQ")
    (take-credits state :contestant)
    (run-on state "HQ")
    (is (= 3 (:position (get-in @state [:run]))) "Initial position outermost Ice Wall")
    (let [cr (get-content state :hq 0)
          i1 (get-character state :hq 0)
          i2 (get-character state :hq 1)
          i3 (get-character state :hq 2)]
      (core/reveal state :contestant cr)
      (is (= 5 (:credit (get-contestant))))
      (core/reveal state :contestant i3)
      (run-continue state)
      (is (= 2 (:position (get-in @state [:run]))) "Passed Ice Wall")
      (card-ability state :contestant cr 0)
      (is (= 3 (:position (get-in @state [:run]))) "Challenger approaching previous Ice Wall")
      (is (empty? (get-content state :hq))
          "Code Replicatior discarded from root of HQ"))))

(deftest contestantorate-troubleshooter
  ;; Contestantorate Troubleshooter - Pay X credits and discard to add X strength to a piece of revealed Character
  (do-game
    (new-game (default-contestant [(qty "Quandary" 2) "Contestantorate Troubleshooter"])
              (default-challenger))
    (core/gain state :contestant :credit 5)
    (play-from-hand state :contestant "Contestantorate Troubleshooter" "HQ")
    (play-from-hand state :contestant "Quandary" "HQ")
    (play-from-hand state :contestant "Quandary" "HQ")
    (let [ct (get-content state :hq 0)
          q1 (get-character state :hq 0)
          q2 (get-character state :hq 1)]
      (core/reveal state :contestant q1)
      (is (= 8 (:credit (get-contestant))))
      (core/reveal state :contestant ct)
      (card-ability state :contestant ct 0)
      (click-prompt state :contestant "5")
      (click-card state :contestant q2)
      (is (nil? (:current-strength (refresh q2))) "Outer Quandary unrevealed; can't be targeted")
      (click-card state :contestant q1)
      (is (= 5 (:current-strength (refresh q1))) "Inner Quandary boosted to 5 strength")
      (is (empty? (get-content state :hq))
          "Contestantorate Troubleshooter discarded from root of HQ")
      (take-credits state :contestant)
      (is (zero? (:current-strength (refresh q1)))
          "Inner Quandary back to default 0 strength after turn ends"))))

(deftest crisium-grid
  ;; Crisium Grid
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Crisium Grid" 2)])
                (default-challenger ["Desperado" "Temüjin Contract"]))
      (play-from-hand state :contestant "Crisium Grid" "HQ")
      (core/reveal state :contestant (get-content state :hq 0))
      (take-credits state :contestant)
      (is (= 4 (:credit (get-contestant))) "Contestant has 4 credits")
      (core/gain state :challenger :credit 4)
      (play-from-hand state :challenger "Desperado")
      (play-from-hand state :challenger "Temüjin Contract")
      (click-prompt state :challenger "HQ")
      (run-empty-locale state "HQ")
      (is (= 2 (:credit (get-challenger))) "No Desperado or Temujin credits")
      (is (not (:successful-run (:register (get-challenger)))) "No successful run in register")))
  (testing "with Gauntlet, #3082"
    (do-game
      (new-game (default-contestant [(qty "Crisium Grid" 2)(qty "Vanilla" 2)])
                (default-challenger ["The Gauntlet" "Temüjin Contract"]))
      (play-from-hand state :contestant "Crisium Grid" "HQ")
      (play-from-hand state :contestant "Vanilla" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (core/reveal state :contestant (get-content state :hq 0))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 4)
      (play-from-hand state :challenger "The Gauntlet")
      (run-on state "HQ")
      (run-successful state)
      (is (seq (:prompt (get-challenger))) "The Gauntlet has a prompt"))))

(deftest cyberdex-virus-suite
  ;; Cyberdex Virus Suite
  (testing "Purge ability"
    (do-game
      (new-game (default-contestant [(qty "Cyberdex Virus Suite" 3)])
                (default-challenger ["Cache" "Medium"]))
      (play-from-hand state :contestant "Cyberdex Virus Suite" "HQ")
      (take-credits state :contestant 2)
      ;; challenger's turn
      ;; place cache and medium
      (play-from-hand state :challenger "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state :challenger (refresh card)))
            cache (find-card "Cache" (get-resource state))
            cvs (get-content state :hq 0)]
        (is (= 3 (virus-counters cache)))
        (play-from-hand state :challenger "Medium")
        (take-credits state :challenger 2)
        (core/reveal state :contestant cvs)
        (card-ability state :contestant cvs 0)
        ;; nothing in hq content
        (is (empty? (get-content state :hq)) "CVS was discarded")
        ;; purged counters
        (is (zero? (virus-counters cache))
            "Cache has no counters")
        (is (zero? (virus-counters (find-card "Medium" (get-resource state))))
            "Medium has no counters"))))
  (testing "Purge on access"
    (do-game
      (new-game (default-contestant [(qty "Cyberdex Virus Suite" 3)])
                (default-challenger ["Cache" "Medium"]))
      (play-from-hand state :contestant "Cyberdex Virus Suite" "New party")
      (take-credits state :contestant 2)
      ;; challenger's turn
      ;; place cache and medium
      (play-from-hand state :challenger "Cache")
      (let [virus-counters (fn [card] (core/get-virus-counters state :challenger (refresh card)))
            cache (find-card "Cache" (get-resource state))
            cvs (get-content state :party1 0)]
        (is (= 3 (virus-counters cache)))
        (play-from-hand state :challenger "Medium")
        (run-empty-locale state "Locale 1")
        ;; contestant now has optional prompt to trigger virus purge
        (click-prompt state :contestant "Yes")
        ;; challenger has prompt to discard CVS
        (click-prompt state :challenger "Pay 1 [Credits] to discard")
        ;; purged counters
        (is (zero? (virus-counters cache))
            "Cache has no counters")
        (is (zero? (virus-counters (find-card "Medium" (get-resource state))))
            "Medium has no counters"))))
  (testing "Don't interrupt archives access, #1647"
    (do-game
      (new-game (default-contestant ["Cyberdex Virus Suite" "Braintrust"])
                (default-challenger ["Cache"]))
      (discard-from-hand state :contestant "Cyberdex Virus Suite")
      (discard-from-hand state :contestant "Braintrust")
      (take-credits state :contestant)
      ;; challenger's turn
      ;; place cache
      (play-from-hand state :challenger "Cache")
      (let [cache (get-resource state 0)]
        (is (= 3 (get-counters (refresh cache) :virus)))
        (run-empty-locale state "Archives")
        (click-prompt state :challenger "Cyberdex Virus Suite")
        (click-prompt state :contestant "Yes")
        (is (pos? (count (:prompt (get-challenger)))) "CVS purge did not interrupt archives access")
        ;; purged counters
        (is (zero? (get-counters (refresh cache) :virus))
            "Cache has no counters")))))

(deftest drone-screen
  ;; Drone Screen
  (do-game
    (new-game (default-contestant ["Drone Screen"])
              (default-challenger))
    (play-from-hand state :contestant "Drone Screen" "New party")
    (let [drone (get-content state :party1 0)]
      (core/reveal state :contestant drone)
      (core/gain state :challenger :tag 1)
      (take-credits state :contestant)
      (run-on state "Locale 1")
      (is (zero? (-> (get-challenger) :discard count)) "Heap should start empty")
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should discard 1 card from meat damage from losing Drone Screen trace"))))

(deftest forced-connection
  ;; Forced Connection - ambush, trace(3) give the challenger 2 tags
  (do-game
    (new-game (default-contestant [(qty "Forced Connection" 3)])
              (default-challenger))
    (starting-hand state :contestant ["Forced Connection" "Forced Connection"])
    (play-from-hand state :contestant "Forced Connection" "New party")
    (take-credits state :contestant)
    (is (zero? (:tag (get-challenger))) "Challenger starts with 0 tags")
    (run-empty-locale state :party1)
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "0")
    (click-prompt state :challenger "Pay 0 [Credits] to discard") ; discard
    (is (= 2 (:tag (get-challenger))) "Challenger took two tags")
    (run-empty-locale state "Archives")
    (is (= 2 (:tag (get-challenger))) "Challenger doesn't take tags when accessed from Archives")
    (run-empty-locale state "HQ")
    (click-prompt state :contestant "0")
    (click-prompt state :challenger "3")
    (click-prompt state :challenger "Pay 0 [Credits] to discard") ; discard
    (is (= 2 (:tag (get-challenger))) "Challenger doesn't take tags when trace won")))

(deftest georgia-emelyov
  ;; Georgia Emelyov
  (do-game
    (new-game (default-contestant ["Georgia Emelyov"])
              (default-challenger))
    (play-from-hand state :contestant "Georgia Emelyov" "New party")
    (let [geo (get-content state :party1 0)]
      (core/reveal state :contestant geo)
      (take-credits state :contestant)
      (run-on state "Locale 1")
      (run-jack-out state)
      (is (= 1 (count (:discard (get-challenger)))) "Challenger took 1 net damage")
      (card-ability state :contestant (refresh geo) 0)
      (click-prompt state :contestant "Archives")
      (let [geo (get-content state :archives 0)]
        (is geo "Georgia moved to Archives")
        (run-on state "Archives")
        (run-jack-out state)
        (is (= 2 (count (:discard (get-challenger)))) "Challenger took 1 net damage")
        (run-on state "HQ")
        (run-jack-out state)
        (is (= 2 (count (:discard (get-challenger)))) "Challenger did not take damage")))))

(deftest giordano-memorial-field
  ;; Giordano Memorial Field
  (do-game
    (new-game (default-contestant ["Giordano Memorial Field" "Hostile Takeover"])
              (default-contestant [(qty "Fan Site" 3)]))
    (play-from-hand state :contestant "Giordano Memorial Field" "New party")
    (core/reveal state :contestant (get-content state :party1 0))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Fan Site")
    (play-from-hand state :challenger "Fan Site")
    (play-from-hand state :challenger "Fan Site")
    (take-credits state :challenger)
    (play-and-score state "Hostile Takeover")
    (take-credits state :contestant)
    (run-empty-locale state "Locale 1")
    (let [credits (:credit (get-challenger))]
      (click-prompt state :challenger "Pay 6 [Credits]")
      (is (= (- credits 6) (:credit (get-challenger))) "Challenger pays 6 credits to not end the run"))
    (click-prompt state :challenger "No action")
    (run-empty-locale state "Locale 1")
    (is (= 1 (-> (get-challenger) :prompt first :choices count)) "Challenger should only get 1 choice")
    (is (= "End the run" (-> (get-challenger) :prompt first :choices first)) "Only choice should be End the run")
    (click-prompt state :challenger "End the run")
    (is (not (:run @state)) "Run should be ended from Giordano Memorial Field ability")))

(deftest helheim-locales
  ;; Helheim Locales - Full test
  (do-game
    (new-game (default-contestant ["Helheim Locales" "Gutenberg" "Vanilla"
                             "Jackson Howard" "Hedge Fund"])
              (default-challenger))
    (play-from-hand state :contestant "Helheim Locales" "R&D")
    (play-from-hand state :contestant "Gutenberg" "R&D")
    (play-from-hand state :contestant "Vanilla" "R&D")
    (take-credits state :contestant)
    (run-on state "R&D")
    (is (:run @state))
    (let [helheim (get-content state :rd 0)
          gutenberg (get-character state :rd 0)
          vanilla (get-character state :rd 1)]
      (core/reveal state :contestant helheim)
      (core/reveal state :contestant gutenberg)
      (core/reveal state :contestant vanilla)
      (is (= 6 (:current-strength (refresh gutenberg))))
      (is (zero? (:current-strength (refresh vanilla))))
      (card-ability state :contestant helheim 0)
      (click-card state :contestant (find-card "Jackson Howard" (:hand (get-contestant))))
      (is (= 1 (count (:discard (get-contestant)))))
      (is (= 8 (:current-strength (refresh gutenberg))))
      (is (= 2 (:current-strength (refresh vanilla))))
      (card-ability state :contestant helheim 0)
      (click-card state :contestant (find-card "Hedge Fund" (:hand (get-contestant))))
      (is (= 2 (count (:discard (get-contestant)))))
      (is (= 10 (:current-strength (refresh gutenberg))))
      (is (= 4 (:current-strength (refresh vanilla))))
      (run-jack-out state)
      (is (not (:run @state)))
      (is (= 6 (:current-strength (refresh gutenberg))))
      (is (zero? (:current-strength (refresh vanilla)))))))

(deftest hokusai-grid
  ;; Hokusai Grid - Do 1 net damage when run successful on its locale
  (do-game
    (new-game (default-contestant ["Hokusai Grid"])
              (default-challenger))
    (play-from-hand state :contestant "Hokusai Grid" "HQ")
    (take-credits state :contestant)
    (core/reveal state :contestant (get-content state :hq 0))
    (run-empty-locale state :rd)
    (is (empty? (:discard (get-challenger))) "No net damage done for successful run on R&D")
    (run-empty-locale state :hq)
    (is (= 1 (count (:discard (get-challenger)))) "1 net damage done for successful run on HQ")))

(deftest intake
  ;; Intake - Trace4, add an placed resource or virtual radicle to the grip
  (do-game
    (new-game (default-contestant [(qty "Intake" 3)])
              (default-challenger ["Corroder" "Fester" "Daily Casts"]))
    (starting-hand state :contestant ["Intake" "Intake"])
    (play-from-hand state :contestant "Intake" "New party")
    (take-credits state :contestant)
    (core/gain state :challenger :click 5 :credit 10)
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "Fester")
    (play-from-hand state :challenger "Daily Casts")
    (run-on state "R&D")
    (run-successful state)
    (click-prompt state :contestant "0") ; trace
    (click-prompt state :challenger "0")
    (is (empty? (:hand (get-challenger))) "Challenger starts with no cards in hand")
    (click-card state :contestant (get-resource state 0))
    (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")
    (click-prompt state :challenger "Pay 0 [Credits] to discard") ; discard
    (run-on state "Archives")
    (run-successful state)
    (is (empty? (:prompt (get-contestant))) "No prompt from Archives access")
    (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")
    (run-on state "Locale 1")
    (run-successful state)
    (click-prompt state :contestant "0") ; trace
    (click-prompt state :challenger "0")
    (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")
    (click-card state :contestant (get-radicle state 0))
    (is (= 2 (count (:hand (get-challenger)))) "Challenger has 2 cards in hand")
    (click-prompt state :challenger "No action") ; discard
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :contestant "0") ; trace
    (click-prompt state :challenger "0")
    (click-prompt state :contestant "Done")
    (click-prompt state :challenger "No action") ; discard
    (is (empty? (:prompt (get-contestant))) "Prompt closes after done")
    (is (= 2 (count (:hand (get-challenger)))) "Challenger has 2 cards in hand")
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :contestant "0") ; trace
    (click-prompt state :challenger "5")
    (is (empty? (:prompt (get-contestant))) "Prompt closes after lost trace")))

(deftest jinja-city-grid
  ;; Jinja City Grid - place drawn character, lowering place cost by 4
  (testing "Single draws"
    (do-game
    (new-game (default-contestant ["Jinja City Grid" (qty "Vanilla" 3) (qty "Ice Wall" 3)])
              (default-challenger))
    (starting-hand state :contestant ["Jinja City Grid"])
    (core/gain state :contestant :click 6)
    (play-from-hand state :contestant "Jinja City Grid" "New party")
    (core/reveal state :contestant (get-content state :party1 0))
    (dotimes [n 5]
      (core/click-draw state :contestant 1)
      (click-prompt state :contestant (-> (get-contestant) :prompt first :choices first))
      (is (= 4 (:credit (get-contestant))) "Not charged to place character")
      (is (= (inc n) (count (get-in @state [:contestant :locales :party1 :characters]))) (str n " Character protecting Party1")))
    (core/click-draw state :contestant 1)
    (click-prompt state :contestant (-> (get-contestant) :prompt first :choices first))
    (is (= 3 (:credit (get-contestant))) "Charged to place character")
    (is (= 6 (count (get-in @state [:contestant :locales :party1 :characters]))) "6 Character protecting Party1")))
  (testing "Drawing non-character on challenger's turn"
    (do-game
      (new-game
        (default-contestant ["Jinja City Grid" (qty "Hedge Fund" 3)])
        (make-deck "Laramy Fisk: Savvy Investor" ["Eden Shard"]))
      (starting-hand state :contestant ["Jinja City Grid"])
      (play-from-hand state :contestant "Jinja City Grid" "HQ")
      (core/reveal state :contestant (get-content state :hq 0))
      (take-credits state :contestant)
      (run-empty-locale state :rd)
      (click-prompt state :challenger "Yes")
      (is (= :bogus (-> (get-contestant) :prompt first :prompt-type)) "Contestant has a bogus prompt to fake out the challenger")
      (click-prompt state :contestant "Carry on!")
      (click-prompt state :challenger "No action"))))

(deftest keegan-lane
  ;; Keegan Lane - Discard self and remove 1 Challenger tag to discard a resource
  (do-game
    (new-game (default-contestant ["Keegan Lane"])
              (default-challenger ["Corroder"]))
    (play-from-hand state :contestant "Keegan Lane" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Corroder")
    (run-on state :hq)
    (let [keeg (get-content state :hq 0)]
      (core/reveal state :contestant keeg)
      (card-ability state :contestant keeg 0)
      (is (= 1 (count (get-content state :hq))) "Keegan didn't fire, Challenger has no tags")
      (core/gain state :challenger :tag 2)
      (card-ability state :contestant keeg 0)
      (click-card state :contestant (get-resource state 0))
      (is (= 1 (:tag (get-challenger))) "1 tag removed")
      (is (= 1 (count (:discard (get-contestant)))) "Keegan discarded")
      (is (= 1 (count (:discard (get-challenger)))) "Corroder discarded"))))

(deftest manta-grid
  ;; If the Challenger has fewer than 6 or no unspent clicks on successful run, contestant gains a click next turn.
  (do-game
    (new-game (default-contestant ["Manta Grid"])
              (default-challenger))
    (starting-hand state :challenger [])
    (is (= 3 (:click (get-contestant))) "Contestant has 3 clicks")
    (play-from-hand state :contestant "Manta Grid" "HQ")
    (core/reveal state :contestant (get-content state :hq 0))
    (take-credits state :contestant)
    (core/click-draw state :challenger nil)
    (core/click-draw state :challenger nil)
    (run-empty-locale state "HQ")
    (click-prompt state :challenger "No action") ; don't discard Manta Grid
    (is (= 1 (:click (get-challenger))) "Running last click")
    (run-empty-locale state "HQ")
    (click-prompt state :challenger "No action") ; don't discard Manta Grid
    (take-credits state :challenger)
    (is (= 5 (:click (get-contestant))) "Contestant gained 2 clicks due to 2 runs with < 6 Challenger credits")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (is (= 3 (:click (get-contestant))) "Contestant back to 3 clicks")
    (take-credits state :contestant)
    (take-credits state :challenger 3)
    (run-empty-locale state "HQ")
    (click-prompt state :challenger "No action") ; don't discard Manta Grid
    (take-credits state :challenger)
    (is (= 4 (:click (get-contestant))) "Contestant gained a click due to running last click")))

(deftest marcus-batty
  ;; Marcus Batty
  (testing "Simultaneous Interaction with Security Nexus"
    (do-game
      (new-game (default-contestant ["Marcus Batty" "Enigma"])
                (default-challenger ["Security Nexus"]))
      (play-from-hand state :contestant "Marcus Batty" "HQ")
      (play-from-hand state :contestant "Enigma" "HQ")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 8)
      (play-from-hand state :challenger "Security Nexus")
      (let [mb (get-content state :hq 0)
            en (get-character state :hq 0)
            sn (-> @state :challenger :rig :hazard first)]
        (run-on state "HQ")
        (core/reveal state :contestant mb)
        (core/reveal state :contestant en)
        (card-ability state :contestant mb 0)
        (card-ability state :challenger sn 0)
        ;; both prompts should be on Batty
        (is (prompt-is-card? state :contestant mb) "Contestant prompt is on Marcus Batty")
        (is (prompt-is-card? state :challenger mb) "Challenger prompt is on Marcus Batty")
        (click-prompt state :contestant "0 [Credits]")
        (click-prompt state :challenger "0 [Credits]")
        (is (prompt-is-card? state :contestant sn) "Contestant prompt is on Security Nexus")
        (is (prompt-is-type? state :challenger :waiting) "Challenger prompt is waiting for Contestant")))))

(deftest mumbad-city-grid
  ;; Mumbad City Grid - when challenger passes a piece of character, swap that character with another from this locale
  (testing "1 character"
    (do-game
      (new-game (default-contestant ["Mumbad City Grid" "Quandary"])
                (default-challenger))
      (play-from-hand state :contestant "Mumbad City Grid" "New party")
      (play-from-hand state :contestant "Quandary" "Locale 1")
      (let [mcg (get-content state :party1 0)]
        (core/reveal state :contestant mcg)
        (take-credits state :contestant)
        (run-on state "Locale 1")
        (is (= 1 (count (get-in @state [:contestant :locales :party1 :characters]))) "1 character on locale")
        (card-ability state :contestant (refresh mcg) 0)
        (run-continue state)
        (card-ability state :contestant (refresh mcg) 0)
        (run-jack-out state)
        (is (= 1 (count (get-in @state [:contestant :locales :party1 :characters]))) "Still 1 character on locale"))))
  (testing "fire before pass"
    (do-game
      (new-game (default-contestant ["Mumbad City Grid" "Quandary" "Ice Wall"])
                (default-challenger))
      (play-from-hand state :contestant "Mumbad City Grid" "New party")
      (play-from-hand state :contestant "Quandary" "Locale 1")
      (play-from-hand state :contestant "Ice Wall" "Locale 1")
      (let [mcg (get-content state :party1 0)]
        (core/reveal state :contestant mcg)
        (take-credits state :contestant)
        (run-on state "Locale 1")
        (is (= 2 (:position (:run @state))) "Challenger at position 2")
        (is (= 2 (count (get-in @state [:contestant :locales :party1 :characters]))) "2 character on locale")
        (is (= "Quandary" (:title (first (get-in @state [:contestant :locales :party1 :characters])))) "Quandary inner character")
        (is (= "Ice Wall" (:title (second (get-in @state [:contestant :locales :party1 :characters])))) "Ice Wall outer character")
        (card-ability state :contestant (refresh mcg) 0)
        (run-continue state)
        (is (= 1 (:position (:run @state))) "Challenger at position 1")
        (card-ability state :contestant (refresh mcg) 0)
        (click-card state :contestant (get-character state :party1 0))
        (is (= 1 (:position (:run @state))) "Challenger at position 1")
        (is (= "Quandary" (:title (second (get-in @state [:contestant :locales :party1 :characters])))) "Quandary outer character")
        (is (= "Ice Wall" (:title (first (get-in @state [:contestant :locales :party1 :characters])))) "Ice Wall inner character")
        (run-jack-out state)
        (is (= 2 (count (get-in @state [:contestant :locales :party1 :characters]))) "Still 2 character on locale")))))

(deftest mumbad-virtual-tour
  ;; Tests that Mumbad Virtual Tour forces discard when no :slow-discard
  (do-game
    (new-game (default-contestant [(qty "Mumbad Virtual Tour" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Mumbad Virtual Tour" "New party")
    (take-credits state :contestant)
    (run-empty-locale state "HQ")
    ;; MVT does not force discard when not placed
    (click-prompt state :challenger "No action")
    (is (= 5 (:credit (get-challenger))) "Challenger not forced to discard MVT in HQ")
    (is (empty? (:discard (get-contestant))) "MVT in HQ is not discarded")
    (run-empty-locale state "Locale 1")
    (is (= 1 (-> @state :challenger :prompt first :choices count)) "Should only have a single option")
    (click-prompt state :challenger "Pay 5 [Credits] to discard")
    (is (zero? (:credit (get-challenger))) "Challenger forced to discard MVT")
    (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-contestant))))) "MVT discarded"))
  (testing "interaction with Imp"
    (do-game
      (new-game (default-contestant [(qty "Mumbad Virtual Tour" 2)])
                (default-challenger ["Imp"]))
      (play-from-hand state :contestant "Mumbad Virtual Tour" "New party")
      (play-from-hand state :contestant "Mumbad Virtual Tour" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Imp")
      ;; Reset credits to 5
      (core/gain state :challenger :credit 2)
      (run-empty-locale state "Locale 1")
      ;; Challenger not force to discard since Imp is placed
      (is (= 2 (-> @state :challenger :prompt first :choices count)) "Challenger has 2 choices when Imp is placed")
      (is (= 5 (:credit (get-challenger))) "Challenger not forced to discard MVT when Imp placed")
      (is (empty? (:discard (get-contestant))) "MVT is not force-discarded when Imp placed")
      (let [imp (get-resource state 0)]
        (click-prompt state :challenger "[Imp]: Discard card")
        (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-contestant))))) "MVT discarded with Imp")
        ;; Discard Imp to reset :slow-discard flag
        (core/move state :challenger (refresh imp) :discard)
        (is (not (core/any-flag-fn? state :challenger :slow-discard true))))))
  (testing "interactions with Imp and various amounts of money"
    (do-game
      (new-game (default-contestant [(qty "Mumbad Virtual Tour" 3)])
                (default-challenger ["Imp"]))
      (play-from-hand state :contestant "Mumbad Virtual Tour" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Imp")
      (is (= 3 (:credit (get-challenger))) "Challenger paid place costs")
      (core/gain state :challenger :credit 2)
      (run-empty-locale state "Locale 1")
      (is (= #{"[Imp]: Discard card" "Pay 5 [Credits] to discard"}
             (->> (get-challenger) :prompt first :choices (into #{}))) "Should have Imp and MVT options")
      (click-prompt state :challenger "[Imp]: Discard card")
      (take-credits state :challenger)
      (core/lose state :challenger :credit (:credit (get-challenger)))
      (play-from-hand state :contestant "Mumbad Virtual Tour" "New party")
      (take-credits state :contestant)
      (run-empty-locale state "Locale 2")
      (is (= ["[Imp]: Discard card"] (-> (get-challenger) :prompt first :choices)) "Should only have Imp option")
      (click-prompt state :challenger "[Imp]: Discard card")
      (take-credits state :challenger)
      (core/lose state :challenger :credit (:credit (get-challenger)))
      (play-from-hand state :contestant "Mumbad Virtual Tour" "New party")
      (take-credits state :contestant)
      (run-empty-locale state "Locale 3")
      (is (= ["No action"] (-> (get-challenger) :prompt first :choices)) "Should only have no action option")
      (click-prompt state :challenger "No action")
      (is (= 2 (->> (get-contestant) :discard count)) "Challenger was not forced to discard MVT")))
  (testing "not forced to discard when credits below 5"
    (do-game
      (new-game (default-contestant [(qty "Mumbad Virtual Tour" 3)])
                (default-challenger ["Cache"]))
      (play-from-hand state :contestant "Mumbad Virtual Tour" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Cache")
      (is (= 4 (:credit (get-challenger))) "Challenger paid place costs")
      (run-empty-locale state "Locale 1")
      (is (= ["No action"] (-> (get-challenger) :prompt first :choices)) "Can't discard"))))

(deftest mwanza-city-grid
  ;; Mwanza City Grid - challenger accesses 3 additional cards, gain 2C for each card accessed
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Mwanza City Grid" (qty "Hedge Fund" 5)])
                (default-challenger))
      (play-from-hand state :contestant "Mwanza City Grid")
      (is (= #{"R&D" "HQ"} (-> (get-contestant) :prompt first :choices set)) "Mwanza can only be placed in root of HQ or R&D")
      (click-prompt state :contestant "HQ")
      (take-credits state :contestant)
      (run-on state "HQ")
      (let [mcg (get-content state :hq 0)]
        (core/reveal state :contestant mcg)
        (is (= 7 (:credit (get-contestant))) "Contestant starts with 7 credits")
        (run-successful state)
        (click-prompt state :challenger "Mwanza City Grid")
        (click-prompt state :challenger "No action")
        (dotimes [c 4]
          (click-prompt state :challenger "Card from hand")
          (click-prompt state :challenger "No action"))
        (is (empty? (:prompt (get-challenger))) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-contestant))) "Contestant gains 10 credits"))))
  (testing "effect persists through current run after discard"
    (do-game
      (new-game (default-contestant ["Mwanza City Grid" (qty "Hedge Fund" 5)])
                (default-challenger))
      (play-from-hand state :contestant "Mwanza City Grid" "HQ")
      (take-credits state :contestant)
      (run-on state "HQ")
      (let [mcg (get-content state :hq 0)]
        (core/reveal state :contestant mcg)
        (is (= 7 (:credit (get-contestant))) "Contestant starts with 7 credits")
        (run-successful state)
        (click-prompt state :challenger "Mwanza City Grid")
        (click-prompt state :challenger "Pay 5 [Credits] to discard")
        (dotimes [c 4]
          (click-prompt state :challenger "Card from hand")
          (click-prompt state :challenger "No action"))
        (is (empty? (:prompt (get-challenger))) "Prompt closed after accessing cards")
        (is (= 17 (:credit (get-contestant))) "Contestant gains 10 credits"))))
  (testing "works well with replacement effects"
    ;; Regression test for #3456
    (do-game
      (new-game (default-contestant ["Mwanza City Grid" "Hedge Fund"])
                (default-challenger ["Embezzle"]))
      (play-from-hand state :contestant "Mwanza City Grid" "HQ")
      (take-credits state :contestant)
      (core/reveal state :contestant (get-content state :hq 0))
      (is (= 7 (:credit (get-contestant))) "Contestant starts with 7 credits")
      (play-run-event state (first (:hand (get-challenger))) :hq)
      (click-prompt state :challenger "Character")
      (is (zero? (count (:discard (get-contestant)))) "No cards discarded from HQ")
      (is (not (:run @state)) "Run ended after Embezzle completed - no accesses from Mwanza")
      (is (= 7 (:credit (get-contestant))) "Contestant did not gain any money from Mwanza")))
  (testing "interaction with Kitsune"
    ;; Regression test for #3469
    (do-game
      (new-game (default-contestant ["Mwanza City Grid" "Breached Dome"
                               (qty "Kitsune" 2) (qty "Hedge Fund" 3)])
                (default-challenger))
      (core/draw state :contestant 1) ; Draw last card of deck
      (play-from-hand state :contestant "Mwanza City Grid" "HQ")
      (play-from-hand state :contestant "Kitsune" "HQ")
      (play-from-hand state :contestant "Kitsune" "R&D")
      (take-credits state :contestant)
      (let [mwanza (get-content state :hq 0)
            k-hq (get-character state :hq 0)
            k-rd (get-character state :rd 0)]
        (core/reveal state :contestant mwanza)
        (core/reveal state :contestant k-hq)
        (core/reveal state :contestant k-rd)
        (run-on state "HQ")
        (card-subroutine state :contestant k-hq 0)
        (click-card state :contestant (find-card "Breached Dome" (:hand (get-contestant))))
        (is (= 2 (-> (get-challenger) :hand count)) "Challenger took 1 meat from Breached Dome access from Kitsune")
        (click-prompt state :challenger "No action")
        ;; Access 3 more cards from HQ
        (dotimes [c 3]
          (click-prompt state :challenger "Card from hand")
          (click-prompt state :challenger "No action"))
        (run-jack-out state)
        (run-on state "R&D")
        (card-subroutine state :contestant k-rd 0)
        (click-card state :contestant (find-card "Breached Dome" (:hand (get-contestant))))
        (is (= 1 (-> (get-challenger) :hand count)) "Challenger took 1 meat from Breached Dome access from Kitsune")
        (click-prompt state :challenger "No action")
        ;; Access 3 more cards from HQ
        (dotimes [c 3]
          (click-prompt state :challenger "Card from hand")
          (click-prompt state :challenger "No action"))
        (run-jack-out state)
        (is (= 2 (-> (get-contestant) :discard count)) "Two Kitsunes discarded after resolving their subroutines")))))

(deftest neotokyo-grid
  ;; NeoTokyo Grid - Gain 1c the first time per turn a card in this locale gets an advancement
  (do-game
    (new-game (default-contestant ["NeoTokyo Grid" "Nisei MK II"
                             "Shipment from SanSan" "Ice Wall"])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "NeoTokyo Grid" "New party")
    (play-from-hand state :contestant "Nisei MK II" "Locale 1")
    (core/reveal state :contestant (get-content state :party1 0))
    (let [nis (get-content state :party1 1)]
      (play-from-hand state :contestant "Shipment from SanSan")
      (click-prompt state :contestant "2")
      (click-card state :contestant nis)
      (is (= 2 (get-counters (refresh nis) :advancement)) "2 advancements on agenda")
      (is (= 4 (:credit (get-contestant))) "Gained 1 credit")
      (core/advance state :contestant {:card (refresh nis)})
      (is (= 3 (get-counters (refresh nis) :advancement)) "3 advancements on agenda")
      (is (= 3 (:credit (get-contestant))) "No credit gained")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (play-from-hand state :contestant "Ice Wall" "Locale 1")
      (core/advance state :contestant {:card (refresh (get-character state :party1 0))})
      (is (= 2 (:credit (get-contestant))) "No credit gained from advancing Character"))))

(deftest oberth-protocol
  ;; Oberth Protocol
  (do-game
    (new-game (default-contestant ["Hostile Takeover" "Oberth Protocol" "Oaktown Renovation"])
              (default-challenger))
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :contestant "Oberth Protocol" "Locale 1")
    (play-from-hand state :contestant "Oaktown Renovation" "Locale 1")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (let [oberth (get-content state :party1 0)
          oak (get-content state :party1 1) ]
      (core/reveal state :contestant (refresh oberth))
      (click-card state :contestant (get-scored state :contestant 0))
      (advance state oak)
      (is (= 2 (get-counters (refresh oak) :advancement)) "Oaktown should have 2 advancement tokens on it"))))

(deftest off-the-grid
  ;; Off the Grid run restriction - and interaction with RP
  (do-game
   (new-game
    (make-deck "Cardnum: Replicating Perfection" [(qty "Off the Grid" 3)
                                                  (qty "Mental Health Clinic" 3)])
    (default-challenger))
   (play-from-hand state :contestant "Off the Grid" "New party")
   (play-from-hand state :contestant "Mental Health Clinic" "Locale 1")
   (let [otg (get-content state :party1 0)]
     (take-credits state :contestant)
     (core/reveal state :contestant (refresh otg))
     (is (not (core/can-run-locale? state "Locale 1")) "Challenger can only run on centrals")
     (run-empty-locale state "R&D")
     (is (not (core/can-run-locale? state "Locale 1")) "Challenger cannot run on Off the Grid")
     (take-credits state :challenger)
     (take-credits state :contestant)
     (is (not (core/can-run-locale? state "Locale 1")) "Off the Grid prevention persisted")
     (run-empty-locale state "HQ")
     (is (boolean (core/can-run-locale? state "Locale 1")) "Challenger can run on Locale 1")
     (is (= nil (refresh otg)) "Off the Grid discarded"))))

(deftest old-hollywood-grid
  ;; Old Hollywood Grid
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Old Hollywood Grid" (qty "House of Knives" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Old Hollywood Grid" "New party")
      (play-from-hand state :contestant "House of Knives" "Locale 1")
      (take-credits state :contestant 1)
      (let [ohg (get-content state :party1 0)
            hok (get-content state :party1 1)]
        (run-on state "Locale 1")
        (core/reveal state :contestant ohg)
        (run-successful state)
        ;; challenger now chooses which to access.
        (click-card state :challenger hok)
        (click-prompt state :challenger "No action")
        (is (zero? (count (:scored (get-challenger)))) "No stolen agendas")
        (click-card state :challenger ohg)
        (click-prompt state :challenger "No action")
        (core/steal state :challenger (find-card "House of Knives" (:hand (get-contestant))))
        (run-empty-locale state "Locale 1")
        (click-card state :challenger hok)
        (click-prompt state :challenger "Steal")
        (is (= 2 (count (:scored (get-challenger)))) "2 stolen agendas"))))
  (testing "Central locale"
    (do-game
      (new-game (default-contestant ["Old Hollywood Grid" (qty "House of Knives" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Old Hollywood Grid" "HQ")
      (take-credits state :contestant 2)
      (let [ohg (get-content state :hq 0)]
        (run-on state "HQ")
        (core/reveal state :contestant ohg)
        (run-successful state)
        ;; challenger now chooses which to access.
        (click-prompt state :challenger "Card from hand")
        (click-prompt state :challenger "No action")
        (is (zero? (count (:scored (get-challenger)))) "No stolen agendas")
        (click-prompt state :challenger "Old Hollywood Grid")
        (click-prompt state :challenger "Pay 4 [Credits] to discard") ;; discard OHG
        (run-empty-locale state "HQ")
        (click-prompt state :challenger "Steal")
        (is (= 1 (count (:scored (get-challenger)))) "1 stolen agenda"))))
  (testing "Gang Sign interaction. Prevent the steal outside of a run. #2169"
    (do-game
      (new-game (default-contestant ["Old Hollywood Grid" (qty "Project Beale" 2)])
                (default-challenger ["Gang Sign"]))
      (play-from-hand state :contestant "Old Hollywood Grid" "HQ")
      (play-from-hand state :contestant "Project Beale" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Gang Sign")
      (take-credits state :challenger)
      (core/reveal state :contestant (get-content state :hq 0))
      (score-agenda state :contestant (get-content state :party1 0))
      ;; Gang sign fires
      (click-prompt state :challenger "Card from hand")
      (click-prompt state :challenger "No action")
      (is (zero? (count (:scored (get-challenger)))) "No stolen agendas")))
  (testing "Discard order"
    (do-game
      (new-game (default-contestant ["Old Hollywood Grid" "Project Beale"])
                (default-challenger))
      (play-from-hand state :contestant "Old Hollywood Grid" "New party")
      (play-from-hand state :contestant "Project Beale" "Locale 1")
      (take-credits state :contestant)
      (let [ohg (get-content state :party1 0)
            pb (get-content state :party1 1)]
        (run-on state "Locale 1")
        (core/reveal state :contestant ohg)
        (run-successful state)
        (is (empty? (:scored (get-challenger))) "Start with no stolen agendas")
        ;; challenger now chooses which to access.
        (click-card state :challenger (refresh ohg))
        (click-prompt state :challenger "Pay 4 [Credits] to discard") ;; discard OHG
        (click-card state :challenger (refresh pb))
        (click-prompt state :challenger "No action")
        (is (empty? (:scored (get-challenger))) "End with no stolen agendas")
        (run-empty-locale state "Locale 1")
        (click-prompt state :challenger "Steal")
        (is (= 1 (count (:scored (get-challenger)))) "1 stolen agenda"))))
  (testing "Steal other agendas"
    (do-game
      (new-game (default-contestant ["Old Hollywood Grid" (qty "Project Beale" 2)])
                (default-challenger))
      (play-from-hand state :contestant "Old Hollywood Grid" "New party")
      (play-from-hand state :contestant "Project Beale" "Locale 1")
      (play-from-hand state :contestant "Project Beale" "New party")
      (take-credits state :contestant)
      (let [ohg (get-content state :party1 0)
            pb (get-content state :party1 1)]
        (core/reveal state :contestant ohg)
        (run-empty-locale state "Locale 2")
        (click-prompt state :challenger "Steal")
        (is (= 1 (count (:scored (get-challenger)))) "1 stolen agenda")))))

(deftest overseer-matrix
  ;; Overseer Matrix - contestant takes a tag when discarding a card in this locale
  (testing "Basic functionality"
    (do-game
      (new-game (default-contestant ["Overseer Matrix" "Red Herrings"])
                (default-challenger))
      (play-from-hand state :contestant "Overseer Matrix" "New party")
      (play-from-hand state :contestant "Red Herrings" "Locale 1")
      (take-credits state :contestant)
      (let [om (get-content state :party1 0)
            rh (get-content state :party1 1)]
        (run-on state "Locale 1")
        (core/reveal state :contestant om)
        (run-successful state)
        (is (zero? (:tag (get-challenger))) "Challenger starts with no tags")
        (click-card state :challenger rh)
        (click-prompt state :challenger "Pay 1 [Credits] to discard")
        (click-prompt state :contestant "Yes")
        (is (= 1 (:tag (get-challenger))) "Challenger takes a tag")
        (click-card state :challenger om)
        (click-prompt state :challenger "Pay 2 [Credits] to discard")
        (click-prompt state :contestant "Yes")
        (is (= 2 (:tag (get-challenger))) "Challenger takes a tag"))))
  (testing "Effect persists after discard"
    (do-game
      (new-game (default-contestant ["Overseer Matrix" (qty "Red Herrings" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Overseer Matrix" "New party")
      (play-from-hand state :contestant "Red Herrings" "Locale 1")
      (take-credits state :contestant)
      (let [om (get-content state :party1 0)
            rh (get-content state :party1 1)]
        (run-on state "Locale 1")
        (core/reveal state :contestant om)
        (run-successful state)
        (is (zero? (:tag (get-challenger))) "Challenger starts with no tags")
        (click-card state :challenger om)
        (click-prompt state :challenger "Pay 2 [Credits] to discard")
        (click-prompt state :contestant "Yes")
        (is (= 1 (:tag (get-challenger))) "Challenger takes a tag")
        (click-card state :challenger rh)
        (click-prompt state :challenger "Pay 1 [Credits] to discard")
        (click-prompt state :contestant "Yes")
        (is (= 2 (:tag (get-challenger))) "Challenger takes a tag"))))
  (testing "Effect ends after current run"
    (do-game
      (new-game (default-contestant ["Overseer Matrix" (qty "Red Herrings" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Overseer Matrix" "New party")
      (play-from-hand state :contestant "Red Herrings" "Locale 1")
      (take-credits state :contestant)
      (let [om (get-content state :party1 0)
            rh (get-content state :party1 1)]
        (run-on state "Locale 1")
        (core/reveal state :contestant om)
        (run-successful state)
        (is (zero? (:tag (get-challenger))) "Challenger starts with no tags")
        (click-card state :challenger om)
        (click-prompt state :challenger "Pay 2 [Credits] to discard")
        (click-prompt state :contestant "Yes")
        (is (= 1 (:tag (get-challenger))) "Challenger takes a tag")
        (click-card state :challenger rh)
        (click-prompt state :challenger "No action")
        (is (= 1 (:tag (get-challenger))) "Challenger doesn't take a tag")
        (run-on state "Locale 1")
        (run-successful state)
        (click-prompt state :challenger "Pay 1 [Credits] to discard")
        (is (empty? (:prompt (get-contestant))) "No prompt for Overseer Matrix")
        (is (= 1 (:tag (get-challenger))) "Challenger doesn't take a tag")))))

(deftest port-anson-grid
  ;; Port Anson Grid - Prevent the Challenger from jacking out until they discard a resource
  (do-game
    (new-game (default-contestant ["Port Anson Grid" "Data Raven"])
              (default-challenger ["Faerie" "Technical Writer"]))
    (play-from-hand state :contestant "Port Anson Grid" "New party")
    (play-from-hand state :contestant "Data Raven" "Locale 1")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Technical Writer")
    (play-from-hand state :challenger "Faerie")
    (let [pag (get-content state :party1 0)
          fae (get-resource state 0)
          tw (get-radicle state 0)]
      (run-on state "Locale 1")
      (core/reveal state :contestant pag)
      (is (:cannot-jack-out (get-in @state [:run])) "Jack out disabled for Challenger") ; UI button greyed out
      (core/discard state :challenger tw)
      (is (:cannot-jack-out (get-in @state [:run])) "Radicle discard didn't disable jack out prevention")
      (core/discard state :challenger fae)
      (is (nil? (:cannot-jack-out (get-in @state [:run]))) "Jack out enabled by resource discard")
      (run-on state "Locale 1")
      (is (:cannot-jack-out (get-in @state [:run])) "Prevents jack out when region is revealed prior to run"))))

(deftest prisec
  ;; Prisec
  (testing "Basic test - Pay 2 credits to give challenger 1 tag and do 1 meat damage, only when placed"
    (do-game
      (new-game (default-contestant [(qty "Prisec" 2)])
                (default-challenger))
      (play-from-hand state :contestant "Prisec" "New party")
      (take-credits state :contestant)
      (run-empty-locale state "Locale 1")
      (let [pre-creds (:credit (get-contestant))]
        (click-prompt state :contestant "Yes")
        (is (= (- pre-creds 2) (:credit (get-contestant))) "Pay 2 [Credits] to pay for Prisec"))
      (is (= 1 (:tag (get-challenger))) "Give challenger 1 tag")
      (is (= 1 (count (:discard (get-challenger)))) "Prisec does 1 damage")
      ;; Challenger discards Prisec
      (click-prompt state :challenger "Pay 3 [Credits] to discard")
      (run-empty-locale state "HQ")
      (is (not (:prompt @state)) "Prisec does not trigger from HQ")))
  (testing "Multiple unrevealed regions in Archives interaction with DRT"
    (do-game
      (new-game (default-contestant [(qty "Prisec" 2) "Dedicated Response Team"])
                (default-challenger [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
      (play-from-hand state :contestant "Dedicated Response Team" "New party")
      (play-from-hand state :contestant "Prisec" "Archives")
      (play-from-hand state :contestant "Prisec" "Archives")
      (core/gain state :contestant :click 1 :credit 14)
      (core/reveal state :contestant (get-content state :party1 0))
      (take-credits state :contestant)
      (run-empty-locale state :archives)
      (is (:run @state) "Run still active")
      (click-prompt state :challenger "Unrevealed region in Archives")
      (click-card state :challenger (get-content state :archives 0))
      (click-prompt state :contestant "Yes") ; contestant pay for PriSec
      (click-prompt state :challenger "No action") ; challenger don't pay to discard
      (is (:run @state) "Run still active")
      (click-prompt state :challenger "Unrevealed region in Archives")
      (click-prompt state :contestant "Yes") ; contestant pay for PriSec
      (click-prompt state :challenger "No action") ; challenger don't pay to discard
      (is (not (:run @state)) "Run ended")
      (is (= 4 (count (:discard (get-challenger)))) "Challenger took 4 meat damage"))))

(deftest product-placement
  ;; Product Placement - Gain 2 credits when Challenger accesses it
  (do-game
    (new-game (default-contestant ["Product Placement"])
              (default-challenger))
    (play-from-hand state :contestant "Product Placement" "New party")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))))
    (let [pp (get-content state :party1 0)]
      (run-empty-locale state "Locale 1")
      (is (= 9 (:credit (get-contestant))) "Gained 2 credits from Challenger accessing Product Placement")
      (click-prompt state :challenger "Pay 2 [Credits] to discard") ; Challenger discards PP
      (run-empty-locale state "Archives")
      (is (= 9 (:credit (get-contestant)))
          "No credits gained when Product Placement accessed in Archives"))))

(deftest red-herrings
  ;; Red Herrings
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Red Herrings" "House of Knives"])
                (default-challenger))
      (play-from-hand state :contestant "Red Herrings" "New party")
      (play-from-hand state :contestant "House of Knives" "Locale 1")
      (take-credits state :contestant 1)
      (let [rh (get-content state :party1 0)
            hok (get-content state :party1 1)]
        (core/reveal state :contestant rh)
        (run-empty-locale state "Locale 1")
        ;; challenger now chooses which to access.
        (click-card state :challenger hok)
        ;; prompt should be asking for the 5cr cost
        (is (= "House of Knives" (:title (:card (first (:prompt (get-challenger))))))
            "Prompt to pay 5cr")
        (click-prompt state :challenger "No action")
        (is (= 5 (:credit (get-challenger))) "Challenger was not charged 5cr")
        (is (zero? (count (:scored (get-challenger)))) "No scored agendas")
        (click-card state :challenger rh)
        (click-prompt state :challenger "No action")
        (run-empty-locale state "Locale 1")
        (click-card state :challenger hok)
        (click-prompt state :challenger "Pay 5 [Credits] to steal")
        (is (zero? (:credit (get-challenger))) "Challenger was charged 5cr")
        (is (= 1 (count (:scored (get-challenger)))) "1 scored agenda"))))
  (testing "Cost increase even when discarded"
    (do-game
      (new-game (default-contestant [(qty "Red Herrings" 3) (qty "House of Knives" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Red Herrings" "New party")
      (play-from-hand state :contestant "House of Knives" "Locale 1")
      (take-credits state :contestant 1)
      (core/gain state :challenger :credit 1)
      (let [rh (get-content state :party1 0)
            hok (get-content state :party1 1)]
        (core/reveal state :contestant rh)
        (run-empty-locale state "Locale 1")
        ;; challenger now chooses which to access.
        (click-card state :challenger rh)
        (click-prompt state :challenger "Pay 1 [Credits] to discard") ; pay to discard
        (click-card state :challenger hok)
        ;; should now have prompt to pay 5cr for HoK
        (click-prompt state :challenger "Pay 5 [Credits] to steal")
        (is (zero? (:credit (get-challenger))) "Challenger was charged 5cr")
        (is (= 1 (count (:scored (get-challenger)))) "1 scored agenda"))))
  (testing "Discarded from HQ"
    (do-game
      (new-game (default-contestant ["Red Herrings" "House of Knives"])
                (default-challenger))
      (discard-from-hand state :contestant "Red Herrings")
      (is (= 1 (count (:discard (get-contestant)))) "1 card in Archives")
      (take-credits state :contestant)
      (run-empty-locale state "HQ")
      ;; prompt should be asking to steal HoK
      (is (= "Steal" (first (:choices (first (:prompt (get-challenger))))))
          "Challenger being asked to Steal")))
  (testing "Don't affect runs on other locales"
    (do-game
      (new-game (default-contestant ["Red Herrings" "House of Knives"])
                (default-challenger))
      (play-from-hand state :contestant "Red Herrings" "New party")
      (play-from-hand state :contestant "House of Knives" "New party")
      (take-credits state :contestant 1)
      (let [rh (get-content state :party1 0)]
        (core/reveal state :contestant rh)
        (run-empty-locale state "Locale 2")
        ;; access is automatic
        (click-prompt state :challenger "Steal")
        (is (= 5 (:credit (get-challenger))) "Challenger was not charged 5cr")
        (is (= 1 (count (:scored (get-challenger)))) "1 scored agenda")))))

(deftest ruhr-valley
  ;; Ruhr Valley
  (testing "Basic test - As an additional cost to make a run on this locale, the Challenger must spend a click."
    (do-game
      (new-game (default-contestant ["Ruhr Valley"])
                (default-challenger))
      (play-from-hand state :contestant "Ruhr Valley" "HQ")
      (take-credits state :contestant)
      (let [ruhr (get-content state :hq 0)]
        (core/reveal state :contestant ruhr)
        (is (= 4 (:click (get-challenger))))
        (run-on state :hq)
        (run-jack-out state)
        (is (= 2 (:click (get-challenger))))
        (take-credits state :challenger 1)
        (is (= 1 (:click (get-challenger))))
        (is (not (core/can-run-locale? state "HQ")) "Challenger can't run - no additional clicks")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 4 (:click (get-challenger))))
        (is (= 7 (:credit (get-challenger))))
        (run-on state :hq)
        (run-successful state)
        (click-prompt state :challenger "Pay 4 [Credits] to discard") ; pay to discard / 7 cr - 4 cr
        (is (= 2 (:click (get-challenger))))
        (is (= 3 (:credit (get-challenger))))
        (run-on state :hq)
        (run-jack-out state)
        (is (= 1 (:click (get-challenger)))))))
  (testing "If the challenger discards with one click left, the ability to run is enabled"
    (do-game
      (new-game (default-contestant ["Ruhr Valley"])
                (default-challenger))
      (play-from-hand state :contestant "Ruhr Valley" "HQ")
      (take-credits state :contestant)
      (let [ruhr (get-content state :hq 0)]
        (core/reveal state :contestant ruhr)
        (is (= 4 (:click (get-challenger))))
        (run-on state :rd)
        (run-jack-out state)
        (is (= 3 (:click (get-challenger))))
        (run-on state :hq)
        (run-successful state)
        (click-prompt state :challenger "Pay 4 [Credits] to discard") ; pay to discard / 6 cr - 4 cr
        (is (= 1 (:click (get-challenger))))
        (run-on state :hq)))))

(deftest ryon-knight
  ;; Ryon Knight - Discard during run to do 1 brain damage if Challenger has no clicks remaining
  (do-game
    (new-game (default-contestant ["Ryon Knight"])
              (default-challenger))
    (play-from-hand state :contestant "Ryon Knight" "HQ")
    (take-credits state :contestant)
    (let [ryon (get-content state :hq 0)]
      (run-on state :hq)
      (core/reveal state :contestant ryon)
      (card-ability state :contestant ryon 0)
      (is (= 3 (:click (get-challenger))))
      (is (zero? (:brain-damage (get-challenger))))
      (is (= 1 (count (get-content state :hq))) "Ryon ability didn't fire with 3 Challenger clicks left")
      (run-jack-out state)
      (take-credits state :challenger 2)
      (run-on state :hq)
      (card-ability state :contestant ryon 0)
      (is (zero? (:click (get-challenger))))
      (is (= 1 (:brain-damage (get-challenger))) "Did 1 brain damage")
      (is (= 1 (count (:discard (get-contestant)))) "Ryon discarded"))))

(deftest satellite-grid
  ;; Satellite Grid - Add 1 fake advancement on all Character protecting locale
  (do-game
    (new-game (default-contestant ["Satellite Grid" (qty "Ice Wall" 2)])
              (default-challenger))
    (play-from-hand state :contestant "Satellite Grid" "HQ")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (let [iw1 (get-character state :hq 0)
          iw2 (get-character state :rd 0)
          sg (get-content state :hq 0)]
      (core/gain state :contestant :click 1)
      (advance state iw1)
      (core/reveal state :contestant sg)
      (core/reveal state :contestant (refresh iw1))
      (is (= 1 (:extra-advance-counter (refresh iw1))) "1 fake advancement token")
      (is (= 1 (get-counters (refresh iw1) :advancement)) "Only 1 real advancement token")
      (is (= 3 (:current-strength (refresh iw1))) "Satellite Grid counter boosting strength by 1")
      (core/reveal state :contestant (refresh iw2))
      (is (= 1 (:current-strength (refresh iw2))) "Satellite Grid not impacting Character elsewhere")
      (core/hide state :contestant sg)
      (is (= 2 (:current-strength (refresh iw1))) "Ice Wall strength boost only from real advancement"))))

(deftest self-destruct
  ;; Self-destruct
  (do-game
    (new-game (default-contestant ["Self-destruct" "Dedicated Response Team" "Ice Wall"])
              (default-challenger))
    (core/gain state :contestant :credit 100 :click 4)
    (play-from-hand state :contestant "Self-destruct" "New party")
    (play-from-hand state :contestant "Dedicated Response Team" "Locale 1")
    (play-from-hand state :contestant "Ice Wall" "Locale 1")
    (let [self (get-content state :party1 0)]
      (take-credits state :contestant)
      (run-on state "Locale 1")
      (card-ability state :contestant self 0)
      (is (= 3 (-> (get-contestant) :discard count)) "All 3 cards from Locale 1 should be in discard")
      (is (= 2 (-> (get-contestant) :prompt first :base)) "Self-destruct base trace should start at 2")
      (is (zero? (-> (get-challenger) :discard count)) "Challenger should have no cards in heap")
      (click-prompt state :contestant "0")
      (click-prompt state :challenger "0")
      (is (= 3 (-> (get-challenger) :discard count)) "Challenger should take 3 net damage from losing Self-destruct trace"))))

(deftest signal-jamming
  ;; Discard to stop places for the rest of the run
  (do-game
    (new-game (default-contestant [(qty "Signal Jamming" 3)])
              (default-challenger [(qty "Self-modifying Code" 3) "Reaver"]))
    (starting-hand state :challenger ["Self-modifying Code" "Self-modifying Code"])
    (play-from-hand state :contestant "Signal Jamming" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Self-modifying Code")
    (play-from-hand state :challenger "Self-modifying Code")
    (let [smc1 (get-resource state 0)
          smc2 (get-resource state 1)
          sj (get-content state :hq 0)]
      (core/reveal state :contestant sj)
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :contestant sj 0)
      (card-ability state :challenger smc1 0)
      (is (empty? (:prompt (get-challenger))) "SJ blocking SMC")
      (run-jack-out state)
      (card-ability state :challenger smc2 0)
      (click-prompt state :challenger "Reaver"))))

(deftest strongbox
  ;; Strongbox
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Strongbox" "House of Knives"])
                (default-challenger))
      (play-from-hand state :contestant "Strongbox" "New party")
      (play-from-hand state :contestant "House of Knives" "Locale 1")
      (take-credits state :contestant 1)
      (let [sb (get-content state :party1 0)
            hok (get-content state :party1 1)]
        (core/reveal state :contestant sb)
        (run-empty-locale state "Locale 1")
        (click-card state :challenger hok)
        (is (= "House of Knives" (:title (:card (first (:prompt (get-challenger))))))
            "Prompt to pay 5cr")
        (click-prompt state :challenger "No action")
        (is (= 3 (:click (get-challenger))) "Challenger was not charged 1click")
        (is (zero? (count (:scored (get-challenger)))) "No scored agendas")
        (click-card state :challenger sb)
        (click-prompt state :challenger "No action")
        (run-empty-locale state "Locale 1")
        (click-card state :challenger hok)
        (click-prompt state :challenger "Pay [Click] to steal")
        (is (= 1 (:click (get-challenger))) "Challenger was charged 1click")
        (is (= 1 (count (:scored (get-challenger)))) "1 scored agenda"))))
  (testing "Click cost even when discarded"
    (do-game
      (new-game (default-contestant [(qty "Strongbox" 3) (qty "House of Knives" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Strongbox" "New party")
      (play-from-hand state :contestant "House of Knives" "Locale 1")
      (take-credits state :contestant 1)
      (core/gain state :challenger :credit 1)
      (let [sb (get-content state :party1 0)
            hok (get-content state :party1 1)]
        (core/reveal state :contestant sb)
        (run-empty-locale state "Locale 1")
        (click-card state :challenger sb)
        (click-prompt state :challenger "Pay 1 [Credits] to discard") ; pay to discard
        (click-card state :challenger hok)
        (click-prompt state :challenger "Pay [Click] to steal")
        (is (= 2 (:click (get-challenger))) "Challenger was charged 1click")
        (is (= 1 (count (:scored (get-challenger)))) "1 scored agenda")))))

(deftest surat-city-grid
  ;; Surat City Grid - Trigger on reveal of a card in/protecting same locale to reveal another card at 2c discount
  (do-game
    (new-game (default-contestant [(qty "Surat City Grid" 2) (qty "Cyberdex Virus Suite" 2)
                             "Enigma" "Wraparound"])
              (default-challenger))
    (core/gain state :contestant :credit 15 :click 8)
    (play-from-hand state :contestant "Surat City Grid" "New party")
    (play-from-hand state :contestant "Wraparound" "Locale 1")
    (play-from-hand state :contestant "Cyberdex Virus Suite" "Locale 1")
    (let [scg1 (get-content state :party1 0)
          cvs1 (get-content state :party1 1)
          wrap (get-character state :party1 0)]
      (core/reveal state :contestant scg1)
      (core/reveal state :contestant cvs1)
      (is (= 15 (:credit (get-contestant))))
      (is (= (:cid scg1) (-> (get-contestant) :prompt first :card :cid)) "Surat City Grid triggered from region in same party")
      (click-prompt state :contestant "Yes")
      (click-card state :contestant wrap)
      (is (:revealed (refresh wrap)) "Wraparound is revealed")
      (is (= 15 (:credit (get-contestant))) "Wraparound revealed for free with 2c discount from SCG")
      (play-from-hand state :contestant "Surat City Grid" "HQ")
      (play-from-hand state :contestant "Enigma" "HQ")
      (play-from-hand state :contestant "Cyberdex Virus Suite" "HQ")
      (let [scg2 (get-content state :hq 0)
            cvs2 (get-content state :hq 1)
            enig (get-character state :hq 0)]
        (core/reveal state :contestant scg2)
        (core/reveal state :contestant cvs2)
        (is (empty? (:prompt (get-contestant))) "SCG didn't trigger, regions in root of same central aren't considered in locale")
        (core/hide state :contestant (refresh wrap))
        (core/reveal state :contestant enig)
        (is (= (:cid scg2) (-> (get-contestant) :prompt first :card :cid)) "SCG did trigger for Character protecting HQ")))))

(deftest tempus
  ;; Tempus - Trace^3, the challenger chooses to lose 2 clicks or take 1 brain damage
  (do-game
    (new-game (default-contestant [(qty "Tempus" 3)])
              (default-challenger [(qty "Sure Gamble" 3)]))
    (starting-hand state :contestant ["Tempus"])
    (play-from-hand state :contestant "Tempus" "New party")
    (take-credits state :contestant)
    (run-on state "R&D")
    (run-successful state)
    (click-prompt state :contestant "0") ; trace
    (click-prompt state :challenger "0")
    (is (= 3 (:click (get-challenger))) "Challenger starts with 3 clicks")
    (click-prompt state :challenger "Lose [Click][Click]")
    (is (= 1 (:click (get-challenger))) "Challenger loses 2 clicks")
    (click-prompt state :challenger "Pay 0 [Credits] to discard") ; discard
    (run-on state "Locale 1")
    (run-successful state)
    (click-prompt state :contestant "0") ; trace
    (is (zero? (:brain-damage (get-challenger))) "Challenger starts with 0 brain damage")
    (click-prompt state :challenger "0")
    (is (= 1 (:brain-damage (get-challenger))) "Challenger took 1 brain damage")
    (click-prompt state :challenger "Pay 0 [Credits] to discard") ; discard
    (take-credits state :challenger)
    (take-credits state :contestant)
    (run-on state "Archives")
    (run-successful state)
    (is (= 1 (:brain-damage (get-challenger))) "Challenger takes no brain damage")
    (is (= 3 (:click (get-challenger))) "Challenger loses no clicks")
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :contestant "0") ; trace
    (click-prompt state :challenger "0")
    (is (= 1 (:brain-damage (get-challenger))) "Challenger starts with 1 brain damage")
    (click-prompt state :challenger "Take 1 brain damage")
    (is (= 2 (:brain-damage (get-challenger))) "Challenger took 1 brain damage")
    (click-prompt state :challenger "No action") ; don't discard
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :contestant "0") ; trace
    (click-prompt state :challenger "4")
    (click-prompt state :challenger "Pay 0 [Credits] to discard")))

(deftest the-twins
  ;; The Twins
  (do-game
    (new-game (default-contestant ["The Twins" (qty "Ice Wall" 10)])
              (default-challenger ["Corroder"]))
    (starting-hand state :contestant ["The Twins" "Ice Wall" "Ice Wall"])
    (play-from-hand state :contestant "The Twins" "New party")
    (play-from-hand state :contestant "Ice Wall" "Locale 1")
    (let [twins (get-content state :party1 0)
          iw (get-character state :party1 0)]
      (core/reveal state :contestant twins)
      (core/reveal state :contestant iw)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Corroder")
      (let [cor (get-resource state 0)]
        (run-on state "Locale 1")
        (card-ability state :challenger cor 0)
        (run-continue state)
        (is (zero? (-> @state :run :position)) "Run should be at position 0")
        (card-ability state :contestant twins 0)
        (click-card state :contestant (-> (get-contestant) :hand first))
        (is (= 1 (-> @state :run :position)) "Run should be moved back to position 1")))))

(deftest tori-hanzo
  ;; Tori Hanzō - Pay to do 1 brain damage instead of net damage
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Pup" "Tori Hanzō"])
                (default-challenger [(qty "Sure Gamble" 3) "Net Shield"]))
      (core/gain state :contestant :credit 10)
      (play-from-hand state :contestant "Pup" "HQ")
      (play-from-hand state :contestant "Tori Hanzō" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Net Shield")
      (run-on state "HQ")
      (let [pup (get-character state :hq 0)
            tori (get-content state :hq 0)
            nshld (get-resource state 0)]
        (core/reveal state :contestant pup)
        (core/reveal state :contestant tori)
        (card-subroutine state :contestant pup 0)
        (card-ability state :challenger nshld 0)
        (click-prompt state :challenger "Done")
        (is (empty? (:discard (get-challenger))) "1 net damage prevented")
        (card-subroutine state :contestant pup 0)
        (click-prompt state :challenger "Done") ; decline to prevent
        (is (= 1 (count (:discard (get-challenger)))) "1 net damage; previous prevention stopped Tori ability")
        (run-jack-out state)
        (run-on state "HQ")
        (card-subroutine state :contestant pup 0)
        (click-prompt state :challenger "Done")
        (click-prompt state :contestant "Yes")
        (is (= 2 (count (:discard (get-challenger)))) "1 brain damage suffered")
        (is (= 1 (:brain-damage (get-challenger)))))))
  (testing "with Hokusai Grid: Issue #2702"
    (do-game
      (new-game (default-contestant ["Tori Hanzō" "Hokusai Grid"])
                (default-challenger))
      (core/gain state :contestant :credit 5)
      (play-from-hand state :contestant "Hokusai Grid" "Archives")
      (play-from-hand state :contestant "Tori Hanzō" "Archives")
      (take-credits state :contestant)
      (run-on state "Archives")
      (let [hg (get-content state :archives 0)
            tori (get-content state :archives 1)]
        (core/reveal state :contestant hg)
        (core/reveal state :contestant tori)
        (run-successful state)
        (click-prompt state :contestant "No") ; Tori prompt to pay 2c to replace 1 net with 1 brain
        (is (= 1 (count (:discard (get-challenger)))) "1 net damage suffered")
        (click-prompt state :challenger "Hokusai Grid")
        (click-prompt state :challenger "No action")
        (click-prompt state :challenger "Tori Hanzō")
        (click-prompt state :challenger "No action")
        (is (and (empty (:prompt (get-challenger))) (not (:run @state))) "No prompts, run ended")
        (run-empty-locale state "Archives")
        (click-prompt state :contestant "Yes") ; Tori prompt to pay 2c to replace 1 net with 1 brain
        (is (= 2 (count (:discard (get-challenger)))))
        (is (= 1 (:brain-damage (get-challenger))) "1 brain damage suffered")
        (click-prompt state :challenger "Hokusai Grid")
        (click-prompt state :challenger "No action")
        (click-prompt state :challenger "Tori Hanzō")
        (click-prompt state :challenger "No action")
        (is (and (empty (:prompt (get-challenger))) (not (:run @state))) "No prompts, run ended"))))
  (testing "breaking subsequent net damage: Issue #3176"
    (do-game
      (new-game (default-contestant ["Tori Hanzō" (qty "Pup" 2) (qty "Neural EMP" 2)])
                (default-challenger))
      (core/gain state :contestant :credit 8)
      (play-from-hand state :contestant "Tori Hanzō" "New party")
      (play-from-hand state :contestant "Pup" "Locale 1")
      (take-credits state :contestant)
      (run-on state "Locale 1")
      (let [tori (get-content state :party1 0)
            pup (get-character state :party1 0)]
        (core/reveal state :contestant pup)
        (core/reveal state :contestant tori)
        (card-subroutine state :contestant pup 0)
        (click-prompt state :contestant "Yes") ; pay 2c to replace 1 net with 1 brain
        (is (= 1 (count (:discard (get-challenger)))) "1 brain damage suffered")
        (is (= 1 (:brain-damage (get-challenger))))
        (run-jack-out state)
        (take-credits state :challenger)
        (play-from-hand state :contestant "Neural EMP")
        (is (= 2 (count (:discard (get-challenger)))) "Net damage processed correctly")))))

(deftest underway-grid
  ;; Underway Grid - prevent expose of cards in locale
  (do-game
    (new-game (default-contestant ["Eve Campaign"
                             "Underway Grid"])
              (default-challenger ["Drive By"]))
    (play-from-hand state :contestant "Underway Grid" "New party")
    (play-from-hand state :contestant "Eve Campaign" "Locale 1")
    (take-credits state :contestant)
    (core/reveal state :contestant (get-content state :party1 0))
    (let [eve1 (get-content state :party1 1)]
      (play-from-hand state :challenger "Drive By")
      (click-card state :challenger eve1)
      (is (empty? (:discard (get-contestant))) "Expose and discard prevented"))))

(deftest valley-grid
  ;; Valley Grid
  (testing "Reduce Challenger max hand size and restore it even if discarded"
    (do-game
      (new-game (default-contestant [(qty "Valley Grid" 3) (qty "Ice Wall" 3)])
                (default-challenger))
      (play-from-hand state :contestant "Valley Grid" "New party")
      (take-credits state :contestant 2)
      (run-on state "Locale 1")
      (let [vg (get-content state :party1 0)]
        (core/reveal state :contestant vg)
        (card-ability state :contestant vg 0)
        (card-ability state :contestant vg 0) ; only need the run to exist for test, just pretending the Challenger has broken all subs on 2 character
        (is (= 3 (core/hand-size state :challenger)) "Challenger max hand size reduced by 2")
        (is (= 2 (get-in (refresh vg) [:times-used])) "Saved number of times Valley Grid used")
        (run-successful state)
        (click-prompt state :challenger "Pay 3 [Credits] to discard") ; pay to discard
        (take-credits state :challenger 3)
        (is (= 5 (core/hand-size state :challenger)) "Challenger max hand size increased by 2 at start of Contestant turn")))))

(deftest warroid-tracker
  ;; Warroid Tracker
  (testing "Discarding Warroid starts trace"
    (do-game
      (new-game (default-contestant ["Warroid Tracker"])
                (default-challenger ["Corroder" "Dyson Mem Chip"]))
      (play-from-hand state :contestant "Warroid Tracker" "New party")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Corroder")
      (play-from-hand state :challenger "Dyson Mem Chip")
      (let [war (get-content state :party1 0)
            cor (get-resource state 0)
            mem (get-hazard state 0)]
        (core/reveal state :contestant war)
        (run-empty-locale state "Locale 1")
        (click-prompt state :challenger "Pay 4 [Credits] to discard")
        (click-prompt state :contestant "0")
        (click-prompt state :challenger "0")
        (is (zero? (-> (get-challenger) :discard count)) "Challenger should start with 0 cards in heap")
        (click-card state :challenger cor)
        (click-card state :challenger mem)
        (is (= 2 (-> (get-challenger) :discard count)) "Challenger should discard 2 placed cards"))))
  (testing "Discarding from central triggers Warroid in root"
    ;; Regression test for #3725
    (do-game
      (new-game (default-contestant ["Warroid Tracker" (qty "Hedge Fund" 3)])
                (default-challenger ["Clan Vengeance" "Corroder" "Dyson Mem Chip"]))
      (play-from-hand state :contestant "Warroid Tracker" "HQ")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Corroder")
      (play-from-hand state :challenger "Dyson Mem Chip")
      (play-from-hand state :challenger "Clan Vengeance")
      (let [war (get-content state :hq 0)
            clv (get-radicle state 0)
            cor (get-resource state 0)
            mem (get-hazard state 0)]
        (core/reveal state :contestant war)
        (core/add-counter state :challenger clv :power 2)
        (card-ability state :challenger (refresh clv) 0)
        ;; Prompt choice checks there is a trace prompt from Warroid
        (click-prompt state :contestant "0")
        (click-prompt state :challenger "0")
        (is (= 1 (-> (get-challenger) :discard count)) "Challenger should start with 1 card in heap (Clan Vengeance)")
        (click-card state :challenger cor)
        (click-card state :challenger mem)
        (is (= 3 (-> (get-challenger) :discard count)) "Challenger should discard 2 placed cards (and CV already in heap)")
        (is (= 2 (count (:discard (get-contestant)))) "Two cards discarded from HQ by Clan Vengeance")))))
