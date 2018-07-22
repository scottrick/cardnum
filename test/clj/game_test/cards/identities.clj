(ns game-test.cards.identities
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "identities"))

(deftest ^{:card-title "419:-amoral-scammer"}
  FourHundredAndNineTeen-amoral-scammer
  ;; 419
  (testing "basic test: Amoral Scammer - expose first placed card unless contestant pays 1 credit"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations"
                   ["PAD Campaign" "The Cleaners" (qty "Pup" 3) "Oaktown Renovation"])
        (make-deck "419: Amoral Scammer" []))
      (is (= 5 (:credit (get-contestant))) "Starts with 5 credits")
      (play-from-hand state :contestant "Pup" "HQ")
      (prompt-choice :challenger "Yes")
      (prompt-choice :contestant "Yes")
      (is (= 4 (:credit (get-contestant))) "Pays 1 credit to not expose card")
      (play-from-hand state :contestant "Pup" "HQ")
      (is (empty? (:prompt (get-challenger))) "No option on second place")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (play-from-hand state :contestant "Pup" "Archives")
      (prompt-choice :challenger "No")
      (is (empty? (:prompt (get-contestant))) "No prompt if Challenger chooses No")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (play-from-hand state :contestant "The Cleaners" "New party")
      (prompt-choice :challenger "Yes")
      (prompt-choice :contestant "No")
      (is (last-log-contains? state "exposes The Cleaners") "Placed card was exposed")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (play-from-hand state :contestant "Oaktown Renovation" "New party")
      (is (empty? (:prompt (get-contestant))) "Cannot expose faceup agendas")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (core/lose state :contestant :credit (:credit (get-contestant)))
      (is (zero? (:credit (get-contestant))) "Contestant has no credits")
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (prompt-choice :challenger "Yes")
      (is (empty? (:prompt (get-contestant))) "No prompt if Contestant has no credits")
      (is (last-log-contains? state "exposes PAD Campaign") "Placed card was exposed")))
  (testing "Verify expose can be blocked"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations" ["Underway Grid" "Pup"])
        (make-deck "419: Amoral Scammer" []))
      (play-from-hand state :contestant "Underway Grid" "New party")
      (prompt-choice :challenger "No")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (play-from-hand state :contestant "Pup" "Locale 1")
      (prompt-choice :challenger "Yes")
      (let [ug (get-in @state [:contestant :locales :party1 :content 0])]
        (core/reveal state :contestant ug)
        (prompt-choice :contestant "No")
        (is (last-log-contains? state "uses Underway Grid to prevent 1 card from being exposed") "Exposure was prevented"))))
  (testing "Ixodidae shouldn't trigger off 419's ability"
    (do-game
      (new-game (default-contestant ["PAD Campaign"])
                (make-deck "419: Amoral Scammer" ["Ixodidae"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Ixodidae")
      (take-credits state :challenger)
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (let [contestant-credits (:credit (get-contestant))
            challenger-credits (:credit (get-challenger))]
        (prompt-choice :challenger "Yes")
        (prompt-choice :contestant "Yes")
        (is (= 1 (- contestant-credits (:credit (get-contestant)))) "Should lose 1 credit from 419 ability")
        (is (zero? (- challenger-credits (:credit (get-challenger)))) "Should not gain any credits from Ixodidae")))))

(deftest adam:-compulsive-hacker
  ;; Adam
  (testing "Allow challenger to choose directives"
    (do-game
      (new-game
        (default-contestant)
        (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
        {:dont-start-game true})
      (is (= 4 (count (get-in @state [:challenger :play-area]))) "All directives are in the challenger's play area")
      (is (zero? (count (get-in @state [:challenger :hand]))))
      (prompt-select :challenger (find-card "Neutralize All Threats" (get-in @state [:challenger :play-area])))
      (prompt-select :challenger (find-card "Safety First" (get-in @state [:challenger :play-area])))
      (prompt-select :challenger (find-card "Always Be Running" (get-in @state [:challenger :play-area])))
      (is (= 3 (count (get-radicle state))) "3 directives were placed")
      (is (zero? (count (get-in @state [:challenger :play-area]))) "The play area is empty")
      (let [nat (find-card "Neutralize All Threats" (get-radicle state))
            sf (find-card "Safety First" (get-radicle state))
            abr (find-card "Always Be Running" (get-radicle state))]
        (is (and nat sf abr) "The chosen directives were placed"))))
  (testing "Directives should not grant Pālanā credits"
    (do-game
      (new-game
        (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
        (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
        {:dont-start-game true})
      (prompt-select :challenger (find-card "Neutralize All Threats" (get-in @state [:challenger :play-area])))
      (prompt-select :challenger (find-card "Safety First" (get-in @state [:challenger :play-area])))
      (prompt-select :challenger (find-card "Always Be Running" (get-in @state [:challenger :play-area])))
      (prompt-choice :contestant "Keep")
      (prompt-choice :challenger "Keep")
      (core/start-turn state :contestant nil)
      (is (= 5 (:credit (get-contestant))) "Pālanā does not gain credit from Adam's starting Directives")))
  (testing "Neutralize All Threats interaction with advanceable traps"
    (do-game
      (new-game
        (default-contestant [(qty "Cerebral Overwriter" 3)])
        (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
        {:dont-start-game true})
      (prompt-select :challenger (find-card "Neutralize All Threats" (get-in @state [:challenger :play-area])))
      (prompt-select :challenger (find-card "Safety First" (get-in @state [:challenger :play-area])))
      (prompt-select :challenger (find-card "Always Be Running" (get-in @state [:challenger :play-area])))
      (prompt-choice :contestant "Keep")
      (prompt-choice :challenger "Keep")
      (core/start-turn state :contestant nil)
      (play-from-hand state :contestant "Cerebral Overwriter" "New party")
      (advance state (get-content state :party1 0) 2)
      (take-credits state :contestant)
      (run-empty-locale state :party1)
      (prompt-choice :contestant "Yes")
      (prompt-choice-partial :challenger "Pay")
      (is (= 2 (:brain-damage (get-challenger))) "Challenger took 2 brain damage")
      (is (= 1 (count (:discard (get-contestant)))) "1 card in archives"))))

(deftest andromeda:-dispossessed-ristie
  ;; Andromeda - 9 card starting hand, 1 link
  (testing "Basic test"
    (do-game
      (new-game
        (default-contestant)
        (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                     (qty "Security Testing" 3) (qty "Bank Job" 3)]))
      (is (= 1 (:link (get-challenger))) "1 link")
      (is (= 9 (count (:hand (get-challenger)))) "9 cards in Andromeda starting hand")))
  (testing "9 card starting hand after mulligan"
    (do-game
      (new-game
        (default-contestant)
        (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                     (qty "Security Testing" 3) (qty "Bank Job" 3)])
        {:mulligan :challenger})
      (is (= 1 (:link (get-challenger))) "1 link")
      (is (= 9 (count (:hand (get-challenger)))) "9 cards in Andromeda starting hand")))
  (testing "should not grant Palana credits"
    (do-game
      (new-game
        (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
        (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                     (qty "Security Testing" 3) (qty "Bank Job" 3)]))
      (is (= 5 (:credit (get-contestant))) "Palana does not gain credit from Andromeda's starting hand"))))

(deftest apex:-invasive-predator
  ;; Apex - Allow facedown place of a second console. Issue #1326
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Apex: Invasive Predator" [(qty "Heartbeat" 2)]))
    (take-credits state :contestant)
    (core/end-phase-12 state :challenger nil)
    (prompt-choice :challenger "Done") ; no facedown place on turn 1
    (play-from-hand state :challenger "Heartbeat")
    (is (= 1 (count (get-hazard state))))
    (take-credits state :challenger)
    (take-credits state :contestant)
    (core/end-phase-12 state :challenger nil)
    (prompt-select :challenger (find-card "Heartbeat" (:hand (get-challenger))))
    (is (= 1 (count (get-challenger-facedown state))) "2nd console placed facedown")))

(deftest asa-group:-security-through-vigilance
  (testing "Asa Group should not allow placing operations"
    (do-game
      (new-game
        (make-deck "Asa Group: Security Through Vigilance" ["Pup" "BOOM!" "Urban Renewal"])
        (default-challenger))
      (play-from-hand state :contestant "Pup" "New party")
      (prompt-select :contestant (find-card "BOOM!" (:hand (get-contestant))))
      (is (empty? (get-content state :party1)) "Asa Group placed an event in a locale")
      (prompt-select :contestant (find-card "Urban Renewal" (:hand (get-contestant))))
      (is (= "Urban Renewal" (:title (get-content state :party1 0))) "Asa Group can place an site in a party")))
  (testing "Asa Group should not allow placing agendas"
    (do-game
      (new-game
        (make-deck "Asa Group: Security Through Vigilance" ["Pup" "Project Vitruvius" "Urban Renewal"])
        (default-challenger))
      (play-from-hand state :contestant "Pup" "New party")
      (prompt-select :contestant (find-card "Project Vitruvius" (:hand (get-contestant))))
      (is (empty? (get-content state :party1)) "Asa Group did not place Agenda with its ability")
      (prompt-select :contestant (find-card "Urban Renewal" (:hand (get-contestant))))
      (is (= "Urban Renewal" (:title (get-content state :party1 0))) "Asa Group can place an site in a party")))
  (testing "Asa Group ordering correct when playing Mirrormontestanth"
    (do-game
      (new-game
        (make-deck "Asa Group: Security Through Vigilance" ["Shipment from MirrorMontestanth"
                                                            "Pup"
                                                            "Red Herrings"
                                                            "Marilyn Campaign"
                                                            "Project Vitruvius"])
        (default-challenger))
      (let  [marilyn (find-card "Marilyn Campaign" (:hand (get-contestant)))
             pup (find-card "Pup" (:hand (get-contestant)))
             herrings (find-card "Red Herrings" (:hand (get-contestant)))
             vitruvius (find-card "Project Vitruvius" (:hand (get-contestant)))]
        (play-from-hand state :contestant "Shipment from MirrorMontestanth")
        (prompt-select :contestant marilyn)
        (prompt-choice :contestant "New party")
        (is (= (:cid marilyn) (:cid (get-content state :party1 0))) "Marilyn is placed as first card")
        (prompt-select :contestant herrings) ;; This should be the Asa prompt, should be automatically placed in party1
        (is (= (:cid herrings) (:cid (get-content state :party1 1))) "Red Herrings is placed in Locale 1")
        (prompt-select :contestant vitruvius)
        (prompt-choice :contestant "New party")
        (prompt-select :contestant pup)
        (prompt-choice :contestant "New party")
        (is (empty? (:prompt (get-contestant))) "No more prompts")
        (is (= 6 (count (:locales (get-contestant)))) "There are six locales, including centrals"))))
  (testing "don't allow placeation of operations"
    (do-game
      (new-game
        (make-deck "Asa Group: Security Through Vigilance" ["Pup" "BOOM!" "Urban Renewal"])
        (default-challenger))
      (play-from-hand state :contestant "Pup" "New party")
      (prompt-select :contestant (find-card "BOOM!" (:hand (get-contestant))))
      (is (empty? (get-content state :party1)) "Asa Group placed an event in a locale")
      (prompt-select :contestant (find-card "Urban Renewal" (:hand (get-contestant))))
      (is (= "Urban Renewal" (:title (get-content state :party1 0))) "Asa Group can place an site in a party"))))

(deftest ^{:card-title "ayla-\"bios\"-rahim:-simulant-specialist"}
  ayla
  ;; Ayla - choose & use cards for NVRAM
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Ayla \"Bios\" Rahim: Simulant Specialist" ["Sure Gamble" "Desperado"
                                                             "Security Testing" "Bank Job"
                                                             "Heartbeat" "Eater"])
      {:dont-start-game true})
    (is (= 6 (count (get-in @state [:challenger :play-area]))) "Deck cards are in play area")
    (is (zero? (count (get-in @state [:challenger :hand]))))
    (prompt-select :challenger (find-card "Sure Gamble" (get-in @state [:challenger :play-area])))
    (prompt-select :challenger (find-card "Desperado" (get-in @state [:challenger :play-area])))
    (prompt-select :challenger (find-card "Bank Job" (get-in @state [:challenger :play-area])))
    (prompt-select :challenger (find-card "Eater" (get-in @state [:challenger :play-area])))
    (is (= 4 (count (:hosted (:identity (get-challenger))))) "4 cards in NVRAM")
    (is (zero? (count (get-in @state [:challenger :play-area]))) "The play area is empty")
    (prompt-choice :contestant "Keep")
    (prompt-choice :challenger "Keep")
    (take-credits state :contestant)
    (is (= 2 (count (get-in @state [:challenger :hand]))) "There are 2 cards in the challenger's Grip")
    (card-ability state :challenger (:identity (get-challenger)) 0)
    (prompt-card :challenger (find-card "Bank Job" (:hosted (:identity (get-challenger)))))
    (is (= 3 (count (get-in @state [:challenger :hand]))) "There are 3 cards in the challenger's Grip")))

(deftest cerebral-imaging:-infinite-frontiers
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game
      (make-deck "Cerebral Imaging: Infinite Frontiers" [(qty "Hedge Fund" 3)])
      (default-challenger))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 13 (:credit (get-contestant))) "Has 13 credits")
    (is (= 13 (core/hand-size state :contestant)) "Max hand size is 13")))

(deftest chaos-theory:-wunderkind
  ;; Chaos Theory, start with +1 MU
  (do-game
    (new-game (default-contestant)
              (make-deck "Chaos Theory: Wünderkind" []))
    (is (= 5 (core/available-mu state)) "Chaos Theory starts the game with +1 MU")))

(deftest chronos-protocol:-selective-mind-mapping
  ;; Chronos Protocol - Choose Challenger discard for first net damage of a turn
  (testing "Basic test"
    (do-game
      (new-game
        (make-deck "Chronos Protocol: Selective Mind-mapping" ["Pup" (qty "Neural EMP" 2)])
        (default-challenger [(qty "Imp" 3)]))
      (play-from-hand state :contestant "Pup" "HQ")
      (take-credits state :contestant)
      (run-on state :hq)
      (let [pup (get-character state :hq 0)]
        (core/reveal state :contestant pup)
        (card-subroutine state :contestant pup 0)
        (prompt-choice :contestant "Yes")
        (let [imp (find-card "Imp" (:hand (get-challenger)))]
          (prompt-choice :contestant imp)
          (is (= 1 (count (:discard (get-challenger)))))
          (card-subroutine state :contestant pup 0)
          (is (empty? (:prompt (get-contestant))) "No choice on second net damage")
          (is (= 2 (count (:discard (get-challenger)))))
          (run-jack-out state)
          (take-credits state :challenger)
          (core/move state :challenger (find-card "Imp" (:discard (get-challenger))) :hand)
          (play-from-hand state :contestant "Neural EMP")
          (prompt-choice :contestant "No")
          (is (= 2 (count (:discard (get-challenger)))) "Damage dealt after declining ability")
          (play-from-hand state :contestant "Neural EMP")
          (is (empty? (:prompt (get-contestant))) "No choice after declining on first damage")
          (is (= 3 (count (:discard (get-challenger)))))))))
  (testing "with Obokata: Pay 4 net damage to steal.  Only 3 damage left after Chronos.  No trigger of damage prevent."
    (do-game
      (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Obokata Protocol" 5)])
                (default-challenger [(qty "Sure Gamble" 3) "Inti" "Feedback Filter"]))
      (core/gain state :challenger :credit 10)
      (play-from-hand state :contestant "Obokata Protocol" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Feedback Filter")
      (run-empty-locale state "Locale 1")
      (prompt-choice-partial :challenger "Pay")
      (prompt-choice :contestant "Yes")
      (prompt-card :contestant (find-card "Inti" (:hand (get-challenger))))
      (is (empty? (:prompt (get-challenger))) "Feedback Filter net damage prevention opportunity not given")
      (is (= 4 (count (:discard (get-challenger)))) "Challenger paid 4 net damage")))
  (testing "vs Employee Strike. Issue #1958"
    (do-game
      (new-game
        (make-deck "Chronos Protocol: Selective Mind-mapping" ["Pup"])
        (default-challenger ["Employee Strike" (qty "Scrubbed" 3) "Sure Gamble"]))
      (play-from-hand state :contestant "Pup" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Employee Strike")
      (run-on state :hq)
      (let [pup (get-character state :hq 0)]
        (core/reveal state :contestant pup)
        (card-subroutine state :contestant pup 0)
        (is (empty? (:prompt (get-contestant))) "No choice because of Employee Strike")
        (card-subroutine state :contestant pup 0)
        (is (= 2 (count (:discard (get-challenger)))))
        (run-jack-out state)
        (take-credits state :challenger)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Scrubbed")
        (run-on state :hq)
        (card-subroutine state :contestant pup 0)
        (is (not (empty? (:prompt (get-contestant)))) "Employee Strike out of play - Ability turned on correctly")))))

(deftest edward-kim:-humanity's-hammer
  ;; Edward Kim
  (testing "Discard first operation accessed each turn, but not if first one was in Archives"
    (do-game
      (new-game
        (default-contestant [(qty "Hedge Fund" 3) (qty "Restructure" 2) "PAD Campaign"])
        (make-deck "Edward Kim: Humanity's Hammer" ["Eater" (qty "Sure Gamble" 2)]))
      (play-from-hand state :contestant "Hedge Fund")
      (discard-from-hand state :contestant "PAD Campaign")
      (take-credits state :contestant)
      (run-empty-locale state "Archives")
      (run-empty-locale state "HQ")
      (is (= 2 (count (:discard (get-contestant)))) "No operation discarded from HQ; accessed one in Archives first")
      (take-credits state :challenger)
      (core/move state :contestant (find-card "Hedge Fund" (:discard (get-contestant))) :hand)
      (is (= 1 (count (:discard (get-contestant)))))
      (take-credits state :contestant)
      (run-empty-locale state "Archives")
      (run-empty-locale state "HQ")
      (is (= 2 (count (:discard (get-contestant)))) "1 operation discarded from HQ; accessed non-operation in Archives first")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Hedge Fund")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Eater")
      (let [eater (get-resource state 0)]
        (run-on state "Archives")
        (card-ability state :challenger eater 0) ; pretend to break a sub so no cards in Archives will be accessed
        (run-successful state)
        (is (= 3 (count (:discard (get-contestant)))))
        (run-empty-locale state "HQ")
        (is (= 4 (count (:discard (get-contestant)))) "1 operation discarded from HQ; accessed non-operation in Archives first"))))
  (testing "Do not trigger maw on first Operation access (due to discard)"
    (do-game
      (new-game
        (default-contestant [(qty "Hedge Fund" 3) (qty "Restructure" 2)])
        (make-deck "Edward Kim: Humanity's Hammer" ["Maw" (qty "Sure Gamble" 2)]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sure Gamble")
      (play-from-hand state :challenger "Maw")
      (is (zero? (count (:discard (get-contestant)))) "No cards in Archives")
      (run-empty-locale state "HQ")
      (is (= 1 (count (:discard (get-contestant)))) "Only one card discarded from HQ, by Ed Kim")
      (run-empty-locale state "HQ")
      (prompt-choice :challenger "No action")
      (is (= 2 (count (:discard (get-contestant)))) "One more card discarded from HQ, by Maw"))))

(deftest exile:-streethawk
  ;; Exile
  (testing "Simultaneous-resolution prompt shown for interaction with Customized Secretary"
    (do-game
      (new-game
        (default-contestant)
        (make-deck "Exile: Streethawk" [(qty "Customized Secretary" 3) (qty "Clone Chip" 3)
                                        (qty "Sure Gamble" 3)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Customized Secretary" "Clone Chip"])
      (discard-from-hand state :challenger "Customized Secretary")
      (play-from-hand state :challenger "Clone Chip")
      (card-ability state :challenger (get-hazard state 0) 0)
      (prompt-select :challenger (find-card "Customized Secretary" (:discard (get-challenger))))
      ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
      (is (= 2 (-> (get-challenger) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
      (prompt-choice :challenger "Exile: Streethawk")
      (is (= 1 (count (:hand (get-challenger)))) "Exile drew a card"))))

(deftest freedom-khumalo:-crypto-anarchist
  ;; Freedom Khumalo - Can spend virus counters from other cards to discard accessed cards with play/reveal costs
  (testing "Only works with Sites, Character, Operations, and Regions"
    (letfn [(fk-test [card]
              (do-game
                (new-game (default-contestant [card])
                          (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Cache"]))
                (take-credits state :contestant)
                (play-from-hand state :challenger "Cache")
                (run-empty-locale state "HQ")
                (prompt-choice-partial :challenger "Freedom")
                (prompt-select :challenger (get-resource state 0))
                (prompt-select :challenger (get-resource state 0))
                (is (= 1 (count (:discard (get-contestant))))
                    (str "Accessed " card " should have been discarded after selecting two virus counters"))))]
      (doall (map fk-test
                  ["Dedicated Response Team"
                   "Consulting Visit"
                   "Builder"
                   "Research Station"]))))
  (testing "Triggers when play/reveal cost less than or equal to number of available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game (default-contestant [card])
                          (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Cache"]))
                (take-credits state :contestant)
                (play-from-hand state :challenger "Cache")
                (run-empty-locale state "HQ")
                (let [cost (->> (get-contestant) :hand first :cost)]
                  (prompt-choice-partial :challenger "Freedom")
                  (when (pos? cost)
                    (dotimes [_ cost]
                      (prompt-select :challenger (get-resource state 0))))
                  (is (= 1 (count (:discard (get-contestant))))
                      (str "Accessed " card " should have been discarded after selecting " cost " virus counters")))))]
      (doall (map fk-test
                  ["Beanstalk Royalties"
                   "Aggressive Negotiation"
                   "Consulting Visit"
                   "Door to Door"]))))
  (testing "Doesn't trigger when there aren't enough available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game (default-contestant [card])
                          (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Cache"]))
                (take-credits state :contestant)
                (play-from-hand state :challenger "Cache")
                (run-empty-locale state "HQ")
                (is (= 1 (-> @state :challenger :prompt first :choices count)) "Should only have 1 option")
                (is (= "No action" (-> @state :challenger :prompt first :choices first)) "Only option should be 'No action'")))]
      (doall (map fk-test
                  ["Archer"
                   "Fire Wall"
                   "Colossus"
                   "Tyrant"]))))
  (testing "Can use multiple resources for virus counter payment"
    (do-game
      (new-game (default-contestant ["Dedicated Response Team"])
                (make-deck "Freedom Khumalo: Crypto-Anarchist"
                           ["Cache" "Virus Breeding Ground"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Cache")
      (play-from-hand state :challenger "Virus Breeding Ground")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-empty-locale state "HQ")
      (prompt-choice-partial :challenger "Freedom")
      (prompt-select :challenger (get-resource state 0))
      (prompt-select :challenger (get-radicle state 0))
      (is (= 1 (count (:discard (get-contestant))))
          (str "Accessed Dedicated Response Team should have been discarded after selecting 2 virus counters"))))
  (testing "Can use viruses on hosted cards"
    (do-game
      (new-game (default-contestant [(qty "Ice Wall" 2)])
                (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Trypano"]))
      (play-from-hand state :contestant "Ice Wall" "R&D")
      (let [iw (get-character state :rd 0)]
        (take-credits state :contestant)
        (play-from-hand state :challenger "Trypano")
        (prompt-select :challenger (refresh iw))
        (take-credits state :challenger)
        (take-credits state :contestant)
        (prompt-choice :challenger "Yes")
        (run-empty-locale state "HQ")
        (prompt-choice-partial :challenger "Freedom")
        (prompt-select :challenger (-> (refresh iw) :hosted first)))
      (is (= 1 (count (:discard (get-contestant)))) "Accessed Ice Wall should be discarded after selecting 1 virus counter")))
  (testing "Doesn't trigger when accessing an Agenda"
    (do-game
      (new-game (default-contestant ["Hostile Takeover"])
                (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Cache"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Cache")
      (run-empty-locale state "HQ")
      (is (= 1 (->> @state :challenger :prompt first :choices count)) "Should only have 1 option")
      (is (= "Steal" (-> @state :challenger :prompt first :choices first)) "Only option should be 'Steal'")))
  (testing "Shows multiple prompts when playing Imp"
    (do-game
      (new-game (default-contestant ["Dedicated Response Team"])
                (make-deck "Freedom Khumalo: Crypto-Anarchist"
                           ["Sure Gamble" "Cache" "Imp"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sure Gamble")
      (play-from-hand state :challenger "Cache")
      (play-from-hand state :challenger "Imp")
      (run-empty-locale state "HQ")
      (is (= 4 (-> @state :challenger :prompt first :choices count)) "Should have 4 options: Freedom, Imp, Discard, No action")))
  (testing "Should return to access prompts when Done is pressed"
    (do-game
      (new-game (default-contestant ["Dedicated Response Team"])
                (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Cache"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Cache")
      (run-empty-locale state "HQ")
      (is (= 3 (->> @state :challenger :prompt first :choices count)) "Should have 3 choices: Freedom, Discard, No action")
      (prompt-choice-partial :challenger "Freedom")
      (prompt-select :challenger (get-resource state 0))
      (prompt-choice :challenger "Done")
      (is (= 3 (-> @state :challenger :prompt first :choices count))
          (str "Should go back to access prompts, with 3 choices: Freedom, Discard, No action. "
               "Chocharacters seen: " (-> @state :challenger :prompt first :choices)))
      (prompt-choice-partial :challenger "Freedom")
      (prompt-select :challenger (get-resource state 0))
      (prompt-select :challenger (get-resource state 0))
      (is (= 1 (count (:discard (get-contestant)))) "Card should now be properly discarded")))
  (testing "Shouldn't grant additional accesses after discarding accessed card. #3423"
    (do-game
      (new-game (default-contestant [(qty "Ice Wall" 10)])
                (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Cache"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Cache")
      (run-empty-locale state "R&D")
      (prompt-choice-partial :challenger "Freedom")
      (prompt-select :challenger (get-resource state 0))
      (is (= 1 (count (:discard (get-contestant)))) "Accessed Ice Wall should be discarded now")
      (is (not (:run @state)) "Run ended")))
  (testing "Shouldn't give Aumakua additional counters on discard. #3479"
    (do-game
      (new-game (default-contestant [(qty "Ice Wall" 10)])
                (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Cache" "Aumakua"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Cache")
      (play-from-hand state :challenger "Aumakua")
      (run-empty-locale state "R&D")
      (is (zero? (get-counters (get-resource state 1) :virus)) "Aumakuma shouldn't have any virus counters yet.")
      (prompt-choice-partial :challenger "Freedom")
      (prompt-select :challenger (get-resource state 0))
      (is (= 1 (count (:discard (get-contestant)))) "Ice Wall should be discarded now")
      (is (zero? (get-counters (get-resource state 1) :virus)) "Aumakua doesn't gain any virus counters from discard ability.")
      (is (not (:run @state)) "Run ended")))
  (testing "interaction with discard-cost-bonuses, and declining ability once initiated"
    (do-game
      (new-game (default-contestant ["The Board"])
                (make-deck "Freedom Khumalo: Crypto-Anarchist" ["Skulljack" "Imp" "Sure Gamble"]))
      (play-from-hand state :contestant "The Board" "New party")
      (take-credits state :contestant)
      (run-empty-locale state "Locale 1")
      (is (= 1 (-> (get-challenger) :prompt first :choices count)) "Challenger doesn't have enough credits to discard")
      (prompt-choice :challenger "No action")
      (play-from-hand state :challenger "Imp")
      (core/add-counter state :challenger (get-resource state 0) :virus 5)
      (play-from-hand state :challenger "Skulljack")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-empty-locale state "Locale 1")
      (is (= 6 (core/discard-cost state :challenger (get-content state :party1 0))) "The Board should cost 6 to discard")
      (is (= 3 (-> (get-challenger) :prompt first :choices count)) "Challenger can use Freedom or Imp to discard")
      (prompt-choice-partial :challenger "Freedom")
      (prompt-select :challenger (get-resource state 0))
      (prompt-choice :challenger "Done")
      (is (= 6 (core/discard-cost state :challenger (get-content state :party1 0))) "Skulljack shouldn't trigger a second time")
      (is (= 3 (-> (get-challenger) :prompt first :choices count)) "Challenger can still use Freedom or Imp the second time around")
      (prompt-choice-partial :challenger "Imp")
      (is (= 2 (:agenda-point (get-challenger))) "Challenger should discard The Board with Imp and gain 2 agenda points"))))

(deftest gabriel-santiago:-consummate-professional
  ;; Gabriel Santiago - Gain 2c on first successful HQ run each turn
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Gabriel Santiago: Consummate Professional" ["Easy Mark"]))
    (take-credits state :contestant)
    (run-empty-locale state :rd)
    (is (= 5 (:credit (get-challenger))) "No credits gained")
    (run-empty-locale state :hq)
    (is (= 7 (:credit (get-challenger))) "Gained 2c")
    (run-empty-locale state :hq)
    (is (= 7 (:credit (get-challenger))) "No credits gained")))

(deftest gagarin-deep-space:-expanding-the-horizon
  ;; Gagarin - pay 1c to access each card in party
  (do-game
    (new-game
      (make-deck "Gagarin Deep Space: Expanding the Horizon" ["PAD Campaign" "Caprcharacter Nisei"])
      (default-challenger))
    (core/lose state :challenger :credit 4)
    (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
    (play-from-hand state :contestant "PAD Campaign" "New party")
    (take-credits state :contestant)
    (run-empty-locale state :party1)
    (prompt-select :challenger (get-content state :party1 0))
    (is (zero? (:credit (get-challenger))) "Paid 1 credit to access")
    (prompt-choice :challenger "No") ; Dismiss discard prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-locale state :party1)
    (prompt-select :challenger (get-content state :party1 0))
    (prompt-choice :challenger "No action") ; Could not afford message dismissed
    (is (empty? (:prompt (get-challenger))) "Challenger cannot access so no discard prompt")
    (is (not (last-log-contains? state "PAD Campaign")) "No card name was logged")
    (run-empty-locale state :hq)
    (prompt-choice :challenger "No") ; Dismiss discard prompt
    (is (last-log-contains? state "Caprcharacter") "Accessed card name was logged")))

(deftest grndl:-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (testing "Basic test"
    (do-game
      (new-game
        (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
        (default-challenger))
      (is (= 10 (:credit (get-contestant))) "GRNDL starts with 10 credits")
      (is (= 1 (:bad-publicity (get-contestant))) "GRNDL starts with 1 bad publicity")))
  (testing "vs Valencia - only 1 bad pub at start"
    (do-game
      (new-game
        (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
        (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
      (is (= 10 (:credit (get-contestant))) "GRNDL starts with 10 credits")
      (is (= 1 (:bad-publicity (get-contestant))) "GRNDL starts with 1 bad publicity"))))

(deftest haarpsichord-studios:-entertainment-unleashed
  ;; Haarpsichord Studios
  (testing "Prevent stealing more than 1 agenda per turn"
    (do-game
      (new-game
        (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
        (default-challenger ["Gang Sign"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Gang Sign")
      (run-empty-locale state "HQ")
      (prompt-choice :challenger "Steal")
      (is (= 1 (:agenda-point (get-challenger))))
      (run-empty-locale state "HQ")
      (prompt-choice :challenger "No action")
      (is (= 1 (:agenda-point (get-challenger))) "Second steal of turn prevented")
      (take-credits state :challenger)
      (play-from-hand state :contestant "15 Minutes" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (prompt-choice :challenger "Card from hand")
      (prompt-choice :challenger "Steal")
      (is (= 2 (:agenda-point (get-challenger))) "Steal prevention didn't carry over to Contestant turn")))
  (testing "Interactions with Employee Strike. Issue #1313"
    (do-game
      (new-game
        (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
        (default-challenger ["Employee Strike" "Scrubbed"]))
      (take-credits state :contestant)
      (core/gain state :challenger :click 5)
      (run-empty-locale state "HQ")
      (prompt-choice :challenger "Steal")
      (is (= 1 (:agenda-point (get-challenger))))
      (play-from-hand state :challenger "Employee Strike")
      (run-empty-locale state "HQ")
      (prompt-choice :challenger "Steal")
      (is (= 2 (:agenda-point (get-challenger))) "Second steal not prevented")
      (play-from-hand state :challenger "Scrubbed")
      (run-empty-locale state "HQ")
      (prompt-choice :challenger "No action")
      (is (= 2 (:agenda-point (get-challenger))) "Third steal prevented"))))

(deftest haas-bioroid:-architects-of-tomorrow
  ;; Architects of Tomorrow - prompt to reveal after passing bioroid
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Architects of Tomorrow" [(qty "Eli 1.0" 2) "Pup"])
      (default-challenger))
    (core/gain state :contestant :credit 3)
    (play-from-hand state :contestant "Eli 1.0" "Archives")
    (play-from-hand state :contestant "Pup" "Archives")
    (play-from-hand state :contestant "Eli 1.0" "HQ")
    (take-credits state :contestant)
    (run-on state "Archives")
    (core/reveal state :contestant (get-character state :archives 1))
    (run-continue state)
    (core/reveal state :contestant (get-character state :archives 0))
    (is (= 3 (:credit (get-contestant))) "Contestant has 3 credits after revealing Eli 1.0")
    (run-continue state)
    (prompt-select :contestant (get-character state :hq 0))
    (is (= 3 (:credit (get-contestant))) "Contestant not charged for Architects of Tomorrow reveal of Eli 1.0")))

(deftest haas-bioroid:-engineering-the-future
  ;; Engineereing the Future
  (testing "interaction with Employee Strike"
    (do-game
      (new-game
        (make-deck "Haas-Bioroid: Engineering the Future" [(qty "Eli 1.0" 3) "Paywall Implementation"])
        (default-challenger ["Employee Strike"]))
      (take-credits state :contestant)
      (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits at turn end")
      (play-from-hand state :challenger "Employee Strike")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Eli 1.0" "New party")
      (is (= 8 (:credit (get-contestant))) "Contestant did not gain 1cr from EtF")
      (play-from-hand state :contestant "Paywall Implementation")
      (play-from-hand state :contestant "Eli 1.0" "New party")
      (is (= 8 (:credit (get-contestant))) "Contestant did not gain 1cr from EtF")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (play-from-hand state :contestant "Eli 1.0" "New party")
      (is (= 9 (:credit (get-contestant))) "Contestant gained 1cr from EtF"))))

(deftest haas-bioroid:-stronger-together
  ;; Stronger Together - +1 strength for Bioroid character
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Stronger Together" ["Eli 1.0"])
      (default-challenger))
    (play-from-hand state :contestant "Eli 1.0" "Archives")
    (let [eli (get-character state :archives 0)]
      (core/reveal state :contestant eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest iain-stirling:-retired-spook
  ;; Iain Stirling - Gain 2 credits when behind
  (do-game
    (new-game
      (default-contestant ["Breaking News"])
      (make-deck "Iain Stirling: Retired Spook" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Breaking News" "New party")
    (let [ag1 (get-in @state [:contestant :locales :party1 :content 0])]
      (core/advance state :contestant {:card (refresh ag1)})
      (core/advance state :contestant {:card (refresh ag1)})
      (core/score state :contestant {:card (refresh ag1)})
      (take-credits state :contestant)
      (is (= 1 (:agenda-point (get-contestant))) "Contestant gains 1 agenda point from Breaking News")
      (take-credits state :challenger 1)
      (is (= 8 (:credit (get-challenger))) "Gained 2 credits from being behind on points"))))

(deftest industrial-genomics:-growing-solutions
  ;; Industrial Genomics - Increase discard cost
  (testing "Basic test"
    (do-game
      (new-game
        (make-deck "Industrial Genomics: Growing Solutions"
                   [(qty "PAD Campaign" 3) (qty "Hedge Fund" 3)])
        (default-challenger))
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (discard-from-hand state :contestant "PAD Campaign")
      (discard-from-hand state :contestant "PAD Campaign")
      (discard-from-hand state :contestant "Hedge Fund")
      (discard-from-hand state :contestant "Hedge Fund")
      (let [pad (get-content state :party1 0)]
        (core/reveal state :contestant pad)
        (take-credits state :contestant)
        (run-empty-locale state "Locale 1")
        (is (= 8 (core/discard-cost state :challenger (refresh pad)))))))
  (testing "with Product Recall"
    (do-game
      (new-game
        (make-deck "Industrial Genomics: Growing Solutions"
                   ["Product Recall" (qty "PAD Campaign" 3) (qty "Hedge Fund" 2)])
        (default-challenger))
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (discard-from-hand state :contestant "PAD Campaign")
      (discard-from-hand state :contestant "PAD Campaign")
      (discard-from-hand state :contestant "Hedge Fund")
      (discard-from-hand state :contestant "Hedge Fund")
      (let [pad (get-content state :party1 0)]
        (core/reveal state :contestant pad)
        (take-credits state :contestant)
        (run-empty-locale state "Locale 1")
        (is (= 8 (core/discard-cost state :challenger (refresh pad))))
        (run-jack-out state)
        (take-credits state :challenger)
        (play-from-hand state :contestant "Product Recall")
        (let [credits (:credit (get-contestant))]
          (prompt-select :contestant pad)
          (is (= (+ credits 8) (:credit (get-contestant))) "Gain 8 credits from discarding PAD Campaign"))))))

(deftest jemison-astronautics:-sacrifcharacter.-audacity.-success.
  ;; Jemison Astronautics - Place advancements when forfeiting agendas
  (testing "Basic test"
    (do-game
      (new-game
        (make-deck "Jemison Astronautics: Sacrifcharacter. Audacity. Success."
                   ["Enforcer 1.0" "Hostile Takeover" "Ice Wall" "Global Food Initiative"])
        (default-challenger ["Data Dealer"]))
      (play-from-hand state :contestant "Enforcer 1.0" "HQ")
      (play-from-hand state :contestant "Ice Wall" "R&D")
      (play-from-hand state :contestant "Hostile Takeover" "New party")
      (let [enf (get-character state :hq 0)
            iwall (get-character state :rd 0)]
        (take-credits state :contestant)
        (play-from-hand state :challenger "Data Dealer")
        (run-empty-locale state "Locale 1")
        (prompt-choice :challenger "Steal")
        (let [dd (get-radicle state 0)]
          (card-ability state :challenger dd 0)
          (prompt-select :challenger (get-in (get-challenger) [:scored 0]))
          (is (empty? (:prompt (get-contestant))) "No Jemison prompt for Challenger forfeit")
          (take-credits state :challenger)
          (play-from-hand state :contestant "Global Food Initiative" "New party")
          (score-agenda state :contestant (get-content state :party2 0))
          (core/reveal state :contestant enf)
          (prompt-select :contestant (get-in (get-contestant) [:scored 0]))
          (prompt-select :contestant iwall)
          (is (= 4 (get-counters (refresh iwall) :advancement)) "Jemison placed 4 advancements")))))
  (testing "24/7 - Armed Intimidation combination"
    ;; Expected result: 24/7 causes Forfeit, Jemison places counters, AI triggers
    (do-game
      (new-game
        (make-deck "Jemison Astronautics: Sacrifcharacter. Audacity. Success."
                   ["Armed Intimidation" "Hostile Takeover"
                    "24/7 News Cycle" "Ice Wall"])
        (default-challenger))
      (play-and-score state "Hostile Takeover")
      (is (= 1 (:agenda-point (get-contestant))) "Contestant has 1 agenda points from Hostile Takeover")
      (is (= 12 (:credit (get-contestant))) "Contestant has 12 credits after scoring Hostile Takeover with play-score")
      (play-and-score state "Armed Intimidation")
      (prompt-choice :challenger "Take 2 tags")
      (is (= 3 (:agenda-point (get-contestant))) "Contestant has 3 agenda points from HT + Armed Intimidation")
      (is (= 2 (:tag (get-challenger))) "Challenger took 2 tags from AI")
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (play-from-hand state :contestant "24/7 News Cycle")
      (prompt-select :contestant (get-scored state :contestant 0)) ; select HT to forfeit
      (let [character-wall (get-character state :hq 0)]
        (prompt-select :contestant character-wall) ; The Jemison forfeit triggers
        (is (= 2 (get-counters (refresh character-wall) :advancement)) "Ice Wall has 2 advancement counters from HT forfeit"))
      (prompt-select :contestant (get-scored state :contestant 0)) ; select AI to trigger
      (prompt-choice :challenger "Take 2 tags") ; First challenger has prompt
      (is (= 4 (:tag (get-challenger))) "Challenger took 2 more tags from AI -- happens at the end of all the async completion"))))

(deftest jesminder-sareen:-girl-behind-the-curtain
  ;; Jesminder Sareen - avoid tags only during a run
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["SEA Source" "Data Raven"])
                (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Data Raven" "Archives")
      (take-credits state :contestant)
      (let [dr (-> @state :contestant :locales :archives :characters first)]
        (core/reveal state :contestant dr)
        (core/click-run state :challenger {:locale "Archives"})
        (card-ability state :contestant dr 0)
        (is (zero? (:tag (get-challenger))) "Jesminder avoided first tag during the run")
        (card-ability state :contestant dr 0)
        (is (= 1 (:tag (get-challenger))) "Jesminder did not avoid the second tag during the run")
        (core/no-action state :contestant nil)
        (core/continue state :challenger nil)
        (core/no-action state :contestant nil)
        (core/successful-run state :challenger nil)
        (run-empty-locale state "R&D") ; clear per-run buffer
        (take-credits state :challenger)
        (play-from-hand state :contestant "SEA Source")
        (prompt-choice :contestant 0)
        (prompt-choice :challenger 0)
        (is (= 2 (:tag (get-challenger))) "Jesminder did not avoid the tag outside of a run"))))
  (testing "don't avoid John Masanori tag"
    (do-game
      (new-game (default-contestant)
                (make-deck "Jesminder Sareen: Girl Behind the Curtain" ["John Masanori"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "John Masanori")
      (run-on state "HQ")
      (core/jack-out state :challenger nil)
      (is (= 1 (:tag (get-challenger))) "Jesminder did not avoid John Masanori tag"))))

(deftest cardnum-biotech:-life-imagined
  ;; Cardnum Biotech
  (testing "Brewery net damage"
    (do-game
      (new-game
        (make-deck "Cardnum Biotech: Life Imagined" ["Braintrust"])
        (default-challenger)
        {:dont-start-turn true})
      (prompt-choice :contestant "The Brewery")
      (core/start-turn state :contestant nil)
      (card-ability state :contestant (:identity (get-contestant)) 1)
      (is (= 1 (count (:hand (get-challenger)))) "Challenger took 2 net damage from Brewery flip")))
  (testing "Greenhouse four advancement tokens"
    (do-game
      (new-game
        (make-deck "Cardnum Biotech: Life Imagined" ["Braintrust"])
        (default-challenger)
        {:dont-start-turn true})
      (prompt-choice :contestant "The Greenhouse")
      (core/start-turn state :contestant nil)
      (play-from-hand state :contestant "Braintrust" "New party")
      (take-credits state :contestant)
      (take-credits state :challenger)
      (let [bt (get-content state :party1 0)]
        (is (zero? (get-counters (refresh bt) :advancement)) "No advancement counters on agenda")
        (card-ability state :contestant (:identity (get-contestant)) 1)
        (prompt-select :contestant (refresh bt))
        (is (= 4 (get-counters (refresh bt) :advancement)) "Four advancement counters on agenda"))))
  (testing "Tank shuffle Archives into R&D"
    (do-game
      (new-game
        (make-deck "Cardnum Biotech: Life Imagined" [(qty "Hedge Fund" 3)])
        (default-challenger)
        {:dont-start-turn true})
      (prompt-choice :contestant "The Tank")
      (core/start-turn state :contestant nil)
      (play-from-hand state :contestant "Hedge Fund")
      (play-from-hand state :contestant "Hedge Fund")
      (play-from-hand state :contestant "Hedge Fund")
      (take-credits state :challenger)
      (is (= 3 (count (:discard (get-contestant)))) "Archives started with 3 cards")
      (is (zero? (count (:deck (get-contestant)))) "R&D started empty")
      (card-ability state :contestant (:identity (get-contestant)) 1)
      (is (zero? (count (:discard (get-contestant)))) "Archives ended empty")
      (is (= 3 (count (:deck (get-contestant)))) "R&D ended with 3 cards"))))

(deftest cardnum:-personal-evolution
  ;; Personal Evolution - Prevent challenger from running on parties unless they first run on a central
  (do-game
    (new-game
      (make-deck "Cardnum: Personal Evolution" [(qty "Braintrust" 6)])
      (default-challenger [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Braintrust" "New party")
    (take-credits state :contestant)
    (run-empty-locale state "Locale 1")
    (prompt-choice :challenger "Steal")
    (is (= 2 (count (:hand (get-challenger)))) "Challenger took 1 net damage from steal")))

(deftest cardnum:-potential-unleashed
  ;; Potential Unleashed - when the challenger takes at least one net damage, mill 1 from their deck
  (do-game
    (new-game (make-deck "Cardnum: Potential Unleashed" ["Philotic Entanglement" "Neural EMP" (qty "Braintrust" 3)])
              (default-challenger [(qty "Employee Strike" 10)]))
    (play-from-hand state :contestant "Braintrust" "New party")
    (play-from-hand state :contestant "Braintrust" "New party")
    (take-credits state :contestant)
    (run-empty-locale state "Locale 1")
    (prompt-choice :challenger "Steal")
    (run-empty-locale state "Locale 2")
    (prompt-choice :challenger "Steal")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Philotic Entanglement" "New party")
    (score-agenda state :contestant (get-content state :party3 0))
    (is (= 3 (count (:discard (get-challenger)))))
    (play-from-hand state :contestant "Neural EMP")
    (is (= 5 (count (:discard (get-challenger)))))))

(deftest cardnum:-replicating-perfection
  ;; Replicating Perfection - Prevent challenger from running on parties unless they first run on a central
  (testing "Basic test"
    (do-game
      (new-game
        (make-deck "Cardnum: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
        (default-challenger))
      (play-from-hand state :contestant "Mental Health Clinic" "New party")
      (take-credits state :contestant)
      (is (not (core/can-run-locale? state "Locale 1")) "Challenger can only run on centrals")
      (run-empty-locale state "HQ")
      (is (boolean (core/can-run-locale? state "Locale 1")) "Challenger can run on parties")))
  (testing "interaction with Employee Strike. Issue #1313 and #1956."
    (do-game
      (new-game
        (make-deck "Cardnum: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
        (default-challenger ["Employee Strike" "Scrubbed"]))
      (play-from-hand state :contestant "Mental Health Clinic" "New party")
      (take-credits state :contestant)
      (is (not (core/can-run-locale? state "Locale 1")) "Challenger can only run on centrals")
      (play-from-hand state :challenger "Employee Strike")
      (is (boolean (core/can-run-locale? state "Locale 1")) "Challenger can run on parties")
      (play-from-hand state :challenger "Scrubbed")
      (is (not (core/can-run-locale? state "Locale 1")) "Challenger can only run on centrals"))))

(deftest ^{:card-title "kate-\"mac\"-mccaffrey:-digital-tinker"}
  kate
  ;; Kate 'Mac' McCaffrey
  (testing "Place discount"
    (do-game
      (new-game (default-contestant)
                (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" ["Magnum Opus"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Magnum Opus")
      (is (= 1 (:credit (get-challenger))) "Placed Magnum Opus for 4 credits")))
  (testing "No discount for 0 cost"
    (do-game
      (new-game (default-contestant)
                (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                           ["Magnum Opus"
                            "Self-modifying Code"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Self-modifying Code")
      (play-from-hand state :challenger "Magnum Opus")
      (is (zero? (:credit (get-challenger))) "No Kate discount on second resource place")))
  (testing "Can afford only with the discount"
    (do-game
      (new-game (default-contestant)
                (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" ["Magnum Opus"]))
      (take-credits state :contestant)
      (core/lose state :challenger :credit 1)
      (is (= 4 (:credit (get-challenger))))
      (play-from-hand state :challenger "Magnum Opus")
      (is (= 1 (count (get-resource state))) "Magnum Opus placed")
      (is (zero? (:credit (get-challenger))) "Placed Magnum Opus for 4 credits"))))

(deftest ^{:card-title "ken-\"express\"-tenma:-disappeared-clone"}
  ken
  ;; Ken 'Express' Tenma - Gain 1 credit when first Run event played
  (do-game
    (new-game (default-contestant)
              (make-deck "Ken \"Express\" Tenma: Disappeared Clone" [(qty "Account Siphon" 2)]))
    (take-credits state :contestant)
    (play-run-event state (first (:hand (get-challenger))) :hq)
    (is (= 6 (:credit (get-challenger))) "Gained 1 credit for first Run event")
    (prompt-choice :challenger "Replacement effect")
    (play-run-event state (first (:hand (get-challenger))) :hq)
    (is (= 16 (:credit (get-challenger))) "No credit gained for second Run event")))

(deftest khan:-savvy-skiptracer
  ;; Khan
  (testing "proper order of events when vs. Caprcharacter"
    (do-game
      (new-game
        (default-contestant ["Eli 1.0" "Caprcharacter Nisei"])
        (make-deck "Khan: Savvy Skiptracer" ["Corroder"]))
      (play-from-hand state :contestant "Eli 1.0" "Archives")
      (play-from-hand state :contestant "Caprcharacter Nisei" "Archives")
      (core/reveal state :contestant (get-content state :archives 0))
      (take-credits state :contestant)
      (run-on state "Archives")
      (run-continue state)
      (is (and (empty? (:prompt (get-contestant)))
               (= 1 (count (:prompt (get-challenger))))
               (= "Khan: Savvy Skiptracer" (-> (get-challenger) :prompt first :card :title)))
          "Only Khan prompt showing")
      (prompt-select :challenger (first (:hand (get-challenger))))
      (is (find-card "Corroder" (-> (get-challenger) :rig :resource)) "Corroder placed")
      (is (= 4 (:credit (get-challenger))) "1cr discount from Khan")
      (is (= "Caprcharacter Nisei" (-> (get-challenger) :prompt first :card :title)) "Caprcharacter prompt showing")
      (prompt-choice :challenger "0 [Credits]")
      (prompt-choice :contestant "1 [Credits]")
      (is (not (:run @state)) "Run ended"))))

(deftest laramy-fisk:-savvy-investor
  ;; Laramy Fisk
  (testing "placing a Shard should still give option to force Contestant draw"
    (do-game
      (new-game
        (default-contestant [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3)])
        (make-deck "Laramy Fisk: Savvy Investor" ["Eden Shard"]))
      (starting-hand state :contestant ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Eli 1.0" "Eli 1.0"])
      (take-credits state :contestant)
      (run-on state "R&D")
      (core/no-action state :contestant nil)
      ;; at Successful Run stage -- click Eden Shard to place
      (play-from-hand state :challenger "Eden Shard")
      (is (= 5 (:credit (get-challenger))) "Eden Shard place was free")
      (is (= "Eden Shard" (:title (get-radicle state 0))) "Eden Shard placed")
      (is (= "Identity" (-> (get-challenger) :prompt first :card :type)) "Fisk prompt showing")
      (prompt-choice :challenger "Yes")
      (is (not (:run @state)) "Run ended")
      (is (= 6 (count (:hand (get-contestant)))) "Contestant forced to draw"))))

(deftest leela-patel:-trained-pragmatist
  ;; Leela Patel
  (testing "complicated interaction with mutiple Gang Sign"
    (do-game
      (new-game
        (make-deck "Titan Transnational: Investing In Your Future" ["Project Atlas"
                                                                    "Hostile Takeover"
                                                                    "Geothermal Fracking"])
        (make-deck "Leela Patel: Trained Pragmatist" [(qty "Gang Sign" 2)]))
      (play-from-hand state :contestant "Project Atlas" "New party")
      (play-from-hand state :contestant "Hostile Takeover" "New party")
      (play-from-hand state :contestant "Geothermal Fracking" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Gang Sign")
      (play-from-hand state :challenger "Gang Sign")
      (take-credits state :challenger)
      (score-agenda state :contestant (get-content state :party1 0))
      (prompt-choice :challenger "Leela Patel: Trained Pragmatist")
      (prompt-select :challenger (get-content state :party2 0))
      (is (find-card "Hostile Takeover" (:hand (get-contestant))) "Hostile Takeover returned to hand")
      (prompt-choice :challenger "Gang Sign")
      (prompt-choice :challenger "Card from hand")
      (prompt-choice :challenger "Steal")
      (is (find-card "Hostile Takeover" (:scored (get-challenger))) "Hostile Takeover stolen with Gang Sign")
      (prompt-select :challenger (get-content state :party3 0))
      (is (find-card "Geothermal Fracking" (:hand (get-contestant))) "Geothermal Fracking returned to hand")
      (prompt-choice :challenger "Card from hand")
      (prompt-choice :challenger "Steal")
      (is (find-card "Hostile Takeover" (:scored (get-challenger))) "Geothermal Fracking stolen with Gang Sign")
      (prompt-choice :challenger "Done")))
  (testing "issues with lingering successful run prompt"
    (do-game
      (new-game
        (make-deck "NBN: Making News" ["Breaking News" "SanSan City Grid"])
        (make-deck "Leela Patel: Trained Pragmatist" []))
      (starting-hand state :contestant ["SanSan City Grid"])
      (play-from-hand state :contestant "SanSan City Grid" "New party")
      (take-credits state :contestant)
      (run-empty-locale state :rd)
      (prompt-choice :challenger "Steal")
      (prompt-select :challenger (get-content state :party1 0))
      (is (not (:run @state)) "Run is over")))
  (testing "regions returned to hand in the middle of a run do not break the run. Issue #2008"
    (do-game
      (new-game (default-contestant [(qty "Crisium Grid" 3) (qty "Project Atlas" 3) "Shock!"])
                (make-deck "Leela Patel: Trained Pragmatist" ["Sure Gamble"]))
      (starting-hand state :contestant ["Crisium Grid" "Crisium Grid" "Crisium Grid" "Project Atlas" "Shock!" "Project Atlas"])
      (play-from-hand state :contestant "Crisium Grid" "HQ")
      (play-from-hand state :contestant "Crisium Grid" "Archives")
      (play-from-hand state :contestant "Crisium Grid" "R&D")
      (discard-from-hand state :contestant "Project Atlas")
      (discard-from-hand state :contestant "Shock!")
      (take-credits state :contestant)
      (run-empty-locale state "HQ")
      (prompt-choice :challenger "Card from hand")
      (prompt-choice :challenger "Steal")
      (prompt-select :challenger (get-content state :hq 0))
      (is (not (get-content state :hq 0)) "Region returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses")
      (run-empty-locale state "R&D")
      (prompt-choice :challenger "Card from deck")
      (prompt-choice :challenger "Steal")
      (prompt-select :challenger (get-content state :rd 0))
      (is (not (get-content state :rd 0)) "Region returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses")
      (run-empty-locale state "Archives")
      (prompt-choice :challenger "Shock!")
      (prompt-choice :challenger "Project Atlas")
      (prompt-choice :challenger "Steal")
      (prompt-select :challenger (get-content state :archives 0))
      (is (not (get-content state :archives 0)) "Region returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses"))))

(deftest maxx:-maximum-punk-rock
  ;; MaxX
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                      "Eater"]))
      (starting-hand state :challenger ["Eater"])
      (take-credits state :contestant)
      (is (= 2 (count (:discard (get-challenger)))) "MaxX discarded 2 cards at start of turn")
      (is (last-log-contains? state "Wyldside, Wyldside")
          "Maxx did log discarded card names")))
  (testing "with Dummy Box. Check that mills don't trigger discard prevention #3246"
    (do-game
      (new-game (default-contestant)
                (make-deck "MaxX: Maximum Punk Rock" [(qty "Dummy Box" 30)]))
      (take-credits state :contestant)
      (is (= 2 (count (:discard (get-challenger)))) "MaxX discarded 2 cards at start of turn")
      (play-from-hand state :challenger "Dummy Box")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (empty? (:prompt (get-challenger))) "Dummy Box not fired from mill")))
  (testing "with Wyldside - using Wyldside during Step 1.2 should lose 1 click"
    (do-game
      (new-game (default-contestant)
                (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                      (qty "Sure Gamble" 3)
                                                      (qty "Infiltration" 3)
                                                      (qty "Corroder" 3)
                                                      (qty "Eater" 3)]))
      (take-credits state :contestant)
      (is (= 2 (count (:discard (get-challenger)))) "MaxX discarded 2 cards at start of turn")
      (starting-hand state :challenger ["Wyldside"])
      (play-from-hand state :challenger "Wyldside")
      (take-credits state :challenger 3)
      (is (= 5 (:credit (get-challenger))) "Challenger has 5 credits at end of first turn")
      (is (find-card "Wyldside" (get-radicle state)) "Wyldside was placed")
      (take-credits state :contestant)
      (is (zero? (:click (get-challenger))) "Challenger has 0 clicks")
      (is (:challenger-phase-12 @state) "Challenger is in Step 1.2")
      (let [maxx (get-in @state [:challenger :identity])
            wyld (find-card "Wyldside" (get-radicle state))]
        (card-ability state :challenger maxx 0)
        (card-ability state :challenger wyld 0)
        (core/end-phase-12 state :challenger nil)
        (is (= 4 (count (:discard (get-challenger)))) "MaxX discarded 2 cards at start of turn")
        (is (= 3 (:click (get-challenger))) "Wyldside caused 1 click to be lost")
        (is (= 3 (count (:hand (get-challenger)))) "3 cards drawn total")))))

(deftest mti-mwekundu:-life-improved
  ;; Mti Mwekundu: Life Improved - when locale is approached, place character from HQ at the innermost position
  (testing "No character"
    (do-game
      (new-game (make-deck "Mti Mwekundu: Life Improved" ["Enigma"])
                (default-challenger))
      (take-credits state :contestant)
      (run-on state "HQ")
      (is (zero? (get-in @state [:run :position])) "Initial position approaching locale")
      (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
      (prompt-select :contestant (find-card "Enigma" (:hand (get-contestant))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new character")
      (is (= "Enigma" (:title (get-character state :hq 0))) "Enigma was placed")
      (is (empty? (:hand (get-contestant))) "Enigma removed from HQ")))
  (testing "Multiple character"
    (do-game
      (new-game (make-deck "Mti Mwekundu: Life Improved" ["Enigma" "Ice Wall" "Bloom"])
                (default-challenger))
      (play-from-hand state :contestant "Ice Wall" "R&D")
      (play-from-hand state :contestant "Bloom" "R&D")
      (take-credits state :contestant)
      (run-on state "R&D")
      (run-continue state)
      (run-continue state)
      (is (zero? (get-in @state [:run :position])) "Initial position approaching locale")
      (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
      (prompt-select :contestant (find-card "Enigma" (:hand (get-contestant))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new character")
      (is (= "Enigma" (:title (get-character state :rd 0))) "Enigma was placed")
      (is (empty? (:hand (get-contestant))) "Enigma removed from HQ")))
  (testing "with Kakugo, passing shouldn't fire net damage twcharacter. #3588"
    (do-game
      (new-game (make-deck "Mti Mwekundu: Life Improved" ["Kakugo"])
                (default-challenger))
      (take-credits state :contestant)
      (run-on state "HQ")
      (is (zero? (get-in @state [:run :position])) "Initial position approaching locale")
      (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
      (prompt-select :contestant (find-card "Kakugo" (:hand (get-contestant))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new character")
      (is (= "Kakugo" (:title (get-character state :hq 0))) "Kakugo was placed")
      (is (empty? (:hand (get-contestant))) "Kakugo removed from HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (run-continue state)
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should take 1 net damage from Kakugo"))))

(deftest nasir-meidan:-cyber-explorer
  ;; Nasir
  (testing "Basic test"
    (do-game
      (new-game
        (default-contestant [(qty "Ice Wall" 3)])
        (make-deck "Nasir Meidan: Cyber Explorer" []))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (take-credits state :contestant)
      (run-on state "HQ")
      (let [iwall (get-character state :hq 0)
            nasir (get-in @state [:challenger :identity])]
        (core/reveal state :contestant iwall)
        (is (= 5 (:credit (get-challenger))) "Nasir Ability does not trigger automatically")
        (card-ability state :challenger nasir 0)
        (is (= 1 (:credit (get-challenger))) "Credits at 1 after Nasir ability trigger"))))
  (testing "with Xanadu"
    (do-game
      (new-game
        (default-contestant ["Ice Wall"])
        (make-deck "Nasir Meidan: Cyber Explorer" ["Xanadu"]))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (take-credits state :contestant)
      (swap! state assoc-in [:challenger :credit] 6)
      (play-from-hand state :challenger "Xanadu")
      (run-on state "HQ")
      (let [iwall (get-in @state [:contestant :locales :hq :characters 0])
            nasir (get-in @state [:challenger :identity])]
        (core/reveal state :contestant iwall)
        (is (= 3 (:credit (get-challenger))) "Pay 3 to place Xanadu")
        (card-ability state :challenger nasir 0)
        (is (= 2 (:credit (get-challenger))) "Gain 1 more credit due to Xanadu")))))

(deftest nbn:-controlling-the-message
  ;; NBN: Controlling the Message
  (testing "Trace to tag Challenger when first placed Contestant card is discarded"
    (do-game
      (new-game
        (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 3)])
        (default-challenger ["Forger"]))
      (play-from-hand state :contestant "Launch Campaign" "New party")
      (play-from-hand state :contestant "Launch Campaign" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Forger")
      ; discard from HQ first - #2321
      (run-empty-locale state "HQ")
      (prompt-choice-partial :challenger "Pay")
      (run-empty-locale state "Locale 1")
      (prompt-choice-partial :challenger "Pay")
      (prompt-choice :contestant "Yes")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (empty? (:prompt (get-challenger))) "Forger can't avoid the tag")
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 unpreventable tag")
      (core/gain state :challenger :credit 2)
      (run-empty-locale state "Locale 2")
      (prompt-choice-partial :challenger "Pay")
      (is (empty? (:prompt (get-contestant))) "No trace chance on 2nd discarded card of turn")))
  (testing "Interaction with Dedicated Response Team"
    (do-game
      (new-game
        (make-deck "NBN: Controlling the Message" ["Launch Campaign" "Dedicated Response Team"])
        (default-challenger))
      (play-from-hand state :contestant "Launch Campaign" "New party")
      (play-from-hand state :contestant "Dedicated Response Team" "New party")
      (core/reveal state :contestant (get-content state :party2 0))
      (take-credits state :contestant)
      (run-empty-locale state "Locale 1")
      (prompt-choice-partial :challenger "Pay")
      (prompt-choice :contestant "Yes")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 1 (:tag (get-challenger))) "Challenger took 1 unpreventable tag")
      (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 meat damage from DRT"))))

(deftest new-angeles-sol:-your-news
  ;; New Angeles Sol - interaction with challenger stealing agendas
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Paywall Implementation" 2) "Breaking News"])
      (default-challenger))
    (play-from-hand state :contestant "Breaking News" "New party")
    (play-from-hand state :contestant "Paywall Implementation")
    (take-credits state :contestant)
    (is (= 6 (:credit (get-contestant))))
    (run-empty-locale state :party1)
    (is (= 7 (:credit (get-contestant))) "Contestant gained 1cr from successful run")
    (prompt-choice :challenger "Steal")
    (prompt-choice :contestant "Yes")
    (is (find-card "Paywall Implementation" (:discard (get-contestant))) "Paywall discarded before Sol triggers")
    (prompt-select :contestant (find-card "Paywall Implementation" (:hand (get-contestant))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Paywall Implementation" (:current (get-contestant))) "Paywall back in play")))

(deftest next-design:-guarding-the-net
  ;; Next Design.  Place up to 3 Character before game starts, one per locale max, and re-draw to 5
  (do-game
    (new-game
      (make-deck "NEXT Design: Guarding the Net" [(qty "Snowflake" 10)])
      (default-challenger)
      {:dont-start-turn true})
    (prompt-select :contestant (find-card "Snowflake" (:hand (get-contestant))))
    (prompt-choice :contestant "HQ")
    (prompt-select :contestant (find-card "Snowflake" (:hand (get-contestant))))
    (prompt-choice :contestant "R&D")
    (prompt-select :contestant (find-card "Snowflake" (:hand (get-contestant))))
    (prompt-choice :contestant "New party")
    (is (= 2 (count (:hand (get-contestant)))) "Contestant should have 2 cards in hand")
    (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
    (is (= 5 (count (:hand (get-contestant)))) "Contestant should start with 5 cards in hand")))

(deftest nisei-division:-the-next-generation
  ;; Nisei Division - Gain 1 credit from every psi game
  (do-game
    (new-game
      (make-deck "Nisei Division: The Next Generation" [(qty "Snowflake" 2)])
      (default-challenger))
    (play-from-hand state :contestant "Snowflake" "HQ")
    (play-from-hand state :contestant "Snowflake" "HQ")
    (take-credits state :contestant)
    (let [s1 (get-in @state [:contestant :locales :hq :characters 0])
          s2 (get-in @state [:contestant :locales :hq :characters 1])]
      (run-on state "HQ")
      (core/reveal state :contestant s2)
      (is (= 4 (:credit (get-contestant))))
      (card-subroutine state :contestant s2 0)
      (prompt-choice :contestant "0 [Credits]")
      (prompt-choice :challenger "0 [Credits]")
      (is (= 5 (:credit (get-contestant))) "Gained 1 credit from psi game")
      (core/no-action state :contestant nil)
      (core/reveal state :contestant s1)
      (is (= 4 (:credit (get-contestant))))
      (card-subroutine state :contestant s1 0)
      (prompt-choice :contestant "0 [Credits]")
      (prompt-choice :challenger "1 [Credits]")
      (is (= 5 (:credit (get-contestant))) "Gained 1 credit from psi game"))))

(deftest noise:-hacker-extraordinaire
  ;; Noise: Hacker Extraordinaire
  (do-game
    (new-game
      (default-contestant [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)])
      (make-deck "Noise: Hacker Extraordinaire" ["Datasucker" "Cache" "Sure Gamble" (qty "Clone Chip" 2) (qty "Sharpshooter" 2)]))
    (starting-hand state :challenger ["Datasucker" "Sure Gamble" "Clone Chip" "Clone Chip" "Cache"])
    (is (= 6 (count (:hand (get-contestant)))) "Contestant should start with 6 cards in hand")
    (is (= 5 (count (:deck (get-contestant)))) "Contestant deck should contain 5 cards")
    (take-credits state :contestant)
    (is (zero? (count (:discard (get-contestant)))) "Archives started empty")
    (play-from-hand state :challenger "Datasucker")
    (is (= 1 (count (:discard (get-contestant)))) "Playing virus should cause card to be discarded from R&D")
    (is (= 4 (count (:deck (get-contestant)))) "Card discarded to Archives by Noise should come from R&D")
    (play-from-hand state :challenger "Sure Gamble")
    (is (= 1 (count (:discard (get-contestant)))) "Playing non-virus should not cause card to be discarded from R&D")
    (core/click-draw state :challenger nil)
    (play-from-hand state :challenger "Clone Chip")
    (play-from-hand state :challenger "Clone Chip")
    (discard-from-hand state :challenger "Cache")
    (discard-from-hand state :challenger "Sharpshooter")
    (take-credits state :challenger)
    ;; playing virus via Clone Chip on Contestant's turn should trigger Noise ability
    (let [chip (get-hazard state 0)]
      (card-ability state :challenger chip 0)
      (prompt-select :challenger (find-card "Cache" (:discard (get-challenger))))
      (let [ds (get-resource state 1)]
        (is (not (nil? ds)))
        (is (= (:title ds) "Cache"))))
    (is (= 2 (count (:discard (get-contestant)))) "Playing virus via Clone Chip on contestant's turn should trigger Noise ability")
    (is (= 2 (count (:deck (get-contestant)))) "Card discarded to Archives by Noise should come from R&D")
    ;; playing non-virus via Clone Chip on Contestant's turn should NOT trigger Noise ability
    (let [chip-2 (get-hazard state 0)]
      (card-ability state :challenger chip-2 0)
      (prompt-select :challenger (find-card "Sharpshooter" (:discard (get-challenger))))
      (let [ss (get-resource state 2)]
        (is (not (nil? ss)))
        (is (= (:title ss) "Sharpshooter"))))
    (is (= 2 (count (:discard (get-contestant)))) "Playing non-virus via Clone Chip on contestant's turn should not trigger Noise ability")))

(deftest null:-whistleblower
  ;; Null
  (testing "Basic test"
    (do-game
      (new-game
        (default-contestant [(qty "Wraparound" 3)])
        (make-deck "Null: Whistleblower" [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Wraparound" "HQ")
      (play-from-hand state :contestant "Wraparound" "HQ")
      (take-credits state :contestant)
      (run-on state "HQ")
      (let [null (get-in @state [:challenger :identity])
            wrap1 (get-character state :hq 0)
            wrap2 (get-character state :hq 1)]
        (card-ability state :challenger null 0)
        (is (empty? (:prompt (get-challenger))) "Ability won't work on unrevealed Character")
        (core/reveal state :contestant wrap2)
        (card-ability state :challenger null 0)
        (prompt-select :challenger (find-card "Sure Gamble" (:hand (get-challenger))))
        (is (= 5 (:current-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
        (run-continue state)
        (core/reveal state :contestant wrap1)
        (card-ability state :challenger null 0)
        (is (empty? (:prompt (get-challenger))) "Ability already used this turn")
        (run-jack-out state)
        (is (= 7 (:current-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))
  (testing "does not affect next character when current is discarded. Issue #1788."
    (do-game
      (new-game
        (default-contestant ["Wraparound" "Spiderweb"])
        (make-deck "Null: Whistleblower" [(qty "Parasite" 3)]))
      (play-from-hand state :contestant "Spiderweb" "HQ")
      (play-from-hand state :contestant "Wraparound" "HQ")
      (take-credits state :contestant)
      (core/gain state :contestant :credit 10)
      (let [null (get-in @state [:challenger :identity])
            spider (get-character state :hq 0)
            wrap (get-character state :hq 1)]
        (core/reveal state :contestant spider)
        (core/reveal state :contestant wrap)
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger (refresh spider))
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :challenger null 0)
        (prompt-select :challenger (first (:hand (get-challenger))))
        (is (find-card "Spiderweb" (:discard (get-contestant))) "Spiderweb discarded by Parasite + Null")
        (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Null")))))

(deftest omar-keung:-conspiracy-theorist
  ;; Omar Keung
  (testing "Make a successful run on the chosen locale once per turn"
    (do-game
      (new-game
        (default-contestant)
        (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
      (take-credits state :contestant)
      (let [omar (get-in @state [:challenger :identity])]
        (card-ability state :challenger omar 0)
        (run-successful state)
        (prompt-choice :challenger "HQ")
        (is (= [:hq] (get-in @state [:challenger :register :successful-run])))
        (is (= "You accessed Hedge Fund." (-> (get-challenger) :prompt first :msg)))
        (prompt-choice :challenger "No action")
        (is (= 3 (:click (get-challenger))))
        (card-ability state :challenger omar 0)
        (is (= 3 (:click (get-challenger))))
        (take-credits state :challenger)
        (take-credits state :contestant)
        (run-empty-locale state :rd)
        (is (= [:rd] (get-in @state [:challenger :register :successful-run])))
        (card-ability state :challenger omar 0)
        (run-successful state)
        (prompt-choice :challenger "HQ")
        (is (= [:hq :rd] (get-in @state [:challenger :register :successful-run]))))))
  (testing "Ash prevents access, but not successful run"
    (do-game
      (new-game
        (default-contestant ["Ash 2X3ZB9CY"])
        (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Ash 2X3ZB9CY" "HQ")
      (take-credits state :contestant)
      (let [omar (get-in @state [:challenger :identity])
            ash (get-content state :hq 0)]
        (core/reveal state :contestant ash)
        (card-ability state :challenger omar 0)
        (run-successful state)
        (prompt-choice :challenger "HQ")
        (prompt-choice :contestant 0)
        (prompt-choice :challenger 0)
        (is (= (:cid ash) (-> (get-challenger) :prompt first :card :cid)))
        (is (= :hq (-> (get-challenger) :register :successful-run first))))))
  (testing "Crisium Grid prevents prompt"
    (do-game
      (new-game
        (default-contestant ["Crisium Grid"])
        (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Crisium Grid" "Archives")
      (take-credits state :contestant)
      (let [omar (get-in @state [:challenger :identity])
            cr (get-content state :archives 0)]
        (core/reveal state :contestant cr)
        (card-ability state :challenger omar 0)
        (run-successful state)
        (is (= (:cid cr) (-> (get-challenger) :prompt first :card :cid)))
        (is (empty? (-> (get-challenger) :register :successful-run)))
        (is (= :archives (get-in @state [:run :locale 0]))))))
  (testing "When selecting R&D, ability adds counters to Medium"
    (do-game
      (new-game
        (default-contestant)
        (make-deck "Omar Keung: Conspiracy Theorist" ["Medium"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Medium")
      (let [omar (get-in @state [:challenger :identity])
            medium (get-resource state 0)]
        (card-ability state :challenger omar 0)
        (run-successful state)
        (prompt-choice :challenger "R&D")
        (is (= 1 (get-counters (refresh medium) :virus))))))
  (testing "When selecting HQ, ability adds counters to Nerve Agent"
    (do-game
      (new-game
        (default-contestant)
        (make-deck "Omar Keung: Conspiracy Theorist" ["Nerve Agent"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Nerve Agent")
      (let [omar (get-in @state [:challenger :identity])
            nerve (get-resource state 0)]
        (card-ability state :challenger omar 0)
        (run-successful state)
        (prompt-choice :challenger "HQ")
        (is (= 1 (get-counters (refresh nerve) :virus)))))))

(deftest quetzal:-free-spirit
  ;; Quetzal
  (do-game
    (new-game
      (default-contestant [(qty "Ice Wall" 3)])
      (make-deck "Quetzal: Free Spirit" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (run-on state "HQ")
    (let [q (get-in @state [:challenger :identity])
          iwall (get-character state :hq 0)
          qdef (core/card-def (get-in @state [:challenger :identity]))
          qmsg (get-in qdef [:abilities 0 :msg])]
      (core/reveal state :contestant iwall)
      (card-ability state :challenger q 0)
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (run-jack-out state)
      (core/click-credit state :challenger nil)
      (run-on state "HQ")
      (card-ability state :challenger (refresh q) 0)
      (is (not (last-log-contains? state qmsg)) "Quetzal ability did not trigger")
      (run-jack-out state)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (core/click-credit state :challenger nil)
      (run-on state "HQ")
      (card-ability state :challenger (refresh q) 0)
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (core/jack-out state :challenger nil))))

(deftest reina-roja:-freedom-fighter
  ;; Reina Roja - Increase cost of first revealed Character
  (do-game
    (new-game
      (default-contestant [(qty "Quandary" 3)])
      (make-deck "Reina Roja: Freedom Fighter" []))
    (play-from-hand state :contestant "Quandary" "R&D")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))))
    (run-on state "R&D")
    (let [quan (get-character state :rd 0)]
      (core/reveal state :contestant quan)
      (is (= 5 (:credit (get-contestant))) "Reveal cost increased by 1"))))

(deftest ^{:card-title "rielle-\"kit\"-peddler:-transhuman"}
  kit
  ;; Rielle "Kit" Peddler - Give Character Code Gate
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 2)])
              (make-deck "Rielle \"Kit\" Peddler: Transhuman" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (run-on state "HQ")
    (let [k (get-in @state [:challenger :identity])
          iwall (get-character state :hq 0)]
      (core/reveal state :contestant iwall)
      (card-ability state :challenger k 0)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate"))))

(deftest skontestantios-defense-systems:-persuasive-power
  ; Remove a card from game when it moves to discard once per round
  (do-game
    (new-game (make-deck "Skontestantios Defense Systems: Persuasive Power" ["Hedge Fund" (qty "Quandary" 4)])
              (default-challenger ["The Maker's Eye" "Lucky Find"]))
    (play-from-hand state :contestant "Hedge Fund")
    (dotimes [_ 4] (core/move state :contestant (first (:hand (get-contestant))) :deck))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Lucky Find")
    (play-from-hand state :challenger "The Maker's Eye")
    (is (= :rd (get-in @state [:run :locale 0])))
    ; Don't allow a run-event in progress to be targeted #2963
    (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
    (is (empty? (filter #(= "The Maker's Eye" (:title %)) (-> (get-contestant) :prompt first :choices))) "No Maker's Eye choice")
    (prompt-choice :contestant "Cancel")
    (run-successful state)
    (prompt-choice :challenger "Card from deck")
    (is (= "You accessed Quandary." (-> (get-challenger) :prompt first :msg)) "1st quandary")
    (prompt-choice :challenger "No action")
    (prompt-choice :challenger "Card from deck")
    (is (= "You accessed Quandary." (-> (get-challenger) :prompt first :msg)) "2nd quandary")
    (prompt-choice :challenger "No action")
    (prompt-choice :challenger "Card from deck")
    (is (= "You accessed Quandary." (-> (get-challenger) :prompt first :msg)) "3rd quandary")
    (prompt-choice :challenger "No action")
    (is (not (:run @state)))
    (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
    (prompt-card :contestant (find-card "The Maker's Eye" (:discard (get-challenger))))
    (is (= 1 (count (get-in @state [:challenger :rfg]))) "One card RFGed")
    (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
    (is (empty? (:prompt (get-contestant))) "Cannot use Skontestantios twcharacter")))

(deftest silhouette:-stealth-operative
  ;; Silhouette
  (testing "Expose trigger ability resolves completely before access. Issue #2173"
    (do-game
      (new-game
        (default-contestant ["Psychic Field" (qty "Fetal AI" 10)])
        (make-deck "Silhouette: Stealth Operative" ["Feedback Filter" "Inside Job"]))
      (starting-hand state :contestant ["Psychic Field" "Fetal AI"])
      (play-from-hand state :contestant "Psychic Field" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Feedback Filter")
      (is (= 3 (:credit (get-challenger))) "Challenger has 3 credits")
      (let [psychic (get-content state :party1 0)
            ff (get-hazard state 0)]
        (run-empty-locale state :hq)
        (is (:run @state) "On successful run trigger effects")
        (prompt-select :challenger psychic)
        (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")
        (prompt-choice :contestant "2 [Credits]")
        (prompt-choice :challenger "0 [Credits]")
        (card-ability state :challenger ff 0)
        (prompt-choice :challenger "Done")
        (is (zero? (:credit (get-challenger))) "Challenger has no more credits left")
        (is (= 1 (count (:hand (get-challenger)))) "Prevented 1 net damage")
        (is (empty? (:discard (get-challenger))) "No cards discarded")
        (is (:run @state) "On run access phase")
        (prompt-choice :challenger "Done")
        (is (empty? (:hand (get-challenger))) "Suffered 1 net damage due to accessing Fetal AI")
        (is (= 1 (count (:discard (get-challenger)))) "Discarded 1 card due to net damage")
        (is (:run @state) "Resolving access triggers")
        (prompt-choice :challenger "No action")
        (is (zero? (count (:scored (get-challenger)))) "Challenger has no credits to be able to steal Fetal AI")
        (is (not (:run @state)) "Run has now ended")
        (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
  (testing "with Temüjin; broken interaction with other successful-run triggers. Issue #1968"
    (do-game
      (new-game
        (default-contestant ["PAD Campaign" (qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "Beanstalk Royalties" 3)])
        (make-deck "Silhouette: Stealth Operative" ["Temüjin Contract" "Desperado"]))
      (starting-hand state :contestant ["Hedge Fund" "PAD Campaign"])
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Temüjin Contract")
      (prompt-choice :challenger "HQ")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-empty-locale state :hq)
      (prompt-choice :challenger "Temüjin Contract")
      (prompt-select :challenger (get-content state :party1 0))
      (prompt-choice :challenger "No action")
      (is (= "HQ" (:locale-target (get-radicle state 0))) "Temujin still targeting HQ")
      (is (= 16 (get-counters (get-radicle state 0) :credit)) "16 cr on Temujin")
      (is (= 8 (:credit (get-challenger))) "Gained 4cr")
      ;; second run
      (run-empty-locale state :hq)
      (prompt-choice :challenger "No action")
      (is (= "HQ" (:locale-target (get-radicle state 0))) "Temujin still targeting HQ")
      (is (= 12 (:credit (get-challenger))) "Gained 4cr")
      (is (= 12 (get-counters (get-radicle state 0) :credit)) "12 cr on Temujin"))))

(deftest spark-agency:-worldswide-reach
  ;; Spark Agency - Revealing advertisements
  (do-game
    (new-game
      (make-deck "Spark Agency: Worldswide Reach" [(qty "Launch Campaign" 3)])
      (default-challenger))
    (play-from-hand state :contestant "Launch Campaign" "New party")
    (play-from-hand state :contestant "Launch Campaign" "New party")
    (play-from-hand state :contestant "Launch Campaign" "New party")
    (let [lc1 (get-content state :party1 0)
          lc2 (get-content state :party2 0)
          lc3 (get-content state :party3 0)]
      (core/reveal state :contestant lc1)
      (is (= 4 (:credit (get-challenger)))
          "Challenger lost 1 credit from reveal of advertisement (Contestant turn)")
      (core/reveal state :contestant lc3)
      (is (= 4 (:credit (get-challenger)))
          "Challenger did not lose credit from second Spark reveal")
      (take-credits state :contestant)
      (run-on state "Locale 1")
      (core/reveal state :contestant lc2)
      (is (= 3 (:credit (get-challenger)))
          "Challenger lost 1 credit from reveal of advertisement (Challenger turn)"))))

(deftest sso-industries:-fueling-innovation
  ;; SSO Industries: Fueling Innovation - add advancement tokens on character for faceup agendas
  (do-game
    (new-game
      (make-deck "SSO Industries: Fueling Innovation"
                 [(qty "Hortum" 2) (qty "Oaktown Renovation" 2) "Braintrust"])
      (default-challenger))
    (play-from-hand state :contestant "Braintrust" "New party")
    (take-credits state :contestant)
    (is (empty? (:prompt (get-contestant))) "Not prompted when no faceup agenda available")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Oaktown Renovation" "New party")
    (take-credits state :contestant)
    (is (empty? (:prompt (get-contestant))) "Not prompted when no character available")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hortum" "HQ")
    (play-from-hand state :contestant "Hortum" "R&D")
    (let [h0 (get-character state :hq 0)
          h1 (get-character state :rd 0)]
      (is (zero? (get-counters (refresh h0) :advancement)) "Starts with 0 tokens")
      (is (zero? (get-counters (refresh h1) :advancement)) "Starts with 0 tokens")
      (take-credits state :contestant)
      (prompt-choice :contestant "Yes")
      (prompt-select :contestant (refresh h0))
      (is (= 2 (get-counters (refresh h0) :advancement)) "Gains 2 tokens")
      (is (zero? (get-counters (refresh h1) :advancement)) "Stays at 0 tokens")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Oaktown Renovation" "New party")
      (take-credits state :contestant)
      (prompt-choice :contestant "Yes")
      (prompt-select :contestant (refresh h1))
      (is (= 2 (get-counters (refresh h0) :advancement)) "Stays at 2 tokens")
      (is (= 4 (get-counters (refresh h1) :advancement)) "Gains 4 tokens")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (empty? (:prompt (get-contestant))) "Not prompted when all character advanced"))))

(deftest strategic-innovations:-future-forward
  ;; Strategic Innovations: Future Forward
  (do-game
    (new-game
      (make-deck "Strategic Innovations: Future Forward"
                 [(qty "Hedge Fund" 2) (qty "Eli 1.0" 2) (qty "Crick" 2)])
      (default-challenger))
    (play-from-hand state :contestant "Eli 1.0" "New party")
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Crick" "New party")
    (let [i1 (get-character state :party1 0)
          i2 (get-character state :party2 0)]
      (take-credits state :contestant 0)
      (take-credits state :challenger)
      (core/reveal state :contestant i1)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 1 (count (:prompt (get-contestant)))) "Contestant prompted to trigger Strategic Innovations")
      (prompt-select :contestant (first (:discard (get-contestant))))
      (is (empty? (:discard (get-contestant))) "Hedge Fund moved back to R&D")
      (take-credits state :contestant)
      (core/reveal state :contestant i2)
      (take-credits state :challenger)
      (is (zero? (count (:prompt (get-contestant))))
          "Contestant not prompted to trigger Strategic Innovations"))))

(deftest the-foundry:-refining-the-process
  ;; The Foundry
  (testing "interaction with Accelerated Beta Test"
    (do-game
      (new-game
        (make-deck "The Foundry: Refining the Process" [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)])
        (default-challenger))
      (starting-hand state :contestant ["Accelerated Beta Test"])
      (play-from-hand state :contestant "Accelerated Beta Test" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (prompt-choice :contestant "Yes")
      (prompt-select :contestant (find-card "Eli 1.0" (:play-area (get-contestant))))
      (prompt-choice :contestant "Archives")
      (prompt-choice :contestant "Yes")
      (is (empty? (:play-area (get-contestant))) "Play area shuffled into R&D"))))

(deftest the-outfit:-family-owned-and-operated
  ;; The Outfit - Gain 3 whenever you take at least 1 bad publicity
  (testing "basic test"
    (do-game
      (new-game
        (make-deck "The Outfit: Family Owned and Operated" ["Hostile Takeover" "Profiteering"])
        (default-challenger))
      (play-from-hand state :contestant "Hostile Takeover" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (is (= 1 (:bad-publicity (get-contestant))) "Take 1 bad publicity")
      (is (= 15 (:credit (get-contestant))) "Contestant should gain 10 credits")
      (play-from-hand state :contestant "Profiteering" "New party")
      (score-agenda state :contestant (get-content state :party2 0))
      (prompt-choice :contestant "3")  ;; Take 3 bad publicity from Profiteering, gain 15
      (is (= 4 (:bad-publicity (get-contestant))) "Contestant should gain 1 bad publicity")
      (is (= 33 (:credit (get-contestant))) "Contestant should gain 18 credits")))
  (testing "with Profiteering - Only gain 3 credits when taking more than 1 bad publicity in a single effect"
    (do-game
      (new-game
        (make-deck "The Outfit: Family Owned and Operated" ["Profiteering"])
        (default-challenger))
      (play-from-hand state :contestant "Profiteering" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (prompt-choice :contestant "3")
      (is (= 3 (:bad-publicity (get-contestant))) "Take 3 bad publicity")
      (is (= 23 (:credit (get-contestant))) "Gain 15 from Profiteering + 3 from The Outfit")))
  (testing "vs Valencia - 1 bad pub at start means 8 credits to start with"
    (do-game
      (new-game
        (make-deck "The Outfit: Family Owned and Operated" ["Hostile Takeover"])
        (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
      (is (= 1 (:bad-publicity (get-contestant))) "The Outfit starts with 1 bad publicity")
      (is (= 8 (:credit (get-contestant))) "The Outfit starts with 8 credits")
      (play-from-hand state :contestant "Hostile Takeover" "New party")
      (score-agenda state :contestant (get-content state :party1 0))
      (is (= 2 (:bad-publicity (get-contestant))) "Take 1 bad publicity")
      (is (= 18 (:credit (get-contestant))) "Gain 7 from Hostile Takeover + 3 from The Outfit"))))

(deftest titan-transnational:-investing-in-your-future
  ;; Titan Transnational
  (testing "Add a counter to a scored agenda"
    (do-game
      (new-game
        (make-deck "Titan Transnational: Investing In Your Future" ["Project Atlas"])
        (default-challenger))
      (play-from-hand state :contestant "Project Atlas" "New party")
      (let [atl (get-content state :party1 0)]
        (core/gain state :contestant :click 1)
        (core/advance state :contestant {:card (refresh atl)})
        (core/advance state :contestant {:card (refresh atl)})
        (core/advance state :contestant {:card (refresh atl)})
        (core/score state :contestant {:card (refresh atl)})
        (let [scored (get-scored state :contestant 0)]
          (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))
  (testing "only use one counter of Contestantorate Sales Team"
    (do-game
      (new-game
        (make-deck "Titan Transnational: Investing In Your Future" ["Contestantorate Sales Team" "Mark Yale"])
        (default-challenger))
      (play-from-hand state :contestant "Contestantorate Sales Team" "New party")
      (play-from-hand state :contestant "Mark Yale" "New party")
      (let [cst (get-content state :party1 0)
            my (get-content state :party2 0)]
        (core/gain state :contestant :click 3)
        (core/advance state :contestant {:card (refresh cst)})
        (core/advance state :contestant {:card (refresh cst)})
        (core/advance state :contestant {:card (refresh cst)})
        (core/advance state :contestant {:card (refresh cst)})
        (core/score state :contestant {:card (refresh cst)})
        (let [scored (get-scored state :contestant 0)]
          (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
          (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
          (core/reveal state :contestant my)
          (card-ability state :contestant my 1)
          (prompt-select :contestant (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
          (card-ability state :contestant my 1)
          (prompt-select :contestant (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "No agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale"))))))

(deftest weyland-consortium:-builder-of-nations
  ;; Builder of Nations
  (testing "1 meat damage per turn at most"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations" [(qty "Hedge Fund" 3)])
        (default-challenger))
      (let [bon (get-in @state [:contestant :identity])]
        (card-ability state :contestant bon 0)
        (prompt-choice :contestant "Cancel")
        (is (zero? (count (:discard (get-challenger)))) "Challenger took no meat damage from BoN")
        (card-ability state :contestant bon 0)
        (prompt-choice :contestant "Yes")
        (is (= 1 (count (:discard (get-challenger)))) "Challenger took 1 meat damage from BoN")
        (card-ability state :contestant bon 0)
        (is (= 1 (count (:discard (get-challenger)))) "Challenger took only 1 meat damage from BoN total")
        (is (zero? (count (:prompt (get-contestant))))))))
  (testing "2 meat damage from ID ability when The Cleaners is scored"
    (do-game
      (new-game
        (make-deck "Weyland Consortium: Builder of Nations" [(qty "The Cleaners" 3) (qty "Ice Wall" 3)])
        (default-challenger [(qty "Sure Gamble" 2)]))
      (play-from-hand state :contestant "The Cleaners" "New party")
      (let [clean (get-content state :party1 0)]
        (score-agenda state :contestant clean)
        (let [bon (get-in @state [:contestant :identity])]
          (card-ability state :contestant bon 0)
          (prompt-choice :contestant "Yes")
          (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 meat damage from BoN/Cleaners combo"))))))

(deftest whizzard:-master-gamer
  ;; Whizzard - Recurring credits
  (do-game
    (new-game (default-contestant)
              (make-deck "Whizzard: Master Gamer" ["Sure Gamble"]))
    (let [click-whizzard (fn [n] (dotimes [i n] (card-ability state :challenger (:identity (get-challenger)) 0)))]
      (is (changes-credits (get-challenger) 1 (click-whizzard 1)))
      (is (changes-credits (get-challenger) 2 (click-whizzard 5)) "Can't take more than 3 Whizzard credits")
      (take-credits state :contestant)
      (is (changes-credits (get-challenger) 3 (click-whizzard 3)) "Credits reset at start of Challenger's turn")
      (take-credits state :challenger)
      (is (changes-credits (get-challenger) 0 (click-whizzard 1)) "Credits don't reset at start of Contestant's turn"))))

(deftest wyvern:-chemically-enhanced
  ;; Wyvern: Chemically Enhanced
  (do-game
    (new-game (default-contestant [(qty "Launch Campaign" 3)])
              (make-deck "Wyvern: Chemically Enhanced"
                         [(qty "Sure Gamble" 2) "Corroder"
                          "Clone Chip" "Easy Mark"]))
    (play-from-hand state :contestant "Launch Campaign" "New party")
    (play-from-hand state :contestant "Launch Campaign" "New party")
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Easy Mark")
    (play-from-hand state :challenger "Corroder")
    (run-empty-locale state "Locale 1")
    (prompt-choice-partial :challenger "Pay")  ;; discard Launch Campaign, should trigger wyvern
    (is (= "Sure Gamble" (:title (last (:discard (get-challenger)))))
        "Sure Gamble still in Wyvern's discard")
    (is (some #(= "Easy Mark" (:title %)) (:deck (get-challenger))) "Easy Mark moved to deck")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Clone Chip")
    (run-empty-locale state "Locale 2")
    (prompt-choice-partial :challenger "Pay")
    (is (= "Sure Gamble" (:title (last (:discard (get-challenger))))) "Sure Gamble still in Wyvern's discard")))
