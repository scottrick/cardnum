(ns game-test.cards.resources
  (:require [game.core :as core]
            [game.utils :refer [has?]]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "resources"))

(deftest au-revoir
  ;; Au Revoir - Gain 1 credit every time you jack out
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Au Revoir" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Au Revoir")
    (run-on state "Archives")
    (core/no-action state :contestant nil)
    (core/jack-out state :challenger nil)
    (is (= 5 (:credit (get-challenger))) "Gained 1 credit from jacking out")
    (play-from-hand state :challenger "Au Revoir")
    (run-on state "Archives")
    (core/no-action state :contestant nil)
    (core/jack-out state :challenger nil)
    (is (= 6 (:credit (get-challenger))) "Gained 1 credit from each copy of Au Revoir")))

(deftest consume
  ;; Consume - gain virus counter for discarding contestant card. click to get 2c per counter.
  (testing "Discard and cash out"
    (do-game
      (new-game (default-contestant ["Adonis Campaign"])
                (default-challenger ["Consume"]))
      (play-from-hand state :contestant "Adonis Campaign" "New party")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Consume")
      (let [c (get-resource state 0)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (run-empty-locale state "Locale 1")
        (prompt-choice-partial :challenger "Pay")
        (prompt-choice-partial :challenger "Yes")
        (is (= 1 (count (:discard (get-contestant)))) "Adonis Campaign discarded")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (zero? (:credit (get-challenger))) "Challenger starts with no credits")
        (card-ability state :challenger c 0)
        (is (= 2 (:credit (get-challenger))) "Challenger gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters"))))
  (testing "Hivemind interaction"
    (do-game
      (new-game (default-contestant ["Adonis Campaign"])
                (default-challenger ["Consume" "Hivemind"]))
      (play-from-hand state :contestant "Adonis Campaign" "New party")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 3)
      (play-from-hand state :challenger "Consume")
      (play-from-hand state :challenger "Hivemind")
      (let [c (get-resource state 0)
            h (get-resource state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (run-empty-locale state "Locale 1")
        (prompt-choice-partial :challenger "Pay")
        (prompt-choice-partial :challenger "Yes")
        (is (= 1 (count (:discard (get-contestant)))) "Adonis Campaign discarded")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind retains counter")
        (is (zero? (:credit (get-challenger))) "Challenger starts with no credits")
        (card-ability state :challenger c 0)
        (is (= 4 (:credit (get-challenger))) "Challenger gains 4 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters"))))
  (testing "Hivemind counters only"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Consume" "Hivemind"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Consume")
      (play-from-hand state :challenger "Hivemind")
      (let [c (get-resource state 0)
            h (get-resource state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (is (zero? (:credit (get-challenger))) "Challenger starts with no credits")
        (card-ability state :challenger c 0)
        (is (= 2 (:credit (get-challenger))) "Challenger gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters")))))

(deftest crescentus
  ;; Crescentus should only work on revealed character
  (do-game
    (new-game (default-contestant ["Quandary"])
              (default-challenger ["Crescentus"]))
    (play-from-hand state :contestant "Quandary" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Crescentus")
    (run-on state "HQ")
    (let [cres (get-resource state 0)
          q (get-character state :hq 0)]
      (card-ability state :challenger cres 0)
      (is (not (nil? (get-resource state 0))) "Crescentus could not be used because the Character is not revealed")
      (core/reveal state :contestant q)
      (is (:revealed (refresh q)) "Quandary is now revealed")
      (card-ability state :challenger cres 0)
      (is (nil? (get-resource state 0)) "Crescentus could be used because the Character is revealed")
      (is (not (:revealed (refresh q))) "Quandary is no longer revealed"))))

(deftest datasucker
  ;; Datasucker - Reduce strength of encountered Character
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Fire Wall"])
                (default-challenger ["Datasucker"]))
      (play-from-hand state :contestant "Fire Wall" "New party")
      (take-credits state :contestant)
      (core/gain state :challenger :click 3)
      (play-from-hand state :challenger "Datasucker")
      (let [ds (get-resource state 0)
            fw (get-character state :party1 0)]
        (run-empty-locale state "Archives")
        (is (= 1 (get-counters (refresh ds) :virus)))
        (run-empty-locale state "Archives")
        (is (= 2 (get-counters (refresh ds) :virus)))
        (run-on state "Locale 1")
        (run-continue state)
        (run-successful state)
        (is (= 2 (get-counters (refresh ds) :virus)) "No counter gained, not a central locale")
        (run-on state "Locale 1")
        (core/reveal state :contestant fw)
        (is (= 5 (:current-strength (refresh fw))))
        (card-ability state :challenger ds 0)
        (is (= 1 (get-counters (refresh ds) :virus)) "1 counter spent from Datasucker")
        (is (= 4 (:current-strength (refresh fw))) "Fire Wall strength lowered by 1"))))
  (testing "does not affect next character when current is discarded. Issue #1788"
    (do-game
      (new-game
        (default-contestant ["Wraparound" "Spiderweb"])
        (default-contestant ["Datasucker" "Parasite"]))
      (play-from-hand state :contestant "Spiderweb" "HQ")
      (play-from-hand state :contestant "Wraparound" "HQ")
      (take-credits state :contestant)
      (core/gain state :contestant :credit 10)
      (play-from-hand state :challenger "Datasucker")
      (let [sucker (get-resource state 0)
            spider (get-character state :hq 0)
            wrap (get-character state :hq 1)]
        (core/add-counter state :challenger sucker :virus 2)
        (core/reveal state :contestant spider)
        (core/reveal state :contestant wrap)
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger (refresh spider))
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :challenger (refresh sucker) 0)
        (card-ability state :challenger (refresh sucker) 0)
        (is (find-card "Spiderweb" (:discard (get-contestant))) "Spiderweb discarded by Parasite + Datasucker")
        (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Datasucker")))))

(deftest dhegdheer
  ;; Dheghdheer - hosting a breaker with strength based on unused MU should calculate correctly
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Adept" "Dhegdheer"]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 5)
    (play-from-hand state :challenger "Dhegdheer")
    (play-from-hand state :challenger "Adept")
    (is (= 3 (:credit (get-challenger))) "3 credits left after individual places")
    (is (= 2 (core/available-mu state)) "2 MU used")
    (let [dheg (get-resource state 0)
          adpt (get-resource state 1)]
      (is (= 4 (:current-strength (refresh adpt))) "Adept at 4 strength individually")
      (card-ability state :challenger dheg 1)
      (prompt-select :challenger (refresh adpt))
      (let [hosted-adpt (first (:hosted (refresh dheg)))]
        (is (= 4 (:credit (get-challenger))) "4 credits left after hosting")
        (is (= 4 (core/available-mu state)) "0 MU used")
        (is (= 6 (:current-strength (refresh hosted-adpt))) "Adept at 6 strength hosted")))))

(deftest disrupter
  ;; Disrupter
  (do-game
    (new-game (default-contestant [(qty "SEA Source" 2)])
              (default-challenger ["Disrupter"]))
    (take-credits state :contestant)
    (run-empty-locale state "Archives")
    (play-from-hand state :challenger "Disrupter")
    (take-credits state :challenger)
    (play-from-hand state :contestant "SEA Source")
    (prompt-choice :challenger "Yes")
    (is (zero? (-> (get-contestant) :prompt first :base)) "Base trace should now be 0")
    (is (= 1 (-> (get-challenger) :discard count)) "Disrupter should be in Heap")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (zero? (:tag (get-challenger))) "Challenger should gain no tag from beating trace")
    (play-from-hand state :contestant "SEA Source")
    (is (= 3 (-> (get-contestant) :prompt first :base)) "Base trace should be reset to 3")))

(deftest diwan
  ;; Diwan - Full test
  (do-game
    (new-game (default-contestant [(qty "Character Wall" 3) (qty "Fire Wall" 3) (qty "Crisium Grid" 2)])
              (default-challenger ["Diwan"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Diwan")
    (prompt-choice :challenger "HQ")
    (take-credits state :challenger)
    (is (= 8 (:credit (get-contestant))) "8 credits for contestant at start of second turn")
    (play-from-hand state :contestant "Character Wall" "R&D")
    (is (= 8 (:credit (get-contestant))) "Diwan did not charge extra for place on another locale")
    (play-from-hand state :contestant "Character Wall" "HQ")
    (is (= 7 (:credit (get-contestant))) "Diwan charged 1cr to place character protecting the named locale")
    (play-from-hand state :contestant "Crisium Grid" "HQ")
    (is (= 7 (:credit (get-contestant))) "Diwan didn't charge to place another region in root of HQ")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Character Wall" "HQ")
    (is (= 5 (:credit (get-contestant))) "Diwan charged 1cr + 1cr to place a second character protecting the named locale")
    (core/gain state :contestant :click 1)
    (core/purge state :contestant)
    (play-from-hand state :contestant "Fire Wall" "HQ") ; 2cr cost from normal place cost
    (is (= "Diwan" (-> (get-challenger) :discard first :title)) "Diwan was discarded from purge")
    (is (= 3 (:credit (get-contestant))) "No charge for places after Diwan purged")))

(deftest djinn
  ;; Djinn
  (testing "Hosted Chakana does not disable advancing agendas. Issue #750"
    (do-game
      (new-game (default-contestant ["Priority Requisition"])
                (default-challenger ["Djinn" "Chakana"]))
      (play-from-hand state :contestant "Priority Requisition" "New party")
      (take-credits state :contestant 2)
      (play-from-hand state :challenger "Djinn")
      (let [djinn (get-resource state 0)
            agenda (get-content state :party1 0)]
        (is agenda "Agenda was placed")
        (card-ability state :challenger djinn 1)
        (prompt-select :challenger (find-card "Chakana" (:hand (get-challenger))))
        (let [chak (first (:hosted (refresh djinn)))]
          (is (= "Chakana" (:title chak)) "Djinn has a hosted Chakana")
          ;; manually add 3 counters
          (core/add-counter state :challenger (first (:hosted (refresh djinn))) :virus 3)
          (take-credits state :challenger 2)
          (core/advance state :contestant {:card agenda})
          (is (= 1 (get-counters (refresh agenda) :advancement)) "Agenda was advanced")))))
  (testing "Host a non-characterbreaker resource"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Djinn" "Chakana"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Djinn")
      (is (= 3 (core/available-mu state)))
      (let [djinn (get-resource state 0)]
        (card-ability state :challenger djinn 1)
        (prompt-select :challenger (find-card "Chakana" (:hand (get-challenger))))
        (is (= 3 (core/available-mu state)) "No memory used to host on Djinn")
        (is (= "Chakana" (:title (first (:hosted (refresh djinn))))) "Djinn has a hosted Chakana")
        (is (= 1 (:credit (get-challenger))) "Full cost to host on Djinn"))))
  (testing "Tutor a virus resource"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Djinn" "Parasite"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Djinn")
      (core/move state :challenger (find-card "Parasite" (:hand (get-challenger))) :deck)
      (is (zero? (count (:hand (get-challenger)))) "No cards in hand after moving Parasite to deck")
      (let [djinn (get-resource state 0)]
        (card-ability state :challenger djinn 0)
        (prompt-card :challenger (find-card "Parasite" (:deck (get-challenger))))
        (is (= "Parasite" (:title (first (:hand (get-challenger))))) "Djinn moved Parasite to hand")
        (is (= 2 (:credit (get-challenger))) "1cr to use Djinn ability")
        (is (= 2 (:click (get-challenger))) "1click to use Djinn ability")))))

(deftest equivocation
  ;; Equivocation - interactions with other successful-run events.
  (do-game
    (new-game
      (default-contestant [(qty "Restructure" 3) (qty "Hedge Fund" 3)])
      (make-deck "Laramy Fisk: Savvy Investor" ["Equivocation" "Desperado"]))
    (starting-hand state :contestant ["Hedge Fund"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Equivocation")
    (play-from-hand state :challenger "Desperado")
    (run-empty-locale state :rd)
    (prompt-choice :challenger "Laramy Fisk: Savvy Investor")
    (prompt-choice :challenger "Yes")
    (is (= 2 (count (:hand (get-contestant)))) "Contestant forced to draw by Fisk")
    (prompt-choice :challenger "Yes") ; Equivocation prompt
    (prompt-choice :challenger "Yes") ; force the draw
    (is (= 1 (:credit (get-challenger))) "Challenger gained 1cr from Desperado")
    (is (= 3 (count (:hand (get-contestant)))) "Contestant forced to draw by Equivocation")
    (prompt-choice :challenger "No action")
    (is (not (:run @state)) "Run ended")))

(deftest false-echo
  ;; False Echo - choice for Contestant
  (do-game
    (new-game (default-contestant [(qty "Character Wall" 3)])
              (default-challenger [(qty "False Echo" 3)]))
    (play-from-hand state :contestant "Character Wall" "Archives")
    (play-from-hand state :contestant "Character Wall" "Archives")
    (take-credits state :contestant)
    (play-from-hand state :challenger "False Echo")
    (play-from-hand state :challenger "False Echo")
    (run-on state "Archives")
    (run-continue state)
    (let [echo1 (get-resource state 0)
          echo2 (get-resource state 1)]
      (card-ability state :challenger echo1 0)
      (prompt-choice :contestant "Add to HQ")
      (is (= 2 (count (:hand (get-contestant)))) "Character Wall added to HQ")
      (is (= 1 (count (:discard (get-challenger)))) "False Echo discarded")
      (run-continue state)
      (card-ability state :challenger echo2 0)
      (prompt-choice :contestant "Reveal")
      (is (:revealed (get-character state :archives 0)) "Character Wall revealed")
      (is (= 2 (count (:discard (get-challenger)))) "False Echo discarded"))))

(deftest gravedigger
  ;; Gravedigger - Gain counters when Contestant cards are discarded, spend click-counter to mill Contestant
  (do-game
    (new-game (default-contestant [(qty "Launch Campaign" 2) (qty "Enigma" 2)])
              (default-challenger ["Gravedigger"]))
    (play-from-hand state :contestant "Launch Campaign" "New party")
    (play-from-hand state :contestant "Launch Campaign" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Gravedigger")
    (let [gd (get-resource state 0)]
      (core/discard state :contestant (get-content state :party1 0))
      (is (= 1 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/discard state :contestant (get-content state :party2 0))
      (is (= 2 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/move state :contestant (find-card "Enigma" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Enigma" (:hand (get-contestant))) :deck)
      (is (= 2 (count (:deck (get-contestant)))))
      (card-ability state :challenger gd 0)
      (is (= 1 (get-counters (refresh gd) :virus)) "Spent 1 counter from Gravedigger")
      (is (= 2 (:click (get-challenger))) "Spent 1 click")
      (is (= 1 (count (:deck (get-contestant)))))
      (is (= 3 (count (:discard (get-contestant)))) "Milled 1 card from R&D"))))

(deftest harbinger
  ;; Harbinger
  (testing "place facedown when Blacklist placed"
    (do-game
      (new-game (default-contestant ["Blacklist"])
                (default-challenger ["Harbinger"]))
      (play-from-hand state :contestant "Blacklist" "New party")
      (core/reveal state :contestant (get-content state :party1 0))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Harbinger")
      (core/discard state :challenger (-> (get-challenger) :rig :resource first))
      (is (zero? (count (:discard (get-challenger)))) "Harbinger not in heap")
      (is (-> (get-challenger) :rig :facedown first :facedown) "Harbinger placed facedown"))))

(deftest hyperdriver
  ;; Hyperdriver - Remove from game to gain 3 clicks
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Hyperdriver"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Hyperdriver")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (:challenger-phase-12 @state) "Challenger in Step 1.2")
      (let [hyp (get-resource state 0)]
        (card-ability state :challenger hyp 0)
        (core/end-phase-12 state :challenger nil)
        (is (= 7 (:click (get-challenger))) "Gained 3 clicks")
        (is (= 1 (count (:rfg (get-challenger)))) "Hyperdriver removed from game"))))
  (testing "triggering a Dhegdeered Hyperdriver should not grant +3 MU"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Hyperdriver" "Dhegdheer"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Dhegdheer")
      (let [dheg (get-resource state 0)]
        (card-ability state :challenger dheg 0)
        (prompt-select :challenger (find-card "Hyperdriver" (:hand (get-challenger))))
        (is (= 4 (core/available-mu state)) "0 MU used by Hyperdriver hosted on Dhegdheer")
        (is (= 2 (:click (get-challenger))) "2 clicks used")
        (is (= 3 (:credit (get-challenger))) "2 credits used")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (:challenger-phase-12 @state) "Challenger in Step 1.2")
        (let [hyp (first (:hosted (refresh dheg)))]
          (card-ability state :challenger hyp 0)
          (core/end-phase-12 state :challenger nil)
          (is (= 7 (:click (get-challenger))) "Used Hyperdriver")
          (is (= 4 (core/available-mu state)) "Still 0 MU used after Hyperdriver removed from game"))))))

(deftest imp
  ;; Imp
  (testing "Full test"
    (letfn [(imp-test [card]
              (do-game
                (new-game (default-contestant [card])
                          (default-challenger ["Imp"]))
                (take-credits state :contestant)
                (play-from-hand state :challenger "Imp")
                (run-empty-locale state "HQ")
                (prompt-choice-partial :challenger "Imp")
                (is (= 1 (count (:discard (get-contestant)))))))]
      (doall (map imp-test
                  ["Hostile Takeover"
                   "Dedicated Response Team"
                   "Beanstalk Royalties"
                   "Character Wall"
                   "Oberth Protocol"]))))
  (testing "vs an ambush"
    (do-game
      (new-game (default-contestant ["Prisec"])
                (default-challenger ["Imp" (qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Prisec" "New party")
      (take-credits state :contestant)
      (let [credits (:credit (get-contestant))
            tags (:tag (get-challenger))
            grip (count (:hand (get-challenger)))
            archives (count (:discard (get-contestant)))]
        (play-from-hand state :challenger "Imp")
        (run-empty-locale state :party1)
        (prompt-choice :contestant "Yes")
        (prompt-choice-partial :challenger "Imp")
        (is (= 2 (- credits (:credit (get-contestant)))) "Contestant paid 2 for Prisec")
        (is (= 1 (- (:tag (get-challenger)) tags)) "Challenger has 1 tag")
        (is (= 2 (- grip (count (:hand (get-challenger))))) "Challenger took 1 meat damage")
        (is (= 1 (- (count (:discard (get-contestant))) archives)) "Used Imp to discard Prisec"))))
  (testing "vs The Future Perfect"
    ;; Psi-game happens on access [5.5.1], Imp is a discard ability [5.5.2]
    (do-game
      (new-game (default-contestant ["The Future Perfect"])
                (default-challenger ["Imp"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Imp")
      (testing "Access, contestant wins psi-game"
        (run-empty-locale state "HQ")
        ;; Should access TFP at this point
        (prompt-choice :contestant "1 [Credits]")
        (prompt-choice :challenger "0 [Credits]")
        (prompt-choice-partial :challenger "Imp")
        (take-credits state :challenger)
        (is (= "The Future Perfect" (get-in @state [:contestant :discard 0 :title])) "TFP discarded")
        (is (zero? (:agenda-point (get-challenger))) "Challenger did not steal TFP")
        (core/move state :contestant (find-card "The Future Perfect" (:discard (get-contestant))) :hand))
      (take-credits state :challenger)
      (take-credits state :contestant)
      (testing "Access, challenger wins psi-game"
        (run-empty-locale state "HQ")
        ;; Access prompt for TFP
        (prompt-choice :contestant "0 [Credits]")
        (prompt-choice :challenger "0 [Credits]")
        ;; Fail psi game
        (prompt-choice-partial :challenger "Imp")
        (is (= "The Future Perfect" (get-in @state [:contestant :discard 0 :title])) "TFP discarded")
        (is (zero? (:agenda-point (get-challenger))) "Challenger did not steal TFP"))))
  (testing "vs cards in Archives"
    (do-game
      (new-game (default-contestant ["Hostile Takeover"])
                (default-challenger ["Imp"]))
      (core/move state :contestant (find-card "Hostile Takeover" (:hand (get-contestant))) :discard)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Imp")
      (run-empty-locale state "Archives")
      (is (= ["Steal"] (->> (get-challenger) :prompt first :choices)) "Should only get the option to steal Hostile on access in Archives"))))

(deftest incubator
  ;; Incubator - Gain 1 virus counter per turn; discard to move them to an placed virus resource
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Incubator" "Datasucker"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Datasucker")
    (play-from-hand state :challenger "Incubator")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (let [ds (get-resource state 0)
          incub (get-resource state 1)]
      (is (= 1 (get-counters (refresh incub) :virus)) "Incubator gained 1 virus counter")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 2 (get-counters (refresh incub) :virus)) "Incubator has 2 virus counters")
      (card-ability state :challenger incub 0)
      (prompt-select :challenger ds)
      (is (= 2 (get-counters (refresh ds) :virus)) "Datasucker has 2 virus counters moved from Incubator")
      (is (= 1 (count (get-resource state))))
      (is (= 1 (count (:discard (get-challenger)))) "Incubator discarded")
      (is (= 3 (:click (get-challenger)))))))

(deftest ixodidae
  ;; Ixodidae should not trigger on psi-games
  (do-game
    (new-game (default-contestant ["Snowflake"])
              (default-challenger ["Ixodidae" "Lamprey"]))
    (play-from-hand state :contestant "Snowflake" "HQ")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))) "Contestant at 7 credits")
    (play-from-hand state :challenger "Ixodidae")
    (play-from-hand state :challenger "Lamprey")
    (is (= 3 (:credit (get-challenger))) "Challenger paid 3 credits to place Ixodidae and Lamprey")
    (run-on state :hq)
    (let [s (get-character state :hq 0)]
      (core/reveal state :contestant s)
      (card-subroutine state :contestant s 0)
      (is (prompt-is-card? :contestant s) "Contestant prompt is on Snowflake")
      (is (prompt-is-card? :challenger s) "Challenger prompt is on Snowflake")
      (is (= 6 (:credit (get-contestant))) "Contestant paid 1 credit to revealz Snowflake")
      (prompt-choice :contestant "1")
      (prompt-choice :challenger "1")
      (is (= 5 (:credit (get-contestant))) "Contestant paid 1 credit to psi game")
      (is (= 2 (:credit (get-challenger))) "Challenger did not gain 1 credit from Ixodidae when contestant spent on psi game")
      (run-continue state)
      (run-successful state)
      (is (= 4 (:credit (get-contestant))) "Contestant lost 1 credit to Lamprey")
      (is (= 3 (:credit (get-challenger))) "Challenger gains 1 credit from Ixodidae due to Lamprey"))))

(deftest lamprey
  ;; Lamprey - Contestant loses 1 credit for each successful HQ run; discarded on purge
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Lamprey"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Lamprey")
    (let [lamp (get-resource state 0)]
      (run-empty-locale state :hq)
      (is (= 7 (:credit (get-contestant))) "Contestant lost 1 credit")
      (run-empty-locale state :hq)
      (is (= 6 (:credit (get-contestant))) "Contestant lost 1 credit")
      (run-empty-locale state :hq)
      (is (= 5 (:credit (get-contestant))) "Contestant lost 1 credit")
      (take-credits state :challenger)
      (core/purge state :contestant)
      (is (empty? (get-resource state)) "Lamprey discarded by purge"))))

(deftest leprechaun
  ;; Leprechaun - hosting a breaker with strength based on unused MU should calculate correctly
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Adept" "Leprechaun"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5)
      (play-from-hand state :challenger "Leprechaun")
      (play-from-hand state :challenger "Adept")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (let [lep (get-resource state 0)
            adpt (get-resource state 1)]
        (is (= 3 (:current-strength (refresh adpt))) "Adept at 3 strength individually")
        (card-ability state :challenger lep 1)
        (prompt-select :challenger (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh lep)))]
          (is (= 3 (core/available-mu state)) "1 MU used")
          (is (= 5 (:current-strength (refresh hosted-adpt))) "Adept at 5 strength hosted")))))
  (testing "Keep MU the same when hosting or discarding hosted resources"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Leprechaun" "Hyperdriver" "Imp"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Leprechaun")
      (let [lep (get-resource state 0)]
        (card-ability state :challenger lep 0)
        (prompt-select :challenger (find-card "Hyperdriver" (:hand (get-challenger))))
        (is (= 2 (:click (get-challenger))))
        (is (= 2 (:credit (get-challenger))))
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not deducted from available MU")
        (card-ability state :challenger lep 0)
        (prompt-select :challenger (find-card "Imp" (:hand (get-challenger))))
        (is (= 1 (:click (get-challenger))))
        (is (zero? (:credit (get-challenger))))
        (is (= 3 (core/available-mu state)) "Imp 1 MU not deducted from available MU")
        ;; Discard Hyperdriver
        (core/move state :challenger (find-card "Hyperdriver" (:hosted (refresh lep))) :discard)
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not added to available MU")
        (core/move state :challenger (find-card "Imp" (:hosted (refresh lep))) :discard) ; discard Imp
        (is (= 3 (core/available-mu state)) "Imp 1 MU not added to available MU")))))

(deftest magnum-opus
  ;; Magnum Opus - Gain 2 cr
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Magnum Opus"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Magnum Opus")
    (is (= 2 (core/available-mu state)))
    (is (zero? (:credit (get-challenger))))
    (let [mopus (get-resource state 0)]
      (card-ability state :challenger mopus 0)
      (is (= 2 (:credit (get-challenger))) "Gain 2cr"))))

(deftest nyashia
  ;; Nyashia
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 10)])
              (default-challenger ["Nyashia"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Nyashia")
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :challenger "Yes")
    (is (= 2 (+ (get-in @state [:challenger :rd-access]) (:access-bonus (:run @state) 0))))))

(deftest origami
  ;; Origami - Increases Challenger max hand size
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Origami" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Origami")
    (is (= 6 (core/hand-size state :challenger)))
    (play-from-hand state :challenger "Origami")
    (is (= 9 (core/hand-size state :challenger)) "Max hand size increased by 2 for each copy placed")))

(deftest paintbrush
  ;; Paintbrush - Give revealed Character a chosen subtype until the end of the next run
  (do-game
    (new-game (default-contestant ["Character Wall"])
              (default-challenger ["Paintbrush"]))
    (play-from-hand state :contestant "Character Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Paintbrush")
    (is (= 2 (core/available-mu state)))
    (let [iwall (get-character state :hq 0)
          pb (get-resource state 0)]
      (card-ability state :challenger pb 0)
      (prompt-select :challenger iwall)
      (is (= 3 (:click (get-challenger))) "Character Wall not revealed, so no click charged")
      (prompt-choice :challenger "Done") ; cancel out
      (core/reveal state :contestant iwall)
      (card-ability state :challenger pb 0)
      (prompt-select :challenger iwall)
      (prompt-choice :challenger "Code Gate")
      (is (= 2 (:click (get-challenger))) "Click charged")
      (is (= true (has? (refresh iwall) :subtype "Code Gate")) "Character Wall gained Code Gate")
      (run-empty-locale state "Archives")
      (is (= false (has? (refresh iwall) :subtype "Code Gate")) "Character Wall lost Code Gate at the end of the run"))))

(deftest parasite
  (testing "Basic functionality: Gain 1 counter every Challenger turn"
    (do-game
      (new-game (default-contestant [(qty "Wraparound" 3) (qty "Hedge Fund" 3)])
                (default-challenger [(qty "Parasite" 3) (qty "Sure Gamble" 3)]))
      (play-from-hand state :contestant "Wraparound" "HQ")
      (let [wrap (get-character state :hq 0)]
        (core/reveal state :contestant wrap)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger wrap)
        (is (= 3 (core/available-mu state)) "Parasite consumes 1 MU")
        (let [psite (first (:hosted (refresh wrap)))]
          (is (zero? (get-counters psite :virus)) "Parasite has no counters yet")
          (take-credits state :challenger)
          (take-credits state :contestant)
          (is (= 1 (get-counters (refresh psite) :virus))
              "Parasite gained 1 virus counter at start of Challenger turn")
          (is (= 6 (:current-strength (refresh wrap))) "Wraparound reduced to 6 strength")))))
  (testing "Placed facedown w/ Apex"
    (do-game
      (new-game (default-contestant)
                (make-deck "Apex: Invasive Predator" ["Parasite"]))
      (take-credits state :contestant)
      (core/end-phase-12 state :challenger nil)
      (prompt-select :challenger (find-card "Parasite" (:hand (get-challenger))))
      (is (empty? (:prompt (get-challenger))) "No prompt to host Parasite")
      (is (= 1 (count (get-challenger-facedown state))) "Parasite placed face down")))
  (testing "Placed on undiscardable Architect should keep gaining counters past 3 and make strength go negative"
    (do-game
      (new-game (default-contestant [(qty "Architect" 3) (qty "Hedge Fund" 3)])
                (default-challenger [(qty "Parasite" 3) "Grimoire"]))
      (play-from-hand state :contestant "Architect" "HQ")
      (let [arch (get-character state :hq 0)]
        (core/reveal state :contestant arch)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Grimoire")
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger arch)
        (let [psite (first (:hosted (refresh arch)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :challenger)
          (take-credits state :contestant)
          (take-credits state :challenger)
          (take-credits state :contestant)
          (take-credits state :challenger)
          (take-credits state :contestant)
          (is (= 4 (get-counters (refresh psite) :virus)) "Parasite has 4 counters")
          (is (= -1 (:current-strength (refresh arch))) "Architect at -1 strength")))))
  (testing "Should stay on hosted card moved by Builder"
    (do-game
      (new-game (default-contestant [(qty "Builder" 3) "Character Wall"])
                (default-challenger [(qty "Parasite" 3)]))
      (play-from-hand state :contestant "Character Wall" "HQ")
      (play-from-hand state :contestant "Builder" "Archives")
      (let [builder (get-character state :archives 0)]
        (core/reveal state :contestant builder)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger builder)
        (let [psite (first (:hosted (refresh builder)))]
          (take-credits state :challenger)
          (take-credits state :contestant)
          (is (= 3 (:current-strength (refresh builder))) "Builder reduced to 3 strength")
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :challenger))
        (let [orig-builder (refresh builder)]
          (card-ability state :contestant builder 0)
          (prompt-choice :contestant "HQ")
          (let [moved-builder (get-character state :hq 1)]
            (is (= (:current-strength orig-builder) (:current-strength moved-builder)) "Builder's state is maintained")
            (let [orig-psite (dissoc (first (:hosted orig-builder)) :host)
                  moved-psite (dissoc (first (:hosted moved-builder)) :host)]
              (is (= orig-psite moved-psite) "Hosted Parasite is maintained"))
            (take-credits state :contestant)
            (let [updated-builder (refresh moved-builder)
                  updated-psite (first (:hosted updated-builder))]
              (is (= 2 (:current-strength updated-builder)) "Builder strength still reduced")
              (is (= 2 (get-counters (refresh updated-psite) :virus)) "Parasite counters still incremented")))))))
  (testing "Use Hivemind counters when placed; instantly discard Character if counters >= Character strength"
    (do-game
      (new-game (default-contestant [(qty "Enigma" 3) (qty "Hedge Fund" 3)])
                (default-challenger ["Parasite"
                                 "Grimoire"
                                 "Hivemind"
                                 "Sure Gamble"]))
      (play-from-hand state :contestant "Enigma" "HQ")
      (let [enig (get-character state :hq 0)]
        (core/reveal state :contestant enig)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Sure Gamble")
        (play-from-hand state :challenger "Grimoire")
        (play-from-hand state :challenger "Hivemind")
        (let [hive (get-resource state 0)]
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind has 2 counters")
          (play-from-hand state :challenger "Parasite")
          (prompt-select :challenger enig)
          (is (= 1 (count (:discard (get-contestant)))) "Enigma discarded instantly")
          (is (= 4 (core/available-mu state)))
          (is (= 2 (count (:discard (get-challenger)))) "Parasite discarded when Enigma was discarded")))))
  (testing "Discarded along with host Character when its strength has been reduced to 0"
    (do-game
      (new-game (default-contestant [(qty "Enigma" 3) (qty "Hedge Fund" 3)])
                (default-challenger [(qty "Parasite" 3) "Grimoire"]))
      (play-from-hand state :contestant "Enigma" "HQ")
      (let [enig (get-character state :hq 0)]
        (core/reveal state :contestant enig)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Grimoire")
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger enig)
        (let [psite (first (:hosted (refresh enig)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (is (= 1 (:current-strength (refresh enig))) "Enigma reduced to 1 strength")
          (take-credits state :challenger)
          (take-credits state :contestant)
          (is (= 1 (count (:discard (get-contestant)))) "Enigma discarded")
          (is (= 1 (count (:discard (get-challenger)))) "Parasite discarded when Enigma was discarded"))))))

(deftest pheromones
  ;; Pheromones ability shouldn't have a NullPointerException when fired with 0 virus counter
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Pheromones"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Pheromones")
    (let [ph (get-resource state 0)]
      (card-ability state :challenger (refresh ph) 0)
      (run-on state "HQ")
      (run-successful state)
      (prompt-choice :challenger "No action")
      (is (= 1 (get-counters (refresh ph) :virus)) "Pheromones gained 1 counter")
      (card-ability state :challenger (refresh ph) 0)))) ; this doesn't do anything, but shouldn't crash

(deftest plague
  ;; Plague
  (do-game
    (new-game (default-contestant ["Mark Yale"])
              (default-challenger ["Plague"]))
    (play-from-hand state :contestant "Mark Yale" "New party")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Plague")
    (prompt-choice :challenger "Locale 1")
    (let [plague (get-resource state 0)]
      (run-empty-locale state "Locale 1")
      (is (= 2 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-locale state "Locale 1")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-locale state "Archives")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague did not gain counters"))))

(deftest progenitor
  ;; Progenitor
  (testing "Hosting Hivemind, using Virus Breeding Ground. Issue #738"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Progenitor" "Virus Breeding Ground" "Hivemind"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Progenitor")
      (play-from-hand state :challenger "Virus Breeding Ground")
      (is (= 4 (core/available-mu state)))
      (let [prog (get-resource state 0)
            vbg (get-radicle state 0)]
        (card-ability state :challenger prog 0)
        (prompt-select :challenger (find-card "Hivemind" (:hand (get-challenger))))
        (is (= 4 (core/available-mu state)) "No memory used to host on Progenitor")
        (let [hive (first (:hosted (refresh prog)))]
          (is (= "Hivemind" (:title hive)) "Hivemind is hosted on Progenitor")
          (is (= 1 (get-counters hive :virus)) "Hivemind has 1 counter")
          (is (zero? (:credit (get-challenger))) "Full cost to host on Progenitor")
          (take-credits state :challenger 1)
          (take-credits state :contestant)
          (card-ability state :challenger vbg 0) ; use VBG to transfer 1 token to Hivemind
          (prompt-select :challenger hive)
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
          (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))
  (testing "Keep MU the same when hosting or discarding hosted resources"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Progenitor" "Hivemind"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Progenitor")
      (let [pro (get-resource state 0)]
        (card-ability state :challenger pro 0)
        (prompt-select :challenger (find-card "Hivemind" (:hand (get-challenger))))
        (is (= 2 (:click (get-challenger))))
        (is (= 2 (:credit (get-challenger))))
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not deducted from available MU")
        ;; Discard Hivemind
        (core/move state :challenger (find-card "Hivemind" (:hosted (refresh pro))) :discard)
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not added to available MU")))))

(deftest reaver
  ;; Reaver - Draw a card the first time you discard an placed card each turn
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["PAD Campaign"])
                (default-challenger ["Reaver" (qty "Fall Guy" 5)]))
      (starting-hand state :challenger ["Reaver" "Fall Guy"])
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (core/gain state :challenger :click 1)
      (play-from-hand state :challenger "Reaver")
      (is (= 1 (count (:hand (get-challenger)))) "One card in hand")
      (run-empty-locale state "Locale 1")
      (prompt-choice-partial :challenger "Pay") ; Discard PAD campaign
      (is (= 2 (count (:hand (get-challenger)))) "Drew a card from discard of contestant card")
      (play-from-hand state :challenger "Fall Guy")
      (play-from-hand state :challenger "Fall Guy")
      (is (zero? (count (:hand (get-challenger)))) "No cards in hand")
      ; No draw from Fall Guy discard as Reaver already fired this turn
      (card-ability state :challenger (get-radicle state 0) 1)
      (is (zero? (count (:hand (get-challenger)))) "No cards in hand")
      (take-credits state :challenger)
      ; Draw from Fall Guy discard on contestant turn
      (card-ability state :challenger (get-radicle state 0) 1)
      (is (= 1 (count (:hand (get-challenger)))) "One card in hand")))
  (testing "with Freelance Coding Construct - should not draw when discard from hand #2671"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Reaver" 9) "Imp" "Snitch" "Freelance Coding Contract"]))
      (starting-hand state :challenger ["Reaver" "Imp" "Snitch" "Freelance Coding Contract"])
      (take-credits state :contestant)
      (play-from-hand state :challenger "Reaver")
      (is (= 3 (count (:hand (get-challenger)))) "Four cards in hand")
      (is (= 3 (:credit (get-challenger))) "3 credits")
      (play-from-hand state :challenger "Freelance Coding Contract")
      (prompt-select :challenger (find-card "Snitch" (:hand (get-challenger))))
      (prompt-select :challenger (find-card "Imp" (:hand (get-challenger))))
      (prompt-choice :challenger "Done")
      (is (= 7 (:credit (get-challenger))) "7 credits - FCC fired")
      (is (zero? (count (:hand (get-challenger)))) "No cards in hand"))))

(deftest rng-key
  ;; RNG Key - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost
  (testing "Basic behaviour - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost"
    (do-game
      (new-game (default-contestant [(qty "Enigma" 5) "Hedge Fund"])
                (default-challenger ["RNG Key" (qty "Paperclip" 2)]))
      (starting-hand state :contestant ["Hedge Fund"])
      (starting-hand state :challenger ["RNG Key"])
      (take-credits state :contestant)
      (testing "Gain 3 credits"
        (play-from-hand state :challenger "RNG Key")
        (is (= 5 (:credit (get-challenger))) "Starts at 5 credits")
        (run-on state "HQ")
        (run-successful state)
        (prompt-choice :challenger "Yes")
        (prompt-choice :challenger 5)
        (prompt-choice :challenger "Gain 3 [Credits]")
        (is (= 8 (:credit (get-challenger))) "Gained 3 credits")
        (prompt-choice :challenger "No action"))
      (testing "Do not trigger on second successful run"
        (run-on state "R&D")
        (run-successful state)
        (prompt-choice :challenger "No action")
        (take-credits state :challenger)
        (take-credits state :contestant))
      (testing "Do not trigger on archives"
        (run-on state "Archives")
        (run-successful state))
      (testing "Do not get choice if trigger declined"
        (run-on state "R&D")
        (run-successful state)
        (prompt-choice :challenger "No")
        (prompt-choice :challenger "No action"))
      (run-on state "HQ")
      (run-successful state)
      (prompt-choice :challenger "No action")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (testing "Do not gain credits / cards if guess incorrect"
        (run-on state "R&D")
        (run-successful state)
        (prompt-choice :challenger "Yes")
        (prompt-choice :challenger 2)
        (prompt-choice :challenger "No action"))
      (take-credits state :challenger)
      (take-credits state :contestant)
      (testing "Gain 2 cards"
        (is (zero? (count (:hand (get-challenger)))) "Started with 0 cards")
        (run-on state "R&D")
        (run-successful state)
        (prompt-choice :challenger "Yes")
        (prompt-choice :challenger 3)
        (prompt-choice :challenger "Draw 2 cards")
        (prompt-choice :challenger "No action")
        (is (= 2 (count (:hand (get-challenger)))) "Gained 2 cards")
        (is (zero? (count (:deck (get-challenger)))) "Cards came from stack"))))
  (testing "Do not pay out if accessing an region first -- regression test for #3150"
    (do-game
      (new-game (default-contestant ["Hokusai Grid" "Hedge Fund"])
                (default-challenger ["RNG Key"]))
      (play-from-hand state :contestant "Hokusai Grid" "HQ")
      (take-credits state :contestant)
      (testing "Gain 3 credits"
        (play-from-hand state :challenger "RNG Key")
        (is (= 5 (:credit (get-challenger))) "Starts at 5 credits")
        (run-on state "HQ")
        (run-successful state)
        (prompt-choice :challenger "Yes")
        (prompt-choice :challenger 2)
        (prompt-choice :challenger "Unrevealed region in HQ")
        (is (= "You accessed Hokusai Grid." (-> (get-challenger) :prompt first :msg))
            "No RNG Key prompt, straight to access prompt")
        (is (= 5 (:credit (get-challenger))) "Gained no credits")))))

(deftest scheherazade
  ;; Scheherazade - Gain 1 credit when it hosts a resource
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Scheherazade" "Cache"
                               "Inti" "Fall Guy"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Scheherazade")
    (let [sch (get-resource state 0)]
      (card-ability state :challenger sch 0)
      (prompt-select :challenger (find-card "Inti" (:hand (get-challenger))))
      (is (= 1 (count (:hosted (refresh sch)))))
      (is (= 2 (:click (get-challenger))) "Spent 1 click to place and host")
      (is (= 6 (:credit (get-challenger))) "Gained 1 credit")
      (is (= 3 (core/available-mu state)) "Resources hosted on Scheh consume MU")
      (card-ability state :challenger sch 0)
      (prompt-select :challenger (find-card "Cache" (:hand (get-challenger))))
      (is (= 2 (count (:hosted (refresh sch)))))
      (is (= 6 (:credit (get-challenger))) "Gained 1 credit")
      (card-ability state :challenger sch 0)
      (prompt-select :challenger (find-card "Fall Guy" (:hand (get-challenger))))
      (is (= 2 (count (:hosted (refresh sch)))) "Can't host non-resource")
      (is (= 1 (count (:hand (get-challenger))))))))

(deftest self-modifying-code
  ;; Discard & pay 2 to search deck for a resource and place it. Shuffle.
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Self-modifying Code" 3) "Reaver"]))
    (starting-hand state :challenger ["Self-modifying Code" "Self-modifying Code"])
    (core/gain state :challenger :credit 5)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Self-modifying Code")
    (play-from-hand state :challenger "Self-modifying Code")
    (let [smc1 (get-resource state 0)
          smc2 (get-resource state 1)]
      (card-ability state :challenger smc1 0)
      (prompt-card :challenger (find-card "Reaver" (:deck (get-challenger))))
      (is (= 6 (:credit (get-challenger))) "Paid 2 for SMC, 2 for place - 6 credits left")
      (is (= 1 (core/available-mu state)) "SMC MU refunded")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (card-ability state :challenger smc2 0)
      (is (= 1 (count (:hand (get-challenger)))) "1 card drawn due to Reaver before SMC resource selection")
      (is (zero? (count (:deck (get-challenger)))) "Deck empty"))))

(deftest sneakdoor-beta
  (testing "Gabriel Santiago, Ash on HQ should prevent Sneakdoor HQ access but still give Gabe credits. Issue #1138."
    (do-game
      (new-game (default-contestant ["Ash 2X3ZB9CY"])
                (make-deck "Gabriel Santiago: Consummate Professional" ["Sneakdoor Beta"]))
      (play-from-hand state :contestant "Ash 2X3ZB9CY" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sneakdoor Beta")
      (is (= 1 (:credit (get-challenger))) "Sneakdoor cost 4 credits")
      (let [sb (get-resource state 0)
            ash (get-content state :hq 0)]
        (core/reveal state :contestant ash)
        (card-ability state :challenger sb 0)
        (run-successful state)
        (prompt-choice :contestant 0)
        (prompt-choice :challenger 0)
        (is (= 3 (:credit (get-challenger))) "Gained 2 credits from Gabe's ability")
        (is (= (:cid ash) (-> (get-challenger) :prompt first :card :cid)) "Ash interrupted HQ access after Sneakdoor run")
        (is (= :hq (-> (get-challenger) :register :successful-run first)) "Successful Run on HQ recorded"))))
  (testing "do not switch to HQ if Archives has Crisium Grid. Issue #1229."
    (do-game
      (new-game (default-contestant ["Crisium Grid" "Priority Requisition" "Private Security Force"])
                (default-challenger ["Sneakdoor Beta"]))
      (play-from-hand state :contestant "Crisium Grid" "Archives")
      (discard-from-hand state :contestant "Priority Requisition")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sneakdoor Beta")
      (let [sb (get-resource state 0)
            cr (get-content state :archives 0)]
        (core/reveal state :contestant cr)
        (card-ability state :challenger sb 0)
        (run-successful state)
        (is (= :archives (get-in @state [:run :locale 0])) "Crisium Grid stopped Sneakdoor Beta from switching to HQ"))))
  (testing "Allow Nerve Agent to gain counters. Issue #1158/#955"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Sneakdoor Beta" "Nerve Agent"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Nerve Agent")
      (play-from-hand state :challenger "Sneakdoor Beta")
      (let [nerve (get-resource state 0)
            sb (get-resource state 1)]
        (card-ability state :challenger sb 0)
        (run-successful state)
        (is (= 1 (get-counters (refresh nerve) :virus)))
        (card-ability state :challenger sb 0)
        (run-successful state)
        (is (= 2 (get-counters (refresh nerve) :virus))))))
  (testing "Grant Security Testing credits on HQ."
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Security Testing" "Sneakdoor Beta"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sneakdoor Beta")
      (play-from-hand state :challenger "Security Testing")
      (take-credits state :challenger)
      (is (= 3 (:credit (get-challenger))))
      (take-credits state :contestant)
      (let [sb (get-resource state 0)]
        (prompt-choice :challenger "HQ")
        (card-ability state :challenger sb 0)
        (run-successful state)
        (is (not (:run @state)) "Switched to HQ and ended the run from Security Testing")
        (is (= 5 (:credit (get-challenger))) "Sneakdoor switched to HQ and earned Security Testing credits")))))

(deftest snitch
  ;; Snitch - Only works on unrevealed character
  (do-game
    (new-game (default-contestant [(qty "Quandary" 2)])
              (default-challenger ["Snitch"]))
    (play-from-hand state :contestant "Quandary" "R&D")
    (play-from-hand state :contestant "Quandary" "HQ")
    (let [hqcharacter (get-character state :hq 0)]
      (core/reveal state :contestant hqcharacter))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Snitch")
    (let [snitch (get-resource state 0)]
      ;; unrevealed character scenario
      (run-on state "R&D")
      (card-ability state :challenger snitch 0)
      (is (prompt-is-card? :challenger snitch) "Option to jack out")
      (prompt-choice :challenger "Yes")
      ;; revealed character scenario
      (run-on state "HQ")
      (card-ability state :challenger snitch 0)
      (is (empty? (get-in @state [:challenger :prompt])) "No option to jack out")
      ;; no character scenario
      (run-on state "Archives")
      (card-ability state :challenger snitch 0)
      (is (empty? (get-in @state [:challenger :prompt])) "No option to jack out"))))

(deftest surfer
  ;; Surfer - Swap position with character before or after when encountering a Barrier Character
  (do-game
   (new-game (default-contestant ["Character Wall" "Quandary"])
             (default-challenger ["Surfer"]))
   (play-from-hand state :contestant "Quandary" "HQ")
   (play-from-hand state :contestant "Character Wall" "HQ")
   (take-credits state :contestant)
   (play-from-hand state :challenger "Surfer")
   (is (= 3 (:credit (get-challenger))) "Paid 2 credits to place Surfer")
   (core/reveal state :contestant (get-character state :hq 1))
   (run-on state "HQ")
   (is (= 2 (get-in @state [:run :position])) "Starting run at position 2")
   (let [surf (get-resource state 0)]
     (card-ability state :challenger surf 0)
     (prompt-select :challenger (get-character state :hq 0))
     (is (= 1 (:credit (get-challenger))) "Paid 2 credits to use Surfer")
     (is (= 1 (get-in @state [:run :position])) "Now at next position (1)")
     (is (= "Character Wall" (:title (get-character state :hq 0))) "Character Wall now at position 1"))))

(deftest takobi
  ;; Takobi - 2 power counter to add +3 strength to a non-AI characterbreaker for encounter
  (do-game
    (new-game (default-contestant ["Enigma"])
              (default-challenger ["Takobi" "Corroder" "Faust"]))
    (play-from-hand state :contestant "Enigma" "HQ")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Takobi")
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "Faust")
    (let [tako (get-resource state 0)
          corr (get-resource state 1)
          faus (get-resource state 2)]
      (dotimes [_ 3]
        (card-ability state :challenger tako 0))
      (is (= 3 (get-counters (refresh tako) :power)) "3 counters on Takobi")
      (run-on state "HQ")
      (card-ability state :challenger tako 1)
      (is (empty? (:prompt (get-challenger))) "No prompt for un-revealed character")
      (core/reveal state :contestant (get-character state :hq 0))
      (card-ability state :challenger tako 1)
      (prompt-select :challenger (refresh faus))
      (is (not-empty (:prompt (get-challenger))) "Can't select AI breakers")
      (prompt-select :challenger (refresh corr))
      (is (empty? (:prompt (get-challenger))) "Can select non-AI breakers")
      (is (= 5 (:current-strength (refresh corr))) "Corroder at +3 strength")
      (is (= 1 (get-counters (refresh tako) :power)) "1 counter on Takobi")
      (card-ability state :challenger tako 1)
      (is (empty? (:prompt (get-challenger))) "No prompt when too few power counters")
      (core/no-action state :contestant nil)
      (run-continue state)
      (is (= 2 (:current-strength (refresh corr))) "Corroder returned to normal strength"))))

(deftest trypano
  (testing "Hivemind and Architect interactions"
    (do-game
      (new-game (default-contestant [(qty "Architect" 2)])
                (default-challenger [(qty "Trypano" 2) "Hivemind"]))
      (play-from-hand state :contestant "Architect" "HQ")
      (play-from-hand state :contestant "Architect" "R&D")
      (let [architect-revealed (get-character state :hq 0)
            architect-unrevealed (get-character state :rd 0)]
        (core/reveal state :contestant architect-revealed)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Trypano")
        (prompt-select :challenger (game.core/get-card state architect-revealed))
        (play-from-hand state :challenger "Trypano")
        (prompt-select :challenger architect-unrevealed)
        (is (= 2 (core/available-mu state)) "Trypano consumes 1 MU"))
      ;; wait 4 turns to make both Trypanos have 4 counters on them
      (dotimes [n 4]
        (take-credits state :challenger)
        (take-credits state :contestant)
        (prompt-choice :challenger "Yes")
        (prompt-choice :challenger "Yes"))
      (is (zero? (count (:discard (get-challenger)))) "Trypano not in discard yet")
      (is (= 1 (count (get-in @state [:contestant :locales :rd :characters]))) "Unrevealed Archiect is not discarded")
      (is (= 1 (count (get-in @state [:contestant :locales :hq :characters]))) "Revealed Archiect is not discarded")
      (play-from-hand state :challenger "Hivemind") ; now Hivemind makes both Trypanos have 5 counters
      (is (zero? (count (get-in @state [:contestant :locales :rd :characters]))) "Unrevealed Archiect was discarded")
      (is (= 1 (count (get-in @state [:contestant :locales :hq :characters]))) "Revealed Archiect was not discarded")
      (is (= 1 (count (:discard (get-challenger)))) "Trypano went to discard")))
  (testing "Fire when Hivemind gains counters"
    (do-game
      (new-game (default-contestant ["Architect"])
                (default-challenger ["Trypano" "Hivemind" (qty "Surge" 2)]))
      (play-from-hand state :contestant "Architect" "R&D")
      (let [architect-unrevealed (get-character state :rd 0)]
        (take-credits state :contestant)
        (play-from-hand state :challenger "Trypano")
        (prompt-select :challenger architect-unrevealed)
        (is (zero? (count (:discard (get-challenger)))) "Trypano not in discard yet")
        (is (= 1 (count (get-character state :rd))) "Unrevealed Architect is not discarded")
        (play-from-hand state :challenger "Hivemind")
        (let [hive (get-resource state 0)]
          (is (= 1 (get-counters (refresh hive) :virus)) "Hivemind starts with 1 virus counter")
          (play-from-hand state :challenger "Surge")
          (prompt-select :challenger (refresh hive))
          (is (= 3 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters")
          (play-from-hand state :challenger "Surge")
          (prompt-select :challenger (refresh hive))
          (is (= 5 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters (now at 5)")
          (is (zero? (count (get-character state :rd))) "Unrevealed Architect was discarded")
          (is (= 3 (count (:discard (get-challenger)))) "Trypano went to discard"))))))

(deftest upya
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Upya"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Upya")
    (dotimes [_ 3]
      (run-empty-locale state "R&D"))
    (is (= 3 (get-counters (get-resource state 0) :power)) "3 counters on Upya")
    (take-credits state :contestant)
    (dotimes [_ 3]
      (run-empty-locale state "R&D"))
    (is (= 6 (get-counters (get-resource state 0) :power)) "6 counters on Upya")
    (let [upya (get-resource state 0)]
      (card-ability state :challenger upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 2 (:click (get-challenger))) "Gained 2 clicks")
      (card-ability state :challenger upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "Upya not used more than once a turn")
      (is (= 2 (:click (get-challenger))) "Still at 2 clicks"))
    (take-credits state :challenger)
    (take-credits state :contestant)
    (let [upya (get-resource state 0)]
      (card-ability state :challenger upya 0)
      (is (zero? (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 5 (:click (get-challenger))) "Gained 2 clicks"))))

(deftest wari
  (do-game
    (new-game (default-contestant ["Character Wall"])
              (default-challenger ["Wari"]))
    (play-from-hand state :contestant "Character Wall" "R&D")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Wari")
    (run-empty-locale state "HQ")
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger "Barrier")
    (prompt-select :challenger (get-character state :rd 0))
    (is (= 1 (count (:discard (get-challenger)))) "Wari in heap")
    (is (not (empty? (get-in @state [:challenger :prompt]))) "Challenger is currently accessing Character Wall")))
