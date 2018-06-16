(ns test.cards.resources
  (:require [game.core :as core]
            [game.utils :refer :all]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest au-revoir
  ;; Au Revoir - Gain 1 credit every time you jack out
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Au Revoir" 2)]))
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

(deftest crescentus
  ;; Crescentus should only work on rezzed character
  (do-game
    (new-game (default-contestant [(qty "Quandary" 1)])
              (default-challenger [(qty "Crescentus" 1)]))
    (play-from-hand state :contestant "Quandary" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Crescentus")
    (run-on state "HQ")
    (let [cres (get-in @state [:challenger :rig :resource 0])
          q (get-character state :hq 0)]
      (card-ability state :challenger cres 0)
      (is (not (nil? (get-in @state [:challenger :rig :resource 0]))) "Crescentus could not be used because the Character is not rezzed")
      (core/rez state :contestant q)
      (is (get-in (refresh q) [:rezzed]) "Quandary is now rezzed")
      (card-ability state :challenger cres 0)
      (is (nil? (get-in @state [:challenger :rig :resource 0])) "Crescentus could be used because the Character is rezzed")
      (is (not (get-in (refresh q) [:rezzed])) "Quandary is no longer rezzed"))))

(deftest datasucker
  ;; Datasucker - Reduce strength of encountered Character
  (do-game
    (new-game (default-contestant [(qty "Fire Wall" 1)])
              (default-challenger [(qty "Datasucker" 1)]))
    (play-from-hand state :contestant "Fire Wall" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :click 3)
    (play-from-hand state :challenger "Datasucker")
    (let [ds (get-in @state [:challenger :rig :resource 0])
          fw (get-character state :remote1 0)]
      (run-empty-server state "Archives")
      (is (= 1 (get-counters (refresh ds) :virus)))
      (run-empty-server state "Archives")
      (is (= 2 (get-counters (refresh ds) :virus)))
      (run-on state "Server 1")
      (run-continue state)
      (run-successful state)
      (is (= 2 (get-counters (refresh ds) :virus)) "No counter gained, not a central server")
      (run-on state "Server 1")
      (core/rez state :contestant fw)
      (is (= 5 (:current-strength (refresh fw))))
      (card-ability state :challenger ds 0)
      (is (= 1 (get-counters (refresh ds) :virus)) "1 counter spent from Datasucker")
      (is (= 4 (:current-strength (refresh fw))) "Fire Wall strength lowered by 1"))))

(deftest datasucker-trashed
  ;; Datasucker - does not affect next character when current is trashed. Issue #1788.
  (do-game
    (new-game
      (default-contestant [(qty "Wraparound" 1) (qty "Spiderweb" 1)])
      (default-contestant [(qty "Datasucker" 1) (qty "Parasite" 1)]))
    (play-from-hand state :contestant "Spiderweb" "HQ")
    (play-from-hand state :contestant "Wraparound" "HQ")
    (take-credits state :contestant)
    (core/gain state :contestant :credit 10)
    (play-from-hand state :challenger "Datasucker")
    (let [sucker (get-resource state 0)
          spider (get-character state :hq 0)
          wrap (get-character state :hq 1)]
      (core/add-counter state :challenger sucker :virus 2)
      (core/rez state :contestant spider)
      (core/rez state :contestant wrap)
      (play-from-hand state :challenger "Parasite")
      (prompt-select :challenger (refresh spider))
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :challenger (refresh sucker) 0)
      (card-ability state :challenger (refresh sucker) 0)
      (is (find-card "Spiderweb" (:discard (get-contestant))) "Spiderweb trashed by Parasite + Datasucker")
      (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Datasucker"))))

(deftest diwan
  ;; Diwan - Full test
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3) (qty "Fire Wall" 3) (qty "Crisium Grid" 2)])
              (default-challenger [(qty "Diwan" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Diwan")
    (prompt-choice :challenger "HQ")
    (take-credits state :challenger)
    (is (= 8 (:credit (get-contestant))) "8 credits for contestant at start of second turn")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (is (= 8 (:credit (get-contestant))) "Diwan did not charge extra for install on another server")
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (is (= 7 (:credit (get-contestant))) "Diwan charged 1cr to install character protecting the named server")
    (play-from-hand state :contestant "Crisium Grid" "HQ")
    (is (= 7 (:credit (get-contestant))) "Diwan didn't charge to install another region in root of HQ")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (is (= 5 (:credit (get-contestant))) "Diwan charged 1cr + 1cr to install a second character protecting the named server")
    (core/gain state :contestant :click 1)
    (core/purge state :contestant)
    (play-from-hand state :contestant "Fire Wall" "HQ") ; 2cr cost from normal install cost
    (is (= "Diwan" (-> (get-challenger) :discard first :title)) "Diwan was trashed from purge")
    (is (= 3 (:credit (get-contestant))) "No charge for installs after Diwan purged")))

(deftest djinn-host-chakana
  ;; Djinn - Hosted Chakana does not disable advancing agendas. Issue #750
  (do-game
    (new-game (default-contestant [(qty "Priority Requisition" 1)])
              (default-challenger [(qty "Djinn" 1) (qty "Chakana" 1)]))
    (play-from-hand state :contestant "Priority Requisition" "New remote")
    (take-credits state :contestant 2)
    (play-from-hand state :challenger "Djinn")
    (let [djinn (get-in @state [:challenger :rig :resource 0])
          agenda (get-content state :remote1 0)]
      (is agenda "Agenda was installed")
      (card-ability state :challenger djinn 1)
      (prompt-select :challenger (find-card "Chakana" (:hand (get-challenger))))
      (let [chak (first (:hosted (refresh djinn)))]
        (is (= "Chakana" (:title chak)) "Djinn has a hosted Chakana")
        ;; manually add 3 counters
        (core/add-counter state :challenger (first (:hosted (refresh djinn))) :virus 3)
        (take-credits state :challenger 2)
        (core/advance state :contestant {:card agenda})
        (is (= 1 (:advance-counter (refresh agenda))) "Agenda was advanced")))))

(deftest djinn-host-resource
  ;; Djinn - Host a non-icebreaker resource
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Djinn" 1) (qty "Chakana" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Djinn")
    (is (= 3 (:memory (get-challenger))))
    (let [djinn (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger djinn 1)
      (prompt-select :challenger (find-card "Chakana" (:hand (get-challenger))))
      (is (= 3 (:memory (get-challenger))) "No memory used to host on Djinn")
      (is (= "Chakana" (:title (first (:hosted (refresh djinn))))) "Djinn has a hosted Chakana")
      (is (= 1 (:credit (get-challenger))) "Full cost to host on Djinn"))))

(deftest djinn-tutor-virus
  ;; Djinn - Tutor a virus resource
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Djinn" 1) (qty "Parasite" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Djinn")
    (core/move state :challenger (find-card "Parasite" (:hand (get-challenger))) :deck)
    (is (zero? (count (:hand (get-challenger)))) "No cards in hand after moving Parasite to deck")
    (let [djinn (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger djinn 0)
      (prompt-card :challenger (find-card "Parasite" (:deck (get-challenger))))
      (is (= "Parasite" (:title (first (:hand (get-challenger))))) "Djinn moved Parasite to hand")
      (is (= 2 (:credit (get-challenger))) "1cr to use Djinn ability")
      (is (= 2 (:click (get-challenger))) "1click to use Djinn ability"))))

(deftest equivocation
  ;; Equivocation - interactions with other successful-run events.
  (do-game
    (new-game
      (default-contestant [(qty "Restructure" 3) (qty "Hedge Fund" 3)])
      (make-deck "Laramy Fisk: Savvy Investor" [(qty "Equivocation" 1) (qty "Desperado" 1)]))
    (starting-hand state :contestant ["Hedge Fund"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Equivocation")
    (play-from-hand state :challenger "Desperado")
    (run-empty-server state :rd)
    (prompt-choice :challenger "Laramy Fisk: Savvy Investor")
    (prompt-choice :challenger "Yes")
    (is (= 2 (count (:hand (get-contestant)))) "Contestant forced to draw by Fisk")
    (prompt-choice :challenger "Yes") ; Equivocation prompt
    (prompt-choice :challenger "Yes") ; force the draw
    (is (= 1 (:credit (get-challenger))) "Challenger gained 1cr from Desperado")
    (is (= 3 (count (:hand (get-contestant)))) "Contestant forced to draw by Equivocation")
    (prompt-choice :challenger "OK")
    (is (not (:run @state)) "Run ended")))

(deftest false-echo
  ;; False Echo - choice for Contestant
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 3)])
              (default-challenger [(qty "False Echo" 3)]))
    (play-from-hand state :contestant "Ice Wall" "Archives")
    (play-from-hand state :contestant "Ice Wall" "Archives")
    (take-credits state :contestant)
    (play-from-hand state :challenger "False Echo")
    (play-from-hand state :challenger "False Echo")
    (run-on state "Archives")
    (run-continue state)
    (let [echo1 (get-resource state 0)
          echo2 (get-resource state 1)]
      (card-ability state :challenger echo1 0)
      (prompt-choice :contestant "Add to HQ")
      (is (= 2 (count (:hand (get-contestant)))) "Ice Wall added to HQ")
      (is (= 1 (count (:discard (get-challenger)))) "False Echo trashed")
      (run-continue state)
      (card-ability state :challenger echo2 0)
      (prompt-choice :contestant "Rez")
      (is (:rezzed (get-character state :archives 0)) "Ice Wall rezzed")
      (is (= 2 (count (:discard (get-challenger)))) "False Echo trashed"))))

(deftest gravedigger
  ;; Gravedigger - Gain counters when Contestant cards are trashed, spend click-counter to mill Contestant
  (do-game
    (new-game (default-contestant [(qty "Launch Campaign" 2) (qty "Enigma" 2)])
              (default-challenger [(qty "Gravedigger" 1)]))
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Gravedigger")
    (let [gd (get-in @state [:challenger :rig :resource 0])]
      (core/trash state :contestant (get-content state :remote1 0))
      (is (= 1 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/trash state :contestant (get-content state :remote2 0))
      (is (= 2 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/move state :contestant (find-card "Enigma" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Enigma" (:hand (get-contestant))) :deck)
      (is (= 2 (count (:deck (get-contestant)))))
      (card-ability state :challenger gd 0)
      (is (= 1 (get-counters (refresh gd) :virus)) "Spent 1 counter from Gravedigger")
      (is (= 2 (:click (get-challenger))) "Spent 1 click")
      (is (= 1 (count (:deck (get-contestant)))))
      (is (= 3 (count (:discard (get-contestant)))) "Milled 1 card from R&D"))))

(deftest harbinger-blacklist
  ;; Harbinger - install facedown when Blacklist installed
  (do-game
    (new-game (default-contestant [(qty "Blacklist" 1)])
              (default-challenger [(qty "Harbinger" 1)]))
    (play-from-hand state :contestant "Blacklist" "New remote")
    (core/rez state :contestant (get-content state :remote1 0) )
    (take-credits state :contestant)
    (play-from-hand state :challenger "Harbinger")
    (core/trash state :challenger (-> (get-challenger) :rig :resource first))
    (is (= 0 (count (:discard (get-challenger)))) "Harbinger not in heap")
    (is (-> (get-challenger) :rig :facedown first :facedown) "Harbinger installed facedown")))

(deftest hyperdriver
  ;; Hyperdriver - Remove from game to gain 3 clicks
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Hyperdriver" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Hyperdriver")
    (is (= 1 (:memory (get-challenger))) "3 MU used")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (is (:challenger-phase-12 @state) "Challenger in Step 1.2")
    (let [hyp (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger hyp 0)
      (core/end-phase-12 state :challenger nil)
      (is (= 7 (:click (get-challenger))) "Gained 3 clicks")
      (is (= 1 (count (:rfg (get-challenger)))) "Hyperdriver removed from game"))))

(deftest hyperdriver-dhegdheer
  ;; triggering a Dhegdeered Hyperdriver should not grant +3 MU
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Hyperdriver" 1)
                               (qty "Dhegdheer" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Dhegdheer")
    (let [dheg (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger dheg 0)
      (prompt-select :challenger (find-card "Hyperdriver" (:hand (get-challenger))))
      (is (= 4 (:memory (get-challenger))) "0 MU used")
      (is (= 2 (:click (get-challenger))) "2 clicks used")
      (is (= 3 (:credit (get-challenger))) "2 credits used")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (:challenger-phase-12 @state) "Challenger in Step 1.2")
      (let [hyp (first (:hosted (refresh dheg)))]        
        (card-ability state :challenger hyp 0)
        (core/end-phase-12 state :challenger nil)
        (is (= 7 (:click (get-challenger))) "Used Hyperdriver")
        (is (= 4 (:memory (get-challenger))) "Still 0 MU used")))))

(deftest imp-the-future-perfect
  ;; Trashing TFP with Imp should not trigger psi-game -- Issue #1844
  (do-game
    (new-game (default-contestant [(qty "The Future Perfect" 1)])
              (default-challenger [(qty "Imp" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Imp")
    (testing "Trash before access click"
      (run-empty-server state "HQ")
      ;; Should access TFP at this point
      (card-ability state :challenger (get-resource state 0) 0)
      (is (empty? (get-in @state [:challenger :prompt])) "Should be no psi-game prompt for TFP")
      (is (= "The Future Perfect" (get-in @state [:contestant :discard 0 :title])) "TFP trashed")
      (is (= 0 (:agenda-point (get-challenger))) "Challenger did not steal TFP")
      (core/move state :contestant (find-card "The Future Perfect" (:discard (get-contestant))) :hand))
    (take-credits state :challenger)
    (take-credits state :contestant)
    (testing "Trashing after lose psi game"
      (run-empty-server state "HQ")
      ;; Access prompt for TFP
      (prompt-choice :challenger "Access")
      (prompt-choice :contestant "0 [Credit]")
      (prompt-choice :challenger "1 [Credit]")
      ;; Fail psi game
      (card-ability state :challenger (get-resource state 0) 0)
      (is (empty? (get-in @state [:challenger :prompt])) "Should be no steal prompt for TFP")
      (is (= "The Future Perfect" (get-in @state [:contestant :discard 0 :title])) "TFP trashed")
      (is (= 0 (:agenda-point (get-challenger))) "Challenger did not steal TFP"))))

(deftest incubator-transfer-virus-counters
  ;; Incubator - Gain 1 virus counter per turn; trash to move them to an installed virus resource
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Incubator" 1) (qty "Datasucker" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Datasucker")
    (play-from-hand state :challenger "Incubator")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (let [ds (get-in @state [:challenger :rig :resource 0])
          incub (get-in @state [:challenger :rig :resource 1])]
      (is (= 1 (get-counters (refresh incub) :virus)) "Incubator gained 1 virus counter")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (is (= 2 (get-counters (refresh incub) :virus)) "Incubator has 2 virus counters")
      (card-ability state :challenger incub 0)
      (prompt-select :challenger ds)
      (is (= 2 (get-counters (refresh ds) :virus)) "Datasucker has 2 virus counters moved from Incubator")
      (is (= 1 (count (get-in @state [:challenger :rig :resource]))))
      (is (= 1 (count (:discard (get-challenger)))) "Incubator trashed")
      (is (= 3 (:click (get-challenger)))))))

(deftest ixodidae
  ;; Ixodidae should not trigger on psi-games
  (do-game
    (new-game (default-contestant [(qty "Snowflake" 1)])
              (default-challenger [(qty "Ixodidae" 1) (qty "Lamprey" 1)]))
    (play-from-hand state :contestant "Snowflake" "HQ")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))) "Contestant at 7 credits")
    (play-from-hand state :challenger "Ixodidae")
    (play-from-hand state :challenger "Lamprey")
    (is (= 3 (:credit (get-challenger))) "Challenger paid 3 credits to install Ixodidae and Lamprey")
    (run-on state :hq)
    (let [s (get-character state :hq 0)]
      (core/rez state :contestant s)
      (card-subroutine state :contestant s 0)
      (is (prompt-is-card? :contestant s) "Contestant prompt is on Snowflake")
      (is (prompt-is-card? :challenger s) "Challenger prompt is on Snowflake")
      (is (= 6 (:credit (get-contestant))) "Contestant paid 1 credit to rezz Snowflake")
      (prompt-choice :contestant "1")
      (prompt-choice :challenger "1")
      (is (= 5 (:credit (get-contestant))) "Contestant paid 1 credit to psi game")
      (is (= 2 (:credit (get-challenger))) "Challenger did not gain 1 credit from Ixodidae when contestant spent on psi game")
      (run-continue state)
      (run-successful state)
      (is (= 4 (:credit (get-contestant))) "Contestant lost 1 credit to Lamprey")
      (is (= 3 (:credit (get-challenger))) "Challenger gains 1 credit from Ixodidae due to Lamprey"))))

(deftest lamprey
  ;; Lamprey - Contestant loses 1 credit for each successful HQ run; trashed on purge
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Lamprey" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Lamprey")
    (let [lamp (get-in @state [:challenger :rig :resource 0])]
      (run-empty-server state :hq)
      (is (= 7 (:credit (get-contestant))) "Contestant lost 1 credit")
      (run-empty-server state :hq)
      (is (= 6 (:credit (get-contestant))) "Contestant lost 1 credit")
      (run-empty-server state :hq)
      (is (= 5 (:credit (get-contestant))) "Contestant lost 1 credit")
      (take-credits state :challenger)
      (core/purge state :contestant)
      (is (empty? (get-in @state [:challenger :rig :resource])) "Lamprey trashed by purge"))))

(deftest leprechaun-mu-savings
  ;; Leprechaun - Keep MU the same when hosting or trashing hosted resources
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Leprechaun" 1) (qty "Hyperdriver" 1) (qty "Imp" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Leprechaun")
    (let [lep (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger lep 0)
      (prompt-select :challenger (find-card "Hyperdriver" (:hand (get-challenger))))
      (is (= 2 (:click (get-challenger))))
      (is (= 2 (:credit (get-challenger))))
      (is (= 3 (:memory (get-challenger))) "Hyperdriver 3 MU not deducted from available MU")
      (card-ability state :challenger lep 0)
      (prompt-select :challenger (find-card "Imp" (:hand (get-challenger))))
      (is (= 1 (:click (get-challenger))))
      (is (= 0 (:credit (get-challenger))))
      (is (= 3 (:memory (get-challenger))) "Imp 1 MU not deducted from available MU")
      ;; Trash Hyperdriver
      (core/move state :challenger (find-card "Hyperdriver" (:hosted (refresh lep))) :discard)
      (is (= 3 (:memory (get-challenger))) "Hyperdriver 3 MU not added to available MU")
      (core/move state :challenger (find-card "Imp" (:hosted (refresh lep))) :discard) ; trash Imp
      (is (= 3 (:memory (get-challenger))) "Imp 1 MU not added to available MU"))))

(deftest magnum-opus-click
  ;; Magnum Opus - Gain 2 cr
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Magnum Opus" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Magnum Opus")
    (is (= 2 (:memory (get-challenger))))
    (is (= 0 (:credit (get-challenger))))
    (let [mopus (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger mopus 0)
      (is (= 2 (:credit (get-challenger))) "Gain 2cr"))))

(deftest origami
  ;; Origami - Increases Challenger max hand size
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Origami" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Origami")
    (is (= 6 (core/hand-size state :challenger)))
    (play-from-hand state :challenger "Origami")
    (is (= 9 (core/hand-size state :challenger)) "Max hand size increased by 2 for each copy installed")))

(deftest paintbrush
  ;; Paintbrush - Give rezzed Character a chosen subtype until the end of the next run
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 1)])
              (default-challenger [(qty "Paintbrush" 1)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Paintbrush")
    (is (= 2 (:memory (get-challenger))))
    (let [iwall (get-character state :hq 0)
          pb (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger pb 0)
      (prompt-select :challenger iwall)
      (is (= 3 (:click (get-challenger))) "Ice Wall not rezzed, so no click charged")
      (prompt-choice :challenger "Done") ; cancel out
      (core/rez state :contestant iwall)
      (card-ability state :challenger pb 0)
      (prompt-select :challenger iwall)
      (prompt-choice :challenger "Code Gate")
      (is (= 2 (:click (get-challenger))) "Click charged")
      (is (= true (has? (refresh iwall) :subtype "Code Gate")) "Ice Wall gained Code Gate")
      (run-empty-server state "Archives")
      (is (= false (has? (refresh iwall) :subtype "Code Gate")) "Ice Wall lost Code Gate at the end of the run"))))

(deftest parasite-apex
  ;; Parasite - Installed facedown w/ Apex
  (do-game
    (new-game (default-contestant)
              (make-deck "Apex: Invasive Predator" [(qty "Parasite" 1)]))
    (take-credits state :contestant)
    (core/end-phase-12 state :challenger nil)
    (prompt-select :challenger (find-card "Parasite" (:hand (get-challenger))))
    (is (empty? (:prompt (get-challenger))) "No prompt to host Parasite")
    (is (= 1 (count (get-in @state [:challenger :rig :facedown]))) "Parasite installed face down")))

(deftest parasite-architect
  ;; Parasite - Installed on untrashable Architect should keep gaining counters past 3 and make strength go negative
  (do-game
    (new-game (default-contestant [(qty "Architect" 3) (qty "Hedge Fund" 3)])
              (default-challenger [(qty "Parasite" 3) (qty "Grimoire" 1)]))
    (play-from-hand state :contestant "Architect" "HQ")
    (let [arch (get-character state :hq 0)]
      (core/rez state :contestant arch)
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

(deftest parasite-builder-moved
  ;; Parasite - Should stay on hosted card moved by Builder
  (do-game
    (new-game (default-contestant [(qty "Builder" 3) (qty "Ice Wall" 1)])
              (default-challenger [(qty "Parasite" 3)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (play-from-hand state :contestant "Builder" "Archives")
    (let [builder (get-character state :archives 0)
          _ (core/rez state :contestant builder)
          _ (take-credits state :contestant)
          _ (play-from-hand state :challenger "Parasite")
          _ (prompt-select :challenger builder)
          psite (first (:hosted (refresh builder)))
          _ (take-credits state :challenger)
          _ (take-credits state :contestant)
          _ (is (= 3 (:current-strength (refresh builder))) "Builder reduced to 3 strength")
          _ (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          _ (take-credits state :challenger)
          orig-builder (refresh builder)
          _ (card-ability state :contestant builder 0)
          _ (prompt-choice :contestant "HQ")
          moved-builder (get-character state :hq 1)
          _ (is (= (:current-strength orig-builder) (:current-strength moved-builder)) "Builder's state is maintained")
          orig-psite (dissoc (first (:hosted orig-builder)) :host)
          moved-psite (dissoc (first (:hosted moved-builder)) :host)
          _ (is (= orig-psite moved-psite) "Hosted Parasite is maintained")
          _ (take-credits state :contestant)
          updated-builder (refresh moved-builder)
          updated-psite (first (:hosted updated-builder))
          _ (is (= 2 (:current-strength updated-builder)) "Builder strength still reduced")
          _ (is (= 2 (get-counters (refresh updated-psite) :virus)) "Parasite counters still incremented")])))

(deftest parasite-gain-counter
  ;; Parasite - Gain 1 counter every Challenger turn
  (do-game
    (new-game (default-contestant [(qty "Wraparound" 3) (qty "Hedge Fund" 3)])
              (default-challenger [(qty "Parasite" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Wraparound" "HQ")
    (let [wrap (get-character state :hq 0)]
      (core/rez state :contestant wrap)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Parasite")
      (prompt-select :challenger wrap)
      (is (= 3 (:memory (get-challenger))) "Parasite consumes 1 MU")
      (let [psite (first (:hosted (refresh wrap)))]
        (is (= 0 (get-counters psite :virus)) "Parasite has no counters yet")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (get-counters (refresh psite) :virus))
            "Parasite gained 1 virus counter at start of Challenger turn")
        (is (= 6 (:current-strength (refresh wrap))) "Wraparound reduced to 6 strength")))))

(deftest parasite-hivemind-instant-character-trash
  ;; Parasite - Use Hivemind counters when installed; instantly trash Character if counters >= Character strength
  (do-game
    (new-game (default-contestant [(qty "Enigma" 3) (qty "Hedge Fund" 3)])
              (default-challenger [(qty "Parasite" 1)
                               (qty "Grimoire" 1)
                               (qty "Hivemind" 1)
                               (qty "Sure Gamble" 1)]))
    (play-from-hand state :contestant "Enigma" "HQ")
    (let [enig (get-character state :hq 0)]
      (core/rez state :contestant enig)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sure Gamble")
      (play-from-hand state :challenger "Grimoire")
      (play-from-hand state :challenger "Hivemind")
      (let [hive (get-in @state [:challenger :rig :resource 0])]
        (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind has 2 counters")
        (play-from-hand state :challenger "Parasite")
        (prompt-select :challenger enig)
        (is (= 1 (count (:discard (get-contestant)))) "Enigma trashed instantly")
        (is (= 4 (:memory (get-challenger))))
        (is (= 2 (count (:discard (get-challenger)))) "Parasite trashed when Enigma was trashed")))))

(deftest parasite-character-trashed
  ;; Parasite - Trashed along with host Character when its strength has been reduced to 0
  (do-game
    (new-game (default-contestant [(qty "Enigma" 3) (qty "Hedge Fund" 3)])
              (default-challenger [(qty "Parasite" 3) (qty "Grimoire" 1)]))
    (play-from-hand state :contestant "Enigma" "HQ")
    (let [enig (get-character state :hq 0)]
      (core/rez state :contestant enig)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Grimoire")
      (play-from-hand state :challenger "Parasite")
      (prompt-select :challenger enig)
      (let [psite (first (:hosted (refresh enig)))]
        (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
        (is (= 1 (:current-strength (refresh enig))) "Enigma reduced to 1 strength")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (count (:discard (get-contestant)))) "Enigma trashed")
        (is (= 1 (count (:discard (get-challenger)))) "Parasite trashed when Enigma was trashed")))))

(deftest plague
  ;; Plague
  (do-game
    (new-game (default-contestant [(qty "Mark Yale" 1)])
              (default-challenger [(qty "Plague" 1)]))
    (play-from-hand state :contestant "Mark Yale" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Plague")
    (prompt-choice :challenger "Server 1")
    (let [plague (get-in @state [:challenger :rig :resource 0])]
      (run-empty-server state "Server 1")
      (is (= 2 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Server 1")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Archives")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague did not gain counters"))))

(deftest progenitor-host-hivemind
  ;; Progenitor - Hosting Hivemind, using Virus Breeding Ground. Issue #738
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Progenitor" 1) (qty "Virus Breeding Ground" 1) (qty "Hivemind" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Progenitor")
    (play-from-hand state :challenger "Virus Breeding Ground")
    (is (= 4 (:memory (get-challenger))))
    (let [prog (get-in @state [:challenger :rig :resource 0])
          vbg (get-in @state [:challenger :rig :muthereff 0])]
      (card-ability state :challenger prog 0)
      (prompt-select :challenger (find-card "Hivemind" (:hand (get-challenger))))
      (is (= 4 (:memory (get-challenger))) "No memory used to host on Progenitor")
      (let [hive (first (:hosted (refresh prog)))]
        (is (= "Hivemind" (:title hive)) "Hivemind is hosted on Progenitor")
        (is (= 1 (get-counters hive :virus)) "Hivemind has 1 counter")
        (is (= 0 (:credit (get-challenger))) "Full cost to host on Progenitor")
        (take-credits state :challenger 1)
        (take-credits state :contestant)
        (card-ability state :challenger vbg 0) ; use VBG to transfer 1 token to Hivemind
        (prompt-select :challenger hive)
        (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
        (is (= 0 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))

(deftest progenitor-mu-savings
  ;; Progenitor - Keep MU the same when hosting or trashing hosted resources
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Progenitor" 1) (qty "Hivemind" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Progenitor")
    (let [pro (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger pro 0)
      (prompt-select :challenger (find-card "Hivemind" (:hand (get-challenger))))
      (is (= 2 (:click (get-challenger))))
      (is (= 2 (:credit (get-challenger))))
      (is (= 4 (:memory (get-challenger))) "Hivemind 2 MU not deducted from available MU")
      ;; Trash Hivemind
      (core/move state :challenger (find-card "Hivemind" (:hosted (refresh pro))) :discard)
      (is (= 4 (:memory (get-challenger))) "Hivemind 2 MU not added to available MU"))))

(deftest reaver
  ;; Reaver - Draw a card the first time you trash an installed card each turn
  (do-game
    (new-game (default-contestant [(qty "PAD Campaign" 1)])
              (default-challenger [(qty "Reaver" 1) (qty "Fall Guy" 5)]))
    (starting-hand state :challenger ["Reaver" "Fall Guy"])
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (core/gain state :challenger :click 1)
    (play-from-hand state :challenger "Reaver")
    (is (= 1 (count (:hand (get-challenger)))) "One card in hand")
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Yes") ; Trash PAD campaign
    (is (= 2 (count (:hand (get-challenger)))) "Drew a card from trash of contestant card")
    (play-from-hand state :challenger "Fall Guy")
    (play-from-hand state :challenger "Fall Guy")
    (is (= 0 (count (:hand (get-challenger)))) "No cards in hand")
    ; No draw from Fall Guy trash as Reaver already fired this turn
    (card-ability state :challenger (get-muthereff state 0) 1)
    (is (= 0 (count (:hand (get-challenger)))) "No cards in hand")
    (take-credits state :challenger)
    ; Draw from Fall Guy trash on contestant turn
    (card-ability state :challenger (get-muthereff state 0) 1)
    (is (= 1 (count (:hand (get-challenger)))) "One card in hand")))

(deftest reaver-fcc
  ;; Reaver / Freelance Coding Construct - should not draw when trash from hand #2671
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Reaver" 9) (qty "Imp" 1) (qty "Snitch" 1) (qty "Freelance Coding Contract" 1)]))
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
    (is (= 0 (count (:hand (get-challenger)))) "No cards in hand")))

(deftest rng-key
  ;; RNG Key - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost
  (do-game
    (new-game (default-contestant [(qty "Enigma" 5) (qty "Hedge Fund" 1)])
              (default-challenger [(qty "RNG Key" 1) (qty "Paperclip" 2)]))
    (starting-hand state :contestant ["Hedge Fund"])
    (starting-hand state :challenger ["RNG Key"])
    (take-credits state :contestant)

    (play-from-hand state :challenger "RNG Key")
    (is (= 5 (:credit (get-challenger))) "Starts at 5 credits")
    (run-on state "HQ")
    (run-successful state)
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger 5)
    (prompt-choice :challenger "Gain 3 [Credits]")
    (is (= 8 (:credit (get-challenger))) "Gained 3 credits")
    (prompt-choice :challenger "OK")

    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :challenger "OK")
    (take-credits state :challenger)
    (take-credits state :contestant)

    (run-on state "Archives")
    (run-successful state)
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :challenger "No")
    (run-on state "HQ")
    (run-successful state)
    (prompt-choice :challenger "OK")
    (take-credits state :challenger)
    (take-credits state :contestant)

    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger 2)
    (prompt-choice :challenger "OK")

    (take-credits state :challenger)
    (take-credits state :contestant)

    (is (= 0 (count (:hand (get-challenger)))) "Started with 0 cards")
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger 3)
    (prompt-choice :challenger "Draw 2 cards")
    (prompt-choice :challenger "OK")
    (is (= 2 (count (:hand (get-challenger)))) "Gained 2 cards")
    (is (= 0 (count (:deck (get-challenger)))) "Cards came from deck")))

(deftest scheherazade
  ;; Scheherazade - Gain 1 credit when it hosts a resource
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Scheherazade" 1) (qty "Cache" 1)
                               (qty "Inti" 1) (qty "Fall Guy" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Scheherazade")
    (let [sch (get-in @state [:challenger :rig :resource 0])]
      (card-ability state :challenger sch 0)
      (prompt-select :challenger (find-card "Inti" (:hand (get-challenger))))
      (is (= 1 (count (:hosted (refresh sch)))))
      (is (= 2 (:click (get-challenger))) "Spent 1 click to install and host")
      (is (= 6 (:credit (get-challenger))) "Gained 1 credit")
      (is (= 3 (:memory (get-challenger))) "Resources hosted on Scheh consume MU")
      (card-ability state :challenger sch 0)
      (prompt-select :challenger (find-card "Cache" (:hand (get-challenger))))
      (is (= 2 (count (:hosted (refresh sch)))))
      (is (= 6 (:credit (get-challenger))) "Gained 1 credit")
      (card-ability state :challenger sch 0)
      (prompt-select :challenger (find-card "Fall Guy" (:hand (get-challenger))))
      (is (= 2 (count (:hosted (refresh sch)))) "Can't host non-resource")
      (is (= 1 (count (:hand (get-challenger))))))))

(deftest self-modifying-code
  ;; Trash & pay 2 to search deck for a resource and install it. Shuffle.
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Self-modifying Code" 3) (qty "Reaver" 1)]))
    (starting-hand state :challenger ["Self-modifying Code" "Self-modifying Code"])
    (core/gain state :challenger :credit 5)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Self-modifying Code")
    (play-from-hand state :challenger "Self-modifying Code")
    (let [smc1 (get-in @state [:challenger :rig :resource 0])
          smc2 (get-in @state [:challenger :rig :resource 1])]
      (card-ability state :challenger smc1 0)
      (prompt-card :challenger (find-card "Reaver" (:deck (get-challenger))))
      (is (= 6 (:credit (get-challenger))) "Paid 2 for SMC, 2 for install - 6 credits left")
      (is (= 1 (:memory (get-challenger))) "SMC MU refunded")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (card-ability state :challenger smc2 0)
      (= 1 (count (:hand (get-challenger))) "1 card drawn due to Reaver before SMC resource selection")
      (= 0 (count (:deck (get-challenger))) "Deck empty"))))

(deftest sneakdoor-nerve-agent
  ;; Sneakdoor Beta - Allow Nerve Agent to gain counters. Issue #1158/#955
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Sneakdoor Beta" 1) (qty "Nerve Agent" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Nerve Agent")
    (play-from-hand state :challenger "Sneakdoor Beta")
    (let [nerve (get-in @state [:challenger :rig :resource 0])
          sb (get-in @state [:challenger :rig :resource 1])]
      (card-ability state :challenger sb 0)
      (run-successful state)
      (is (= 1 (get-counters (refresh nerve) :virus)))
      (card-ability state :challenger sb 0)
      (run-successful state)
      (is (= 2 (get-counters (refresh nerve) :virus))))))

(deftest sneakdoor-ash
  ;; Sneakdoor Beta - Gabriel Santiago, Ash on HQ should prevent Sneakdoor HQ access but still give Gabe credits.
  ;; Issue #1138.
  (do-game
    (new-game (default-contestant [(qty "Ash 2X3ZB9CY" 1)])
              (make-deck "Gabriel Santiago: Consummate Professional" [(qty "Sneakdoor Beta" 1)]))
    (play-from-hand state :contestant "Ash 2X3ZB9CY" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sneakdoor Beta")
    (is (= 1 (:credit (get-challenger))) "Sneakdoor cost 4 credits")
    (let [sb (get-in @state [:challenger :rig :resource 0])
          ash (get-content state :hq 0)]
      (core/rez state :contestant ash)
      (card-ability state :challenger sb 0)
      (run-successful state)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 3 (:credit (get-challenger))) "Gained 2 credits from Gabe's ability")
      (is (= (:cid ash) (-> (get-challenger) :prompt first :card :cid)) "Ash interrupted HQ access after Sneakdoor run")
      (is (= :hq (-> (get-challenger) :register :successful-run first)) "Successful Run on HQ recorded"))))

(deftest sneakdoor-crisium
  ;; Sneakdoor Beta - do not switch to HQ if Archives has Crisium Grid. Issue #1229.
  (do-game
    (new-game (default-contestant [(qty "Crisium Grid" 1) (qty "Priority Requisition" 1) (qty "Private Security Force" 1)])
              (default-challenger [(qty "Sneakdoor Beta" 1)]))
    (play-from-hand state :contestant "Crisium Grid" "Archives")
    (trash-from-hand state :contestant "Priority Requisition")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sneakdoor Beta")
    (let [sb (get-resource state 0)
          cr (get-content state :archives 0)]
      (core/rez state :contestant cr)
      (card-ability state :challenger sb 0)
      (run-successful state)
      (is (= :archives (get-in @state [:run :server 0])) "Crisium Grid stopped Sneakdoor Beta from switching to HQ"))))

(deftest sneakdoor-sectest
  ;; Sneakdoor Beta - Grant Security Testing credits on HQ.
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Security Testing" 1) (qty "Sneakdoor Beta" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sneakdoor Beta")
    (play-from-hand state :challenger "Security Testing")
    (take-credits state :challenger)
    (is (= 3 (:credit (get-challenger))))
    (take-credits state :contestant)
    (let [sb (get-in @state [:challenger :rig :resource 0])]
      (prompt-choice :challenger "HQ")
      (card-ability state :challenger sb 0)
      (run-successful state)
      (is (not (:run @state)) "Switched to HQ and ended the run from Security Testing")
      (is (= 5 (:credit (get-challenger))) "Sneakdoor switched to HQ and earned Security Testing credits"))))

(deftest snitch
  ;; Snitch - Only works on unrezzed character
  (do-game
    (new-game (default-contestant [(qty "Quandary" 2)])
              (default-challenger [(qty "Snitch" 1)]))
    (play-from-hand state :contestant "Quandary" "R&D")
    (play-from-hand state :contestant "Quandary" "HQ")
    (let [hqcharacter (get-character state :hq 0)]
      (core/rez state :contestant hqcharacter))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Snitch")
    (let [snitch (get-in @state [:challenger :rig :resource 0])]
      ;; unrezzed character scenario
      (run-on state "R&D")
      (card-ability state :challenger snitch 0)
      (is (prompt-is-card? :challenger snitch) "Option to jack out")
      (prompt-choice :challenger "Yes")
      ;; rezzed character scenario
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
   (new-game (default-contestant [(qty "Ice Wall" 1) (qty "Quandary" 1)])
             (default-challenger [(qty "Surfer" 1)]))
   (play-from-hand state :contestant "Quandary" "HQ")
   (play-from-hand state :contestant "Ice Wall" "HQ")
   (take-credits state :contestant)
   (play-from-hand state :challenger "Surfer")
   (is (= 3 (:credit (get-challenger))) "Paid 2 credits to install Surfer")
   (core/rez state :contestant (get-character state :hq 1))
   (run-on state "HQ")
   (is (= 2 (get-in @state [:run :position])) "Starting run at position 2")
   (let [surf (get-in @state [:challenger :rig :resource 0])]
     (card-ability state :challenger surf 0)
     (prompt-select :challenger (get-character state :hq 0))
     (is (= 1 (:credit (get-challenger))) "Paid 2 credits to use Surfer")
     (is (= 1 (get-in @state [:run :position])) "Now at next position (1)")
     (is (= "Ice Wall" (:title (get-character state :hq 0))) "Ice Wall now at position 1"))))

(deftest takobi
  ;; Takobi - 2 power counter to add +3 strength to a non-AI icebreaker for encounter
  (do-game
    (new-game (default-contestant [(qty "Enigma" 1)])
              (default-challenger [(qty "Takobi" 1) (qty "Corroder" 1) (qty "Faust" 1)]))
    (play-from-hand state :contestant "Enigma" "HQ")
    (take-credits state :contestant)

    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Takobi")
    (play-from-hand state :challenger "Corroder")
    (play-from-hand state :challenger "Faust")
    (let [tako (get-in @state [:challenger :rig :resource 0])
          corr (get-in @state [:challenger :rig :resource 1])
          faus (get-in @state [:challenger :rig :resource 2])]
      (dotimes [_ 3]
        (card-ability state :challenger tako 0))
      (is (= 3 (get-counters (refresh tako) :power)) "3 counters on Takobi")

      (run-on state "HQ")
      (card-ability state :challenger tako 1)
      (is (empty? (:prompt (get-challenger))) "No prompt for un-rezzed character")
      (core/rez state :contestant (get-character state :hq 0))
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

(deftest upya
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Upya" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Upya")
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
    (is (= 3 (get-counters (get-resource state 0) :power)) "3 counters on Upya")
    (take-credits state :contestant)
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
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
      (is (= 0 (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 5 (:click (get-challenger))) "Gained 2 clicks"))))

(deftest wari
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 1)])
              (default-challenger [(qty "Wari" 1)]))
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Wari")
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger "Barrier")
    (prompt-select :challenger (get-character state :rd 0))
    (is (= 1 (count (:discard (get-challenger)))) "Wari in heap")
    (is (not (empty? (get-in @state [:challenger :prompt]))) "Challenger is currently accessing Ice Wall")))

