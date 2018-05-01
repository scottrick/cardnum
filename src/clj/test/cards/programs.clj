(ns test.cards.programs
  (:require [game.core :as core]
            [game.utils :refer :all]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest au-revoir
  ;; Au Revoir - Gain 1 credit every time you jack out
  (do-game
    (new-game (default-minion) (default-hero [(qty "Au Revoir" 2)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Au Revoir")
    (run-on state "Archives")
    (core/no-action state :minion nil)
    (core/jack-out state :hero nil)
    (is (= 5 (:credit (get-hero))) "Gained 1 credit from jacking out")
    (play-from-hand state :hero "Au Revoir")
    (run-on state "Archives")
    (core/no-action state :minion nil)
    (core/jack-out state :hero nil)
    (is (= 6 (:credit (get-hero))) "Gained 1 credit from each copy of Au Revoir")))

(deftest crescentus
  ;; Crescentus should only work on rezzed ice
  (do-game
    (new-game (default-minion [(qty "Quandary" 1)])
              (default-hero [(qty "Crescentus" 1)]))
    (play-from-hand state :minion "Quandary" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Crescentus")
    (run-on state "HQ")
    (let [cres (get-in @state [:hero :rig :program 0])
          q (get-ice state :hq 0)]
      (card-ability state :hero cres 0)
      (is (not (nil? (get-in @state [:hero :rig :program 0]))) "Crescentus could not be used because the ICE is not rezzed")
      (core/rez state :minion q)
      (is (get-in (refresh q) [:rezzed]) "Quandary is now rezzed")
      (card-ability state :hero cres 0)
      (is (nil? (get-in @state [:hero :rig :program 0])) "Crescentus could be used because the ICE is rezzed")
      (is (not (get-in (refresh q) [:rezzed])) "Quandary is no longer rezzed"))))

(deftest datasucker
  ;; Datasucker - Reduce strength of encountered ICE
  (do-game
    (new-game (default-minion [(qty "Fire Wall" 1)])
              (default-hero [(qty "Datasucker" 1)]))
    (play-from-hand state :minion "Fire Wall" "New remote")
    (take-credits state :minion)
    (core/gain state :hero :click 3)
    (play-from-hand state :hero "Datasucker")
    (let [ds (get-in @state [:hero :rig :program 0])
          fw (get-ice state :remote1 0)]
      (run-empty-server state "Archives")
      (is (= 1 (get-counters (refresh ds) :virus)))
      (run-empty-server state "Archives")
      (is (= 2 (get-counters (refresh ds) :virus)))
      (run-on state "Server 1")
      (run-continue state)
      (run-successful state)
      (is (= 2 (get-counters (refresh ds) :virus)) "No counter gained, not a central server")
      (run-on state "Server 1")
      (core/rez state :minion fw)
      (is (= 5 (:current-strength (refresh fw))))
      (card-ability state :hero ds 0)
      (is (= 1 (get-counters (refresh ds) :virus)) "1 counter spent from Datasucker")
      (is (= 4 (:current-strength (refresh fw))) "Fire Wall strength lowered by 1"))))

(deftest datasucker-trashed
  ;; Datasucker - does not affect next ice when current is trashed. Issue #1788.
  (do-game
    (new-game
      (default-minion [(qty "Wraparound" 1) (qty "Spiderweb" 1)])
      (default-minion [(qty "Datasucker" 1) (qty "Parasite" 1)]))
    (play-from-hand state :minion "Spiderweb" "HQ")
    (play-from-hand state :minion "Wraparound" "HQ")
    (take-credits state :minion)
    (core/gain state :minion :credit 10)
    (play-from-hand state :hero "Datasucker")
    (let [sucker (get-program state 0)
          spider (get-ice state :hq 0)
          wrap (get-ice state :hq 1)]
      (core/add-counter state :hero sucker :virus 2)
      (core/rez state :minion spider)
      (core/rez state :minion wrap)
      (play-from-hand state :hero "Parasite")
      (prompt-select :hero (refresh spider))
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :hero (refresh sucker) 0)
      (card-ability state :hero (refresh sucker) 0)
      (is (find-card "Spiderweb" (:discard (get-minion))) "Spiderweb trashed by Parasite + Datasucker")
      (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Datasucker"))))

(deftest diwan
  ;; Diwan - Full test
  (do-game
    (new-game (default-minion [(qty "Ice Wall" 3) (qty "Fire Wall" 3) (qty "Crisium Grid" 2)])
              (default-hero [(qty "Diwan" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Diwan")
    (prompt-choice :hero "HQ")
    (take-credits state :hero)
    (is (= 8 (:credit (get-minion))) "8 credits for minion at start of second turn")
    (play-from-hand state :minion "Ice Wall" "R&D")
    (is (= 8 (:credit (get-minion))) "Diwan did not charge extra for install on another server")
    (play-from-hand state :minion "Ice Wall" "HQ")
    (is (= 7 (:credit (get-minion))) "Diwan charged 1cr to install ice protecting the named server")
    (play-from-hand state :minion "Crisium Grid" "HQ")
    (is (= 7 (:credit (get-minion))) "Diwan didn't charge to install another upgrade in root of HQ")
    (take-credits state :minion)
    (take-credits state :hero)
    (play-from-hand state :minion "Ice Wall" "HQ")
    (is (= 5 (:credit (get-minion))) "Diwan charged 1cr + 1cr to install a second ice protecting the named server")
    (core/gain state :minion :click 1)
    (core/purge state :minion)
    (play-from-hand state :minion "Fire Wall" "HQ") ; 2cr cost from normal install cost
    (is (= "Diwan" (-> (get-hero) :discard first :title)) "Diwan was trashed from purge")
    (is (= 3 (:credit (get-minion))) "No charge for installs after Diwan purged")))

(deftest djinn-host-chakana
  ;; Djinn - Hosted Chakana does not disable advancing agendas. Issue #750
  (do-game
    (new-game (default-minion [(qty "Priority Requisition" 1)])
              (default-hero [(qty "Djinn" 1) (qty "Chakana" 1)]))
    (play-from-hand state :minion "Priority Requisition" "New remote")
    (take-credits state :minion 2)
    (play-from-hand state :hero "Djinn")
    (let [djinn (get-in @state [:hero :rig :program 0])
          agenda (get-content state :remote1 0)]
      (is agenda "Agenda was installed")
      (card-ability state :hero djinn 1)
      (prompt-select :hero (find-card "Chakana" (:hand (get-hero))))
      (let [chak (first (:hosted (refresh djinn)))]
        (is (= "Chakana" (:title chak)) "Djinn has a hosted Chakana")
        ;; manually add 3 counters
        (core/add-counter state :hero (first (:hosted (refresh djinn))) :virus 3)
        (take-credits state :hero 2)
        (core/advance state :minion {:card agenda})
        (is (= 1 (:advance-counter (refresh agenda))) "Agenda was advanced")))))

(deftest djinn-host-program
  ;; Djinn - Host a non-icebreaker program
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Djinn" 1) (qty "Chakana" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Djinn")
    (is (= 3 (:memory (get-hero))))
    (let [djinn (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero djinn 1)
      (prompt-select :hero (find-card "Chakana" (:hand (get-hero))))
      (is (= 3 (:memory (get-hero))) "No memory used to host on Djinn")
      (is (= "Chakana" (:title (first (:hosted (refresh djinn))))) "Djinn has a hosted Chakana")
      (is (= 1 (:credit (get-hero))) "Full cost to host on Djinn"))))

(deftest djinn-tutor-virus
  ;; Djinn - Tutor a virus program
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Djinn" 1) (qty "Parasite" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Djinn")
    (core/move state :hero (find-card "Parasite" (:hand (get-hero))) :deck)
    (is (zero? (count (:hand (get-hero)))) "No cards in hand after moving Parasite to deck")
    (let [djinn (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero djinn 0)
      (prompt-card :hero (find-card "Parasite" (:deck (get-hero))))
      (is (= "Parasite" (:title (first (:hand (get-hero))))) "Djinn moved Parasite to hand")
      (is (= 2 (:credit (get-hero))) "1cr to use Djinn ability")
      (is (= 2 (:click (get-hero))) "1click to use Djinn ability"))))

(deftest equivocation
  ;; Equivocation - interactions with other successful-run events.
  (do-game
    (new-game
      (default-minion [(qty "Restructure" 3) (qty "Hedge Fund" 3)])
      (make-deck "Laramy Fisk: Savvy Investor" [(qty "Equivocation" 1) (qty "Desperado" 1)]))
    (starting-hand state :minion ["Hedge Fund"])
    (take-credits state :minion)
    (play-from-hand state :hero "Equivocation")
    (play-from-hand state :hero "Desperado")
    (run-empty-server state :rd)
    (prompt-choice :hero "Laramy Fisk: Savvy Investor")
    (prompt-choice :hero "Yes")
    (is (= 2 (count (:hand (get-minion)))) "Corp forced to draw by Fisk")
    (prompt-choice :hero "Yes") ; Equivocation prompt
    (prompt-choice :hero "Yes") ; force the draw
    (is (= 1 (:credit (get-hero))) "Runner gained 1cr from Desperado")
    (is (= 3 (count (:hand (get-minion)))) "Corp forced to draw by Equivocation")
    (prompt-choice :hero "OK")
    (is (not (:run @state)) "Run ended")))

(deftest false-echo
  ;; False Echo - choice for Corp
  (do-game
    (new-game (default-minion [(qty "Ice Wall" 3)])
              (default-hero [(qty "False Echo" 3)]))
    (play-from-hand state :minion "Ice Wall" "Archives")
    (play-from-hand state :minion "Ice Wall" "Archives")
    (take-credits state :minion)
    (play-from-hand state :hero "False Echo")
    (play-from-hand state :hero "False Echo")
    (run-on state "Archives")
    (run-continue state)
    (let [echo1 (get-program state 0)
          echo2 (get-program state 1)]
      (card-ability state :hero echo1 0)
      (prompt-choice :minion "Add to HQ")
      (is (= 2 (count (:hand (get-minion)))) "Ice Wall added to HQ")
      (is (= 1 (count (:discard (get-hero)))) "False Echo trashed")
      (run-continue state)
      (card-ability state :hero echo2 0)
      (prompt-choice :minion "Rez")
      (is (:rezzed (get-ice state :archives 0)) "Ice Wall rezzed")
      (is (= 2 (count (:discard (get-hero)))) "False Echo trashed"))))

(deftest gravedigger
  ;; Gravedigger - Gain counters when Corp cards are trashed, spend click-counter to mill Corp
  (do-game
    (new-game (default-minion [(qty "Launch Campaign" 2) (qty "Enigma" 2)])
              (default-hero [(qty "Gravedigger" 1)]))
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Gravedigger")
    (let [gd (get-in @state [:hero :rig :program 0])]
      (core/trash state :minion (get-content state :remote1 0))
      (is (= 1 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/trash state :minion (get-content state :remote2 0))
      (is (= 2 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/move state :minion (find-card "Enigma" (:hand (get-minion))) :deck)
      (core/move state :minion (find-card "Enigma" (:hand (get-minion))) :deck)
      (is (= 2 (count (:deck (get-minion)))))
      (card-ability state :hero gd 0)
      (is (= 1 (get-counters (refresh gd) :virus)) "Spent 1 counter from Gravedigger")
      (is (= 2 (:click (get-hero))) "Spent 1 click")
      (is (= 1 (count (:deck (get-minion)))))
      (is (= 3 (count (:discard (get-minion)))) "Milled 1 card from R&D"))))

(deftest harbinger-blacklist
  ;; Harbinger - install facedown when Blacklist installed
  (do-game
    (new-game (default-minion [(qty "Blacklist" 1)])
              (default-hero [(qty "Harbinger" 1)]))
    (play-from-hand state :minion "Blacklist" "New remote")
    (core/rez state :minion (get-content state :remote1 0) )
    (take-credits state :minion)
    (play-from-hand state :hero "Harbinger")
    (core/trash state :hero (-> (get-hero) :rig :program first))
    (is (= 0 (count (:discard (get-hero)))) "Harbinger not in heap")
    (is (-> (get-hero) :rig :facedown first :facedown) "Harbinger installed facedown")))

(deftest hyperdriver
  ;; Hyperdriver - Remove from game to gain 3 clicks
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Hyperdriver" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Hyperdriver")
    (is (= 1 (:memory (get-hero))) "3 MU used")
    (take-credits state :hero)
    (take-credits state :minion)
    (is (:hero-phase-12 @state) "Runner in Step 1.2")
    (let [hyp (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero hyp 0)
      (core/end-phase-12 state :hero nil)
      (is (= 7 (:click (get-hero))) "Gained 3 clicks")
      (is (= 1 (count (:rfg (get-hero)))) "Hyperdriver removed from game"))))

(deftest hyperdriver-dhegdheer
  ;; triggering a Dhegdeered Hyperdriver should not grant +3 MU
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Hyperdriver" 1)
                               (qty "Dhegdheer" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Dhegdheer")
    (let [dheg (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero dheg 0)
      (prompt-select :hero (find-card "Hyperdriver" (:hand (get-hero))))
      (is (= 4 (:memory (get-hero))) "0 MU used")
      (is (= 2 (:click (get-hero))) "2 clicks used")
      (is (= 3 (:credit (get-hero))) "2 credits used")
      (take-credits state :hero)
      (take-credits state :minion)
      (is (:hero-phase-12 @state) "Runner in Step 1.2")
      (let [hyp (first (:hosted (refresh dheg)))]        
        (card-ability state :hero hyp 0)
        (core/end-phase-12 state :hero nil)
        (is (= 7 (:click (get-hero))) "Used Hyperdriver")
        (is (= 4 (:memory (get-hero))) "Still 0 MU used")))))

(deftest imp-the-future-perfect
  ;; Trashing TFP with Imp should not trigger psi-game -- Issue #1844
  (do-game
    (new-game (default-minion [(qty "The Future Perfect" 1)])
              (default-hero [(qty "Imp" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Imp")
    (testing "Trash before access click"
      (run-empty-server state "HQ")
      ;; Should access TFP at this point
      (card-ability state :hero (get-program state 0) 0)
      (is (empty? (get-in @state [:hero :prompt])) "Should be no psi-game prompt for TFP")
      (is (= "The Future Perfect" (get-in @state [:minion :discard 0 :title])) "TFP trashed")
      (is (= 0 (:agenda-point (get-hero))) "Runner did not steal TFP")
      (core/move state :minion (find-card "The Future Perfect" (:discard (get-minion))) :hand))
    (take-credits state :hero)
    (take-credits state :minion)
    (testing "Trashing after lose psi game"
      (run-empty-server state "HQ")
      ;; Access prompt for TFP
      (prompt-choice :hero "Access")
      (prompt-choice :minion "0 [Credit]")
      (prompt-choice :hero "1 [Credit]")
      ;; Fail psi game
      (card-ability state :hero (get-program state 0) 0)
      (is (empty? (get-in @state [:hero :prompt])) "Should be no steal prompt for TFP")
      (is (= "The Future Perfect" (get-in @state [:minion :discard 0 :title])) "TFP trashed")
      (is (= 0 (:agenda-point (get-hero))) "Runner did not steal TFP"))))

(deftest incubator-transfer-virus-counters
  ;; Incubator - Gain 1 virus counter per turn; trash to move them to an installed virus program
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Incubator" 1) (qty "Datasucker" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Datasucker")
    (play-from-hand state :hero "Incubator")
    (take-credits state :hero)
    (take-credits state :minion)
    (let [ds (get-in @state [:hero :rig :program 0])
          incub (get-in @state [:hero :rig :program 1])]
      (is (= 1 (get-counters (refresh incub) :virus)) "Incubator gained 1 virus counter")
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 2 (get-counters (refresh incub) :virus)) "Incubator has 2 virus counters")
      (card-ability state :hero incub 0)
      (prompt-select :hero ds)
      (is (= 2 (get-counters (refresh ds) :virus)) "Datasucker has 2 virus counters moved from Incubator")
      (is (= 1 (count (get-in @state [:hero :rig :program]))))
      (is (= 1 (count (:discard (get-hero)))) "Incubator trashed")
      (is (= 3 (:click (get-hero)))))))

(deftest ixodidae
  ;; Ixodidae should not trigger on psi-games
  (do-game
    (new-game (default-minion [(qty "Snowflake" 1)])
              (default-hero [(qty "Ixodidae" 1) (qty "Lamprey" 1)]))
    (play-from-hand state :minion "Snowflake" "HQ")
    (take-credits state :minion)
    (is (= 7 (:credit (get-minion))) "Corp at 7 credits")
    (play-from-hand state :hero "Ixodidae")
    (play-from-hand state :hero "Lamprey")
    (is (= 3 (:credit (get-hero))) "Runner paid 3 credits to install Ixodidae and Lamprey")
    (run-on state :hq)
    (let [s (get-ice state :hq 0)]
      (core/rez state :minion s)
      (card-subroutine state :minion s 0)
      (is (prompt-is-card? :minion s) "Corp prompt is on Snowflake")
      (is (prompt-is-card? :hero s) "Runner prompt is on Snowflake")
      (is (= 6 (:credit (get-minion))) "Corp paid 1 credit to rezz Snowflake")
      (prompt-choice :minion "1")
      (prompt-choice :hero "1")
      (is (= 5 (:credit (get-minion))) "Corp paid 1 credit to psi game")
      (is (= 2 (:credit (get-hero))) "Runner did not gain 1 credit from Ixodidae when minion spent on psi game")
      (run-continue state)
      (run-successful state)
      (is (= 4 (:credit (get-minion))) "Corp lost 1 credit to Lamprey")
      (is (= 3 (:credit (get-hero))) "Runner gains 1 credit from Ixodidae due to Lamprey"))))

(deftest lamprey
  ;; Lamprey - Corp loses 1 credit for each successful HQ run; trashed on purge
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Lamprey" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Lamprey")
    (let [lamp (get-in @state [:hero :rig :program 0])]
      (run-empty-server state :hq)
      (is (= 7 (:credit (get-minion))) "Corp lost 1 credit")
      (run-empty-server state :hq)
      (is (= 6 (:credit (get-minion))) "Corp lost 1 credit")
      (run-empty-server state :hq)
      (is (= 5 (:credit (get-minion))) "Corp lost 1 credit")
      (take-credits state :hero)
      (core/purge state :minion)
      (is (empty? (get-in @state [:hero :rig :program])) "Lamprey trashed by purge"))))

(deftest leprechaun-mu-savings
  ;; Leprechaun - Keep MU the same when hosting or trashing hosted programs
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Leprechaun" 1) (qty "Hyperdriver" 1) (qty "Imp" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Leprechaun")
    (let [lep (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero lep 0)
      (prompt-select :hero (find-card "Hyperdriver" (:hand (get-hero))))
      (is (= 2 (:click (get-hero))))
      (is (= 2 (:credit (get-hero))))
      (is (= 3 (:memory (get-hero))) "Hyperdriver 3 MU not deducted from available MU")
      (card-ability state :hero lep 0)
      (prompt-select :hero (find-card "Imp" (:hand (get-hero))))
      (is (= 1 (:click (get-hero))))
      (is (= 0 (:credit (get-hero))))
      (is (= 3 (:memory (get-hero))) "Imp 1 MU not deducted from available MU")
      ;; Trash Hyperdriver
      (core/move state :hero (find-card "Hyperdriver" (:hosted (refresh lep))) :discard)
      (is (= 3 (:memory (get-hero))) "Hyperdriver 3 MU not added to available MU")
      (core/move state :hero (find-card "Imp" (:hosted (refresh lep))) :discard) ; trash Imp
      (is (= 3 (:memory (get-hero))) "Imp 1 MU not added to available MU"))))

(deftest magnum-opus-click
  ;; Magnum Opus - Gain 2 cr
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Magnum Opus" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Magnum Opus")
    (is (= 2 (:memory (get-hero))))
    (is (= 0 (:credit (get-hero))))
    (let [mopus (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero mopus 0)
      (is (= 2 (:credit (get-hero))) "Gain 2cr"))))

(deftest origami
  ;; Origami - Increases Runner max hand size
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Origami" 2)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Origami")
    (is (= 6 (core/hand-size state :hero)))
    (play-from-hand state :hero "Origami")
    (is (= 9 (core/hand-size state :hero)) "Max hand size increased by 2 for each copy installed")))

(deftest paintbrush
  ;; Paintbrush - Give rezzed ICE a chosen subtype until the end of the next run
  (do-game
    (new-game (default-minion [(qty "Ice Wall" 1)])
              (default-hero [(qty "Paintbrush" 1)]))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Paintbrush")
    (is (= 2 (:memory (get-hero))))
    (let [iwall (get-ice state :hq 0)
          pb (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero pb 0)
      (prompt-select :hero iwall)
      (is (= 3 (:click (get-hero))) "Ice Wall not rezzed, so no click charged")
      (prompt-choice :hero "Done") ; cancel out
      (core/rez state :minion iwall)
      (card-ability state :hero pb 0)
      (prompt-select :hero iwall)
      (prompt-choice :hero "Code Gate")
      (is (= 2 (:click (get-hero))) "Click charged")
      (is (= true (has? (refresh iwall) :subtype "Code Gate")) "Ice Wall gained Code Gate")
      (run-empty-server state "Archives")
      (is (= false (has? (refresh iwall) :subtype "Code Gate")) "Ice Wall lost Code Gate at the end of the run"))))

(deftest parasite-apex
  ;; Parasite - Installed facedown w/ Apex
  (do-game
    (new-game (default-minion)
              (make-deck "Apex: Invasive Predator" [(qty "Parasite" 1)]))
    (take-credits state :minion)
    (core/end-phase-12 state :hero nil)
    (prompt-select :hero (find-card "Parasite" (:hand (get-hero))))
    (is (empty? (:prompt (get-hero))) "No prompt to host Parasite")
    (is (= 1 (count (get-in @state [:hero :rig :facedown]))) "Parasite installed face down")))

(deftest parasite-architect
  ;; Parasite - Installed on untrashable Architect should keep gaining counters past 3 and make strength go negative
  (do-game
    (new-game (default-minion [(qty "Architect" 3) (qty "Hedge Fund" 3)])
              (default-hero [(qty "Parasite" 3) (qty "Grimoire" 1)]))
    (play-from-hand state :minion "Architect" "HQ")
    (let [arch (get-ice state :hq 0)]
      (core/rez state :minion arch)
      (take-credits state :minion)
      (play-from-hand state :hero "Grimoire")
      (play-from-hand state :hero "Parasite")
      (prompt-select :hero arch)
      (let [psite (first (:hosted (refresh arch)))]
        (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
        (take-credits state :hero)
        (take-credits state :minion)
        (take-credits state :hero)
        (take-credits state :minion)
        (take-credits state :hero)
        (take-credits state :minion)
        (is (= 4 (get-counters (refresh psite) :virus)) "Parasite has 4 counters")
        (is (= -1 (:current-strength (refresh arch))) "Architect at -1 strength")))))

(deftest parasite-builder-moved
  ;; Parasite - Should stay on hosted card moved by Builder
  (do-game
    (new-game (default-minion [(qty "Builder" 3) (qty "Ice Wall" 1)])
              (default-hero [(qty "Parasite" 3)]))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (play-from-hand state :minion "Builder" "Archives")
    (let [builder (get-ice state :archives 0)
          _ (core/rez state :minion builder)
          _ (take-credits state :minion)
          _ (play-from-hand state :hero "Parasite")
          _ (prompt-select :hero builder)
          psite (first (:hosted (refresh builder)))
          _ (take-credits state :hero)
          _ (take-credits state :minion)
          _ (is (= 3 (:current-strength (refresh builder))) "Builder reduced to 3 strength")
          _ (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          _ (take-credits state :hero)
          orig-builder (refresh builder)
          _ (card-ability state :minion builder 0)
          _ (prompt-choice :minion "HQ")
          moved-builder (get-ice state :hq 1)
          _ (is (= (:current-strength orig-builder) (:current-strength moved-builder)) "Builder's state is maintained")
          orig-psite (dissoc (first (:hosted orig-builder)) :host)
          moved-psite (dissoc (first (:hosted moved-builder)) :host)
          _ (is (= orig-psite moved-psite) "Hosted Parasite is maintained")
          _ (take-credits state :minion)
          updated-builder (refresh moved-builder)
          updated-psite (first (:hosted updated-builder))
          _ (is (= 2 (:current-strength updated-builder)) "Builder strength still reduced")
          _ (is (= 2 (get-counters (refresh updated-psite) :virus)) "Parasite counters still incremented")])))

(deftest parasite-gain-counter
  ;; Parasite - Gain 1 counter every Runner turn
  (do-game
    (new-game (default-minion [(qty "Wraparound" 3) (qty "Hedge Fund" 3)])
              (default-hero [(qty "Parasite" 3) (qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (core/rez state :minion wrap)
      (take-credits state :minion)
      (play-from-hand state :hero "Parasite")
      (prompt-select :hero wrap)
      (is (= 3 (:memory (get-hero))) "Parasite consumes 1 MU")
      (let [psite (first (:hosted (refresh wrap)))]
        (is (= 0 (get-counters psite :virus)) "Parasite has no counters yet")
        (take-credits state :hero)
        (take-credits state :minion)
        (is (= 1 (get-counters (refresh psite) :virus))
            "Parasite gained 1 virus counter at start of Runner turn")
        (is (= 6 (:current-strength (refresh wrap))) "Wraparound reduced to 6 strength")))))

(deftest parasite-hivemind-instant-ice-trash
  ;; Parasite - Use Hivemind counters when installed; instantly trash ICE if counters >= ICE strength
  (do-game
    (new-game (default-minion [(qty "Enigma" 3) (qty "Hedge Fund" 3)])
              (default-hero [(qty "Parasite" 1)
                               (qty "Grimoire" 1)
                               (qty "Hivemind" 1)
                               (qty "Sure Gamble" 1)]))
    (play-from-hand state :minion "Enigma" "HQ")
    (let [enig (get-ice state :hq 0)]
      (core/rez state :minion enig)
      (take-credits state :minion)
      (play-from-hand state :hero "Sure Gamble")
      (play-from-hand state :hero "Grimoire")
      (play-from-hand state :hero "Hivemind")
      (let [hive (get-in @state [:hero :rig :program 0])]
        (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind has 2 counters")
        (play-from-hand state :hero "Parasite")
        (prompt-select :hero enig)
        (is (= 1 (count (:discard (get-minion)))) "Enigma trashed instantly")
        (is (= 4 (:memory (get-hero))))
        (is (= 2 (count (:discard (get-hero)))) "Parasite trashed when Enigma was trashed")))))

(deftest parasite-ice-trashed
  ;; Parasite - Trashed along with host ICE when its strength has been reduced to 0
  (do-game
    (new-game (default-minion [(qty "Enigma" 3) (qty "Hedge Fund" 3)])
              (default-hero [(qty "Parasite" 3) (qty "Grimoire" 1)]))
    (play-from-hand state :minion "Enigma" "HQ")
    (let [enig (get-ice state :hq 0)]
      (core/rez state :minion enig)
      (take-credits state :minion)
      (play-from-hand state :hero "Grimoire")
      (play-from-hand state :hero "Parasite")
      (prompt-select :hero enig)
      (let [psite (first (:hosted (refresh enig)))]
        (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
        (is (= 1 (:current-strength (refresh enig))) "Enigma reduced to 1 strength")
        (take-credits state :hero)
        (take-credits state :minion)
        (is (= 1 (count (:discard (get-minion)))) "Enigma trashed")
        (is (= 1 (count (:discard (get-hero)))) "Parasite trashed when Enigma was trashed")))))

(deftest plague
  ;; Plague
  (do-game
    (new-game (default-minion [(qty "Mark Yale" 1)])
              (default-hero [(qty "Plague" 1)]))
    (play-from-hand state :minion "Mark Yale" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Plague")
    (prompt-choice :hero "Server 1")
    (let [plague (get-in @state [:hero :rig :program 0])]
      (run-empty-server state "Server 1")
      (is (= 2 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Server 1")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Archives")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague did not gain counters"))))

(deftest progenitor-host-hivemind
  ;; Progenitor - Hosting Hivemind, using Virus Breeding Ground. Issue #738
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Progenitor" 1) (qty "Virus Breeding Ground" 1) (qty "Hivemind" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Progenitor")
    (play-from-hand state :hero "Virus Breeding Ground")
    (is (= 4 (:memory (get-hero))))
    (let [prog (get-in @state [:hero :rig :program 0])
          vbg (get-in @state [:hero :rig :resource 0])]
      (card-ability state :hero prog 0)
      (prompt-select :hero (find-card "Hivemind" (:hand (get-hero))))
      (is (= 4 (:memory (get-hero))) "No memory used to host on Progenitor")
      (let [hive (first (:hosted (refresh prog)))]
        (is (= "Hivemind" (:title hive)) "Hivemind is hosted on Progenitor")
        (is (= 1 (get-counters hive :virus)) "Hivemind has 1 counter")
        (is (= 0 (:credit (get-hero))) "Full cost to host on Progenitor")
        (take-credits state :hero 1)
        (take-credits state :minion)
        (card-ability state :hero vbg 0) ; use VBG to transfer 1 token to Hivemind
        (prompt-select :hero hive)
        (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
        (is (= 0 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))

(deftest progenitor-mu-savings
  ;; Progenitor - Keep MU the same when hosting or trashing hosted programs
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Progenitor" 1) (qty "Hivemind" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Progenitor")
    (let [pro (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero pro 0)
      (prompt-select :hero (find-card "Hivemind" (:hand (get-hero))))
      (is (= 2 (:click (get-hero))))
      (is (= 2 (:credit (get-hero))))
      (is (= 4 (:memory (get-hero))) "Hivemind 2 MU not deducted from available MU")
      ;; Trash Hivemind
      (core/move state :hero (find-card "Hivemind" (:hosted (refresh pro))) :discard)
      (is (= 4 (:memory (get-hero))) "Hivemind 2 MU not added to available MU"))))

(deftest reaver
  ;; Reaver - Draw a card the first time you trash an installed card each turn
  (do-game
    (new-game (default-minion [(qty "PAD Campaign" 1)])
              (default-hero [(qty "Reaver" 1) (qty "Fall Guy" 5)]))
    (starting-hand state :hero ["Reaver" "Fall Guy"])
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (take-credits state :minion)
    (core/gain state :hero :credit 10)
    (core/gain state :hero :click 1)
    (play-from-hand state :hero "Reaver")
    (is (= 1 (count (:hand (get-hero)))) "One card in hand")
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes") ; Trash PAD campaign
    (is (= 2 (count (:hand (get-hero)))) "Drew a card from trash of minion card")
    (play-from-hand state :hero "Fall Guy")
    (play-from-hand state :hero "Fall Guy")
    (is (= 0 (count (:hand (get-hero)))) "No cards in hand")
    ; No draw from Fall Guy trash as Reaver already fired this turn
    (card-ability state :hero (get-resource state 0) 1)
    (is (= 0 (count (:hand (get-hero)))) "No cards in hand")
    (take-credits state :hero)
    ; Draw from Fall Guy trash on minion turn
    (card-ability state :hero (get-resource state 0) 1)
    (is (= 1 (count (:hand (get-hero)))) "One card in hand")))

(deftest reaver-fcc
  ;; Reaver / Freelance Coding Construct - should not draw when trash from hand #2671
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Reaver" 9) (qty "Imp" 1) (qty "Snitch" 1) (qty "Freelance Coding Contract" 1)]))
    (starting-hand state :hero ["Reaver" "Imp" "Snitch" "Freelance Coding Contract"])
    (take-credits state :minion)
    (play-from-hand state :hero "Reaver")
    (is (= 3 (count (:hand (get-hero)))) "Four cards in hand")
    (is (= 3 (:credit (get-hero))) "3 credits")
    (play-from-hand state :hero "Freelance Coding Contract")
    (prompt-select :hero (find-card "Snitch" (:hand (get-hero))))
    (prompt-select :hero (find-card "Imp" (:hand (get-hero))))
    (prompt-choice :hero "Done")
    (is (= 7 (:credit (get-hero))) "7 credits - FCC fired")
    (is (= 0 (count (:hand (get-hero)))) "No cards in hand")))

(deftest rng-key
  ;; RNG Key - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost
  (do-game
    (new-game (default-minion [(qty "Enigma" 5) (qty "Hedge Fund" 1)])
              (default-hero [(qty "RNG Key" 1) (qty "Paperclip" 2)]))
    (starting-hand state :minion ["Hedge Fund"])
    (starting-hand state :hero ["RNG Key"])
    (take-credits state :minion)

    (play-from-hand state :hero "RNG Key")
    (is (= 5 (:credit (get-hero))) "Starts at 5 credits")
    (run-on state "HQ")
    (run-successful state)
    (prompt-choice :hero "Yes")
    (prompt-choice :hero 5)
    (prompt-choice :hero "Gain 3 [Credits]")
    (is (= 8 (:credit (get-hero))) "Gained 3 credits")
    (prompt-choice :hero "OK")

    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :hero "OK")
    (take-credits state :hero)
    (take-credits state :minion)

    (run-on state "Archives")
    (run-successful state)
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :hero "No")
    (run-on state "HQ")
    (run-successful state)
    (prompt-choice :hero "OK")
    (take-credits state :hero)
    (take-credits state :minion)

    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :hero "Yes")
    (prompt-choice :hero 2)
    (prompt-choice :hero "OK")

    (take-credits state :hero)
    (take-credits state :minion)

    (is (= 0 (count (:hand (get-hero)))) "Started with 0 cards")
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :hero "Yes")
    (prompt-choice :hero 3)
    (prompt-choice :hero "Draw 2 cards")
    (prompt-choice :hero "OK")
    (is (= 2 (count (:hand (get-hero)))) "Gained 2 cards")
    (is (= 0 (count (:deck (get-hero)))) "Cards came from deck")))

(deftest scheherazade
  ;; Scheherazade - Gain 1 credit when it hosts a program
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Scheherazade" 1) (qty "Cache" 1)
                               (qty "Inti" 1) (qty "Fall Guy" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Scheherazade")
    (let [sch (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero sch 0)
      (prompt-select :hero (find-card "Inti" (:hand (get-hero))))
      (is (= 1 (count (:hosted (refresh sch)))))
      (is (= 2 (:click (get-hero))) "Spent 1 click to install and host")
      (is (= 6 (:credit (get-hero))) "Gained 1 credit")
      (is (= 3 (:memory (get-hero))) "Programs hosted on Scheh consume MU")
      (card-ability state :hero sch 0)
      (prompt-select :hero (find-card "Cache" (:hand (get-hero))))
      (is (= 2 (count (:hosted (refresh sch)))))
      (is (= 6 (:credit (get-hero))) "Gained 1 credit")
      (card-ability state :hero sch 0)
      (prompt-select :hero (find-card "Fall Guy" (:hand (get-hero))))
      (is (= 2 (count (:hosted (refresh sch)))) "Can't host non-program")
      (is (= 1 (count (:hand (get-hero))))))))

(deftest self-modifying-code
  ;; Trash & pay 2 to search deck for a program and install it. Shuffle.
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Self-modifying Code" 3) (qty "Reaver" 1)]))
    (starting-hand state :hero ["Self-modifying Code" "Self-modifying Code"])
    (core/gain state :hero :credit 5)
    (take-credits state :minion)
    (play-from-hand state :hero "Self-modifying Code")
    (play-from-hand state :hero "Self-modifying Code")
    (let [smc1 (get-in @state [:hero :rig :program 0])
          smc2 (get-in @state [:hero :rig :program 1])]
      (card-ability state :hero smc1 0)
      (prompt-card :hero (find-card "Reaver" (:deck (get-hero))))
      (is (= 6 (:credit (get-hero))) "Paid 2 for SMC, 2 for install - 6 credits left")
      (is (= 1 (:memory (get-hero))) "SMC MU refunded")
      (take-credits state :hero)
      (take-credits state :minion)
      (card-ability state :hero smc2 0)
      (= 1 (count (:hand (get-hero))) "1 card drawn due to Reaver before SMC program selection")
      (= 0 (count (:deck (get-hero))) "Deck empty"))))

(deftest sneakdoor-nerve-agent
  ;; Sneakdoor Beta - Allow Nerve Agent to gain counters. Issue #1158/#955
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Sneakdoor Beta" 1) (qty "Nerve Agent" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :credit 10)
    (play-from-hand state :hero "Nerve Agent")
    (play-from-hand state :hero "Sneakdoor Beta")
    (let [nerve (get-in @state [:hero :rig :program 0])
          sb (get-in @state [:hero :rig :program 1])]
      (card-ability state :hero sb 0)
      (run-successful state)
      (is (= 1 (get-counters (refresh nerve) :virus)))
      (card-ability state :hero sb 0)
      (run-successful state)
      (is (= 2 (get-counters (refresh nerve) :virus))))))

(deftest sneakdoor-ash
  ;; Sneakdoor Beta - Gabriel Santiago, Ash on HQ should prevent Sneakdoor HQ access but still give Gabe credits.
  ;; Issue #1138.
  (do-game
    (new-game (default-minion [(qty "Ash 2X3ZB9CY" 1)])
              (make-deck "Gabriel Santiago: Consummate Professional" [(qty "Sneakdoor Beta" 1)]))
    (play-from-hand state :minion "Ash 2X3ZB9CY" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Sneakdoor Beta")
    (is (= 1 (:credit (get-hero))) "Sneakdoor cost 4 credits")
    (let [sb (get-in @state [:hero :rig :program 0])
          ash (get-content state :hq 0)]
      (core/rez state :minion ash)
      (card-ability state :hero sb 0)
      (run-successful state)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (= 3 (:credit (get-hero))) "Gained 2 credits from Gabe's ability")
      (is (= (:cid ash) (-> (get-hero) :prompt first :card :cid)) "Ash interrupted HQ access after Sneakdoor run")
      (is (= :hq (-> (get-hero) :register :successful-run first)) "Successful Run on HQ recorded"))))

(deftest sneakdoor-crisium
  ;; Sneakdoor Beta - do not switch to HQ if Archives has Crisium Grid. Issue #1229.
  (do-game
    (new-game (default-minion [(qty "Crisium Grid" 1) (qty "Priority Requisition" 1) (qty "Private Security Force" 1)])
              (default-hero [(qty "Sneakdoor Beta" 1)]))
    (play-from-hand state :minion "Crisium Grid" "Archives")
    (trash-from-hand state :minion "Priority Requisition")
    (take-credits state :minion)
    (play-from-hand state :hero "Sneakdoor Beta")
    (let [sb (get-program state 0)
          cr (get-content state :archives 0)]
      (core/rez state :minion cr)
      (card-ability state :hero sb 0)
      (run-successful state)
      (is (= :archives (get-in @state [:run :server 0])) "Crisium Grid stopped Sneakdoor Beta from switching to HQ"))))

(deftest sneakdoor-sectest
  ;; Sneakdoor Beta - Grant Security Testing credits on HQ.
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Security Testing" 1) (qty "Sneakdoor Beta" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Sneakdoor Beta")
    (play-from-hand state :hero "Security Testing")
    (take-credits state :hero)
    (is (= 3 (:credit (get-hero))))
    (take-credits state :minion)
    (let [sb (get-in @state [:hero :rig :program 0])]
      (prompt-choice :hero "HQ")
      (card-ability state :hero sb 0)
      (run-successful state)
      (is (not (:run @state)) "Switched to HQ and ended the run from Security Testing")
      (is (= 5 (:credit (get-hero))) "Sneakdoor switched to HQ and earned Security Testing credits"))))

(deftest snitch
  ;; Snitch - Only works on unrezzed ice
  (do-game
    (new-game (default-minion [(qty "Quandary" 2)])
              (default-hero [(qty "Snitch" 1)]))
    (play-from-hand state :minion "Quandary" "R&D")
    (play-from-hand state :minion "Quandary" "HQ")
    (let [hqice (get-ice state :hq 0)]
      (core/rez state :minion hqice))
    (take-credits state :minion)
    (play-from-hand state :hero "Snitch")
    (let [snitch (get-in @state [:hero :rig :program 0])]
      ;; unrezzed ice scenario
      (run-on state "R&D")
      (card-ability state :hero snitch 0)
      (is (prompt-is-card? :hero snitch) "Option to jack out")
      (prompt-choice :hero "Yes")
      ;; rezzed ice scenario
      (run-on state "HQ")
      (card-ability state :hero snitch 0)
      (is (empty? (get-in @state [:hero :prompt])) "No option to jack out")
      ;; no ice scenario
      (run-on state "Archives")
      (card-ability state :hero snitch 0)
      (is (empty? (get-in @state [:hero :prompt])) "No option to jack out"))))

(deftest surfer
  ;; Surfer - Swap position with ice before or after when encountering a Barrier ICE
  (do-game
   (new-game (default-minion [(qty "Ice Wall" 1) (qty "Quandary" 1)])
             (default-hero [(qty "Surfer" 1)]))
   (play-from-hand state :minion "Quandary" "HQ")
   (play-from-hand state :minion "Ice Wall" "HQ")
   (take-credits state :minion)
   (play-from-hand state :hero "Surfer")
   (is (= 3 (:credit (get-hero))) "Paid 2 credits to install Surfer")
   (core/rez state :minion (get-ice state :hq 1))
   (run-on state "HQ")
   (is (= 2 (get-in @state [:run :position])) "Starting run at position 2")
   (let [surf (get-in @state [:hero :rig :program 0])]
     (card-ability state :hero surf 0)
     (prompt-select :hero (get-ice state :hq 0))
     (is (= 1 (:credit (get-hero))) "Paid 2 credits to use Surfer")
     (is (= 1 (get-in @state [:run :position])) "Now at next position (1)")
     (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall now at position 1"))))

(deftest takobi
  ;; Takobi - 2 power counter to add +3 strength to a non-AI icebreaker for encounter
  (do-game
    (new-game (default-minion [(qty "Enigma" 1)])
              (default-hero [(qty "Takobi" 1) (qty "Corroder" 1) (qty "Faust" 1)]))
    (play-from-hand state :minion "Enigma" "HQ")
    (take-credits state :minion)

    (core/gain state :hero :credit 10)
    (play-from-hand state :hero "Takobi")
    (play-from-hand state :hero "Corroder")
    (play-from-hand state :hero "Faust")
    (let [tako (get-in @state [:hero :rig :program 0])
          corr (get-in @state [:hero :rig :program 1])
          faus (get-in @state [:hero :rig :program 2])]
      (dotimes [_ 3]
        (card-ability state :hero tako 0))
      (is (= 3 (get-counters (refresh tako) :power)) "3 counters on Takobi")

      (run-on state "HQ")
      (card-ability state :hero tako 1)
      (is (empty? (:prompt (get-hero))) "No prompt for un-rezzed ice")
      (core/rez state :minion (get-ice state :hq 0))
      (card-ability state :hero tako 1)
      (prompt-select :hero (refresh faus))
      (is (not-empty (:prompt (get-hero))) "Can't select AI breakers")
      (prompt-select :hero (refresh corr))
      (is (empty? (:prompt (get-hero))) "Can select non-AI breakers")
      (is (= 5 (:current-strength (refresh corr))) "Corroder at +3 strength")
      (is (= 1 (get-counters (refresh tako) :power)) "1 counter on Takobi")
      (card-ability state :hero tako 1)
      (is (empty? (:prompt (get-hero))) "No prompt when too few power counters")
      (core/no-action state :minion nil)
      (run-continue state)
      (is (= 2 (:current-strength (refresh corr))) "Corroder returned to normal strength"))))

(deftest upya
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Upya" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Upya")
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
    (is (= 3 (get-counters (get-program state 0) :power)) "3 counters on Upya")
    (take-credits state :minion)
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
    (is (= 6 (get-counters (get-program state 0) :power)) "6 counters on Upya")
    (let [upya (get-program state 0)]
      (card-ability state :hero upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 2 (:click (get-hero))) "Gained 2 clicks")
      (card-ability state :hero upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "Upya not used more than once a turn")
      (is (= 2 (:click (get-hero))) "Still at 2 clicks"))
    (take-credits state :hero)
    (take-credits state :minion)
    (let [upya (get-program state 0)]
      (card-ability state :hero upya 0)
      (is (= 0 (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 5 (:click (get-hero))) "Gained 2 clicks"))))

(deftest wari
  (do-game
    (new-game (default-minion [(qty "Ice Wall" 1)])
              (default-hero [(qty "Wari" 1)]))
    (play-from-hand state :minion "Ice Wall" "R&D")
    (take-credits state :minion)
    (play-from-hand state :hero "Wari")
    (run-empty-server state "HQ")
    (prompt-choice :hero "Yes")
    (prompt-choice :hero "Barrier")
    (prompt-select :hero (get-ice state :rd 0))
    (is (= 1 (count (:discard (get-hero)))) "Wari in heap")
    (is (not (empty? (get-in @state [:hero :prompt]))) "Runner is currently accessing Ice Wall")))

