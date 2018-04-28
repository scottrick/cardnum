(ns test.cards.identities
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adam-directives
  ;; Adam - Allow runner to choose directives
  (do-game
    (new-game
      (default-corp)
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (is (= 4 (count (get-in @state [:hazPlayer :play-area]))) "All directives are in the runner's play area")
    (is (= 0 (count (get-in @state [:hazPlayer :hand]))))
    (prompt-select :hazPlayer (find-card "Neutralize All Threats" (get-in @state [:hazPlayer :play-area])))
    (prompt-select :hazPlayer (find-card "Safety First" (get-in @state [:hazPlayer :play-area])))
    (prompt-select :hazPlayer (find-card "Always Be Running" (get-in @state [:hazPlayer :play-area])))
    (is (= 3 (count (get-in @state [:hazPlayer :rig :resource]))) "3 directives were installed")
    (is (= 0 (count (get-in @state [:hazPlayer :play-area]))) "The play area is empty")
    (let [nat (find-card "Neutralize All Threats" (get-in @state [:hazPlayer :rig :resource]))
          sf (find-card "Safety First" (get-in @state [:hazPlayer :rig :resource]))
          abr (find-card "Always Be Running" (get-in @state [:hazPlayer :rig :resource]))]
      (is (and nat sf abr) "The chosen directives were installed"))))

(deftest adam-palana
  ;; Adam - Directives should not grant Pālanā credits.
  (do-game
    (new-game
      (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (prompt-select :hazPlayer (find-card "Neutralize All Threats" (get-in @state [:hazPlayer :play-area])))
    (prompt-select :hazPlayer (find-card "Safety First" (get-in @state [:hazPlayer :play-area])))
    (prompt-select :hazPlayer (find-card "Always Be Running" (get-in @state [:hazPlayer :play-area])))
    (prompt-choice :resPlayer "Keep")
    (prompt-choice :hazPlayer "Keep")
    (core/start-turn state :resPlayer nil)
    (is (= 5 (:credit (get-corp))) "Pālanā does not gain credit from Adam's starting Directives")))

(deftest adam-advanceable-traps
  ;; Adam - Neutralize All Threats interaction with advanceable traps.
  (do-game
    (new-game
      (default-corp [(qty "Cerebral Overwriter" 3)])
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (prompt-select :hazPlayer (find-card "Neutralize All Threats" (get-in @state [:hazPlayer :play-area])))
    (prompt-select :hazPlayer (find-card "Safety First" (get-in @state [:hazPlayer :play-area])))
    (prompt-select :hazPlayer (find-card "Always Be Running" (get-in @state [:hazPlayer :play-area])))
    (prompt-choice :resPlayer "Keep")
    (prompt-choice :hazPlayer "Keep")
    (core/start-turn state :resPlayer nil)

    (play-from-hand state :resPlayer "Cerebral Overwriter" "New remote")
    (advance state (get-content state :remote1 0) 2)
    (take-credits state :resPlayer)
    (run-empty-server state :remote1)
    (prompt-choice :hazPlayer "No") ; Dismiss prompt from non-exiled Find the Truth directive
    (prompt-choice :resPlayer "Yes")
    (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
    (is (= 1 (count (:discard (get-corp)))) "1 card in archives")))

(deftest andromeda
  ;; Andromeda - 9 card starting hand, 1 link
  (do-game
    (new-game
      (default-corp)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 1 (:link (get-runner))) "1 link")
    (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-mulligan
  ;; Andromeda - 9 card starting hand after mulligan
  (do-game
    (new-game
      (default-corp)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)])
      {:mulligan :hazPlayer})
    (is (= 1 (:link (get-runner))) "1 link")
    (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-palana
  ;; Andromeda - should not grant Palana credits.
  (do-game
    (new-game
      (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 5 (:credit (get-corp))) "Palana does not gain credit from Andromeda's starting hand")))

(deftest apex-facedown-console
  ;; Apex - Allow facedown install of a second console. Issue #1326
  (do-game
    (new-game
      (default-corp)
      (make-deck "Apex: Invasive Predator" [(qty "Heartbeat" 2)]))
    (take-credits state :resPlayer)
    (core/end-phase-12 state :hazPlayer nil)
    (prompt-choice :hazPlayer "Done") ; no facedown install on turn 1
    (play-from-hand state :hazPlayer "Heartbeat")
    (is (= 1 (count (get-in @state [:hazPlayer :rig :hardware]))))
    (take-credits state :hazPlayer)
    (take-credits state :resPlayer)
    (core/end-phase-12 state :hazPlayer nil)
    (prompt-select :hazPlayer (find-card "Heartbeat" (:hand (get-runner))))
    (is (= 1 (count (get-in @state [:hazPlayer :rig :facedown]))) "2nd console installed facedown")))

(deftest ayla
  ;; Ayla - choose & use cards for NVRAM
  (do-game
    (new-game
      (default-corp)
      (make-deck "Ayla \"Bios\" Rahim: Simulant Specialist" [(qty "Sure Gamble" 1) (qty "Desperado" 1)
                                                             (qty "Security Testing" 1) (qty "Bank Job" 1)
                                                             (qty "Heartbeat" 1) (qty "Eater" 1)])
      {:dont-start-game true})
    (is (= 6 (count (get-in @state [:hazPlayer :play-area]))) "Deck cards are in play area")
    (is (= 0 (count (get-in @state [:hazPlayer :hand]))))
    (prompt-select :hazPlayer (find-card "Sure Gamble" (get-in @state [:hazPlayer :play-area])))
    (prompt-select :hazPlayer (find-card "Desperado" (get-in @state [:hazPlayer :play-area])))
    (prompt-select :hazPlayer (find-card "Bank Job" (get-in @state [:hazPlayer :play-area])))
    (prompt-select :hazPlayer (find-card "Eater" (get-in @state [:hazPlayer :play-area])))
    (is (= 4 (count (:hosted (:identity (get-runner))))) "4 cards in NVRAM")
    (is (= 0 (count (get-in @state [:hazPlayer :play-area]))) "The play area is empty")
    (prompt-choice :resPlayer "Keep")
    (prompt-choice :hazPlayer "Keep")
    (take-credits state :resPlayer)
    (is (= 2 (count (get-in @state [:hazPlayer :hand]))) "There are 2 cards in the runner's Grip")
    (card-ability state :hazPlayer (:identity (get-runner)) 0)
    (prompt-card :hazPlayer (find-card "Bank Job" (:hosted (:identity (get-runner)))))
    (is (= 3 (count (get-in @state [:hazPlayer :hand]))) "There are 3 cards in the runner's Grip")))

(deftest cerebral-imaging-max-hand-size
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game
      (make-deck "Cerebral Imaging: Infinite Frontiers" [(qty "Hedge Fund" 3)])
      (default-runner))
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Has 13 credits")
    (is (= 13 (core/hand-size state :resPlayer)) "Max hand size is 13")))

(deftest chronos-protocol
  ;; Chronos Protocol - Choose Runner discard for first net damage of a turn
  (do-game
    (new-game
      (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1) (qty "Neural EMP" 2)])
      (default-runner [(qty "Imp" 3)]))
    (play-from-hand state :resPlayer "Pup" "HQ")
    (take-credits state :resPlayer)
    (run-on state :hq)
    (let [pup (get-ice state :hq 0)]
      (core/rez state :resPlayer pup)
      (card-subroutine state :resPlayer pup 0)
      (prompt-choice :resPlayer "Yes")
      (let [imp (find-card "Imp" (:hand (get-runner)))]
        (prompt-choice :resPlayer imp)
        (is (= 1 (count (:discard (get-runner)))))
        (card-subroutine state :resPlayer pup 0)
        (is (empty? (:prompt (get-corp))) "No choice on second net damage")
        (is (= 2 (count (:discard (get-runner)))))
        (run-jack-out state)
        (take-credits state :hazPlayer)
        (core/move state :hazPlayer (find-card "Imp" (:discard (get-runner))) :hand)
        (play-from-hand state :resPlayer "Neural EMP")
        (prompt-choice :resPlayer "No")
        (is (= 2 (count (:discard (get-runner)))) "Damage dealt after declining ability")
        (play-from-hand state :resPlayer "Neural EMP")
        (is (empty? (:prompt (get-corp))) "No choice after declining on first damage")
        (is (= 3 (count (:discard (get-runner)))))))))

(deftest chronos-protocol-employee-strike
  ;; Chronos Protocol - Issue #1958 also affects Chronos Protocol
  (do-game
    (new-game
      (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1)])
      (default-runner [(qty "Employee Strike" 1) (qty "Scrubbed" 3) (qty "Sure Gamble" 1)]))
    (play-from-hand state :resPlayer "Pup" "HQ")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Employee Strike")
    (run-on state :hq)
    (let [pup (get-ice state :hq 0)]
      (core/rez state :resPlayer pup)
      (card-subroutine state :resPlayer pup 0)
      (is (empty? (:prompt (get-corp))) "No choice because of Employee Strike")
      (card-subroutine state :resPlayer pup 0)
      (is (= 2 (count (:discard (get-runner)))))
      (run-jack-out state)
      (take-credits state :hazPlayer)
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Scrubbed")
      (run-on state :hq)
      (card-subroutine state :resPlayer pup 0)
      (is (not (empty? (:prompt (get-corp)))) "Employee Strike out of play - Ability turned on correctly"))))

(deftest edward-kim
  ;; Edward Kim - Trash first operation accessed each turn, but not if first one was in Archives
  (do-game
    (new-game
      (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 2) (qty "PAD Campaign" 1)])
      (make-deck "Edward Kim: Humanity's Hammer" [(qty "Eater" 1) (qty "Sure Gamble" 2)]))
    (play-from-hand state :resPlayer "Hedge Fund")
    (trash-from-hand state :resPlayer "PAD Campaign")
    (take-credits state :resPlayer)
    (run-empty-server state "Archives")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-corp)))) "No operation trashed from HQ; accessed one in Archives first")
    (take-credits state :hazPlayer)
    (core/move state :resPlayer (find-card "Hedge Fund" (:discard (get-corp))) :hand)
    (is (= 1 (count (:discard (get-corp)))))
    (take-credits state :resPlayer)
    (run-empty-server state "Archives")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Eater")
    (let [eater (get-in @state [:hazPlayer :rig :program 0])]
      (run-on state "Archives")
      (card-ability state :hazPlayer eater 0) ; pretend to break a sub so no cards in Archives will be accessed
      (run-successful state)
      (is (= 3 (count (:discard (get-corp)))))
      (run-empty-server state "HQ")
      (is (= 4 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first"))))

(deftest edward-kim-maw
  ;; Edward Kim - Do not trigger maw on first Operation access (due to trash)
  (do-game
    (new-game
      (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 2)])
      (make-deck "Edward Kim: Humanity's Hammer" [(qty "Maw" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Sure Gamble")
    (play-from-hand state :hazPlayer "Maw")
    (is (= 0 (count (:discard (get-corp)))) "No cards in Archives")
    (run-empty-server state "HQ")
    (is (= 1 (count (:discard (get-corp)))) "Only one card trashed from HQ, by Ed Kim")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-corp)))) "One more card trashed from HQ, by Maw")))


(deftest exile-customized-secretary
  ;; Exile - simultaneous-resolution prompt shown for interaction with Customized Secretary
  (do-game
    (new-game
      (default-corp)
      (make-deck "Exile: Streethawk" [(qty "Customized Secretary" 3) (qty "Clone Chip" 3)
                                      (qty "Sure Gamble" 3)]))
    (take-credits state :resPlayer)
    (starting-hand state :hazPlayer ["Customized Secretary" "Clone Chip"])
    (trash-from-hand state :hazPlayer "Customized Secretary")
    (play-from-hand state :hazPlayer "Clone Chip")
    (card-ability state :hazPlayer (get-hardware state 0) 0)
    (prompt-select :hazPlayer (find-card "Customized Secretary" (:discard (get-runner))))
    ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
    (is (= 2 (-> (get-runner) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
    (prompt-choice :hazPlayer "Exile: Streethawk")
    (is (= 1 (count (:hand (get-runner)))) "Exile drew a card")))

(deftest gabriel-santiago
  ;; Gabriel Santiago - Gain 2c on first successful HQ run each turn
  (do-game
    (new-game
      (default-corp)
      (make-deck "Gabriel Santiago: Consummate Professional" [(qty "Easy Mark" 1)]))
    (take-credits state :resPlayer)
    (run-empty-server state :rd)
    (is (= 5 (:credit (get-runner))) "No credits gained")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "Gained 2c")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "No credits gained")))

(deftest gagarin
  ;; Gagarin - pay 1c to access each card in remote
  (do-game
    (new-game
      (make-deck "Gagarin Deep Space: Expanding the Horizon" [(qty "PAD Campaign" 1) (qty "Caprice Nisei" 1)])
      (default-runner))
    (core/lose state :hazPlayer :credit 4)
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (play-from-hand state :resPlayer "PAD Campaign" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state :remote1)
    (prompt-select :hazPlayer (get-content state :remote1 0))
    (is (= 0 (:credit (get-runner))) "Paid 1 credit to access")
    (prompt-choice :hazPlayer "No") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-server state :remote1)
    (prompt-select :hazPlayer (get-content state :remote1 0))
    (prompt-choice :hazPlayer "OK") ; Could not afford message dismissed
    (is (empty? (:prompt (get-runner))) "Runner cannot access so no trash prompt")
    (is (not (last-log-contains? state "PAD Campaign")) "No card name was logged")
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "No") ; Dismiss trash prompt
    (is (last-log-contains? state "Caprice") "Accessed card name was logged")))

(deftest grndl-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (default-runner))
    (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity")))

(deftest grndl-valencia
  ;; GRNDL vs Valencia - only 1 bad pub at start
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
    (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity")))

(deftest haarpsichord-studios
  ;; Haarpsichord Studios - Prevent stealing more than 1 agenda per turn
  (do-game
    (new-game
      (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
      (default-runner [(qty "Gang Sign" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Gang Sign")
    (run-empty-server state "HQ")
    (prompt-choice :hazPlayer "Steal")
    (is (= 1 (:agenda-point (get-runner))))
    (run-empty-server state "HQ")
    (prompt-choice :hazPlayer "Steal")
    (is (= 1 (:agenda-point (get-runner))) "Second steal of turn prevented")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "15 Minutes" "New remote")
    (score-agenda state :resPlayer (get-content state :remote1 0))
    (prompt-choice :hazPlayer "Card from hand")
    (prompt-choice :hazPlayer "Steal")
    (is (= 2 (:agenda-point (get-runner))) "Steal prevention didn't carry over to Corp turn")))

(deftest haarpsichord-studios-employee-strike
  ;; Haarpsichord Studios - Interactions with Employee Strike. Issue #1313.
  (do-game
    (new-game
      (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
      (default-runner [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :click 5)
    (run-empty-server state "HQ")
    (prompt-choice :hazPlayer "Steal")
    (is (= 1 (:agenda-point (get-runner))))
    (play-from-hand state :hazPlayer "Employee Strike")
    (run-empty-server state "HQ")
    (prompt-choice :hazPlayer "Steal")
    (is (= 2 (:agenda-point (get-runner))) "Second steal not prevented")
    (play-from-hand state :hazPlayer "Scrubbed")
    (run-empty-server state "HQ")
    (prompt-choice :hazPlayer "Steal")
    (is (= 2 (:agenda-point (get-runner))) "Third steal prevented")))

(deftest haas-bioroid-architects-of-tomorrow
  ;; Architects of Tomorrow - prompt to rez after passing bioroid
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Architects of Tomorrow" [(qty "Eli 1.0" 2) (qty "Pup" 1)])
      (default-runner))
    (core/gain state :resPlayer :credit 3)
    (play-from-hand state :resPlayer "Eli 1.0" "Archives")
    (play-from-hand state :resPlayer "Pup" "Archives")
    (play-from-hand state :resPlayer "Eli 1.0" "HQ")
    (take-credits state :resPlayer)
    (run-on state "Archives")
    (core/rez state :resPlayer (get-ice state :archives 1))
    (run-continue state)
    (core/rez state :resPlayer (get-ice state :archives 0))
    (is (= 3 (:credit (get-corp))) "Corp has 3 credits after rezzing Eli 1.0")
    (run-continue state)
    (prompt-select :resPlayer (get-ice state :hq 0))
    (is (= 3 (:credit (get-corp))) "Corp not charged for Architects of Tomorrow rez of Eli 1.0")))

(deftest haas-bioroid-asa-group
  ;; Asa Group - don't allow installation of operations
  (do-game
    (new-game
      (make-deck "Asa Group: Security Through Vigilance" [(qty "Pup" 1) (qty "BOOM!" 1) (qty "Urban Renewal" 1)])
      (default-runner))
    (play-from-hand state :resPlayer "Pup" "New remote")
    (prompt-select :resPlayer (find-card "BOOM!" (:hand (get-corp))))
    (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
    (prompt-select :resPlayer (find-card "Urban Renewal" (:hand (get-corp))))
    (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))

(deftest haas-bioroid-engineering-the-future-employee-strike
  ;; EtF - interaction with Employee Strike
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Engineering the Future" [(qty "Eli 1.0" 3) (qty "Paywall Implementation" 1)])
      (default-runner [(qty "Employee Strike" 1)]))
    (take-credits state :resPlayer)
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits at turn end")
    (play-from-hand state :hazPlayer "Employee Strike")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Eli 1.0" "New remote")
    (is (= 8 (:credit (get-corp))) "Corp did not gain 1cr from EtF")
    (play-from-hand state :resPlayer "Paywall Implementation")
    (play-from-hand state :resPlayer "Eli 1.0" "New remote")
    (is (= 8 (:credit (get-corp))) "Corp did not gain 1cr from EtF")
    (take-credits state :resPlayer)
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Eli 1.0" "New remote")
    (is (= 9 (:credit (get-corp))) "Corp gained 1cr from EtF")))

(deftest haas-bioroid-stronger-together
  ;; Stronger Together - +1 strength for Bioroid ice
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Stronger Together" [(qty "Eli 1.0" 1)])
      (default-runner))
    (play-from-hand state :resPlayer "Eli 1.0" "Archives")
    (let [eli (get-ice state :archives 0)]
      (core/rez state :resPlayer eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest iain-stirling-credits
  ;; Iain Stirling - Gain 2 credits when behind
  (do-game
    (new-game
      (default-corp [(qty "Breaking News" 1)])
      (make-deck "Iain Stirling: Retired Spook" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Breaking News" "New remote")
    (let [ag1 (get-in @state [:resPlayer :servers :remote1 :content 0])]
      (core/advance state :resPlayer {:card (refresh ag1)})
      (core/advance state :resPlayer {:card (refresh ag1)})
      (core/score state :resPlayer {:card (refresh ag1)})
      (take-credits state :resPlayer)
      (is (= 1 (:agenda-point (get-corp))) "Corp gains 1 agenda point from Breaking News")
      (take-credits state :hazPlayer 1)
      (is (= 8 (:credit (get-runner))) "Gained 2 credits from being behind on points"))))

(deftest industrial-genomics-trash-cost
  ;; Industrial Genomics - Increase trash cost
  (do-game
    (new-game
      (make-deck "Industrial Genomics: Growing Solutions" [(qty "PAD Campaign" 3)
                                                           (qty "Hedge Fund" 3)])
      (default-runner))
    (play-from-hand state :resPlayer "PAD Campaign" "New remote")
    (trash-from-hand state :resPlayer "PAD Campaign")
    (trash-from-hand state :resPlayer "PAD Campaign")
    (trash-from-hand state :resPlayer "Hedge Fund")
    (trash-from-hand state :resPlayer "Hedge Fund")
    (let [pad (get-content state :remote1 0)]
      (core/rez state :resPlayer pad)
      (take-credits state :resPlayer)
      (run-empty-server state "Server 1")
      (is (= 8 (core/trash-cost state :hazPlayer (refresh pad)))))))

(deftest jemison-astronautics
  ;; Jemison Astronautics - Place advancements when forfeiting agendas
  (do-game
    (new-game
      (make-deck "Jemison Astronautics: Sacrifice. Audacity. Success." [(qty "Enforcer 1.0" 1) (qty "Hostile Takeover" 1)
                                                                        (qty "Ice Wall" 1) (qty "Global Food Initiative" 1)])
      (default-runner [(qty "Data Dealer" 1)]))
    (play-from-hand state :resPlayer "Enforcer 1.0" "HQ")
    (play-from-hand state :resPlayer "Ice Wall" "R&D")
    (play-from-hand state :resPlayer "Hostile Takeover" "New remote")
    (let [enf (get-ice state :hq 0)
          iwall (get-ice state :rd 0)]
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Data Dealer")
      (run-empty-server state "Server 1")
      (prompt-choice :hazPlayer "Steal")
      (let [dd (get-resource state 0)]
        (card-ability state :hazPlayer dd 0)
        (prompt-select :hazPlayer (get-in (get-runner) [:scored 0]))
        (is (empty? (:prompt (get-corp))) "No Jemison prompt for Runner forfeit")
        (take-credits state :hazPlayer)
        (play-from-hand state :resPlayer "Global Food Initiative" "New remote")
        (score-agenda state :resPlayer (get-content state :remote2 0))
        (core/rez state :resPlayer enf)
        (prompt-select :resPlayer (get-in (get-corp) [:scored 0]))
        (prompt-select :resPlayer iwall)
        (is (= 4 (:advance-counter (refresh iwall))) "Jemison placed 4 advancements")))))

(deftest jesminder-sareen-ability
  ;; Jesminder Sareen - avoid tags only during a run
  (do-game
    (new-game (default-corp [(qty "SEA Source" 1) (qty "Data Raven" 1)])
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Data Raven" "Archives")
    (take-credits state :resPlayer)
    (let [dr (-> @state :resPlayer :servers :archives :ices first)]
      (core/rez state :resPlayer dr)
      (core/click-run state :hazPlayer {:server "Archives"})
      (card-ability state :resPlayer dr 0)
      (is (= 0 (:tag (get-runner))) "Jesminder avoided first tag during the run")
      (card-ability state :resPlayer dr 0)
      (is (= 1 (:tag (get-runner))) "Jesminder did not avoid the second tag during the run")
      (core/no-action state :resPlayer nil)
      (core/continue state :hazPlayer nil)
      (core/no-action state :resPlayer nil)
      (core/successful-run state :hazPlayer nil)
      (run-empty-server state "R&D") ; clear per-run buffer
      (take-credits state :hazPlayer)
      (play-from-hand state :resPlayer "SEA Source")
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (= 2 (:tag (get-runner))) "Jesminder did not avoid the tag outside of a run"))))

(deftest jesminder-john-masanori
  ;; Jesminder Sareen - don't avoid John Masanori tag
  (do-game
    (new-game (default-corp)
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "John Masanori" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "John Masanori")
    (run-on state "HQ")
    (core/jack-out state :hazPlayer nil)
    (is (= 1 (:tag (get-runner))) "Jesminder did not avoid John Masanori tag")))

(deftest jinteki-biotech-brewery
  ;; Jinteki Biotech - Brewery net damage
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Braintrust" 1)])
      (default-runner)
      {:dont-start-turn true})
    (prompt-choice :resPlayer "The Brewery")
    (core/start-turn state :resPlayer nil)
    (card-ability state :resPlayer (:identity (get-corp)) 1)
    (is (= 1 (count (:hand (get-runner)))) "Runner took 2 net damage from Brewery flip")))

(deftest jinteki-biotech-greenhouse
  ;; Jinteki Biotech - Greenhouse four advancement tokens
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Braintrust" 1)])
      (default-runner)
      {:dont-start-turn true})
    (prompt-choice :resPlayer "The Greenhouse")
    (core/start-turn state :resPlayer nil)
    (play-from-hand state :resPlayer "Braintrust" "New remote")
    (take-credits state :resPlayer)
    (take-credits state :hazPlayer)
    (let [bt (get-content state :remote1 0)]
      (is (nil? (:advance-counter (refresh bt))) "No advancement counters on agenda")
      (card-ability state :resPlayer (:identity (get-corp)) 1)
      (prompt-select :resPlayer (refresh bt))
      (is (= 4 (:advance-counter (refresh bt))) "Four advancement counters on agenda"))))

(deftest jinteki-biotech-tank
  ;; Jinteki Biotech - Tank shuffle Archives into R&D
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Hedge Fund" 3)])
      (default-runner)
      {:dont-start-turn true})
    (prompt-choice :resPlayer "The Tank")
    (core/start-turn state :resPlayer nil)
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :hazPlayer)
    (is (= 3 (count (:discard (get-corp)))) "Archives started with 3 cards")
    (is (= 0 (count (:deck (get-corp)))) "R&D started empty")
    (card-ability state :resPlayer (:identity (get-corp)) 1)
    (is (= 0 (count (:discard (get-corp)))) "Archives ended empty")
    (is (= 3 (count (:deck (get-corp)))) "R&D ended with 3 cards")))

(deftest jinteki-personal-evolution
  ;; Personal Evolution - Prevent runner from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Personal Evolution" [(qty "Braintrust" 6)])
      (default-runner [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Braintrust" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Steal")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage from steal")))

(deftest jinteki-potential-unleashed
  ;; PU - when the runner takes at least one net damage, mill 1 from their deck
  (do-game
    (new-game (make-deck "Jinteki: Potential Unleashed" [(qty "Philotic Entanglement" 1) (qty "Neural EMP" 1) (qty "Braintrust" 3)])
              (default-runner [(qty "Employee Strike" 10)]))
    (play-from-hand state :resPlayer "Braintrust" "New remote")
    (play-from-hand state :resPlayer "Braintrust" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Steal")
    (run-empty-server state "Server 2")
    (prompt-choice :hazPlayer "Steal")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Philotic Entanglement" "New remote")
    (score-agenda state :resPlayer (get-content state :remote3 0))
    (is (= 3 (count (:discard (get-runner)))))
    (play-from-hand state :resPlayer "Neural EMP")
    (is (= 5 (count (:discard (get-runner)))))))

(deftest jinteki-replicating-perfection
  ;; Replicating Perfection - Prevent runner from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-runner))
    (play-from-hand state :resPlayer "Mental Health Clinic" "New remote")
    (take-credits state :resPlayer)
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
    (run-empty-server state "HQ")
    (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")))

(deftest jinteki-replicating-perfection-employee-strike
  ;; Replicating Perfection - interaction with Employee Strike. Issue #1313 and #1956.
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-runner [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (play-from-hand state :resPlayer "Mental Health Clinic" "New remote")
    (take-credits state :resPlayer)
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
    (play-from-hand state :hazPlayer "Employee Strike")
    (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")
    (play-from-hand state :hazPlayer "Scrubbed")
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")))

(deftest kate-mac-mccaffrey-discount
  ;; Kate 'Mac' McCaffrey - Install discount
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Magnum Opus")
    (is (= 1 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))

(deftest kate-mac-mccaffrey-no-discount
  ;; Kate 'Mac' McCaffrey - No discount for 0 cost
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                         [(qty "Magnum Opus" 1)
                          (qty "Self-modifying Code" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Self-modifying Code")
    (play-from-hand state :hazPlayer "Magnum Opus")
    (is (= 0 (:credit (get-runner))) "No Kate discount on second program install")))

(deftest kate-mac-mccaffrey-discount-cant-afford
  ;; Kate 'Mac' McCaffrey - Can Only Afford With the Discount
  (do-game
    (new-game (default-corp)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :resPlayer)
    (core/lose state :hazPlayer :credit 1)
    (is (= 4 (:credit (get-runner))))
    (play-from-hand state :hazPlayer "Magnum Opus")
    (is (= 1 (count (get-in @state [:hazPlayer :rig :program]))) "Magnum Opus installed")
    (is (= 0 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))

(deftest ken-tenma-run-event-credit
  ;; Ken 'Express' Tenma - Gain 1 credit when first Run event played
  (do-game
    (new-game (default-corp)
              (make-deck "Ken \"Express\" Tenma: Disappeared Clone" [(qty "Account Siphon" 2)]))
    (take-credits state :resPlayer)
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit for first Run event")
    (prompt-choice :hazPlayer "Run ability")
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 16 (:credit (get-runner))) "No credit gained for second Run event")))

(deftest khan-vs-caprice
  ;; Khan - proper order of events when vs. Caprice
  (do-game
    (new-game
      (default-corp [(qty "Eli 1.0" 1) (qty "Caprice Nisei" 1)])
      (make-deck "Khan: Savvy Skiptracer" [(qty "Corroder" 1)]))
    (play-from-hand state :resPlayer "Eli 1.0" "Archives")
    (play-from-hand state :resPlayer "Caprice Nisei" "Archives")
    (core/rez state :resPlayer (get-content state :archives 0))
    (take-credits state :resPlayer)
    (run-on state "Archives")
    (run-continue state)
    (is (and (empty? (:prompt (get-corp)))
             (= 1 (count (:prompt (get-runner))))
             (= "Khan: Savvy Skiptracer" (-> (get-runner) :prompt first :card :title)))
        "Only Khan prompt showing")
    (prompt-select :hazPlayer (first (:hand (get-runner))))
    (is (find-card "Corroder" (-> (get-runner) :rig :program)) "Corroder installed")
    (is (= 4 (:credit (get-runner))) "1cr discount from Khan")
    (is (= "Caprice Nisei" (-> (get-runner) :prompt first :card :title)) "Caprice prompt showing")
    (prompt-choice :hazPlayer "0 [Credits]")
    (prompt-choice :resPlayer "1 [Credits]")
    (is (not (:run @state)) "Run ended")))

(deftest laramy-fisk-shards
  ;; Laramy Fisk - installing a Shard should still give option to force Corp draw.
  (do-game
    (new-game
      (default-corp [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3)])
      (make-deck "Laramy Fisk: Savvy Investor" [(qty "Eden Shard" 1)]))
    (starting-hand state :resPlayer ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Eli 1.0" "Eli 1.0"])
    (take-credits state :resPlayer)
    (run-on state "R&D")
    (core/no-action state :resPlayer nil)
    ;; at Successful Run stage -- click Eden Shard to install
    (play-from-hand state :hazPlayer "Eden Shard")
    (is (= 5 (:credit (get-runner))) "Eden Shard install was free")
    (is (= "Eden Shard" (:title (get-resource state 0))) "Eden Shard installed")
    (is (= "Identity" (-> (get-runner) :prompt first :card :type)) "Fisk prompt showing")
    (prompt-choice :hazPlayer "Yes")
    (is (not (:run @state)) "Run ended")
    (is (= 6 (count (:hand (get-corp)))) "Corp forced to draw")))

(deftest leela-gang-sign-complicated
  ;; Leela Patel - complicated interaction with mutiple Gang Sign
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Project Atlas" 1)
                                                                  (qty "Hostile Takeover" 1)
                                                                  (qty "Geothermal Fracking" 1)])
      (make-deck "Leela Patel: Trained Pragmatist" [(qty "Gang Sign" 2)]))
    (play-from-hand state :resPlayer "Project Atlas" "New remote")
    (play-from-hand state :resPlayer "Hostile Takeover" "New remote")
    (play-from-hand state :resPlayer "Geothermal Fracking" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Gang Sign")
    (play-from-hand state :hazPlayer "Gang Sign")
    (take-credits state :hazPlayer)
    (score-agenda state :resPlayer (get-content state :remote1 0))
    (prompt-choice :hazPlayer "Leela Patel: Trained Pragmatist")
    (prompt-select :hazPlayer (get-content state :remote2 0))
    (is (find-card "Hostile Takeover" (:hand (get-corp))) "Hostile Takeover returned to hand")
    (prompt-choice :hazPlayer "Gang Sign")
    (prompt-choice :hazPlayer "Card from hand")
    (prompt-choice :hazPlayer "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-runner))) "Hostile Takeover stolen with Gang Sign")
    (prompt-select :hazPlayer (get-content state :remote3 0))
    (is (find-card "Geothermal Fracking" (:hand (get-corp))) "Geothermal Fracking returned to hand")
    (prompt-choice :hazPlayer "Card from hand")
    (prompt-choice :hazPlayer "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-runner))) "Geothermal Fracking stolen with Gang Sign")
    (prompt-choice :hazPlayer "Done")))

(deftest leela-lingering-successful-run-prompt
  ;; Leela Patel - issues with lingering successful run prompt
  (do-game
    (new-game
      (make-deck "NBN: Making News" [(qty "Breaking News" 1) (qty "SanSan City Grid" 1)])
      (make-deck "Leela Patel: Trained Pragmatist" []))
    (starting-hand state :resPlayer ["SanSan City Grid"])
    (play-from-hand state :resPlayer "SanSan City Grid" "New remote")
    (take-credits state :resPlayer)
    (run-empty-server state :rd)
    (prompt-choice :hazPlayer "Steal")
    (prompt-select :hazPlayer (get-content state :remote1 0))
    (is (not (:run @state)) "Run is over")))

(deftest leela-upgrades
  ;; Leela Patel - upgrades returned to hand in the middle of a run do not break the run. Issue #2008.
  (do-game
    (new-game (default-corp [(qty "Crisium Grid" 3) (qty "Project Atlas" 3) (qty "Shock!" 1)])
              (make-deck "Leela Patel: Trained Pragmatist" [(qty "Sure Gamble" 1)]))
    (starting-hand state :resPlayer ["Crisium Grid" "Crisium Grid" "Crisium Grid" "Project Atlas" "Shock!" "Project Atlas"])
    (play-from-hand state :resPlayer "Crisium Grid" "HQ")
    (play-from-hand state :resPlayer "Crisium Grid" "Archives")
    (play-from-hand state :resPlayer "Crisium Grid" "R&D")
    (trash-from-hand state :resPlayer "Project Atlas")
    (trash-from-hand state :resPlayer "Shock!")
    (take-credits state :resPlayer)
    (run-empty-server state "HQ")
    (prompt-choice :hazPlayer "Card from hand")
    (prompt-choice :hazPlayer "Steal")
    (prompt-select :hazPlayer (get-content state :hq 0))
    (is (not (get-content state :hq 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "R&D")
    (prompt-choice :hazPlayer "Card from deck")
    (prompt-choice :hazPlayer "Steal")
    (prompt-select :hazPlayer (get-content state :rd 0))
    (is (not (get-content state :rd 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "Archives")
    (prompt-choice :hazPlayer "Shock!")
    (prompt-choice :hazPlayer "Project Atlas")
    (prompt-choice :hazPlayer "Steal")
    (prompt-select :hazPlayer (get-content state :archives 0))
    (is (not (get-content state :archives 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")))

(deftest maxx
  (do-game
    (new-game (default-corp)
              (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                    (qty "Eater" 1)]))
    (starting-hand state :hazPlayer ["Eater"])
    (take-credits state :resPlayer)
    (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
    (is (last-log-contains? state "Wyldside, Wyldside")
        "Maxx did log trashed card names")))

(deftest maxx-wyldside-start-of-turn
  ;; MaxX and Wyldside - using Wyldside during Step 1.2 should lose 1 click
  (do-game
    (new-game (default-corp)
              (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                     (qty "Sure Gamble" 3)
                                                     (qty "Infiltration" 3)
                                                     (qty "Corroder" 3)
                                                     (qty "Eater" 3)]))
    (take-credits state :resPlayer)
    (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
    (starting-hand state :hazPlayer ["Wyldside"])
    (play-from-hand state :hazPlayer "Wyldside")
    (take-credits state :hazPlayer 3)
    (is (= 5 (:credit (get-runner))) "Runner has 5 credits at end of first turn")
    (is (find-card "Wyldside" (get-in @state [:hazPlayer :rig :resource])) "Wyldside was installed")
    (take-credits state :resPlayer)
    (is (= 0 (:click (get-runner))) "Runner has 0 clicks")
    (is (:hazPlayer-phase-12 @state) "Runner is in Step 1.2")
    (let [maxx (get-in @state [:hazPlayer :identity])
          wyld (find-card "Wyldside" (get-in @state [:hazPlayer :rig :resource]))]
      (card-ability state :hazPlayer maxx 0)
      (card-ability state :hazPlayer wyld 0)
      (core/end-phase-12 state :hazPlayer nil)
      (is (= 4 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (is (= 3 (:click (get-runner))) "Wyldside caused 1 click to be lost")
      (is (= 3 (count (:hand (get-runner)))) "3 cards drawn total"))))

(deftest nasir-ability-basic
  ;; Nasir Ability - Basic
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Nasir Meidan: Cyber Explorer" []))
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (take-credits state :resPlayer)

    (run-on state "HQ")
    (let [iwall (get-ice state :hq 0)
          nasir (get-in @state [:hazPlayer :identity])]
      (core/rez state :resPlayer iwall)
      (is (= 5 (:credit (get-runner))) "Nasir Ability does not trigger automatically")
      (card-ability state :hazPlayer nasir 0)
      (is (= 1 (:credit (get-runner))) "Credits at 1 after Nasir ability trigger"))))

(deftest nasir-ability-xanadu
  ;; Nasir Ability - Xanadu
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 1)])
      (make-deck "Nasir Meidan: Cyber Explorer" [(qty "Xanadu" 1)]))
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (take-credits state :resPlayer)

    (swap! state assoc-in [:hazPlayer :credit] 6)
    (play-from-hand state :hazPlayer "Xanadu")
    (run-on state "HQ")
    (let [iwall (get-in @state [:resPlayer :servers :hq :ices 0])
          nasir (get-in @state [:hazPlayer :identity])]
      (core/rez state :resPlayer iwall)
      (is (= 3 (:credit (get-runner))) "Pay 3 to install Xanadu")
      (card-ability state :hazPlayer nasir 0)
      (is (= 2 (:credit (get-runner))) "Gain 1 more credit due to Xanadu"))))

(deftest nbn-controlling-the-message
  ;; NBN: Controlling the Message - Trace to tag Runner when first installed Corp card is trashed
  (do-game
    (new-game
      (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 3)])
      (default-runner [(qty "Forger" 1)]))
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Forger")
    ; trash from HQ first - #2321
    (run-empty-server state "HQ")
    (prompt-choice :hazPlayer "Yes")
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Yes")
    (prompt-choice :resPlayer "Yes")
    (prompt-choice :resPlayer 0)
    (prompt-choice :hazPlayer 0)
    (is (empty? (:prompt (get-runner))) "Forger can't avoid the tag")
    (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
    (run-empty-server state "Server 2")
    (prompt-choice :hazPlayer "Yes")
    (is (empty? (:prompt (get-corp))) "No trace chance on 2nd trashed card of turn")))

(deftest nbn-controlling-the-message-drt
  ;; NBN: Controlling the Message - Interaction with Dedicated Response Team
  (do-game
    (new-game
      (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 1) (qty "Dedicated Response Team" 1)])
      (default-runner))
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (play-from-hand state :resPlayer "Dedicated Response Team" "New remote")
    (core/rez state :resPlayer (get-content state :remote2 0))
    (take-credits state :resPlayer)
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Yes")
    (prompt-choice :resPlayer "Yes")
    (prompt-choice :resPlayer 0)
    (prompt-choice :hazPlayer 0)
    (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
    (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from DRT")))

(deftest new-angeles-sol-on-steal
  ;; New Angeles Sol - interaction with runner stealing agendas
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Paywall Implementation" 2) (qty "Breaking News" 1)])
      (default-runner))
    (play-from-hand state :resPlayer "Breaking News" "New remote")
    (play-from-hand state :resPlayer "Paywall Implementation")
    (take-credits state :resPlayer)
    (is (= 6 (:credit (get-corp))))
    (run-empty-server state :remote1)
    (is (= 7 (:credit (get-corp))) "Corp gained 1cr from successful run")
    (prompt-choice :hazPlayer "Steal")
    (prompt-choice :resPlayer "Yes")
    (is (find-card "Paywall Implementation" (:discard (get-corp))) "Paywall trashed before Sol triggers")
    (prompt-select :resPlayer (find-card "Paywall Implementation" (:hand (get-corp))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Paywall Implementation" (:current (get-corp))) "Paywall back in play")))

(deftest nisei-division
  ;; Nisei Division - Gain 1 credit from every psi game
  (do-game
    (new-game
      (make-deck "Nisei Division: The Next Generation" [(qty "Snowflake" 2)])
      (default-runner))
    (play-from-hand state :resPlayer "Snowflake" "HQ")
    (play-from-hand state :resPlayer "Snowflake" "HQ")
    (take-credits state :resPlayer)
    (let [s1 (get-in @state [:resPlayer :servers :hq :ices 0])
          s2 (get-in @state [:resPlayer :servers :hq :ices 1])]
      (run-on state "HQ")
      (core/rez state :resPlayer s2)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :resPlayer s2 0)
      (prompt-choice :resPlayer "0 [Credits]")
      (prompt-choice :hazPlayer "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game")
      (core/no-action state :resPlayer nil)
      (core/rez state :resPlayer s1)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :resPlayer s1 0)
      (prompt-choice :resPlayer "0 [Credits]")
      (prompt-choice :hazPlayer "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game"))))

(deftest noise-ability
  ;; Noise: Hacker Extraordinaire - Ability
  (do-game
    (new-game
      (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)])
      (make-deck "Noise: Hacker Extraordinaire" [(qty "Datasucker" 1) (qty "Cache" 1) (qty "Sure Gamble" 1) (qty "Clone Chip" 2) (qty "Sharpshooter" 2)]))
    (starting-hand state :hazPlayer ["Datasucker" "Sure Gamble" "Clone Chip" "Clone Chip" "Cache"])
    (is (= 6 (count (:hand (get-corp)))) "Corp should start with 6 cards in hand")
    (is (= 5 (count (:deck (get-corp)))) "Corp deck should contain 5 cards")
    (take-credits state :resPlayer)
    (is (= 0 (count (:discard (get-corp)))) "Archives started empty")
    (play-from-hand state :hazPlayer "Datasucker")
    (is (= 1 (count (:discard (get-corp)))) "Playing virus should cause card to be trashed from R&D")
    (is (= 4 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    (play-from-hand state :hazPlayer "Sure Gamble")
    (is (= 1 (count (:discard (get-corp)))) "Playing non-virus should not cause card to be trashed from R&D")
    (core/click-draw state :hazPlayer nil)
    (play-from-hand state :hazPlayer "Clone Chip")
    (play-from-hand state :hazPlayer "Clone Chip")
    (trash-from-hand state :hazPlayer "Cache")
    (trash-from-hand state :hazPlayer "Sharpshooter")
    (take-credits state :hazPlayer)
    ;; playing virus via Clone Chip on Corp's turn should trigger Noise ability
    (let [chip (get-in @state [:hazPlayer :rig :hardware 0])]
      (card-ability state :hazPlayer chip 0)
      (prompt-select :hazPlayer (find-card "Cache" (:discard (get-runner))))
      (let [ds (get-in @state [:hazPlayer :rig :program 1])]
        (is (not (nil? ds)))
        (is (= (:title ds) "Cache"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing virus via Clone Chip on corp's turn should trigger Noise ability")
    (is (= 2 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    ;; playing non-virus via Clone Chip on Corp's turn should NOT trigger Noise ability
    (let [chip-2 (get-in @state [:hazPlayer :rig :hardware 0])]
      (card-ability state :hazPlayer chip-2 0)
      (prompt-select :hazPlayer (find-card "Sharpshooter" (:discard (get-runner))))
      (let [ss (get-in @state [:hazPlayer :rig :program 2])]
        (is (not (nil? ss)))
        (is (= (:title ss) "Sharpshooter"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing non-virus via Clone Chip on corp's turn should not trigger Noise ability")))

(deftest null-ability
  ;; Null ability - once per turn
  (do-game
    (new-game
      (default-corp [(qty "Wraparound" 3)])
      (make-deck "Null: Whistleblower" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Wraparound" "HQ")
    (play-from-hand state :resPlayer "Wraparound" "HQ")
    (take-credits state :resPlayer)
    (run-on state "HQ")
    (let [null (get-in @state [:hazPlayer :identity])
          wrap1 (get-ice state :hq 0)
          wrap2 (get-ice state :hq 1)]
      (card-ability state :hazPlayer null 0)
      (is (empty? (:prompt (get-runner))) "Ability won't work on unrezzed ICE")
      (core/rez state :resPlayer wrap2)
      (card-ability state :hazPlayer null 0)
      (prompt-select :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))))
      (is (= 5 (:current-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
      (run-continue state)
      (core/rez state :resPlayer wrap1)
      (card-ability state :hazPlayer null 0)
      (is (empty? (:prompt (get-runner))) "Ability already used this turn")
      (run-jack-out state)
      (is (= 7 (:current-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))

(deftest null-trashed
  ;; Null ability - does not affect next ice when current is trashed. Issue #1788.
  (do-game
    (new-game
      (default-corp [(qty "Wraparound" 1) (qty "Spiderweb" 1)])
      (make-deck "Null: Whistleblower" [(qty "Parasite" 3)]))
    (play-from-hand state :resPlayer "Spiderweb" "HQ")
    (play-from-hand state :resPlayer "Wraparound" "HQ")
    (take-credits state :resPlayer)
    (core/gain state :resPlayer :credit 10)
    (let [null (get-in @state [:hazPlayer :identity])
          spider (get-ice state :hq 0)
          wrap (get-ice state :hq 1)]
      (core/rez state :resPlayer spider)
      (core/rez state :resPlayer wrap)
      (play-from-hand state :hazPlayer "Parasite")
      (prompt-select :hazPlayer (refresh spider))
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :hazPlayer null 0)
      (prompt-select :hazPlayer (first (:hand (get-runner))))
      (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Null")
      (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Null"))))

(deftest omar-ability
  ;; Omar Keung - Make a successful run on the chosen server once per turn
  (do-game
    (new-game
      (default-corp)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (take-credits state :resPlayer)
    (let [omar (get-in @state [:hazPlayer :identity])]
      (card-ability state :hazPlayer omar 0)
      (run-successful state)
      (prompt-choice :hazPlayer "HQ")
      (is (= [:hq] (-> (get-runner) :register :successful-run)))
      (is (= "You accessed Hedge Fund" (-> (get-runner) :prompt first :msg)))
      (prompt-choice :hazPlayer "OK")
      (is (= 3 (:click (get-runner))))
      (card-ability state :hazPlayer omar 0)
      (is (= 3 (:click (get-runner))))
      (take-credits state :hazPlayer)
      (take-credits state :resPlayer)
      (run-empty-server state :rd)
      (is (= [:rd] (-> (get-runner) :register :successful-run)))
      (card-ability state :hazPlayer omar 0)
      (run-successful state)
      (prompt-choice :hazPlayer "HQ")
      (is (= [:hq :rd] (-> (get-runner) :register :successful-run))))))

(deftest omar-ash
  ;; Omar Keung - Ash prevents access, but not successful run
  (do-game
    (new-game
      (default-corp [(qty "Ash 2X3ZB9CY" 1)])
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Ash 2X3ZB9CY" "HQ")
    (take-credits state :resPlayer)
    (let [omar (get-in @state [:hazPlayer :identity])
          ash (get-content state :hq 0)]
      (core/rez state :resPlayer ash)
      (card-ability state :hazPlayer omar 0)
      (run-successful state)
      (prompt-choice :hazPlayer "HQ")
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (= (:cid ash) (-> (get-runner) :prompt first :card :cid)))
      (is (= :hq (-> (get-runner) :register :successful-run first))))))

(deftest omar-crisium-grid
  ;; Omar Keung - Crisium Grid prevents prompt
  (do-game
    (new-game
      (default-corp [(qty "Crisium Grid" 1)])
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Crisium Grid" "Archives")
    (take-credits state :resPlayer)
    (let [omar (get-in @state [:hazPlayer :identity])
          cr (get-content state :archives 0)]
      (core/rez state :resPlayer cr)
      (card-ability state :hazPlayer omar 0)
      (run-successful state)
      (is (= (:cid cr) (-> (get-runner) :prompt first :card :cid)))
      (is (empty? (-> (get-runner) :register :successful-run)))
      (is (= :archives (get-in @state [:run :server 0]))))))

(deftest omar-medium
  ;; Omar Keung - When selecting R&D, ability adds counters to Medium
  (do-game
    (new-game
      (default-corp)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Medium" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Medium")
    (let [omar (get-in @state [:hazPlayer :identity])
          medium (get-in @state [:hazPlayer :rig :program 0])]
      (card-ability state :hazPlayer omar 0)
      (run-successful state)
      (prompt-choice :hazPlayer "R&D")
      (is (= 1 (get-counters (refresh medium) :virus))))))

(deftest omar-nerve-agent
  ;; Omar Keung - When selecting HQ, ability adds counters to Nerve Agent
  (do-game
    (new-game
      (default-corp)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Nerve Agent" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Nerve Agent")
    (let [omar (get-in @state [:hazPlayer :identity])
          nerve (get-in @state [:hazPlayer :rig :program 0])]
      (card-ability state :hazPlayer omar 0)
      (run-successful state)
      (prompt-choice :hazPlayer "HQ")
      (is (= 1 (get-counters (refresh nerve) :virus))))))

(deftest quetzal-ability
  ;; Quetzal ability- once per turn
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Quetzal: Free Spirit" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (take-credits state :resPlayer)
    (run-on state "HQ")
    (let [q (get-in @state [:hazPlayer :identity])
          iwall (get-ice state :hq 0)
          qdef (core/card-def (get-in @state [:hazPlayer :identity]))]
      (core/rez state :resPlayer iwall)
      (card-ability state :hazPlayer q 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg]))
          "Quetzal ability did trigger")
      (run-jack-out state)
      (core/click-credit state :hazPlayer nil)
      (run-on state "HQ")
      (card-ability state :hazPlayer (refresh q) 0)
      (is (not (last-log-contains? state (get-in qdef [:abilities 0 :msg])))
          "Quetzal ability did not trigger")
      (run-jack-out state)
      (take-credits state :hazPlayer)
      (take-credits state :resPlayer)
      (core/click-credit state :hazPlayer nil)
      (run-on state "HQ")
      (card-ability state :hazPlayer (refresh q) 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg]))
          "Quetzal ability did trigger")
      (core/jack-out state :hazPlayer nil))))

(deftest reina-rez-cost-increase
  ;; Reina Roja - Increase cost of first rezzed ICE
  (do-game
    (new-game
      (default-corp [(qty "Quandary" 3)])
      (make-deck "Reina Roja: Freedom Fighter" []))
    (play-from-hand state :resPlayer "Quandary" "R&D")
    (take-credits state :resPlayer)
    (is (= 7 (:credit (get-corp))))
    (run-on state "R&D")
    (let [quan (get-ice state :rd 0)]
      (core/rez state :resPlayer quan)
      (is (= 5 (:credit (get-corp))) "Rez cost increased by 1"))))

(deftest rielle-kit-peddler-ability
  ;; Rielle "Kit" Peddler - Give ICE Code Gate
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 2)])
              (make-deck "Rielle \"Kit\" Peddler: Transhuman" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Ice Wall" "HQ")
    (take-credits state :resPlayer)
    (run-on state "HQ")
    (let [k (get-in @state [:hazPlayer :identity])
          iwall (get-ice state :hq 0)]
      (core/rez state :resPlayer iwall)
      (card-ability state :hazPlayer k 0)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate"))))

(deftest skorpios
  ; Remove a card from game when it moves to discard once per round
  (do-game
    (new-game (make-deck "Skorpios Defense Systems: Persuasive Power" [(qty "Hedge Fund" 1) (qty "Quandary" 4)])
              (default-runner [(qty "The Maker's Eye" 1) (qty "Lucky Find" 1)]))
    (play-from-hand state :resPlayer "Hedge Fund")
    (dotimes [_ 4] (core/move state :resPlayer (first (:hand (get-corp))) :deck))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Lucky Find")
    (play-from-hand state :hazPlayer "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    ; Don't allow a run-event in progress to be targeted #2963
    (card-ability state :resPlayer (get-in @state [:resPlayer :identity]) 0)
    (is (empty? (filter #(= "The Maker's Eye" (:title %)) (-> (get-corp) :prompt first :choices))) "No Maker's Eye choice")
    (prompt-choice :resPlayer "Cancel")
    (run-successful state)
    (prompt-choice :hazPlayer "Card from deck")
    (is (= "You accessed Quandary" (-> (get-runner) :prompt first :msg)) "1st quandary")
    (prompt-choice :hazPlayer "OK")
    (prompt-choice :hazPlayer "Card from deck")
    (is (= "You accessed Quandary" (-> (get-runner) :prompt first :msg)) "2nd quandary")
    (prompt-choice :hazPlayer "OK")
    (prompt-choice :hazPlayer "Card from deck")
    (is (= "You accessed Quandary" (-> (get-runner) :prompt first :msg)) "3rd quandary")
    (prompt-choice :hazPlayer "OK")
    (is (not (:run @state)))
    (card-ability state :resPlayer (get-in @state [:resPlayer :identity]) 0)
    (prompt-choice :resPlayer (find-card "The Maker's Eye" (:discard (get-runner))))
    (is (= 1 (count (get-in @state [:hazPlayer :rfg]))) "One card RFGed")
    (card-ability state :resPlayer (get-in @state [:resPlayer :identity]) 0)
    (is (empty? (:prompt (get-corp))) "Cannot use Skorpios twice")))

(deftest silhouette-expose-trigger-before-access
  ;; Silhouette - Expose trigger ability resolves completely before access. Issue #2173.
  (do-game
    (new-game
      (default-corp [(qty "Psychic Field" 1) (qty "Fetal AI" 10)])
      (make-deck "Silhouette: Stealth Operative" [(qty "Feedback Filter" 1) (qty "Inside Job" 1)]))
    (starting-hand state :resPlayer ["Psychic Field" "Fetal AI"])
    (play-from-hand state :resPlayer "Psychic Field" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Feedback Filter")
    (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
    (let [psychic (get-content state :remote1 0)
          ff (get-hardware state 0)]
      (run-empty-server state :hq)
      (is (:run @state) "On successful run trigger effects")
      (prompt-select :hazPlayer psychic)
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
      (prompt-choice :resPlayer "2 [Credits]")
      (prompt-choice :hazPlayer "0 [Credits]")
      (card-ability state :hazPlayer ff 0)
      (prompt-choice :hazPlayer "Done")
      (is (= 0 (:credit (get-runner))) "Runner has no more credits left")
      (is (= 1 (count (:hand (get-runner)))) "Prevented 1 net damage")
      (is (empty? (:discard (get-runner))) "No cards discarded")
      (is (:run @state) "On run access phase")
      (prompt-choice :hazPlayer "Access")
      (prompt-choice :hazPlayer "Done")
      (is (empty? (:hand (get-runner))) "Suffered 1 net damage due to accessing Fetal AI")
      (is (= 1 (count (:discard (get-runner)))) "Discarded 1 card due to net damage")
      (is (:run @state) "Resolving access triggers")
      (prompt-choice :hazPlayer "Yes")
      (is (= 0 (count (:scored (get-runner)))) "Runner has no credits to be able to steal Fetal AI")
      (is (not (:run @state)) "Run has now ended")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest silhouette-temujin-weirdness
  ;; Silhouette - broken interaction with other successful-run triggers. Issue #1968.
  (do-game
    (new-game
      (default-corp [(qty "PAD Campaign" 1) (qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "Beanstalk Royalties" 3)])
      (make-deck "Silhouette: Stealth Operative" [(qty "Temüjin Contract" 1) (qty "Desperado" 1)]))
    (starting-hand state :resPlayer ["Hedge Fund" "PAD Campaign"])
    (play-from-hand state :resPlayer "PAD Campaign" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Temüjin Contract")
    (prompt-choice :hazPlayer "HQ")
    (take-credits state :hazPlayer)
    (take-credits state :resPlayer)
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "Temüjin Contract")
    (prompt-select :hazPlayer (get-content state :remote1 0))
    (prompt-choice :hazPlayer "OK")
    (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
    (is (= 16 (get-counters (get-resource state 0) :credit)) "16 cr on Temujin")
    (is (= 8 (:credit (get-runner))) "Gained 4cr")

    ;; second run
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "OK")
    (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
    (is (= 12 (:credit (get-runner))) "Gained 4cr")
    (is (= 12 (get-counters (get-resource state 0) :credit)) "12 cr on Temujin")))

(deftest spark-advertisements
  ;; Spark Agency - Rezzing advertisements
  (do-game
    (new-game
      (make-deck "Spark Agency: Worldswide Reach" [(qty "Launch Campaign" 3)])
      (default-runner))
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (let [lc1 (get-content state :remote1 0)
          lc2 (get-content state :remote2 0)
          lc3 (get-content state :remote3 0)]
      (core/rez state :resPlayer lc1)
      (is (= 4 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Corp turn)")
      (core/rez state :resPlayer lc3)
      (is (= 4 (:credit (get-runner)))
          "Runner did not lose credit from second Spark rez")
      (take-credits state :resPlayer)
      (run-on state "Server 1")
      (core/rez state :resPlayer lc2)
      (is (= 3 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Runner turn)"))))

(deftest strategic-innovations-future-forward
  ;; Strategic Innovations: Future Forward - Ability
  (do-game
    (new-game
      (make-deck "Strategic Innovations: Future Forward" [(qty "Hedge Fund" 2)
                                                          (qty "Eli 1.0" 2)
                                                          (qty "Crick" 2)])
      (default-runner))
    (play-from-hand state :resPlayer "Eli 1.0" "New remote")
    (play-from-hand state :resPlayer "Hedge Fund")
    (play-from-hand state :resPlayer "Crick" "New remote")
    (let [i1 (get-ice state :remote1 0)
          i2 (get-ice state :remote2 0)]
      (take-credits state :resPlayer 0)
      (take-credits state :hazPlayer)
      (core/rez state :resPlayer i1)
      (take-credits state :resPlayer)
      (take-credits state :hazPlayer)
      (is (= 1 (count (:prompt (get-corp)))) "Corp prompted to trigger Strategic Innovations")
      (prompt-select :resPlayer (first (:discard (get-corp))))
      (is (empty? (:discard (get-corp))) "Hedge Fund moved back to R&D")
      (take-credits state :resPlayer)
      (core/rez state :resPlayer i2)
      (take-credits state :hazPlayer)
      (is (= 0 (count (:prompt (get-corp))))
          "Corp not prompted to trigger Strategic Innovations"))))

(deftest the-foundry-abt
  ;; The Foundry - interaction with Accelerated Beta Test
  (do-game
    (new-game
      (make-deck "The Foundry: Refining the Process" [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)])
      (default-runner))
    (starting-hand state :resPlayer ["Accelerated Beta Test"])
    (play-from-hand state :resPlayer "Accelerated Beta Test" "New remote")
    (score-agenda state :resPlayer (get-content state :remote1 0))
    (prompt-choice :resPlayer "Yes")
    (prompt-select :resPlayer (find-card "Eli 1.0" (:play-area (get-corp))))
    (prompt-choice :resPlayer "Archives")
    (prompt-choice :resPlayer "Yes")
    (is (empty? (:play-area (get-corp))) "Play area shuffled into R&D")))

(deftest titan-agenda-counter
  ;; Titan Transnational - Add a counter to a scored agenda
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Project Atlas" 1)])
      (default-runner))
    (play-from-hand state :resPlayer "Project Atlas" "New remote")
    (let [atl (get-content state :remote1 0)]
      (core/gain state :resPlayer :click 1)
      (core/advance state :resPlayer {:card (refresh atl)})
      (core/advance state :resPlayer {:card (refresh atl)})
      (core/advance state :resPlayer {:card (refresh atl)})
      (core/score state :resPlayer {:card (refresh atl)})
      (let [scored (get-in @state [:resPlayer :scored 0])]
        (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))

(deftest titan-corporate-sales-team
  ;; Titan, only use one counter of Corporate Sales Team
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Corporate Sales Team" 1) (qty "Mark Yale" 1)])
      (default-runner))
    (play-from-hand state :resPlayer "Corporate Sales Team" "New remote")
    (play-from-hand state :resPlayer "Mark Yale" "New remote")
    (let [cst (get-content state :remote1 0)
          my (get-content state :remote2 0)]
      (core/gain state :resPlayer :click 3)
      (core/advance state :resPlayer {:card (refresh cst)})
      (core/advance state :resPlayer {:card (refresh cst)})
      (core/advance state :resPlayer {:card (refresh cst)})
      (core/advance state :resPlayer {:card (refresh cst)})
      (core/score state :resPlayer {:card (refresh cst)})
      (let [scored (get-in @state [:resPlayer :scored 0])]
        (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
        (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
        (core/rez state :resPlayer my)
        (card-ability state :resPlayer my 1)
        (prompt-select :resPlayer (refresh scored))
        (is (= 0 (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
        (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
        (card-ability state :resPlayer my 1)
        (prompt-select :resPlayer (refresh scored))
        (is (= 0 (get-counters (refresh scored) :agenda)) "No agenda counter used by Mark Yale")
        (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")))))

(deftest weyland-builder
  ;; Builder of Nations - 1 meat damage per turn at most
  (do-game
    (new-game
      (make-deck "Weyland Consortium: Builder of Nations" [(qty "Hedge Fund" 3)])
      (default-runner))
      (let [bon (get-in @state [:resPlayer :identity])]
        (card-ability state :resPlayer bon 0)
        (prompt-choice :resPlayer "Cancel")
        (is (= 0 (count (:discard (get-runner)))) "Runner took no meat damage from BoN")
        (card-ability state :resPlayer bon 0)
        (prompt-choice :resPlayer "Yes")
        (is (= 1 (count (:discard (get-runner)))) "Runner took 1 meat damage from BoN")
        (card-ability state :resPlayer bon 0)
        (is (= 1 (count (:discard (get-runner)))) "Runner took only 1 meat damage from BoN total")
        (is (= 0 (count (:prompt (get-corp))))))))

(deftest weyland-builder-cleaners
  ;; Builder of Nations - 2 meat damage from ID ability when The Cleaners is scored
  (do-game
    (new-game
      (make-deck "Weyland Consortium: Builder of Nations" [(qty "The Cleaners" 3) (qty "Ice Wall" 3)])
      (default-runner [(qty "Sure Gamble" 2)]))
    (play-from-hand state :resPlayer "The Cleaners" "New remote")
    (let [clean (get-content state :remote1 0)]
      (score-agenda state :resPlayer clean)
    (let [bon (get-in @state [:resPlayer :identity])]
      (card-ability state :resPlayer bon 0)
      (prompt-choice :resPlayer "Yes")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from BoN/Cleaners combo")))))

(deftest whizzard
  ;; Whizzard - Recurring credits
  (do-game
    (new-game (default-corp) (make-deck "Whizzard: Master Gamer" ["Sure Gamble"]))

    (let [click-whizzard (fn [n] (dotimes [i n] (card-ability state :hazPlayer (:identity (get-runner)) 0)))]
      (is (changes-credits (get-runner) 1 (click-whizzard 1)))
      (is (changes-credits (get-runner) 2 (click-whizzard 5)) "Can't take more than 3 Whizzard credits")

      (take-credits state :resPlayer)
      (is (changes-credits (get-runner) 3 (click-whizzard 3)) "Credits reset at start of Runner's turn")

      (take-credits state :hazPlayer)
      (is (changes-credits (get-runner) 0 (click-whizzard 1)) "Credits don't reset at start of Corp's turn"))))

(deftest wyvern-chemically-enhanced
  ;; Wyvern: Chemically Enhanced - Ability
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 3)])
              (make-deck "Wyvern: Chemically Enhanced" [(qty "Sure Gamble" 2)
                                                        (qty "Corroder" 1)
                                                        (qty "Clone Chip" 1)
                                                        (qty "Easy Mark" 1)]))
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (play-from-hand state :resPlayer "Launch Campaign" "New remote")
    (take-credits state :resPlayer)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :hazPlayer "Sure Gamble")
    (play-from-hand state :hazPlayer "Easy Mark")
    (play-from-hand state :hazPlayer "Corroder")
    (run-empty-server state "Server 1")
    (prompt-choice :hazPlayer "Yes")
    ;; trash Launch Campaign, should trigger wyvern
    (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
        "Sure Gamble still in Wyvern's discard")
    (is (some #(= "Easy Mark" (:title %)) (:deck (get-runner))) "Easy Mark moved to deck")
    (take-credits state :hazPlayer)
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Clone Chip")
    (run-empty-server state "Server 2")
    (prompt-choice :hazPlayer "Yes")
    (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
        "Sure Gamble still in Wyvern's discard")))
