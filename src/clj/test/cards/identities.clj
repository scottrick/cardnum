(ns test.cards.identities
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adam-directives
  ;; Adam - Allow challenger to choose directives
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (is (= 4 (count (get-in @state [:challenger :play-area]))) "All directives are in the challenger's play area")
    (is (= 0 (count (get-in @state [:challenger :hand]))))
    (prompt-select :challenger (find-card "Neutralize All Threats" (get-in @state [:challenger :play-area])))
    (prompt-select :challenger (find-card "Safety First" (get-in @state [:challenger :play-area])))
    (prompt-select :challenger (find-card "Always Be Running" (get-in @state [:challenger :play-area])))
    (is (= 3 (count (get-in @state [:challenger :rig :resource]))) "3 directives were installed")
    (is (= 0 (count (get-in @state [:challenger :play-area]))) "The play area is empty")
    (let [nat (find-card "Neutralize All Threats" (get-in @state [:challenger :rig :resource]))
          sf (find-card "Safety First" (get-in @state [:challenger :rig :resource]))
          abr (find-card "Always Be Running" (get-in @state [:challenger :rig :resource]))]
      (is (and nat sf abr) "The chosen directives were installed"))))

(deftest adam-palana
  ;; Adam - Directives should not grant Pālanā credits.
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

(deftest adam-advanceable-traps
  ;; Adam - Neutralize All Threats interaction with advanceable traps.
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

    (play-from-hand state :contestant "Cerebral Overwriter" "New remote")
    (advance state (get-content state :remote1 0) 2)
    (take-credits state :contestant)
    (run-empty-server state :remote1)
    (prompt-choice :challenger "No") ; Dismiss prompt from non-exiled Find the Truth directive
    (prompt-choice :contestant "Yes")
    (is (= 2 (:brain-damage (get-challenger))) "Challenger took 2 brain damage")
    (is (= 1 (count (:discard (get-contestant)))) "1 card in archives")))

(deftest andromeda
  ;; Andromeda - 9 card starting hand, 1 link
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 1 (:link (get-challenger))) "1 link")
    (is (= 9 (count (:hand (get-challenger)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-mulligan
  ;; Andromeda - 9 card starting hand after mulligan
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)])
      {:mulligan :challenger})
    (is (= 1 (:link (get-challenger))) "1 link")
    (is (= 9 (count (:hand (get-challenger)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-palana
  ;; Andromeda - should not grant Palana credits.
  (do-game
    (new-game
      (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 5 (:credit (get-contestant))) "Palana does not gain credit from Andromeda's starting hand")))

(deftest apex-facedown-console
  ;; Apex - Allow facedown install of a second console. Issue #1326
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Apex: Invasive Predator" [(qty "Heartbeat" 2)]))
    (take-credits state :contestant)
    (core/end-phase-12 state :challenger nil)
    (prompt-choice :challenger "Done") ; no facedown install on turn 1
    (play-from-hand state :challenger "Heartbeat")
    (is (= 1 (count (get-in @state [:challenger :rig :hardware]))))
    (take-credits state :challenger)
    (take-credits state :contestant)
    (core/end-phase-12 state :challenger nil)
    (prompt-select :challenger (find-card "Heartbeat" (:hand (get-challenger))))
    (is (= 1 (count (get-in @state [:challenger :rig :facedown]))) "2nd console installed facedown")))

(deftest ayla
  ;; Ayla - choose & use cards for NVRAM
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Ayla \"Bios\" Rahim: Simulant Specialist" [(qty "Sure Gamble" 1) (qty "Desperado" 1)
                                                             (qty "Security Testing" 1) (qty "Bank Job" 1)
                                                             (qty "Heartbeat" 1) (qty "Eater" 1)])
      {:dont-start-game true})
    (is (= 6 (count (get-in @state [:challenger :play-area]))) "Deck cards are in play area")
    (is (= 0 (count (get-in @state [:challenger :hand]))))
    (prompt-select :challenger (find-card "Sure Gamble" (get-in @state [:challenger :play-area])))
    (prompt-select :challenger (find-card "Desperado" (get-in @state [:challenger :play-area])))
    (prompt-select :challenger (find-card "Bank Job" (get-in @state [:challenger :play-area])))
    (prompt-select :challenger (find-card "Eater" (get-in @state [:challenger :play-area])))
    (is (= 4 (count (:hosted (:identity (get-challenger))))) "4 cards in NVRAM")
    (is (= 0 (count (get-in @state [:challenger :play-area]))) "The play area is empty")
    (prompt-choice :contestant "Keep")
    (prompt-choice :challenger "Keep")
    (take-credits state :contestant)
    (is (= 2 (count (get-in @state [:challenger :hand]))) "There are 2 cards in the challenger's Grip")
    (card-ability state :challenger (:identity (get-challenger)) 0)
    (prompt-card :challenger (find-card "Bank Job" (:hosted (:identity (get-challenger)))))
    (is (= 3 (count (get-in @state [:challenger :hand]))) "There are 3 cards in the challenger's Grip")))

(deftest cerebral-imaging-max-hand-size
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game
      (make-deck "Cerebral Imaging: Infinite Frontiers" [(qty "Hedge Fund" 3)])
      (default-challenger))
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (is (= 13 (:credit (get-contestant))) "Has 13 credits")
    (is (= 13 (core/hand-size state :contestant)) "Max hand size is 13")))

(deftest chronos-protocol
  ;; Chronos Protocol - Choose Challenger discard for first net damage of a turn
  (do-game
    (new-game
      (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1) (qty "Neural EMP" 2)])
      (default-challenger [(qty "Imp" 3)]))
    (play-from-hand state :contestant "Pup" "HQ")
    (take-credits state :contestant)
    (run-on state :hq)
    (let [pup (get-character state :hq 0)]
      (core/rez state :contestant pup)
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

(deftest chronos-protocol-employee-strike
  ;; Chronos Protocol - Issue #1958 also affects Chronos Protocol
  (do-game
    (new-game
      (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1)])
      (default-challenger [(qty "Employee Strike" 1) (qty "Scrubbed" 3) (qty "Sure Gamble" 1)]))
    (play-from-hand state :contestant "Pup" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Employee Strike")
    (run-on state :hq)
    (let [pup (get-character state :hq 0)]
      (core/rez state :contestant pup)
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
      (is (not (empty? (:prompt (get-contestant)))) "Employee Strike out of play - Ability turned on correctly"))))

(deftest edward-kim
  ;; Edward Kim - Trash first operation accessed each turn, but not if first one was in Archives
  (do-game
    (new-game
      (default-contestant [(qty "Hedge Fund" 3) (qty "Restructure" 2) (qty "PAD Campaign" 1)])
      (make-deck "Edward Kim: Humanity's Hammer" [(qty "Eater" 1) (qty "Sure Gamble" 2)]))
    (play-from-hand state :contestant "Hedge Fund")
    (trash-from-hand state :contestant "PAD Campaign")
    (take-credits state :contestant)
    (run-empty-server state "Archives")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-contestant)))) "No operation trashed from HQ; accessed one in Archives first")
    (take-credits state :challenger)
    (core/move state :contestant (find-card "Hedge Fund" (:discard (get-contestant))) :hand)
    (is (= 1 (count (:discard (get-contestant)))))
    (take-credits state :contestant)
    (run-empty-server state "Archives")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-contestant)))) "1 operation trashed from HQ; accessed non-operation in Archives first")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Eater")
    (let [eater (get-in @state [:challenger :rig :program 0])]
      (run-on state "Archives")
      (card-ability state :challenger eater 0) ; pretend to break a sub so no cards in Archives will be accessed
      (run-successful state)
      (is (= 3 (count (:discard (get-contestant)))))
      (run-empty-server state "HQ")
      (is (= 4 (count (:discard (get-contestant)))) "1 operation trashed from HQ; accessed non-operation in Archives first"))))

(deftest edward-kim-maw
  ;; Edward Kim - Do not trigger maw on first Operation access (due to trash)
  (do-game
    (new-game
      (default-contestant [(qty "Hedge Fund" 3) (qty "Restructure" 2)])
      (make-deck "Edward Kim: Humanity's Hammer" [(qty "Maw" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Maw")
    (is (= 0 (count (:discard (get-contestant)))) "No cards in Archives")
    (run-empty-server state "HQ")
    (is (= 1 (count (:discard (get-contestant)))) "Only one card trashed from HQ, by Ed Kim")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-contestant)))) "One more card trashed from HQ, by Maw")))


(deftest exile-customized-secretary
  ;; Exile - simultaneous-resolution prompt shown for interaction with Customized Secretary
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Exile: Streethawk" [(qty "Customized Secretary" 3) (qty "Clone Chip" 3)
                                      (qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Customized Secretary" "Clone Chip"])
    (trash-from-hand state :challenger "Customized Secretary")
    (play-from-hand state :challenger "Clone Chip")
    (card-ability state :challenger (get-hardware state 0) 0)
    (prompt-select :challenger (find-card "Customized Secretary" (:discard (get-challenger))))
    ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
    (is (= 2 (-> (get-challenger) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
    (prompt-choice :challenger "Exile: Streethawk")
    (is (= 1 (count (:hand (get-challenger)))) "Exile drew a card")))

(deftest gabriel-santiago
  ;; Gabriel Santiago - Gain 2c on first successful HQ run each turn
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Gabriel Santiago: Consummate Professional" [(qty "Easy Mark" 1)]))
    (take-credits state :contestant)
    (run-empty-server state :rd)
    (is (= 5 (:credit (get-challenger))) "No credits gained")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-challenger))) "Gained 2c")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-challenger))) "No credits gained")))

(deftest gagarin
  ;; Gagarin - pay 1c to access each card in remote
  (do-game
    (new-game
      (make-deck "Gagarin Deep Space: Expanding the Horizon" [(qty "PAD Campaign" 1) (qty "Caprcharacter Nisei" 1)])
      (default-challenger))
    (core/lose state :challenger :credit 4)
    (is (= 1 (:credit (get-challenger))) "Challenger has 1 credit")
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (run-empty-server state :remote1)
    (prompt-select :challenger (get-content state :remote1 0))
    (is (= 0 (:credit (get-challenger))) "Paid 1 credit to access")
    (prompt-choice :challenger "No") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-server state :remote1)
    (prompt-select :challenger (get-content state :remote1 0))
    (prompt-choice :challenger "OK") ; Could not afford message dismissed
    (is (empty? (:prompt (get-challenger))) "Challenger cannot access so no trash prompt")
    (is (not (last-log-contains? state "PAD Campaign")) "No card name was logged")
    (run-empty-server state :hq)
    (prompt-choice :challenger "No") ; Dismiss trash prompt
    (is (last-log-contains? state "Caprcharacter") "Accessed card name was logged")))

(deftest grndl-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (default-challenger))
    (is (= 10 (:credit (get-contestant))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-contestant))) "GRNDL starts with 1 bad publicity")))

(deftest grndl-valencia
  ;; GRNDL vs Valencia - only 1 bad pub at start
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
    (is (= 10 (:credit (get-contestant))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-contestant))) "GRNDL starts with 1 bad publicity")))

(deftest haarpsichord-studios
  ;; Haarpsichord Studios - Prevent stealing more than 1 agenda per turn
  (do-game
    (new-game
      (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
      (default-challenger [(qty "Gang Sign" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Gang Sign")
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Steal")
    (is (= 1 (:agenda-point (get-challenger))))
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Steal")
    (is (= 1 (:agenda-point (get-challenger))) "Second steal of turn prevented")
    (take-credits state :challenger)
    (play-from-hand state :contestant "15 Minutes" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (prompt-choice :challenger "Card from hand")
    (prompt-choice :challenger "Steal")
    (is (= 2 (:agenda-point (get-challenger))) "Steal prevention didn't carry over to Contestant turn")))

(deftest haarpsichord-studios-employee-strike
  ;; Haarpsichord Studios - Interactions with Employee Strike. Issue #1313.
  (do-game
    (new-game
      (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
      (default-challenger [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :click 5)
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Steal")
    (is (= 1 (:agenda-point (get-challenger))))
    (play-from-hand state :challenger "Employee Strike")
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Steal")
    (is (= 2 (:agenda-point (get-challenger))) "Second steal not prevented")
    (play-from-hand state :challenger "Scrubbed")
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Steal")
    (is (= 2 (:agenda-point (get-challenger))) "Third steal prevented")))

(deftest haas-bioroid-architects-of-tomorrow
  ;; Architects of Tomorrow - prompt to rez after passing bioroid
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Architects of Tomorrow" [(qty "Eli 1.0" 2) (qty "Pup" 1)])
      (default-challenger))
    (core/gain state :contestant :credit 3)
    (play-from-hand state :contestant "Eli 1.0" "Archives")
    (play-from-hand state :contestant "Pup" "Archives")
    (play-from-hand state :contestant "Eli 1.0" "HQ")
    (take-credits state :contestant)
    (run-on state "Archives")
    (core/rez state :contestant (get-character state :archives 1))
    (run-continue state)
    (core/rez state :contestant (get-character state :archives 0))
    (is (= 3 (:credit (get-contestant))) "Contestant has 3 credits after rezzing Eli 1.0")
    (run-continue state)
    (prompt-select :contestant (get-character state :hq 0))
    (is (= 3 (:credit (get-contestant))) "Contestant not charged for Architects of Tomorrow rez of Eli 1.0")))

(deftest haas-bioroid-asa-group
  ;; Asa Group - don't allow installation of operations
  (do-game
    (new-game
      (make-deck "Asa Group: Security Through Vigilance" [(qty "Pup" 1) (qty "BOOM!" 1) (qty "Urban Renewal" 1)])
      (default-challenger))
    (play-from-hand state :contestant "Pup" "New remote")
    (prompt-select :contestant (find-card "BOOM!" (:hand (get-contestant))))
    (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
    (prompt-select :contestant (find-card "Urban Renewal" (:hand (get-contestant))))
    (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))

(deftest haas-bioroid-engineering-the-future-employee-strike
  ;; EtF - interaction with Employee Strike
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Engineering the Future" [(qty "Eli 1.0" 3) (qty "Paywall Implementation" 1)])
      (default-challenger [(qty "Employee Strike" 1)]))
    (take-credits state :contestant)
    (is (= 8 (:credit (get-contestant))) "Contestant has 8 credits at turn end")
    (play-from-hand state :challenger "Employee Strike")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Eli 1.0" "New remote")
    (is (= 8 (:credit (get-contestant))) "Contestant did not gain 1cr from EtF")
    (play-from-hand state :contestant "Paywall Implementation")
    (play-from-hand state :contestant "Eli 1.0" "New remote")
    (is (= 8 (:credit (get-contestant))) "Contestant did not gain 1cr from EtF")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (play-from-hand state :contestant "Eli 1.0" "New remote")
    (is (= 9 (:credit (get-contestant))) "Contestant gained 1cr from EtF")))

(deftest haas-bioroid-stronger-together
  ;; Stronger Together - +1 strength for Bioroid character
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Stronger Together" [(qty "Eli 1.0" 1)])
      (default-challenger))
    (play-from-hand state :contestant "Eli 1.0" "Archives")
    (let [eli (get-character state :archives 0)]
      (core/rez state :contestant eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest iain-stirling-credits
  ;; Iain Stirling - Gain 2 credits when behind
  (do-game
    (new-game
      (default-contestant [(qty "Breaking News" 1)])
      (make-deck "Iain Stirling: Retired Spook" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Breaking News" "New remote")
    (let [ag1 (get-in @state [:contestant :servers :remote1 :content 0])]
      (core/advance state :contestant {:card (refresh ag1)})
      (core/advance state :contestant {:card (refresh ag1)})
      (core/score state :contestant {:card (refresh ag1)})
      (take-credits state :contestant)
      (is (= 1 (:agenda-point (get-contestant))) "Contestant gains 1 agenda point from Breaking News")
      (take-credits state :challenger 1)
      (is (= 8 (:credit (get-challenger))) "Gained 2 credits from being behind on points"))))

(deftest industrial-genomics-trash-cost
  ;; Industrial Genomics - Increase trash cost
  (do-game
    (new-game
      (make-deck "Industrial Genomics: Growing Solutions" [(qty "PAD Campaign" 3)
                                                           (qty "Hedge Fund" 3)])
      (default-challenger))
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (trash-from-hand state :contestant "PAD Campaign")
    (trash-from-hand state :contestant "PAD Campaign")
    (trash-from-hand state :contestant "Hedge Fund")
    (trash-from-hand state :contestant "Hedge Fund")
    (let [pad (get-content state :remote1 0)]
      (core/rez state :contestant pad)
      (take-credits state :contestant)
      (run-empty-server state "Server 1")
      (is (= 8 (core/trash-cost state :challenger (refresh pad)))))))

(deftest jemison-astronautics
  ;; Jemison Astronautics - Place advancements when forfeiting agendas
  (do-game
    (new-game
      (make-deck "Jemison Astronautics: Sacrifcharacter. Audacity. Success." [(qty "Enforcer 1.0" 1) (qty "Hostile Takeover" 1)
                                                                        (qty "Ice Wall" 1) (qty "Global Food Initiative" 1)])
      (default-challenger [(qty "Data Dealer" 1)]))
    (play-from-hand state :contestant "Enforcer 1.0" "HQ")
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (let [enf (get-character state :hq 0)
          iwall (get-character state :rd 0)]
      (take-credits state :contestant)
      (play-from-hand state :challenger "Data Dealer")
      (run-empty-server state "Server 1")
      (prompt-choice :challenger "Steal")
      (let [dd (get-resource state 0)]
        (card-ability state :challenger dd 0)
        (prompt-select :challenger (get-in (get-challenger) [:scored 0]))
        (is (empty? (:prompt (get-contestant))) "No Jemison prompt for Challenger forfeit")
        (take-credits state :challenger)
        (play-from-hand state :contestant "Global Food Initiative" "New remote")
        (score-agenda state :contestant (get-content state :remote2 0))
        (core/rez state :contestant enf)
        (prompt-select :contestant (get-in (get-contestant) [:scored 0]))
        (prompt-select :contestant iwall)
        (is (= 4 (:advance-counter (refresh iwall))) "Jemison placed 4 advancements")))))

(deftest jesminder-sareen-ability
  ;; Jesminder Sareen - avoid tags only during a run
  (do-game
    (new-game (default-contestant [(qty "SEA Source" 1) (qty "Data Raven" 1)])
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Data Raven" "Archives")
    (take-credits state :contestant)
    (let [dr (-> @state :contestant :servers :archives :characters first)]
      (core/rez state :contestant dr)
      (core/click-run state :challenger {:server "Archives"})
      (card-ability state :contestant dr 0)
      (is (= 0 (:tag (get-challenger))) "Jesminder avoided first tag during the run")
      (card-ability state :contestant dr 0)
      (is (= 1 (:tag (get-challenger))) "Jesminder did not avoid the second tag during the run")
      (core/no-action state :contestant nil)
      (core/continue state :challenger nil)
      (core/no-action state :contestant nil)
      (core/successful-run state :challenger nil)
      (run-empty-server state "R&D") ; clear per-run buffer
      (take-credits state :challenger)
      (play-from-hand state :contestant "SEA Source")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 2 (:tag (get-challenger))) "Jesminder did not avoid the tag outside of a run"))))

(deftest jesminder-john-masanori
  ;; Jesminder Sareen - don't avoid John Masanori tag
  (do-game
    (new-game (default-contestant)
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "John Masanori" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "John Masanori")
    (run-on state "HQ")
    (core/jack-out state :challenger nil)
    (is (= 1 (:tag (get-challenger))) "Jesminder did not avoid John Masanori tag")))

(deftest jinteki-biotech-brewery
  ;; Jinteki Biotech - Brewery net damage
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Braintrust" 1)])
      (default-challenger)
      {:dont-start-turn true})
    (prompt-choice :contestant "The Brewery")
    (core/start-turn state :contestant nil)
    (card-ability state :contestant (:identity (get-contestant)) 1)
    (is (= 1 (count (:hand (get-challenger)))) "Challenger took 2 net damage from Brewery flip")))

(deftest jinteki-biotech-greenhouse
  ;; Jinteki Biotech - Greenhouse four advancement tokens
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Braintrust" 1)])
      (default-challenger)
      {:dont-start-turn true})
    (prompt-choice :contestant "The Greenhouse")
    (core/start-turn state :contestant nil)
    (play-from-hand state :contestant "Braintrust" "New remote")
    (take-credits state :contestant)
    (take-credits state :challenger)
    (let [bt (get-content state :remote1 0)]
      (is (nil? (:advance-counter (refresh bt))) "No advancement counters on agenda")
      (card-ability state :contestant (:identity (get-contestant)) 1)
      (prompt-select :contestant (refresh bt))
      (is (= 4 (:advance-counter (refresh bt))) "Four advancement counters on agenda"))))

(deftest jinteki-biotech-tank
  ;; Jinteki Biotech - Tank shuffle Archives into R&D
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Hedge Fund" 3)])
      (default-challenger)
      {:dont-start-turn true})
    (prompt-choice :contestant "The Tank")
    (core/start-turn state :contestant nil)
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :challenger)
    (is (= 3 (count (:discard (get-contestant)))) "Archives started with 3 cards")
    (is (= 0 (count (:deck (get-contestant)))) "R&D started empty")
    (card-ability state :contestant (:identity (get-contestant)) 1)
    (is (= 0 (count (:discard (get-contestant)))) "Archives ended empty")
    (is (= 3 (count (:deck (get-contestant)))) "R&D ended with 3 cards")))

(deftest jinteki-personal-evolution
  ;; Personal Evolution - Prevent challenger from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Personal Evolution" [(qty "Braintrust" 6)])
      (default-challenger [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Braintrust" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Steal")
    (is (= 2 (count (:hand (get-challenger)))) "Challenger took 1 net damage from steal")))

(deftest jinteki-potential-unleashed
  ;; PU - when the challenger takes at least one net damage, mill 1 from their deck
  (do-game
    (new-game (make-deck "Jinteki: Potential Unleashed" [(qty "Philotic Entanglement" 1) (qty "Neural EMP" 1) (qty "Braintrust" 3)])
              (default-challenger [(qty "Employee Strike" 10)]))
    (play-from-hand state :contestant "Braintrust" "New remote")
    (play-from-hand state :contestant "Braintrust" "New remote")
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Steal")
    (run-empty-server state "Server 2")
    (prompt-choice :challenger "Steal")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Philotic Entanglement" "New remote")
    (score-agenda state :contestant (get-content state :remote3 0))
    (is (= 3 (count (:discard (get-challenger)))))
    (play-from-hand state :contestant "Neural EMP")
    (is (= 5 (count (:discard (get-challenger)))))))

(deftest jinteki-replicating-perfection
  ;; Replicating Perfection - Prevent challenger from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-challenger))
    (play-from-hand state :contestant "Mental Health Clinic" "New remote")
    (take-credits state :contestant)
    (is (not (core/can-run-server? state "Server 1")) "Challenger can only run on centrals")
    (run-empty-server state "HQ")
    (is (boolean (core/can-run-server? state "Server 1")) "Challenger can run on remotes")))

(deftest jinteki-replicating-perfection-employee-strike
  ;; Replicating Perfection - interaction with Employee Strike. Issue #1313 and #1956.
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-challenger [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (play-from-hand state :contestant "Mental Health Clinic" "New remote")
    (take-credits state :contestant)
    (is (not (core/can-run-server? state "Server 1")) "Challenger can only run on centrals")
    (play-from-hand state :challenger "Employee Strike")
    (is (boolean (core/can-run-server? state "Server 1")) "Challenger can run on remotes")
    (play-from-hand state :challenger "Scrubbed")
    (is (not (core/can-run-server? state "Server 1")) "Challenger can only run on centrals")))

(deftest kate-mac-mccaffrey-discount
  ;; Kate 'Mac' McCaffrey - Install discount
  (do-game
    (new-game (default-contestant)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Magnum Opus")
    (is (= 1 (:credit (get-challenger))) "Installed Magnum Opus for 4 credits")))

(deftest kate-mac-mccaffrey-no-discount
  ;; Kate 'Mac' McCaffrey - No discount for 0 cost
  (do-game
    (new-game (default-contestant)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                         [(qty "Magnum Opus" 1)
                          (qty "Self-modifying Code" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Self-modifying Code")
    (play-from-hand state :challenger "Magnum Opus")
    (is (= 0 (:credit (get-challenger))) "No Kate discount on second program install")))

(deftest kate-mac-mccaffrey-discount-cant-afford
  ;; Kate 'Mac' McCaffrey - Can Only Afford With the Discount
  (do-game
    (new-game (default-contestant)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :contestant)
    (core/lose state :challenger :credit 1)
    (is (= 4 (:credit (get-challenger))))
    (play-from-hand state :challenger "Magnum Opus")
    (is (= 1 (count (get-in @state [:challenger :rig :program]))) "Magnum Opus installed")
    (is (= 0 (:credit (get-challenger))) "Installed Magnum Opus for 4 credits")))

(deftest ken-tenma-run-event-credit
  ;; Ken 'Express' Tenma - Gain 1 credit when first Run event played
  (do-game
    (new-game (default-contestant)
              (make-deck "Ken \"Express\" Tenma: Disappeared Clone" [(qty "Account Siphon" 2)]))
    (take-credits state :contestant)
    (play-run-event state (first (:hand (get-challenger))) :hq)
    (is (= 6 (:credit (get-challenger))) "Gained 1 credit for first Run event")
    (prompt-choice :challenger "Run ability")
    (play-run-event state (first (:hand (get-challenger))) :hq)
    (is (= 16 (:credit (get-challenger))) "No credit gained for second Run event")))

(deftest khan-vs-caprcharacter
  ;; Khan - proper order of events when vs. Caprcharacter
  (do-game
    (new-game
      (default-contestant [(qty "Eli 1.0" 1) (qty "Caprcharacter Nisei" 1)])
      (make-deck "Khan: Savvy Skiptracer" [(qty "Corroder" 1)]))
    (play-from-hand state :contestant "Eli 1.0" "Archives")
    (play-from-hand state :contestant "Caprcharacter Nisei" "Archives")
    (core/rez state :contestant (get-content state :archives 0))
    (take-credits state :contestant)
    (run-on state "Archives")
    (run-continue state)
    (is (and (empty? (:prompt (get-contestant)))
             (= 1 (count (:prompt (get-challenger))))
             (= "Khan: Savvy Skiptracer" (-> (get-challenger) :prompt first :card :title)))
        "Only Khan prompt showing")
    (prompt-select :challenger (first (:hand (get-challenger))))
    (is (find-card "Corroder" (-> (get-challenger) :rig :program)) "Corroder installed")
    (is (= 4 (:credit (get-challenger))) "1cr discount from Khan")
    (is (= "Caprcharacter Nisei" (-> (get-challenger) :prompt first :card :title)) "Caprcharacter prompt showing")
    (prompt-choice :challenger "0 [Credits]")
    (prompt-choice :contestant "1 [Credits]")
    (is (not (:run @state)) "Run ended")))

(deftest laramy-fisk-shards
  ;; Laramy Fisk - installing a Shard should still give option to force Contestant draw.
  (do-game
    (new-game
      (default-contestant [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3)])
      (make-deck "Laramy Fisk: Savvy Investor" [(qty "Eden Shard" 1)]))
    (starting-hand state :contestant ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Eli 1.0" "Eli 1.0"])
    (take-credits state :contestant)
    (run-on state "R&D")
    (core/no-action state :contestant nil)
    ;; at Successful Run stage -- click Eden Shard to install
    (play-from-hand state :challenger "Eden Shard")
    (is (= 5 (:credit (get-challenger))) "Eden Shard install was free")
    (is (= "Eden Shard" (:title (get-resource state 0))) "Eden Shard installed")
    (is (= "Identity" (-> (get-challenger) :prompt first :card :type)) "Fisk prompt showing")
    (prompt-choice :challenger "Yes")
    (is (not (:run @state)) "Run ended")
    (is (= 6 (count (:hand (get-contestant)))) "Contestant forced to draw")))

(deftest leela-gang-sign-complicated
  ;; Leela Patel - complicated interaction with mutiple Gang Sign
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Project Atlas" 1)
                                                                  (qty "Hostile Takeover" 1)
                                                                  (qty "Geothermal Fracking" 1)])
      (make-deck "Leela Patel: Trained Pragmatist" [(qty "Gang Sign" 2)]))
    (play-from-hand state :contestant "Project Atlas" "New remote")
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (play-from-hand state :contestant "Geothermal Fracking" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Gang Sign")
    (play-from-hand state :challenger "Gang Sign")
    (take-credits state :challenger)
    (score-agenda state :contestant (get-content state :remote1 0))
    (prompt-choice :challenger "Leela Patel: Trained Pragmatist")
    (prompt-select :challenger (get-content state :remote2 0))
    (is (find-card "Hostile Takeover" (:hand (get-contestant))) "Hostile Takeover returned to hand")
    (prompt-choice :challenger "Gang Sign")
    (prompt-choice :challenger "Card from hand")
    (prompt-choice :challenger "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-challenger))) "Hostile Takeover stolen with Gang Sign")
    (prompt-select :challenger (get-content state :remote3 0))
    (is (find-card "Geothermal Fracking" (:hand (get-contestant))) "Geothermal Fracking returned to hand")
    (prompt-choice :challenger "Card from hand")
    (prompt-choice :challenger "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-challenger))) "Geothermal Fracking stolen with Gang Sign")
    (prompt-choice :challenger "Done")))

(deftest leela-lingering-successful-run-prompt
  ;; Leela Patel - issues with lingering successful run prompt
  (do-game
    (new-game
      (make-deck "NBN: Making News" [(qty "Breaking News" 1) (qty "SanSan City Grid" 1)])
      (make-deck "Leela Patel: Trained Pragmatist" []))
    (starting-hand state :contestant ["SanSan City Grid"])
    (play-from-hand state :contestant "SanSan City Grid" "New remote")
    (take-credits state :contestant)
    (run-empty-server state :rd)
    (prompt-choice :challenger "Steal")
    (prompt-select :challenger (get-content state :remote1 0))
    (is (not (:run @state)) "Run is over")))

(deftest leela-upgrades
  ;; Leela Patel - upgrades returned to hand in the middle of a run do not break the run. Issue #2008.
  (do-game
    (new-game (default-contestant [(qty "Crisium Grid" 3) (qty "Project Atlas" 3) (qty "Shock!" 1)])
              (make-deck "Leela Patel: Trained Pragmatist" [(qty "Sure Gamble" 1)]))
    (starting-hand state :contestant ["Crisium Grid" "Crisium Grid" "Crisium Grid" "Project Atlas" "Shock!" "Project Atlas"])
    (play-from-hand state :contestant "Crisium Grid" "HQ")
    (play-from-hand state :contestant "Crisium Grid" "Archives")
    (play-from-hand state :contestant "Crisium Grid" "R&D")
    (trash-from-hand state :contestant "Project Atlas")
    (trash-from-hand state :contestant "Shock!")
    (take-credits state :contestant)
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Card from hand")
    (prompt-choice :challenger "Steal")
    (prompt-select :challenger (get-content state :hq 0))
    (is (not (get-content state :hq 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "R&D")
    (prompt-choice :challenger "Card from deck")
    (prompt-choice :challenger "Steal")
    (prompt-select :challenger (get-content state :rd 0))
    (is (not (get-content state :rd 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "Archives")
    (prompt-choice :challenger "Shock!")
    (prompt-choice :challenger "Project Atlas")
    (prompt-choice :challenger "Steal")
    (prompt-select :challenger (get-content state :archives 0))
    (is (not (get-content state :archives 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")))

(deftest maxx
  (do-game
    (new-game (default-contestant)
              (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                    (qty "Eater" 1)]))
    (starting-hand state :challenger ["Eater"])
    (take-credits state :contestant)
    (is (= 2 (count (:discard (get-challenger)))) "MaxX discarded 2 cards at start of turn")
    (is (last-log-contains? state "Wyldside, Wyldside")
        "Maxx did log trashed card names")))

(deftest maxx-wyldside-start-of-turn
  ;; MaxX and Wyldside - using Wyldside during Step 1.2 should lose 1 click
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
    (is (find-card "Wyldside" (get-in @state [:challenger :rig :resource])) "Wyldside was installed")
    (take-credits state :contestant)
    (is (= 0 (:click (get-challenger))) "Challenger has 0 clicks")
    (is (:challenger-phase-12 @state) "Challenger is in Step 1.2")
    (let [maxx (get-in @state [:challenger :identity])
          wyld (find-card "Wyldside" (get-in @state [:challenger :rig :resource]))]
      (card-ability state :challenger maxx 0)
      (card-ability state :challenger wyld 0)
      (core/end-phase-12 state :challenger nil)
      (is (= 4 (count (:discard (get-challenger)))) "MaxX discarded 2 cards at start of turn")
      (is (= 3 (:click (get-challenger))) "Wyldside caused 1 click to be lost")
      (is (= 3 (count (:hand (get-challenger)))) "3 cards drawn total"))))

(deftest nasir-ability-basic
  ;; Nasir Ability - Basic
  (do-game
    (new-game
      (default-contestant [(qty "Ice Wall" 3)])
      (make-deck "Nasir Meidan: Cyber Explorer" []))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)

    (run-on state "HQ")
    (let [iwall (get-character state :hq 0)
          nasir (get-in @state [:challenger :identity])]
      (core/rez state :contestant iwall)
      (is (= 5 (:credit (get-challenger))) "Nasir Ability does not trigger automatically")
      (card-ability state :challenger nasir 0)
      (is (= 1 (:credit (get-challenger))) "Credits at 1 after Nasir ability trigger"))))

(deftest nasir-ability-xanadu
  ;; Nasir Ability - Xanadu
  (do-game
    (new-game
      (default-contestant [(qty "Ice Wall" 1)])
      (make-deck "Nasir Meidan: Cyber Explorer" [(qty "Xanadu" 1)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)

    (swap! state assoc-in [:challenger :credit] 6)
    (play-from-hand state :challenger "Xanadu")
    (run-on state "HQ")
    (let [iwall (get-in @state [:contestant :servers :hq :characters 0])
          nasir (get-in @state [:challenger :identity])]
      (core/rez state :contestant iwall)
      (is (= 3 (:credit (get-challenger))) "Pay 3 to install Xanadu")
      (card-ability state :challenger nasir 0)
      (is (= 2 (:credit (get-challenger))) "Gain 1 more credit due to Xanadu"))))

(deftest nbn-controlling-the-message
  ;; NBN: Controlling the Message - Trace to tag Challenger when first installed Contestant card is trashed
  (do-game
    (new-game
      (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 3)])
      (default-challenger [(qty "Forger" 1)]))
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Forger")
    ; trash from HQ first - #2321
    (run-empty-server state "HQ")
    (prompt-choice :challenger "Yes")
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Yes")
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (empty? (:prompt (get-challenger))) "Forger can't avoid the tag")
    (is (= 1 (:tag (get-challenger))) "Challenger took 1 unpreventable tag")
    (run-empty-server state "Server 2")
    (prompt-choice :challenger "Yes")
    (is (empty? (:prompt (get-contestant))) "No trace chance on 2nd trashed card of turn")))

(deftest nbn-controlling-the-message-drt
  ;; NBN: Controlling the Message - Interaction with Dedicated Response Team
  (do-game
    (new-game
      (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 1) (qty "Dedicated Response Team" 1)])
      (default-challenger))
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (play-from-hand state :contestant "Dedicated Response Team" "New remote")
    (core/rez state :contestant (get-content state :remote2 0))
    (take-credits state :contestant)
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Yes")
    (prompt-choice :contestant "Yes")
    (prompt-choice :contestant 0)
    (prompt-choice :challenger 0)
    (is (= 1 (:tag (get-challenger))) "Challenger took 1 unpreventable tag")
    (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 meat damage from DRT")))

(deftest new-angeles-sol-on-steal
  ;; New Angeles Sol - interaction with challenger stealing agendas
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Paywall Implementation" 2) (qty "Breaking News" 1)])
      (default-challenger))
    (play-from-hand state :contestant "Breaking News" "New remote")
    (play-from-hand state :contestant "Paywall Implementation")
    (take-credits state :contestant)
    (is (= 6 (:credit (get-contestant))))
    (run-empty-server state :remote1)
    (is (= 7 (:credit (get-contestant))) "Contestant gained 1cr from successful run")
    (prompt-choice :challenger "Steal")
    (prompt-choice :contestant "Yes")
    (is (find-card "Paywall Implementation" (:discard (get-contestant))) "Paywall trashed before Sol triggers")
    (prompt-select :contestant (find-card "Paywall Implementation" (:hand (get-contestant))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Paywall Implementation" (:current (get-contestant))) "Paywall back in play")))

(deftest nisei-division
  ;; Nisei Division - Gain 1 credit from every psi game
  (do-game
    (new-game
      (make-deck "Nisei Division: The Next Generation" [(qty "Snowflake" 2)])
      (default-challenger))
    (play-from-hand state :contestant "Snowflake" "HQ")
    (play-from-hand state :contestant "Snowflake" "HQ")
    (take-credits state :contestant)
    (let [s1 (get-in @state [:contestant :servers :hq :characters 0])
          s2 (get-in @state [:contestant :servers :hq :characters 1])]
      (run-on state "HQ")
      (core/rez state :contestant s2)
      (is (= 4 (:credit (get-contestant))))
      (card-subroutine state :contestant s2 0)
      (prompt-choice :contestant "0 [Credits]")
      (prompt-choice :challenger "0 [Credits]")
      (is (= 5 (:credit (get-contestant))) "Gained 1 credit from psi game")
      (core/no-action state :contestant nil)
      (core/rez state :contestant s1)
      (is (= 4 (:credit (get-contestant))))
      (card-subroutine state :contestant s1 0)
      (prompt-choice :contestant "0 [Credits]")
      (prompt-choice :challenger "1 [Credits]")
      (is (= 5 (:credit (get-contestant))) "Gained 1 credit from psi game"))))

(deftest noise-ability
  ;; Noise: Hacker Extraordinaire - Ability
  (do-game
    (new-game
      (default-contestant [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)])
      (make-deck "Noise: Hacker Extraordinaire" [(qty "Datasucker" 1) (qty "Cache" 1) (qty "Sure Gamble" 1) (qty "Clone Chip" 2) (qty "Sharpshooter" 2)]))
    (starting-hand state :challenger ["Datasucker" "Sure Gamble" "Clone Chip" "Clone Chip" "Cache"])
    (is (= 6 (count (:hand (get-contestant)))) "Contestant should start with 6 cards in hand")
    (is (= 5 (count (:deck (get-contestant)))) "Contestant deck should contain 5 cards")
    (take-credits state :contestant)
    (is (= 0 (count (:discard (get-contestant)))) "Archives started empty")
    (play-from-hand state :challenger "Datasucker")
    (is (= 1 (count (:discard (get-contestant)))) "Playing virus should cause card to be trashed from R&D")
    (is (= 4 (count (:deck (get-contestant)))) "Card trashed to Archives by Noise should come from R&D")
    (play-from-hand state :challenger "Sure Gamble")
    (is (= 1 (count (:discard (get-contestant)))) "Playing non-virus should not cause card to be trashed from R&D")
    (core/click-draw state :challenger nil)
    (play-from-hand state :challenger "Clone Chip")
    (play-from-hand state :challenger "Clone Chip")
    (trash-from-hand state :challenger "Cache")
    (trash-from-hand state :challenger "Sharpshooter")
    (take-credits state :challenger)
    ;; playing virus via Clone Chip on Contestant's turn should trigger Noise ability
    (let [chip (get-in @state [:challenger :rig :hardware 0])]
      (card-ability state :challenger chip 0)
      (prompt-select :challenger (find-card "Cache" (:discard (get-challenger))))
      (let [ds (get-in @state [:challenger :rig :program 1])]
        (is (not (nil? ds)))
        (is (= (:title ds) "Cache"))))
    (is (= 2 (count (:discard (get-contestant)))) "Playing virus via Clone Chip on contestant's turn should trigger Noise ability")
    (is (= 2 (count (:deck (get-contestant)))) "Card trashed to Archives by Noise should come from R&D")
    ;; playing non-virus via Clone Chip on Contestant's turn should NOT trigger Noise ability
    (let [chip-2 (get-in @state [:challenger :rig :hardware 0])]
      (card-ability state :challenger chip-2 0)
      (prompt-select :challenger (find-card "Sharpshooter" (:discard (get-challenger))))
      (let [ss (get-in @state [:challenger :rig :program 2])]
        (is (not (nil? ss)))
        (is (= (:title ss) "Sharpshooter"))))
    (is (= 2 (count (:discard (get-contestant)))) "Playing non-virus via Clone Chip on contestant's turn should not trigger Noise ability")))

(deftest null-ability
  ;; Null ability - once per turn
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
      (is (empty? (:prompt (get-challenger))) "Ability won't work on unrezzed Character")
      (core/rez state :contestant wrap2)
      (card-ability state :challenger null 0)
      (prompt-select :challenger (find-card "Sure Gamble" (:hand (get-challenger))))
      (is (= 5 (:current-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
      (run-continue state)
      (core/rez state :contestant wrap1)
      (card-ability state :challenger null 0)
      (is (empty? (:prompt (get-challenger))) "Ability already used this turn")
      (run-jack-out state)
      (is (= 7 (:current-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))

(deftest null-trashed
  ;; Null ability - does not affect next character when current is trashed. Issue #1788.
  (do-game
    (new-game
      (default-contestant [(qty "Wraparound" 1) (qty "Spiderweb" 1)])
      (make-deck "Null: Whistleblower" [(qty "Parasite" 3)]))
    (play-from-hand state :contestant "Spiderweb" "HQ")
    (play-from-hand state :contestant "Wraparound" "HQ")
    (take-credits state :contestant)
    (core/gain state :contestant :credit 10)
    (let [null (get-in @state [:challenger :identity])
          spider (get-character state :hq 0)
          wrap (get-character state :hq 1)]
      (core/rez state :contestant spider)
      (core/rez state :contestant wrap)
      (play-from-hand state :challenger "Parasite")
      (prompt-select :challenger (refresh spider))
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :challenger null 0)
      (prompt-select :challenger (first (:hand (get-challenger))))
      (is (find-card "Spiderweb" (:discard (get-contestant))) "Spiderweb trashed by Parasite + Null")
      (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Null"))))

(deftest omar-ability
  ;; Omar Keung - Make a successful run on the chosen server once per turn
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (let [omar (get-in @state [:challenger :identity])]
      (card-ability state :challenger omar 0)
      (run-successful state)
      (prompt-choice :challenger "HQ")
      (is (= [:hq] (-> (get-challenger) :register :successful-run)))
      (is (= "You accessed Hedge Fund" (-> (get-challenger) :prompt first :msg)))
      (prompt-choice :challenger "OK")
      (is (= 3 (:click (get-challenger))))
      (card-ability state :challenger omar 0)
      (is (= 3 (:click (get-challenger))))
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-empty-server state :rd)
      (is (= [:rd] (-> (get-challenger) :register :successful-run)))
      (card-ability state :challenger omar 0)
      (run-successful state)
      (prompt-choice :challenger "HQ")
      (is (= [:hq :rd] (-> (get-challenger) :register :successful-run))))))

(deftest omar-ash
  ;; Omar Keung - Ash prevents access, but not successful run
  (do-game
    (new-game
      (default-contestant [(qty "Ash 2X3ZB9CY" 1)])
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Ash 2X3ZB9CY" "HQ")
    (take-credits state :contestant)
    (let [omar (get-in @state [:challenger :identity])
          ash (get-content state :hq 0)]
      (core/rez state :contestant ash)
      (card-ability state :challenger omar 0)
      (run-successful state)
      (prompt-choice :challenger "HQ")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= (:cid ash) (-> (get-challenger) :prompt first :card :cid)))
      (is (= :hq (-> (get-challenger) :register :successful-run first))))))

(deftest omar-crisium-grid
  ;; Omar Keung - Crisium Grid prevents prompt
  (do-game
    (new-game
      (default-contestant [(qty "Crisium Grid" 1)])
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Crisium Grid" "Archives")
    (take-credits state :contestant)
    (let [omar (get-in @state [:challenger :identity])
          cr (get-content state :archives 0)]
      (core/rez state :contestant cr)
      (card-ability state :challenger omar 0)
      (run-successful state)
      (is (= (:cid cr) (-> (get-challenger) :prompt first :card :cid)))
      (is (empty? (-> (get-challenger) :register :successful-run)))
      (is (= :archives (get-in @state [:run :server 0]))))))

(deftest omar-medium
  ;; Omar Keung - When selecting R&D, ability adds counters to Medium
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Medium" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Medium")
    (let [omar (get-in @state [:challenger :identity])
          medium (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger omar 0)
      (run-successful state)
      (prompt-choice :challenger "R&D")
      (is (= 1 (get-counters (refresh medium) :virus))))))

(deftest omar-nerve-agent
  ;; Omar Keung - When selecting HQ, ability adds counters to Nerve Agent
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Nerve Agent" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Nerve Agent")
    (let [omar (get-in @state [:challenger :identity])
          nerve (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger omar 0)
      (run-successful state)
      (prompt-choice :challenger "HQ")
      (is (= 1 (get-counters (refresh nerve) :virus))))))

(deftest quetzal-ability
  ;; Quetzal ability- once per turn
  (do-game
    (new-game
      (default-contestant [(qty "Ice Wall" 3)])
      (make-deck "Quetzal: Free Spirit" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (run-on state "HQ")
    (let [q (get-in @state [:challenger :identity])
          iwall (get-character state :hq 0)
          qdef (core/card-def (get-in @state [:challenger :identity]))]
      (core/rez state :contestant iwall)
      (card-ability state :challenger q 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg]))
          "Quetzal ability did trigger")
      (run-jack-out state)
      (core/click-credit state :challenger nil)
      (run-on state "HQ")
      (card-ability state :challenger (refresh q) 0)
      (is (not (last-log-contains? state (get-in qdef [:abilities 0 :msg])))
          "Quetzal ability did not trigger")
      (run-jack-out state)
      (take-credits state :challenger)
      (take-credits state :contestant)
      (core/click-credit state :challenger nil)
      (run-on state "HQ")
      (card-ability state :challenger (refresh q) 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg]))
          "Quetzal ability did trigger")
      (core/jack-out state :challenger nil))))

(deftest reina-rez-cost-increase
  ;; Reina Roja - Increase cost of first rezzed Character
  (do-game
    (new-game
      (default-contestant [(qty "Quandary" 3)])
      (make-deck "Reina Roja: Freedom Fighter" []))
    (play-from-hand state :contestant "Quandary" "R&D")
    (take-credits state :contestant)
    (is (= 7 (:credit (get-contestant))))
    (run-on state "R&D")
    (let [quan (get-character state :rd 0)]
      (core/rez state :contestant quan)
      (is (= 5 (:credit (get-contestant))) "Rez cost increased by 1"))))

(deftest rielle-kit-peddler-ability
  ;; Rielle "Kit" Peddler - Give Character Code Gate
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 2)])
              (make-deck "Rielle \"Kit\" Peddler: Transhuman" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Ice Wall" "HQ")
    (take-credits state :contestant)
    (run-on state "HQ")
    (let [k (get-in @state [:challenger :identity])
          iwall (get-character state :hq 0)]
      (core/rez state :contestant iwall)
      (card-ability state :challenger k 0)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate"))))

(deftest skorpios
  ; Remove a card from game when it moves to discard once per round
  (do-game
    (new-game (make-deck "Skorpios Defense Systems: Persuasive Power" [(qty "Hedge Fund" 1) (qty "Quandary" 4)])
              (default-challenger [(qty "The Maker's Eye" 1) (qty "Lucky Find" 1)]))
    (play-from-hand state :contestant "Hedge Fund")
    (dotimes [_ 4] (core/move state :contestant (first (:hand (get-contestant))) :deck))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Lucky Find")
    (play-from-hand state :challenger "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    ; Don't allow a run-event in progress to be targeted #2963
    (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
    (is (empty? (filter #(= "The Maker's Eye" (:title %)) (-> (get-contestant) :prompt first :choices))) "No Maker's Eye choice")
    (prompt-choice :contestant "Cancel")
    (run-successful state)
    (prompt-choice :challenger "Card from deck")
    (is (= "You accessed Quandary" (-> (get-challenger) :prompt first :msg)) "1st quandary")
    (prompt-choice :challenger "OK")
    (prompt-choice :challenger "Card from deck")
    (is (= "You accessed Quandary" (-> (get-challenger) :prompt first :msg)) "2nd quandary")
    (prompt-choice :challenger "OK")
    (prompt-choice :challenger "Card from deck")
    (is (= "You accessed Quandary" (-> (get-challenger) :prompt first :msg)) "3rd quandary")
    (prompt-choice :challenger "OK")
    (is (not (:run @state)))
    (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
    (prompt-choice :contestant (find-card "The Maker's Eye" (:discard (get-challenger))))
    (is (= 1 (count (get-in @state [:challenger :rfg]))) "One card RFGed")
    (card-ability state :contestant (get-in @state [:contestant :identity]) 0)
    (is (empty? (:prompt (get-contestant))) "Cannot use Skorpios twcharacter")))

(deftest silhouette-expose-trigger-before-access
  ;; Silhouette - Expose trigger ability resolves completely before access. Issue #2173.
  (do-game
    (new-game
      (default-contestant [(qty "Psychic Field" 1) (qty "Fetal AI" 10)])
      (make-deck "Silhouette: Stealth Operative" [(qty "Feedback Filter" 1) (qty "Inside Job" 1)]))
    (starting-hand state :contestant ["Psychic Field" "Fetal AI"])
    (play-from-hand state :contestant "Psychic Field" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Feedback Filter")
    (is (= 3 (:credit (get-challenger))) "Challenger has 3 credits")
    (let [psychic (get-content state :remote1 0)
          ff (get-hardware state 0)]
      (run-empty-server state :hq)
      (is (:run @state) "On successful run trigger effects")
      (prompt-select :challenger psychic)
      (is (= 1 (count (:hand (get-challenger)))) "Challenger has 1 card in hand")
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :challenger "0 [Credits]")
      (card-ability state :challenger ff 0)
      (prompt-choice :challenger "Done")
      (is (= 0 (:credit (get-challenger))) "Challenger has no more credits left")
      (is (= 1 (count (:hand (get-challenger)))) "Prevented 1 net damage")
      (is (empty? (:discard (get-challenger))) "No cards discarded")
      (is (:run @state) "On run access phase")
      (prompt-choice :challenger "Access")
      (prompt-choice :challenger "Done")
      (is (empty? (:hand (get-challenger))) "Suffered 1 net damage due to accessing Fetal AI")
      (is (= 1 (count (:discard (get-challenger)))) "Discarded 1 card due to net damage")
      (is (:run @state) "Resolving access triggers")
      (prompt-choice :challenger "Yes")
      (is (= 0 (count (:scored (get-challenger)))) "Challenger has no credits to be able to steal Fetal AI")
      (is (not (:run @state)) "Run has now ended")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest silhouette-temujin-weirdness
  ;; Silhouette - broken interaction with other successful-run triggers. Issue #1968.
  (do-game
    (new-game
      (default-contestant [(qty "PAD Campaign" 1) (qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "Beanstalk Royalties" 3)])
      (make-deck "Silhouette: Stealth Operative" [(qty "Temüjin Contract" 1) (qty "Desperado" 1)]))
    (starting-hand state :contestant ["Hedge Fund" "PAD Campaign"])
    (play-from-hand state :contestant "PAD Campaign" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Temüjin Contract")
    (prompt-choice :challenger "HQ")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (run-empty-server state :hq)
    (prompt-choice :challenger "Temüjin Contract")
    (prompt-select :challenger (get-content state :remote1 0))
    (prompt-choice :challenger "OK")
    (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
    (is (= 16 (get-counters (get-resource state 0) :credit)) "16 cr on Temujin")
    (is (= 8 (:credit (get-challenger))) "Gained 4cr")

    ;; second run
    (run-empty-server state :hq)
    (prompt-choice :challenger "OK")
    (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
    (is (= 12 (:credit (get-challenger))) "Gained 4cr")
    (is (= 12 (get-counters (get-resource state 0) :credit)) "12 cr on Temujin")))

(deftest spark-advertisements
  ;; Spark Agency - Rezzing advertisements
  (do-game
    (new-game
      (make-deck "Spark Agency: Worldswide Reach" [(qty "Launch Campaign" 3)])
      (default-challenger))
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (let [lc1 (get-content state :remote1 0)
          lc2 (get-content state :remote2 0)
          lc3 (get-content state :remote3 0)]
      (core/rez state :contestant lc1)
      (is (= 4 (:credit (get-challenger)))
          "Challenger lost 1 credit from rez of advertisement (Contestant turn)")
      (core/rez state :contestant lc3)
      (is (= 4 (:credit (get-challenger)))
          "Challenger did not lose credit from second Spark rez")
      (take-credits state :contestant)
      (run-on state "Server 1")
      (core/rez state :contestant lc2)
      (is (= 3 (:credit (get-challenger)))
          "Challenger lost 1 credit from rez of advertisement (Challenger turn)"))))

(deftest strategic-innovations-future-forward
  ;; Strategic Innovations: Future Forward - Ability
  (do-game
    (new-game
      (make-deck "Strategic Innovations: Future Forward" [(qty "Hedge Fund" 2)
                                                          (qty "Eli 1.0" 2)
                                                          (qty "Crick" 2)])
      (default-challenger))
    (play-from-hand state :contestant "Eli 1.0" "New remote")
    (play-from-hand state :contestant "Hedge Fund")
    (play-from-hand state :contestant "Crick" "New remote")
    (let [i1 (get-character state :remote1 0)
          i2 (get-character state :remote2 0)]
      (take-credits state :contestant 0)
      (take-credits state :challenger)
      (core/rez state :contestant i1)
      (take-credits state :contestant)
      (take-credits state :challenger)
      (is (= 1 (count (:prompt (get-contestant)))) "Contestant prompted to trigger Strategic Innovations")
      (prompt-select :contestant (first (:discard (get-contestant))))
      (is (empty? (:discard (get-contestant))) "Hedge Fund moved back to R&D")
      (take-credits state :contestant)
      (core/rez state :contestant i2)
      (take-credits state :challenger)
      (is (= 0 (count (:prompt (get-contestant))))
          "Contestant not prompted to trigger Strategic Innovations"))))

(deftest the-foundry-abt
  ;; The Foundry - interaction with Accelerated Beta Test
  (do-game
    (new-game
      (make-deck "The Foundry: Refining the Process" [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)])
      (default-challenger))
    (starting-hand state :contestant ["Accelerated Beta Test"])
    (play-from-hand state :contestant "Accelerated Beta Test" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (prompt-choice :contestant "Yes")
    (prompt-select :contestant (find-card "Eli 1.0" (:play-area (get-contestant))))
    (prompt-choice :contestant "Archives")
    (prompt-choice :contestant "Yes")
    (is (empty? (:play-area (get-contestant))) "Play area shuffled into R&D")))

(deftest titan-agenda-counter
  ;; Titan Transnational - Add a counter to a scored agenda
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Project Atlas" 1)])
      (default-challenger))
    (play-from-hand state :contestant "Project Atlas" "New remote")
    (let [atl (get-content state :remote1 0)]
      (core/gain state :contestant :click 1)
      (core/advance state :contestant {:card (refresh atl)})
      (core/advance state :contestant {:card (refresh atl)})
      (core/advance state :contestant {:card (refresh atl)})
      (core/score state :contestant {:card (refresh atl)})
      (let [scored (get-in @state [:contestant :scored 0])]
        (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))

(deftest titan-contestantorate-sales-team
  ;; Titan, only use one counter of Contestantorate Sales Team
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Contestantorate Sales Team" 1) (qty "Mark Yale" 1)])
      (default-challenger))
    (play-from-hand state :contestant "Contestantorate Sales Team" "New remote")
    (play-from-hand state :contestant "Mark Yale" "New remote")
    (let [cst (get-content state :remote1 0)
          my (get-content state :remote2 0)]
      (core/gain state :contestant :click 3)
      (core/advance state :contestant {:card (refresh cst)})
      (core/advance state :contestant {:card (refresh cst)})
      (core/advance state :contestant {:card (refresh cst)})
      (core/advance state :contestant {:card (refresh cst)})
      (core/score state :contestant {:card (refresh cst)})
      (let [scored (get-in @state [:contestant :scored 0])]
        (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
        (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
        (core/rez state :contestant my)
        (card-ability state :contestant my 1)
        (prompt-select :contestant (refresh scored))
        (is (= 0 (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
        (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
        (card-ability state :contestant my 1)
        (prompt-select :contestant (refresh scored))
        (is (= 0 (get-counters (refresh scored) :agenda)) "No agenda counter used by Mark Yale")
        (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")))))

(deftest weyland-builder
  ;; Builder of Nations - 1 meat damage per turn at most
  (do-game
    (new-game
      (make-deck "Weyland Consortium: Builder of Nations" [(qty "Hedge Fund" 3)])
      (default-challenger))
      (let [bon (get-in @state [:contestant :identity])]
        (card-ability state :contestant bon 0)
        (prompt-choice :contestant "Cancel")
        (is (= 0 (count (:discard (get-challenger)))) "Challenger took no meat damage from BoN")
        (card-ability state :contestant bon 0)
        (prompt-choice :contestant "Yes")
        (is (= 1 (count (:discard (get-challenger)))) "Challenger took 1 meat damage from BoN")
        (card-ability state :contestant bon 0)
        (is (= 1 (count (:discard (get-challenger)))) "Challenger took only 1 meat damage from BoN total")
        (is (= 0 (count (:prompt (get-contestant))))))))

(deftest weyland-builder-cleaners
  ;; Builder of Nations - 2 meat damage from ID ability when The Cleaners is scored
  (do-game
    (new-game
      (make-deck "Weyland Consortium: Builder of Nations" [(qty "The Cleaners" 3) (qty "Ice Wall" 3)])
      (default-challenger [(qty "Sure Gamble" 2)]))
    (play-from-hand state :contestant "The Cleaners" "New remote")
    (let [clean (get-content state :remote1 0)]
      (score-agenda state :contestant clean)
    (let [bon (get-in @state [:contestant :identity])]
      (card-ability state :contestant bon 0)
      (prompt-choice :contestant "Yes")
      (is (= 2 (count (:discard (get-challenger)))) "Challenger took 2 meat damage from BoN/Cleaners combo")))))

(deftest whizzard
  ;; Whizzard - Recurring credits
  (do-game
    (new-game (default-contestant) (make-deck "Whizzard: Master Gamer" ["Sure Gamble"]))

    (let [click-whizzard (fn [n] (dotimes [i n] (card-ability state :challenger (:identity (get-challenger)) 0)))]
      (is (changes-credits (get-challenger) 1 (click-whizzard 1)))
      (is (changes-credits (get-challenger) 2 (click-whizzard 5)) "Can't take more than 3 Whizzard credits")

      (take-credits state :contestant)
      (is (changes-credits (get-challenger) 3 (click-whizzard 3)) "Credits reset at start of Challenger's turn")

      (take-credits state :challenger)
      (is (changes-credits (get-challenger) 0 (click-whizzard 1)) "Credits don't reset at start of Contestant's turn"))))

(deftest wyvern-chemically-enhanced
  ;; Wyvern: Chemically Enhanced - Ability
  (do-game
    (new-game (default-contestant [(qty "Launch Campaign" 3)])
              (make-deck "Wyvern: Chemically Enhanced" [(qty "Sure Gamble" 2)
                                                        (qty "Corroder" 1)
                                                        (qty "Clone Chip" 1)
                                                        (qty "Easy Mark" 1)]))
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (play-from-hand state :contestant "Launch Campaign" "New remote")
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Easy Mark")
    (play-from-hand state :challenger "Corroder")
    (run-empty-server state "Server 1")
    (prompt-choice :challenger "Yes")
    ;; trash Launch Campaign, should trigger wyvern
    (is (= "Sure Gamble" (:title (last (:discard (get-challenger)))))
        "Sure Gamble still in Wyvern's discard")
    (is (some #(= "Easy Mark" (:title %)) (:deck (get-challenger))) "Easy Mark moved to deck")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Clone Chip")
    (run-empty-server state "Server 2")
    (prompt-choice :challenger "Yes")
    (is (= "Sure Gamble" (:title (last (:discard (get-challenger)))))
        "Sure Gamble still in Wyvern's discard")))
