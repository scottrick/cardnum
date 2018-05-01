(ns test.cards.identities
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adam-directives
  ;; Adam - Allow hero to choose directives
  (do-game
    (new-game
      (default-minion)
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (is (= 4 (count (get-in @state [:hero :play-area]))) "All directives are in the hero's play area")
    (is (= 0 (count (get-in @state [:hero :hand]))))
    (prompt-select :hero (find-card "Neutralize All Threats" (get-in @state [:hero :play-area])))
    (prompt-select :hero (find-card "Safety First" (get-in @state [:hero :play-area])))
    (prompt-select :hero (find-card "Always Be Running" (get-in @state [:hero :play-area])))
    (is (= 3 (count (get-in @state [:hero :rig :resource]))) "3 directives were installed")
    (is (= 0 (count (get-in @state [:hero :play-area]))) "The play area is empty")
    (let [nat (find-card "Neutralize All Threats" (get-in @state [:hero :rig :resource]))
          sf (find-card "Safety First" (get-in @state [:hero :rig :resource]))
          abr (find-card "Always Be Running" (get-in @state [:hero :rig :resource]))]
      (is (and nat sf abr) "The chosen directives were installed"))))

(deftest adam-palana
  ;; Adam - Directives should not grant Pālanā credits.
  (do-game
    (new-game
      (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (prompt-select :hero (find-card "Neutralize All Threats" (get-in @state [:hero :play-area])))
    (prompt-select :hero (find-card "Safety First" (get-in @state [:hero :play-area])))
    (prompt-select :hero (find-card "Always Be Running" (get-in @state [:hero :play-area])))
    (prompt-choice :minion "Keep")
    (prompt-choice :hero "Keep")
    (core/start-turn state :minion nil)
    (is (= 5 (:credit (get-minion))) "Pālanā does not gain credit from Adam's starting Directives")))

(deftest adam-advanceable-traps
  ;; Adam - Neutralize All Threats interaction with advanceable traps.
  (do-game
    (new-game
      (default-minion [(qty "Cerebral Overwriter" 3)])
      (make-deck "Adam: Compulsive Hacker" [(qty "Sure Gamble" 3)])
      {:dont-start-game true})
    (prompt-select :hero (find-card "Neutralize All Threats" (get-in @state [:hero :play-area])))
    (prompt-select :hero (find-card "Safety First" (get-in @state [:hero :play-area])))
    (prompt-select :hero (find-card "Always Be Running" (get-in @state [:hero :play-area])))
    (prompt-choice :minion "Keep")
    (prompt-choice :hero "Keep")
    (core/start-turn state :minion nil)

    (play-from-hand state :minion "Cerebral Overwriter" "New remote")
    (advance state (get-content state :remote1 0) 2)
    (take-credits state :minion)
    (run-empty-server state :remote1)
    (prompt-choice :hero "No") ; Dismiss prompt from non-exiled Find the Truth directive
    (prompt-choice :minion "Yes")
    (is (= 2 (:brain-damage (get-hero))) "Runner took 2 brain damage")
    (is (= 1 (count (:discard (get-minion)))) "1 card in archives")))

(deftest andromeda
  ;; Andromeda - 9 card starting hand, 1 link
  (do-game
    (new-game
      (default-minion)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 1 (:link (get-hero))) "1 link")
    (is (= 9 (count (:hand (get-hero)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-mulligan
  ;; Andromeda - 9 card starting hand after mulligan
  (do-game
    (new-game
      (default-minion)
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)])
      {:mulligan :hero})
    (is (= 1 (:link (get-hero))) "1 link")
    (is (= 9 (count (:hand (get-hero)))) "9 cards in Andromeda starting hand")))

(deftest andromeda-palana
  ;; Andromeda - should not grant Palana credits.
  (do-game
    (new-game
      (make-deck "Pālanā Foods: Sustainable Growth" [(qty "Hedge Fund" 3)])
      (make-deck "Andromeda: Dispossessed Ristie" [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                                   (qty "Security Testing" 3) (qty "Bank Job" 3)]))
    (is (= 5 (:credit (get-minion))) "Palana does not gain credit from Andromeda's starting hand")))

(deftest apex-facedown-console
  ;; Apex - Allow facedown install of a second console. Issue #1326
  (do-game
    (new-game
      (default-minion)
      (make-deck "Apex: Invasive Predator" [(qty "Heartbeat" 2)]))
    (take-credits state :minion)
    (core/end-phase-12 state :hero nil)
    (prompt-choice :hero "Done") ; no facedown install on turn 1
    (play-from-hand state :hero "Heartbeat")
    (is (= 1 (count (get-in @state [:hero :rig :hardware]))))
    (take-credits state :hero)
    (take-credits state :minion)
    (core/end-phase-12 state :hero nil)
    (prompt-select :hero (find-card "Heartbeat" (:hand (get-hero))))
    (is (= 1 (count (get-in @state [:hero :rig :facedown]))) "2nd console installed facedown")))

(deftest ayla
  ;; Ayla - choose & use cards for NVRAM
  (do-game
    (new-game
      (default-minion)
      (make-deck "Ayla \"Bios\" Rahim: Simulant Specialist" [(qty "Sure Gamble" 1) (qty "Desperado" 1)
                                                             (qty "Security Testing" 1) (qty "Bank Job" 1)
                                                             (qty "Heartbeat" 1) (qty "Eater" 1)])
      {:dont-start-game true})
    (is (= 6 (count (get-in @state [:hero :play-area]))) "Deck cards are in play area")
    (is (= 0 (count (get-in @state [:hero :hand]))))
    (prompt-select :hero (find-card "Sure Gamble" (get-in @state [:hero :play-area])))
    (prompt-select :hero (find-card "Desperado" (get-in @state [:hero :play-area])))
    (prompt-select :hero (find-card "Bank Job" (get-in @state [:hero :play-area])))
    (prompt-select :hero (find-card "Eater" (get-in @state [:hero :play-area])))
    (is (= 4 (count (:hosted (:identity (get-hero))))) "4 cards in NVRAM")
    (is (= 0 (count (get-in @state [:hero :play-area]))) "The play area is empty")
    (prompt-choice :minion "Keep")
    (prompt-choice :hero "Keep")
    (take-credits state :minion)
    (is (= 2 (count (get-in @state [:hero :hand]))) "There are 2 cards in the hero's Grip")
    (card-ability state :hero (:identity (get-hero)) 0)
    (prompt-card :hero (find-card "Bank Job" (:hosted (:identity (get-hero)))))
    (is (= 3 (count (get-in @state [:hero :hand]))) "There are 3 cards in the hero's Grip")))

(deftest cerebral-imaging-max-hand-size
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game
      (make-deck "Cerebral Imaging: Infinite Frontiers" [(qty "Hedge Fund" 3)])
      (default-hero))
    (play-from-hand state :minion "Hedge Fund")
    (play-from-hand state :minion "Hedge Fund")
    (is (= 13 (:credit (get-minion))) "Has 13 credits")
    (is (= 13 (core/hand-size state :minion)) "Max hand size is 13")))

(deftest chronos-protocol
  ;; Chronos Protocol - Choose Runner discard for first net damage of a turn
  (do-game
    (new-game
      (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1) (qty "Neural EMP" 2)])
      (default-hero [(qty "Imp" 3)]))
    (play-from-hand state :minion "Pup" "HQ")
    (take-credits state :minion)
    (run-on state :hq)
    (let [pup (get-ice state :hq 0)]
      (core/rez state :minion pup)
      (card-subroutine state :minion pup 0)
      (prompt-choice :minion "Yes")
      (let [imp (find-card "Imp" (:hand (get-hero)))]
        (prompt-choice :minion imp)
        (is (= 1 (count (:discard (get-hero)))))
        (card-subroutine state :minion pup 0)
        (is (empty? (:prompt (get-minion))) "No choice on second net damage")
        (is (= 2 (count (:discard (get-hero)))))
        (run-jack-out state)
        (take-credits state :hero)
        (core/move state :hero (find-card "Imp" (:discard (get-hero))) :hand)
        (play-from-hand state :minion "Neural EMP")
        (prompt-choice :minion "No")
        (is (= 2 (count (:discard (get-hero)))) "Damage dealt after declining ability")
        (play-from-hand state :minion "Neural EMP")
        (is (empty? (:prompt (get-minion))) "No choice after declining on first damage")
        (is (= 3 (count (:discard (get-hero)))))))))

(deftest chronos-protocol-employee-strike
  ;; Chronos Protocol - Issue #1958 also affects Chronos Protocol
  (do-game
    (new-game
      (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1)])
      (default-hero [(qty "Employee Strike" 1) (qty "Scrubbed" 3) (qty "Sure Gamble" 1)]))
    (play-from-hand state :minion "Pup" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Employee Strike")
    (run-on state :hq)
    (let [pup (get-ice state :hq 0)]
      (core/rez state :minion pup)
      (card-subroutine state :minion pup 0)
      (is (empty? (:prompt (get-minion))) "No choice because of Employee Strike")
      (card-subroutine state :minion pup 0)
      (is (= 2 (count (:discard (get-hero)))))
      (run-jack-out state)
      (take-credits state :hero)
      (take-credits state :minion)
      (play-from-hand state :hero "Scrubbed")
      (run-on state :hq)
      (card-subroutine state :minion pup 0)
      (is (not (empty? (:prompt (get-minion)))) "Employee Strike out of play - Ability turned on correctly"))))

(deftest edward-kim
  ;; Edward Kim - Trash first operation accessed each turn, but not if first one was in Archives
  (do-game
    (new-game
      (default-minion [(qty "Hedge Fund" 3) (qty "Restructure" 2) (qty "PAD Campaign" 1)])
      (make-deck "Edward Kim: Humanity's Hammer" [(qty "Eater" 1) (qty "Sure Gamble" 2)]))
    (play-from-hand state :minion "Hedge Fund")
    (trash-from-hand state :minion "PAD Campaign")
    (take-credits state :minion)
    (run-empty-server state "Archives")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-minion)))) "No operation trashed from HQ; accessed one in Archives first")
    (take-credits state :hero)
    (core/move state :minion (find-card "Hedge Fund" (:discard (get-minion))) :hand)
    (is (= 1 (count (:discard (get-minion)))))
    (take-credits state :minion)
    (run-empty-server state "Archives")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-minion)))) "1 operation trashed from HQ; accessed non-operation in Archives first")
    (take-credits state :hero)
    (play-from-hand state :minion "Hedge Fund")
    (take-credits state :minion)
    (play-from-hand state :hero "Eater")
    (let [eater (get-in @state [:hero :rig :program 0])]
      (run-on state "Archives")
      (card-ability state :hero eater 0) ; pretend to break a sub so no cards in Archives will be accessed
      (run-successful state)
      (is (= 3 (count (:discard (get-minion)))))
      (run-empty-server state "HQ")
      (is (= 4 (count (:discard (get-minion)))) "1 operation trashed from HQ; accessed non-operation in Archives first"))))

(deftest edward-kim-maw
  ;; Edward Kim - Do not trigger maw on first Operation access (due to trash)
  (do-game
    (new-game
      (default-minion [(qty "Hedge Fund" 3) (qty "Restructure" 2)])
      (make-deck "Edward Kim: Humanity's Hammer" [(qty "Maw" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Sure Gamble")
    (play-from-hand state :hero "Maw")
    (is (= 0 (count (:discard (get-minion)))) "No cards in Archives")
    (run-empty-server state "HQ")
    (is (= 1 (count (:discard (get-minion)))) "Only one card trashed from HQ, by Ed Kim")
    (run-empty-server state "HQ")
    (is (= 2 (count (:discard (get-minion)))) "One more card trashed from HQ, by Maw")))


(deftest exile-customized-secretary
  ;; Exile - simultaneous-resolution prompt shown for interaction with Customized Secretary
  (do-game
    (new-game
      (default-minion)
      (make-deck "Exile: Streethawk" [(qty "Customized Secretary" 3) (qty "Clone Chip" 3)
                                      (qty "Sure Gamble" 3)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Customized Secretary" "Clone Chip"])
    (trash-from-hand state :hero "Customized Secretary")
    (play-from-hand state :hero "Clone Chip")
    (card-ability state :hero (get-hardware state 0) 0)
    (prompt-select :hero (find-card "Customized Secretary" (:discard (get-hero))))
    ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
    (is (= 2 (-> (get-hero) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
    (prompt-choice :hero "Exile: Streethawk")
    (is (= 1 (count (:hand (get-hero)))) "Exile drew a card")))

(deftest gabriel-santiago
  ;; Gabriel Santiago - Gain 2c on first successful HQ run each turn
  (do-game
    (new-game
      (default-minion)
      (make-deck "Gabriel Santiago: Consummate Professional" [(qty "Easy Mark" 1)]))
    (take-credits state :minion)
    (run-empty-server state :rd)
    (is (= 5 (:credit (get-hero))) "No credits gained")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-hero))) "Gained 2c")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-hero))) "No credits gained")))

(deftest gagarin
  ;; Gagarin - pay 1c to access each card in remote
  (do-game
    (new-game
      (make-deck "Gagarin Deep Space: Expanding the Horizon" [(qty "PAD Campaign" 1) (qty "Caprice Nisei" 1)])
      (default-hero))
    (core/lose state :hero :credit 4)
    (is (= 1 (:credit (get-hero))) "Runner has 1 credit")
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (take-credits state :minion)
    (run-empty-server state :remote1)
    (prompt-select :hero (get-content state :remote1 0))
    (is (= 0 (:credit (get-hero))) "Paid 1 credit to access")
    (prompt-choice :hero "No") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-server state :remote1)
    (prompt-select :hero (get-content state :remote1 0))
    (prompt-choice :hero "OK") ; Could not afford message dismissed
    (is (empty? (:prompt (get-hero))) "Runner cannot access so no trash prompt")
    (is (not (last-log-contains? state "PAD Campaign")) "No card name was logged")
    (run-empty-server state :hq)
    (prompt-choice :hero "No") ; Dismiss trash prompt
    (is (last-log-contains? state "Caprice") "Accessed card name was logged")))

(deftest grndl-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (default-hero))
    (is (= 10 (:credit (get-minion))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-minion))) "GRNDL starts with 1 bad publicity")))

(deftest grndl-valencia
  ;; GRNDL vs Valencia - only 1 bad pub at start
  (do-game
    (new-game
      (make-deck "GRNDL: Power Unleashed" [(qty "Hedge Fund" 3)])
      (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Sure Gamble" 3)]))
    (is (= 10 (:credit (get-minion))) "GRNDL starts with 10 credits")
    (is (= 1 (:bad-publicity (get-minion))) "GRNDL starts with 1 bad publicity")))

(deftest haarpsichord-studios
  ;; Haarpsichord Studios - Prevent stealing more than 1 agenda per turn
  (do-game
    (new-game
      (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
      (default-hero [(qty "Gang Sign" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Gang Sign")
    (run-empty-server state "HQ")
    (prompt-choice :hero "Steal")
    (is (= 1 (:agenda-point (get-hero))))
    (run-empty-server state "HQ")
    (prompt-choice :hero "Steal")
    (is (= 1 (:agenda-point (get-hero))) "Second steal of turn prevented")
    (take-credits state :hero)
    (play-from-hand state :minion "15 Minutes" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    (prompt-choice :hero "Card from hand")
    (prompt-choice :hero "Steal")
    (is (= 2 (:agenda-point (get-hero))) "Steal prevention didn't carry over to Corp turn")))

(deftest haarpsichord-studios-employee-strike
  ;; Haarpsichord Studios - Interactions with Employee Strike. Issue #1313.
  (do-game
    (new-game
      (make-deck "Haarpsichord Studios: Entertainment Unleashed" [(qty "15 Minutes" 3)])
      (default-hero [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :click 5)
    (run-empty-server state "HQ")
    (prompt-choice :hero "Steal")
    (is (= 1 (:agenda-point (get-hero))))
    (play-from-hand state :hero "Employee Strike")
    (run-empty-server state "HQ")
    (prompt-choice :hero "Steal")
    (is (= 2 (:agenda-point (get-hero))) "Second steal not prevented")
    (play-from-hand state :hero "Scrubbed")
    (run-empty-server state "HQ")
    (prompt-choice :hero "Steal")
    (is (= 2 (:agenda-point (get-hero))) "Third steal prevented")))

(deftest haas-bioroid-architects-of-tomorrow
  ;; Architects of Tomorrow - prompt to rez after passing bioroid
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Architects of Tomorrow" [(qty "Eli 1.0" 2) (qty "Pup" 1)])
      (default-hero))
    (core/gain state :minion :credit 3)
    (play-from-hand state :minion "Eli 1.0" "Archives")
    (play-from-hand state :minion "Pup" "Archives")
    (play-from-hand state :minion "Eli 1.0" "HQ")
    (take-credits state :minion)
    (run-on state "Archives")
    (core/rez state :minion (get-ice state :archives 1))
    (run-continue state)
    (core/rez state :minion (get-ice state :archives 0))
    (is (= 3 (:credit (get-minion))) "Corp has 3 credits after rezzing Eli 1.0")
    (run-continue state)
    (prompt-select :minion (get-ice state :hq 0))
    (is (= 3 (:credit (get-minion))) "Corp not charged for Architects of Tomorrow rez of Eli 1.0")))

(deftest haas-bioroid-asa-group
  ;; Asa Group - don't allow installation of operations
  (do-game
    (new-game
      (make-deck "Asa Group: Security Through Vigilance" [(qty "Pup" 1) (qty "BOOM!" 1) (qty "Urban Renewal" 1)])
      (default-hero))
    (play-from-hand state :minion "Pup" "New remote")
    (prompt-select :minion (find-card "BOOM!" (:hand (get-minion))))
    (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
    (prompt-select :minion (find-card "Urban Renewal" (:hand (get-minion))))
    (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))

(deftest haas-bioroid-engineering-the-future-employee-strike
  ;; EtF - interaction with Employee Strike
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Engineering the Future" [(qty "Eli 1.0" 3) (qty "Paywall Implementation" 1)])
      (default-hero [(qty "Employee Strike" 1)]))
    (take-credits state :minion)
    (is (= 8 (:credit (get-minion))) "Corp has 8 credits at turn end")
    (play-from-hand state :hero "Employee Strike")
    (take-credits state :hero)
    (play-from-hand state :minion "Eli 1.0" "New remote")
    (is (= 8 (:credit (get-minion))) "Corp did not gain 1cr from EtF")
    (play-from-hand state :minion "Paywall Implementation")
    (play-from-hand state :minion "Eli 1.0" "New remote")
    (is (= 8 (:credit (get-minion))) "Corp did not gain 1cr from EtF")
    (take-credits state :minion)
    (take-credits state :hero)
    (play-from-hand state :minion "Eli 1.0" "New remote")
    (is (= 9 (:credit (get-minion))) "Corp gained 1cr from EtF")))

(deftest haas-bioroid-stronger-together
  ;; Stronger Together - +1 strength for Bioroid ice
  (do-game
    (new-game
      (make-deck "Haas-Bioroid: Stronger Together" [(qty "Eli 1.0" 1)])
      (default-hero))
    (play-from-hand state :minion "Eli 1.0" "Archives")
    (let [eli (get-ice state :archives 0)]
      (core/rez state :minion eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest iain-stirling-credits
  ;; Iain Stirling - Gain 2 credits when behind
  (do-game
    (new-game
      (default-minion [(qty "Breaking News" 1)])
      (make-deck "Iain Stirling: Retired Spook" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Breaking News" "New remote")
    (let [ag1 (get-in @state [:minion :servers :remote1 :content 0])]
      (core/advance state :minion {:card (refresh ag1)})
      (core/advance state :minion {:card (refresh ag1)})
      (core/score state :minion {:card (refresh ag1)})
      (take-credits state :minion)
      (is (= 1 (:agenda-point (get-minion))) "Corp gains 1 agenda point from Breaking News")
      (take-credits state :hero 1)
      (is (= 8 (:credit (get-hero))) "Gained 2 credits from being behind on points"))))

(deftest industrial-genomics-trash-cost
  ;; Industrial Genomics - Increase trash cost
  (do-game
    (new-game
      (make-deck "Industrial Genomics: Growing Solutions" [(qty "PAD Campaign" 3)
                                                           (qty "Hedge Fund" 3)])
      (default-hero))
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (trash-from-hand state :minion "PAD Campaign")
    (trash-from-hand state :minion "PAD Campaign")
    (trash-from-hand state :minion "Hedge Fund")
    (trash-from-hand state :minion "Hedge Fund")
    (let [pad (get-content state :remote1 0)]
      (core/rez state :minion pad)
      (take-credits state :minion)
      (run-empty-server state "Server 1")
      (is (= 8 (core/trash-cost state :hero (refresh pad)))))))

(deftest jemison-astronautics
  ;; Jemison Astronautics - Place advancements when forfeiting agendas
  (do-game
    (new-game
      (make-deck "Jemison Astronautics: Sacrifice. Audacity. Success." [(qty "Enforcer 1.0" 1) (qty "Hostile Takeover" 1)
                                                                        (qty "Ice Wall" 1) (qty "Global Food Initiative" 1)])
      (default-hero [(qty "Data Dealer" 1)]))
    (play-from-hand state :minion "Enforcer 1.0" "HQ")
    (play-from-hand state :minion "Ice Wall" "R&D")
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (let [enf (get-ice state :hq 0)
          iwall (get-ice state :rd 0)]
      (take-credits state :minion)
      (play-from-hand state :hero "Data Dealer")
      (run-empty-server state "Server 1")
      (prompt-choice :hero "Steal")
      (let [dd (get-resource state 0)]
        (card-ability state :hero dd 0)
        (prompt-select :hero (get-in (get-hero) [:scored 0]))
        (is (empty? (:prompt (get-minion))) "No Jemison prompt for Runner forfeit")
        (take-credits state :hero)
        (play-from-hand state :minion "Global Food Initiative" "New remote")
        (score-agenda state :minion (get-content state :remote2 0))
        (core/rez state :minion enf)
        (prompt-select :minion (get-in (get-minion) [:scored 0]))
        (prompt-select :minion iwall)
        (is (= 4 (:advance-counter (refresh iwall))) "Jemison placed 4 advancements")))))

(deftest jesminder-sareen-ability
  ;; Jesminder Sareen - avoid tags only during a run
  (do-game
    (new-game (default-minion [(qty "SEA Source" 1) (qty "Data Raven" 1)])
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Data Raven" "Archives")
    (take-credits state :minion)
    (let [dr (-> @state :minion :servers :archives :ices first)]
      (core/rez state :minion dr)
      (core/click-run state :hero {:server "Archives"})
      (card-ability state :minion dr 0)
      (is (= 0 (:tag (get-hero))) "Jesminder avoided first tag during the run")
      (card-ability state :minion dr 0)
      (is (= 1 (:tag (get-hero))) "Jesminder did not avoid the second tag during the run")
      (core/no-action state :minion nil)
      (core/continue state :hero nil)
      (core/no-action state :minion nil)
      (core/successful-run state :hero nil)
      (run-empty-server state "R&D") ; clear per-run buffer
      (take-credits state :hero)
      (play-from-hand state :minion "SEA Source")
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (= 2 (:tag (get-hero))) "Jesminder did not avoid the tag outside of a run"))))

(deftest jesminder-john-masanori
  ;; Jesminder Sareen - don't avoid John Masanori tag
  (do-game
    (new-game (default-minion)
              (make-deck "Jesminder Sareen: Girl Behind the Curtain" [(qty "John Masanori" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "John Masanori")
    (run-on state "HQ")
    (core/jack-out state :hero nil)
    (is (= 1 (:tag (get-hero))) "Jesminder did not avoid John Masanori tag")))

(deftest jinteki-biotech-brewery
  ;; Jinteki Biotech - Brewery net damage
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Braintrust" 1)])
      (default-hero)
      {:dont-start-turn true})
    (prompt-choice :minion "The Brewery")
    (core/start-turn state :minion nil)
    (card-ability state :minion (:identity (get-minion)) 1)
    (is (= 1 (count (:hand (get-hero)))) "Runner took 2 net damage from Brewery flip")))

(deftest jinteki-biotech-greenhouse
  ;; Jinteki Biotech - Greenhouse four advancement tokens
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Braintrust" 1)])
      (default-hero)
      {:dont-start-turn true})
    (prompt-choice :minion "The Greenhouse")
    (core/start-turn state :minion nil)
    (play-from-hand state :minion "Braintrust" "New remote")
    (take-credits state :minion)
    (take-credits state :hero)
    (let [bt (get-content state :remote1 0)]
      (is (nil? (:advance-counter (refresh bt))) "No advancement counters on agenda")
      (card-ability state :minion (:identity (get-minion)) 1)
      (prompt-select :minion (refresh bt))
      (is (= 4 (:advance-counter (refresh bt))) "Four advancement counters on agenda"))))

(deftest jinteki-biotech-tank
  ;; Jinteki Biotech - Tank shuffle Archives into R&D
  (do-game
    (new-game
      (make-deck "Jinteki Biotech: Life Imagined" [(qty "Hedge Fund" 3)])
      (default-hero)
      {:dont-start-turn true})
    (prompt-choice :minion "The Tank")
    (core/start-turn state :minion nil)
    (play-from-hand state :minion "Hedge Fund")
    (play-from-hand state :minion "Hedge Fund")
    (play-from-hand state :minion "Hedge Fund")
    (take-credits state :hero)
    (is (= 3 (count (:discard (get-minion)))) "Archives started with 3 cards")
    (is (= 0 (count (:deck (get-minion)))) "R&D started empty")
    (card-ability state :minion (:identity (get-minion)) 1)
    (is (= 0 (count (:discard (get-minion)))) "Archives ended empty")
    (is (= 3 (count (:deck (get-minion)))) "R&D ended with 3 cards")))

(deftest jinteki-personal-evolution
  ;; Personal Evolution - Prevent hero from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Personal Evolution" [(qty "Braintrust" 6)])
      (default-hero [(qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Braintrust" "New remote")
    (take-credits state :minion)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Steal")
    (is (= 2 (count (:hand (get-hero)))) "Runner took 1 net damage from steal")))

(deftest jinteki-potential-unleashed
  ;; PU - when the hero takes at least one net damage, mill 1 from their deck
  (do-game
    (new-game (make-deck "Jinteki: Potential Unleashed" [(qty "Philotic Entanglement" 1) (qty "Neural EMP" 1) (qty "Braintrust" 3)])
              (default-hero [(qty "Employee Strike" 10)]))
    (play-from-hand state :minion "Braintrust" "New remote")
    (play-from-hand state :minion "Braintrust" "New remote")
    (take-credits state :minion)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Steal")
    (run-empty-server state "Server 2")
    (prompt-choice :hero "Steal")
    (take-credits state :hero)
    (play-from-hand state :minion "Philotic Entanglement" "New remote")
    (score-agenda state :minion (get-content state :remote3 0))
    (is (= 3 (count (:discard (get-hero)))))
    (play-from-hand state :minion "Neural EMP")
    (is (= 5 (count (:discard (get-hero)))))))

(deftest jinteki-replicating-perfection
  ;; Replicating Perfection - Prevent hero from running on remotes unless they first run on a central
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-hero))
    (play-from-hand state :minion "Mental Health Clinic" "New remote")
    (take-credits state :minion)
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
    (run-empty-server state "HQ")
    (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")))

(deftest jinteki-replicating-perfection-employee-strike
  ;; Replicating Perfection - interaction with Employee Strike. Issue #1313 and #1956.
  (do-game
    (new-game
      (make-deck "Jinteki: Replicating Perfection" [(qty "Mental Health Clinic" 3)])
      (default-hero [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (play-from-hand state :minion "Mental Health Clinic" "New remote")
    (take-credits state :minion)
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
    (play-from-hand state :hero "Employee Strike")
    (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")
    (play-from-hand state :hero "Scrubbed")
    (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")))

(deftest kate-mac-mccaffrey-discount
  ;; Kate 'Mac' McCaffrey - Install discount
  (do-game
    (new-game (default-minion)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Magnum Opus")
    (is (= 1 (:credit (get-hero))) "Installed Magnum Opus for 4 credits")))

(deftest kate-mac-mccaffrey-no-discount
  ;; Kate 'Mac' McCaffrey - No discount for 0 cost
  (do-game
    (new-game (default-minion)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker"
                         [(qty "Magnum Opus" 1)
                          (qty "Self-modifying Code" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Self-modifying Code")
    (play-from-hand state :hero "Magnum Opus")
    (is (= 0 (:credit (get-hero))) "No Kate discount on second program install")))

(deftest kate-mac-mccaffrey-discount-cant-afford
  ;; Kate 'Mac' McCaffrey - Can Only Afford With the Discount
  (do-game
    (new-game (default-minion)
              (make-deck "Kate \"Mac\" McCaffrey: Digital Tinker" [(qty "Magnum Opus" 1)]))
    (take-credits state :minion)
    (core/lose state :hero :credit 1)
    (is (= 4 (:credit (get-hero))))
    (play-from-hand state :hero "Magnum Opus")
    (is (= 1 (count (get-in @state [:hero :rig :program]))) "Magnum Opus installed")
    (is (= 0 (:credit (get-hero))) "Installed Magnum Opus for 4 credits")))

(deftest ken-tenma-run-event-credit
  ;; Ken 'Express' Tenma - Gain 1 credit when first Run event played
  (do-game
    (new-game (default-minion)
              (make-deck "Ken \"Express\" Tenma: Disappeared Clone" [(qty "Account Siphon" 2)]))
    (take-credits state :minion)
    (play-run-event state (first (:hand (get-hero))) :hq)
    (is (= 6 (:credit (get-hero))) "Gained 1 credit for first Run event")
    (prompt-choice :hero "Run ability")
    (play-run-event state (first (:hand (get-hero))) :hq)
    (is (= 16 (:credit (get-hero))) "No credit gained for second Run event")))

(deftest khan-vs-caprice
  ;; Khan - proper order of events when vs. Caprice
  (do-game
    (new-game
      (default-minion [(qty "Eli 1.0" 1) (qty "Caprice Nisei" 1)])
      (make-deck "Khan: Savvy Skiptracer" [(qty "Corroder" 1)]))
    (play-from-hand state :minion "Eli 1.0" "Archives")
    (play-from-hand state :minion "Caprice Nisei" "Archives")
    (core/rez state :minion (get-content state :archives 0))
    (take-credits state :minion)
    (run-on state "Archives")
    (run-continue state)
    (is (and (empty? (:prompt (get-minion)))
             (= 1 (count (:prompt (get-hero))))
             (= "Khan: Savvy Skiptracer" (-> (get-hero) :prompt first :card :title)))
        "Only Khan prompt showing")
    (prompt-select :hero (first (:hand (get-hero))))
    (is (find-card "Corroder" (-> (get-hero) :rig :program)) "Corroder installed")
    (is (= 4 (:credit (get-hero))) "1cr discount from Khan")
    (is (= "Caprice Nisei" (-> (get-hero) :prompt first :card :title)) "Caprice prompt showing")
    (prompt-choice :hero "0 [Credits]")
    (prompt-choice :minion "1 [Credits]")
    (is (not (:run @state)) "Run ended")))

(deftest laramy-fisk-shards
  ;; Laramy Fisk - installing a Shard should still give option to force Corp draw.
  (do-game
    (new-game
      (default-minion [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3)])
      (make-deck "Laramy Fisk: Savvy Investor" [(qty "Eden Shard" 1)]))
    (starting-hand state :minion ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Eli 1.0" "Eli 1.0"])
    (take-credits state :minion)
    (run-on state "R&D")
    (core/no-action state :minion nil)
    ;; at Successful Run stage -- click Eden Shard to install
    (play-from-hand state :hero "Eden Shard")
    (is (= 5 (:credit (get-hero))) "Eden Shard install was free")
    (is (= "Eden Shard" (:title (get-resource state 0))) "Eden Shard installed")
    (is (= "Identity" (-> (get-hero) :prompt first :card :type)) "Fisk prompt showing")
    (prompt-choice :hero "Yes")
    (is (not (:run @state)) "Run ended")
    (is (= 6 (count (:hand (get-minion)))) "Corp forced to draw")))

(deftest leela-gang-sign-complicated
  ;; Leela Patel - complicated interaction with mutiple Gang Sign
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Project Atlas" 1)
                                                                  (qty "Hostile Takeover" 1)
                                                                  (qty "Geothermal Fracking" 1)])
      (make-deck "Leela Patel: Trained Pragmatist" [(qty "Gang Sign" 2)]))
    (play-from-hand state :minion "Project Atlas" "New remote")
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (play-from-hand state :minion "Geothermal Fracking" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Gang Sign")
    (play-from-hand state :hero "Gang Sign")
    (take-credits state :hero)
    (score-agenda state :minion (get-content state :remote1 0))
    (prompt-choice :hero "Leela Patel: Trained Pragmatist")
    (prompt-select :hero (get-content state :remote2 0))
    (is (find-card "Hostile Takeover" (:hand (get-minion))) "Hostile Takeover returned to hand")
    (prompt-choice :hero "Gang Sign")
    (prompt-choice :hero "Card from hand")
    (prompt-choice :hero "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-hero))) "Hostile Takeover stolen with Gang Sign")
    (prompt-select :hero (get-content state :remote3 0))
    (is (find-card "Geothermal Fracking" (:hand (get-minion))) "Geothermal Fracking returned to hand")
    (prompt-choice :hero "Card from hand")
    (prompt-choice :hero "Steal")
    (is (find-card "Hostile Takeover" (:scored (get-hero))) "Geothermal Fracking stolen with Gang Sign")
    (prompt-choice :hero "Done")))

(deftest leela-lingering-successful-run-prompt
  ;; Leela Patel - issues with lingering successful run prompt
  (do-game
    (new-game
      (make-deck "NBN: Making News" [(qty "Breaking News" 1) (qty "SanSan City Grid" 1)])
      (make-deck "Leela Patel: Trained Pragmatist" []))
    (starting-hand state :minion ["SanSan City Grid"])
    (play-from-hand state :minion "SanSan City Grid" "New remote")
    (take-credits state :minion)
    (run-empty-server state :rd)
    (prompt-choice :hero "Steal")
    (prompt-select :hero (get-content state :remote1 0))
    (is (not (:run @state)) "Run is over")))

(deftest leela-upgrades
  ;; Leela Patel - upgrades returned to hand in the middle of a run do not break the run. Issue #2008.
  (do-game
    (new-game (default-minion [(qty "Crisium Grid" 3) (qty "Project Atlas" 3) (qty "Shock!" 1)])
              (make-deck "Leela Patel: Trained Pragmatist" [(qty "Sure Gamble" 1)]))
    (starting-hand state :minion ["Crisium Grid" "Crisium Grid" "Crisium Grid" "Project Atlas" "Shock!" "Project Atlas"])
    (play-from-hand state :minion "Crisium Grid" "HQ")
    (play-from-hand state :minion "Crisium Grid" "Archives")
    (play-from-hand state :minion "Crisium Grid" "R&D")
    (trash-from-hand state :minion "Project Atlas")
    (trash-from-hand state :minion "Shock!")
    (take-credits state :minion)
    (run-empty-server state "HQ")
    (prompt-choice :hero "Card from hand")
    (prompt-choice :hero "Steal")
    (prompt-select :hero (get-content state :hq 0))
    (is (not (get-content state :hq 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "R&D")
    (prompt-choice :hero "Card from deck")
    (prompt-choice :hero "Steal")
    (prompt-select :hero (get-content state :rd 0))
    (is (not (get-content state :rd 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")
    (run-empty-server state "Archives")
    (prompt-choice :hero "Shock!")
    (prompt-choice :hero "Project Atlas")
    (prompt-choice :hero "Steal")
    (prompt-select :hero (get-content state :archives 0))
    (is (not (get-content state :archives 0)) "Upgrade returned to hand")
    (is (not (:run @state)) "Run ended, no more accesses")))

(deftest maxx
  (do-game
    (new-game (default-minion)
              (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                    (qty "Eater" 1)]))
    (starting-hand state :hero ["Eater"])
    (take-credits state :minion)
    (is (= 2 (count (:discard (get-hero)))) "MaxX discarded 2 cards at start of turn")
    (is (last-log-contains? state "Wyldside, Wyldside")
        "Maxx did log trashed card names")))

(deftest maxx-wyldside-start-of-turn
  ;; MaxX and Wyldside - using Wyldside during Step 1.2 should lose 1 click
  (do-game
    (new-game (default-minion)
              (make-deck "MaxX: Maximum Punk Rock" [(qty "Wyldside" 3)
                                                     (qty "Sure Gamble" 3)
                                                     (qty "Infiltration" 3)
                                                     (qty "Corroder" 3)
                                                     (qty "Eater" 3)]))
    (take-credits state :minion)
    (is (= 2 (count (:discard (get-hero)))) "MaxX discarded 2 cards at start of turn")
    (starting-hand state :hero ["Wyldside"])
    (play-from-hand state :hero "Wyldside")
    (take-credits state :hero 3)
    (is (= 5 (:credit (get-hero))) "Runner has 5 credits at end of first turn")
    (is (find-card "Wyldside" (get-in @state [:hero :rig :resource])) "Wyldside was installed")
    (take-credits state :minion)
    (is (= 0 (:click (get-hero))) "Runner has 0 clicks")
    (is (:hero-phase-12 @state) "Runner is in Step 1.2")
    (let [maxx (get-in @state [:hero :identity])
          wyld (find-card "Wyldside" (get-in @state [:hero :rig :resource]))]
      (card-ability state :hero maxx 0)
      (card-ability state :hero wyld 0)
      (core/end-phase-12 state :hero nil)
      (is (= 4 (count (:discard (get-hero)))) "MaxX discarded 2 cards at start of turn")
      (is (= 3 (:click (get-hero))) "Wyldside caused 1 click to be lost")
      (is (= 3 (count (:hand (get-hero)))) "3 cards drawn total"))))

(deftest nasir-ability-basic
  ;; Nasir Ability - Basic
  (do-game
    (new-game
      (default-minion [(qty "Ice Wall" 3)])
      (make-deck "Nasir Meidan: Cyber Explorer" []))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (take-credits state :minion)

    (run-on state "HQ")
    (let [iwall (get-ice state :hq 0)
          nasir (get-in @state [:hero :identity])]
      (core/rez state :minion iwall)
      (is (= 5 (:credit (get-hero))) "Nasir Ability does not trigger automatically")
      (card-ability state :hero nasir 0)
      (is (= 1 (:credit (get-hero))) "Credits at 1 after Nasir ability trigger"))))

(deftest nasir-ability-xanadu
  ;; Nasir Ability - Xanadu
  (do-game
    (new-game
      (default-minion [(qty "Ice Wall" 1)])
      (make-deck "Nasir Meidan: Cyber Explorer" [(qty "Xanadu" 1)]))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (take-credits state :minion)

    (swap! state assoc-in [:hero :credit] 6)
    (play-from-hand state :hero "Xanadu")
    (run-on state "HQ")
    (let [iwall (get-in @state [:minion :servers :hq :ices 0])
          nasir (get-in @state [:hero :identity])]
      (core/rez state :minion iwall)
      (is (= 3 (:credit (get-hero))) "Pay 3 to install Xanadu")
      (card-ability state :hero nasir 0)
      (is (= 2 (:credit (get-hero))) "Gain 1 more credit due to Xanadu"))))

(deftest nbn-controlling-the-message
  ;; NBN: Controlling the Message - Trace to tag Runner when first installed Corp card is trashed
  (do-game
    (new-game
      (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 3)])
      (default-hero [(qty "Forger" 1)]))
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Forger")
    ; trash from HQ first - #2321
    (run-empty-server state "HQ")
    (prompt-choice :hero "Yes")
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes")
    (prompt-choice :minion "Yes")
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (is (empty? (:prompt (get-hero))) "Forger can't avoid the tag")
    (is (= 1 (:tag (get-hero))) "Runner took 1 unpreventable tag")
    (run-empty-server state "Server 2")
    (prompt-choice :hero "Yes")
    (is (empty? (:prompt (get-minion))) "No trace chance on 2nd trashed card of turn")))

(deftest nbn-controlling-the-message-drt
  ;; NBN: Controlling the Message - Interaction with Dedicated Response Team
  (do-game
    (new-game
      (make-deck "NBN: Controlling the Message" [(qty "Launch Campaign" 1) (qty "Dedicated Response Team" 1)])
      (default-hero))
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (play-from-hand state :minion "Dedicated Response Team" "New remote")
    (core/rez state :minion (get-content state :remote2 0))
    (take-credits state :minion)
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes")
    (prompt-choice :minion "Yes")
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (is (= 1 (:tag (get-hero))) "Runner took 1 unpreventable tag")
    (is (= 2 (count (:discard (get-hero)))) "Runner took 2 meat damage from DRT")))

(deftest new-angeles-sol-on-steal
  ;; New Angeles Sol - interaction with hero stealing agendas
  (do-game
    (new-game
      (make-deck "New Angeles Sol: Your News" [(qty "Paywall Implementation" 2) (qty "Breaking News" 1)])
      (default-hero))
    (play-from-hand state :minion "Breaking News" "New remote")
    (play-from-hand state :minion "Paywall Implementation")
    (take-credits state :minion)
    (is (= 6 (:credit (get-minion))))
    (run-empty-server state :remote1)
    (is (= 7 (:credit (get-minion))) "Corp gained 1cr from successful run")
    (prompt-choice :hero "Steal")
    (prompt-choice :minion "Yes")
    (is (find-card "Paywall Implementation" (:discard (get-minion))) "Paywall trashed before Sol triggers")
    (prompt-select :minion (find-card "Paywall Implementation" (:hand (get-minion))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Paywall Implementation" (:current (get-minion))) "Paywall back in play")))

(deftest nisei-division
  ;; Nisei Division - Gain 1 credit from every psi game
  (do-game
    (new-game
      (make-deck "Nisei Division: The Next Generation" [(qty "Snowflake" 2)])
      (default-hero))
    (play-from-hand state :minion "Snowflake" "HQ")
    (play-from-hand state :minion "Snowflake" "HQ")
    (take-credits state :minion)
    (let [s1 (get-in @state [:minion :servers :hq :ices 0])
          s2 (get-in @state [:minion :servers :hq :ices 1])]
      (run-on state "HQ")
      (core/rez state :minion s2)
      (is (= 4 (:credit (get-minion))))
      (card-subroutine state :minion s2 0)
      (prompt-choice :minion "0 [Credits]")
      (prompt-choice :hero "0 [Credits]")
      (is (= 5 (:credit (get-minion))) "Gained 1 credit from psi game")
      (core/no-action state :minion nil)
      (core/rez state :minion s1)
      (is (= 4 (:credit (get-minion))))
      (card-subroutine state :minion s1 0)
      (prompt-choice :minion "0 [Credits]")
      (prompt-choice :hero "1 [Credits]")
      (is (= 5 (:credit (get-minion))) "Gained 1 credit from psi game"))))

(deftest noise-ability
  ;; Noise: Hacker Extraordinaire - Ability
  (do-game
    (new-game
      (default-minion [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)])
      (make-deck "Noise: Hacker Extraordinaire" [(qty "Datasucker" 1) (qty "Cache" 1) (qty "Sure Gamble" 1) (qty "Clone Chip" 2) (qty "Sharpshooter" 2)]))
    (starting-hand state :hero ["Datasucker" "Sure Gamble" "Clone Chip" "Clone Chip" "Cache"])
    (is (= 6 (count (:hand (get-minion)))) "Corp should start with 6 cards in hand")
    (is (= 5 (count (:deck (get-minion)))) "Corp deck should contain 5 cards")
    (take-credits state :minion)
    (is (= 0 (count (:discard (get-minion)))) "Archives started empty")
    (play-from-hand state :hero "Datasucker")
    (is (= 1 (count (:discard (get-minion)))) "Playing virus should cause card to be trashed from R&D")
    (is (= 4 (count (:deck (get-minion)))) "Card trashed to Archives by Noise should come from R&D")
    (play-from-hand state :hero "Sure Gamble")
    (is (= 1 (count (:discard (get-minion)))) "Playing non-virus should not cause card to be trashed from R&D")
    (core/click-draw state :hero nil)
    (play-from-hand state :hero "Clone Chip")
    (play-from-hand state :hero "Clone Chip")
    (trash-from-hand state :hero "Cache")
    (trash-from-hand state :hero "Sharpshooter")
    (take-credits state :hero)
    ;; playing virus via Clone Chip on Corp's turn should trigger Noise ability
    (let [chip (get-in @state [:hero :rig :hardware 0])]
      (card-ability state :hero chip 0)
      (prompt-select :hero (find-card "Cache" (:discard (get-hero))))
      (let [ds (get-in @state [:hero :rig :program 1])]
        (is (not (nil? ds)))
        (is (= (:title ds) "Cache"))))
    (is (= 2 (count (:discard (get-minion)))) "Playing virus via Clone Chip on minion's turn should trigger Noise ability")
    (is (= 2 (count (:deck (get-minion)))) "Card trashed to Archives by Noise should come from R&D")
    ;; playing non-virus via Clone Chip on Corp's turn should NOT trigger Noise ability
    (let [chip-2 (get-in @state [:hero :rig :hardware 0])]
      (card-ability state :hero chip-2 0)
      (prompt-select :hero (find-card "Sharpshooter" (:discard (get-hero))))
      (let [ss (get-in @state [:hero :rig :program 2])]
        (is (not (nil? ss)))
        (is (= (:title ss) "Sharpshooter"))))
    (is (= 2 (count (:discard (get-minion)))) "Playing non-virus via Clone Chip on minion's turn should not trigger Noise ability")))

(deftest null-ability
  ;; Null ability - once per turn
  (do-game
    (new-game
      (default-minion [(qty "Wraparound" 3)])
      (make-deck "Null: Whistleblower" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Wraparound" "HQ")
    (play-from-hand state :minion "Wraparound" "HQ")
    (take-credits state :minion)
    (run-on state "HQ")
    (let [null (get-in @state [:hero :identity])
          wrap1 (get-ice state :hq 0)
          wrap2 (get-ice state :hq 1)]
      (card-ability state :hero null 0)
      (is (empty? (:prompt (get-hero))) "Ability won't work on unrezzed ICE")
      (core/rez state :minion wrap2)
      (card-ability state :hero null 0)
      (prompt-select :hero (find-card "Sure Gamble" (:hand (get-hero))))
      (is (= 5 (:current-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
      (run-continue state)
      (core/rez state :minion wrap1)
      (card-ability state :hero null 0)
      (is (empty? (:prompt (get-hero))) "Ability already used this turn")
      (run-jack-out state)
      (is (= 7 (:current-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))

(deftest null-trashed
  ;; Null ability - does not affect next ice when current is trashed. Issue #1788.
  (do-game
    (new-game
      (default-minion [(qty "Wraparound" 1) (qty "Spiderweb" 1)])
      (make-deck "Null: Whistleblower" [(qty "Parasite" 3)]))
    (play-from-hand state :minion "Spiderweb" "HQ")
    (play-from-hand state :minion "Wraparound" "HQ")
    (take-credits state :minion)
    (core/gain state :minion :credit 10)
    (let [null (get-in @state [:hero :identity])
          spider (get-ice state :hq 0)
          wrap (get-ice state :hq 1)]
      (core/rez state :minion spider)
      (core/rez state :minion wrap)
      (play-from-hand state :hero "Parasite")
      (prompt-select :hero (refresh spider))
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :hero null 0)
      (prompt-select :hero (first (:hand (get-hero))))
      (is (find-card "Spiderweb" (:discard (get-minion))) "Spiderweb trashed by Parasite + Null")
      (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Null"))))

(deftest omar-ability
  ;; Omar Keung - Make a successful run on the chosen server once per turn
  (do-game
    (new-game
      (default-minion)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (take-credits state :minion)
    (let [omar (get-in @state [:hero :identity])]
      (card-ability state :hero omar 0)
      (run-successful state)
      (prompt-choice :hero "HQ")
      (is (= [:hq] (-> (get-hero) :register :successful-run)))
      (is (= "You accessed Hedge Fund" (-> (get-hero) :prompt first :msg)))
      (prompt-choice :hero "OK")
      (is (= 3 (:click (get-hero))))
      (card-ability state :hero omar 0)
      (is (= 3 (:click (get-hero))))
      (take-credits state :hero)
      (take-credits state :minion)
      (run-empty-server state :rd)
      (is (= [:rd] (-> (get-hero) :register :successful-run)))
      (card-ability state :hero omar 0)
      (run-successful state)
      (prompt-choice :hero "HQ")
      (is (= [:hq :rd] (-> (get-hero) :register :successful-run))))))

(deftest omar-ash
  ;; Omar Keung - Ash prevents access, but not successful run
  (do-game
    (new-game
      (default-minion [(qty "Ash 2X3ZB9CY" 1)])
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Ash 2X3ZB9CY" "HQ")
    (take-credits state :minion)
    (let [omar (get-in @state [:hero :identity])
          ash (get-content state :hq 0)]
      (core/rez state :minion ash)
      (card-ability state :hero omar 0)
      (run-successful state)
      (prompt-choice :hero "HQ")
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (= (:cid ash) (-> (get-hero) :prompt first :card :cid)))
      (is (= :hq (-> (get-hero) :register :successful-run first))))))

(deftest omar-crisium-grid
  ;; Omar Keung - Crisium Grid prevents prompt
  (do-game
    (new-game
      (default-minion [(qty "Crisium Grid" 1)])
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Crisium Grid" "Archives")
    (take-credits state :minion)
    (let [omar (get-in @state [:hero :identity])
          cr (get-content state :archives 0)]
      (core/rez state :minion cr)
      (card-ability state :hero omar 0)
      (run-successful state)
      (is (= (:cid cr) (-> (get-hero) :prompt first :card :cid)))
      (is (empty? (-> (get-hero) :register :successful-run)))
      (is (= :archives (get-in @state [:run :server 0]))))))

(deftest omar-medium
  ;; Omar Keung - When selecting R&D, ability adds counters to Medium
  (do-game
    (new-game
      (default-minion)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Medium" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Medium")
    (let [omar (get-in @state [:hero :identity])
          medium (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero omar 0)
      (run-successful state)
      (prompt-choice :hero "R&D")
      (is (= 1 (get-counters (refresh medium) :virus))))))

(deftest omar-nerve-agent
  ;; Omar Keung - When selecting HQ, ability adds counters to Nerve Agent
  (do-game
    (new-game
      (default-minion)
      (make-deck "Omar Keung: Conspiracy Theorist" [(qty "Nerve Agent" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Nerve Agent")
    (let [omar (get-in @state [:hero :identity])
          nerve (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero omar 0)
      (run-successful state)
      (prompt-choice :hero "HQ")
      (is (= 1 (get-counters (refresh nerve) :virus))))))

(deftest quetzal-ability
  ;; Quetzal ability- once per turn
  (do-game
    (new-game
      (default-minion [(qty "Ice Wall" 3)])
      (make-deck "Quetzal: Free Spirit" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (take-credits state :minion)
    (run-on state "HQ")
    (let [q (get-in @state [:hero :identity])
          iwall (get-ice state :hq 0)
          qdef (core/card-def (get-in @state [:hero :identity]))]
      (core/rez state :minion iwall)
      (card-ability state :hero q 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg]))
          "Quetzal ability did trigger")
      (run-jack-out state)
      (core/click-credit state :hero nil)
      (run-on state "HQ")
      (card-ability state :hero (refresh q) 0)
      (is (not (last-log-contains? state (get-in qdef [:abilities 0 :msg])))
          "Quetzal ability did not trigger")
      (run-jack-out state)
      (take-credits state :hero)
      (take-credits state :minion)
      (core/click-credit state :hero nil)
      (run-on state "HQ")
      (card-ability state :hero (refresh q) 0)
      (is (last-log-contains? state (get-in qdef [:abilities 0 :msg]))
          "Quetzal ability did trigger")
      (core/jack-out state :hero nil))))

(deftest reina-rez-cost-increase
  ;; Reina Roja - Increase cost of first rezzed ICE
  (do-game
    (new-game
      (default-minion [(qty "Quandary" 3)])
      (make-deck "Reina Roja: Freedom Fighter" []))
    (play-from-hand state :minion "Quandary" "R&D")
    (take-credits state :minion)
    (is (= 7 (:credit (get-minion))))
    (run-on state "R&D")
    (let [quan (get-ice state :rd 0)]
      (core/rez state :minion quan)
      (is (= 5 (:credit (get-minion))) "Rez cost increased by 1"))))

(deftest rielle-kit-peddler-ability
  ;; Rielle "Kit" Peddler - Give ICE Code Gate
  (do-game
    (new-game (default-minion [(qty "Ice Wall" 2)])
              (make-deck "Rielle \"Kit\" Peddler: Transhuman" [(qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Ice Wall" "HQ")
    (take-credits state :minion)
    (run-on state "HQ")
    (let [k (get-in @state [:hero :identity])
          iwall (get-ice state :hq 0)]
      (core/rez state :minion iwall)
      (card-ability state :hero k 0)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate"))))

(deftest skorpios
  ; Remove a card from game when it moves to discard once per round
  (do-game
    (new-game (make-deck "Skorpios Defense Systems: Persuasive Power" [(qty "Hedge Fund" 1) (qty "Quandary" 4)])
              (default-hero [(qty "The Maker's Eye" 1) (qty "Lucky Find" 1)]))
    (play-from-hand state :minion "Hedge Fund")
    (dotimes [_ 4] (core/move state :minion (first (:hand (get-minion))) :deck))
    (take-credits state :minion)
    (play-from-hand state :hero "Lucky Find")
    (play-from-hand state :hero "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    ; Don't allow a run-event in progress to be targeted #2963
    (card-ability state :minion (get-in @state [:minion :identity]) 0)
    (is (empty? (filter #(= "The Maker's Eye" (:title %)) (-> (get-minion) :prompt first :choices))) "No Maker's Eye choice")
    (prompt-choice :minion "Cancel")
    (run-successful state)
    (prompt-choice :hero "Card from deck")
    (is (= "You accessed Quandary" (-> (get-hero) :prompt first :msg)) "1st quandary")
    (prompt-choice :hero "OK")
    (prompt-choice :hero "Card from deck")
    (is (= "You accessed Quandary" (-> (get-hero) :prompt first :msg)) "2nd quandary")
    (prompt-choice :hero "OK")
    (prompt-choice :hero "Card from deck")
    (is (= "You accessed Quandary" (-> (get-hero) :prompt first :msg)) "3rd quandary")
    (prompt-choice :hero "OK")
    (is (not (:run @state)))
    (card-ability state :minion (get-in @state [:minion :identity]) 0)
    (prompt-choice :minion (find-card "The Maker's Eye" (:discard (get-hero))))
    (is (= 1 (count (get-in @state [:hero :rfg]))) "One card RFGed")
    (card-ability state :minion (get-in @state [:minion :identity]) 0)
    (is (empty? (:prompt (get-minion))) "Cannot use Skorpios twice")))

(deftest silhouette-expose-trigger-before-access
  ;; Silhouette - Expose trigger ability resolves completely before access. Issue #2173.
  (do-game
    (new-game
      (default-minion [(qty "Psychic Field" 1) (qty "Fetal AI" 10)])
      (make-deck "Silhouette: Stealth Operative" [(qty "Feedback Filter" 1) (qty "Inside Job" 1)]))
    (starting-hand state :minion ["Psychic Field" "Fetal AI"])
    (play-from-hand state :minion "Psychic Field" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Feedback Filter")
    (is (= 3 (:credit (get-hero))) "Runner has 3 credits")
    (let [psychic (get-content state :remote1 0)
          ff (get-hardware state 0)]
      (run-empty-server state :hq)
      (is (:run @state) "On successful run trigger effects")
      (prompt-select :hero psychic)
      (is (= 1 (count (:hand (get-hero)))) "Runner has 1 card in hand")
      (prompt-choice :minion "2 [Credits]")
      (prompt-choice :hero "0 [Credits]")
      (card-ability state :hero ff 0)
      (prompt-choice :hero "Done")
      (is (= 0 (:credit (get-hero))) "Runner has no more credits left")
      (is (= 1 (count (:hand (get-hero)))) "Prevented 1 net damage")
      (is (empty? (:discard (get-hero))) "No cards discarded")
      (is (:run @state) "On run access phase")
      (prompt-choice :hero "Access")
      (prompt-choice :hero "Done")
      (is (empty? (:hand (get-hero))) "Suffered 1 net damage due to accessing Fetal AI")
      (is (= 1 (count (:discard (get-hero)))) "Discarded 1 card due to net damage")
      (is (:run @state) "Resolving access triggers")
      (prompt-choice :hero "Yes")
      (is (= 0 (count (:scored (get-hero)))) "Runner has no credits to be able to steal Fetal AI")
      (is (not (:run @state)) "Run has now ended")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))

(deftest silhouette-temujin-weirdness
  ;; Silhouette - broken interaction with other successful-run triggers. Issue #1968.
  (do-game
    (new-game
      (default-minion [(qty "PAD Campaign" 1) (qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "Beanstalk Royalties" 3)])
      (make-deck "Silhouette: Stealth Operative" [(qty "Temüjin Contract" 1) (qty "Desperado" 1)]))
    (starting-hand state :minion ["Hedge Fund" "PAD Campaign"])
    (play-from-hand state :minion "PAD Campaign" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Temüjin Contract")
    (prompt-choice :hero "HQ")
    (take-credits state :hero)
    (take-credits state :minion)
    (run-empty-server state :hq)
    (prompt-choice :hero "Temüjin Contract")
    (prompt-select :hero (get-content state :remote1 0))
    (prompt-choice :hero "OK")
    (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
    (is (= 16 (get-counters (get-resource state 0) :credit)) "16 cr on Temujin")
    (is (= 8 (:credit (get-hero))) "Gained 4cr")

    ;; second run
    (run-empty-server state :hq)
    (prompt-choice :hero "OK")
    (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
    (is (= 12 (:credit (get-hero))) "Gained 4cr")
    (is (= 12 (get-counters (get-resource state 0) :credit)) "12 cr on Temujin")))

(deftest spark-advertisements
  ;; Spark Agency - Rezzing advertisements
  (do-game
    (new-game
      (make-deck "Spark Agency: Worldswide Reach" [(qty "Launch Campaign" 3)])
      (default-hero))
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (let [lc1 (get-content state :remote1 0)
          lc2 (get-content state :remote2 0)
          lc3 (get-content state :remote3 0)]
      (core/rez state :minion lc1)
      (is (= 4 (:credit (get-hero)))
          "Runner lost 1 credit from rez of advertisement (Corp turn)")
      (core/rez state :minion lc3)
      (is (= 4 (:credit (get-hero)))
          "Runner did not lose credit from second Spark rez")
      (take-credits state :minion)
      (run-on state "Server 1")
      (core/rez state :minion lc2)
      (is (= 3 (:credit (get-hero)))
          "Runner lost 1 credit from rez of advertisement (Runner turn)"))))

(deftest strategic-innovations-future-forward
  ;; Strategic Innovations: Future Forward - Ability
  (do-game
    (new-game
      (make-deck "Strategic Innovations: Future Forward" [(qty "Hedge Fund" 2)
                                                          (qty "Eli 1.0" 2)
                                                          (qty "Crick" 2)])
      (default-hero))
    (play-from-hand state :minion "Eli 1.0" "New remote")
    (play-from-hand state :minion "Hedge Fund")
    (play-from-hand state :minion "Crick" "New remote")
    (let [i1 (get-ice state :remote1 0)
          i2 (get-ice state :remote2 0)]
      (take-credits state :minion 0)
      (take-credits state :hero)
      (core/rez state :minion i1)
      (take-credits state :minion)
      (take-credits state :hero)
      (is (= 1 (count (:prompt (get-minion)))) "Corp prompted to trigger Strategic Innovations")
      (prompt-select :minion (first (:discard (get-minion))))
      (is (empty? (:discard (get-minion))) "Hedge Fund moved back to R&D")
      (take-credits state :minion)
      (core/rez state :minion i2)
      (take-credits state :hero)
      (is (= 0 (count (:prompt (get-minion))))
          "Corp not prompted to trigger Strategic Innovations"))))

(deftest the-foundry-abt
  ;; The Foundry - interaction with Accelerated Beta Test
  (do-game
    (new-game
      (make-deck "The Foundry: Refining the Process" [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)])
      (default-hero))
    (starting-hand state :minion ["Accelerated Beta Test"])
    (play-from-hand state :minion "Accelerated Beta Test" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    (prompt-choice :minion "Yes")
    (prompt-select :minion (find-card "Eli 1.0" (:play-area (get-minion))))
    (prompt-choice :minion "Archives")
    (prompt-choice :minion "Yes")
    (is (empty? (:play-area (get-minion))) "Play area shuffled into R&D")))

(deftest titan-agenda-counter
  ;; Titan Transnational - Add a counter to a scored agenda
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Project Atlas" 1)])
      (default-hero))
    (play-from-hand state :minion "Project Atlas" "New remote")
    (let [atl (get-content state :remote1 0)]
      (core/gain state :minion :click 1)
      (core/advance state :minion {:card (refresh atl)})
      (core/advance state :minion {:card (refresh atl)})
      (core/advance state :minion {:card (refresh atl)})
      (core/score state :minion {:card (refresh atl)})
      (let [scored (get-in @state [:minion :scored 0])]
        (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))

(deftest titan-minionorate-sales-team
  ;; Titan, only use one counter of Corporate Sales Team
  (do-game
    (new-game
      (make-deck "Titan Transnational: Investing In Your Future" [(qty "Corporate Sales Team" 1) (qty "Mark Yale" 1)])
      (default-hero))
    (play-from-hand state :minion "Corporate Sales Team" "New remote")
    (play-from-hand state :minion "Mark Yale" "New remote")
    (let [cst (get-content state :remote1 0)
          my (get-content state :remote2 0)]
      (core/gain state :minion :click 3)
      (core/advance state :minion {:card (refresh cst)})
      (core/advance state :minion {:card (refresh cst)})
      (core/advance state :minion {:card (refresh cst)})
      (core/advance state :minion {:card (refresh cst)})
      (core/score state :minion {:card (refresh cst)})
      (let [scored (get-in @state [:minion :scored 0])]
        (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
        (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
        (core/rez state :minion my)
        (card-ability state :minion my 1)
        (prompt-select :minion (refresh scored))
        (is (= 0 (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
        (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
        (card-ability state :minion my 1)
        (prompt-select :minion (refresh scored))
        (is (= 0 (get-counters (refresh scored) :agenda)) "No agenda counter used by Mark Yale")
        (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")))))

(deftest weyland-builder
  ;; Builder of Nations - 1 meat damage per turn at most
  (do-game
    (new-game
      (make-deck "Weyland Consortium: Builder of Nations" [(qty "Hedge Fund" 3)])
      (default-hero))
      (let [bon (get-in @state [:minion :identity])]
        (card-ability state :minion bon 0)
        (prompt-choice :minion "Cancel")
        (is (= 0 (count (:discard (get-hero)))) "Runner took no meat damage from BoN")
        (card-ability state :minion bon 0)
        (prompt-choice :minion "Yes")
        (is (= 1 (count (:discard (get-hero)))) "Runner took 1 meat damage from BoN")
        (card-ability state :minion bon 0)
        (is (= 1 (count (:discard (get-hero)))) "Runner took only 1 meat damage from BoN total")
        (is (= 0 (count (:prompt (get-minion))))))))

(deftest weyland-builder-cleaners
  ;; Builder of Nations - 2 meat damage from ID ability when The Cleaners is scored
  (do-game
    (new-game
      (make-deck "Weyland Consortium: Builder of Nations" [(qty "The Cleaners" 3) (qty "Ice Wall" 3)])
      (default-hero [(qty "Sure Gamble" 2)]))
    (play-from-hand state :minion "The Cleaners" "New remote")
    (let [clean (get-content state :remote1 0)]
      (score-agenda state :minion clean)
    (let [bon (get-in @state [:minion :identity])]
      (card-ability state :minion bon 0)
      (prompt-choice :minion "Yes")
      (is (= 2 (count (:discard (get-hero)))) "Runner took 2 meat damage from BoN/Cleaners combo")))))

(deftest whizzard
  ;; Whizzard - Recurring credits
  (do-game
    (new-game (default-minion) (make-deck "Whizzard: Master Gamer" ["Sure Gamble"]))

    (let [click-whizzard (fn [n] (dotimes [i n] (card-ability state :hero (:identity (get-hero)) 0)))]
      (is (changes-credits (get-hero) 1 (click-whizzard 1)))
      (is (changes-credits (get-hero) 2 (click-whizzard 5)) "Can't take more than 3 Whizzard credits")

      (take-credits state :minion)
      (is (changes-credits (get-hero) 3 (click-whizzard 3)) "Credits reset at start of Runner's turn")

      (take-credits state :hero)
      (is (changes-credits (get-hero) 0 (click-whizzard 1)) "Credits don't reset at start of Corp's turn"))))

(deftest wyvern-chemically-enhanced
  ;; Wyvern: Chemically Enhanced - Ability
  (do-game
    (new-game (default-minion [(qty "Launch Campaign" 3)])
              (make-deck "Wyvern: Chemically Enhanced" [(qty "Sure Gamble" 2)
                                                        (qty "Corroder" 1)
                                                        (qty "Clone Chip" 1)
                                                        (qty "Easy Mark" 1)]))
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (play-from-hand state :minion "Launch Campaign" "New remote")
    (take-credits state :minion)
    (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
    (play-from-hand state :hero "Sure Gamble")
    (play-from-hand state :hero "Easy Mark")
    (play-from-hand state :hero "Corroder")
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes")
    ;; trash Launch Campaign, should trigger wyvern
    (is (= "Sure Gamble" (:title (last (:discard (get-hero)))))
        "Sure Gamble still in Wyvern's discard")
    (is (some #(= "Easy Mark" (:title %)) (:deck (get-hero))) "Easy Mark moved to deck")
    (take-credits state :hero)
    (take-credits state :minion)
    (play-from-hand state :hero "Clone Chip")
    (run-empty-server state "Server 2")
    (prompt-choice :hero "Yes")
    (is (= "Sure Gamble" (:title (last (:discard (get-hero)))))
        "Sure Gamble still in Wyvern's discard")))
