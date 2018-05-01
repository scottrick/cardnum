(ns test.cards.icebreakers
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest atman-install-0
  ;; Atman - Installing with 0 power counters
  (do-game
    (new-game (default-minion) (default-hero [(qty "Atman" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Atman")
    (prompt-choice :hero 0)
    (is (= 3 (:memory (get-hero))))
    (let [atman (get-in @state [:hero :rig :program 0])]
      (is (= 0 (get-counters atman :power)) "0 power counters")
      (is (= 0 (:current-strength atman)) "0 current strength"))))

(deftest atman-install-2
  ;; Atman - Installing with 2 power counters
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Atman" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Atman")
    (prompt-choice :hero 2)
    (is (= 3 (:memory (get-hero))))
    (let [atman (get-in @state [:hero :rig :program 0])]
      (is (= 2 (get-counters atman :power)) "2 power counters")
      (is (= 2 (:current-strength atman)) "2 current strength"))))

(deftest baba-yaga
  (do-game
    (new-game
      (default-minion)
      (default-hero [(qty "Baba Yaga" 1) (qty "Faerie" 1) (qty "Yog.0" 1)(qty "Sharpshooter" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :credit 10)
    (play-from-hand state :hero "Baba Yaga")
    (play-from-hand state :hero "Sharpshooter")
    (let [baba (get-program state 0)
          base-abicount (count (:abilities baba))]
      (card-ability state :hero baba 0)
      (prompt-select :hero (find-card "Faerie" (:hand (get-hero))))
      (is (= (+ 2 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 2 subroutines from Faerie")
      (card-ability state :hero (refresh baba) 0)
      (prompt-select :hero (find-card "Yog.0" (:hand (get-hero))))
      (is (= (+ 3 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 1 subroutine from Yog.0")
      (core/trash state :hero (first (:hosted (refresh baba))))
      (is (= (inc base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga lost 2 subroutines from trashed Faerie")
      (card-ability state :hero baba 1)
      (prompt-select :hero (find-card "Sharpshooter" (:program (:rig (get-hero)))))
      (is (= 2 (count (:hosted (refresh baba)))) "Faerie and Sharpshooter hosted on Baba Yaga")
      (is (= 1 (:memory (get-hero))) "1 MU left with 2 breakers on Baba Yaga")
      (is (= 4 (:credit (get-hero))) "-5 from Baba, -1 from Sharpshooter played into Rig, -5 from Yog"))))

(deftest chameleon-clonechip
  ;; Chameleon - Install on minion turn, only returns to hand at end of hero's turn
  (do-game
    (new-game (default-minion) (default-hero [(qty "Chameleon" 1) (qty "Clone Chip" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Clone Chip")
    (core/move state :hero (find-card "Chameleon" (:hand (get-hero))) :discard)
    (take-credits state :hero)
    (is (= 0 (count (:hand (get-hero)))))
    ;; Install Chameleon on minion turn
    (take-credits state :minion 1)
    (let [chip (get-in @state [:hero :rig :hardware 0])]
      (card-ability state :hero chip 0)
      (prompt-select :hero (find-card "Chameleon" (:discard (get-hero))))
      (prompt-choice :hero "Sentry"))
    (take-credits state :minion)
    (is (= 0 (count (:hand (get-hero)))) "Chameleon not returned to hand at end of minion turn")
    (take-credits state :hero)
    (is (= 1 (count (:hand (get-hero)))) "Chameleon returned to hand at end of hero's turn")))

(deftest chameleon-scheherazade
  ;; Chameleon - Returns to hand after hosting. #977
  (do-game
    (new-game (default-minion) (default-hero [(qty "Chameleon" 2) (qty "Scheherazade" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Chameleon")
    (prompt-choice :hero "Barrier")
    (is (= 3 (:credit (get-hero))) "-2 from playing Chameleon")
    ;; Host the Chameleon on Scheherazade that was just played (as in Personal Workshop/Hayley ability scenarios)
    (play-from-hand state :hero "Scheherazade")
    (let [scheherazade (get-in @state [:hero :rig :program 1])]
      (card-ability state :hero scheherazade 1) ; Host an installed program
      (prompt-select :hero (find-card "Chameleon" (:program (:rig (get-hero)))))
      (is (= 4 (:credit (get-hero))) "+1 from hosting onto Scheherazade")
      ;; Install another Chameleon directly onto Scheherazade
      (card-ability state :hero scheherazade 0) ; Install and host a program from Grip
      (prompt-select :hero (find-card "Chameleon" (:hand (get-hero))))
      (prompt-choice :hero "Code Gate")
      (is (= 2 (count (:hosted (refresh scheherazade)))) "2 Chameleons hosted on Scheherazade")
      (is (= 3 (:credit (get-hero))) "-2 from playing Chameleon, +1 from installing onto Scheherazade"))
    (is (= 0 (count (:hand (get-hero)))) "Both Chameleons in play - hand size 0")
    (take-credits state :hero)
    (is (= 2 (count (:hand (get-hero)))) "Both Chameleons returned to hand - hand size 2")))

(deftest cerberus
  ;; Cerberus - boost 1 for 1 cred. Break for 1 counter
  (do-game
   (new-game (default-minion)
             (default-hero [(qty "Cerberus \"Rex\" H2" 1)]))
   (take-credits state :minion)
   (play-from-hand state :hero "Cerberus \"Rex\" H2")
   (is (= 2 (:credit (get-hero))) "2 credits left after install")
   (let [rex (get-in @state [:hero :rig :program 0])]
     (is (= 4 (get-counters rex :power)) "Start with 4 counters")
     ;; boost strength
     (card-ability state :hero rex 1)
     (is (= 1 (:credit (get-hero))) "Spend 1 credit to boost")
     (is (= 2 (:current-strength (refresh rex))) "At strength 2 after boost")
     ;; break
     (card-ability state :hero rex 0)
     (is (= 1 (:credit (get-hero))) "No credits spent to break")
     (is (= 3 (get-counters (refresh rex) :power)) "One counter used to break"))))

(deftest crypsis
  ;; Crypsis - Loses a virus counter after encountering ice it broke
  (do-game
    (new-game (default-minion [(qty "Ice Wall" 1)])
              (default-hero [(qty "Crypsis" 2)]))
    (play-from-hand state :minion "Ice Wall" "Archives")
    (take-credits state :minion)
    (core/gain state :hero :credit 100)
    (play-from-hand state :hero "Crypsis")
    (let [crypsis (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero crypsis 2)
      (is (= 1 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 1 virus counter")

      (run-on state "Archives")
      (core/rez state :minion (get-ice state :archives 0))
      (card-ability state :hero (refresh crypsis) 0) ; Match strength
      (card-ability state :hero (refresh crypsis) 1) ; Break
      (is (= 1 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 1 virus counter")
      (run-continue state)
      (is (= 0 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= 0 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 0 virus counters")

      (run-on state "Archives")
      (card-ability state :hero (refresh crypsis) 0) ; Match strength
      (card-ability state :hero (refresh crypsis) 1) ; Break
      (is (= 0 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-hero)))))
          "Crypsis was trashed"))

    (take-credits state :hero)
    (take-credits state :minion)

    (play-from-hand state :hero "Crypsis")
    (let [crypsis (get-in @state [:hero :rig :program 0])]
      (run-on state "Archives")
      (card-ability state :hero (refresh crypsis) 0) ; Match strength
      (card-ability state :hero (refresh crypsis) 1) ; Break
      (is (nil? (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has nil virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-hero)))))
          "Crypsis was trashed"))))

(deftest deus-x-multiple-hostile-infrastructure
  ;; Multiple Hostile Infrastructure vs. Deus X
  (do-game
    (new-game
      (default-minion [(qty "Hostile Infrastructure" 3)])
      (default-hero [(qty "Deus X" 3) (qty "Sure Gamble" 2)]))
    (play-from-hand state :minion "Hostile Infrastructure" "New remote")
    (play-from-hand state :minion "Hostile Infrastructure" "New remote")
    (play-from-hand state :minion "Hostile Infrastructure" "New remote")
    (core/gain state :minion :credit 10)
    (core/rez state :minion (get-content state :remote1 0))
    (core/rez state :minion (get-content state :remote2 0))
    (core/rez state :minion (get-content state :remote3 0))
    (take-credits state :minion)
    (core/gain state :hero :credit 10)
    (play-from-hand state :hero "Deus X")
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Yes")
    (let [dx (get-program state 0)]
      (card-ability state :hero dx 1)
      (prompt-choice :hero "Done")
      (is (= 2 (count (:hand (get-hero)))) "Deus X prevented one Hostile net damage"))))

(deftest deus-x-fetal-jinteki-pe
  ;; Multiple sources of net damage vs. Deus X
  (do-game
    (new-game
      (make-deck "Jinteki: Personal Evolution" [(qty "Fetal AI" 6)])
      (default-hero [(qty "Deus X" 3) (qty "Sure Gamble" 2)]))
    (play-from-hand state :minion "Fetal AI" "New remote")
    (take-credits state :minion)
    (core/gain state :hero :credit 10)
    (play-from-hand state :hero "Deus X")
    (run-empty-server state "Server 1")
    (prompt-choice :hero "Access")
    (let [dx (get-program state 0)]
      (card-ability state :hero dx 1)
      (prompt-choice :hero "Done")
      (prompt-choice :hero "Yes")
      (is (= 3 (count (:hand (get-hero)))) "Deus X prevented net damage from accessing Fetal AI, but not from Personal Evolution")
      (is (= 1 (count (:scored (get-hero)))) "Fetal AI stolen"))))

(deftest faerie-auto-trash
  ;; Faerie - trash after encounter is over, not before.
  (do-game
    (new-game
      (default-minion [(qty "Caduceus" 1)])
      (default-hero [(qty "Faerie" 1)]))
    (play-from-hand state :minion "Caduceus" "Archives")
    (take-credits state :minion)
    (play-from-hand state :hero "Faerie")
    (let [fae (get-program state 0)]
      (run-on state :archives)
      (core/rez state :minion (get-ice state :archives 0))
      (card-ability state :hero fae 0)
      (is (refresh fae) "Faerie not trashed until encounter over")
      (run-continue state)
      (is (find-card "Faerie" (:discard (get-hero))) "Faerie trashed"))))

(deftest faust-pump
  ;; Faust - Pump by discarding
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Faust" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Faust")
    (let [faust (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero faust 1)
      (prompt-card :hero (first (:hand (get-hero))))
      (is (= 4 (:current-strength (refresh faust))) "4 current strength")
      (is (= 1 (count (:discard (get-hero)))) "1 card trashed"))))

(deftest faust-pump
  ;; Faust - Pump does not trigger trash prevention. #760
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Faust" 1)
                               (qty "Sacrificial Construct" 1)
                               (qty "Fall Guy" 1)
                               (qty "Astrolabe" 1)
                               (qty "Gordian Blade" 1 )
                               (qty "Armitage Codebusting" 1)]))
    (take-credits state :minion)
    (core/draw state :hero 1)
    (play-from-hand state :hero "Faust")
    (play-from-hand state :hero "Fall Guy")
    (play-from-hand state :hero "Sacrificial Construct")
    (is (= 2 (count (get-in @state [:hero :rig :resource]))) "Resources installed")
    (let [faust (get-in @state [:hero :rig :program 0])]
      (card-ability state :hero faust 1)
      (prompt-card :hero (find-card "Astrolabe" (:hand (get-hero))))
      (is (empty? (:prompt (get-hero))) "No trash-prevention prompt for hardware")
      (card-ability state :hero faust 1)
      (prompt-card :hero (find-card "Gordian Blade" (:hand (get-hero))))
      (is (empty? (:prompt (get-hero))) "No trash-prevention prompt for program")
      (card-ability state :hero faust 1)
      (prompt-card :hero (find-card "Armitage Codebusting" (:hand (get-hero))))
      (is (empty? (:prompt (get-hero))) "No trash-prevention prompt for resource"))))

(deftest femme-counter
  ;; Femme Fatale counter test
  (do-game
   (new-game (default-minion [(qty "Ice Wall" 1)])
             (default-hero [(qty "Femme Fatale" 2)]))
   (play-from-hand state :minion "Ice Wall" "HQ")
   (take-credits state :minion)
   (core/gain state :hero :credit 18)
   (let [iw (get-ice state :hq 0)]
    (play-from-hand state :hero "Femme Fatale")
    (prompt-select :hero iw)
    (is (:icon (refresh iw)) "Ice Wall has an icon")
    (core/trash state :hero (get-in @state [:hero :rig :program 0]))
    (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after Femme trashed")
    (play-from-hand state :hero "Femme Fatale")
    (prompt-select :hero iw)
    (is (:icon (refresh iw)) "Ice Wall has an icon")
    (core/trash state :minion iw)
    (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after itself trashed"))))

(deftest nanotk-install-ice-during-run
  ;; Na'Not'K - Strength adjusts accordingly when ice installed during run
  (do-game
    (new-game (default-minion [(qty "Architect" 1) (qty "Eli 1.0" 1)])
              (default-hero [(qty "Na'Not'K" 1)]))
    (play-from-hand state :minion "Architect" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Na'Not'K")
    (let [nanotk (get-program state 0)
          architect (get-ice state :hq 0)]
      (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
      (run-on state "HQ")
      (core/rez state :minion architect)
      (is (= 2 (:current-strength (refresh nanotk))) "1 ice on HQ")
      (card-subroutine state :minion (refresh architect) 1)
      (prompt-select :minion (find-card "Eli 1.0" (:hand (get-minion))))
      (prompt-choice :minion "HQ")
      (is (= 3 (:current-strength (refresh nanotk))) "2 ice on HQ")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))

(deftest nanotk-redirect
  ;; Na'Not'K - Strength adjusts accordingly when run redirected to another server
  (do-game
    (new-game (default-minion [(qty "Susanoo-no-Mikoto" 1) (qty "Crick" 1) (qty "Cortex Lock" 1)])
              (default-hero [(qty "Na'Not'K" 1)]))
    (play-from-hand state :minion "Cortex Lock" "HQ")
    (play-from-hand state :minion "Susanoo-no-Mikoto" "HQ")
    (play-from-hand state :minion "Crick" "Archives")
    (take-credits state :minion)
    (play-from-hand state :hero "Na'Not'K")
    (let [nanotk (get-program state 0)
          susanoo (get-ice state :hq 1)]
      (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
      (run-on state "HQ")
      (core/rez state :minion susanoo)
      (is (= 3 (:current-strength (refresh nanotk))) "2 ice on HQ")
      (card-subroutine state :minion (refresh susanoo) 0)
      (is (= 2 (:current-strength (refresh nanotk))) "1 ice on Archives")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))

(deftest overmind-counters
  ;; Overmind - Start with counters equal to unused MU
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Overmind" 1) (qty "Akamatsu Mem Chip" 2)]))
    (take-credits state :minion)
    (take-credits state :hero 1)
    (play-from-hand state :hero "Akamatsu Mem Chip")
    (play-from-hand state :hero "Akamatsu Mem Chip")
    (is (= 6 (:memory (get-hero))))
    (play-from-hand state :hero "Overmind")
    (is (= 5 (:memory (get-hero))))
    (let [ov (get-in @state [:hero :rig :program 0])]
      (is (= 5 (get-counters (refresh ov) :power)) "Overmind has 5 counters"))))

(deftest paperclip
  ;; Paperclip - prompt to install on encounter, but not if another is installed
  (do-game
    (new-game (default-minion [(qty "Vanilla" 1)])
              (default-hero [(qty "Paperclip" 2)]))
    (play-from-hand state :minion "Vanilla" "Archives")
    (take-credits state :minion)
    (trash-from-hand state :hero "Paperclip")
    (run-on state "Archives")
    (core/rez state :minion (get-ice state :archives 0))
    (prompt-choice :hero "Yes") ; install paperclip
    (run-continue state)
    (run-successful state)
    (is (not (:run @state)) "Run ended")
    (trash-from-hand state :hero "Paperclip")
    (run-on state "Archives")
    (is (empty? (:prompt (get-hero))) "No prompt to install second Paperclip")))

(deftest paperclip-multiple
  ;; Paperclip - do not show a second install prompt if user said No to first, when multiple are in heap
  (do-game
    (new-game (default-minion [(qty "Vanilla" 2)])
              (default-hero [(qty "Paperclip" 3)]))
    (play-from-hand state :minion "Vanilla" "Archives")
    (play-from-hand state :minion "Vanilla" "Archives")
    (take-credits state :minion)
    (trash-from-hand state :hero "Paperclip")
    (trash-from-hand state :hero "Paperclip")
    (trash-from-hand state :hero "Paperclip")
    (run-on state "Archives")
    (core/rez state :minion (get-ice state :archives 1))
    (prompt-choice :hero "No")
    (is (empty? (:prompt (get-hero))) "No additional prompts to rez other copies of Paperclip")
    (run-continue state)
    ;; we should get the prompt on a second ice even after denying the first.
    (core/rez state :minion (get-ice state :archives 0))
    (prompt-choice :hero "No")
    (is (empty? (:prompt (get-hero))) "No additional prompts to rez other copies of Paperclip")
    (core/jack-out state :hero)
    ;; Run again, make sure we get the prompt to install again.
    (run-on state "Archives")
    (prompt-choice :hero "No")
    (is (empty? (:prompt (get-hero))) "No additional prompts to rez other copies of Paperclip")))

(deftest shiv
  ;; Shiv - Gain 1 strength for each installed breaker; no MU cost when 2+ link
  (do-game
    (new-game
      (default-minion)
      (make-deck "Nasir Meidan: Cyber Explorer" [(qty "Shiv" 1) (qty "Inti" 2)
                                                 (qty "Access to Globalsec" 1)]))
    (is (= 1 (:link (get-hero))) "1 link")
    (take-credits state :minion)
    (play-from-hand state :hero "Shiv")
    (let [shiv (get-program state 0)]
      (is (= 1 (:current-strength (refresh shiv))) "1 installed breaker; 1 strength")
      (play-from-hand state :hero "Inti")
      (is (= 2 (:current-strength (refresh shiv))) "2 installed breakers; 2 strength")
      (play-from-hand state :hero "Inti")
      (is (= 3 (:current-strength (refresh shiv))) "3 installed breakers; 3 strength")
      (is (= 1 (:memory (get-hero))) "3 MU consumed")
      (play-from-hand state :hero "Access to Globalsec")
      (is (= 2 (:link (get-hero))) "2 link")
      (is (= 2 (:memory (get-hero))) "Shiv stops using MU when 2+ link"))))

(deftest snowball
  ;; Snowball - Strength boost until end of run when used to break a subroutine
  (do-game
   (new-game (default-minion [(qty "Spiderweb" 1) (qty "Fire Wall" 1) (qty "Hedge Fund" 1)])
             (default-hero [(qty "Snowball" 1)]))
   (play-from-hand state :minion "Hedge Fund")
   (play-from-hand state :minion "Fire Wall" "HQ")
   (play-from-hand state :minion "Spiderweb" "HQ")
   (take-credits state :minion)
   (core/gain state :hero :credit 10)
   (play-from-hand state :hero "Snowball")
   (let [sp (get-ice state :hq 1)
         fw (get-ice state :hq 0)
         snow (get-program state 0)]
     (run-on state "HQ")
     (core/rez state :minion sp)
     (core/rez state :minion fw)
     (card-ability state :hero snow 1) ; match strength
     (is (= 2 (:current-strength (refresh snow))))
     (card-ability state :hero snow 0) ; strength matched, break a sub
     (card-ability state :hero snow 0) ; break a sub
     (is (= 4 (:current-strength (refresh snow))) "Broke 2 subs, gained 2 more strength")
     (run-continue state)
     (is (= 3 (:current-strength (refresh snow))) "Has +2 strength until end of run; lost 1 per-encounter boost")
     (card-ability state :hero snow 1)
     (card-ability state :hero snow 1) ; match strength
     (is (= 5 (:current-strength (refresh snow))) "Matched strength, gained 2")
     (card-ability state :hero snow 0) ; strength matched, break a sub
     (is (= 6 (:current-strength (refresh snow))) "Broke 1 sub, gained 1 more strength")
     (run-continue state)
     (is (= 4 (:current-strength (refresh snow))) "+3 until-end-of-run strength")
     (run-jack-out state)
     (is (= 1 (:current-strength (refresh snow))) "Back to default strength"))))

(deftest study-guide
  ;; Study Guide - 2c to add a power counter; +1 strength per counter
  (do-game
   (new-game (default-minion)
             (default-hero [(qty "Study Guide" 1) (qty "Sure Gamble" 1)]))
   (take-credits state :minion)
   (play-from-hand state :hero "Sure Gamble")
   (play-from-hand state :hero "Study Guide")
   (let [sg (get-program state 0)]
     (card-ability state :hero sg 1)
     (is (= 4 (:credit (get-hero))) "Paid 2c")
     (is (= 1 (get-counters (refresh sg) :power)) "Has 1 power counter")
     (is (= 1 (:current-strength (refresh sg))) "1 strength")
     (card-ability state :hero sg 1)
     (is (= 2 (:credit (get-hero))) "Paid 2c")
     (is (= 2 (get-counters (refresh sg) :power)) "Has 2 power counters")
     (is (= 2 (:current-strength (refresh sg))) "2 strength"))))

(deftest wyrm
  ;; Wyrm reduces strength of ice
  (do-game
   (new-game (default-minion [(qty "Ice Wall" 1)])
             (default-hero [(qty "Wyrm" 1)]))
   (play-from-hand state :minion "Ice Wall" "HQ")
   (take-credits state :minion)
   (play-from-hand state :hero "Wyrm")
   (run-on state "HQ")
   (let [ice-wall (get-ice state :hq 0)
         wyrm (get-in @state [:hero :rig :program 0])]
     (core/rez state :minion ice-wall)
     (card-ability state :hero wyrm 1)
     (is (= 0 (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to 0")
     (card-ability state :hero wyrm 1)
     (is (= -1 (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to -1"))))

(deftest yusuf
  ;; Yusuf gains virus counters on successful runs and can spend virus counters from any installed card
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Yusuf" 1) (qty "Cache" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Yusuf")
    (play-from-hand state :hero "Cache")
    (let [yusuf (get-program state 0)
          cache (get-program state 1)]
      (run-empty-server state "Archives")
      (is (= 1 (get-in (refresh yusuf) [:counter :virus])) "Yusuf has 1 virus counter")
      (is (= 3 (:current-strength (refresh yusuf))) "Initial Yusuf strength")
      (is (= 3 (get-in (refresh cache) [:counter :virus])) "Initial Cache virus counters")
      (card-ability state :hero yusuf 0)
      (prompt-select :hero cache)
      (prompt-choice :hero 1)
      (is (= 2 (get-in (refresh cache) [:counter :virus])) "Cache lost a virus counter to pump")
      (is (= 4 (:current-strength (refresh yusuf))) "Yusuf strength 4")
      (is (= 1 (get-in (refresh yusuf) [:counter :virus])) "Initial Yusuf virus counters")
      (card-ability state :hero yusuf 0)
      (prompt-select :hero yusuf)
      (prompt-choice :hero 1)
      (is (= 5 (:current-strength (refresh yusuf))) "Yusuf strength 5")
      (is (= 0 (get-in (refresh yusuf) [:counter :virus])) "Yusuf lost a virus counter")
      (card-ability state :hero yusuf 1)
      (prompt-select :hero cache)
      (prompt-choice :hero 1)
      (is (= 1 (get-in (refresh cache) [:counter :virus])) "Cache lost a virus counter to break"))))
