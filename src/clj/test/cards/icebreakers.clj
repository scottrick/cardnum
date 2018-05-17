(ns test.cards.icebreakers
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest atman-install-0
  ;; Atman - Installing with 0 power counters
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Atman" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Atman")
    (prompt-chocharacter :challenger 0)
    (is (= 3 (:memory (get-challenger))))
    (let [atman (get-in @state [:challenger :rig :program 0])]
      (is (= 0 (get-counters atman :power)) "0 power counters")
      (is (= 0 (:current-strength atman)) "0 current strength"))))

(deftest atman-install-2
  ;; Atman - Installing with 2 power counters
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Atman" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Atman")
    (prompt-chocharacter :challenger 2)
    (is (= 3 (:memory (get-challenger))))
    (let [atman (get-in @state [:challenger :rig :program 0])]
      (is (= 2 (get-counters atman :power)) "2 power counters")
      (is (= 2 (:current-strength atman)) "2 current strength"))))

(deftest baba-yaga
  (do-game
    (new-game
      (default-contestant)
      (default-challenger [(qty "Baba Yaga" 1) (qty "Faerie" 1) (qty "Yog.0" 1)(qty "Sharpshooter" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Baba Yaga")
    (play-from-hand state :challenger "Sharpshooter")
    (let [baba (get-program state 0)
          base-abicount (count (:abilities baba))]
      (card-ability state :challenger baba 0)
      (prompt-select :challenger (find-card "Faerie" (:hand (get-challenger))))
      (is (= (+ 2 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 2 subroutines from Faerie")
      (card-ability state :challenger (refresh baba) 0)
      (prompt-select :challenger (find-card "Yog.0" (:hand (get-challenger))))
      (is (= (+ 3 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 1 subroutine from Yog.0")
      (core/trash state :challenger (first (:hosted (refresh baba))))
      (is (= (inc base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga lost 2 subroutines from trashed Faerie")
      (card-ability state :challenger baba 1)
      (prompt-select :challenger (find-card "Sharpshooter" (:program (:rig (get-challenger)))))
      (is (= 2 (count (:hosted (refresh baba)))) "Faerie and Sharpshooter hosted on Baba Yaga")
      (is (= 1 (:memory (get-challenger))) "1 MU left with 2 breakers on Baba Yaga")
      (is (= 4 (:credit (get-challenger))) "-5 from Baba, -1 from Sharpshooter played into Rig, -5 from Yog"))))

(deftest chameleon-clonechip
  ;; Chameleon - Install on contestant turn, only returns to hand at end of challenger's turn
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Chameleon" 1) (qty "Clone Chip" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Clone Chip")
    (core/move state :challenger (find-card "Chameleon" (:hand (get-challenger))) :discard)
    (take-credits state :challenger)
    (is (= 0 (count (:hand (get-challenger)))))
    ;; Install Chameleon on contestant turn
    (take-credits state :contestant 1)
    (let [chip (get-in @state [:challenger :rig :hardware 0])]
      (card-ability state :challenger chip 0)
      (prompt-select :challenger (find-card "Chameleon" (:discard (get-challenger))))
      (prompt-chocharacter :challenger "Sentry"))
    (take-credits state :contestant)
    (is (= 0 (count (:hand (get-challenger)))) "Chameleon not returned to hand at end of contestant turn")
    (take-credits state :challenger)
    (is (= 1 (count (:hand (get-challenger)))) "Chameleon returned to hand at end of challenger's turn")))

(deftest chameleon-scheherazade
  ;; Chameleon - Returns to hand after hosting. #977
  (do-game
    (new-game (default-contestant) (default-challenger [(qty "Chameleon" 2) (qty "Scheherazade" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Chameleon")
    (prompt-chocharacter :challenger "Barrier")
    (is (= 3 (:credit (get-challenger))) "-2 from playing Chameleon")
    ;; Host the Chameleon on Scheherazade that was just played (as in Personal Workshop/Hayley ability scenarios)
    (play-from-hand state :challenger "Scheherazade")
    (let [scheherazade (get-in @state [:challenger :rig :program 1])]
      (card-ability state :challenger scheherazade 1) ; Host an installed program
      (prompt-select :challenger (find-card "Chameleon" (:program (:rig (get-challenger)))))
      (is (= 4 (:credit (get-challenger))) "+1 from hosting onto Scheherazade")
      ;; Install another Chameleon directly onto Scheherazade
      (card-ability state :challenger scheherazade 0) ; Install and host a program from Grip
      (prompt-select :challenger (find-card "Chameleon" (:hand (get-challenger))))
      (prompt-chocharacter :challenger "Code Gate")
      (is (= 2 (count (:hosted (refresh scheherazade)))) "2 Chameleons hosted on Scheherazade")
      (is (= 3 (:credit (get-challenger))) "-2 from playing Chameleon, +1 from installing onto Scheherazade"))
    (is (= 0 (count (:hand (get-challenger)))) "Both Chameleons in play - hand size 0")
    (take-credits state :challenger)
    (is (= 2 (count (:hand (get-challenger)))) "Both Chameleons returned to hand - hand size 2")))

(deftest cerberus
  ;; Cerberus - boost 1 for 1 cred. Break for 1 counter
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Cerberus \"Rex\" H2" 1)]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Cerberus \"Rex\" H2")
   (is (= 2 (:credit (get-challenger))) "2 credits left after install")
   (let [rex (get-in @state [:challenger :rig :program 0])]
     (is (= 4 (get-counters rex :power)) "Start with 4 counters")
     ;; boost strength
     (card-ability state :challenger rex 1)
     (is (= 1 (:credit (get-challenger))) "Spend 1 credit to boost")
     (is (= 2 (:current-strength (refresh rex))) "At strength 2 after boost")
     ;; break
     (card-ability state :challenger rex 0)
     (is (= 1 (:credit (get-challenger))) "No credits spent to break")
     (is (= 3 (get-counters (refresh rex) :power)) "One counter used to break"))))

(deftest crypsis
  ;; Crypsis - Loses a virus counter after encountering character it broke
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 1)])
              (default-challenger [(qty "Crypsis" 2)]))
    (play-from-hand state :contestant "Ice Wall" "Archives")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 100)
    (play-from-hand state :challenger "Crypsis")
    (let [crypsis (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger crypsis 2)
      (is (= 1 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 1 virus counter")

      (run-on state "Archives")
      (core/rez state :contestant (get-character state :archives 0))
      (card-ability state :challenger (refresh crypsis) 0) ; Match strength
      (card-ability state :challenger (refresh crypsis) 1) ; Break
      (is (= 1 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 1 virus counter")
      (run-continue state)
      (is (= 0 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= 0 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 0 virus counters")

      (run-on state "Archives")
      (card-ability state :challenger (refresh crypsis) 0) ; Match strength
      (card-ability state :challenger (refresh crypsis) 1) ; Break
      (is (= 0 (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-challenger)))))
          "Crypsis was trashed"))

    (take-credits state :challenger)
    (take-credits state :contestant)

    (play-from-hand state :challenger "Crypsis")
    (let [crypsis (get-in @state [:challenger :rig :program 0])]
      (run-on state "Archives")
      (card-ability state :challenger (refresh crypsis) 0) ; Match strength
      (card-ability state :challenger (refresh crypsis) 1) ; Break
      (is (nil? (get-in (refresh crypsis) [:counter :virus]))
          "Crypsis has nil virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-challenger)))))
          "Crypsis was trashed"))))

(deftest deus-x-multiple-hostile-infrastructure
  ;; Multiple Hostile Infrastructure vs. Deus X
  (do-game
    (new-game
      (default-contestant [(qty "Hostile Infrastructure" 3)])
      (default-challenger [(qty "Deus X" 3) (qty "Sure Gamble" 2)]))
    (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
    (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
    (play-from-hand state :contestant "Hostile Infrastructure" "New remote")
    (core/gain state :contestant :credit 10)
    (core/rez state :contestant (get-content state :remote1 0))
    (core/rez state :contestant (get-content state :remote2 0))
    (core/rez state :contestant (get-content state :remote3 0))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Deus X")
    (run-empty-server state "Server 1")
    (prompt-chocharacter :challenger "Yes")
    (let [dx (get-program state 0)]
      (card-ability state :challenger dx 1)
      (prompt-chocharacter :challenger "Done")
      (is (= 2 (count (:hand (get-challenger)))) "Deus X prevented one Hostile net damage"))))

(deftest deus-x-fetal-jinteki-pe
  ;; Multiple sources of net damage vs. Deus X
  (do-game
    (new-game
      (make-deck "Jinteki: Personal Evolution" [(qty "Fetal AI" 6)])
      (default-challenger [(qty "Deus X" 3) (qty "Sure Gamble" 2)]))
    (play-from-hand state :contestant "Fetal AI" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Deus X")
    (run-empty-server state "Server 1")
    (prompt-chocharacter :challenger "Access")
    (let [dx (get-program state 0)]
      (card-ability state :challenger dx 1)
      (prompt-chocharacter :challenger "Done")
      (prompt-chocharacter :challenger "Yes")
      (is (= 3 (count (:hand (get-challenger)))) "Deus X prevented net damage from accessing Fetal AI, but not from Personal Evolution")
      (is (= 1 (count (:scored (get-challenger)))) "Fetal AI stolen"))))

(deftest faerie-auto-trash
  ;; Faerie - trash after encounter is over, not before.
  (do-game
    (new-game
      (default-contestant [(qty "Caduceus" 1)])
      (default-challenger [(qty "Faerie" 1)]))
    (play-from-hand state :contestant "Caduceus" "Archives")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Faerie")
    (let [fae (get-program state 0)]
      (run-on state :archives)
      (core/rez state :contestant (get-character state :archives 0))
      (card-ability state :challenger fae 0)
      (is (refresh fae) "Faerie not trashed until encounter over")
      (run-continue state)
      (is (find-card "Faerie" (:discard (get-challenger))) "Faerie trashed"))))

(deftest faust-pump
  ;; Faust - Pump by discarding
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Faust" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Faust")
    (let [faust (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger faust 1)
      (prompt-card :challenger (first (:hand (get-challenger))))
      (is (= 4 (:current-strength (refresh faust))) "4 current strength")
      (is (= 1 (count (:discard (get-challenger)))) "1 card trashed"))))

(deftest faust-pump
  ;; Faust - Pump does not trigger trash prevention. #760
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Faust" 1)
                               (qty "Sacrificial Construct" 1)
                               (qty "Fall Guy" 1)
                               (qty "Astrolabe" 1)
                               (qty "Gordian Blade" 1 )
                               (qty "Armitage Codebusting" 1)]))
    (take-credits state :contestant)
    (core/draw state :challenger 1)
    (play-from-hand state :challenger "Faust")
    (play-from-hand state :challenger "Fall Guy")
    (play-from-hand state :challenger "Sacrificial Construct")
    (is (= 2 (count (get-in @state [:challenger :rig :resource]))) "Resources installed")
    (let [faust (get-in @state [:challenger :rig :program 0])]
      (card-ability state :challenger faust 1)
      (prompt-card :challenger (find-card "Astrolabe" (:hand (get-challenger))))
      (is (empty? (:prompt (get-challenger))) "No trash-prevention prompt for hardware")
      (card-ability state :challenger faust 1)
      (prompt-card :challenger (find-card "Gordian Blade" (:hand (get-challenger))))
      (is (empty? (:prompt (get-challenger))) "No trash-prevention prompt for program")
      (card-ability state :challenger faust 1)
      (prompt-card :challenger (find-card "Armitage Codebusting" (:hand (get-challenger))))
      (is (empty? (:prompt (get-challenger))) "No trash-prevention prompt for resource"))))

(deftest femme-counter
  ;; Femme Fatale counter test
  (do-game
   (new-game (default-contestant [(qty "Ice Wall" 1)])
             (default-challenger [(qty "Femme Fatale" 2)]))
   (play-from-hand state :contestant "Ice Wall" "HQ")
   (take-credits state :contestant)
   (core/gain state :challenger :credit 18)
   (let [iw (get-character state :hq 0)]
    (play-from-hand state :challenger "Femme Fatale")
    (prompt-select :challenger iw)
    (is (:icon (refresh iw)) "Ice Wall has an icon")
    (core/trash state :challenger (get-in @state [:challenger :rig :program 0]))
    (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after Femme trashed")
    (play-from-hand state :challenger "Femme Fatale")
    (prompt-select :challenger iw)
    (is (:icon (refresh iw)) "Ice Wall has an icon")
    (core/trash state :contestant iw)
    (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after itself trashed"))))

(deftest nanotk-install-character-during-run
  ;; Na'Not'K - Strength adjusts accordingly when character installed during run
  (do-game
    (new-game (default-contestant [(qty "Architect" 1) (qty "Eli 1.0" 1)])
              (default-challenger [(qty "Na'Not'K" 1)]))
    (play-from-hand state :contestant "Architect" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Na'Not'K")
    (let [nanotk (get-program state 0)
          architect (get-character state :hq 0)]
      (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
      (run-on state "HQ")
      (core/rez state :contestant architect)
      (is (= 2 (:current-strength (refresh nanotk))) "1 character on HQ")
      (card-subroutine state :contestant (refresh architect) 1)
      (prompt-select :contestant (find-card "Eli 1.0" (:hand (get-contestant))))
      (prompt-chocharacter :contestant "HQ")
      (is (= 3 (:current-strength (refresh nanotk))) "2 character on HQ")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))

(deftest nanotk-redirect
  ;; Na'Not'K - Strength adjusts accordingly when run redirected to another server
  (do-game
    (new-game (default-contestant [(qty "Susanoo-no-Mikoto" 1) (qty "Crick" 1) (qty "Cortex Lock" 1)])
              (default-challenger [(qty "Na'Not'K" 1)]))
    (play-from-hand state :contestant "Cortex Lock" "HQ")
    (play-from-hand state :contestant "Susanoo-no-Mikoto" "HQ")
    (play-from-hand state :contestant "Crick" "Archives")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Na'Not'K")
    (let [nanotk (get-program state 0)
          susanoo (get-character state :hq 1)]
      (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
      (run-on state "HQ")
      (core/rez state :contestant susanoo)
      (is (= 3 (:current-strength (refresh nanotk))) "2 character on HQ")
      (card-subroutine state :contestant (refresh susanoo) 0)
      (is (= 2 (:current-strength (refresh nanotk))) "1 character on Archives")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))

(deftest overmind-counters
  ;; Overmind - Start with counters equal to unused MU
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Overmind" 1) (qty "Akamatsu Mem Chip" 2)]))
    (take-credits state :contestant)
    (take-credits state :challenger 1)
    (play-from-hand state :challenger "Akamatsu Mem Chip")
    (play-from-hand state :challenger "Akamatsu Mem Chip")
    (is (= 6 (:memory (get-challenger))))
    (play-from-hand state :challenger "Overmind")
    (is (= 5 (:memory (get-challenger))))
    (let [ov (get-in @state [:challenger :rig :program 0])]
      (is (= 5 (get-counters (refresh ov) :power)) "Overmind has 5 counters"))))

(deftest paperclip
  ;; Paperclip - prompt to install on encounter, but not if another is installed
  (do-game
    (new-game (default-contestant [(qty "Vanilla" 1)])
              (default-challenger [(qty "Paperclip" 2)]))
    (play-from-hand state :contestant "Vanilla" "Archives")
    (take-credits state :contestant)
    (trash-from-hand state :challenger "Paperclip")
    (run-on state "Archives")
    (core/rez state :contestant (get-character state :archives 0))
    (prompt-chocharacter :challenger "Yes") ; install paperclip
    (run-continue state)
    (run-successful state)
    (is (not (:run @state)) "Run ended")
    (trash-from-hand state :challenger "Paperclip")
    (run-on state "Archives")
    (is (empty? (:prompt (get-challenger))) "No prompt to install second Paperclip")))

(deftest paperclip-multiple
  ;; Paperclip - do not show a second install prompt if user said No to first, when multiple are in heap
  (do-game
    (new-game (default-contestant [(qty "Vanilla" 2)])
              (default-challenger [(qty "Paperclip" 3)]))
    (play-from-hand state :contestant "Vanilla" "Archives")
    (play-from-hand state :contestant "Vanilla" "Archives")
    (take-credits state :contestant)
    (trash-from-hand state :challenger "Paperclip")
    (trash-from-hand state :challenger "Paperclip")
    (trash-from-hand state :challenger "Paperclip")
    (run-on state "Archives")
    (core/rez state :contestant (get-character state :archives 1))
    (prompt-chocharacter :challenger "No")
    (is (empty? (:prompt (get-challenger))) "No additional prompts to rez other copies of Paperclip")
    (run-continue state)
    ;; we should get the prompt on a second character even after denying the first.
    (core/rez state :contestant (get-character state :archives 0))
    (prompt-chocharacter :challenger "No")
    (is (empty? (:prompt (get-challenger))) "No additional prompts to rez other copies of Paperclip")
    (core/jack-out state :challenger)
    ;; Run again, make sure we get the prompt to install again.
    (run-on state "Archives")
    (prompt-chocharacter :challenger "No")
    (is (empty? (:prompt (get-challenger))) "No additional prompts to rez other copies of Paperclip")))

(deftest shiv
  ;; Shiv - Gain 1 strength for each installed breaker; no MU cost when 2+ link
  (do-game
    (new-game
      (default-contestant)
      (make-deck "Nasir Meidan: Cyber Explorer" [(qty "Shiv" 1) (qty "Inti" 2)
                                                 (qty "Access to Globalsec" 1)]))
    (is (= 1 (:link (get-challenger))) "1 link")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Shiv")
    (let [shiv (get-program state 0)]
      (is (= 1 (:current-strength (refresh shiv))) "1 installed breaker; 1 strength")
      (play-from-hand state :challenger "Inti")
      (is (= 2 (:current-strength (refresh shiv))) "2 installed breakers; 2 strength")
      (play-from-hand state :challenger "Inti")
      (is (= 3 (:current-strength (refresh shiv))) "3 installed breakers; 3 strength")
      (is (= 1 (:memory (get-challenger))) "3 MU consumed")
      (play-from-hand state :challenger "Access to Globalsec")
      (is (= 2 (:link (get-challenger))) "2 link")
      (is (= 2 (:memory (get-challenger))) "Shiv stops using MU when 2+ link"))))

(deftest snowball
  ;; Snowball - Strength boost until end of run when used to break a subroutine
  (do-game
   (new-game (default-contestant [(qty "Spiderweb" 1) (qty "Fire Wall" 1) (qty "Hedge Fund" 1)])
             (default-challenger [(qty "Snowball" 1)]))
   (play-from-hand state :contestant "Hedge Fund")
   (play-from-hand state :contestant "Fire Wall" "HQ")
   (play-from-hand state :contestant "Spiderweb" "HQ")
   (take-credits state :contestant)
   (core/gain state :challenger :credit 10)
   (play-from-hand state :challenger "Snowball")
   (let [sp (get-character state :hq 1)
         fw (get-character state :hq 0)
         snow (get-program state 0)]
     (run-on state "HQ")
     (core/rez state :contestant sp)
     (core/rez state :contestant fw)
     (card-ability state :challenger snow 1) ; match strength
     (is (= 2 (:current-strength (refresh snow))))
     (card-ability state :challenger snow 0) ; strength matched, break a sub
     (card-ability state :challenger snow 0) ; break a sub
     (is (= 4 (:current-strength (refresh snow))) "Broke 2 subs, gained 2 more strength")
     (run-continue state)
     (is (= 3 (:current-strength (refresh snow))) "Has +2 strength until end of run; lost 1 per-encounter boost")
     (card-ability state :challenger snow 1)
     (card-ability state :challenger snow 1) ; match strength
     (is (= 5 (:current-strength (refresh snow))) "Matched strength, gained 2")
     (card-ability state :challenger snow 0) ; strength matched, break a sub
     (is (= 6 (:current-strength (refresh snow))) "Broke 1 sub, gained 1 more strength")
     (run-continue state)
     (is (= 4 (:current-strength (refresh snow))) "+3 until-end-of-run strength")
     (run-jack-out state)
     (is (= 1 (:current-strength (refresh snow))) "Back to default strength"))))

(deftest study-guide
  ;; Study Guide - 2c to add a power counter; +1 strength per counter
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Study Guide" 1) (qty "Sure Gamble" 1)]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Sure Gamble")
   (play-from-hand state :challenger "Study Guide")
   (let [sg (get-program state 0)]
     (card-ability state :challenger sg 1)
     (is (= 4 (:credit (get-challenger))) "Paid 2c")
     (is (= 1 (get-counters (refresh sg) :power)) "Has 1 power counter")
     (is (= 1 (:current-strength (refresh sg))) "1 strength")
     (card-ability state :challenger sg 1)
     (is (= 2 (:credit (get-challenger))) "Paid 2c")
     (is (= 2 (get-counters (refresh sg) :power)) "Has 2 power counters")
     (is (= 2 (:current-strength (refresh sg))) "2 strength"))))

(deftest wyrm
  ;; Wyrm reduces strength of character
  (do-game
   (new-game (default-contestant [(qty "Ice Wall" 1)])
             (default-challenger [(qty "Wyrm" 1)]))
   (play-from-hand state :contestant "Ice Wall" "HQ")
   (take-credits state :contestant)
   (play-from-hand state :challenger "Wyrm")
   (run-on state "HQ")
   (let [character-wall (get-character state :hq 0)
         wyrm (get-in @state [:challenger :rig :program 0])]
     (core/rez state :contestant character-wall)
     (card-ability state :challenger wyrm 1)
     (is (= 0 (:current-strength (refresh character-wall))) "Strength of Ice Wall reduced to 0")
     (card-ability state :challenger wyrm 1)
     (is (= -1 (:current-strength (refresh character-wall))) "Strength of Ice Wall reduced to -1"))))

(deftest yusuf
  ;; Yusuf gains virus counters on successful runs and can spend virus counters from any installed card
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Yusuf" 1) (qty "Cache" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Yusuf")
    (play-from-hand state :challenger "Cache")
    (let [yusuf (get-program state 0)
          cache (get-program state 1)]
      (run-empty-server state "Archives")
      (is (= 1 (get-in (refresh yusuf) [:counter :virus])) "Yusuf has 1 virus counter")
      (is (= 3 (:current-strength (refresh yusuf))) "Initial Yusuf strength")
      (is (= 3 (get-in (refresh cache) [:counter :virus])) "Initial Cache virus counters")
      (card-ability state :challenger yusuf 0)
      (prompt-select :challenger cache)
      (prompt-chocharacter :challenger 1)
      (is (= 2 (get-in (refresh cache) [:counter :virus])) "Cache lost a virus counter to pump")
      (is (= 4 (:current-strength (refresh yusuf))) "Yusuf strength 4")
      (is (= 1 (get-in (refresh yusuf) [:counter :virus])) "Initial Yusuf virus counters")
      (card-ability state :challenger yusuf 0)
      (prompt-select :challenger yusuf)
      (prompt-chocharacter :challenger 1)
      (is (= 5 (:current-strength (refresh yusuf))) "Yusuf strength 5")
      (is (= 0 (get-in (refresh yusuf) [:counter :virus])) "Yusuf lost a virus counter")
      (card-ability state :challenger yusuf 1)
      (prompt-select :challenger cache)
      (prompt-chocharacter :challenger 1)
      (is (= 1 (get-in (refresh cache) [:counter :virus])) "Cache lost a virus counter to break"))))
