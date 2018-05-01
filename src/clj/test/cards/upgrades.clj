(ns test.cards.upgrades
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest amazon-industrial-zone
  ;; Amazon Industrial Zone - Immediately rez ICE installed over its server at 3 credit discount
  (do-game
    (new-game (default-minion [(qty "Spiderweb" 1) (qty "Amazon Industrial Zone" 1)])
              (default-hero))
    (take-credits state :minion 1)
    (play-from-hand state :minion "Amazon Industrial Zone" "New remote")
    (let [aiz (get-content state :remote1 0)]
      (core/rez state :minion aiz)
      (is (= 2 (:credit (get-minion))))
      (play-from-hand state :minion "Spiderweb" "Server 1")
      (prompt-choice :minion "Yes") ; optional ability
      (let [spid (get-ice state :remote1 0)]
        (is (get-in (refresh spid) [:rezzed]) "Spiderweb rezzed")
        (is (= 1 (:credit (get-minion))) "Paid only 1 credit to rez")))))

(deftest ben-musashi
  ;; Ben Musashi - pay 2 net damage to steal from this server
  (do-game
    (new-game (default-minion [(qty "Ben Musashi" 1) (qty "House of Knives" 1)])
              (default-hero))
    (play-from-hand state :minion "Ben Musashi" "New remote")
    (play-from-hand state :minion "House of Knives" "Server 1")
    (take-credits state :minion 1)
    (let [bm (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (core/rez state :minion bm)
      (run-empty-server state "Server 1")
      ;; hero now chooses which to access.
      (prompt-select :hero hok)
      ;; prompt should be asking for the 2 net damage cost
      (is (= "House of Knives" (:title (:card (first (:prompt (get-hero))))))
          "Prompt to pay 2 net damage")
      (prompt-choice :hero "No")
      (is (= 5 (:credit (get-hero))) "Runner did not pay 2 net damage")
      (is (= 0 (count (:scored (get-hero)))) "No scored agendas")
      (prompt-select :hero bm)
      (prompt-choice :hero "No")
      (run-empty-server state "Server 1")
      (prompt-select :hero hok)
      (prompt-choice :hero "Yes")
      (is (= 2 (count (:discard (get-hero)))) "Runner took 2 net")
      (is (= 1 (count (:scored (get-hero)))) "1 scored agenda"))))

(deftest ben-musashi-rd
  ;; Ben Musashi - on R&D access
  (do-game
    (new-game (default-minion [(qty "Ben Musashi" 1) (qty "House of Knives" 1)])
              (default-hero))
    (starting-hand state :minion ["Ben Musashi"])
    (play-from-hand state :minion "Ben Musashi" "R&D")
    (take-credits state :minion)
    (let [bm (get-content state :rd 0)]
      (core/rez state :minion bm)
      (run-empty-server state "R&D")
      ;; hero now chooses which to access.
      (prompt-choice :hero "Card from deck")
      ;; prompt should be asking for the 2 net damage cost
      (is (= "House of Knives" (:title (:card (first (:prompt (get-hero))))))
          "Prompt to pay 2 net damage")
      (prompt-choice :hero "No")
      (is (= 5 (:credit (get-hero))) "Runner did not pay 2 net damage")
      (is (= 0 (count (:scored (get-hero)))) "No scored agendas")
      (prompt-choice :hero "Ben Musashi")
      (prompt-choice :hero "No")
      (run-empty-server state "R&D")
      (prompt-choice :hero "Card from deck")
      (prompt-choice :hero "Yes")
      (is (= 2 (count (:discard (get-hero)))) "Runner took 2 net")
      (is (= 1 (count (:scored (get-hero)))) "1 scored agenda"))))

(deftest ben-musashi-trash
  ;; Ben Musashi - pay even when trashed
  (do-game
    (new-game (default-minion [(qty "Ben Musashi" 3) (qty "House of Knives" 3)])
              (default-hero))
    (play-from-hand state :minion "Ben Musashi" "New remote")
    (play-from-hand state :minion "House of Knives" "Server 1")
    (take-credits state :minion 1)
    (core/gain state :hero :credit 1)
    (let [bm (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (core/rez state :minion bm)
      (run-empty-server state "Server 1")
      ;; hero now chooses which to access.
      (prompt-select :hero bm)
      (prompt-choice :hero "Yes") ; pay to trash
      (prompt-select :hero hok)
      ;; should now have prompt to pay 2 net for HoK
      (prompt-choice :hero "Yes")
      (is (= 2 (count (:discard (get-hero)))) "Runner took 2 net")
      (is (= 1 (count (:scored (get-hero)))) "1 scored agenda"))))

(deftest ben-musashi-obokata
  ;; Check hero chooses order of payment
  (do-game
    (new-game (default-minion [(qty "Ben Musashi" 1) (qty "Obokata Protocol" 1)])
              (default-hero [(qty "Sure Gamble" 6)]))
    (play-from-hand state :minion "Ben Musashi" "New remote")
    (play-from-hand state :minion "Obokata Protocol" "Server 1")
    (take-credits state :minion)
    (let [bm (get-content state :remote1 0)
          op (get-content state :remote1 1)]
      (core/rez state :minion bm)
      (run-empty-server state "Server 1")
      ;; hero now chooses which to access.
      (prompt-select :hero op)
      ;; prompt should be asking for the net damage costs
      (is (= "Obokata Protocol" (:title (:card (first (:prompt (get-hero))))))
          "Prompt to pay steal costs")
      (prompt-choice :hero "2 net damage")
      (is (= 2 (count (:discard (get-hero)))) "Runner took 2 net damage")
      (is (= 0 (count (:scored (get-hero)))) "No scored agendas")
      (prompt-choice :hero "4 net damage")
      (is (= 5 (count (:discard (get-hero)))) "Runner took 4 net damage")
      (is (= 1 (count (:scored (get-hero)))) "Scored agenda"))))

(deftest ben-musashi-fetal-ai
  ;; Check Fetal AI can be stolen #2586
  (do-game
    (new-game (default-minion [(qty "Ben Musashi" 1) (qty "Fetal AI" 1)])
              (default-hero [(qty "Sure Gamble" 5)]))
    (play-from-hand state :minion "Ben Musashi" "New remote")
    (play-from-hand state :minion "Fetal AI" "Server 1")
    (take-credits state :minion)
    (let [bm (get-content state :remote1 0)
          fai (get-content state :remote1 1)]
      (core/rez state :minion bm)
      (run-empty-server state "Server 1")
      ;; hero now chooses which to access.
      (prompt-select :hero fai)
      (prompt-choice :hero "Access")
      ;; prompt should be asking for the net damage costs
      (is (= "Fetal AI" (:title (:card (first (:prompt (get-hero))))))
          "Prompt to pay steal costs")
      (prompt-choice :hero "2 [Credits]")
      (is (= 3 (:credit (get-hero))) "Runner paid 2 credits")
      (is (= 0 (count (:scored (get-hero)))) "No scored agendas")
      (prompt-choice :hero "2 net damage")
      (is (= 4 (count (:discard (get-hero)))) "Runner took 4 net damage - 2 from Fetal, 2 from Ben")
      (is (= 1 (count (:scored (get-hero)))) "Scored agenda"))))

(deftest bernice-mai
  ;; Bernice Mai - successful and unsuccessful
  (do-game
    (new-game (default-minion [(qty "Bernice Mai" 3) (qty "Hedge Fund" 3) (qty "Wall of Static" 3)])
              (default-hero))
    (starting-hand state :minion ["Bernice Mai" "Bernice Mai" "Bernice Mai"])
    (play-from-hand state :minion "Bernice Mai" "New remote")
    (play-from-hand state :minion "Bernice Mai" "New remote")
    (play-from-hand state :minion "Bernice Mai" "R&D")
    (core/rez state :minion (get-content state :remote1 0))
    (take-credits state :minion)
    (run-empty-server state :remote1)
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (prompt-choice :hero "Yes")
    (is (= 1 (:tag (get-hero))))
    (is (= 2 (:credit (get-hero))) "Runner paid 3cr to trash Bernice")
    (core/rez state :minion (get-content state :remote2 0))
    (core/gain state :hero :credit 20)
    (run-empty-server state :remote2)
    (prompt-choice :minion 0)
    (prompt-choice :hero 10)
    (is (not (get-content state :remote2 0)) "Bernice auto-trashed from unsuccessful trace")
    (is (not (:run @state)) "Run ended when Bernice was trashed from server")
    (core/rez state :minion (get-content state :rd 0))
    (run-empty-server state :rd)
    (prompt-choice :minion 0)
    (prompt-choice :hero 10)
    (is (:card (first (:prompt (get-hero)))) "Accessing a card from R&D; not showing Bernice Mai as possible access")))

(deftest bernice-mai-drt
  ;; Bernice Mai - interaction with Dedicated Response Team
  (do-game
    (new-game (default-minion [(qty "Bernice Mai" 3) (qty "Dedicated Response Team" 1)])
              (default-hero))
    (play-from-hand state :minion "Bernice Mai" "New remote")
    (play-from-hand state :minion "Dedicated Response Team" "New remote")
    (core/rez state :minion (get-content state :remote1 0))
    (core/rez state :minion (get-content state :remote2 0))
    (take-credits state :minion)
    (run-empty-server state :remote1)
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (prompt-choice :hero "Yes")
    (is (= 1 (:tag (get-hero))))
    (is (= 2 (:credit (get-hero))) "Runner paid 3cr to trash Bernice")
    (is (= 2 (count (:discard (get-hero)))) "Runner took 1 meat damage")))

(deftest breaker-bay-grid
  ;; Breaker Bay Grid - Reduce rez cost of other cards in this server by 5 credits
  (do-game
   (new-game (default-minion [(qty "Breaker Bay Grid" 2) (qty "The Root" 1) (qty "Strongbox" 1)])
             (default-hero))
   (core/gain state :minion :click 1)
   (play-from-hand state :minion "Breaker Bay Grid" "New remote")
   (play-from-hand state :minion "The Root" "Server 1")
   (let [bbg1 (get-content state :remote1 0)
         root (get-content state :remote1 1)]
     (core/rez state :minion bbg1)
     (core/rez state :minion root)
     (is (= 4 (:credit (get-minion))) "Paid only 1 to rez The Root")
     (play-from-hand state :minion "Breaker Bay Grid" "R&D")
     (play-from-hand state :minion "Strongbox" "R&D")
     (let [bbg2 (get-content state :rd 0)
           sbox (get-content state :rd 1)]
       (core/rez state :minion bbg2)
       (core/rez state :minion sbox)
       (is (= 1 (:credit (get-minion))) "Paid full 3 credits to rez Strongbox")))))

(deftest calibration-testing
  ;; Calibration Testing - advanceable / non-advanceable
  (do-game
    (new-game (default-minion [(qty "Calibration Testing" 2) (qty "Project Junebug" 1) (qty "PAD Campaign" 1)])
              (default-hero))
    (core/gain state :minion :credit 10)
    (core/gain state :minion :click 1)
    (play-from-hand state :minion "Calibration Testing" "New remote")
    (play-from-hand state :minion "Project Junebug" "Server 1")
    (let [ct (get-content state :remote1 0)
          pj (get-content state :remote1 1)]
      (core/rez state :minion ct)
      (card-ability state :minion ct 0)
      (prompt-select :minion pj)
      (is (= 1 (:advance-counter (refresh pj))) "Project Junebug advanced")
      (is (= 1 (count (:discard (get-minion)))) "Calibration Testing trashed"))
    (play-from-hand state :minion "Calibration Testing" "New remote")
    (play-from-hand state :minion "PAD Campaign" "Server 2")
    (let [ct (get-content state :remote2 0)
          pad (get-content state :remote2 1)]
      (core/rez state :minion ct)
      (card-ability state :minion ct 0)
      (prompt-select :minion pad)
      (is (= 1 (:advance-counter (refresh pad))) "PAD Campaign advanced")
      (is (= 2 (count (:discard (get-minion)))) "Calibration Testing trashed"))))

(deftest caprice-nisei
  ;; Caprice Nisei - Psi game for ETR after hero passes last ice
  (do-game
   (new-game (default-minion [(qty "Caprice Nisei" 3) (qty "Quandary" 3)])
             (default-hero))
   (play-from-hand state :minion "Caprice Nisei" "New remote")
   (take-credits state :minion)
   (let [caprice (get-content state :remote1 0)]
     ;; Check Caprice triggers properly on no ice (and rezzed)
     (core/rez state :minion caprice)
     (run-on state "Server 1")
     (is (prompt-is-card? :minion caprice)
         "Caprice prompt even with no ice, once hero makes run")
     (is (prompt-is-card? :hero caprice) "Runner has Caprice prompt")
     (prompt-choice :minion "0 [Credits]")
     (prompt-choice :hero "1 [Credits]")
     (take-credits state :hero)


     (play-from-hand state :minion "Quandary" "Server 1")
     (play-from-hand state :minion "Quandary" "Server 1")
     (take-credits state :minion)

     ;; Check Caprice triggers properly on multiple ice
     (run-on state "Server 1")
     (run-continue state)
     (is (empty? (get-in @state [:minion :prompt])) "Caprice not trigger on first ice")
     (run-continue state) ; Caprice prompt after this
     (is (prompt-is-card? :minion caprice)
         "Corp has Caprice prompt (triggered automatically as hero passed last ice)")
     (is (prompt-is-card? :hero caprice) "Runner has Caprice prompt")
     (prompt-choice :minion "0 [Credits]")
     (prompt-choice :hero "1 [Credits]")
     (is (not (:run @state)) "Run ended by Caprice")
     (is (empty? (get-in @state [:minion :prompt])) "Caprice prompted cleared")

     ;; Check Caprice does not trigger on other servers
     (run-on state "HQ")
     (is (empty? (get-in @state [:minion :prompt])) "Caprice does not trigger on other servers"))))

(deftest chilo-city-grid
  ;; ChiLo City Grid - Give 1 tag for successful traces during runs on its server
  (do-game
    (new-game (default-minion [(qty "Caduceus" 2) (qty "ChiLo City Grid" 1)])
              (default-hero))
    (play-from-hand state :minion "ChiLo City Grid" "New remote")
    (play-from-hand state :minion "Caduceus" "Server 1")
    (take-credits state :minion)
    (let [chilo (get-content state :remote1 0)
          cad (get-ice state :remote1 0)]
      (run-on state "R&D")
      (core/rez state :minion cad)
      (core/rez state :minion chilo)
      (card-subroutine state :minion cad 0)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (= 3 (:credit (get-minion))) "Trace was successful")
      (is (= 0 (:tag (get-hero))) "No tags given for run on different server")
      (run-successful state)
      (run-on state "Server 1")
      (card-subroutine state :minion cad 0)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (= 6 (:credit (get-minion))) "Trace was successful")
      (is (= 1 (:tag (get-hero)))
          "Runner took 1 tag given from successful trace during run on ChiLo server"))))

(deftest minionorate-troubleshooter
  ;; Corporate Troubleshooter - Pay X credits and trash to add X strength to a piece of rezzed ICE
  (do-game
    (new-game (default-minion [(qty "Quandary" 2) (qty "Corporate Troubleshooter" 1)])
              (default-hero))
    (core/gain state :minion :credit 5)
    (play-from-hand state :minion "Corporate Troubleshooter" "HQ")
    (play-from-hand state :minion "Quandary" "HQ")
    (play-from-hand state :minion "Quandary" "HQ")
    (let [ct (get-content state :hq 0)
          q1 (get-ice state :hq 0)
          q2 (get-ice state :hq 1)]
      (core/rez state :minion q1)
      (is (= 8 (:credit (get-minion))))
      (core/rez state :minion ct)
      (card-ability state :minion ct 0)
      (prompt-choice :minion 5)
      (prompt-select :minion q2)
      (is (nil? (:current-strength (refresh q2))) "Outer Quandary unrezzed; can't be targeted")
      (prompt-select :minion q1)
      (is (= 5 (:current-strength (refresh q1))) "Inner Quandary boosted to 5 strength")
      (is (empty? (get-content state :hq))
          "Corporate Troubleshooter trashed from root of HQ")
      (take-credits state :minion)
      (is (= 0 (:current-strength (refresh q1)))
          "Inner Quandary back to default 0 strength after turn ends"))))

(deftest crisium-grid
  ;; Crisium Grid - various interactions
  (do-game
    (new-game (default-minion [(qty "Crisium Grid" 2)])
              (default-hero [(qty "Desperado" 1) (qty "Temüjin Contract" 1)]))
    (play-from-hand state :minion "Crisium Grid" "HQ")
    (core/rez state :minion (get-content state :hq 0))
    (take-credits state :minion)
    (is (= 4 (:credit (get-minion))) "Corp has 4 credits")
    (core/gain state :hero :credit 4)
    (play-from-hand state :hero "Desperado")
    (play-from-hand state :hero "Temüjin Contract")
    (prompt-choice :hero "HQ")
    (run-empty-server state "HQ")
    (is (= 2 (:credit (get-hero))) "No Desperado or Temujin credits")
    (is (not (:successful-run (:register (get-hero)))) "No successful run in register")))

(deftest cyberdex-virus-suite-purge
  ;; Cyberdex Virus Suite - Purge ability
  (do-game
    (new-game (default-minion [(qty "Cyberdex Virus Suite" 3)])
              (default-hero [(qty "Cache" 1) (qty "Medium" 1)]))
    (play-from-hand state :minion "Cyberdex Virus Suite" "HQ")
    (take-credits state :minion 2)
    ;; hero's turn
    ;; install cache and medium
    (play-from-hand state :hero "Cache")
    (let [virus-counters (fn [card] (core/get-virus-counters state :hero (refresh card)))
          cache (find-card "Cache" (get-in @state [:hero :rig :program]))
          cvs (get-content state :hq 0)]
      (is (= 3 (virus-counters cache)))
      (play-from-hand state :hero "Medium")
      (take-credits state :hero 2)
      (core/rez state :minion cvs)
      (card-ability state :minion cvs 0)
      ;; nothing in hq content
      (is (empty? (get-content state :hq)) "CVS was trashed")
      ;; purged counters
      (is (zero? (virus-counters cache))
          "Cache has no counters")
      (is (zero? (virus-counters (find-card "Medium" (get-in @state [:hero :rig :program]))))
          "Medium has no counters"))))

(deftest cyberdex-virus-suite-access
  ;; Cyberdex Virus Suite - Purge on access
  (do-game
    (new-game (default-minion [(qty "Cyberdex Virus Suite" 3)])
              (default-hero [(qty "Cache" 1) (qty "Medium" 1)]))
    (play-from-hand state :minion "Cyberdex Virus Suite" "New remote")
    (take-credits state :minion 2)
    ;; hero's turn
    ;; install cache and medium
    (play-from-hand state :hero "Cache")
    (let [virus-counters (fn [card] (core/get-virus-counters state :hero (refresh card)))
          cache (find-card "Cache" (get-in @state [:hero :rig :program]))
          cvs (get-content state :remote1 0)]
      (is (= 3 (virus-counters cache)))
      (play-from-hand state :hero "Medium")
      (run-empty-server state "Server 1")
      ;; minion now has optional prompt to trigger virus purge
      (prompt-choice :minion "Yes")
      ;; hero has prompt to trash CVS
      (prompt-choice :hero "Yes")
      ;; purged counters
      (is (zero? (virus-counters cache))
          "Cache has no counters")
      (is (zero? (virus-counters (find-card "Medium" (get-in @state [:hero :rig :program]))))
          "Medium has no counters"))))

(deftest cyberdex-virus-suite-archives-access
  ;; Cyberdex Virus Suite - Don't interrupt archives access. Issue #1647.
  (do-game
    (new-game (default-minion [(qty "Cyberdex Virus Suite" 1) (qty "Braintrust" 1)])
              (default-hero [(qty "Cache" 1)]))
    (trash-from-hand state :minion "Cyberdex Virus Suite")
    (trash-from-hand state :minion "Braintrust")
    (take-credits state :minion)
    ;; hero's turn
    ;; install cache
    (play-from-hand state :hero "Cache")
    (let [cache (get-program state 0)]
      (is (= 3 (get-counters (refresh cache) :virus)))
      (run-empty-server state "Archives")
      (prompt-choice :hero "Cyberdex Virus Suite")
      (prompt-choice :minion "Yes")
      (is (pos? (count (:prompt (get-hero)))) "CVS purge did not interrupt archives access")
      ;; purged counters
      (is (zero? (get-counters (refresh cache) :virus))
          "Cache has no counters"))))

(deftest forced-connection
  ;; Forced Connection - ambush, trace(3) give the hero 2 tags
  (do-game
    (new-game (default-minion [(qty "Forced Connection" 3)])
              (default-hero))
    (starting-hand state :minion ["Forced Connection" "Forced Connection"])
    (play-from-hand state :minion "Forced Connection" "New remote")
    (take-credits state :minion)
    (is (= 0 (:tag (get-hero))) "Runner starts with 0 tags")
    (run-empty-server state :remote1)
    (prompt-choice :minion 0)
    (prompt-choice :hero 0)
    (prompt-choice :hero "Yes") ; trash
    (is (= 2 (:tag (get-hero))) "Runner took two tags")
    (run-empty-server state "Archives")
    (is (= 2 (:tag (get-hero))) "Runner doesn't take tags when accessed from Archives")
    (run-empty-server state "HQ")
    (prompt-choice :minion 0)
    (prompt-choice :hero 3)
    (prompt-choice :hero "Yes") ; trash
    (is (= 2 (:tag (get-hero))) "Runner doesn't take tags when trace won")))

(deftest ghost-branch-dedicated-response-team
  ;; Ghost Branch - with Dedicated Response Team
  (do-game
    (new-game (default-minion [(qty "Ghost Branch" 1) (qty "Dedicated Response Team" 1)])
              (default-hero))
    (play-from-hand state :minion "Ghost Branch" "New remote")
    (play-from-hand state :minion "Dedicated Response Team" "New remote")
    (core/gain state :minion :click 1)
    (let [gb (get-content state :remote1 0)
          drt (get-content state :remote2 0)]
      (core/advance state :minion {:card gb})
      (core/advance state :minion {:card (refresh gb)})
      (is (= 2 (:advance-counter (refresh gb))) "Ghost Branch advanced twice")
      (take-credits state :minion)
      (run-on state "Server 1")
      (core/rez state :minion drt)
      (run-successful state)
      (is (prompt-is-type? :hero :waiting) "Runner has prompt to wait for Ghost Branch")
      (prompt-choice :minion "Yes")
      (is (= 2 (:tag (get-hero))) "Runner has 2 tags")
      (prompt-choice :hero "Yes")
      (is (= 2 (count (:discard (get-hero)))) "Runner took 2 meat damage"))))

(deftest georgia-emelyov
  ;; Georgia Emelyov
  (do-game
    (new-game (default-minion [(qty "Georgia Emelyov" 1)])
              (default-hero))
    (play-from-hand state :minion "Georgia Emelyov" "New remote")
    (let [geo (get-content state :remote1 0)]
      (core/rez state :minion geo)
      (take-credits state :minion)
      (run-on state "Server 1")
      (run-jack-out state)
      (is (= 1 (count (:discard (get-hero)))) "Runner took 1 net damage")
      (card-ability state :minion (refresh geo) 0)
      (prompt-choice :minion "Archives")
      (let [geo (get-content state :archives 0)]
        (is geo "Georgia moved to Archives")
        (run-on state "Archives")
        (run-jack-out state)
        (is (= 2 (count (:discard (get-hero)))) "Runner took 1 net damage")
        (run-on state "HQ")
        (run-jack-out state)
        (is (= 2 (count (:discard (get-hero)))) "Runner did not take damage")))))

(deftest helheim-servers
  ;; Helheim Servers - Full test
  (do-game
    (new-game (default-minion [(qty "Helheim Servers" 1) (qty "Gutenberg" 1) (qty "Vanilla" 1)
                             (qty "Jackson Howard" 1) (qty "Hedge Fund" 1)])
              (default-hero))
    (play-from-hand state :minion "Helheim Servers" "R&D")
    (play-from-hand state :minion "Gutenberg" "R&D")
    (play-from-hand state :minion "Vanilla" "R&D")
    (take-credits state :minion)
    (run-on state "R&D")
    (is (:run @state))
    (let [helheim (get-content state :rd 0)
          gutenberg (get-ice state :rd 0)
          vanilla (get-ice state :rd 1)]
      (core/rez state :minion helheim)
      (core/rez state :minion gutenberg)
      (core/rez state :minion vanilla)
      (is (= 6 (:current-strength (refresh gutenberg))))
      (is (= 0 (:current-strength (refresh vanilla))))
      (card-ability state :minion helheim 0)
      (prompt-select :minion (find-card "Jackson Howard" (:hand (get-minion))))
      (is (= 1 (count (:discard (get-minion)))))
      (is (= 8 (:current-strength (refresh gutenberg))))
      (is (= 2 (:current-strength (refresh vanilla))))
      (card-ability state :minion helheim 0)
      (prompt-select :minion (find-card "Hedge Fund" (:hand (get-minion))))
      (is (= 2 (count (:discard (get-minion)))))
      (is (= 10 (:current-strength (refresh gutenberg))))
      (is (= 4 (:current-strength (refresh vanilla))))
      (run-jack-out state)
      (is (not (:run @state)))
      (is (= 6 (:current-strength (refresh gutenberg))))
      (is (= 0 (:current-strength (refresh vanilla)))))))

(deftest hokusai-grid
  ;; Hokusai Grid - Do 1 net damage when run successful on its server
  (do-game
    (new-game (default-minion [(qty "Hokusai Grid" 1)])
              (default-hero))
    (play-from-hand state :minion "Hokusai Grid" "HQ")
    (take-credits state :minion)
    (core/rez state :minion (get-content state :hq 0))
    (run-empty-server state :rd)
    (is (empty? (:discard (get-hero))) "No net damage done for successful run on R&D")
    (run-empty-server state :hq)
    (is (= 1 (count (:discard (get-hero)))) "1 net damage done for successful run on HQ")))

(deftest jinja-city-grid
  ;; Jinja City Grid - install drawn ice, lowering install cost by 4
  (do-game
    (new-game (default-minion [(qty "Jinja City Grid" 1) (qty "Vanilla" 3) (qty "Ice Wall" 3)])
              (default-hero))
    (starting-hand state :minion ["Jinja City Grid"])
    (core/gain state :minion :click 6)
    (play-from-hand state :minion "Jinja City Grid" "New remote")
    (core/rez state :minion (get-content state :remote1 0))
    (dotimes [n 5]
      (core/click-draw state :minion 1)
      (prompt-choice :minion "Yes")
      (is (= 4 (:credit (get-minion))) "Not charged to install ice")
      (is (= (inc n) (count (get-in @state [:minion :servers :remote1 :ices]))) (str n " ICE protecting Remote1")))
    (core/click-draw state :minion 1)
    (prompt-choice :minion "Yes")
    (is (= 3 (:credit (get-minion))) "Charged to install ice")
    (is (= 6 (count (get-in @state [:minion :servers :remote1 :ices]))) "6 ICE protecting Remote1")))

(deftest keegan-lane
  ;; Keegan Lane - Trash self and remove 1 Runner tag to trash a program
  (do-game
    (new-game (default-minion [(qty "Keegan Lane" 1)])
              (default-hero [(qty "Corroder" 1)]))
    (play-from-hand state :minion "Keegan Lane" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Corroder")
    (run-on state :hq)
    (let [keeg (get-content state :hq 0)]
      (core/rez state :minion keeg)
      (card-ability state :minion keeg 0)
      (is (= 1 (count (get-content state :hq))) "Keegan didn't fire, Runner has no tags")
      (core/gain state :hero :tag 2)
      (card-ability state :minion keeg 0)
      (prompt-select :minion (get-program state 0))
      (is (= 1 (:tag (get-hero))) "1 tag removed")
      (is (= 1 (count (:discard (get-minion)))) "Keegan trashed")
      (is (= 1 (count (:discard (get-hero)))) "Corroder trashed"))))

(deftest manta-grid
  ;; If the Runner has fewer than 6 or no unspent clicks on successful run, minion gains a click next turn.
  (do-game
    (new-game (default-minion [(qty "Manta Grid" 1)])
              (default-hero))
    (starting-hand state :hero [])
    (is (= 3 (:click (get-minion))) "Corp has 3 clicks")
    (play-from-hand state :minion "Manta Grid" "HQ")
    (core/rez state :minion (get-content state :hq 0))
    (take-credits state :minion)
    (core/click-draw state :hero nil)
    (core/click-draw state :hero nil)
    (run-empty-server state "HQ")
    (prompt-choice :hero "No") ; don't trash Manta Grid
    (is (= 1 (:click (get-hero))) "Running last click")
    (run-empty-server state "HQ")
    (prompt-choice :hero "No") ; don't trash Manta Grid
    (take-credits state :hero)
    (is (= 5 (:click (get-minion))) "Corp gained 2 clicks due to 2 runs with < 6 Runner credits")
    (take-credits state :minion)
    (take-credits state :hero)
    (is (= 3 (:click (get-minion))) "Corp back to 3 clicks")
    (take-credits state :minion)
    (take-credits state :hero 3)
    (run-empty-server state "HQ")
    (prompt-choice :hero "No") ; don't trash Manta Grid
    (take-credits state :hero)
    (is (= 4 (:click (get-minion))) "Corp gained a click due to running last click")))

(deftest marcus-batty-security-nexus
  ;; Marcus Batty - Simultaneous Interaction with Security Nexus
  (do-game
    (new-game (default-minion [(qty "Marcus Batty" 1) (qty "Enigma" 1)])
              (default-hero [(qty "Security Nexus" 1)]))
    (play-from-hand state :minion "Marcus Batty" "HQ")
    (play-from-hand state :minion "Enigma" "HQ")
    (take-credits state :minion)
    (core/gain state :hero :credit 8)
    (play-from-hand state :hero "Security Nexus")
    (let [mb (get-content state :hq 0)
          en (get-ice state :hq 0)
          sn (-> @state :hero :rig :hardware first)]
      (run-on state "HQ")
      (core/rez state :minion mb)
      (core/rez state :minion en)
      (card-ability state :minion mb 0)
      (card-ability state :hero sn 0)
      ;; both prompts should be on Batty
      (is (prompt-is-card? :minion mb) "Corp prompt is on Marcus Batty")
      (is (prompt-is-card? :hero mb) "Runner prompt is on Marcus Batty")
      (prompt-choice :minion "0")
      (prompt-choice :hero "0")
      (is (prompt-is-card? :minion sn) "Corp prompt is on Security Nexus")
      (is (prompt-is-type? :hero :waiting) "Runner prompt is waiting for Corp"))))

(deftest mumbad-virtual-tour-force-trash
  ;; Tests that Mumbad Virtual Tour forces trash when no :slow-trash
  (do-game
    (new-game (default-minion [(qty "Mumbad Virtual Tour" 2)])
              (default-hero))
    (play-from-hand state :minion "Mumbad Virtual Tour" "New remote")
    (take-credits state :minion)
    (run-empty-server state "HQ")
    ;; MVT does not force trash when not installed
    (prompt-choice :hero "No")
    (is (= 5 (:credit (get-hero))) "Runner not forced to trash MVT in HQ")
    (is (empty? (:discard (get-minion))) "MVT in HQ is not trashed")
    (run-empty-server state "Server 1")
    ;; Toast should show at this point to notify hero they were forced to trash MVT
    (is (= 0 (:credit (get-hero))) "Runner forced to trash MVT")
    (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-minion))))) "MVT trashed")))

(deftest mumbad-virtual-tour-slow-trash
  ;; Tests that Mumbad Virtual Tour does not force trash with :slow-trash
  (do-game
    (new-game (default-minion [(qty "Mumbad Virtual Tour" 2)])
              (default-hero [(qty "Imp" 1)]))
    (play-from-hand state :minion "Mumbad Virtual Tour" "New remote")
    (play-from-hand state :minion "Mumbad Virtual Tour" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Imp")
    ;; Reset credits to 5
    (core/gain state :hero :credit 2)
    (run-empty-server state "Server 1")
    ;; Runner not force to trash since Imp is installed
    (is (= 5 (:credit (get-hero))) "Runner not forced to trash MVT when Imp installed")
    (is (empty? (:discard (get-minion))) "MVT is not force-trashed when Imp installed")
    (let [imp (get-program state 0)]
      (card-ability state :hero imp 0)
      (is (= "Mumbad Virtual Tour" (:title (first (:discard (get-minion)))))
          "MVT trashed with Imp")
      ;; Trash Imp to reset :slow-trash flag
      (core/move state :hero (refresh imp) :discard)
      (is (not (core/any-flag-fn? state :hero :slow-trash true))))))

(deftest neotokyo-grid
  ;; NeoTokyo Grid - Gain 1c the first time per turn a card in this server gets an advancement
  (do-game
    (new-game (default-minion [(qty "NeoTokyo Grid" 1) (qty "Nisei MK II" 1)
                             (qty "Shipment from SanSan" 1) (qty "Ice Wall" 1)])
              (default-hero))
    (core/gain state :minion :click 2)
    (play-from-hand state :minion "NeoTokyo Grid" "New remote")
    (play-from-hand state :minion "Nisei MK II" "Server 1")
    (core/rez state :minion (get-content state :remote1 0))
    (let [nis (get-content state :remote1 1)]
      (play-from-hand state :minion "Shipment from SanSan")
      (prompt-choice :minion "2")
      (prompt-select :minion nis)
      (is (= 2 (:advance-counter (refresh nis))) "2 advancements on agenda")
      (is (= 4 (:credit (get-minion))) "Gained 1 credit")
      (core/advance state :minion {:card (refresh nis)})
      (is (= 3 (:advance-counter (refresh nis))) "3 advancements on agenda")
      (is (= 3 (:credit (get-minion))) "No credit gained")
      (take-credits state :minion)
      (take-credits state :hero)
      (play-from-hand state :minion "Ice Wall" "Server 1")
      (core/advance state :minion {:card (refresh (get-ice state :remote1 0))})
      (is (= 2 (:credit (get-minion))) "No credit gained from advancing ICE"))))

(deftest off-the-grid
  ;; Off the Grid run ability - and interaction with RP
  (do-game
   (new-game
    (make-deck "Jinteki: Replicating Perfection" [(qty "Off the Grid" 3)
                                                  (qty "Mental Health Clinic" 3)])
    (default-hero))
   (play-from-hand state :minion "Off the Grid" "New remote")
   (play-from-hand state :minion "Mental Health Clinic" "Server 1")
   (let [otg (get-content state :remote1 0)]
     (take-credits state :minion)
     (core/rez state :minion (refresh otg))
     (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
     (run-empty-server state "R&D")
     (is (not (core/can-run-server? state "Server 1")) "Runner cannot run on Off the Grid")
     (take-credits state :hero)
     (take-credits state :minion)
     (is (not (core/can-run-server? state "Server 1")) "Off the Grid prevention persisted")
     (run-empty-server state "HQ")
     (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on Server 1")
     (is (= nil (refresh otg)) "Off the Grid trashed"))))

(deftest old-hollywood-grid
  ;; Old Hollywood Grid - Ability
  (do-game
    (new-game (default-minion [(qty "Old Hollywood Grid" 1) (qty "House of Knives" 3)])
              (default-hero))
    (play-from-hand state :minion "Old Hollywood Grid" "New remote")
    (play-from-hand state :minion "House of Knives" "Server 1")
    (take-credits state :minion 1)
    (let [ohg (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (run-on state "Server 1")
      (core/rez state :minion ohg)
      (run-successful state)
      ;; hero now chooses which to access.
      (prompt-select :hero hok)
      ;; prompt shows "You cannot steal"
      (prompt-choice :hero "OK")
      (is (= 0 (count (:scored (get-hero)))) "No stolen agendas")
      (prompt-select :hero ohg)
      (prompt-choice :hero "No")
      (core/steal state :hero (find-card "House of Knives" (:hand (get-minion))))
      (run-empty-server state "Server 1")
      (prompt-select :hero hok)
      (prompt-choice :hero "Yes")
      (is (= 2 (count (:scored (get-hero)))) "2 stolen agendas"))))

(deftest old-hollywood-grid-central
  ;; Old Hollywood Grid - Central server
  (do-game
    (new-game (default-minion [(qty "Old Hollywood Grid" 1) (qty "House of Knives" 3)])
              (default-hero))
    (play-from-hand state :minion "Old Hollywood Grid" "HQ")
    (take-credits state :minion 2)
    (let [ohg (get-content state :hq 0)]
      (run-on state "HQ")
      (core/rez state :minion ohg)
      (run-successful state)
      ;; hero now chooses which to access.
      (prompt-choice :hero "Card from hand")
      ;; prompt shows "You cannot steal"
      (prompt-choice :hero "OK")
      (is (= 0 (count (:scored (get-hero)))) "No stolen agendas")
      (prompt-choice :hero "Old Hollywood Grid")
      ;; trash OHG
      (prompt-choice :hero "Yes")
      (run-empty-server state "HQ")
      (prompt-choice :hero "Steal")
      (is (= 1 (count (:scored (get-hero)))) "1 stolen agenda"))))

(deftest old-hollywood-grid-gang-sign
  ;; Old Hollywood Grid - Gang Sign interaction. Prevent the steal outside of a run. #2169
  (do-game
    (new-game (default-minion [(qty "Old Hollywood Grid" 1) (qty "Project Beale" 2)])
              (default-hero [(qty "Gang Sign" 1)]))
    (play-from-hand state :minion "Old Hollywood Grid" "HQ")
    (play-from-hand state :minion "Project Beale" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Gang Sign")
    (take-credits state :hero)
    (core/rez state :minion (get-content state :hq 0))
    (score-agenda state :minion (get-content state :remote1 0))
    ;; Gang sign fires
    (prompt-choice :hero "Card from hand")
    ;; prompt shows "You cannot steal"
    (prompt-choice :hero "OK")
    (is (= 0 (count (:scored (get-hero)))) "No stolen agendas")))

(deftest port-anson-grid
  ;; Port Anson Grid - Prevent the Runner from jacking out until they trash a program
  (do-game
    (new-game (default-minion [(qty "Port Anson Grid" 1) (qty "Data Raven" 1)])
              (default-hero [(qty "Faerie" 1) (qty "Technical Writer" 1)]))
    (play-from-hand state :minion "Port Anson Grid" "New remote")
    (play-from-hand state :minion "Data Raven" "Server 1")
    (take-credits state :minion)
    (play-from-hand state :hero "Technical Writer")
    (play-from-hand state :hero "Faerie")
    (let [pag (get-content state :remote1 0)
          fae (get-in @state [:hero :rig :program 0])
          tw (get-in @state [:hero :rig :resource 0])]
      (run-on state "Server 1")
      (core/rez state :minion pag)
      (is (:cannot-jack-out (get-in @state [:run])) "Jack out disabled for Runner") ; UI button greyed out
      (core/trash state :hero tw)
      (is (:cannot-jack-out (get-in @state [:run])) "Resource trash didn't disable jack out prevention")
      (core/trash state :hero fae)
      (is (nil? (:cannot-jack-out (get-in @state [:run]))) "Jack out enabled by program trash")
      (run-on state "Server 1")
      (is (:cannot-jack-out (get-in @state [:run])) "Prevents jack out when upgrade is rezzed prior to run"))))

(deftest prisec
  ;; Prisec - Pay 2 credits to give hero 1 tag and do 1 meat damage, only when installed
  (do-game
    (new-game (default-minion [(qty "Prisec" 2)])
              (default-hero))
    (play-from-hand state :minion "Prisec" "New remote")
    (take-credits state :minion)
    (run-empty-server state "Server 1")
    (let [pre-creds (:credit (get-minion))]
      (prompt-choice :minion "Yes")
      (is (= (- pre-creds 2) (:credit (get-minion))) "Pay 2 [Credits] to pay for Prisec"))
    (is (= 1 (:tag (get-hero))) "Give hero 1 tag")
    (is (= 1 (count (:discard (get-hero)))) "Prisec does 1 damage")
    ;; Runner trashes Prisec
    (prompt-choice :hero "Yes")
    (run-empty-server state "HQ")
    (is (not (:prompt @state)) "Prisec does not trigger from HQ")))

(deftest prisec-dedicated-response-team
  ;; Multiple unrezzed upgrades in Archives interaction with DRT.
  (do-game
    (new-game (default-minion [(qty "Prisec" 2) (qty "Dedicated Response Team" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Diesel" 3)]))
    (play-from-hand state :minion "Dedicated Response Team" "New remote")
    (play-from-hand state :minion "Prisec" "Archives")
    (play-from-hand state :minion "Prisec" "Archives")
    (core/gain state :minion :click 1 :credit 14)
    (core/rez state :minion (get-content state :remote1 0))
    (take-credits state :minion)

    (run-empty-server state :archives)
    (is (:run @state) "Run still active")
    (prompt-choice :hero "Unrezzed upgrade in Archives")
    (prompt-select :hero (get-content state :archives 0))
    (prompt-choice :minion "Yes") ; minion pay for PriSec
    (prompt-choice :hero "No") ; hero don't pay to trash
    (is (:run @state) "Run still active")
    (prompt-choice :hero "Unrezzed upgrade in Archives")
    (prompt-choice :minion "Yes") ; minion pay for PriSec
    (prompt-choice :hero "No") ; hero don't pay to trash
    (is (not (:run @state)) "Run ended")
    (is (= 4 (count (:discard (get-hero)))) "Runner took 4 meat damage")))

(deftest product-placement
  ;; Product Placement - Gain 2 credits when Runner accesses it
  (do-game
    (new-game (default-minion [(qty "Product Placement" 1)])
              (default-hero))
    (play-from-hand state :minion "Product Placement" "New remote")
    (take-credits state :minion)
    (is (= 7 (:credit (get-minion))))
    (let [pp (get-content state :remote1 0)]
      (run-empty-server state "Server 1")
      (is (= 9 (:credit (get-minion))) "Gained 2 credits from Runner accessing Product Placement")
      (prompt-choice :hero "Yes") ; Runner trashes PP
      (run-empty-server state "Archives")
      (is (= 9 (:credit (get-minion)))
          "No credits gained when Product Placement accessed in Archives"))))

(deftest red-herrings
  ;; Red Herrings - Ability
  (do-game
    (new-game (default-minion [(qty "Red Herrings" 1) (qty "House of Knives" 1)])
              (default-hero))
    (play-from-hand state :minion "Red Herrings" "New remote")
    (play-from-hand state :minion "House of Knives" "Server 1")
    (take-credits state :minion 1)

    (let [rh (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (core/rez state :minion rh)
      (run-empty-server state "Server 1")
      ;; hero now chooses which to access.
      (prompt-select :hero hok)
      ;; prompt should be asking for the 5cr cost
      (is (= "House of Knives" (:title (:card (first (:prompt (get-hero))))))
          "Prompt to pay 5cr")
      (prompt-choice :hero "No")
      (is (= 5 (:credit (get-hero))) "Runner was not charged 5cr")
      (is (= 0 (count (:scored (get-hero)))) "No scored agendas")
      (prompt-select :hero rh)
      (prompt-choice :hero "No")
      (run-empty-server state "Server 1")
      (prompt-select :hero hok)
      (prompt-choice :hero "Yes")
      (is (= 0 (:credit (get-hero))) "Runner was charged 5cr")
      (is (= 1 (count (:scored (get-hero)))) "1 scored agenda"))))

(deftest red-herrings-trash
  ;; Red Herrings - Cost increase even when trashed
  (do-game
    (new-game (default-minion [(qty "Red Herrings" 3) (qty "House of Knives" 3)])
              (default-hero))
    (play-from-hand state :minion "Red Herrings" "New remote")
    (play-from-hand state :minion "House of Knives" "Server 1")
    (take-credits state :minion 1)
    (core/gain state :hero :credit 1)
    (let [rh (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (core/rez state :minion rh)
      (run-empty-server state "Server 1")
      ;; hero now chooses which to access.
      (prompt-select :hero rh)
      (prompt-choice :hero "Yes") ; pay to trash
      (prompt-select :hero hok)
      ;; should now have prompt to pay 5cr for HoK
      (prompt-choice :hero "Yes")
      (is (= 0 (:credit (get-hero))) "Runner was charged 5cr")
      (is (= 1 (count (:scored (get-hero)))) "1 scored agenda"))))

(deftest red-herrings-trash-from-hand
  ;; Red Herrings - Trashed from Hand
  (do-game
    (new-game (default-minion [(qty "Red Herrings" 1) (qty "House of Knives" 1)])
              (default-hero))
    (trash-from-hand state :minion "Red Herrings")
    (is (= 1 (count (:discard (get-minion)))) "1 card in Archives")
    (take-credits state :minion)

    (run-empty-server state "HQ")
    ;; prompt should be asking to steal HoK
    (is (= "Steal" (first (:choices (first (:prompt (get-hero))))))
        "Runner being asked to Steal")))

(deftest red-herrings-other-server
  ;; Red Herrings - Don't affect runs on other servers
  (do-game
    (new-game (default-minion [(qty "Red Herrings" 1) (qty "House of Knives" 1)])
              (default-hero))
    (play-from-hand state :minion "Red Herrings" "New remote")
    (play-from-hand state :minion "House of Knives" "New remote")
    (take-credits state :minion 1)

    (let [rh (get-content state :remote1 0)]
      (core/rez state :minion rh)
      (run-empty-server state "Server 2")
      ;; access is automatic
      (prompt-choice :hero "Steal")
      (is (= 5 (:credit (get-hero))) "Runner was not charged 5cr")
      (is (= 1 (count (:scored (get-hero)))) "1 scored agenda"))))

(deftest ruhr-valley
  ;; Ruhr Valley - As an additional cost to make a run on this server, the Runner must spend a click.
  (do-game
    (new-game (default-minion [(qty "Ruhr Valley" 1)])
              (default-hero))
    (play-from-hand state :minion "Ruhr Valley" "HQ")
    (take-credits state :minion)
    (let [ruhr (get-content state :hq 0)]
      (core/rez state :minion ruhr)
      (is (= 4 (:click (get-hero))))
      (run-on state :hq)
      (run-jack-out state)
      (is (= 2 (:click (get-hero))))
      (take-credits state :hero 1)
      (is (= 1 (:click (get-hero))))
      (is (not (core/can-run-server? state "HQ")) "Runner can't run - no additional clicks")
      (take-credits state :hero)
      (take-credits state :minion)
      (is (= 4 (:click (get-hero))))
      (is (= 7 (:credit (get-hero))))
      (run-on state :hq)
      (run-successful state)
      (prompt-choice :hero "Yes") ; pay to trash / 7 cr - 4 cr
      (is (= 2 (:click (get-hero))))
      (is (= 3 (:credit (get-hero))))
      (run-on state :hq)
      (run-jack-out state)
      (is (= 1 (:click (get-hero)))))))

(deftest ruhr-valley-enable-state
  ;; Ruhr Valley - If the hero trashes with one click left, the ability to run is enabled
  (do-game
    (new-game (default-minion [(qty "Ruhr Valley" 1)])
              (default-hero))
    (play-from-hand state :minion "Ruhr Valley" "HQ")
    (take-credits state :minion)
    (let [ruhr (get-content state :hq 0)]
      (core/rez state :minion ruhr)
      (is (= 4 (:click (get-hero))))
      (run-on state :rd)
      (run-jack-out state)
      (is (= 3 (:click (get-hero))))
      (run-on state :hq)
      (run-successful state)
      (prompt-choice :hero "Yes") ; pay to trash / 6 cr - 4 cr
      (is (= 1 (:click (get-hero))))
      (run-on state :hq))))

(deftest ryon-knight
  ;; Ryon Knight - Trash during run to do 1 brain damage if Runner has no clicks remaining
  (do-game
    (new-game (default-minion [(qty "Ryon Knight" 1)])
              (default-hero))
    (play-from-hand state :minion "Ryon Knight" "HQ")
    (take-credits state :minion)
    (let [ryon (get-content state :hq 0)]
      (run-on state :hq)
      (core/rez state :minion ryon)
      (card-ability state :minion ryon 0)
      (is (= 3 (:click (get-hero))))
      (is (= 0 (:brain-damage (get-hero))))
      (is (= 1 (count (get-content state :hq))) "Ryon ability didn't fire with 3 Runner clicks left")
      (run-jack-out state)
      (take-credits state :hero 2)
      (run-on state :hq)
      (card-ability state :minion ryon 0)
      (is (= 0 (:click (get-hero))))
      (is (= 1 (:brain-damage (get-hero))) "Did 1 brain damage")
      (is (= 1 (count (:discard (get-minion)))) "Ryon trashed"))))

(deftest satellite-grid
  ;; Satellite Grid - Add 1 fake advancement on all ICE protecting server
  (do-game
    (new-game (default-minion [(qty "Satellite Grid" 1) (qty "Ice Wall" 2)])
              (default-hero))
    (play-from-hand state :minion "Satellite Grid" "HQ")
    (play-from-hand state :minion "Ice Wall" "HQ")
    (play-from-hand state :minion "Ice Wall" "R&D")
    (let [iw1 (get-ice state :hq 0)
          iw2 (get-ice state :rd 0)
          sg (get-content state :hq 0)]
      (core/gain state :minion :click 1)
      (advance state iw1)
      (core/rez state :minion sg)
      (core/rez state :minion (refresh iw1))
      (is (= 1 (:extra-advance-counter (refresh iw1))) "1 fake advancement token")
      (is (= 1 (:advance-counter (refresh iw1))) "Only 1 real advancement token")
      (is (= 3 (:current-strength (refresh iw1))) "Satellite Grid counter boosting strength by 1")
      (core/rez state :minion (refresh iw2))
      (is (= 1 (:current-strength (refresh iw2))) "Satellite Grid not impacting ICE elsewhere")
      (core/derez state :minion sg)
      (is (= 2 (:current-strength (refresh iw1))) "Ice Wall strength boost only from real advancement"))))

(deftest signal-jamming
  ;; Trash to stop installs for the rest of the run
  (do-game
    (new-game (default-minion [(qty "Signal Jamming" 3)])
              (default-hero [(qty "Self-modifying Code" 3) (qty "Reaver" 1)]))
    (starting-hand state :hero ["Self-modifying Code" "Self-modifying Code"])
    (play-from-hand state :minion "Signal Jamming" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Self-modifying Code")
    (play-from-hand state :hero "Self-modifying Code")
    (let [smc1 (get-in @state [:hero :rig :program 0])
          smc2 (get-in @state [:hero :rig :program 1])
          sj (get-content state :hq 0)]
      (core/rez state :minion sj)
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :minion sj 0)
      (card-ability state :hero smc1 0)
      (is (empty? (:prompt (get-hero))) "SJ blocking SMC")
      (run-jack-out state)
      (card-ability state :hero smc2 0)
      (prompt-card :hero (find-card "Reaver" (:deck (get-hero)))))))

(deftest strongbox
  ;; Strongbox - Ability
  (do-game
    (new-game (default-minion [(qty "Strongbox" 1) (qty "House of Knives" 1)])
              (default-hero))
    (play-from-hand state :minion "Strongbox" "New remote")
    (play-from-hand state :minion "House of Knives" "Server 1")
    (take-credits state :minion 1)

    (let [sb (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (core/rez state :minion sb)
      (run-empty-server state "Server 1")
      (prompt-select :hero hok)
      (is (= "House of Knives" (:title (:card (first (:prompt (get-hero))))))
          "Prompt to pay 5cr")
      (prompt-choice :hero "No")
      (is (= 3 (:click (get-hero))) "Runner was not charged 1click")
      (is (= 0 (count (:scored (get-hero)))) "No scored agendas")
      (prompt-select :hero sb)
      (prompt-choice :hero "No")
      (run-empty-server state "Server 1")
      (prompt-select :hero hok)
      (prompt-choice :hero "Yes")
      (is (= 1 (:click (get-hero))) "Runner was charged 1click")
      (is (= 1 (count (:scored (get-hero)))) "1 scored agenda"))))

(deftest strongbox-trash
  ;; Strongbox - Click cost even when trashed
  (do-game
    (new-game (default-minion [(qty "Strongbox" 3) (qty "House of Knives" 3)])
              (default-hero))
    (play-from-hand state :minion "Strongbox" "New remote")
    (play-from-hand state :minion "House of Knives" "Server 1")
    (take-credits state :minion 1)

    (core/gain state :hero :credit 1)
    (let [sb (get-content state :remote1 0)
          hok (get-content state :remote1 1)]
      (core/rez state :minion sb)
      (run-empty-server state "Server 1")
      (prompt-select :hero sb)
      (prompt-choice :hero "Yes") ; pay to trash
      (prompt-select :hero hok)
      (prompt-choice :hero "Yes")
      (is (= 2 (:click (get-hero))) "Runner was charged 1click")
      (is (= 1 (count (:scored (get-hero)))) "1 scored agenda"))))

(deftest surat-city-grid
  ;; Surat City Grid - Trigger on rez of a card in/protecting same server to rez another card at 2c discount
  (do-game
    (new-game (default-minion [(qty "Surat City Grid" 2) (qty "Cyberdex Virus Suite" 2)
                             (qty "Enigma" 1) (qty "Wraparound" 1)])
              (default-hero))
    (core/gain state :minion :credit 15 :click 8)
    (play-from-hand state :minion "Surat City Grid" "New remote")
    (play-from-hand state :minion "Wraparound" "Server 1")
    (play-from-hand state :minion "Cyberdex Virus Suite" "Server 1")
    (let [scg1 (get-content state :remote1 0)
          cvs1 (get-content state :remote1 1)
          wrap (get-ice state :remote1 0)]
      (core/rez state :minion scg1)
      (core/rez state :minion cvs1)
      (is (= 15 (:credit (get-minion))))
      (is (= (:cid scg1) (-> (get-minion) :prompt first :card :cid)) "Surat City Grid triggered from upgrade in same remote")
      (prompt-choice :minion "Yes")
      (prompt-select :minion wrap)
      (is (get-in (refresh wrap) [:rezzed]) "Wraparound is rezzed")
      (is (= 15 (:credit (get-minion))) "Wraparound rezzed for free with 2c discount from SCG")
      (play-from-hand state :minion "Surat City Grid" "HQ")
      (play-from-hand state :minion "Enigma" "HQ")
      (play-from-hand state :minion "Cyberdex Virus Suite" "HQ")
      (let [scg2 (get-content state :hq 0)
            cvs2 (get-content state :hq 1)
            enig (get-ice state :hq 0)]
        (core/rez state :minion scg2)
        (core/rez state :minion cvs2)
        (is (empty? (:prompt (get-minion))) "SCG didn't trigger, upgrades in root of same central aren't considered in server")
        (core/derez state :minion (refresh wrap))
        (core/rez state :minion enig)
        (is (= (:cid scg2) (-> (get-minion) :prompt first :card :cid)) "SCG did trigger for ICE protecting HQ")))))

(deftest tori-hanzo
  ;; Tori Hanzō - Pay to do 1 brain damage instead of net damage
  (do-game
    (new-game (default-minion [(qty "Pup" 1) (qty "Tori Hanzō" 1)])
              (default-hero [(qty "Sure Gamble" 3) (qty "Net Shield" 1)]))
    (core/gain state :minion :credit 10)
    (play-from-hand state :minion "Pup" "HQ")
    (play-from-hand state :minion "Tori Hanzō" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Net Shield")
    (run-on state "HQ")
    (let [pup (get-ice state :hq 0)
          tori (get-content state :hq 0)
          nshld (get-in @state [:hero :rig :program 0])]
      (core/rez state :minion pup)
      (core/rez state :minion tori)
      (card-subroutine state :minion pup 0)
      (card-ability state :hero nshld 0)
      (prompt-choice :hero "Done")
      (is (empty? (:discard (get-hero))) "1 net damage prevented")
      (card-subroutine state :minion pup 0)
      (prompt-choice :hero "Done") ; decline to prevent
      (is (= 1 (count (:discard (get-hero)))) "1 net damage; previous prevention stopped Tori ability")
      (run-jack-out state)
      (run-on state "HQ")
      (card-subroutine state :minion pup 0)
      (prompt-choice :hero "Done")
      (prompt-choice :minion "Yes")
      (is (= 2 (count (:discard (get-hero)))) "1 brain damage suffered")
      (is (= 1 (:brain-damage (get-hero)))))))

(deftest tori-hanzo-hokusai
  ;; Tori Hanzō + Hokusai Grid: Issue #2702
  (do-game
    (new-game (default-minion [(qty "Tori Hanzō" 1) (qty "Hokusai Grid" 1)])
              (default-hero))
    (core/gain state :minion :credit 5)
    (play-from-hand state :minion "Hokusai Grid" "Archives")
    (play-from-hand state :minion "Tori Hanzō" "Archives")
    (take-credits state :minion)
    (run-on state "Archives")
    (let [hg (get-content state :archives 0)
          tori (get-content state :archives 1)]
      (core/rez state :minion hg)
      (core/rez state :minion tori)
      (run-successful state)
      (prompt-choice :minion "No") ; Tori prompt to pay 2c to replace 1 net with 1 brain
      (is (= 1 (count (:discard (get-hero)))) "1 net damage suffered")
      (prompt-choice :hero "Hokusai Grid")
      (prompt-choice :hero "No")
      (prompt-choice :hero "Tori Hanzō")
      (prompt-choice :hero "No")
      (is (and (empty (:prompt (get-hero))) (not (:run @state))) "No prompts, run ended")
      (run-empty-server state "Archives")
      (prompt-choice :minion "Yes") ; Tori prompt to pay 2c to replace 1 net with 1 brain
      (is (= 2 (count (:discard (get-hero)))))
      (is (= 1 (:brain-damage (get-hero))) "1 brain damage suffered")
      (prompt-choice :hero "Hokusai Grid")
      (prompt-choice :hero "No")
      (prompt-choice :hero "Tori Hanzō")
      (prompt-choice :hero "No")
      (is (and (empty (:prompt (get-hero))) (not (:run @state))) "No prompts, run ended"))))

(deftest underway-grid
  ;; Underway Grid - prevent expose of cards in server
  (do-game
    (new-game (default-minion [(qty "Eve Campaign" 1)
                             (qty "Underway Grid" 1)])
              (default-hero [(qty "Drive By" 1)]))
    (play-from-hand state :minion "Underway Grid" "New remote")
    (play-from-hand state :minion "Eve Campaign" "Server 1")
    (take-credits state :minion)
    (core/rez state :minion (get-content state :remote1 0))
    (let [eve1 (get-content state :remote1 1)]
      (play-from-hand state :hero "Drive By")
      (prompt-select :hero eve1)
      (is (empty? (:discard (get-minion))) "Expose and trash prevented"))))

(deftest valley-grid-trash
  ;; Valley Grid - Reduce Runner max hand size and restore it even if trashed
  (do-game
    (new-game (default-minion [(qty "Valley Grid" 3) (qty "Ice Wall" 3)])
              (default-hero))
    (play-from-hand state :minion "Valley Grid" "New remote")
    (take-credits state :minion 2)
    (run-on state "Server 1")
    (let [vg (get-content state :remote1 0)]
      (core/rez state :minion vg)
      (card-ability state :minion vg 0)
      (card-ability state :minion vg 0) ; only need the run to exist for test, just pretending the Runner has broken all subs on 2 ice
      (is (= 3 (core/hand-size state :hero)) "Runner max hand size reduced by 2")
      (is (= 2 (get-in (refresh vg) [:times-used])) "Saved number of times Valley Grid used")
      (run-successful state)
      (prompt-choice :hero "Yes") ; pay to trash
      (take-credits state :hero 3)
      (is (= 5 (core/hand-size state :hero)) "Runner max hand size increased by 2 at start of Corp turn"))))
