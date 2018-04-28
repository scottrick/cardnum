(ns test.cards.hardware
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest akamatsu-mem
  ;; Akamatsu Mem Chip - Gain 1 memory
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Akamatsu Mem Chip" 3)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Akamatsu Mem Chip")
    (is (= 5 (:memory (get-runner))) "Gain 1 memory")))

(deftest archives-interface
  ;; Archives Interface - Remove 1 card in Archives from the game instead of accessing it
  (do-game
    (new-game (default-corp [(qty "Shock!" 1) (qty "Launch Campaign" 1)])
              (default-runner [(qty "Archives Interface" 1) (qty "Imp" 1)]))
    (take-credits state :resPlayer)
    (core/move state :resPlayer (find-card "Shock!" (:hand (get-corp))) :discard)
    (core/move state :resPlayer (find-card "Launch Campaign" (:hand (get-corp))) :discard)
    (play-from-hand state :hazPlayer "Archives Interface")
    (run-empty-server state :archives)
    (prompt-choice :hazPlayer "Yes")
    (prompt-choice :hazPlayer (find-card "Shock!" (:discard (get-corp))))
    (is (= "Shock!" (:title (first (:rfg (get-corp))))) "Shock! removed from game")
    (is (empty? (:discard (get-runner))) "Didn't access Shock!, no net damage taken")))

(deftest astrolabe-memory
  ;; Astrolabe - Gain 1 memory
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Astrolabe" 3)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Astrolabe")
    (is (= 5 (:memory (get-runner))) "Gain 1 memory")))

(deftest astrolabe-draw
  ;; Astrolabe - Draw on new server install
  (do-game
    (new-game (default-corp [(qty "Snare!" 3)])
              (default-runner [(qty "Astrolabe" 3) (qty "Sure Gamble" 3) (qty "Cloak" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Astrolabe")
    (take-credits state :hazPlayer 3)
    ;; corp's turn. install something from HQ to trigger Astrolabe draw
    (play-from-hand state :resPlayer "Snare!" "New remote")
    (is (= 5 (count (:hand (get-runner)))) "Drew 1 card from server install")
    ;; install over the old server; make sure nothing is drawn
    (play-from-hand state :resPlayer "Snare!" "Server 0")
    (is (= 5 (count (:hand (get-runner)))) "Did not draw")
    (is (= 1 (count (:deck (get-runner)))) "1 card left in deck")))

(deftest box-e
  ;; Box-E - +2 MU, +2 max hand size
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Box-E" 1)]))
   (take-credits state :resPlayer)
   (play-from-hand state :hazPlayer "Box-E")
   (is (= 6 (:memory (get-runner))))
   (is (= 7 (core/hand-size state :hazPlayer)))))

(deftest blackguard
  ;; Blackguard - +2 MU, forced rez of exposed ice
  (do-game
   (new-game (default-corp [(qty "Ice Wall" 1)])
             (default-runner [(qty "Blackguard" 1)
                              (qty "Snitch" 1)]))
   (play-from-hand state :resPlayer "Ice Wall" "Archives")
   (take-credits state :resPlayer)
   (core/gain state :hazPlayer :credit 100)
   (play-from-hand state :hazPlayer "Blackguard")
   (is (= 6 (:memory (get-runner))) "Runner has 6 MU")
   (play-from-hand state :hazPlayer "Snitch")
   (let [snitch (get-in @state [:hazPlayer :rig :program 0])
         iwall (get-ice state :archives 0)]
     (run-on state :archives)
     (card-ability state :hazPlayer snitch 0)
     (is (:rezzed (refresh iwall)) "Ice Wall was rezzed"))))

(deftest brain-chip
  ;; Brain Chip handsize and memory limit
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Brain Chip" 1)]))
   (take-credits state :resPlayer)
   (play-from-hand state :hazPlayer "Brain Chip")
   (swap! state assoc-in [:hazPlayer :agenda-point] -2) ; hard set ap
   (is (= (core/hand-size state :hazPlayer) 5) "Hand size unaffected")
   (is (= (get-in @state [:hazPlayer :memory]) 4) "Memory limit unaffected")
   (swap! state assoc-in [:hazPlayer :agenda-point] 2)
   (is (= (core/hand-size state :hazPlayer) 7) "Hand size increased by 2")
   (is (= (get-in @state [:hazPlayer :memory]) 6) "Memory limit increased by 2")
   (core/move state :hazPlayer (get-in @state [:hazPlayer :rig :hardware 0]) :discard)
   (is (= (core/hand-size state :hazPlayer) 5) "Hand size reset")
   (is (= (get-in @state [:hazPlayer :memory]) 4) "Memory limit reset")))

(deftest clone-chip
  ;; Test clone chip usage- outside and during run
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Datasucker" 1) (qty "Clone Chip" 2)]))
    (take-credits state :resPlayer)
    (trash-from-hand state :hazPlayer "Datasucker")
    (play-from-hand state :hazPlayer "Clone Chip")
    (let [chip (get-in @state [:hazPlayer :rig :hardware 0])]
      (card-ability state :hazPlayer chip 0)
      (prompt-select :hazPlayer (find-card "Datasucker" (:discard (get-runner))))
      (let [ds (get-in @state [:hazPlayer :rig :program 0])]
        (is (not (nil? ds)))
        (is (= (:title ds) "Datasucker"))))))

(deftest clone-chip-dont-install-choices-runner-cant-afford
  ;; Test clone chip usage - dont show inavalid choices
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Inti" 1) (qty "Magnum Opus" 1) (qty "Clone Chip" 1)]))
    (take-credits state :resPlayer)
    (trash-from-hand state :hazPlayer "Inti")
    (trash-from-hand state :hazPlayer "Magnum Opus")
    (play-from-hand state :hazPlayer "Clone Chip")
    (is (= (get-in @state [:hazPlayer :click]) 3) "Runner has 3 clicks left")
    (let [chip (get-in @state [:hazPlayer :rig :hardware 0])]
      (card-ability state :hazPlayer chip 0)
      (prompt-select :hazPlayer (find-card "Magnum Opus" (:discard (get-runner))))
      (is (nil? (get-in @state [:hazPlayer :rig :program 0])) "No program was installed"))
    (let [chip (get-in @state [:hazPlayer :rig :hardware 0])]
      (is (not (nil? chip)) "Clone Chip is still installed")
      (is (= (get-in @state [:hazPlayer :click]) 3) "Runner has 3 clicks left")
      (card-ability state :hazPlayer chip 0)
      (prompt-select :hazPlayer (find-card "Inti" (:discard (get-runner))))
      (let [inti (get-in @state [:hazPlayer :rig :program 0])]
        (is (not (nil? inti)) "Program was installed")
        (is (= (:title inti) "Inti") "Program is Inti")
        (is (= (get-in @state [:hazPlayer :click]) 3) "Runner has 3 clicks left")))))

(deftest comet-event-play
  ;; Comet - Play event without spending a click after first event played
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Comet" 3) (qty "Easy Mark" 2)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Comet")
    (let [comet (get-in @state [:hazPlayer :rig :hardware 0])]
      (play-from-hand state :hazPlayer "Easy Mark")
      (is (= true (:comet-event (core/get-card state comet)))) ; Comet ability enabled
      (card-ability state :hazPlayer comet 0)
      (is (= (:cid comet) (-> @state :hazPlayer :prompt first :card :cid)))
      (prompt-select :hazPlayer (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 7 (:credit (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (nil? (:comet-event (core/get-card state comet))) "Comet ability disabled"))))

(deftest cortez-chip
  ;; Cortez Chip - Trash to add 2 credits to rez cost of an ICE until end of turn
  (do-game
    (new-game (default-corp [(qty "Quandary" 1)])
              (default-runner [(qty "Cortez Chip" 1)]))
    (play-from-hand state :resPlayer "Quandary" "R&D")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Cortez Chip")
    (let [quan (get-ice state :rd 0)
          cortez (get-hardware state 0)]
      (card-ability state :hazPlayer cortez 0)
      (prompt-select :hazPlayer quan)
      (is (= 1 (count (:discard (get-runner)))) "Cortez Chip trashed")
      (core/rez state :resPlayer quan)
      (is (= 4 (:credit (get-corp))) "Paid 3c instead of 1c to rez Quandary"))))

(deftest cybersolutions-mem-chip
  ;; CyberSolutions Mem Chip- Gain 2 memory
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "CyberSolutions Mem Chip" 3)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "CyberSolutions Mem Chip")
    (is (= 6 (:memory (get-runner))) "Gain 2 memory")))

(deftest desperado
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Desperado" 3)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Desperado")
    (run-empty-server state :archives)
    (is (= 5 (:memory (get-runner))) "Gain 1 memory")
    (is (= 3 (:credit (get-runner))) "Got 1c for successful run on Desperado")))

(deftest dinosaurus-strength-boost-mu-savings
  ;; Dinosaurus - Boost strength of hosted icebreaker; keep MU the same when hosting or trashing hosted breaker
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Dinosaurus" 1) (qty "Battering Ram" 1)]))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 5)
    (play-from-hand state :hazPlayer "Dinosaurus")
    (let [dino (get-in @state [:hazPlayer :rig :hardware 0])]
      (card-ability state :hazPlayer dino 0)
      (prompt-select :hazPlayer (find-card "Battering Ram" (:hand (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (= 0 (:credit (get-runner))))
      (is (= 4 (:memory (get-runner))) "Battering Ram 2 MU not deducted from available MU")
      (let [ram (first (:hosted (refresh dino)))]
        (is (= 5 (:current-strength (refresh ram)))
            "Dinosaurus giving +2 strength to Battering Ram")
        ;; Trash Battering Ram
        (core/move state :hazPlayer (find-card "Battering Ram" (:hosted (refresh dino))) :discard)
        (is (= 4 (:memory (get-runner))) "Battering Ram 2 MU not added to available MU")))))

(deftest doppelganger
  ;; Doppelgänger - run again when successful
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Doppelgänger" 1)]))
    (core/gain state :resPlayer :bad-publicity 1)
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Doppelgänger")
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "OK")
    (is (= 0 (:run-credit (get-runner))) "Runner lost BP credits")
    (prompt-choice :hazPlayer "Yes")
    (prompt-choice :hazPlayer "R&D")
    (is (:run @state) "New run started")
    (is (= [:rd] (:server (:run @state))) "Running on R&D")
    (is (= 1 (:run-credit (get-runner))) "Runner has 1 BP credit")))

(deftest dorm-computer
  ;; make a run and avoid all tags for the remainder of the run
  (do-game
    (new-game (default-corp [(qty "Snare!" 1)])
              (default-runner [(qty "Dorm Computer" 1)]))
    (play-from-hand state :resPlayer "Snare!" "New remote")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Dorm Computer")
    (let [dorm (get-in @state [:hazPlayer :rig :hardware 0])]
      (card-ability state :hazPlayer dorm 0)
      (prompt-choice :hazPlayer "Server 1")
      (run-empty-server state "Server 1")
      (is (:run @state) "New run started")
      (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :resPlayer "Yes")
      (is (= 0 (:tag (get-runner))) "Runner has 0 tags")
      (is (= 3 (get-counters (refresh dorm) :power))))
      ))

(deftest feedback-filter
  ;; Feedback Filter - Prevent net and brain damage
  (do-game
    (new-game (default-corp [(qty "Data Mine" 1)
                             (qty "Cerebral Overwriter" 1)
                             (qty "Mushin No Shin" 1)])
              (default-runner [(qty "Feedback Filter" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :resPlayer "Mushin No Shin")
    (prompt-select :resPlayer (find-card "Cerebral Overwriter" (:hand (get-corp))))
    (play-from-hand state :resPlayer "Data Mine" "Server 1")
    (let [co (get-content state :remote1 0)
          dm (get-ice state :remote1 0)]
      (is (= 3 (:advance-counter (refresh co))) "3 advancements on Overwriter")
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Sure Gamble")
      (play-from-hand state :hazPlayer "Feedback Filter")
      (is (= 7 (:credit (get-runner))))
      (let [ff (get-in @state [:hazPlayer :rig :hardware 0])]
        (run-on state "Server 1")
        (core/rez state :resPlayer dm)
        (card-subroutine state :resPlayer dm 0)
        (card-ability state :hazPlayer ff 0)
        (prompt-choice :hazPlayer "Done")
        (is (= 3 (count (:hand (get-runner)))) "1 net damage prevented")
        (is (= 4 (:credit (get-runner))))
        (run-successful state)
        (prompt-choice :resPlayer "Yes") ; pay 3 to fire Overwriter
        (card-ability state :hazPlayer ff 1)
        (prompt-choice :hazPlayer "Done")
        (prompt-choice :hazPlayer "Yes") ; trash Overwriter for 0
        (is (= 1 (:brain-damage (get-runner))) "2 of the 3 brain damage prevented")
        (is (= 2 (count (:hand (get-runner)))))
        (is (empty? (get-in @state [:hazPlayer :rig :hardware])) "Feedback Filter trashed")))))

(deftest grimoire
  ;; Grimoire - Gain 2 MU, add a free virus counter to installed virus programs
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Grimoire" 1) (qty "Imp" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Grimoire")
    (is (= 6 (:memory (get-runner))) "Gained 2 MU")
    (play-from-hand state :hazPlayer "Imp")
    (let [imp (get-in @state [:hazPlayer :rig :program 0])]
      (is (= 3 (get-counters (refresh imp) :virus)) "Imp received an extra virus counter on install"))))

(deftest llds-processor
  ;; LLDS Processor - Add 1 strength until end of turn to an icebreaker upon install
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "LLDS Processor" 2) (qty "Inti" 1) (qty "Passport" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "LLDS Processor")
    (play-from-hand state :hazPlayer "Inti")
    (play-from-hand state :hazPlayer "LLDS Processor")
    (play-from-hand state :hazPlayer "Passport")
    (let [inti (get-in @state [:hazPlayer :rig :program 0])
          pass (get-in @state [:hazPlayer :rig :program 1])]
      (is (= 2 (:current-strength (refresh inti))) "Strength boosted by 1; 1 copy of LLDS when installed")
      (is (= 4 (:current-strength (refresh pass))) "Strength boosted by 2; 2 copies of LLDS when installed")
      (take-credits state :hazPlayer)
      (is (= 1 (:current-strength (refresh inti))) "Strength reduced to default")
      (is (= 2 (:current-strength (refresh pass))) "Strength reduced to default"))))

(deftest maw
  ;; Once per turn, first time runner declines to steal or trash, trash a HQ card at random
  (do-game
    (new-game (default-corp [(qty "BOOM!" 5)])
              (default-runner [(qty "Maw" 1)]))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 20)
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "No")
    (is (= 0 (count (:discard (get-corp)))) "No HQ card in discard before Maw installed")
    (play-from-hand state :hazPlayer "Maw")
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "No")
    (is (= 0 (count (:discard (get-corp)))) "HQ card not trashed by Maw as first decline already happened")
    (take-credits state :hazPlayer)
    (take-credits state :resPlayer)
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "No")
    (is (= 1 (count (:discard (get-corp)))) "HQ card trashed by Maw")
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "No")
    (is (= 1 (count (:discard (get-corp)))) "2nd HQ card on same turn not trashed by Maw")))

(deftest maw-card-seen
  ;; Check trashed card is trashed face-up if it's the card that is accessed, issue #2695
  ;; Also checks Maw auto-trashes on Operation with no trash cost
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 1)])
              (default-runner [(qty "Maw" 1)]))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 20)
    (play-from-hand state :hazPlayer "Maw")
    (run-empty-server state :hq)
    ;; (is (= 0 (count (:discard (get-corp)))) "HQ card not trashed by Maw yet")
    (prompt-choice :hazPlayer "OK")
    (is (= 1 (count (:discard (get-corp)))) "HQ card trashed by Maw now")
    (is (:seen (first (:discard (get-corp)))) "Trashed card is registered as seen since it was accessed")))

(deftest maw-hiro
  ;; Maw with Hiro in hand - Hiro not moved to runner scored area on trash decline #2638
  (do-game
    (new-game (default-corp [(qty "Chairman Hiro" 1)])
              (default-runner [(qty "Maw" 1)]))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 20)
    (play-from-hand state :hazPlayer "Maw")
    (run-empty-server state :hq)
    (prompt-choice :hazPlayer "No")
    (is (= 0 (count (:scored (get-corp)))) "Hiro not scored")
    (is (= 1 (count (:discard (get-corp)))) "Hiro trashed by Maw")))

(deftest maya
  ;; Maya - Move accessed card to bottom of R&D
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
              (default-runner [(qty "Maya" 1) (qty "Sure Gamble" 3)]))
    (core/move state :resPlayer (find-card "Scorched Earth" (:hand (get-corp))) :deck)
    (core/move state :resPlayer (find-card "Snare!" (:hand (get-corp))) :deck)
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Maya")
    (let [maya (get-in @state [:hazPlayer :rig :hardware 0])
          accessed (first (:deck (get-corp)))]
      (run-empty-server state :rd)
      (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
      (card-ability state :hazPlayer maya 0)
      (is (empty? (:prompt (get-runner))) "No more prompts for runner")
      (is (not (:run @state)) "Run is ended")
      (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
      (take-credits state :hazPlayer)
      (core/draw state :resPlayer)
      (take-credits state :resPlayer)
      (core/move state :resPlayer (find-card "Snare!" (:hand (get-corp))) :deck)
      (core/move state :resPlayer (find-card "Scorched Earth" (:hand (get-corp))) :deck)
      (let [accessed (first (:deck (get-corp)))]
        (run-empty-server state :rd)
        (prompt-choice :resPlayer "Yes")
        (is (= 0 (count (:hand (get-runner)))) "Runner took Snare! net damage")
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
        (card-ability state :hazPlayer maya 0)
        (is (empty? (:prompt (get-runner))) "No more prompts for runner")
        (is (not (:run @state)) "Run is ended")
        (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")))))

(deftest maya-multi-access
  ;; Maya - Does not interrupt multi-access.
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
              (default-runner [(qty "Maya" 1) (qty "Sure Gamble" 3) (qty "R&D Interface" 1)]))
    (core/move state :resPlayer (find-card "Scorched Earth" (:hand (get-corp))) :deck)
    (core/move state :resPlayer (find-card "Snare!" (:hand (get-corp))) :deck)
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 10)
    (play-from-hand state :hazPlayer "Maya")
    (play-from-hand state :hazPlayer "R&D Interface")
    (let [maya (get-in @state [:hazPlayer :rig :hardware 0])
          accessed (first (:deck (get-corp)))]
      (run-empty-server state :rd)
      (prompt-choice :hazPlayer "Card from deck")
      (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
      (card-ability state :hazPlayer maya 0)
      (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
      (is (:prompt (get-runner)) "Runner has next access prompt"))))

(deftest obelus
  ;; Obelus - Increase max hand size with tags, draw cards on first successful HQ/R&D run
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Obelus" 1) (qty "Nerve Agent" 1)
                               (qty "Sure Gamble" 3) (qty "Cache" 3)]))
    (take-credits state :resPlayer)
    (starting-hand state :hazPlayer ["Obelus" "Nerve Agent"])
    (core/gain state :hazPlayer :credit 10 :click 3)
    (play-from-hand state :hazPlayer "Nerve Agent")
    (let [nerve (get-in @state [:hazPlayer :rig :program 0])]
      (run-empty-server state :hq)
      (is (= 1 (get-counters (refresh nerve) :virus)) "1 virus counter on Nerve Agent")
      (prompt-choice :hazPlayer "OK")
      (play-from-hand state :hazPlayer "Obelus")
      (core/gain state :hazPlayer :tag 1)
      (is (= 6 (core/hand-size state :hazPlayer)) "Max hand size is 6")
      (core/lose state :hazPlayer :tag 1)
      (is (= 5 (core/hand-size state :hazPlayer)) "Max hand size is 5")
      (run-empty-server state :hq)
      (is (= 2 (get-counters (refresh nerve) :virus)) "2 virus counters on Nerve Agent")
      (prompt-choice :hazPlayer 1)
      (prompt-choice :hazPlayer "Card from hand")
      (prompt-choice :hazPlayer "OK")
      (prompt-choice :hazPlayer "Card from hand")
      (prompt-choice :hazPlayer "OK")
      (is (empty? (:hand (get-runner))) "No cards drawn by Obelus, already had successful HQ run")
      (take-credits state :hazPlayer)
      (take-credits state :resPlayer)
      (run-empty-server state :hq)
      (is (= 3 (get-counters (refresh nerve) :virus)) "3 virus counters on Nerve Agent")
      (prompt-choice :hazPlayer 2)
      (prompt-choice :hazPlayer "Card from hand")
      (prompt-choice :hazPlayer "OK")
      (prompt-choice :hazPlayer "Card from hand")
      (prompt-choice :hazPlayer "OK")
      (prompt-choice :hazPlayer "Card from hand")
      (prompt-choice :hazPlayer "OK")
      (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards"))))

(deftest obelus-hades-shard
  ;; Obelus - using Hades Shard during run to increase draw
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 3)])
              (default-runner [(qty "Obelus" 1) (qty "Hades Shard" 1)
                               (qty "Sure Gamble" 3) (qty "Cache" 3)]))
    (starting-hand state :resPlayer ["Hedge Fund" "Hedge Fund"])
    (trash-from-hand state :resPlayer "Hedge Fund")
    (trash-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :resPlayer)
    (starting-hand state :hazPlayer ["Obelus" "Hades Shard"])
    (core/gain state :hazPlayer :credit 10)
    (play-from-hand state :hazPlayer "Obelus")
    (play-from-hand state :hazPlayer "Hades Shard")
    (run-empty-server state "R&D")
    (card-ability state :hazPlayer (get-resource state 0) 0)
    (prompt-choice :hazPlayer "OK")
    (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards")))

(deftest plascrete
  ;; Plascrete Carapace - Prevent meat damage
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 1)])
              (default-runner [(qty "Plascrete Carapace" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Plascrete Carapace")
    (let [plas (get-in @state [:hazPlayer :rig :hardware 0])]
      (is (= 4 (get-counters (refresh plas) :power)) "4 counters on install")
      (take-credits state :hazPlayer)
      (core/gain state :hazPlayer :tag 1)
      (play-from-hand state :resPlayer "Scorched Earth")
      (card-ability state :hazPlayer plas 0)
      (card-ability state :hazPlayer plas 0)
      (card-ability state :hazPlayer plas 0)
      (card-ability state :hazPlayer plas 0)
      (prompt-choice :hazPlayer "Done")
      (is (= 1 (count (:hand (get-runner)))) "All meat damage prevented")
      (is (empty? (get-in @state [:hazPlayer :rig :hardware])) "Plascrete depleted and trashed"))))

(deftest rabbit-hole
  ;; Rabbit Hole - +1 link, optionally search Stack to install more copies
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Sure Gamble" 1) (qty "Rabbit Hole" 3)]))
    (take-credits state :resPlayer)
    (core/move state :hazPlayer (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (core/move state :hazPlayer (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (play-from-hand state :hazPlayer "Sure Gamble")
    (play-from-hand state :hazPlayer "Rabbit Hole")
    (is (= 1 (:link (get-runner))))
    (prompt-choice :hazPlayer "Yes")
    (prompt-choice :hazPlayer "Yes")
    (is (= 3 (:link (get-runner))))
    (is (= 3 (count (get-in @state [:hazPlayer :rig :hardware]))))
    (is (= 2 (:click (get-runner))) "Clickless installs of extra 2 copies")
    (is (= 3 (:credit (get-runner))) "Paid 2c for each of 3 copies")))

(deftest recon-drone
  ;; trash and pay X to prevent that much damage from a card you are accessing
  (do-game
    (new-game (default-corp [(qty "Snare!" 1) (qty "House of Knives" 1)
                             (qty "Prisec" 1) (qty "Cerebral Overwriter" 1)])
              (default-runner [(qty "Recon Drone" 10)]))
    (core/gain state :resPlayer :click 10)
    (core/gain state :resPlayer :credit 100)
    (play-from-hand state :resPlayer "House of Knives" "New remote")
    (play-from-hand state :resPlayer "Snare!" "New remote")
    (play-from-hand state :resPlayer "Prisec" "New remote")
    (play-from-hand state :resPlayer "Cerebral Overwriter" "New remote")
    (score-agenda state :resPlayer (get-content state :remote1 0))
    (core/advance state :resPlayer (get-content state :remote4 0))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :click 100)
    (core/gain state :hazPlayer :credit 100)
    (core/draw state :hazPlayer)
    (core/draw state :hazPlayer)
    (core/draw state :hazPlayer)
    (core/draw state :hazPlayer)
    (play-from-hand state :hazPlayer "Recon Drone")
    (play-from-hand state :hazPlayer "Recon Drone")
    (play-from-hand state :hazPlayer "Recon Drone")
    (play-from-hand state :hazPlayer "Recon Drone")
    (let [rd1 (get-in @state [:hazPlayer :rig :hardware 0])
          rd2 (get-in @state [:hazPlayer :rig :hardware 1])
          rd3 (get-in @state [:hazPlayer :rig :hardware 2])
          rd4 (get-in @state [:hazPlayer :rig :hardware 3])
          hok (get-in @state [:resPlayer :scored 0])]
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
        "Runner has prompt to wait for Snare!")
      (prompt-choice :resPlayer "Yes")
      (card-ability state :hazPlayer rd1 0)
      (prompt-choice :hazPlayer 3)
      (prompt-choice :hazPlayer "Done")
      (is (= 5 (count (:hand (get-runner)))) "Runner took no net damage")
      ; fire HOK while accessing Snare!
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (card-ability state :resPlayer hok 0)
      ; Recon Drone ability won't fire as we are not accessing HOK
      (card-ability state :hazPlayer rd2 0)
      (is (nil? (:number (:choices (first (:prompt (get-runner)))))) "No choice to prevent damage from HOK")
      (prompt-choice :hazPlayer "Done")
      (is (= 4 (count (:hand (get-runner)))) "Runner took 1 net damage from HOK")
      (prompt-choice :resPlayer "No")
      (core/lose state :hazPlayer :credit 100)
      ; can only stop 1 damage due to credits
      (core/gain state :hazPlayer :credit 1)
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :resPlayer "Yes")
      (card-ability state :hazPlayer rd2 0)
      (is (= 1 (:number (:choices (first (:prompt (get-runner)))))) "Recon Drone choice limited to runner credits")
      (prompt-choice :hazPlayer 1)
      (prompt-choice :hazPlayer "Done")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 2 net damage from Snare!")
      (core/gain state :hazPlayer :credit 100)
      (run-empty-server state "Server 3")
      (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
          "Runner has prompt to wait for Prisec")
      (prompt-choice :resPlayer "Yes")
      (card-ability state :hazPlayer rd3 0)
      (is (= 1 (:number (:choices (first (:prompt (get-runner)))))) "Recon Drone choice limited to 1 meat")
      (prompt-choice :hazPlayer 1)
      (prompt-choice :hazPlayer "Done")
      (is (= 2 (count (:hand (get-runner)))) "Runner took no meat damage")
      (run-empty-server state "Server 4")
      (is (= :waiting (-> @state :hazPlayer :prompt first :prompt-type))
          "Runner has prompt to wait for Cerebral Overwriter")
      (prompt-choice :resPlayer "Yes")
      (card-ability state :hazPlayer rd4 0)
      (prompt-choice :hazPlayer 1)
      (prompt-choice :hazPlayer "Done")
      (is (= 2 (count (:hand (get-runner)))) "Runner took no brain damage"))))

(deftest ramujan-reliant
  ;; Prevent up to X net or brain damage.
  (do-game
    (new-game (default-corp [(qty "Data Mine" 1)
                             (qty "Snare!" 1)])
              (default-runner [(qty "Ramujan-reliant 550 BMI" 4) (qty "Sure Gamble" 6)]))
    (starting-hand state :hazPlayer
                   ["Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Sure Gamble"])
    (play-from-hand state :resPlayer "Data Mine" "Server 1")
    (play-from-hand state :resPlayer "Snare!" "Server 1")
    (let [sn (get-content state :remote1 0)
          dm (get-ice state :remote1 0)]
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Ramujan-reliant 550 BMI")
      (play-from-hand state :hazPlayer "Ramujan-reliant 550 BMI")
      (play-from-hand state :hazPlayer "Ramujan-reliant 550 BMI")
      (let [rr1 (get-in @state [:hazPlayer :rig :hardware 0])
            rr2 (get-in @state [:hazPlayer :rig :hardware 1])
            rr3 (get-in @state [:hazPlayer :rig :hardware 2])]
        (run-on state "Server 1")
        (core/rez state :resPlayer dm)
        (card-subroutine state :resPlayer dm 0)
        (card-ability state :hazPlayer rr1 0)
        (prompt-choice :hazPlayer 1)
        (is (last-log-contains? state "Sure Gamble")
            "Ramujan did log trashed card names")
        (is (= 2 (count (:hand (get-runner)))) "1 net damage prevented")
        (run-successful state)
        (take-credits state :hazPlayer)
        (take-credits state :resPlayer)
        (play-from-hand state :hazPlayer "Ramujan-reliant 550 BMI")
        (run-empty-server state "Server 1")
        (prompt-choice :resPlayer "Yes")
        (card-ability state :hazPlayer rr2 0)
        (prompt-choice :hazPlayer 3)
        (is (last-log-contains? state "Sure Gamble, Sure Gamble, Sure Gamble")
            "Ramujan did log trashed card names")
        (is (= 1 (count (:hand (get-runner)))) "3 net damage prevented")))))

(deftest ramujan-reliant-empty
  ;; Prevent up to X net or brain damage. Empty stack
  (do-game
    (new-game (default-corp [(qty "Data Mine" 1)])
              (default-runner [(qty "Ramujan-reliant 550 BMI" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :resPlayer "Data Mine" "Server 1")
    (let [dm (get-ice state :remote1 0)]
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Ramujan-reliant 550 BMI")
      (let [rr1 (get-in @state [:hazPlayer :rig :hardware 0])]
        (run-on state "Server 1")
        (core/rez state :resPlayer dm)
        (card-subroutine state :resPlayer dm 0)
        (card-ability state :hazPlayer rr1 0)
        (prompt-choice :hazPlayer 1)
        (is (= 0 (count (:hand (get-runner)))) "Not enough cards in Stack for Ramujan to work")))))

(deftest replicator-bazaar
  ;; Replicator - interaction with Bazaar. Issue #1511.
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Replicator" 1) (qty "Bazaar" 1) (qty "Spy Camera" 6)]))
    (letfn [(count-spy [n] (= n (count (filter #(= "Spy Camera" (:title %)) (-> (get-runner) :rig :hardware)))))]
      (take-credits state :resPlayer)
      (starting-hand state :hazPlayer ["Replicator" "Bazaar" "Spy Camera"])
      (play-from-hand state :hazPlayer "Replicator")
      (play-from-hand state :hazPlayer "Bazaar")
      (play-from-hand state :hazPlayer "Spy Camera") ; 1 installed
      (is (count-spy 1) "1 Spy Cameras installed")
      (prompt-choice :hazPlayer "Yes") ; for now, choosing Replicator then shows its optional Yes/No
      (prompt-choice :hazPlayer "Yes") ; Bazaar triggers, 2 installed
      (is (count-spy 2) "2 Spy Cameras installed")
      (prompt-choice :hazPlayer "Yes")
      (prompt-choice :hazPlayer "Yes")  ; 3 installed
      (is (count-spy 3) "3 Spy Cameras installed")

      (prompt-choice :hazPlayer "Yes")
      (prompt-choice :hazPlayer "Yes")  ; 4 installed
      (is (count-spy 4) "4 Spy Cameras installed")

      (prompt-choice :hazPlayer "Yes")
      (prompt-choice :hazPlayer "Yes")  ; 5 installed
      (is (count-spy 5) "5 Spy Cameras installed")

      (prompt-choice :hazPlayer "Yes")
      (prompt-choice :hazPlayer "Yes")  ; 6 installed
      (is (count-spy 6) "6 Spy Cameras installed"))))

(deftest respirocytes-multiple
  ;; Should draw multiple cards when multiple respirocytes are in play
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Respirocytes" 3) (qty "Sure Gamble" 3)]))
   (take-credits state :resPlayer)
   (starting-hand state :hazPlayer ["Respirocytes" "Respirocytes" "Respirocytes" "Sure Gamble"])
   (dotimes [_ 2]
     (play-from-hand state :hazPlayer "Respirocytes"))
   (is (= 2 (count (:discard (get-runner)))) "2 damage done")
   (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards")))

(deftest sifr
  ;; Once per turn drop encountered ICE to zero strenght
  ;; Also handle archangel then re-install sifr should not break the game #2576
  (do-game
    (new-game (default-corp [(qty "Archangel" 1) (qty "IP Block" 1) (qty "Hedge Fund" 1)])
              (default-runner [(qty "Modded" 1) (qty "Clone Chip" 1) (qty "Şifr" 1) (qty "Parasite" 1)]))
    (core/gain state :resPlayer :credit 100)
    (core/gain state :hazPlayer :credit 100)
    (play-from-hand state :resPlayer "Archangel" "HQ")
    (play-from-hand state :resPlayer "IP Block" "HQ")
    (take-credits state :resPlayer)
    (trash-from-hand state :hazPlayer "Parasite")
    (play-from-hand state :hazPlayer "Şifr")
    (is (= 2 (count (:hand (get-runner)))) "Modded and Clone Chip in hand")
    (let [arch (get-ice state :hq 0)
          ip (get-ice state :hq 1)
          sifr (get-hardware state 0)]
      (core/rez state :resPlayer arch)
      (core/rez state :resPlayer ip)
      (is (= 4 (:current-strength (refresh ip))))
      (run-on state :hq)
      (is (= 2 (:position (:run @state))))
      (card-ability state :hazPlayer sifr 0)
      (is (= 0 (:current-strength (refresh ip))))
      (run-continue state)
      (is (= 1 (:position (:run @state))))
      (is (= 2 (count (:hand (get-runner))))) ; pre archangel
      (card-subroutine state :resPlayer arch 0) ; fire archangel
      (is (not (empty? (:prompt (get-corp)))) "Archangel trace prompt - corp")
      (prompt-choice :resPlayer 0)
      (is (not (empty? (:prompt (get-runner)))) "Archangel trace prompt - runner")
      (prompt-choice :hazPlayer 0)
      (prompt-select :resPlayer sifr)
      (is (= 3 (count (:hand (get-runner))))) ; sifr got lifted to hand
      (run-jack-out state)
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :hazPlayer "Modded")
      (is (not (empty? (:prompt (get-runner)))) "Modded choice prompt exists")
      (prompt-select :hazPlayer (find-card "Şifr" (:hand (get-runner))))
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :hazPlayer "Clone Chip")
      (take-credits state :hazPlayer)
      (take-credits state :resPlayer 4)
      (let [chip (get-hardware state 1)]
        (is (nil? (:sifr-target (refresh sifr))) "Sifr cleaned up on leave play")
        (is (= 0 (count (:discard (get-corp)))) "No Corp cards trashed")
        (card-ability state :hazPlayer chip 0)
        (prompt-select :hazPlayer (find-card "Parasite" (:discard (get-runner))))
        (let [para (get-program state 0)]
          (prompt-select :hazPlayer ip)
          (is (= 0 (count (:discard (get-corp)))) "IP Block Not Trashed")
          (is (= 1 (count (:hosted (refresh ip)))) "Parasite is hosted"))))))

(deftest spinal-modem
  ;; Spinal Modem - +1 MU, 2 recurring credits, take 1 brain damage on successful trace during run
  (do-game
    (new-game (default-corp [(qty "Caduceus" 1)])
              (default-runner [(qty "Spinal Modem" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :resPlayer "Caduceus" "HQ")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Spinal Modem")
    (let [cad (get-ice state :hq 0)
          sm (get-hardware state 0)]
      (is (= 5 (:memory (get-runner))))
      (is (= 2 (:rec-counter (refresh sm))))
      (run-on state :hq)
      (core/rez state :resPlayer cad)
      (card-subroutine state :resPlayer cad 0)
      (prompt-choice :resPlayer 0)
      (prompt-choice :hazPlayer 0)
      (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 4 (core/hand-size state :hazPlayer)) "Reduced hand size"))))

(deftest spy-camera
  ;; Spy Camera - Full test
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Spy Camera" 6) (qty "Sure Gamble" 1) (qty "Desperado" 1)
                               (qty "Diesel" 1) (qty "Corroder" 1) (qty "Patron" 1) (qty "Kati Jones" 1)]))
    (starting-hand state :hazPlayer ["Spy Camera" "Spy Camera" "Spy Camera"
                                  "Spy Camera" "Spy Camera" "Spy Camera"])
    (is (= 6 (count (:hand (get-runner)))))
    (core/move state :resPlayer (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :click 3)
    (dotimes [_ 6] (play-from-hand state :hazPlayer "Spy Camera"))
    (let [spy (get-hardware state 5)]
      ;; look at top 6 cards
      (card-ability state :hazPlayer spy 0)
      (prompt-choice :hazPlayer (find-card "Sure Gamble" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Desperado" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Diesel" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Corroder" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Patron" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Kati Jones" (:deck (get-runner))))
      ;; try starting over
      (prompt-choice :hazPlayer "Start over")
      (prompt-choice :hazPlayer (find-card "Kati Jones" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Patron" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Corroder" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Diesel" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Desperado" (:deck (get-runner))))
      (prompt-choice :hazPlayer (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
      (prompt-choice :hazPlayer "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
      (is (= "Desperado" (:title (second (:deck (get-runner))))))
      (is (= "Diesel" (:title (second (rest (:deck (get-runner)))))))
      (is (= "Corroder" (:title (second (rest (rest (:deck (get-runner))))))))
      (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-runner)))))))))
      (is (= "Kati Jones" (:title (second (rest (rest (rest (rest (:deck (get-runner))))))))))
      ;; look at top card of R&D
      (card-ability state :hazPlayer spy 1)
      (let [topcard (get-in (first (get-in @state [:hazPlayer :prompt])) [:msg])]
        (is (= "The top card of R&D is Hedge Fund" topcard)))
      (is (= 1 (count (:discard (get-runner))))))))

(deftest the-gauntlet-not-with-gang-sign
  ;; Access additional cards on run on HQ, not with Gang Sign
  ;; Issue #2749
  (do-game
    (new-game (default-corp [(qty "Hostile Takeover" 1)
                             (qty "Hedge Fund" 3)])
              (default-runner [(qty "The Gauntlet" 1)
                               (qty "Gang Sign" 1)]))
    (take-credits state :resPlayer)
    (core/gain state :hazPlayer :credit 5)
    (play-from-hand state :hazPlayer "Gang Sign")
    (play-from-hand state :hazPlayer "The Gauntlet")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Hostile Takeover" "New remote")
    (score-agenda state :resPlayer (get-content state :remote1 0))
    ;; Gang Sign should trigger, without The Gauntlet pop-up
    (let [gs (get-resource state 0)]
      (prompt-is-card? :hazPlayer gs))
    ;; This will throw error if The Gauntlet triggers.
    (prompt-choice :hazPlayer "Card from hand")))

(deftest the-personal-touch
  ;; The Personal Touch - Give +1 strength to an icebreaker
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "The Personal Touch" 1)
                               (qty "Paricia" 1)
                               (qty "Faerie" 1)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Paricia")
    (play-from-hand state :hazPlayer "Faerie")
    (let [par (get-in @state [:hazPlayer :rig :program 0])
          fae (get-in @state [:hazPlayer :rig :program 1])]
      (is (= 2 (:current-strength (refresh fae))))
      (play-from-hand state :hazPlayer "The Personal Touch")
      (prompt-select :hazPlayer par)
      (is (nil? (:hosted (refresh par))) "TPT can't be hosted on a non-icebreaker")
      (prompt-select :hazPlayer fae)
      (is (= 1 (count (:hosted (refresh fae)))) "TPT hosted on Faerie")
      (is (= 3 (:current-strength (refresh fae))) "Faerie receiving +1 strength from TPT"))))

(deftest titanium-ribs
  ;; Titanium Ribs - Choose cards lost to damage, but not on Corp turn against Chronos Protocol
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1) (qty "Viktor 1.0" 1)
                                                                     (qty "Neural EMP" 1)])
              (default-runner [(qty "Titanium Ribs" 2) (qty "Sure Gamble" 1)
                               (qty "Fall Guy" 1) (qty "Kati Jones" 1)]))
    (play-from-hand state :resPlayer "Pup" "HQ")
    (play-from-hand state :resPlayer "Viktor 1.0" "R&D")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Fall Guy")
    (play-from-hand state :hazPlayer "Titanium Ribs")
    (prompt-select :hazPlayer (find-card "Titanium Ribs" (:hand (get-runner))))
    (prompt-select :hazPlayer (find-card "Kati Jones" (:hand (get-runner))))
    (is (empty? (:prompt (get-runner))) "Fall Guy didn't try to prevent trashing of Kati")
    (is (= 2 (count (:discard (get-runner)))) "2 cards trashed for Ribs installation meat damage")
    (run-on state "HQ")
    (let [pup (get-ice state :hq 0)]
      (core/rez state :resPlayer pup)
      (card-subroutine state :resPlayer pup 0)
      (prompt-select :hazPlayer (find-card "Sure Gamble" (:hand (get-runner)))) ; Ribs takes precedence over CP on Runner turn
      (is (= 3 (count (:discard (get-runner)))) "Chose card lost from 1 net damage")
      (run-jack-out state)
      (take-credits state :hazPlayer)
      (core/move state :hazPlayer (find-card "Sure Gamble" (:discard (get-runner))) :hand)
      (core/move state :hazPlayer (find-card "Kati Jones" (:discard (get-runner))) :hand)
      (play-from-hand state :resPlayer "Neural EMP")
      (prompt-choice :resPlayer "Yes")
      (let [kati (find-card "Kati Jones" (:hand (get-runner)))]
        (prompt-choice :resPlayer kati) ; Chronos Protocol takes precedence over Ribs on Corp turn
        (is (= 2 (count (:discard (get-runner)))) "Card chosen by Corp for first net damage")))))

(deftest turntable-swap
  ;; Turntable - Swap a stolen agenda for a scored agenda
  (do-game
    (new-game (default-corp [(qty "Domestic Sleepers" 1) (qty "Project Vitruvius" 1)])
              (default-runner [(qty "Turntable" 1)]))
    (play-from-hand state :resPlayer "Project Vitruvius" "New remote")
    (let [ag1 (get-content state :remote1 0)]
      (score-agenda state :resPlayer ag1)
      (take-credits state :resPlayer)
      (play-from-hand state :hazPlayer "Turntable")
      (is (= 3 (:credit (get-runner))))
      (let [tt (get-in @state [:hazPlayer :rig :hardware 0])]
        (run-empty-server state "HQ")
        (prompt-choice :hazPlayer "Steal")
        (is (= 0 (:agenda-point (get-runner))) "Stole Domestic Sleepers")
        (is (prompt-is-card? :hazPlayer tt))
        (prompt-choice :hazPlayer "Yes")
        (prompt-select :hazPlayer (find-card "Project Vitruvius" (:scored (get-corp))))
        (is (= 2 (:agenda-point (get-runner))) "Took Project Vitruvius from Corp")
        (is (= 0 (:agenda-point (get-corp))) "Swapped Domestic Sleepers to Corp")))))

(deftest turntable-mandatory-upgrades
  ;; Turntable - Swap a Mandatory Upgrades away from the Corp reduces Corp clicks per turn
  ;;           - Corp doesn't gain a click on the Runner's turn when it receives a Mandatory Upgrades
  (do-game
    (new-game (default-corp [(qty "Mandatory Upgrades" 2) (qty "Project Vitruvius" 1)])
              (default-runner [(qty "Turntable" 1)]))
    (score-agenda state :resPlayer (find-card "Mandatory Upgrades" (:hand (get-corp))))
    (is (= 4 (:click-per-turn (get-corp))) "Up to 4 clicks per turn")
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Turntable")
    (let [tt (get-in @state [:hazPlayer :rig :hardware 0])]
      ;; steal Project Vitruvius and swap for Mandatory Upgrades
      (core/steal state :hazPlayer (find-card "Project Vitruvius" (:hand (get-corp))))
      (is (prompt-is-card? :hazPlayer tt))
      (prompt-choice :hazPlayer "Yes")
      (prompt-select :hazPlayer (find-card "Mandatory Upgrades" (:scored (get-corp))))
      (is (= 3 (:click-per-turn (get-corp))) "Back down to 3 clicks per turn")
      ;; steal second Mandatory Upgrades and swap for Project Vitruvius
      (core/steal state :hazPlayer (find-card "Mandatory Upgrades" (:hand (get-corp))))
      (is (prompt-is-card? :hazPlayer tt))
      (prompt-choice :hazPlayer "Yes")
      (prompt-select :hazPlayer (find-card "Project Vitruvius" (:scored (get-corp))))
      (is (= 0 (:click (get-corp))) "Corp doesn't gain a click on Runner's turn")
      (is (= 4 (:click-per-turn (get-corp)))))))

(deftest vigil
  ;; Vigil - Draw 1 card when turn begins if Corp HQ is filled to max hand size
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 3) (qty "PAD Campaign" 2)])
              (default-runner [(qty "Vigil" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :resPlayer)
    (play-from-hand state :hazPlayer "Vigil")
    (is (= 5 (:memory (get-runner))))
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :hazPlayer (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (is (empty? (:hand (get-runner))))
    (take-credits state :hazPlayer)
    (is (= (count (:hand (get-corp))) (core/hand-size state :resPlayer)) "Corp hand filled to max")
    (take-credits state :resPlayer)
    (is (= 1 (count (:hand (get-runner)))) "Drew 1 card")
    (take-credits state :hazPlayer)
    (play-from-hand state :resPlayer "Hedge Fund")
    (take-credits state :resPlayer)
    (is (not= (count (:hand (get-corp))) (core/hand-size state :resPlayer)) "Corp hand below max")
    (is (= 1 (count (:hand (get-runner)))) "No card drawn")))

(deftest zamba
  ;; Zamba - Whenever corp card is exposed you may gain 1 credit
  (do-game
   (new-game (default-corp [(qty "Ice Wall" 1)])
             (default-runner [(qty "Zamba" 1) (qty "Infiltration" 2)]))
   (play-from-hand state :resPlayer "Ice Wall" "Archives")
   (take-credits state :resPlayer)
   (play-from-hand state :hazPlayer "Zamba")
   (is (= 6 (:memory (get-runner))) "Gain 2 memory")
   (is (= 1 (:credit (get-runner))) "At 1 credit")
   (play-from-hand state :hazPlayer "Infiltration")
   (prompt-choice :hazPlayer "Expose a card")
   (prompt-select :hazPlayer (get-ice state :archives 0))
   (is (= 2 (:credit (get-runner))) "Gained 1 credit from exposing")
   (play-from-hand state :hazPlayer "Infiltration")
   (prompt-choice :hazPlayer "Expose a card")
   (prompt-select :hazPlayer (get-ice state :archives 0))
   (is (= 3 (:credit (get-runner))) "Gained 1 more credit from exposing")))