(ns test.cards.hardware
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest akamatsu-mem
  ;; Akamatsu Mem Chip - Gain 1 memory
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Akamatsu Mem Chip" 3)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Akamatsu Mem Chip")
    (is (= 5 (:memory (get-hero))) "Gain 1 memory")))

(deftest archives-interface
  ;; Archives Interface - Remove 1 card in Archives from the game instead of accessing it
  (do-game
    (new-game (default-minion [(qty "Shock!" 1) (qty "Launch Campaign" 1)])
              (default-hero [(qty "Archives Interface" 1) (qty "Imp" 1)]))
    (take-credits state :minion)
    (core/move state :minion (find-card "Shock!" (:hand (get-minion))) :discard)
    (core/move state :minion (find-card "Launch Campaign" (:hand (get-minion))) :discard)
    (play-from-hand state :hero "Archives Interface")
    (run-empty-server state :archives)
    (prompt-choice :hero "Yes")
    (prompt-choice :hero (find-card "Shock!" (:discard (get-minion))))
    (is (= "Shock!" (:title (first (:rfg (get-minion))))) "Shock! removed from game")
    (is (empty? (:discard (get-hero))) "Didn't access Shock!, no net damage taken")))

(deftest astrolabe-memory
  ;; Astrolabe - Gain 1 memory
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Astrolabe" 3)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Astrolabe")
    (is (= 5 (:memory (get-hero))) "Gain 1 memory")))

(deftest astrolabe-draw
  ;; Astrolabe - Draw on new server install
  (do-game
    (new-game (default-minion [(qty "Snare!" 3)])
              (default-hero [(qty "Astrolabe" 3) (qty "Sure Gamble" 3) (qty "Cloak" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Astrolabe")
    (take-credits state :hero 3)
    ;; minion's turn. install something from HQ to trigger Astrolabe draw
    (play-from-hand state :minion "Snare!" "New remote")
    (is (= 5 (count (:hand (get-hero)))) "Drew 1 card from server install")
    ;; install over the old server; make sure nothing is drawn
    (play-from-hand state :minion "Snare!" "Server 0")
    (is (= 5 (count (:hand (get-hero)))) "Did not draw")
    (is (= 1 (count (:deck (get-hero)))) "1 card left in deck")))

(deftest box-e
  ;; Box-E - +2 MU, +2 max hand size
  (do-game
   (new-game (default-minion)
             (default-hero [(qty "Box-E" 1)]))
   (take-credits state :minion)
   (play-from-hand state :hero "Box-E")
   (is (= 6 (:memory (get-hero))))
   (is (= 7 (core/hand-size state :hero)))))

(deftest blackguard
  ;; Blackguard - +2 MU, forced rez of exposed ice
  (do-game
   (new-game (default-minion [(qty "Ice Wall" 1)])
             (default-hero [(qty "Blackguard" 1)
                              (qty "Snitch" 1)]))
   (play-from-hand state :minion "Ice Wall" "Archives")
   (take-credits state :minion)
   (core/gain state :hero :credit 100)
   (play-from-hand state :hero "Blackguard")
   (is (= 6 (:memory (get-hero))) "Runner has 6 MU")
   (play-from-hand state :hero "Snitch")
   (let [snitch (get-in @state [:hero :rig :program 0])
         iwall (get-ice state :archives 0)]
     (run-on state :archives)
     (card-ability state :hero snitch 0)
     (is (:rezzed (refresh iwall)) "Ice Wall was rezzed"))))

(deftest brain-chip
  ;; Brain Chip handsize and memory limit
  (do-game
   (new-game (default-minion)
             (default-hero [(qty "Brain Chip" 1)]))
   (take-credits state :minion)
   (play-from-hand state :hero "Brain Chip")
   (swap! state assoc-in [:hero :agenda-point] -2) ; hard set ap
   (is (= (core/hand-size state :hero) 5) "Hand size unaffected")
   (is (= (get-in @state [:hero :memory]) 4) "Memory limit unaffected")
   (swap! state assoc-in [:hero :agenda-point] 2)
   (is (= (core/hand-size state :hero) 7) "Hand size increased by 2")
   (is (= (get-in @state [:hero :memory]) 6) "Memory limit increased by 2")
   (core/move state :hero (get-in @state [:hero :rig :hardware 0]) :discard)
   (is (= (core/hand-size state :hero) 5) "Hand size reset")
   (is (= (get-in @state [:hero :memory]) 4) "Memory limit reset")))

(deftest clone-chip
  ;; Test clone chip usage- outside and during run
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Datasucker" 1) (qty "Clone Chip" 2)]))
    (take-credits state :minion)
    (trash-from-hand state :hero "Datasucker")
    (play-from-hand state :hero "Clone Chip")
    (let [chip (get-in @state [:hero :rig :hardware 0])]
      (card-ability state :hero chip 0)
      (prompt-select :hero (find-card "Datasucker" (:discard (get-hero))))
      (let [ds (get-in @state [:hero :rig :program 0])]
        (is (not (nil? ds)))
        (is (= (:title ds) "Datasucker"))))))

(deftest clone-chip-dont-install-choices-hero-cant-afford
  ;; Test clone chip usage - dont show inavalid choices
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Inti" 1) (qty "Magnum Opus" 1) (qty "Clone Chip" 1)]))
    (take-credits state :minion)
    (trash-from-hand state :hero "Inti")
    (trash-from-hand state :hero "Magnum Opus")
    (play-from-hand state :hero "Clone Chip")
    (is (= (get-in @state [:hero :click]) 3) "Runner has 3 clicks left")
    (let [chip (get-in @state [:hero :rig :hardware 0])]
      (card-ability state :hero chip 0)
      (prompt-select :hero (find-card "Magnum Opus" (:discard (get-hero))))
      (is (nil? (get-in @state [:hero :rig :program 0])) "No program was installed"))
    (let [chip (get-in @state [:hero :rig :hardware 0])]
      (is (not (nil? chip)) "Clone Chip is still installed")
      (is (= (get-in @state [:hero :click]) 3) "Runner has 3 clicks left")
      (card-ability state :hero chip 0)
      (prompt-select :hero (find-card "Inti" (:discard (get-hero))))
      (let [inti (get-in @state [:hero :rig :program 0])]
        (is (not (nil? inti)) "Program was installed")
        (is (= (:title inti) "Inti") "Program is Inti")
        (is (= (get-in @state [:hero :click]) 3) "Runner has 3 clicks left")))))

(deftest comet-event-play
  ;; Comet - Play event without spending a click after first event played
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Comet" 3) (qty "Easy Mark" 2)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Comet")
    (let [comet (get-in @state [:hero :rig :hardware 0])]
      (play-from-hand state :hero "Easy Mark")
      (is (= true (:comet-event (core/get-card state comet)))) ; Comet ability enabled
      (card-ability state :hero comet 0)
      (is (= (:cid comet) (-> @state :hero :prompt first :card :cid)))
      (prompt-select :hero (find-card "Easy Mark" (:hand (get-hero))))
      (is (= 7 (:credit (get-hero))))
      (is (= 2 (:click (get-hero))))
      (is (nil? (:comet-event (core/get-card state comet))) "Comet ability disabled"))))

(deftest cortez-chip
  ;; Cortez Chip - Trash to add 2 credits to rez cost of an ICE until end of turn
  (do-game
    (new-game (default-minion [(qty "Quandary" 1)])
              (default-hero [(qty "Cortez Chip" 1)]))
    (play-from-hand state :minion "Quandary" "R&D")
    (take-credits state :minion)
    (play-from-hand state :hero "Cortez Chip")
    (let [quan (get-ice state :rd 0)
          cortez (get-hardware state 0)]
      (card-ability state :hero cortez 0)
      (prompt-select :hero quan)
      (is (= 1 (count (:discard (get-hero)))) "Cortez Chip trashed")
      (core/rez state :minion quan)
      (is (= 4 (:credit (get-minion))) "Paid 3c instead of 1c to rez Quandary"))))

(deftest cybersolutions-mem-chip
  ;; CyberSolutions Mem Chip- Gain 2 memory
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "CyberSolutions Mem Chip" 3)]))
    (take-credits state :minion)
    (play-from-hand state :hero "CyberSolutions Mem Chip")
    (is (= 6 (:memory (get-hero))) "Gain 2 memory")))

(deftest desperado
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Desperado" 3)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Desperado")
    (run-empty-server state :archives)
    (is (= 5 (:memory (get-hero))) "Gain 1 memory")
    (is (= 3 (:credit (get-hero))) "Got 1c for successful run on Desperado")))

(deftest dinosaurus-strength-boost-mu-savings
  ;; Dinosaurus - Boost strength of hosted icebreaker; keep MU the same when hosting or trashing hosted breaker
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Dinosaurus" 1) (qty "Battering Ram" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :credit 5)
    (play-from-hand state :hero "Dinosaurus")
    (let [dino (get-in @state [:hero :rig :hardware 0])]
      (card-ability state :hero dino 0)
      (prompt-select :hero (find-card "Battering Ram" (:hand (get-hero))))
      (is (= 2 (:click (get-hero))))
      (is (= 0 (:credit (get-hero))))
      (is (= 4 (:memory (get-hero))) "Battering Ram 2 MU not deducted from available MU")
      (let [ram (first (:hosted (refresh dino)))]
        (is (= 5 (:current-strength (refresh ram)))
            "Dinosaurus giving +2 strength to Battering Ram")
        ;; Trash Battering Ram
        (core/move state :hero (find-card "Battering Ram" (:hosted (refresh dino))) :discard)
        (is (= 4 (:memory (get-hero))) "Battering Ram 2 MU not added to available MU")))))

(deftest doppelganger
  ;; Doppelgänger - run again when successful
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Doppelgänger" 1)]))
    (core/gain state :minion :bad-publicity 1)
    (take-credits state :minion)
    (play-from-hand state :hero "Doppelgänger")
    (run-empty-server state :hq)
    (prompt-choice :hero "OK")
    (is (= 0 (:run-credit (get-hero))) "Runner lost BP credits")
    (prompt-choice :hero "Yes")
    (prompt-choice :hero "R&D")
    (is (:run @state) "New run started")
    (is (= [:rd] (:server (:run @state))) "Running on R&D")
    (is (= 1 (:run-credit (get-hero))) "Runner has 1 BP credit")))

(deftest dorm-computer
  ;; make a run and avoid all tags for the remainder of the run
  (do-game
    (new-game (default-minion [(qty "Snare!" 1)])
              (default-hero [(qty "Dorm Computer" 1)]))
    (play-from-hand state :minion "Snare!" "New remote")
    (take-credits state :minion)
    (play-from-hand state :hero "Dorm Computer")
    (let [dorm (get-in @state [:hero :rig :hardware 0])]
      (card-ability state :hero dorm 0)
      (prompt-choice :hero "Server 1")
      (run-empty-server state "Server 1")
      (is (:run @state) "New run started")
      (is (= :waiting (-> @state :hero :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :minion "Yes")
      (is (= 0 (:tag (get-hero))) "Runner has 0 tags")
      (is (= 3 (get-counters (refresh dorm) :power))))
      ))

(deftest feedback-filter
  ;; Feedback Filter - Prevent net and brain damage
  (do-game
    (new-game (default-minion [(qty "Data Mine" 1)
                             (qty "Cerebral Overwriter" 1)
                             (qty "Mushin No Shin" 1)])
              (default-hero [(qty "Feedback Filter" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :minion "Mushin No Shin")
    (prompt-select :minion (find-card "Cerebral Overwriter" (:hand (get-minion))))
    (play-from-hand state :minion "Data Mine" "Server 1")
    (let [co (get-content state :remote1 0)
          dm (get-ice state :remote1 0)]
      (is (= 3 (:advance-counter (refresh co))) "3 advancements on Overwriter")
      (take-credits state :minion)
      (play-from-hand state :hero "Sure Gamble")
      (play-from-hand state :hero "Feedback Filter")
      (is (= 7 (:credit (get-hero))))
      (let [ff (get-in @state [:hero :rig :hardware 0])]
        (run-on state "Server 1")
        (core/rez state :minion dm)
        (card-subroutine state :minion dm 0)
        (card-ability state :hero ff 0)
        (prompt-choice :hero "Done")
        (is (= 3 (count (:hand (get-hero)))) "1 net damage prevented")
        (is (= 4 (:credit (get-hero))))
        (run-successful state)
        (prompt-choice :minion "Yes") ; pay 3 to fire Overwriter
        (card-ability state :hero ff 1)
        (prompt-choice :hero "Done")
        (prompt-choice :hero "Yes") ; trash Overwriter for 0
        (is (= 1 (:brain-damage (get-hero))) "2 of the 3 brain damage prevented")
        (is (= 2 (count (:hand (get-hero)))))
        (is (empty? (get-in @state [:hero :rig :hardware])) "Feedback Filter trashed")))))

(deftest grimoire
  ;; Grimoire - Gain 2 MU, add a free virus counter to installed virus programs
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Grimoire" 1) (qty "Imp" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Grimoire")
    (is (= 6 (:memory (get-hero))) "Gained 2 MU")
    (play-from-hand state :hero "Imp")
    (let [imp (get-in @state [:hero :rig :program 0])]
      (is (= 3 (get-counters (refresh imp) :virus)) "Imp received an extra virus counter on install"))))

(deftest llds-processor
  ;; LLDS Processor - Add 1 strength until end of turn to an icebreaker upon install
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "LLDS Processor" 2) (qty "Inti" 1) (qty "Passport" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "LLDS Processor")
    (play-from-hand state :hero "Inti")
    (play-from-hand state :hero "LLDS Processor")
    (play-from-hand state :hero "Passport")
    (let [inti (get-in @state [:hero :rig :program 0])
          pass (get-in @state [:hero :rig :program 1])]
      (is (= 2 (:current-strength (refresh inti))) "Strength boosted by 1; 1 copy of LLDS when installed")
      (is (= 4 (:current-strength (refresh pass))) "Strength boosted by 2; 2 copies of LLDS when installed")
      (take-credits state :hero)
      (is (= 1 (:current-strength (refresh inti))) "Strength reduced to default")
      (is (= 2 (:current-strength (refresh pass))) "Strength reduced to default"))))

(deftest maw
  ;; Once per turn, first time hero declines to steal or trash, trash a HQ card at random
  (do-game
    (new-game (default-minion [(qty "BOOM!" 5)])
              (default-hero [(qty "Maw" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :credit 20)
    (run-empty-server state :hq)
    (prompt-choice :hero "No")
    (is (= 0 (count (:discard (get-minion)))) "No HQ card in discard before Maw installed")
    (play-from-hand state :hero "Maw")
    (run-empty-server state :hq)
    (prompt-choice :hero "No")
    (is (= 0 (count (:discard (get-minion)))) "HQ card not trashed by Maw as first decline already happened")
    (take-credits state :hero)
    (take-credits state :minion)
    (run-empty-server state :hq)
    (prompt-choice :hero "No")
    (is (= 1 (count (:discard (get-minion)))) "HQ card trashed by Maw")
    (run-empty-server state :hq)
    (prompt-choice :hero "No")
    (is (= 1 (count (:discard (get-minion)))) "2nd HQ card on same turn not trashed by Maw")))

(deftest maw-card-seen
  ;; Check trashed card is trashed face-up if it's the card that is accessed, issue #2695
  ;; Also checks Maw auto-trashes on Operation with no trash cost
  (do-game
    (new-game (default-minion [(qty "Hedge Fund" 1)])
              (default-hero [(qty "Maw" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :credit 20)
    (play-from-hand state :hero "Maw")
    (run-empty-server state :hq)
    ;; (is (= 0 (count (:discard (get-minion)))) "HQ card not trashed by Maw yet")
    (prompt-choice :hero "OK")
    (is (= 1 (count (:discard (get-minion)))) "HQ card trashed by Maw now")
    (is (:seen (first (:discard (get-minion)))) "Trashed card is registered as seen since it was accessed")))

(deftest maw-hiro
  ;; Maw with Hiro in hand - Hiro not moved to hero scored area on trash decline #2638
  (do-game
    (new-game (default-minion [(qty "Chairman Hiro" 1)])
              (default-hero [(qty "Maw" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :credit 20)
    (play-from-hand state :hero "Maw")
    (run-empty-server state :hq)
    (prompt-choice :hero "No")
    (is (= 0 (count (:scored (get-minion)))) "Hiro not scored")
    (is (= 1 (count (:discard (get-minion)))) "Hiro trashed by Maw")))

(deftest maya
  ;; Maya - Move accessed card to bottom of R&D
  (do-game
    (new-game (default-minion [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
              (default-hero [(qty "Maya" 1) (qty "Sure Gamble" 3)]))
    (core/move state :minion (find-card "Scorched Earth" (:hand (get-minion))) :deck)
    (core/move state :minion (find-card "Snare!" (:hand (get-minion))) :deck)
    (take-credits state :minion)
    (play-from-hand state :hero "Maya")
    (let [maya (get-in @state [:hero :rig :hardware 0])
          accessed (first (:deck (get-minion)))]
      (run-empty-server state :rd)
      (is (= (:cid accessed) (:cid (:card (first (:prompt (get-hero)))))) "Accessing the top card of R&D")
      (card-ability state :hero maya 0)
      (is (empty? (:prompt (get-hero))) "No more prompts for hero")
      (is (not (:run @state)) "Run is ended")
      (is (= (:cid accessed) (:cid (last (:deck (get-minion))))) "Maya moved the accessed card to the bottom of R&D")
      (take-credits state :hero)
      (core/draw state :minion)
      (take-credits state :minion)
      (core/move state :minion (find-card "Snare!" (:hand (get-minion))) :deck)
      (core/move state :minion (find-card "Scorched Earth" (:hand (get-minion))) :deck)
      (let [accessed (first (:deck (get-minion)))]
        (run-empty-server state :rd)
        (prompt-choice :minion "Yes")
        (is (= 0 (count (:hand (get-hero)))) "Runner took Snare! net damage")
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-hero)))))) "Accessing the top card of R&D")
        (card-ability state :hero maya 0)
        (is (empty? (:prompt (get-hero))) "No more prompts for hero")
        (is (not (:run @state)) "Run is ended")
        (is (= (:cid accessed) (:cid (last (:deck (get-minion))))) "Maya moved the accessed card to the bottom of R&D")))))

(deftest maya-multi-access
  ;; Maya - Does not interrupt multi-access.
  (do-game
    (new-game (default-minion [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
              (default-hero [(qty "Maya" 1) (qty "Sure Gamble" 3) (qty "R&D Interface" 1)]))
    (core/move state :minion (find-card "Scorched Earth" (:hand (get-minion))) :deck)
    (core/move state :minion (find-card "Snare!" (:hand (get-minion))) :deck)
    (take-credits state :minion)
    (core/gain state :hero :credit 10)
    (play-from-hand state :hero "Maya")
    (play-from-hand state :hero "R&D Interface")
    (let [maya (get-in @state [:hero :rig :hardware 0])
          accessed (first (:deck (get-minion)))]
      (run-empty-server state :rd)
      (prompt-choice :hero "Card from deck")
      (is (= (:cid accessed) (:cid (:card (first (:prompt (get-hero)))))) "Accessing the top card of R&D")
      (card-ability state :hero maya 0)
      (is (= (:cid accessed) (:cid (last (:deck (get-minion))))) "Maya moved the accessed card to the bottom of R&D")
      (is (:prompt (get-hero)) "Runner has next access prompt"))))

(deftest obelus
  ;; Obelus - Increase max hand size with tags, draw cards on first successful HQ/R&D run
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Obelus" 1) (qty "Nerve Agent" 1)
                               (qty "Sure Gamble" 3) (qty "Cache" 3)]))
    (take-credits state :minion)
    (starting-hand state :hero ["Obelus" "Nerve Agent"])
    (core/gain state :hero :credit 10 :click 3)
    (play-from-hand state :hero "Nerve Agent")
    (let [nerve (get-in @state [:hero :rig :program 0])]
      (run-empty-server state :hq)
      (is (= 1 (get-counters (refresh nerve) :virus)) "1 virus counter on Nerve Agent")
      (prompt-choice :hero "OK")
      (play-from-hand state :hero "Obelus")
      (core/gain state :hero :tag 1)
      (is (= 6 (core/hand-size state :hero)) "Max hand size is 6")
      (core/lose state :hero :tag 1)
      (is (= 5 (core/hand-size state :hero)) "Max hand size is 5")
      (run-empty-server state :hq)
      (is (= 2 (get-counters (refresh nerve) :virus)) "2 virus counters on Nerve Agent")
      (prompt-choice :hero 1)
      (prompt-choice :hero "Card from hand")
      (prompt-choice :hero "OK")
      (prompt-choice :hero "Card from hand")
      (prompt-choice :hero "OK")
      (is (empty? (:hand (get-hero))) "No cards drawn by Obelus, already had successful HQ run")
      (take-credits state :hero)
      (take-credits state :minion)
      (run-empty-server state :hq)
      (is (= 3 (get-counters (refresh nerve) :virus)) "3 virus counters on Nerve Agent")
      (prompt-choice :hero 2)
      (prompt-choice :hero "Card from hand")
      (prompt-choice :hero "OK")
      (prompt-choice :hero "Card from hand")
      (prompt-choice :hero "OK")
      (prompt-choice :hero "Card from hand")
      (prompt-choice :hero "OK")
      (is (= 3 (count (:hand (get-hero)))) "Obelus drew 3 cards"))))

(deftest obelus-hades-shard
  ;; Obelus - using Hades Shard during run to increase draw
  (do-game
    (new-game (default-minion [(qty "Hedge Fund" 3) (qty "Restructure" 3)])
              (default-hero [(qty "Obelus" 1) (qty "Hades Shard" 1)
                               (qty "Sure Gamble" 3) (qty "Cache" 3)]))
    (starting-hand state :minion ["Hedge Fund" "Hedge Fund"])
    (trash-from-hand state :minion "Hedge Fund")
    (trash-from-hand state :minion "Hedge Fund")
    (take-credits state :minion)
    (starting-hand state :hero ["Obelus" "Hades Shard"])
    (core/gain state :hero :credit 10)
    (play-from-hand state :hero "Obelus")
    (play-from-hand state :hero "Hades Shard")
    (run-empty-server state "R&D")
    (card-ability state :hero (get-resource state 0) 0)
    (prompt-choice :hero "OK")
    (is (= 3 (count (:hand (get-hero)))) "Obelus drew 3 cards")))

(deftest plascrete
  ;; Plascrete Carapace - Prevent meat damage
  (do-game
    (new-game (default-minion [(qty "Scorched Earth" 1)])
              (default-hero [(qty "Plascrete Carapace" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Plascrete Carapace")
    (let [plas (get-in @state [:hero :rig :hardware 0])]
      (is (= 4 (get-counters (refresh plas) :power)) "4 counters on install")
      (take-credits state :hero)
      (core/gain state :hero :tag 1)
      (play-from-hand state :minion "Scorched Earth")
      (card-ability state :hero plas 0)
      (card-ability state :hero plas 0)
      (card-ability state :hero plas 0)
      (card-ability state :hero plas 0)
      (prompt-choice :hero "Done")
      (is (= 1 (count (:hand (get-hero)))) "All meat damage prevented")
      (is (empty? (get-in @state [:hero :rig :hardware])) "Plascrete depleted and trashed"))))

(deftest rabbit-hole
  ;; Rabbit Hole - +1 link, optionally search Stack to install more copies
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Sure Gamble" 1) (qty "Rabbit Hole" 3)]))
    (take-credits state :minion)
    (core/move state :hero (find-card "Rabbit Hole" (:hand (get-hero))) :deck)
    (core/move state :hero (find-card "Rabbit Hole" (:hand (get-hero))) :deck)
    (play-from-hand state :hero "Sure Gamble")
    (play-from-hand state :hero "Rabbit Hole")
    (is (= 1 (:link (get-hero))))
    (prompt-choice :hero "Yes")
    (prompt-choice :hero "Yes")
    (is (= 3 (:link (get-hero))))
    (is (= 3 (count (get-in @state [:hero :rig :hardware]))))
    (is (= 2 (:click (get-hero))) "Clickless installs of extra 2 copies")
    (is (= 3 (:credit (get-hero))) "Paid 2c for each of 3 copies")))

(deftest recon-drone
  ;; trash and pay X to prevent that much damage from a card you are accessing
  (do-game
    (new-game (default-minion [(qty "Snare!" 1) (qty "House of Knives" 1)
                             (qty "Prisec" 1) (qty "Cerebral Overwriter" 1)])
              (default-hero [(qty "Recon Drone" 10)]))
    (core/gain state :minion :click 10)
    (core/gain state :minion :credit 100)
    (play-from-hand state :minion "House of Knives" "New remote")
    (play-from-hand state :minion "Snare!" "New remote")
    (play-from-hand state :minion "Prisec" "New remote")
    (play-from-hand state :minion "Cerebral Overwriter" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    (core/advance state :minion (get-content state :remote4 0))
    (take-credits state :minion)
    (core/gain state :hero :click 100)
    (core/gain state :hero :credit 100)
    (core/draw state :hero)
    (core/draw state :hero)
    (core/draw state :hero)
    (core/draw state :hero)
    (play-from-hand state :hero "Recon Drone")
    (play-from-hand state :hero "Recon Drone")
    (play-from-hand state :hero "Recon Drone")
    (play-from-hand state :hero "Recon Drone")
    (let [rd1 (get-in @state [:hero :rig :hardware 0])
          rd2 (get-in @state [:hero :rig :hardware 1])
          rd3 (get-in @state [:hero :rig :hardware 2])
          rd4 (get-in @state [:hero :rig :hardware 3])
          hok (get-in @state [:minion :scored 0])]
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :hero :prompt first :prompt-type))
        "Runner has prompt to wait for Snare!")
      (prompt-choice :minion "Yes")
      (card-ability state :hero rd1 0)
      (prompt-choice :hero 3)
      (prompt-choice :hero "Done")
      (is (= 5 (count (:hand (get-hero)))) "Runner took no net damage")
      ; fire HOK while accessing Snare!
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :hero :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (card-ability state :minion hok 0)
      ; Recon Drone ability won't fire as we are not accessing HOK
      (card-ability state :hero rd2 0)
      (is (nil? (:number (:choices (first (:prompt (get-hero)))))) "No choice to prevent damage from HOK")
      (prompt-choice :hero "Done")
      (is (= 4 (count (:hand (get-hero)))) "Runner took 1 net damage from HOK")
      (prompt-choice :minion "No")
      (core/lose state :hero :credit 100)
      ; can only stop 1 damage due to credits
      (core/gain state :hero :credit 1)
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :hero :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :minion "Yes")
      (card-ability state :hero rd2 0)
      (is (= 1 (:number (:choices (first (:prompt (get-hero)))))) "Recon Drone choice limited to hero credits")
      (prompt-choice :hero 1)
      (prompt-choice :hero "Done")
      (is (= 2 (count (:hand (get-hero)))) "Runner took 2 net damage from Snare!")
      (core/gain state :hero :credit 100)
      (run-empty-server state "Server 3")
      (is (= :waiting (-> @state :hero :prompt first :prompt-type))
          "Runner has prompt to wait for Prisec")
      (prompt-choice :minion "Yes")
      (card-ability state :hero rd3 0)
      (is (= 1 (:number (:choices (first (:prompt (get-hero)))))) "Recon Drone choice limited to 1 meat")
      (prompt-choice :hero 1)
      (prompt-choice :hero "Done")
      (is (= 2 (count (:hand (get-hero)))) "Runner took no meat damage")
      (run-empty-server state "Server 4")
      (is (= :waiting (-> @state :hero :prompt first :prompt-type))
          "Runner has prompt to wait for Cerebral Overwriter")
      (prompt-choice :minion "Yes")
      (card-ability state :hero rd4 0)
      (prompt-choice :hero 1)
      (prompt-choice :hero "Done")
      (is (= 2 (count (:hand (get-hero)))) "Runner took no brain damage"))))

(deftest ramujan-reliant
  ;; Prevent up to X net or brain damage.
  (do-game
    (new-game (default-minion [(qty "Data Mine" 1)
                             (qty "Snare!" 1)])
              (default-hero [(qty "Ramujan-reliant 550 BMI" 4) (qty "Sure Gamble" 6)]))
    (starting-hand state :hero
                   ["Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Sure Gamble"])
    (play-from-hand state :minion "Data Mine" "Server 1")
    (play-from-hand state :minion "Snare!" "Server 1")
    (let [sn (get-content state :remote1 0)
          dm (get-ice state :remote1 0)]
      (take-credits state :minion)
      (play-from-hand state :hero "Ramujan-reliant 550 BMI")
      (play-from-hand state :hero "Ramujan-reliant 550 BMI")
      (play-from-hand state :hero "Ramujan-reliant 550 BMI")
      (let [rr1 (get-in @state [:hero :rig :hardware 0])
            rr2 (get-in @state [:hero :rig :hardware 1])
            rr3 (get-in @state [:hero :rig :hardware 2])]
        (run-on state "Server 1")
        (core/rez state :minion dm)
        (card-subroutine state :minion dm 0)
        (card-ability state :hero rr1 0)
        (prompt-choice :hero 1)
        (is (last-log-contains? state "Sure Gamble")
            "Ramujan did log trashed card names")
        (is (= 2 (count (:hand (get-hero)))) "1 net damage prevented")
        (run-successful state)
        (take-credits state :hero)
        (take-credits state :minion)
        (play-from-hand state :hero "Ramujan-reliant 550 BMI")
        (run-empty-server state "Server 1")
        (prompt-choice :minion "Yes")
        (card-ability state :hero rr2 0)
        (prompt-choice :hero 3)
        (is (last-log-contains? state "Sure Gamble, Sure Gamble, Sure Gamble")
            "Ramujan did log trashed card names")
        (is (= 1 (count (:hand (get-hero)))) "3 net damage prevented")))))

(deftest ramujan-reliant-empty
  ;; Prevent up to X net or brain damage. Empty stack
  (do-game
    (new-game (default-minion [(qty "Data Mine" 1)])
              (default-hero [(qty "Ramujan-reliant 550 BMI" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :minion "Data Mine" "Server 1")
    (let [dm (get-ice state :remote1 0)]
      (take-credits state :minion)
      (play-from-hand state :hero "Ramujan-reliant 550 BMI")
      (let [rr1 (get-in @state [:hero :rig :hardware 0])]
        (run-on state "Server 1")
        (core/rez state :minion dm)
        (card-subroutine state :minion dm 0)
        (card-ability state :hero rr1 0)
        (prompt-choice :hero 1)
        (is (= 0 (count (:hand (get-hero)))) "Not enough cards in Stack for Ramujan to work")))))

(deftest replicator-bazaar
  ;; Replicator - interaction with Bazaar. Issue #1511.
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Replicator" 1) (qty "Bazaar" 1) (qty "Spy Camera" 6)]))
    (letfn [(count-spy [n] (= n (count (filter #(= "Spy Camera" (:title %)) (-> (get-hero) :rig :hardware)))))]
      (take-credits state :minion)
      (starting-hand state :hero ["Replicator" "Bazaar" "Spy Camera"])
      (play-from-hand state :hero "Replicator")
      (play-from-hand state :hero "Bazaar")
      (play-from-hand state :hero "Spy Camera") ; 1 installed
      (is (count-spy 1) "1 Spy Cameras installed")
      (prompt-choice :hero "Yes") ; for now, choosing Replicator then shows its optional Yes/No
      (prompt-choice :hero "Yes") ; Bazaar triggers, 2 installed
      (is (count-spy 2) "2 Spy Cameras installed")
      (prompt-choice :hero "Yes")
      (prompt-choice :hero "Yes")  ; 3 installed
      (is (count-spy 3) "3 Spy Cameras installed")

      (prompt-choice :hero "Yes")
      (prompt-choice :hero "Yes")  ; 4 installed
      (is (count-spy 4) "4 Spy Cameras installed")

      (prompt-choice :hero "Yes")
      (prompt-choice :hero "Yes")  ; 5 installed
      (is (count-spy 5) "5 Spy Cameras installed")

      (prompt-choice :hero "Yes")
      (prompt-choice :hero "Yes")  ; 6 installed
      (is (count-spy 6) "6 Spy Cameras installed"))))

(deftest respirocytes-multiple
  ;; Should draw multiple cards when multiple respirocytes are in play
  (do-game
   (new-game (default-minion)
             (default-hero [(qty "Respirocytes" 3) (qty "Sure Gamble" 3)]))
   (take-credits state :minion)
   (starting-hand state :hero ["Respirocytes" "Respirocytes" "Respirocytes" "Sure Gamble"])
   (dotimes [_ 2]
     (play-from-hand state :hero "Respirocytes"))
   (is (= 2 (count (:discard (get-hero)))) "2 damage done")
   (is (= 2 (count (:hand (get-hero)))) "Drew 2 cards")))

(deftest sifr
  ;; Once per turn drop encountered ICE to zero strenght
  ;; Also handle archangel then re-install sifr should not break the game #2576
  (do-game
    (new-game (default-minion [(qty "Archangel" 1) (qty "IP Block" 1) (qty "Hedge Fund" 1)])
              (default-hero [(qty "Modded" 1) (qty "Clone Chip" 1) (qty "Şifr" 1) (qty "Parasite" 1)]))
    (core/gain state :minion :credit 100)
    (core/gain state :hero :credit 100)
    (play-from-hand state :minion "Archangel" "HQ")
    (play-from-hand state :minion "IP Block" "HQ")
    (take-credits state :minion)
    (trash-from-hand state :hero "Parasite")
    (play-from-hand state :hero "Şifr")
    (is (= 2 (count (:hand (get-hero)))) "Modded and Clone Chip in hand")
    (let [arch (get-ice state :hq 0)
          ip (get-ice state :hq 1)
          sifr (get-hardware state 0)]
      (core/rez state :minion arch)
      (core/rez state :minion ip)
      (is (= 4 (:current-strength (refresh ip))))
      (run-on state :hq)
      (is (= 2 (:position (:run @state))))
      (card-ability state :hero sifr 0)
      (is (= 0 (:current-strength (refresh ip))))
      (run-continue state)
      (is (= 1 (:position (:run @state))))
      (is (= 2 (count (:hand (get-hero))))) ; pre archangel
      (card-subroutine state :minion arch 0) ; fire archangel
      (is (not (empty? (:prompt (get-minion)))) "Archangel trace prompt - minion")
      (prompt-choice :minion 0)
      (is (not (empty? (:prompt (get-hero)))) "Archangel trace prompt - hero")
      (prompt-choice :hero 0)
      (prompt-select :minion sifr)
      (is (= 3 (count (:hand (get-hero))))) ; sifr got lifted to hand
      (run-jack-out state)
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :hero "Modded")
      (is (not (empty? (:prompt (get-hero)))) "Modded choice prompt exists")
      (prompt-select :hero (find-card "Şifr" (:hand (get-hero))))
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :hero "Clone Chip")
      (take-credits state :hero)
      (take-credits state :minion 4)
      (let [chip (get-hardware state 1)]
        (is (nil? (:sifr-target (refresh sifr))) "Sifr cleaned up on leave play")
        (is (= 0 (count (:discard (get-minion)))) "No Corp cards trashed")
        (card-ability state :hero chip 0)
        (prompt-select :hero (find-card "Parasite" (:discard (get-hero))))
        (let [para (get-program state 0)]
          (prompt-select :hero ip)
          (is (= 0 (count (:discard (get-minion)))) "IP Block Not Trashed")
          (is (= 1 (count (:hosted (refresh ip)))) "Parasite is hosted"))))))

(deftest spinal-modem
  ;; Spinal Modem - +1 MU, 2 recurring credits, take 1 brain damage on successful trace during run
  (do-game
    (new-game (default-minion [(qty "Caduceus" 1)])
              (default-hero [(qty "Spinal Modem" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :minion "Caduceus" "HQ")
    (take-credits state :minion)
    (play-from-hand state :hero "Spinal Modem")
    (let [cad (get-ice state :hq 0)
          sm (get-hardware state 0)]
      (is (= 5 (:memory (get-hero))))
      (is (= 2 (:rec-counter (refresh sm))))
      (run-on state :hq)
      (core/rez state :minion cad)
      (card-subroutine state :minion cad 0)
      (prompt-choice :minion 0)
      (prompt-choice :hero 0)
      (is (= 1 (:brain-damage (get-hero))) "Took 1 brain damage")
      (is (= 1 (count (:discard (get-hero)))))
      (is (= 4 (core/hand-size state :hero)) "Reduced hand size"))))

(deftest spy-camera
  ;; Spy Camera - Full test
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "Spy Camera" 6) (qty "Sure Gamble" 1) (qty "Desperado" 1)
                               (qty "Diesel" 1) (qty "Corroder" 1) (qty "Patron" 1) (qty "Kati Jones" 1)]))
    (starting-hand state :hero ["Spy Camera" "Spy Camera" "Spy Camera"
                                  "Spy Camera" "Spy Camera" "Spy Camera"])
    (is (= 6 (count (:hand (get-hero)))))
    (core/move state :minion (find-card "Hedge Fund" (:hand (get-minion))) :deck)
    (take-credits state :minion)
    (core/gain state :hero :click 3)
    (dotimes [_ 6] (play-from-hand state :hero "Spy Camera"))
    (let [spy (get-hardware state 5)]
      ;; look at top 6 cards
      (card-ability state :hero spy 0)
      (prompt-choice :hero (find-card "Sure Gamble" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Desperado" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Diesel" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Corroder" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Patron" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Kati Jones" (:deck (get-hero))))
      ;; try starting over
      (prompt-choice :hero "Start over")
      (prompt-choice :hero (find-card "Kati Jones" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Patron" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Corroder" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Diesel" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Desperado" (:deck (get-hero))))
      (prompt-choice :hero (find-card "Sure Gamble" (:deck (get-hero)))) ;this is the top card on stack
      (prompt-choice :hero "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-hero))))))
      (is (= "Desperado" (:title (second (:deck (get-hero))))))
      (is (= "Diesel" (:title (second (rest (:deck (get-hero)))))))
      (is (= "Corroder" (:title (second (rest (rest (:deck (get-hero))))))))
      (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-hero)))))))))
      (is (= "Kati Jones" (:title (second (rest (rest (rest (rest (:deck (get-hero))))))))))
      ;; look at top card of R&D
      (card-ability state :hero spy 1)
      (let [topcard (get-in (first (get-in @state [:hero :prompt])) [:msg])]
        (is (= "The top card of R&D is Hedge Fund" topcard)))
      (is (= 1 (count (:discard (get-hero))))))))

(deftest the-gauntlet-not-with-gang-sign
  ;; Access additional cards on run on HQ, not with Gang Sign
  ;; Issue #2749
  (do-game
    (new-game (default-minion [(qty "Hostile Takeover" 1)
                             (qty "Hedge Fund" 3)])
              (default-hero [(qty "The Gauntlet" 1)
                               (qty "Gang Sign" 1)]))
    (take-credits state :minion)
    (core/gain state :hero :credit 5)
    (play-from-hand state :hero "Gang Sign")
    (play-from-hand state :hero "The Gauntlet")
    (take-credits state :hero)
    (play-from-hand state :minion "Hostile Takeover" "New remote")
    (score-agenda state :minion (get-content state :remote1 0))
    ;; Gang Sign should trigger, without The Gauntlet pop-up
    (let [gs (get-resource state 0)]
      (prompt-is-card? :hero gs))
    ;; This will throw error if The Gauntlet triggers.
    (prompt-choice :hero "Card from hand")))

(deftest the-personal-touch
  ;; The Personal Touch - Give +1 strength to an icebreaker
  (do-game
    (new-game (default-minion)
              (default-hero [(qty "The Personal Touch" 1)
                               (qty "Paricia" 1)
                               (qty "Faerie" 1)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Paricia")
    (play-from-hand state :hero "Faerie")
    (let [par (get-in @state [:hero :rig :program 0])
          fae (get-in @state [:hero :rig :program 1])]
      (is (= 2 (:current-strength (refresh fae))))
      (play-from-hand state :hero "The Personal Touch")
      (prompt-select :hero par)
      (is (nil? (:hosted (refresh par))) "TPT can't be hosted on a non-icebreaker")
      (prompt-select :hero fae)
      (is (= 1 (count (:hosted (refresh fae)))) "TPT hosted on Faerie")
      (is (= 3 (:current-strength (refresh fae))) "Faerie receiving +1 strength from TPT"))))

(deftest titanium-ribs
  ;; Titanium Ribs - Choose cards lost to damage, but not on Corp turn against Chronos Protocol
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1) (qty "Viktor 1.0" 1)
                                                                     (qty "Neural EMP" 1)])
              (default-hero [(qty "Titanium Ribs" 2) (qty "Sure Gamble" 1)
                               (qty "Fall Guy" 1) (qty "Kati Jones" 1)]))
    (play-from-hand state :minion "Pup" "HQ")
    (play-from-hand state :minion "Viktor 1.0" "R&D")
    (take-credits state :minion)
    (play-from-hand state :hero "Fall Guy")
    (play-from-hand state :hero "Titanium Ribs")
    (prompt-select :hero (find-card "Titanium Ribs" (:hand (get-hero))))
    (prompt-select :hero (find-card "Kati Jones" (:hand (get-hero))))
    (is (empty? (:prompt (get-hero))) "Fall Guy didn't try to prevent trashing of Kati")
    (is (= 2 (count (:discard (get-hero)))) "2 cards trashed for Ribs installation meat damage")
    (run-on state "HQ")
    (let [pup (get-ice state :hq 0)]
      (core/rez state :minion pup)
      (card-subroutine state :minion pup 0)
      (prompt-select :hero (find-card "Sure Gamble" (:hand (get-hero)))) ; Ribs takes precedence over CP on Runner turn
      (is (= 3 (count (:discard (get-hero)))) "Chose card lost from 1 net damage")
      (run-jack-out state)
      (take-credits state :hero)
      (core/move state :hero (find-card "Sure Gamble" (:discard (get-hero))) :hand)
      (core/move state :hero (find-card "Kati Jones" (:discard (get-hero))) :hand)
      (play-from-hand state :minion "Neural EMP")
      (prompt-choice :minion "Yes")
      (let [kati (find-card "Kati Jones" (:hand (get-hero)))]
        (prompt-choice :minion kati) ; Chronos Protocol takes precedence over Ribs on Corp turn
        (is (= 2 (count (:discard (get-hero)))) "Card chosen by Corp for first net damage")))))

(deftest turntable-swap
  ;; Turntable - Swap a stolen agenda for a scored agenda
  (do-game
    (new-game (default-minion [(qty "Domestic Sleepers" 1) (qty "Project Vitruvius" 1)])
              (default-hero [(qty "Turntable" 1)]))
    (play-from-hand state :minion "Project Vitruvius" "New remote")
    (let [ag1 (get-content state :remote1 0)]
      (score-agenda state :minion ag1)
      (take-credits state :minion)
      (play-from-hand state :hero "Turntable")
      (is (= 3 (:credit (get-hero))))
      (let [tt (get-in @state [:hero :rig :hardware 0])]
        (run-empty-server state "HQ")
        (prompt-choice :hero "Steal")
        (is (= 0 (:agenda-point (get-hero))) "Stole Domestic Sleepers")
        (is (prompt-is-card? :hero tt))
        (prompt-choice :hero "Yes")
        (prompt-select :hero (find-card "Project Vitruvius" (:scored (get-minion))))
        (is (= 2 (:agenda-point (get-hero))) "Took Project Vitruvius from Corp")
        (is (= 0 (:agenda-point (get-minion))) "Swapped Domestic Sleepers to Corp")))))

(deftest turntable-mandatory-upgrades
  ;; Turntable - Swap a Mandatory Upgrades away from the Corp reduces Corp clicks per turn
  ;;           - Corp doesn't gain a click on the Runner's turn when it receives a Mandatory Upgrades
  (do-game
    (new-game (default-minion [(qty "Mandatory Upgrades" 2) (qty "Project Vitruvius" 1)])
              (default-hero [(qty "Turntable" 1)]))
    (score-agenda state :minion (find-card "Mandatory Upgrades" (:hand (get-minion))))
    (is (= 4 (:click-per-turn (get-minion))) "Up to 4 clicks per turn")
    (take-credits state :minion)
    (play-from-hand state :hero "Turntable")
    (let [tt (get-in @state [:hero :rig :hardware 0])]
      ;; steal Project Vitruvius and swap for Mandatory Upgrades
      (core/steal state :hero (find-card "Project Vitruvius" (:hand (get-minion))))
      (is (prompt-is-card? :hero tt))
      (prompt-choice :hero "Yes")
      (prompt-select :hero (find-card "Mandatory Upgrades" (:scored (get-minion))))
      (is (= 3 (:click-per-turn (get-minion))) "Back down to 3 clicks per turn")
      ;; steal second Mandatory Upgrades and swap for Project Vitruvius
      (core/steal state :hero (find-card "Mandatory Upgrades" (:hand (get-minion))))
      (is (prompt-is-card? :hero tt))
      (prompt-choice :hero "Yes")
      (prompt-select :hero (find-card "Project Vitruvius" (:scored (get-minion))))
      (is (= 0 (:click (get-minion))) "Corp doesn't gain a click on Runner's turn")
      (is (= 4 (:click-per-turn (get-minion)))))))

(deftest vigil
  ;; Vigil - Draw 1 card when turn begins if Corp HQ is filled to max hand size
  (do-game
    (new-game (default-minion [(qty "Hedge Fund" 3) (qty "PAD Campaign" 2)])
              (default-hero [(qty "Vigil" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :minion)
    (play-from-hand state :hero "Vigil")
    (is (= 5 (:memory (get-hero))))
    (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
    (core/move state :hero (find-card "Sure Gamble" (:hand (get-hero))) :deck)
    (is (empty? (:hand (get-hero))))
    (take-credits state :hero)
    (is (= (count (:hand (get-minion))) (core/hand-size state :minion)) "Corp hand filled to max")
    (take-credits state :minion)
    (is (= 1 (count (:hand (get-hero)))) "Drew 1 card")
    (take-credits state :hero)
    (play-from-hand state :minion "Hedge Fund")
    (take-credits state :minion)
    (is (not= (count (:hand (get-minion))) (core/hand-size state :minion)) "Corp hand below max")
    (is (= 1 (count (:hand (get-hero)))) "No card drawn")))

(deftest zamba
  ;; Zamba - Whenever minion card is exposed you may gain 1 credit
  (do-game
   (new-game (default-minion [(qty "Ice Wall" 1)])
             (default-hero [(qty "Zamba" 1) (qty "Infiltration" 2)]))
   (play-from-hand state :minion "Ice Wall" "Archives")
   (take-credits state :minion)
   (play-from-hand state :hero "Zamba")
   (is (= 6 (:memory (get-hero))) "Gain 2 memory")
   (is (= 1 (:credit (get-hero))) "At 1 credit")
   (play-from-hand state :hero "Infiltration")
   (prompt-choice :hero "Expose a card")
   (prompt-select :hero (get-ice state :archives 0))
   (is (= 2 (:credit (get-hero))) "Gained 1 credit from exposing")
   (play-from-hand state :hero "Infiltration")
   (prompt-choice :hero "Expose a card")
   (prompt-select :hero (get-ice state :archives 0))
   (is (= 3 (:credit (get-hero))) "Gained 1 more credit from exposing")))