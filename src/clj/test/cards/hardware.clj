(ns test.cards.hardware
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest akamatsu-mem
  ;; Akamatsu Mem Chip - Gain 1 memory
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Akamatsu Mem Chip" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Akamatsu Mem Chip")
    (is (= 5 (:memory (get-challenger))) "Gain 1 memory")))

(deftest archives-interface
  ;; Archives Interface - Remove 1 card in Archives from the game instead of accessing it
  (do-game
    (new-game (default-contestant [(qty "Shock!" 1) (qty "Launch Campaign" 1)])
              (default-challenger [(qty "Archives Interface" 1) (qty "Imp" 1)]))
    (take-credits state :contestant)
    (core/move state :contestant (find-card "Shock!" (:hand (get-contestant))) :discard)
    (core/move state :contestant (find-card "Launch Campaign" (:hand (get-contestant))) :discard)
    (play-from-hand state :challenger "Archives Interface")
    (run-empty-server state :archives)
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger (find-card "Shock!" (:discard (get-contestant))))
    (is (= "Shock!" (:title (first (:rfg (get-contestant))))) "Shock! removed from game")
    (is (empty? (:discard (get-challenger))) "Didn't access Shock!, no net damage taken")))

(deftest astrolabe-memory
  ;; Astrolabe - Gain 1 memory
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Astrolabe" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Astrolabe")
    (is (= 5 (:memory (get-challenger))) "Gain 1 memory")))

(deftest astrolabe-draw
  ;; Astrolabe - Draw on new server install
  (do-game
    (new-game (default-contestant [(qty "Snare!" 3)])
              (default-challenger [(qty "Astrolabe" 3) (qty "Sure Gamble" 3) (qty "Cloak" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Astrolabe")
    (take-credits state :challenger 3)
    ;; contestant's turn. install something from HQ to trigger Astrolabe draw
    (play-from-hand state :contestant "Snare!" "New remote")
    (is (= 5 (count (:hand (get-challenger)))) "Drew 1 card from server install")
    ;; install over the old server; make sure nothing is drawn
    (play-from-hand state :contestant "Snare!" "Server 0")
    (is (= 5 (count (:hand (get-challenger)))) "Did not draw")
    (is (= 1 (count (:deck (get-challenger)))) "1 card left in deck")))

(deftest box-e
  ;; Box-E - +2 MU, +2 max hand size
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Box-E" 1)]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Box-E")
   (is (= 6 (:memory (get-challenger))))
   (is (= 7 (core/hand-size state :challenger)))))

(deftest blackguard
  ;; Blackguard - +2 MU, forced rez of exposed character
  (do-game
   (new-game (default-contestant [(qty "Ice Wall" 1)])
             (default-challenger [(qty "Blackguard" 1)
                              (qty "Snitch" 1)]))
   (play-from-hand state :contestant "Ice Wall" "Archives")
   (take-credits state :contestant)
   (core/gain state :challenger :credit 100)
   (play-from-hand state :challenger "Blackguard")
   (is (= 6 (:memory (get-challenger))) "Challenger has 6 MU")
   (play-from-hand state :challenger "Snitch")
   (let [snitch (get-in @state [:challenger :rig :program 0])
         iwall (get-character state :archives 0)]
     (run-on state :archives)
     (card-ability state :challenger snitch 0)
     (is (:rezzed (refresh iwall)) "Ice Wall was rezzed"))))

(deftest brain-chip
  ;; Brain Chip handsize and memory limit
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Brain Chip" 1)]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Brain Chip")
   (swap! state assoc-in [:challenger :agenda-point] -2) ; hard set ap
   (is (= (core/hand-size state :challenger) 5) "Hand size unaffected")
   (is (= (get-in @state [:challenger :memory]) 4) "Memory limit unaffected")
   (swap! state assoc-in [:challenger :agenda-point] 2)
   (is (= (core/hand-size state :challenger) 7) "Hand size increased by 2")
   (is (= (get-in @state [:challenger :memory]) 6) "Memory limit increased by 2")
   (core/move state :challenger (get-in @state [:challenger :rig :hardware 0]) :discard)
   (is (= (core/hand-size state :challenger) 5) "Hand size reset")
   (is (= (get-in @state [:challenger :memory]) 4) "Memory limit reset")))

(deftest clone-chip
  ;; Test clone chip usage- outside and during run
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Datasucker" 1) (qty "Clone Chip" 2)]))
    (take-credits state :contestant)
    (trash-from-hand state :challenger "Datasucker")
    (play-from-hand state :challenger "Clone Chip")
    (let [chip (get-in @state [:challenger :rig :hardware 0])]
      (card-ability state :challenger chip 0)
      (prompt-select :challenger (find-card "Datasucker" (:discard (get-challenger))))
      (let [ds (get-in @state [:challenger :rig :program 0])]
        (is (not (nil? ds)))
        (is (= (:title ds) "Datasucker"))))))

(deftest clone-chip-dont-install-choices-challenger-cant-afford
  ;; Test clone chip usage - dont show inavalid choices
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Inti" 1) (qty "Magnum Opus" 1) (qty "Clone Chip" 1)]))
    (take-credits state :contestant)
    (trash-from-hand state :challenger "Inti")
    (trash-from-hand state :challenger "Magnum Opus")
    (play-from-hand state :challenger "Clone Chip")
    (is (= (get-in @state [:challenger :click]) 3) "Challenger has 3 clicks left")
    (let [chip (get-in @state [:challenger :rig :hardware 0])]
      (card-ability state :challenger chip 0)
      (prompt-select :challenger (find-card "Magnum Opus" (:discard (get-challenger))))
      (is (nil? (get-in @state [:challenger :rig :program 0])) "No program was installed"))
    (let [chip (get-in @state [:challenger :rig :hardware 0])]
      (is (not (nil? chip)) "Clone Chip is still installed")
      (is (= (get-in @state [:challenger :click]) 3) "Challenger has 3 clicks left")
      (card-ability state :challenger chip 0)
      (prompt-select :challenger (find-card "Inti" (:discard (get-challenger))))
      (let [inti (get-in @state [:challenger :rig :program 0])]
        (is (not (nil? inti)) "Program was installed")
        (is (= (:title inti) "Inti") "Program is Inti")
        (is (= (get-in @state [:challenger :click]) 3) "Challenger has 3 clicks left")))))

(deftest comet-event-play
  ;; Comet - Play event without spending a click after first event played
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Comet" 3) (qty "Easy Mark" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Comet")
    (let [comet (get-in @state [:challenger :rig :hardware 0])]
      (play-from-hand state :challenger "Easy Mark")
      (is (= true (:comet-event (core/get-card state comet)))) ; Comet ability enabled
      (card-ability state :challenger comet 0)
      (is (= (:cid comet) (-> @state :challenger :prompt first :card :cid)))
      (prompt-select :challenger (find-card "Easy Mark" (:hand (get-challenger))))
      (is (= 7 (:credit (get-challenger))))
      (is (= 2 (:click (get-challenger))))
      (is (nil? (:comet-event (core/get-card state comet))) "Comet ability disabled"))))

(deftest cortez-chip
  ;; Cortez Chip - Trash to add 2 credits to rez cost of an Character until end of turn
  (do-game
    (new-game (default-contestant [(qty "Quandary" 1)])
              (default-challenger [(qty "Cortez Chip" 1)]))
    (play-from-hand state :contestant "Quandary" "R&D")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Cortez Chip")
    (let [quan (get-character state :rd 0)
          cortez (get-hardware state 0)]
      (card-ability state :challenger cortez 0)
      (prompt-select :challenger quan)
      (is (= 1 (count (:discard (get-challenger)))) "Cortez Chip trashed")
      (core/rez state :contestant quan)
      (is (= 4 (:credit (get-contestant))) "Paid 3c instead of 1c to rez Quandary"))))

(deftest cybersolutions-mem-chip
  ;; CyberSolutions Mem Chip- Gain 2 memory
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "CyberSolutions Mem Chip" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "CyberSolutions Mem Chip")
    (is (= 6 (:memory (get-challenger))) "Gain 2 memory")))

(deftest desperado
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Desperado" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Desperado")
    (run-empty-server state :archives)
    (is (= 5 (:memory (get-challenger))) "Gain 1 memory")
    (is (= 3 (:credit (get-challenger))) "Got 1c for successful run on Desperado")))

(deftest dinosaurus-strength-boost-mu-savings
  ;; Dinosaurus - Boost strength of hosted icebreaker; keep MU the same when hosting or trashing hosted breaker
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Dinosaurus" 1) (qty "Battering Ram" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 5)
    (play-from-hand state :challenger "Dinosaurus")
    (let [dino (get-in @state [:challenger :rig :hardware 0])]
      (card-ability state :challenger dino 0)
      (prompt-select :challenger (find-card "Battering Ram" (:hand (get-challenger))))
      (is (= 2 (:click (get-challenger))))
      (is (= 0 (:credit (get-challenger))))
      (is (= 4 (:memory (get-challenger))) "Battering Ram 2 MU not deducted from available MU")
      (let [ram (first (:hosted (refresh dino)))]
        (is (= 5 (:current-strength (refresh ram)))
            "Dinosaurus giving +2 strength to Battering Ram")
        ;; Trash Battering Ram
        (core/move state :challenger (find-card "Battering Ram" (:hosted (refresh dino))) :discard)
        (is (= 4 (:memory (get-challenger))) "Battering Ram 2 MU not added to available MU")))))

(deftest doppelganger
  ;; Doppelgänger - run again when successful
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Doppelgänger" 1)]))
    (core/gain state :contestant :bad-publicity 1)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Doppelgänger")
    (run-empty-server state :hq)
    (prompt-choice :challenger "OK")
    (is (= 0 (:run-credit (get-challenger))) "Challenger lost BP credits")
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger "R&D")
    (is (:run @state) "New run started")
    (is (= [:rd] (:server (:run @state))) "Running on R&D")
    (is (= 1 (:run-credit (get-challenger))) "Challenger has 1 BP credit")))

(deftest dorm-computer
  ;; make a run and avoid all tags for the remainder of the run
  (do-game
    (new-game (default-contestant [(qty "Snare!" 1)])
              (default-challenger [(qty "Dorm Computer" 1)]))
    (play-from-hand state :contestant "Snare!" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Dorm Computer")
    (let [dorm (get-in @state [:challenger :rig :hardware 0])]
      (card-ability state :challenger dorm 0)
      (prompt-choice :challenger "Server 1")
      (run-empty-server state "Server 1")
      (is (:run @state) "New run started")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
          "Challenger has prompt to wait for Snare!")
      (prompt-choice :contestant "Yes")
      (is (= 0 (:tag (get-challenger))) "Challenger has 0 tags")
      (is (= 3 (get-counters (refresh dorm) :power))))
      ))

(deftest feedback-filter
  ;; Feedback Filter - Prevent net and brain damage
  (do-game
    (new-game (default-contestant [(qty "Data Mine" 1)
                             (qty "Cerebral Overwriter" 1)
                             (qty "Mushin No Shin" 1)])
              (default-challenger [(qty "Feedback Filter" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Mushin No Shin")
    (prompt-select :contestant (find-card "Cerebral Overwriter" (:hand (get-contestant))))
    (play-from-hand state :contestant "Data Mine" "Server 1")
    (let [co (get-content state :remote1 0)
          dm (get-character state :remote1 0)]
      (is (= 3 (:advance-counter (refresh co))) "3 advancements on Overwriter")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sure Gamble")
      (play-from-hand state :challenger "Feedback Filter")
      (is (= 7 (:credit (get-challenger))))
      (let [ff (get-in @state [:challenger :rig :hardware 0])]
        (run-on state "Server 1")
        (core/rez state :contestant dm)
        (card-subroutine state :contestant dm 0)
        (card-ability state :challenger ff 0)
        (prompt-choice :challenger "Done")
        (is (= 3 (count (:hand (get-challenger)))) "1 net damage prevented")
        (is (= 4 (:credit (get-challenger))))
        (run-successful state)
        (prompt-choice :contestant "Yes") ; pay 3 to fire Overwriter
        (card-ability state :challenger ff 1)
        (prompt-choice :challenger "Done")
        (prompt-choice :challenger "Yes") ; trash Overwriter for 0
        (is (= 1 (:brain-damage (get-challenger))) "2 of the 3 brain damage prevented")
        (is (= 2 (count (:hand (get-challenger)))))
        (is (empty? (get-in @state [:challenger :rig :hardware])) "Feedback Filter trashed")))))

(deftest grimoire
  ;; Grimoire - Gain 2 MU, add a free virus counter to installed virus programs
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Grimoire" 1) (qty "Imp" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Grimoire")
    (is (= 6 (:memory (get-challenger))) "Gained 2 MU")
    (play-from-hand state :challenger "Imp")
    (let [imp (get-in @state [:challenger :rig :program 0])]
      (is (= 3 (get-counters (refresh imp) :virus)) "Imp received an extra virus counter on install"))))

(deftest llds-processor
  ;; LLDS Processor - Add 1 strength until end of turn to an icebreaker upon install
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "LLDS Processor" 2) (qty "Inti" 1) (qty "Passport" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "LLDS Processor")
    (play-from-hand state :challenger "Inti")
    (play-from-hand state :challenger "LLDS Processor")
    (play-from-hand state :challenger "Passport")
    (let [inti (get-in @state [:challenger :rig :program 0])
          pass (get-in @state [:challenger :rig :program 1])]
      (is (= 2 (:current-strength (refresh inti))) "Strength boosted by 1; 1 copy of LLDS when installed")
      (is (= 4 (:current-strength (refresh pass))) "Strength boosted by 2; 2 copies of LLDS when installed")
      (take-credits state :challenger)
      (is (= 1 (:current-strength (refresh inti))) "Strength reduced to default")
      (is (= 2 (:current-strength (refresh pass))) "Strength reduced to default"))))

(deftest maw
  ;; Once per turn, first time challenger declines to steal or trash, trash a HQ card at random
  (do-game
    (new-game (default-contestant [(qty "BOOM!" 5)])
              (default-challenger [(qty "Maw" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 20)
    (run-empty-server state :hq)
    (prompt-choice :challenger "No")
    (is (= 0 (count (:discard (get-contestant)))) "No HQ card in discard before Maw installed")
    (play-from-hand state :challenger "Maw")
    (run-empty-server state :hq)
    (prompt-choice :challenger "No")
    (is (= 0 (count (:discard (get-contestant)))) "HQ card not trashed by Maw as first decline already happened")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (run-empty-server state :hq)
    (prompt-choice :challenger "No")
    (is (= 1 (count (:discard (get-contestant)))) "HQ card trashed by Maw")
    (run-empty-server state :hq)
    (prompt-choice :challenger "No")
    (is (= 1 (count (:discard (get-contestant)))) "2nd HQ card on same turn not trashed by Maw")))

(deftest maw-card-seen
  ;; Check trashed card is trashed face-up if it's the card that is accessed, issue #2695
  ;; Also checks Maw auto-trashes on Operation with no trash cost
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 1)])
              (default-challenger [(qty "Maw" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 20)
    (play-from-hand state :challenger "Maw")
    (run-empty-server state :hq)
    ;; (is (= 0 (count (:discard (get-contestant)))) "HQ card not trashed by Maw yet")
    (prompt-choice :challenger "OK")
    (is (= 1 (count (:discard (get-contestant)))) "HQ card trashed by Maw now")
    (is (:seen (first (:discard (get-contestant)))) "Trashed card is registered as seen since it was accessed")))

(deftest maw-hiro
  ;; Maw with Hiro in hand - Hiro not moved to challenger scored area on trash decline #2638
  (do-game
    (new-game (default-contestant [(qty "Chairman Hiro" 1)])
              (default-challenger [(qty "Maw" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 20)
    (play-from-hand state :challenger "Maw")
    (run-empty-server state :hq)
    (prompt-choice :challenger "No")
    (is (= 0 (count (:scored (get-contestant)))) "Hiro not scored")
    (is (= 1 (count (:discard (get-contestant)))) "Hiro trashed by Maw")))

(deftest maya
  ;; Maya - Move accessed card to bottom of R&D
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
              (default-challenger [(qty "Maya" 1) (qty "Sure Gamble" 3)]))
    (core/move state :contestant (find-card "Scorched Earth" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Snare!" (:hand (get-contestant))) :deck)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Maya")
    (let [maya (get-in @state [:challenger :rig :hardware 0])
          accessed (first (:deck (get-contestant)))]
      (run-empty-server state :rd)
      (is (= (:cid accessed) (:cid (:card (first (:prompt (get-challenger)))))) "Accessing the top card of R&D")
      (card-ability state :challenger maya 0)
      (is (empty? (:prompt (get-challenger))) "No more prompts for challenger")
      (is (not (:run @state)) "Run is ended")
      (is (= (:cid accessed) (:cid (last (:deck (get-contestant))))) "Maya moved the accessed card to the bottom of R&D")
      (take-credits state :challenger)
      (core/draw state :contestant)
      (take-credits state :contestant)
      (core/move state :contestant (find-card "Snare!" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Scorched Earth" (:hand (get-contestant))) :deck)
      (let [accessed (first (:deck (get-contestant)))]
        (run-empty-server state :rd)
        (prompt-choice :contestant "Yes")
        (is (= 0 (count (:hand (get-challenger)))) "Challenger took Snare! net damage")
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-challenger)))))) "Accessing the top card of R&D")
        (card-ability state :challenger maya 0)
        (is (empty? (:prompt (get-challenger))) "No more prompts for challenger")
        (is (not (:run @state)) "Run is ended")
        (is (= (:cid accessed) (:cid (last (:deck (get-contestant))))) "Maya moved the accessed card to the bottom of R&D")))))

(deftest maya-multi-access
  ;; Maya - Does not interrupt multi-access.
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
              (default-challenger [(qty "Maya" 1) (qty "Sure Gamble" 3) (qty "R&D Interface" 1)]))
    (core/move state :contestant (find-card "Scorched Earth" (:hand (get-contestant))) :deck)
    (core/move state :contestant (find-card "Snare!" (:hand (get-contestant))) :deck)
    (take-credits state :contestant)
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Maya")
    (play-from-hand state :challenger "R&D Interface")
    (let [maya (get-in @state [:challenger :rig :hardware 0])
          accessed (first (:deck (get-contestant)))]
      (run-empty-server state :rd)
      (prompt-choice :challenger "Card from deck")
      (is (= (:cid accessed) (:cid (:card (first (:prompt (get-challenger)))))) "Accessing the top card of R&D")
      (card-ability state :challenger maya 0)
      (is (= (:cid accessed) (:cid (last (:deck (get-contestant))))) "Maya moved the accessed card to the bottom of R&D")
      (is (:prompt (get-challenger)) "Challenger has next access prompt"))))

(deftest obelus
  ;; Obelus - Increase max hand size with tags, draw cards on first successful HQ/R&D run
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Obelus" 1) (qty "Nerve Agent" 1)
                               (qty "Sure Gamble" 3) (qty "Cache" 3)]))
    (take-credits state :contestant)
    (starting-hand state :challenger ["Obelus" "Nerve Agent"])
    (core/gain state :challenger :credit 10 :click 3)
    (play-from-hand state :challenger "Nerve Agent")
    (let [nerve (get-in @state [:challenger :rig :program 0])]
      (run-empty-server state :hq)
      (is (= 1 (get-counters (refresh nerve) :virus)) "1 virus counter on Nerve Agent")
      (prompt-choice :challenger "OK")
      (play-from-hand state :challenger "Obelus")
      (core/gain state :challenger :tag 1)
      (is (= 6 (core/hand-size state :challenger)) "Max hand size is 6")
      (core/lose state :challenger :tag 1)
      (is (= 5 (core/hand-size state :challenger)) "Max hand size is 5")
      (run-empty-server state :hq)
      (is (= 2 (get-counters (refresh nerve) :virus)) "2 virus counters on Nerve Agent")
      (prompt-choice :challenger 1)
      (prompt-choice :challenger "Card from hand")
      (prompt-choice :challenger "OK")
      (prompt-choice :challenger "Card from hand")
      (prompt-choice :challenger "OK")
      (is (empty? (:hand (get-challenger))) "No cards drawn by Obelus, already had successful HQ run")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-empty-server state :hq)
      (is (= 3 (get-counters (refresh nerve) :virus)) "3 virus counters on Nerve Agent")
      (prompt-choice :challenger 2)
      (prompt-choice :challenger "Card from hand")
      (prompt-choice :challenger "OK")
      (prompt-choice :challenger "Card from hand")
      (prompt-choice :challenger "OK")
      (prompt-choice :challenger "Card from hand")
      (prompt-choice :challenger "OK")
      (is (= 3 (count (:hand (get-challenger)))) "Obelus drew 3 cards"))))

(deftest obelus-hades-shard
  ;; Obelus - using Hades Shard during run to increase draw
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 3) (qty "Restructure" 3)])
              (default-challenger [(qty "Obelus" 1) (qty "Hades Shard" 1)
                               (qty "Sure Gamble" 3) (qty "Cache" 3)]))
    (starting-hand state :contestant ["Hedge Fund" "Hedge Fund"])
    (trash-from-hand state :contestant "Hedge Fund")
    (trash-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (starting-hand state :challenger ["Obelus" "Hades Shard"])
    (core/gain state :challenger :credit 10)
    (play-from-hand state :challenger "Obelus")
    (play-from-hand state :challenger "Hades Shard")
    (run-empty-server state "R&D")
    (card-ability state :challenger (get-resource state 0) 0)
    (prompt-choice :challenger "OK")
    (is (= 3 (count (:hand (get-challenger)))) "Obelus drew 3 cards")))

(deftest plascrete
  ;; Plascrete Carapace - Prevent meat damage
  (do-game
    (new-game (default-contestant [(qty "Scorched Earth" 1)])
              (default-challenger [(qty "Plascrete Carapace" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Plascrete Carapace")
    (let [plas (get-in @state [:challenger :rig :hardware 0])]
      (is (= 4 (get-counters (refresh plas) :power)) "4 counters on install")
      (take-credits state :challenger)
      (core/gain state :challenger :tag 1)
      (play-from-hand state :contestant "Scorched Earth")
      (card-ability state :challenger plas 0)
      (card-ability state :challenger plas 0)
      (card-ability state :challenger plas 0)
      (card-ability state :challenger plas 0)
      (prompt-choice :challenger "Done")
      (is (= 1 (count (:hand (get-challenger)))) "All meat damage prevented")
      (is (empty? (get-in @state [:challenger :rig :hardware])) "Plascrete depleted and trashed"))))

(deftest rabbit-hole
  ;; Rabbit Hole - +1 link, optionally search Stack to install more copies
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Sure Gamble" 1) (qty "Rabbit Hole" 3)]))
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Rabbit Hole" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Rabbit Hole" (:hand (get-challenger))) :deck)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Rabbit Hole")
    (is (= 1 (:link (get-challenger))))
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger "Yes")
    (is (= 3 (:link (get-challenger))))
    (is (= 3 (count (get-in @state [:challenger :rig :hardware]))))
    (is (= 2 (:click (get-challenger))) "Clickless installs of extra 2 copies")
    (is (= 3 (:credit (get-challenger))) "Paid 2c for each of 3 copies")))

(deftest recon-drone
  ;; trash and pay X to prevent that much damage from a card you are accessing
  (do-game
    (new-game (default-contestant [(qty "Snare!" 1) (qty "House of Knives" 1)
                             (qty "Prisec" 1) (qty "Cerebral Overwriter" 1)])
              (default-challenger [(qty "Recon Drone" 10)]))
    (core/gain state :contestant :click 10)
    (core/gain state :contestant :credit 100)
    (play-from-hand state :contestant "House of Knives" "New remote")
    (play-from-hand state :contestant "Snare!" "New remote")
    (play-from-hand state :contestant "Prisec" "New remote")
    (play-from-hand state :contestant "Cerebral Overwriter" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (core/advance state :contestant (get-content state :remote4 0))
    (take-credits state :contestant)
    (core/gain state :challenger :click 100)
    (core/gain state :challenger :credit 100)
    (core/draw state :challenger)
    (core/draw state :challenger)
    (core/draw state :challenger)
    (core/draw state :challenger)
    (play-from-hand state :challenger "Recon Drone")
    (play-from-hand state :challenger "Recon Drone")
    (play-from-hand state :challenger "Recon Drone")
    (play-from-hand state :challenger "Recon Drone")
    (let [rd1 (get-in @state [:challenger :rig :hardware 0])
          rd2 (get-in @state [:challenger :rig :hardware 1])
          rd3 (get-in @state [:challenger :rig :hardware 2])
          rd4 (get-in @state [:challenger :rig :hardware 3])
          hok (get-in @state [:contestant :scored 0])]
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
        "Challenger has prompt to wait for Snare!")
      (prompt-choice :contestant "Yes")
      (card-ability state :challenger rd1 0)
      (prompt-choice :challenger 3)
      (prompt-choice :challenger "Done")
      (is (= 5 (count (:hand (get-challenger)))) "Challenger took no net damage")
      ; fire HOK while accessing Snare!
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
          "Challenger has prompt to wait for Snare!")
      (card-ability state :contestant hok 0)
      ; Recon Drone ability won't fire as we are not accessing HOK
      (card-ability state :challenger rd2 0)
      (is (nil? (:number (:choices (first (:prompt (get-challenger)))))) "No choice to prevent damage from HOK")
      (prompt-choice :challenger "Done")
      (is (= 4 (count (:hand (get-challenger)))) "Challenger took 1 net damage from HOK")
      (prompt-choice :contestant "No")
      (core/lose state :challenger :credit 100)
      ; can only stop 1 damage due to credits
      (core/gain state :challenger :credit 1)
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
          "Challenger has prompt to wait for Snare!")
      (prompt-choice :contestant "Yes")
      (card-ability state :challenger rd2 0)
      (is (= 1 (:number (:choices (first (:prompt (get-challenger)))))) "Recon Drone choice limited to challenger credits")
      (prompt-choice :challenger 1)
      (prompt-choice :challenger "Done")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger took 2 net damage from Snare!")
      (core/gain state :challenger :credit 100)
      (run-empty-server state "Server 3")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
          "Challenger has prompt to wait for Prisec")
      (prompt-choice :contestant "Yes")
      (card-ability state :challenger rd3 0)
      (is (= 1 (:number (:choices (first (:prompt (get-challenger)))))) "Recon Drone choice limited to 1 meat")
      (prompt-choice :challenger 1)
      (prompt-choice :challenger "Done")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger took no meat damage")
      (run-empty-server state "Server 4")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
          "Challenger has prompt to wait for Cerebral Overwriter")
      (prompt-choice :contestant "Yes")
      (card-ability state :challenger rd4 0)
      (prompt-choice :challenger 1)
      (prompt-choice :challenger "Done")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger took no brain damage"))))

(deftest ramujan-reliant
  ;; Prevent up to X net or brain damage.
  (do-game
    (new-game (default-contestant [(qty "Data Mine" 1)
                             (qty "Snare!" 1)])
              (default-challenger [(qty "Ramujan-reliant 550 BMI" 4) (qty "Sure Gamble" 6)]))
    (starting-hand state :challenger
                   ["Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Sure Gamble"])
    (play-from-hand state :contestant "Data Mine" "Server 1")
    (play-from-hand state :contestant "Snare!" "Server 1")
    (let [sn (get-content state :remote1 0)
          dm (get-character state :remote1 0)]
      (take-credits state :contestant)
      (play-from-hand state :challenger "Ramujan-reliant 550 BMI")
      (play-from-hand state :challenger "Ramujan-reliant 550 BMI")
      (play-from-hand state :challenger "Ramujan-reliant 550 BMI")
      (let [rr1 (get-in @state [:challenger :rig :hardware 0])
            rr2 (get-in @state [:challenger :rig :hardware 1])
            rr3 (get-in @state [:challenger :rig :hardware 2])]
        (run-on state "Server 1")
        (core/rez state :contestant dm)
        (card-subroutine state :contestant dm 0)
        (card-ability state :challenger rr1 0)
        (prompt-choice :challenger 1)
        (is (last-log-contains? state "Sure Gamble")
            "Ramujan did log trashed card names")
        (is (= 2 (count (:hand (get-challenger)))) "1 net damage prevented")
        (run-successful state)
        (take-credits state :challenger)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Ramujan-reliant 550 BMI")
        (run-empty-server state "Server 1")
        (prompt-choice :contestant "Yes")
        (card-ability state :challenger rr2 0)
        (prompt-choice :challenger 3)
        (is (last-log-contains? state "Sure Gamble, Sure Gamble, Sure Gamble")
            "Ramujan did log trashed card names")
        (is (= 1 (count (:hand (get-challenger)))) "3 net damage prevented")))))

(deftest ramujan-reliant-empty
  ;; Prevent up to X net or brain damage. Empty stack
  (do-game
    (new-game (default-contestant [(qty "Data Mine" 1)])
              (default-challenger [(qty "Ramujan-reliant 550 BMI" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :contestant "Data Mine" "Server 1")
    (let [dm (get-character state :remote1 0)]
      (take-credits state :contestant)
      (play-from-hand state :challenger "Ramujan-reliant 550 BMI")
      (let [rr1 (get-in @state [:challenger :rig :hardware 0])]
        (run-on state "Server 1")
        (core/rez state :contestant dm)
        (card-subroutine state :contestant dm 0)
        (card-ability state :challenger rr1 0)
        (prompt-choice :challenger 1)
        (is (= 0 (count (:hand (get-challenger)))) "Not enough cards in Stack for Ramujan to work")))))

(deftest replicator-bazaar
  ;; Replicator - interaction with Bazaar. Issue #1511.
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Replicator" 1) (qty "Bazaar" 1) (qty "Spy Camera" 6)]))
    (letfn [(count-spy [n] (= n (count (filter #(= "Spy Camera" (:title %)) (-> (get-challenger) :rig :hardware)))))]
      (take-credits state :contestant)
      (starting-hand state :challenger ["Replicator" "Bazaar" "Spy Camera"])
      (play-from-hand state :challenger "Replicator")
      (play-from-hand state :challenger "Bazaar")
      (play-from-hand state :challenger "Spy Camera") ; 1 installed
      (is (count-spy 1) "1 Spy Cameras installed")
      (prompt-choice :challenger "Yes") ; for now, choosing Replicator then shows its optional Yes/No
      (prompt-choice :challenger "Yes") ; Bazaar triggers, 2 installed
      (is (count-spy 2) "2 Spy Cameras installed")
      (prompt-choice :challenger "Yes")
      (prompt-choice :challenger "Yes")  ; 3 installed
      (is (count-spy 3) "3 Spy Cameras installed")

      (prompt-choice :challenger "Yes")
      (prompt-choice :challenger "Yes")  ; 4 installed
      (is (count-spy 4) "4 Spy Cameras installed")

      (prompt-choice :challenger "Yes")
      (prompt-choice :challenger "Yes")  ; 5 installed
      (is (count-spy 5) "5 Spy Cameras installed")

      (prompt-choice :challenger "Yes")
      (prompt-choice :challenger "Yes")  ; 6 installed
      (is (count-spy 6) "6 Spy Cameras installed"))))

(deftest respirocytes-multiple
  ;; Should draw multiple cards when multiple respirocytes are in play
  (do-game
   (new-game (default-contestant)
             (default-challenger [(qty "Respirocytes" 3) (qty "Sure Gamble" 3)]))
   (take-credits state :contestant)
   (starting-hand state :challenger ["Respirocytes" "Respirocytes" "Respirocytes" "Sure Gamble"])
   (dotimes [_ 2]
     (play-from-hand state :challenger "Respirocytes"))
   (is (= 2 (count (:discard (get-challenger)))) "2 damage done")
   (is (= 2 (count (:hand (get-challenger)))) "Drew 2 cards")))

(deftest sifr
  ;; Once per turn drop encountered Character to zero strenght
  ;; Also handle archangel then re-install sifr should not break the game #2576
  (do-game
    (new-game (default-contestant [(qty "Archangel" 1) (qty "IP Block" 1) (qty "Hedge Fund" 1)])
              (default-challenger [(qty "Modded" 1) (qty "Clone Chip" 1) (qty "Şifr" 1) (qty "Parasite" 1)]))
    (core/gain state :contestant :credit 100)
    (core/gain state :challenger :credit 100)
    (play-from-hand state :contestant "Archangel" "HQ")
    (play-from-hand state :contestant "IP Block" "HQ")
    (take-credits state :contestant)
    (trash-from-hand state :challenger "Parasite")
    (play-from-hand state :challenger "Şifr")
    (is (= 2 (count (:hand (get-challenger)))) "Modded and Clone Chip in hand")
    (let [arch (get-character state :hq 0)
          ip (get-character state :hq 1)
          sifr (get-hardware state 0)]
      (core/rez state :contestant arch)
      (core/rez state :contestant ip)
      (is (= 4 (:current-strength (refresh ip))))
      (run-on state :hq)
      (is (= 2 (:position (:run @state))))
      (card-ability state :challenger sifr 0)
      (is (= 0 (:current-strength (refresh ip))))
      (run-continue state)
      (is (= 1 (:position (:run @state))))
      (is (= 2 (count (:hand (get-challenger))))) ; pre archangel
      (card-subroutine state :contestant arch 0) ; fire archangel
      (is (not (empty? (:prompt (get-contestant)))) "Archangel trace prompt - contestant")
      (prompt-choice :contestant 0)
      (is (not (empty? (:prompt (get-challenger)))) "Archangel trace prompt - challenger")
      (prompt-choice :challenger 0)
      (prompt-select :contestant sifr)
      (is (= 3 (count (:hand (get-challenger))))) ; sifr got lifted to hand
      (run-jack-out state)
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :challenger "Modded")
      (is (not (empty? (:prompt (get-challenger)))) "Modded choice prompt exists")
      (prompt-select :challenger (find-card "Şifr" (:hand (get-challenger))))
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :challenger "Clone Chip")
      (take-credits state :challenger)
      (take-credits state :contestant 4)
      (let [chip (get-hardware state 1)]
        (is (nil? (:sifr-target (refresh sifr))) "Sifr cleaned up on leave play")
        (is (= 0 (count (:discard (get-contestant)))) "No Contestant cards trashed")
        (card-ability state :challenger chip 0)
        (prompt-select :challenger (find-card "Parasite" (:discard (get-challenger))))
        (let [para (get-program state 0)]
          (prompt-select :challenger ip)
          (is (= 0 (count (:discard (get-contestant)))) "IP Block Not Trashed")
          (is (= 1 (count (:hosted (refresh ip)))) "Parasite is hosted"))))))

(deftest spinal-modem
  ;; Spinal Modem - +1 MU, 2 recurring credits, take 1 brain damage on successful trace during run
  (do-game
    (new-game (default-contestant [(qty "Caduceus" 1)])
              (default-challenger [(qty "Spinal Modem" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :contestant "Caduceus" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Spinal Modem")
    (let [cad (get-character state :hq 0)
          sm (get-hardware state 0)]
      (is (= 5 (:memory (get-challenger))))
      (is (= 2 (:rec-counter (refresh sm))))
      (run-on state :hq)
      (core/rez state :contestant cad)
      (card-subroutine state :contestant cad 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 1 (:brain-damage (get-challenger))) "Took 1 brain damage")
      (is (= 1 (count (:discard (get-challenger)))))
      (is (= 4 (core/hand-size state :challenger)) "Reduced hand size"))))

(deftest spy-camera
  ;; Spy Camera - Full test
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Spy Camera" 6) (qty "Sure Gamble" 1) (qty "Desperado" 1)
                               (qty "Diesel" 1) (qty "Corroder" 1) (qty "Patron" 1) (qty "Kati Jones" 1)]))
    (starting-hand state :challenger ["Spy Camera" "Spy Camera" "Spy Camera"
                                  "Spy Camera" "Spy Camera" "Spy Camera"])
    (is (= 6 (count (:hand (get-challenger)))))
    (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
    (take-credits state :contestant)
    (core/gain state :challenger :click 3)
    (dotimes [_ 6] (play-from-hand state :challenger "Spy Camera"))
    (let [spy (get-hardware state 5)]
      ;; look at top 6 cards
      (card-ability state :challenger spy 0)
      (prompt-choice :challenger (find-card "Sure Gamble" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Desperado" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Diesel" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Corroder" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Patron" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Kati Jones" (:deck (get-challenger))))
      ;; try starting over
      (prompt-choice :challenger "Start over")
      (prompt-choice :challenger (find-card "Kati Jones" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Patron" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Corroder" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Diesel" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Desperado" (:deck (get-challenger))))
      (prompt-choice :challenger (find-card "Sure Gamble" (:deck (get-challenger)))) ;this is the top card on stack
      (prompt-choice :challenger "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-challenger))))))
      (is (= "Desperado" (:title (second (:deck (get-challenger))))))
      (is (= "Diesel" (:title (second (rest (:deck (get-challenger)))))))
      (is (= "Corroder" (:title (second (rest (rest (:deck (get-challenger))))))))
      (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-challenger)))))))))
      (is (= "Kati Jones" (:title (second (rest (rest (rest (rest (:deck (get-challenger))))))))))
      ;; look at top card of R&D
      (card-ability state :challenger spy 1)
      (let [topcard (get-in (first (get-in @state [:challenger :prompt])) [:msg])]
        (is (= "The top card of R&D is Hedge Fund" topcard)))
      (is (= 1 (count (:discard (get-challenger))))))))

(deftest the-gauntlet-not-with-gang-sign
  ;; Access additional cards on run on HQ, not with Gang Sign
  ;; Issue #2749
  (do-game
    (new-game (default-contestant [(qty "Hostile Takeover" 1)
                             (qty "Hedge Fund" 3)])
              (default-challenger [(qty "The Gauntlet" 1)
                               (qty "Gang Sign" 1)]))
    (take-credits state :contestant)
    (core/gain state :challenger :credit 5)
    (play-from-hand state :challenger "Gang Sign")
    (play-from-hand state :challenger "The Gauntlet")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hostile Takeover" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    ;; Gang Sign should trigger, without The Gauntlet pop-up
    (let [gs (get-resource state 0)]
      (prompt-is-card? :challenger gs))
    ;; This will throw error if The Gauntlet triggers.
    (prompt-choice :challenger "Card from hand")))

(deftest the-personal-touch
  ;; The Personal Touch - Give +1 strength to an icebreaker
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "The Personal Touch" 1)
                               (qty "Paricia" 1)
                               (qty "Faerie" 1)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Paricia")
    (play-from-hand state :challenger "Faerie")
    (let [par (get-in @state [:challenger :rig :program 0])
          fae (get-in @state [:challenger :rig :program 1])]
      (is (= 2 (:current-strength (refresh fae))))
      (play-from-hand state :challenger "The Personal Touch")
      (prompt-select :challenger par)
      (is (nil? (:hosted (refresh par))) "TPT can't be hosted on a non-icebreaker")
      (prompt-select :challenger fae)
      (is (= 1 (count (:hosted (refresh fae)))) "TPT hosted on Faerie")
      (is (= 3 (:current-strength (refresh fae))) "Faerie receiving +1 strength from TPT"))))

(deftest titanium-ribs
  ;; Titanium Ribs - Choose cards lost to damage, but not on Contestant turn against Chronos Protocol
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1) (qty "Viktor 1.0" 1)
                                                                     (qty "Neural EMP" 1)])
              (default-challenger [(qty "Titanium Ribs" 2) (qty "Sure Gamble" 1)
                               (qty "Fall Guy" 1) (qty "Kati Jones" 1)]))
    (play-from-hand state :contestant "Pup" "HQ")
    (play-from-hand state :contestant "Viktor 1.0" "R&D")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Fall Guy")
    (play-from-hand state :challenger "Titanium Ribs")
    (prompt-select :challenger (find-card "Titanium Ribs" (:hand (get-challenger))))
    (prompt-select :challenger (find-card "Kati Jones" (:hand (get-challenger))))
    (is (empty? (:prompt (get-challenger))) "Fall Guy didn't try to prevent trashing of Kati")
    (is (= 2 (count (:discard (get-challenger)))) "2 cards trashed for Ribs installation meat damage")
    (run-on state "HQ")
    (let [pup (get-character state :hq 0)]
      (core/rez state :contestant pup)
      (card-subroutine state :contestant pup 0)
      (prompt-select :challenger (find-card "Sure Gamble" (:hand (get-challenger)))) ; Ribs takes precedence over CP on Challenger turn
      (is (= 3 (count (:discard (get-challenger)))) "Chose card lost from 1 net damage")
      (run-jack-out state)
      (take-credits state :challenger)
      (core/move state :challenger (find-card "Sure Gamble" (:discard (get-challenger))) :hand)
      (core/move state :challenger (find-card "Kati Jones" (:discard (get-challenger))) :hand)
      (play-from-hand state :contestant "Neural EMP")
      (prompt-choice :contestant "Yes")
      (let [kati (find-card "Kati Jones" (:hand (get-challenger)))]
        (prompt-choice :contestant kati) ; Chronos Protocol takes precedence over Ribs on Contestant turn
        (is (= 2 (count (:discard (get-challenger)))) "Card chosen by Contestant for first net damage")))))

(deftest turntable-swap
  ;; Turntable - Swap a stolen agenda for a scored agenda
  (do-game
    (new-game (default-contestant [(qty "Domestic Sleepers" 1) (qty "Project Vitruvius" 1)])
              (default-challenger [(qty "Turntable" 1)]))
    (play-from-hand state :contestant "Project Vitruvius" "New remote")
    (let [ag1 (get-content state :remote1 0)]
      (score-agenda state :contestant ag1)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Turntable")
      (is (= 3 (:credit (get-challenger))))
      (let [tt (get-in @state [:challenger :rig :hardware 0])]
        (run-empty-server state "HQ")
        (prompt-choice :challenger "Steal")
        (is (= 0 (:agenda-point (get-challenger))) "Stole Domestic Sleepers")
        (is (prompt-is-card? :challenger tt))
        (prompt-choice :challenger "Yes")
        (prompt-select :challenger (find-card "Project Vitruvius" (:scored (get-contestant))))
        (is (= 2 (:agenda-point (get-challenger))) "Took Project Vitruvius from Contestant")
        (is (= 0 (:agenda-point (get-contestant))) "Swapped Domestic Sleepers to Contestant")))))

(deftest turntable-mandatory-upgrades
  ;; Turntable - Swap a Mandatory Upgrades away from the Contestant reduces Contestant clicks per turn
  ;;           - Contestant doesn't gain a click on the Challenger's turn when it receives a Mandatory Upgrades
  (do-game
    (new-game (default-contestant [(qty "Mandatory Upgrades" 2) (qty "Project Vitruvius" 1)])
              (default-challenger [(qty "Turntable" 1)]))
    (score-agenda state :contestant (find-card "Mandatory Upgrades" (:hand (get-contestant))))
    (is (= 4 (:click-per-turn (get-contestant))) "Up to 4 clicks per turn")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Turntable")
    (let [tt (get-in @state [:challenger :rig :hardware 0])]
      ;; steal Project Vitruvius and swap for Mandatory Upgrades
      (core/steal state :challenger (find-card "Project Vitruvius" (:hand (get-contestant))))
      (is (prompt-is-card? :challenger tt))
      (prompt-choice :challenger "Yes")
      (prompt-select :challenger (find-card "Mandatory Upgrades" (:scored (get-contestant))))
      (is (= 3 (:click-per-turn (get-contestant))) "Back down to 3 clicks per turn")
      ;; steal second Mandatory Upgrades and swap for Project Vitruvius
      (core/steal state :challenger (find-card "Mandatory Upgrades" (:hand (get-contestant))))
      (is (prompt-is-card? :challenger tt))
      (prompt-choice :challenger "Yes")
      (prompt-select :challenger (find-card "Project Vitruvius" (:scored (get-contestant))))
      (is (= 0 (:click (get-contestant))) "Contestant doesn't gain a click on Challenger's turn")
      (is (= 4 (:click-per-turn (get-contestant)))))))

(deftest vigil
  ;; Vigil - Draw 1 card when turn begins if Contestant HQ is filled to max hand size
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 3) (qty "PAD Campaign" 2)])
              (default-challenger [(qty "Vigil" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Vigil")
    (is (= 5 (:memory (get-challenger))))
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Sure Gamble" (:hand (get-challenger))) :deck)
    (is (empty? (:hand (get-challenger))))
    (take-credits state :challenger)
    (is (= (count (:hand (get-contestant))) (core/hand-size state :contestant)) "Contestant hand filled to max")
    (take-credits state :contestant)
    (is (= 1 (count (:hand (get-challenger)))) "Drew 1 card")
    (take-credits state :challenger)
    (play-from-hand state :contestant "Hedge Fund")
    (take-credits state :contestant)
    (is (not= (count (:hand (get-contestant))) (core/hand-size state :contestant)) "Contestant hand below max")
    (is (= 1 (count (:hand (get-challenger)))) "No card drawn")))

(deftest zamba
  ;; Zamba - Whenever contestant card is exposed you may gain 1 credit
  (do-game
   (new-game (default-contestant [(qty "Ice Wall" 1)])
             (default-challenger [(qty "Zamba" 1) (qty "Infiltration" 2)]))
   (play-from-hand state :contestant "Ice Wall" "Archives")
   (take-credits state :contestant)
   (play-from-hand state :challenger "Zamba")
   (is (= 6 (:memory (get-challenger))) "Gain 2 memory")
   (is (= 1 (:credit (get-challenger))) "At 1 credit")
   (play-from-hand state :challenger "Infiltration")
   (prompt-choice :challenger "Expose a card")
   (prompt-select :challenger (get-character state :archives 0))
   (is (= 2 (:credit (get-challenger))) "Gained 1 credit from exposing")
   (play-from-hand state :challenger "Infiltration")
   (prompt-choice :challenger "Expose a card")
   (prompt-select :challenger (get-character state :archives 0))
   (is (= 3 (:credit (get-challenger))) "Gained 1 more credit from exposing")))