(ns game-test.cards.hazard
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "hazard"))

(deftest acacia
  ;; Acacia - Optionally gain credits for number of virus tokens then discard
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Acacia" "Virus Breeding Ground" "Datasucker"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Acacia")
    (play-from-hand state :challenger "Virus Breeding Ground")
    (play-from-hand state :challenger "Datasucker")
    (core/add-counter state :challenger (get-radicle state 0) :virus 4)
    (core/add-counter state :challenger (get-resource state 0) :virus 3)
    (take-credits state :challenger)
    (is (= 2 (:credit (get-challenger))) "Challenger initial credits")
    (core/purge state :contestant)
    (prompt-choice :challenger "Yes")
    (is (= 9 (:credit (get-challenger))) "Challenger gained 9 credits")
    (is (= 1 (count (:discard (get-challenger)))) "Acacia has discarded")))

(deftest akamatsu-mem-chip
  ;; Akamatsu Mem Chip - Gain 1 memory
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Akamatsu Mem Chip" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Akamatsu Mem Chip")
    (is (= 5 (core/available-mu state)) "Gain 1 memory")))

(deftest archives-interface
  ;; Archives Interface - Remove 1 card in Archives from the game instead of accessing it
  (do-game
    (new-game (default-contestant ["Shock!" "Launch Campaign"])
              (default-challenger ["Archives Interface" "Imp"]))
    (take-credits state :contestant)
    (core/move state :contestant (find-card "Shock!" (:hand (get-contestant))) :discard)
    (core/move state :contestant (find-card "Launch Campaign" (:hand (get-contestant))) :discard)
    (play-from-hand state :challenger "Archives Interface")
    (run-empty-server state :archives)
    (prompt-choice :challenger "Yes")
    (prompt-card :challenger (find-card "Shock!" (:discard (get-contestant))))
    (is (= "Shock!" (:title (first (:rfg (get-contestant))))) "Shock! removed from game")
    (is (empty? (:discard (get-challenger))) "Didn't access Shock!, no net damage taken")))

(deftest astrolabe
  ;; Astrolabe - Draw on new server install
  (do-game
    (new-game (default-contestant [(qty "Snare!" 3)])
              (default-challenger [(qty "Astrolabe" 3) (qty "Sure Gamble" 3) "Cloak"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Astrolabe")
    (is (= 5 (core/available-mu state)) "Gain 1 memory")
    (take-credits state :challenger 3)
    ;; contestant's turn. install something from HQ to trigger Astrolabe draw
    (play-from-hand state :contestant "Snare!" "New remote")
    (is (= 5 (count (:hand (get-challenger)))) "Drew 1 card from server install")
    ;; install over the old server; make sure nothing is drawn
    (play-from-hand state :contestant "Snare!" "Server 0")
    (is (= 5 (count (:hand (get-challenger)))) "Did not draw")
    (is (= 1 (count (:deck (get-challenger)))) "1 card left in deck")))

(deftest blackguard
  ;; Blackguard - +2 MU, forced rez of exposed character
  (do-game
   (new-game (default-contestant ["Ice Wall"])
             (default-challenger ["Blackguard"
                              "Snitch"]))
   (play-from-hand state :contestant "Ice Wall" "Archives")
   (take-credits state :contestant)
   (core/gain state :challenger :credit 100)
   (play-from-hand state :challenger "Blackguard")
   (is (= 6 (core/available-mu state)) "Challenger has 6 MU")
   (play-from-hand state :challenger "Snitch")
   (let [snitch (get-resource state 0)
         iwall (get-character state :archives 0)]
     (run-on state :archives)
     (card-ability state :challenger snitch 0)
     (is (:rezzed (refresh iwall)) "Ice Wall was rezzed"))))

(deftest box-e
  ;; Box-E - +2 MU, +2 max hand size
  (do-game
   (new-game (default-contestant)
             (default-challenger ["Box-E"]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Box-E")
   (is (= 6 (core/available-mu state)))
   (is (= 7 (core/hand-size state :challenger)))))

(deftest brain-chip
  ;; Brain Chip handsize and memory limit
  (do-game
   (new-game (default-contestant)
             (default-challenger ["Brain Chip"]))
   (take-credits state :contestant)
   (play-from-hand state :challenger "Brain Chip")
   (swap! state assoc-in [:challenger :agenda-point] -2) ; hard set ap
   (is (= 5 (core/hand-size state :challenger)) "Hand size unaffected")
   (is (= 4 (core/available-mu state)) "Memory limit unaffected")
   (swap! state assoc-in [:challenger :agenda-point] 2)
   (is (= 7 (core/hand-size state :challenger)) "Hand size increased by 2")
   (is (= 6 (core/available-mu state)) "Memory limit increased by 2")
   (core/move state :challenger (get-hazard state 0) :discard)
   (is (= 5 (core/hand-size state :challenger)) "Hand size reset")
   (is (= 4 (core/available-mu state)) "Memory limit reset")))

(deftest clone-chip
  ;; Test clone chip usage- outside and during run
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Datasucker" (qty "Clone Chip" 2)]))
      (take-credits state :contestant)
      (discard-from-hand state :challenger "Datasucker")
      (play-from-hand state :challenger "Clone Chip")
      (let [chip (get-hazard state 0)]
        (card-ability state :challenger chip 0)
        (prompt-select :challenger (find-card "Datasucker" (:discard (get-challenger))))
        (let [ds (get-resource state 0)]
          (is (not (nil? ds)))
          (is (= (:title ds) "Datasucker"))))))
  (testing "don't show inavalid choices"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Inti" "Magnum Opus" "Clone Chip"]))
      (take-credits state :contestant)
      (discard-from-hand state :challenger "Inti")
      (discard-from-hand state :challenger "Magnum Opus")
      (play-from-hand state :challenger "Clone Chip")
      (is (= (get-in @state [:challenger :click]) 3) "Challenger has 3 clicks left")
      (let [chip (get-hazard state 0)]
        (card-ability state :challenger chip 0)
        (prompt-select :challenger (find-card "Magnum Opus" (:discard (get-challenger))))
        (is (nil? (get-resource state 0)) "No resource was installed"))
      (let [chip (get-hazard state 0)]
        (is (not (nil? chip)) "Clone Chip is still installed")
        (is (= (get-in @state [:challenger :click]) 3) "Challenger has 3 clicks left")
        (card-ability state :challenger chip 0)
        (prompt-select :challenger (find-card "Inti" (:discard (get-challenger))))
        (let [inti (get-resource state 0)]
          (is (not (nil? inti)) "Resource was installed")
          (is (= (:title inti) "Inti") "Resource is Inti")
          (is (= (get-in @state [:challenger :click]) 3) "Challenger has 3 clicks left"))))))

(deftest comet
  ;; Comet - Play event without spending a click after first event played
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Comet" 3) (qty "Easy Mark" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Comet")
    (let [comet (get-hazard state 0)]
      (play-from-hand state :challenger "Easy Mark")
      (is (= true (:comet-event (core/get-card state comet)))) ; Comet ability enabled
      (card-ability state :challenger comet 0)
      (is (= (:cid comet) (-> @state :challenger :prompt first :card :cid)))
      (prompt-select :challenger (find-card "Easy Mark" (:hand (get-challenger))))
      (is (= 7 (:credit (get-challenger))))
      (is (= 2 (:click (get-challenger))))
      (is (nil? (:comet-event (core/get-card state comet))) "Comet ability disabled"))))

(deftest cortez-chip
  ;; Cortez Chip - Discard to add 2 credits to rez cost of an Character until end of turn
  (do-game
    (new-game (default-contestant ["Quandary"])
              (default-challenger ["Cortez Chip"]))
    (play-from-hand state :contestant "Quandary" "R&D")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Cortez Chip")
    (let [quan (get-character state :rd 0)
          cortez (get-hazard state 0)]
      (card-ability state :challenger cortez 0)
      (prompt-select :challenger quan)
      (is (= 1 (count (:discard (get-challenger)))) "Cortez Chip discarded")
      (core/rez state :contestant quan)
      (is (= 4 (:credit (get-contestant))) "Paid 3c instead of 1c to rez Quandary"))))

(deftest cybersolutions-mem-chip
  ;; CyberSolutions Mem Chip- Gain 2 memory
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "CyberSolutions Mem Chip" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "CyberSolutions Mem Chip")
    (is (= 6 (core/available-mu state)) "Gain 2 memory")))

(deftest daredevil
  ;; Daredevil
  (do-game
    (new-game (default-contestant [(qty "Ice Wall" 2)])
              (default-challenger ["Daredevil" (qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
    (starting-hand state :challenger ["Daredevil"])
    (play-from-hand state :contestant "Ice Wall" "Archives")
    (play-from-hand state :contestant "Ice Wall" "Archives")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Daredevil")
    (is (= 6 (core/available-mu state)) "Gained 2 MU")
    (run-on state "HQ")
    (is (empty? (:hand (get-challenger))) "No cards drawn")
    (run-jack-out state)
    (run-on state "Archives")
    (is (= 2 (count (:hand (get-challenger)))) "Drew 2 cards")
    (run-jack-out state)
    (run-on state "Archives")
    (is (= 2 (count (:hand (get-challenger)))) "No cards drawn")))

(deftest desperado
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Desperado" 3)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Desperado")
    (run-empty-server state :archives)
    (is (= 5 (core/available-mu state)) "Gain 1 memory")
    (is (= 3 (:credit (get-challenger))) "Got 1c for successful run on Desperado")))

(deftest dinosaurus
  ;; Dinosaurus
  (testing "Hosting a breaker with strength based on unused MU should calculate correctly"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Adept" "Dinosaurus"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5)
      (play-from-hand state :challenger "Dinosaurus")
      (play-from-hand state :challenger "Adept")
      (is (= 2 (core/available-mu state)) "2 MU used")
      (let [dino (get-hazard state 0)
            adpt (get-resource state 0)]
        (is (= 4 (:current-strength (refresh adpt))) "Adept at 4 strength individually")
        (card-ability state :challenger dino 1)
        (prompt-select :challenger (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh dino)))]
          (is (= 4 (core/available-mu state)) "0 MU used")
          (is (= 8 (:current-strength (refresh hosted-adpt))) "Adept at 8 strength hosted")))))
  (testing "Boost strength of hosted characterbreaker; keep MU the same when hosting or discarding hosted breaker"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Dinosaurus" "Battering Ram"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5)
      (play-from-hand state :challenger "Dinosaurus")
      (let [dino (get-hazard state 0)]
        (card-ability state :challenger dino 0)
        (prompt-select :challenger (find-card "Battering Ram" (:hand (get-challenger))))
        (is (= 2 (:click (get-challenger))))
        (is (zero? (:credit (get-challenger))))
        (is (= 4 (core/available-mu state)) "Battering Ram 2 MU not deducted from available MU")
        (let [ram (first (:hosted (refresh dino)))]
          (is (= 5 (:current-strength (refresh ram)))
              "Dinosaurus giving +2 strength to Battering Ram")
          ;; Discard Battering Ram
          (core/move state :challenger (find-card "Battering Ram" (:hosted (refresh dino))) :discard)
          (is (= 4 (core/available-mu state))
              "Battering Ram 2 MU not added to available MU when Battering Ram was discarded"))))))

(deftest doppelganger
  ;; Doppelgänger - run again when successful
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Doppelgänger"]))
    (core/gain state :contestant :bad-publicity 1)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Doppelgänger")
    (run-empty-server state :hq)
    (prompt-choice :challenger "No action")
    (is (zero? (:run-credit (get-challenger))) "Challenger lost BP credits")
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger "R&D")
    (is (:run @state) "New run started")
    (is (= [:rd] (:server (:run @state))) "Running on R&D")
    (is (= 1 (:run-credit (get-challenger))) "Challenger has 1 BP credit")))

(deftest dorm-computer
  ;; make a run and avoid all tags for the remainder of the run
  (do-game
    (new-game (default-contestant ["Snare!"])
              (default-challenger ["Dorm Computer"]))
    (play-from-hand state :contestant "Snare!" "New remote")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Dorm Computer")
    (let [dorm (get-hazard state 0)]
      (card-ability state :challenger dorm 0)
      (prompt-choice :challenger "Server 1")
      (run-empty-server state "Server 1")
      (is (:run @state) "New run started")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type))
          "Challenger has prompt to wait for Snare!")
      (prompt-choice :contestant "Yes")
      (is (zero? (:tag (get-challenger))) "Challenger has 0 tags")
      (is (= 3 (get-counters (refresh dorm) :power))))))

(deftest feedback-filter
  ;; Feedback Filter - Prevent net and brain damage
  (do-game
    (new-game (default-contestant ["Data Mine"
                             "Cerebral Overwriter"
                             "Mushin No Shin"])
              (default-challenger [(qty "Feedback Filter" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :contestant "Mushin No Shin")
    (prompt-select :contestant (find-card "Cerebral Overwriter" (:hand (get-contestant))))
    (play-from-hand state :contestant "Data Mine" "Server 1")
    (let [co (get-content state :remote1 0)
          dm (get-character state :remote1 0)]
      (is (= 3 (get-counters (refresh co) :advancement)) "3 advancements on Overwriter")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Sure Gamble")
      (play-from-hand state :challenger "Feedback Filter")
      (is (= 7 (:credit (get-challenger))))
      (let [ff (get-hazard state 0)]
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
        (prompt-choice :challenger "Yes") ; discard Overwriter for 0
        (is (= 1 (:brain-damage (get-challenger))) "2 of the 3 brain damage prevented")
        (is (= 2 (count (:hand (get-challenger)))))
        (is (empty? (get-hazard state)) "Feedback Filter discarded")))))

(deftest flame-out
  ;; Flame-out - start with 9 credits, use for hosted resource, discard hosted resource at end of turn when credits used
  (testing "Basic behavior"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Flame-out" "Mimic"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Flame-out")
      (let [fo (get-hazard state 0)]
        (card-ability state :challenger fo 2)
        (prompt-select :challenger (find-card "Mimic" (:hand (get-challenger))))
        (take-credits state :challenger)
        (take-credits state :contestant)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic still hosted")
        (is (= 2 (:credit (get-challenger))) "Challenger starts with 2 credits")
        (card-ability state :challenger fo 0)
        (is (= 3 (:credit (get-challenger))) "Challenger gains 1 credit")
        (is (= 8 (get-counters (refresh fo) :credit)) "Took 1 credit from Flame-out")
        (take-credits state :challenger)
        (is (empty? (:hosted (refresh fo))) "Mimic discarded")
        (is (= 1 (count (:discard (get-challenger)))) "Mimic in discard"))))
  (testing "Contestant turn usage"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Flame-out" "Mimic"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Flame-out")
      (let [fo (get-hazard state 0)]
        (card-ability state :challenger fo 2)
        (prompt-select :challenger (find-card "Mimic" (:hand (get-challenger))))
        (take-credits state :challenger)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic hosted")
        (is (= 2 (:credit (get-challenger))) "Challenger starts with 2 credits")
        (card-ability state :challenger fo 1)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic still hosted")
        (is (= 11 (:credit (get-challenger))) "Challenger gains 9 credit")
        (is (zero? (get-counters (refresh fo) :credit)) "Took all credits from Flame-out")
        (take-credits state :contestant)
        (is (empty? (:hosted (refresh fo))) "Mimic discarded")))))

(deftest friday-chip
  ;; Friday Chip - gain counters for discarding cards, move a counter on turn start
  (do-game
    (new-game (default-contestant ["Adonis Campaign" "Hedge Fund"])
              (default-challenger ["Friday Chip" "Aumakua"]))
    (play-from-hand state :contestant "Adonis Campaign" "New remote")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 20)
    (play-from-hand state :challenger "Friday Chip")
    (play-from-hand state :challenger "Aumakua")
    (let [fc (get-hazard state 0)
          aum (get-resource state 0)]
      (is (zero? (get-counters fc :virus)) "Friday Chip starts with 0 counters")
      (is (zero? (get-counters aum :virus)) "Auakua starts with 0 counters")
      (run-on state "Server 1")
      (run-successful state)
      (prompt-choice-partial :challenger "Pay") ; discard Adonis Campaing
      (prompt-choice :challenger "Yes") ; gain virus counter
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip gains a counter on discard")
      (is (zero? (get-counters (refresh aum) :virus)) "Aumakua doesn't gain a counter")
      (run-on state "HQ")
      (run-successful state)
      (prompt-choice :challenger "No action")
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip doesn't gain a counter on non-discard")
      (is (= 1 (get-counters (refresh aum) :virus)) "Aumakua gains a counter on non-discard")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (prompt-select :challenger aum)
      (is (= 2 (get-counters (refresh aum) :virus)) "Aumakua gained 1 counter")
      (is (zero? (get-counters (refresh fc) :virus)) "Friday Chip lost 1 counter"))))

(deftest grimoire
  ;; Grimoire - Gain 2 MU, add a free virus counter to installed virus resources
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Grimoire" "Imp"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Grimoire")
    (is (= 6 (core/available-mu state)) "Gained 2 MU")
    (play-from-hand state :challenger "Imp")
    (let [imp (get-resource state 0)]
      (is (= 3 (get-counters (refresh imp) :virus)) "Imp received an extra virus counter on install"))))

(deftest heartbeat
  ;; Heartbeat - +1 MU, discard installed card to prevent 1 damage
  (do-game
    (new-game (default-contestant ["Pup" "Neural Katana"])
              (make-deck "Apex: Invasive Predator" [(qty "Heartbeat" 2) (qty "Sure Gamble" 2) "Cache"]))
    (play-from-hand state :contestant "Pup" "HQ")
    (play-from-hand state :contestant "Neural Katana" "R&D")
    (take-credits state :contestant)
    (core/end-phase-12 state :challenger nil)
    (prompt-select :challenger (find-card "Heartbeat" (:hand (get-challenger))))
    (play-from-hand state :challenger "Heartbeat")
    (is (= 5 (core/available-mu state)) "Gained 1 MU")
    (play-from-hand state :challenger "Cache")
    (let [hb (get-hazard state 0)
          cache (get-resource state 0)
          hbdown (get-challenger-facedown state 0)
          pup (get-character state :hq 0)
          nk (get-character state :rd 0)]
      (core/rez state :contestant pup)
      (core/rez state :contestant nk)
      (card-subroutine state :contestant (refresh pup) 0)
      (card-ability state :challenger hb 0)
      (prompt-select :challenger cache)
      (prompt-choice :challenger "Done")
      (is (= 1 (count (:discard (get-challenger)))) "Prevented 1 net damage")
      (is (= 2 (count (:hand (get-challenger)))))
      (card-subroutine state :contestant (refresh nk) 0)
      (card-ability state :challenger hb 0)
      (prompt-select :challenger hbdown)
      (prompt-choice :challenger "Done")
      (is (= 4 (count (:discard (get-challenger)))) "Prevented 1 of 3 net damage; used facedown card"))))

(deftest hippo
  ;; Hippo - remove from game to discard outermost piece of character if all subs broken
  (testing "No character"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Hippo"]))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hazard state)) "Hippo installed")
      (card-ability state :challenger (get-hazard state 0) 0)
      (is (empty? (:rfg (get-challenger))) "Hippo not RFGed")
      (is (not-empty (get-hazard state)) "Hippo still installed")))
  (testing "Single character"
    (do-game
      (new-game (default-contestant ["Ice Wall"])
                (default-challenger ["Hippo"]))
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (core/rez state :contestant (get-character state :hq 0))
      (take-credits state :contestant)
      (play-from-hand state :challenger "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hazard state)) "Hippo installed")
      (is (= 1 (count (get-in @state [:contestant :servers :hq :characters]))) "Ice Wall installed")
      (card-ability state :challenger (get-hazard state 0) 0)
      (is (empty? (get-in @state [:contestant :servers :hq :characters])) "Ice Wall removed")
      (is (= 1 (count (:discard (get-contestant)))) "Ice Wall discarded")
      (is (= 1 (count (:rfg (get-challenger)))) "Hippo RFGed")
      (is (empty? (get-hazard state)) "Hippo removed")))
  (testing "Multiple character"
    (do-game
      (new-game (default-contestant ["Ice Wall" "Enigma"])
                (default-challenger ["Hippo"]))
      (play-from-hand state :contestant "Enigma" "HQ")
      (play-from-hand state :contestant "Ice Wall" "HQ")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hazard state)) "Hippo installed")
      (is (= 2 (count (get-in @state [:contestant :servers :hq :characters]))) "2 character installed")
      (is (= "Ice Wall" (:title (get-character state :hq 1))) "Ice Wall outermost")
      (is (= "Enigma" (:title (get-character state :hq 0))) "Enigma innermost")
      (card-ability state :challenger (get-hazard state 0) 0)
      (is (= 1 (count (get-in @state [:contestant :servers :hq :characters]))) "Ice removed")
      (is (= 1 (count (:discard (get-contestant)))) "Ice discarded")
      (is (= "Ice Wall" (:title (first (:discard (get-contestant))))) "Ice Wall in discard")
      (is (= "Enigma" (:title (get-character state :hq 0))) "Enigma still innermost")
      (is (= 1 (count (:rfg (get-challenger)))) "Hippo RFGed")
      (is (empty? (get-hazard state)) "Hippo removed"))))

(deftest knobkierie
  ;; Knobkierie - first successful run, place a virus counter on a virus resource
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Knobkierie" "Hivemind" "Eater"]))
    (core/gain state :challenger :credit 20)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Knobkierie")
    (play-from-hand state :challenger "Eater")
    (run-on state "HQ")
    (run-successful state)
    (prompt-choice :challenger "No action")
    (is (empty? (:prompt (get-challenger))) "No prompt if not virus resource installed")
    (take-credits state :challenger)
    (take-credits state :contestant)
    (play-from-hand state :challenger "Hivemind")
    (let [hv (find-card "Hivemind" (get-resource state))]
      (is (= 1 (get-counters (refresh hv) :virus)) "Hivemind starts with 1 virus counters")
      (run-on state "HQ")
      (run-successful state)
      (prompt-choice :challenger "Yes") ; gain virus counter
      (prompt-select :challenger (find-card "Hivemind" (get-resource state)))
      (prompt-choice :challenger "No action")
      (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind gains a counter on successful run")
      (run-on state "HQ")
      (run-successful state)
      (prompt-choice :challenger "No action")
      (is (empty? (:prompt (get-challenger))) "No prompt after first run")
      (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind doesn't gain a counter after first run"))))

(deftest llds-processor
  ;; LLDS Processor - Add 1 strength until end of turn to an characterbreaker upon install
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "LLDS Processor" 2) "Inti" "Passport"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "LLDS Processor")
    (play-from-hand state :challenger "Inti")
    (play-from-hand state :challenger "LLDS Processor")
    (play-from-hand state :challenger "Passport")
    (let [inti (get-resource state 0)
          pass (get-resource state 1)]
      (is (= 2 (:current-strength (refresh inti))) "Strength boosted by 1; 1 copy of LLDS when installed")
      (is (= 4 (:current-strength (refresh pass))) "Strength boosted by 2; 2 copies of LLDS when installed")
      (take-credits state :challenger)
      (is (= 1 (:current-strength (refresh inti))) "Strength reduced to default")
      (is (= 2 (:current-strength (refresh pass))) "Strength reduced to default"))))

(deftest maw
  ;; Maw - Once per turn, first time challenger declines to steal or discard, discard a HQ card at random
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "BOOM!" 5)])
                (default-challenger ["Maw"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 20)
      (run-empty-server state :hq)
      (prompt-choice :challenger "No action")
      (is (zero? (count (:discard (get-contestant)))) "No HQ card in discard before Maw installed")
      (play-from-hand state :challenger "Maw")
      (run-empty-server state :hq)
      (prompt-choice :challenger "No action")
      (is (zero? (count (:discard (get-contestant)))) "HQ card not discarded by Maw as first decline already happened")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-empty-server state :hq)
      (prompt-choice :challenger "No action")
      (is (= 1 (count (:discard (get-contestant)))) "HQ card discarded by Maw")
      (run-empty-server state :hq)
      (prompt-choice :challenger "No action")
      (is (= 1 (count (:discard (get-contestant)))) "2nd HQ card on same turn not discarded by Maw")))
  (testing "Check discarded card is discarded face-up if it's the card that is accessed, issue #2695"
    ;; Also checks Maw auto-discards on Operation with no discard cost
    (do-game
      (new-game (default-contestant ["Hedge Fund"])
                (default-challenger ["Maw"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 20)
      (play-from-hand state :challenger "Maw")
      (run-empty-server state :hq)
      (is (zero? (count (:discard (get-contestant)))) "HQ card not discarded by Maw yet")
      (prompt-choice :challenger "No action")
      (is (= 1 (count (:discard (get-contestant)))) "HQ card discarded by Maw now")
      (is (:seen (first (:discard (get-contestant)))) "Discarded card is registered as seen since it was accessed")))
  (testing "with Hiro in hand - Hiro not moved to challenger scored area on discard decline. #2638"
    (do-game
      (new-game (default-contestant ["Chairman Hiro"])
                (default-challenger ["Maw"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 20)
      (play-from-hand state :challenger "Maw")
      (run-empty-server state :hq)
      (prompt-choice :challenger "No action")
      (is (zero? (count (:scored (get-challenger)))) "Hiro not scored")
      (is (= 1 (count (:discard (get-contestant)))) "Hiro discarded by Maw")))
  (testing "Maw shouldn't trigger on stolen agenda. #3433"
    (do-game
      (new-game (default-contestant ["Hostile Takeover"
                               (qty "Ice Wall" 5)])
                (default-challenger ["Maw"]))
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 20)
      (play-from-hand state :challenger "Maw")
      (run-empty-server state :remote1)
      (prompt-choice :challenger "Steal")
      (is (zero? (count (:discard (get-contestant)))) "No HQ card in discard as agenda was stolen")))
  (testing "Maw shouldn't trigger when accessing a card in archives. #3388"
    (do-game
      (new-game (default-contestant ["Rashida Jaheem" "Cyberdex Virus Suite" (qty "Ice Wall" 4)])
                (make-deck "Alcharacter Merchant: Clan Agitator" ["Maw" "Imp"]))
      (core/move state :contestant (find-card "Rashida Jaheem" (:hand (get-contestant))) :deck)
      (discard-from-hand state :contestant "Cyberdex Virus Suite")
      (take-credits state :contestant)
      (core/gain state :challenger :credit 100)
      (play-from-hand state :challenger "Maw")
      (play-from-hand state :challenger "Imp")
      (run-empty-server state :archives)
      (prompt-card :contestant (find-card "Ice Wall" (:hand (get-contestant)))) ;; Alcharacter's ability
      (prompt-choice :challenger "Cyberdex Virus Suite")
      (prompt-choice :contestant "Yes")
      (run-empty-server state :rd)
      (prompt-choice-partial :challenger "Pay")
      (is (= 3 (count (:discard (get-contestant)))) "Ice Wall, CVS, and Rashida")
      (is (empty? (:prompt (get-challenger))) "No more prompts for challenger")))
 (testing "Maw should trigger when declining to steal. #3388"
    (do-game
      (new-game (default-contestant [(qty "Obokata Protocol" 2) (qty "Ice Wall" 4)])
                (make-deck "Alcharacter Merchant: Clan Agitator" ["Maw" "Archives Interface"]))
      (discard-from-hand state :contestant "Ice Wall")
      (starting-hand state :contestant ["Obokata Protocol" "Obokata Protocol"])
      (take-credits state :contestant)
      (core/gain state :challenger :credit 100)
      (play-from-hand state :challenger "Maw")
      (play-from-hand state :challenger "Archives Interface")
      (run-empty-server state :archives)
      (prompt-card :contestant (find-card "Obokata Protocol" (:hand (get-contestant))))
      (prompt-choice :challenger "Yes")
      (prompt-card :challenger (find-card "Ice Wall" (:discard (get-contestant))))
      (prompt-choice :challenger "No action")
      (run-empty-server state :hq)
      (prompt-choice :challenger "No action")
      (is (= 2 (count (:discard (get-contestant)))) "Ice Wall and Obokata"))))

(deftest maya
  ;; Maya - Move accessed card to bottom of R&D
  (testing "Basic test"
    (do-game
      (new-game (default-contestant [(qty "Hedge Fund" 2) (qty "Snare!" 2) "Hostile Takeover" "Scorched Earth"])
                (default-challenger ["Maya" (qty "Sure Gamble" 3)]))
      (core/move state :contestant (find-card "Hostile Takeover" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Snare!" (:hand (get-contestant))) :deck)
      (take-credits state :contestant)
      (play-from-hand state :challenger "Maya")
      (let [maya (get-hazard state 0)
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
          (is (zero? (count (:hand (get-challenger)))) "Challenger took Snare! net damage")
          (is (= (:cid accessed) (:cid (:card (first (:prompt (get-challenger)))))) "Accessing the top card of R&D")
          (card-ability state :challenger maya 0)
          (is (empty? (:prompt (get-challenger))) "No more prompts for challenger")
          (is (not (:run @state)) "Run is ended")
          (is (= (:cid accessed) (:cid (last (:deck (get-contestant))))) "Maya moved the accessed card to the bottom of R&D")))))
  (testing "Does not interrupt multi-access"
    (do-game
      (new-game (default-contestant [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
                (default-challenger ["Maya" (qty "Sure Gamble" 3) "R&D Interface"]))
      (core/move state :contestant (find-card "Scorched Earth" (:hand (get-contestant))) :deck)
      (core/move state :contestant (find-card "Snare!" (:hand (get-contestant))) :deck)
      (take-credits state :contestant)
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Maya")
      (play-from-hand state :challenger "R&D Interface")
      (let [maya (get-hazard state 0)
            accessed (first (:deck (get-contestant)))]
        (run-empty-server state :rd)
        (prompt-choice :challenger "Card from deck")
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-challenger)))))) "Accessing the top card of R&D")
        (card-ability state :challenger maya 0)
        (is (= (:cid accessed) (:cid (last (:deck (get-contestant))))) "Maya moved the accessed card to the bottom of R&D")
        (is (:prompt (get-challenger)) "Challenger has next access prompt")))))

(deftest net-ready-eyes
  ;; Net-Ready Eyes
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Sure Gamble" 3) "Net-Ready Eyes" "Peacock"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Peacock")
    (play-from-hand state :challenger "Net-Ready Eyes")
    (is (= 3 (count (:discard (get-challenger)))) "Took 2 damage on NRE install")
    (run-on state "HQ")
    (let [pea (get-resource state 0)]
      (prompt-select :challenger pea)
      (is (= 3 (:current-strength (refresh pea))) "Peacock strength boosted")
      (run-continue state)
      (run-successful state)
      (prompt-choice :challenger "No action")
      (is (= 2 (:current-strength (refresh pea))) "Peacock strength back to default"))))

(deftest obelus
  ;; Obelus - Increase max hand size with tags, draw cards on first successful HQ/R&D run
  (testing "Basic test"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Obelus" "Nerve Agent"
                                 (qty "Sure Gamble" 3) (qty "Cache" 3)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Obelus" "Nerve Agent"])
      (core/gain state :challenger :credit 10 :click 3)
      (play-from-hand state :challenger "Nerve Agent")
      (let [nerve (get-resource state 0)]
        (run-empty-server state :hq)
        (is (= 1 (get-counters (refresh nerve) :virus)) "1 virus counter on Nerve Agent")
        (prompt-choice :challenger "No action")
        (play-from-hand state :challenger "Obelus")
        (core/gain state :challenger :tag 1)
        (is (= 6 (core/hand-size state :challenger)) "Max hand size is 6")
        (core/lose state :challenger :tag 1)
        (is (= 5 (core/hand-size state :challenger)) "Max hand size is 5")
        (run-empty-server state :hq)
        (is (= 2 (get-counters (refresh nerve) :virus)) "2 virus counters on Nerve Agent")
        (prompt-choice :challenger 1)
        (prompt-choice :challenger "Card from hand")
        (prompt-choice :challenger "No action")
        (prompt-choice :challenger "Card from hand")
        (prompt-choice :challenger "No action")
        (is (empty? (:hand (get-challenger))) "No cards drawn by Obelus, already had successful HQ run")
        (take-credits state :challenger)
        (take-credits state :contestant)
        (run-empty-server state :hq)
        (is (= 3 (get-counters (refresh nerve) :virus)) "3 virus counters on Nerve Agent")
        (prompt-choice :challenger 2)
        (prompt-choice :challenger "Card from hand")
        (prompt-choice :challenger "No action")
        (prompt-choice :challenger "Card from hand")
        (prompt-choice :challenger "No action")
        (prompt-choice :challenger "Card from hand")
        (prompt-choice :challenger "No action")
        (is (= 3 (count (:hand (get-challenger)))) "Obelus drew 3 cards"))))
  (testing "running and discarding Crisium Grid makes run neither successful/unsuccessful"
    (do-game
      (new-game (default-contestant ["Hedge Fund" "Crisium Grid"])
                (default-challenger ["Obelus" (qty "Sure Gamble" 3)]))
      (starting-hand state :contestant ["Crisium Grid"])
      (play-from-hand state :contestant "Crisium Grid" "R&D")
      (core/rez state :contestant (get-content state :rd 0))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Obelus"])
      (core/gain state :challenger :credit 5)
      (play-from-hand state :challenger "Obelus")
      (is (empty? (:hand (get-challenger))) "No cards in hand")
      (run-empty-server state "R&D")
      (prompt-choice :challenger "Crisium Grid")
      (prompt-choice-partial :challenger "Pay")
      (prompt-choice-partial :challenger "Card")
      (prompt-choice-partial :challenger "No")
      (is (empty? (:hand (get-challenger))) "Crisium Grid blocked successful run")
      (run-empty-server state "R&D")
      (prompt-choice-partial :challenger "No")
      (is (= 1 (count (:hand (get-challenger)))) "Obelus drew a card on first successful run")))
  (testing "using Hades Shard during run to increase draw"
    (do-game
      (new-game (default-contestant [(qty "Hedge Fund" 3) (qty "Restructure" 3)])
                (default-challenger ["Obelus" "Hades Shard"
                                 (qty "Sure Gamble" 3) (qty "Cache" 3)]))
      (starting-hand state :contestant ["Hedge Fund" "Hedge Fund"])
      (discard-from-hand state :contestant "Hedge Fund")
      (discard-from-hand state :contestant "Hedge Fund")
      (take-credits state :contestant)
      (starting-hand state :challenger ["Obelus" "Hades Shard"])
      (core/gain state :challenger :credit 10)
      (play-from-hand state :challenger "Obelus")
      (play-from-hand state :challenger "Hades Shard")
      (run-empty-server state "R&D")
      (card-ability state :challenger (get-radicle state 0) 0)
      (prompt-choice :challenger "No action")
      (is (= 3 (count (:hand (get-challenger)))) "Obelus drew 3 cards")))
  (testing "running a remote server first doesn't block card draw"
    (do-game
      (new-game (default-contestant ["Urban Renewal" "Hedge Fund"])
                (default-challenger ["Obelus" (qty "Sure Gamble" 3)]))
      (starting-hand state :contestant ["Urban Renewal"])
      (play-from-hand state :contestant "Urban Renewal" "New remote")
      (take-credits state :contestant)
      (starting-hand state :challenger ["Obelus"])
      (play-from-hand state :challenger "Obelus")
      (is (empty? (:hand (get-challenger))) "No cards in hand")
      (run-empty-server state "Server 1")
      (prompt-choice-partial :challenger "No")
      (run-empty-server state "R&D")
      (prompt-choice-partial :challenger "No")
      (is (= 1 (count (:hand (get-challenger)))) "Obelus drew a card on first successful run"))))

(deftest plascrete-carapace
  ;; Plascrete Carapace - Prevent meat damage
  (do-game
    (new-game (default-contestant ["Scorched Earth"])
              (default-challenger ["Plascrete Carapace" "Sure Gamble"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Plascrete Carapace")
    (let [plas (get-hazard state 0)]
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
      (is (empty? (get-hazard state)) "Plascrete depleted and discarded"))))

(deftest rabbit-hole
  ;; Rabbit Hole - +1 link, optionally search Stack to install more copies
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Sure Gamble" (qty "Rabbit Hole" 3)]))
    (take-credits state :contestant)
    (core/move state :challenger (find-card "Rabbit Hole" (:hand (get-challenger))) :deck)
    (core/move state :challenger (find-card "Rabbit Hole" (:hand (get-challenger))) :deck)
    (play-from-hand state :challenger "Sure Gamble")
    (play-from-hand state :challenger "Rabbit Hole")
    (is (= 1 (:link (get-challenger))))
    (prompt-choice :challenger "Yes")
    (prompt-choice :challenger "Yes")
    (is (= 3 (:link (get-challenger))))
    (is (= 3 (count (get-hazard state))))
    (is (= 2 (:click (get-challenger))) "Clickless installs of extra 2 copies")
    (is (= 3 (:credit (get-challenger))) "Paid 2c for each of 3 copies")))

(deftest ramujan-reliant-550-bmi
  ;; Prevent up to X net or brain damage.
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Data Mine" "Snare!"])
                (default-challenger [(qty "Ramujan-reliant 550 BMI" 4)
                                 (qty "Sure Gamble" 6)]))
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
        (let [rr1 (get-hazard state 0)
              rr2 (get-hazard state 1)
              rr3 (get-hazard state 2)]
          (run-on state "Server 1")
          (core/rez state :contestant dm)
          (card-subroutine state :contestant dm 0)
          (card-ability state :challenger rr1 0)
          (prompt-choice :challenger 1)
          (is (last-log-contains? state "Sure Gamble")
              "Ramujan did log discarded card names")
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
              "Ramujan did log discarded card names")
          (is (= 1 (count (:hand (get-challenger)))) "3 net damage prevented")))))
  (testing "Prevent up to X net or brain damage. Empty stack"
    (do-game
      (new-game (default-contestant ["Data Mine"])
                (default-challenger ["Ramujan-reliant 550 BMI" "Sure Gamble"]))
      (play-from-hand state :contestant "Data Mine" "Server 1")
      (let [dm (get-character state :remote1 0)]
        (take-credits state :contestant)
        (play-from-hand state :challenger "Ramujan-reliant 550 BMI")
        (let [rr1 (get-hazard state 0)]
          (run-on state "Server 1")
          (core/rez state :contestant dm)
          (card-subroutine state :contestant dm 0)
          (card-ability state :challenger rr1 0)
          (prompt-choice :challenger 1)
          (is (zero? (count (:hand (get-challenger)))) "Not enough cards in Stack for Ramujan to work"))))))

(deftest recon-drone
  ;; discard and pay X to prevent that much damage from a card you are accessing
  (do-game
    (new-game (default-contestant ["Snare!" "House of Knives"
                             "Prisec" "Cerebral Overwriter"])
              (default-challenger [(qty "Recon Drone" 10)]))
    (core/gain state :contestant :click 10)
    (core/gain state :contestant :credit 100)
    (play-from-hand state :contestant "House of Knives" "New remote")
    (play-from-hand state :contestant "Snare!" "New remote")
    (play-from-hand state :contestant "Prisec" "New remote")
    (play-from-hand state :contestant "Cerebral Overwriter" "New remote")
    (score-agenda state :contestant (get-content state :remote1 0))
    (advance state (get-content state :remote4 0))
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
    (let [rd1 (get-hazard state 0)
          rd2 (get-hazard state 1)
          rd3 (get-hazard state 2)
          rd4 (get-hazard state 3)
          hok (get-scored state :contestant 0)]
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type)) "Challenger has prompt to wait for Snare!")
      (prompt-choice :contestant "Yes")
      (card-ability state :challenger rd1 0)
      (prompt-choice :challenger 3)
      (prompt-choice :challenger "Done")
      (prompt-choice :challenger "No action")
      (is (= 5 (count (:hand (get-challenger)))) "Challenger took no net damage")
      ; fire HOK while accessing Snare!
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type)) "Challenger has prompt to wait for Snare!")
      (card-ability state :contestant hok 0)
      ; Recon Drone ability won't fire as we are not accessing HOK
      (card-ability state :challenger rd2 0)
      (is (nil? (:number (:choices (first (:prompt (get-challenger)))))) "No choice to prevent damage from HOK")
      (prompt-choice :challenger "Done")
      (is (= 4 (count (:hand (get-challenger)))) "Challenger took 1 net damage from HOK")
      (prompt-choice :contestant "No")
      (prompt-choice :challenger "No action")
      (core/lose state :challenger :credit 100)
      ; can only stop 1 damage due to credits
      (core/gain state :challenger :credit 1)
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type)) "Challenger has prompt to wait for Snare!")
      (prompt-choice :contestant "Yes")
      (card-ability state :challenger rd2 0)
      (is (= 1 (:number (:choices (first (:prompt (get-challenger)))))) "Recon Drone choice limited to challenger credits")
      (prompt-choice :challenger 1)
      (prompt-choice :challenger "Done")
      (prompt-choice-partial :challenger "Pay")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger took 2 net damage from Snare!")
      (core/gain state :challenger :credit 100)
      (run-empty-server state "Server 3")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type)) "Challenger has prompt to wait for Prisec")
      (prompt-choice :contestant "Yes")
      (card-ability state :challenger rd3 0)
      (is (= 1 (:number (:choices (first (:prompt (get-challenger)))))) "Recon Drone choice limited to 1 meat")
      (prompt-choice :challenger 1)
      (prompt-choice :challenger "Done")
      (prompt-choice-partial :challenger "Pay")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger took no meat damage")
      (run-empty-server state "Server 4")
      (is (= :waiting (-> @state :challenger :prompt first :prompt-type)) "Challenger has prompt to wait for Cerebral Overwriter")
      (prompt-choice :contestant "Yes")
      (card-ability state :challenger rd4 0)
      (prompt-choice :challenger 1)
      (prompt-choice :challenger "Done")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger took no brain damage"))))

(deftest replicator
  ;; Replicator
  (testing "interaction with Bazaar. Issue #1511"
    (do-game
      (new-game (default-contestant)
                (default-challenger ["Replicator" "Bazaar" (qty "Spy Camera" 6)]))
      (letfn [(count-spy [n] (= n (count (filter #(= "Spy Camera" (:title %)) (-> (get-challenger) :rig :hazard)))))]
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
        (is (count-spy 6) "6 Spy Cameras installed")))))

(deftest respirocytes
  (testing "Should draw multiple cards when multiple respirocytes are in play"
    (do-game
      (new-game (default-contestant)
                (default-challenger [(qty "Respirocytes" 3) (qty "Sure Gamble" 3)]))
      (take-credits state :contestant)
      (starting-hand state :challenger ["Respirocytes" "Respirocytes" "Respirocytes" "Sure Gamble"])
      (dotimes [_ 2]
        (play-from-hand state :challenger "Respirocytes"))
      (is (= 2 (count (:discard (get-challenger)))) "2 damage done")
      (is (= 2 (count (:hand (get-challenger)))) "Drew 2 cards"))))

(deftest rubicon-switch
  ;; Rubicon Switch
  (do-game
   (new-game (default-contestant ["Ice Wall" "Pachinko"])
             (default-challenger ["Rubicon Switch"]))
   (play-from-hand state :contestant "Ice Wall" "HQ")
   (play-from-hand state :contestant "Pachinko" "R&D")
   (let [iw (get-character state :hq 0)
         pach (get-character state :rd 0)]
     (core/rez state :contestant iw)
     (take-credits state :contestant)
     (play-from-hand state :challenger "Rubicon Switch")
     (core/rez state :contestant pach)
     (let [rs (get-hazard state 0)]
       (card-ability state :challenger rs 0)
       (prompt-choice :challenger 1)
       (prompt-select :challenger (refresh iw))
       (is (:rezzed (refresh iw)) "Ice Wall rezzed last turn can't be targeted")
       (prompt-select :challenger (refresh pach))
       (is (not (:rezzed (refresh pach))) "Pachinko derezzed")
       (is (= 2 (:click (get-challenger))) "Spent 1 click")
       (is (= 1 (:credit (get-challenger))) "Spent 1c")))))

(deftest security-nexus
  ;; Security Nexus
  (do-game
    (new-game (default-contestant ["Ice Wall"])
              (default-challenger ["Security Nexus"]))
    (play-from-hand state :contestant "Ice Wall" "R&D")
    (take-credits state :contestant)
    (core/gain state :challenger :credit 100)
    (play-from-hand state :challenger "Security Nexus")
    (let [nexus (get-hazard state 0)]
      (run-on state :rd)
      (card-ability state :challenger nexus 0)
      (is (zero? (:tag (get-challenger))) "Challenger should have no tags to start")
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (not (:run @state)) "Run should end from losing Security Nexus trace")
      (is (= 1 (:tag (get-challenger))) "Challenger should take 1 tag from losing Security Nexus trace")
      (take-credits state :challenger)
      (take-credits state :contestant)
      (run-on state :rd)
      (card-ability state :challenger nexus 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 10)
      (is (:run @state) "Run should still be going on from winning Security Nexus trace")
      (is (= 1 (:tag (get-challenger))) "Challenger should still only have 1 tag"))))

(deftest sifr
  ;; Sifr - Once per turn drop encountered Character to zero strenght
  ;; Also handle archangel then re-install sifr should not break the game #2576
  (do-game
    (new-game (default-contestant ["Archangel" "IP Block" "Hedge Fund"])
              (default-challenger ["Modded" "Clone Chip" "Şifr" "Parasite"]))
    (core/gain state :contestant :credit 100)
    (core/gain state :challenger :credit 100)
    (play-from-hand state :contestant "Archangel" "HQ")
    (play-from-hand state :contestant "IP Block" "HQ")
    (take-credits state :contestant)
    (discard-from-hand state :challenger "Parasite")
    (play-from-hand state :challenger "Şifr")
    (is (= 2 (count (:hand (get-challenger)))) "Modded and Clone Chip in hand")
    (let [arch (get-character state :hq 0)
          ip (get-character state :hq 1)
          sifr (get-hazard state 0)]
      (core/rez state :contestant arch)
      (core/rez state :contestant ip)
      (is (= 4 (:current-strength (refresh ip))))
      (run-on state :hq)
      (is (= 2 (:position (:run @state))))
      (card-ability state :challenger sifr 0)
      (is (zero? (:current-strength (refresh ip))))
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
      (let [chip (get-hazard state 1)]
        (is (nil? (:sifr-target (refresh sifr))) "Sifr cleaned up on leave play")
        (is (zero? (count (:discard (get-contestant)))) "No Contestant cards discarded")
        (card-ability state :challenger chip 0)
        (prompt-select :challenger (find-card "Parasite" (:discard (get-challenger))))
        (let [para (get-resource state 0)]
          (prompt-select :challenger ip)
          (is (zero? (count (:discard (get-contestant)))) "IP Block Not Discarded")
          (is (= 1 (count (:hosted (refresh ip)))) "Parasite is hosted"))))))

(deftest spinal-modem
  ;; Spinal Modem - +1 MU, 2 recurring credits, take 1 brain damage on successful trace during run
  (do-game
    (new-game (default-contestant ["Caduceus"])
              (default-challenger ["Spinal Modem" "Sure Gamble"]))
    (play-from-hand state :contestant "Caduceus" "HQ")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Spinal Modem")
    (let [cad (get-character state :hq 0)
          sm (get-hazard state 0)]
      (is (= 5 (core/available-mu state)))
      (is (= 2 (get-counters (refresh sm) :recurring)))
      (run-on state :hq)
      (core/rez state :contestant cad)
      (card-subroutine state :contestant cad 0)
      (prompt-choice :contestant 0)
      (prompt-choice :challenger 0)
      (is (= 1 (:brain-damage (get-challenger))) "Took 1 brain damage")
      (is (= 1 (count (:discard (get-challenger)))))
      (is (= 4 (core/hand-size state :challenger)) "Reduced hand size"))))

(deftest sports-hopper
  ;; Sports Hopper
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Sports Hopper" 3) (qty "Sure Gamble" 3)]))
    (starting-hand state :challenger ["Sports Hopper"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Sports Hopper")
    (is (= 1 (:link (get-challenger))) "Gained 1 link")
    (card-ability state :challenger (get-hazard state 0) 0)
    (is (= 1 (count (:discard (get-challenger)))))
    (is (= 3 (count (:hand (get-challenger)))) "Drew 3 cards")
    (is (zero? (:link (get-challenger))) "Lost link")))

(deftest spy-camera
  ;; Spy Camera - Full test
  (do-game
    (new-game (default-contestant)
              (default-challenger [(qty "Spy Camera" 6) "Sure Gamble" "Desperado"
                               "Diesel" "Corroder" "Patron" "Kati Jones"]))
    (starting-hand state :challenger ["Spy Camera" "Spy Camera" "Spy Camera"
                                  "Spy Camera" "Spy Camera" "Spy Camera"])
    (is (= 6 (count (:hand (get-challenger)))))
    (core/move state :contestant (find-card "Hedge Fund" (:hand (get-contestant))) :deck)
    (take-credits state :contestant)
    (core/gain state :challenger :click 3)
    (dotimes [_ 6] (play-from-hand state :challenger "Spy Camera"))
    (let [spy (get-hazard state 5)]
      ;; look at top 6 cards
      (card-ability state :challenger spy 0)
      (prompt-card :challenger (find-card "Sure Gamble" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Desperado" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Diesel" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Corroder" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Patron" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Kati Jones" (:deck (get-challenger))))
      ;; try starting over
      (prompt-choice :challenger "Start over")
      (prompt-card :challenger (find-card "Kati Jones" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Patron" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Corroder" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Diesel" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Desperado" (:deck (get-challenger))))
      (prompt-card :challenger (find-card "Sure Gamble" (:deck (get-challenger)))) ;this is the top card on stack
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

(deftest the-gauntlet
  (testing "Access additional cards on run on HQ, not with Gang Sign. Issue #2749"
    (do-game
      (new-game (default-contestant ["Hostile Takeover"
                               (qty "Hedge Fund" 3)])
                (default-challenger ["The Gauntlet"
                                 "Gang Sign"]))
      (take-credits state :contestant)
      (core/gain state :challenger :credit 5)
      (play-from-hand state :challenger "Gang Sign")
      (play-from-hand state :challenger "The Gauntlet")
      (take-credits state :challenger)
      (play-from-hand state :contestant "Hostile Takeover" "New remote")
      (score-agenda state :contestant (get-content state :remote1 0))
      ;; Gang Sign should trigger, without The Gauntlet pop-up
      (let [gs (get-radicle state 0)]
        (prompt-is-card? :challenger gs))
      ;; This will throw error if The Gauntlet triggers.
      (prompt-choice :challenger "Card from hand"))))

(deftest the-personal-touch
  ;; The Personal Touch - Give +1 strength to an characterbreaker
  (do-game
    (new-game (default-contestant)
              (default-challenger ["The Personal Touch"
                               "Paricia"
                               "Faerie"]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Paricia")
    (play-from-hand state :challenger "Faerie")
    (let [par (get-resource state 0)
          fae (get-resource state 1)]
      (is (= 2 (:current-strength (refresh fae))))
      (play-from-hand state :challenger "The Personal Touch")
      (prompt-select :challenger par)
      (is (nil? (:hosted (refresh par))) "TPT can't be hosted on a non-characterbreaker")
      (prompt-select :challenger fae)
      (is (= 1 (count (:hosted (refresh fae)))) "TPT hosted on Faerie")
      (is (= 3 (:current-strength (refresh fae))) "Faerie receiving +1 strength from TPT"))))

(deftest titanium-ribs
  ;; Titanium Ribs - Choose cards lost to damage, but not on Contestant turn against Chronos Protocol
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" ["Pup" "Viktor 1.0"
                                                                     "Neural EMP"])
              (default-challenger [(qty "Titanium Ribs" 2) "Sure Gamble"
                               "Fall Guy" "Kati Jones"]))
    (play-from-hand state :contestant "Pup" "HQ")
    (play-from-hand state :contestant "Viktor 1.0" "R&D")
    (take-credits state :contestant)
    (play-from-hand state :challenger "Fall Guy")
    (play-from-hand state :challenger "Titanium Ribs")
    (prompt-select :challenger (find-card "Titanium Ribs" (:hand (get-challenger))))
    (prompt-select :challenger (find-card "Kati Jones" (:hand (get-challenger))))
    (is (empty? (:prompt (get-challenger))) "Fall Guy didn't try to prevent discarding of Kati")
    (is (= 2 (count (:discard (get-challenger)))) "2 cards discarded for Ribs installation meat damage")
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

(deftest turntable
  ;; Turntable - Swap a stolen agenda for a scored agenda
  (testing "Basic test"
    (do-game
      (new-game (default-contestant ["Domestic Sleepers" "Project Vitruvius"])
                (default-challenger ["Turntable"]))
      (play-from-hand state :contestant "Project Vitruvius" "New remote")
      (let [ag1 (get-content state :remote1 0)]
        (score-agenda state :contestant ag1)
        (take-credits state :contestant)
        (play-from-hand state :challenger "Turntable")
        (is (= 3 (:credit (get-challenger))))
        (let [tt (get-hazard state 0)]
          (run-empty-server state "HQ")
          (prompt-choice :challenger "Steal")
          (is (zero? (:agenda-point (get-challenger))) "Stole Domestic Sleepers")
          (is (prompt-is-card? :challenger tt))
          (prompt-choice :challenger "Yes")
          (prompt-select :challenger (find-card "Project Vitruvius" (:scored (get-contestant))))
          (is (= 2 (:agenda-point (get-challenger))) "Took Project Vitruvius from Contestant")
          (is (zero? (:agenda-point (get-contestant))) "Swapped Domestic Sleepers to Contestant")))))
  (testing "vs Mandatory Regions"
    ;; Turntable - Swap a Mandatory Regions away from the Contestant reduces Contestant clicks per turn
    ;;           - Contestant doesn't gain a click on the Challenger's turn when it receives a Mandatory Regions
    (do-game
      (new-game (default-contestant [(qty "Mandatory Regions" 2) "Project Vitruvius"])
                (default-challenger ["Turntable"]))
      (score-agenda state :contestant (find-card "Mandatory Regions" (:hand (get-contestant))))
      (is (= 4 (:click-per-turn (get-contestant))) "Up to 4 clicks per turn")
      (take-credits state :contestant)
      (play-from-hand state :challenger "Turntable")
      (let [tt (get-hazard state 0)]
        ;; steal Project Vitruvius and swap for Mandatory Regions
        (core/steal state :challenger (find-card "Project Vitruvius" (:hand (get-contestant))))
        (is (prompt-is-card? :challenger tt))
        (prompt-choice :challenger "Yes")
        (prompt-select :challenger (find-card "Mandatory Regions" (:scored (get-contestant))))
        (is (= 3 (:click-per-turn (get-contestant))) "Back down to 3 clicks per turn")
        ;; steal second Mandatory Regions and swap for Project Vitruvius
        (core/steal state :challenger (find-card "Mandatory Regions" (:hand (get-contestant))))
        (is (prompt-is-card? :challenger tt))
        (prompt-choice :challenger "Yes")
        (prompt-select :challenger (find-card "Project Vitruvius" (:scored (get-contestant))))
        (is (zero? (:click (get-contestant))) "Contestant doesn't gain a click on Challenger's turn")
        (is (= 4 (:click-per-turn (get-contestant))))))))

(deftest vigil
  ;; Vigil - Draw 1 card when turn begins if Contestant HQ is filled to max hand size
  (do-game
    (new-game (default-contestant [(qty "Hedge Fund" 3) (qty "PAD Campaign" 2)])
              (default-challenger ["Vigil" (qty "Sure Gamble" 2)]))
    (take-credits state :contestant)
    (play-from-hand state :challenger "Vigil")
    (is (= 5 (core/available-mu state)))
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
   (new-game (default-contestant ["Ice Wall"])
             (default-challenger ["Zamba" (qty "Infiltration" 2)]))
   (play-from-hand state :contestant "Ice Wall" "Archives")
   (take-credits state :contestant)
   (play-from-hand state :challenger "Zamba")
   (is (= 6 (core/available-mu state)) "Gain 2 memory")
   (is (= 1 (:credit (get-challenger))) "At 1 credit")
   (play-from-hand state :challenger "Infiltration")
   (prompt-choice :challenger "Expose a card")
   (prompt-select :challenger (get-character state :archives 0))
   (is (= 2 (:credit (get-challenger))) "Gained 1 credit from exposing")
   (play-from-hand state :challenger "Infiltration")
   (prompt-choice :challenger "Expose a card")
   (prompt-select :challenger (get-character state :archives 0))
   (is (= 3 (:credit (get-challenger))) "Gained 1 more credit from exposing")))

(deftest zer0
  ;; Zer0 - Once per turn, deal 1 damage to self, to gain 1 credit and 2 cards.
  (do-game
    (new-game (default-contestant)
              (default-challenger ["Zer0" "Corroder" (qty "Sure Gamble" 2)]))
    (starting-hand state :challenger ["Zer0" "Corroder"])
    (take-credits state :contestant)
    (play-from-hand state :challenger "Zer0")
    (is (= 4 (:credit (get-challenger))) "Challenger has 4 credits")
    (let  [z (get-hazard state 0)]
      (card-ability state :challenger z 0)
      (is (= 5 (:credit (get-challenger))) "Challenger has 5 credits")
      (is (= 2 (count (:hand (get-challenger)))) "Challenger has 2 cards")
      (is (find-card "Corroder" (:discard (get-challenger))) "Corroder is in heap"))))
