(ns game-test.cards.hardware
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "sites"))

(deftest advanced-assembly-lines
  ;; Advanced Assembly Lines
  (do-game
    (new-game (default-contestant ["Advanced Assembly Lines"
                             "PAD Campaign"])
              (default-challenger))
    (play-from-hand state :contestant "Advanced Assembly Lines" "New party")
    (let [aal (get-content state :party1 0)
          credits (:credit (get-contestant))
          hq (count (:hand (get-contestant)))]
      (core/reveal state :contestant aal)
      (is (= (+ credits 2) (:credit (get-contestant))) "Spend 1 gain 3")
      (card-ability state :contestant aal 0)
      (prompt-select :contestant (find-card "PAD Campaign" (:hand (get-contestant))))
      (prompt-choice :contestant "New party")
      (is (= (- hq 1) (count (:hand (get-contestant)))) "Placed 1 card, hq is empty"))))

(deftest alexa-belsky
  ;; Alexa Belsky
  (do-game
    (new-game
      (default-contestant ["Alexa Belsky" "Hedge Fund" "Breaking News"
                     "Gutenberg" "Product Placement" "Jackson Howard"])
      (default-challenger))
    (play-from-hand state :contestant "Alexa Belsky" "New party")
    (let [alexa (get-content state :party1 0)]
      (core/reveal state :contestant alexa)
      (card-ability state :contestant alexa 0)
      (is (= 1 (count (:discard (get-contestant)))) "Alexa Belsky discarded")
      (is (= 5 (count (:hand (get-contestant)))))
      (is (zero? (count (:deck (get-contestant)))))
      (prompt-choice :challenger 5) ;Challenger chooses to pay 5 credits so 2 cards are prevented from being shuffled
      (is (= 2 (count (:hand (get-contestant)))))
      (is (= 3 (count (:deck (get-contestant)))))
      (is (zero? (:credit (get-challenger)))))))

(deftest bioroid-work-crew
  ;; Bioroid Work Crew
  (letfn [(bwc-test [card]
            (do-game
              (new-game
                (default-contestant ["Bioroid Work Crew" card])
                (default-challenger))
              (play-from-hand state :contestant "Bioroid Work Crew" "New party")
              (let [bwc (get-content state :party1 0)]
                (core/reveal state :contestant bwc)
                (card-ability state :contestant bwc 0)
                (prompt-select :contestant (find-card card (:hand (get-contestant))))
                (prompt-choice :contestant "New party")
                (is (zero? (count (:hand (get-contestant)))))
                (is (= 1 (count (:discard (get-contestant)))) "Card should be discarded now"))))]
    (doall (map bwc-test
                ["Hostile Takeover"
                 "Dedicated Response Team"
                 "Builder"
                 "Research Station"]))))

(deftest broadcast-square
  ;; Broadcast Square - Trace 3: Prevent all bad publicity
  (do-game
    (new-game (default-contestant ["Profiteering" "Hostile Takeover" "Broadcast Square"])
              (default-challenger))
    (play-from-hand state :contestant "Broadcast Square" "New party")
    (core/reveal state :contestant (get-content state :party1 0))
    (is (= 3 (:credit (get-contestant))) "Contestant should have spent 2 credits")
    (play-from-hand state :contestant "Profiteering" "New party")
    (score-agenda state :contestant (get-content state :party2 0))
    (prompt-choice :contestant "3")  ;; Take 3 bad publicity from Profiteering, gain 15 (if bad publicity actually taken)
    (prompt-choice :contestant 0)  ;; Contestant doesn't pump trace, base 3
    (prompt-choice :challenger 0)  ;; Challenger doesn't pump trace; loses trace
    (is (= 1 (:agenda-point (get-contestant))) "Contestant should score a 1-point agenda")
    (is (zero? (:bad-publicity (get-contestant))) "Contestant should gain 0 bad publicity")
    (is (= 3 (:credit (get-contestant))) "Contestant should gain 0 credits")
    (play-from-hand state :contestant "Hostile Takeover" "New party")
    (score-agenda state :contestant (get-content state :party3 0))
    (prompt-choice :contestant 0)  ;; Contestant doesn't pump trace, base 3
    (prompt-choice :challenger 3)  ;; Challenger pumps trace; wins trace
    (is (= 2 (:agenda-point (get-contestant))) "Contestant should score a 1-point agenda")
    (is (= 1 (:bad-publicity (get-contestant))) "Contestant should gain 1 bad publicity from failed trace")
    (is (= 10 (:credit (get-contestant))) "Contestant should gain 7 credits")))

(deftest capital-investors
  ;; Capital Investors - Click for 2 credits
  (do-game
    (new-game (default-contestant ["Capital Investors"])
              (default-challenger))
    (play-from-hand state :contestant "Capital Investors" "New party")
    (let [cap (get-content state :party1 0)]
      (core/reveal state :contestant cap)
      (card-ability state :contestant cap 0)
      (card-ability state :contestant cap 0)
      (is (zero? (:click (get-contestant))) "Used twcharacter, spent 2 clicks")
      (is (= 7 (:credit (get-contestant))) "Used twcharacter, gained 4 credits"))))

(deftest cybernetics-court
  ;; Cybernetics Court
  (do-game
    (new-game (default-contestant ["Cybernetics Court"])
              (default-challenger))
    (play-from-hand state :contestant "Cybernetics Court" "New party")
    (core/reveal state :contestant (get-content state :party1 0))
    (is (= 9 (get-hand-size :contestant)) "Contestant should have hand size of 9")))

(deftest dedicated-locale
  ;; Dedicated Locales
  (do-game
    (new-game (default-contestant ["Dedicated Locale"])
              (default-challenger))
    (play-from-hand state :contestant "Dedicated Locale" "New party")
    (let [locales (get-content state :party1 0)]
      (core/reveal state :contestant locales)
      (is (= 2 (get-counters (refresh locales) :recurring)) "Should have 2 recurring credits"))))

(deftest echo-chamber
  ;; Echo Chamber - 3 clicks to become 1 point agenda
  (do-game
    (new-game (default-contestant ["Echo Chamber"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Echo Chamber" "New party")
    (let [ec (get-content state :party1 0)]
      (core/reveal state :contestant ec)
      (card-ability state :contestant ec 0))
    (is (= 1 (:agendapoints (get-scored state :contestant 0))) "Echo Chamber added to Contestant score area")))

(deftest eliza's-toybox
  ;; Eliza's Toybox - Reveal a card ignoring all costs
  (do-game
    (new-game (default-contestant ["Eliza's Toybox" "Wotan" "Archer"])
              (default-challenger))
    (play-from-hand state :contestant "Wotan" "R&D")
    (play-from-hand state :contestant "Archer" "HQ")
    (play-from-hand state :contestant "Eliza's Toybox" "New party")
    (let [wotan (get-character state :rd 0)
          archer (get-character state :hq 0)
          eliza (get-content state :party1 0)]
      (core/reveal state :contestant eliza)
      (is (= 1 (:credit (get-contestant))))
      (is (zero? (:click (get-contestant))) "3 clicks spent")
      (core/gain state :contestant :click 6)
      (card-ability state :contestant eliza 0)
      (prompt-select :contestant wotan)
      (is (:revealed (refresh wotan)))
      (is (= 3 (:click (get-contestant))) "3 clicks spent")
      (is (= 1 (:credit (get-contestant))) "No credits spent")
      (card-ability state :contestant eliza 0)
      (prompt-select :contestant archer)
      (is (:revealed (refresh archer)))
      (is (zero? (:click (get-contestant))) "3 clicks spent")
      (is (= 1 (:credit (get-contestant))) "No credits or agendas spent"))))

(deftest estelle-moon
  ;; Estelle Moon
  (letfn [(estelle-test [number]
            (do-game
              (new-game (default-contestant ["Estelle Moon" (qty "Encryption Protocol" 20)])
                        (default-challenger))
              (starting-hand state :contestant (repeat 9 "Encryption Protocol"))
              (core/move state :contestant (find-card "Estelle Moon" (:deck (get-contestant))) :hand)
              (play-from-hand state :contestant "Estelle Moon" "New party")
              (let [em (get-content state :party1 0)]
                (core/reveal state :contestant (refresh em))
                (core/gain state :contestant :click 10)
                (dotimes [_ number]
                  (play-from-hand state :contestant "Encryption Protocol" "New party"))
                (let [credits (:credit (get-contestant))
                      hand (count (:hand (get-contestant)))]
                  (card-ability state :contestant (refresh em) 0)
                  (is (= (* 2 number) (- (:credit (get-contestant)) credits)) (str "Should gain " (* 2 number) " credits"))
                  (is (= number (- (count (:hand (get-contestant))) hand)) (str "Should draw " number " cards"))
                  (is (= 1 (-> (get-contestant) :discard count)) "Estelle Moon should be discarded")))))]
    (doall (map estelle-test (range 10)))))

(deftest executive-search-firm
  ;; Executive Search Firm
  (do-game
    (new-game (default-contestant ["Executive Search Firm" "Elizabeth Mills"
                             "Midori" "Shannon Claire"])
              (default-challenger))
    (starting-hand state :contestant ["Executive Search Firm"])
    (core/gain state :contestant :click 4)
    (play-from-hand state :contestant "Executive Search Firm" "New party")
    (doseq [card ["Elizabeth Mills" "Midori" "Shannon Claire"]]
      (let [esf (get-content state :party1 0)
            number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))]
        (card-ability state :contestant esf 0)
        (prompt-choice :contestant (find-card card (:deck (get-contestant))))
        (is (= card (-> (get-contestant) :hand first :title)) (str card " should be in hand"))
        (core/move state :contestant (find-card card (:hand (get-contestant))) :deck)
        (is (< number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))) "Should be shuffled")))))

(deftest fumiko-yamamori
  ;; Fumiko Yamamori
  (do-game
    (new-game
      (default-contestant ["Fumiko Yamamori"])
      (default-challenger))
    (core/gain state :contestant :credit 10)
    (play-from-hand state :contestant "Fumiko Yamamori" "New party")
    (let [fumiko (get-content state :party1 0)]
      (core/reveal state :contestant (refresh fumiko))
      (core/psi-game state :contestant (refresh fumiko)
                     {:equal  {:msg "resolve equal bets effect"}
                      :not-equal {:msg "resolve unequal bets effect"}})
      (prompt-choice :contestant "2 [Credits]")
      (prompt-choice :challenger "0 [Credits]")
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should discard a card to meat damage"))))

(deftest indian-union-stock-exchange
  ;; Indian Union Stock Exchange
  (do-game
    (new-game (make-deck "Argus Security: Protection Guaranteed"
                         ["Indian Union Stock Exchange" "Beanstalk Royalties"
                          "Kill Switch" "Net Police"])
              (default-challenger))
    (core/gain state :contestant :click 3)
    (play-from-hand state :contestant "Indian Union Stock Exchange" "New party")
    (core/reveal state :contestant (get-content state :party1 0))
    (let [credits (:credit (get-contestant))]
      (play-from-hand state :contestant "Beanstalk Royalties")
      (is (= (+ 3 credits) (:credit (get-contestant))) "Contestant should only gain 3 credits"))
    (let [credits (:credit (get-contestant))]
      (play-from-hand state :contestant "Kill Switch")
      (is (= credits (:credit (get-contestant))) "Contestant should neither gain nor lose any credits"))
    (let [credits (:credit (get-contestant))]
      (play-from-hand state :contestant "Net Police" "New party")
      (core/reveal state :contestant (get-content state :party2 0))
      (is (= credits (:credit (get-contestant))) "Contestant should neither gain nor lose any credits"))))

(deftest isabel-mcguire
  ;; Isabel McGuire
  (do-game
    (new-game (default-contestant ["Character Wall" "Isabel McGuire"])
              (default-challenger))
    (play-from-hand state :contestant "Isabel McGuire" "New party")
    (play-from-hand state :contestant "Character Wall" "HQ")
    (is (zero? (-> (get-contestant) :hand count)))
    (let [isabel (get-content state :party1 0)
          iw (get-character state :hq 0)]
      (core/reveal state :contestant isabel)
      (card-ability state :contestant isabel 0)
      (prompt-select :contestant (refresh iw))
      (is (= 1 (-> (get-contestant) :hand count))))))

(deftest jackson-howard
  ;; Jackson Howard - Draw 2 cards
  (do-game
    (new-game (default-contestant [(qty "Jackson Howard" 3)
                             (qty "Hedge Fund" 3)
                             (qty "Restructure" 2)])
              (default-challenger))
    ;; guaranteed to be at least 1 jhow in hand after draw, and 2 cards in R&D
    (play-from-hand state :contestant "Jackson Howard" "New party")
    (let [jhow (get-content state :party1 0)]
      (core/reveal state :contestant jhow)
      (is (= 5 (count (:hand (get-contestant)))))
      (is (= 2 (:click (get-contestant))))
      (card-ability state :contestant jhow 0)
      (is (= 7 (count (:hand (get-contestant)))) "Drew 2 cards")
      (is (= 1 (:click (get-contestant)))))))

(deftest levy-university
  ;; Levy University
  (do-game
    (new-game (default-contestant ["Levy University" "Character Wall" (qty "Fire Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant ["Levy University"])
    (play-from-hand state :contestant "Levy University" "New party")
    (let [levy (get-content state :party1 0)
          number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))]
      (core/reveal state :contestant levy)
      (is (zero? (-> (get-contestant) :hand count)) "HQ should be empty")
      (let [clicks (:click (get-contestant))
            credits (:credit (get-contestant))]
        (card-ability state :contestant (refresh levy) 0)
        (prompt-card :contestant (find-card "Character Wall" (:deck (get-contestant))))
        (is (= (- credits 1) (:credit (get-contestant))) "Levy University ability should cost 1 credit")
        (is (= (- clicks 1) (:click (get-contestant))) "Levy University ability should cost 1 click"))
      (is (= 1 (-> (get-contestant) :hand count)) "HQ should have 1 card")
      (is (= "Character Wall" (-> (get-contestant) :hand first :title)) "HQ should contain Character Wall")
      (is (< number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))) "Contestant should shuffle deck"))))

(deftest lily-lockwell
  ;; Lily Lockwell
  (do-game
    (new-game (default-contestant ["Lily Lockwell" "Beanstalk Royalties" (qty "Fire Wall" 10)])
              (default-challenger))
    (core/gain state :contestant :click 10)
    (starting-hand state :contestant ["Lily Lockwell" "Beanstalk Royalties"])
    (play-from-hand state :contestant "Lily Lockwell" "New party")
    (core/gain state :challenger :tag 2)
    (let [lily (get-content state :party1 0)
          clicks (:click (get-contestant))
          number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))
          hand (-> (get-contestant) :hand count)]
      (core/reveal state :contestant lily)
      (is (= (+ 3 hand) (-> (get-contestant) :hand count)) "Revealing Lily Lockwell should draw 3 cards")
      (core/move state :contestant (find-card "Beanstalk Royalties" (:hand (get-contestant))) :deck)
      (card-ability state :contestant (refresh lily) 0)
      (prompt-card :contestant (find-card "Beanstalk Royalties" (-> (get-contestant) :prompt first :choices)))
      (is (= "Beanstalk Royalties" (-> (get-contestant) :deck first :title)) "Beanstalk Royalties should be moved to top of R&D")
      (is (= 1 (:tag (get-challenger))) "Challenger should have 1 tag from Lily Lockwell ability")
      (is (= (- clicks 1) (:click (get-contestant))) "Lily Lockwell ability should cost 1 click")
      (is (< number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))) "Contestant should shuffle deck")
      (core/draw state :contestant)
      (card-ability state :contestant (refresh lily) 0)
      (prompt-choice :contestant "Cancel")
      (is (last-log-contains? state "did not find") "Lily Lockwell's ability didn't find an operation")
      (is (zero? (:tag (get-challenger))) "Challenger should have 0 tags from Lily Lockwell ability even when no operation found"))))

(deftest mr.-stone
  ;; Mr Stone
  (do-game
    (new-game (default-contestant ["Mr. Stone"])
              (default-challenger))
    (play-from-hand state :contestant "Mr. Stone" "New party")
    (let [stone (get-content state :party1 0)]
      (core/reveal state :contestant stone)
      (core/tag-challenger state :challenger 1)
      (is (= 1 (-> (get-challenger) :discard count)) "Challenger should take 1 meat damage from gaining 1 tag")
      (core/tag-challenger state :challenger 5)
      (is (= 2 (-> (get-challenger) :discard count)) "Challenger should take 1 meat damage from gaining 5 tags"))))

(deftest mumba-temple
  ;; Mumba Temple
  (do-game
    (new-game (default-contestant ["Mumba Temple"])
              (default-challenger))
    (play-from-hand state :contestant "Mumba Temple" "New party")
    (let [mumba (get-content state :party1 0)]
      (core/reveal state :contestant mumba)
      (is (= 2 (get-counters (refresh mumba) :recurring)) "Should have 2 recurring credits"))))

(deftest mumbad-city-hall
  ;; Mumbad City Hall
  (do-game
    (new-game (default-contestant ["Mumbad City Hall"
                             "PAD Factory"
                             "Salem's Hospitality"])
              (default-challenger))
    (core/gain state :contestant :click 3 :credit 100)
    (starting-hand state :contestant ["Mumbad City Hall"])
    (play-from-hand state :contestant "Mumbad City Hall" "New party")
    (let [mumbad (get-content state :party1 0)]
      (core/reveal state :contestant mumbad)
      (card-ability state :contestant mumbad 0)
      (prompt-card :contestant (find-card "PAD Factory" (:deck (get-contestant))))
      (prompt-choice :contestant "New party")
      (is (= "PAD Factory" (:title (get-content state :party2 0))))
      (card-ability state :contestant mumbad 0)
      (prompt-card :contestant (find-card "Salem's Hospitality" (:deck (get-contestant))))
      (prompt-choice :contestant "Sure Gamble")
      (is (= 3 (-> (get-challenger) :discard count)) "Challenger should have discarded all cards from Salem's Hospitality"))))

(deftest primary-transmission-dish
  ;; Primary Transmission Dish
  (do-game
    (new-game (default-contestant ["Primary Transmission Dish"])
              (default-challenger))
    (play-from-hand state :contestant "Primary Transmission Dish" "New party")
    (let [dish (get-content state :party1 0)]
      (core/reveal state :contestant dish)
      (is (= 3 (get-counters (refresh dish) :recurring)) "Should have 3 recurring credits"))))

(deftest private-contracts
  ;; Private Contracts
  (do-game
    (new-game (default-contestant ["Private Contracts"])
              (default-challenger))
    (play-from-hand state :contestant "Private Contracts" "New party")
    (let [pri (get-content state :party1 0)]
      (core/reveal state :contestant pri)
      (is (= 14 (get-counters (refresh pri) :credit)) "Should start with 14 credits")
      (is (zero? (-> (get-contestant) :discard count)) "Contestant should have 0 cards in Archives")
      (core/gain state :contestant :click 7)
      (core/lose state :contestant :credit 2)
      (dotimes [_ 7]
        (card-ability state :contestant pri 0))
      (is (= 1 (-> (get-contestant) :discard count)) "Private Contracts should be in discard")
      (is (= 14 (:credit (get-contestant))) "Contestant should now have 14 credits"))))

(deftest quarantine-system
  ;; Forfeit agenda to reveal up to 3 Character with 2 credit discount per agenda point
  (do-game
    (new-game
      (default-contestant [(qty "Chiyashi" 3) "Quarantine System" "Project Beale"])
      (default-challenger))
    (core/gain state :contestant :credit 100)
    (core/gain state :contestant :click 100)
    (play-from-hand state :contestant "Chiyashi" "HQ")
    (play-from-hand state :contestant "Chiyashi" "HQ")
    (play-from-hand state :contestant "Chiyashi" "HQ")
    (play-from-hand state :contestant "Quarantine System" "New party")
    (play-from-hand state :contestant "Project Beale" "New party")
    (is (= 102 (:credit (get-contestant))) "Contestant has 102 creds")
    (let [ch1 (get-character state :hq 0)
          ch2 (get-character state :hq 1)
          ch3 (get-character state :hq 2)
          qs (get-content state :party1 0)
          beale (get-content state :party2 0)]
      (core/reveal state :contestant qs)
      (card-ability state :contestant qs 0)
      (is (empty? (:prompt (get-contestant))) "No prompt to reveal Character")
      (score-agenda state :contestant beale)
      ; 1 on reveal
      (is (= 101 (:credit (get-contestant))) "Contestant has 101 creds")
      (card-ability state :contestant qs 0)
      (prompt-select :contestant (get-scored state :contestant 0))
      (prompt-select :contestant ch1)
      (prompt-select :contestant ch2)
      (prompt-select :contestant ch3)
      ; pay 8 per Chiyashi - 24 total
      (is (= 77 (:credit (get-contestant))) "Contestant has 77 creds")
      (is (empty? (:prompt (get-contestant))) "No prompt to reveal Character"))))

(deftest reconstruction-contract
  ;; Reconstruction Contract - place advancement token when challenger takes meat damage
  (do-game
    (new-game (default-contestant ["Reconstruction Contract" "Scorched Earth" "Pup"])
              (default-challenger [(qty "Sure Gamble" 3) (qty "Imp" 3)]))
    (core/gain state :challenger :tag 1)
    (core/gain state :contestant :credit 5)
    (starting-hand state :challenger ["Sure Gamble" "Sure Gamble" "Sure Gamble" "Imp" "Imp"])
    (play-from-hand state :contestant "Reconstruction Contract" "New party")
    (let [rc (get-content state :party1 0)]
      (core/reveal state :contestant (refresh rc))
      (play-from-hand state :contestant "Scorched Earth")
      (is (= 4 (count (:discard (get-challenger)))))
      (is (= 1 (get-counters (refresh rc) :advancement)) "Reconstruction Contract has 1 advancement token")
      (starting-hand state :challenger ["Imp" "Imp"])
      (play-from-hand state :contestant "Pup" "HQ")
      (core/reveal state :contestant (get-character state :hq 0))
      (card-subroutine state :contestant (get-character state :hq 0) 0)
      (is (= 5 (count (:discard (get-challenger)))))
      (is (= 1 (get-counters (refresh rc) :advancement)) "Reconstruction Contract doesn't get advancement token for net damage"))))

(deftest sealed-vault
  ;; Sealed Vault - Store credits for 1c, retrieve credits by discarding or spending click
  (do-game
    (new-game (default-contestant ["Sealed Vault" "Hedge Fund"])
              (default-challenger))
    (play-from-hand state :contestant "Sealed Vault" "New party")
    (play-from-hand state :contestant "Hedge Fund")
    (let [sv (get-content state :party1 0)]
      (core/reveal state :contestant sv)
      (card-ability state :contestant sv 0)
      (prompt-choice :contestant 8)
      (is (= 8 (get-counters (refresh sv) :credit)) "8 credits stored on Sealed Vault")
      (is (zero? (:credit (get-contestant))))
      (card-ability state :contestant sv 1)
      (prompt-choice :contestant 8)
      (is (zero? (get-counters (refresh sv) :credit)) "Credits removed from Sealed Vault")
      (is (= 8 (:credit (get-contestant))))
      (is (zero? (:click (get-contestant))) "Spent a click")
      (card-ability state :contestant sv 0)
      (prompt-choice :contestant 7)
      (is (= 7 (get-counters (refresh sv) :credit)) "7 credits stored on Sealed Vault")
      (is (zero? (:credit (get-contestant))))
      (card-ability state :contestant sv 2)
      (prompt-choice :contestant 7)
      (is (= 7 (:credit (get-contestant))))
      (is (= 2 (count (:discard (get-contestant)))) "Sealed Vault discarded"))))

(deftest security-subcontract
  ;; Security Subcontract
  (do-game
    (new-game (default-contestant ["Security Subcontract" "Character Wall"])
              (default-challenger))
    (play-from-hand state :contestant "Security Subcontract" "New party")
    (play-from-hand state :contestant "Character Wall" "HQ")
    (let [ss (get-content state :party1 0)
          iw (get-character state :hq 0)]
      (core/reveal state :contestant ss)
      (core/reveal state :contestant iw)
      (card-ability state :contestant ss 0)
      (let [credits (:credit (get-contestant))
            clicks (:click (get-contestant))]
        (prompt-select :contestant iw)
        (is (= (+ credits 4) (:credit (get-contestant))) "Contestant should gain 4 from Security Subcontract ability")
        (is (= "Character Wall" (-> (get-contestant) :discard first :title)) "Character Wall should be in Archives from Security Subcontract ability")
        (is (= (- clicks 1) (:click (get-contestant))) "Contestant should lose 1 click from Security Subcontract ability")))))

(deftest shannon-claire
  ;; Shannon Claire
  (do-game
    (new-game (default-contestant ["Shannon Claire" "Hostile Takeover" "Ronin" (qty "Character Wall" 10)])
              (default-challenger))
    (starting-hand state :contestant ["Shannon Claire" "Ronin"])
    (core/move state :contestant (find-card "Ronin" (:hand (get-contestant))) :deck)
    (play-from-hand state :contestant "Shannon Claire" "New party")
    (let [shannon (get-content state :party1 0)]
      (core/reveal state :contestant shannon)
      (is (zero? (count (:hand (get-contestant)))) "Contestant should have 0 cards in hand to start")
      (card-ability state :contestant shannon 0)
      (is (= "Ronin" (-> (get-contestant) :hand first :title)) "Contestant should draw Ronin with Shannon's click ability")
      (let [number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))]
        (card-ability state :contestant shannon 1)
        (prompt-card :contestant (find-card "Hostile Takeover" (:deck (get-contestant))))
        (is (= "Hostile Takeover" (-> (get-contestant) :deck last :title))
            "Agenda selected with Shannon's R&D ability should be on bottom of deck")
        (is (< number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))) "Searching R&D should shuffle deck")))
    (core/move state :contestant (find-card "Hostile Takeover" (:deck (get-contestant))) :discard)
    (core/move state :contestant (find-card "Shannon Claire" (:discard (get-contestant))) :hand)
    (play-from-hand state :contestant "Shannon Claire" "New party")
    (let [shannon (get-content state :party2 0)
          number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck))]
      (core/reveal state :contestant shannon)
      (card-ability state :contestant shannon 2)
      (prompt-card :contestant (find-card "Hostile Takeover" (:discard (get-contestant))))
      (is (= "Hostile Takeover" (-> (get-contestant) :deck last :title))
          "Agenda selected with Shannon's Archives ability should be on bottom of deck")
      (is (= number-of-shuffles (count (core/turn-events state :contestant :contestant-shuffle-deck)))
          "Searching Archives shouldn't shuffle deck"))))

(deftest team-sponsorship
  ;; Team Sponsorship
  (testing "Place from HQ"
    (do-game
      (new-game (default-contestant ["Domestic Sleepers"
                               "Team Sponsorship"
                               "Adonis Campaign"])
                (default-challenger))
      (play-from-hand state :contestant "Team Sponsorship" "New party")
      (play-from-hand state :contestant "Domestic Sleepers" "New party")
      (let [ag1 (get-content state :party2 0)
            tsp (get-content state :party1 0)]
        (core/reveal state :contestant tsp)
        (score-agenda state :contestant ag1)
        (prompt-select :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
        (prompt-choice :contestant "New party")
        (is (= "Adonis Campaign" (:title (get-content state :party3 0)))
            "Adonis placed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:hand (get-contestant)))) "No Adonis in hand"))))
  (testing "Place from Archives"
    (do-game
      (new-game (default-contestant ["Domestic Sleepers"
                               "Team Sponsorship"
                               "Adonis Campaign"])
                (default-challenger))
      (play-from-hand state :contestant "Team Sponsorship" "New party")
      (play-from-hand state :contestant "Domestic Sleepers" "New party")
      (discard-from-hand state :contestant "Adonis Campaign")
      (let [ag1 (get-content state :party2 0)
            tsp (get-content state :party1 0)]
        (core/reveal state :contestant tsp)
        (score-agenda state :contestant ag1)
        (prompt-select :contestant (find-card "Adonis Campaign" (:discard (get-contestant))))
        (prompt-choice :contestant "New party")
        (is (= "Adonis Campaign" (:title (get-content state :party3 0)))
            "Adonis placed by Team Sponsorship")
        (is (nil? (find-card "Adonis Campaign" (:discard (get-contestant)))) "No Adonis in discard"))))
  (testing "Multiple places"
    (do-game
      (new-game (default-contestant ["Domestic Sleepers"
                               (qty "Team Sponsorship" 2)
                               (qty "Adonis Campaign" 2)])
                (default-challenger))
      (play-from-hand state :contestant "Team Sponsorship" "New party")
      (play-from-hand state :contestant "Team Sponsorship" "New party")
      (play-from-hand state :contestant "Domestic Sleepers" "New party")
      (discard-from-hand state :contestant "Adonis Campaign")
      (let [ag1 (get-content state :party3 0)
            tsp2 (get-content state :party2 0)
            tsp1 (get-content state :party1 0)]
        (core/reveal state :contestant tsp1)
        (core/reveal state :contestant tsp2)
        (score-agenda state :contestant ag1)
        (prompt-choice :contestant "Team Sponsorship")
        (prompt-select :contestant (find-card "Adonis Campaign" (:discard (get-contestant))))
        (prompt-choice :contestant "New party")
        (prompt-select :contestant (find-card "Adonis Campaign" (:hand (get-contestant))))
        (prompt-choice :contestant "New party")
        (is (= "Adonis Campaign" (:title (get-content state :party4 0)))
            "Adonis placed by Team Sponsorship")
        (is (= "Adonis Campaign" (:title (get-content state :party5 0)))
            "Adonis placed by Team Sponsorship"))))
  (testing "Score 5 points in one window"
    (do-game
      (new-game (default-contestant [(qty "AstroScript Pilot Resource" 3)
                               "Team Sponsorship"
                               "Breaking News"
                               "SanSan City Grid"])
                (default-challenger))
      (play-from-hand state :contestant "SanSan City Grid" "New party")
      (core/gain state :contestant :credit 100 :click 5)
      (core/reveal state :contestant (get-content state :party1 0))
      (play-from-hand state :contestant "AstroScript Pilot Resource" "New party")
      (score-agenda state :contestant (get-content state :party2 0))
      (play-from-hand state :contestant "AstroScript Pilot Resource" "Locale 1")
      (play-from-hand state :contestant "Team Sponsorship" "New party")
      (core/reveal state :contestant (get-content state :party3 0))
      (score-agenda state :contestant (get-content state :party1 1))
      (prompt-select :contestant (find-card "AstroScript Pilot Resource" (:hand (get-contestant))))
      (is (zero? (get-counters (second (:scored (get-contestant))) :agenda)) "AstroScript not resolved yet")
      (prompt-choice :contestant "Locale 1")
      (is (= 1 (get-counters (second (:scored (get-contestant))) :agenda)) "AstroScript resolved")
      (card-ability state :contestant (first (:scored (get-contestant))) 0)
      (prompt-select :contestant (get-content state :party1 1))
      (card-ability state :contestant (second (:scored (get-contestant))) 0)
      (prompt-select :contestant (get-content state :party1 1))
      (core/score state :contestant {:card (get-content state :party1 1)})
      (prompt-select :contestant (find-card "Breaking News" (:hand (get-contestant))))
      (prompt-choice :contestant "Locale 1")
      (card-ability state :contestant (second (next (:scored (get-contestant)))) 0)
      (prompt-select :contestant (get-content state :party1 1))
      (core/score state :contestant {:card (get-content state :party1 1)})
      (prompt-choice :contestant "Done")
      (is (= 7 (:agenda-point (get-contestant))) "Scored 5 points in one turn"))))

(deftest tenma-line
  ;; Tenma Line - Swap 2 pieces of placed Character
  (do-game
    (new-game (default-contestant ["Tenma Line" "Harvester"
                             "Aimor" "Lockdown"])
              (default-challenger))
    (core/gain state :contestant :click 2)
    (play-from-hand state :contestant "Tenma Line" "New party")
    (play-from-hand state :contestant "Harvester" "HQ")
    (play-from-hand state :contestant "Aimor" "HQ")
    (play-from-hand state :contestant "Lockdown" "R&D")
    (core/reveal state :contestant (get-content state :rd 0))
    (core/reveal state :contestant (get-content state :party1 0))
    (is (= 1 (:click (get-contestant))))
    (card-ability state :contestant (get-content state :party1 0) 0)
    (prompt-select :contestant (get-character state :rd 0))
    (prompt-select :contestant (get-character state :hq 1))
    (is (empty? (:prompt (get-contestant))))
    (is (zero? (:click (get-contestant))) "Spent 1 click")
    (is (= "Aimor" (:title (get-character state :rd 0))) "Aimor swapped to R&D")
    (is (= "Lockdown" (:title (get-character state :hq 1))) "Lockdown swapped to HQ outer position")))

(deftest turtlebacks
  ;; Turtlebacks - Gain 1 credit for every new locale created
  (do-game
    (new-game (default-contestant ["Turtlebacks" (qty "PAD Campaign" 2) "Wraparound"])
              (default-challenger))
    (core/gain state :contestant :click 1)
    (play-from-hand state :contestant "Turtlebacks" "New party")
    (let [tb (get-content state :party1 0)]
      (core/reveal state :contestant tb)
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (is (= 4 (:credit (get-contestant))) "Gained 1 credit for new locale created")
      (play-from-hand state :contestant "Wraparound" "Locale 1")
      (is (= 4 (:credit (get-contestant))) "No credit gained for place into existing locale")
      (play-from-hand state :contestant "PAD Campaign" "New party")
      (is (= 5 (:credit (get-contestant))) "Gained 1 credit for new locale created"))))

(deftest warden-fatuma
  ;; Warden Fatuma - revealed bioroid character gains an additional sub
  (do-game
    (new-game (default-contestant ["Warden Fatuma" "Kakugo"
                             "Eli 2.0" "Ichi 2.0"])
              (default-challenger))
    (core/gain state :contestant :credit 20 :click 5)
    (play-from-hand state :contestant "Kakugo" "Archives")
    (play-from-hand state :contestant "Eli 2.0" "HQ")
    (play-from-hand state :contestant "Ichi 2.0" "R&D")
    (play-from-hand state :contestant "Warden Fatuma" "New party")
    (let [wf (get-content state :party1 0)
          kak (get-character state :archives 0)
          eli (get-character state :hq 0)
          ichi (get-character state :rd 0)]
      (core/reveal state :contestant kak)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo starts with 1 sub")
      (core/reveal state :contestant eli)
      (is (= 2 (count (:subroutines (refresh eli)))) "Eli 2.0 starts with 2 subs")
      (is (zero? (count (:subroutines (refresh ichi)))) "Unrevealed Ichi 2.0 starts with 0 subs")
      (core/reveal state :contestant wf)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 2.0 gains 1 sub")
      (is (zero? (count (:subroutines (refresh ichi)))) "Unrevealed Ichi 2.0 stays at 0 subs")
      (core/reveal state :contestant ichi)
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh eli)))) "Eli 2.0 stays at 1 sub")
      (is (= 3 (count (:subroutines (refresh ichi)))) "Ichi 2.0 reveales with 3 subs")
      (core/hide state :contestant (refresh wf))
      (is (= 1 (count (:subroutines (refresh kak)))) "Kakugo stays at 1 sub")
      (is (= 2 (count (:subroutines (refresh eli)))) "Eli 2.0 reverts")
      (is (= 2 (count (:subroutines (refresh ichi)))) "Ichi 2.0 reverts"))))

(deftest worlds-plaza
  ;; Worlds Plaza
  (do-game
    (new-game (default-contestant ["Worlds Plaza"
                             "Personalized Portal"
                             "Dedicated Response Team"
                             "Honeyfarm"])
              (default-challenger))
    (core/gain state :contestant :credit 10 :click 10)
    (play-from-hand state :contestant "Worlds Plaza" "New party")
    (let [plaza (get-content state :party1 0)]
      (core/reveal state :contestant plaza)
      (card-ability state :contestant plaza 0)
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant plaza 0)
        (prompt-select :contestant (find-card "Personalized Portal" (:hand (get-contestant))))
        (is (= (- credits 1) (:credit (get-contestant))) "Contestant should only spend 1 credit to reveal Personalized Portal"))
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant plaza 0)
        (prompt-select :contestant (find-card "Dedicated Response Team" (:hand (get-contestant))))
        (is (= credits (:credit (get-contestant))) "Contestant should spend 0 credit to reveal Dedicated Response Team"))
      (let [credits (:credit (get-contestant))]
        (card-ability state :contestant plaza 0)
        (prompt-select :contestant (find-card "Honeyfarm" (:hand (get-contestant))))
        (is (= credits (:credit (get-contestant))) "Contestant should spend 0 credit to reveal Honeyfarm")))))

(deftest zealous-judge
  ;; Zealous Judge
  (do-game
    (new-game (default-contestant ["Zealous Judge"])
              (default-challenger))
    (play-from-hand state :contestant "Zealous Judge" "New party")
    (let [judge (get-content state :party1 0)]
      (core/reveal state :contestant judge)
      (is (not (:revealed (refresh judge))) "Zealous Judge can't be revealed until Challenger is tagged")
      (core/gain state :challenger :tag 1)
      (core/reveal state :contestant judge)
      (is (:revealed (refresh judge)) "Zealous Judge can be revealed while the Challenger is tagged")
      (card-ability state :contestant judge 0)
      (is (= 2 (:tag (get-challenger))) "Challenger should gain a tag from Zealous Judge's ability"))))
