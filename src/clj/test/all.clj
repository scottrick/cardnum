(ns test.all
  (:require [clojure.test :refer :all]
            test.core
            test.cards.agendas
            test.cards.assets
            test.cards.events
            test.cards.hazard
            test.cards.character
            test.cards.icebreakers
            test.cards.identities
            test.cards.operations
            test.cards.resources
            test.cards.resources
            test.cards.upgrades
            test.games.scenarios))

(deftest all-tests
  (run-tests 'test.core)
  (run-all-tests #"test.cards.*")
  (run-all-tests #"test.games.*"))
