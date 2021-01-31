(ns game-test.cards.resources
  (:require [game.core :as core]
            [game.utils :refer [has?]]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "resources"))
