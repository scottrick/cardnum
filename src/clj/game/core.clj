(ns game.core
  (:require [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clj-time.core :as t]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cardnum.utils :refer [str->int parse-deck-string INFINITY] :as utils]
            [cardnum.cards :refer [all-cards]]))

(load "core/events")    ; triggering of events
(load "core/cards")     ; retrieving and updating cards
(load "core/costs")     ; application of costs to play
(load "core/rules")     ; core game rules
(load "core/turns")     ; the turn sequence
(load "core/actions")   ; functions linked to UI actions
(load "core/abilities") ; support for card abilities and prompts
(load "core/placing")   ; placing and interacting with placed cards and locales
(load "core/hosting")   ; hosting routines
(load "core/runs")      ; the run sequence
(load "core/character") ; character interactions
(load "core/flags")     ; various miscellaneous manipulations of specific effects
(load "core/io")        ; routines for parsing input or printing to the log
(load "core/misc")      ; misc stuff
(load "cards")          ; card definitions
