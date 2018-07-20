(ns ^:figwheel-no-load dev.meccg
  (:require
    [meccg.main :as main]
    [devtools.core :as devtools]))

(enable-console-print!)

(devtools/install!)

(main/init!)