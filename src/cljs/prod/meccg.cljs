(ns prod.meccg
  (:require
    [meccg.main :as main]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(main/init!)
