(ns web.utils
  (:require [ring.util.response :as resp]
            [monger.collection :as mc]
            [web.db :refer [db]]))

(defn tick
  "Call f with args every ms. First call will be after ms"
  [callback ms]
  (future
    (while true
      (do (Thread/sleep ms)
          (callback)))))

(defn map-values
  [m keys f]
  (reduce #(update-in %1 [%2] f) m keys))

(defn response [status-code msg]
  (-> (resp/response msg)
      (resp/status status-code)))

(defn remove-once [pred coll]
  (let [[head tail] (split-with (complement pred) coll)]
    (vec (concat head (rest tail)))))

(def df (java.text.SimpleDateFormat. "EEE MMM dd HH:mm:ss zzz yyyy"))

(defn my-value-reader [key value]
  (if (= key :gameid)
    (java.util.UUID/fromString value)
    (if (= key (or :date :started))
      (.parse df (str value))
      (if (= key (or :zone :previous-zone))
        (mapv keyword value)
        value))))

(defn my-value-writer [key value]
  (if (= key :gameid)
    (str value)
    (if (= key (or :date :started))
      (str value)
      value)))
