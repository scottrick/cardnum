(ns web.utils
  (:require [ring.util.response :as resp]
            [monger.collection :as mc]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [web.db :refer [db]]))

(def proj-dir (str (io/file (System/getProperty "user.dir"))))

(defn tick
  "Call f with args every ms. First call will be after ms"
  [callback ms]
  (future
    (while true
      (do (Thread/sleep ms)
          (callback)))))

(defn deaccent-strip
  "Remove diacritical marks from a string, from http://www.matt-reid.co.uk/blog_post.php?id=69"
  [s]
  (if (nil? s) ""
               (let [normalized (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)]
                 (string/replace (string/replace normalized #"\p{InCombiningDiacriticalMarks}+" "")
                                 #"(\bt+)(\w+)(.*)" ""))))

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
