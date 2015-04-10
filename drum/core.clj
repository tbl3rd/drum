(ns drum.core
  (:require [clojure.java.io :as jio]
            [clojure.pprint]
            [gloss.core :as gc]
            [gloss.io :as gio]))

(def bytes4 [:byte :byte :byte :byte])
(def bytes8 [:byte :byte :byte :byte :byte :byte :byte :byte])
(def hexify (partial map (partial format "%02x")))

;; To explore the binary fixture files.  (I kept only the useful ones.)
;;
(defn int32      [x] (hexify (gio/decode bytes4 (gio/encode :int32 x))))
(defn int64      [x] (hexify (gio/decode bytes8 (gio/encode :int64 x))))
(defn float32    [x] (hexify (gio/decode bytes4 (gio/encode :float32 x))))
(defn float32-le [x] (hexify (gio/decode bytes4 (gio/encode :float32-le x))))

(float32 98.4)   ;;=> ("42" "c4" "cc" "cd")
(float32-le 118) ;;=> ("00" "00" "ec" "42")
(int64 143)      ;;=> ("00" "00" "00" "00" "00" "00" "00" "8f")

(defn get-bytes
  "Return the bytes in FILE."
  [file]
  (let [file (jio/file file)
        size (.length file)
        result (byte-array size)]
    (with-open [reader (jio/input-stream file)]
      (.read reader result 0 size)
      result)))

(defn read-fixture
  "The map from decoding the fixture in FILE.
  (trim s) is just s without trailing nuls.
  One file has junk in it: hence (gio/decode ... false)."
  [file]
  (let [track (gc/ordered-map
               :id :ubyte
               :pad (gc/finite-frame :int32 (gc/string :utf-8))
               :steps [bytes4 bytes4 bytes4 bytes4])
        payload (gc/ordered-map
                 :version (gc/string :utf-8 :length 32)
                 :tempo :float32-le
                 :tracks (gc/repeated track :prefix :none))
        frame (gc/ordered-map
               :splice (gc/string :utf-8 :delimiters ["SPLICE"])
               :payload (gc/finite-frame :int64 payload)
               :junk (gc/repeated :byte :prefix :none))
        raw (gio/decode frame (get-bytes file) false)]
    (letfn [(trim [s]
              (let [n (.indexOf s (str (char 0)))]
                (if (= -1 n) s (subs s 0 n))))]
      (update-in raw [:payload :version] trim))))

(defn read-fixtures
  "Read fixture files from directory."
  [directory]
  (for [file (file-seq (jio/file directory))
        :let [name (.getName file)]
        :when (.endsWith name ".splice")]
    (let [fixture (read-fixture file)]
      (when (not (empty? (:junk fixture)))
        (println "Warning: the" name "file has junk in it.")
        (clojure.pprint/pprint fixture))
      (assoc fixture :file name))))

(defn show-track
  "Show TRACK as a string."
  [{:keys [id pad steps] :as track}]
  (letfn [(bar [s] (apply str (map ["-" "x"] (map int s))))]
    (apply (partial format "%-15s |%s|%s|%s|%s|")
           (format "(%d) %s" id pad)
           (map bar steps))))

(defn show-pattern
  "Show PATTERN as a string."
  [{:keys [file payload junk] :as pattern}]
  (let [{:keys [version tempo tracks]} payload]
    (apply vector
           file
           (str "Saved with HW Version: " version)
           (str "Tempo: " (if (== tempo (int tempo))
                            (format "%d" (int tempo))
                            (format "%s" tempo)))
           (map show-track tracks))))

(def result
  (vec (map show-pattern (read-fixtures "go/fixtures"))))

(def expected-output
  "The expected string output from the decoder running over 'go/fixtures'."
  [["pattern_1.splice"                            
    "Saved with HW Version: 0.808-alpha"
    "Tempo: 120"
    "(0) kick        |x---|x---|x---|x---|"
    "(1) snare       |----|x---|----|x---|"
    "(2) clap        |----|x-x-|----|----|"
    "(3) hh-open     |--x-|--x-|x-x-|--x-|"
    "(4) hh-close    |x---|x---|----|x--x|"
    "(5) cowbell     |----|----|--x-|----|"]
   ["pattern_2.splice"
    "Saved with HW Version: 0.808-alpha"
    "Tempo: 98.4"
    "(0) kick        |x---|----|x---|----|"
    "(1) snare       |----|x---|----|x---|"
    "(3) hh-open     |--x-|--x-|x-x-|--x-|"
    "(5) cowbell     |----|----|x---|----|"]
   ["pattern_3.splice"
    "Saved with HW Version: 0.808-alpha"
    "Tempo: 118"
    "(40) kick       |x---|----|x---|----|"
    "(1) clap        |----|x---|----|x---|"
    "(3) hh-open     |--x-|--x-|x-x-|--x-|"
    "(5) low-tom     |----|---x|----|----|"
    "(12) mid-tom    |----|----|x---|----|"
    "(9) hi-tom      |----|----|-x--|----|"]
   ["pattern_4.splice"
    "Saved with HW Version: 0.909"
    "Tempo: 240"
    "(0) SubKick     |----|----|----|----|"
    "(1) Kick        |x---|----|x---|----|"
    "(99) Maracas    |x-x-|x-x-|x-x-|x-x-|"
    "(255) Low Conga |----|x---|----|x---|"]
   ["pattern_5.splice"
    "Saved with HW Version: 0.708-alpha"
    "Tempo: 999"
    "(1) Kick        |x---|----|x---|----|"
    "(2) HiHat       |x-x-|x-x-|x-x-|x-x-|"]])

(str "You " (if (= expected-output result) "WIN" "LOSE") "!")
