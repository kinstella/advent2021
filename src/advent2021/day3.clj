(ns advent.day3
  (:require [clojure.string :as str]))

(def data (slurp "resources/data/day3/input.data"))
(def binarystrings (str/split data #"\n"))

(defn gamma [bins]
  (let [strlen (count (first bins))]
    (apply str (map (fn [m]
                      (if (> (get m \0) (get m \1)) 1 0))
                    (for [x (range strlen)]
                      (frequencies (map #(nth % x) bins)))))))

(defn gamma-to-epsilon
  "just flipping string bits"
  [s]
  (apply str (map #(if (= % \0) "1" "0") s)))

(defn power-consumption [s]
  (let [gamma (gamma s)
        eps (gamma-to-epsilon gamma)]
    (* (Integer/parseInt gamma 2)
       (Integer/parseInt  eps 2))))

;;; part 2

(defn limit-string [bins place valtofind]
  (let [freqcount (frequencies (map #(nth % place) bins))
        filter-digit (case valtofind
                       :oxygen-gen
                       (if (> (get freqcount \0) (get freqcount \1)) \0 \1)
                       :co2-scrubber
                       (if (> (get freqcount \0) (get freqcount \1)) \1 \0))]
    (filter #(= filter-digit (nth % place)) bins)))

(defn find-val [valtofind bins]
  (loop [x 0
         stringset bins]
    (if (= 1 (count stringset))
      (first stringset)
      (recur
       (inc x)
       (limit-string stringset x valtofind)))))

(defn calc-life-support-rating [bins]
  (* (Integer/parseInt (find-val :oxygen-gen bins) 2)
     (Integer/parseInt (find-val :co2-scrubber bins) 2)))

(comment
  ;; part 1:
  (power-consumption binarystrings)

  ;; part 1:
  (calc-life-support-rating binarystrings)

  #_endcomment)
