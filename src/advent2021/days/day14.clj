(ns advent2021.days.day14
  (:require [clojure.string :as str :refer [split split-lines]]))

;(def split-data (split (slurp "resources/data/day14/example.data") #"\n\n"))
(def split-data (split (slurp "resources/data/day14/input.data") #"\n\n"))
(def poly-template (first split-data))
(def rules (mapv #(split % #" -> ") (split-lines (second split-data))))

(defn build-swaps [rules]
  "create a map of the pair to match, mapped to a full replacement string"
  (into {} (mapv (fn [[needle newstr]]
                   {needle (str (first needle) newstr  (last needle))}) rules)))

(defn pair-insertion [t rules]
  (loop [word-part t
         new-word ""]
    (let [a-pair (str (first word-part) (second word-part))
          first-segment (if (> (count new-word) 1)
                          (subs new-word 0 (dec (count new-word))))]
      (if (< (count word-part) 1)
        new-word
        (recur (rest word-part)
               (if (get rules a-pair)
                 (str first-segment (get rules a-pair))
                 new-word))))))

(defn run-steps [n rules]
  (let [new-rules (build-swaps rules)
        final-word (reduce
                    (fn [new-word x]
                      (println "Step " (inc x))
                      (pair-insertion new-word new-rules))
                    poly-template
                    (vec (range 0 n)))]
    (let [freqs (frequencies final-word)
          maxfreq (reduce max (vals freqs))
          minfreq (reduce min (vals freqs))]
      (- maxfreq minfreq))))

(comment

  ; part 1
  (run-steps 10 rules)

  #_endcomment)