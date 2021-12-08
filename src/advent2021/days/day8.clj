(ns advent2021.days.day8
  (:require [clojure.string :as str]))

(def example-data (slurp "resources/data/day8/example.data"))
(def data (slurp "resources/data/day8/input.data"))
(def lines (str/split-lines data))

(def known-pats [2 3 4 7])

(defn parse-signal-patterns [line]
  (let [[patterns outputs] (map str/trim (str/split line #"\|"))]
    {:patterns (str/split patterns #"\s+")
     :outputs (str/split outputs #"\s+")}))

(defn count-known-digits [line]
  (let [outputs (:outputs (parse-signal-patterns line))]
    (count (filter (fn [ctval] (some #(= % ctval) known-pats))
                   (map count outputs)))))

(defn tabulate-part-one [lines]
  (reduce + (mapv #(count-known-digits %) lines)))
