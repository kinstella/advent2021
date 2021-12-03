(ns advent.day3
  (:require [clojure.string :as str]))

(def data (slurp "resources/data/day3/input.data"))
(def binarystrings (str/split data #"\n"))

(defn gamma [bins]
  (let [strlen (count (first bins))]
    (str/join "" (map (fn [m]
                        (if (> (get m \0) (get m \1))
                          1
                          0)) (for [x (range strlen)]
                                (frequencies (map #(nth % x) bins)))))))

(defn gamma-to-epsilon
  "just flipping string bits"
  [s]
  (apply str (map #(if (= % \0)
                     "1"
                     "0") s)))

(defn process-part1 [s]
  (let [gamma (gamma s)
        eps (gamma-to-epsilon gamma)]
    (* (Integer/parseInt gamma 2)
       (Integer/parseInt  eps 2))))

(comment
  (process-part1 binarystrings)
  #_endcomment)