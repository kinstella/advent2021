(ns advent2021.days.day7
  (:require [clojure.string :as str]))

;(def data (slurp "resources/data/day7/example.data"))
(def data (slurp "resources/data/day7/input.data"))
(def positions (map #(Integer/parseInt %) (str/split data #",")))

; Part 1 - constant cost
(defn assess-fuel-cost [ps num]
  (reduce + (map #(Math/abs (- num %))
                 ps)))

(defn lowest-fuel-cost-part1 [ps]
  (reduce (fn [lowest new-num]
            (min lowest new-num))
          (for [x (range 0 (reduce max ps))]
            (assess-fuel-cost ps x))))

; Part 2 - increasing cost
(defn assess-increasing-cost [ps num]
  (reduce + (map #(let [d (Math/abs (- num %))]
                    (apply + (range 1 (inc d)))) ps)))

(defn lowest-fuel-cost-part2 [ps]
  (reduce (fn [lowest new-num]
            (min lowest new-num))
          (for [x (range 0 (reduce max ps))]
            (assess-increasing-cost ps x))))

(comment
  (lowest-fuel-cost-part1 positions)
  (lowest-fuel-cost-part2 positions)

  #_endcomment)
