(ns advent2021.days.day7
  (:require [clojure.string :as str]))

;(def data (slurp "resources/data/day7/example.data"))
(def data (slurp "resources/data/day7/input.data"))
(def positions (map #(Integer/parseInt %) (str/split data #",")))

; Part 1 - constant cost
(defn assess-fuel-cost [positions num]
  (reduce + (map #(Math/abs (- num %))
                 positions)))

(defn lowest-fuel-cost-part1 []
  (reduce (fn [lowest new-num]
            (min lowest new-num))
          (for [x (range 0 (reduce max positions))]
            (assess-fuel-cost positions x))))

; Part 2 - increasing cost
(defn assess-increasing-cost [positions num]
  (reduce + (map #(let [d (Math/abs (- num %))]
                    (apply + (range 1 (inc d)))) positions)))

(defn lowest-fuel-cost-part2 []
  (reduce (fn [lowest new-num]
            (min lowest new-num))
          (for [x (range 0 (reduce max positions))]
            (assess-increasing-cost positions x))))

(comment
  (lowest-fuel-cost-part1)
  (lowest-fuel-cost-part2)

  #_endcomment)
