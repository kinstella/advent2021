(ns advent2021.days.core
  (:require [advent2021.days.day1 :as day1]
            [advent2021.days.day2 :as day2]
            [advent2021.days.day3 :as day3]
            [advent2021.days.day4 :as day4]
            [advent2021.days.day5 :as day5]
            [advent2021.days.day6 :as day6]))

(defn init [])

(defn handle-run [day part]
  (case day
    "1"
    (case part
      "1" day1/part1
      "2" day1/part2)))
