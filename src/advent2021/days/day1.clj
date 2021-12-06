(ns advent2021.days.day1
  (:require [clojure.string :as str]))

(def data (slurp "resources/data/day1/input.data"))
(def theints (map #(Integer/parseInt %) (str/split data #"\n")))
(defn part1 [numset]
  (loop [remset numset
         result 0]
    (if (empty? remset)
      result
      (recur (rest remset)
             (if
              (and (second remset)
                   (< (first remset) (second remset)))
               (do
                 (inc result))
               result)))))

(defn part2 [nums]
  (loop [remset nums
         inccounter 0
         lastsum nil]
    (if (empty? remset)
      inccounter
      (recur (rest remset)
             (if
              (and (second remset)
                   (not (nil? lastsum))
                   (< lastsum (apply + (take 3 remset))))
               (inc inccounter)
               inccounter)
             (apply + (take 3 remset))))))

(comment
  (part1 theints) ;;
  (part2 theints) ;;
  )