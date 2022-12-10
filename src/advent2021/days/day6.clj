(ns advent2021.days.day6
  (:require [clojure.string :as str]))

(def startingfish (mapv #(Integer/parseInt %)
                        (str/split
                         (slurp "resources/data/day06/input.txt") #",")))

(defn initmap [freqmap]
  (reduce (fn [newm e]
            (if (nil? (get freqmap e))
              (conj newm {e 0})
              (conj newm {e (get freqmap e)})))
          {}
          (range 0 9)))

(defn fish-count-by-num [days startingfish]
  (loop [freqmap (initmap (frequencies startingfish))
         d 0]
    (if (>= d days)
      ;; total of fish
      (reduce + (vals freqmap))
      ;; shift map to next day
      (let [newmap (reduce (fn [newmap k]
                             (assoc newmap (dec k) (+
                                                    (get freqmap k))))
                           {}
                           (range 9 -1 -1))
            newmap (assoc newmap
                          8
                          (get newmap -1)
                          6 (+
                             (get newmap 6)
                             (get newmap -1))
                          -1 0)]
        (recur newmap
               (inc d))))))

(comment
  
  (fish-count-by-num 256 startingfish)
  
  #_endcomment)