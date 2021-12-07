(ns advent2021.days.day6
  (:require [clojure.string :as str]))

(def startingfish (map #(Integer/parseInt %)
                       (str/split
                        (slurp "resources/data/day6/example.data") #",")))

(defn dec-fish [f]
  (let [newf (dec f)]
    (if (< newf 0)
      6
      newf)))

(defn simulate-fish [days startingfish]
  (loop [d (doall (range 1 days))
         curfish startingfish]
    (println "day: " (first d) " count: " (count curfish))
    (let [nextfish (map dec-fish curfish)
          babycount (count (filter #(= 0 %) curfish))
          nextfish (concat nextfish (take babycount (repeat 8)))]
      (if (< (count d)  1)
        nextfish
        (recur (rest d)
               nextfish)))))

(defn fish-spawned
  [days fnum]
  (let [remontimer (- fnum 6)]
    (quot (- days remontimer) 6)))

(comment

  ; part 1
  (count (simulate-fish 256 startingfish))



  #_endcomment)