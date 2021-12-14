(ns advent2021.days.day6
  (:require [clojure.string :as str]))

(def startingfish (map #(Integer/parseInt %)
                       (str/split
                        (slurp "resources/data/day6/input.data") #",")))

(defn dec-fish [f]
  (let [newf (dec f)]
    (if (< newf 0)
      6
      newf)))

(defn simulate-fish [days startingfish]
  (reduce (fn [generation d]
            (let [nextfish (mapv dec-fish generation)
                  babycount (count (filter #(= 0 %) generation))]
              (into nextfish (take babycount (repeat 8)))))
          startingfish
          (range 0 days)))


(defn fish-spawned
  [days fnum]
  (let [remontimer (- fnum 6)]
    (quot (- days remontimer) 6)))

(comment

  ; part 1
  (count (simulate-fish 256 startingfish))


  #_endcomment)