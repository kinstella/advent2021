(ns advent2021.days.day9
  (:require [clojure.string :as str]))

(def example-data (slurp "resources/data/day9/example.data"))
(def data (slurp "resources/data/day9/input.data"))
(def lines (str/split-lines data))

(defn risk-level [height]
  (+ 1 height))

(defn lowest-of-neighbors? [row col matrix]
  (let [curnum (aget matrix row col)
        neighbors [(when (> col 0) (aget matrix row (dec col)))
                   (when (< col (dec (alength matrix))) (aget matrix row (inc col)))
                   (when (> row 0) (aget matrix (dec row) col))
                   (when (< row (dec (count matrix))) (aget matrix (inc row) col))]
        lowest (apply min (filter some? neighbors))]
    (< curnum lowest)))

(defn find-lowest-points [matrix]
  (for [row (range 0 (alength matrix))]
    (for [col (range 0 (alength (aget matrix row)))]
      (when (lowest-of-neighbors? row col matrix)
        (aget matrix row col)))))

(defn create-matrix [lines]
  (to-array-2d
   (mapv (fn [i]
           (mapv #(Integer/parseInt %)
                 (str/split i #""))) lines)))

(defn part-1 [lines]
  (let [matrix (create-matrix lines)]
    (reduce +
            (mapv risk-level
                  (filter some?
                          (flatten (find-lowest-points matrix)))))))

(comment
  (part-1 lines)

  #_endcomment)