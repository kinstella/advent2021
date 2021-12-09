(ns advent2021.days.day9
  (:require [clojure.string :as str]))

(def example-data (slurp "resources/data/day9/example.data"))
(def data (slurp "resources/data/day9/input.data"))
(def lines (str/split-lines data))

(defn risk-level [height]
  (+ 1 height))

(defn lowest-of-neighbors? [row col matrix]
  (let [curnum (nth (nth matrix row) col)
        neighbors (into [] [(when (> col 0)
                              (nth (nth matrix row) (dec col)))
                            (when (< col (dec (count (first matrix))))
                              (nth (nth matrix row) (inc col)))
                            (when (> row 0)
                              (nth (nth matrix (dec row)) col))
                            (when (< row (dec (count matrix)))
                              (nth (nth matrix (inc row)) col))])
        lowest (apply min (filter some? neighbors))]
    (< curnum lowest)))

(defn find-lowest-points [matrix]
  (for [row (range 0 (count matrix))]
    (let [currow (nth matrix row)]
      (for [col (range 0 (count currow))]
        (do
          (when (lowest-of-neighbors? row col matrix)
            (conj (nth currow col))))))))

(defn create-matrix [lines]
  (mapv (fn [i]
          (map #(Integer/parseInt %) (str/split i #""))) lines))

(defn part-1 [lines]
  (let [matrix (create-matrix lines)]
    (reduce +
            (mapv risk-level
                  (filter some?
                          (flatten (find-lowest-points matrix)))))))

(comment
  (part-1 lines)
  #_endcomment)