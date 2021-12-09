(ns advent2021.days.day9
  (:require [clojure.string :as str]))
(def example-data (slurp "resources/data/day9/example.data"))
(def data (slurp "resources/data/day9/input.data"))
(def lines (str/split-lines data))

(defn risk-level [height]
  (+ 1 height))

(defn lowest-of-neighbors? [row col matrix]
  (let [curnum (aget matrix row col)
        neighbors (into [] [(when (> col 0)
                              (aget matrix row (dec col)))
                            (when (< col (dec (count (first matrix))))
                              (aget matrix row (inc col)))
                            (when (> row 0)
                              (aget matrix (dec row) col))
                            (when (< row (dec (count matrix)))
                              (aget matrix (inc row) col))])
        lowest (apply min (filter some? neighbors))]
    (< curnum lowest)))

(defn get-basin-neighbors [rowcol matrix]
  (let [[row col] rowcol
        neighbors [(when (> col 0)
                     (when (< (aget matrix row (dec col)) 9)
                       [row (dec col)]))
                   (when (< col (dec (count (first matrix))))
                     (when (< (aget matrix row (inc col)) 9)
                       [row (inc col)]))
                   (when (> row 0)
                     (when (< (aget matrix (dec row) col) 9)
                       [(dec row) col]))
                   (when (< row (dec (count matrix)))
                     (when (< (aget matrix (inc row) col) 9)
                       [(inc row) col]))]]
    (filter some? neighbors)))

(defn collect-basin [row-col matrix]
  (loop [searchset #{row-col}
         basinset #{}]
    (if (empty? searchset)
      basinset
      (let [nextneighbors (get-basin-neighbors (first searchset) matrix)]
        (recur (apply conj
                      (rest searchset)
                      (filter #(not (contains? basinset %)) nextneighbors))
               (conj basinset (first searchset)))))))

(defn find-lowest-points [matrix]
  (for [row (range 0 (count matrix))]
    (let [currow (aget matrix row)]
      (for [col (range 0 (count currow))]
        (when (lowest-of-neighbors? row col matrix)
          (conj (aget currow col)))))))

(defn find-lowest-rowcol [matrix]
  (for [row (range 0 (count matrix))]
    (let [currow (aget matrix row)]
      (for [col (range 0 (count currow))]
        (when (lowest-of-neighbors? row col matrix)
          (conj [row col]))))))

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

(defn part-2 [lines]
  (let [matrix (create-matrix lines)
        lowestrowcols (filter some?
                              (apply concat (find-lowest-rowcol matrix)))
        sizes (reduce (fn [counts next-rowcol]
                        (let [basin (collect-basin next-rowcol matrix)]
                          (conj counts (count basin))))
                      []
                      lowestrowcols)]
    (reduce * (take 3 (reverse (sort sizes))))))


(comment
  (part-1 lines)
  (part-2 lines)
  #_endcomment)