(ns advent2021.days.day13
  (:require [clojure.string :as str]))

;; (def data (slurp "resources/data/day13/example.data"))
;; (def paper (str/split data #"\n\n"))

(def data (slurp "resources/data/day13/input.data"))
(def paper (str/split data #"\n\n"))

(def dots
  (sort (map (fn [r]
               (let [[x y] (str/split r #",")]
                 [(Integer/parseInt x)
                  (Integer/parseInt y)])) (str/split-lines (first paper)))))


(defn count-dots [m]
  (count (filter #(= % "#") (flatten m))))


(def folds (map
            #(str/split (last (str/split % #"fold along ")) #"=")
            (str/split-lines (last paper))))


(defn create-matrix [d]
  (let [max-x (inc (apply max (map first dots)))
        max-y (inc (apply max (map last dots)))]
    (println "maxx: " max-x " maxy" max-y)
    (doall (mapv (fn [y]
                   (doall
                    (mapv (fn [x]
                            (if (some #(= % [x y]) dots)
                              (do
                                "#")
                              y))  (range 0 max-x)))) (range 0 max-y)))))

(defn coalesce-row [m y1 y2]
  (mapv (fn [a b] (if (or (= a "#") (= b "#"))
                    "#"
                    "."))
        (nth m y1) (nth m y2)))

(defn fold-at-y [m given-y]
  (reverse
   (vec (for [y (range (inc given-y) (count m))]
          (coalesce-row m y (- (count m) (inc y)))))))

(defn fold-at-x [m given-x]
  (let [max-x (count (nth m 0))]
    (for [y (range 0 (count m))]
      (for [x (range 0 given-x)]
        (if (or (= (nth (nth m y) x) "#")
                (= (nth (nth m y) (- max-x (inc x))) "#"))
          "#"
          ".")))))

(defn display-matrix [m]
  (mapv #(println %) m))


(defn perform-folds [folds m]
  (reduce (fn [end-m fold]
            (let [[folddir foldat] fold]
              (if (= folddir "x")
                (fold-at-x end-m (Integer/parseInt foldat))
                (fold-at-y end-m (Integer/parseInt foldat)))))
          m
          folds))

(comment

  (def matrix (create-matrix dots))
  (count matrix)
  (count (nth matrix 0))

; part 1
  (def folded-at-x-655 (fold-at-x matrix 655))
  (count-dots folded-at-x-655)

; part 2
  (def final-matrix (perform-folds folds matrix))
  (display-matrix final-matrix)

  #_endcomment)

