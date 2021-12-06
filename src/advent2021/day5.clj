(ns advent.day5
  (:require [clojure.string :as str]))

#_(def lines (str/split-lines
              (slurp "resources/data/day5/example.data")))
(def lines (str/split-lines
            (slurp "resources/data/day5/input.data")))

(def graph-atom (atom []))

(defn lines-to-segmaps [lines]
  (let [lines-to-vecs (map #(mapv
                             str/trim
                             (str/split % #"->")) lines)]
    (map (fn [v]
           (let [start (map #(Integer/parseInt %) (str/split (first v) #","))
                 end (map #(Integer/parseInt %)
                          (str/split (last v) #","))]
             {:start-x (first start)
              :start-y (last start)
              :end-x (first end)
              :end-y (last end)})) lines-to-vecs)))

(defn generate-graph [lines]
  (let [segs (lines-to-segmaps lines)]
    (let [max-x (reduce (fn [m s]
                          (max (:start-x s)
                               (:end-x s)
                               m)) 0 segs)
          max-y (reduce (fn [m s]
                          (max (:start-y s)
                               (:end-y s)
                               m)) 0 segs)]
      (reset! graph-atom
              (mapv (fn [x] (mapv (fn [y] ((constantly 0) x y))
                                  (range (inc max-y)))) (range (inc max-x)))))))

(defn points-from-seg [seg]
  (cond
    (= (:start-x seg) (:end-x seg)) ; horizontal segment...
    (for [y (range (min (:start-y seg) (:end-y seg))
                   (inc (max (:start-y seg) (:end-y seg))))]
      (conj [(:end-x seg) y]))

    (= (:start-y seg) (:end-y seg)) ; vertical segment
    (vec (zipmap
          (range (min (:start-x seg) (:end-x seg))
                 (inc (max (:start-x seg) (:end-x seg))))
          (repeat (:end-y seg))))
    :else ; something else
    nil))

(defn get-point [point]
  (nth
   (nth @graph-atom (second point))
   (first point)))

(defn plot-point [point s]
  (let [grid @graph-atom
        curvalue (get-point point)
        newvalue (inc curvalue)
        row-to-update (nth @graph-atom (second point))
        new-row (assoc row-to-update (first point) newvalue)]
    (assoc grid (second point) new-row)
    (reset! graph-atom (assoc grid  (second point) new-row))))

(defn plot-line [seg]
  (let [thepoints (points-from-seg seg)]
    (mapv #(plot-point % 1) thepoints)))

(defn plot-everything [d]
  (reset! graph-atom [])
  (generate-graph d)
  (let [allsegments (lines-to-segmaps d)]
    (mapv #(plot-line %) allsegments))
  (count (map dec (filter #(> % 1) (flatten @graph-atom)))))

(comment
  ;; how many functional programming points do I lose for using an atom here?

  ; part 1
  (plot-everything lines)

  #_endcomment)
