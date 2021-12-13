(ns advent2021.days.day12
  (:require [clojure.string :as str]))

; Oh Hai, graph theory...

;; (def example1-data (slurp "resources/data/day12/example1.data"))
;; (def lines (str/split-lines example1-data))

(def data (slurp "resources/data/day12/input.data"))
(def lines (str/split-lines data))

(def full-paths (atom []))

(defn big-cave? [point]
  (not (= point (str/lower-case point))))

(defn sm-cave? [point]
  (= point (str/lower-case point)))

(defn get-adjacent [point edges]
  (filter some? (mapv (fn [[e1 e2]] (cond (= e1 point) e2
                                          (= e2 point) e1
                                          :else nil)) edges)))

(defn adjacency-map [lines]
  (let [edges (into #{} (mapv #(str/split % #"-") lines))
        vertices (into #{} (concat (mapv last edges) (mapv first edges)))]
    (reduce (fn [adj v]
              (merge adj {v (vec (get-adjacent v edges))}))
            {}
            vertices)))

(defn in-vec? [v x]
  (some #(= x %) v))

(defn find-goal [g goal pathsofar]
  (let [neighbors (get g (last pathsofar))]
    (doall
     (for [x neighbors]
       (do
         (cond
           (= x goal);
           (reset! full-paths (conj @full-paths (conj pathsofar x)))

           (sm-cave? x)
           (if (in-vec? pathsofar x)
             nil
             (find-goal g goal (conj pathsofar x)))

           (big-cave? x)
           (find-goal g goal (conj pathsofar x))

           :else
           (println "x is...?!" x)))))))

(defn part-1 []
  (def g (adjacency-map lines))
  (def full-paths (atom []))
  (find-goal g "end" ["start"]))

(comment

  (part-1)

  #_endcomment)
