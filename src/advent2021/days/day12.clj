(ns advent2021.days.day12
  (:require [clojure.string :as str]))

; Oh Hai, graph theory...

;; (def example1-data (slurp "resources/data/day12/example1.data"))
;; (def lines (str/split-lines example1-data))

(def data (slurp "resources/data/day12/input.data"))
(def lines (str/split-lines data))

(def full-paths (atom []))
(def sm-cave-max (atom 1))

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

(defn max-visited? [v x]
  (> (count (filter #(= x %) v)) @sm-cave-max))

(defn visit-count [v x]
  (count (filter #(= x %) v)))

(defn find-goal [g goal pathsofar]
  (let [designated-double (atom {})
        neighbors (get g (last pathsofar))]
    (doall
     (for [x neighbors]
       (do
         (cond
           (= x goal);
           (reset! full-paths (conj @full-paths (conj pathsofar x)))

           ; if small, not start, and not visited more than once
           (and (sm-cave? x)
                (not (= x "start"))
                (not (max-visited? pathsofar x)))
           (do
             (if (empty? @designated-double)
               (reset! designated-double {x 1}))

             (find-goal g goal (conj pathsofar x)))

            ; if small, not start, and < max of a designated double
           (and (sm-cave? x)
                (not (= x "start"))
                (and
                 (get @designated-double x)
                 (< (get @designated-double x) 2)))
           (do
             (reset! designated-double {x 2})
             (find-goal g goal (conj pathsofar x)))

           (and (sm-cave? x)
                (not (= x "start"))
                (empty? @designated-double)
                (max-visited? pathsofar x))
           (do
             (reset! designated-double {x 1})
             (find-goal g goal (conj pathsofar x)))

           (big-cave? x)
           (find-goal g goal (conj pathsofar x))

           (and (sm-cave? x)
                (not (= x "start"))
                (max-visited? pathsofar x))
           (do (println "x visited too many times" x)
               (println "the designated double is empty? " (empty? @designated-double) @designated-double)
               (println "the pathsofar is:" pathsofar))))))))


(defn part-1 []
  (reset! sm-cave-max 1)
  (def g (adjacency-map lines))
  (def full-paths (atom []))
  (find-goal g "end" ["start"]))

(defn part-2 []
  (reset! sm-cave-max 1)
  (def g (adjacency-map lines))
  (def full-paths (atom []))
  (find-goal g "end" ["start"]))

(comment
  (def sm-cave-max (atom 1))

  @sm-cave-max

  (part-1)
  (part-2)
  (count @full-paths)


  (def some-atom (atom {}))

  (get @some-atom 100)

  (get @some-atom "a")

  (empty? @some-atom)

  #_endcomment)
