(ns advent2021.days.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def example-data (slurp "resources/data/day8/example.data"))
(def data (slurp "resources/data/day8/input.data"))
(def lines (str/split-lines data))

(def known-pats [2 3 4 7])
; lettercount -> candidate digit
(def known-pats-map {2 1
                     3 7
                     4 4
                     7 8})
(def segs-for-digit {1 #{:a :b}
                     2 #{:d :a :f :g :c}
                     3 #{:d :a :f :b :c}
                     4 #{:e :f :a :b}
                     5 #{:d :e :f :b :c}
                     6 #{:d :e :f :b :c :g}
                     7 #{:a :b :d}
                     8 #{:a :b :c :d :e :f :g}
                     9 #{:d :e :a :f :b :c}
                     0 #{:d :a :b :c :g :e}})

(defn parse-signal-patterns [line]
  (let [[patterns outputs] (map str/trim (str/split line #"\|"))]
    {:patterns (str/split patterns #"\s+")
     :outputs (str/split outputs #"\s+")}))

;; Part 1

(defn count-known-digits [line]
  (let [outputs (:outputs (parse-signal-patterns line))]
    (count (filter (fn [ctval] (some #(= % ctval) known-pats))
                   (map count outputs)))))

(defn tabulate-part-one [lines]
  (reduce + (mapv #(count-known-digits %) lines)))

;; Part 2

(defn add-pat-to-candmap
  "Take a pattern, determine which digit it might be, then add letters into candidate segs"
  [pattern]
  (when-let [cand-dig (get known-pats-map (count pattern))]
    (println "candidate digit is: " cand-dig)
    (let [cand-map {:a #{} :b #{} :c #{} :d #{} :e #{} :f #{} :g #{}}
          cand-segs (get segs-for-digit cand-dig)]
      (println "cand map is: " cand-map)
      (reduce ; iterate through each pattern letter and 
       (fn [cur-map real-seg] ; associate the letter into the candidate map for each possible segment
         (println "realseg is: " real-seg)
         (println "realseg map: " (real-seg cur-map))

         (assoc-in cur-map [real-seg]
                   (merge (real-seg cur-map) (set pattern))))
       cand-map
       cand-segs))))

(defn collect-candidates [signals]
  (let [cand-map {:a #{} :b #{} :c #{} :d #{} :e #{} :f #{} :g #{}}]
    ; for each signal...
    (reduce
     (fn [cur-map new-sig]
       (let [candmap-for-sig (add-pat-to-candmap new-sig)]
         (println "for signal " new-sig " candidate map is now:" candmap-for-sig)
              ; if sig matches which-digit, then add each of its letters to test-candmap
         (conj cur-map candmap-for-sig)))
     (conj [] cand-map)
     signals)))

(defn process-line [line]
  (let [sigspats (parse-signal-patterns line)
        sigs (:outputs sigspats)
        _ (println "signals: " sigs)
        full-cand-map (collect-candidates sigs)]
    (println "full-cand-map: " full-cand-map)
    (map (fn [m]
           (println "the possible options for :b" (:a m))
           (:b m)) full-cand-map)))

(process-line "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
fdgacbe cefdb cefbgd gcbe")


(comment

  ; part 1
  (tabulate-part-one lines)

  ; part 2
  )