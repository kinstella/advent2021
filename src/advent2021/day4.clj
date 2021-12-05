(ns advent.day4
  (:require [clojure.string :as str]))

(def lines (str/split-lines
            (slurp "resources/data/day4/input.data")))

(defn rm-vec-ws [a]
  (mapv #(str/split (str/trim %) #"\s+") a))

(defn lines-to-boards [lines]
  (->> (rest lines)
       (partition-by #(= "" %))
       (filter #(not= [""] %))
       (map rm-vec-ws)))

(defn has-bingo? [b]
     ;; concat all rows and cols as rows
  (let [all-valid-seqs
        (concat b (for [i (range (count (first b)))]
                    (mapv #(nth % i) b)))]
      ; are any just "X"?
    (> (count (filter
               (fn [r] (every? #(= "X" %) r)) all-valid-seqs)) 0)))

(defn board-sum [board]
  (->> board
       flatten
       (filter #(not= % "X"))
       (mapv #(Integer/parseInt %))
       (apply +)))

(defn replace-match-with-X
  "Just wanted to make it like real Bingo :D"
  [call board]
  (map (fn [row]
         (mapv #(if (= call %)
                  "X"
                  %) row)) board))

(defn find-winning-board [lines]
  (let [calls (str/split (first lines) #",")
        boards (lines-to-boards (rest lines))]
    (loop [rem-calls calls
           curboards boards
           lastcall nil]

      (let [call (first rem-calls)] ; testing each call
        (cond
          (some has-bingo? curboards)  ;;return product
          (* (Integer/parseInt lastcall) (board-sum (first (filter has-bingo? curboards))))

          ; failing base case
          (or (nil? call) (empty? rem-calls))
          (do (println "no bingo was found " curboards)
              nil)

          :else
          (recur (rest rem-calls)
                 (mapv
                  #(replace-match-with-X call %)
                  curboards)
                 call))))))
(comment
  (find-winning-board lines)

  #_endcomment)
