(ns advent2021.days.day4
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
          (println "no bingo was found " curboards)

          :else
          (recur (rest rem-calls)
                 (mapv
                  #(replace-match-with-X call %)
                  curboards)
                 call))))))

;; TODO: could use reduce instead of doing all this
(defn sum-before-bingo [board calls]
  (loop [rem-calls calls
         newboard board
         lastcall nil]
    (if (has-bingo? newboard)
      (* (Integer/parseInt lastcall)
         (board-sum newboard))
      (recur (rest rem-calls)
             (replace-match-with-X
              (first rem-calls) newboard)
             (first rem-calls)))))

;; similar to find-winning-board, but lets remove each winning board until we have one board left
(defn find-last-winning [lines]
  (let [calls (str/split (first lines) #",")
        boards (lines-to-boards (rest lines))]
    (loop [rem-calls calls
           curboards boards
           lastcall nil]

      (let [call (first rem-calls)] ; testing each call
        (cond
          (= 1 (count curboards)) ; down to the last board...
          (sum-before-bingo (last curboards) rem-calls)

          ; failing base case
          (or (nil? call) (empty? rem-calls))
          (println "no bingo was found " curboards)

          :else
          (let [assessed-boards (mapv
                                 #(replace-match-with-X call %)
                                 curboards)]
            (recur (rest rem-calls)
                   (filter #(not (has-bingo? %)) assessed-boards)
                   call)))))))

(comment
  (find-winning-board lines)
  (find-last-winning lines)
  #_endcomment)
