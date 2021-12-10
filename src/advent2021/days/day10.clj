(ns advent2021.days.day10
  (:require [clojure.string :as str]))

(def example-data (slurp "resources/data/day10/example.data"))
(def data (slurp "resources/data/day10/input.data"))
(def lines (str/split-lines example-data))

(def bracket-map {"[" "]"
                  "(" ")"
                  "{" "}"
                  "<" ">"})

(def score-map {")" 3
                "]" 57
                "}" 1197
                ">" 25137})

(defn dothing [charvec])

(defn parse-line [charvec]
  (let [opening-brackets (set (keys bracket-map))
        closing-brackets (set (vals bracket-map))]
    (loop [remchars charvec
           curstack []
           lastitem nil]
      (cond
        (empty? remchars) ; ran out of chars to parse 
        {:endstack curstack
         :illegal-char nil}

        (nil? curstack) ; got an illegal-character
        {:endstack curstack
         :illegal-char lastitem}

        :else
        (let [curitem (first remchars)]
          (recur (rest remchars)

                 (cond (contains? opening-brackets curitem) ; opening bracket? add it to the stack
                       (conj curstack curitem)

                       (contains? closing-brackets curitem) ; closing bracket?
                       (if
                        (= (get bracket-map (last curstack)) curitem) ; is it the expected closing bracket?
                         (pop curstack) ; then remove that last item from the stack

                         (do (println "illegal character!" curitem) ; it not, then it's illegal
                             nil))
                       :else ;; uh, found something else entirely
                       (println "found neither type of bracket !?\n"))
                 curitem))))))

(defn process-line [line]
  (let [splitline (str/split line #"")]
    (parse-line splitline)))

(defn part-1 [lines]
  (let [illegal-chars
        (map :illegal-char (filter #(some? (:illegal-char %)) (mapv process-line lines)))]
    ; then get the sum of scores
    (reduce + (map #(get score-map %) illegal-chars))))

; part two things...
(def completion-scores {")" 1
                        "]" 2
                        "}" 3
                        ">" 4})

(defn score-completion-string [remstack]
  (reduce (fn [tab curitem]
            (+ (* 5 tab) (get completion-scores curitem)))
          0
          remstack))

(defn part-2 [lines]
  (let [rem-stacks ; find remaining stacks at completion of parsing
        (map :endstack (filter #(some? (:endstack %)) (mapv process-line lines)))
    ; reverse those and score the chars
        completion-scores (mapv
                           (fn [rs]
                             (score-completion-string
                              (map #(get bracket-map %) (reverse rs)))) rem-stacks)]
    ; and get the middle score of final scores
    (nth (sort completion-scores) (/ (count completion-scores) 2))))

(comment

  (part-1 lines)
  (part-2 lines)

  #_endcomment)
