(ns advent2021.days.day2
  (:require [clojure.string :as str]))

(def data (slurp "resources/data/day2/input.data"))
(def commands (str/split data #"\n"))

(def hpos (atom 0))
(def depth (atom 0))
(def aim (atom 0))

(defn interpet-part1 [inst]
  (let [[command arg] (str/split inst #" ")
        arg (Integer/parseInt arg)]
    (println "Command: " command " arg: " arg)
    (case command
      "forward"
      (do
        (swap! hpos (partial + arg)))
      "up" (reset! depth (- @depth arg))
      "down" (swap! depth (partial + arg))
      (println "got unrecognized command " command))))

(defn process-part1 [c]
  (reset! hpos 0)
  (reset! depth 0)
  (doall (map interpet-part1 c))
  {:hpos @hpos :depth @depth})

;; Part 2

(defn interpet-part2 [inst]
  (let [[command arg] (str/split inst #" ")
        arg (Integer/parseInt arg)]
    (case command
      "forward" (do
                  (swap! hpos (partial + arg))
                  (reset! depth (+ @depth (* @aim arg))))
      "up" (reset! aim (- @aim arg))
      "down" (reset! aim (+ @aim arg))
      (println "unrecognized command: " command))))

(defn process-part2 [c]
  (reset! hpos 0)
  (reset! depth 0)
  (reset! aim 0)
  (doall (map interpet-part2 c))
  {:hpos @hpos :depth @depth})

(comment
  (let [m (process-part2 commands)]
    (* (:hpos m) (:depth m)))


  #_endcomment)
