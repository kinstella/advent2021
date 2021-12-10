(ns advent2021.core
  (:require
   [advent2021.days.core :as days])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print "What day do you want to run? ")
  (flush)
  (let [picked-day (read-line)]
    (println "Day " picked-day)
    (print "And what part? ")
    (flush)
    (let [picked-part (read-line)]
      (print "Part " picked-part)
      (days/handle-run picked-day picked-part))))
