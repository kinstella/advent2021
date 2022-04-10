(ns advent2021.days.day16
  (:require [clojure.string :as str :refer [split split-lines]]))

(def data (slurp "resources/data/day15/input.data"))
(def lines (str/split-lines data))

(def example-packet "110100101111111000101000
VVVTTTAAAAABBBBBCCCCC")

;;  First three bits - packet version -- 
;;      110 = 6
;; Second three bits - packet type ID -- 
;;      100 = 4