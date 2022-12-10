(ns advent2021.days.day6
  (:require [clojure.string :as str]))

(def startingfish (mapv #(Integer/parseInt %)
                        (str/split
                         (slurp "resources/data/day6/input.data") #",")))

(def babyfish (atom 0))

(def fishperday (atom []))

(defn dec-fish [f]
  (let [newf (dec f)]
    (if (< newf 0)
      (do
        (swap! babyfish inc)
        6)
      newf)))

(defn fish-simulator [d startingfishv]
  (loop [rmdays (range 0 d)
         fishv startingfishv]
    (reset! babyfish 0)
    (swap! fishperday conj {(first rmdays) (count fishv)})
    (if (empty? rmdays)
      (println "count! " (count fishv))
      (let [nextfish (mapv dec-fish fishv)]
        (recur
         (rest rmdays)
         (reduce conj nextfish (vec (take @babyfish (repeat 8)))))))))


(defn initmap [freqmap]
  (reduce (fn [newm e]
            (if (nil? (get freqmap e))
              (conj newm {e 0})
              (conj newm {e (get freqmap e)})))
          {}
          (range 0 9)))

(defn fish-count-by-num [days startingfish]
  (loop [freqmap (initmap (frequencies startingfish))
         d 0]
    (println "day: " d " " freqmap)
    (if (>= d days)
      ;; total of fish
      (do
        (println "fmap: " freqmap)
        (reduce +  (vals freqmap)))
      ;; shift map to next day

      (let [newmap (reduce-kv (fn [newmap k v]
                                (if (= k 0)
                                  (conj newmap {6 v} {8 v})
                                  (conj newmap {(dec k) v})))
                              {}
                              freqmap)]
        (recur newmap
               (inc d))))))

(comment



  (fish-count-by-num 18 [3 4 3 1 2])
  (def startingmap {5 22, 1 209, 4 21, 2 29, 3 19})


  ; part 1

  #_endcomment)