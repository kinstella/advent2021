(ns advent2021.days.day11
  (:require [clojure.string :as str]))

(def example-data (slurp "resources/data/day11/example.data"))
(def data (slurp "resources/data/day11/input.data"))
(def lines (str/split-lines data))

; some atoms to make life easier and to invite the judgment of functional purists
(def flash-count (atom 0))
(def flashed-items (atom []))
(def flashed-in-step (atom #{}))

(defn create-matrix [rows]
  (mapv (fn [i]
          (mapv #(Integer/parseInt %)
                (str/split i #""))) rows))

(defn display-matrix [m]
  (mapv #(println (str/join "" %)) m)
  m)

(defn in-matrix?
  [m x y]
  "bounds checking for matrix points"
  (and (> x -1)
       (> y -1)
       (< x (count m))
       (< y (count (nth m x)))))

(defn get-at [m x y]
  (nth (nth m x) y))

(defn handle-flashing [m r c]
  (if (not (contains? @flashed-in-step [r c]))
    (do
      (reset! flashed-in-step (conj @flashed-in-step [r c]))
      (swap! flash-count inc) ; incr the global flash count
      (reset! flashed-items (conj (vec @flashed-items) [r c])) ; add point to global flashed items
      )
    (println "This  " [r c] " has already flashed this step."))
  ; cycle through matrix until all flashing and adjacent flashing is done
  0)

(defn inc-point [m r c]
  ; if it flashed, then add it to flashlist - return new matrix
  (let [retmatrix (if (and (in-matrix? m r c) (not (contains? @flashed-in-step [r c])))
                    (let [curval (get-at m r c)]
                      (if (< curval 9)
                        (assoc-in m [r c] (inc curval))
                        (assoc-in m [r c] (handle-flashing m r c))))
                    m ; not a valid matrix point, so just returning the matrix
                    )]
    retmatrix))

(defn inc-neighbors
  " increase all neighbors of flashed point. 
   return a matrix after they've all been increased"
  [m [r c]]
  (vec (-> m
           (inc-point (dec r) (inc c)) ; right-up
           (inc-point r (inc c))       ; right
           (inc-point (inc r) (inc c)) ; right-down
           (inc-point (inc r) c)       ; down
           (inc-point (inc r) (dec c)) ; left-down
           (inc-point r (dec c))       ; left
           (inc-point (dec r) (dec c)) ; left-up
           (inc-point (dec r) c))))     ; up

(defn process-flashed-items
  "process list of flashed items"
  [given-m]
  (loop [cur-flashed @flashed-items
         m given-m]
    (if (empty? cur-flashed)
      m
      (let [new-matrix (inc-neighbors m (first cur-flashed))]
          ;(display-matrix new-matrix)
        (reset! flashed-items (rest @flashed-items))
        (recur @flashed-items
               new-matrix)))))

(defn inc-matrix
  "from starting matrix m, 
   return a matrix after incrementing each element"
  [m]
  (vec (for [x (range 0 (count m))]
         (vec (for [y (range 0 (count (nth m x)))]
                (if (< (get-at m x y) 9)
                  (inc (get-at m x y))
                  (do (handle-flashing m x y)
                      0)))))))

(defn process-steps [m s]
  (reset! flash-count 0)
  (reset! flashed-items [])
  (reset! flashed-in-step [])
  (let [final-m
        (reduce
         (fn [newm step]
           (if (= 100 (count @flashed-in-step))
             (do (println "STEP: " step)
                 (reduced newm))
             (do (reset! flashed-in-step #{})
                 (-> newm
               ;(display-matrix)
                     (inc-matrix)
                     (process-flashed-items)))))
         m
         (range 0 s))]
    (display-matrix final-m)))

(comment

  (def the-matrix (create-matrix lines))

; part 1  
  (process-steps the-matrix 100)
  @flash-count

  ; part 2
  (process-steps the-matrix 340)

  #_endcomment)