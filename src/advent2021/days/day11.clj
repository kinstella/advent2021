(ns advent2021.days.day11
  (:require [clojure.string :as str]))

(def example-data (slurp "resources/data/day11/example.data"))
(def data (slurp "resources/data/day11/input.data"))
(def lines (str/split-lines data))

; some atoms to make life easier and to invite the judgment of functional purists
(def flash-count (atom 0))
(def flashed-items (atom []))
(def flashed-in-step (atom #{}))
(def iterations (atom 0))

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
  (println "flashed in this step is:  " @flashed-in-step)
  (if (not (contains? @flashed-in-step [r c]))
    (do
      (reset! flashed-in-step (conj @flashed-in-step [r c]))
      (swap! flash-count inc) ; incr the global flash count
      (reset! flashed-items (conj (vec @flashed-items) [r c])) ; add point to global flashed items
      (println "added " [r c] " to to-be-flashed list..." @flashed-items))
    (println "This  " [r c] " has already flashed this step."))
  ; cycle through matrix until all flashing and adjacent flashing is done
  0)

(defn inc-point [m r c]
  ; if it flashed, then add it to flashlist 
  ; return new matrix
  m
  (let [retmatrix (if (and (in-matrix? m r c) (not (contains? @flashed-in-step [r c])))
                    (let [curval (get-at m r c)]
                      (if (< curval 9)
                        (assoc-in m [r c] (inc curval))
                        (assoc-in m [r c] (handle-flashing m r c))))
                    m ; not a valid matrix point, so just returning the matrix
                    )]
    retmatrix))

(defn inc-neighbors [m [r c]]
  ; increase all neighbors of flashed point. 
  ; return a matrix after they've all been increased
 ;;(println "increasing neighbors of: " [r c])
  (vec (-> m
           (inc-point (dec r) (inc c)) ; right-up
           (inc-point r (inc c))       ; right
           (inc-point (inc r) (inc c)) ; right-down
           (inc-point (inc r) c)       ; down
           (inc-point (inc r) (dec c)) ; left-down
           (inc-point r (dec c))       ; left
           (inc-point (dec r) (dec c)) ; left-up
           (inc-point (dec r) c))))     ; up

;; (defn deal-with-flashed-items [given-m]
;;   ; take a matrix, 
;;   (println "There are now " (count @flashed-items) " items flashed.")
;;   (reset! iterations 0)
;;   (let [finished-m
;;         (or (while (seq @flashed-items)
;;               (swap! iterations inc)
;;               (println "\n\niterations: " @iterations)
;;               (if (> @iterations 1000)
;;                 (do (display-matrix given-m)
;;                     (throw (Exception. (str "WTF too many iterations." @flashed-items)))))
;;               (reduce (fn [m item]
;;                         ;(println "within reduce loop, flashed-items is: " @flashed-items)
;;                         (let [increased-m (inc-neighbors m item)]
;;                           (reset! flashed-items (rest @flashed-items))
;;                           (println "after increasing neighbors, matrix is: " (display-matrix increased-m))
;;                           increased-m))
;;                       given-m ; starting with given-m
;;                       @flashed-items)) given-m)]
;;     (println "are we done with whle loop? returning finished-m\n " (display-matrix finished-m))
;;     finished-m))

(defn deal-with-flashed-items [given-m]
  ; take a matrix, 
  (println "There are now " (count @flashed-items) " items flashed.")
  (reset! iterations 0)
  (loop [cur-flashed @flashed-items
         m given-m]

    (println "to-be-flashed list is: " @flashed-items)
    (println "to-be-flashed count is: " (count @flashed-items))
    (println "cur flashed is: " cur-flashed)
    (if (empty? cur-flashed)
      m
      (do
        (println "current value of flashed item is: "
                 (get-at m
                         (first (first cur-flashed))
                         (last (first cur-flashed))))

        (let [new-matrix (inc-neighbors m (first cur-flashed))]
          (display-matrix new-matrix)
          (reset! flashed-items (rest @flashed-items))
          (println "to-be-flashed is now:" @flashed-items)
          (recur @flashed-items
                 new-matrix))))))

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
  (reduce
   (fn [newm step]
     (println "step " step)
     (if (= 100 (count @flashed-in-step))
       (throw (Exception. (str "flashed in step count is: " (count @flashed-in-step) ". We can stop now!"))))
     (reset! flashed-in-step #{})
     (-> newm
         (display-matrix)
         (inc-matrix)
         (deal-with-flashed-items)))
   m
   (range 0 s)))

(comment

  (def the-matrix (create-matrix lines))
  the-matrix

  (process-steps the-matrix 340)
  (reset! flashed-items  [])
  (reset! flash-count 0)

  (reset! flashed-items  [[0 0]])
  @flashed-items
  (deal-with-flashed-items the-matrix)
  (inc-point (inc-point the-matrix 0 2) 0 2)

  (conj #{} [1 2])

  (inc-neighbors the-matrix [1 8])

  (inc-matrix the-matrix)
  @flash-count
  #_endcomment)