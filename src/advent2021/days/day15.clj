(ns advent2021.days.day15
  (:require [clojure.string :as str :refer [split split-lines]]))

(def split-data
  (mapv (fn [d]
          (mapv #(Integer/parseInt %)  (split d #"")))
        (split-lines (slurp "resources/data/day15/example.data"))))

; just some Djikstra
(defn create-matrix [d]
  ; read the data, create a 2d matrix
  )

(defn find-neighbors [pt m]
  ; for any point of coordinates x,y,
  ;  retrieve a set of the corresponding coords and values for each neighbors
  (let [max-r (count m)
        max-c (count (first m))
        [r c] pt]
    (conj #{}
          (when (< (inc c) max-c)
            {[r (inc c)] (nth (nth m r) (inc c))})
          (when (< (inc r) max-r)
            {[(inc r) c] (nth (nth m (inc r)) c)})
          (when (>= (dec r) 0)
            {[(dec r) c] (nth (nth m (dec r)) c)})
          (when  (>= (dec c) 0)
            {[r (dec c)] (nth (nth m r) (dec c))}))))

(defn djikstra-matrix [m start-pt end-pt]
  (let [max-r (count m)
        max-c (count (first m))
        [r c] start-pt]

    (loop []
      (let [neighbors (find-neighbors start-pt m)]

        (reduce
         (fn [min-cost n])

         neighbors)))))


;; def solve(matrix):
;;     last_row = len(matrix) - 1
;;     last_col = len(matrix[0]) - 1
;;     goal = (last_row, last_col)

;;     stack, closed_set = [], {(0, 0), }
;;     heapq.heappush(stack, (0, (0, 0)))

;;     while stack:
;;         cur_cost, cur_coords = heapq.heappop(stack)
;;         if cur_coords == goal:
;;             return cur_cost

;;         neighbors = [(y, x) for (y, x) in get_neighbors(*cur_coords)
;;                      if 0 <= y <= last_row and 0 <= x <= last_col
;;                      and (y, x) not in closed_set]
;;         for neighbor in neighbors:
;;             y, x = neighbor
;;             cost = cur_cost + matrix[y][x]

;;             node_in_stack = [n for n in stack if n[1] == neighbor]
;;             if not node_in_stack:
;;                 heapq.heappush(stack, (cost, neighbor))
;;             elif cost < node_in_stack[0][0]:
;;                 stack.pop(stack.index(node_in_stack[0]))
;;                 heapq.heappush(stack, (cost, neighbor))

;;         closed_set.add(cur_coords)