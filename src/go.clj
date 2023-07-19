(ns go
  (:require [graph :as g]))

(defn make-board [graph size]
  {:board graph :size size})

; TODO kill stones
(defn update-board [board [i j]]
  board)
  ;(if (<= 0 i))
  ;(-> board
  ;    (update-board [(dec i) j])
  ;    (update-board [i (dec j)])
  ;    (update-board [(inc i) j])
  ;    (update-board [i (inc j)]))

; TODO do not let place a stone if it's a suicide
(defn put-stone [board position color]
  (let [graph (:board board)
        board (g/set-node graph position color)
        board (assoc board :board board)]
    (update-board board position)))

(defn get-at [board position]
  (g/get-value (:board board) position))

(defn square-board-board [size]
  (let [graph (atom (g/make-graph))]
    (doseq [i (range size)]
      (doseq [j (range size)]
        (swap! graph g/set-node [i j] nil)
        (when (< 0 i)
          (let [prev-i (dec i)]
            (swap! graph g/add-edge [i j] [prev-i j])
            (swap! graph g/add-edge [prev-i j] [i j])))
        (when (< 0 j)
          (let [prev-j (dec j)]
            (swap! graph g/add-edge [i j] [i prev-j])
            (swap! graph g/add-edge [i prev-j] [i j])))))
    (make-board @graph size)))
