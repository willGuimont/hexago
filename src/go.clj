(ns go
  (:require [graph :as g]))

(defn make-board [graph size]
  {:graph graph :size size})

(defn get-at [board position]
  (g/get-value (:graph board) position))

(defn get-neighbors-at [board position]
  (g/get-neighbors-at (:graph board) position))

(defn remove-at- [board position]
  (let [graph (:graph board)
        graph (g/set-node graph position nil)]
    (assoc board :graph graph)))

(defn get-liberties [board position]
  (let [graph (:graph board)
        color (get-at board position)
        value-pred? #(= % color)
        search (g/breadth-first-search graph position :value-predicate? value-pred?)
        liberties (filter #(= nil (get-at board %)) (:seen search))]
    (if (= nil color)
      0
      (count liberties))))

(defn kill-group-if-stuck- [board position]
  (let [color (get-at board position)
        liberties (get-liberties board position)
        search (g/breadth-first-search (:graph board) position :value-predicate? #(= % color))
        visited (:visited search)
        new-board (atom board)]
    (if (or (= nil color) (not= 0 liberties))
      board
      (do
        (doseq [v visited]
          (swap! new-board remove-at- v))))
    @new-board))

(defn update-board- [board [i j]]
  (-> board
      (kill-group-if-stuck- [(dec i) j])
      (kill-group-if-stuck- [(inc i) j])
      (kill-group-if-stuck- [i (dec j)])
      (kill-group-if-stuck- [i (dec j)])))

(defn put-stone [board position color]
  (let [graph (:graph board)
        new-graph (g/set-node graph position color)
        new-board (assoc board :graph new-graph)]
    (if (= nil (get-at board position))
      (let [updated-board (update-board- new-board position)]
        (if (= 0 (get-liberties updated-board position))
          nil
          (update-board- new-board position)))
      nil)))

(defn make-square-board [size]
  (let [graph (atom (g/make-graph))]
    (dotimes [i size]
      (dotimes [j size]
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
