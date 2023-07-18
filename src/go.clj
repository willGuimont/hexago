(ns go
  (:require [graph :as g]))

(defn make-board [graph]
  {:board graph})

(defn square-board-graph [size]
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
    @graph))
