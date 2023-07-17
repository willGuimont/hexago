(ns board)

(defrecord Board [cells])

(defn make-board []
  (Board. (vec (repeat 9 nil))))
