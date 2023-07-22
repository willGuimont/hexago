(ns go
  (:require [clojure.set :as s]
            [graph :as g]))

(defn make-board [graph size]
  {:graph graph :size size})

(defn get-at [board position]
  (g/get-value (:graph board) position))

(defn get-neighbors-at [board position]
  (g/get-neighbors-at (:graph board) position))

(defn remove-stone [board position]
  (let [graph (:graph board)
        graph (g/set-node graph position nil)]
    (assoc board :graph graph)))

(defn get-liberties [board position]
  (let [graph (:graph board)
        color (get-at board position)
        search (g/search graph position :value-predicate? #(= % color))
        liberties (filter #(= nil (get-at board %)) (:seen search))]
    (if (= nil color)
      0
      (count liberties))))

(defn kill-group-if-stuck- [{board :board kill :kill} position]
  (let [color (get-at board position)
        liberties (get-liberties board position)
        search (g/search (:graph board) position :value-predicate? #(= % color))
        visited (:visited search)
        new-board (atom board)
        new-kills (atom kill)]
    (if (or (= nil color) (not= 0 liberties))
      board
      (do
        (doseq [v visited]
          (swap! new-board remove-stone v)
          (swap! new-kills inc))))
    {:board @new-board :kill @new-kills}))

(defn update-board- [board [i j]]
  (-> {:board board :kill 0}
      (kill-group-if-stuck- [(dec i) j])
      (kill-group-if-stuck- [(inc i) j])
      (kill-group-if-stuck- [i (dec j)])
      (kill-group-if-stuck- [i (inc j)])))

(defn put-stone [board position color]
  (let [graph (:graph board)
        new-graph (g/set-node graph position color)
        new-board (assoc board :graph new-graph)]
    (if (= nil (get-at board position))
      (let [{updated-board :board kill :kill} (update-board- new-board position)]
        (if (= 0 (get-liberties updated-board position))
          nil
          {:board updated-board :kill kill}))
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

(defn make-tri-board [size])

(defn make-hexa-board [size])

(defn score [board]
  (let [graph (:graph board)
        values (:values graph)
        empty-pos (atom (set (map (fn [[k _]] k) (filter (fn [[_ v]] (= nil v)) values))))
        score (atom {:white 0 :black 0})]
    (while (not-empty @empty-pos)
      (let [starting-pos (first @empty-pos)
            search (g/search graph starting-pos :value-predicate? #(= % nil))
            seen (:seen search)
            num-empty (count (:visited search))
            seen-pos (->> seen)
            seen-color (disj (->> (:seen search)
                                  (map (partial get-at board))
                                  (set)) nil)]
        (when (= 1 (count seen-color))
          (let [color (first seen-color)]
            (swap! score update color + num-empty)))
        (swap! empty-pos s/difference seen-pos)))
    @score))
