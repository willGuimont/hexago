(ns go
  (:require [clojure.set :as set]
            [clojure.set :as s]
            [graph :as g]))

(defn make-board [graph size]
  {:graph (g/prune graph) :size size})

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
  (reduce kill-group-if-stuck- {:board board :kill 0} (get-neighbors-at board [i j])))

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

(defn add-edges- [graph x y]
  (-> graph
      (g/add-edge x y)
      (g/add-edge y x)))

(defn make-square-board [size]
  (let [graph (atom (g/make-graph))]
    (dotimes [i size]
      (dotimes [j size]
        (swap! graph g/set-node [i j] nil)
        (when (< 0 i)
          (let [prev-i (dec i)]
            (swap! graph add-edges- [i j] [prev-i j])))
        (when (< 0 j)
          (let [prev-j (dec j)]
            (swap! graph add-edges- [i j] [i prev-j])))))
    (make-board @graph size)))

(defn make-hexa-board [size]
  (let [graph (atom (g/make-graph))
        full-size (inc (* 2 (dec size)))
        half-size (quot full-size 2)]
    (dotimes [i full-size]
      (let [get-iter #(+ size (if (<= % half-size) % (- full-size (inc %))))
            num-iter (get-iter i)
            dec-num-iter (dec num-iter)
            i-gt (< 0 i)
            prev-i (dec i)]
        (dotimes [j num-iter]
          (let [prev-j (dec j)
                j-gt (< 0 j)
                next-j (inc j)
                next-j-prev-row (dec (get-iter prev-i))]
            (swap! graph g/set-node [i j] nil)
            (when i-gt
              (swap! graph add-edges- [i j] [prev-i j]))
            (when j-gt
              (swap! graph add-edges- [i j] [i prev-j]))
            (when (and i-gt j-gt (<= i half-size))
              (swap! graph add-edges- [i j] [prev-i prev-j]))
            (when (and (= j dec-num-iter) i-gt)
              (swap! graph add-edges- [i j] [prev-i next-j-prev-row]))
            (when (and (> i half-size) (< j num-iter))
              (swap! graph add-edges- [i j] [prev-i next-j]))))))
    (make-board @graph size)))

(defn score [board]
  (let [graph (:graph board)
        values (:values graph)
        empty-pos (atom (set (map (fn [[k _]] k) (filter (fn [[_ v]] (= nil v)) values))))
        score (atom {:score {:white 0 :black 0} :territory {:white #{} :black {}}})]
    (while (not-empty @empty-pos)
      (let [starting-pos (first @empty-pos)
            search (g/search graph starting-pos :value-predicate? #(= % nil))
            seen (:seen search)
            visited (:visited search)
            num-empty (count visited)
            seen-pos (->> seen)
            seen-color (disj (->> (:seen search)
                                  (map (partial get-at board))
                                  (set)) nil)]
        (when (= 1 (count seen-color))
          (let [color (first seen-color)]
            (swap! score update-in [:score color] + num-empty)
            (swap! score update-in [:territory color] set/union visited)))
        (swap! empty-pos s/difference seen-pos)))
    @score))
