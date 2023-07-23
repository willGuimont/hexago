(ns game
  (:require [clojure.set :as set]
            [go :as g]))

(defn make-game [board]
  {:board board :turn :black :num-pass 0 :history [] :territory {:white #{} :black #{}} :score {:white 6.5 :black 0}})

(defn make-square-game [size]
  (make-game (go/make-square-board size)))

(defn make-hexa-game [size]
  (make-game (go/make-hexa-board size)))

(defn get-turn [game]
  (:turn game))

(defn get-at [game position]
  (go/get-at (:board game) position))

(defn remove-stone [game position]
  (update game :board go/remove-stone position))

(defn get-neighbors-at [game position]
  (g/get-neighbors-at (:board game) position))

(defn get-cells [game]
  (set (keys (get-in game [:board :graph :values]))))

(defn finished? [game]
  (>= (:num-pass game) 2))

(defn next-turn [turn]
  (cond
    (= turn :white) :black
    (= turn :black) :white))

(defn play-turn [game position]
  (let [board (:board game)
        turn (:turn game)
        {board-new :board kill :kill} (g/put-stone board position turn)
        game-new (assoc game :board board-new)]
    (if (or (= nil board-new) (= board-new (last (:history game))))
      game
      (some-> game-new
              (update :turn next-turn)
              (assoc :num-pass 0)
              (update-in [:score turn] + kill)
              (update :history conj board)))))

(defn pass [game]
  (-> game
      (update :turn next-turn)
      (update :num-pass inc)
      (update :history conj (:board game))))

(defn score [game]
  (let [{s :score t :territory} (g/score (:board game))]
    (-> game
      (update :score #(merge-with + s %))
      (update :territory #(merge-with set/union t %)))))
