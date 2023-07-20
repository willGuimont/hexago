(ns game
  (:require [go :as g]))

(defn make-game [board]
  {:board board :turn :black :num-pass 0})

(defn next-turn [turn]
  (cond
    (= turn :white) :black
    (= turn :black) :white))

(defn play-turn [game position]
  (let [board (:board game)
        board (g/put-stone board position (:turn game))
        game (assoc game :board board)]
    (some-> game
            (update :turn next-turn)
            (assoc :num-pass 0))))

(defn pass [game]
  (update game :num-pass inc))
