(ns game
  (:require [go :as g]))

(defn make-game [board]
  {:board board :turn :black :num-pass 0 :history []})

(defn next-turn [turn]
  (cond
    (= turn :white) :black
    (= turn :black) :white))

(defn play-turn [game position]
  (let [board (:board game)
        board-new (g/put-stone board position (:turn game))
        game-new (assoc game :board board-new)]
    (if (or (= nil board-new) (= board-new (last (:history game))))
      game
      (some-> game-new
              (update :turn next-turn)
              (assoc :num-pass 0)
              (update :history conj board)))))

(defn pass [game]
  (-> game
    (update :num-pass inc)
    (update :history conj (:board game))))
