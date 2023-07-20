(ns game-test
  (:require [clojure.test :refer :all]
            [game :as g]
            [go :as go]))

(deftest make-game
  (let [board (go/make-square-board 9)
        game (g/make-game board)]
    (is (every? #(contains? game %) [:turn :board :num-pass]))
    (is (= (:turn game) :black))
    (is (= (:board game) board))
    (is (= (:num-pass game) 0))))

(deftest next-color
  (is (= (g/next-turn :white) :black))
  (is (= (g/next-turn :black) :white)))

(deftest play-turn
  (let [game (g/make-game (go/make-square-board 9))
        game-1 (g/pass game)
        game-2 (g/play-turn game-1 [0 0])
        game-3 (g/play-turn game-2 [0 1])
        game-4 (g/play-turn game-3 [0 2])
        board (:board game-4)]
    (is (= (:num-pass game-4) 0))
    (is (= (:turn game-4) :white))
    (is (= (go/get-at board [0 0]) :black))
    (is (= (last (:history game-4)) (:board game-3)))))

(deftest play-turn-ko
  (let [game (-> (g/make-game (go/make-square-board 9))
                 (g/play-turn [0 1])
                 (g/play-turn [3 1])
                 (g/play-turn [1 0])
                 (g/play-turn [2 2])
                 (g/play-turn [1 2])
                 (g/play-turn [2 0])
                 (g/play-turn [2 1])
                 (g/play-turn [1 1]))]
    (is (= (go/get-at (:board game) [2 1]) nil))
    (is (= (g/play-turn game [2 1]) game))))

(deftest play-turn-bad-move
  (let [game (g/make-game (go/make-square-board 9))
        game-1 (g/play-turn game [0 0])
        game-2 (g/play-turn game-1 [0 0])]
    (is (= game-2 game-1))))

(deftest pass
  (let [game (g/make-game (go/make-square-board 9))
        game-1 (g/pass game)
        game-2 (g/pass game-1)]
    (is (:num-pass game-1) 1)
    (is (:num-pass game-2) 2)
    (is (= (last (:history game-2)) (:board game-1)))))
