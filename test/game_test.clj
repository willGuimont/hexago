(ns game-test
  (:require [clojure.test :refer :all]
            [graph :as graph]
            [go :as go]
            [game :as g]))

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
        game (g/pass game)
        game (g/play-turn game [0 0])
        board (:board game)]
    (is (= (:num-pass game) 0))
    (is (= (:turn game) :white))
    (is (= (go/get-at board [0 0]) :black))))

(deftest pass
  (let [game (g/make-game (go/make-square-board 9))
        game-1 (g/pass game)
        game-2 (g/pass game-1)]
    (is (:num-pass game-1) 1)
    (is (:num-pass game-2) 2)))
