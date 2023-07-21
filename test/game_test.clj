(ns game-test
  (:require [clojure.test :refer :all]
            [game :as g]))

(deftest make-game
  (let [game (g/make-square-game 9)
        board (:board game)]
    (is (every? #(contains? game %) [:turn :board :num-pass :score]))
    (is (= (:turn game) :black))
    (is (= (:board game) board))
    (is (= (:num-pass game) 0))
    (is (= (:score game) {:white 6.5 :black 0}))))

(deftest get-cells
  (let [game (g/make-square-game 9)]
    (is (= (count (g/get-cells game)) 81))))

(deftest remove-stone
  (let [game (-> (g/make-square-game 9)
                 (g/play-turn [0 0])
                 (g/remove-stone [0 0]))]
    (is (= (g/get-at game [0 0]) nil))))

(deftest finished?
  (let [game (g/make-square-game 9)
        game-1 (g/pass game)
        game-2 (g/pass game-1)
        game-3 (g/pass game-2)]
    (is (not (g/finished? game)))
    (is (not (g/finished? game-1)))
    (is (g/finished? game-2))
    (is (g/finished? game-3))))

(deftest next-color
  (is (= (g/next-turn :white) :black))
  (is (= (g/next-turn :black) :white)))

(deftest play-turn
  (let [game (g/make-square-game 9)
        game-1 (g/pass game)
        game-2 (g/play-turn game-1 [0 0])
        game-3 (g/play-turn game-2 [0 1])
        game-4 (g/play-turn game-3 [0 2])]
    (is (= (:num-pass game-4) 0))
    (is (= (:turn game-4) :black))
    (is (= (g/get-at game-4 [0 0]) :white))
    (is (= (last (:history game-4)) (:board game-3)))))

(deftest play-turn-capture
  (let [board (-> (g/make-square-game 9)
                  (g/play-turn [0 0])
                  (g/play-turn [1 0])
                  (g/play-turn [8 8])
                  (g/play-turn [0 1])
                  (g/pass)
                  (g/play-turn [7 8])
                  (g/pass)
                  (g/play-turn [8 7]))]
    (is (= (:score board) {:white 8.5 :black 0}))))

(deftest play-turn-capture-corner
  (let [board (-> (g/make-square-game 9)
                  (g/play-turn [7 8])
                  (g/play-turn [8 8])
                  (g/play-turn [8 7]))]
    (is (= (:score board) {:white 6.5 :black 1}))))

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
    (is (= (g/get-at game [2 1]) nil))
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
    (is (= (last (:history game-2)) (:board game-1)))
    (is (= (:turn game) :black))
    (is (= (:turn game-1) :white))
    (is (= (:turn game-2) :black))))

(deftest score
  (let [game (-> (g/make-square-game 4)
                 (g/play-turn [1 0])
                 (g/play-turn [2 2])
                 (g/play-turn [0 1])
                 (g/pass)
                 (g/play-turn [1 1]))]
    (is (g/score game) {:black 1 :white 0})))
