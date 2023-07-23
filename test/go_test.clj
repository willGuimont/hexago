(ns go-test
  (:require [clojure.test :refer :all]
            [clojure.walk :as walk]
            [go]))

(deftest create-board
  (let [board (go/make-square-board 9)]
    (is (every? #(contains? board %) [:graph :size]))
    (is (= (count (get-in board [:graph :values])) 81))
    (is (= (:size board) 9))))

(deftest remove-stone
  (let [board (go/make-square-board 9)
        {board :board kill :kill} (go/put-stone board [0 0] :black)
        board (go/remove-stone board [0 0])]
    (is (= (go/get-at board [0 0]) nil))))

(deftest get-liberties-simple
  (let [board (go/make-square-board 9)
        {board :board kill :kill} (go/put-stone board [0 0] :black)
        {board :board kill :kill} (go/put-stone board [0 3] :black)
        {board :board kill :kill} (go/put-stone board [3 3] :black)]
    (is (= (go/get-liberties board [0 0]) 2))
    (is (= (go/get-liberties board [0 3]) 3))
    (is (= (go/get-liberties board [3 3]) 4))))

(deftest get-liberties-multiple
  (let [board (go/make-square-board 9)
        {board :board kill :kill} (go/put-stone board [0 0] :white)
        {board :board kill :kill} (go/put-stone board [1 0] :white)
        {board :board kill :kill} (go/put-stone board [2 0] :white)
        {board :board kill :kill} (go/put-stone board [0 1] :black)
        {board :board kill :kill} (go/put-stone board [1 1] :black)
        {board :board kill :kill} (go/put-stone board [2 1] :black)]
    (is (= (go/get-liberties board [0 0]) 1))))

(deftest put-stone
  (let [board (go/make-square-board 9)
        {board :board kill :kill} (go/put-stone board [0 0] :white)]
    (is (= (go/get-at board [0 0]) :white))
    (is (= kill 0))))

(deftest put-stone-suicide
  (let [board (-> (go/make-square-board 9)
                  (go/put-stone [1 0] :white)
                  (go/put-stone [0 1] :white))]
    (is (= (go/put-stone board [0 0] :black) nil))))

(deftest put-stone-not-replace
  (let [board (-> (go/make-square-board 9)
                  (go/put-stone [0 0] :white)
                  (go/put-stone [0 0] :black))]
    (is (= board nil))))

(deftest put-stone-update
  (let [board (-> (go/make-square-board 9)
                  (go/put-stone [0 0] :white)
                  (go/put-stone [1 0] :black)
                  (go/put-stone [0 1] :black))]
    (is (= (go/get-at board [0 0]) nil))))

(defn count-elements [data]
  (walk/prewalk
    (fn [x]
      (if (map? x)
        (into {} (map (fn [[k v]] [k (count v)]) x))
        x))
    data))

(deftest make-square-board
  (let [board (go/make-square-board 9)
        graph (:graph board)]
    (is (= (count (:values graph)) 81))
    (is (= (count (:neighbors graph)) 81))
    (is (every? (fn [[_ v]] (<= v 4)) (count-elements (:neighbors graph))))))

(deftest make-hexa-board
  (let [board (go/make-hexa-board 4)
        graph (:graph board)]
    (is (= (count (:values graph)) 37))
    (is (= (count (:neighbors graph)) 37))
    (is (every? (fn [[_ v]] (<= v 6)) (count-elements (:neighbors graph))))))


(deftest score
  (let [board (go/make-square-board 3)
        {board :board} (go/put-stone board [1 0] :black)
        {board :board} (go/put-stone board [0 1] :black)
        {board :board} (go/put-stone board [1 1] :black)
        {board :board} (go/put-stone board [2 2] :white)
        score (go/score board)]
    (is (= (:score score) {:black 1 :white 0}))
    (is (= (:territory score) {:white #{} :black #{[0 0]}}))))
