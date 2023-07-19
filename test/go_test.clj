(ns go-test
  (:require [clojure.test :refer :all]
            [clojure.walk :as walk]
            [go]))

(deftest create-board
  (let [board (go/square-board-board 9)]
    (is (every? #(contains? board %) [:board :size]))
    (is (= 81 (count (get-in board [:board :values]))))
    (is (= 9 (:size board)))))

(deftest put-stone
  (let [board (go/square-board-board 9)
        board (go/put-stone board [0 0] :white)]
    (is (= :white (go/get-at board [0 0])))))

(deftest put-stone-suicide
  (let [board (-> (go/square-board-board 9)
                  (go/put-stone [1 0] :white)
                  (go/put-stone [0 1] :white))]
    (is (= board (go/put-stone board [0 0] :black)))))

(deftest put-stone-update
  (let [board (-> (go/square-board-board 9)
                  (go/put-stone [0 0] :white)
                  (go/put-stone [1 0] :black)
                  (go/put-stone [0 1] :black))]
    (is (= nil (go/get-at board [0 0])))))

(defn count-elements [data]
  (walk/prewalk
    (fn [x]
      (if (map? x)
        (into {} (map (fn [[k v]] [k (count v)]) x))
        x))
    data))

(deftest square-board-graph
  (let [board (go/square-board-board 9)
        graph (:board board)]
    (is (= 81 (count (:values graph))))
    (is (= 81 (count (:neighbors graph))))
    (is (every? (fn [[k v]] (<= v 4)) (count-elements (:neighbors graph))))))
