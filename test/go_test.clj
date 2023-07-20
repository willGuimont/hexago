(ns go-test
  (:require [clojure.test :refer :all]
            [clojure.walk :as walk]
            [go]))

(deftest create-board
  (let [board (go/make-square-board 9)]
    (is (every? #(contains? board %) [:graph :size]))
    (is (= (count (get-in board [:graph :values])) 81))
    (is (= (:size board) 9))))

(deftest get-liberties-simple
  (let [board (-> (go/make-square-board 9)
                  (go/put-stone [0 0] :black)
                  (go/put-stone [0 3] :black)
                  (go/put-stone [3 3] :black))]
    (is (= (go/get-liberties board [0 0]) 2))
    (is (= (go/get-liberties board [0 3]) 3))
    (is (= (go/get-liberties board [3 3]) 4))))

(deftest get-liberties-multiple
  (let [board (-> (go/make-square-board 9)
                  (go/put-stone [0 0] :white)
                  (go/put-stone [1 0] :white)
                  (go/put-stone [2 0] :white)
                  (go/put-stone [0 1] :black)
                  (go/put-stone [1 1] :black)
                  (go/put-stone [2 1] :black))]
    (is (= (go/get-liberties board [0 0]) 1))))

(deftest put-stone
  (let [board (go/make-square-board 9)
        board (go/put-stone board [0 0] :white)]
    (is (= (go/get-at board [0 0]) :white))))

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

(deftest square-board-graph
  (let [board (go/make-square-board 9)
        graph (:graph board)]
    (is (= (count (:values graph)) 81))
    (is (= (count (:neighbors graph)) 81))
    (is (every? (fn [[k v]] (<= v 4)) (count-elements (:neighbors graph))))))
