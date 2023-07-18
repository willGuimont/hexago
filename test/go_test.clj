(ns go-test
  (:require [clojure.test :refer :all]
            [clojure.walk :as walk]
            [graph :as g]
            [go]))

(deftest create-board
  (let [graph (g/make-graph)
        board (go/make-board graph)]
    (is (every? #(contains? board %) []))
    (is (= graph (:board board)))))

(defn count-elements [data]
  (walk/prewalk
    (fn [x]
      (if (map? x)
        (into {} (map (fn [[k v]] [k (count v)]) x))
        x))
    data))

(deftest square-board-graph
  (let [graph (go/square-board-graph 9)]
    (is (= 81 (count (:values graph))))
    (is (= 81 (count (:neighbors graph))))
    (is (every? (fn [[k v]] (<= v 4)) (count-elements (:neighbors graph))))))
