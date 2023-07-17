(ns graph-test
  (:require [clojure.test :refer :all]
            [graph :as g]))

(deftest create-graph
  (let [graph (g/make-graph)]
    (is (every? #(contains? graph %) [:values :neighbors]))
    (is (= {} (:values graph)))
    (is (= {} (:neighbors graph)))))

(deftest set-node
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2))]
    (is (= 1 (g/get-value graph :A)))
    (is (= 2 (g/get-value graph :B)))))

(deftest add-edge
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :A :C))]
    (is (contains? (g/get-neighbors graph :A) :B))
    (is (contains? (g/get-neighbors graph :A) :C))
    (is (= #{} (g/get-neighbors graph :B)))))

(deftest breadth-first-search
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :B :C))]
    (println (g/breadth-first-search graph :A :value-predicate? #(not= % 3)))))
