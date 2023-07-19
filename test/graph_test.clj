(ns graph-test
  (:require [clojure.test :refer :all]
            [graph :as g]))

(deftest create-graph
  (let [graph (g/make-graph)]
    (is (every? #(contains? graph %) [:values :neighbors]))
    (is (= (:values graph) {}))
    (is (= (:neighbors graph) {}))))

(deftest set-node
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2))]
    (is (= (g/get-value graph :A) 1))
    (is (= (g/get-value graph :B) 2))))

(deftest add-edge
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :A :C))]
    (is (contains? (g/get-neighbors graph :A) :B))
    (is (contains? (g/get-neighbors graph :A) :C))
    (is (= (g/get-neighbors graph :B) #{}))))

(deftest breadth-first-search
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :B :C))
        search (g/breadth-first-search graph :A)]
    (is (= (:visited search)) #{:A :B :C})
    (is (= (:seen search) #{:A :B :C}))))

(deftest breadth-first-search-value-pred
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :B :C))
        search (g/breadth-first-search graph :A :value-predicate? #(not= % 3))]
    (is (= (:visited search) #{:A :B}))
    (is (= (:seen search) #{:A :B :C}))))

(deftest breadth-first-search-id-pred
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :B :C))
        search (g/breadth-first-search graph :A :id-predicate? #(not= % :C))]
    (is (= (:visited search) #{:A :B}))
    (is (= (:seen search) #{:A :B :C}))))
