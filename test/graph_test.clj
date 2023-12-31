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
    (is (contains? (g/get-neighbors-at graph :A) :B))
    (is (contains? (g/get-neighbors-at graph :A) :C))
    (is (= (g/get-neighbors-at graph :B) #{}))))

(deftest search
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :B :C))
        search (g/search graph :A)]
    (is (= (:visited search)) #{:A :B :C})
    (is (= (:seen search) #{:A :B :C}))))

(deftest search-value-pred
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :B :C))
        search (g/search graph :A :value-predicate? #(not= % 3))]
    (is (= (:visited search) #{:A :B}))
    (is (= (:seen search) #{:A :B :C}))))

(deftest search-id-pred
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :B :C))
        search (g/search graph :A :id-predicate? #(not= % :C))]
    (is (= (:visited search) #{:A :B}))
    (is (= (:seen search) #{:A :B :C}))))

(deftest prune
  (let [graph (-> (g/make-graph)
                  (g/set-node :A 1)
                  (g/set-node :B 2)
                  (g/set-node :C 3)
                  (g/add-edge :A :B)
                  (g/add-edge :A :C)
                  (g/add-edge :A :E)
                  (g/add-edge :D :A)
                  (g/prune))]
    (is (= (count (:neighbors graph)) 1))
    (is (= (count (get-in graph [:neighbors :A])) 2))))
