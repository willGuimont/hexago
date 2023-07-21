(ns graph)

(defn make-graph []
  {:values {} :neighbors {}})

(defn get-value [graph id]
  (get-in graph [:values id]))

(defn get-neighbors-at [graph id]
  (get-in graph [:neighbors id] #{}))

(defn set-node [graph id value]
  (-> graph
      (assoc-in [:values id] value)))

(defn add-edge [graph from to]
  (let [neighbors (get-neighbors-at graph from)
        new-neighbors (conj neighbors to)]
    (-> graph
        (assoc-in [:neighbors from] new-neighbors))))

(defn breadth-first-search [graph starting-node & {:keys [value-predicate? id-predicate?]}]
  (loop [queue (atom [starting-node])
         visited (atom #{})
         seen (atom #{starting-node})]
    (while (not-empty @queue)
      (let [current-node (peek @queue)]
        (swap! queue pop)
        (swap! visited conj current-node)
        (let [neighbors (get-neighbors-at graph current-node)]
          (doseq [neighbor neighbors]
            (when (and (not (contains? @seen neighbor))
                       (if (nil? id-predicate?) true (id-predicate? neighbor))
                       (if (nil? value-predicate?) true (value-predicate? (get-value graph neighbor))))
              (swap! queue conj neighbor)
              (swap! seen conj neighbor))
            (swap! seen conj neighbor)))))
    {:visited @visited :seen @seen}))
