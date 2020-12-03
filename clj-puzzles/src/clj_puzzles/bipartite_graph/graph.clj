(ns clj-puzzles.bipartite-graph.graph)

(defn add-edge [adj e1 e2]
  (update-in adj [e1 :adj] #(if (nil? %1) [%2] (conj %1 %2)) e2))

(defn to-adj-list [edges]
  "accepts a 2-d vector of edges representing an undirected graph
  and produces a hashmap based adjacency list"
  (reduce 
    #(add-edge (add-edge %1 (first %2) (second %2)) (second %2) (first %2))
    {} edges))

(comment 
  (= 
    ({1 {:adj [2 3]} 2 {:adj [4]}})
    (to-adj-list [[1 2] [1 3] [2 4]])))

(defn- color [node c] (assoc (val node) :red? c))

(defn bipartite? 
  ([graph] (bipartite? graph (color (first graph) true)))
  ([graph v] 
   (println graph v)
   (let [edges (map (partial get graph) (:adj v))]
     edges)))


