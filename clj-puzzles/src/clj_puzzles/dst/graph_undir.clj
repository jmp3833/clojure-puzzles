(ns clj-puzzles.dst.graph-undir)

(defn to-adj-list [edges]
  "accepts a 2-d vector of edges representing an undirected graph
  and produces a hashmap based, undirected adjacency list"
  (reduce 
    #(-> %1 (add-edge (first %2) (second %2)) (add-edge (second %2) (first %2)))
    {} edges))

(defn add-edge [graph e1 e2]
  (update-in graph [e1 :adj] #(if (nil? %1) [%2] (conj %1 %2)) e2))

(comment 
  (= 
    ({1 {:adj [2 3]} 2 {:adj [4]}})
    (to-adj-list [[1 2] [1 3] [2 4]])))
