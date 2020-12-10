(ns clj-puzzles.bipartite-graph.graph
  (:require [clj-puzzles.dst.graph-undir :as g]
            [clj-puzzles.dst.queue :as q]))

(defn color [graph e c] (assoc-in graph [e :red?] c))

(defn bipartite? 
  "Graph as represented in clj-puzzles.dst.graph-undir/to-adj-list"
  ([graph] 
   (let [root (first (first graph))] 
     (bipartite? (color (g/discover graph root) root true) (q/queue [root]))))
  ([graph queue] 
   (if (empty? queue) true
     (let [e (first queue)
           c (get-in graph [e :red?])
           adj (get-in graph [e :adj])
           undiscovered (filter #(not (get-in graph [% :discovered])) adj)]
       (if (some #(= c (get-in graph [% :red?])) adj) false
         (recur 
           (reduce #(color (g/discover %1 %2) %2 (not c)) graph undiscovered) 
           (apply conj (pop queue) undiscovered)))))))

(comment 
  (= true (bipartite? (g/to-adj-list [[1 2] [1 3] [2 4]])))
  (= false (bipartite? (g/to-adj-list [[1 2] [1 3] [2 3]])))
  (= false (bipartite? (g/to-adj-list [[1 2] [2 3] [3 4] [4 5] [1 5]])))
