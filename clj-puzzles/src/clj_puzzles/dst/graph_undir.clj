(ns clj-puzzles.dst.graph-undir
  (:require [clj-puzzles.dst.queue :as q]))

(defn- add-edge [graph e1 e2]
  (update-in graph [e1 :adj] #(if (nil? %1) [%2] (conj %1 %2)) e2))

(defn to-adj-list [edges]
  "accepts a 2-d vector of edges representing an undirected graph
  and produces a hashmap based, undirected adjacency list"
  (reduce 
    #(-> %1 (add-edge (first %2) (second %2)) (add-edge (second %2) (first %2)))
    {} edges))

(comment 
  (= 
    ({1 {:adj [2 3]} 2 {:adj [4]}})
    (to-adj-list [[1 2] [1 3] [2 4]])))

(defn- discover [graph e] 
  (assoc-in graph [e :discovered] true))

(defn bfs 
  ([graph ele] (let [root (first (first graph))] (bfs (discover graph root) (q/queue [root]) ele)))
  ([graph queue ele] 
   (if (empty? queue) false
     (let [e (first queue)]
       (if (= ele e)
         true 
         (let [adj (get-in graph [e :adj])
               undiscovered (filter #(not (get-in graph [% :discovered])) adj)]
           (cond 
             (some #(= ele %) undiscovered) true
             :else (recur 
                     (reduce discover graph undiscovered) 
                     (apply conj (pop queue) undiscovered) ele))))))))
