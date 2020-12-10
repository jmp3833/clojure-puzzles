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

(defn discover [graph e] 
  (assoc-in graph [e :discovered] true))

(defn consumer [node & adjacent]
  (partial (some #(= ) (conj adjacent node))))

(defn bfs 
  "traverses graph and applies p (a fn) accepting arity [root & adj],
  representing the currently selected node and all adjacent undiscovered nodes.

  Returns true if an element in the graph satisfies predicate p, false otherwise."
  ([graph p] 
   (assert (fn? p) "p must be a fn of arity [root & adj]")
   (let [root (first (first graph))] 
               (bfs (discover graph root) (q/queue [root]) p)))
  ([graph queue p] 
   (if (empty? queue) false
     (let [e (first queue)]
       (let [adj (get-in graph [e :adj])
             undiscovered (filter #(not (get-in graph [% :discovered])) adj)]
         (cond 
           (apply p e undiscovered) true
           :else (recur 
                   (reduce discover graph undiscovered) 
                   (apply conj (pop queue) undiscovered)
                   p)))))))

(defn bfs-ele [graph ele]
  "Leverages bfs to declare if element ele is present in graph. 

  Accepts graph representation produced by to-adj-list. 
  If any root or undiscovered adjacent node is equal to ele, returns true. Otherwise false."

  (bfs graph (fn [root & adj] (some #(= % ele) (conj adj root)))))
