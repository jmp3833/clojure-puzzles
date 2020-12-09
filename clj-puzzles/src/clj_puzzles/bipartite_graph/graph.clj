(ns clj-puzzles.bipartite-graph.graph
  (:require [clj-puzzles.dst.queue :as q]))

(defn- color [adj e red?]
  (assoc-in adj [e :red?] red?))

(defn- enqueue-adjacent [q g adj]
  (apply conj q (filter #(and (not (:red? (val %))) (.contains adj (first %))) g)))

(defn bipartite? 
  "Graph as represented in clj-puzzles.dst.graph-undir/to-adj-list"
  ([graph] (let [g' (color graph (key (first graph)) true)]
             (bipartite? 
               g' 
               (q/queue [(first g')]))))
  ([graph q] 
   (if (empty? q) true
     (let [v (first q)
           adj (get-in v [1 :adj])
           edges (map (partial get graph) adj)
           c (get-in v [1 :red?])]
       (cond 
         (some #(= c (:red? %)) edges) false 
         :else (let [g' (reduce #(color %1 %2 (not c)) graph adj)
                     q' (enqueue-adjacent (pop q) g' adj)]
                 (recur g' q')))))))
