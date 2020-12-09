(ns clj-puzzles.bipartite-graph.graph
  (:require [clj-puzzles.dst.queue :as q]))

(defn bipartite? 
  "Graph as represented in clj-puzzles.dst.graph-undir/to-adj-list"
