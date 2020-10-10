(load-file "../../dst/heap.clj")

(defn merge-w-limit [limit result lists]
  (let [minheap
        (reduce 
          (fn [heap l] (add heap (first l))) 
          [] 
          lists)] 
    minheap))

(merge-w-limit 2 [] [[-1 4 7] [0 3 11] [-100 4 5]]) 
