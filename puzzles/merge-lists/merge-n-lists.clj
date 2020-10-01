(load-file "../../dst/heap.clj")

(defn merge-w-limit [limit result lists]
  (let [m (first 
            (reduce 
              (fn [heap l] (add heap (first l))) 
              [] 
              lists))] m))

(merge-w-limit 2 [] [[-1 4 7] [0 3 11] [-100 4 5]]) 
