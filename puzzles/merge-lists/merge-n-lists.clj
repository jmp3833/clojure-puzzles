(load-file "../../dst/heap.clj")

(defn merge-w-limit 
  ([lists limit]
   (let [firsts (map-indexed (fn [i lst] [i (first lst)]) lists)
         nexts (mapv rest lists)]
     (merge-w-limit 
       nexts
       limit
       (reduce 
         (fn [coll ele] (add coll {:i (get ele 0) :v (get ele 1)} :v))
         []
         firsts) 
       [])))

  ([lists limit minheap result] 
   (cond 
     (empty? minheap) result
     (= limit (count result)) result
     :else (let [f (first minheap)
                 rlocal (conj result (first minheap))
                 hlocal (delete minheap :v)
                 nxt (get lists (:i f))]
             lists))))

(merge-w-limit '[(-1 4 7) (0 3 11) (-100 4 5)] 2) 
