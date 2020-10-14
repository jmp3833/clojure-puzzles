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
                 idx (:i f)
                 rlocal (conj result (first minheap))
                 hlocal (delete minheap :v)
                 nxt (first (get lists idx))
                 llocal (assoc lists idx (rest (get lists idx)))]
             (recur 
               llocal
               limit
               (if (some? nxt) (add hlocal {:i idx :v nxt} :v) hlocal)
               rlocal)))))

;;Works on infinite seqs! 
(map :v (merge-w-limit 
          [(iterate inc 100) (iterate inc 0)] 10000))
