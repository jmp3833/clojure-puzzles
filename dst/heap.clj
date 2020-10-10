(defn parent-idx [idx] (int (. Math floor (/ (dec idx) 2))))

(defn swap-until [heap idx] 
  (if (<= idx 0) heap 
    (let [parent-idx (parent-idx idx)
          value (get heap idx) 
          parent (get heap parent-idx)]
      (cond 
        (<= parent value) heap
        (nil? parent-idx) heap
        (zero? idx) heap
        :else (recur 
                (assoc (assoc heap parent-idx value) idx parent) 
                parent-idx)))))

(defn smallest-idx [heap idx]
  (let [left-idx (inc (* 2 idx))
        right-idx (inc left-idx)
        parent (get heap idx)
        local-heap [[parent idx] 
                    [(get heap left-idx) left-idx] 
                    [(get heap right-idx) right-idx]]]
    (reduce 
      #(if (< (get %1 0) (get %2 0)) %1 %2)
      (filter 
        #(some? (get % 0)) 
        local-heap))))

(defn heapify 
  ([heap] (heapify heap 0))
  ([heap idx] 
   (if (empty? heap) heap
     (let [smallest (smallest-idx heap idx)
           s-idx (get smallest 1)
           s-val (get smallest 0)]
       (if
         (= s-idx idx) heap
         (heapify (assoc (assoc heap idx s-val) s-idx (get heap idx)) s-idx))))))

(defn add [heap ele] 
  (if 
    (not (vector? heap)) "please, vectors only" 
    (let [appended (conj heap ele)]
      (if-let [p-idx (parent-idx (dec (count appended)))]
        (swap-until appended (dec (count appended)))
        appended))))

(defn delete [heap] 
  (let [lst (dec (count heap))
        swap (assoc heap 0 (get heap lst))]
    (heapify (vec (take lst swap)))))

(defn sort [heap result]
  (println "heap" heap "result" result)
  (let [ele (first heap)
        modified (delete heap)
        r (conj result ele)]
    (if 
      (empty? modified) r
      (recur modified r))))

(sort (reduce add [] [3 0 1 0 -500]) [])
