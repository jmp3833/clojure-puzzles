(defn parent-idx [idx] (int (. Math floor (/ (dec idx) 2))))

(defn swap-until 
  ([heap idx] (swap-until heap idx identity))
  ([heap idx ex] 
   (if (<= idx 0) heap 
     (let [parent-idx (parent-idx idx)
           value (get heap idx)
           parent (get heap parent-idx)
           value-ex (ex value)
           parent-ex (ex parent)]
       (cond 
         (<= parent-ex value-ex) heap
         (nil? parent-idx) heap
         (zero? idx) heap
         :else (recur 
                 (assoc (assoc heap parent-idx value) idx parent) 
                 parent-idx
                 ex))))))

(defn smallest-idx 
  ([heap idx] (smallest-idx heap idx identity))
  ([heap idx ex]
  (let [left-idx (inc (* 2 idx))
        right-idx (inc left-idx)
        parent (get heap idx)
        parent-ex (ex parent)
        local-heap [[parent idx] 
                    [(get heap left-idx) left-idx] 
                    [(get heap right-idx) right-idx]]]
    (reduce 
      #(if (< (ex (get %1 0)) (ex (get %2 0))) %1 %2)
      (filter 
        #(some? (get % 0)) 
        local-heap)))))

(defn heapify 
  ([heap ex] (heapify heap 0 ex))
  ([heap idx ex] 
   (if (empty? heap) heap
     (let [smallest (smallest-idx heap idx ex)
           s-idx (get smallest 1)
           s-val (get smallest 0)]
       (if
         (= s-idx idx) heap
         (recur
           (assoc (assoc heap idx s-val) s-idx (get heap idx)) 
           s-idx
           ex))))))

(defn add 
  ([heap ele] (add heap ele identity))
  ([heap ele ex] 
   (if 
     (not (vector? heap)) "please, vectors only" 
     (let [appended (conj heap ele)]
       (if-let [p-idx (parent-idx (dec (count appended)))]
         (swap-until appended (dec (count appended)) ex)
         appended)))))

(defn delete [heap ex] 
  (let [lst (dec (count heap))
        swap (assoc heap 0 (get heap lst))]
    (heapify (vec (take lst swap)) ex)))

(defn hsort 
  ([heap] (hsort heap [] identity))
  ([heap ex] (hsort heap [] ex))
  ([heap result ex]
   (assert 
     (number? (ex (first heap))) 
     "requires non-empty heap with numeric value extracted by ex")
   (let [ele (first heap)
         modified (delete heap ex)
         r (conj result ele)]
     (if 
       (empty? modified) r
       (recur modified r ex)))))
