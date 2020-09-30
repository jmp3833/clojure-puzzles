(defn parent-idx [idx] (int (. Math floor (/ (dec idx) 2))))

(defn add [heap ele] 
  (if 
    (not (vector? heap)) "please, vectors only" 
    (let [appended (conj heap ele)]
      (if-let [p-idx (parent-idx (dec (count appended)))]
        (swap-until appended (dec (count appended)))
        appended))))

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

(defn children-idx [heap idx] 
  (let [left (inc (* 2 idx))
        right (inc left)]
    {:left left :right right}))

(reduce #(add %1 %2) [] [3 1 5 0])
