(ns clj-puzzles.houses.heist)

(defn lindex [idx]
  (inc (* idx 2)))

(defn rindex [idx]
  (inc (lindex idx)))

(defn n-to-0 [v]
  (if (nil? v) 0 v))

(defn btree-heist 
  ([tree] (btree-heist tree (atom {}) 0))
  ([tree memo idx]
   (assert (vector? tree) "tree must be a vector")
   (cond 
     (contains? @memo idx) (get @memo idx)
     (>= idx (count tree)) 0 
     :else 
     (let [part (partial btree-heist tree memo)
           l (part (lindex idx))
           r (part (rindex idx))
           ll (part (lindex (lindex idx)))
           rr (part (rindex (rindex idx)))
           lr (part (lindex (rindex idx)))
           rl (part (rindex (lindex idx))) 
           value (max 
                   (+ (n-to-0 (get tree idx)) ll rr lr rl)
                   (+ 0 l r))]
       (swap! memo assoc idx value)
       value))))
