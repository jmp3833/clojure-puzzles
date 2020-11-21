(ns clj-puzzles.houses.heist)

(defn lindex [idx]
  (inc (* idx 2)))

(defn rindex [idx]
  (inc (lindex idx)))

(defn n-to-0 [v]
  (if (nil? v) 0 v))

(declare memo)

(defn btree-heist 
  ([tree] (btree-heist tree 0))
  ([tree idx memo]
   ;need to assoc in memo for each call
   (cond 
     (>= idx (count tree)) 0 
     (contains? memo idx) (get memo idx)
     :else 
     (let [part (partial btree-heist tree)
           l (part (lindex idx))
           r (part (rindex idx))
           ll (part (lindex (lindex idx)))
           rr (part (rindex (rindex idx)))
           lr (part (lindex (rindex idx)))
           rl (part (rindex (lindex idx)))]
       (max 
         (+ (n-to-0 (get tree idx)) ll rr lr rl)
         (+ 0 l r))))))
