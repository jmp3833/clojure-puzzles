(ns clj-puzzles.dst.bst)

(defn insert
  ([data] (insert nil data))
  ([tree data] 
   (let [newnode {:l nil :r nil :d data}]
     (println (:d tree))
     (if-let [head (:d tree)]
       (cond
         (and (<= data head) (nil? (:l tree))) (assoc tree :l newnode)
         (and (> data head) (nil? (:r tree))) (assoc tree :r newnode)
         (<= data head) (assoc tree :l (insert (:l tree) data))
         :else (assoc tree :r (insert (:r tree) data)))
       newnode))))

