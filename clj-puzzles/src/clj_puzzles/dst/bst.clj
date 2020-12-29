(ns clj-puzzles.dst.bst)

(defn insert
  ([data] (insert nil data))
  ([tree data] 
   (let [newnode {:l nil :r nil :d data}]
     (if-let [head (:d tree)]
       (cond
         (and (<= data head) (nil? (:l tree))) (assoc tree :l newnode)
         (and (> data head) (nil? (:r tree))) (assoc tree :r newnode)
         (<= data head) (assoc tree :l (insert (:l tree) data))
         :else (assoc tree :r (insert (:r tree) data)))
       newnode))))

(defn delete-subtree-min [tree]
  (if-let [l (:l tree)] 
    (if-let [ll (:l l)]
      (assoc l :l (delete-subtree-min l))
      (assoc tree :l nil)) ;how to extract min value here?
      nil))

(defn delete [tree data]
  (if-let [head (:d tree)]
    (cond
      (and (= data head) (nil? (:l tree)) (nil? (:r tree))) nil
      (and (= data head) (nil? (:l tree))) (assoc tree :l nil :r nil :d (get-in tree [:l :d])) 
      (and (= data head) (nil? (:r tree))) (assoc tree :l nil :r nil :d (get-in tree [:r :d])) 
      (= data head) (let [])
      :else nil)))
      ;grab smallest element from right subtree and replace with node to delete

