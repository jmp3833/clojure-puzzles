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

(defn subtree-min [tree]
  (if-let [l (:l tree)] (if-let [ll (:l l)] (subtree-min l) (:d l)) (:d tree)))

(defn delete [tree data]
  (let [head (:d tree)
        noleft? (nil? (:l tree))
        noright? (nil? (:r tree))
        node? (= data head)
        lte? (<= data head)
        gt? (not lte?)]
    (cond
      (nil? head) nil
      (and node? noleft? noright?) nil 
      (and node? noleft?) (assoc tree :l nil :r nil :d (get-in tree [:l :d])) 
      (and node? noright?) (assoc tree :l nil :r nil :d (get-in tree [:r :d])) 
      node?
      (let [sm (subtree-min (:r tree))]
        (assoc (delete tree sm) :d sm))
      lte? (assoc tree :l (delete (:l tree) data))
      :else (assoc tree :r (delete (:r tree) data)))))
