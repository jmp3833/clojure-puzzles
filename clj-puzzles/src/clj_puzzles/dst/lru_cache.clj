(ns clj-puzzles.dst.lru-cache)

(defn hash-dll-add!
  ([k v] (hash-dll-add! [nil nil] k v))
  ([[l m] k v] 
   (let [newnode (ref {:prev nil :next nil :data v})]
     (if (nil? l)
       [newnode {k newnode}]
       (dosync
         (ref-set l (assoc @l :prev newnode))
         (ref-set newnode (assoc @newnode :next l))
         [newnode (assoc m k newnode)])))))

(defn hash-dll-del! [[l m] k]
  (let [node (get m k)
        prev (:prev @node)
        nxt (:next @node)]
    (dosync 
      (ref-set prev (assoc @prev :next nxt))
      [l (dissoc m k)])))
