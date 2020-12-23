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

(defn get! [[l m] k f]
  "Get element from cache at [k]ey, 
  populating the cache with [f]allback otherwise"
  (if (contains? m k) 
    (let [v (get m k)]
      (when (some? (:prev @v))
        (hash-dll-del! [l m] k)
        (hash-dll-add! [l m] k (:data @v)))
      (:data @v))
      (let [v (f)]
        (hash-dll-add! [l m] k v)
        v)))
