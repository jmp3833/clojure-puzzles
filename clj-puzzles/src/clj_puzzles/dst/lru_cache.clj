(ns clj-puzzles.dst.lru-cache)

(defn hash-dll-add!
  ([[l m] k v] 
   (let [newnode (ref {:prev nil :next nil :data v})]
     (dosync
       (if (nil? @l)
         (do 
           (ref-set l @newnode)
           (ref-set m {k l})
           nil)
         (do 
           (ref-set l (assoc @l :prev newnode))
           (ref-set newnode (assoc @newnode :next l))
           (ref-set m (assoc @m k newnode))
           nil))))))

(defn hash-dll-del! [[l m] k]
  (let [node (get @m k)
        prev (:prev @node)
        nxt (:next @node)]
    (dosync 
      (ref-set prev (assoc @prev :next nxt))
      (ref-set m (dissoc @m k))
      nil)))

(defn init [size]
  [(ref nil) (ref nil) size])

(defn get! [[l m s] k f]
  "Get element from cache at [k]ey, 
  populating the cache with [f]allback otherwise.
  if cache is at maximum [s]ize, evicts least recently used item"
  (if (contains? @m k) 
    (let [v (get @m k)]
      (when (some? (:prev @v))
        (hash-dll-del! [l m] k)
        (hash-dll-add! [l m] k (:data @v)))
      (:data @v))
    (let [v (f)]
      (hash-dll-add! [l m] k v)
      v)))
