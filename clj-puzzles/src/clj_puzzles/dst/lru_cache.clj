(ns clj-puzzles.dst.lru-cache)

(defn hash-dll-add!
  ([cache k v] 
   (let [l (:list cache)
         m (:map cache)
         newnode (ref {:prev nil :next nil :data v})]
     (dosync
       (if (nil? @l)
         (do 
           (ref-set l @newnode)
           (ref-set m {k l})
           (ref-set (:last cache) l)
           nil)
         (do 
           (ref-set l (assoc @l :prev newnode))
           (ref-set newnode (assoc @newnode :next l))
           (ref-set m (assoc @m k newnode))
           nil))))))

(defn hash-dll-del! [cache k]
  ;TODO check first and update last (if deleted)
  (let [m (:map cache)
        node (get @m k)
        prev (:prev @node)
        nxt (:next @node)]
    (dosync 
      (ref-set prev (assoc @prev :next nxt))
      (ref-set m (dissoc @m k))
      nil)))

(defn init! [size]
  {:list (ref nil) :map (ref nil) :last {ref nil} :size size})

(defn get! [cache k f]
  "Get element from cache at [k]ey, 
  populating the cache with [f]allback otherwise.
  if cache is at maximum size, evicts least recently used item"
  (let [m (:map cache)
        l (:list cache)]
    (if (contains? @m k) 
      (let [v (get @m k)]
        (when (some? (:prev @v))
          (hash-dll-del! cache k)
          (hash-dll-add! cache k (:data @v)))
        (:data @v))
      (let [v (f)]
        (hash-dll-add! cache k v)
        ;TODO honor size and delete final ele here (if required)
        v))))

(comment 
  (set! *print-level* 3)                             ; recursive data structure :'(

  (def c (cache/init 3))                             ; []
  (cache/get! c 1 (fn [] 10))                        ; [[1 10]]
  (cache/get! c 1 (fn [] "cache hit! not invoked"))  ; [[1 10]]
  (cache/get! c 2 (fn [] 20))                        ; [[2 20] [1 10]]
  (cache/get! c 3 (fn [] 30))                        ; [[3 30] [2 20] [1 10]]
  (cache/get! c 4 (fn [] 40)))                       ; [[4 40] [3 30] [2 20]]
