(ns clj-puzzles.dst.lru-cache)

(defn str-list 
  ([l] (str-list l ""))
  ([l sb] 
   (let [node @l
         v (str sb (:data node))]
     (if (nil? (:next node)) 
       v
       (recur (:next node) (str v "<->"))))))

(defn hash-dll-add!
  [cache k v] 
   (let [l (:list @cache)
         m (:map @cache)
         newnode (ref {:prev nil :next nil :data v})]
     (dosync
       (ref-set cache (assoc @cache :list newnode))
       (if (nil? @l)
         (do 
           (ref-set l @newnode)
           (ref-set m {k l})
           (ref-set (:last @cache) l))
         (do 
           (ref-set l (assoc @l :prev newnode))
           (ref-set newnode (assoc @newnode :next l))
           (ref-set m (assoc @m k newnode))))))
  nil)

(defn hash-dll-del! [cache k]
  (let [l (:list @cache)
        m (:map @cache)
        node (get @m k)
        prev (:prev @node)
        nxt (:next @node)
        lst (:last @node)]
    (dosync 
      (ref-set m (dissoc @m k))
      (cond 
        (some? prev) (ref-set prev (assoc @prev :next nxt))
        (nil? nxt) (ref-set lst (:prev @lst))
        :else (ref-set l (:next @l)))
      nil)))

(defn init! [size]
  (ref {:list (ref nil) :map (ref nil) :last (ref nil) :size size}))

(defn get! [c k f]
  "Get element from cache at [k]ey, 
  populating the cache with [f]allback otherwise.
  if cache is at maximum size, evicts least recently used item"
  (let [m (:map @c)
        l (:list @c)]
    (if (contains? @m k) 
      (let [v (get @m k)]
        (when (some? (:prev @v))
          (hash-dll-del! c k)
          (hash-dll-add! c k (:data @v)))
        (:data @v))
      (let [v (f)]
        (hash-dll-add! c k v)
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
