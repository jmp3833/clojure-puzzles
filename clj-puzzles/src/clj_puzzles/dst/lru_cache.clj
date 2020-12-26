(ns clj-puzzles.dst.lru-cache)

"
- [ ] reverse pointer broken on delete
- [ ] :last reference doesn't have prev pointer 
"

(defn hash-dll-add!
  [cache k v] 
  (let [l (:list @cache)
        m (:map @cache)
        newnode (ref {:prev nil :next nil :data v})]
    (dosync
      (ref-set cache (assoc @cache :list newnode))
      (if (nil? @l)
        (do 
          (ref-set l newnode)
          (ref-set m {k l})
          (ref-set (:last @cache) l))
        (do 
          (ref-set newnode (assoc @newnode :next l))
          (ref-set l (assoc @l :prev newnode))
          (ref-set m (assoc @m k newnode))))))
  nil)

(defn hash-dll-del! [cache k]
  (let [l (:list @cache) m (:map @cache)]
    (when-let [node (get @m k)]
      (dosync 
        (if (nil? (:prev @node))
          (ref-set l (:next @l)) ;delete front of list
          (do 
            (when (nil? (:next @node)) (ref-set cache (assoc @cache :last (:prev @node))))
            (ref-set (:prev @node) (assoc (deref (:prev @node)) :next (:next @node)))))
        (ref-set m (dissoc @m k)))))
  nil)

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

(defn str-list 
  ([l] (str-list l ""))
  ([l sb] 
   (let [node @l
         v (str sb (:data node))]
     (if (nil? (:next node)) 
       v
       (recur (:next node) (str v "<->"))))))

(defn str-list-tail
  ([l] (str-list-tail l ""))
  ([l sb] 
   (let [node @l
         v (str sb (:data node))]
     (if (nil? (:prev node)) 
       v
       (recur (:prev node) (str v "<->"))))))

(comment 
  (set! *print-level* 3)                             ; recursive data structure :'(

  (def c (cache/init 3))                             ; []
  (cache/get! c 1 (fn [] 10))                        ; [[1 10]]
  (cache/get! c 1 (fn [] "cache hit! not invoked"))  ; [[1 10]]
  (cache/get! c 2 (fn [] 20))                        ; [[2 20] [1 10]]
  (cache/get! c 3 (fn [] 30))                        ; [[3 30] [2 20] [1 10]]
  (cache/get! c 4 (fn [] 40)))                       ; [[4 40] [3 30] [2 20]]
