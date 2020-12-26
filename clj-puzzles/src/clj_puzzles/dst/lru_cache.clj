(ns clj-puzzles.dst.lru-cache)

"
- [ ] delete tail
- [ ] delete head
- [x] delete doesn't remove prev pointers
- [x] reverse pointer broken on delete
- [x] double ref on list in map  
"

(defn hash-dll-add!
  [cache k v] 
  (let [oldhead (:list @cache)
        m (:map @cache)
        newhead (ref {:prev nil :next nil :data v})]
    (dosync
      (ref-set cache (assoc @cache :list newhead)) 
      (if (nil? @oldhead)
        (do 
          (ref-set m {k newhead}) 
          (ref-set (:last @cache) newhead)) 
        (do 
          (ref-set newhead (assoc @newhead :next oldhead)) 
          (ref-set oldhead (assoc @oldhead :prev newhead))
          (ref-set m (assoc @m k newhead))))))
  nil)

(defn hash-dll-del! [cache k]
  (let [curhead (:list @cache) 
        m (:map @cache)]
    (when-let [todelete (get @m k)]
      (dosync 
        (if (nil? (:prev @todelete)) ;head of list
          (ref-set curhead (:next @curhead)) 
          (do 
            (when (nil? (:next @todelete)) ;tail of list
              (ref-set cache (assoc @cache :last (:prev @todelete))))
            (ref-set (:prev @todelete) (assoc @(:prev @todelete) :next (:next @todelete))) ;overwrite next pointers
            (ref-set (:next @todelete) (assoc @(:next @todelete) :prev (:prev @todelete))))) ;overwrite prev pointers
        (ref-set m (dissoc @m k))))) ;always dissoc from map regardless of list location
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
