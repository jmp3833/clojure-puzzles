(ns clj-puzzles.dst.lru-cache)

(defn hash-dll-add!
  [cache k v] 
  (let [oldhead (:list @cache)
        m (:map @cache)
        newhead (ref {:prev nil :next nil :data v :k k})]
    (dosync
      (ref-set cache (assoc @cache :list newhead)) 
      (if (nil? @oldhead)
        (do 
          (ref-set m {k newhead}) 
          (ref-set cache (assoc @cache :last newhead))) 
        (do 
          (ref-set newhead (assoc @newhead :next oldhead)) 
          (ref-set oldhead (assoc @oldhead :prev newhead))
          (ref-set m (assoc @m k newhead))))))
  nil)

(defn hash-dll-del! [cache k]
  (dosync
    (let [curhead (:list @cache) 
          m (:map @cache)]
      (when-let [todelete (get @m k)]
        (cond
          (and (nil? (:prev @todelete)) (nil? (:next @todelete))) (ref-set curhead nil)
          (nil? (:prev @todelete)) 
          (do 
            (ref-set cache (assoc @cache :list (:next @todelete)))
            (ref-set (:next @todelete) (assoc @(:next @todelete) :prev nil)))
          (nil? (:next @todelete)) 
          (do 
            (ref-set cache (assoc @cache :last (:prev @todelete)))
            (ref-set (:prev @todelete) (assoc @(:prev @todelete) :next (:next @todelete))))
          :else 
          (do
            (ref-set (:prev @todelete) (assoc @(:prev @todelete) :next (:next @todelete))) 
            (ref-set (:next @todelete) (assoc @(:next @todelete) :prev (:prev @todelete))))))
      (ref-set m (dissoc @m k))))
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
        (when (<= (:size @c) (count @m))
          (hash-dll-del! c (:k @(:last @c))))
        (hash-dll-add! c k v)
        v))))

;print helpers and test cases

(defn str-list 
  ([l accessor] (str-list l "" accessor))
  ([l sb accessor] 
   (let [node @l
         v (str sb (:data node))]
     (if (nil? (accessor node)) 
       v
       (recur (accessor node) (str v "<->") accessor)))))

(defn str-map [m]
  (map (fn [[k v]] (str k "->" (:data @v))) m))

(comment 
  (set! *print-level* 3)                             ; recursive data structure :'(

  (def c (cache/init! 3))                            ; []
  (cache/get! c 1 (fn [] 10))                        ; [[1 10]]
  (cache/get! c 1 (fn [] "cache hit! not invoked"))  ; [[1 10]]
  (cache/get! c 2 (fn [] 20))                        ; [[2 20] [1 10]]
  (cache/get! c 3 (fn [] 30))                        ; [[3 30] [2 20] [1 10]]
  (cache/get! c 4 (fn [] 40)))                       ; [[4 40] [3 30] [2 20]]

(comment 
  (defn setup []
    (def c (init! 3)) 
    (get! c 1 (fn [] 10))                        
    (get! c 2 (fn [] 20))                       
    (get! c 3 (fn [] 30))))
