(require '[clojure.string :as str])

(def example "1-2--3--4-5--6--7")
(def numbers (str/split example #"[-]+"))
(def dashes (str/split example #"[0-9]+"))

(def dash-counts (map (fn [dash] (count dash)) dashes))

(def nodes 
  (map (fn [x y] 
         (hash-map 
           :depth x 
           :val (Integer/parseInt y))) 
       dash-counts numbers))

(defn push 
  ([val] (push '() val))
  ([stack val] (conj stack val)))

(defn pop
  [stack] (hash-map 
            :val (first stack) 
            :stack (rest stack)))

(defn peek [stack] (first stack)) 

(defn t [tstack node] 
  (let [{tree :tree stack :stack} tstack]
    (cond 
      (nil? tree) 
      (hash-map
        :tree (hash-map :val (get node :val) :left nil :right nil)
        :stack (push stack node))
      (> (get node :depth) (get (peek stack) :depth))
      (t (hash-map 
           :tree (get tree :left) 
           :stack stack)
         node))))

(t (t nil (first nodes)) (second nodes))

(defn build-from-inorder 
  ([nodes] (build-from-inorder 
             (rest nodes)
             (hash-map :root (get (first nodes) :val))
             (push (first nodes))))
  ([nodes, tree, stack]
   (cond 
     (nil? (first nodes)) stack
     (> 
       (get (first nodes) :depth)
       (get (peek stack) :depth))
     ;;Push on the stack and add to left of node 
     (build-from-inorder 
       (rest nodes) 
       tree
       (push stack (first nodes)))

     (< 
       (get (first nodes) :depth)
       (get (peek stack) :depth)) 
     (build-from-inorder 
       (rest nodes) 
       tree 
       (push stack (first nodes)))

     (=
      (get (first nodes) :depth)
      (get (peek stack) :depth)) 
     (build-from-inorder 
       (rest nodes) 
       tree 
       (push stack (first nodes)))
     )))

(defn split-inorder-dashes
  "Takes a sequence of dash counts and groups by first element, 
  2nd element to next element of the same value (exclusive),
  and the remainder."
  [coll] 
  (def split-remainder (split-with 
                         #(not= (second coll) %1) 
                         (rest (rest coll))))
  (hash-map 
    :root (first coll)
    :left (cons (second coll) (get split-remainder 0))
    :right (get split-remainder 1)))

(defrecord Node [el left right])

(defn build-from-inorder 
  "Takes output from split-inorder-dashes and builds into a binary tree"
  [split, tree, help] 
  (def lsubtree (split-inorder-dashes (get split :left)))
  (def rsubtree (split-inorder-dashes (get split :right)))

  (println "split" split)
  (println "lsubtree" lsubtree)
  (println "rsubtree" rsubtree)
  (println "recurring on " help)

  (cond 
    (< (+ 
         (count(get split :left))
         (count(get split :right))
         ) 3) 
    (hash-map
      :val (get split :root)
      :left (first (get split :left))
      :right (first (get split :right)))
    :else 
    (hash-map
      :val (get split :root)
      :left (build-from-inorder lsubtree tree "left")
      :right (build-from-inorder rsubtree tree "right"))))

(build-from-inorder (split-inorder-dashes dash-counts) (hash-map) "root")
