(ns clj-puzzles.dst.linked-list)

(defn dll-add
  ([v] (dll-add v nil))
  ([v l] {:v v :p nil :n (if (nil? l) nil (assoc l :p v))}))

(defn hash-dll-add 
  ([v] (hash-dll-add [{} nil] v))
  ([[h l] v]
   (let [n (dll-add v l)]
     [(assoc h v n) n])))

(comment 
  (clojure.pprint/pprint 
    (reduce ll/hash-dll-add (ll/hash-dll-add 10) '[20 30 40 50])))
