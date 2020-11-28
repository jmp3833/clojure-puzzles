(ns clj-puzzles.lis.lis
  "solutions to longest increasing subsequence problem")

(def test1 [1 4 3 4 2 3])
(def test2 [10 9 2 5 3 7 101 18])
(def test3 (take 1000 (iterate inc 0)))

(defn lis-recursive 
  ([s] (lis-recursive s nil 0 (atom {})))
  ([s c l memo]
   (if-let [s1 (first s)]
     (let [m (get-in @memo [s c l])
           r (cond 
               (some? m) m
               (or (nil? c) (> s1 c)) 
               (max 
                 (lis-recursive (rest s) s1 (inc l) memo) 
                 (lis-recursive (rest s) c l memo))
               :else (lis-recursive (rest s) c l memo))]
       (swap! memo assoc-in [s c l] r)
       r)
     l))) 
