(ns clj-puzzles.lis.lis
  "solutions to longest increasing subsequence problem")

(def test1 [1 4 3 4 2 3])
(def test2 [10 9 2 5 3 7 101 18])
(def test3 (vec (take 100 (iterate inc 0))))
(def test4 (vec (take 1000 (iterate inc 0))))

(defn lis-recursive 
  ([s] (lis-recursive s nil (atom {})))
  ([s c memo]
   (if-let [s1 (first s)]
     (let [m (get-in @memo [s c])
           r (cond 
               (some? m) m
               (or (nil? c) (> s1 c)) 
               (max 
                 (inc (lis-recursive (rest s) s1 memo))
                 (lis-recursive (rest s) c memo))
               :else (lis-recursive (rest s) c memo))]
       (swap! memo assoc-in [s c] r)
       r)
     0))) 

(defn compute [s dp i] 
  (if (zero? i) 1
    (reduce 
      max 
      (map 
        #(if (< (get s %) (get s i)) (inc (get dp %)) 1)
        (take i (iterate inc 0))))))

(defn lis-dp 
  ([s] (lis-dp s (into [] (repeat (count s) 1)) 0))
  ([s dp i]
   "dp[i] = LIS of subsequence indices s[0]...s[i]"
   (assert (vector? s) "sequence must be a vector")
   (if-let [_ (get s i)] 
     (recur s (assoc dp i (compute s dp i)) (inc i))
     (reduce max dp))))
