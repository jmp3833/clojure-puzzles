(ns clj-puzzles.lcs.lcs)

(defn lcss
  "Computes the longest common subsequence between two strings"
  ([s1 s2] (lcss s1 s2 (into [] (repeat (inc (count s1)) [])) 0 0))
  ([s1 s2 dp i j] 
   (let [idx (count s1) 
         jdx (count s2)
         dpl (gen-dp s1 s2 dp i j)]
     (cond
       (and (= idx i) (= jdx j)) (get-in dpl [i j]) 
       (= idx i) (recur s1 s2 dpl 0 (inc j))
       :else (recur s1 s2 dpl (inc i) j)))))

(defn gen-dp [s1 s2 dp i j]
  "return a dp with slot [i j] populated based on lcs algorithm"
  (let [p (partial assoc-in dp [i j])]
    (cond 
      (or (= 0 i) (= 0 j)) (p 0)
      :else 
      (let [c1 (get s1 i) 
            c2 (get s2 j)
            vi (get-in dp [(dec i) j]) 
            vj (get-in dp [i (dec j)])
            vij (get-in dp [(dec i) (dec j)])]
        (if (= c1 c2) (p (inc vij)) (p (max vi vj)))))))
