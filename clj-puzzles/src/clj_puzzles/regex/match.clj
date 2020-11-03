(ns clj-puzzles.regex.match)

(defn dot-or-equal [c1 c2] 
  (or (= c1 c2) (= c2 \.)))

(defn matches 
  ([s1 pattern] (matches s1 pattern nil))
  ([s1 pattern prev]
   (let [c (first s1)
         p (first pattern)
         next-star? (= (second pattern) \*)]
     (cond 
       (and (nil? c) (nil? p)) true
       next-star? (recur 
                    (if (not= c p) s1 (drop-while #(dot-or-equal % p) s1))
                    (rest (rest pattern))
                    nil)
       (dot-or-equal c p) (recur (rest s1) (rest pattern) p)
       :else false))))

(defn- compute-dp [s p dp i j] 
  (assoc-in 
    dp 
    [i j] 
    (cond 
      (and (zero? i) (zero? j)) true
      (zero? i) false
      (zero? j) false
      :else (let [pv (get p (dec j))
                  pvd (get p (dec (dec j)))
                  sv (get s (dec i))] 
              (cond
                (or (= pv \.) (= pv sv)) (get-in dp [(dec i) (dec j)])
                (= pv \*) (if 
                            (= sv pvd) 
                            (get-in dp [(dec i) j])        ; prev form w/ same pattern
                            (get-in dp [i (dec (dec j))])) ; eliminate char* block entirely
                :else false)))))

(defn matches-dp 
  ([s p] (matches-dp s p (into [] (repeat (inc (count s)) [])) 0 0))
  ([s p dp i j]
   (let [cs (count s)
         cp (count p)
         dpi (compute-dp s p dp i j)]
     (cond
       (and (= i cs) (= j cp)) (get-in dpi [i j])
       :else (recur s p dpi (if (= i cs) 0 (inc i)) (if (= i cs) (inc j) j))))))
