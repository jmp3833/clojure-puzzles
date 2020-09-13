(defn ed 
  ([s1 s2] 
   (ed s1 s2 (- (count s1) 1) (- (count s2) 1)))
  ([s1 s2 i j] 
   (let [vi (get s1 i)
         vj (get s2 j)]
     (cond 
       (nil? vi) (+ j 1) ;;Exhaust vec j by adding chars
       (nil? vj) (+ i 1) ;;Exhaust vec i by deleting chars
       (= vi vj) (ed s1 s2 (- i 1) (- j 1)) ;;Skip
       :else (min ;;del, add, replace
                  (+ 1 (ed s1 s2 (- i 1) j))
                  (+ 1 (ed s1 s2 i (- j 1)))
                  (+ 1 (ed s1 s2 (- i 1) (- j 1))))))))

(ed (vec "horse") (vec "ros"))
