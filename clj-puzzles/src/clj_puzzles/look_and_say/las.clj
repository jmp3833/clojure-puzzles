(ns clj-puzzles.look-and-say.las)

(defn las-segment 
  ([s] 
   (las-segment s nil 0 []))
  ([s prev prev-freq result]
   (cond 
     (empty? s) (conj result prev-freq prev)
     (= (first s) prev) (recur (rest s) prev (inc prev-freq) result)
     :else (recur (rest s) (first s) 1 (if (nil? prev) result (conj result prev-freq prev))))))

(defn las 
  ([n]
   (cond 
     (= n 0) [1]
     :else 
     (las-segment (las (dec n))))))
