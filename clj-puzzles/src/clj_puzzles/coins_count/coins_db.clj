(defn coins-count-dp-array 
  ([coins denomination] 
   (let [worst (+ 1 denomination)
         answer (coins-count-dp-array 
                  coins 
                  denomination 
                  (vec (take worst (iterate (fn [x] worst) 0))))] 
     (if (= answer worst) -1 answer)))

  ([coins denomination dp]
   (loop [answer (+ denomination 1) dp-sub dp 
          dp dp 
          i 0]
     (if (nil? (first dp-sub))
       answer
       (let [worst (+ denomination 1)
             local-optimum (loop-coins coins dp worst worst i)] 
         (recur 
           local-optimum 
           (subvec dp-sub 1) 
           (if 
             (< local-optimum worst) 
             (assoc dp i local-optimum) 
             dp)
           (+ 1 i)))))))

(defn loop-coins [coins dp optimal worst i] 
  (let [coin (first coins)
        solution (cond 
                   (nil? coin) worst
                   (< (- i coin) 0) worst
                   :else (min worst (+ 1 (get dp (- i coin)))))]
    (if (nil? coin) 
      optimal
      (recur 
        (subvec coins 1) dp (min solution optimal) worst i))))

(coins-count-dp-array '[5 15] 10)
