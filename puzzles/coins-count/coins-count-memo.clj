(defn coins-count-dp-array
  ([coins denomination]
   (coins-count-dp-array
     coins
     denomination
     (take denomination (iterate (fn [x] (+ 1 denomination)) 0))))

  ;; i - coin < 0 ? invalid no update
  ;; take min of current vector value OR dp[i - coin] - 1
  ([coins denomination dp]
   (loop [dp-sub dp
          i 0]
     (if (some? (first dp-sub))
       (loop [coins coins
              optimal (+ 1 denomination)]
         (let [coin (first coins)
               cur (- i coin)
               (println "cur " cur)
               solution (cond
                          (nil? coin) (+ 1 denomination)
                          (< cur 0) (+ 1 denomination)
                          :else (get dp cur))]
           (println coin)
           (println solution)

           (if (nil? coin)
             optimal
             (recur
               (rest coins) (min solution optimal)))))

       (recur (rest dp-sub) (+ 1 i))))))

(coins-count-dp-array '[5 15] 10)
~
