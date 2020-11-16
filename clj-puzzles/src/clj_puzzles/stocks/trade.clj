(ns clj-puzzles.stocks.trade)

(comment 
  (time (trade 5 (repeat 10 10))) ;~10ms
  (time (trade-dp (vec (repeat 100 10)) 5))) ;~8ms 
  (time (trade 5 (repeat 100 10))) ; :'(

(defn trade 
  ([k p] (trade k p false))
  ([k p h] 
   (cond
     (empty? p) 0
     (and (zero? k) (not h)) 0 ;can't buy anymore
     h (max 
         (+ (first p) (trade k (rest p) false)) ;sell
         (+ 0 (trade k (rest p) true))) ;HODL
     :else (max 
             (+ (- 0 (first p)) (trade (dec k) (rest p) true)) ;buy
             (+ 0 (trade k (rest p) false)))))) ;HODL

(defn- compute-dp [p i k h dp]
  (cond 
    (and (zero? k) (zero? h)) 0
    (zero? k) Integer/MIN_VALUE
    :else (let [prev_hold #(if (zero? i) (if (zero? %) 0 Integer/MIN_VALUE) (get-in dp [(dec i) k %]))
                prev_buy (if (zero? i) 0 (get-in dp [(dec i) (dec k) 0]))
                prev_sell (if (zero? i) Integer/MIN_VALUE (get-in dp [(dec i) k 1]))]
            (if (zero? h) 
              (max (prev_hold h) (+ prev_sell (get p i)))
              (max (prev_hold h) (- prev_buy (get p i)))))))

(defn trade-dp 
  ([p k] (trade-dp p k 0 0 0 (into [] (repeat (count p) (into [] (repeat (inc k) []))))))
  ([p totk i k h dp]
   ; i increasing from zero, k increasing from 0, h = {0 1}
   (let [dp-i (assoc-in dp [i k h] (compute-dp p i k h dp))
         fin? (and (= i (dec (count p))) (= k totk) (= h 0))]
     (cond 
       fin? (get-in dp-i [i k 0])
       (= h 0) (recur p totk i k 1 dp-i)
       (= k totk) (recur p totk (inc i) 0 0 dp-i)
       :else (recur p totk i (inc k) 0 dp-i)))))
