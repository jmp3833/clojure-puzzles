(ns clj-puzzles.stones-game.stones-dp)

(defn compute-dp [dp piles i j]
  (let [dpull (partial get-in dp)
        s (partial get piles)
        take-left (+ (s i) (dpull [(inc i) j :l]))
        take-right (+ (s j) (dpull [i (dec j) :l]))]
    (if (> take-left take-right)
      {:l (dpull [(inc i) j :f]) :f take-left}
      {:l (dpull [i (dec j) :f]) :f take-right})))

(defn init-dp 
  ([piles]
   (let [c (count piles)]
     (init-dp
       piles 0 0
       (into [] (repeat c (into [] (repeat c 0)))))))
  ([piles i j dp]
   (cond
     (= i (count piles)) dp
     :else 
     (let [dp-up (assoc-in dp [i j] {:f (get piles i) :l 0})]
       (recur piles (inc i) (inc j) dp-up)))))

(defn stones 
  ([piles] (stones (init-dp piles) piles 0 1 1))
  ([dp piles i j orig-j]
   (let [c (count piles)
         endj (= (dec c) j)
         put (partial assoc-in dp [i j])
         dp-ij (put (compute-dp dp piles i j))]
     (cond
       (and endj (zero? i)) (get-in dp-ij [0 j])
       endj (recur dp-ij piles 0 (inc orig-j) (inc orig-j))
       :else (recur dp-ij piles (inc i) (inc j) orig-j)))))
