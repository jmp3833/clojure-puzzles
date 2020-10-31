(ns clj-puzzles.stones-game.stones)

(defn- awonby
  ([piles] (awonby piles true 0))
  ([piles turn ascore] 
   (let [inv (not turn)
         fpartial (partial + (first piles))
         fpartial- (partial - (first piles))
         lpartial- (partial - (last piles))
         lpartial (partial + (last piles))]
     (cond
       (empty? piles) ascore
       :else (let [left (first piles)
                   right (last piles)] 
               (if turn 
                 (max 
                   (awonby (rest piles) inv (fpartial ascore))
                   (awonby (butlast piles) inv (lpartial ascore)))
                 (min
                   (awonby (rest piles) inv (fpartial- ascore))
                   (awonby (butlast piles) inv (lpartial- ascore)))))))))
(defn stones 
  [piles] (let [r (awonby piles)
                total (reduce + piles)
                b (/ (- total r) 2)
                a (- total b)]
            {:a a :b b :total total :wonby r}))

