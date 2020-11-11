(ns clj-puzzles.kmp.kmp)

(defn- compute-dp-val
  [w i j x dp] 
  (let [c (get w i)
        eq (= c (char j))]
    (cond 
      (and (zero? i) (= (first w) (char j))) 1
      (zero? i) 0
      eq (inc i) 
      :else (get-in dp [x j]))))

(defn init-dp
  "Returns a 2-d array 
  [size (inc count w) * 256 ;all ASCIIs]
  encoded state machine to solve KMP for the given word w."
  ([w] (init-dp w 0 0 0 (into [] (repeat (count w) []))))
  ([w i j x dp] 
   (if (= i (count w)) dp
     (let [nxt (compute-dp-val w i j x dp)
           dp-i (assoc-in dp [i j] nxt)
           nextx (if (zero? i) 0 (get-in dp [x (int (get w i))]))]
       (if (= j 255) 
         (recur w (inc i) 0 nextx dp-i)
         (recur w i (inc j) x dp-i))))))

(defn kmp 
  "An impl of the Knuth-Morris-Pratt string search algo
  using a 2-d array encoded state machine. 
  returns 1 if word w is in string s, otherwise 0"
  ([s w] (kmp s w (init-dp w) 0))
  ([s w dp st] 
   (cond 
     (= st (count w)) 1
     (empty? s) 0
     :else (recur (rest s) w dp (get-in dp [st (int (first s))])))))

(defn print-state-machine [dp]
  "A helper to print chars alongside any state transition that does not
  go to state zero, grouped by each state"
  (map (fn [state] 
         (filter 
           #(not (zero? (get % 1))) 
           (map-indexed (fn [i x] [(char i) x]) state))) dp))
