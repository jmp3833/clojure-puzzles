(ns clj-puzzles.4-keys-keyboard.keys)

(defn keyboard 
  ([n] (keyboard n 0 0 (atom {})))
  ([n a b memo] 
   (cond
     (some? (get-in @memo [n a b])) (get-in @memo [n a b])
     (<= n 0) a 
     :else (let [r (max 
                     (keyboard (dec n) (inc a) b memo) 
                     (keyboard (dec (dec n)) a a memo) 
                     (keyboard (dec n) (+ a b) b memo))] 
             (swap! memo assoc-in [n a b] r)
             r)))) 

(defn compute-dp [dp i]
  (if (< i 3) 
    i
    (let [js (vec (range 2 i))
          pastes (mapv (fn [j] (* (inc (- i j)) (get dp (- j 2)))) js)
          type-a (inc (get dp (dec i)))] 
      (println pastes)
      (max type-a (reduce max pastes)))))

(defn keyboard-dp 
  ([n] (keyboard-dp n [] 0))
  ([n dp i]
   (let [dp-n (assoc dp i (compute-dp dp i))] 
     (if (= i n) (get dp-n i) (recur n dp-n (inc i))))))

