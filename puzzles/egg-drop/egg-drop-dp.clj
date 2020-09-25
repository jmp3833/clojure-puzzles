(defn compute-dp [dp egg-row eggs] 
  "given a precomputed vec of incrementing vals = to number of floors,
  overwite values based on previously computed values of dp [i j],
  or assign to static values if floor = 0 or eggs = 1"
  (reduce
    #(conj 
       % 
       (cond 
         (= eggs 1) %2
         (= 0 %2) 0
         (= 1 %2) 1
         :else (let [xseq (range 1 (inc %2))]
                 (apply 
                   max
                   (map (fn [x] 
                          (let [egg-not-broken (get % (- %2 x))
                                egg-broken (get-in dp [(dec (dec eggs)) x])]
                            (min 
                              (inc egg-broken)
                              (inc egg-not-broken))))
                        xseq)))))
    []
    egg-row))

(defn egg-drop-dp [eggs floors]
  "construct a matrix of size 'eggs by floors + 1'
  and compute the egg-drop solution for each permulation"
  (get-in 
    (reduce 
      (fn [dp floors] 
        (conj dp (compute-dp dp floors (inc (count dp)))))
      [] 
      (vec (repeat eggs (vec (range 0 (inc floors))))))
    [(dec eggs) floors]))

(egg-drop-dp 2 100)
