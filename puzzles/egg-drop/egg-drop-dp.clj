(defn compute-dp [dp egg-row eggs] 
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
                          (min 
                            (inc (get % (- %2 x)))
                            (inc (get-in dp [(dec (dec eggs)) x]))))
                        xseq)))))
    []
    egg-row))

(defn egg-drop-dp [eggs floors]
  (reduce 
    (fn [dp floors] 
      (conj dp (compute-dp dp floors (inc (count dp)))))
    [] 
    (vec (repeat eggs (vec (range 0 (inc floors)))))))

(egg-drop-dp 3 14)
