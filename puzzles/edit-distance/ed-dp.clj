(defn pick-ed [s1 s2 i j dp]
  (cond
    (and (= i 0) (= j 0)) 0
    (= i 0) j
    (= j 0) i
    (= (get s1 (dec i)) (get s2 (dec j))) (get-in dp [(dec i) (dec j)])
    :else (min
            (inc (get-in dp [(dec i) (dec j)]))
            (inc (get-in dp [i (dec j)]))
            (inc (get-in dp [(dec i) j])))))

(defn ed-dp [s1 s2]
  (let [dpi (count s1)
        dpj (inc (count s2))]
    (loop [i 0
           dp-out (vec (repeat (inc dpi) []))]
      (let [result
            (loop [j 0 dp-i dp-out]
              (if (= j dpj)
                dp-i
                (recur
                  (inc j)
                  (update-in dp-i [i] conj (pick-ed s1 s2 i j dp-i)))))]
        (if (= i dpi)
          (get-in result [dpi (dec dpj)])
          (recur (inc i) result))))))

(ed-dp "hor" "horse")


