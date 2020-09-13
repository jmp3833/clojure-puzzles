(defrecord Path [val type])

(defn pick-ed [s1 s2 i j dp]
  (cond
    (and (= i 0) (= j 0)) (Path. 0 :start)
    (= i 0) (Path. j :add)
    (= j 0) (Path. i :del)
    (= (get s1 (dec i)) (get s2 (dec j))) (Path. (get-in dp [(dec i) (dec j) :val]) :skip)
    :else (min-key :val
                   (Path. (inc (get-in dp [(dec i) (dec j) :val])) :replace)
                   (Path. (inc (get-in dp [i (dec j) :val])) :add)
                   (Path. (inc (get-in dp [(dec i) j :val])) :del))))

(defn gen-dp [s1 s2]
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
          result
          (recur (inc i) result))))))

(defn gen-path 
  "Traverse backwards in dp following Path instructions"
  ([dp] (gen-path dp (dec (count dp)) (dec (count (first dp))) []))
  ([dp i j result] 
   (reverse 
     (if (and (zero? i) (zero? j)) result
       (let [type (get-in dp [i j :type])
             withtype (conj result type)
             deci (max 0 (dec i))
             decj (max 0 (dec j))] 
         (cond 
           (= type :add) (gen-path dp i decj withtype)
           (= type :del) (gen-path dp deci j withtype)
           (= type :replace) (gen-path dp deci decj withtype)
           :else (gen-path dp deci decj withtype)))))))

(gen-path (gen-dp "rad" "apple"))
