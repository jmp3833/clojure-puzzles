(ns clj-puzzles.spiral.spiral)

(defn oob [n r' c' result] 
  (or (< r' 0) (= r' n) (< c' 0) (= c' n) 
      (let [f (get-in result [c' r'])] (and (some? f) (not= f 0)))))

(defn spiral 
  ([n] (spiral n (* n n) '[1 0 -1 0] '[0 1 0 -1] 0 0 0 1 (into [] (repeat n (into [] (repeat n 0))))))
  ([n limit dir-row dir-column dir r c cur result]
   (let [rep (partial spiral n limit dir-row dir-column)
         r' (+ r (get dir-row dir))
         c' (+ c (get dir-column dir))
         result' (assoc-in result [c r] cur)]
     (cond 
       (> cur limit) result
       (oob n r' c' result)
       (let [dir' (if (= dir (dec n)) 0 (inc dir))]
         (rep dir' (+ r (get dir-row dir')) (+ c (get dir-column dir')) (inc cur) result'))
       :else (rep dir r' c' (inc cur) result')))))
