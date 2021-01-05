(ns clj-puzzles.edit-distance.ed-simple)

(defn ed 
  ([s1 s2] (ed s1 s2 0 0 s1 s2))
  ([s1 s2 i j seg1 seg2]
   (let [c1 (first seg1) c2 (first seg2)]
     (cond
       (and (nil? c1) (nil? c2)) true
       (nil? c1) (= (str s1 c2) s2) ;can only add char
       (nil? c2) (= (butlast s1) (into [] s2)) ;can only del char
       (= c1 c2) (recur s1 s2 (inc i) (inc j) (rest seg1) (rest seg2))
       :else
       (let [add (str (subs s1 0 i) c2 (subs s1 i))
             del (if (zero? i) (subs s1 1) (str (subs s1 0 i) (subs s1 (inc i))))
             replce (if (zero? i) (str c2 (subs s1 (inc i))) (str (subs s1 0 i) c2 (subs s1 (inc i))))]
         (or 
           (= s2 replce)
           (= s2 del)
           (= s2 add)))))))
