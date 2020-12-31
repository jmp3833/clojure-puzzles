(ns clj-puzzles.next-greater-number.ngn)

(defn ngn 
  "returns a vec of the the next greater integer 
  for every value in vec nums, 
  or nil if one doesn't exist"
  ([nums] (ngn nums (dec (count nums)) '()))
  ([nums i stack] 
   (assert (vector? nums) "nums must be a vec of int")
   (if-let [n (get nums i)]
     (if-let [top (first stack)]
       (if
         (<= top n) (ngn nums i (rest stack)) 
         (ngn (assoc nums i (first stack)) (dec i) (cons n stack))) 
       (ngn (assoc nums i nil) (dec i) (cons n stack)))
     nums)))

(assert (= (ngn '[2 1 2 4 3]) '[4 2 4 nil nil]))
