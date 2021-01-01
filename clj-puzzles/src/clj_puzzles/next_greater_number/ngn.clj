(ns clj-puzzles.next-greater-number.ngn)

(defn ngn 
  "returns a vec of the the next greater integer 
  for every value in vec nums, 
  or nil if one doesn't exist"
  ([nums] (ngn nums (dec (count nums)) '()))
  ([nums i stack] 
   (assert (vector? nums) "nums must be a vec of int")
   (assert (seq? stack) "stack must be seqable")
   (if-let [n (get nums i)]
     (if-let [top (first stack)]
       (if
         (<= top n) (ngn nums i (rest stack)) 
         (ngn (assoc nums i (first stack)) (dec i) (cons n stack))) 
       (ngn (assoc nums i nil) (dec i) (cons n stack)))
     nums)))

(defn ngn-delta 
  "similar to ngn, but computes the delta of the indices 
  between the current element and its ngn. Zero if no ngn exists"
  ([nums] (ngn-delta nums (dec (count nums)) '()))
  ([nums i stack] 
   (assert (vector? nums) "nums must be a vec of int")
   (assert (seq? stack) "stack must be seqable")
   (if-let [n (get nums i)]
     (if-let [[top-ele top-idx] (first stack)]
       (if
         (<= top-ele n) (ngn-delta nums i (rest stack)) 
         (ngn-delta (assoc nums i (- top-idx i)) (dec i) (cons [n i] stack))) 
       (ngn-delta (assoc nums i 0) (dec i) (cons [n i] stack)))
     nums)))

(defn ngn-circular
  "similar to ngn, but with a circular array."
  ([nums] 
   (let [n (count nums)] (ngn-circular nums (dec (* 2 n)) '() n (into [] (repeat n 0)))))
  ([nums i stack len result] 
   (if (< i 0) 
     result
     (let [idx (mod i len)
           n (get nums idx)]
       (if-let [top (first stack)]
         (if
           (<= top n) 
           (ngn-circular nums i (rest stack) len result) 
           (ngn-circular nums (dec i) (cons n stack) len (assoc result idx (first stack)))) 
         (ngn-circular nums (dec i) (cons n stack) len (assoc result idx nil)))))))

(comment 
  (assert (= (ngn '[2 1 2 4 3]) '[4 2 4 nil nil]))
  (assert (= (ngn-delta '[73 74 75 71 69 72 76 73]) '[1 1 4 2 1 1 0 0]))
  (assert (= (ngn-circular '[2 1 2 4 3]) '[4 2 4 nil 4])))
