(defn merge-w-limit [l1 l2 limit result]
  (let [v1 (first l1)
        v2 (first l2)]
    (cond 
      (and (nil? v1) (nil? v2)) result ;;sum of ele < limit
      (= (count result) limit) result ;;at limit
      (nil? v1) (recur l1 (rest l2) limit (conj result v2)) ;;l1 empty
      (nil? v2) (recur (rest l1) l2 limit (conj result v1)) ;;l2 empty
      (<= v1 v2) (recur (rest l1) l2 limit (conj result v1)) ;;v1 <= v2
      :else (recur l1 (rest l2) limit (conj result v2))))) ;;v1 > v2

(merge-w-limit [-1 4 7] [0 3 11] 6 [])
(merge-w-limit [-1 4 7] [0 3 11] 10 []) ;;bigger limit

