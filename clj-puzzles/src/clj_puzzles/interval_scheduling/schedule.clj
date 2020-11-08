(ns clj-puzzles.interval-scheduling.schedule)

(defn prune-sorted-intervals [intervals cur total]
  (let [interval (first intervals)]
    (cond 
      (empty? intervals) total
      (< (first interval) (second cur)) (recur (rest intervals) cur total)
      :else (recur (rest intervals) interval (inc total)))))

(defn max-intervals [intervals]
  (let [sorted-end-first (sort #(- (second %1) (second %2)) intervals)]
    (prune-sorted-intervals (rest intervals) (first intervals) 1)))
