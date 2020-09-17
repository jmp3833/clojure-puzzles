(defn egg-drop [eggs floors] 
  (cond 
    (zero? floors) 0
    (zero? floors) floors
    :else (apply 
            min 
            (map 
              #(inc (max 
                      (egg-drop (dec eggs) (dec %))
                      (egg-drop eggs (- floors %))))
              (range 1 (inc floors))))))

(def egg-drop-memo (memoize egg-drop))
