(ns game-of-life.core)

(defn is-alive? [state]
  (> state 0)
  )

(defn step-cell [state num-live-neighbors]
  (cond
   (and (is-alive? state) (< num-live-neighbors 2)) 0
   (and (is-alive? state) (or (= 2 num-live-neighbors) (= 3 num-live-neighbors))) 1
   (and (is-alive? state) (> num-live-neighbors 3)) 0
   (and (not (is-alive? state)) (= num-live-neighbors 3)) 1 
   :else -1)
  )

(defn step [cells]
  (vec (repeat (count cells) 0))
  )

(defn update-cell [cell]
  (let [state (:state cell)
        n (:neighbors cell)]
    ;; (println state, n)
    (step-cell state n))
  )

(defn update-cells [cells]
  (vec (map update-cell cells))
  )

(defn grid [n1 n2]
  (let [ xyseq 
        (for [ x (range 0 n1)
              y (range 0 n2)]
          [x y])]

    (vec xyseq)
  ))

(defn grid2 [n1 n2]
  (let [ xyseq 
        (for [ x (range 0 n1)
              y (range 0 n2)]
          {:posn [x y] :state 0})]

    (vec xyseq)
  ))
       

  (defn neighbors? [posn1 posn2]
    (let [dx1 (Math/abs (- (first posn2) (first posn1)))
          dx2 (Math/abs (- (second posn2) (second posn1)))]

      (cond
       (= posn1 posn2) false
       :else (and (<= dx1 1) (<= dx2 1))
       )
      )
    )

(defn neighbors-of [grid posn]
  (let [x1 (first posn)
        x2 (second posn)]

    (loop [posns grid neighbors []]
      (cond
       (empty? posns) neighbors
       (neighbors? posn (:posn (first posns))) (recur
                                        (drop 1 posns)
                                        (conj neighbors (first posns)))
       :else (recur
              (drop 1 posns)
              neighbors)
       
       )))
  )
