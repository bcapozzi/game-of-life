(ns game-of-life.core)

(defn is-alive? [state]
  (> state 0)
  )

(defn matches-posn? [cell posn]
  (= (:posn cell) posn)
  )

(defn get-cell [cells posn]
  (first (filter #(matches-posn? % posn) cells))
  )

(defn is-cell-at-posn-alive? [cells posn]
  (is-alive? (:state (get-cell cells posn)))
  )

(defn unhandled-case [state num-live-neighbors]
  (println "unhandled: ", state, num-live-neighbors)
  -1
  )

(defn step-cell [state num-live-neighbors]
  "Codifies the rules of the game, returning the next state"
  (cond
   (and (is-alive? state) (< num-live-neighbors 2)) 0
   (and (is-alive? state) (or (= 2 num-live-neighbors) (= 3 num-live-neighbors))) 1
   (and (is-alive? state) (> num-live-neighbors 3)) 0
   (and (not (is-alive? state)) (= num-live-neighbors 3)) 1 
   :else 0
      ;; (unhandled-case state num-live-neighbors)
   )
  )

(defn grid2 [n1 n2]
  "Construct a two-dimensional grid given sizes n1, n2"
  (let [ xyseq 
        (for [ x (range 0 n1)
              y (range 0 n2)]
          {:posn [x y] :state 0})]

    (vec xyseq)
  ))
       

(defn neighbors? [posn1 posn2]
  "Determine if two positions are adjacent in the grid"
    (let [dx1 (Math/abs (- (first posn2) (first posn1)))
          dx2 (Math/abs (- (second posn2) (second posn1)))]

      (cond
       (= posn1 posn2) false
       :else (and (<= dx1 1) (<= dx2 1))
       )
      )
    )

(defn neighbors-of [grid posn]
  "Given a position, return all cells in the grid that are adjacent"
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

(defn matches-posn? [cell posn]
  (= (:posn cell) posn)
  )

(defn is-in-set? [cell posn-set]
  "Determine if cell position matches any of the supplied positions"
  (loop [posns posn-set result []]
    (cond
     (empty? posns) false
     (matches-posn? cell (first posns)) true
     :else (recur (drop 1 posns) result)
     )
    )
  )

(defn mark-alive [cell]
  (assoc cell :state 1)
  )

(defn mark-alive-at-posns [cells posns-to-mark]
  (for [c cells]
    (if (is-in-set? c posns-to-mark)
      (mark-alive c)
      c)
    )
  )

(defn count-if-alive [cells]
  (count (filter #( is-alive? %) (map :state cells)))
  )

(defn update-cell [grid cell]
  (let [state (:state cell)
        n (count-if-alive (neighbors-of grid (:posn cell)))
        new-state (step-cell state n)]
    
    (assoc cell :state new-state)
    
    )
  )

(defn step [cells]
  (for [c cells]
    (update-cell cells c))
  )
