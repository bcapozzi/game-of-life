(ns game-of-life.core-spec
  (:require [speclj.core :refer :all]
            [game-of-life.core :refer :all]))

;; Rules:  Conway's game of life
;; 1.  Any live cell with fewer than two live neighbors dies,
;;     as if caused by under-population.
;; 2.  Any live cell with two or three live neighbours lives on to the
;;     next generation.
;; 3.  Any live cell with more than three live neighbours dies, as if by
;;     overcrowding
;; 4   Any dead cell with exactly three live neighbours becomes a live
;;     cell, as if by reproduction.


(describe
 "Cell"
 (let [cells [0 0 1 0 0]]
   (it "all cells should be dead if no cell has two or more neighbors"
       (should= [0 0 0 0 0] (step cells)))
   )

 (it "should die if is alive and has less than two live neighbors"
     (should= 0 (step-cell 1 0))
     (should= 0 (step-cell 1 1)))

 (it "should live if is alive and has two or three live neighbors"
     (should= 1 (step-cell 1 2))
     (should= 1 (step-cell 1 3)))

 (it "should die if it is alive and has more than three live neighbors"
     (should= 0 (step-cell 1 4)))

 (it "should become alive if it is dead and has exactly three live neighbors"
     (should= 1 (step-cell 0 3)))

 (it "can identify neighbors"
     (should= false (neighbors? [0 0] [0 2]))
     (should= false (neighbors? [0 0] [2 0]))
     (should= false (neighbors? [0 0] [2 2]))
     (should= true (neighbors? [0 0] [0 1]))
     (should= true (neighbors? [0 0] [1 0]))
     (should= true (neighbors? [0 0] [1 1]))
     (should= true (neighbors? [1 1] [0 0]))
     (should= true (neighbors? [1 1] [2 2]))
     (should= true (neighbors? [1 1] [1 2]))
     (should= true (neighbors? [1 1] [2 1]))     
     )

 (it "does not identify itself as a neighbor"
     (should= false (neighbors? [1 1] [1 1]))     )
 
  (let [cells (grid2 3 3)]
    (it "can find neighbors of a given cell"
        (should= 3 (count (neighbors-of cells [0 0])))
        (should= 8 (count (neighbors-of cells [1 1])))
        )
    )

)
