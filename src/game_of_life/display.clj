(ns game-of-life.display
  ( :require [game-of-life.core :refer :all]))

(import '(javax.swing JFrame JPanel )
        '(java.awt Color Graphics Graphics2D))

(def dim 3)

(def world2d
  (apply vector
         (map (fn [_]
                (apply vector (map (fn [_] (ref (struct cell 0)))
                                   (range dim))))
              (range dim))))

(defn place2d [[x y]]
  (-> world2d (nth x) (nth y)))

(defn init-state []
  (doall
   (for [x (range dim) y (range dim)]
     (sync nil
           (let [p (place2d [x y])
                 r (rand-int 2)]
             (if (> r 0) (alter p assoc :state 1))
             ))
     ))
  )

(defn draw-cell [g w h cell nx ny]
  (let [x1 (first (:posn cell))
        x2 (second (:posn cell))
        dx (* w  (/ x1 nx))
        dy (* h  (/ x2 ny))
        cw (/ w nx)
        ch (/ h ny)]

    (if (> (:state cell) 0)
      (.setColor g (new Color 0 255 0 255))
      (.setColor g (new Color 255 0 0 255)))
    
    (.fillRect g dx dy cw ch)

    (.setColor g (new Color 255 255 255 255))
    (.drawRect g dx dy cw ch)
    )


  )

(defn get-num-x [cells]
  (count (distinct (map last (map :posn cells))))
  )

(defn get-num-y [cells]
  (count (distinct (map last (map :posn cells))))
  )

(defn draw-cells [g w h cells]
  (let [nx (get-num-x cells)
        ny (get-num-y cells)]

    (loop [cells cells]
      (if (> (count cells) 0)
        (let [curr (first cells)]
          (draw-cell g w h curr nx ny)
          (recur (drop 1 cells))))
      )

    )
  )

(defn get-cells []

  (doall (for [x (range dim) y (range dim)]
           {:posn [x y] :state (:state @(place2d [x y]))}
            ))
  )

(defn render [ #^Graphics g w h ]
  (doto g
    (.setColor (Color/BLACK))
    (.fillRect 0 0 w h)
    (.setColor (Color/GREEN))
    ;; (.drawArc 200 200 20 20 0 360)
    )

  (draw-cells g w h (get-cells))
  
  )

(defn create-panel []
    "Create a panel with a customised render"

  (proxy [JPanel] []
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (render g (. this getWidth) (. this getHeight)))))


(defn update-state []
  (let [cells (get-cells)
        new-cells (step cells)]
    (doall (for [c new-cells]
             (sync nil
                   (let [p (place2d (:posn c))]
                     (alter p assoc :state (:state c))))))

    )
  )

(defn run [nsteps]

  (init-state)
  (let [frame (JFrame. "Conway's Game of Life")
        panel (create-panel)]

    (doto frame
      (.add panel)
      (.setSize 640 400)
      (.setVisible true))

    (loop [n 0]
      (update-state)
      (.  panel (repaint))
      (. Thread (sleep 1000))
      (cond
       (>= n nsteps) nil
       :else (recur (inc n)))
      ))
  )


