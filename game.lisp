(proclaim '(optimize speed))

(needs
 (fun macro-possible) random-order
 (fun macro-danger) layout-hexes ; Randomly generated parameter, likewise for next two
 (fun macro-danger) layout-ships ; above
 (fun macro-danger) layout-chances ; above
 fun layout-resources
 (fun macro-possible) query-victorypoints
 (fun macro-possible) query-victorypoints-devcards
 macro advance-game
 param hex-pieces
 param ship-pieces
 param chance-pieces
 param wood-pieces
 param brick-pieces
 param sheep-pieces
 param grain-pieces
 param ore-pices
 param devcard-pieces
 var devcards-played
 var winner
 )

;;; Game Procedures

(defgeneric random-order (array)
  (:documentation "Randomize an array regardless of the contents"))

(tests
 (random-order ()) -> ()
 (random-order (vector)) -> #()
 (random-order (list nil)) -> (NIL)
 (random-order (vector 0)) -> #(0)
 (random-order (list 2 3 3 4 4 5 5 6 6 8 8 9 9 10 10 11 11 12)) -> (...) ; chances
 (random-order #(#(#(0 0 0) #(1 0 0) #(1 1 0) #(1 1 1) #(0 1 1) #(0 0 1))
		 #(#(-1 0 1) #(0 0 1) #(0 1 1) #(0 1 2) #(-1 1 2) #(-1 0 2))
		 #(#(0 1 1) #(1 1 1) #(1 2 1) #(1 2 2) #(0 2 2) #(0 1 2))
		 #(#(1 1 0) #(2 1 0) #(2 2 0) #(2 2 1) #(1 2 1) #(1 1 1))
		 #(#(-2 0 2) #(-1 0 2) #(-1 1 2) #(-1 1 3) #(-2 1 3) #(-2 0 3))
		 #(#(-1 1 2) #(0 1 2) #(0 2 2) #(0 2 3) #(-1 2 3) #(-1 1 3))
		 #(#(0 2 2) #(1 2 2) #(1 3 2) #(1 3 3) #(0 3 3) #(0 2 3))
		 #(#(1 2 1) #(2 2 1) #(2 3 1) #(2 3 2) #(1 3 2) #(1 2 2))
		 #(#(2 2 0) #(3 2 0) #(3 3 0) #(3 3 1) #(2 3 1) #(2 2 1))
		 #(#(-2 1 3) #(-1 1 3) #(-1 2 3) #(-1 2 4) #(-2 2 4) #(-2 1 4))
		 #(#(-1 2 3) #(0 2 3) #(0 3 3) #(0 3 4) #(-1 3 4) #(-1 2 4))
		 #(#(0 3 3) #(1 3 3) #(1 4 3) #(1 4 4) #(0 4 4) #(0 3 4))
		 #(#(1 3 2) #(2 3 2) #(2 4 2) #(2 4 3) #(1 4 3) #(1 3 3))
		 #(#(2 3 1) #(3 3 1) #(3 4 1) #(3 4 2) #(2 4 2) #(2 3 2))
		 #(#(-2 2 4) #(-1 2 4) #(-1 3 4) #(-1 3 5) #(-2 3 5) #(-2 2 5))
		 #(#(-1 3 4) #(0 3 4) #(0 4 4) #(0 4 5) #(-1 4 5) #(-1 3 5))
		 #(#(0 4 4) #(1 4 4) #(1 5 4) #(1 5 5) #(0 5 5) #(0 4 5))
		 #(#(1 4 3) #(2 4 3) #(2 5 3) #(2 5 4) #(1 5 4) #(1 4 4))
		 #(#(2 4 2) #(3 4 2) #(3 5 2) #(3 5 3) #(2 5 3) #(2 4 3)))) ; hexes by points
 )

(defmethod random-order ((array list))
  (labels ((pick-object (out length in)
	     (let ((item (nth (random length) in)))
	       (if (= 1 length) (append out (list item)) (funcall #'pick-object (append out (list item)) (1- length) (remove item in :count 1))))))
    (if array (funcall #'pick-object '() (length array) array))))

(defmethod random-order ((array vector))
  (labels ((pick-object (out length in)
	     (let ((item (elt in (random length))))
	       (if (= 1 length) (concatenate 'vector out (vector item)) (funcall #'pick-object (concatenate 'vector out (vector item)) (1- length) (remove item in :count 1))))))
    (if (not (equalp #() array)) (funcall #'pick-object (make-array 0) (length array) array))))  

(defmacro order-chances () ; Alternatives methods in rules. If you desire those, modify this.
  '(random-order chance-pieces))

;;; Turn Phases

(defun build-initial-phase (current-player)
  (process-actions until build-initial
		   only allow build-initial only from current-player))

(defun build-resource-phase (current-player)
  (process-actions until build-resource
		   only allow build-resource only from current-player))

(defun roll-phase (current-player)
  (setf devcards-played 0)
  (process-actions until roll
		   allow '(roll play) only from current-player))

(defun build-trade-phase (current-player everyone)
  (process-actions until end
		   only allow trade from everyone ; Trades need to involve current-player
		   allow '(end play build-road build-settlement build-city build-devcard) only from current-player))

(defun end-phase (current-player board inventories)
  (if (< 9 (+ (query-victorypoints board current-player)
	      (query-victorypoints-devcards inventories current-player)))))  

;;; Entire Game Progression in order

(defun layout-board (hex-pieces ship-pieces chance-pieces)
  (layout-hexes (random-order hex-pieces))
  (layout-ships (random-order ship-pieces))
  (layout-chances (order-chances) hexes))

(defun layout-bank (wood-pieces brick-pieces sheep-pieces grain-pieces ore-pieces devcard-pieces)
  (layout-resources wood-pieces brick-pieces sheep-pieces grain-pieces ore-pieces)
  (random-order devcard-pieces))

(defun assign-colors nil)

(defun layout-inventories (player-colors road-player-pieces settlement-player-pieces city-player-pieces)
  (loop for color in player-colors
	collect (make-inventory color road-player-pieces settlement-player-pieces city-player-pieces)))

(defun assign-order nil)

(defun start-game (player-order)
  ;; Serpentine draft, implemented recursively
  (let ((remaining-players (lamdba (order)
				   (build-initial-phase (car order))
				   (remaining-players (cdr order))
				   (build-resource-phase (car order)))))
    (remaining-players player-order)))

(defun game-loop nil)
