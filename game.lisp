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
		   allow '(end play build-road build-settlement
			   build-city build-devcard) only from current-player))

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
