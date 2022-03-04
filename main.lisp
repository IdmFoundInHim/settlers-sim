(defun able-directions (point)
  "Get direction of potential outward path in each dimension (x, y, z)"
  (if (mod (reduce #'+ point) 2)
      (vector 1 1 -1)
      (vector -1 -1 1)))

(defun able-moves (point)
  "Get movement vectors for each potential outward path from a point"
  (let ((moves (make-array 3)))
    (dotimes (i 3)
      (push (let ((move (make-array 3 :initial-element 0)))
	      (setf (elt move i) (elt (able-directions point) i))
	      move)
	    moves))
    (nreverse moves)))

(defun die () "Result of rolling a six-sided die" (+ 1 (random 5)))

(defun dice () "Result of rolling two dice" (+ (die) (die)))
 
(defclass rules ()
  ((geography 
    :initform (random-geography))
   (game
    :initform (make-adjustable-vector :element-type 'list))

(defclass cooperative ()
  ((inventory :initform (make-adjustable-vector :element-type 'symbol))))
				