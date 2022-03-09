(defun able-directions (point)
  "Get direction of potential outward path in each dimension (x, y, z)"
  (if (mod (reduce #'+ point) 2)
      (vector 1 1 -1)
      (vector -1 -1 1)))

(defun able-moves (point)
  "Get movement vectors for each potential outward path from a point"
  (let ((moves (make-array 3 :fill-pointer 0)))
    (dotimes (i 3)
      (vector-push (let ((move (make-array 3 :initial-element 0)))
		     (setf (elt move i) (elt (able-directions point) i))
		     move)
		   moves))
    (nreverse moves)))

(defun able-destinations (point)
  "Get potential destinations moving outward from a point"
  (loop for move across (able-moves point)
     collect (+vector point move)))

(defun die () "Result of rolling a six-sided die" (+ 1 (random 5)))

(defun dice () "Result of rolling two dice" (+ (die) (die)))
 
(defclass rules ()
  ((geography 
    :initform (random-geography))
   (game
    :initform (make-adjustable-vector :element-type 'list))

(defclass cooperative ()
  ((inventory :initform (make-adjustable-vector :element-type 'symbol))))
				
(defun random-geography ()
  "Generate a new random geography according to base game rules"
  (let ((geography (make-adjustable-vector :element-type 'list)))
    geography))