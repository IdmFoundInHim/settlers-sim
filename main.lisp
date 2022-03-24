(defparameter *points-literal*
  '(#(3 3 5) #(3 2 5) #(3 2 4) #(3 1 4) #(3 1 3) #(3 0 3) #(3 0 2) #(2 4 5) #(2 3 5) #(2 3 4)
    #(2 2 4) #(2 2 3) #(2 1 3) #(2 1 2) #(2 0 2) #(2 0 1) #(1 5 5) #(1 4 5) #(1 4 4) #(1 3 4)
    #(1 3 3) #(1 2 3) #(1 2 2) #(1 1 2) #(1 1 1) #(1 0 1) #(1 0 0) #(0 5 5) #(0 5 4) #(0 4 4)
    #(0 4 3) #(0 3 3) #(0 3 2) #(0 2 2) #(0 2 1) #(0 1 1) #(0 1 0) #(0 0 0) #(-1 5 4) #(-1 5 3)
    #(-1 4 3) #(-1 4 2) #(-1 3 2) #(-1 3 1) #(-1 2 1) #(-1 2 0) #(-1 1 0) #(-2 5 3) #(-2 4 3)
    #(-2 4 2) #(-2 4 1) #(-2 3 1) #(-2 3 0) #(-2 2 0)))

(defun able-directions (point)
  "Get direction of potential outward path in each dimension (x, y, z)"
  (if (= (mod (reduce #'+ point) 2) 0)
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
    moves))

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
    :initform (make-adjustable-vector :element-type 'list))))

(defclass cooperative ()
  ((inventory :initform (make-adjustable-vector :element-type 'symbol))))
				
(defun random-geography ()
  "Generate a new random geography according to base game rules"
  (let ((geography (make-adjustable-vector :element-type 'list)))
    geography))

(defmacro deduce-points ()
'(prog ()
  (defparameter *borders* ())
  (defparameter *points* ())
  (defun surrounding (point) (loop for dest in (able-destinations point) when (not (member dest *points* :test #'equalp)) do (pushnew dest *points*) do (if (not (member dest *borders* :test #'equalp)) (surrounding dest))))
  (let ((*borders* '(#(-2 2 0) #(-2 3 0) #(-2 3 1) #(-2 4 1) #(-2 4 2) #(-2 4 3) #(-2 5 3) #(-1 5 3) #(-1 5 4) #(0 5 4) #(0 5 5) #(1 5 5) #(1 4 5) #(2 4 5) #(2 3 5) #(3 3 5) #(3 2 5) #(3 2 4) #(3 1 4) #(3 1 3) #(3 0 3) #(3 0 2) #(2 0 2) #(2 0 1) #(1 0 1) #(1 0 0) #(0 0 0) #(0 1 0) #(-1 1 0) #(-1 2 0))) (*points* ()))
    (surrounding (vector 1 2 2))
    *points*)))

