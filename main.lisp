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

