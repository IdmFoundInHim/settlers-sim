(defmacro point-from-abbreviated-literal (abbreviated)
  (multiple-value-bind (x yz) (truncate abbreviated 100) ; truncate performs Euclidean division
    `(vector ,x ,@(multiple-value-list (truncate (abs yz) 10)))))

(defgeneric point-from-abbreviated (abbreviated)
  (:documentation "Converts shorthand for board coordinates to usable vectors"))

(defmethod point-from-abbreviated ((abbreviated integer))
  (multiple-value-bind (x yz) (truncate abbreviated 100)
    (multiple-value-bind (y z) (truncate (abs yz) 10)
	(vector x y z))))

(defmethod point-from-abbreviated ((abbreviated string)) ; inconsistent error handling
  (flet ((parse-single-digit-integers (number) (map 'vector #'digit-char-p number)))
	(case (length abbreviated)
	  (3 (parse-single-digit-integers abbreviated)) ; Returns NIL values inside vector on bad input
	  (4 (if (char= #\- (char abbreviated 0))
		 (map 'vector  #'* #(-1 1 1) (parse-single-digit-integers (subseq abbreviated 1))) ; throws error about types on bad input
		 (vector nil nil nil))) ; to match above case
	  (5 (vector nil nil nil))))) ; not yet implemented	 

(defmacro points-from-abbreviated-literals (&rest abbreviated)
  `(vector ,@(mapcar (lambda (point) (point-from-abbreviated point)) abbreviated)))

(defconstant +origin-hex-points+ (points-from-abbreviated-literals 000 100 110 111 011 001))

(defun point-adder (point)
  (lambda (other-point) (map 'vector #'+ point other-point)))

(defun get-points-on-hex (bottom-right-point)
  (map 'vector (point-adder bottom-right-point) +origin-hex-points+))

(defconstant +hex-corner-points+ (points-from-abbreviated-literals 000 -101 011 110 -202 -112 022 121 220 -213 -123 033 132 231 -224 -134 044 143 242))

(defconstant +hex-point-vectors+ (map 'vector #'get-points-on-hex +hex-corner-points+))
