(defun paths-at (point)
  "(INTEGER-VECTOR3) -> INTEGER-VECTOR3-VECTOR")

(defun hexes-at (point)
  "(INTEGER-VECTOR3) -> HASHTAB-VECTOR")

(defun ports-at (point)
  "(INTEGER-VECTOR3) -> HASHTAB-VECTOR")

(defun hexes-with (resource)
  "(SYMBOL) -> HASHTAB-VECTOR")

(defun ports-with (resource)
  "(SYMBOL) -> HASHTAB-VECTOR")

(defun paths-between (point-start point-end)
  "(INTEGER-VECTOR3 INTEGER-VECTOR3) -> INTEGER-VECTOR3-VECTOR
Shortest sequence of movements between two points.
Convenience function.")