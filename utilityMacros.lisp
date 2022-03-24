(defmacro with-gensyms (symbols &body body)
  `(let ,(loop for sym in symbols collect `(,sym (gensym)))
     ,@body))

(defmacro +vector (vec1 vec2)
  (with-gensyms (a b)
    `(loop for ,a across ,vec1 for ,b across ,vec2 collecting (+ ,a ,b))))

(defmacro make-adjustable-vector (&rest extra-args)
  `(make-array 1 :fill-pointer 0 :adjustable t ,@extra-args))