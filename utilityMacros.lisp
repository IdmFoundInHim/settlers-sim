(defmacro with-gensyms (symbols &body body)
  `(let ,(loop for sym in symbols collect `(,sym (gensym)))
     ,@body))

(defmacro +elementwise (seq1 seq2)
  (with-gensyms (a b)
    `(loop for ,a in ,seq1 for ,b in ,seq2 collecting (+ ,a ,b))))

(defmacro make-adjustable-vector (&rest extra-args)
  `(make-array 1 :fill-pointer 0 :adjustable t ,@extra-args))