(defun factorial (n)
 (if (<= n 1)
  1
  (* n (factorial (- n 1)))
 )
 )

(dolist (n '(0 1 5 69))
 (format t "~a! = ~a~%" n (factorial n))
 )
