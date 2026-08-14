;;; This is the classic recursive factorial implementation
(defun recursive-factorial (n)
  "Calculate n!"
  (if (> n 1)
      (* n (recursive-factorial (1- n)))
      1
      )
  )

;;; This is a Lisp-inspired equivalent
(defun factorial (n)
  (apply #'* (loop for i from 1 to n collect i))
  )
