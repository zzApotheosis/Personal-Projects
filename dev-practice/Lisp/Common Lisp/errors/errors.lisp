(defpackage :my-errors-package
  (:use :cl)
  )

(in-package :my-errors-package)

(let ((lower-limit -5)
      (upper-limit 100)
      )
  
  (defun doubler (n)
    (when (>= n upper-limit)
      (error (format nil "Number (~a) is too big!" n))
      )
    (when (<= n lower-limit)
      (error (format nil "Number (~a) is too small!" n))
      )
    (* n 2)
    )

  (defun safe-doubler (n)
    (handler-case
	(my-errors-package::doubler n)
      (simple-error (e)
	(format t "~a~%" e)
	)
      (error (e)
	(format t "Caught generic error: ~a~%" e)
	)
      )
    )
  )

(let ((nums '(-10 5 99 100)))
  (dolist (num nums)
    (format t "(doubler ~a) = ~a~%" num (my-errors-package::safe-doubler num))
    )
  )

