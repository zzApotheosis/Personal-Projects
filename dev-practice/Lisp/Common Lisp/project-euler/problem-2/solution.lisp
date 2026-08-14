(defun even-fibonacci (limit)
  (loop with previous = 1
        with current = 1
        while (< current limit)
        when (= 0 (mod current 2))
        collect current
        do (let ((next (+ current previous)))
             (setf previous current)
             (setf current next))))

(defun solution ()
  (let* ((n 4000000)
         (expected 4613732)
         (actual (apply #'+ (even-fibonacci n))))
    (assert (= expected actual))
    actual
    )
  )
