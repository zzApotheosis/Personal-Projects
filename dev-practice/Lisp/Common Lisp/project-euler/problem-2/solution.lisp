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
  (assert (= (apply #'+ (even-fibonacci 4000000))
             4613732
             )
          )
  )
