(defun sum-of-squares (n)
  (apply #'+ (loop for i from 1 to n collect (expt i 2)))
  )

(defun square-of-sum (n)
  (expt (apply #'+ (loop for i from 1 to n collect i)) 2)
  )

(defun solve (n)
  (- (square-of-sum n) (sum-of-squares n))
  )

(defun solution ()
  (let* ((n 100)
         (expected 25164150)
         (actual (solve n)))
    (assert (= expected actual))
    actual
    )
  )
