(defun prime-list (n)
  (let ((value-list))
    (loop with test-value = 1
          with current-value = n
          do (loop while (= 0 (mod current-value test-value))
                   do (push test-value value-list)
                   (setf current-value (/ current-value test-value))
                   (when (= 1 test-value) (incf test-value))
                   )
          (incf test-value)
          while (<= test-value current-value)
          )
    (nreverse value-list)
    )
  )

(defun largest-prime-factor (n)
  (car (last (prime-list n)))
  )

(defun solution ()
  (let* ((n 600851475143)
         (expected 6857)
         (actual (largest-prime-factor n)))
    (assert (= expected actual))
    actual
    )
  )

