(defun is-prime (n)
  (block nil
    (loop named prime-loop
          for i from 2 to (sqrt n)
          do
          (when (= (mod n i) 0)
            (return nil)
            )
          )
    (return t)
    )
  )

(defun nth-prime (n)
  (loop with count = 0
        with value = 1
        do

        (incf value)

        (when (is-prime value)
          (incf count)
          )
        
        (when (= n count)
          (return value)
          )
        )
  )

(defun solve (n)
  (nth-prime n)
  )

(defun solution ()
  (let* ((n 10001)
         (expected 104743)
         (actual (solve n)))
    (assert (= expected actual))
    actual
    )
  )
