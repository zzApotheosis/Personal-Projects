(defun sum-values (limit)
  (loop for i below limit
        when (or (= (mod i 3) 0)
                 (= (mod i 5) 0)
                 )
        sum i)
  )

(defun solution ()
  (let* ((n 1000)
         (expected 233168)
         (actual (sum-values n)))
    (assert (= expected actual))
    actual
    )
  )
