(defun digit-list (n base)
  (loop while (> n 0)
        collect (mod n base)
        do
        (setf n (truncate n base))
        )
  )


(defun is-palindrome (n)
  (let ((n-split (digit-list n 10)))
    (equal n-split (reverse n-split))
    )
  )

(defun digit-length (n base)
  (loop with current-value = n
        with count = 1
        while (> current-value 0)
        do
        (setf current-value (truncate current-value base))
        sum count)
  )

(defun largest-n-digit-palindrome (n)
  (let ((matches)
        (max 0))
    (loop with x = 1
          while (<= (digit-length x 10) n)
          do
          (loop with y = 1
                while (<= (digit-length y 10) n)
                do
                (let ((n (* x y)))
                  (when (is-palindrome n)
                    (push n matches)
                    )
                  )
                (incf y)
                )
          (incf x)
          )
    
    (dolist (value matches)
      (when (> value max)
        (setf max value)
        )
      )
    max
    )
  )

(defun solution ()
  (let* ((n 3)
         (expected 906609)
         (actual (largest-n-digit-palindrome n)))
    (assert (= expected actual))
    actual
    )
  )
