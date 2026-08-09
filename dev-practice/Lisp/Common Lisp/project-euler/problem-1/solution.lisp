(defun solution ()
  (let ((limit 1000)
        (value 0))
    (dotimes (i limit)
      (when (or (= (mod i 3) 0) (= (mod i 5) 0))
        (setf value (+ value i))
        )
      )

    (assert (= value 233168))
    )
  )

