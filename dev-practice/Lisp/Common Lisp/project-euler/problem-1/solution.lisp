(defun solution ()
  (assert (=
           (let ((limit 1000))
             (loop for i below limit
                   when (or (= (mod i 3) 0)
                            (= (mod i 5) 0)
                            )
                   sum i)
             )
           
           233168
           )
          )
  )

