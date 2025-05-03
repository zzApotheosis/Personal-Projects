(defmacro with-cd (new-dir &rest body)
  "Change directory and execute a body of code. Change back to the original directory when finisehd."
  `(let ((old-dir default-directory))
     (cd ,new-dir)
     ,@body
     (cd old-dir)
     )
  )
