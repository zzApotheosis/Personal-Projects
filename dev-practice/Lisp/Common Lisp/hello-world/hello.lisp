(format t "Hello world! Welcome to ANSI Common Lisp!~%")
(write-line "Hello world! This goes to stdout." *standard-output*)
(write-line "Hello world! This goes to stderr." *error-output*)
(format t "~a ~a~%" "Hello world!" "This is a formatted string.")

(let ((n (lambda () "Hello world! This is a value returned by a lambda function."))
        )
  (format t "~a~%" (funcall n))
  )
