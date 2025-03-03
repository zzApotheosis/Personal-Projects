;; This is an extremely simple example showing Lisp's homoiconicity, i.e. data is code and code is data

(defun main ()
  (let ((*standard-output* *error-output*))
    (format t "This is code!~%")
    )
  
  (let ((data '(format t "This is data!~%"))
	)
    (eval data)
    )
  )

(main)
