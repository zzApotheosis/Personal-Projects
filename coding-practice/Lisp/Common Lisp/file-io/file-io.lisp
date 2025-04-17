;; Get a new pseudo-random state on each invocation
(setf *random-state* (make-random-state t))

(defun greet (name)
  "A simple greet function."

  (let ((rand (random 1.0)))
    (let ((salutation (if (> rand 0.2) "Nice to see you!" "Not gonna lie, you kinda sus.")))
      (format nil "Hello, ~a! ~a~%" name salutation)
      )
    )
  )

(let ((file-name "example.txt")
      (names '("Alice" "Bob" "Eve")))

  ;; Write the file
  (with-open-file (stream file-name :direction :output :if-exists :supersede)
    (write-line "Hello, Common Lisp!" stream)
    (dolist (name names)
      (format stream (greet name))
      )
    )

  ;; Read the file
  (with-open-file (stream file-name :direction :input)
    (loop while (listen stream)
	  do (let ((line (read-line stream)))
	       (format t "~a~%" line)
	       )
	  )
    )
  )
    

