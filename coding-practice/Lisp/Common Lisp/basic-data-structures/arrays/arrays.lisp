;; Single-dimensional array (AKA vector)
(let ((my-array (make-array 3
			    :initial-contents '(11 22 33))
	)
      )

  ;; Iteration by indexing (standard C-style "for" loop)
  (format t "Iteration by indexing:~%")
  (loop for i from 0 below (array-dimension my-array 0) do
	(format t "my-array[~d] = ~a~%" i (aref my-array i))
	)

  ;; Iteration without indexing (known as "foreach" in other languages)
  (format t "Iteration without indexing:~%")
  (loop for i across my-array do
	(format t "~a~%" i)
	)

  ;; Iteration without indexing... but also tracks the index anyway
  (format t "Iteration without indexing, but incrementing a value with each iteration:~%")
  (loop for item across my-array
	for index from 0 do
	(format t "my-array[~d] = ~a~%" index item)
	)
  )

;; Multi-dimensional arrays
(let ((my-array (make-array '(2 3)
			    :initial-contents '((1 2 3)
						(4 5 6))
			    )
	)
      )

  ;; Multi-dimensional iteration
  (format t "Multi-dimensional iteration by indexing:~%")
  (loop for i from 0 below (array-dimension my-array 0) do
	(loop for j from 0 below (array-dimension my-array 1) do
	      (format t "(~d, ~d) = ~d~%" i j (aref my-array i j))
	      )
	)
  )
