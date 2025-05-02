;; There are two ways to build a vector in Common Lisp

(let ((my-vector #(1 2 3 4 5)) ; Literal notation
      (my-other-vector (vector 6 7 8 9 10)) ; Function notation
      )

  ;; Iteration by indexing
  (format t "Iteration by indexing:~%")
  (loop for i from 0 below (array-dimension my-vector 0) do
	(format t "my-vector[~d] = ~d~%" i (aref my-vector i))
	)
  (loop for i from 0 below (array-dimension my-other-vector 0) do
  	(format t "my-other-vector[~d] = ~d~%" i (aref my-other-vector i))
	)

  ;; Iteration over elements
  (format t "Iteration over elements:~%")
  (loop for item across my-vector do
	(format t "~d~%" item)
	)
  (loop for item across my-other-vector do
	(format t "~d~%" item)
	)

  ;; Iteration by indexing, but also keeps track of index
  (format t "Iteration by element and indexing:~%")
  (loop for item across my-vector
	for i from 0 do
	(format t "my-vector[~d] = ~d~%" i item)
	)
  (loop for item across my-other-vector
	for i from 0 do
	(format t "my-other-vector[~d] = ~d~%" i item)
	)
  )
