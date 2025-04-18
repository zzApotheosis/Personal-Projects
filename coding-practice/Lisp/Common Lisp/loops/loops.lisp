;; The ANSI Common Lisp standard does a good job documenting how the (loop) iteration construct works
;; The LispWorks HyperSpec documentation is a good way to view the (loop) documentation without actually purchasing the ANSI Common Lisp Standard.
;; https://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm

(defun factorial (n)
  "A simple factorial function."
  (let ((result))
    (unless (< n 0)
	(if (> n 1)
	    (setf result (* n (factorial (1- n))))
	    (setf result 1)
	    )
	)

    ;; Will be NIL if the input is negative
    result
    )
  )

(let ((names '("Alice" "Bob" "Charlie" "Dustin" "Eve" "Fern" "Gail" "Hannah"))
      (nums '(0 1 2 3 4 5 6 7 8 9 10 15 20 25 35 41 43 45 47 1337 69 420 -101))
      )
  
  (loop named for-as-in-list-clause for name in names
	do (format t "Name: ~a~%" name)
	)

  (loop named arithmetic-clause for i from 0 to (1- 5)
   do (format t "Hi: ~a~%" i)
	)

  (loop named with-clause-and-while-clause with i = 0 while (< i (1- (length names)))
	do (format t "names[~a] = ~a~%" i (nth i names))
	do (incf i)
	)

  (let ((even-nums)
	(odd-nums)
	)
    (setf even-nums (loop for num in nums when (= (mod num 2) 0) collect num))
    (setf odd-nums  (loop for num in nums when (= (mod num 2) 1) collect num))
    (format t "Odd numbers: ~a~%" odd-nums)
    (format t "Even numbers: ~a~%" even-nums)
    )

  ;; Super simple list iteration
  (dolist (num nums)
    (format t "~a! = ~a~%" num (factorial num))
    )
  )
