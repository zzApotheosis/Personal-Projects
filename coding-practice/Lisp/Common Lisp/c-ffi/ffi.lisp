;; NOTE: This example will only work in SBCL, so this is not portable across Common Lisp implementations

;; Apparently this used to be required, but sb-alien is a native part of SBCL so it's already built-in now
;;(require :sb-alien)

(sb-alien:load-shared-object "./libfoo.so")

(sb-alien:define-alien-routine "my_add" sb-alien:int
  (a sb-alien:int)
  (b sb-alien:int)
  )
(sb-alien:define-alien-routine "set_data" sb-alien:void
  (a sb-alien:int)
  )
(sb-alien:define-alien-routine "get_data" sb-alien:int)

(let ((a-nums '(5 6 2 -15 420
		;; 123456789999 ;; SBCL won't allow you to use numbers that aren't valid 32-bit integers :)
		))
      (b-nums '(10 -7 0 -123456 69
		;; 122222333333 ;; SBCL won't allow you to use numbers that aren't valid 32-bit integers :)
		))
      )

  (loop for i from 0 below (length a-nums) do
	(set-data (nth i a-nums))
	(let ((a (get-data))
	      (b (nth i b-nums))
	      )
	  (format t "~a + ~a = ~a~%" a b (my-add a b))
	  )
	)
  )

(let ((test-nums (list 10 20 30 40 50 69))
      )
  (loop for item in test-nums do
	(format t "Before setting data to ~a, static data = ~a~%" item (get-data))
	(set-data item)
	(format t "After setting data, static data = ~a~%" (get-data))
	)
  )
