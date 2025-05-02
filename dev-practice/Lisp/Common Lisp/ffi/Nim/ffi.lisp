;; NOTE: This example will only work in SBCL, so this is not portable across Common Lisp implementations

;; Apparently this used to be required, but sb-alien is a native part of SBCL so it's already built-in now
;;(require :sb-alien)

(handler-case
    (progn
      (sb-alien:load-shared-object "./libfoo.so")

      (sb-alien:define-alien-routine "add" sb-alien:int
	(a sb-alien:int)
	(b sb-alien:int)
	)
      (sb-alien:define-alien-routine "greet" sb-alien:c-string
	(name sb-alien:c-string))
            
      (let ((a-nums '(5 6 2 -15 420
		      ;; 123456789999 ;; SBCL won't allow you to use numbers that aren't valid 32-bit integers :)
		      ))
	    (b-nums '(10 -7 0 -123456 69
		      ;; 122222333333 ;; SBCL won't allow you to use numbers that aren't valid 32-bit integers :)
		      ))
	    (name "Alice")
	    )

	(loop for i from 0 below (length a-nums) do
	      (let ((a (nth i a-nums))
		    (b (nth i b-nums))
		    )
		(format t "~a + ~a = ~a~%" a b (add a b))
		)
	      )
	
	(format t "~a~%" (greet name))
	)

      ;; Unload shared object (optional, but good practice)
      (sb-alien:unload-shared-object "./libfoo.so")
      )

  (error (e)
    (format t "~a~%" e)
    )
  )

