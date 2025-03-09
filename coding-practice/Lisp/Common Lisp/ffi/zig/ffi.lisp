;; NOTE: This example will only work in SBCL, so this is not portable across Common Lisp implementations

;; Apparently this used to be required, but sb-alien is a native part of SBCL so it's already built-in now
;;(require :sb-alien)

(handler-case
    (progn
      (sb-alien:load-shared-object "./libfoo.so")

      (sb-alien:define-alien-routine "zig_foo" sb-alien:void
	(a sb-alien:int)
	)

      (let ((a-nums '(5 6 2 -15 420
		      ;; 123456789999 ;; SBCL won't allow you to use numbers that aren't valid 32-bit integers :)
		      ))
	    )

	(loop for i from 0 below (length a-nums) do
	      (let ((a (nth i a-nums))
		    )
		(format t "zig_foo(~a)~%" a)
		(zig-foo a)
		)
	      )
	)
      )

  (error (e)
    (format t "~a~%" e)
    )
  )
