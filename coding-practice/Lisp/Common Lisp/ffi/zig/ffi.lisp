;; NOTE: This example will only work in SBCL, so this is not portable across Common Lisp implementations

;; Apparently this used to be required, but sb-alien is a native part of SBCL so it's already built-in now
;;(require :sb-alien)

;; Make sure we have a different RNG seed each time this is invoked
(setf *random-state* (make-random-state t))

(handler-case
    (progn
      (sb-alien:load-shared-object "./libfoo.so")

      (sb-alien:define-alien-routine "zig_foo" sb-alien:void
	(a sb-alien:int)
	)

      (let ((iter 10)
	    (max-num 110)
	    )
	(loop for i from 0 below iter do
	      (let ((a (random (1+ max-num)))
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
