;; NOTE: This example will only work in SBCL, so this is not portable across Common Lisp implementations

;; Apparently this used to be required, but sb-alien is a native part of SBCL so it's already built-in now
;;(require :sb-alien)

(handler-case
    (progn
      (let ((libfoo "./libfoo.so"))
	(sb-alien:load-shared-object libfoo)

	(sb-alien:define-alien-routine ("cpp_ffi" my-cpp-ffi) sb-alien:void
	  )

	(my-cpp-ffi)

	(sb-alien:unload-shared-object libfoo)
	)
      )

  (error (e)
    (format t "~a~%" e)
    )
  )
