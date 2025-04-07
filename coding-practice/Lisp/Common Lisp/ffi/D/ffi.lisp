;; NOTE: This example will only work in SBCL, so this is not portable across Common Lisp implementations

;; Apparently this used to be required, but sb-alien is a native part of SBCL so it's already built-in now
;;(require :sb-alien)

(defun c-string-to-lisp-string (c-string-pointer)
  "Converts a C string pointer to a Lisp string."
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
	(offset 0))
    (loop
     (let ((char (sb-alien:deref c-string-pointer offset)))
       (if (zerop char)
	   (return (coerce result 'string))
	   (vector-push-extend (code-char char) result)
	   )
       )
     (incf offset)
     )
    )
  )

(handler-case
    (progn
      (sb-alien:load-shared-object "./libfoo.so")

      (sb-alien:define-alien-routine ("add" add) sb-alien:int
	(a sb-alien:int)
	(b sb-alien:int)
	)

      (let ((a-nums '(5 6 2 -15 420
		      ;; 123456789999 ;; SBCL won't allow you to use numbers that aren't valid 32-bit integers :)
		      ))
	    (b-nums '(10 -7 0 -123456 69
		      ;; 122222333333 ;; SBCL won't allow you to use numbers that aren't valid 32-bit integers :)
		      ))
	    )

	(loop for i from 0 below (length a-nums) do
	      (let ((a (nth i a-nums))
		    (b (nth i b-nums))
		    )
		(format t "~a + ~a = ~a~%" a b (add a b))
		)
	      )
	)

      (sb-alien:with-alien ((name (* sb-alien:char)))
	(setf name (sb-alien:make-alien-string "Alice"))
	(sb-alien:with-alien ((output (* sb-alien:char)))
	  (setf output (sb-alien:alien-funcall (sb-alien:extern-alien "greet" (function (* sb-alien:char) (* sb-alien:char))) name))
	  (format t "~a~%" (c-string-to-lisp-string output))
	  ;;(sb-alien:free-alien output)
	  (sb-alien:alien-funcall (sb-alien:extern-alien "free_greet_result" (function sb-alien:void (* sb-alien:char))) output)
	  )
	(sb-alien:free-alien name)
	)
      )

  (error (e)
    (format t "~a~%" e)
    )
  )
