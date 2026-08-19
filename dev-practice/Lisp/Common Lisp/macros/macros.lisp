(defmacro my-when (condition &rest body)
 `(if ,condition
   (progn ,@body)
   ))

(my-when (> 5 3)
 (format t "5 is greater than 3!~%")
 )

(my-when (> 2 4)
 (format t "2 is greater than... 4? Nah, that's not right~%")
 )

(defmacro my-debug (&body expression)
  `(when (>= *debug-level* 3)
     ,@expression
     )
  )

(defconstant *debug-level* 3)

(my-debug
 (format t "DEBUG STATEMENT!~%")
 (format t "SECOND STATEMENT!~%")
 )

(my-debug) ; Literally does nothing, but it's a valid usage of the macro!

(format t "standalone statement!~%")

(format t "~%MACRO EXPANSION:~%~a~%~%"
	(macroexpand-1 '(my-debug (format t "one frick~%") (format t "two frick~%")))
	)

;; A simple macro to perform code in another working directory temporarily
(require 'sb-posix)
(defmacro with-chdir (new-dir &rest body)
  `(let ((old-dir (sb-posix:getcwd)))
     (sb-posix:chdir ,new-dir)
     ,@body
     (sb-posix:chdir old-dir)
     )
  )

(format t "with-chdir example:~%")
(format t "cwd = ~a~%" (sb-posix:getcwd))
(with-chdir "/tmp"
  (format t "cwd = ~a~%" (sb-posix:getcwd))
  (format t "Nice!~%")
  )
(format t "cwd = ~a~%" (sb-posix:getcwd))
(format t "~%")

;; A simple assertion macro
;; Also, I just figured out that (defmacro) is a special operator, which is neither a macro nor a function. Wild!
(defmacro expect (test-expr err-expr)
  `(if ,test-expr
       t
       (error "Expectation failed: ~a~%~s~%" ,err-expr ',test-expr)
       )
  )

;; This is a true statement, so the err-expr never runs
(handler-case
    (expect (= 69 69) "This should never show!")
  (simple-error (e)
    (format t "Caught: ~a~%" e)
    )
  (error (e)
    (format t "Error: ~a~%" e)
    )
  )

;; The expression in this macro evaluates to nil, so the err-expr runs
(handler-case
    (expect (equal (let ((nums '(1 2 3 4 5)))
		     (dolist (num nums)
		       (if (= num 3)
			   (return num))
		       )
		     )
		   "ligma")
	    "NOT GOOD")
  (simple-error (e)
    (format t "Caught: ~a~%" e)
    )
  (error (e)
    (format t "Error: ~a~%" e)
    )
  )

#|
These are macros that other languages provide, but
Common Lisp leaves the implementation to the programmer

For example, this would be the equivalent in C:

unsigned int i = 0;
i += 1;

And this macro in Common Lisp:
(let ((i 0))
(+= i 1))

The Common Lisp macro, which didn't exist before defining it,
expands to this code:
(let ((i 0))
(setf i (+ i 1)))

But the powerful part of the Common Lisp macro is that
it is not limited to a single operand. We can pass as
many arguments to the += macro as we want, and they can
even be function calls themselves.

(let ((i 0))
(+= i 1 2 3 (expt 2 8))
(assert (= 262 i)))
|#

(defmacro += (var &rest args)
  (if (= 0 (length args))
      var
      `(setf ,var (+ ,var ,@args))
      )
  )

(defmacro -= (var &rest args)
  (if (= 0 (length args))
      var
      `(setf ,var (- ,var ,@args))
      )
  )

