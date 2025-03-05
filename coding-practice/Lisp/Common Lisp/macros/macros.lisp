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

(format t "~%MACRO EXPANSION:~%~a~%"
	(macroexpand-1 '(my-debug (format t "one frick~%") (format t "two frick~%")))
	)
