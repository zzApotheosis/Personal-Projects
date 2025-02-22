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
