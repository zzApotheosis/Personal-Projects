(defparameter *my-param* "Param string")

(defvar *my-var* "Var string")

(defun print-me (m)
  "Print stuff"
  (format t "Printing: ~a~%" m)
  )

(print-me *my-param*)
(print-me *my-var*)

(defparameter *my-param* "Is this the Krusty Krab?")
(defvar *my-var* "No, this is Patrick.")

(print-me *my-param*)
(print-me *my-var*)

;; These are lexical bindings. Basically temporary variables that are bound for the lifetime of the (let) statement
(let ((data '("What" "if" "we" "made" "a" "ridiculous" "data" "structure?")))
  (dolist (d data)
    (progn
      (print-me d)
      (setf d (format nil "~a~a" d "?"))
      (print-me d)
      )
    )
  )

