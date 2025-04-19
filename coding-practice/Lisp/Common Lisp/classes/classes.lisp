(defpackage :my-package
  (:use :cl)
  )

(in-package :my-package)

;; Main function
(defun main ()
  ;; Creating an instance of the Point class
  '(let ((p (make-instance 'point :x 1 :y 2 :z 3 :name "Foo"))
	)
    (format t "Point \"~a\" coordinates: X=~A, Y=~A, Z=~A~%"
	    (object-name p)
	    (x p)
	    (y p)
	    (z p)
	    )
    )
  )

;; Base class (Superclass of Point)
(defclass object ()
  ((name :initarg :name :accessor object-name)
   )
  )

;; Point Class
(defclass point (object)
  ((x :initarg :x :accessor x :initform 0 :documentation "The X coordinate value.")
   (y :initarg :y :accessor y :initform 0 :documentation "The Y coordinate value.")
   (z :initarg :z :accessor z :initform 0 :documentation "The Z coordinate value.")
   )
  )

;; Execute main
(format t "~a~%" (main))
(eval (main))

;; Note that this example sets up the (main) function in a special way.
;; (main) returns a literal list containing code. I do it this way because
;; I want to show the code to the user before evaluating it. And all it took
;; was a single quote!
