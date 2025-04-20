(defpackage :my-package
  (:use :cl)
  )

(in-package :my-package)

;; Main function
(defun main ()
  ;; Creating an instance of the Point class
  '(let ((p-1 (make-instance 'point :x 1 :y 2 :z 3 :name "Foo"))
	 (origin (make-instance 'point :x 0 :y 0 :z 0 :name "Origin"))
	 (p-2 (make-instance 'point :x 7 :y -3 :z 14 :name "Bar"))
	 )
    (p-print p-1)
    (p-print origin)
    (p-print p-2)
    (let ((points (list origin p-2)))
      (dolist (point-i points)
	(format t "Distance between ~a and ~a: ~a~%" (object-name p-1) (object-name point-i) (distance p-1 point-i))
	)
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
  ((x :initarg :x :accessor point-x :initform 0 :documentation "The X coordinate value.")
   (y :initarg :y :accessor point-y :initform 0 :documentation "The Y coordinate value.")
   (z :initarg :z :accessor point-z :initform 0 :documentation "The Z coordinate value.")
   )
  )

;; "Methods"
(defgeneric distance (a b)
  (:documentation "Calculate the distance between two objects.")
  )
(defgeneric p-print (p)
  (:documentation "Print a given object.")
  )

(defmethod distance ((a point)
		     (b point))
  (let ((delta-x (- (point-x a) (point-x b)))
	(delta-y (- (point-y a) (point-y b)))
	(delta-z (- (point-z a) (point-z b)))
	)
    (sqrt (+ (expt delta-x 2) (expt delta-y 2) (expt delta-z 2)))
    )
  )

(defmethod p-print ((p point))
  (format t "Point \"~a\" coordinates: X=~A, Y=~A, Z=~A~%"
	  (object-name p)
	  (point-x p)
	  (point-y p)
	  (point-z p)
	  )
  )

;; Execute main
(format t "~a~%" (main))
(eval (main))

;; Note that this example sets up the (main) function in a special way.
;; (main) returns a literal list containing code. I do it this way because
;; I want to show the code to the user before evaluating it. And all it took
;; was a single quote!
