;;; This is an example that Grok produced
;;; I just get to use it as a great OOP example for Common Lisp
;;; It seems to be much easier to ask AI for instant feedback to
;;; my questions :)


(defclass point ()
  ((x :initarg :x :accessor x :initform 0)
   (y :initarg :y :accessor y :initform 0)
   (z :initarg :z :accessor z :initform 0)))

;; Creating an instance of the Point class
(let ((p (make-instance 'point :x 1 :y 2 :z 3)))
  (format t "Point coordinates: X=~A, Y=~A, Z=~A~%" (x p) (y p) (z p)))
