;;; This is an example that Gemini produced
;;; I just get to use it as a great OOP example for Common Lisp
;;; It seems to be much easier to ask AI for instant feedback to
;;; my questions :)

(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y)
   (z :initarg :z :accessor point-z))
  (:documentation "Represents a point in 3D Cartesian space."))

;; Example usage:

;; Create a new point object.
(defvar my-point (make-instance 'point :x 10 :y 20 :z 30))

;; Access the x, y, and z values.
(format t "X: ~a~%" (point-x my-point))  ; Output: X: 10
(format t "Y: ~a~%" (point-y my-point))  ; Output: Y: 20
(format t "Z: ~a~%" (point-z my-point))  ; Output: Z: 30

;; Change the x value.
(setf (point-x my-point) 15)
(format t "New X: ~a~%" (point-x my-point)) ; Output: New X: 15

;; Another way to create a point (using keyword arguments):
(defvar another-point (make-instance 'point :y 5 :z 7 :x 2)) ; Order doesn't matter

(format t "Another point X: ~a~%" (point-x another-point)) ; Output: Another point X: 2
(format t "Another point Y: ~a~%" (point-y another-point)) ; Output: Another point Y: 5
(format t "Another point Z: ~a~%" (point-z another-point)) ; Output: Another point Z: 7


;; Example of a function that uses the point class:
(defun point-distance (p1 p2)
  (let ((dx (- (point-x p1) (point-x p2)))
        (dy (- (point-y p1) (point-y p2)))
        (dz (- (point-z p1) (point-z p2))))
    (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(defvar p1 (make-instance 'point :x 0 :y 0 :z 0))
(defvar p2 (make-instance 'point :x 3 :y 4 :z 0))

(format t "Distance between p1 and p2: ~a~%" (point-distance p1 p2)) ; Output: Distance between p1 and p2: 5.0
