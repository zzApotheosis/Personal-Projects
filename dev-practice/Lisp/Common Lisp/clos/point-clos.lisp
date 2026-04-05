;;;; Point Class Example in Common Lisp (CLOS)
;;;; Demonstrates object-oriented programming concepts in Common Lisp

(defpackage :point-example
  (:use :common-lisp)
  (:export :point
           :make-point
           :point-x
           :point-y
           :distance-to
           :distance-from-origin
           :translate
           :scale
           :point-equal
           :point-to-string
           :midpoint))

(in-package :point-example)

;;; ============================================
;;; CLASS DEFINITION
;;; ============================================

(defclass point ()
  ((x :initarg :x
      :initform 0
      :accessor point-x
      :documentation "The x-coordinate of the point")
   (y :initarg :y
      :initform 0
      :accessor point-y
      :documentation "The y-coordinate of the point"))
  (:documentation "A class representing a point in 2D Cartesian coordinates"))

;;; ============================================
;;; CONSTRUCTOR FUNCTION
;;; ============================================

(defun make-point (x y)
  "Creates a new point with the given x and y coordinates."
  (make-instance 'point :x x :y y))

;;; ============================================
;;; PRINT METHOD (for nice output)
;;; ============================================

(defmethod print-object ((p point) stream)
  "Custom print method for point objects."
  (print-unreadable-object (p stream :type t)
    (format stream "(~a, ~a)" (point-x p) (point-y p))))

;;; ============================================
;;; DISTANCE METHODS
;;; ============================================

(defmethod distance-to ((p1 point) (p2 point))
  "Calculates the Euclidean distance between two points."
  (let ((dx (- (point-x p2) (point-x p1)))
        (dy (- (point-y p2) (point-y p1))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defmethod distance-from-origin ((p point))
  "Calculates the distance from the point to the origin (0, 0)."
  (sqrt (+ (* (point-x p) (point-x p))
           (* (point-y p) (point-y p)))))

;;; ============================================
;;; TRANSFORMATION METHODS
;;; ============================================

(defmethod translate ((p point) dx dy)
  "Returns a new point translated by dx and dy."
  (make-point (+ (point-x p) dx)
              (+ (point-y p) dy)))

(defmethod scale ((p point) factor)
  "Returns a new point scaled by the given factor from origin."
  (make-point (* (point-x p) factor)
              (* (point-y p) factor)))

;;; ============================================
;;; COMPARISON METHOD
;;; ============================================

(defmethod point-equal ((p1 point) (p2 point) &optional (tolerance 0.0001))
  "Checks if two points are equal within a tolerance."
  (and (< (abs (- (point-x p1) (point-x p2))) tolerance)
       (< (abs (- (point-y p1) (point-y p2))) tolerance)))

;;; ============================================
;;; UTILITY METHODS
;;; ============================================

(defmethod point-to-string ((p point))
  "Returns a string representation of the point."
  (format nil "(~a, ~a)" (point-x p) (point-y p)))

(defmethod midpoint ((p1 point) (p2 point))
  "Returns the midpoint between two points."
  (make-point (/ (+ (point-x p1) (point-x p2)) 2)
              (/ (+ (point-y p1) (point-y p2)) 2)))

;;; ============================================
;;; DEMONSTRATION
;;; ============================================

(defun demonstrate-point-class ()
  "Demonstrates the Point class functionality."
  (format t "~%=== Common Lisp CLOS Point Class Demo ===~%~%")
  
  ;; Creating points
  (format t "Creating points:~%")
  (let ((p1 (make-point 3 4))
        (p2 (make-point 0 0))
        (p3 (make-point 6 8)))
    (format t "  p1 = ~a~%" p1)
    (format t "  p2 = ~a~%" p2)
    (format t "  p3 = ~a~%~%" p3)
    
    ;; Accessing slots
    (format t "Accessing coordinates:~%")
    (format t "  p1 x-coordinate: ~a~%" (point-x p1))
    (format t "  p1 y-coordinate: ~a~%~%" (point-y p1))
    
    ;; Distance calculations
    (format t "Distance calculations:~%")
    (format t "  Distance from p1 to p2: ~,3f~%" (distance-to p1 p2))
    (format t "  Distance from p1 to origin: ~,3f~%" (distance-from-origin p1))
    (format t "  Distance from p1 to p3: ~,3f~%~%" (distance-to p1 p3))
    
    ;; Transformations
    (format t "Transformations:~%")
    (let ((p4 (translate p1 2 3))
          (p5 (scale p1 2)))
      (format t "  p1 translated by (2, 3): ~a~%" p4)
      (format t "  p1 scaled by factor 2: ~a~%~%" p5))
    
    ;; Midpoint
    (format t "Midpoint:~%")
    (format t "  Midpoint between p1 and p3: ~a~%~%" (midpoint p1 p3))
    
    ;; Comparison
    (format t "Comparison:~%")
    (let ((p4 (make-point 3 4)))
      (format t "  p1 = ~a, p4 = ~a~%" p1 p4)
      (format t "  p1 equal to p4? ~a~%" (point-equal p1 p4))
      (format t "  p1 equal to p2? ~a~%" (point-equal p1 p2))))
  
  (format t "~%=== Demo Complete ===~%"))

;; Run the demonstration
(demonstrate-point-class)
