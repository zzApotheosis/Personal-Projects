;; Using defvar
(defvar *my-var* 10)    ; Declares *my-var* as a special variable and initializes it to 10
(setf *my-var* 20)      ; Reassigns *my-var* to 20
(defvar *my-var* 30)    ; Re-declaring with defvar does NOT change the value
(format t "After defvar: ~a~%" *my-var*)  ; Prints 20 (not 30)

;; Using defparameter
(defparameter *my-param* 100)  ; Declares *my-param* as a special variable and sets it to 100
(setf *my-param* 200)          ; Reassigns *my-param* to 200
(defparameter *my-param* 300)  ; Re-declaring with defparameter DOES change the value
(format t "After defparameter: ~a~%" *my-param*)  ; Prints 300

;; Using defconstant
(defconstant +my-const+ 1000)  ; Declares +my-const+ as a constant with value 1000
;; (setf +my-const+ 2000)      ; Uncommenting this would signal an error!
(format t "After defconstant: ~a~%" +my-const+)  ; Prints 1000

;; Demonstrate dynamic binding with let
(let ((*my-var* 40)             ; Temporarily binds *my-var* to 40
      (*my-param* 400))         ; Temporarily binds *my-param* to 400
  
  (format t "In let - *my-var*: ~a, *my-param*: ~a~%" *my-var* *my-param*)
  
  ;; +my-const+ can't be rebound with let, but its value is still accessible
  (format t "In let - +my-const+: ~a~%" +my-const+)
  )

;; Original values after let
(format t "After let - *my-var*: ~a, *my-param*: ~a~%" *my-var* *my-param*)
