;; defstruct is a macro!
(defstruct person
  (name "Unknown" :type string)
  (age 0 :type integer)
  )

(let ((a (make-person :name "Alice" :age 30))
      (b (make-person))
      )
  (format t "Name: ~a~%" (person-name a)) ; person-name is automatically created by defstruct
  (format t "Age: ~a~%" (person-age a)) ; person-age is automatically created by defstruct

  ;; It's Alice's birthday!
  (format t "Happy birthday, ~a!~%" (person-name a))
  (incf (person-age a))

  (format t "Name: ~a~%" (person-name a))
  (format t "Age: ~a~%" (person-age a))

  (format t "~%")

  (format t "Name: ~a~%" (person-name b)) ; Mysterious guy
  (format t "Age: ~a~%" (person-age b)) ; Mysterious guy... is young!
  )

;; Note: (defstruct) is similar to (defclass). Be sure to read and study how they differ!
