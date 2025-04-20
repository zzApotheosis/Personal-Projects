(defpackage :academic-effort-calculator
  (:use :cl)
  )

(in-package :academic-effort-calculator)

(defstruct course
  "A structure representing grades in an academic course."
  (total-grade 1.0)
  (current-relative-grade nil)
  (current-progress 0.0)
  (current-grade nil)
  (desired-grade nil)
  (required-effort nil)
  )

(defun main ()
  (let ((my-course (make-course
			:total-grade 1.0
			:current-relative-grade 0.9592
			:current-progress 0.85
			:desired-grade 0.9))
	)

    (setf (course-current-grade my-course)
	  (*
	   (course-current-relative-grade my-course)
	   (course-current-progress my-course)
	   )
	  )

    (setf (course-required-effort my-course)
	  (/
	   (- (course-desired-grade my-course) (course-current-grade my-course))
	   (- (course-total-grade my-course) (course-current-progress my-course))
	   )
	  )

    (format t "~a~%" (course-required-effort my-course))
    )
  )

(defun clamp (&key (min 0.0) (max 1.0) val)
  (unless val
    (format t "VAL NOT DEFINED?~%")
    (return-from clamp val)
    )

  (cond
    ((< val min) min)
    ((> val max) max)
    (t val)
    )
  )

;; Execute (main)
(main)
