;; Define a new package called "MY-PACKAGE"
(defpackage #:my-package
  (:use #:cl)
  (:export #:my-function #:hello)
  )

;; Switch to the "MY-PACKAGE" package
(in-package #:my-package)

;; Define a function in "MY-PACKAGE"
(defun my-function (x)
  (* x 2))

(defun hello (name)
  (format t "Hello ~a!~%" name)
  )

;; Now this function is accessible as MY-PACKAGE:MY-FUNCTION from other packages
