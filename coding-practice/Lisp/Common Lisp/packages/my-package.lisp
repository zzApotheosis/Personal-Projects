;; Define a new package called "MY-PACKAGE"
(defpackage :my-package
  (:use :cl)
  (:export :my-function)
  )

;; Switch to the "MY-PACKAGE" package
(in-package :my-package)

;; Define a function in "MY-PACKAGE"
(defun my-function (x)
  (* x 2))

;; Now this function is accessible as MY-PACKAGE:MY-FUNCTION from other packages
