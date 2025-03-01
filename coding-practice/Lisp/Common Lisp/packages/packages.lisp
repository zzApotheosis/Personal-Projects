;; Load another Lisp file for use
(load "my-package.lisp")

;; Redefine namespace and/or package scope
(in-package :common-lisp-user) ;; NOTE, :cl-user is an alias for :common-lisp-user

;; Use the loaded function from my-package
(format t "~a~%" (my-package::my-function 5))

;; This will only work if my-function is exported from my-package!
(format t "~a~%" (my-package:my-function 10))
