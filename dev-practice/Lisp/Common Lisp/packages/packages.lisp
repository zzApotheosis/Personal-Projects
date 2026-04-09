;; Load another Lisp file for use
(load (merge-pathnames "my-package.lisp" "my-package/"))

;; Redefine namespace and/or package scope
;;(in-package #:common-lisp-user) ;; NOTE, :cl-user is an alias for :common-lisp-user

;; Use the loaded function from my-package
(let ((n))
  (setq n 5)
  (format t "(my-package::my-function ~a) = ~a~%" n (my-package::my-function n))
  (setq n 10)
  ;; This will only work if my-function is exported from my-package!
  (format t "(my-package:my-function ~a) = ~a~%" n (my-package:my-function n))

  (my-package:hello "Steven")
  )

