(defun main ()
  (format t "(cons 'a 'b) == '(a . b)? ~a~%" (if (equal (cons 'a 'b) '(a . b)) "true" "false"))
  (format t "(list 'a 'b) == '(a . (b . nil))? ~a~%" (if (equal (list 'a 'b) '(a . (b . nil))) "true" "false"))
  (format t "(list 'a 'b) == (cons 'a (cons 'b nil))? ~a~%" (if (equal (list 'a 'b) (cons 'a (cons 'b nil))) "true" "false"))
  )

(main)
