(format t "Dumb list                      : ~s~%" (list 1 2 3))
(format t "Dumb list expressed differently: ~s~%" '(1 2 3)) ; This one's expressed as a literal list (single quote notation)
(format t "(list 1 2 3) == '(1 2 3) ? ~a~%" (if (equal
						 (list 1 2 3)
						 '(1 2 3))
						"true"
						"false"
						)
	)

(defvar *my-list-literal* '(list '(1 . "one") '(2 . "two") '(3 . "three")))
(format t "List literal          : ~s~%" *my-list-literal*)
(format t "List literal evaluated: ~s~%" (eval *my-list-literal*))
