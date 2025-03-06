(let ((my-table
       ;; (make-hash-table) ; This won't work in this implementation! The :test 'equal is needed because we want to reference data in our hash table by querying by value of the keys, not by the equivalent object used to store the original values, hence we use :test 'equal
	(make-hash-table :test 'equal)
	))
  
  ;; Setting values in the hash table
  (setf (gethash 1 my-table) "one") ; Integer key
  (setf (gethash "two" my-table) 2) ; String key
  (setf (gethash 'three my-table) '(+ 1 2)) ; Symbol key

  ;; Literal references of the keys in the hash table
  (format t "my-table[1] = ~a~%" (gethash 1 my-table))
  (format t "my-table[\"two\"] = ~a~%" (gethash "two" my-table))
  (format t "my-table['three] = ~a~%" (gethash 'three my-table))

  ;; The way this literal list works is a bit weird... I'll have to spend some time to understand why this works
  (let ((keys '(1 "two" three)))
    (dolist (key keys)
      (format t "my-table[~a] = ~a~%" key (gethash key my-table))
      )
    )
  )
