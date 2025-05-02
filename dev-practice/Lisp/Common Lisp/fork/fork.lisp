(require :sb-posix)

;;(loop for symbol being the external-symbols of :sb-posix do
;;      (format t "~a~%" symbol))

(defun fork-and-exec-example ()
  (let ((cpid (sb-posix:fork)))
    (cond
      ((< cpid 0)
       ;; Fork failed
       (format t "Fork failed!~%")
       )

      ((= cpid 0)
       ;; Child process
       (format t "In child process (PID: ~a)~%" (sb-posix:getpid))
       
       ;; Replace child process with a new program (e.g., "ls" or "dir" equivalent)
       ;;(sb-posix:execvp "echo" (vector "ECHO" "Hello from child!"))

       ;;(let ((args (sb-alien:make-alien (array c-string 3))))
       ;;	 (setf (sb-alien:deref args 0) "echo")
       ;;	 (setf (sb-alien:deref args 1) "Hello from execvp!")
       ;;	 (setf (sb-alien:deref args 2) (sb-alien:null-alien (* c-string)))
       ;;	 (alien-funcall (extern-alien "execvp" (function int c-string (* c-string)))
       ;;			"echo" args)
       ;;	 )
       
       ;; If execvp returns, it failed
       ;;(format t "Exec failed!~%")
       (format t "Child process completed.~%")
       (sb-posix:exit 0)
       )

      (t
       ;; Parent process
       (format t "In parent process (PID: ~a), child PID: ~a~%" (sb-posix:getpid) cpid)

       ;; Wait for child to finish
       (sb-posix:wait)
       (format t "Parent process completed.~%")
       )
      )
    )
  )

(fork-and-exec-example)
