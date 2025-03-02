(defpackage :multi-threading-example
  (:use :cl)
  )

(in-package :multi-threading-example)

(defparameter *shared-counter* 0)
(defparameter *counter-lock* (sb-thread:make-mutex :name "counter-lock"))
(defparameter *thread-count* 420)
(defparameter *increments-per-thread* 69)
(defparameter *wait-period* (round (/ 100000000 *thread-count*))) ; A time period to wait between write operations

(defun increment-counter (id)
  "Increments the shared counter safely using a mutex."
  (loop repeat *increments-per-thread* do
	(sb-unix:nanosleep 0 *wait-period*)
	(sb-thread:with-mutex (*counter-lock*)
	  (format t "Thread #~d incrementing shared data to: ~d~%" id (1+ *shared-counter*))
	  (incf *shared-counter*)
	  )
	)
  )

(defun run-example ()
  "Run the multi-threading example and return the final counter value."
  (setf *shared-counter* 0)

  (let ((threads nil))

    ;; Create threads
    (dotimes (i *thread-count*)
      (push (sb-thread:make-thread #'increment-counter
				   :name (format nil "Thread-~d" i)
				   :arguments (list i))
	    threads)
      )

    ;; Join threads after completion
    (dolist (thread threads)
      (sb-thread:join-thread thread))

    ;; Return the final value of the shared data
    *shared-counter*))

;; Define the main function
(defun main ()
  "Main example function."
  (format t "Starting multi-threading example...~%")

  (block main-block
    ;;(format t "~a~%" *wait-period*)
    ;;(return-from main-block)
    (let ((result (run-example)))
      (format t "Final counter value: ~d~%" result)
      (format t "Expected value: ~d~%" (* *thread-count* *increments-per-thread*))
      )
    )
  )

;; Run the example
(multi-threading-example::main)	
