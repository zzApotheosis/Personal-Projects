#!/bin/env -S sbcl --script
(defstruct job-queue
  (jobs nil :type list))

;; Enqueue: Adds a job (closure) to the end of the queue
(defun enqueue (queue job)
  (setf (job-queue-jobs queue) (append (job-queue-jobs queue) (list job)))
  (values t nil)) ; Returning (values success error) to mimic Rust's Result<(), String>

;; Pop: Removes and returns the first job from the queue
(defun pop-job (queue)
  (if (null (job-queue-jobs queue))
      nil ; Mimics Option::None
      (let ((job (car (job-queue-jobs queue))))
        (setf (job-queue-jobs queue) (cdr (job-queue-jobs queue)))
        job))) ; Returns the job, mimicking Option::Some(Job)

;; Size: Returns the number of jobs in the queue
(defun queue-size (queue)
  (length (job-queue-jobs queue)))

;; Clear: Empties the queue
(defun clear-queue (queue)
  (setf (job-queue-jobs queue) nil))

;; Main Execution
(defun main ()
  (let ((n 16)
        (queue (make-job-queue)))

    ;; Enqueue loop
    ;; Note: 'dotimes' automatically captures the loop variable 'i' correctly for closures
    (dotimes (i n)
      ;; We use a lexical closure to capture the current value of i
      (let ((current-i i))
        (multiple-value-bind (success err)
            (enqueue queue (lambda () 
                             (format *error-output* "[~D] Check this out!~%" current-i)))
          (when err
            (format *error-output* "~A~%" err))))
             )

    ;; Assert size > 0
    (assert (> (queue-size queue) 0))

    ;; Processing loop
    (loop while (> (queue-size queue) 0) do
      (let ((job (pop-job queue)))
        (when job
          (funcall job)
          )
        )
      )

    ;; Assert size == 0
    (assert (= (queue-size queue) 0)
            )
    )
  )

;; Run the main function
(main)
