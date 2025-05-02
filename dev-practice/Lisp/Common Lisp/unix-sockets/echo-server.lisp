(require 'sb-bsd-sockets)

(defpackage :unix-socket-echo-server
  (:use :cl :sb-bsd-sockets))
(in-package :unix-socket-echo-server)

(defun echo-server (socket-path)
  "Create a Unix socket echo server at the given filesystem path."
  (let ((server-socket (make-instance 'local-socket :type :stream)))
    (unwind-protect
         (progn
           ;; Bind the socket to the filesystem path
           (socket-bind server-socket socket-path)
           ;; Listen for incoming connections (backlog of 5)
           (socket-listen server-socket 5)
           (format t "Server listening on ~A~%" socket-path)
           
           ;; Main loop to accept and handle connections
           (loop
             (let ((client-socket (socket-accept server-socket)))
               (unwind-protect
                    (let ((stream (socket-make-stream client-socket
                                                     :input t
                                                     :output t
                                                     :buffering :full)))
                      (format t "Client connected~%")
                      ;; Echo loop: read lines and send them back
                      (ignore-errors
                       (loop for line = (read-line stream nil nil)
                            while line
                            do (format t "~a~%" line)
                               (write-line line stream)
                               (force-output stream))
                       )
		      )
                    

                      ;; Ensure client socket is closed
                      (ignore-errors
                       (socket-close client-socket)
                       )
                      (format t "Client disconnected~%")
                )
              )
            )
          )
      ;; Cleanup: close server socket and remove socket file
      (socket-close server-socket)
      (when (probe-file socket-path)
        (delete-file socket-path))
      (format t "Server shut down~%"))))

;; Run the server
(defun start-server ()
  (echo-server "/tmp/echo-server.sock"))

;; Call this to start the server in your SBCL REPL
(start-server)
