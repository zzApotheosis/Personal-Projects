;; Define a book structure
(defstruct book
  id
  title
  author)

;; Our "database" - a list to store books
(defvar *books* '())
(defvar *id-counter* 0)

;; CREATE - Add a new book
(defun create-book (title author)
  (let ((new-book (make-book 
                   :id (incf *id-counter*)
                   :title title
                   :author author)))
    (push new-book *books*)
    (format t "Book created with ID: ~a~%" (book-id new-book))
    new-book))

;; READ - Get all books or a specific book by ID
(defun read-books (&optional id)
  (if id
      (find id *books* :key #'book-id)
      *books*))

;; UPDATE - Modify an existing book
(defun update-book (id &key title author)
  (let ((book (read-books id)))
    (when book
      (when title (setf (book-title book) title))
      (when author (setf (book-author book) author))
      (format t "Book ~a updated~%" id)
      book)))

;; DELETE - Remove a book by ID
(defun delete-book (id)
  (let ((original-length (length *books*)))
    (setf *books* (remove id *books* :key #'book-id))
    (if (< (length *books*) original-length)
        (format t "Book ~a deleted~%" id)
        (format t "Book ~a not found~%" id))))

;; Helper function to print a book
(defun print-book (book)
  (format t "ID: ~a, Title: ~a, Author: ~a~%"
          (book-id book)
          (book-title book)
          (book-author book)))

;; Simple interactive interface
(defun crud-interface ()
  (loop
    (format t "~%CRUD Operations:~%")
    (format t "1. Create book~%")
    (format t "2. Read all books~%")
    (format t "3. Read book by ID~%")
    (format t "4. Update book~%")
    (format t "5. Delete book~%")
    (format t "6. Exit~%")
    (format t "Enter choice: ")
    (finish-output)
    
    (let ((choice (parse-integer (read-line) :junk-allowed t)))
      (case choice
        (1 (format t "Enter title: ")
           (finish-output)
           (let ((title (read-line)))
             (format t "Enter author: ")
             (finish-output)
             (let ((author (read-line)))
               (create-book title author))))
        
        (2 (if *books*
               (dolist (book *books*)
                 (print-book book))
               (format t "No books in database~%")))
        
        (3 (format t "Enter book ID: ")
           (finish-output)
           (let ((id (parse-integer (read-line) :junk-allowed t)))
             (let ((book (read-books id)))
               (if book
                   (print-book book)
                   (format t "Book not found~%")))))
        
        (4 (format t "Enter book ID: ")
           (finish-output)
           (let ((id (parse-integer (read-line) :junk-allowed t)))
             (when (read-books id)
               (format t "Enter new title (or press Enter to skip): ")
               (finish-output)
               (let ((title (read-line)))
                 (format t "Enter new author (or press Enter to skip): ")
                 (finish-output)
                 (let ((author (read-line)))
                   (update-book id 
                               :title (if (string= title "") nil title)
                               :author (if (string= author "") nil author)))))))
        
        (5 (format t "Enter book ID: ")
           (finish-output)
           (let ((id (parse-integer (read-line) :junk-allowed t)))
             (delete-book id)))
        
        (6 (format t "Goodbye!~%")
           (return))
        
        (otherwise (format t "Invalid choice~%"))))))

;; Start the application
(defun start ()
  (format t "Welcome to the Book CRUD Application!~%")
  (crud-interface))

(start)
