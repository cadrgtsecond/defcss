(defpackage #:defcss
  (:use #:cl #:iterate)
  (:export #:def #:make-file #:file->string #:*current-file*))
(in-package #:defcss)

(let ((counter 0))
  (defun generate-unique-class (original)
    "Generates a unique class name"
    (incf counter)
    (let ((classname (format nil "~a~a" original counter)))
      (values (format nil ".~a" classname) classname))))

(defstruct file
  "A css file containing css classes"
  (classes (make-hash-table) :type hash-table))

(defun file-ref (class file)
  (gethash class (file-classes file)))
(defsetf file-ref (class file) (val)
  `(setf (gethash ,class (file-classes ,file)) ,val))

(defun file->string (&optional (file *current-file*))
  (apply
    #'lass:compile-and-write
    (loop for class being the hash-values of (file-classes file)
          collect class)))

(defparameter *current-file* (make-file))
(declaim (type file *current-file*))

(defun expand-class (name args-and-parents body)
  ;; TODO: Do something with args-and-parents
  (declare (ignore args-and-parents))
  (multiple-value-bind (css-name real-name) (generate-unique-class (string-downcase (string name)))
    `(progn
       (setf (file-ref ',name *current-file*) '(,css-name ,@body))
       (define-symbol-macro ,name ,real-name))))

(defmacro def (name-and-args &body body)
  (if (not (listp name-and-args))
     `(def (,name-and-args) ,@body)
      (expand-class
        (car name-and-args)
        (cdr name-and-args)
        body)))

;;; Tests
#+nil
(def test
  :background "red"
  :color "yellow")
