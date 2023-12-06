(defpackage #:defcss
  (:use #:cl #:iterate)
  (:export #:defcss #:make-file #:file->string #:*current-file*))
(in-package #:defcss)

(let ((counter 0))
  (defun unique-name (original)
    "Generates a unique style name"
    (incf counter)
    (let ((name (format nil "~a~a" original counter)))
      (values (format nil ".~a" name) name))))

(defstruct file
  "A css file containing css styles"
  (styles (make-hash-table) :type hash-table))

(defun file-ref (style file)
  (gethash style (file-styles file)))
(defsetf file-ref (style file) (val)
  `(setf (gethash ,style (file-styles ,file)) ,val))

(defun file->string (&optional (file *current-file*))
  (apply
    #'lass:compile-and-write
    (loop for style being the hash-values of (file-styles file)
          collect (style-value style))))

(defparameter *current-file* (make-file))
(declaim (type file *current-file*))

(defstruct (style (:constructor %make-style))
  value
  class)

(defun make-style (name body)
  (multiple-value-bind (file-name dotless-name)
                       (unique-name (string-downcase (string name)))
    (%make-style
      :value `(,file-name ,@body)
      :class dotless-name)))

(defmacro-clause (for d in-style s)
  "Iterate through a LASS style. Returns (k v) for properties and (k nil) for substyles"
  (a:with-gensyms (l)
    `(progn
       (for ,l
            :initially ,s
            :then (if (listp (car ,l)) (cdr ,l) (cddr ,l)))
       (while ,l)
       (for ,d = ,l))))

(defun merge-style-bodies (s1 s2)
  "Merges style s1 with style s2. Both styles are simple LASS expressions, not `style` objects"
  (iter (with res = s2)
        (for (k v) in-style s1)

        (if (not (listp k))
          (setf (getf res k) v)
          ;; TODO: merge sub-styles properly
          ;; Also this is inefficient
          (push k (cdr (last res))))

        (finally (return res))))
#+nil
(merge-style-bodies
  '(:background "red"
    :color "white"
    (substyle :hello 10 :world 20)
    (another :test 20))
  '((substyle :key 10)))

(defun expand-style (name args-and-parents body)
  ;; TODO: Do something with args-and-parents
  (declare (ignore args-and-parents))
  (let ((style (make-style name body)))
    `(progn
       (setf (file-ref ',name *current-file*) ,style)
       (defvar ,name)
       (setf ,name ,(style-class style)))))

(defmacro defcss (name-and-args &body body)
  (if (not (listp name-and-args))
     `(defcss (,name-and-args) ,@body)
      (expand-style
        (car name-and-args)
        (cdr name-and-args)
        body)))

;;; Tests
#+nil
(defcss test
  :background "red"
  :color "yellow")

#+nil
(defcss other
  :background "blue"
  :color "white")

#+nil
(file->string *current-file*)
