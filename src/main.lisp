(defpackage #:defcss
  (:use #:cl #:iterate)
  (:export #:defcss #:make-file #:file->string #:style-name)
  (:local-nicknames (#:util #:serapeum/bundle)))
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

(defun file->string (file)
  (apply
    #'lass:compile-and-write
    (loop for style being the hash-values of (file-styles file)
          collect (style-value style))))

(defstruct (style (:constructor %make-style))
  value
  class)

(defun make-style (name body parents)
  (multiple-value-bind (file-name dotless-name)
                       (unique-name (string-downcase (string name)))
    (%make-style
      :value `(,file-name ,@(reduce #'merge-style-bodies
                              (append
                                (mapcar
                                   (util:compose #'cdr #'style-value #'symbol-value)
                                   parents)
                                (list body))))
      :class dotless-name)))

(defmacro-clause (for d in-style s)
  "Iterate through a LASS style. Returns (k v) for properties and (k nil) for substyles"
  (util:with-gensyms (l)
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

(defun parse-args-and-parents (a-and-p)
  "Seperates arguments and parent styles from the list given to `defcss`. Return `args` and `parents`"
  (util:partition #'listp a-and-p))
#+nil
(parse-args-and-parents '(parent1 parent2 (:opt "val") (:other "val")))

(defun expand-style (name args parents body)
  (let ((file (cadr (assoc :in args)))
        (style (make-style name body parents)))
    (print file)
    `(progn
       ,(when file
          `(setf (file-ref ',name ,file) ,style))
       (defvar ,name)
       (setf ,name ,style))))

(defmacro defcss (name-and-args &body body)
  (if (not (listp name-and-args))
     `(defcss (,name-and-args) ,@body)
      (multiple-value-bind (args parents)
          (parse-args-and-parents (cdr name-and-args))
        (expand-style (car name-and-args) args parents body))))

;;; Tests
#+nil
(defparameter *test-file* (make-file))
#+nil
(defcss test
  :background "red"
  :color "yellow")

#+nil
(defcss flexing
  :display "flex"
  :flex-direction "row")

#+nil
;; This should be the only thing in *test-file*
(defcss (composed test flexing (:in *test-file*)))

#+nil
(file->string *test-file*)
