(defpackage #:defcss
  (:use #:cl #:iterate)
  (:export #:defcss #:make-css-file #:style-class #:style #:file->string #:file)
  (:local-nicknames (#:util #:serapeum/bundle)))
(in-package #:defcss)

(let ((counter 0))
  (defun unique-name (original)
    "Generates a unique style name"
    (incf counter)
    (values
      (format nil ".~a~a" original counter)
      (format nil "~a~a" original counter))))

(defstruct (style (:constructor %make-style))
  (value nil :type list)
  (class nil :type string))

(defun css-string (val)
  "Converts a value to a nice-looking string(That isn't shouty case)"
  (string-downcase (string val)))

(defun make-style (name body &key parents mangle-p)
  (multiple-value-bind (file-in-name class)
      (if mangle-p
        (unique-name (css-string name))
        (values name (css-string name)))
    (%make-style
      :value `(,file-in-name
                ,@(iter (for p :in parents)
                        (reducing (cdr (style-value p))
                                  :by #'merge-style-bodies
                                  :into res)
                        (finally (return (merge-style-bodies res body)))))
      :class class)))

(defstruct (file (:constructor make-css-file))
  "A css file containing css styles"
  (styles (make-hash-table) :type hash-table))

(util:defplace file-ref (style file)
  (gethash style (file-styles file)))

(defun file->string (file)
  "Convert a `file` to a string that may be served by a web server"
  (apply
    #'lass:compile-and-write
    (loop for style being the hash-values of (file-styles file)
          collect (style-value style))))

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

(defun expand-style (name args parents body)
  (let ((file (util:assocadr :in args))
        (global-p (util:assocadr :global args)))
    (when (and global-p (not file))
      (error "Global css definitions require a file. Use :IN to specify one"))
    (util:once-only ((style `(make-style
                               ',name
                               ',body
                               :parents (list ,@parents)
                               :mangle-p ,(not global-p))))
      `(progn
         ,(when file
            `(setf (file-ref ',name ,file) ,style))
         ,(when (not global-p)
            `(defparameter ,name ,style))))))

(defmacro defcss (name-and-args &body body)
  (multiple-value-bind (args parents)
      (util:partition #'listp (util:cdr-safe name-and-args))
    (expand-style (util:ensure-car name-and-args) args parents body)))

;;; Tests
#+nil
(defparameter *test-file* (make-css-file))
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
(defcss (:root (:global t) (:in *test-file*))
  :--background-color "red")

#+nil
(file->string *test-file*)
