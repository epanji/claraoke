(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros
;;;
(defmacro claraoke-internal:output-stream-from-designator (stream)
  (let ((var (gensym "STREAM")))
    `(let ((,var ,stream))
       (cond ((null ,var) *standard-output*)
             ((eq ,var t) *terminal-io*)
             (t ,var)))))

(defmacro claraoke-internal:mimic-accessor (name (accessor object) &body body)
  "Mimic generated ACCESSOR for NAME with specializer on OBJECT.
Write BODY if necessary for returning specializer on T otherwise it will returning NIL."
  `(progn (defmethod (setf ,name) (new-value ,object)
            (setf (,accessor ,object) new-value))
          (defmethod ,name (,object &key)
            (if (typep ,object 'standard-object)
                (,accessor ,object)
                (progn ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions
;;;
(defun claraoke-internal:version ()
  "Return the CLARAOKE version."
  #.(let ((file (merge-pathnames "../version.lisp-expr" (or *compile-file-pathname* *load-truename*))))
      (format nil "CLARAOKE v~A" (with-open-file (stream file) (read stream)))))

(defun claraoke-internal:integer-from-string (string &optional (default 0))
  (check-type string (or null string))
  (check-type default integer)
  (or (parse-integer (string string) :junk-allowed t) default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods
;;;
(defmethod claraoke:print-script ((object string) &optional stream)
  (princ object (claraoke-internal:output-stream-from-designator stream)))

(defmethod claraoke:print-script ((object null) &optional stream)
  (declare (ignore stream))
  (warn 'claraoke:null-object-warning))

