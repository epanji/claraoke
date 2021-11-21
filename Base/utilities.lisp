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

(defmacro claraoke-internal:deletef (item sequence &rest keyword-arguments)
  "Delete item destructively from location holding sequence."
  `(setf ,sequence (delete ,item ,sequence ,@keyword-arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions
;;;
(defun claraoke-internal:version ()
  "Return the CLARAOKE version."
  #.(let ((file (merge-pathnames "../version.lisp-expr" (or *compile-file-pathname* *load-truename*))))
      (format nil "CLARAOKE v~A" (with-open-file (stream file) (read stream)))))

(defun claraoke-internal:script-note ()
  (let ((repository "https://github.com/epanji/claraoke"))
    (format nil "Script generated by ~A ( ~A )" (claraoke-internal:version) repository)))

(defun claraoke-internal:integer-from-string (string &optional (default 0))
  (check-type string (or null string))
  (check-type default integer)
  (or (parse-integer (string string) :junk-allowed t) default))

(defun claraoke-internal:number-string-p (string)
  (check-type string string)
  (let ((limit (subseq string 0 (min 32 (length string)))))
    (numberp (read-from-string limit))))

(defun claraoke-internal:number-or-string (string)
  (check-type string string)
  (let* ((limit (subseq string 0 (min 32 (length string))))
         (value (read-from-string limit)))
    (if (numberp value) value string)))

(defun claraoke-internal:distinct-number-and-string (strings)
  (mapcar 'claraoke-internal:number-or-string strings))

(defun claraoke-internal:print-symbols
    (package &key (external t) internal inherited
               (function t) (class t) others
     &aux options result rfunction rclass rothers)
  (unless (null external) (pushnew :external options))
  (unless (null internal) (pushnew :internal options))
  (unless (null inherited) (pushnew :inherited options))
  (do-symbols (symbol package)
    (multiple-value-bind (sym acc)
        (find-symbol (string symbol) package)
      (when (member acc options)
        (cond ((fboundp sym)
               (pushnew sym rfunction))
              ((find-class sym nil)
               (pushnew sym rclass))
              (t (pushnew sym rothers))))))
  (setf result (append (unless (null function) rfunction)
                       (unless (null class) rclass)
                       (unless (null others) rothers)))
  (format nil "~{~&#:~(~A~)~}" (sort result 'string-lessp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods
;;;
(defmethod claraoke:print-script ((object string) &optional stream)
  (princ object (claraoke-internal:output-stream-from-designator stream)))

(defmethod claraoke:print-script ((object number) &optional stream)
  (princ object (claraoke-internal:output-stream-from-designator stream)))

(defmethod claraoke:print-script ((object character) &optional stream)
  (princ object (claraoke-internal:output-stream-from-designator stream)))

(defmethod claraoke:print-script ((object null) &optional stream)
  (declare (ignore stream))
  (warn 'claraoke:null-object-warning))

