(cl:in-package #:claraoke-base)

(defun claraoke:version ()
  "v0.0.1")

(defun claraoke-internal:integer-from-string (string &optional (default 0))
  (check-type string (or null string))
  (check-type default integer)
  (or (parse-integer (string string) :junk-allowed t) default))

(defmacro claraoke-internal:output-stream-from-designator (stream)
  (let ((var (gensym "STREAM")))
    `(let ((,var ,stream))
       (cond ((null ,var) *standard-output*)
             ((eq ,var t) *terminal-io*)
             (t ,var)))))

(defmethod claraoke:print-script ((object null) &optional stream)
  (declare (ignore stream))
  (warn 'claraoke:null-object-warning))

