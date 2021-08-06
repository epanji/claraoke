(cl:in-package #:claraoke-base)

(defun claraoke:version ()
  "v0.0.1")

(defmacro claraoke-internal:output-stream-from-designator (stream)
  (let ((var (gensym "STREAM")))
    `(let ((,var ,stream))
       (cond ((null ,var) *standard-output*)
             ((eq ,var t) *terminal-io*)
             (t ,var)))))

