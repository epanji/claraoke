(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print script (ASS)
;;;
(defmethod claraoke:print-script ((object text) &optional stream)
  (let ((overrides (claraoke:overrides object))
        (string (claraoke:.text object))
        (stream (claraoke-internal:output-stream-from-designator stream)))
    (loop for index from 0
          for char across string
          and override = (claraoke:find-override overrides index)
          do (print-override override stream)
             (princ char stream))
    object))

(defmethod claraoke:print-script ((object override) &optional stream)
  (print-override object stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print override (Internal)
;;;
(defgeneric print-override (object &optional stream)
  (:method ((object null) &optional stream)
    (declare (ignore stream))
    object)
  (:method ((object cons) &optional stream)
    (loop for override in object
          do (print-override override stream))
    object)
  (:method ((object modifier) &optional stream)
    (let ((stream (claraoke-internal:output-stream-from-designator stream))
          (control (format-control object))
          (arg1 (slot-value object 'arg1))
          (arg2 (slot-value object 'arg2))
          (arg3 (slot-value object 'arg3))
          (arg4 (slot-value object 'arg4))
          (arg5 (slot-value object 'arg5))
          (arg6 (slot-value object 'arg6)))
      (format stream control arg1 arg2 arg3 arg4 arg5 arg6))
    object)
  (:method ((object override) &optional stream)
    (call-next-method object stream))
  (:method ((object batch) &optional stream)
    (let ((stream (claraoke-internal:output-stream-from-designator stream))
          (overrides (claraoke:overrides object)))
      (unless (null overrides)
        (princ #\{ stream))
      (print-override (reverse overrides) stream)
      (unless (null overrides)
        (princ #\} stream)))
    object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print Object
;;;
(defmethod print-object ((object text) stream)
  (princ "#<" stream)
  (princ (claraoke:.text object) stream)
  (princ ">" stream))

(defmethod print-object ((object override) stream)
  (princ (claraoke:index object) stream)
  (print-override object stream))

(defmethod print-object ((object unknown) stream)
  (princ #\? stream)
  (print-override object stream))

