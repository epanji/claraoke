(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print script (ASS)
;;;
(defmethod claraoke:print-script ((object text) &optional stream)
  (let ((overrides (claraoke:overrides object))
        (string (claraoke:.text object)))
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
(defvar *stream* nil)
(defvar *stream-endp* nil)

(defgeneric print-override (object &optional stream)
  (:method :around (object &optional stream)
    (let ((*stream-endp* (not (streamp *stream*)))
          (*stream* (if (streamp *stream*)
                        *stream*
                        (make-string-output-stream))))
      (unwind-protect (call-next-method object *stream*)
        (when *stream-endp*
          (princ (get-output-stream-string *stream*)
                 (claraoke-internal:output-stream-from-designator stream))
          (close *stream*)
          object))))
  (:method ((object null) &optional stream)
    (declare (ignore stream))
    object)
  (:method ((object cons) &optional stream)
    (loop for override in object
          do (print-override override stream))
    object)
  (:method ((object modifier) &optional stream)
    (let ((control (format-control object))
          (arg1 (claraoke:arg1 object))
          (arg2 (claraoke:arg2 object))
          (arg3 (claraoke:arg3 object))
          (arg4 (claraoke:arg4 object))
          (arg5 (claraoke:arg5 object))
          (arg6 (claraoke:arg6 object))
          (arg7 (claraoke:arg7 object)))
      (format stream control arg1 arg2 arg3 arg4 arg5 arg6 arg7))
    object)
  (:method ((object override) &optional stream)
    (call-next-method object stream))
  (:method ((object batch) &optional stream)
    (let ((newline (claraoke:find-modifier object 'newline))
          (modifiers (claraoke:modifiers object)))
      (when (typep newline 'newline)
        (print-override newline stream)
        (setf modifiers (remove newline modifiers)))
      (unless (null modifiers)
        (princ #\{ stream))
      (print-override (reverse modifiers) stream)
      (unless (null modifiers)
        (princ #\} stream)))
    object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print Object
;;;
(defmethod print-object ((object text) stream)
  (let ((overrides (claraoke:overrides object)))
    (princ #\# stream)
    (princ #\< stream)
    (prin1 (claraoke:.text object) stream)
    (unless (null overrides)
      (princ #\Space stream)
      (princ #\( stream)
      (princ (length overrides) stream)
      (princ #\) stream))
    (princ #\> stream)))

(defmethod print-object ((object override) stream)
  (princ (claraoke:index object) stream)
  (print-override object stream))

(defmethod print-object ((object unknown) stream)
  (princ #\? stream)
  (print-override object stream))

