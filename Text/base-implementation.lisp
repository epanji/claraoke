(cl:in-package #:claraoke-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print script (ASS)
;;;
(defmethod claraoke:print-script ((object text) &optional stream)
  (let ((overrides (claraoke:overrides object))
        (string (claraoke:text object))
        (stream (claraoke-internal:output-stream-from-designator stream)))
    (loop for position from 0
          for char across string
          and override = (claraoke:find-override overrides position)
          do (print-override override stream)
             (princ char stream))
    object))

(defmethod claraoke:print-script ((object override) &optional stream)
  (print-override object stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print override (Internal)
;;;
(defmethod print-override ((object override) &optional stream)
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (princ #\{ stream)
    (print-override (claraoke:text object) stream)
    (princ #\} stream)
    object))

(defmethod print-override ((object list) &optional stream)
  ;; LISTP for CONS and NULL is T
  (let ((stream (claraoke-internal:output-stream-from-designator stream)))
    (loop for string in object
          do (princ #\\ stream)
             (princ string stream))
    object))

(defmethod print-override ((object string) &optional stream)
  ;; Function split-sequence always return CONS or NULL
  (let ((strings (split-sequence:split-sequence #\; object :remove-empty-subseqs t)))
    (print-override strings stream)
    object))

