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
  (:method ((object override) &optional stream)
    (let ((stream (claraoke-internal:output-stream-from-designator stream))
          (string (claraoke:.text object)))
      (unless (= 1 (length string))
        (princ #\{ stream))
      (print-override string stream)
      (unless (= 1 (length string))
        (princ #\} stream))
      object))
  (:method ((object list) &optional stream)
    ;; LISTP for CONS and NULL is T
    (let ((stream (claraoke-internal:output-stream-from-designator stream)))
      (loop for string in object
            do (princ #\\ stream)
               (princ string stream))
      object))
  (:method ((object string) &optional stream)
    ;; Function split-sequence always return CONS or NULL
    (let ((strings (split-sequence:split-sequence #\; object :remove-empty-subseqs t)))
      (print-override strings stream)
      object)))

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

