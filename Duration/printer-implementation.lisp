(cl:in-package #:claraoke-duration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print Script
;;;
(defmethod claraoke:print-script ((object duration) &optional stream)
  (claraoke:print-script (claraoke:durationstring object) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print Object
;;;
(defmethod print-object ((object duration) stream)
  (princ "#<" stream)
  (princ (claraoke:durationstring object) stream)
  (princ ">" stream))

