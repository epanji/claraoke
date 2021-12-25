(cl:in-package #:claraoke-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print Script
;;;
(defmethod claraoke:print-script ((object color) &optional stream)
  (claraoke:print-script (claraoke:colorstring object) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print Object
;;;
(defmethod print-object ((object color) stream)
  (princ #\# stream)
  (princ #\< stream)
  (princ (claraoke:colorstring object) stream)
  (princ #\> stream))

