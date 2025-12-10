(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print script
;;;
(defgeneric claraoke:print-script (object &optional stream)
  (:documentation "Return OBJECT argument with output printed to STREAM argument."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print remake
;;;
(defgeneric claraoke:print-remake (object &optional stream name)
  (:documentation "Return OBJECT argument with output printed to STREAM argument and NAME argument as variable name."))

