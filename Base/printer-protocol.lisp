(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print script
;;;
(defgeneric claraoke:print-script (object &optional stream)
  (:documentation "Return OBJECT argument with output printed to STREAM argument."))

