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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print combine remake
;;;
(defgeneric claraoke:print-combine-remake (stream name &rest subtitles)
  (:documentation "Return STRING if STREAM argument is NIL, otherwise print output to stream and return NIL.
STREAM argument could be boolean or any stream designator.
NAME argument could be string without space or boolean for default name.
SUBTITLE argument could be subtitle object or pathname designator."))

