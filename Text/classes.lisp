(cl:in-package #:claraoke-text)

(defclass text ()
  ((%text
    :initform ""
    :initarg :text
    :accessor claraoke:text)
   (%overrides
    :initform '()
    :initarg :overrides
    :accessor claraoke:overrides)))

(defmethod print-object ((object text) stream)
  (princ (claraoke:text object) stream))

(defclass override ()
  ((%position
    :initform 0
    :initarg :position
    :accessor claraoke:position)
   (%text
    :initform ""
    :initarg :text
    :accessor claraoke:text)))

(defmethod print-object ((object override) stream)
  (princ (claraoke:position object) stream)
  (print-override object stream))

