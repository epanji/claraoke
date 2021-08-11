(cl:in-package #:claraoke-text)

(defclass text ()
  ((%text
    :initform ""
    :initarg :text
    :accessor claraoke:.text)
   (%overrides
    :initform '()
    :initarg :overrides
    :accessor claraoke:overrides)))

(defmethod print-object ((object text) stream)
  (princ (claraoke:.text object) stream))

(defclass override ()
  ((%index
    :initform 0
    :initarg :index
    :accessor claraoke:index)
   (%text
    :initform ""
    :initarg :text
    :accessor claraoke:.text)))

(defmethod print-object ((object override) stream)
  (princ (claraoke:index object) stream)
  (print-override object stream))

