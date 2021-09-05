(cl:in-package #:claraoke-text)

(defclass text ()
  ((%original-text
    :initform nil
    :initarg :original-text
    :accessor claraoke:original-text)
   (%text
    :initform ""
    :initarg :text
    :accessor claraoke:.text)
   (%overrides
    :initform '()
    :initarg :overrides
    :accessor claraoke:overrides)))

(defclass override ()
  ((%index
    :initform 0
    :initarg :index
    :accessor claraoke:index)
   (%text
    :initform ""
    :initarg :text
    :accessor claraoke:.text)))

