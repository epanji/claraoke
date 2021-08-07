(cl:in-package #:claraoke-duration)

(defclass duration ()
  ((%hours
    :initform 0
    :initarg :h
    :initarg :hours
    :accessor claraoke:hours)
   (%minutes
    :initform 0
    :initarg :m
    :initarg :minutes
    :accessor claraoke:minutes)
   (%seconds
    :initform 0
    :initarg :s
    :initarg :seconds
    :accessor claraoke:seconds)
   (%centiseconds
    :initform 0
    :initarg :cs
    :initarg :centiseconds
    :accessor claraoke:centiseconds)))

(defmethod print-object ((object duration) stream)
  (princ (claraoke:durationstring object) stream))

