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
    :accessor claraoke:centiseconds)
   ;; Extra digit affect durationstringp, synch-duration,
   ;; increase-duration, decrease-duration, #<duration object>,
   ;; duration from string "0:00:00.00x" which x is optional.
   (%extradigit
    :initform 0
    :initarg :ed
    :initarg :extradigit
    :accessor claraoke:extradigit)))

