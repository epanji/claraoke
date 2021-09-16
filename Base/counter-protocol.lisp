(cl:in-package #:claraoke-base)

(defclass claraoke:interval-counter ()
  ((%interval
    :initform 60
    :accessor claraoke:interval)
   (%interval-frequency
    :initform 8
    :initarg :interval-frequency
    :accessor claraoke:interval-frequency)
   (%interval-counter
    :initform nil
    :accessor claraoke:interval-counter)))

