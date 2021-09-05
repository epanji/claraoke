(cl:in-package #:claraoke-color)

(defclass color ()
  ((%red
    :initform 0
    :initarg :red
    :accessor claraoke:red)
   (%green
    :initform 0
    :initarg :green
    :accessor claraoke:green)
   (%blue
    :initform 0
    :initarg :blue
    :accessor claraoke:blue)
   (%alpha
    :initform nil
    :initarg :alpha
    :accessor claraoke:alpha)))

