(cl:in-package #:claraoke-text)

(defclass claraoke:text ()
  ((%text
    :initform ""
    :initarg :text
    :accessor claraoke:text)
   (%overrides
    :initform '()
    :initarg :overrides
    :accessor claraoke:overrides)))

(defclass claraoke:override ()
  ((%position
    :initform 0
    :initarg :position
    :accessor claraoke:position)
   (%text
    :initform ""
    :initarg :text
    :accessor claraoke:text)))

(defmethod claraoke:override ((position integer) (text string))
  (make-instance 'claraoke:override :position position :text text))

