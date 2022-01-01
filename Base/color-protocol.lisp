(cl:in-package #:claraoke-base)

(defgeneric claraoke:color (color)
  (:documentation "Return COLOR object from various COLOR argument."))

(defgeneric claraoke:colorp (color)
  (:documentation "Return BOOLEAN for COLOR object from COLOR argument."))

(defgeneric claraoke:colorstring (color)
  (:documentation "Return STRING represent acceptable color from COLOR argument."))

(defgeneric claraoke:colorstringp (color)
  (:documentation "Return BOOLEAN for STRING represent acceptable color from COLOR argument."))

(defgeneric claraoke:alpha (object)
  (:documentation "Return INTEGER with range 0 to 255 from various OBJECT argument."))

(defgeneric claraoke:alphap (object)
  (:documentation "Return BOOLEAN for INTEGER with range 0 to 255 from OBJECT argument."))

(defgeneric claraoke:alphastring (object)
  (:documentation "Return STRING represent acceptable alpha from OBJECT argument."))

(defgeneric claraoke:alphastringp (object)
  (:documentation "Return BOOLEAN for STRING represent acceptable alpha from OBJECT argument."))

(defgeneric claraoke:rgb (red green blue &optional alpha)
  (:documentation "Return COLOR object from RED, GREEN, BLUE and optional ALPHA arguments."))

(defgeneric claraoke:random-color (&optional alpha)
  (:documentation "Return COLOR object from random RGB and optional ALPHA argument."))

