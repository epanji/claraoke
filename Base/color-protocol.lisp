(cl:in-package #:claraoke-base)

(defgeneric claraoke:color (color))

(defgeneric claraoke:colorp (color))

(defgeneric claraoke:colorstring (color))

(defgeneric claraoke:colorstringp (color))

(defgeneric claraoke:alpha (object))

(defgeneric claraoke:alphap (object))

(defgeneric claraoke:alphastring (object))

(defgeneric claraoke:alphastringp (object))

(defgeneric claraoke:rgb (red green blue &optional alpha))

(defgeneric claraoke:random-color (&optional alpha))

