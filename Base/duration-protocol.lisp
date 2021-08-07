(cl:in-package #:claraoke-base)

(defgeneric claraoke:duration (object))

(defgeneric claraoke:durationp (object))

(defgeneric claraoke:durationstring (object))

(defgeneric claraoke:durationstringp (object))

(defgeneric claraoke:durationinteger (object))

(defgeneric claraoke:durationintegerp (object))

(defgeneric claraoke:sync-duration (duration source))

(defgeneric claraoke:increase-duration (duration addition))

(defgeneric claraoke:decrease-duration (duration subtraction))

(defgeneric claraoke:duration-lessp (duration1 duration2))

(defgeneric claraoke:duration-greaterp (duration1 duration2))

(defgeneric claraoke:duration-difference (duration1 duration2))

