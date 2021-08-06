(cl:in-package #:claraoke-base)

(defgeneric claraoke:duration (duration))

(defgeneric claraoke:durationp (duration))

(defgeneric claraoke:durationstring (duration))

(defgeneric claraoke:durationstringp (string))

(defgeneric claraoke:durationinteger (duration))

(defgeneric claraoke:durationintegerp (string))

(defgeneric claraoke:sync-duration (duration source))

(defgeneric claraoke:increase-duration (duration addition))

(defgeneric claraoke:decrease-duration (duration subtraction))

(defgeneric claraoke:duration-lessp (duration1 duration2))

(defgeneric claraoke:duration-greaterp (duration1 duration2))

(defgeneric claraoke:duration-difference (duration1 duration2))

