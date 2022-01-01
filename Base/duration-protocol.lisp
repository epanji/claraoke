(cl:in-package #:claraoke-base)

(defgeneric claraoke:duration (object)
  (:documentation "Return DURATION object from various OBJECT argument."))

(defgeneric claraoke:durationp (object)
  (:documentation "Return BOOLEAN for DURATION object from OBJECT argument."))

(defgeneric claraoke:durationstring (object)
  (:documentation "Return STRING represent acceptable duration from OBJECT argument."))

(defgeneric claraoke:durationstringp (object)
  (:documentation "Return BOOLEAN for STRING represent acceptable duration from OBJECT argument."))

(defgeneric claraoke:durationinteger (object)
  (:documentation "Return INTEGER represent acceptable duration from OBJECT argument."))

(defgeneric claraoke:durationintegerp (object)
  (:documentation "Return BOOLEAN for INTEGER represent acceptable duration from OBJECT argument."))

(defgeneric claraoke:synch-duration (duration source)
  (:documentation "Return DURATION object with synchronized values from various SOURCE argument.
The DURATION should be EQ with DURATION argument."))

(defgeneric claraoke:increase-duration (duration addition)
  (:documentation "Return DURATION object with addition values from ADDITION argument.
The DURATION should be EQ with DURATION argument."))

(defgeneric claraoke:decrease-duration (duration subtraction)
  (:documentation "Return DURATION object with subtraction values from SUBTRACTION argument.
The DURATION should be EQ with DURATION argument."))

(defgeneric claraoke:duration-lessp (duration1 duration2)
  (:documentation "Return BOOLEAN represent DURATION1 argument less than DURATION2 argument."))

(defgeneric claraoke:duration-greaterp (duration1 duration2)
  (:documentation "Return BOOLEAN represent DURATION1 argument greater than DURATION2 argument."))

(defgeneric claraoke:duration-difference (duration1 duration2)
  (:documentation "Return INTEGER represent difference value from DURATION1 and DURATION2 arguments.
The INTEGER should be non negative value."))

