(cl:in-package #:claraoke-base)

(defgeneric claraoke:print-script (object &optional stream)
  (:documentation "Output readable printed representation of OBJECT to the specified STREAM."))

