(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse Script
;;;
(defgeneric claraoke:parse-script (object &rest initargs &key &allow-other-keys)
  (:documentation "Return SCRIPT object from parsing OBJECT argument and other arguments."))

