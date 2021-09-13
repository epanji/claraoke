(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse Script
;;;
(defgeneric claraoke:parse-script (object &rest initargs &key &allow-other-keys)
  (:documentation "Parse OBJECT to be SCRIPT instance."))

