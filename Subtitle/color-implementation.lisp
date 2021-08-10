(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; After initialization
;;;
(defmethod initialize-instance :after ((instance style) &rest initargs &key &allow-other-keys)
  (let ((primary (claraoke:primary-colour instance))
        (secondary (claraoke:secondary-colour instance))
        (outline (claraoke:outline-colour instance))
        (back (claraoke:back-colour instance)))
    (setf (claraoke:primary-colour instance)
          (claraoke:color primary))
    (setf (claraoke:secondary-colour instance)
          (claraoke:color secondary))
    (setf (claraoke:outline-colour instance)
          (claraoke:color outline))
    (setf (claraoke:back-colour instance)
          (claraoke:color back))))

