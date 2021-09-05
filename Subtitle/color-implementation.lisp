(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; After initialization
;;;
(defmethod initialize-instance :after ((instance style) &key)
  (let ((primary (claraoke:primary-colour instance))
        (secondary (claraoke:secondary-colour instance))
        (outline (claraoke:outline-colour instance))
        (back (claraoke:back-colour instance)))
    (setf (claraoke:primary-colour instance) (claraoke:color primary)
          (claraoke:secondary-colour instance) (claraoke:color secondary)
          (claraoke:outline-colour instance) (claraoke:color outline)
          (claraoke:back-colour instance) (claraoke:color back))))

