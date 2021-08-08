(cl:in-package #:claraoke-subtitle)

(defmethod initialize-instance :after ((instance event) &key &allow-other-keys)
  (let ((duration1 (claraoke:duration (claraoke:start instance)))
        (duration2 (claraoke:duration (claraoke:end instance))))
    (setf (claraoke:start instance) duration1)
    (setf (claraoke:end instance) duration2)
    (when (claraoke:duration-greaterp duration1 duration2)
      (claraoke:sync-duration duration2 duration1)
      (claraoke:increase-duration duration2 "1.25"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration start and end accessors
;;;
(defmethod (setf claraoke:start) :around (new-value (event event))
  (let ((duration1 (claraoke:duration new-value))
        (duration2 (claraoke:end event)))
    (when (claraoke:duration-greaterp duration1 duration2)
      (setf (claraoke:end event) duration1))
    (call-next-method duration1 event)))

(defmethod (setf claraoke:end) :around (new-value (event event))
  (call-next-method (claraoke:duration new-value) event))

;; TODO event have slot length

