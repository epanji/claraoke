(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; After initialization
;;;
(defmethod initialize-instance :after ((instance event) &key duration &allow-other-keys)
  (let ((duration1 (claraoke:duration (claraoke:start instance)))
        (duration2 (claraoke:duration (claraoke:end instance))))
    (setf (claraoke:end instance) duration2)
    (unless (null duration)
      (claraoke:synch-duration
       duration2
       (+ (claraoke:durationinteger duration1)
          (claraoke:durationinteger (claraoke:duration duration)))))
    (setf (claraoke:start instance) duration1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration start and end accessors
;;;
(defmethod (setf claraoke:start) :around (new-value (event event))
  (let ((duration1 (claraoke:duration new-value))
        (duration2 (claraoke:duration (claraoke:end event))))
    (when (claraoke:duration-greaterp duration1 duration2)
      (claraoke:synch-duration duration2 duration1)
      (claraoke:increase-duration duration2 "0.25"))
    (call-next-method duration1 event)))

(defmethod (setf claraoke:end) :around (new-value (event event))
  (call-next-method (claraoke:duration new-value) event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration length
;;;
(defgeneric claraoke:duration-length (object &optional start)
  (:method ((object claraoke-duration:duration) &optional (start 0))
    (let ((copy-duration (claraoke:duration (claraoke:durationinteger object))))
      (claraoke:decrease-duration copy-duration start)))
  (:method ((object integer) &optional (start 0))
    (claraoke:duration-length (claraoke:duration object) start))
  (:method ((object string) &optional (start 0))
    (claraoke:duration-length (claraoke:duration object) start))
  (:method ((object event) &optional (start 0))
    (let ((start (or (claraoke:start object) (claraoke:duration start)))
          (end (claraoke:end object)))
      (claraoke:duration-length end start))))

(defmethod (setf claraoke:duration-length) (new-value (object event))
  (let ((new-duration (claraoke:duration new-value))
        (duration1 (claraoke:durationinteger (claraoke:start object)))
        (duration2 (claraoke:duration (claraoke:end object))))
    (claraoke:synch-duration duration2 (claraoke:increase-duration new-duration duration1))
    object))

