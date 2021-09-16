(cl:in-package #:claraoke-subtitle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interval
;;;
(defmethod claraoke:interval ((object subtitle))
  (claraoke:interval (claraoke:events object)))

(defmethod (setf claraoke:interval) (new-value (object subtitle))
  (setf (claraoke:interval (claraoke:events object)) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interval frequency
;;;
(defmethod claraoke:interval-frequency ((object subtitle))
  (claraoke:interval-frequency (claraoke:events object)))

(defmethod (setf claraoke:interval-frequency) (new-value (object subtitle))
  (setf (claraoke:interval-frequency (claraoke:events object)) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interval counter
;;;
(defmethod claraoke:interval-counter ((object subtitle))
  (claraoke:interval-counter (claraoke:events object)))

(defmethod (setf claraoke:interval-counter) (new-value (object subtitle))
  (setf (claraoke:interval-counter (claraoke:events object)) new-value))

