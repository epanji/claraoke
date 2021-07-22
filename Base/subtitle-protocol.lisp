(cl:in-package #:claraoke-base)

(defgeneric claraoke:print-subtitle (subtitle &optional stream))

(defgeneric claraoke:insert-style (subtitle style))

(defgeneric claraoke:delete-style (subtitle style))

(defgeneric claraoke:find-style (subtitle content))

(defgeneric claraoke:insert-event (subtitle event))

(defgeneric claraoke:delete-event (subtitle event))

(defgeneric claraoke:find-event (subtitle position))

(defgeneric claraoke:last-event (subtitle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Position will not be arranged manually.
;;; Because event depends on start duration, sort event
;;; will re-arranged using duration.
;;;
(defgeneric claraoke:sort-event (subtitle))

