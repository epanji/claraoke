(cl:in-package #:claraoke-base)

(defgeneric claraoke:subtitle (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:script-info (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:style (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:insert-style (subtitle style))

(defgeneric claraoke:delete-style (subtitle style))

(defgeneric claraoke:find-style (subtitle content))

(defgeneric claraoke:insert-event (subtitle event))

(defgeneric claraoke:delete-event (subtitle event))

(defgeneric claraoke:find-event (subtitle index))

(defgeneric claraoke:last-event (subtitle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index will not be arranged manually.
;;; Because event depends on start duration, sort event
;;; will re-arranged using start duration.
;;;
(defgeneric claraoke:sort-events (subtitle))

