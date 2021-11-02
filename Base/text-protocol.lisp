(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text have overrides
;;;
;;; Overrides: newline and batch
;;; Batch have ordered modifiers
;;; Newline is modifier and override too
;;;
(defgeneric claraoke:text (object &rest initargs &key &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override
;;;
(defgeneric claraoke:override (object index &rest initargs &key &allow-other-keys))

(defgeneric claraoke:insert-override (object override))

(defgeneric claraoke:delete-override (object override))

(defgeneric claraoke:find-override (object override))

(defgeneric claraoke:increase-override (object &optional delta))

(defgeneric claraoke:decrease-override (object &optional delta))

(defgeneric claraoke:sort-overrides (text))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifier
;;;
(defgeneric claraoke:modifier (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:insert-modifier (object modifier))

(defgeneric claraoke:delete-modifier (object modifier))

(defgeneric claraoke:find-modifier (object modifier))

(defgeneric claraoke:increase-modifier (object &optional delta key))

(defgeneric claraoke:decrease-modifier (object &optional delta key))

(defgeneric claraoke:update-modifier (object &optional delta key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenient
;;;
(defgeneric claraoke:increase-karaoke (object &optional delta))

(defgeneric claraoke:decrease-karaoke (object &optional delta))

(defgeneric claraoke:update-karaoke (object value))

