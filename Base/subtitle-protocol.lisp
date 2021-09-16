(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Script (Subtitle)
;;;
(defgeneric claraoke:subtitle (object &rest initargs &key &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sections
;;;
;;; Script info, Styles, Events, Fonts, Graphics
;;;
(defgeneric claraoke:script-info (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:styles (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:events (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:fonts (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:graphics (object &rest initargs &key &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub Sections
;;;
;;; Note, Info, Style, Dialogue, Comment
;;; Picture, Sound, Movie
;;;
(defgeneric claraoke:note (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:info (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:style (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:dialogue (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:comment (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:picture (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:sound (object &rest initargs &key &allow-other-keys))

(defgeneric claraoke:movie (object &rest initargs &key &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operation
;;;
(defgeneric claraoke:insert-info (object info))

(defgeneric claraoke:delete-info (object info))

(defgeneric claraoke:find-info (object info))

(defgeneric claraoke:insert-note (object note))

(defgeneric claraoke:delete-note (object note))

(defgeneric claraoke:find-note (object note))

(defgeneric claraoke:insert-style (object style))

(defgeneric claraoke:delete-style (object style))

(defgeneric claraoke:find-style (object style))

(defgeneric claraoke:insert-event (object event &rest initargs &key &allow-other-keys))

(defgeneric claraoke:delete-event (object event))

(defgeneric claraoke:find-event (object index))

(defgeneric claraoke:insert-font (object font))

(defgeneric claraoke:delete-font (object font))

(defgeneric claraoke:find-font (object font))

(defgeneric claraoke:insert-graphic (object graphic))

(defgeneric claraoke:delete-graphic (object graphic))

(defgeneric claraoke:find-graphic (object graphic))

(defgeneric claraoke:last-event (object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index will not be arranged manually.
;;; Because event depends on start duration, sort event
;;; will re-arranged using start duration.
;;;
(defgeneric claraoke:sort-events (object))

