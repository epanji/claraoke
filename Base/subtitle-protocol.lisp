(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Script (Subtitle)
;;;
(defgeneric claraoke:subtitle (object &rest initargs &key &allow-other-keys)
  (:documentation "Return SUBTITLE object which created base on arguments."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sections
;;;
;;; Script info, Styles, Events, Fonts, Graphics
;;;
(defgeneric claraoke:script-info (object &rest initargs &key &allow-other-keys)
  (:documentation "Return SCRIPT INFO object from OBJECT argument or create new one from arguments."))

(defgeneric claraoke:styles (object &rest initargs &key &allow-other-keys)
  (:documentation "Return STYLES object from OBJECT argument or create new one from arguments."))

(defgeneric claraoke:events (object &rest initargs &key &allow-other-keys)
  (:documentation "Return EVENTS object from OBJECT argument or create new one from arguments."))

(defgeneric claraoke:fonts (object &rest initargs &key &allow-other-keys)
  (:documentation "Return FONTS object from OBJECT argument or create new one from arguments."))

(defgeneric claraoke:graphics (object &rest initargs &key &allow-other-keys)
  (:documentation "Return GRAPHICS object from OBJECT argument or create new one from arguments."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub Sections
;;;
;;; Note, Info, Style, Dialogue, Comment
;;; Picture, Sound, Movie, Command
;;;
(defgeneric claraoke:note (object &rest initargs &key &allow-other-keys)
  (:documentation "Return NOTE object which created base on arguments."))

(defgeneric claraoke:info (object &rest initargs &key &allow-other-keys)
  (:documentation "Return INFO object which created base on arguments."))

(defgeneric claraoke:style (object &rest initargs &key &allow-other-keys)
  (:documentation "Return STYLE object which created base on arguments.
It will behave like .STYLE accessor when having same OBJECT argument."))

(defgeneric claraoke:dialogue (object &rest initargs &key &allow-other-keys)
  (:documentation "Return DIALOGUE object which created base on arguments."))

(defgeneric claraoke:comment (object &rest initargs &key &allow-other-keys)
  (:documentation "Return COMMENT object which created base on arguments."))

(defgeneric claraoke:picture (object &rest initargs &key &allow-other-keys)
  (:documentation "Return PICTURE object which created base on arguments."))

(defgeneric claraoke:sound (object &rest initargs &key &allow-other-keys)
  (:documentation "Return SOUND object which created base on arguments."))

(defgeneric claraoke:movie (object &rest initargs &key &allow-other-keys)
  (:documentation "Return MOVIE object which created base on arguments."))

(defgeneric claraoke:command (object &rest initargs &key &allow-other-keys)
  (:documentation "Return COMMAND object which created base on arguments."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operation
;;;
(defgeneric claraoke:insert-info (object info)
  (:documentation "Return LIST of info after inserting INFO argument into OBJECT argument."))

(defgeneric claraoke:delete-info (object info)
  (:documentation "Return LIST of info after deleting INFO argument from OBJECT argument."))

(defgeneric claraoke:find-info (object info)
  (:documentation "Return INFO object from OBJECT argument with hint from INFO argument."))

(defgeneric claraoke:insert-note (object note)
  (:documentation "Return LIST of notes after inserting NOTE argument into OBJECT argument."))

(defgeneric claraoke:delete-note (object note)
  (:documentation "Return LIST of notes after deleting NOTE argument from OBJECT argument."))

(defgeneric claraoke:find-note (object note)
  (:documentation "Return NOTE object from OBJECT argument with hint from NOTE argument."))

(defgeneric claraoke:insert-style (object style)
  (:documentation "Return LIST of styles after inserting STYLE argument into OBJECT argument."))

(defgeneric claraoke:delete-style (object style)
  (:documentation "Return LIST of styles after deleting STYLE argument from OBJECT argument."))

(defgeneric claraoke:find-style (object style)
  (:documentation "Return STYLE object from OBJECT argument with hint from STYLE argument."))

(defgeneric claraoke:insert-event (object event &rest initargs &key &allow-other-keys)
  (:documentation "Return LIST of events after inserting EVENT argument into OBJECT argument.
INITARGS argument will affect EVENT argument during insertion."))

(defgeneric claraoke:delete-event (object event)
  (:documentation "Return LIST of events after deleting EVENT argument from OBJECT argument."))

(defgeneric claraoke:find-event (object index)
  (:documentation "Return EVENT object from OBJECT argument with hint from EVENT argument."))

(defgeneric claraoke:insert-font (object font)
  (:documentation "Return LIST of fonts after inserting FONT argument into OBJECT argument."))

(defgeneric claraoke:delete-font (object font)
  (:documentation "Return LIST of fonts after deleting FONT argument from OBJECT argument."))

(defgeneric claraoke:find-font (object font)
  (:documentation "Return FONT object from OBJECT argument with hint from FONT argument."))

(defgeneric claraoke:insert-graphic (object graphic)
  (:documentation "Return LIST of graphics after inserting GRAPHIC argument into OBJECT argument."))

(defgeneric claraoke:delete-graphic (object graphic)
  (:documentation "Return LIST of graphics after deleting GRAPHIC argument from OBJECT argument."))

(defgeneric claraoke:find-graphic (object graphic)
  (:documentation "Return GRAPHIC object from OBJECT argument with hint from GRAPHIC argument."))

(defgeneric claraoke:last-event (object)
  (:documentation "Return EVENT object from last event in OBJECT argument."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Index will not be arranged manually.
;;; Because event depends on start duration, sort event
;;; will re-arranged using start duration.
;;;
(defgeneric claraoke:sort-events (object)
  (:documentation "Return LIST of events which already sorted and saved in OBJECT argument."))

