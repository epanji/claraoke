(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text have overrides
;;;
;;; Overrides: newline and batch
;;; Batch have ordered modifiers
;;; Newline is modifier and override too
;;;
(defgeneric claraoke:text (object &rest initargs &key &allow-other-keys)
  (:documentation "Return TEXT object which created base on arguments.
It will behave like .TEXT accessor when having same OBJECT argument."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Override
;;;
(defgeneric claraoke:override (object index &rest initargs &key &allow-other-keys)
  (:documentation "Return OVERRIDE object or MODIFIER object depends on INDEX argument and other arguments."))

(defgeneric claraoke:insert-override (object override)
  (:documentation "Return OBJECT argument after OVERRIDE argument has been inserted."))

(defgeneric claraoke:delete-override (object override)
  (:documentation "Return OBJECT argument after OVERRIDE argument has been deleted."))

(defgeneric claraoke:find-override (object override)
  (:documentation "Return OVERRIDE object from OBJECT argument with hint from OVERRIDE argument."))

(defgeneric claraoke:increase-override (object &optional delta)
  (:documentation "Return OBJECT argument after increased by DELTA argument."))

(defgeneric claraoke:decrease-override (object &optional delta)
  (:documentation "Return OBJECT argument after decreased by DELTA argument."))

(defgeneric claraoke:sort-overrides (text)
  (:documentation "Return TEXT argument after overrides have been sorted."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifier
;;;
(defgeneric claraoke:modifier (object &rest initargs &key &allow-other-keys)
  (:documentation "Return MODIFIER object which created base on arguments."))

(defgeneric claraoke:insert-modifier (object modifier)
  (:documentation "Return OBJECT argument after MODIFIER argument has been inserted."))

(defgeneric claraoke:delete-modifier (object modifier)
  (:documentation "Return OBJECT argument after MODIFIER argument has been deleted."))

(defgeneric claraoke:find-modifier (object modifier)
  (:documentation "Return MODIFIER object from OBJECT argument with hint from MODIFIER argument."))

(defgeneric claraoke:increase-modifier (object &optional delta key)
  (:documentation "Return OBJECT argument after KEY argument has been increased by DELTA argument."))

(defgeneric claraoke:decrease-modifier (object &optional delta key)
  (:documentation "Return OBJECT argument after KEY argument has been decreased by DELTA argument."))

(defgeneric claraoke:update-modifier (object value &optional key)
  (:documentation "Return OBJECT argument after KEY argument has been updated by VALUE argument."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenient
;;;
(defgeneric claraoke:insert-karaoke (object index &optional value)
  (:documentation "Return OBJECT argument after KARAOKE object with VALUE argument has been inserted into OVERRIDE object within OBJECT argument.
The OVERRIDE object choosen by hint from INDEX argument.
If OVERRIDE object does not exists, it will be created base on INDEX argument.
If KARAOKE, KARAOKE-FILL, KARAOKE-OUTLINE object exists, it will be change to KARAOKE object with VALUE argument."))

(defgeneric claraoke:insert-karaoke-fill (object index &optional value)
  (:documentation "Return OBJECT argument after KARAOKE-FILL object with VALUE argument has been inserted into OVERRIDE object within OBJECT argument.
The OVERRIDE object choosen by hint from INDEX argument.
If OVERRIDE object does not exists, it will be created base on INDEX argument.
If KARAOKE, KARAOKE-FILL, KARAOKE-OUTLINE object exists, it will be change to KARAOKE-FILL object with VALUE argument."))

(defgeneric claraoke:insert-karaoke-outline (object index &optional value)
  (:documentation "Return OBJECT argument after KARAOKE-OUTLINE object with VALUE argument has been inserted into OVERRIDE object within OBJECT argument.
The OVERRIDE object choosen by hint from INDEX argument.
If OVERRIDE object does not exists, it will be created base on INDEX argument.
If KARAOKE, KARAOKE-FILL, KARAOKE-OUTLINE object exists, it will be change to KARAOKE-OUTLINE object with VALUE argument."))

(defgeneric claraoke:increase-karaoke (object &optional delta)
  (:documentation "Return OBJECT argument after any TYPE karaoke has been founded and increased by DELTA argument."))

(defgeneric claraoke:decrease-karaoke (object &optional delta)
  (:documentation "Return OBJECT argument after any TYPE karaoke has been founded and decreased by DELTA argument."))

(defgeneric claraoke:update-karaoke (object value)
  (:documentation "Return OBJECT argument after any TYPE karaoke has been founded and updated by DELTA argument."))

