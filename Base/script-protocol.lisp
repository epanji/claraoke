(cl:in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Abstract Script
;;;
(defclass claraoke:script ()
  ((%lines
    :initarg :lines
    :accessor claraoke:lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Abstract Line
;;;
;;; 1. Section Line (Abstract Class)
;;; 2. Table Line (Abstract Class)
;;;
(defclass claraoke:line ()
  ((%descriptor
    :initarg :descriptor
    :reader claraoke:descriptor)))

(defclass claraoke:section-line (claraoke:line claraoke:script)
  ((%header
    :initform nil
    :initarg :header
    :reader claraoke:header)))

(defclass claraoke:table-line (claraoke:line)
  ((%separator
    :initarg :separator
    :reader claraoke:separator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Value mixin
;;;
(defclass claraoke:value-mixin ()
  ((%value
    :initarg :value
    :accessor claraoke:value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions
;;;
(defgeneric claraoke:insert-line (script line)
  (:documentation "Return LIST of lines after inserting LINE argument into SCRIPT argument."))

(defgeneric claraoke:delete-line (script line)
  (:documentation "Return LIST of lines after deleting LINE argument from SCRIPT argument."))

(defgeneric claraoke:find-line (script line)
  (:documentation "Return LINE object from SCRIPT argument with hint from LINE argument."))

(defgeneric claraoke:last-line (script)
  (:documentation "Return LINE object from last line in SCRIPT argument."))

