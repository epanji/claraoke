;;;; conditions.lisp

(in-package #:claraoke-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Abstract class
;;;
(defun report-object-error (condition stream)
  (format stream "~&Object ~A is not type ~A.~%"
          (object condition)
          (expected-type condition)))

(define-condition object-error (error)
  ((%object
    :initarg :object
    :initform nil
    :accessor object)
   (%expected-type
    :initform (error "Need to provide this from subclass.")
    :initarg :expected-type
    :reader expected-type))
  (:report report-object-error))

(defun report-creation-error (condition stream)
  (format stream "~&Can not create ~A from ~A.~%"
          (output-type condition)
          (input condition)))

(define-condition creation-error (error)
  ((%output-type
    :initform (error "Need to provide this from subclass.")
    :initarg :output-type
    :reader output-type)
   (%input
    :initarg :input
    :initform nil
    :accessor input))
  (:report report-creation-error))

(defun report-existent-error (condition stream)
  (format stream "~&~A not found."
          (item condition)))

(define-condition existent-error (error)
  ((%item
    :initarg :item
    :initform nil
    :accessor item))
  (:report report-existent-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Concrete class
;;;
(define-condition claraoke:style-not-found (existent-error)
  ())

(define-condition claraoke:failed-to-create-duration (creation-error)
  ()
  (:default-initargs
   :output-type "DURATION"))

(define-condition claraoke:failed-to-create-subtitle (creation-error)
  ()
  (:default-initargs
   :output-type "SUBTITLE"))

(define-condition claraoke:failed-to-create-style (creation-error)
  ()
  (:default-initargs
   :output-type "STYLE"))

(define-condition claraoke:object-must-be-subtitle (object-error)
  ()
  (:default-initargs
   :expected-object "SUBTITLE"))

(define-condition claraoke:object-must-be-duration (object-error)
  ()
  (:default-initargs
   :expected-object "DURATION"))

(define-condition claraoke:object-must-be-style (object-error)
  ()
  (:default-initargs
   :expected-object "STYLE"))

(define-condition claraoke:object-must-be-event (object-error)
  ()
  (:default-initargs
   :expected-object "EVENT"))

(define-condition claraoke:object-must-be-integer (object-error)
  ()
  (:default-initargs
   :expected-object "INTEGER"))

(define-condition claraoke:object-must-be-text (object-error)
  ()
  (:default-initargs
   :expected-object "TEXT"))

(define-condition claraoke:object-must-be-override (object-error)
  ()
  (:default-initargs
   :expected-object "OVERRIDE"))

