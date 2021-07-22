;;;; conditions.lisp

(in-package #:claraoke-base)

(define-condition claraoke:claraoke-error (error)
  ())

(define-condition claraoke:object-must-be-subtitle
    (claraoke:claraoke-error)
  ((%object :initarg :object :reader object)))

(define-condition claraoke:object-must-be-style
    (claraoke:claraoke-error)
  ((%object :initarg :object :reader object)))

(define-condition claraoke:object-must-be-event
    (claraoke:claraoke-error)
  ((%object :initarg :object :reader object)))

(define-condition claraoke:object-must-be-integer
    (claraoke:claraoke-error)
  ((%object :initarg :object :reader object)))

(define-condition claraoke:style-not-found
    (claraoke:claraoke-error)
  ((%object :initarg :object :reader object)))

;; (define-condition claraoke:file-not-found (claraoke:claraoke-error)
;;   ())

;; (define-condition claraoke:failed-writing-to-file (claraoke:claraoke-error)
;;   ())

;; (define-condition claraoke:abort-writing-to-file (claraoke:claraoke-error)
;;   ())

