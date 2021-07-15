;;;; conditions.lisp

(in-package #:claraoke-base)

(define-condition claraoke:claraoke-error (error)
  ())

;; (define-condition claraoke:file-not-found (claraoke:claraoke-error)
;;   ())

;; (define-condition claraoke:failed-writing-to-file (claraoke:claraoke-error)
;;   ())

;; (define-condition claraoke:abort-writing-to-file (claraoke:claraoke-error)
;;   ())

