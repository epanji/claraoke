;;;; packages.lisp

(in-package #:cl-user)

(defpackage #:claraoke
  (:use)
  (:export
   #:hours
   #:minutes
   #:seconds
   #:centiseconds
   #:duration
   #:durationp
   #:durationstring
   #:durationstringp
   #:durationinteger
   #:durationintegerp
   #:increase-duration
   #:decrease-duration
   ;; #:file-not-found
   ;; #:failed-writing-to-file
   ;; #:abort-writing-to-file
   #:claraoke-error
   #|..Mores..|#))

(defpackage #:claraoke-internal
  (:use #:common-lisp)
  (:export
   #|..Mores..|#))

(defpackage #:claraoke-base
  (:use #:common-lisp)
  (:export))
