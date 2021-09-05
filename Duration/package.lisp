(cl:in-package #:cl-user)

(defpackage #:claraoke-duration
  (:use #:common-lisp)
  (:intern
   #:+duration-characters+)
  (:export
   #:duration
   #:duration-character-p))

