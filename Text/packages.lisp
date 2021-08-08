(cl:in-package #:cl-user)

(defpackage #:claraoke-text
  (:use #:common-lisp)
  (:intern
   #:%compare-override
   #| ... |#)
  (:export
   #:text
   #:override
   #:print-override
   #:same-override-p
   #:override-lessp
   #:override-greaterp
   #| ... |#))

