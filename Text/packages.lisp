(cl:in-package #:cl-user)

(defpackage #:claraoke-text
  (:use #:common-lisp)
  (:export
   #:text
   #:override
   #:print-override
   #:%compare-override
   #:same-override-p
   #:override-lessp
   #:override-greaterp
   #| ... |#))

