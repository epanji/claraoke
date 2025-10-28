(cl:in-package #:cl-user)

(defpackage #:claraoke-color
  (:use #:common-lisp)
  (:intern
   #:*color-names*
   #:keyword-from-name
   #:normalize-color-name
   #:register-color-name)
  (:export
   #:ass-color
   #:ass-color-p
   #:color
   #:color-from-name
   #:color-to-integer
   #:dec-from-hexstring
   #:html-color
   #:html-color-p
   #:integer-to-color))

