(cl:in-package #:cl-user)

(defpackage #:claraoke-color
  (:use #:common-lisp)
  (:intern
   #:*color-names*
   #:keyword-from-name
   #:min-max-delta-color
   #:normalize-color-name
   #:register-color-name)
  (:export
   #:ass-color
   #:ass-color-p
   #:cmyk-values
   #:color
   #:color-cmyk
   #:color-from-name
   #:color-hsl
   #:color-hsv
   #:color-to-integer
   #:combine-colors
   #:dec-from-hexstring
   #:hsvsl-values
   #:html-color
   #:html-color-p
   #:integer-to-color))

