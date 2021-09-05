(cl:in-package #:cl-user)

(defpackage #:claraoke-text
  (:use #:common-lisp)
  (:intern
   #:%compare-override
   #:*consonant*
   #:*estimate-karaoke-duration*
   #:*index*
   #:*length*
   #:*string*
   #:*strong-consonant*
   #:*text-index*
   #:*vowel*
   #:*weak-consonant*
   #:advance
   #:build-string-or-override
   #:compute-override
   #:consume
   #:consume-override
   #:consume-spelling
   #:consume-text
   #:end-override-matcher
   #:end-spelling-matcher
   #:normalize-override-text
   #:peek
   #:start-override-matcher
   #:valid-index-p)
  (:export
   #:defile-text
   #:override
   #:override-greaterp
   #:override-lessp
   #:print-override
   #:purify-text
   #:same-override-p
   #:text))

