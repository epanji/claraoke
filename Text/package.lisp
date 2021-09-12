(cl:in-package #:cl-user)

(defpackage #:claraoke-text
  (:use #:common-lisp)
  (:intern
   #:%compare-override
   #:*consonants*
   #:*index*
   #:*length*
   #:*spell-duration-in-centiseconds*
   #:*string*
   #:*strong-consonants*
   #:*text-index*
   #:*vowels*
   #:*weak-consonants*
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

