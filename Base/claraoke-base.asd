;;;; claraoke-base.asd

(in-package #:asdf-user)

(defsystem #:claraoke-base
  :depends-on ("split-sequence")
  :serial t
  :components
  ((:file "packages")
   (:file "duration-protocol")
   (:file "conditions")
   ;; (:file "io-protocol")
   ;; (:file "io-methods")
   #| ... |#))
