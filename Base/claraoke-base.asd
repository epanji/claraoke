(cl:in-package #:asdf-user)

(defsystem #:claraoke-base
  :depends-on ("split-sequence")
  :serial t
  :components
  ((:file "packages")
   (:file "text-protocol")
   (:file "color-protocol")
   (:file "duration-protocol")
   (:file "subtitle-protocol")
   (:file "conditions")
   (:file "base-protocol")
   (:file "base-implementation")
   #| ... |#))

