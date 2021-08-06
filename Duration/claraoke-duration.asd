(cl:in-package #:asdf-user)

(defsystem "claraoke-duration"
  :depends-on ("claraoke-base")
  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "duration-implementation")
   #| ... |#))

